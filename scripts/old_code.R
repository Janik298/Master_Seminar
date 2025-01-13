# Make to yearqrt
df_compensation_of_employees <- df_compensation_of_employees %>% 
    mutate(Quarter = as.yearqtr(observation_date, format = "%Y-%m-%d")) %>% 
    dplyr::select(-observation_date)

df_contr_social_gov <- df_contr_social_gov %>% 
    mutate(Quarter = as.yearqtr(observation_date, format = "%Y-%m-%d")) %>% 
    dplyr::select(-observation_date) %>% 
    rename(contr_social_gov = A061RC1Q027SBEA)

df_personal_current_taxes <- df_personal_current_taxes %>% 
    mutate(Quarter = as.yearqtr(observation_date, format = "%Y-%m-%d")) %>% 
    dplyr::select(-observation_date) %>% 
    rename(personal_current_taxes = W055RC1Q027SBEA)

df_personal_current_transfer <- df_personal_current_transfer %>% 
    mutate(Quarter = as.yearqtr(observation_date, format = "%Y-%m-%d")) %>% 
    dplyr::select(-observation_date) %>% 
    rename(personal_current_transfer = A577RC1Q027SBEA)


# Combine to one dataframe
df_labor_income <- df_compensation_of_employees %>% 
    inner_join(df_contr_social_gov, by = "Quarter") %>% 
    inner_join(df_personal_current_taxes, by = "Quarter") %>% 
    inner_join(df_personal_current_transfer, by = "Quarter") %>% 
    select("Quarter", 1:5)

# Calculate Labor Income
df_labor_income <- df_labor_income %>% 
    mutate(labor_income = COE + personal_current_transfer)

# Make labor_income logarithmic
df_labor_income <- df_labor_income %>% 
    mutate(labor_income = log(labor_income))





#------------------------





```{r eval = FALSE}
## Step 1: Estimate \omega Using Sieve Minimum Distance (SMD)
library(splines)

# Create basis functions for \psi_t and \phi_t
create_basis <- function(data, var, knots = 5) {
    range_var <- range(data[[var]], na.rm = TRUE)
    return(bs(data[[var]], degree = 3, knots = seq(range_var[1], range_var[2], length.out = knots), intercept = TRUE))
}


# Generate basis functions
basis_psi <- create_basis(data, "psi_t", knots = 5)
basis_phi <- create_basis(data, "phi_t", knots = 5)

# Create tensor product of basis functions
create_tensor_product <- function(basis_psi, basis_phi) {
    n_psi <- ncol(basis_psi)
    n_phi <- ncol(basis_phi)
    tensor_product <- matrix(0, nrow = nrow(basis_psi), ncol = n_psi * n_phi)
    col_idx <- 1
    for (i in 1:n_psi) {
        for (j in 1:n_phi) {
            tensor_product[, col_idx] <- basis_psi[, i] * basis_phi[, j]
            col_idx <- col_idx + 1
        }
    }
    return(tensor_product)
}

basis_tensor <- create_tensor_product(basis_psi, basis_phi)

# Define the objective function for SMD estimation
smd_objective <- function(params, basis_tensor, returns, consumption_growth, leverage, net_worth) {
    beta <- params[1]
    gamma <- params[2]
    
    # Compute \Omega as a linear combination of basis functions
    omega <- as.numeric(basis_tensor %*% params[-(1:2)])
    
    # Compute the SDF m_t = beta * (C_t / C_t-1)^(-gamma) * \Omega
    sdf <- beta * (consumption_growth^(-gamma)) * omega
    
    # Compute residuals for the moment conditions
    residuals <- sdf * returns - 1
    
    # Return the sum of squared residuals
    return(sum(residuals^2))
}

# Initial parameters: beta, gamma, and coefficients for \Omega
init_params <- c(0.99, 10, rep(0, ncol(basis_tensor)))

# Optimization for SMD estimation
optim_result <- optim(
    par = init_params,
    fn = smd_objective,
    basis_tensor = basis_tensor,
    returns = data$returns,
    consumption_growth = data$consumption_growth,
    leverage = data$phi_t,
    net_worth = data$psi_t,
    method = "BFGS"
)

# Extract estimated parameters
beta_hat <- optim_result$par[1]
gamma_hat <- optim_result$par[2]
omega_hat <- optim_result$par[-(1:2)]

## Step 2: GMM Estimation of Preference Parameters \beta and \gamma

# Compute \Omega from the estimated parameters
omega_estimated <- as.numeric(basis_tensor %*% omega_hat)

# Define instruments (conditioning variables)
compute_instruments <- function(data) {
    wt <- data.frame(
        cay_t = scale(data$cay_t),
        RREL_t = scale(data$RREL_t),
        SPEX_t = scale(data$SPEX_t),
        consumption_growth = scale(data$consumption_growth),
        leverage_t = scale(data$phi_t)
    )
    return(as.matrix(wt))
}

instruments <- compute_instruments(data)

# Define the GMM objective function
gmm_objective <- function(params, returns, omega_estimated, instruments) {
    beta <- params[1]
    gamma <- params[2]
    
    # Compute the SDF m_t
    sdf <- beta * (data$consumption_growth^(-gamma)) * omega_estimated
    
    # Moment conditions
    residuals <- sdf * returns - 1
    moments <- t(instruments) %*% residuals / nrow(instruments)
    
    # Return the quadratic form of moments
    return(sum(moments^2))
}

# Initial parameters for GMM
init_gmm_params <- c(beta_hat, gamma_hat)

# GMM optimization
gmm_result <- optim(
    par = init_gmm_params,
    fn = gmm_objective,
    returns = data$returns,
    omega_estimated = omega_estimated,
    instruments = instruments,
    method = "BFGS"
)

# Extract GMM-estimated preference parameters
beta_gmm <- gmm_result$par[1]
gamma_gmm <- gmm_result$par[2]

## Step 3: Compute Risk Prices Using Linear Model

# Compute \beta_j,c and \beta_j,\Omega for each asset
betas <- lapply(split(data, data$asset_id), function(asset_data) {
    fit <- lm(returns ~ consumption_growth + log(omega_estimated), data = asset_data)
    return(coef(fit))
})
betas <- do.call(rbind, betas)

# Cross-sectional regression for risk prices
cross_sectional_fit <- lm(avg_excess_return ~ betas[, "consumption_growth"] + betas[, "log(omega_estimated)"], data = data)

# Extract risk prices
lambda_c <- coef(cross_sectional_fit)["betas[, \"consumption_growth\"]"]
lambda_omega <- coef(cross_sectional_fit)["betas[, \"log(omega_estimated)\"]"]

## Results
cat("Estimated beta (GMM):", beta_gmm, "\n")
cat("Estimated gamma (GMM):", gamma_gmm, "\n")
cat("Risk price of consumption growth (lambda_c):", lambda_c, "\n")
cat("Risk price of omega (lambda_omega):", lambda_omega, "\n")

```

















```{r}
## Step 1: Estimate \omega Using Sieve Minimum Distance (SMD)

# Create basis functions for \psi_t and \phi_t
create_basis <- function(data, var, knots = 5) {
    # Generate cubic B-spline basis functions for a given variable
    range_var <- range(data[[var]], na.rm = TRUE)  # Determine range of the variable
    return(bs(data[[var]], degree = 3, knots = seq(range_var[1], range_var[2], length.out = knots), intercept = TRUE))
}

# Generate basis functions for \psi_t (net worth share) and \phi_t (leverage)
basis_psi <- create_basis(df_all_assets, "psi_t", knots = 5)
basis_phi <- create_basis(df_all_assets, "phi_t", knots = 5)

# Create tensor product of basis functions for \psi_t and \phi_t
create_tensor_product <- function(basis_psi, basis_phi) {
    # Combine basis functions to capture interactions between \psi_t and \phi_t
    n_psi <- ncol(basis_psi)  # Number of basis functions for \psi_t
    n_phi <- ncol(basis_phi)  # Number of basis functions for \phi_t
    tensor_product <- matrix(0, nrow = nrow(basis_psi), ncol = n_psi * n_phi)
    col_idx <- 1
    for (i in 1:n_psi) {
        for (j in 1:n_phi) {
            tensor_product[, col_idx] <- basis_psi[, i] * basis_phi[, j]  # Compute interaction terms
            col_idx <- col_idx + 1
        }
    }
    return(tensor_product)
}

basis_tensor <- create_tensor_product(basis_psi, basis_phi)  # Tensor product for \Omega approximation


# Define the moment conditions function for GMM with \omega updating
gmm_moment_conditions <- function(params, data, basis_tensor, instruments) {
    beta <- params[1]  # Preference parameter \beta
    gamma <- params[2]  # Preference parameter \gamma
    omega_coeffs <- params[-(1:2)]  # Coefficients for \Omega
    
    # Compute \Omega dynamically as a linear combination of basis functions
    omega <- as.numeric(basis_tensor %*% omega_coeffs)
    
    # Compute the SDF m_t
    sdf <- beta * (data$consumption_growth^(-gamma)) * omega
    
    # Compute residuals
    residuals <- sdf * data$returns - 1  # Residuals from moment conditions
    
    # Multiply residuals by instruments
    moments <- residuals * instruments  # Element-wise multiplication
    
    # Return the moments (matrix of moment conditions)
    return(as.matrix(moments))
}

# Define instruments (conditioning variables)
compute_instruments <- function(data) {
    # Create standardized conditioning variables (instruments)
    wt <- data.frame(
        cay_t = scale(data$cay_t),  # Log consumption-wealth ratio
        RREL_t = scale(data$RREL_t),  # Relative T-bill rate
        consumption_growth = scale(data$consumption_growth),  # Consumption growth
        leverage_t = scale(data$phi_t)  # Aggregate leverage
    )
    return(as.matrix(wt))  # Return instruments as matrix
}

instruments <- compute_instruments(df_all_assets)  # Compute instruments

# Combine initial estimates for GMM: \beta, \gamma, and \omega coefficients
beta_hat <- 0.99  # Initial estimate for \beta
gamma_hat <- 10  # Initial estimate for \gamma
init_gmm_params <- c(beta_hat, gamma_hat, omega_hat)

# Perform GMM estimation using the gmm package
library(gmm)  # Load the gmm package
gmm_result <- gmm(
    g = gmm_moment_conditions,         # Moment conditions function
    x = instruments,                   # Instruments
    t0 = init_gmm_params,              # Initial parameter estimates
    basis_tensor = basis_tensor,       # Basis functions for \Omega
    data = data                        # Data for estimation
)

# Extract the GMM-estimated parameters
beta_gmm <- coef(gmm_result)["beta"]   # GMM-estimated \beta
gamma_gmm <- coef(gmm_result)["gamma"] # GMM-estimated \gamma
omega_gmm <- coef(gmm_result)[-(1:2)]   # GMM-estimated coefficients for \Omega

# Compute updated \Omega using GMM estimates
omega_estimated_gmm <- as.numeric(basis_tensor %*% omega_gmm)

# Display results
cat("GMM-estimated beta:", beta_gmm, "\n")
cat("GMM-estimated gamma:", gamma_gmm, "\n")

## Step 3: Compute Risk Prices Using Linear Model

# Compute \beta_j,c and \beta_j,\Omega for each asset
betas <- lapply(split(data, data$asset_id), function(asset_data) {
    # Time-series regression for each asset
    fit <- lm(returns ~ consumption_growth + log(omega_estimated_gmm), data = asset_data)
    return(coef(fit))  # Return coefficients (factor loadings)
})
betas <- do.call(rbind, betas)  # Combine results for all assets

# Cross-sectional regression for risk prices
cross_sectional_fit <- lm(avg_excess_return ~ betas[, "consumption_growth"] + betas[, "log(omega_estimated_gmm)"], data = data)

# Extract risk prices
lambda_c <- coef(cross_sectional_fit)["betas[, \"consumption_growth\"]"]  # Risk price of consumption growth
lambda_omega <- coef(cross_sectional_fit)["betas[, \"log(omega_estimated_gmm)\"]"]  # Risk price of \Omega

## Results
cat("Risk price of consumption growth (lambda_c):", lambda_c, "\n")  # Output \lambda_c
cat("Risk price of omega (lambda_omega):", lambda_omega, "\n")  # Output \lambda_\Omega

```



# Load required packages
library(Sieve)

# Load and preprocess data
data <- df_equity  # Replace with actual data
phi <- data$phi_t  # Replace with actual column
psi <- data$psi_t     # Replace with actual column
consumption_growth <- data$consumption_growth
returns <- data$returns

# Define basis functions
basis_functions <- sieve_preprocess(
    X = cbind(phi, psi),
    basisN = 10,          # Number of basis functions
    type = "polynomial",  # Basis type: polynomial, cosine, etc.
    interaction_order = 5
)




# Sieve estimation
sieve_model <- sieve_solver(
    model = basis_functions,
    Y = returns,
    l1 = TRUE  # Use l1 regularization
)

# Extract coefficients of the pricing wedge
coefficients <- sieve_model$coef
print(coefficients)


summary(phi)
summary(psi)

head(basis_functions$X)  # Inspect the first few rows of generated basis functions
summary(basis_functions$X)  # Check the variability



plot(phi, psi, main = "Scatterplot of phi vs psi", xlab = "phi", ylab = "psi")


set.seed(123)  # Set seed for reproducibility

# Number of observations
n <- 1000

# Simulate phi (aggregate leverage) and psi (net worth share)
phi <- runif(n, min = 2, max = 7)            # Uniformly distributed leverage
psi <- rnorm(n, mean = 0.9, sd = 0.02)       # Normally distributed net worth share

# Simulate nonlinear interaction
interaction <- 0.5 * phi^2 - 0.3 * psi^2 + 0.2 * phi * psi

# Add noise
noise <- rnorm(n, mean = 0, sd = 0.5)

# Simulate returns (dependent variable)
returns <- 1 + 0.7 * phi - 0.4 * psi + interaction + noise

# Combine into a data frame
example_data <- data.frame(
    phi = phi,
    psi = psi,
    returns = returns
)

# View first few rows
head(example_data)

# Standardize phi and psi
phi <- scale(example_data$phi)
psi <- scale(example_data$psi)

# Generate basis functions
basis_functions <- sieve_preprocess(
    X = cbind(phi, psi),
    basisN = 50,
    type = "cosine",
    interaction_order = 2
)

# Run sieve solver
sieve_model <- sieve_solver(
    model = basis_functions,
    Y = example_data$returns,
    l1 = TRUE  # Use l1 regularization
)

basis_functions$X

# View model coefficients
print(sieve_model$coef)







# Newer with Sieve package

```{r}
# Preprocess the data for Sieve estimation
sieve_model <- sieve_preprocess(
    X = df_all_assets[, c("psi_t", "phi_t")],  # State variables
    basisN = 5,                       # Number of basis functions
    type = "cosine",                  # Use cosine basis functions
    interaction_order = 2              # Allow pairwise interactions
)


# Define the SMD objective function using Sieve
smd_objective_sieve <- function(params, sieve_model, returns, consumption_growth, beta, gamma) {
    # Debug input dimensions
    if (is.null(params) || length(params) == 0) stop("Error: params is NULL or empty.")
    
    # Compute \Omega using the sieve basis
    omega <- sieve_predict(model = sieve_model, params = params)$predictY
    if (any(is.na(omega))) stop("Error: NA values in omega computation.")
    
    # Compute the SDF
    sdf <- beta * (consumption_growth^(-gamma)) * omega
    if (any(is.na(sdf))) stop("Error: NA values in SDF computation.")
    
    # Compute residuals
    residuals <- sdf * returns - 1
    if (any(is.na(residuals))) stop("Error: NA values in residuals.")
    
    return(sum(residuals^2))
}

# Define a grid for \beta and \gamma
beta_grid <- seq(0.95, 0.99, length.out = 2)  # Example grid for \beta
gamma_grid <- seq(5, 15, length.out = 2)  # Example grid for \gamma

# Initialize storage for results
smd_results <- list()
min_residual <- Inf  # Track the minimum residual
best_params <- NULL  # Store the best \beta, \gamma, and \omega

# Initialize a counter
counter <- 1
total_combinations <- length(beta_grid) * length(gamma_grid)


# Loop over all combinations of \beta and \gamma
for (beta in beta_grid) {
    for (gamma in gamma_grid) {
        # Print progress
        cat("Processing combination", counter, "of", total_combinations, "(beta:", beta, ", gamma:", gamma, ")\n")
        counter <- counter + 1
        
        # Fit the Sieve model using the solver
        sieve_fit <- sieve_solver(
            model = sieve_model,  # Preprocessed sieve model
            Y = df_all_assets$returns,     # Target variable (returns)
            l1 = FALSE,
            family = "gaussian"
        )
        
        
        # Evaluate the objective function
        smd_result <- smd_objective_sieve(
            params = sieve_fit$beta_hat,  # Coefficients from the fitted model
            sieve_model = sieve_model,   # Preprocessed sieve model
            returns = df_all_assets$returns,      # Returns
            consumption_growth = df_all_assets$consumption_growth,  # Consumption growth
            beta = beta,                 # Current \beta
            gamma = gamma                # Current \gamma
        )
        
        # Check if this combination yields the lowest residual
        if (smd_result < min_residual) {
            min_residual <- smd_result
            best_params <- list(beta = beta, gamma = gamma, omega = sieve_fit$beta_hat)
        }
        
        # Store the results
        smd_results[[paste0("beta_", beta, "_gamma_", gamma)]] <- sieve_fit$beta_hat
    }
}

# Extract the best \beta, \gamma, and \Omega
best_beta <- best_params$beta
best_gamma <- best_params$gamma
omega_hat <- best_params$omega

# Output the best parameters
cat("Best beta:", best_beta, "\n")
cat("Best gamma:", best_gamma, "\n")
cat("Minimum residual:", min_residual, "\n")

# Define the GMM moment conditions function
gmm_moment_conditions <- function(params, data, instruments) {
    beta <- params[1]
    gamma <- params[2]
    omega_coeffs <- params[-(1:2)]
    
    # Compute \Omega dynamically using the best sieve model
    omega <- sieve_predict(model = sieve_model, params = omega_coeffs)$predictY
    
    # Compute the SDF
    sdf <- beta * (data$consumption_growth^(-gamma)) * omega
    
    # Compute residuals
    residuals <- sdf * data$returns - 1
    
    # Multiply residuals by instruments
    moments <- residuals * instruments
    
    return(as.matrix(moments))
}

# Prepare instruments for GMM estimation
instruments <- cbind(
    df_all_assets$cay_t,
    df_all_assets$RREL_t,
    df_all_assets$SPEX_t,
    df_all_assets$consumption_growth,
    df_all_assets$phi_t
)

# Combine initial estimates for GMM
init_gmm_params <- c(best_beta, best_gamma, omega_hat)

# Perform GMM estimation
gmm_result <- gmm(
    g = gmm_moment_conditions,
    x = instruments,
    t0 = init_gmm_params,
    data = df_all_assets
)

# Extract the GMM-estimated parameters
beta_gmm <- coef(gmm_result)["beta"]
gamma_gmm <- coef(gmm_result)["gamma"]
omega_gmm <- coef(gmm_result)[-(1:2)]

# Output GMM results
cat("GMM-estimated beta:", beta_gmm, "\n")
cat("GMM-estimated gamma:", gamma_gmm, "\n")

```



##########-------------##############





# New Version

```{r}
# Step 1: Estimate \omega Using Sieve Minimum Distance (SMD)
library(splines)  # Load splines library for basis function creation

# Create basis functions for \psi_t and \phi_t
create_basis <- function(data, var, knots = 5) {
    # Generate cubic B-spline basis functions for a given variable
    range_var <- range(data[[var]], na.rm = TRUE)  # Determine range of the variable
    return(bs(data[[var]], degree = 3, knots = seq(range_var[1], range_var[2], length.out = knots), intercept = TRUE))
}

# Generate basis functions for \psi_t (net worth share) and \phi_t (leverage)
basis_psi <- create_basis(df_all_assets, "psi_t", knots = 5)
basis_phi <- create_basis(df_all_assets, "phi_t", knots = 5)

# Create tensor product of basis functions for \psi_t and \phi_t
create_tensor_product <- function(basis_psi, basis_phi) {
    # Combine basis functions to capture interactions between \psi_t and \phi_t
    n_psi <- ncol(basis_psi)  # Number of basis functions for \psi_t
    n_phi <- ncol(basis_phi)  # Number of basis functions for \phi_t
    tensor_product <- matrix(0, nrow = nrow(basis_psi), ncol = n_psi * n_phi)
    col_idx <- 1
    for (i in 1:n_psi) {
        for (j in 1:n_phi) {
            tensor_product[, col_idx] <- basis_psi[, i] * basis_phi[, j]  # Compute interaction terms
            col_idx <- col_idx + 1
        }
    }
    return(tensor_product)
}


basis_tensor <- create_tensor_product(basis_psi, basis_phi)  # Tensor product for \Omega approximation

# Define the SMD objective function
smd_objective <- function(params, basis_tensor, returns, consumption_growth, beta, gamma) {
    omega_coeffs <- params  # Coefficients for \Omega
    
    # Compute Omega
    omega <- as.numeric(basis_tensor %*% omega_coeffs)
    if (any(!is.finite(omega))) print("Non-finite values in omega")
    
    
    # Compute SDF
    sdf <- beta * (consumption_growth^(-gamma)) * omega
    if (any(!is.finite(sdf))) print("Non-finite values in sdf")
    
    # Compute residuals
    residuals <- sdf * returns - 1
    if (any(!is.finite(residuals))) print("Non-finite values in residuals")
    
    # Return the sum of squared residuals
    return(sum(residuals^2))
}


# Define a grid for \beta and \gamma
beta_grid <- seq(0.98, 0.99, length.out = 2)  # Example grid for \beta
gamma_grid <- seq(10, 15, length.out = 2)  # Example grid for \gamma

# Initialize storage for results
smd_results <- list()
min_residual <- Inf  # Track the minimum residual
best_params <- NULL  # Store the best \beta, \gamma, and \omega

# Initialize a counter
start_time <- Sys.time()
cat("Start time:", start_time, "\n")

# Loop over all combinations of \beta and \gamma
for (beta in beta_grid) {
    for (gamma in gamma_grid) {
        # Print progress
        iteration_start <- Sys.time()
        cat("Starting beta =", beta, ", gamma =", gamma, "at", iteration_start, "\n")
        
        
        # Initial parameters for \Omega (starting with zeros)
        init_omega <- runif(ncol(basis_tensor), min = -0.05, max = 0.05)  # Small random values
        
        # Optimize \Omega coefficients using SMD
        smd_result <- tryCatch({
            optim(
                par = init_omega,
                fn = smd_objective,
                basis_tensor = basis_tensor,
                returns = df_all_assets$returns,
                consumption_growth = df_all_assets$consumption_growth,
                beta = beta,
                gamma = gamma,
                method = "BFGS"
            )
        }, error = function(e) {
            cat("Error for beta =", beta, "gamma =", gamma, ":", e$message, "\n")
            return(NULL)
        })
        
        if (is.null(smd_result)) next
        
        
        # Check if this combination yields the lowest residual
        if (smd_result$value < min_residual) {
            min_residual <- smd_result$value
            best_params <- list(beta = beta, gamma = gamma, omega = smd_result$par)
        }
        
        # Store the results
        smd_results[[paste0("beta_", beta, "_gamma_", gamma)]] <- smd_result$par
        
        # Iteration Counter
        iteration_end <- Sys.time()
        cat("Completed beta =", beta, ", gamma =", gamma, "in", iteration_end - iteration_start, "seconds\n")
    }
}

end_time <- Sys.time()
cat("End time:", end_time, "\nTotal time elapsed:", end_time - start_time, "\n")


# Extract the best \beta, \gamma, and \Omega
best_beta <- best_params$beta
best_gamma <- best_params$gamma
omega_hat <- best_params$omega

# Output the best parameters
cat("Best beta:", best_beta, "\n")
cat("Best gamma:", best_gamma, "\n")
cat("Minimum residual:", min_residual, "\n")

## Step 2: GMM Estimation of Preference Parameters \beta and \gamma Using the SMD \Omega

# Embed basis_tensor in the data object
df_all_assets$basis_tensor <- basis_tensor

# Define the moment conditions function for GMM with \omega updating
gmm_moment_conditions <- function(params, data, instruments, basis_tensor, returns, consumption_growth) {
    beta <- params[1]  # Preference parameter \beta
    gamma <- params[2]  # Preference parameter \gamma
    omega_coeffs <- params[-(1:2)]  # Coefficients for \Omega
    
    
    # Compute \Omega dynamically as a linear combination of basis functions
    omega <- as.numeric(basis_tensor %*% omega_coeffs)
    if (any(!is.finite(omega))) stop("Non-finite values in omega")  
    
    # Compute the SDF m_t
    # Compute consumption_growth^(-gamma)
    growth_term <- consumption_growth^(-gamma)
    if (any(!is.finite(growth_term))) stop("Non-finite values in consumption_growth^(-gamma)")
    
    # Compute SDF
    sdf <- beta * growth_term * omega
    if (any(!is.finite(sdf))) stop("Non-finite values in sdf")
    
    if (any(!is.finite(df_all_assets$returns))) stop("Non-finite values in returns")
    
    # Compute residuals
    residuals <- sdf * returns - 1  # Residuals from moment conditions
    if (any(!is.finite(residuals))) stop("Non-finite values in residuals")
    
    # Multiply residuals by instruments
    moments <- residuals * instruments  # Element-wise multiplication
    if (any(!is.finite(moments))) stop("Non-finite values in moments")
    
    # Return the moments (matrix of moment conditions)
    return(as.matrix(moments))
}

# Define instruments (conditioning variables)
compute_instruments <- function(data) {
    # Create standardized conditioning variables (instruments)
    wt <- data.frame(
        cay_t = scale(data$cay_t),  # Log consumption-wealth ratio
        RREL_t = scale(data$RREL_t),  # Relative T-bill rate
        consumption_growth = scale(data$consumption_growth)  # Consumption growth
    )
    return(as.matrix(wt))  # Return instruments as matrix
}

instruments <- compute_instruments(df_all_assets)  # Compute instruments

# Combine initial estimates for GMM: \beta, \gamma, and \omega coefficients
beta_hat <- 0.99  # Initial estimate for \beta
gamma_hat <- 1  # Initial estimate for \gamma
init_omega <- rep(0, ncol(df_all_assets$basis_tensor)) 
init_gmm_params <- c(beta_hat, gamma_hat, omega_hat)

# Perform GMM estimation using the gmm package
library(gmm)  # Load the gmm package
gmm_result <- gmm(
    g = function(params, data) {
        gmm_moment_conditions(
            params = params,
            data = df_all_assets,
            instruments = instruments,
            returns = df_all_assets$returns,
            consumption_growth = df_all_assets$consumption_growth,
            basis_tensor = df_all_assets$basis_tensor
        )
    },    # Moment conditions function
    x = instruments,                   # Instruments
    t0 = init_gmm_params,              # Initial parameter estimates
    data = df_all_assets  ,          # Data for estimation
    prewhite = FALSE  # Disable prewhitening
)


# Extract the GMM-estimated parameters
beta_gmm <- coef(gmm_result)["beta"]   # GMM-estimated \beta
gamma_gmm <- coef(gmm_result)["gamma"] # GMM-estimated \gamma
omega_gmm <- coef(gmm_result)[-(1:2)]   # GMM-estimated coefficients for \Omega

# Compute updated \Omega using GMM estimates
omega_estimated_gmm <- as.numeric(basis_tensor %*% omega_gmm)

# Display results
cat("GMM-estimated beta:", beta_gmm, "\n")
cat("GMM-estimated gamma:", gamma_gmm, "\n")

# Check for non-positive or missing values
if (any(df_all_assets$consumption_growth <= 0)) {
    cat("Non-positive values in consumption_growth:\n")
    print(df_all_assets$consumption_growth[df_all_assets$consumption_growth <= 0])
}



## Step 3: Compute Risk Prices Using Linear Model

# Compute \beta_j,c and \beta_j,\Omega for each asset
betas <- lapply(split(data, data$asset_id), function(asset_data) {
    # Time-series regression for each asset
    fit <- lm(returns ~ consumption_growth + log(omega_estimated_gmm), data = asset_data)
    return(coef(fit))  # Return coefficients (factor loadings)
})
betas <- do.call(rbind, betas)  # Combine results for all assets

# Cross-sectional regression for risk prices
cross_sectional_fit <- lm(avg_excess_return ~ betas[, "consumption_growth"] + betas[, "log(omega_estimated_gmm)"], data = data)

# Extract risk prices
lambda_c <- coef(cross_sectional_fit)["betas[, \"consumption_growth\"]"]  # Risk price of consumption growth
lambda_omega <- coef(cross_sectional_fit)["betas[, \"log(omega_estimated_gmm)\"]"]  # Risk price of \Omega

## Results
cat("Risk price of consumption growth (lambda_c):", lambda_c, "\n")  # Output \lambda_c
cat("Risk price of omega (lambda_omega):", lambda_omega, "\n")  # Output \lambda_\Omega


# Check for missing values in instruments
cat("Missing values in instruments:", sum(is.na(instruments)), "\n")

# Check for missing values in returns and consumption_growth
cat("Missing values in returns:", sum(is.na(df_all_assets$returns)), "\n")
cat("Missing values in consumption_growth:", sum(is.na(df_all_assets$consumption_growth)), "\n")

# If missing values are present, remove or impute them
if (any(is.na(instruments))) {
    instruments <- instruments[complete.cases(instruments), ]
}
df_all_assets <- na.omit(df_all_assets)  # Remove rows with any missing values


# Check variability in instruments
apply(instruments, 2, function(x) cat("Variance of column:", var(x), "\n"))

cat("Dimensions of instruments:", dim(instruments), "\n")
cat("Rows in df_all_assets:", nrow(df_all_assets), "\n")

if (nrow(instruments) != nrow(df_all_assets)) {
    stop("Mismatch between instruments and df_all_assets row counts.")
}



```












```{r}
# Step 1: Estimate \omega Using Sieve Minimum Distance (SMD)

df <- df_equity

# Standardize the key variables
columns_to_standardize <- c("psi_t", "phi_t", "cay_t", "T_Bill_Rate", 
                            "RREL_t", "consumption_growth", "avg_excess_return")

df <- df %>%
    mutate(across(all_of(columns_to_standardize), ~ scale(.) %>% as.numeric()))



# Create B-spline basis functions for psi_t and phi_t
# Define the degree of the spline and number of knots
degree <- 3  # Cubic splines
knots_psi <- quantile(df$psi_t, probs = seq(0, 1, length.out = 5))
knots_phi <- quantile(df$phi_t, probs = seq(0, 1, length.out = 5))


# Generate basis functions for psi_t and phi_t
df <- df %>%
    mutate(
        b_psi_t = bs(psi_t, knots = knots_psi, degree = degree, intercept = TRUE),
        b_phi_t = bs(phi_t, knots = knots_phi, degree = degree, intercept = TRUE)
    )


# Combine the basis functions using tensor products for Omega(psi_t, phi_t)
# Each row contains the tensor product of basis functions for psi_t and phi_t
basis_tensor <- outer(
    1:nrow(df), 1:nrow(df), 
    FUN = function(i, j) {
        as.vector(tcrossprod(as.matrix(df$b_psi_t[i, ]), as.matrix(df$b_phi_t[j, ])))
    }
)

library(Matrix) 

# Define parameters for basis functions
degree <- 3  # Degree for cubic splines
n_knots <- 5  # Number of knots
knots_psi <- quantile(df$psi_t, probs = seq(0, 1, length.out = n_knots))
knots_phi <- quantile(df$phi_t, probs = seq(0, 1, length.out = n_knots))

# Generate sparse basis functions for psi_t and phi_t
sparse_basis_psi <- bs(df$psi_t, knots = knots_psi, degree = degree, intercept = TRUE)
sparse_basis_phi <- bs(df$phi_t, knots = knots_phi, degree = degree, intercept = TRUE)

# Combine the basis functions using a sequential approach (avoiding full tensor)
num_obs <- nrow(df)
num_basis_psi <- ncol(sparse_basis_psi)
num_basis_phi <- ncol(sparse_basis_phi)

# Initialize sparse matrix for tensor product
tensor_product <- Matrix(0, nrow = num_obs, ncol = num_basis_psi * num_basis_phi, sparse = TRUE)

# Fill the sparse tensor product matrix
for (i in 1:num_basis_psi) {
    for (j in 1:num_basis_phi) {
        tensor_product[, (i - 1) * num_basis_phi + j] <- sparse_basis_psi[, i] * sparse_basis_phi[, j]
    }
}

# Save the tensor product and coefficients
saveRDS(tensor_product, "tensor_product_sparse.rds")
cat("Sparse tensor product matrix saved successfully.\n")




beta_init <- 0.99
gamma_init <- 5
params_init <- c(beta = beta_init, gamma = gamma_init)


# GMM Moment Conditions Function
gmm_moment_conditions_step1 <- function(coeffs, data, params) {
    beta <- params["beta"]
    gamma <- params["gamma"]
    
    # Calculate the HI-SDF (m_t)
    consumption_growth <- data$consumption_growth
    m_t <- beta * (consumption_growth)^(-gamma) * (tensor_product %*% coeffs)
    
    # Compute residuals (moment conditions)
    residuals <- m_t * data$returns - 1
    return(colMeans(residuals))  # Return the averaged moment conditions
}

# Optimize GMM for Basis Coefficients
gmm_fit_step1 <- optim(
    par = basis_coeffs_init,  # Initial coefficients
    fn = function(coeffs) {
        moment_conditions <- gmm_moment_conditions_step1(coeffs, df, params_init)
        return(sum(moment_conditions^2))  # Objective: Minimize the sum of squared moments
    },
    method = "BFGS",  # Optimization method
    control = list(reltol = 1e-6)
)

# Extract the estimated coefficients
basis_coeffs <- gmm_fit_step1$par
cat("Step 1: Basis coefficients estimated.\n")

# Use the basis coefficients to compute the pricing wedge Omega
omega_est <- tensor_product %*% basis_coeffs


# Step 2: Estimate Preference Parameters \beta and \gamma Using GMM

# Updated Moment Conditions Function for Preference Parameters
gmm_moment_conditions_step2 <- function(params, data, omega) {
    beta <- params["beta"]
    gamma <- params["gamma"]
    
    # Ensure parameters are within valid bounds
    if (beta <= 0 || beta > 1 || gamma <= 0) {
        return(rep(Inf, nrow(data)))  # Return large values to avoid invalid solutions
    }
    
    # Calculate the HI-SDF (m_t)
    consumption_growth <- data$consumption_growth
    if (any(consumption_growth <= 0)) {
        stop("Error: consumption_growth contains non-positive values.")
    }
    
    m_t <- beta * (consumption_growth)^(-gamma) * omega
    
    # Compute residuals (moment conditions)
    residuals <- m_t * data$returns - 1
    return(colMeans(residuals))  # Return the averaged moment conditions
}


# Check for non-positive consumption growth
if (any(df$consumption_growth <= 0)) {
    cat("Warning: Non-positive values detected in consumption_growth.\n")
    cat("Count of problematic rows:", sum(df$consumption_growth <= 0), "\n")
}

df$consumption_growth <- df$consumption_growth + 3

# Optimization with Parameter Bounds
gmm_fit_step2 <- optim(
    par = params_init,  # Initial preference parameters
    fn = function(params) {
        moment_conditions <- gmm_moment_conditions_step2(params, df, omega_est)
        return(sum(moment_conditions^2))  # Objective: Minimize the sum of squared moments
    },
    method = "L-BFGS-B",  # Optimization method with bounds
    lower = c(beta = 0.001, gamma = 0.1),  # Lower bounds
    upper = c(beta = 100, gamma = 50),  # Upper bounds
    control = list(factr = 1e7)
)

# Extract estimated preference parameters
preference_params <- gmm_fit_step2$par
cat("Step 2: Preference parameters estimated.\n")

# Display results
cat("Estimated Preference Parameters (beta, gamma):\n", preference_params, "\n")

# Save results for validation and further analysis
saveRDS(list(preference_params = preference_params), "preference_parameters_results.rds")


objective_value <- function(beta, gamma) {
    params <- c(beta = beta, gamma = gamma)
    moment_conditions <- gmm_moment_conditions_step2(params, df, omega_est)
    return(sum(moment_conditions^2))
}

# Test objective function at different beta and gamma
grid <- expand.grid(beta = seq(0.8, 1, length.out = 5), gamma = seq(0.1, 2, length.out = 5))
grid$objective <- mapply(objective_value, grid$beta, grid$gamma)

print(grid)

str(df_equity)
```