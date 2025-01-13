## Semi-parametric Minimum Distance Estimation of \Omega ####


# First Step Sieve Least Squares Estimation


# Step 1: Simulate data
set.seed(2)
T <- 500  # Number of time periods

# Simulate state variables
phi <- rnorm(T, mean = 0.5, sd = 0.1)  # Aggregate leverage
psi <- rnorm(T, mean = 0.7, sd = 0.1)  # Net worth share

# Simulate consumption data
C_t <- exp(cumsum(rnorm(T+1, mean = 0.02, sd = 0.01)))  # Simulate consumption as a geometric random walk
C_t_minus_1 <- c(NA, head(C_t, -1))  # Lagged consumption
C_g <- (C_t / C_t_minus_1)[-1]  # Approximate MRS with gamma = 2
returns <- rnorm(T, mean = 1.05, sd = 0.1)  # Simulate returns

# Simulate instruments
cay <- rnorm(T, mean = 0, sd = 1)  # Consumption-wealth ratio
RREL <- rnorm(T, mean = 0, sd = 0.1)  # Relative T-bill rate
SPEX <- rnorm(T, mean = 0, sd = 0.1)  # S&P excess return
instrument_matrix <- data.frame(cay, RREL, SPEX)



phi <- df_equity$phi_t
psi <- df_equity$psi_t
C_g <- df_equity$consumption_growth
returns <- df_equity$returns
cay <- df_equity$cay_t
RREL <- df_equity$RREL_t
instrument_matrix <- data.frame(cay, RREL)

# Step 2: Define basis functions
phi_basis <- ns(phi, df = 5)  # B-spline for phi
psi_basis <- ns(psi, df = 5)  # B-spline for psi
basis_matrix <- cbind(phi_basis, psi_basis)


# Step 3: Define the objective function for Sieve Least Squares with HI-SDF
sieve_objective_HISDF <- function(beta, gamma, return_omega = FALSE) {
    # Dependent variable for Sieve Projection
    Y <- 1 / (beta * C_g^(-gamma))  # Target for Omega
    
    # Solve for sieve coefficients dynamically based on beta and gamma
    sieve_coefficients <- solve(t(basis_matrix) %*% basis_matrix) %*% t(basis_matrix) %*% Y
    
    # Compute Omega using the updated sieve coefficients
    omega <- basis_matrix %*% sieve_coefficients
    
    # Compute HI-SDF
    HI_SDF <- beta * C_g^(-gamma) * omega
    
    # Compute residuals using instruments
    residuals <- rowSums(HI_SDF * returns) - 1
    weighted_residuals <- residuals %*% as.matrix(instrument_matrix)
    
    # Return Omega or the objective value
    if (return_omega) {
        return(omega)
    } else {
        return(sum(weighted_residuals^2))
    }
}



# Step 4: Perform a grid search over beta and gamma
beta_values <- seq(0.8, 0.99, length.out = 15)
gamma_values <- seq(5, 50, length.out = 15)

results <- expand.grid(gamma = gamma_values, beta = beta_values)
results$objective <- NA

# Evaluate the objective function for each (beta, gamma) pair
for (i in 1:nrow(results)) {
    beta <- results$beta[i]
    gamma <- results$gamma[i]
    results$objective[i] <- sieve_objective_HISDF(beta, gamma)
}



# Step 5: Find the optimal beta and gamma
optimal_params <- results[which.min(results$objective), ]
cat("Optimal beta:", optimal_params$beta, "\n")
cat("Optimal gamma:", optimal_params$gamma, "\n")

# Visualize the objective function surface
library(ggplot2)
ggplot(results, aes(x = beta, y = gamma, fill = objective)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Objective Function Surface with HI-SDF", x = "Beta", y = "Gamma", fill = "Objective")



# Compute Omega for the optimal beta and gamma
optimal_omega <- sieve_objective_HISDF(optimal_params$beta, optimal_params$gamma, return_omega = TRUE)







chi_i <- function(beta, gamma, c_g, phi, psi, C_g, returns, cay, RREL, omega) {

}





cons_growth <- df_equity$consumption_growth
intermediaries <- df_equity[, c("psi_t", "phi_t")] # Contains psi_t and phi_t
returns <- df_equity$returns          # Contains R_i,t



basis <- function(psi, phi, degree = 3) {
    # Create spline basis for psi and phi
    bs(psi, degree = degree) %*% t(bs(phi, degree = degree))
}



smd_objective <- function(params, cons_growth, psi_t, phi_t, returns) {
    beta <- params[1]   # Discount factor
    gamma <- params[2]  # Risk aversion
    Omega <- basis(psi_t, phi_t) %*% params[-(1:2)]  # Nonparametric term
    
    # Compute SDF
    mt <- beta * (cons_growth^(-gamma)) * Omega
    
    # Residuals from Euler equation
    residuals <- mt * returns - 1
    
    # Print debugging information
    print(list(beta = beta, gamma = gamma, Omega = Omega[1:5], residuals = residuals[1:5]))
    
    
    return(mean(residuals^2))  # Objective is to minimize mean squared residuals
}


# Initial guess for parameters
initial_params <- c(0.98, 2, rep(0, ncol(basis(intermediaries$psi_t, intermediaries$phi_t))))

# Optimize
smd_fit <- optim(
    par = initial_params,
    fn = smd_objective,
    cons_growth = cons_growth,
    psi_t = intermediaries$psi_t,
    phi_t = intermediaries$phi_t,
    returns = returns,
    method = "BFGS",
    control = list(trace = 1, REPORT = 1) # Print optimization progress 
)



### GMM Estimation of \Omega

moment_condition <- function(theta, data) {
    beta <- theta[1]
    gamma <- theta[2]
    Omega <- data$Omega
    
    # Compute SDF
    mt <- beta * (data$cons_growth^(-gamma)) * Omega
    
    # Residuals
    residuals <- mt * data$returns - 1
    return(colMeans(residuals))
}


gmm_fit <- gmm(
    g = moment_condition,
    x = list(
        cons_growth = cons_growth,
        returns = returns,
        Omega = smd_fit$par[-(1:2)]  # Use estimated Omega from SMD
    ),
    t0 = smd_fit$par[1:2]  # Use initial estimates of beta and gamma
)
summary(gmm_fit)


library(ggplot2)

# Generate grid
psi_grid <- seq(min(intermediaries$psi), max(intermediaries$psi), length.out = 100)
phi_grid <- seq(min(intermediaries$phi), max(intermediaries$phi), length.out = 100)

# Compute Omega on grid
Omega_grid <- outer(psi_grid, phi_grid, function(psi, phi) {
    sum(smd_fit$par[-(1:2)] * basis(psi, phi))
})

# Plot
ggplot(data = expand.grid(psi = psi_grid, phi = phi_grid), aes(x = psi, y = phi)) +
    geom_tile(aes(fill = Omega_grid)) +
    scale_fill_gradient2() +
    labs(title = "Pricing Wedge Omega", x = "Psi (Net Worth Share)", y = "Phi (Leverage)")








#### The making of the estimation by Janik aka Beats by Janik ####


# Load the data 

# Load the splines package
library(splines)

# Example data
psi_t <- c(0.7, 0.8, 0.75, 0.72)  # Net worth share
phi_t <- c(3, 3.2, 3.1, 3.05)     # Aggregate leverage

# Define the number of knots
K_psi <- 3
K_phi <- 4





# Efficient computation of the tensor product basis
compute_design_product <- function(bs_psi, bs_phi) {
    n <- nrow(bs_psi) # Number of rows in the B-spline basis for ψ
    d_psi <- ncol(bs_psi) # Number of basis functions for ψ
    d_phi <- ncol(bs_phi) # Number of basis functions for φ
    
    # Initialize the tensor product matrix
    tensor_product <- matrix(0, nrow = n, ncol = d_psi * d_phi)
    
    # Compute the tensor product row by row
    for (i in 1:n) {
        tensor_product[i, ] <- as.vector(outer(bs_psi[i, ], bs_phi[i, ]))
    }
    
    return(tensor_product)
}


# Define the SMD procedure for a given beta and gamma
smd_error <- function(beta, gamma, psi_t, phi_t, C_growth, R, K_psi, K_phi) {
    # Generate B-spline basis
    B_psi <- bs(psi_t, knots = quantile(psi_t, probs = seq(0.25, 0.75, length.out = K_psi - 2)), degree = 3, intercept = TRUE)
    B_phi <- bs(phi_t, knots = quantile(phi_t, probs = seq(0.25, 0.75, length.out = K_phi - 2)), degree = 3, intercept = TRUE)
    
    # Tensor-product basis
    design_matrix <- compute_design_product(B_psi, B_phi)
    
    # Dependent variable
    y <- (1 / beta) * (C_growth^gamma) / R
    
    
    # df_design_matrix <- as.data.frame(design_matrix)

    # Perform OLS
    ols_result <- lm(y ~ design_matrix - 1)  # "-1" excludes intercept since it's part of design_matrix
    
    # Compute residuals
    residuals <- resid(ols_result)
    
    # Return the total squared error
    mean(residuals^2)
}





# Function to estimate Omega given beta and gamma
estimate_omega <- function(beta, gamma, psi_t, phi_t, C_growth, R, K_psi, K_phi) {
    # Generate B-spline basis
    B_psi <- bs(psi_t, knots = quantile(psi_t, probs = seq(0.25, 0.75, length.out = K_psi - 2)), degree = 3, intercept = TRUE)
    B_phi <- bs(phi_t, knots = quantile(phi_t, probs = seq(0.25, 0.75, length.out = K_phi - 2)), degree = 3, intercept = TRUE)
    
    # Tensor-product basis
    design_matrix <- compute_design_product(B_psi, B_phi)
    
    # Dependent variable
    y <- (1 / beta) * (C_growth^gamma) / R
    
    # Perform OLS
    ols_result <- lm(y ~ design_matrix - 1)  # "-1" excludes intercept since it's part of design_matrix
    
    # Return coefficients
    list(coefficients = coef(ols_result), B_psi = B_psi, B_phi = B_phi, design_matrix = design_matrix)
}





# Define the grid for beta and gamma
beta_grid <- seq(0.95, 0.99, by = 0.01)
gamma_grid <- seq(1, 5, by = 0.5)

# Example data
psi_t <- c(0.7, 0.8, 0.75, 0.72)  # Net worth share
phi_t <- c(3, 3.2, 3.1, 3.05)     # Aggregate leverage
C_growth <- c(1.02, 1.01, 1.03, 1.02)  # Consumption growth
R <- c(1.04, 1.03, 1.05, 1.06)         # Returns
K_psi <- 3  # Number of knots for psi_t
K_phi <- 3  # Number of knots for phi_t

# Initialize results
results <- expand.grid(beta = beta_grid, gamma = gamma_grid)
results$error <- NA

# Loop over the grid
for (i in 1:nrow(results)) {
    beta <- results$beta[i]
    gamma <- results$gamma[i]
    
    # Compute SMD error for this pair
    results$error[i] <- smd_error(beta, gamma, psi_t, phi_t, C_growth, R, K_psi, K_phi)
}

# Find the best beta and gamma
optimal <- results[which.min(results$error), ]
print(optimal)


























# 3 Estimation of the HI-SDF Model

In this section, we will estimate the HI-SDF model proposed by Sa Mai (2023). 

First we will define the moment conditions and the sieve estimation of the conditional mean M(w_t).

Step 0: Simulate the data

```{r}

```




Step 1: Load data

```{r}
# Step 1: Estimate \omega Using Sieve Minimum Distance (SMD)

data <- df_equity


# Assign variables
psi <- data$psi_t
phi <- data$phi_t
returns <- data$returns # Example: asset returns
consumption_growth <- data$consumption_growth



# Extract instruments from the data (adjust column names as needed)
instruments <- as.matrix(data[, c("cay_t", "RREL_t", "avg_excess_return", "phi_t")]) 
instruments <- scale(instruments, center = rep(1, 4)) # Standardize the instruments
```


Step 2: Define initial guesses for β and γ

```{r}
# Initial guesses for β and γ
beta <- 0.98 # Time discount factor
gamma <- 15  # Risk aversion
params <- c(beta, gamma) # Combine into a vector
```




Step 3: Setup Sieve Approximation

```{r}
# Define knots for ψ and φ based on quantiles
knots_psi <- quantile(psi, probs = seq(0, 1, length.out = 4))
knots_phi <- quantile(phi, probs = seq(0, 1, length.out = 4))

# Generate B-spline basis functions
bs_psi <- bs(psi, knots = knots_psi, degree = 3, intercept = TRUE)
bs_phi <- bs(phi, knots = knots_phi, degree = 3, intercept = TRUE)

# Efficient computation of the tensor product basis
compute_tensor_product <- function(bs_psi, bs_phi) {
    n <- nrow(bs_psi) # Number of rows in the B-spline basis for ψ
    d_psi <- ncol(bs_psi) # Number of basis functions for ψ
    d_phi <- ncol(bs_phi) # Number of basis functions for φ
    
    # Initialize the tensor product matrix
    tensor_product <- matrix(0, nrow = n, ncol = d_psi * d_phi)
    
    # Compute the tensor product row by row
    for (i in 1:n) {
        tensor_product[i, ] <- as.vector(outer(bs_psi[i, ], bs_phi[i, ]))
    }
    
    return(tensor_product)
}

# Call the function
tensor_product <- compute_tensor_product(bs_psi, bs_phi)


# Function to fit Ω(ψ, φ)
estimate_omega <- function(returns, tensor_product) {
    # Perform cross-validation for ridge regression
    cv_fit <- cv.glmnet(as.matrix(tensor_product),
                        returns,
                        alpha = 0,
                        intercept = FALSE)
    
    # Extract coefficients for the optimal lambda
    coefficients <- as.numeric(coef(cv_fit, s = "lambda.min")[-1]) # Exclude intercept
    
    
    # Check the length of omega_coefficients
    cat("Length of omega_coefficients:",
        length(coefficients),
        "\n")
    cat("Number of columns in tensor_product:",
        ncol(tensor_product),
        "\n")
    
    # Check for NA values in coefficients
    if (any(is.na(coefficients))) {
        stop(
            "NA values detected in omega_coefficients. Check for multicollinearity or missing data."
        )
    }
    
    return(coefficients)
}
```



Step 4: GMM Estimation for β and γ and Ω(ψ, φ)

```{r}
# Compute the SDF
compute_sdf <- function(beta, gamma, consumption_growth, tensor_product, omega_coefficients) {
    # Consumption growth term
    consumption_ratio <- consumption_growth
    sdf <- beta * (consumption_ratio ^ (-gamma)) * (tensor_product %*% omega_coefficients)
    return(as.vector(sdf))
}

# Compute the residuals for GMM
compute_gmm_residuals <- function(params, consumption_growth, returns, tensor_product, omega_coefficients, instruments) {
    beta <- params[1]
    gamma <- params[2]
    
    # Compute SDF
    sdf <- compute_sdf(beta, gamma, consumption_growth, tensor_product, omega_coefficients)
    
    # Compute residuals based on Euler equation
    residuals <- (sdf * returns - 1) %*% instruments
    
    
    # Debugging: Check residual statistics
    apply(residuals, 2, summary)  # Summarize each moment
    
    
    # Normalize residuals to prevent dominance
    residuals <- residuals / max(abs(residuals))
    
    return(residuals)
}


# GMM objective function
gmm_objective <- function(params, consumption_growth, returns, tensor_product, omega_coefficients, instruments, ...) {
    
    # Compute residuals
    residuals <- compute_gmm_residuals(params, consumption_growth, returns, tensor_product, omega_coefficients, instruments)
    
    # GMM objective: Minimize the sum of squared residuals
    return(sum(residuals^2))
}

gmm_objective_debug <- function(params, consumption_growth, returns, tensor_product, omega_coefficients, instruments) {
    obj_val <- gmm_objective(params, consumption_growth, returns, tensor_product, omega_coefficients, instruments)
    cat("Objective Function Value:", obj_val, "\n")
    return(obj_val)
}

```



Step 5: Iterative Procedure

```{r}
# Tolerance and max iterations
tolerance <- 1e-8
max_iterations <- 100
iteration <- 0
converged <- FALSE

while (!converged && iteration < max_iterations) {
    iteration <- iteration + 1
    
    print(iteration)
    
    
    # Step 1: Estimate Ω(ψ, φ)
    omega_coefficients <- estimate_omega(returns, tensor_product)
    
    print(cat("Omega Coeffs for tensor product:", omega_coefficients, "\n"))
    
    # Step 2: Update β and γ using GMM
    opt <- optim(
        par = params,
        fn = gmm_objective,
        consumption_growth = consumption_growth,
        returns = returns,
        tensor_product = tensor_product,
        omega_coefficients = omega_coefficients,
        instruments = instruments,
        method = "BFGS",
        control = list(trace = 1, REPORT = 1) 
    )
    
    # Check convergence
    params_new <- opt$par
    cat("New Params (Iteration):", params_new, "\n")  # Debug line
    cat("Param Change:", max(abs(params_new - params)), "\n")
    if (max(abs(params_new - params)) < tolerance) {
        converged <- TRUE
    }
    
    params <- params_new # Update parameters
    cat("Iteration:", iteration, "Beta:", params[1], "Gamma:", params[2], "\n")
}

# Final estimates
beta_final <- params[1]
gamma_final <- params[2]
omega_final <- omega_coefficients

```




##################

library(splines)
library(np)

# Step 1: Define Knots for Psi and Phi
define_knots <- function(psi_t, phi_t, K_psi, K_phi) {
    knots_psi <- quantile(psi_t, probs = seq(0.25, 0.75, length.out = K_psi - 2))
    knots_phi <- quantile(phi_t, probs = seq(0.25, 0.75, length.out = K_phi - 2))
    return(list(knots_psi = knots_psi, knots_phi = knots_phi))
}

# Step 2: Generate B-spline Basis Functions
generate_bsplines <- function(psi_t, phi_t, knots_psi, knots_phi) {
    B_psi <- bs(psi_t, knots = knots_psi, degree = 3, intercept = TRUE)
    B_phi <- bs(phi_t, knots = knots_phi, degree = 3, intercept = TRUE)
    return(list(B_psi = B_psi, B_phi = B_phi))
}

# Step 3: Tensor Product of B-splines
tensor_product_basis <- function(B_psi, B_phi) {
    n <- nrow(B_psi) # Number of rows in the B-spline basis for ψ
    d_psi <- ncol(B_psi) # Number of basis functions for ψ
    d_phi <- ncol(B_phi) # Number of basis functions for φ
    
    # Initialize the tensor product matrix
    tensor_product <- matrix(0, nrow = n, ncol = d_psi * d_phi)
    
    # Compute the tensor product row by row
    for (i in 1:n) {
        tensor_product[i, ] <- as.vector(outer(B_psi[i, ], B_phi[i, ]))
    }
    
    return(tensor_product)
}

# Step 4.1: Estimate M_hat(w_t, nu, Omega)
estimate_conditional_moment <- function(chi_values, w_t_basis) {
    # Compute (P'P)^(-1)
    P <- as.matrix(w_t_basis)  # Basis matrix for instruments
    P_P_inverse <- solve(t(P) %*% P)  # Compute (P'P)^(-1)
    
    # Compute P' * chi
    P_chi <- t(P) %*% chi_values  # Weighted sum of chi with basis functions
    
    # Compute M_hat
    M_hat <- P %*% (P_P_inverse %*% P_chi)
    return(M_hat)
}

# Step 4.2: Define Chi(z_{t+1}, nu, Omega)
compute_chi <- function(z_t, Omega, R, beta, gamma, C_growth) {
    # Compute the moment function
    chi <- Omega * R - (1 / beta) * (C_growth^gamma)
    return(chi)
}


sieve_least_squares <- function(M_hat, design_matrix, weights) {
    # Perform weighted least squares regression
    ols_result <- lm(M_hat ~ design_matrix - 1, weights = weights)
    return(list(model = ols_result, coefficients = coef(ols_result)))
}


# Step 5: Grid Search over Beta and Gamma
grid_search_smd <- function(beta_grid, gamma_grid, psi_t, phi_t, z_t, w_t, K_psi, K_phi) {
    results <- expand.grid(beta = beta_grid, gamma = gamma_grid)
    results$error <- NA
    
    for (i in 1:nrow(results)) {
        beta <- results$beta[i]
        gamma <- results$gamma[i]
        
        # Estimate conditional moment
        cond_moment <- estimate_conditional_moment(z_t, w_t)
        
        # Define weights (uniform weights for simplicity)
        weights <- rep(1, length(cond_moment))
        
        # Generate B-splines and tensor product
        knots <- define_knots(psi_t, phi_t, K_psi, K_phi)
        basis <- generate_bsplines(psi_t, phi_t, knots$knots_psi, knots$knots_phi)
        design_matrix <- tensor_product_basis(basis$B_psi, basis$B_phi)
        
        # Perform Sieve Least Squares
        slse_result <- sieve_least_squares(cond_moment, design_matrix, weights)
        
        # Compute error
        residuals <- residuals(slse_result$model)
        results$error[i] <- mean(residuals^2)
    }
    
    # Return the optimal beta and gamma
    optimal <- results[which.min(results$error), ]
    return(list(optimal = optimal, results = results))
}

# Example Data
set.seed(123)
psi_t <- runif(101, 0.6, 0.9)  # Net worth share
phi_t <- runif(101, 2.5, 4)    # Aggregate leverage
z_t <- rnorm(100)              # Observations (e.g., returns or consumption growth)
w_t <- cbind(lag(psi_t), lag(phi_t))  # Instruments (lagged state variables)
psi_t <- psi_t[-1]  # Remove first element to match dimensions
phi_t <- phi_t[-1]  # Remove first element to match dimensions
w_t <- w_t[-1, ]    # Remove first row to match dimensions
K_psi <- 3
K_phi <- 3
beta_grid <- seq(0.95, 0.99, by = 0.01)
gamma_grid <- seq(1, 5, by = 0.5)

# Perform Grid Search
smd_results <- grid_search_smd(beta_grid, gamma_grid, psi_t, phi_t, z_t, w_t, K_psi, K_phi)

# Extract Optimal Beta and Gamma
optimal_beta <- smd_results$optimal$beta
optimal_gamma <- smd_results$optimal$gamma
print(smd_results$optimal)


#################

data <- df_equity

psi <- data$psi_t
phi <- data$phi_t


# Step 1 Define knots

# Define quantile-based knots for psi and phi
num_knots <- 5  # Number of knots
knots_psi <- quantile(psi, probs = seq(0, 1, length.out = num_knots))
knots_phi <- quantile(phi, probs = seq(0, 1, length.out = num_knots))

library(splines)

# Create B-spline basis functions for psi and phi
b_spline_psi <- bs(psi, knots = knots_psi[-c(1, length(knots_psi))], degree = 3, intercept = TRUE)
b_spline_phi <- bs(phi, knots = knots_phi[-c(1, length(knots_phi))], degree = 3, intercept = TRUE)



# Compute tensor product of the B-spline basis functions
tensor_product <- tensor_product_basis(b_spline_psi, b_spline_phi)

# Print dimensions and first few rows
cat("Dimensions of the tensor product:", dim(tensor_product), "\n")



# Function to compute chi(z_{t+1}, nu, Omega)
compute_chi <- function(data, beta, gamma, tensor_product) {
    # data: Data frame or matrix with z_{t+1} (columns: consumption growth, returns, etc.)
    # beta, gamma: Parameters
    # tensor_product: Estimated tensor product basis for Omega(psi, phi)
    
    # Extract variables from data
    C_growth <- data$consumption_growth # C_{t+1}/C_t
    returns <- data$returns             # R_{t+1}
    
    # Compute the pricing kernel
    m_t <- beta * (C_growth^(-gamma)) * tensor_product  # M_t
    
    # Compute chi
    chi_values <- m_t * returns - 1
    
    return(chi_values)
}


# Step 4: Sieve Least Squares Estimation

sieve_least_squares <- function(tensor_product, chi_values) {
    # tensor_product: Tensor product of B-spline basis functions (from Step 3)
    # chi_values: Vector of observed values of chi(z_{t+1}, nu, Omega)
    
    # Compute the design matrix P'P
    P <- tensor_product
    PP_inv <- solve(t(P) %*% P)  # Inverse of P'P
    
    # Compute the coefficients (P'P)^(-1) P' * chi
    coefficients <- PP_inv %*% t(P) %*% chi_values
    
    # Estimated function M_hat(w_t, nu, Omega)
    M_hat <- P %*% coefficients  # Fitted values
    
    return(list(M_hat = M_hat, coefficients = coefficients))
}

# Example usage:
# Simulated chi values (replace this with actual data)
set.seed(123)
chi_values <- rnorm(nrow(tensor_product))  # Observed chi(z_{t+1}, nu, Omega)

# Perform sieve least squares estimation
result <- sieve_least_squares(tensor_product, chi_values)

# Extract the estimated M_hat
M_hat <- result$M_hat

# Print results
cat("First few estimated M_hat values:\n")
head(M_hat)
