# Simulate data
set.seed(123)
T <- 100  # Number of observations
df_equity <- data.frame(
  psi_t = runif(T, 0, 1),  # Simulated psi_t values
  phi_t = runif(T, 0, 1),  # Simulated phi_t values
  consumption_growth = rnorm(T, mean = 1, sd = 0.1),  # Simulated consumption growth
  returns = 1 + 0.05 * runif(T, 0, 1) + 0.1 * runif(T, 0, 1) + rnorm(T, 0, 0.01)  # Simulated returns
)
psi_t <- df_equity$psi_t
phi_t <- df_equity$phi_t
C_t <- df_equity$consumption_growth
R_t <- df_equity$returns

# Basis functions (e.g., polynomial)
P <- poly(cbind(psi_t, phi_t), degree = 3, raw = TRUE)  # Design matrix
# Ensure consistency in dimensions
P_t <- as.matrix(P)[-1, ]  # Remove the first row to match length of kappa
P_t_cross <- t(P_t) %*% P_t  # P'P

# Moment conditions (kappa)
beta <- 0.99
gamma <- 2
kappa <- beta * (C_t[-1] / C_t[-length(C_t)])^(-gamma) * R_t[-1] - 1

kappa <- beta * (C_t[-1])^(-gamma) * R_t[-1] - 1

# Sieve estimation of conditional mean M(w_t)
M_w <- t(P_t) %*% kappa  # Project moment condition onto basis
M_est <- solve(P_t_cross) %*% M_w  # Sieve least squares

# GMM Estimation
W <- solve(P_t_cross)  # Weighting matrix based on (P'P)

objective <- function(a) {
  residuals <- kappa - P_t %*% a
  projected_residuals <- t(residuals) %*% P_t  # Project residuals
  return(as.numeric(projected_residuals %*% W %*% t(projected_residuals)))  # Weighted norm
}
a_est <- optim(rep(0, ncol(P_t)), objective)$par  # Minimize the norm

# Final estimated coefficients of Omega
cat("Estimated Coefficients for Omega:\n", a_est, "\n")


# Reconstruct Omega
Omega <- function(psi, phi) {
    B <- predict(poly(cbind(psi, phi), degree = 3, raw = TRUE))  # Generate basis functions
    Omega_value <- sum(a_est * B)  # Combine coefficients and basis functions
    return(Omega_value)
}

# Example: Compute Omega for specific values
psi_new <- 0.5  # Example psi_t value
phi_new <- 0.8  # Example phi_t value

Omega_value <- Omega(psi_new, phi_new)
cat("Reconstructed Omega for psi =", psi_new, "and phi =", phi_new, "is:", Omega_value, "\n")



# Create a grid of psi and phi
psi_grid <- seq(0, 1, length.out = 100)
phi_grid <- seq(0, 1, length.out = 100)
Omega_grid <- outer(psi_grid, phi_grid, Vectorize(Omega))  # Compute Omega for each grid point

# Plot the surface
library(plotly)
plot_ly(x = psi_grid, y = phi_grid, z = Omega_grid) %>%
    add_surface() %>%
    layout(
        title = "Reconstructed Omega(psi, phi)",
        scene = list(
            xaxis = list(title = "psi"),
            yaxis = list(title = "phi"),
            zaxis = list(title = "Omega")
        )
    )





# Semiparametric Minimum Distance Estimation ####

#### One Regressor ####


# Simulated data for demonstration
set.seed(123)
n <- 1000
z <- rnorm(n)  # Instrumental variable
x <- z + rnorm(n)  # Endogenous regressor
y <- 2 * x^2 + rnorm(n)  # Structural relationship

# First Stage: Estimate E[X|Z] nonparametrically using a kernel smoother
kernel_smooth <- function(z_eval, z, x, bandwidth = 0.5) {
    weights <- dnorm((z - z_eval) / bandwidth)
    sum(weights * x) / sum(weights)
}

x_hat <- sapply(z, kernel_smooth, z = z, x = x)

# Second Stage: Estimate g(x) using series approximation (polynomial basis)
poly_basis <- function(x, degree = 3) {
    sapply(0:degree, function(d) x^d)
}

basis_matrix <- poly_basis(x_hat, degree = 3)
coefficients <- solve(t(basis_matrix) %*% basis_matrix) %*% t(basis_matrix) %*% y

# Structural Function Estimate
g_hat <- function(x_new) {
    new_basis <- poly_basis(x_new, degree = 3)
    as.vector(new_basis %*% coefficients)
}

# Testing the structural function
x_test <- seq(min(x), max(x), length.out = 100)
y_test <- g_hat(x_test)

# Plot results
plot(x, y, main = "SMD Estimation of Structural Function", pch = 16, col = "grey")
lines(x_test, y_test, col = "blue", lwd = 2)
legend("topleft", legend = "Estimated Function", col = "blue", lty = 1, lwd = 2)



#### Two Regressors ####

# Simulated data for two-parameter estimation
set.seed(123)
n <- 1000
z1 <- rnorm(n)  # First instrumental variable
z2 <- rnorm(n)  # Second instrumental variable
x1 <- z1 + rnorm(n)  # First endogenous regressor
x2 <- z2 + rnorm(n)  # Second endogenous regressor
y <- 3 * x1^2 + 2 * x2 + rnorm(n)  # Structural relationship

# First Stage: Estimate E[X1|Z] and E[X2|Z] nonparametrically
kernel_smooth <- function(z_eval, z, x, bandwidth = 0.5) {
    weights <- dnorm((z - z_eval) / bandwidth)
    sum(weights * x) / sum(weights)
}

x1_hat <- sapply(z1, kernel_smooth, z = z1, x = x1)
x2_hat <- sapply(z2, kernel_smooth, z = z2, x = x2)

# Second Stage: Estimate g(x1, x2) using series approximation (polynomial basis)
poly_basis_2d <- function(x1, x2, degree = 2) {
    terms <- expand.grid(0:degree, 0:degree)
    terms <- terms[rowSums(terms) <= degree, ]
    apply(terms, 1, function(p) x1^p[1] * x2^p[2])
}



# Transpose of poly_basis_2d is the basis matrix
basis_matrix <- t(poly_basis_2d(x1_hat, x2_hat, degree = 2))
basis_matrix_t <- t(basis_matrix)  # Transpose of the basis matrix

# Regularization parameter
lambda <- 1e-4  # Regularization term

# Debugging dimensions before constructing ridge_matrix
cat("Dimensions of basis_matrix:", dim(basis_matrix), "\n")
cat("Dimensions of basis_matrix_t:", dim(basis_matrix_t), "\n")

# Ridge regularization: Construct the regularized matrix
ridge_matrix <- basis_matrix %*% basis_matrix_t + lambda * diag(nrow(basis_matrix))



# Debugging dimensions after ridge_matrix
cat("Dimensions of ridge_matrix:", dim(ridge_matrix), "\n")

coefficients <- coefficients <- solve(ridge_matrix, basis_matrix %*% y)


# Structural Function Estimate
g_hat <- function(x1_new, x2_new) {
    new_basis <- t(poly_basis_2d(x1_new, x2_new, degree = 2))
    as.vector(new_basis %*% coefficients)
}

# Testing the structural function
x1_test <- seq(min(x1), max(x1), length.out = 100)
x2_test <- seq(min(x2), max(x2), length.out = 100)
grid <- expand.grid(x1_test, x2_test)
new_basis <- t(poly_basis_2d(x1_test, x2_test, degree = 2))
y_test <- g_hat(grid[, 1], grid[, 2])



# Plot results (slice of the structural function for visualization)
library(lattice)
wireframe(matrix(y_test, nrow = 100), row.values = x1_test, column.values = x2_test,
          xlab = "X1", ylab = "X2", zlab = "g(X1, X2)", 
          main = "SMD Estimation of Structural Function")




























#### ------------------------

# Load required libraries
library(splines2)
library(dplyr)

# Step 1: Data Preparation
# Assuming `data` is the dataset with columns: psi_t, phi_t, consumption_growth, returns
prepare_data <- function(data) {
    list(
        psi_t = data$psi_t,
        phi_t = data$phi_t,
        consumption_growth = data$consumption_growth,
        returns = data$returns
    )
}

# Step 2: Generate Tensor-Product Cubic Splines
generate_basis <- function(psi, phi, n_knots = 5) {
    # Ensure psi and phi have enough unique values for spline generation
    if (length(unique(psi)) < n_knots || length(unique(phi)) < n_knots) {
        stop("Not enough unique values in psi or phi for the specified number of knots.")
    }
    
    # Define knots explicitly
    psi_knots <- quantile(psi, probs = seq(0.2, 0.8, length.out = n_knots))
    phi_knots <- quantile(phi, probs = seq(0.2, 0.8, length.out = n_knots))
    
    # Set boundary knots explicitly
    psi_boundary <- range(psi)
    phi_boundary <- range(phi)
    
    # Generate splines
    psi_basis <- bSpline(psi, knots = psi_knots, degree = 3, Boundary.knots = psi_boundary)
    phi_basis <- bSpline(phi, knots = phi_knots, degree = 3, Boundary.knots = phi_boundary)
    
    # Create tensor-product basis
    tensor_basis <- model.matrix(~ psi_basis * phi_basis - 1)
    return(tensor_basis)
}


# Step 3: Moment Conditions
compute_moment_conditions <- function(beta, gamma, consumption_growth, returns, basis) {
    kappa <- beta * (consumption_growth[-1] / consumption_growth[-length(consumption_growth)])^(-gamma) * returns[-1] - 1
    list(kappa = kappa, basis = basis[-1, ])  # Match dimensions with kappa
}

# Step 4: GMM Objective Function
gmm_objective <- function(alpha, kappa, basis, W) {
    residuals <- kappa - basis %*% alpha
    norm <- t(residuals) %*% W %*% residuals
    return(as.numeric(norm))
}

# Step 5: GMM Estimation with Iterative Weighting
estimate_omega <- function(moment_data, max_iter = 5) {
    kappa <- moment_data$kappa
    basis <- moment_data$basis
    
    # Ensure kappa and basis have compatible dimensions
    if (length(kappa) != nrow(basis)) {
        stop("Dimensions of kappa and basis are not compatible.")
    }
    
    P_t_cross <- t(basis) %*% basis
    W <- solve(P_t_cross)  # Initial weighting matrix
    
    alpha <- rep(0, ncol(basis))  # Initialize coefficients
    
    for (i in 1:max_iter) {
        # Define GMM Objective Function
        gmm_objective <- function(alpha) {
            residuals <- kappa - basis %*% alpha
            projected_residuals <- t(basis) %*% residuals
            return(as.numeric(t(projected_residuals) %*% W %*% projected_residuals))
        }
        
        # Optimize to estimate alpha
        result <- optim(alpha, gmm_objective, method = "BFGS")
        alpha <- result$par
        
        # Update Residuals and Weighting Matrix
        residuals <- kappa - basis %*% alpha
        W <- solve(t(basis) %*% (residuals %*% t(residuals)) %*% basis + diag(1e-6, ncol(basis)))  # Regularized
    }
    
    return(list(alpha = alpha, objective = result$value))
}


# Step 6: Reconstruct Omega
omega_function <- function(psi, phi, alpha, basis_func) {
    # Generate basis for new psi and phi
    basis <- basis_func(psi, phi)
    omega_value <- basis %*% alpha
    return(as.numeric(omega_value))
}

# Step 7: Visualization of Omega
visualize_omega <- function(alpha, psi_range, phi_range, basis_func) {
    # Create a grid of psi and phi
    psi_grid <- seq(min(psi_range), max(psi_range), length.out = 100)
    phi_grid <- seq(min(phi_range), max(phi_range), length.out = 100)
    
    # Compute Omega for each grid point
    Omega_grid <- outer(
        psi_grid, phi_grid,
        Vectorize(function(psi, phi) {
            omega_function(psi, phi, alpha, basis_func)
        })
    )
    
    # Plot the surface
    plot_ly(x = psi_grid, y = phi_grid, z = Omega_grid) %>%
        add_surface() %>%
        layout(
            title = "Reconstructed Omega(psi, phi)",
            scene = list(
                xaxis = list(title = "psi"),
                yaxis = list(title = "phi"),
                zaxis = list(title = "Omega")
            )
        )
}




# Example Usage
# Load your dataset (replace `data` with your actual dataset)
data <- tibble(
    psi_t = runif(100, 0, 1),
    phi_t = runif(100, 0, 1),
    consumption_growth = rnorm(100, mean = 1, sd = 0.1),
    returns = 1 + 0.05 * runif(100, 0, 1) + 0.1 * runif(100, 0, 1) + rnorm(100, 0, 0.01)
)

# Prepare data
prepared_data <- prepare_data(data)

# Generate basis functions
basis <- generate_basis(prepared_data$psi_t, prepared_data$phi_t)

# Compute moment conditions
moment_data <- compute_moment_conditions(
    beta = 0.99,
    gamma = 2,
    consumption_growth = prepared_data$consumption_growth,
    returns = prepared_data$returns,
    basis = basis
)

# Estimate Omega
omega_est <- estimate_omega(moment_data)

# Visualize Omega
visualize_omega(
    alpha = omega_est$alpha,
    psi_range = prepared_data$psi_t,
    phi_range = prepared_data$phi_t,
    basis_func = function(psi, phi) generate_basis(psi, phi)
)







#### -------------------





# Leverage Ratio plot
leverage_ratio_plot %>% 
    ggplot(aes(x = Quarter, y = Value, color = Institution)) +
    geom_line() +
    # Put the values on different axes to make them better comparable
    facet_wrap(~Institution, scales = "free_y") +
    labs(
         x = "Quarter",
         y = "Log-Leverage Ratio") +
    theme_bw() +
    theme(legend.position = "", 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14))





# Plot the data
networth_share_full_table_long %>% 
    ggplot(aes(x = Quarter, y = Value, color = Institution)) +
    geom_line() +
    # Put the values on different axes to make them better comparable
    facet_wrap(~Institution, scales = "free_y") +
    labs(
         x = "Quarter",
         y = "Value") +
    theme_bw() +
    theme(legend.position = "", 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14))
