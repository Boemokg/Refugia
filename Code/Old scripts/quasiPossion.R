(chat <- deviance(Full_mod.1) / df.residual(Full_mod.1))
dredge(Full_mod.1, rank = "QAIC", chat = chat)
dredge(Full_mod.1, rank = "AIC")


if (FALSE) {
  # A 'hacked' constructor for quasibinomial family object that allows for
  # ML estimation
  hacked.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  QAIC(update(Het.m, family = hacked.quasipoisson), chat = chat)
}


if (FALSE) {
  # A 'hacked' constructor for quasibinomial family object that allows for
  # ML estimation
  hacked.quasipoisson <- function(...) {
    res <- quasipoisson(...)
    res$aic <- poisson(...)$aic
    res
  }
  
  # Fit the model with the hacked quasipoisson family
  hacked_model <- update(Full_mod.quasi, family = hacked.quasipoisson)
  
  # Calculate the maximum log-likelihood
  max_log_likelihood <- logLik(Full_mod.1_Spatial)
  
  # Calculate the number of parameters (k)
  num_parameters <- length(coef(Full_mod.1_Spatial))
  
  # Calculate QAIC
  QAIC_value <- AIC(Full_mod.1_Spatial, k = num_parameters) - 2 * max_log_likelihood
  
  # Calculate weights
  weights <- exp(-0.5 * (QAIC_value - min(QAIC_value)))
  
  # Print or use the results as needed
  cat("Maximum Log-Likelihood:", max_log_likelihood, "\n")
  cat("Number of Parameters (k):", num_parameters, "\n")
  cat("QAIC:", QAIC_value, "\n")
  cat("Weights:", weights, "\n")
}







options(na.action = "na.fail")

# Based on "example(predict.glm)", with one number changed to create
# overdispersion


(chat <- deviance(Full_mod.quasi) / df.residual(Full_mod.quasi))

dredge(Full_mod.quasi, rank = "QAIC", chat = chat)
dredge(Full_mod.quasi, rank = "AIC")


## Not run: 
# A 'hacked' constructor for quasibinomial family object that allows for
# ML estimation
hacked.quasibinomial <- function(...) {
  res <- quasibinomial(...)
  res$aic <- binomial(...)$aic
  res
}
QAIC(update(budworm.lg, family = hacked.quasibinomial), chat = chat)

## End(Not run)