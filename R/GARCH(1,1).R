#GARCH(1,1) analysis with t-distribution

#includes options for robustness checks
#QMLE GARCH via dist argument
#asymmetric via model argument

garch_m <- function(data, fx, home_int, US_int, dist = "std", model = "sGARCH"){

  #calculate log returns of exchange rate
  data <- data %>% 
    mutate(log_returns = log(lead({{fx}}, 1)) - log({{fx}}))
  data <- data %>% 
    mutate(log_returns = log_returns*100)
  
  #calculate interest rate differential equivalently to UIP_regression.R
  data <- data %>% mutate(
    days_gap = as.numeric(difftime(lead(Date, 1) , Date, units ="days")),
    interest_rate_differential = {{home_int}} - {{US_int}},
    interest_rate_differential_daily = interest_rate_differential*days_gap/365) %>% 
    na.omit()
  
  
  spec <- ugarchspec(
    #ugarchspec() defines structure of model without calculating it directly
    #ugarchspec() has the following three arguments to specify the garch model
    variance.model = list(model = model, garchOrder = c(1,1)),#sGarch is symmetric garch model and used as default
    mean.model = list(armaOrder = c(0,0), include.mean = TRUE, #TRUE includes intercept
                      archm = TRUE, archpow = 1, 
                      #archm TRUE includes conditional variance estimate in mean equation
                      #archpow gives power for conditional variance estimate in mean equation
                      external.regressors = matrix(data$interest_rate_differential_daily)),
    distribution.model = dist #default is student t dist
  )
  
  garch_estimation <- ugarchfit( #ugarchfit runs the joint MLE
    spec = spec,
    data = data$log_returns,
    solver = "hybrid"#uses multiple optimizers for estimation
  )
  
  #prepare variables for output
  #coefficients with different SEs
  conv_se <- garch_estimation@fit$matcoef #coef with conventional SEs from Hessian
  robust_se <- garch_estimation@fit$robust.matcoef #coef with robust SEs from Bollerslev-Wooldridge
  #check persisitence in var equation, i.e. check stationarity
  persistence <- coef(garch_estimation)["alpha1"] + coef(garch_estimation)["beta1"] 
  persistence_package <- rugarch::persistence(garch_estimation) #directly calculates persistence via packag
  #needed for GJR Garch, because persistence is given by alpha+beta+gamma*0.5
  
  #Wald test for no risk premium at all
  theta <- coef(garch_estimation)#extract estimated parameters
  #mu=alpha_mean, mxreg1 = beta_mean, archm = gamma_mean, omega = alpha_0_var, alpha_1= alpha_1_var, beta_1 = beta_1_var, shape = v 
  V <- garch_estimation@fit$robust.cvar #bollerslev wooldrige cov matrix
  R <- matrix(0, nrow = 2, ncol = length(theta), dimnames = list(NULL, names(theta)))
  #R is a matrix of zeros with 2 rows and as many columns as parameters
  R[1, "mu"] <- 1 #give mu (i.e. alpha) weigh 1
  R[2, "archm"] <- 1 #give archm (i.e. gamma) weigh 1
  r <- c(0,0) # null of alpha and gamma equal 0
  #calculate wald test for no risk premium at all (alpha=gamma=0) via package 
  wald_result <- wald.test(Sigma = V, b = theta, L = R, H0 = r)
  #Sigma needs covariance matrix; b needs coef vector; L is restriction matrix that selects which coef are tested; H0 needs H0 values
  #H0 must have the same length as L rows
  
  #Wald test for beta=1
  R_beta <- matrix(0, nrow = 1, ncol = length(theta), dimnames = list(NULL, names(theta)))
  R_beta[1, "mxreg1"] <- 1 #give mxreg1 (i.e. beta) weigh 1
  #use wald test function to test beta=1
  wald_beta <- wald.test(Sigma =V, b = theta, L = R_beta, H0= 1)
  
  #pearson goodness of fit chi^2 test on standardized residuals to check whether t distribution
  #significant pval means t dist assumption is rejected
  gof_test <- gof(garch_estimation, groups = c(20, 30, 40, 50))
  #convergence indicator of estimation; should be 0
  convergence <- garch_estimation@fit$convergence
  #maximized log likelihood
  loglikelihood <- likelihood(garch_estimation)
  #information criterion with Akaike, Bayes, Shibata, Hannan-Quinn
  infocrit <- infocriteria(garch_estimation)
  
  
  return(list(
    conventional_se = conv_se,
    robust_se = robust_se,
    persistence = persistence,
    persistence_package = persistence_package,
    wald_no_premium = wald_result$result$chi2,  
    wald_beta = wald_beta$result$chi2,
    gof = gof_test,
    convergence = convergence,
    loglikelihood = loglikelihood,
    AIC = infocrit["Akaike", 1],
    BIC = infocrit["Bayes",  1],
    garch_model = garch_estimation
    ))
}
  
