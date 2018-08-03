set.seed(7882)
cpu_threads <- 12 # Windows users set this to 1
library(SelectionSimulator)

logistic_simulation <- function(cov_mat, coefficients, intercept,
                        num_observations, num_simulations, file_name,
                        cpu_cores) {

  file_path <- paste0("./", file_name)

  num_predictors <- length(coefficients)

  constructors <- c(Step = purrr::partial(SelectionSimulator::StepLmWrapper$new, glm_family = binomial),
                    Step2 = purrr::partial(SelectionSimulator::Step2$new, glm_family = binomial),
                    Step3 = purrr::partial(SelectionSimulator::Step3$new, glm_family = binomial))


  predict_gen <- SelectionSimulator::NormalPredictorsGenerator$new(
    num_observations,
    num_predictors,
    norm_rand_var_sd = 1,
    covariance_matrix = cov_mat,
    predictor_names = names(coefficients))

  response_calc <- SelectionSimulator::LogisticResponseCalculator$new(
    coefficients = coefficients,
    intercept = intercept)

  sim_data_gen <- SelectionSimulator::SimulationDataGenerator$
    new(predict_gen, response_calc)

  test_obj <- SelectionSimulator::Simulation$new(sim_data_gen, constructors,
                                                 num_simulations, min(cpu_cores, num_simulations))
  test_obj$simulate()
  saveRDS(test_obj, file_path, compress = "bzip2")
  return(test_obj)
}


# Model 4 : Logistic
cov_mat_threeNeg <- Matrix::diag(replicate(10,1))
cov_mat_threeNeg[[1,2]] <- cov_mat_threeNeg[[2,1]] <- -0.8
cov_mat_threeNeg[[3,2]] <- cov_mat_threeNeg[[2,3]] <- -0.75
cov_mat_threeNeg[[3,1]] <- cov_mat_threeNeg[[1,3]] <- 0.25


coeffs <- c(X1 = 1, X2 = 1, X3 = 1, X4 = 0, X5 = 0,
            X6 = 0, X7 = 0, X8 = 0, X9 = 0, X10 = 0)

logistic <- logistic_simulation(cov_mat_threeNeg,
                        coefficients = coeffs, intercept = 1,
                        num_observations = 100, num_simulations = 1000,
                        file_name = "logistic.RDS", cpu_threads)

