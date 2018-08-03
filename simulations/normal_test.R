set.seed(7882)
cpu_threads <- 12 # Windows users set this to 1
library(SelectionSimulator)

normal_test <- function(cov_mat, coefficients, intercept,
                        num_observations, num_simulations, file_name,
                        cpu_cores) {

  file_path <- paste0("./", file_name)

  num_predictors <- length(coefficients)

  constructors <- c(Step = SelectionSimulator::StepLmWrapper$new,
                    Step2 = SelectionSimulator::Step2$new,
                    Step3 = SelectionSimulator::Step3$new)

  predict_gen <- SelectionSimulator::NormalPredictorsGenerator$new(
    num_observations,
    num_predictors,
    norm_rand_var_sd = 1,
    covariance_matrix = cov_mat,
    predictor_names = names(coefficients))

  response_calc <- SelectionSimulator::LinearNormalResponseCalculator$new(
    norm_rand_var_sd = 1,
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




# Model 1 : Two Negatively Correlated
cov_mat_twoNeg <- rbind(c( 1.00,  -0.8,  0.00, 0.00, 0.00, 0.00),
                        c( -0.8,  1.00,  0.00, 0.00, 0.00, 0.00),
                        c( 0.00,  0.00,  1.00, 0.00, 0.00, 0.00),
                        c( 0.00,  0.00,  0.00, 1.00, 0.00, 0.00),
                        c( 0.00,  0.00,  0.00, 0.00, 1.00, 0.00),
                        c( 0.00,  0.00,  0.00, 0.00, 0.00, 1.00))

coefficients_twoNeg <- c(X1 = 1, X2 = 1, X3 = 0, X4 = 0, X5 = 0, X6 = 0)

test_twoNeg <- normal_test(cov_mat_twoNeg, coefficients_twoNeg, intercept = 9,
                           num_observations = 100, num_simulations = 1000,
                           file_name = "twoNeg.RDS", cpu_threads)


# Model 2 : Three Negatively Correlated
cov_mat_threeNeg <- Matrix::diag(replicate(10,1))
cov_mat_threeNeg[[1,2]] <- cov_mat_threeNeg[[2,1]] <- -0.8
cov_mat_threeNeg[[3,2]] <- cov_mat_threeNeg[[2,3]] <- -0.75
cov_mat_threeNeg[[3,1]] <- cov_mat_threeNeg[[1,3]] <- 0.25

threeNeg_coeff <- c(X1 = 1, X2 = 1, X3 = 1, X4 = 0, X5 =0,
                    X6 = 0, X7 =0, X8 = 0, X9 = 0, X10 = 0)

threeNeg <- normal_test(cov_mat_threeNeg,
                        coefficients = threeNeg_coeff, intercept = 9,
                        num_observations = 100, num_simulations = 1000,
                        file_name = "threeNeg.RDS", cpu_threads)


# Model 3 : Big P
top_left <- rbind( c(1.00, -0.80,  0.25, 0.00, 0.00),
                   c(-0.80,  1.00, -0.5, 0.00, 0.00),
                   c( 0.25, -0.5,  1.00, 0.00, 0.00),
                   c( 0.00,  0.00,  0.00, 1.00, -0.75),
                   c( 0.00,  0.00,  0.00, -0.75, 1.00))

cov_mat <- as.matrix(Matrix::bdiag(top_left, diag(replicate(95,1))))

coefficients <- c(replicate(5,1), replicate(95, 0))
names(coefficients) <- sapply(1:100, function(i) paste0("X", i))

big_p <- normal_test(cov_mat, coefficients, intercept = 9,
                     num_observations = 50, num_simulations = 1000,
                     file_name = "big_p.RDS", cpu_threads)
