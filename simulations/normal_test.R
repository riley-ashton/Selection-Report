normal_test <- function(cov_mat, coefficients, intercept, 
                        num_observations, num_simulations, file_name) {
  
  file_path <- paste0("./simulations/", file_name)
  
  if(file.exists(file_path)) {
    return(readRDS(file_path))
  } else {
    num_predictors <- length(coefficients)
    
    constructors <- c(Step = SelectionSimulator::StepLmWrapper$new,
                      Step2 = SelectionSimulator::Step2$new,
                      Step3 = SelectionSimulator::Step3$new)
    
    predict_gen <- SelectionSimulator::NormalPredictorsGenerator$new(
      num_observations,
      num_predictors,
      norm_rand_var_sd = 1,
      covariance_matrix = cov_mat)
    
    response_calc <- SelectionSimulator::LinearNormalResponseCalculator$new(
      norm_rand_var_sd = 1,
      coefficients = coefficients,
      intercept = intercept)
    
    sim_data_gen <- SelectionSimulator::SimulationDataGenerator$
      new(predict_gen, response_calc)
    
    test_obj <- SelectionSimulator::Simulation$new(sim_data_gen, constructors,
                                                   num_simulations, 1)
    test_obj$simulate()
    saveRDS(test_obj, file_path, compress = "bzip2")
    return(test_obj)
  }
}