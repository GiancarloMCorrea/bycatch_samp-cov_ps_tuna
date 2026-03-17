
# -------------------------------------------------------------------------
run_sdmTMB_model = function(sp_data, this_formula, sp_mesh,  
                     this_model_folder = NULL, this_plot_folder = NULL, this_sp,
                     predDat, effDat, yr_keep, qt_keep, MyGrid = NULL,
                     this_cat = 1, save_model = TRUE, 
                     save_results = TRUE, check_residuals = TRUE) {
  # this_cat is initial model category
  
  check_mod = FALSE # initial state
  this_family = tweedie()

  # -------------------------------------------------------------------------
    # Category 1:
    mod_init <- tryCatch(sdmTMB(
      data = sp_data,
      formula = this_formula[[1]],
      mesh = sp_mesh, 
      time = 'year',
      family = this_family,
      spatial = "on",
      spatiotemporal = "iid"
    ), error = function(e) conditionMessage(e))
    if(!is.character(mod_init)) {
      check_out = sanity(mod_init, silent = TRUE)
      check_mod = all(unlist(check_out[c('hessian_ok', 'eigen_values_ok', 
                                        'nlminb_ok', 'range_ok', 'gradients_ok', 
                                        'se_na_ok', 'sigmas_ok')]))
    }
    
    # Check if model passed
    
    if( !check_mod ) {
      # Category 1 did not pass, run Category 2:
      this_cat = 2
      mod_init <- tryCatch(sdmTMB(
        data = sp_data,
        formula = this_formula[[1]],
        mesh = sp_mesh, 
        time = 'year',
        family = this_family,
        spatial = "on",
        spatiotemporal = "off"
      ), error = function(e) conditionMessage(e))
      if(!is.character(mod_init)) { 
        check_out = sanity(mod_init, silent = TRUE)
        check_mod = all(unlist(check_out[c('hessian_ok', 'eigen_values_ok', 
                                          'nlminb_ok', 'range_ok', 'gradients_ok', 
                                          'se_na_ok', 'sigmas_ok')]))
      }
      
    } # category 2 conditional
    
    # Do not update model with significant covariates:
    mod_upd = mod_init

  # -------------------------------------------------------------------------
  pred_time = NULL # time predictions
  PredGrid = NULL # predictions eff
  if(check_mod) {
    # Plot residuals
    check_df = sanity(mod_upd, silent = TRUE)
    check_df2 = data.frame(name = names(unlist(check_df)), check = unlist(check_df))
    check_df2$category = this_cat
    if(check_residuals) {
      res_check = plot_residuals(mod_upd, plot_dir = this_plot_folder)
      check_df2$unif_test = round(res_check$unif_test, digits = 3)
      check_df2$outl_test = round(res_check$outl_test, digits = 3)
      check_df2$disp_test = round(res_check$disp_test, digits = 3)
    }
    if(save_results) write.csv(check_df2, file = file.path(this_model_folder, 'mod_check.csv'), row.names = FALSE)
    summ_table = get_summary_sdmTMB(model = mod_upd, model_label = this_sp)
    summ_table$check = check_df$all_ok
    summ_table$category = this_cat
    if(save_results) saveRDS(summ_table, file = file.path(this_model_folder, 'mod_summ.rds'))
    
    # Predictions -------------------------------------------------------------
    
    # MAKE PREDICTIONS FOR ANNUAL ESTIMATES (only unobs):
    # Filter prediction data for years and quarters when observations are available
    this_pred_df = predDat %>% filter(year %in% yr_keep, quarter %in% qt_keep)
    this_pred_df = this_pred_df %>% mutate(fyear = factor(year, levels = sort(unique(this_pred_df$year))))
    predictions = predict(mod_upd, newdata = this_pred_df, return_tmb_object = TRUE)
    pred_time = get_index(predictions, area = 1, bias_correct = TRUE)
    # Select relevant columns:
    pred_time = pred_time %>% select(year, est)
    # Merge with estimate from obs data:
    pred_time = rbind(pred_time, 
                      sp_data %>% group_by(year) %>% 
                        summarise(est = sum(bycatch), .groups = "drop")) # estimate from observed data
    # Aggregate again:
    pred_time = pred_time %>% group_by(year) %>% summarise(est = sum(est), .groups = "drop")
    # Add info columns:
    pred_time$sp_name = this_sp
    pred_time$check = check_df$all_ok
    pred_time$category = this_cat
    if(save_results) write.csv(pred_time, file = file.path(this_model_folder, 'pred_est_time.csv'), row.names = FALSE)
    
    # MAKE PREDICTIONS FOR APPENDIX A (using entire effort data: obs and unobs)
    # Filter prediction data for years and quarters when observations are available
    this_pred_df2 = effDat %>% filter(year %in% yr_keep, quarter %in% qt_keep)
    this_pred_df2 = this_pred_df2 %>% mutate(fyear = factor(year, levels = sort(unique(this_pred_df$year))))
    predictions2 = predict(mod_upd, newdata = this_pred_df2, return_tmb_object = TRUE)
    
    # To plot predictions spatially:
    PredGrid = predictions2$data
    PredGrid$bycatch_est = mod_upd$family$linkinv(PredGrid$est)
    
  } # if model converged
  
  # Delete model to save memory
  rm(mod_upd, mod_init, predictions, predictions2)
  
  return(list(pred_time, PredGrid))
  
}

