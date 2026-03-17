rm(list = ls())

# -------------------------------------------------------------------------

# Define type of school to be analyzed:
source('code/aux_code/load_libs.R')
source('code/aux_code/sdmTMB-config.R') # for model-based estimates

# Read 'effort' data:
# This is the observers data that will be treated as 100% 
effPoints = readRDS(file = file.path(data_folder, 'weight_data.rds'))
effPoints = effPoints %>% mutate(marea_id = paste(vessel_code, trip_start_date, sep = '_'))

# Species to analyze:
these_sp = unique(effPoints$sp_name)

# Specify number of simulations:
nSims = 100

# Set seeds
my_seeds = sample(x = -1000:1000, size = nSims, replace = FALSE)

# Mesh cutoff:
if(this_type == 'FOB') mesh_cutoff = 1.5
if(this_type == 'FSC') mesh_cutoff = 1

# N cores for parallel:
n_cores = 10

# Create folder to save sim estimates 
for(k in seq_along(frac_vector)) {
  dir.create(file.path(model_folder, 'sim_est', frac_vector[k]), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(model_folder, 'crossval', frac_vector[k]), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(model_folder, 'obs_sim_data', frac_vector[k]), showWarnings = FALSE, recursive = TRUE)
}

# -------------------------------------------------------------------------
# Estimate true values:
true_data = effPoints %>% group_by(year, sp_name) %>% summarise(true = sum(bycatch), .groups = 'drop')

# -------------------------------------------------------------------------
# Parallel data frame
sim_df = expand_grid(j = 1:nSims, i = 1:length(frac_vector), isp = seq_along(these_sp))

# -------------------------------------------------------------------------
# Start running simulation in parallel:

snowfall::sfInit(parallel=TRUE, cpus=n_cores)
snowfall::sfExportAll()
trash = snowfall::sfLapply(1:nrow(sim_df), function(k_row) {
  
  # Load required libraries:
  require(dplyr)
  require(ggplot2)
  require(sdmTMB)
  require(sf)
  require(fmesher)
  
  # Get sim information:
  j = sim_df %>% slice(k_row) %>% pull(j)
  i = sim_df %>% slice(k_row) %>% pull(i)
  isp = sim_df %>% slice(k_row) %>% pull(isp)
  
    # Define observed data:
    set.seed(my_seeds[j]) # Important to use same obs data for all species
    obs_marea = effPoints %>% group_by(year, marea_id) %>% 
      summarise(.groups = "drop") %>% 
      group_by(year) %>% 
      slice_sample(prop = frac_vector[i]) 
    obs_data = effPoints %>% filter(marea_id %in% obs_marea$marea_id)

    # Prediction data, exclude observed data:
    prediction_data = effPoints %>% filter(!(marea_id %in% obs_marea$marea_id))
    
      # Filter sp:
      sp_data = obs_data %>% filter(sp_name == these_sp[isp])
      
      # Effort sp:
      effSp = effPoints %>% filter(sp_name == these_sp[isp]) # used in ratio estimators
      predSp = prediction_data %>% filter(sp_name == these_sp[isp]) # used in model based
      
      # Save data:
      if(j == 1) {
        sp_data$samp_frac = frac_vector[i]
        saveRDS(sp_data, file = file.path(model_folder, 'obs_sim_data', frac_vector[i], 
                                          paste0("sp_", isp, ".rds")))
      }
      
      # ----------------------------------------
      # Ratio estimator:
      ratio_df = calculate_ratio_bycatch(obs_df = sp_data, eff_df = effSp, type = 'production')
      ratio_df = ratio_df %>% group_by(year, sp_name) %>% summarise(est_spt = sum(est), 
                                                                    est_tot = sum(est_tot),
                                                                    type1 = sum(type1),
                                                                    .groups = 'drop')
      estimate_df = left_join(true_data %>% filter(sp_name == these_sp[isp]), ratio_df, by = c("year", "sp_name"))
      # Add information:
      estimate_df$sim = paste0("sim_", j)
      estimate_df$samp_frac = frac_vector[i]
      
      # ----------------------------------------
      # Model-based estimator:
      this_formula = list(as.formula(bycatch ~ 0 + fyear + quarter + s(trop_catch, k = 3, bs = "cr")))
      # Remove years with no presence (if any):
      yr_pa = sp_data %>% group_by(year) %>% summarise(bycatch = sum(bycatch))
      yr_keep = yr_pa$year[which(yr_pa$bycatch > 0)]
      mod_data = sp_data %>% filter(year %in% yr_keep)
      if(length(yr_keep) == 1) {
        this_formula = list(update(this_formula[[1]], ~ . - fyear))
      }
      # Remove quarters with no presence (if any):
      qt_pa = sp_data %>% group_by(quarter) %>% summarise(bycatch = sum(bycatch))
      qt_keep = qt_pa$quarter[which(qt_pa$bycatch > 0)]
      mod_data = mod_data %>% filter(quarter %in% qt_keep)
      if(length(qt_keep) == 1) {
        this_formula = list(update(this_formula[[1]], ~ . - quarter))
      }
      if(nrow(mod_data) > 0) { # if there is still data, then run model:
        
        # Add fyear:
        mod_data = mod_data %>% mutate(fyear = factor(year, levels = sort(unique(mod_data$year))))
        # Make mesh:
        sp_mesh = sdmTMB::make_mesh(data = mod_data, xy_cols = c('lon', 'lat'),
                                    mesh = fmesher::fm_mesh_2d( mod_data[,c('lon', 'lat')], 
                                                                cutoff = mesh_cutoff ))
        # Run model:
        sdmtmb_df = run_sdmTMB_model(sp_data = mod_data, this_formula = this_formula, 
                                     sp_mesh = sp_mesh, this_sp = these_sp[isp], 
                                     predDat = predSp, effDat = effSp,
                                     yr_keep = yr_keep, qt_keep = qt_keep,
                                     save_model = FALSE, save_results = FALSE,
                                     check_residuals = FALSE)
        # Merge with results df:
        if(!is.null(sdmtmb_df[[1]])) { 
          # Annual bycatch estimates:
          mod_est_df = sdmtmb_df[[1]] %>% rename(est_model = est)
          estimate_df = left_join(estimate_df, 
                                  mod_est_df %>% select(year, est_model, category), 
                                  by = c("year"))
          estimate_df$est_model[is.na(estimate_df$est_model)] = 0 # fill NA with zeros
          estimate_df$category = mean(estimate_df$category, na.rm = TRUE) # to fill NA when missing years
          # MAE and RMSE:
          pred_data = left_join(effSp, sdmtmb_df[[2]] %>% ungroup() %>% select(id_set, bycatch_est), by = "id_set")
          pred_data$bycatch_est[is.na(pred_data$bycatch_est)] = 0 # zeros for no predictions
          out_data = data.frame(rmse = sqrt(mean((pred_data$bycatch - pred_data$bycatch_est)^2)),
                                mae = mean(abs(pred_data$bycatch - pred_data$bycatch_est)) )
          out_data$category = mean(sdmtmb_df[[1]]$category)
        } else { # if model failed, then NA:
          # Annual bycatch estimates:
          estimate_df$est_model = NA
          estimate_df$category = 3 # model failed identifier
          # MAE and RMSE:
          out_data = data.frame(rmse = NA, mae = NA )
          out_data$category = 3
        }
        
      } else { # If missing data, then estimate = 0:
        # Annual bycatch estimates:
        estimate_df$est_model = 0
        estimate_df$category = 4 # no data identifier
        # MAE and RMSE:
        pred_data = effSp
        pred_data$bycatch_est = 0
        out_data = data.frame(rmse = sqrt(mean((pred_data$bycatch - pred_data$bycatch_est)^2)),
                              mae = mean(abs(pred_data$bycatch - pred_data$bycatch_est)) )
        out_data$category = 4
      }
      
      # Add sim and sample_frac scenarios info:
      out_data$sim = paste0("sim_", j)
      out_data$samp_frac = frac_vector[i]
      out_data$sp_name = these_sp[isp]
      
      # Save results
      saveRDS(estimate_df, file = file.path(model_folder, 'sim_est', frac_vector[i], 
                                         paste0("sim_", j, "-sp_", isp, ".rds")))
      saveRDS(out_data, file = file.path(model_folder, 'crossval', frac_vector[i], 
                                         paste0("sim_", j, "-sp_", isp, ".rds")))
      
} )# parallel loop over sims

# Stop cluster
snowfall::sfStop()
