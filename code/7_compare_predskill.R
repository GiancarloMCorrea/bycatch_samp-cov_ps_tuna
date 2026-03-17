rm(list = ls())

# Define type of school to be analyzed:
source('code/aux_code/load_libs.R')

# Read and save results?
save_res = FALSE # FALSE if already saved

# -------------------------------------------------------------------------
# Read observed sim data (only for one simulation):
if(save_res) {
  save_sim = list()
  these_frac = list.files(file.path(model_folder, "crossval"))
  for(k in seq_along(these_frac)) {
    these_sims = list.files(file.path(model_folder, "crossval", these_frac[k]), full.names = TRUE)
    # Read all files:
    save_sim[[k]] = these_sims %>% map(readRDS) %>% bind_rows()
    cat("Sampling fraction", these_frac[k], "done", "\n")
  }
  
  # Merge data:
  est_sim = bind_rows(save_sim)
  # Define factors:
  est_sim = est_sim %>% mutate(samp_frac = factor(samp_frac, levels = frac_vector,
                                                  labels = paste0(frac_vector*100, "%")) )
  # Save results:
  saveRDS(est_sim, file = file.path(model_folder, "crossval_results.rds"))
} else {
  est_sim = readRDS(file.path(model_folder, "crossval_results.rds"))
}

# Define sp factors
est_sim = est_sim %>% mutate(sp_name = factor(sp_name, levels = sp_df$sp_levels,
                                              labels = sp_df$sp_label) )

# -------------------------------------------------------------------------
# Plot mae:
plot_dat = est_sim %>% filter(!(category %in% c(3))) %>% 
  mutate(rmse = round(rmse, digits = 3),
         mae = round(mae, digits = 3))
if(this_type == 'FOB') maxY = 1
if(this_type == 'FSC') maxY = 3

p1 = ggplot(data = plot_dat, aes(x = samp_frac, y = rmse)) +
  geom_boxplot() +
  xlab("Sampling coverage") + ylab("RMSE") +
  theme(strip.text = element_text(size = 11),
        strip.background = element_rect(fill="white"),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        axis.text.y = element_text(size = 10)) +
  coord_cartesian(ylim = c(0, maxY)) +
  facet_wrap(~ sp_name, ncol = 3, scales = 'free_y')
ggsave(paste0('crossval', img_type), plot = p1, path = plot_folder, 
       width = img_width , height = 200, units = 'mm', dpi = img_res)

