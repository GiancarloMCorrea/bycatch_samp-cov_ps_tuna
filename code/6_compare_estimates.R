rm(list = ls())

# Define type of school to be analyzed:
source('code/aux_code/load_libs.R')
colorPal = RColorBrewer::brewer.pal(n = 9, name = "Set1")[1:3]

# Read and save results?
save_res = FALSE # FALSE if already saved

# -------------------------------------------------------------------------
if(save_res) {
  # Read observed sim data:
  save_sim = list()
  these_frac = list.files(file.path(model_folder, "sim_est"))
  for(k in seq_along(these_frac)) {
    these_sims = list.files(file.path(model_folder, "sim_est", these_frac[k]), full.names = TRUE)
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
  saveRDS(est_sim, file = file.path(model_folder, "sim_results.rds"))
} else {
  est_sim = readRDS(file.path(model_folder, "sim_results.rds"))
}

# Define sp factors
est_sim = est_sim %>% mutate(sp_name = factor(sp_name, levels = sp_df$sp_levels,
                                              labels = sp_df$sp_label) )

# -------------------------------------------------------------------------
# Report convergence rate of model-based estimator:
# Just select one year:
plot_data = est_sim %>% group_by(sim, samp_frac, sp_name) %>% 
  summarise(category = mean(category, na.rm=TRUE), .groups = "drop")
plot_data = plot_data %>% mutate(category = factor(category, levels = 1:4, 
                                                   labels = c("\u03C9+\u03B5", "\u03C9", "Model failed", "No data")))

c1 = ggplot(data = plot_data, aes(x = samp_frac, fill = category)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d() + 
  scale_y_continuous(labels = scales::percent) +
  xlab("Sampling coverage") + ylab("Percentage of replicates") +
  guides(fill = guide_legend(title = NULL)) +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill="white"),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~ sp_name, ncol = 3)
ggsave(paste0('conv_model', img_type), plot = c1, path = plot_folder, 
       width = img_width , height = 200, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Report number of times that ratio_tot was used in ratio_spt per grid:
plot_data = est_sim %>% group_by(sim, samp_frac, sp_name) %>% 
  summarise(type1 = sum(type1), .groups = "drop")

d1 = ggplot(data = plot_data, aes(x = samp_frac, y = type1)) +
  geom_boxplot() +
  xlab("Sampling coverage") + ylab("Percentage of replicates") +
  theme(legend.position = 'bottom',
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill="white"),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 9),
        axis.text.y = element_text(size = 10)) +
  facet_wrap(~ sp_name, ncol = 3)
ggsave(paste0('type1', img_type), plot = d1, path = plot_folder, 
       width = img_width , height = 200, units = 'mm', dpi = img_res)

# -------------------------------------------------------------------------
# Calculate relative error:
# Exclude sims that model-based estimator failed
re_data = est_sim %>% filter(!(category %in% c(3))) %>% # exclude replicates model failed
            mutate(re_tot = ((est_tot - true)/true)*100,
                   re_spt = ((est_spt - true)/true)*100,
                   re_model = ((est_model - true)/true)*100)
# when true = 0, NaN are produced. In this case, ratio and model based will be zero as well:
# fill them with 0
re_data = re_data %>% mutate(re_tot = if_else(is.nan(re_tot), 0, re_tot),
                             re_spt = if_else(is.nan(re_spt), 0, re_spt),
                             re_model = if_else(is.nan(re_model), 0, re_model))

# New format for plotting:
re_data = re_data %>% pivot_longer(cols = c("re_tot", "re_spt", "re_model"),
                                   values_to = "re", names_to = c("est_type"))


# -------------------------------------------------------------------------
# Aggregate overall:
agg_data = re_data %>% group_by(samp_frac, sp_name, est_type) %>%
  dplyr::summarise(q025 = quantile(re, probs = 0.05), 
                   q50 = median(re),
                   q975 = quantile(re, probs = 0.95),
                   .groups = "drop") 
agg_data = agg_data %>% mutate(est_type = factor(est_type, levels = c("re_tot", "re_spt", "re_model"),
                                                 labels = c("Ratio (global)", "Ratio (spatial)", "Model-based")))

# Fix when mean > q975 and larger than 100%:
agg_data = agg_data %>% mutate(q975 = if_else(q975 < q50, q50, q975))

# Count observations per group
counts = re_data %>%
  group_by(samp_frac, sp_name) %>%
  summarise( n = n_distinct(sim), y = -110, .groups = "drop")

# Make plot overall:
p2 = ggplot(agg_data, aes(x=samp_frac, y=q50, colour=est_type)) +
  geom_linerange(aes(ymin = q025, ymax = q975), position=position_dodge(0.5)) +
  geom_pointrange(aes(ymin = q025, ymax = q975),  
                  position=position_dodge(0.5), size = 0.2) +
  geom_text(data = counts,
    aes(x = samp_frac, y = y, label = n),
    inherit.aes = FALSE, size = 3
  ) +
  scale_color_manual(values = colorPal) +
  geom_hline(yintercept=0, color=1, linetype='dashed') +
  coord_cartesian(ylim = c(-110, 150)) +
  labs(colour = "Estimator") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill="white"),
        legend.text=element_text(size=10)) +
  xlab("Sampling coverage") + ylab("Relative error (%)") +
  facet_wrap(~sp_name, ncol = 3)
ggsave(paste0('agg_re', img_type), plot = p2, path = plot_folder, 
       width = img_width , height = 200, units = 'mm', dpi = img_res)


# -------------------------------------------------------------------------
# Aggregate by year:
agg_yr_data = re_data %>% group_by(samp_frac, sp_name, est_type, year) %>%
  dplyr::summarise(q025 = quantile(re, probs = 0.05), 
                   q50 = median(re),
                   q975 = quantile(re, probs = 0.95),
                   .groups = "drop") 
agg_yr_data = agg_yr_data %>% mutate(est_type = factor(est_type, levels = c("re_tot", "re_spt", "re_model"),
                                                 labels = c("Ratio (global)", "Ratio (spatial)", "Model-based")))

# Fix when mean > q975 and larger than 100%:
agg_yr_data = agg_yr_data %>% mutate(q975 = if_else(q975 < q50, q50, q975))

# Plot by year
# Make this plot by species group:
all_sp_types = unique(sp_df$sp_type)
for(k in seq_along(all_sp_types)) {
  plot_dat_yr = agg_yr_data %>% 
    filter(sp_name %in% (sp_df %>% filter(sp_type == all_sp_types[k]) %>% pull(sp_label)))
  # Remove category from sp name:
  plot_dat_yr = plot_dat_yr %>% mutate(sp_name = as.character(sp_name))
  plot_dat_yr = plot_dat_yr %>% mutate(sp_name = factor(sp_name, levels = sp_df$sp_label,
                                                        labels = sp_df$sp_levels) )
  
  p3 = ggplot(plot_dat_yr, aes(x=year, y=q50)) +
    geom_line(aes(color = est_type)) +
    geom_ribbon(aes(ymin = q025, ymax = q975, fill = est_type), alpha = 0.3) +
    geom_hline(yintercept=0, color=1, linetype='dashed') +
    scale_color_manual(values = colorPal) +
    scale_fill_manual(values = colorPal) +
    coord_cartesian(ylim = c(-100, 100)) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 9),
          strip.text = element_text(size = 10),
          strip.background = element_rect(fill="white"),
          legend.text=element_text(size=10)) +
    xlab(NULL) + ylab("Relative error (%)") +
    facet_grid(samp_frac~sp_name) +
    guides(colour=guide_legend(title="Estimator"), fill=guide_legend(title="Estimator"))
  ggsave(paste0('yr_re_', gsub(pattern = " ", replacement = "", x = all_sp_types[k]), img_type), 
         plot = p3, path = plot_folder, width = img_width , height = 200, units = 'mm', dpi = img_res)
}

