rm(list = ls())

# Define type of school to be analyzed:
source('code/aux_code/load_libs.R')

# -------------------------------------------------------------------------
source("C:/Use/GitHub/extractOceanVariables/code/copernicus/multiple/matchCOPERNICUS.R")
source("C:/Use/GitHub/extractOceanVariables/code/auxFunctions.R")

# -------------------------------------------------------------------------
# Read data in:
obsPoints = readRDS(file.path(data_folder, 'obsPoints.rds'))

# Assign environmental information:
# -------------------------------------------------------------------------
# Potential temperature surface:

# Define Lan/Lot and Date column names in your dataset:
lonlat_cols = c("lon", "lat")
date_col = "date"
fields = "thetao"
savedir = "C:/Use/OneDrive - AZTI/Data/ICCAT/Env_Data_ATL_2010-onwards/thetao_depth0-100_P1M-m/"
obsPoints = matchCOPERNICUS(data           = obsPoints, 
                            lonlat_cols    = lonlat_cols,
                            date_col       = date_col,
                            var_label      = fields, 
                            var_path     = savedir,
                            depth_range  = c(0, 5),
                            depth_FUN    = 'mean')

# -------------------------------------------------------------------------
# Check env data:
summary(obsPoints)

# Rename:
obsPoints = obsPoints %>% rename(sst = thetao)

# -------------------------------------------------------------------------
# Save data with non standardized env information:
saveRDS(obsPoints, file = file.path(data_folder, 'obsPoints_nostd.rds'))

# -------------------------------------------------------------------------
# Standardized covariates to explore effect better:
obsPoints = obsPoints %>% mutate(sst_nonstd = sst, trop_catch_nonstd = trop_catch)

# Select variables to standardize:
std_cov = c('sst', 'trop_catch')

# Std covariates in observations
for(j in seq_along(std_cov)) {
  mean_var = mean(obsPoints[,std_cov[j]], na.rm = T)
  sd_var = sd(obsPoints[,std_cov[j]], na.rm = T)
  obsPoints[,std_cov[j]] = (obsPoints[,std_cov[j]] - mean_var)/sd_var
}

# -------------------------------------------------------------------------
# Save data with env information:
saveRDS(obsPoints, file = file.path(data_folder, 'obsPoints.rds'))
