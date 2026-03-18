# Read libraries:
library(ggplot2)
library(dplyr)  
library(sf)
library(tibble)
library(readr)
library(purrr)
library(gridExtra)
library(viridis)
library(sdmTMB)
library(DescTools)
library(boot)
library(stars)
library(stringr)
library(lubridate)
library(fmesher)
library(wesanderson)
library(corrplot)
library(car)
library(future)
library(tidyr)
library(reshape2)
require(here)
library(magick)
source('code/aux_code/parameters_for_plots.R')
source('code/aux_code/aux_functions.R')

# SELECT YOUR SET TYPE HERE!:
this_type = 'FOB' # FOB or FSC

# First year to make analyses:
str_yr = 2015

# Define plot and data folder:
# Data folder:
data_folder = here(file.path("data", this_type))
dir.create(data_folder, showWarnings = FALSE, recursive = TRUE)
# Plot folder:
plot_folder = here(file.path("figures", this_type))
dir.create(plot_folder, showWarnings = FALSE, recursive = TRUE)
# Model folder:
model_folder = here(file.path("model", this_type))
dir.create(model_folder, showWarnings = FALSE, recursive = TRUE)

# Specify sampling fraction vector:
frac_vector = c(0.05, seq(from = 0.1, to = 0.5, by = 0.1), 0.7, 0.9)

# Specify species levels:
fob_sp_df = data.frame(sp_levels = c("E. bipinnulata", "Balistidae", "Coryphaenidae", "A. solandri", "Carangidae",
                                    "Carcharhinidae", "M. nigricans", "Sphyrnidae", "Chelonioidea", "Mobulidae",
                                    "Alopiidae", "Lamnidae", "P. glauca"),
                     sp_type = c(rep("Common", times = 5), 
                                 rep("Special interest", times = 5),
                                 rep("Rare", times = 3)),
                     type_abb = c(rep("(Comm.)", times = 5), 
                                  rep("(Sp. Int.)", times = 5),
                                  rep("(Rare)", times = 3))
                     )

fsc_sp_df = data.frame(sp_levels = c('Carcharhinidae', 'I. albicans',
                                     'Mobulidae', 'M. nigricans', 'Sphyrnidae', 'Chelonioidea', 'Molidae',
                                   'Lamnidae', 'P. glauca'),
                     sp_type = c(rep("Common", times = 2), 
                                 rep("Special interest", times = 5),
                                 rep("Rare", times = 2)),
                     type_abb = c(rep("(Comm.)", times = 2), 
                                  rep("(Sp. Int.)", times = 5),
                                  rep("(Rare)", times = 2))
                     )


if(this_type == 'FOB') sp_df = fob_sp_df %>% mutate(sp_label = paste(sp_levels, type_abb))
if(this_type == 'FSC') sp_df = fsc_sp_df %>% mutate(sp_label = paste(sp_levels, type_abb))
