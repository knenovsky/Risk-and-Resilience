## Load libraries
rm(list = ls())
library(rstudioapi)
library(magrittr)
library(tidyverse)
library(data.table)
library(parallel)
library(dimRed)
library(devtools)
library(igraph)
library(energy)
library(ggrepel)
library(ggalt)
library(ggraph)
library(trend)
library(riverplot)
library(RANN)
library(RJSONIO)
library(RSpectra)
library(pcaMethods)

## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## set up path
DATA_DIR <- "../data/"
FIGURE_DIR <- "../figures/01_figures/"
WDI_DIR <- paste0(DATA_DIR, "WDI_data_230609/WDI_CSV")

## set up cluster 
CLUSTER <- rep("localhost", 10)
Sys.setenv(OPENBLAS_NUM_THREADS = 10)


## load data
descriptiveWDI <- data.table::fread("../data/WDI_data_230609/WDI_CSV/WDISeries.csv")
# ALlready described relative indicators

# ##  How we got the new relative indicators
#RELATIVE_INDICATORS_FILE_NAME <- "../data/01_data/relative_indicators_2023_01.R" # nolint
# RELATIVE_INDICATORS<-source(RELATIVE_INDICATORS_FILE_NAME)
# absoluteDATA<-descriptiveWDI[!(descriptiveWDI$`Series Code`%in% RELATIVE_INDICATORS), ]
# candidatesforrelative<-absoluteDATA %>% filter(!(`Aggregation method`%in% "Gap-filled total"))
# ### filter by excluding all indicators that have been exluded in the first version
# WDI_explained_2018<-fread("../../23_01_COCAP/01_low_dim_paper/data/WDI_csv_2018Q1/WDISeries.csv")
# oldindicatorsy<-source("../../23_01_COCAP/01_low_dim_paper/data/relative_indicators_2018Q1.R")
# absoluteIndicators2018<-WDI_explained_2018[!(WDI_explained_2018$`Series Code`%in% oldindicatorsy$value),"Series Code"]
# tentetiveRelative<-candidatesforrelative[!(candidatesforrelative$`Series Code`%in% absoluteIndicators2018$`Series Code`),]
# newReleativeValutes<-tentetiveRelative %>%  filter(grepl("Weighted",`Aggregation method`))
# RELATIVE_INDICATORS_2305<-append(RELATIVE_INDICATORS,newReleativeValutes$`Series Code`)
# save(RELATIVE_INDICATORS_2305,file = "../data/01_data/relative_indicators_2023_06.R")
RELATIVE_INDICATORS_FILE_NAME <- "01_data/relative_indicators_2023_06.R" # nolint



## Load additional functions
source("../libraries/01a_lib_social_indicators.R")
# source("../src/01a_social_indicators_dimred.Rconsolidated_isomap.R")

## Load the base data
wdi_data    <- get_wdi_data()
wdi_country <- get_wdi_country()
wdi_series  <- get_wdi_series()

###
wdi_series <- data.table::fread(paste0(WDI_DIR, "/", "WDISeries.csv")) %>%
  data.table::setnames("Series Code", "Indicator Code")

indicator_sdg <- data.table::fread(get_data_file("indicator_sdg.csv"),
                                   header = TRUE)
sdg_table <- data.table::fread(get_data_file("sdg_goals.csv"),
                               header = TRUE)


indicator_sdg$SDG <- indicator_sdg$`SDG Target` %>%
  strsplit(".", fixed = TRUE) %>%
  sapply(function (x) x[1]) %>% {
    dplyr::if_else(. == "other", "0", .)
  } %>%
  as.integer

###



hdi_data    <- get_hdi_data()

## Derived constants
COUNTRY_CODES       <- get_country_codes(wdi_country)
INDICATOR_CODES     <- get_indicator_codes(wdi_series)
## Calculate and plot the missing value scores!

data_sensitivity_res <- compute_if_no_file(
  get_data_file("data_sensitivity_res.RDS"),
  make_data_sensitivity,
  x                   = wdi_data,
  country_codes       = COUNTRY_CODES,
  relative_indicators = RELATIVE_INDICATORS,
  year_range          = 1990:2022
)



plot_if_no_file(get_plot_file("n_var_vs_n_cntry.pdf"),
                make_plot_n_var_vs_n_cntry,
                x = data_sensitivity_res)

## Calculate the quality of the subsets
qual_comp_res <- compute_if_no_file(
  get_data_file("qual_comp_res.RDS"),
  make_qual_comp_res,
  data = wdi_data,
  data_sensitivity = data_sensitivity_res
)
qual_comp_res %>%  length()
#or function ‘getDimRedData’ for signature ‘"try-error"’
##Error in h(simpleError(msg, call)) : ad
#error in evaluating the argument 'object' in selecting a method for function 'getPars': $ operator is invalid for atomic vectors


#qual_comp_res<-readRDS("../data2023_01/qual_comp_res.RDS")

#get_data_file("qual_comp_res.RDS")


## Do some error checking and diagnostics
qual_comp_res_stats(qual_comp_res)
str(qual_comp_res[[1]])

## correlation of variables of axes with variables
all_cors <- assemble_correlations(qual_comp_res)

## the used variable indices
USED_VAR_IDX <- apply(all_cors$iso, 1, function(x) !all(is.na(x)))
N_USED_VARS <- sum(USED_VAR_IDX)

USED_VAR_NAMES <-
  qual_comp_res %>%
  lapply(function (x) rownames(x$cor_pca)) %>%
  unlist %>%
  unique %>%
  sort

YEARS <-
  qual_comp_res %>%
  lapply(function(x) x$cntry_year$year) %>%
  unlist %>%
  unique %>%
  sort

COUNTRY_NAMES <-
  qual_comp_res %>%
  lapply(function(x) x$cntry_year$`Country Code`) %>%
  unlist %>%
  unique %>%
  sort

## The following section creates a consolidated Isomap using the original data
## and calculating the distances by ignoring NAs and scaling them accordingly.

## SEE pw_geod_holes

## This does not work, because of the large number of gaps, the pca error is
## actually smaller than the isomap error


## Playing around with this, it seems that k = 50 is the sweet spot
cons_iso_pca <- compute_if_no_file(
  get_data_file("cons_iso_pca.RDS"),
  make_cons_iso_pca,
  qual_comp_res = qual_comp_res,
  wdi_data = wdi_data,
  knns = c(25, 50, 100, 250, 350),
  n_used_vars = N_USED_VARS,
  country_names = COUNTRY_NAMES,
  used_var_names = USED_VAR_NAMES,
  years = YEARS,
  ciso = TRUE
  ## .overwrite = TRUE
)
 cbind(cons_iso_pca$occurrence_table[, 1:2], cons_iso_pca$wdi_data_cons_iso[[2]]$d_geo) %>%
  data.table::fwrite(., get_data_file("d_geo.csv"))

cbind(cons_iso_pca$wdi_data_cons_iso[[2]]$d_geo) %>%
  write.table(get_data_file("d_geo_no_ind.csv"),
              row.names = FALSE, col.names = FALSE,
              sep = ",")

cbind(cons_iso_pca$occurrence_table[, 1:2]) %>%
  data.table::fwrite(., get_data_file("d_geo_ind.csv"))

occurrence_table       <- cons_iso_pca$occurrence_table
wdi_data_cons          <- cons_iso_pca$wdi_data_cons
wdi_data_cons_pca      <- cons_iso_pca$wdi_data_cons_pca
wdi_data_cons_iso      <- cons_iso_pca$wdi_data_cons_iso
wdi_data_qual_cons_pca <- cons_iso_pca$wdi_data_qual_cons_pca
wdi_data_qual_cons_iso <- cons_iso_pca$wdi_data_qual_cons_iso
knns                   <- cons_iso_pca$knns
knns
plot_if_no_file(
  get_plot_file("cons_quality_all_data.pdf"),
  plot_cons_iso_pca,
  cons_iso_pca = cons_iso_pca,
  .overwrite = TRUE
)
cons_iso_pca %>% dim
## 50 needs to be one of the values in `knns` and has to be picked manually
wdi_data_cons_df <- make_wdi_data_cons_df(knn = 250,
                                          wdi_data_cons     = wdi_data_cons,
                                          wdi_data_cons_iso = wdi_data_cons_iso,
                                          wdi_data_cons_pca = wdi_data_cons_pca,
                                          wdi_country       = wdi_country,
                                          occurrence_table  = occurrence_table,
                                          hdi_data          = hdi_data)
# data.table::fwrite(wdi_data_cons_df, get_data_file("wdi_data_cons_df_100.csv"))


topic_report(wdi_series = wdi_series, used_var_names = USED_VAR_NAMES)

plot_if_no_file(
  get_plot_file("the_main_gradients.pdf"),
  plot_the_main_gradients,
  wdi_data_cons_df = wdi_data_cons_df,
  prim_font_size = 20,
  .overwrite = TRUE
)

## Create the data for the 3d Visualization
make_3d_visualization_data(
  wdi_data_cons_df = wdi_data_cons_df,
  wdi_series = wdi_series,
  used_var_names = USED_VAR_NAMES,
  .overwrite = TRUE
)

## Calculate the distance correlations between the first 5 isomap axes and the
## variables
dcor_var_iso_cons <- compute_if_no_file(
  get_data_file("wdi_data_cons_dcor.RDS"),
  make_dcor_var_iso_cons,
  wdi_data_cons_df = wdi_data_cons_df,
  used_var_names = USED_VAR_NAMES,
  .overwrite = TRUE
)

## create the data for an interactive table
make_cons_cor_table_data(
  dcor_var_iso_cons = dcor_var_iso_cons,
  wdi_series = wdi_series,
  .overwrite = TRUE
)

dcor_var_iso_cons %>% data.frame() %>% 
  rownames_to_column("indicators")   %>% merge(wdi_series[,c(1:4)], . ,by.x="Indicator Code",by.y="indicators")-> outi_whole

residual_sample_variances <- compute_if_no_file(
  get_data_file("residual_sample_variances.RDS"),
  make_residual_variance_lists,
  qual_comp_res = qual_comp_res,
  wdi_data_cons_df = wdi_data_cons_df,
  used_var_names = USED_VAR_NAMES,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("pca_iso_res_var.pdf"),
  plot_residual_variances,
  residual_sample_variances = residual_sample_variances,
  wdi_data_qual_cons_iso = wdi_data_qual_cons_iso,
  wdi_data_qual_cons_pca = wdi_data_qual_cons_pca,
  .overwrite = TRUE
)

## Get the explained variances like this:
1 - wdi_data_qual_cons_pca
1 - wdi_data_qual_cons_iso[[2]]
format(c(1, wdi_data_qual_cons_iso[[2]]) - c(wdi_data_qual_cons_iso[[2]], 0),
       digits = 2, scientific = FALSE)
1 - residual_sample_variances$hdi

## The number of variables:
N_USED_VARS
## The number of countries

plot_if_no_file(
  get_plot_file("hdi_vs_idx.pdf"),
  plot_hdi_var_dcor,
  prim_font_size = 15,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("hdi_vs_idx_slopes.pdf"),
  plot_hdi_var_dcor_slopes,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("axis_meanings.pdf"),
  plot_axis_meanings,
  all_cors = all_cors,
  .width = 10,
  .height = 70,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("axis_meanings_fig.pdf"),
  plot_axis_meanings_fig,
  all_cors = all_cors,
  dcor_var_iso_cons = dcor_var_iso_cons,
  used_var_idx = USED_VAR_IDX,
  n_used_vars = N_USED_VARS,
  .width = 18.3 / 2.54,
  .height = 24.7 / 2.54,
  .pointsize = 7,
  .overwrite = TRUE
)

make_median_cor_table_data(
  all_cors = all_cors,
  used_var_idx = USED_VAR_IDX,
  relative_indicators = RELATIVE_INDICATORS,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("iso_traj_interesting.pdf"),
  plot_interesting_trajectories,
  wdi_data_cons_df = wdi_data_cons_df,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("iso_traj_interesting_gg.pdf"),
  plot_interesting_trajectories_gg,
  wdi_data_cons_df = wdi_data_cons_df,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("iso_trends.pdf"),
  plot_t_vs_trend,
  wdi_data_cons_df = wdi_data_cons_df,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("slope_iso_world_map.pdf"),
  plot_worldmap_slopes,
  wdi_data_cons_df = wdi_data_cons_df,
  .height = 10,
  .width = 4.3,
  .overwrite = TRUE
)

plot_if_no_file(
  get_plot_file("slope_var_world_map.pdf"),
  plot_worldmap_slopes_var,
  wdi_data_cons_df = wdi_data_cons_df,
  .overwrite = TRUE
)



## These are the variables with maximum dcor
# nolint start
used_var_names_max_dcor <- c(
  dcor_var_iso_cons[, 1] %>% order(decreasing = TRUE) %>% { .[1:40] } %>% { names(dcor_var_iso_cons[., 1]) },
  dcor_var_iso_cons[, 2] %>% order(decreasing = TRUE) %>% { .[1:30] } %>% { names(dcor_var_iso_cons[., 2]) },
  dcor_var_iso_cons[, 3] %>% order(decreasing = TRUE) %>% { .[1:20] } %>% { names(dcor_var_iso_cons[., 3]) },
  dcor_var_iso_cons[, 4] %>% order(decreasing = TRUE) %>% { .[1:10] } %>% { names(dcor_var_iso_cons[., 4]) },
  dcor_var_iso_cons[, 5] %>% order(decreasing = TRUE) %>% { .[1:10]  } %>% { names(dcor_var_iso_cons[., 5]) }
) %>% unique %>% sort
# nolint end
cor_var_iso_cons <- t(cor(wdi_data_cons_df[, PC1:PC5],
                          wdi_data_cons_df[, USED_VAR_NAMES, with = FALSE],
                          use = "pairwise.complete.obs"))

# nolint start
used_var_names_max_cor_pca <- c(
  cor_var_iso_cons[, 1] %>% order(decreasing = TRUE) %>% { .[1:40] } %>% { rownames(cor_var_iso_cons)[.] },
  cor_var_iso_cons[, 2] %>% order(decreasing = TRUE) %>% { .[1:30] } %>% { rownames(cor_var_iso_cons)[.] },
  cor_var_iso_cons[, 3] %>% order(decreasing = TRUE) %>% { .[1:20] } %>% { rownames(cor_var_iso_cons)[.] },
  cor_var_iso_cons[, 4] %>% order(decreasing = TRUE) %>% { .[1:10] } %>% { rownames(cor_var_iso_cons)[.] },
  cor_var_iso_cons[, 5] %>% order(decreasing = TRUE) %>% { .[1:5]  } %>% { rownames(cor_var_iso_cons)[.] }
) %>% unique %>% sort
# nolint end


dcor_var_iso_cons %>% str
cor_var_iso_cons %>% str


