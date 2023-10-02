#Load

## Load libraries
# rm(list = ls())
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

## set up path
DATA_DIR <- "../data/"
FIGURE_DIR <- "../figures/01_figures/"
WDI_DIR <- paste0(DATA_DIR, "WDI_data_230609/WDI_CSV")

## set up cluster 
CLUSTER <- rep("localhost", 20)
Sys.setenv(OPENBLAS_NUM_THREADS = 20)


## load data
descriptiveWDI <- data.table::fread("../data/WDI_data_230609/WDI_CSV/WDISeries.csv")

## load indicator path

RELATIVE_INDICATORS_FILE_NAME <- "01_data/relative_indicators_2023_06.R" # nolint


## Load additional functions
source("../libraries/01a_lib_social_indicators.R")
# source("../src/01a_social_indicators_dimred.Rconsolidated_isomap.R")

## Load the base data
wdi_data    <- get_wdi_data()
wdi_country <- get_wdi_country()
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
#hdi_data%>% fwrite("../data/01_data/01_hdi_data.csv")
# fread("../data/01_data/01_data/01_hdi_data.csv")
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
  get_data_file("2309_qual_comp_res.RDS"),
  make_qual_comp_res,
  data = wdi_data,
  data_sensitivity = data_sensitivity_res
  #,.overwrite = T
)


# qual_comp_res<-readRDS("../data/qual_comp_res.RDS")

#get_data_file("qual_comp_res.RDS")


## Do some error checking and diagnostics
str(qual_comp_res[[1]])

## correlation of variables of axes with variables
all_cors <- list(iso = res_iso, pca = res_pca)


n_var <- length(RELATIVE_INDICATORS)
n_dim <- dim(qual_comp_res[[1]]$cor_iso)[2]
n_sam <- length(qual_comp_res)

res_iso <- array(
    NA_real_, dim = c(n_var, n_dim, n_sam),
    dimnames = list(RELATIVE_INDICATORS, paste0("iso", seq_len(n_dim)), 1:n_sam)
  )
res_pca <- array(
    NA_real_, dim = c(n_var, n_dim, n_sam),
    dimnames = list(RELATIVE_INDICATORS, paste0("PC", seq_len(n_dim)), 1:n_sam)
  )

for (i in seq_len(n_sam)) {
    idx <- RELATIVE_INDICATORS %in% rownames(qual_comp_res[[i]]$cor_pca)
    idx <- RELATIVE_INDICATORS %in% rownames(qual_comp_res[[i]]$cor_pca)
  
    res_pca[idx, , i] <- qual_comp_res[[i]]$cor_pca[,1:5]
    res_iso[idx, , i] <- qual_comp_res[[i]]$cor_iso
  }
  




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


## Playing around with this, it seems that k = 250 is cool
cons_iso_pca <- compute_if_no_file(
  get_data_file("cons_iso_pca_rerun.RDS"),
  make_cons_iso_pca,
  qual_comp_res = qual_comp_res,
  wdi_data = wdi_data,
  knns = c(250),
  n_used_vars = N_USED_VARS,
  country_names = COUNTRY_NAMES,
  used_var_names = USED_VAR_NAMES,
  years = YEARS,
  ciso = F,
   .overwrite = TRUE
)
 cbind(cons_iso_pca$occurrence_table[, 1:2], cons_iso_pca$wdi_data_cons_iso[[2]]$d_geo) %>%
  data.table::fwrite(., get_data_file("d_geo.csv"))

cbind(cons_iso_pca$wdi_data_cons_iso[[2]]$d_geo) %>%
  write.table(get_data_file("d_geo_no_ind.csv"),
              row.names = FALSE, col.names = FALSE,
              sep = ",")
cons_iso_pca$wdi_data_cons_gf %>% data.frame()
cbind(cons_iso_pca$occurrence_table[, 1:2]) %>%
  data.table::fwrite(., get_data_file("d_geo_ind.csv"))
cons_iso_pca
occurrence_table       <- cons_iso_pca$occurrence_table
wdi_data_cons          <- cons_iso_pca$wdi_data_cons
wdi_data_cons_pca      <- cons_iso_pca$wdi_data_cons_pca
wdi_data_cons_iso      <- cons_iso_pca$wdi_data_cons_iso
wdi_data_qual_cons_pca <- cons_iso_pca$wdi_data_qual_cons_pca
wdi_data_qual_cons_iso <- cons_iso_pca$wdi_data_qual_cons_iso
knns                   <- cons_iso_pca$knns
wdi_data_qual_cons_pca
wdi_data_cons_pcaGP_table      <- cbind(occurrence_table[,c(1,2)],cons_iso_pca$wdi_data_cons_gf)

#wdi_data_cons_pcaGP_table %>% fwrite("../data/01_GP_data.csv")

updatepca<-cons_iso_pca$wdi_data_cons_pca$cor 
merge(wdi_explained[,c(1,3)],updatepca %>% data.frame() %>% rownames_to_column("Series Code"),by="Series Code")-> updatepca2

plot_if_no_file(
  get_plot_file("01_2307_08_cons_quality_all_data.pdf"),
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
  get_plot_file("01_230708_pca_iso_res_var.pdf"),
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


## FIGURE 1figure one residual variance plot
residual_sample_variances = residual_sample_variances
wdi_data_qual_cons_pca = wdi_data_qual_cons_pca
wdi_data_qual_cons_iso = wdi_data_qual_cons_iso
old_par <- par()
par(mar = c(3.5, 3.75, 0, 0) + 0.1, las = 1, mgp = c(2.5, 0.25, 0))
on.exit(par(old_par))

max_x <- 13

pca_res <- residual_sample_variances$pca
iso_res <- residual_sample_variances$iso
median_res_var_pca <- residual_sample_variances$median_pca
median_res_var_iso <- residual_sample_variances$median_iso
hdi_res_var <- residual_sample_variances$hdi

max_res_var_1 <- 1
pca_res[[1]]
plot(pca_res[[1]],
   ylim = c(0, max_res_var_1),
   ## xlim = c(1, max_dim),
   xlim = c(1, max_x),
   ylab = "residual variance",
   xlab = "number of dimensions",
   cex.lab = 2,
   las = 1,
   xaxt = "n",
   yaxt = "n",
   bty = "n",
   type = "n")

points(wdi_data_qual_cons_pca,      type = "b",
     lwd = 4, col = lighten("slategrey", 0.8))
points(wdi_data_qual_cons_iso[[2]], type = "b",
     lwd = 4, col = lighten("seagreen", 0.8))
text(1, wdi_data_qual_cons_pca[1],      "PCA",
   pos = 4, offset = 0.5, xpd = TRUE, cex = 2)
text(1, wdi_data_qual_cons_iso[[2]][1], "Ensemble Isomap",
   pos = 4, offset = 0.5, xpd = TRUE, cex = 2)
text(max_x + 0.75,         0.1, "10% Residual Variance", cex = 1.8,
   adj = c(1.1, -0.5), xpd = TRUE)
abline(h = 0.1, col = "gray50", lwd = 2)
par(las = 1, mgp = c(1.5, 0.75, 0))#, xpd = TRUE)
axis(side = 1, at = 1:max_x, labels = ifelse(1:14 %% 2 == 1, 1:14, ""),
   col = "gray50", col.axis = "gray50",
   cex.axis = 1.5, lwd = 1, tcl = -0.25, lwd.ticks = 2)
axis(side = 2, at = seq(0, 1, length.out = 6),
   col = "gray50", col.axis = "gray50",
   cex.axis = 1.5, lwd = 1, tcl = -0.25, lwd.ticks = 2)










##  Supplement figure This one is for the apendix and contains more dimensions:
plot(pca_res[[1]],
   ylim = c(0, max_res_var_1),
   ## xlim = c(1, max_dim),
   xlim = c(1, max_x),
   ylab = "residual variance",
   xlab = "number of dimensions",
   las = 1,
   xaxt = "n",
   yaxt = "n",
   bty = "n",
   type = "n")
lapply(pca_res, function(x) lines(x, pch = ".", col = "#1b9e7711"))
lapply(iso_res, function(x) lines(x, pch = ".", col = "#d95f0211"))
points(median_res_var_pca, type = "b", pch = 4,
     lwd = 3, col = lighten("#1b9e77", 0.8))
points(median_res_var_iso, type = "b", pch = 4,
     lwd = 3, col = lighten("#d95f02", 0.8))
points(wdi_data_qual_cons_pca,      type = "b",
     lwd = 3, col = lighten("#1b9e77", 0.8))
points(wdi_data_qual_cons_iso[[2]], type = "b",
     lwd = 3, col = lighten("#d95f02", 0.8))
legend(1.5, 0.9, lwd = c(1, 3, 3), bty = "n",
     legend = c("PCA", "Ensemble PCA", "Median"), cex = 0.8, lty = 1,
     col = c("#1b9e77", lighten("#1b9e77", 0.8), lighten("#1b9e77", 0.8)),
     pch = c(NA, 1, 4))
legend(0.5, 0.1, lwd = c(1, 3, 3), bty = "n",
     legend = c("Isomap", "Ensemble Isomap", "Median"), cex = 0.8, lty = 1,
     col = c("#d95f02", lighten("#d95f02", 0.8), lighten("#d95f02", 0.8)),
     pch = c(NA, 1, 4))
text(max_x + 0.75, 0.1,         "10% Residual Variance", cex = 0.8,
   adj = c(1.1, -0.5), xpd = TRUE)
abline(h = 0.1, col = "gray50", lwd = 2)
text(max_x + 0.75, hdi_res_var, "HDI Residual Variance", cex = 0.8,
   adj = c(1.1, -0.5), xpd = TRUE)
abline(h = hdi_res_var, col = "gray50", lwd = 2)
par(las = 1, mgp = c(5, 0.25, 0), xpd = TRUE)
axis(side = 1, at = 1:max_x,
   col = "gray50", col.axis = "gray50",
   cex.axis = 0.75, lwd = 1, tcl = -0.125, lwd.ticks = 2)
axis(side = 2, at = seq(0, 1, length.out = 11),
   col = "gray50", col.axis = "gray50",
   cex.axis = 0.75, lwd = 1, tcl = -0.125, lwd.ticks = 2)


