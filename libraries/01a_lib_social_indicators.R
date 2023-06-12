## Contains functions for make_data_sensitivity script
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y

## Bootstrap the data
get_data_file <- function(name) {
  paste0(DATA_DIR, "/", name)}
get_relative_indicators <- function(){
  dget(get_data_file(RELATIVE_INDICATORS_FILE_NAME))}
RELATIVE_INDICATORS <- get_relative_indicators()
get_plot_file <- function(name) paste0(FIGURE_DIR, "/", name)
get_wdi_data <- function(){
  paste0(WDI_DIR, "/", "WDIData.csv") %>%
    data.table::fread(header = TRUE) %>% {
      for (i in 5:ncol(.)) .[[i]] <- as.numeric(.[[i]])
      .
    } %>%
    setkey(`Indicator Code`) %>% {
      .[RELATIVE_INDICATORS]
    }
}

get_wdi_country <- function(){
  data.table::fread(paste0(WDI_DIR, "/", "WDICountry.csv"))
  }
get_wdi_series <- function() {
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

  ## Sanity check, this will probably change with the underlying data
  stopifnot(setequal(
    setdiff(indicator_sdg$`Indicator Name`, wdi_series$`Indicator Name`),
    c("Annual deforestation (% of change)", "tax revenue (current LCU)",
      "GNI per capita (current US$)")
  ))

  result <- merge(wdi_series, indicator_sdg, all.x = TRUE, all.y = FALSE)
  result <- merge(result, sdg_table,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "SDG Target", by.y = "Target Number")
  result[`SDG Target` == "other", `Short Goal` := "Other"]

  ## Sanity check, this will probably change with the underlying data
  stopifnot(all(!is.na(result[!is.na(SDG), "Short Goal"]$`Short Goal`)))

  return(result)
}

get_country_codes <- function(x) sort(unique(x$`Country Code`[x$Region != ""]))
get_indicator_codes <- function(x) x$`Indicator Code` %>% unique %>% sort


compute_if_no_file <- function(file, fun, ..., .overwrite = FALSE) {
  if (.overwrite || !file.exists(file)) {
    res <- fun(...)
    saveRDS(res, file)
    res
  } else {
    readRDS(file)
  }
}

plot_if_no_file <- function(file, fun, ...,
                            .overwrite = FALSE,
                            .width = 7, .height = 7,
                            .pointsize = 12) {
  if (.overwrite || !file.exists(file)) {
    pdf(file, width = .width, height = .height, pointsize = .pointsize)
    on.exit(dev.off())
    fun(...)
  }
}

get_number_of_variables <- function(start_year, end_year,
                                    var_na_fraction, cntry_na_fraction) {
  wdi_data_transformed %>%
    dplyr::filter(year %in% start_year:end_year) ->
    wdi_data_3

  wdi_data_3                              %>%
    dplyr::select(-`Country Code`, -year) %>%
    is.na                                 %>%
    colMeans                              %>%
    { . < var_na_fraction } ->
    col_idx

  number_of_variables <- sum(col_idx)
 

  
  
  #setDT(wdi_data_4)
  #country_na_frac <- wdi_data_4[, mean(is.na(.SD)), by = `Country Code`]
  wdi_data_4 <- wdi_data_3[, c("Country Code", "year", names(col_idx)[col_idx]),
                           with = FALSE]
  
  setDT(wdi_data_4, key = "Country Code")
  country_na_frac <- wdi_data_4[, mean(is.na(.SD)), by = `Country Code`]
  n_countries <- sum(country_na_frac$V1 < cntry_na_fraction)

  return(list(
    start_year          = start_year,
    n_years             = end_year - start_year + 1,
    var_na_fraction     = var_na_fraction,
    number_of_variables = number_of_variables,
    cntry_na_fraction   = cntry_na_fraction,
    n_countries         = n_countries
  ))
}

#' create the subsets
#'
#' requires global state!!
#' - data_sensitivity -> the result of make_data_sensitivity()
#' - data -> the result of get_wdi_data()
create_data <- function (idx) {

  start_year          <- data_sensitivity$start_year[idx]
  n_years             <- data_sensitivity$n_years[idx]
  end_year            <- start_year + n_years - 1
  cntry_na_fraction   <- data_sensitivity$cntry_na_fraction[idx]
  var_na_fraction     <- data_sensitivity$var_na_fraction[idx]
  n_countries         <- data_sensitivity$n_countries[idx]
  number_of_variables <- data_sensitivity$number_of_variables[idx]

  cat(
    "start_year =             ",          start_year, "\n",
    "n_years =                ",             n_years, "\n",
    "end_year =               ",            end_year, "\n",
    "cntry_na_fraction =      ",   cntry_na_fraction, "\n",
    "var_na_fraction =        ",     var_na_fraction, "\n",
    "n_countries =            ",         n_countries, "\n",
    "number_of_variables =    ", number_of_variables, "\n"
  )

  data %>%
    dplyr::select(`Country Code`, `Indicator Code`,
                  num_range("", start_year:end_year))        %>%
    dplyr::filter(`Country Code` %in% COUNTRY_CODES)         %>%
    dplyr::filter(`Indicator Code` %in% RELATIVE_INDICATORS) %>%
    gather(year, value, num_range("", start_year:end_year))  %>%
    spread(`Indicator Code`, value, convert = TRUE)          %>%
    dplyr::mutate(year = as.integer(year))                   %>%
    dplyr::select_if( . %>% { !all(is.na(.) | (. == 0)) })   ->
    data_2

  data_2                                  %>%
    dplyr::select(-`Country Code`, -year) %>%
    is.na                                 %>%
    colMeans                              %>%
    { . < var_na_fraction }               ->
    col_idx

  browser()
  if (number_of_variables != sum(col_idx))
    stop("number_of_variables = ", number_of_variables, "\n",
         "sum(col_idx) = ", sum(col_idx))

  data_3 <- data_2[, c(1:2, which(col_idx) + 2)]

  data.table::setDT(data_3)
  country_na_frac <- data_3[, mean(is.na(.SD)), by = `Country Code`]
  data.table::setkey(country_na_frac)
  country_na_frac <- country_na_frac[V1 < cntry_na_fraction]

  stopifnot(n_countries == sum(country_na_frac$V1 < cntry_na_fraction))

  data.table::setkey(data_3, `Country Code`)
  data_3 <- data_3[country_na_frac$`Country Code`]

  scaled_matrix <- scale(as.matrix(data_3[, -c("Country Code", "year")]))
  country_year <- data_3[, c("Country Code", "year")]

  list(scaled_matrix = scaled_matrix,
       country_year = country_year)
}

pad_zeros <- function(x, n) {
  res <- rep(0, times = n)
  res[1:length(x)] <- x
  res
}

## PCA gives 10 as a true dimensionality (90% of explained variance)
gapfill_data <- function (x, ndim = 10) {
  print(paste("Ncol:", ncol(x)))
  print(paste("Nrow:", nrow(x)))
  ## This may fail if there are too many dimensions, optimum is putting the
  ## actual dimensionality as nPcs which we do not know at this time.
  ppca_res <- pcaMethods::pca(x, "ppca", nPcs = ndim, completeObs = TRUE)
  ppca_res@completeObs
}

get_knn <- function(x) 5 # { ceiling(nrow(x) / 20) }

dimred_pca <- function (x) {
  xe <- dimRed::embed(x, "PCA", ndim = ncol(x))
  xd <- dimRed::getData(dimRed::getDimRedData(xe))
  x_cor <- cor(x, xd[, 1:5])

  list(cor = x_cor, dim_red = xe)
}

dimred_iso <- function (x, knn = get_knn(x)) {
  for (i in 0:5) {
    xe <- try(dimRed::embed(x, "Isomap", ndim = min(ncol(x), 20),
                            knn = knn + 5 * i, get_geod = TRUE))
    if (!inherits(xe, "try-error")) break
  }
  xd <- dimRed::getData(dimRed::getDimRedData(xe))
  geod <- dimRed::getOtherData(xe)$geod
  x_cor <- cor(x, xd[, 1:5])

  list(cor = x_cor, dim_red = xe, d_geo = geod)
}

dimred_pw_iso <- function(x, knn = get_knn(x), ciso = FALSE,
                          regularize = 0, is_dist = FALSE, data) {

  xd <- pw_isomap_holes(x, knn = knn, out_dim = 20, ciso = ciso,
                        regularize = regularize, is_dist = is_dist)

  x_cor <- cor(
    if (is_dist) data else x,
    xd$y[, 1:5], use = "pairwise.complete.obs")
  xe <- dimRed::dimRedResult(data = dimRedData(data = xd$y),
                             org.data = x,
                             method = "Piecwise Isomap",
                             pars = list(knn = knn))

  list(cor = x_cor, dim_red = xe, d_geo = xd$d_geo)
}

euc_dist_approx <- function(m) {
  mtm <- Matrix::tcrossprod(m)
  sq <- rowSums(m * m)
  dd <- outer(sq, sq, "+") - 2 * mtm
  dd <- as.dist(dd)
  dd[dd < 0] <- 0
  sqrt(dd)
}

euc_dist_fast <- function(m) {
  wordspace::dist.matrix(m, method = "euclidean", as.dist = TRUE)
}

quality_pca <- function(x, y,
                        dfun = dist,
                        dmax = ncol(dimRed::getData(dimRed::getDimRedData(y)))) {
  y_data <- dimRed::getData(dimRed::getDimRedData(y))
  d_data <- as.vector(dfun(x))

  on.exit(cat("\n"))
  sapply(seq_len(dmax), function (i) {
    cat("\r", i, "/", dmax)
    1 - ( cor(d_data, as.vector(dfun(y_data[, 1:i, drop = FALSE]))) ^ 2 )
  })
}

quality_iso <- function(x, y,
                        dfun = dist,
                        dmax = ncol(getData(getDimRedData(y)))) {
  knn <- dimRed::getPars(y)$knn
  y_data <- dimRed::getData(dimRed::getDimRedData(y))
  ## as.vector(NULL) is NULL
  d_geo <- as.vector(dimRed::getOtherData(y)$dgeo)
  if (is.null(d_geo)) {
    d_geo <- as.vector(as.dist(igraph::distances(dimRed:::makeKNNgraph(
      x = x, k = knn
    ))))
  }

  on.exit(cat("\n"))
  sapply(seq_len(dmax), function (i) {
    cat("\r", i, "/", dmax)
    1 - ( cor(d_geo, as.vector(dfun(y_data[, 1:i, drop = FALSE]))) ^ 2 )
  })
}

quality_pw_iso <- function(x, y, d_geo = NULL,
                           dfun = dist,
                           dmax = ncol(dimRed::getData(dimRed::getDimRedData(y))),
                           ciso = FALSE) {
  y_data <- dimRed::getData(dimRed::getDimRedData(y))

  if (is.null(d_geo)) {
    knn <- dimRed::getPars(y)$knn
    d_geo <- pw_geod_holes(x, knn = knn, ciso = ciso)
  } else {
    message("d_geo already calculated, ignoring ciso")
  }
  d_geo <- as.vector(as.dist(d_geo))

  if (anyNA(d_geo))
    stop("There are NAs in the final distance matrix, ",
         "this should not have happened, something went wrong.")

  on.exit(cat("\n"))
  sapply(seq_len(dmax), function (i) {
    cat("\r", i, "/", dmax)
    1 - ( cor(d_geo, as.vector(dfun(y_data[, 1:i, drop = FALSE]))) ^ 2 )
  })
}

scale_min_max <- function (x, min = 0, max = 1, na.rm = TRUE) {
  (x - min(x, na.rm)) / max(x, na.rm) * (max - min) + min
}

## Error functions, score_3 looks best in terms of selecting large number of
## countries and variables
score_1 <- function (x, var_miss) {
  x_scaled <- lapply(x, scale_min_max)
  with(x_scaled, {
    (n_years + number_of_variables + n_countries) / 3 -
      (cntry_na_fraction + var_na_fraction) / 2
  })
}
score_2 <- function (x) {
  x_scaled <- lapply(x, scale_min_max)
  with(x_scaled, {
    (n_years * number_of_variables * n_countries) ^ (1 / 3) -
      (cntry_na_fraction * var_na_fraction) ^ (1 / 2)
  })
}
score_3 <- function (x) {
  x_scaled <- lapply(x, scale_min_max)
  with(x_scaled, {
    (number_of_variables * n_countries) ^ (1 / 2) -
      (cntry_na_fraction * var_na_fraction) ^ (1 / 2)
  })
}
score_4 <- function (x) {
  x_scaled <- lapply(x, scale_min_max)
  with(x_scaled, {
    (number_of_variables + n_countries) / 2 -
      (cntry_na_fraction + var_na_fraction) / 2
  })
}

assemble_correlations <- function(x) {

  n_var <- length(RELATIVE_INDICATORS)
  n_dim <- dim(x[[1]]$cor_iso)[2]
  n_sam <- length(x)

  res_iso <- array(
    NA_real_, dim = c(n_var, n_dim, n_sam),
    dimnames = list(RELATIVE_INDICATORS, paste0("iso", seq_len(n_dim)), 1:n_sam)
  )
  res_pca <- array(
    NA_real_, dim = c(n_var, n_dim, n_sam),
    dimnames = list(RELATIVE_INDICATORS, paste0("PC", seq_len(n_dim)), 1:n_sam)
  )

  for (i in seq_len(n_sam)) {
    idx <- RELATIVE_INDICATORS %in% rownames(x[[i]]$cor_pca)

    res_pca[idx, , i] <- x[[i]]$cor_pca
    res_iso[idx, , i] <- x[[i]]$cor_iso
  }

  list(iso = res_iso, pca = res_pca)
}


share_object_between_workers <- function (cl, varlist) {

  host_name <- cl[[1]]$host
  for (h in cl)
    if (h$host != host_name)
      stop("all workers must be on the same host")

  ## create a fifo on the node and retrieve the name
  fifo_name <- clusterEvalQ(cl[1], {
    fifo_name <- tempfile()
    system2("mkfifo", fifo_name)
    fifo_name
  })[[1]]

  ## send the very large object to one process on the node and the name of the
  ## fifo to all nodes
  list_of_vars <- mget(varlist, envir = .GlobalEnv)
  parallel::clusterExport(cl[1], "list_of_vars", envir = environment())
  parallel::clusterExport(cl, "fifo_name",       envir = environment())

  ## does the actual sharing through the fifo
  ## note that a fifo has to be opened for reading
  ## before writing on it
  for(i in 2:length(cl)) {
    parallel::clusterEvalQ(cl[i], { ff <- fifo(fifo_name, open = "r+b")   })
    parallel::clusterEvalQ(cl[1], { ff <- fifo(fifo_name, open = "w+b")
                          saveRDS(list_of_vars, ff)
                          close(ff)                                       })
    parallel::clusterEvalQ(cl[i], { list_of_vars <- readRDS(ff)
                          list2env(list_of_vars, envir = globalenv())
                          close(ff)                                       })
  }

  ## cleanup
  parallel::clusterEvalQ(cl[1], {   unlink(fifo_name)                     })

  ## check if everything is there
  presence_matrix <- Reduce(
    rbind,
    parallel::clusterEvalQ(cl, sapply(names(list_of_vars), exists))
  )
  rownames(presence_matrix) <- seq_along(cl)

  presence_matrix
}

df_to_mat_by <- function (data, val, by) {
  t(as.data.frame(split(data[[val]], data[[by]])))
}

lighten <- function(color, factor = 1.4){
  col <- col2rgb(color)
  col <- col * factor
  col <- rgb(t(as.matrix(apply(col, 1, function(x) if (x > 255) 255 else x))),
             maxColorValue = 255)
  col
}

## Trajectories arab spring
arab_spring_nations <- c("LBY", "EGY", "YEM", "SYR", "IRQ", "TUN", "BHR")
## Trajectories banking crisis
## banking_crisis_nations <- c("GRC", "IRL", "PRT", "ESP", "CYP", "ITA")
banking_crisis_nations <- c("GRC", "IRL", "PRT", "ESP", "CYP", "ITA")
russian_banking_crisis_nations <- c("RUS", "UKR", "LTU", "EST", "LVA", "KAZ",
                                    "ROU", "MDA", "AZE")
hurricanes_2005 <- c("BHS", "BRB", "DOM", "HTI", "JAM", "LCA", "TTO", "VCT")





get_hdi_data <- function () {
  hdi_data <- data.table::fread("../data/Human Development Index (HDI).csv",
                                skip = 1, header = TRUE)
  hdi_data[, `HDI Rank (2015)` := NULL]
  hdi_data <- gather(hdi_data, year, HDI, num_range("", 1990:2015))
  hdi_data$HDI <- hdi_data$HDI %>% as.numeric
  hdi_data$year <- hdi_data$year %>% as.numeric
  hdi_data$Country <- gsub("^ ", "", hdi_data$Country)

  hdi_data$`Country Code` <- NA
  for (i in seq_along(wdi_country$`Long Name`)){ 
    hdi_data$`Country Code`[hdi_data$Country == wdi_country$`Long Name`[i]] <-
      wdi_country$`Country Code`[i]
  }

  for (i in seq_along(wdi_country$`Short Name`)){
    hdi_data$`Country Code`[hdi_data$Country == wdi_country$`Short Name`[i]] <-
      wdi_country$`Country Code`[i]
}

  for (i in seq_along(wdi_country$`Table Name`)){
    hdi_data$`Country Code`[hdi_data$Country == wdi_country$`Table Name`[i]] <-
      wdi_country$`Country Code`[i]
  }

# nolint start
  hdi_data$`Country Code`[hdi_data$`Country` == "Viet Nam"]                                  <- "VNM"
  hdi_data$`Country Code`[hdi_data$`Country` == "Venezuela (Bolivarian Republic of)"]        <- "VEN"
  hdi_data$`Country Code`[hdi_data$`Country` == "The former Yugoslav Republic of Macedonia"] <- "MKD"
  hdi_data$`Country Code`[hdi_data$`Country` == "Tanzania (United Republic of)"]             <- "TZA"
  hdi_data$`Country Code`[hdi_data$`Country` == "Slovakia"]                                  <- "SVK"
  hdi_data$`Country Code`[hdi_data$`Country` == "Sao Tome and Principe"]                     <- "STP"
  hdi_data$`Country Code`[hdi_data$`Country` == "Saint Vincent and the Grenadines"]          <- "VCT"
  hdi_data$`Country Code`[hdi_data$`Country` == "Saint Lucia"]                               <- "LCA"
  hdi_data$`Country Code`[hdi_data$`Country` == "Saint Kitts and Nevis"]                     <- "KNA"
  hdi_data$`Country Code`[hdi_data$`Country` == "Bahamas"]                                   <- "BHS"
  hdi_data$`Country Code`[hdi_data$`Country` == "Bolivia (Plurinational State of)"]          <- "BOL"
  hdi_data$`Country Code`[hdi_data$`Country` == "Congo (Democratic Republic of the)"]        <- "COD"
  ## kolja
  hdi_data$`Country Code`[grepl("Ivoire",hdi_data$Country)]                                  <- "CIV"
  hdi_data$`Country Code`[hdi_data$`Country` == "Gambia"]                                    <- "GMB"
  hdi_data$`Country Code`[hdi_data$`Country` == "Hong Kong, China (SAR)"]                    <- "HKG"
  hdi_data$`Country Code`[hdi_data$`Country` == "Iran (Islamic Republic of)"]                <- "IRN"
  hdi_data$`Country Code`[hdi_data$`Country` == "Korea (Republic of)"]                       <- "KOR"
  hdi_data$`Country Code`[hdi_data$`Country` == "Kyrgyzstan"]                                <- "KGZ"
  hdi_data$`Country Code`[hdi_data$`Country` == "Micronesia (Federated States of)"]          <- "FSM"
  hdi_data$`Country Code`[hdi_data$`Country` == "Moldova (Republic of)"]                     <- "MDA"
# nolint end

  hdi_data
}

rectangle <- function(x, path, xh = NULL, xpad = 0.1, ...) {
  x1 <- if (is.null(xh)) attr(x[[path]], "height") * (1 + xpad)
        else             xh
  x2 <- 0
  y <- range(match(labels(x[[path]]), labels(x)))
  y1 <- y[1]
  y2 <- y[2]
  rect(x1, y1, x2, y2, ...)
}

rectangle_center <- function(x, path, xh = NULL) {
  x1 <- if (is.null(xh)) attr(x[[path]], "height") * (1 + xpad)
        else             xh
  x2 <- 0
  y <- range(match(labels(x[[path]]), labels(x)))
  y1 <- y[1]
  y2 <- y[2]

  return(c( (x1 + x2) / 2, (y1 + y2) / 2 ))
}

labeled_rect <- function(x, path, labels, xh = NULL, xpad = 0.1, ...) {
  rectangle(x, path, xh, xpad, ...)
  cent <- rectangle_center(x, path, xh)
  text(cent[1], cent[2], labels = labels)
}

#' CREATE the data set: how many countries and variables remain for a certain
#' maximally allowed fraction of missing values
make_data_sensitivity <- function(
  x,
  country_codes,
  relative_indicators,
  year_range,
  var_na_fractions   = seq(0.05, 0.7, by = 0.1),
  cntry_na_fractions = seq(0.05, 0.7, by = 0.1),
  .cluster           = CLUSTER
) {
  message("make_data_sensitivity")
  message(Sys.time(), ": initial data transform")
  x %>%
    dplyr::select(`Country Code`, `Indicator Code`,
                  num_range("", year_range))                 %>%
    dplyr::filter(`Country Code` %in% country_codes)         %>%
    dplyr::filter(`Indicator Code` %in% relative_indicators) %>%
    gather(year, value, num_range("", year_range))           %>%
    spread(`Indicator Code`, value, convert = TRUE)          %>%
    dplyr::mutate(year = as.integer(year))                   %>%
    dplyr::select_if( . %>% { !all(is.na(.) | (. == 0)) })   ->
    wdi_data_transformed

  data.table::setDT(wdi_data_transformed)

  message(Sys.time(), ": calculating parameter combinations")
  param_space <- list()
  for (start_year in year_range) {
    for (end_year in start_year:max(year_range)) {
      for (var_na_fraction in var_na_fractions) {
        for (cntry_na_fraction in cntry_na_fractions)
          param_space[[length(param_space) + 1]] <-
            list(start_year        = start_year,
                 end_year          = end_year,
                 var_na_fraction   = var_na_fraction,
                 cntry_na_fraction = cntry_na_fraction)
      }
    }
  }
  message("number of parameter combinations: ", length(param_space))

  message(Sys.time(), ": Set up cluster")
  cl <- parallel::makeCluster(.cluster, outfile = "~/make_data_sensitivity.log")
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, library(tidyverse))
  parallel::clusterEvalQ(cl, library(data.table))
  ## This is important, else data.table will use 48 threads:
  parallel::clusterEvalQ(cl, data.table::setDTthreads(1))
  parallel::clusterExport(cl, "wdi_data_transformed",    envir = environment())
  parallel::clusterExport(cl, "get_number_of_variables", envir = environment())
  parallel::clusterExport(cl, "param_space",             envir = environment())
  message(Sys.time(), ": calculating")
  res <- parallel::parLapplyLB( cl, seq_along(param_space), function(i) {
    do.call(get_number_of_variables, param_space[[i]])
  } )

  message(Sys.time(), ": post processing")
  res <- data.table::rbindlist(res)

  ## calcule the goodness of missing value scores
  res$score_1 <- score_1(res)
  res$score_2 <- score_2(res)
  res$score_3 <- score_3(res)
  res$score_4 <- score_4(res)

  ## We need more rows than columns:
  res <- res[res$n_countries * res$n_years > res$number_of_variables,]

  message(Sys.time(), ": DONE")
  res
}

make_plot_n_var_vs_n_cntry <- function(x) {

  print(
    ggplot(subset(x, score_1 >= sort(score_1, decreasing = TRUE)[1000]),
           aes(x = n_countries, y = number_of_variables,
               color = score_1)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_2 >= sort(score_2, decreasing = TRUE)[1000]),
           aes(x = n_countries, y = number_of_variables,
               color = score_2)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_3 >= sort(score_3, decreasing = TRUE)[1100]),
           aes(x = n_countries, y = number_of_variables,
               size = cntry_na_fraction, shape = factor(var_na_fraction),
               color = score_3)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_3 >= sort(score_3, decreasing = TRUE)[1000]),
           aes(x = n_countries, y = number_of_variables,
               size = cntry_na_fraction, shape = factor(var_na_fraction),
               color = score_3)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_3 >= sort(score_3, decreasing = TRUE)[890]),
           aes(x = n_countries, y = number_of_variables,
               size = cntry_na_fraction, shape = factor(var_na_fraction),
               color = score_3)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_3 >= sort(score_3, decreasing = TRUE)[100]),
           aes(x = n_countries, y = number_of_variables,
               size = cntry_na_fraction, shape = factor(var_na_fraction),
               color = score_3)) +
    geom_point()
  )

  print(
    ggplot(x, aes(x = n_countries, y = number_of_variables, color = score_3)) +
    geom_point()
  )

  print(
    ggplot(subset(x, score_4 >= sort(score_4, decreasing = TRUE)[1000]),
           aes(x = n_countries, y = number_of_variables,
               color = score_4)) +
    geom_point()
  )

  print(
    ggplot(subset(x),
           aes(x = n_countries, y = number_of_variables,
               shape = factor(var_na_fraction),
               size = n_years,
               color = start_year)) +
    geom_point()
  )

  print(
    ggplot(subset(x),
           aes(x = n_years, y = number_of_variables,
               shape = factor(var_na_fraction),
               alpha = n_countries,
               color = start_year)) +
    geom_point()
  )

}

make_qual_comp_res_inner <- function (i) {
  tryCatch({
    print(paste("i:", i))
    i <- ord_idx[i]
    print(paste("Index: ", i))
    ##
    dta      <- try( create_data(i)                                )
    dta2     <- try( gapfill_data(dta$scaled_matrix, ndim = 20)    )
    dta_pca  <- try( dimred_pca(dta2)                              )
    qual_pca <- try( quality_pca(dta2, dta_pca$dim_red, dmax = 20) )
    ##
    ## Search for a good value for k:
    best_iso <- best_iso_qual <- Inf
    knn <- 5
    while (TRUE) {
      message("k = ", knn)
      curr_iso      <- try( dimred_iso(dta2, knn)               )
      curr_iso_qual <- try( quality_iso(dta2, curr_iso$dim_red,
                                        dfun = euc_dist_approx) )
      ##
      if (!inherits(curr_iso, "try-error")) {
        ##
        ## Only compare first dimensions, continue to increase knn until
        ## quality gets worse.
        if (curr_iso_qual[1] > best_iso_qual[1]) break
        ##
        best_iso_qual <- curr_iso_qual
        best_iso <- curr_iso
      }
      ##
      knn <- knn + 5
      ##
      if (knn > (ncol(dta2) / 2)) {
        message("i = ", i, " -- Isomap failed!!")
        break
      }
    }
    ##
    list(idx = i, qual_pca = qual_pca, qual_iso = best_iso_qual,
         d_geo  = best_iso$d_geo, cntry_year = dta$country_year,
         cor_pca = try(dta_pca$cor), cor_iso = try(best_iso$cor))
  },
  error = function (e) {
    message("i = ", i)
    print(e)
    as.character(e)
  })
}

make_qual_comp_res <- function (x, data, data_sensitivity,
                                .cluster = CLUSTER) {
  message("make_qual_comp_res")
  message(Sys.time(), ": set up cluster")

  cl <- parallel::makeCluster(.cluster, outfile = "make_qual_comp_res.log")
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, library(tidyverse))
  parallel::clusterEvalQ(cl, library(data.table))
  parallel::clusterEvalQ(cl, library(pcaMethods))
  parallel::clusterEvalQ(cl, library(dimRed))
  ## This is important, else data.table will use 48 threads per worker:
  parallel::clusterEvalQ(cl, data.table::setDTthreads(1))
  parallel::clusterExport(cl, "data", envir = environment())
  parallel::clusterExport(cl, "data_sensitivity", envir = environment())
  parallel::clusterExport(cl, "euc_dist_approx")
  parallel::clusterExport(cl, "get_knn")
  parallel::clusterExport(cl, "create_data")
  parallel::clusterExport(cl, "gapfill_data")
  parallel::clusterExport(cl, "dimred_pca")
  parallel::clusterExport(cl, "dimred_iso")
  parallel::clusterExport(cl, "quality_pca")
  parallel::clusterExport(cl, "quality_iso")
  parallel::clusterExport(cl, "COUNTRY_CODES")
  parallel::clusterExport(cl, "RELATIVE_INDICATORS")
  ord_idx <- order(data_sensitivity$score_3, decreasing = TRUE)
  parallel::clusterExport(cl, "ord_idx", envir = environment())

  message(Sys.time(), ": Starting Quality Comparison")
  ## standard parallel version
  qual_comp_res <- parallel::clusterApplyLB(cl, 1:1000,
                                            make_qual_comp_res_inner)


  message(Sys.time(), ": DONE")

  message(object.size(qual_comp_res))

  qual_comp_res
}

## Do some error checking and diagnostics
qual_comp_res_stats <- function(x) {
  which(sapply(qual_comp_res, function(x) length(x) != 7)) %>%
    message("errors at indices (should be empty): ", .)
  sapply(qual_comp_res, . %>% { .$cntry_year$year } %>% range) %>%
    range %>%
    paste(collapse = " ") %>%
    message("year range: ", .)
  sapply(qual_comp_res, . %>% { .$cor_pca } %>% rownames) %>%
    unlist %>%
    unique %>%
    length %>%
    message("number of variables: ", .)
  sapply(qual_comp_res, . %>% { .$cntry_year$`Country Code`}) %>%
    unlist %>%
    unique %>%
    length %>%
    message("number of countries: ", .)
}

make_cons_iso_pca <- function (qual_comp_res,
                               wdi_data,
                               knns = c(25, 50, 100, 250, 350),
                               n_used_vars = N_USED_VARS,
                               years = YEARS,
                               country_names = COUNTRY_NAMES,
                               used_var_names = USED_VAR_NAMES,
                               ciso = TRUE) {
  message("make_cons_iso_pca")
  message(Sys.time(), ": START")

  force(ciso)

  dummy_data <- list()
  for (i in seq_along(qual_comp_res)) {
    dummy_data[[i]] <- qual_comp_res[[i]]$cntry_year
    dummy_data[[i]][[paste0("p", i)]] <- TRUE
  }

  occurrence_table <- Reduce(function(x, y)
    data.table:::merge.data.table(x, y, by = c("Country Code", "year"),
                                  all = TRUE),
    dummy_data)
  nobs <- nrow(occurrence_table)

  nvars <- sapply(qual_comp_res, function (x) nrow(x$cor_iso))

  message(Sys.time(), ": Merging distance matrices")
  d_geo <- array(NA, dim = c(nobs, nobs))

  ## 1. scale the distance matrices according to the number of variables
  ##    d * sqrt(N_USED_VARS / nvar)
  ## 2. take the maximum for each distance
  for (i in seq_along(qual_comp_res)) {
    cat("\ri =", i)
    idxs <- which(occurrence_table[[2 + i]])
    d_geo_local_scaled <-
      as.matrix(qual_comp_res[[i]]$d_geo) * sqrt(n_used_vars / nvars[i])
    d_geo[idxs, idxs] <-
      pmax(d_geo[idxs, idxs], d_geo_local_scaled, na.rm = TRUE)
  }
  cat("\n")

  message(Sys.time(), ": filtering wdi_data")
  ## We do PCA later, because we have to filter the exact data we used on the
  ## Isomap
  wdi_data                                                             %>%
    dplyr::select(
             c("Country Code", "Indicator Code", as.character(years))) %>%
    dplyr::filter(`Country Code` %in% country_names)                   %>%
    dplyr::filter(`Indicator Code` %in% used_var_names)                %>%
    gather(year, value, as.character(years))                           %>%
    spread(`Indicator Code`, value, convert = TRUE)                    %>%
    dplyr::mutate(year = as.integer(year))                             ->
    wdi_data_cons

  data.table::setDT(occurrence_table)
  data.table::setDT(wdi_data_cons)
  data.table::setkey(occurrence_table, "Country Code", "year")
  data.table::setkey(wdi_data_cons, "Country Code", "year")
  wdi_data_cons <- wdi_data_cons[occurrence_table[, 1:2]]

  message(Sys.time(), ": gapfilling")
  wdi_data_cons_scaled <- scale(as.matrix(wdi_data_cons[, -1:-2]))
  wdi_data_cons_gf <- gapfill_data(wdi_data_cons_scaled, ndim = 80)

  message(Sys.time(), ": PCA")
  wdi_data_cons_pca <- dimred_pca(wdi_data_cons_gf)
  wdi_data_qual_cons_pca <- quality_pca(
    wdi_data_cons_gf, wdi_data_cons_pca$dim_red,
    dfun = euc_dist_approx, dmax = 20)

  message(Sys.time(), ": Isomap")
  wdi_data_cons_iso <- lapply(knns, function (k)
    dimred_pw_iso(d_geo, knn = k, ciso = ciso, is_dist = TRUE,
                  data = wdi_data_cons[, -c("Country Code", "year")])
    )
  wdi_data_qual_cons_iso <- lapply(wdi_data_cons_iso, function (x)
    quality_pw_iso(d_geo = x$d_geo, y = x$dim_red, dfun = euc_dist_approx)
  )

  message(Sys.time(), ": Done")
  return(list(knns                   = knns,
              occurrence_table       = occurrence_table,
              wdi_data_cons          = wdi_data_cons,
              wdi_data_cons_pca      = wdi_data_cons_pca,
              wdi_data_cons_iso      = wdi_data_cons_iso,
              wdi_data_qual_cons_pca = wdi_data_qual_cons_pca,
              wdi_data_qual_cons_iso = wdi_data_qual_cons_iso))
}

plot_cons_iso_pca <- function (cons_iso_pca) {
  knns <- cons_iso_pca$knns

  plot(cons_iso_pca$wdi_data_qual_cons_pca, type = "b", ylim = 0:1, col = 1)
  for (i in seq_along(cons_iso_pca$wdi_data_qual_cons_iso)) {
    points(cons_iso_pca$wdi_data_qual_cons_iso[[i]], type = "b", col = i + 1)
  }
  legend("topright", legend = c("PCA", knns), pch = 1,
         col = seq_len(length(cons_iso_pca$wdi_data_qual_cons_iso) + 1))
  abline(h = 0.1)
}

## This creates the main data structure for later analysis
make_wdi_data_cons_df <- function(knn = 50,
                                  wdi_data_cons     = wdi_data_cons,
                                  wdi_data_cons_iso = wdi_data_cons_iso,
                                  wdi_data_cons_pca = wdi_data_cons_pca,
                                  wdi_country       = wdi_country,
                                  occurrence_table  = occurrence_table,
                                  hdi_data          = hdi_data) {

  ## merge previously calculated data
  wdi_data_cons_df <- data.table(
    wdi_data_cons[occurrence_table[, 1:2]],
    wdi_data_cons_iso[[which(knns == knn)]]$dim_red@data@data,
    wdi_data_cons_pca$dim_red@data@data[, 1:20],
    check.names = FALSE
  )

  ## some countries do not have the entire time series, expand the data so that
  ## values are NA, but all country/year_range combinations are there
  all_cty_yr_dt <- expand.grid(
    `Country Code` = unique(wdi_data_cons_df$`Country Code`),
    year = unique(wdi_data_cons_df$year))
  wdi_data_cons_df <-
    wdi_data_cons_df[all_cty_yr_dt, on = c("Country Code", "year")]

  ## Add country meta data
  wdi_data_cons_df <- merge(
    wdi_country[, c("Country Code", "Short Name", "Region", "Income Group"),
                with = FALSE],
    wdi_data_cons_df,
    by = "Country Code", all.x = FALSE, all.y = TRUE)

  ## modify region to mark former soviet states
  former_soviet_short_name <- c(
    ## The former soviet states
    c("Armenia", "Azerbaijan", "Belarus", "Estonia",
      "Georgia", "Kazakhstan", "Kyrgyz Republic",
      "Latvia", "Lithuania", "Moldova", "Russia",
      "Tajikistan", "Turkmenistan", "Ukraine",
      "Uzbekistan"),
    ## allied states and their successors, only eastern Europe (e.g. no Cuba,
    ## Mongolia, etc.)
    c("Albania", "Bulgaria", "Czech Republic",
      "Slovak Republic", "Hungary", "Poland",
      "Romania", "Croatia", "Slovenia",
      "Macedonia", "Bosnia and Herzegovina",
      "Kosovo", "Serbia", "Montenegro"))
  wdi_data_cons_df$RegionSoviet <- wdi_data_cons_df$Region
  wdi_data_cons_df$RegionSoviet[
    wdi_data_cons_df$`Short Name` %in% former_soviet_short_name] <- "Post-Soviet"

  ## add hdi data
  wdi_data_cons_df <- merge(wdi_data_cons_df, hdi_data,
                            by = c("Country Code", "year"),
                            all.x = TRUE, all.y = FALSE)

  ## add data so that it can be used with rworldmap
# nolint start
  wdi_data_cons_df$map_name <- wdi_data_cons_df$`Short Name`
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "The Bahamas"]                    <- "Bahamas"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Côte d'Ivoire"]                  <- "Ivory Coast"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Antigua and Barbuda"]            <- "Antigua"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Dem. Rep. Congo"]                <- "Democratic Republic of the Congo"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Congo"]                          <- "Republic of Congo"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Cabo Verde"]                     <- "Cape Verde"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "United Kingdom"]                 <- "UK"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "The Gambia"]                     <- "Gambia"
  ## wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Hong Kong SAR, China"]        <-
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Kyrgyz Republic"]                <- "Kyrgyzstan"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Korea"]                          <- "South Korea" #!!!! verfied
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Lao PDR"]                        <- "Laos"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "St. Lucia"]                      <- "Saint Lucia"
  ## wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Macao SAR, China"]            <-
  ## wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "West Bank and Gaza"]          <-
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "São Tomé and Principe"]          <- "Sao Tome and Principe"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Slovak Republic"]                <- "Slovakia"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Syrian Arab Republic"]           <- "Syria"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "Trinidad and Tobago"]            <- "Trinidad"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "United States"]                  <- "USA"
  wdi_data_cons_df$map_name[wdi_data_cons_df$map_name == "St. Vincent and the Grenadines"] <- "Saint Vincent"
# nolint end

    ## Flip axes so that positive means the same as in human terms
    ## NOTE: If the underlying data changes, these have to be checked
    if (cor(wdi_data_cons_df$HDI,
            wdi_data_cons_df$iso1,
            use = "pairwise.complete.obs") < 0) {
        message("Flipping Axis 1")
        wdi_data_cons_df$iso1 <- -wdi_data_cons_df$iso1
    }

    if (cor(wdi_data_cons_df$SL.TLF.TOTL.FE.ZS,
            wdi_data_cons_df$iso2,
            use = "pairwise.complete.obs") < 0) {
        message("Flipping Axis 2")
        wdi_data_cons_df$iso2 <- -wdi_data_cons_df$iso2
    }

    if (cor(wdi_data_cons_df$SL.EMP.TOTL.SP.ZS,
            wdi_data_cons_df$iso3,
            use = "pairwise.complete.obs") < 0) {
        message("Flipping Axis 3")
        wdi_data_cons_df$iso3 <- -wdi_data_cons_df$iso3
    }

    if (cor(wdi_data_cons_df$VC.IHR.PSRC.P5,
            wdi_data_cons_df$iso4,
            use = "pairwise.complete.obs") > 0) {
        message("Flipping Axis 4")
        wdi_data_cons_df$iso4 <- -wdi_data_cons_df$iso4
    }

    if (cor(wdi_data_cons_df$EG.USE.COMM.GD.PP.KD,
            wdi_data_cons_df$iso5,
            use = "pairwise.complete.obs") > 0) {
        message("Flipping Axis 5")
        wdi_data_cons_df$iso5 <- -wdi_data_cons_df$iso5
    }

  return(wdi_data_cons_df)
}

topic_report <- function(
  wdi_series = wdi_series, used_var_names =  USED_VAR_NAMES
) {
  setkey(wdi_series, "Indicator Code")

  ## Get the topics
  wdi_series$Topic %>%
    as.character %>%
    unique %>%
    strsplit(":") %>%
    sapply(function(x) x[1]) %>%
    unique %>%
    sort

  wdi_series[`Indicator Code` %in% USED_VAR_NAMES]$Topic %>%
    as.character %>%
    unique %>%
    strsplit(":") %>%
    sapply(function(x) x[1]) %>%
    unique %>%
    sort

  ## Get the topics with a count
  message("Topic Table before selection")
  wdi_series$Topic %>%
    as.character %>%
    strsplit(":") %>%
    sapply(function(x) x[1]) %>%
    table %>%
    print

  message("Topic Table after selection")
  wdi_series[`Indicator Code` %in% USED_VAR_NAMES]$Topic %>%
    as.character %>%
    strsplit(":") %>%
    sapply(function(x) x[1]) %>%
    table %>%
    print
}

plot_the_main_gradients <- function(wdi_data_cons_df, prim_font_size = 15) {

  color_scheme <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                    "#66a61e", "#e6ab02", "#a6761d", "#666666")

  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = iso1, y = iso2, label = paste(`Short Name`, year),
               group = `Country Code`, color = RegionSoviet)) +
    geom_point(size = .5, alpha = .8) +
    geom_path(size = 0.1) +
    annotate("text",
             x = wdi_data_cons_df[`Short Name` == "Singapore" &
                                  `year` %in% c(1990, 2016), ]$iso1,
             y = wdi_data_cons_df[`Short Name` == "Singapore" &
                                  `year` %in% c(1990, 2016), ]$iso2,
             label = c("Singapore 1990", "Singapore 2016"),
             hjust = c(-0.02, 0.1), vjust = c(0.5, -0.29),
             size = 5.0) +
    annotate("point",
             x = wdi_data_cons_df[`Short Name` == "Singapore" &
                                  `year` %in% c(1990, 2016), ]$iso1,
             y = wdi_data_cons_df[`Short Name` == "Singapore" &
                                  `year` %in% c(1990, 2016), ]$iso2,
             color = "black", size = 4, shape = "cross") +
    scale_color_manual(values = color_scheme,
                       guide = guide_legend(title = "Region",
                                            override.aes = list(size = 1.5))) +
    labs(x = "Dimension 1", y = "Dimension 2") +
    theme_classic() +
    theme(legend.position = c(0, 0),
          legend.justification = c(0, 0),
          legend.background = element_blank(),
          legend.title = element_text(size = prim_font_size - 5),
          legend.text = element_text(size = prim_font_size - 5),
          panel.background = element_rect(colour = "grey50"),
          ## axis.line = element_line(colour = "grey50"),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          axis.text = element_text(colour = "grey50", size = prim_font_size),
          axis.title = element_text(size = prim_font_size))
  )

  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = iso1, y = iso3, label = paste(`Short Name`, year),
               group = `Country Code`, color = SL.EMP.1524.SP.FE.ZS)) +
    geom_point(size = .5, alpha = .8) +
    geom_path(size = 0.1) +
    scale_color_distiller(palette = "Spectral", na.value = "#FFFFFF00") +
    labs(x = "Dimension 1", y = "Dimension 3") +
    guides(
      color = guide_colourbar(
        title = "Employment to\npopulation ratio,\nages 15-24,\nfemale (%)")) +
    theme_classic() +
    theme(legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.background = element_blank(),
          legend.title = element_text(size = prim_font_size - 5),
          legend.text = element_text(size = prim_font_size - 5),
          panel.background = element_rect(colour = "grey50"),
          ## axis.line = element_line(colour = "grey50"),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          axis.text = element_text(colour = "grey50", size = prim_font_size),
          axis.title = element_text(size = prim_font_size))
  )
}

make_dcor_var_iso_cons <- function(wdi_data_cons_df = wdi_data_cons_df,
                                   used_var_names = USED_VAR_NAMES,
                                   .cluster = CLUSTER) {

  message("make_dcor_var_iso_cons")
  message(Sys.time(), ": START")
  var_comb <- expand.grid(var_name = used_var_names,
                          iso_name = paste0("iso", 1:5),
                          stringsAsFactors = FALSE)

  message(Sys.time(), ": Setting up cluster")
  cl <- parallel::makeCluster(.cluster, outfile = "~/make_dcor_var_iso_cons.log")
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, library(energy))
  parallel::clusterExport(cl, "wdi_data_cons_df", envir = environment())
  parallel::clusterExport(cl, "var_comb", envir = environment())

  message(Sys.time(), ": calculating")
  res <- parLapplyLB(cl, 1:nrow(var_comb), function (i) {
    message("i: ", i, " / ", nrow(var_comb))
    idx <- !is.na(wdi_data_cons_df[[var_comb$var_name[i]]])
    energy::dcor(wdi_data_cons_df[[var_comb$var_name[i]]][idx],
                 wdi_data_cons_df[[var_comb$iso_name[i]]][idx])
  })

  message(Sys.time(), ": post processing")
  wdi_data_cons_dcor <- matrix(
    NA_real_, length(used_var_names), 5,
    dimnames = list(used_var_names, paste0("iso", 1:5))
  )

  for (i in seq_len(nrow(var_comb))) {
    wdi_data_cons_dcor[var_comb$var_name[i],
                       var_comb$iso_name[i]] <- res[[i]]
  }

  message(Sys.time(), ": DONE")
  return(wdi_data_cons_dcor)
}

make_3d_visualization_data <- function (wdi_data_cons_df = wdi_data_cons_df,
                                        wdi_series = wdi_series,
                                        used_var_names = USED_VAR_NAMES,
                                        .overwrite = FALSE) {
  OUTFILE <- "../data/consolidated_dimred_data.json"
  if (!file.exists(OUTFILE) || .overwrite) {

    used_var_full_names <- sapply(USED_VAR_NAMES, function (x) wdi_series[`Indicator Code` == x]$`Indicator Name`) %>% unname

    cons_data_json_list <- list()
    cons_data_json_list[["Names"]]          <-     unname(df_to_mat_by(wdi_data_cons_df, "Short Name",   "Country Code")[, 1])
    cons_data_json_list[["Codes"]]          <-     unname(df_to_mat_by(wdi_data_cons_df, "Country Code", "Country Code")[, 1])
    cons_data_json_list[["Region"]]         <-     unname(df_to_mat_by(wdi_data_cons_df, "RegionSoviet", "Country Code")[, 1])
    cons_data_json_list[["Income Group"]]   <-     unname(df_to_mat_by(wdi_data_cons_df, "Income Group", "Country Code")[, 1])
    cons_data_json_list[["Years"]]          <- as.integer(df_to_mat_by(wdi_data_cons_df, "year", "Country Code")[1, ])
    cons_data_json_list[["Variable Names"]] <- c(paste0("iso", 1:20), paste0("PC", 1:20), "HDI", used_var_names)
    cons_data_json_list[["Full Variable Names"]] <- c(paste("Isomap", 1:20),
                                                      paste("Principal Component", 1:20),
                                                      "Human Development Index",
                                                      used_var_full_names)

    for (i in seq_along(cons_data_json_list[["Variable Names"]])) {
      var <- cons_data_json_list[["Variable Names"]][i]
      full_var <- cons_data_json_list[["Full Variable Names"]][i]
      cons_data_json_list[[ full_var ]] <- df_to_mat_by(wdi_data_cons_df, var, "Country Code")
    }

    ## for (var in cons_data_json_list[["Variable Names"]]) {
    ##   cons_data_json_list[[var]]            <- df_to_mat_by(wdi_data_cons_df, var, "Country Code")
    ## }

    write(RJSONIO::toJSON(cons_data_json_list), OUTFILE)

  }

}

make_cons_cor_table_data <-
  function (dcor_var_iso_cons = dcor_var_iso_cons,
            wdi_series = wdi_series,
            .overwrite = FALSE) {
  OUTFILE <- "../data/consolidated_cor_table_table.json"
  if (!file.exists(OUTFILE) | .overwrite) {
    dcor_var_iso_cons %>%
      data.table::as.data.table(keep.rownames = "Indicator Code") %>%
      { .[wdi_series, on = "Indicator Code"] } %>%
      { .[, c("Short definition", "Unit of measure", "Periodicity",
              "Base Period", "Other notes", "Notes from original source",
              "Related source links", "Other web links",
              "Related indicators", "V21", "License Type") := NULL] } %>%
      RJSONIO::toJSON(.) %>%
      write(OUTFILE)
  }

}

make_residual_variance_lists <- function(qual_comp_res = qual_comp_res,
                                         wdi_data_cons_df = wdi_data_cons_df,
                                         used_var_names = USED_VAR_NAMES) {
  max_dim <- max(sapply(qual_comp_res, function(x) length(x$qual_pca)))
  max_res_var_1 <- max(sapply(qual_comp_res, function(x)
    ifelse(is.numeric(x$qual_pca[1]), x$qual_pca[1], 0))
  )

  pca_res <- lapply(qual_comp_res, function(x) {
    pad_zeros(if (is.numeric(x$qual_pca)) x$qual_pca else 0, max_dim)
  })
  iso_res <- lapply(qual_comp_res, function(x) {
    pad_zeros(if (is.numeric(x$qual_iso)) x$qual_iso else 0, max_dim)
  })

  pca_res_r <- Reduce(rbind, pca_res)
  iso_res_r <- Reduce(rbind, iso_res)
  pca_res_r <- pca_res_r[apply(pca_res_r, 1, function(x) !all(x == 0)), ]
  iso_res_r <- iso_res_r[apply(iso_res_r, 1, function(x) !all(x == 0)), ]

  median_pca_res <- pca_res_r %>% apply(2, median)
  median_iso_res <- iso_res_r %>% apply(2, median)

  hdi_res_var <- 1 - (cor(
    as.vector(dist(wdi_data_cons_df$HDI)),
    as.vector(dist(scale(wdi_data_cons_df[, used_var_names, with = FALSE]))),
    use = "pairwise.complete.obs"
  ) ^ 2)

  return(list(iso = iso_res, pca = pca_res,
              median_iso = median_iso_res,
              median_pca = median_pca_res,
              hdi = hdi_res_var))
}

plot_residual_variances <- function (
    residual_sample_variances = residual_sample_variances,
    wdi_data_qual_cons_pca = wdi_data_qual_cons_pca,
    wdi_data_qual_cons_iso = wdi_data_qual_cons_iso
  ) {
  old_par <- par()
  par(mar = c(3.5, 3.75, 0, 0) + 0.1, las = 1, mgp = c(2.5, 0.25, 0))
  on.exit(par(old_par))

  max_x <- 14

  pca_res <- residual_sample_variances$pca
  iso_res <- residual_sample_variances$iso
  median_res_var_pca <- residual_sample_variances$median_pca
  median_res_var_iso <- residual_sample_variances$median_iso
  hdi_res_var <- residual_sample_variances$hdi

  max_res_var_1 <- 1

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
         lwd = 4, col = lighten("#1b9e77", 0.8))
  points(wdi_data_qual_cons_iso[[2]], type = "b",
         lwd = 4, col = lighten("#d95f02", 0.8))
  text(1, wdi_data_qual_cons_pca[1],      "PCA",
       pos = 4, offset = 0.5, xpd = TRUE, cex = 2)
  text(1, wdi_data_qual_cons_iso[[2]][1], "Ensemble Isomap",
       pos = 4, offset = 0.5, xpd = TRUE, cex = 2)
  text(max_x + 0.75,         0.1, "10% Residual Variance", cex = 1.8,
       adj = c(1.1, -0.5), xpd = TRUE)
  abline(h = 0.1, col = "gray50", lwd = 2)
  text(max_x + 0.75, hdi_res_var, "HDI Residual Variance", cex = 1.8,
       adj = c(1.1, -0.5), xpd = TRUE)
  abline(h = hdi_res_var, col = "gray50", lwd = 2)
  par(las = 1, mgp = c(1.5, 0.75, 0))#, xpd = TRUE)
  axis(side = 1, at = 1:max_x, labels = ifelse(1:14 %% 2 == 1, 1:14, ""),
       col = "gray50", col.axis = "gray50",
       cex.axis = 1.5, lwd = 1, tcl = -0.25, lwd.ticks = 2)
  axis(side = 2, at = seq(0, 1, length.out = 6),
       col = "gray50", col.axis = "gray50",
       cex.axis = 1.5, lwd = 1, tcl = -0.25, lwd.ticks = 2)
  ## This one is for the apendix and contains more dimensions:
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
  ##
  plot(pca_res[[1]],
       ylim = c(0, max_res_var_1),
       ## xlim = c(1, max_dim),
       xlim = c(1, 19),
       ylab = "residual variance",
       xlab = "number of dimensions",
       type = "n")
  sapply(pca_res, function(x) {
    points(seq_along(x) + runif(length(x), -0.5, 0.5), x,
           col = "#00000044", pch = ".")
  })
  sapply(iso_res, function(x) {
    points(seq_along(x) + runif(length(x), -0.5, 0.5), x,
           col = "#0000ff99", pch = ".")
  })
  ## boxplot(x = pca_res_r[,1:20], grp = col(pca_res_r)[,1:20], pch = 3, cex = 0.5, add = TRUE)
  ## boxplot(x = iso_res_r[,1:20], grp = col(iso_res_r)[,1:20], pch = 3, cex = 0.5, border = "darkblue",  add = TRUE)
  abline(h = 0.1)
  legend("topright", legend = c("PCA", "Isomap"),
         lty = 1, col = c("black", "blue"))
  ##
  plot(pca_res[[1]],
       ylim = c(0, max_res_var_1),
       ## xlim = c(1, max_dim),
       xlim = c(1, 19),
       ylab = "residual variance pca - iso",
       xlab = "number of dimensions",
       type = "n")
  grid()
  for (i in 1:length(pca_res)) {
    lines(pca_res[[i]] - iso_res[[i]], col = "#00000011")
  }
}

plot_hdi_var_dcor_by_var <- function (x, by_col = "RegionSoviet",
                                      prim_font_size = 20,
                                      legend_pos = c(0, 1)) {

  color_scheme <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                    "#66a61e", "#e6ab02", "#a6761d", "#666666")

  x_title <- if (grepl("^iso", x))
               paste("Dimension", substr(x, 4, 4))
             else if (grepl("^PC", x))
               paste("PCA Dimension", substr(x, 3, 3))
             else
               x

  legend_title <- if (by_col == "RegionSoviet") "Region" else by_col

  wdi_data_cons_df_no_na <-
    wdi_data_cons_df[, c("Country Code", "year", "RegionSoviet", x, "HDI"),
                     with = FALSE] %>%
    na.exclude


  dcor_val_all <- wdi_data_cons_df_no_na %>% {
    energy::dcor(.[[x]], .$HDI)
  }

  dcor_val_cntry <-
    wdi_data_cons_df_no_na[, .(
      dcor         = energy::dcor(.SD[, x, with = FALSE], .SD$HDI),
      RegionSoviet = .SD[["RegionSoviet"]][1]
    ),
    by = `Country Code`
  ]


  dcor_val_region <-
    wdi_data_cons_df_no_na[, .(
      dcor         = energy::dcor(.SD[, x, with = FALSE], .SD$HDI),
      x = -Inf, y = Inf
    ),
    by = RegionSoviet
  ]

  density_plot <-
    if (by_col == "RegionSoviet")
      dcor_val_cntry %>%
        ggplot() +
        geom_density(aes_string(x = "dcor",
                                color = paste0("factor(", by_col, ")")),
                     adjust = 2, alpha = 0.1, size = 2) +
        geom_density(aes_string(x = "dcor"), color = "black",
                     adjust = 2, alpha = 0.1, size = 2) +
        scale_color_manual(values = color_scheme,
                           guide = guide_legend(title = "Region",
                                                override.aes = list(size = 1.5))) +
        theme_classic() +
        theme(legend.position = c(0.1, 0.9),
              legend.justification = c(0.1, 0.9),
              legend.title = element_text(size = prim_font_size),
              legend.text = element_text(size = prim_font_size),
              legend.background = element_blank(),
              panel.background = element_rect(colour = "grey50"),
              axis.line = element_blank(),
              axis.ticks = element_line(colour = "grey50"),
              axis.text = element_text(colour = "grey50", size = 15),
              axis.title = element_text(size = 20))
    else
      list()


  scatter_plot <- wdi_data_cons_df %>%
    ggplot(aes_string(x = x, y = "HDI", color = by_col, fill = by_col)) +
    geom_path(group = "Short Name") +
    annotate(
      "text", x = -Inf, y = Inf,
      size = 6,
      hjust = -0.1, vjust = 1.5,
      label = dcor_val_all %>%
        format(digits = 3) %>%
        paste("dcor =", .)
    ) + (
      if (by_col == "RegionSoviet")
        scale_color_manual(values = color_scheme,
                           guide = guide_legend(title = legend_title,
                                                override.aes = list(size = 3)))
      else
        scale_color_continuous(guide = guide_legend(title = legend_title))
    ) +
    theme_classic() +
    xlab(x_title) +
    theme(legend.title = element_text(size = prim_font_size),
          legend.text = element_text(size = prim_font_size),
          legend.position = legend_pos,
          legend.justification = legend_pos,
          legend.background = element_blank(),
          panel.background = element_rect(colour = "grey50"),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          axis.text = element_text(colour = "grey50", size = prim_font_size),
          axis.title = element_text(size = prim_font_size + 5))

  scatter_plot_facets <- wdi_data_cons_df %>%
    ggplot(aes_string(x = x, y = "HDI")) +
    geom_path(data = copy(wdi_data_cons_df)[, (by_col) := NULL]) +
    geom_path(aes_string(color = by_col, fill = by_col), group = "Short Name") +
    geom_text(
      data = dcor_val_region,
      aes(x = x, y = y, label = paste("dcor =", format(dcor, digits = 3))),
      hjust = -0.1, vjust = 1.3, size = 5
    ) + (
      if (by_col == "RegionSoviet")
        scale_color_manual(values = color_scheme,
                           guide = FALSE)
      else
        scale_color_continuous(guide = guide_legend(title = legend_title))
    ) +
    facet_wrap(~ RegionSoviet) +
    theme_classic() +
    xlab(x_title) +
    theme(legend.position = legend_pos,
          legend.justification = legend_pos,
          legend.background = element_blank(),
          panel.background = element_rect(colour = "grey50"),
          ## panel.background = element_blank(),
          strip.text = element_text(size = prim_font_size - 3),
          strip.background = element_blank(),
          axis.line = element_blank(),
          ## axis.ticks = element_line(colour = "grey50"),
          axis.ticks = element_blank(),
          ## axis.text = element_text(colour = "grey50"))
          axis.text = element_blank(),
          axis.title = element_text(size = prim_font_size))

  return(list(scatter_plot, density_plot, scatter_plot_facets))
}

plot_hdi_var_dcor <- function(prim_font_size = 20) {
# nolint start
  plot_hdi_var_dcor_by_var("iso1", prim_font_size = prim_font_size, by_col = "RegionSoviet", legend_pos = c(1, 0.05)) %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso2", prim_font_size = prim_font_size, by_col = "RegionSoviet", legend_pos = c(0, 0.05)) %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso3", prim_font_size = prim_font_size, by_col = "RegionSoviet", legend_pos = c(0, 0.05)) %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso4", prim_font_size = prim_font_size, by_col = "RegionSoviet", legend_pos = c(0, 0.05)) %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso5", prim_font_size = prim_font_size, by_col = "RegionSoviet", legend_pos = c(0, 0.05)) %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC1",  prim_font_size = prim_font_size, by_col = "RegionSoviet")                          %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC2",  prim_font_size = prim_font_size, by_col = "RegionSoviet")                          %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC3",  prim_font_size = prim_font_size, by_col = "RegionSoviet", c(1, 1))                 %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC4",  prim_font_size = prim_font_size, by_col = "RegionSoviet", c(1, 1))                 %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC5",  prim_font_size = prim_font_size, by_col = "RegionSoviet", c(1, 1))                 %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso1", prim_font_size = prim_font_size, by_col = "year")                                  %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso2", prim_font_size = prim_font_size, by_col = "year", c(1, 0))                         %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso3", prim_font_size = prim_font_size, by_col = "year", c(0, 0.05))                      %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso4", prim_font_size = prim_font_size, by_col = "year", c(0, 0.05))                      %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("iso5", prim_font_size = prim_font_size, by_col = "year", c(1, 0))                         %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC1",  prim_font_size = prim_font_size, by_col = "year")                                  %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC2",  prim_font_size = prim_font_size, by_col = "year")                                  %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC3",  prim_font_size = prim_font_size, by_col = "year", c(1, 1))                         %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC4",  prim_font_size = prim_font_size, by_col = "year", c(1, 1))                         %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
  plot_hdi_var_dcor_by_var("PC5",  prim_font_size = prim_font_size, by_col = "year", c(1, 1))                         %>% { print(.[[1]]); print(.[[2]]); print(.[[3]]) }
# nolint end
}

plot_hdi_var_dcor_extendend <- function () {

}

plot_hdi_var_dcor_slopes <- function () {

  color_scheme <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                    "#66a61e", "#e6ab02", "#a6761d", "#666666")
  null_year  <-2003

  wdi_data_cons_df_no_na <- wdi_data_cons_df[, c("Country Code", "year", "RegionSoviet", "iso1", "HDI")]
  setDT(wdi_data_cons_df_no_na)
  wdi_data_cons_df_no_na[, iso1 := iso1 - iso1[year == null_year], by = "Country Code"]
  wdi_data_cons_df_no_na[, HDI := HDI - HDI[year == null_year], by = "Country Code"]

  print(
    wdi_data_cons_df_no_na %>%
    ggplot(aes(x = iso1, y = HDI)) +
    geom_path(data = transform(wdi_data_cons_df_no_na, RegionSoviet = NULL),
              alpha = 1,
              group = "Short Name") +
    geom_path(aes(color = RegionSoviet), group = "Short Name") +
    scale_color_manual(values = color_scheme,
                       guide = FALSE) +
    facet_wrap(~ `RegionSoviet`) +
    theme_minimal()
  )

  wdi_data_cons_df_no_na <-
    wdi_data_cons_df[, c("Country Code", "year", "RegionSoviet", "iso1", "HDI")] %>%
    na.exclude
  setDT(wdi_data_cons_df_no_na)
  wdi_data_cons_df_no_na <- wdi_data_cons_df_no_na[,.(iso1 = .SD$iso1[.N] - .SD$iso1[1],
                                                      HDI = .SD$HDI[.N] - .SD$HDI[1],
                                                      RegionSoviet = .SD$RegionSoviet[1]), by = "Country Code"]

  print(
    wdi_data_cons_df_no_na %>%
    ggplot(aes(x = iso1, y = HDI)) +
    geom_density2d(data = transform(wdi_data_cons_df_no_na, RegionSoviet = NULL),
                   color = "black") +
    ## geom_density2d(aes(color = RegionSoviet)) +
    geom_point(aes(color = RegionSoviet)) +
    scale_color_manual(values = color_scheme,
                       guide = FALSE) +
    facet_wrap(~ RegionSoviet) +
    theme_minimal()
  )
}

plot_axis_meanings <- function(all_cors = all_cors,
                               n_used_vars = N_USED_VARS,
                               used_var_idx = USED_VAR_IDX) {
  shrink <- 0.04
  b_xlims <- c(shrink * n_used_vars - 2, 3 + (1 - shrink) * n_used_vars)
  ##
  layout(t(1:5), widths = c(1.94, 1, 1, 1, 1))
  par(mar = c(3, 12, 2, 0) + 0.0)
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$iso[used_var_idx, 1, ] ^ 2),
          main = expression(Iso[1] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mar = c(3, 0, 2, 0) + 0.0)
  par(mgp = c(3, 1.5, 0))
  boxplot(t(all_cors$iso[used_var_idx, 2, ] ^ 2),
          yaxt = "n", main = expression(Iso[2] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$iso[used_var_idx, 3, ] ^ 2),
          yaxt = "n", main = expression(Iso[3] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mgp = c(3, 1.5, 0))
  boxplot(t(all_cors$iso[used_var_idx, 4, ] ^ 2),
          yaxt = "n", main = expression(Iso[4] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mar = c(3, 0, 2, 0.01) + 0.0)
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$iso[used_var_idx, 5, ] ^ 2),
          yaxt = "n", main = expression(Iso[5] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  ##
  par(mar = c(3, 12, 2, 0) + 0.0)
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$pca[used_var_idx, 1, ] ^ 2),
          main = expression(PCA[1] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mar = c(3, 0, 2, 0) + 0.0)
  par(mgp = c(3, 1.5, 0))
  boxplot(t(all_cors$pca[used_var_idx, 2, ] ^ 2),
          yaxt = "n", main = expression(PCA[2] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$pca[used_var_idx, 3, ] ^ 2),
          yaxt = "n", main = expression(PCA[3] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mgp = c(3, 1.5, 0))
  boxplot(t(all_cors$pca[used_var_idx, 4, ] ^ 2),
          yaxt = "n", main = expression(PCA[4] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  par(mar = c(3, 0, 2, 0.01) + 0.0)
  par(mgp = c(3, 0.5, 0))
  boxplot(t(all_cors$pca[used_var_idx, 5, ] ^ 2),
          yaxt = "n", main = expression(PCA[5] ~~ r^2),
          xlim = b_xlims, pch = ".", ylim = 0:1,
          horizontal = TRUE, outline = TRUE, las = 1)
  abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  ##
  image(t(all_cors$iso[, 1, ] ^ 2), useRaster = TRUE)
  image(t(all_cors$pca[, 1, ] ^ 2), useRaster = TRUE)
  ##
  image(t(all_cors$iso[, 2, ] ^ 2), useRaster = TRUE)
  image(t(all_cors$pca[, 2, ] ^ 2), useRaster = TRUE)
  ##
  image(t(all_cors$iso[, 3, ] ^ 2), useRaster = TRUE)
  image(t(all_cors$pca[, 3, ] ^ 2), useRaster = TRUE)
  ##
  image(t(all_cors$iso[, 4, ] ^ 2), useRaster = TRUE)
  image(t(all_cors$pca[, 4, ] ^ 2), useRaster = TRUE)
  ##
  image(t(all_cors$iso[, 5, ] ^ 2), useRaster = TRUE)
  image(t(all_cors$pca[, 5, ] ^ 2), useRaster = TRUE)
}

plot_axis_meanings_fig <- function (all_cors = all_cors,
                                    dcor_var_iso_cons = dcor_var_iso_cons,
                                    used_var_idx = USED_VAR_IDX,
                                    n_used_vars = N_USED_VARS) {
  shrink <- 0.04
  ncols <- 3
  n_items_per_col <- rep(floor(n_used_vars / ncols), times = ncols)
  n_items_per_col[seq_len(n_used_vars %% ncols)] <- n_items_per_col[1] + 1
  used_var_idx_i <- which(used_var_idx)
  col_idx_start <- c(1, cumsum(n_items_per_col[1:(ncols - 1)]) + 1)
  col_idx_end <- c(cumsum(n_items_per_col))
  cex_lab <- 0.9
  ##
  layout(t(seq_len(ncols * 3)), widths = rep(c(2.9, 1, 1), ncols * 3))
  for (i in 1:ncols) {
    b_xlims <-
      c(shrink * n_items_per_col[i] - 2, 3 + (1 - shrink) * n_items_per_col[i])
    par(mar = c(3, 12, 2, 0) + 0.0)
    par(mgp = c(3, 0.5, 0))
    par(bty = "n")
    boxplot(
      t(all_cors$iso[used_var_idx_i[col_idx_start[i]:col_idx_end[i]], 1, ] ^ 2),
      main = expression(Iso[1] ~~ r^2),
      xlim = b_xlims,
      pch = ".",
      ylim = 0:1,
      horizontal = TRUE,
      outline = TRUE,
      las = 1,
      pars = list(cex.axis = cex_lab)
    )
    abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
    par(mar = c(3, 0, 2, 0) + 0.0)
    par(mgp = c(3, 1.5, 0))
    boxplot(
      t(all_cors$iso[used_var_idx_i[col_idx_start[i]:col_idx_end[i]], 2, ] ^ 2),
      yaxt = "n",
      main = expression(Iso[2] ~~ r^2),
      xlim = b_xlims,
      pch = ".",
      ylim = 0:1,
      horizontal = TRUE,
      outline = TRUE,
      las = 1,
      pars = list(cex.axis = cex_lab)
    )
    abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
    par(mar = c(3, 0, 2, 0) + 0.0)
    par(mgp = c(3, 0.5, 0))
    boxplot(
      t(all_cors$iso[used_var_idx_i[col_idx_start[i]:col_idx_end[i]], 3, ] ^ 2),
      yaxt = "n",
      main = expression(Iso[3] ~~ r^2),
      xlim = b_xlims,
      pch = ".",
      ylim = 0:1,
      horizontal = TRUE,
      outline = TRUE,
      las = 1,
      pars = list(cex.axis = cex_lab)
    )
    abline(h = 1:sum(used_var_idx), lty = "dotted", col = "lightgray")
  }
  ##
  layout(t(1:4))
  par(mar = c(3, 12, 2, 1))
  par(mgp = c(3, 0.5, 0))
  max_rows <- 20
  iso_1_order <- order(dcor_var_iso_cons[, 1],
                       decreasing = TRUE, na.last = TRUE)
  b_xlims <- c(1, max_rows)
  boxplot(
    t(all_cors$iso[rev(iso_1_order[seq_len(max_rows)]), 1, ] ^ 2),
    main = expression(Iso[1] ~~ r^2),
    las = 1,
    yaxt = "n",
    xlim = b_xlims,
    horizontal = TRUE
  )
  axis(2, at = seq_len(max_rows), las = 1, lty = 0,
       labels = row.names(all_cors$iso)[rev(iso_1_order[seq_len(max_rows)])])
  iso_2_order <- order(dcor_var_iso_cons[, 2],
                       decreasing = TRUE, na.last = TRUE)
  boxplot(
    t(all_cors$iso[rev(iso_2_order[seq_len(max_rows)]), 2, ] ^ 2),
    main = expression(Iso[2] ~~ r^2),
    las = 1,
    yaxt = "n",
    xlim = b_xlims,
    horizontal = TRUE
  )
  axis(2, at = seq_len(max_rows), las = 1, lty = 0,
       labels = row.names(all_cors$iso)[rev(iso_2_order[seq_len(max_rows)])])
  iso_3_order <- order(dcor_var_iso_cons[, 3],
                       decreasing = TRUE, na.last = TRUE)
  boxplot(
    t(all_cors$iso[rev(iso_3_order[seq_len(max_rows)]), 3, ] ^ 2),
    main = expression(Iso[3] ~~ r^2),
    las = 1,
    yaxt = "n",
    xlim = b_xlims,
    horizontal = TRUE
  )
  axis(2, at = seq_len(max_rows), las = 1, lty = 0,
       labels = row.names(all_cors$iso)[rev(iso_3_order[seq_len(max_rows)])])
  iso_4_order <- order(dcor_var_iso_cons[, 4],
                       decreasing = TRUE, na.last = TRUE)
  boxplot(
    t(all_cors$iso[rev(iso_4_order[seq_len(max_rows)]), 4, ] ^ 2),
    main = expression(Iso[4] ~~ r^2),
    las = 1,
    yaxt = "n",
    xlim = b_xlims,
    horizontal = TRUE
  )
  axis(2, at = seq_len(max_rows), las = 1, lty = 0,
       labels = row.names(all_cors$iso)[rev(iso_4_order[seq_len(max_rows)])])
}

make_median_cor_table_data <- function (
  all_cors = all_cors,
  used_var_idx = USED_VAR_IDX,
  relative_indicators = RELATIVE_INDICATORS,
  .overwrite = FALSE
) {
  OUTFILE <- "../data/cor_table_table.json"
  if (!file.exists(OUTFILE)  || .overwrite) {

    all_cors %>%
      { .$iso[used_var_idx, , ] ^ 2 } %>%
      apply(1:2, median, na.rm = TRUE) %>%
      as.data.table(keep.rownames = "Indicator Code") %>%
      { .[wdi_series, on = "Indicator Code"] } %>%
      { .[`Indicator Code` %in% (relative_indicators[used_var_idx]), ] } %>%
      { .[, c("Short definition", "Unit of measure",
              "Periodicity", "Base Period",
              "Other notes", "Notes from original source",
              "Related source links",
              "Other web links", "Related indicators", "V21",
              "License Type") := NULL] } %>%
      RJSONIO::toJSON(.) %>%
      write(OUTFILE)

  }
}

plot_dev <- function(cntry, data) {
  for (cc in cntry) {
    ## par(new = TRUE)
    try(
      plot(data[.(cc)]$year, data[.(cc), deviation],
           main = data[.(cc)]$`Short Name`[1], type = "b",
           xlab = NA, ylab = "mse deviation")
    )
  }
}


plot_traj <- function(cntry, data) {
  for (cc in cntry) {
    try({
      tmp_data <- data[.(cc)]
      plot(iso3 ~ iso2, tmp_data, type = "l",
           main = tmp_data$`Short Name`[1],
           xlab = "Iso 2", ylab = "Iso 3")
      text(iso3 ~ iso2, tmp_data, labels = data[.(cc)]$year)
    })
  }
}

plot_interesting_trajectories_gg <- function(wdi_data_cons_df = wdi_data_cons_df) {
  p_theme <- theme_classic() +
    theme(panel.background = element_rect(colour = "grey50"),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          axis.text = element_text(colour = "grey50", size = 15),
          axis.title = element_text(size = 20))

  data_hti <- wdi_data_cons_df[.("HTI")][, labs := c(NA,
                                                     "1991 military rule", "", "",
                                                     "1994 Aristides", "",
                                                     "1996 Preval", "", "", "", "",
                                                     "2001 Aristides", "", "",
                                                     "2004 coup d'état", "",
                                                     "2006 Preval", "", "", "",
                                                     "2010 Earthquake",
                                                     "2011 Martelly", "", "", "", "", "")]
  print(
    ggplot(data_hti, aes(x = iso1, y = iso2, label = labs)) +
    geom_path(size = 2, color = "gray50") +
    geom_point(data = data_hti[labs != ""], size = 4) +
    geom_text_repel(size = 7) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 8, label = "Haiti") +
    labs(x = "Dimension 1", y = "Dimension 2") +
    p_theme
  )

  data_grc <- wdi_data_cons_df[.("GRC")][, labs := ifelse(1:nrow(wdi_data_cons_df[.("GRC")]) %% 2 == 1,
                                                          wdi_data_cons_df[.("GRC")]$year, "")]
  print(
    ggplot(data_grc, aes(x = iso1, y = iso2, label = labs)) +
    geom_path(size = 2, color = "gray50") +
    geom_point(data = data_grc[labs != ""], size = 4) +
    geom_text_repel(size = 7) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 8, label = "Greece") +
    labs(x = "Dimension 1", y = "Dimension 2") +
    p_theme
  )

  data_arg <- wdi_data_cons_df[.("ARG")][, labs := c("",
                                                     "1991\nprivatizations", "", "", "",
                                                     "1995\neconomical decline", "", "", "",
                                                     "1999\neconomic crisis", "", "", "",
                                                     "2003\nN Kirchner", "", "", "",
                                                     "2007\nC Kirchner", "", "", "", "", "", "", "", "", "")]
  print(
    ggplot(data_arg, aes(x = iso1, y = iso2, label = labs)) +
    geom_path(size = 2, color = "gray50") +
    geom_point(data = data_arg[labs != ""], size = 4) +
    geom_text_repel(size = 7) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 8, label = "Argentina") +
    labs(x = "Dimension 1", y = "Dimension 2") +
    p_theme
  )

  data_usa <- wdi_data_cons_df[.("USA")][, labs := c("",
                                                     "", "", "1993\nClinton", "", "",
                                                     "", "", "", "", "2000\ndot-com bubble burst",
                                                     "2001\nBush", "", "", "", "",
                                                     "", "", "2008\nfinancial crisis", "2009\nObama", "",
                                                     "", "", "", "", "", "")]
  print(
    ggplot(data_usa, aes(x = iso1, y = iso2, label = labs)) +
    geom_path(size = 2, color = "gray50") +
    geom_point(data = data_usa[labs != ""], size = 4) +
    geom_text_repel(size = 7) +
    annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.5, size = 8, label = "USA") +
    labs(x = "Dimension 1", y = "Dimension 2") +
    p_theme
  )
}

plot_interesting_trajectories <- function(wdi_data_cons_df = wdi_data_cons_df) {

  l_col <-  "grey50"
  l_lwd <-  4
  l_cex  <- 2
  l_cex_ax <- 1.75
  l_cex_tx <- 1.5

  par(xpd = NA, las = 1, mgp = c(3.75, 0.75, 0), mar = c(4.75, 5.25, 0, 0) + 0.1)
  form <- as.formula("iso2 ~ iso1")
  plot(form, wdi_data_cons_df[.("HTI")],
       type = "l", #main = "Haiti",
       lwd = l_lwd, col = l_col,
       bty = "n", xaxt = "n", yaxt = "n",
       xlab = "Dimension 1", ylab = "Dimension 2",
       cex.lab = l_cex)
  box(col = "gray50")
  axis(1, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  axis(2, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  legend("bottomright", "Haiti", bty = "n", cex = l_cex)
  text(form,
       wdi_data_cons_df[.("HTI")],
       labels = c(NA,
                  "1991 military rule", "", "",
                  "1994 Aristides", "",
                  "1996 Preval", "", "", "", "",
                  "2001 Aristides", "", "",
                  "2004 coup d'état", "",
                  "2006 Preval", "", "", "",
                  "2010 Earthquake",
                  "2011 Martelly", "", "", "", ""),
       cex = l_cex_tx)
  form <- as.formula("iso2 ~ iso1")
  plot(form, wdi_data_cons_df[.("GRC")],
       type = "l", #main = "Greece",
       lwd = l_lwd, col = l_col,
       bty = "n", xaxt = "n", yaxt = "n",
       xlab = "Dimension 1", ylab = "Dimension 2",
       cex.lab = l_cex)
  box(col = "gray50")
  axis(1, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  axis(2, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  legend("bottomright", "Greece", bty = "n", cex = l_cex)
  text(form,
       wdi_data_cons_df[.("GRC")],
       labels = ifelse(1:nrow(wdi_data_cons_df[.("GRC")]) %% 2 == 1,
                       wdi_data_cons_df[.("GRC")]$year, ""),
       cex = l_cex_tx)
  form <- as.formula("iso2 ~ iso1")
  plot(form, wdi_data_cons_df[.("ARG")],
       type = "l", #main = "Argentina",
       lwd = l_lwd, col = l_col,
       bty = "n", xaxt = "n", yaxt = "n",
       xlab = "Dimension 1", ylab = "Dimension 2",
       cex.lab = l_cex)
  box(col = "gray50")
  axis(1, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  axis(2, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  legend("bottomright", "Argentina", bty = "n", cex = l_cex)
  text(form,
       wdi_data_cons_df[.("ARG")],
       labels = c("",
                  "1991\nprivatizations", "", "", "",
                  "1995\neconomical decline", "", "", "",
                  "1999\neconomic crisis", "", "", "",
                  "2003\nN Kirchner", "", "", "",
                  "2007\nC Kirchner", "", "", "", "", "", "", "", ""),
       cex = l_cex_tx)
  form <- as.formula("iso2 ~ iso1")
  plot(form, wdi_data_cons_df[.("USA")],
       type = "l", # main = "USA",
       lwd = l_lwd, col = l_col,
       bty = "n", xaxt = "n", yaxt = "n",
       xlab = "Dimension 1", ylab = "Dimension 2",
       cex.lab = l_cex)
  box(col = "gray50")
  axis(1, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  axis(2, col = "gray50", col.axis = "gray50", cex.axis = l_cex_ax,
       lwd = 1, tcl = -0.25, lwd.ticks = 2)
  legend("bottomright", "USA", bty = "n", cex = l_cex)
  text(form,
       wdi_data_cons_df[.("USA")],
       labels = c("",
                  "", "", "1993\nClinton", "", "",
                  "", "", "", "", "2000\ndot-com bubble burst",
                  "2001\nBush", "", "", "", "",
                  "", "", "2008\nfinancial crisis", "2009\nObama", "",
                  "", "", "", "", ""),
       cex = l_cex_tx)
}

plot_t_vs_trend <- function(wdi_data_cons_df) {
  color_scheme <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
                    "#66a61e", "#e6ab02", "#a6761d", "#666666")
  p_theme <- theme_classic() +
    theme(legend.position = "none",
          panel.background = element_rect(colour = "grey50"),
          ## axis.line = element_line(colour = "grey50"),
          axis.line = element_blank(),
          axis.ticks = element_line(colour = "grey50"),
          axis.text = element_text(colour = "grey50", size = 15),
          axis.title = element_text(size = 20))
  label_size  <- 5


  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = year, y = iso1, group = `Short Name`,
               color = RegionSoviet, label = `Short Name`)) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    ## scale_x_continuous(expand = expand_scale()) +
    ## scale_y_continuous(expand = expand_scale()) +
    geom_path(alpha = 0.1, linetype = 1, size = 1) +
    geom_smooth(formula = y ~ x, method = lm, aes(group = RegionSoviet),
                size = 2) +
    geom_label_repel(
      data = wdi_data_cons_df[, .(`Short Name` = `Short Name`[1],
                                  iso1 = mean(iso1, na.rm = TRUE),
                                  year = mean(year, na.rm = TRUE)),
                              by = "RegionSoviet"],
      aes(label = RegionSoviet, fill = RegionSoviet),
      segment.colour = NA, fontface = "bold", color = "white", size = label_size) +
    ylab("Dimension 1") +
    p_theme
  )

  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = year, y = iso2, group = `Short Name`,
               color = RegionSoviet, label = `Short Name`)) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    ## scale_x_continuous(expand = expand_scale()) +
    ## scale_y_continuous(expand = expand_scale()) +
    geom_path(alpha = 0.1, linetype = 1, size = 1) +
    geom_smooth(formula = y ~ x, method = lm,
                aes(group = RegionSoviet),
                size = 2) +
    geom_label_repel(
      data = wdi_data_cons_df[, .(`Short Name` = `Short Name`[1],
                                  iso2 = mean(iso2, na.rm = TRUE),
                                  year = mean(year, na.rm = TRUE)),
                              by = "RegionSoviet"],
      aes(label = RegionSoviet, fill = RegionSoviet),
      segment.colour = NA, fontface = "bold", color = "white", size = label_size) +
    ylab("Dimension 2") +
    p_theme
  )
  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = year, y = iso3, group = `Short Name`,
               color = RegionSoviet, label = `Short Name`)) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    ## scale_x_continuous(expand = expand_scale()) +
    ## scale_y_continuous(expand = expand_scale()) +
    geom_path(alpha = 0.1, linetype = 1, size = 1) +
    geom_smooth(formula = y ~ x, method = lm,
                aes(group = RegionSoviet),
                size = 2) +
    geom_label_repel(
      data = wdi_data_cons_df[, .(`Short Name` = `Short Name`[1],
                                  iso3 = mean(iso3, na.rm = TRUE),
                                  year = mean(year, na.rm = TRUE)),
                              by = "RegionSoviet"],
      aes(label = RegionSoviet, fill = RegionSoviet),
      segment.colour = NA, fontface = "bold", color = "white", size = label_size) +
    ylab("Dimension 3") +
    p_theme
  )
  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = year, y = iso4, group = `Short Name`,
               color = RegionSoviet, label = `Short Name`)) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    ## scale_x_continuous(expand = expand_scale()) +
    ## scale_y_continuous(expand = expand_scale()) +
    geom_path(alpha = 0.1, linetype = 1, size = 1) +
    geom_smooth(formula = y ~ x, method = lm,
                aes(group = RegionSoviet),
                size = 2) +
    geom_label_repel(
      data = wdi_data_cons_df[, .(`Short Name` = `Short Name`[1],
                                  iso4 = mean(iso4, na.rm = TRUE),
                                  year = mean(year, na.rm = TRUE)),
                              by = "RegionSoviet"],
      aes(label = RegionSoviet, fill = RegionSoviet),
      segment.colour = NA, fontface = "bold", color = "white", size = label_size) +
    ylab("Dimension 4") +
    p_theme
  )
  print(
    wdi_data_cons_df %>%
    ggplot(aes(x = year, y = iso5, group = `Short Name`,
               color = RegionSoviet, label = `Short Name`)) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    ## scale_x_continuous(expand = expand_scale()) +
    ## scale_y_continuous(expand = expand_scale()) +
    geom_path(alpha = 0.1, linetype = 1, size = 1) +
    geom_smooth(formula = y ~ x, method = lm,
                aes(group = RegionSoviet),
                size = 2) +
    geom_label_repel(
      data = wdi_data_cons_df[, .(`Short Name` = `Short Name`[1],
                                  iso5 = mean(iso5, na.rm = TRUE),
                                  year = mean(year, na.rm = TRUE)),
                              by = "RegionSoviet"],
      aes(label = RegionSoviet, fill = RegionSoviet),
      segment.colour = NA, fontface = "bold", color = "white", size = label_size) +
    ylab("Dimension 5") +
    p_theme
  )
}

plot_worldmap_slopes_var <- function(wdi_data_cons_df) {
  world_map <- ggplot2::map_data("world")
  for (v in USED_VAR_NAMES[1]) {
    print(
      wdi_data_cons_df %>%
      select("map_name", !!v) %>%
      na.exclude %T>%
      print %>%
      ggplot(aes(map_id = map_name)) +
      geom_map(aes_string(fill = paste0("sens.slope(", v,")$estimates"),
                          color = paste0("sens.slope(", v, ")$p.value < 0.05")),
               map = world_map) #+
      ## coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    )
  }
}

plot_worldmap_slopes <- function(wdi_data_cons_df) {
  world_map <- ggplot2::map_data("world")
  wdi_data_cons_df %>%
    dplyr::filter(!is.na(iso1)) %>%
    dplyr::arrange(`Country Code`, year) %>%
    dplyr::group_by(map_name) %>%
    dplyr::summarise(
      iso1_slope_p = sens.slope(iso1)$p.value,
      iso1_slope   = sens.slope(iso1)$estimates,
      iso2_slope_p = sens.slope(iso2)$p.value,
      iso2_slope   = sens.slope(iso2)$estimates,
      iso3_slope_p = sens.slope(iso3)$p.value,
      iso3_slope   = sens.slope(iso3)$estimates,
      iso4_slope_p = sens.slope(iso4)$p.value,
      iso4_slope   = sens.slope(iso4)$estimates,
      iso5_slope_p = sens.slope(iso5)$p.value,
      iso5_slope   = sens.slope(iso5)$estimates
    ) %>%
    tidyr::gather(key, value, -map_name) %>%
    tidyr::extract(
      key,
      c("component", "slope_p"),
      "(iso[1-5]{1})_(slope_p|slope)"
    ) %>%
    spread(slope_p, value) ->
    wdi_data_transformed

  limits <- quantile(wdi_data_transformed$slope, c(0.1, 0.9))
  wdi_data_transformed$slope[wdi_data_transformed$slope > limits[2]] <- limits[2]
  wdi_data_transformed$slope[wdi_data_transformed$slope < limits[1]] <- limits[1]

  inv_rquant <- function(x) {
    r <- range(wdi_data_transformed$slope)
    (x - r[1]) / (r[2] - r[1])
  }

  print(
    wdi_data_transformed %>%
    ggplot(aes(map_id = map_name)) +
    geom_map(aes(fill = slope, color = slope_p < 0.05),
             map = world_map, size = 0.2) +
    scale_fill_gradient2("Slope ") +
    scale_color_manual("",
                       labels = c(expression(p < 0.05),
                                  expression(p >= 0.05)),
                       breaks = c(TRUE, FALSE),
                       values = c("TRUE" = "black", "FALSE" = "#77777720")) +
    ## coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
    facet_wrap(~ component, ncol = 1,
               labeller = . %>% {
                 gsub("^iso", "Dimension ", .$component) } %>% list) +
    guides(color = guide_legend(override.aes = list(fill = NA, size = 1,
                                                    alpha = 0))) +
    lims(x = c(-179, 179), y = c(-55, 85)) +
    annotate("segment",
             x =    c(-Inf, -Inf,  Inf, Inf),    y = c(-Inf, -Inf, Inf,  Inf),
             xend = c(-Inf,  Inf, -Inf, Inf), yend = c( Inf, -Inf, Inf, -Inf),
             color = "gray50") +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size = 15),
          strip.background = element_blank(),
          panel.grid.minor = element_line(colour = "gray50"),
          panel.grid.major = element_line(colour = "gray50"))
  )
}

#' split the topic column of the wdi series
wdi_series_split_topic <- function (wdi_series) {
  wdi_series$Topic1 <- wdi_series$Topic %>%
    strsplit(": ") %>%
    lapply(. %>% { .[length(.)] }) %>%
    unlist
  wdi_series$Topic2 <- wdi_series$Topic %>%
    strsplit(": ") %>%
    lapply(. %>% { .[max(0, length(.) - 1)] }) %>%
    unlist
  wdi_series$Topic3 <- wdi_series$Topic %>%
    strsplit(": ") %>%
    lapply(. %>% { .[max(0, length(.) - 2)] }) %>%
    lapply(. %>% ifelse(length(.), ., "WDI")) %>%
    unlist
  wdi_series$Topic4 <- wdi_series$Topic %>%
    strsplit(": ") %>%
    lapply(. %>% { .[max(0, length(.) - 3)] }) %>%
    lapply(. %>% ifelse(length(.), ., "WDI")) %>%
    unlist
  wdi_series$Topic5 <- "WDI"

  return(wdi_series)
}

#' creates an igraph tree object from the hierarchical topic structure given by
#' the world bank (topic column in wdi_series)
wdi_series_graph <- function(local_wdi_series,
                             var_names         = var_names,
                             dcor_var_iso_cons = dcor_var_iso_cons) {

  ## local_wdi_series$`Indicator Code`
  edges_level_01 <- local_wdi_series %>% select(`Indicator Code`, Topic1) %>%
    unique %>% rename( from = Topic1, to = `Indicator Code`)
  edges_level_12 <- local_wdi_series %>% select(Topic1, Topic2)           %>%
    unique %>% rename( from = Topic2, to = Topic1          )
  edges_level_23 <- local_wdi_series %>% select(Topic2, Topic3)           %>%
    unique %>% rename( from = Topic3, to = Topic2          )
  edges_level_34 <- local_wdi_series %>% select(Topic3, Topic4)           %>%
    unique %>% rename( from = Topic4, to = Topic3          )
  edges_level_45 <- local_wdi_series %>% select(Topic4, Topic5)           %>%
    unique %>% rename( from = Topic5, to = Topic4          )

  ## create the actual igraph object
  hier_graph <-
    rbind(edges_level_01, edges_level_12, edges_level_23, edges_level_34,
          edges_level_45) %>%
    unique %>%
    filter(to != from) %>%
    ## graph_from_data_frame does not care about column names,
    ## only about positions
    { .[rev(seq_len(ncol(.)))] } %>%
    graph_from_data_frame

  ## additional data:
  graph_idxs <- match(var_names, V(hier_graph)$name)
  dcor_subset <- dcor_var_iso_cons[var_names, ]
  hier_graph <- set_vertex_attr(hier_graph, "which_max_iso", graph_idxs,
                                dcor_subset %>% apply(1, which.max))
  hier_graph <- set_vertex_attr(hier_graph, "dcor_max", graph_idxs,
                                dcor_subset %>% apply(1, max))
  hier_graph <- set_vertex_attr(hier_graph, "dcor1", graph_idxs,
                                dcor_subset[, 1])
  hier_graph <- set_vertex_attr(hier_graph, "dcor2", graph_idxs,
                                dcor_subset[, 2])
  hier_graph <- set_vertex_attr(hier_graph, "dcor3", graph_idxs,
                                dcor_subset[, 3])
  hier_graph <- set_vertex_attr(hier_graph, "dcor4", graph_idxs,
                                dcor_subset[, 4])
  hier_graph <- set_vertex_attr(hier_graph, "dcor5", graph_idxs,
                                dcor_subset[, 5])
  hier_graph <-
    set_vertex_attr(hier_graph, "Indicator.Code", graph_idxs,
                    local_wdi_series$`Indicator Code`[
                                 match(var_names, local_wdi_series$`Indicator Code`)])
}

#' find all connections between nodes where the distances correlation loads
#' highest on the axes, i.e. all pairwise connections between variables that
#' have the highest dcor value on component 1
inner_connections_by_axis <- function (
  axis,
  dcor_var_iso_cons = dcor_var_iso_cons,
  wdi_var_hierarchy_graph = wdi_var_hierarchy_graph,
  var_names = var_names
) {
  res <- dcor_var_iso_cons[var_names, ] %>% apply(1, which.max) %>% {
    tmp <- structure(
      rep(FALSE, wdi_var_hierarchy_graph %>% V %>% length),
      names = V(wdi_var_hierarchy_graph)$name
    )
    tmp[names(.)] <- . == axis
    tmp
  } %>% which

  if (length(res) <= 1)
    return(matrix(NA_integer_, nrow = 2, ncol = 0))
  else
    return(combn(res, 2))
}

manual_label_modifier <- function (x) {
  tab <- data.table::fread(get_data_file("indicator_compressed_names.csv"))
  dict <- structure(tab$`Compressed Name`, .Names = tab$`Indicator Code`)

  return(dict[x])
}

short_name_label_modifier <- function (x) {
  dict <- structure(wdi_series$`Indicator Name`,
                    .Names = wdi_series$`Indicator Code`)
  return(dict[x])
}

stat_filter_sort <- ggproto(
  "stat_filter_sort", StatIdentity,
  setup_data = function (data, params) {
    data <- data[data$filter, ]
    data <- data[order(pi * (data$x > 0) + atan(data$y / data$x)), ]
    data
  },
  default_aes = aes(sort = name, filter = TRUE)
)

#' Draw a bundle plot
#'
#' Draws a circular dendrogram of the hierarchy of the wdi data base. The
#' hierarchy is given by the world bank classification of the indicators.
#'
#' The dendrogram is constructed from `local_wdi_series`
#'
#' additionally connections between the variables are drawn as bundles, the
#' connections are given by `dcor_var_iso_cons` and connections are drawn
#' between the variables with maximum correlation on one axis
#'
#' var_names is the subset of variables drawn
plot_conn_bundle <- function (
  var_names,
  dcor_var_iso_cons = dcor_var_iso_cons,
  local_wdi_series  = wdi_series,
  expand_multiplier = 1 / 2,
  text_size         = 3,
  label_modifier    = I
) {

  col_pal <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
  component_common <- "Dimension"
  component_names <- paste(component_common, 1:5)
  xy_expand <- function(xy, expand) xy * (1 + expand * expand_multiplier)

  local_wdi_series <- wdi_series_split_topic(local_wdi_series)
  wdi_series_used_important <- local_wdi_series[`Indicator Code` %in% var_names]

  stopifnot(length(var_names) == nrow(wdi_series_used_important))

  wdi_series_used <- wdi_series_used_important
  wdi_var_hierarchy_graph <-
    wdi_series_graph(wdi_series_used,
                     var_names = var_names,
                     dcor_var_iso_cons = dcor_var_iso_cons)

  axis_cons <- lapply(1:5, inner_connections_by_axis,
                       dcor_var_iso_cons       = dcor_var_iso_cons,
                       wdi_var_hierarchy_graph = wdi_var_hierarchy_graph,
                       var_names               = var_names)

  level1 <- wdi_series_used[, Topic1:Topic5] %>%
    as.matrix %>%
    { .[, rev(1:5)] } %>%
    apply(1, . %>% { .[which(. != "WDI")][1] }) %>%
    unique

  print(
    wdi_var_hierarchy_graph %>%
    ggraph(layout = "dendrogram", circular = TRUE) +
    geom_node_point(aes(filter = leaf,
                        x = xy_expand(x, dcor1),
                        y = xy_expand(y, dcor1)),
                    colour = col_pal[1]) +
    geom_polygon(aes(sort = name,
                     filter = name %in% var_names,
                     x = xy_expand(x, dcor1),
                     y = xy_expand(y, dcor1)),
                 stat = stat_filter_sort,
                 group = 1,
                 fill = NA,
                 colour = col_pal[1]) +
    geom_node_point(aes(filter = leaf,
                       x = xy_expand(x, dcor2),
                        y = xy_expand(y, dcor2)),
                    colour = col_pal[2]) +
    geom_polygon(aes(sort = name,
                     filter = name %in% var_names,
                     x = xy_expand(x, dcor2),
                     y = xy_expand(y, dcor2)),
                 stat = stat_filter_sort,
                 group = 1,
                 fill = NA,
                 colour = col_pal[2]) +
    geom_node_point(aes(filter = leaf,
                        x = xy_expand(x, dcor3),
                        y = xy_expand(y, dcor3)),
                    colour = col_pal[3]) +
    geom_polygon(aes(sort = name,
                     filter = name %in% var_names,
                     x = xy_expand(x, dcor3),
                     y = xy_expand(y, dcor3)),
                 stat = stat_filter_sort,
                 group = 1,
                 fill = NA,
                 colour = col_pal[3]) +
    geom_node_point(aes(filter = leaf,
                        x = xy_expand(x, dcor4),
                        y = xy_expand(y, dcor4)),
                    colour = col_pal[4]) +
    geom_polygon(aes(sort = name,
                     filter = name %in% var_names,
                     x = xy_expand(x, dcor4),
                     y = xy_expand(y, dcor4)),
                 stat = stat_filter_sort,
                 group = 1,
                 fill = NA,
                 colour = col_pal[4]) +
    geom_node_point(aes(filter = leaf,
                        x = xy_expand(x, dcor5),
                        y = xy_expand(y, dcor5)),
                    colour = col_pal[5]) +
    geom_polygon(aes(sort = name,
                     filter = name %in% var_names,
                     x = xy_expand(x, dcor5),
                     y = xy_expand(y, dcor5)),
                 stat = stat_filter_sort,
                 group = 1,
                 fill = NA,
                 colour = col_pal[5]) +
    geom_node_point(aes(filter = leaf, color = factor(which_max_iso)),
                    size = 0) +
    geom_node_text(aes(filter = leaf,
                       label  = label_modifier(as.character(Indicator.Code)),
                       color  = factor(paste(component_common, which_max_iso)),
                       x      = x * (1 + 1 * expand_multiplier),
                       y      = y * (1 + 1 * expand_multiplier),
                       hjust  = if_else(x > 0, 0, 1),
                       angle  = atan(y / x) / 2 / pi * 360),
                   show.legend = FALSE,
                   size        = text_size) +
    geom_conn_bundle(data = get_con(axis_cons[[1]][1, ], axis_cons[[1]][2, ]),
                     edge_colour = col_pal[1], edge_alpha = 0.3) +
    geom_conn_bundle(data = get_con(axis_cons[[2]][1, ], axis_cons[[2]][2, ]),
                     edge_colour = col_pal[2], edge_alpha = 0.3) +
    geom_conn_bundle(data = get_con(axis_cons[[3]][1, ], axis_cons[[3]][2, ]),
                     edge_colour = col_pal[3], edge_alpha = 0.3) +
    (
      if (ncol(axis_cons[[4]]) > 0)
        geom_conn_bundle(data = get_con(axis_cons[[4]][1, ],
                                        axis_cons[[4]][2, ]),
                         edge_colour = col_pal[4], edge_alpha = 0.3)
      else
        list()
    ) +
    geom_conn_bundle(data = get_con(axis_cons[[5]][1, ], axis_cons[[5]][2, ]),
                     edge_colour = col_pal[5], edge_alpha = 0.3) +
    geom_edge_diagonal() +
    geom_node_label(aes(filter = !leaf,
                        fontface = if_else(name %in% level1, "bold", "plain"),
                        label = name, angle = atan(y / x) / 2 / pi * 360),
                    size = 3) +
    scale_color_manual(limits = component_names,
                       labels = component_names,
                       values = col_pal) +
    guides(colour = guide_legend(title = component_common,
                                 override.aes = list(size = 5))) +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.position = c(0.9, 0.1),
          legend.justification = c(0.9, 0.1)) +
    expand_limits(x = c(-2.7, 2.7), y = c(-2.7, 2.7))
  )
}


riverplot_vars <- function (cors,
                            var_names = rownames(cors),
                            local_wdi_series = wdi_series,
                            label_modifier = I) {

  col_pal <- c("#1b9e77ff",
               "#d95f02ff",
               "#7570b3ff",
               "#e7298aff",
               "#66a61eff")
  small_cors <- cors[var_names, ]
  small_id_codes <- rownames(small_cors)

  max_ax <- apply(small_cors, 1, which.max)
  max_cor <- apply(small_cors, 1, max)

  short_goal_idx <- match(small_id_codes,
                          local_wdi_series[["Indicator Code"]],
                          nomatch = 0)

  e1 <- data.frame(N1      = paste("Dimension", max_ax),
                   N2      = label_modifier(small_id_codes),
                   Value   = max_cor,
                   ## Value = 1,
                   col     = col_pal[max_ax],
                   edgecol = "col",
                   stringsAsFactors = FALSE)
  e2 <- data.frame(N1      = label_modifier(small_id_codes),
                   N2      = local_wdi_series[["Short Goal"]][short_goal_idx],
                   Value   = max_cor,
                   ## Value = 1,
                   col     = col_pal[max_ax],
                   edgecol = "col",
                   stringsAsFactors = FALSE)
  e2 <- na.exclude(e2)

  n1 <- data.frame(ID = rev(sort(unique(e1$N1))),
                   x = 1,
                   y = NA,
                   col = rev(col_pal),
                   textcex = 1,
                   stringsAsFactors = FALSE)
  n2 <- data.frame(ID = e2$N1,
                   x = 2,
                   y = NA,
                   col = col_pal[max_ax],
                   ## textcex = 0.3,
                   textcex = max_cor * 0.5,
                   stringsAsFactors = FALSE)
  n3 <- data.frame(ID = unique(e2$N2),
                   x = 3,
                   y = NA,
                   col = NA,
                   textcex = 1,
                   stringsAsFactors = FALSE)

  ## Sort the right half
  n2 <- n2[order(e2$N2), ]
  n3 <- n3[order(n3$ID), ]
  n3$col <- rep(c("#FFFFFF00", "#BBBBBBBB"), 100)[1:length(unique(e2$N2))]

  ## Sort the edges along the variables, so they won't cross
  ## It is basically sort x along y
  e1 <- e1[order(match(e1$N2, n2$ID)), ]

  edges <- rbind(e1, e2)
  nodes <- rbind(n1, n2, n3)

  ## print(nodes$ID)

  rownames(edges) <- NULL
  rownames(nodes) <- NULL

  river <- riverplot::makeRiver(nodes, edges)

  ## par(lty = 0)
  plot(
    river,
    nodewidth = 15,
    node_margin = 0.00,
    gravity = "center",
    textcex = 0.6,
    srt = 0,
    fix.pdf = 1
  )
  1
}

riverplot_vars_simple <- function (cors,
                            var_names = rownames(cors),
                            local_wdi_series = wdi_series,
                            label_modifier = I) {

  col_pal <- c("#1b9e77ff",
               "#d95f02ff",
               "#7570b3ff",
               "#e7298aff",
               "#66a61eff")
  small_cors <- cors[var_names, ]
  small_id_codes <- rownames(small_cors)

  max_ax <- apply(small_cors, 1, which.max)
  max_cor <- apply(small_cors, 1, max)

  short_goal_idx <- match(small_id_codes,
                          local_wdi_series[["Indicator Code"]],
                          nomatch = 0)

  e1 <- data.frame(N1      = paste("Dimension", max_ax),
                   N2      = label_modifier(small_id_codes),
                   Value   = max_cor,
                   ## Value = 1,
                   col     = col_pal[max_ax],
                   edgecol = "col",
                   stringsAsFactors = FALSE)
  e2 <- data.frame(N1      = label_modifier(small_id_codes),
                   N2      = local_wdi_series[["Short Goal"]][short_goal_idx],
                   Value   = max_cor,
                   ## Value = 1,
                   col     = col_pal[max_ax],
                   edgecol = "col",
                   stringsAsFactors = FALSE)
  e2 <- na.exclude(e2)

  n1 <- data.frame(ID = rev(sort(unique(e1$N1))),
                   x = 1,
                   y = NA,
                   ## labels = rev(sort(unique(e1$N1))),
                   labels = "",
                   nodestyle = "invisible",
                   col = rev(col_pal),
                   textcex = 2,
                   stringsAsFactors = FALSE)
  n2 <- data.frame(ID = e2$N1,
                   x = 1.5,
                   y = NA,
                   labels = "",
                   nodestyle = "invisible",
                   col = col_pal[max_ax],
                   ## textcex = 0.3,
                   textcex = max_cor * 0.5,
                   stringsAsFactors = FALSE)
  n3 <- data.frame(ID = unique(e2$N2),
                   x = 1.65,
                   y = NA,
                   labels = unique(e2$N2),
                   nodestyle = "regular",
                   col = NA,
                   textcex = 1.5,
                   stringsAsFactors = FALSE)

  ## Sort the right half
  n2 <- n2[order(e2$N2), ]
  n3 <- n3[order(n3$ID), ]
  n3$col <- rep(c("#FFFFFF00", "#BBBBBBBB"), 100)[1:length(unique(e2$N2))]

  ## Sort the edges along the variables, so they won't cross
  ## It is basically sort x along y
  e1 <- e1[order(match(e1$N2, n2$ID)), ]

  edges <- rbind(e1, e2)
  nodes <- rbind(n1, n2, n3)

  ## print(nodes$ID)

  rownames(edges) <- NULL
  rownames(nodes) <- NULL

  river <- riverplot::makeRiver(nodes, edges)

  ## par(lty = 0)
  par(mai = c(0, 0, 0, 2), xpd = TRUE)
  plot(
    river,
    nodewidth = 30,
    node_margin = 0.00,
    gravity = "center",
    textcex = 0.6,
    srt = 0,
    fix.pdf = 1
  )
  legend(x = 0.06, y = 0.9, legend = paste("Dimension", 1:5), col = col_pal, lty = 1, lwd = 10, cex = 1.5, bty = "n")
  1
}

## curveseg <- function (x0, x1, y0, y1, width = 1, nsteps = 50, col = "#ffcc0066",
##     grad = NULL, lty = 0, form = c("sin", "line"), fix.pdf = 0)
## {
##     w <- width
##     if (!is.null(grad)) {
##         grad <- colorRampPaletteAlpha(grad)(nsteps)
##     }
##     else {
##         grad <- rep(col, nsteps)
##     }
##     form <- match.arg(form, c("sin", "line"))
##     if (form == "sin") {
##         xx <- seq(-pi/2, pi/2, length.out = nsteps)
##         yy <- y0 + (y1 - y0) * (sin(xx) + 1)/2
##         xx <- seq(x0, x1, length.out = nsteps)
##     }
##     if (form == "line") {
##         xx <- seq(x0, x1, length.out = nsteps)
##         yy <- seq(y0, y1, length.out = nsteps)
##     }

##     ## polygon(c(xx, rev(xx)),
##     ##         c(yy, rev(yy)),
##     ##         col = grad)

##     for (i in 1:(nsteps - 1)) {
##       polygon(c(xx[i], xx[i + 1], xx[i + 1], xx[i]),
##               c(yy[i], yy[i + 1], yy[i + 1] + w, yy[i] + w),
##               col = grad[i], border = grad[i], lty = fix.pdf)
##         lines(c(xx[i], xx[i + 1]), c(yy[i], yy[i + 1]),         lty = lty)
##         lines(c(xx[i], xx[i + 1]), c(yy[i] + w, yy[i + 1] + w), lty = lty)
##     }
## }

## pdf("~/test.pdf")
## plot(c(0, 1), c(0, 2))
## curveseg(0, 1, 0, 1, lty = 0, col = "#ff0000", fix.pdf = 0)
## dev.off()

## assignInNamespace('curveseg', curveseg.new, 'riverplot', pos = -1, envir = as.environment(pos))

# nolint start
### Local Variables:
### flycheck-checker-error-threshold: 700
### flycheck-check-syntax-automatically: (save mode-enabled)
### End:
# nolint end
make_data_sensitivity_step_one <- function(
  x,
  country_codes,
  relative_indicators,
  year_range,
  var_na_fractions   = seq(0.05, 0.7, by = 0.1),
  cntry_na_fractions = seq(0.05, 0.7, by = 0.1),
  .cluster           = CLUSTER
) {
  message("make_data_sensitivity")
  message(Sys.time(), ": initial data transform")
  x %>%
    dplyr::select(`Country Code`, `Indicator Code`,
                  num_range("", year_range))                 %>%
    dplyr::filter(`Country Code` %in% country_codes)         %>%
    dplyr::filter(`Indicator Code` %in% relative_indicators) %>%
    gather(year, value, num_range("", year_range))           %>%
    spread(`Indicator Code`, value, convert = TRUE)          %>%
    dplyr::mutate(year = as.integer(year))                   %>%
    dplyr::select_if( . %>% { !all(is.na(.) | (. == 0)) })   ->
    wdi_data_transformed
    data.table::setDT(wdi_data_transformed)
}

  