## save template for running hector functions
# this could be remove as now the template is directly defined in functions infuse_remove_outliers_tmp() and infuse_estimate_trend_tmp() in R/hector.R
estmatetrend_tmpl = readr::read_file("data-raw/estimatetrend.tmpl")
removeoutliers_tmpl = readr::read_file("data-raw/removeoutliers.tmpl")
usethis::use_data(estmatetrend_tmpl, removeoutliers_tmpl, internal = TRUE, overwrite = T)