## save template for running hector functions

estmatetrend_tmpl = readr::read_file("data-raw/estimatetrend.tmpl")
removeoutliers_tmpl = readr::read_file("data-raw/removeoutliers.tmpl")
usethis::use_data(estmatetrend_tmpl, removeoutliers_tmpl, internal = TRUE, overwrite = T)