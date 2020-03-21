# Phenocamr unit tests
server_check <- phenocam_running(server_rois())

# ancillary functions
test_that("check ancillary routines",{

  # skip if server is down
  skip_if_not(server_check)
  skip_on_cran()
  
  # test meta-data downloads
  expect_output(str(list_sites()))
  expect_silent(list_sites(internal = FALSE))

  # test roi downloads
  expect_output(str(list_rois()))
  expect_silent(list_rois(internal = FALSE))
  
  # download initial data
  expect_message(download_phenocam(site = "bartlett$",
                         veg_type = "DB",
                         roi_id = "1000",
                         frequency = 3,
                         outlier_detection = FALSE,
                         smooth = TRUE,
                         out_dir = tempdir()))

  # transtion dates routine
  png(paste0(tempdir(),"/bartlett_test.png"),900,900)
  expect_silent(transition_dates(paste0(tempdir(),"/bartlett_DB_1000_3day.csv"),
                                     plot = TRUE))
  dev.off()

  # test truncate
  expect_silent(truncate_phenocam(paste0(tempdir(),"/bartlett_DB_1000_3day.csv"),
                                   year = 2015))

  # test outlier routine
  png(paste0(tempdir(),"/bartlett_test.png"),900,900)
  expect_silent(detect_outliers(paste0(tempdir(),"/bartlett_DB_1000_3day.csv"),
                                 plot = TRUE,
                                 snowflag = TRUE))
  dev.off()

  # test expand
  expect_silent(expand_phenocam(paste0(tempdir(),"/bartlett_DB_1000_3day.csv")))

  # smooth test
  expect_silent(smooth_ts(paste0(tempdir(),"/bartlett_DB_1000_3day.csv")))

  # test contract
  expect_silent(contract_phenocam(paste0(tempdir(),"/bartlett_DB_1000_3day.csv")))

  # test grvi routine
  expect_silent(grvi(paste0(tempdir(),"/bartlett_DB_1000_3day.csv")))

  # check daylength routine
  expect_output(str(daylength(doy = 180,
                     latitude = 44)))

  # optimal span routine
  l = sin(seq(1,10,0.01))
  l = l + runif(length(l))

  png(paste0(tempdir(),"/bartlett_test.png"),900,900)
  expect_warning(optimal_span(l,
                             plot = TRUE))
  dev.off()

  expect_silent(optimal_span(l,
                          weights = runif(length(l))))
})
