# Phenocamr unit tests

server_check <- phenocam_running(server_rois())

# download routine
test_that("check download options",{

  # skip if server is down
  skip_if_not(server_check)
  
  # instigate once, needed on windows
  # checks for some reason otherwise
  # the connection is slow
  list_rois()
  
  expect_message(download_phenocam(site = "queens$",
                         veg_type = "DB",
                         roi_id = "1000",
                         frequency = 3,
                         smooth = TRUE,
                         outlier_detection = TRUE,
                         phenophase = TRUE,
                         daymet = FALSE,
                         trim_daymet = FALSE,
                         out_dir = tempdir()))

  expect_message(download_phenocam(site = "bartlett$",
                             veg_type = "DB",
                             roi_id = "1000",
                             frequency = "roistats",
                             out_dir = tempdir()))

  expect_message(download_phenocam(site = "bartlett$",
                             veg_type = "DB",
                             roi_id = "1000",
                             frequency = 1,
                             smooth = TRUE,
                             outlier_detection = FALSE,
                             phenophase = FALSE,
                             daymet = TRUE,
                             trim_daymet = TRUE,
                             out_dir = tempdir()))

  expect_message(download_phenocam(site = "bartlett$",
                                    veg_type = "DB",
                                    roi_id = "1000",
                                    frequency = 3,
                                    smooth = FALSE,
                                    contract = TRUE,
                                    outlier_detection = FALSE,
                                    phenophase = FALSE,
                                    daymet = FALSE,
                                    out_dir = tempdir()))

  df_false = try(download_phenocam(site = "bartlett$",
                             veg_type = "DB",
                             roi_id = "1000",
                             frequency = 3,
                             smooth = FALSE,
                             outlier_detection = FALSE,
                             phenophase = FALSE,
                             daymet = FALSE,
                             trim_daymet = FALSE,
                             out_dir = tempdir()))
  
  expect_message(download_phenocam(site = "bartlett$",
                                   veg_type = "DB",
                                   roi_id = "1000",
                                   frequency = 3,
                                   smooth = FALSE,
                                   outlier_detection = FALSE,
                                   phenophase = FALSE,
                                   daymet = FALSE,
                                   trim_daymet = FALSE,
                                   out_dir = tempdir(),
                                   internal = TRUE))

})
