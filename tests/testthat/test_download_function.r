# Phenocamr unit tests

# download routine
test_that("check download options",{
  
  df = try(download_phenocam(site = "harvard$",
                         vegetation = "DB",
                         roi_id = "1",
                         frequency = 3,
                         smooth = TRUE,
                         outlier_detection = TRUE,
                         phenophase = TRUE,
                         daymet = FALSE,
                         trim_daymet = FALSE,
                         out_dir = tempdir()))
  
  df_daymet = try(download_phenocam(site = "harvard$",
                             vegetation = "DB",
                             roi_id = "1",
                             frequency = 3,
                             smooth = TRUE,
                             outlier_detection = FALSE,
                             phenophase = FALSE,
                             daymet = TRUE,
                             trim_daymet = TRUE,
                             out_dir = tempdir()))
  
  df_false = try(download_phenocam(site = "harvard$",
                             vegetation = "DB",
                             roi_id = "1",
                             frequency = 3,
                             smooth = FALSE,
                             outlier_detection = FALSE,
                             phenophase = FALSE,
                             daymet = FALSE,
                             trim_daymet = FALSE,
                             out_dir = tempdir()))
  
  # see if any of the runs failed
  check = !inherits(df,"try-error") &
          !inherits(df_false, "try-error")
          !inherits(df_daymet,"try-error") &
  
  # check if no error occured
  expect_true(check)
})
