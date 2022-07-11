# download routine
test_that("check download options",{

  # no tests on CRAN, to fickle
  skip_on_cran()
  
  # instigate once, needed on windows
  # checks for some reason otherwise
  # the connection is slow
  list_rois()
  
  expect_message(
    download_phenocam(
      site = "queens$",
      veg_type = "DB",
      roi_id = "1000",
      frequency = 3,
      smooth = TRUE,
      outlier_detection = TRUE,
      phenophase = TRUE,
      daymet = FALSE,
      trim_daymet = FALSE,
      out_dir = tempdir()
      )
    )

  expect_message(
    download_phenocam(site = "bartlett$",
                      veg_type = "DB",
                      roi_id = "1000",
                      frequency = "roistats",
                      out_dir = tempdir()
                      )
    )
  
  expect_message(
    download_phenocam(
      site = "bartlett$",
      veg_type = "DB",
      roi_id = "1000",
      frequency = 1,
      smooth = TRUE,
      outlier_detection = FALSE,
      phenophase = FALSE,
      daymet = TRUE,
      trim_daymet = TRUE,
      out_dir = tempdir()
      )
    )
  
  expect_message(
    download_phenocam(
      site = "bartlett$",
      veg_type = "DB",
      roi_id = "1000",
      frequency = 3,
      smooth = FALSE,
      contract = TRUE,
      outlier_detection = FALSE,
      phenophase = FALSE,
      daymet = FALSE,
      out_dir = tempdir()
      )
    )
  
  df_false = try(
    download_phenocam(
      site = "bartlett$",
      veg_type = "DB",
      roi_id = "1000",
      frequency = 3,
      smooth = FALSE,
      outlier_detection = FALSE,
      phenophase = FALSE,
      daymet = FALSE,
      trim_daymet = FALSE,
      out_dir = tempdir()
      )
    )
  
  expect_message(
    download_phenocam(
      site = "bartlett$",
      veg_type = "DB",
      roi_id = "1000",
      frequency = 3,
      smooth = FALSE,
      outlier_detection = FALSE,
      phenophase = FALSE,
      daymet = FALSE,
      trim_daymet = FALSE,
      out_dir = tempdir(),
      internal = TRUE
      )
    )
  
})


# download routine
test_that("check merge routines",{
  
  skip_on_cran()
  
  # check merges
  download_phenocam(
    site = "harvard$",
    veg_type = "DB",
    roi_id = "1000",
    frequency = "3",
    smooth = FALSE,
    outlier_detection = FALSE,
    phenophase = FALSE
    )
  
  
  # merge data with modis
  expect_type(merge_modis(
    file.path(tempdir(),
              "harvard_DB_1000_3day.csv"),
    product = "MOD13Q1",
    band = "250m_16_days_NDVI"
  ),
  "list")
  
  # merge data with modis
  # no scale
  expect_type(merge_modis(
    file.path(tempdir(),
              "harvard_DB_1000_3day.csv"),
    product = "MCD12Q1",
    band = "LC_Type1"
  ),
  "list")
  
  # merge with daymet
  expect_type(merge_daymet(
    file.path(tempdir(),
              "harvard_DB_1000_3day.csv"),
    internal = TRUE),
    "list")
  
})


