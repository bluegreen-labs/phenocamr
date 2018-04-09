# Phenocamr unit tests

# ancillary functions
test_that("check ancillary routines",{
  
  # test roi downloads
  roi = try(list_rois())
  roi_disk = try(list_rois(internal = FALSE))
  
  # test meta-data downloads
  sites = try(list_sites())
  sites_disk = try(list_sites(internal = FALSE))
  
  # download initial data
  df = try(download_phenocam(site = "harvard$",
                         veg_type = "DB",
                         roi_id = "1",
                         frequency = 3,
                         outlier_detection = FALSE,
                         smooth = TRUE,
                         out_dir = tempdir()))
  
  # transtion dates routine
  transitions = try(transition_dates(paste0(tempdir(),"/harvard_DB_0001_3day.csv"),
                                     plot = TRUE))
  
  # test truncate
  truncate = try(truncate_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"),
                                   year = 2015))
  
  # test outlier routine
  outliers = try(detect_outliers(paste0(tempdir(),"/harvard_DB_0001_3day.csv"),
                                 plot = TRUE,
                                 snowflag = TRUE))
  
  # test expand
  expand = try(expand_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv")))
  
  # test contract
  contract = try(contract_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv")))
  
  # test grvi routine
  grvi_test = try(grvi(paste0(tempdir(),"/harvard_DB_0001_3day.csv")))

  # check daylength routine
  dl = try(daylength(doy = 180,
                     latitude = 44))
  
  # optimal span routine
  l = sin(seq(1,10,0.01))
  l = l + runif(length(l))
  os = try(optimal_span(l,
                        plot = TRUE))
  os_w = try(optimal_span(l,
                          weights = runif(length(l))))
  
  # see if any of the runs failed
  check = !inherits(roi,"try-error") &
          !inherits(roi_disk,"try-error") &
          !inherits(sites,"try-error") &
          !inherits(sites_disk,"try-error") &
          !inherits(df,"try-error") &
          !inherits(truncate, "try-error") &
          !inherits(outliers, "try-error") &
          !inherits(transitions, "try-error") &
          !inherits(expand, "try-error") &
          !inherits(contract, "try-error") &
          !inherits(dl, "try-error") &
          !inherits(os, "try-error") &
          !inherits(os_w, "try-error") &
          !inherits(grvi_test, "try-error")
  
  # check if no error occured
  expect_true(check)
})
