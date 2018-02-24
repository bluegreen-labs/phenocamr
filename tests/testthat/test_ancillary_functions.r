# Phenocamr unit tests

# ancillary functions
test_that("check ancillary routines",{
  
  df = try(download_phenocam(site = "harvard$",
                         vegetation = "DB",
                         roi_id = "1",
                         frequency = 3,
                         outlier_detection = FALSE,
                         smooth = FALSE,
                         out_dir = tempdir()))
  
  # test truncate
  truncate = try(truncate_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv"),
                    year = 2015))
  
  # test expand
  expand = try(expand_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv")))
  
  # test contract
  contract = try(contract_phenocam(paste0(tempdir(),"/harvard_DB_0001_3day.csv")))
  
  # see if any of the runs failed
  check = !inherits(df,"try-error") &
          !inherits(truncate, "try-error") &
          !inherits(expand, "try-error") &
          !inherits(contract, "try-error")
  
  # check if no error occured
  expect_true(check)
})
