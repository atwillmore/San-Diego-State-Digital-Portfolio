#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests
test_that("upload file is formatted correctly before import", {

  translation_tables <- REDCap_info()

  #Create Example File
  test_upload_file <- tibble(`Scan Barcode` = c("625.1023 D0 PL2","625.1023 D-1 PL2"),
                           Patient_ID = "1023",
                           Timepoint = c("D0","D-1"),
                           Sample = "Plasma",
                           `Date and Time of Collection (MM/DD/YY HR:MIN)` = "2021-09-10 10:10:00",
                           `Sample Volume` = NA,
                           `Sample Volume Units` = NA,
                           `Container Type` = "Cardboard box (tall)",
                           `Scan container barcode` = "test",
                           `Grid Position` = c(10,11),
                           `Freezer number` = 4,
                           `Freezer Shelf Number` = 4,
                           `Date Stored in Freezer` = "2021-09-10",
                            `Staff member checking in` = "AW")

  last_record <- 1000000

  #Call format for uplaod function, pass arguments
  sample_upload_test <- format_for_upload(translation_tables$studies,"COMET",last_record,test_upload_file,
                                          translation_tables$timepoint,translation_tables$timepoint_code,translation_tables$type,
                                          translation_tables$type_code,translation_tables$volume_units,translation_tables$volume_units_code,
                                          translation_tables$container,translation_tables$container_code,
                                          translation_tables$freezer,translation_tables$freezer_code,translation_tables$freezer_shelf,translation_tables$freezer_shelf_code)

  #Check that column names are correct for upload
  expect_equal(colnames(sample_upload_test), c("record_id","sample_id","patient_id","timepoint","sample_type","date_time_collection","sample_volume","sample_volume_units",
                                               "container_type","container_matthay_lab_id","grid_position","freezer_number","freezer_shelf_number","date_stored","stored_by",
                                               "sample_storage_complete","sample_status","redcap_event_name"))
  #Check that last record and event name are correct
  expect_equal(as.character(sample_upload_test$record_id), c("comet_1000001","comet_1000002"))
  expect_equal(as.character(sample_upload_test[1,18]), c("comet_arm_2"))


})
