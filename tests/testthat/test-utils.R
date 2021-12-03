#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests


# Check that get_event_name function returns correct event name -----------
test_that("check that get event works", {

  #REDCAP API
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  #List of study names exported from REDCap
  studies <- exportEvents(rcon = rcon_lims)

  #Make sure COMET event name gets returned when searching for COMET
  output_arm<-get_event_name(studies,"COMET")
  expect_equal(output_arm, "comet_arm_2")
})


# Ensure translation tables are composed correctly ------------------------
test_that("check that translation table are pulled correctly", {

  #Call REDCap info function
  redcap_info_test<-REDCap_info()

  #Check that code and labels have same length
  expect_equal(length(redcap_info_test$timepoint), length(redcap_info_test$timepoint_code))
  expect_equal(length(redcap_info_test$type), length(redcap_info_test$type_code))
  expect_equal(length(redcap_info_test$container), length(redcap_info_test$container_code))
  expect_equal(length(redcap_info_test$freezer), length(redcap_info_test$freezer_code))
  expect_equal(length(redcap_info_test$freezer_shelf), length(redcap_info_test$freezer_shelf_code))
  expect_equal(length(redcap_info_test$volume_units), length(redcap_info_test$volume_units_code))

  #Check that studies data frame is loaded correctly
  expect_equal(colnames(redcap_info_test$studies), c("event_name","arm_num","unique_event_name","custom_event_label"))

  #Confirm that all code/labels are under 70 options - if not, more options have to be added to the REDCap_info function
  expect_true(length(redcap_info_test$timepoint) < 70)
  expect_true(length(redcap_info_test$type) < 70)
  expect_true(length(redcap_info_test$container) < 70)
  expect_true(length(redcap_info_test$freezer) < 70)
  expect_true(length(redcap_info_test$freezer_shelf) < 70)
  expect_true(length(redcap_info_test$freezer_shelf_code) < 70)

  #Spot check that order is correct - check that corresponding code and label are found at the same index
  expect_true(which("Day 6" == redcap_info_test$timepoint) ==
                which("6" == redcap_info_test$timepoint_code))

  expect_true(which("Whole Blood" == redcap_info_test$type) ==
                which("3" == redcap_info_test$type_code))

  expect_true(which("Cardboard box (9x9)" == redcap_info_test$container) ==
                which("12" == redcap_info_test$container_code))

  expect_true(which("3" == redcap_info_test$freezer) ==
                which("3" == redcap_info_test$freezer_code))

  expect_true(which("3" == redcap_info_test$freezer_shelf) ==
                which("3" == redcap_info_test$freezer_shelf_code))

  expect_true(which("L" == redcap_info_test$volume_units) ==
                which("3" == redcap_info_test$volume_units_code))

})
