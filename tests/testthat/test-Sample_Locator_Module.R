#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests

test_that("Check that samples are found", {

  #REDCap API
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  #Run sample_locator_server with necessary arguments and inputs - checks sample ID search test (file is in the test folder) against what's been uploaded in REDCap for COMET
  testServer(sample_locator_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url),
             {

               session$setInputs(sample_locator_studies = "COMET", sample_locator_file = list(datapath = "Sample ID Search Test.csv"),
                                 sample_locator_find = TRUE)


               #Check that samples IDs found are as expected
               expect_equal(as.character(sample_locations()$sample_id), c("415.1001 D0 PL8","415.1121 D0 PL10","415.1262 D0 PL6"))


             })
})
