#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests

test_that("Check that redcap export works and format before changing status is correct", {

  #REDCAP API info
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  testServer(change_sample_status_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url),
             {

               session$setInputs(sample_status = 2,studies = "Test" ,distribution_recipient = "Distributed",
                                 sample_status_file = list(datapath = "sample status update test file.csv"))


               #Check that redcap export works
               expect_true(length(redcap_status()$sample_id)>0)

               #Check that columns are correct in current status output
               expect_equal(colnames(current_status()),c("record_id","redcap_event_name","sample_id","sample_status","sample_distribution_recipient"))


             })
})
