#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests


# WIll NEED TO BE UPDATED EVERYTIME A NEW STUDY IS ADDED
test_that("check that barcodes get translated correctly", {


# COMET -------------------------------------------------------------------

  #Create COMET Upload File
  test_comet_upload <- tibble(`Scan Barcode` = c("628.xxxx D-1 SR1",
                                                "628.xxxx D0 PL1",
                                                "628.xxxx D1 WBs1",
                                                "628.xxxx D2 WBm1",
                                                "628.xxxx D3 WB1",
                                                "628.xxxx D10 hPBMC1",
                                                "628.xxxx D11 PBMC1",
                                                "628.xxxx D15 RLT1",
                                                "628.xxxx D20 WU1",
                                                "628.xxxx D21 USP1",
                                                "628.xxxx D30 CLOT1",
                                                "628.xxxx M3 UCL1",
                                                "628.xxxx M6 NS1",
                                                "628.xxxx M12 TAC1"),
                              `Date and Time of Collection (MM/DD/YY HR:MIN)` = NA,
                              `Sample Volume` = NA,
                              `Sample Volume Units` = NA,
                              `Container Type` = NA,
                              `Scan container barcode` = NA,
                              `Grid Position` = NA,
                              `Freezer number` = NA,
                              `Freezer Shelf Number` = NA,
                              `Date Stored in Freezer` = NA,
                              `Staff member checking in` = NA)

  comet_barcode_test <- parse_barcode(test_comet_upload,"COMET")

  #Check Sample Types and Timepoints
  expect_equal(as.character(comet_barcode_test$Timepoint), c("Day -1","Day 0","Day 1","Day 2","Day 3","Day 10","Day 11",
                                                             "Day 15","Day 20","Day 21","Day 30","Month3","Month6","Month12"))
  expect_equal(as.character(comet_barcode_test$Sample), c("Serum","Plasma","Whole blood - Smart Tube Buffer","Whole blood - Max Par Buffer",
                                                          "Whole Blood","hPBMC","PBMC","PBMC in RLT","Urine","Urine Supernatant",
                                                          "Clot","Urine Pellet Clot","Nasal Swabs","TA CyTOF"))


# EARLI -------------------------------------------------------------------

  #Create EARLI Upload File
  test_earli_upload <- tibble(`Scan Barcode` = c("xxx-01",
                                                 "xxx-02",
                                                 "xxx-03",
                                                 "xxx-04",
                                                 "xxx-05",
                                                 "xxx-06",
                                                 "xxx-07",
                                                 "xxx-08A",
                                                 "xxx-08B",
                                                 "xxx-09",
                                                 "xxx-10",
                                                 "xxx-11",
                                                 "xxx-12A",
                                                 "xxx-12B",
                                                 "xxx-13",
                                                 "xxx-14",
                                                 "xxx-15",
                                                 "xxx-16"),
                              `Timepoint (If not in Barcode)` = "Day2",
                              `Patient ID (If not in Barcode)` = NA,
                              `Date and Time of Collection (MM/DD/YY HR:MIN)` = NA,
                              `Sample Volume` = NA,
                              `Sample Volume Units` = NA,
                              `Container Type` = NA,
                              `Scan container barcode` = NA,
                              `Grid Position` = NA,
                              `Freezer number` = NA,
                              `Freezer Shelf Number` = NA,
                              `Date Stored in Freezer` = NA,
                              `Staff member checking in` = NA)

  earli_barcode_test <- parse_barcode(test_earli_upload,"EARLI")

  #EARLI
  #Check sample translation, Timepoint extraction
  expect_equal(as.character(earli_barcode_test$Sample),c("Urine","Urine","Urine","Urine","Citrate","Citrate","Citrate","Citrate","Citrate","EDTA","EDTA","EDTA","EDTA","EDTA","Urine",
               "DNA","DNA","PaxGene"))
  expect_equal(earli_barcode_test$Timepoint[[1]],"Day 2")
})

