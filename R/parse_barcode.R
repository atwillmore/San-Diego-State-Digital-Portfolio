#Study specific extraction of barcode info, select correct columns

parse_barcode <- function(upload_data,study){

  #COMET
  if(study == "COMET"){

    #Translate Timepoint from COMET Barcode to REDCap
    timepoint_barcode <- c("D-1","D0","D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11","D12","D13","D14","D15","D16","D17","D18","D19","D20","D21","D22",
                           "D23","D24","D25","D26","D27","D28","D29","D30","M3","M6","M9","M12")
    timepoint_rc <- c("Day -1","Day 0","Day 1","Day 2","Day 3","Day 4","Day 5","Day 6","Day 7","Day 8","Day 9","Day 10","Day 11",
                      "Day 12","Day 13","Day 14","Day 15","Day 16","Day 17","Day 18","Day 19","Day 20","Day 21",
                      "Day 22","Day 23","Day 24","Day 25","Day 26","Day 27","Day 28","Day 29","Day 30","Month3","Month6","Month9","Month12")

    #Translate Sample Type from COMET Barcode to REDCap

    #Extract sample info from barcode - patient_id, sample type, timepoint.
    samples_for_upload_cleaned <- upload_data %>% separate(`Scan Barcode`, into = c("Patient_ID", "Timepoint", "Sample"), sep = " ", remove = FALSE) %>%
      separate(Patient_ID, into = c("Hosp", "Patient_ID"), sep = "\\.") %>%
      mutate(Sample = case_when(str_detect(Sample,"SR") == TRUE ~ "Serum",
                                str_detect(Sample,"PL") == TRUE ~ "Plasma",
                                str_detect(Sample,"WBs") == TRUE ~ "Whole blood - Smart Tube Buffer",
                                str_detect(Sample,"WBm") == TRUE ~ "Whole blood - Max Par Buffer",
                                str_detect(Sample,"WB") == TRUE ~ "Whole Blood",
                                str_detect(Sample,"hPBMC") == TRUE ~ "hPBMC",
                                str_detect(Sample,"PBMC") == TRUE ~ "PBMC",
                                str_detect(Sample,"RLT") == TRUE ~ "PBMC in RLT",
                                str_detect(Sample,"WU") == TRUE ~ "Urine",
                                str_detect(Sample,"USP") == TRUE ~ "Urine Supernatant",
                                str_detect(Sample,"CLOT") == TRUE ~ "Clot",
                                str_detect(Sample,"UCL") == TRUE ~ "Urine Pellet Clot",
                                str_detect(Sample,"NS") == TRUE ~ "Nasal Swabs",
                                str_detect(Sample,"TAC") == TRUE ~ "TA CyTOF"),
             Timepoint = factor(Timepoint, levels = timepoint_barcode, labels = timepoint_rc)) %>%
      select(`Scan Barcode`,Patient_ID:Sample,`Date and Time of Collection (MM/DD/YY HR:MIN)`:`Staff member checking in`)

  }

  #EARLI
  else if(study == "EARLI" || study == "Test"){

    #Translate sample type from EARLI Barcode to REDCap
    sample_barcode <- c("01","02","03","04","05","06","07","08A","08B","09","10","11","12A","12B","13","14","15","16")
    sample_rc <- c("Urine","Urine","Urine","Urine","Citrate","Citrate","Citrate","Citrate","Citrate","EDTA","EDTA","EDTA","EDTA","EDTA","Urine",
                   "DNA","DNA","PaxGene")

    #Extract sample info from barcode - sample type, recode timepoint to day 1,2,3 - only options for earli
    samples_for_upload_cleaned <- upload_data %>% separate(`Scan Barcode`, into = c("Identifier", "Sample"), sep = "-", remove = FALSE) %>%
      mutate(Sample = factor(Sample, levels = sample_barcode, labels = sample_rc),
             Timepoint = as.character(`Timepoint (If not in Barcode)`),
             Timepoint = case_when(str_detect(Timepoint,"2") == TRUE ~ "Day 2",
                                   str_detect(Timepoint,"1") == TRUE ~ "Day 1",
                                   str_detect(Timepoint,"0") == TRUE ~ "Day 0")) %>%
      select(`Scan Barcode`,`Patient ID (If not in Barcode)`,Timepoint,Sample,`Date and Time of Collection (MM/DD/YY HR:MIN)`:`Staff member checking in`)

  }

  return(samples_for_upload_cleaned)

}
