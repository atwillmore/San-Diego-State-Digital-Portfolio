# Function for the confirmation message -----------------------------------
modal <-function(id){
  modal <- modalDialog(
    "Are you sure you want to upload?",
    title = "Upload",
    footer = tagList(
      actionButton(NS(id,"ok"), "Upload", class = "btn btn-success"),
      actionButton(NS(id,"cancel"), "Cancel", class = "btn btn-warning")
    )
  )

  return(modal)
}

# Function for getting event name from selected study ---------------------
get_event_name<- function(studies_list,selected_study){
  for(row in 1:nrow(studies_list)){
    if(studies_list[row,"event_name"] == selected_study){
      event <- studies_list[row,"unique_event_name"]
      return(event)
    }
  }
}



# Function that returns translation table and event names -----------------
REDCap_info <- function(){

  #REDCAP API
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  #List of study names exported from REDCap to be used in most of the modules
  studies <- exportEvents(rcon = rcon_lims)

  #Data dictionary export for mapping code to labels - LABELS MUST EXACTLY MATCH IN EXCEL TEMPLATES
  dictionary <- exportMetaData(rcon = rcon_lims)

  #Function for parsing out labels and code for a specified field. Will create 2 columns called code and label that function as a translation table
  translate_code_labels <- function(field_name){

    #Filter for field, separate options by pipe (|) into 70 potential options (arbitrary high number), pivot to long and separate by comma into code and label
    translation_table <- dictionary %>% filter(field_name == !!field_name) %>%
      separate(select_choices_or_calculations,into = c("1","2","3","4","5","6","7","8","9","10",
                                                       "11","12","13","14","15","16","17","18","19","20",
                                                       "21","22","23","24","25","26","27","28","29","30",
                                                       "31","32","33","34","35","36","37","38","39","40",
                                                       "41","42","43","44","45","46","47","48","49","50",
                                                       "51","52","53","54","55","56","57","58","59","60",
                                                       "61","62","63","64","65","66","67","68","69","70"), sep = "\\|", fill = "right") %>%
      pivot_longer(`1`:`70`,values_drop_na = TRUE) %>%
      separate(value, into = c("code","label"), sep = ",") %>%
      mutate(across(c(code,label),trimws))

    return(translation_table)

  }

  timepoint_translation <- translate_code_labels("timepoint")
  sample_type_translation <- translate_code_labels("sample_type")
  sample_volume_unit_translation <- translate_code_labels("sample_volume_units")
  container_type_translation <- translate_code_labels("container_type")
  freezer_number_translation <- translate_code_labels("freezer_number")
  freezer_shelf_number_translation <- translate_code_labels("freezer_shelf_number")


  #Create Translation tables - match code to label in redcap. Returned as a list, to access components, would use events_translation_tables[x]
  events_translation_tables <- list(studies = studies,
                                    timepoint = timepoint_translation$label,
                                    timepoint_code = timepoint_translation$code,
                                    type = sample_type_translation$label,
                                    type_code = sample_type_translation$code,
                                    volume_units = sample_volume_unit_translation$label,
                                    volume_units_code = sample_volume_unit_translation$code,
                                    container = container_type_translation$label,
                                    container_code = container_type_translation$code,
                                    freezer = freezer_number_translation$label,
                                    freezer_code = freezer_number_translation$code,
                                    freezer_shelf = freezer_shelf_number_translation$label,
                                    freezer_shelf_code = freezer_shelf_number_translation$code)

  return(events_translation_tables)

}
