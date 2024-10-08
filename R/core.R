library(httr)
library(readr)
library(dplyr)
library(lubridate)
data("disease_list2")

#' Retrieve outbreak data
#'
#' @param diseasename Disease name. To check valid names use disease_list()
#' @param region Globe region. Valid values are "Africa","Asia","Europe","Americas","Oceania"
#' @param country reporting country
#' @param start_date Starting observation date in format YYYYMMDD
#' @param end_date ending observation date in format YYYYMMDD
#' @param species Affected species
#' @import httr
#' @import dplyr
#' @import readr
#' @import lubridate
#' @export
Empres.data <- function(diseasename = NA,
                        region = NA,
                        country = NA,
                        startdate = NA,
                        enddate = NA,
                        species = NA) {
  start_date <- if (!is.na(startdate)) check.date(startdate) else NA
  end_date <- if (!is.na(enddate)) check.date(enddate) else NA
  
  #
  # print(paste("Downloading data for:",disease_t))
  consolidated_data <- data.frame()
  current_start <- start_date
  
  while (!is.na(current_start) && !is.na(end_date) && current_start < end_date) {
    current_end <- min(current_start %m+% months(6) - days(1), end_date)
    data <- fetch_data(current_start, current_end)
    
    if (!is.null(data)) {
      consolidated_data <- bind_rows(consolidated_data, data)
    }
    
    current_start <- current_end + days(1)
  }
  if (!is.na(diseasename) && length(diseasename) > 0) {
    disease_t <- disease.check(diseasename)
    consolidated_data <- consolidated_data %>% filter(disease %in% disease_t)
    for(d in disease_t){
    print(paste("Outbreak data retrieved for:",d, " | ", nrow(consolidated_data[consolidated_data$disease==d,]), "rows found for the date interval"))
    print(paste("for more information check:",disease_list2$url[disease_list2$disease_name %in% d],";",disease_list2$link_EFSA[disease_list2$disease_name %in% d]))
    }
      }else{
        print( paste("No disease was selected:",nrow(consolidated_data), "rows found"))
      }  
      
  if(!is.na(region)){
    reg_t<-region.check(region)
    consolidated_data <- consolidated_data %>% filter(region %in% reg_t)
    }
  
  if (!is.na(species) && length(species) > 0) {
    consolidated_data <- consolidated_data %>% filter(grepl(species, consolidated_data$species))
    print(paste("Outbreak data retrieved for species:", species, " | ", nrow(consolidated_data), "rows found for the date interval"))
  }
  
  return(consolidated_data)

    
}



fetch_data <- function(start_date, end_date) {
  url <- "https://europe-west1-fao-empresi.cloudfunctions.net/getLatestEventsByDate"
  response <- GET(
    url,
    query = list(
      animal_type = "all",
      diagnosis_status = "confirmed",
      disease = "all",
      start_date = start_date,
      end_date = end_date
    )
  )
  
  if (response$status_code == 200) {
    data <- read_csv(content(response, "text"), show_col_types = FALSE)
    if (!is.null(data)) {
      data <- data %>%
        mutate(species_category = case_when(
          grepl("^\\[\"Domestic", species) ~ "Domestic",
          grepl("^\\[\"Wild", species) ~ "Wild",
          grepl("^\\[\"Captive", species) ~ "Captive",
          grepl("^\\[\"Environmental sample", species) ~ "Environmental sample",
          TRUE ~ NA_character_
        ))
    }
    return(data)
  } else {
    warning(paste("Failed to fetch data for", start_date, "to", end_date))
    return(NULL)
  }
}

region.check <- function(region) {
  regions <- c("Africa", "Asia", "Europe", "Americas", "Oceania")
  regn <- match(region, regions)
  if (is.na(regn)) {
    stop("Region not found. Use region.list() for a list of regions")
  }
  return(regions[regn])
}

check.date <- function(date) {
  if (nchar(date) < 8) {
    stop("Date value should be in yyyymmdd format")
  } else if (as.numeric(substr(date, 1, 4)) < 1827) {
    stop("Year value should be higher than 1827")
  } else if (as.numeric(substr(date, 5, 6)) > 12) {
    stop("Month value should be between 01 and 12")
  } else if (as.numeric(substr(date, 7, 8)) > 31) {
    stop("Day value should be between 01 and 31")
  } else if (as.Date(date,format="%Y%m%d") > Sys.Date()) {
    stop("Date should be anterior or equal to the current date")
  } else {
    date<-as.Date(date,format="%Y%m%d")
    return(date)
  }
  
}



disease.check <- function(disease_m) {
  result <- c()
  
  for (disease in disease_m) {
    match1 <- disease_list2 %>% filter(disease_name %in% disease)
    mdisease1 <- match1$disease_name
    
    match2 <- disease_list2 %>% filter(acronym %in% disease)
    mdisease2 <- match2$disease_name
    
    match3 <- disease_list2 %>% filter(portuguese_name %in% disease)
    mdisease3 <- match3$disease_name
    
    match4 <- disease_list2 %>% filter(common_english_name %in% disease)
    mdisease4 <- match4$disease_name
    
    match5 <- disease_list2 %>% filter(portuguese_acronym %in% disease)
    mdisease5 <- match5$disease_name
    
    match <- c(mdisease1, mdisease2, mdisease3, mdisease4, mdisease5)
    match <- c(unique(match))
    if (length(match) == 0) {
      all_diseases <- c(disease_list2$disease_name, disease_list2$portuguese_name, disease_list2$acronym, disease_list2$common_english_name)
      distances <- adist(disease, all_diseases)
      closest_match <- all_diseases[which.min(distances)]
      stop(paste("Disease not found:", disease, ". Did you mean:", closest_match, "? Use disease.list() for a list of diseases."))
    }
    result <- c(result, match)
  }
  
  return(unique(result))
}
