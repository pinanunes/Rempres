library(RCurl)
#load("disease_list.RData")
#' Retrieve outbreak data 
#' 
#' @param disease Disease name. To check valid names use disease_list()
#' @param region Globe region. Valid values are "Africa","Asia","Europe","Americas","Oceania"
#' @param sodate Starting observation date
#' @param eodate ending observation date
#' @param srdate Starting reporting date
#' @param erdate Ending reporting date
#' @import RCurl
#' @export
Empres.data <- function(disease=NA, region=NA,sodate=NA,eodate=NA,srdate=NA,erdate=NA) {

  base.url <- "http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&SRSNAME=EPSG:4326&filter="
  start <- "<And>"
  ## Disease check and parser
  if (missing(disease)) {
    disease <- ""
  } else if (length(disease) == 1) {
    ndisease <- length(disease.check(disease))
    if (ndisease == 1) {
      disease <- parser("equal", "disease", disease.check(disease))
    } else if (ndisease > 1) {
      print(paste("More than one disease match for ", disease, ". Matching diseases:", 
                  sep = ""))
      print(disease.check(disease))
      dis <- ""
      for (n in 1:ndisease) {
        di <- parser("equal", "disease", disease.check(disease)[n])
        dis <- paste(dis, di, sep = "")
      }
      disease <- paste("<Or>", dis, "</Or>", sep = "")
    }
  } else if (length(disease) > 1) {
    nd <- length(disease)
    dis.k <- ""
    for (k in 1:nd) {
      ndisease <- length(disease.check(disease[k]))
      if (ndisease == 1) {
        dis <- parser("equal", "disease", disease.check(disease[k]))
      } else if (ndisease > 1) {
        print(paste("More than one disease match for ", disease, 
                    ". Matching diseases:", sep = ""))
        print(disease.check(disease))
        dis <- ""
        for (n in 1:ndisease) {
          di <- parser("equal", "disease", disease.check(disease[k])[n])
          dis <- paste(dis, di, sep = "")
        }
      }
      dis.k <- paste(dis.k, dis, sep = "")
    }
    disease <- paste("<Or>", dis.k, "</Or>", sep = "")
  }
  ## Region parser
  if(missing(region)){
   
    Reg<-""
  } else if (length(region)==1){
   
    Reg<-parser("equal","region",region.check(region))
  } else if (length(region)>1){
  
    nreg<-length(region)
    reg<-""
    for (n in 1:nreg){
     reg<-paste(reg,parser("equal","region",region.check(region[n])),sep="")  
    }
    Reg<-paste("<Or>",reg,"</Or>",sep="")
  }
  
  ##Date parser
  if(missing(srdate)){
    srdt<-""
  }else{
    srdt<-parser("greater","reportingDateYYYYMMDD",check.date(srdate))
  }
  if(missing(erdate)){
    erdt<-""
  }else{
    erdt<-parser("lesser","reportingDateYYYYMMDD",check.date(erdate))
  }
  if(missing(sodate)){
    sodt<-""
  }else{
    sodt<-parser("greater","observationDateYYYYMMDD",check.date(sodate))
  }
  if(missing(eodate)){
    eodt<-""
  }else{
    eodt<-parser("lesser","observationDateYYYYMMDD",check.date(eodate))
  }
    
  del <- parser("equal", "deleted", "false")
  conf <- parser("equal", "confidential", "false")
  draft<-parser("equal", "draft", "false")
  finish <- "</And>"
  format <- "&outputFormat=CSV"
  sstring <- paste(base.url, start, disease,Reg,sodt,eodt,srdt,erdt,draft,del, conf, finish, format, 
                  sep = "")
  x <- getURL(sstring, ssl.verifypeer = FALSE)
  y <- read.csv(text = x)
  #y<-y[,c("FID","source,"latitude","longitude","region","localityName","localityQuality","observationDate", "reportingDate","status","disease","serotypes","speciesDescription")]
  #print(sstring)
  return(y)
}

region.check<-function(region){
  regions<-c("Africa","Asia","Europe","Americas","Oceania")
  if (any(grep(region,regions, perl = TRUE)) == TRUE) {
    regn<-grep(region,regions, perl = TRUE)
    reg<-regions[regn]
    }
  else {
    stop("Region not found. Use region.list() for a list of regions")
  }
  return(reg)
}

check.date<-function(date){
  if(nchar(date)<8){ 
    stop("Date value should be in yyyymmdd format")
  } else if(as.numeric(substr(date,1,4))<1827){
    stop("Year value should be higher than 1827")
  } else if(as.numeric(substr(date,5,6))>12){
    stop("Month value should be between 01 and 12")
  }else if(as.numeric(substr(date,7,8))>31){
    stop("Day value should be between 01 and 31")
  } else if(as.numeric(substr(date,7,8))>as.numeric(format(Sys.Date(), "%Y%m%d"))){
    stop("Date should be anterior or equal to the current date")
  }else {
    return(date)
  }
  
}

parser <- function(operation, property, value) {
  if (operation == "equal") {
    opb <- "<PropertyIsEqualTo>"
    opf <- "</PropertyIsEqualTo>"
  } else if (operation == "lesser") {
    opb <- "<PropertyIsLessThanOrEqualTo>"
    opf <- "</PropertyIsLessThanOrEqualTo>"
  } else if (operation == "greater") {
    opb <- "<PropertyIsGreaterThanOrEqualTo>"
    opf <- "</PropertyIsGreaterThanOrEqualTo>"
  }
  pr <- paste("<PropertyName>", gsub(" ", "%20", property), "</PropertyName>", 
              sep = "")
  val <- paste("<Literal>", gsub(" ", "%20", value), "</Literal>", sep = "")
  result <- paste(opb, pr, val, opf, sep = "")
  return(result)
}


disease.check <- function(disease) {
  d <- toupper(disease)
  if (any(grep(d, disease.list$Udisease, perl = TRUE)) == TRUE) {
    rows <- grep(d, disease.list$Udisease, perl = TRUE)
    result <- disease.list$disease[rows]
  } else {
    stop("Disease not found. Use disease.list() for a list of diseases.")
  }
  return(result)
}
