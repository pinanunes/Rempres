library(RCurl)
Empres.data <- function(disease,region=""){
  base.url<-"http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&SRSNAME=EPSG:4326&filter="
  start<-"<And>"
 ##Disease check and parser  
  if (length(disease)==0){
    disease<-""
  } else if (length(disease)==1){
     ndisease<-length(disease.check(disease))
     if (ndisease==1){
       disease<-parser("equal","disease",disease.check(disease))
        } else if (ndisease>1){
          print(paste("More than one disease match for ",disease,". Matching diseases:",sep=""))
          print(disease.check(disease))
          dis<-""
          for (n in 1:ndisease){
            di<-parser("equal","disease",disease.check(disease)[n])
            dis<-paste(dis,di,sep="")
          }
         disease<-paste("<Or>",dis,"</Or>",sep="")   
        }
  }else if(length(disease)>1){
          nd<-length(disease)
          dis.k<-""
          for (k in 1:nd){
                          ndisease<-length(disease.check(disease[k]))
                          if (ndisease==1){
                                           dis<-parser("equal","disease",disease.check(disease[k]))          
                                          } 
                          else if (ndisease>1){
                            print(paste("More than one disease match for ",disease,". Matching diseases:",sep=""))
                            print(disease.check(disease))
                                               dis<-""
                                               for (n in 1:ndisease){
                                                                     di<-parser("equal","disease",disease.check(disease[k])[n])
                                                                     dis<-paste(dis,di,sep="")
                                                                    }
                                              }
                           dis.k<-paste(dis.k,dis,sep="")
                          }
          disease<-paste("<Or>",dis.k,"</Or>",sep="")
        }
##Region parser

  
  del<-parser("equal","deleted","false")
  conf<-parser("equal","confidential","false")
  finish<-"</And>"
  format<-"&outputFormat=CSV"
  string<-paste(base.url,start,disease,del,conf,finish,format,sep="")
  x<-getURL(string,ssl.verifypeer = FALSE)
  y <- read.csv(text = x)
  #print(string)
  return(y)
}

parser<-function(operation, property, value){
  if (operation=="equal"){
    opb<-"<PropertyIsEqualTo>"
    opf<-"</PropertyIsEqualTo>"
    }else if(operation=="less"){
      opb<-"<PropertyIsLessThan>"
      opf<-"</PropertyIsLessThan>"
    }else if(operation=="greater"){
      opb<-"<PropertyIsGreaterThan>"
      opf<-"</PropertyIsGreaterThan>"
    }
  pr<-paste("<PropertyName>",gsub(" ","%20",property),"</PropertyName>",sep="")
  val<- paste("<Literal>",gsub(" ","%20",value),"</Literal>",sep="")
  result<-paste(opb,pr,val,opf,sep="")
  return(result)
}


disease.check<-function(disease){
  d<-toupper(disease)
  if (any(grep(d,disease.list$Udisease,perl=TRUE))==TRUE ){
  rows<-grep(d,disease.list$Udisease,perl=TRUE)
  result<-z$disease[rows]
  } else {
    stop("Disease not found. Use disease.list() for a list of diseases.")
  }
  return(result)
}




