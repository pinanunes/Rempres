library(RCurl)
Empres.data <- function(disease,region=""){
  base.url<-"http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&SRSNAME=EPSG:4326&filter="
  start<-"<PropertyIsEqualTo>"
  disease<-gsub(" ","%20",disease)
  disease<-paste("<PropertyName>disease</PropertyName><Literal>",disease,"</Literal>",sep="")
  finish<-"</PropertyIsEqualTo>"
  format<-"&outputFormat=CSV"
  string<-paste(base.url,start,disease,finish,format,sep="")
  x<-getURL(string,ssl.verifypeer = FALSE)
  y <- read.csv(text = x)
  return(y)
}

dados<-Empres.data("Foot and mouth disease")

gsub(" ","%20","African swine fever")

x <- getURL("http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&propertyname=disease&service=wfs",ssl.verifypeer = FALSE)

y <- read.csv(text = x)
