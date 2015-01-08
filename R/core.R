library(RCurl)
Empres.data <- function(disease,region=""){
  base.url<-"http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&SRSNAME=EPSG:4326&filter="
  start<-"<And><PropertyIsEqualTo>"
  disease<-gsub(" ","%20",disease)
  disease<-paste("<PropertyName>disease</PropertyName><Literal>",disease,"</Literal>",sep="")
  finish<-"</PropertyIsEqualTo>"
  del<-"<PropertyIsEqualTo><PropertyName>deleted</PropertyName><Literal>false</Literal></PropertyIsEqualTo>"
  conf<-"<PropertyIsEqualTo><PropertyName>confidential</PropertyName><Literal>false</Literal></PropertyIsEqualTo></And>"
  format<-"&outputFormat=CSV"
  string<-paste(base.url,start,disease,finish,del,conf,format,sep="")
  x<-getURL(string,ssl.verifypeer = FALSE)
  y <- read.csv(text = x)
  print(string)
  return(y)
}



disease.check<-function(disease){
  d<-toupper(disease)
  if (any(grep(d,disease.list$Udisease,perl=TRUE))==TRUE ){
  rows<-grep(d,disease.list$Udisease,perl=TRUE)
  result<-z$disease[rows]
  } else {
    result<-"ERROR"
  }
  return(result)
}

a<-Empres.data(disease.check(c("brucellosis")))
unique(a$disease)
a$confidential
b<-a[a$disease=="disease",]
a<-Empres.data(disease.check("african"))
disease.check("Bluetongue")
dados<-Empres.data("Foot and mouth disease")

gsub(" ","%20","African swine fever")

x <- getURL("http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=empresi3g:outbreaks&propertyname=disease&service=wfs",ssl.verifypeer = FALSE)

y <- read.csv(text = x)
install.packages("roxygen2")
unique(a$disease)
b<-a[a$disease=="African horse sickness",]
string<-"http://empres-i.fao.org/eipws3g/ei3gmapcompgis/localgs/wfs?service=wfs&version=2.0.0&request=getfeature&typeName=empresi3g:outbreaks&propertyName=disease&maxfeatures=100000&outputFormat=CSV"
x<-getURL(string,ssl.verifypeer = FALSE)
y <- read.csv(text = x)
z<-as.data.frame(unique(y$disease))
colnames(z)[1]<-"disease"
sapply(z,%in%,"African swine fever")
toupper("African Swine fever") %in% z$Udisease
d<-toupper("brucellosis")
z$Udisease<-toupper(z$disease)
which(apply(z, 2, function(x) any(grepl(d, x))))
which(any(grep(d,z)))
g<-grep(d,z$disease,perl=TRUE)
z$disease[g]
disease.list<-z
