library("dplyr")
library("tidyverse")
library("purrr")
library("stringr")
library("splus2R")
library("data.table")
library(sqldf)


############
###part 1###
############


#csv appending function
multmerge = function(mypath){
  filenames=list.files(path=mypath, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T, encoding= "UTF-8")})
  Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)}






setwd("~/Desktop/crm/2021/jan-april8th")


#for the next for loop 
csvfilenames <- list.files("~/Desktop/crm/2021/jan-april8th",pattern="*.csv", all.files=FALSE, full.names=FALSE)

#adds file name as a colummn into the csvs  
#cleans each individual RSVO list and stores them as individual csvs
for(i in 1:length(csvfilenames))
{
  tempraw<-read.csv(paste(csvfilenames[i])) #give path if its not your wd
  temp<-read.csv(paste(csvfilenames[i])) #give path if its not your wd
  
  temp=temp[1:7]
  temp$filename_tag<-paste(substr(csvfilenames[i],1,nchar(csvfilenames[i])-4))
  names(temp)[1:8]=c("status", "first", "last", "title", "company", "email","location","event.attended")
  write.csv(temp,paste("~/Desktop/crmappend/",csvfilenames[i],sep=""),row.names=FALSE)
  
}

############
###part 2###
############


setwd("~/Desktop/crmappend")

#compiling all the cleaned csvs togetherr
crmdata=multmerge('~/Desktop/crmappend')

#using the csv name to create the column: city
city=rep("",length(crmdata$event.attended))

for (i in 1:length(crmdata$event.attended)){ 
  if (grepl("Sydney", crmdata$event.attended[i])==TRUE|grepl("Sydney", crmdata$location[i])==TRUE){
    city[i]="Sydney"
  }else if (grepl("Melbourne", crmdata$event.attended[i])==TRUE|grepl("Melbourne", crmdata$location[i])==TRUE){
    city[i]="Melbourne"
  }else if (grepl("Brisbane", crmdata$event.attended[i])==TRUE|grepl("Brisbane", crmdata$location[i])==TRUE){
    city[i]="Brisbane"
  }else if (grepl("Adelaide", crmdata$event.attended[i])==TRUE|grepl("Adelaide", crmdata$location[i])==TRUE){
    city[i]="Adelaide"
  }else if (grepl("Perth", crmdata$event.attended[i])==TRUE|grepl("Perth", crmdata$location[i])==TRUE){
    city[i]="Perth"
  }else if (grepl("Canberra", crmdata$event.attended[i])==TRUE|grepl("Canberra", crmdata$location[i])==TRUE|grepl("ACT", crmdata$location[i])==TRUE){
    city[i]="Canberra"
  }else if (grepl("Singapore", crmdata$event.attended[i])==TRUE|grepl("Singapore", crmdata$location[i])==TRUE){
    city[i]="Singapore"
  }else if (grepl("Auckland", crmdata$event.attended[i])==TRUE|grepl("Auckland", crmdata$location[i])==TRUE){
    city[i]="Auckland"
  }else if (grepl("Bangkok", crmdata$event.attended[i])==TRUE|grepl("Bangkok", crmdata$location[i])==TRUE){
    city[i]="Bangkok"
  }else if (grepl("Jakarta", crmdata$event.attended[i])==TRUE|grepl("Jakarta", crmdata$location[i])==TRUE){
    city[i]="Jakarta"
  }else if (grepl("Hong Kong", crmdata$event.attended[i])==TRUE|grepl("Hong Kong", crmdata$location[i])==TRUE){
    city[i]="Hong Kong"
  }else if (grepl("Shenzhen", crmdata$event.attended[i])==TRUE|grepl("Shenzhen", crmdata$location[i])==TRUE){
    city[i]="Shenzhen"
  }else if (grepl("Webinar", crmdata$event.attended[i])==TRUE|grepl("Webinar", crmdata$location[i])==TRUE){
    city[i]="Webinar"
  }else if (grepl("HK", crmdata$event.attended[i])==TRUE|grepl("HK", crmdata$location[i])==TRUE){
    city[i]="Hong Kong"
  }else if (grepl("Kuala Lumpur", crmdata$event.attended[i])==TRUE|grepl("Kuala Lumpur", crmdata$location[i])==TRUE){
    city[i]="Kuala Lumpur"
  }else if (grepl("Australia", crmdata$event.attended[i])==TRUE|grepl("Australia", crmdata$location[i])==TRUE){
    city[i]="Australia"
  }else if (grepl("Malaysia", crmdata$event.attended[i])==TRUE|grepl("Malaysia", crmdata$location[i])==TRUE){
    city[i]="Malaysia"
  }else if (grepl("Manila", crmdata$event.attended[i])==TRUE|grepl("Manila", crmdata$location[i])==TRUE){
    city[i]="Manilla"
    
  }
}



crmdata1=data.frame(city, crmdata$event.attended,crmdata$status, crmdata$first,crmdata$last,crmdata$title,
                    crmdata$company,crmdata$email 
                    )



names(crmdata1)[1:8]=c("location","eventattended","status", "first", "last", "title", "company","email")

#getting rid of bad observations
crmdata2=crmdata1[which((crmdata1[,2]!=''&crmdata1[,3]!=''&crmdata1[,4]!=''&crmdata1[,8]!='')),]

crmdata3=crmdata2[order(crmdata2$eventattended),]





#de-duplicating against unsubs/bad emails
setwd("~/Desktop/crm")
ddup=read.csv("unsubs.csv")
bounce=read.csv("bounce.csv")


#dedup from unsub
q3=sqldf('select *  from crmdata3 where lower(email)  not in (select lower(email) from ddup)')
q3_=sqldf('select *  from q3 where lower(email)  not in (select lower(email) from bounce)')


#only take rows that have valid contact details
q4=sqldf('select *  from q3_ where 
         email like "%@%"
         
         ')


write.csv(q4,"crm2021april-may27th.csv", row.names = FALSE)


