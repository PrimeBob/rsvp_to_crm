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





################################################################################################################################################
################################################################################################################################################


#
setwd("~/Desktop/crm")


all=read.csv("crm2019-2020.csv")

q5=sqldf('select * from bounce where 
status like "%[FAKE YES] No, extending to colleagues%" OR
status like "%[FAKE YES] NO, referred colleague%" OR
status like "%[FAKE YESY] NO, awayu%" OR
status like "%[HPE]%" OR
status like "%A - Registered%" OR
status like "%Accepted%" OR
status like "%ACCEPTED%" OR
status like "%ACCEPTED - TBC junior%" OR
status like "%ACCEPTED - TBC, IT company%" OR
status like "%ACCEPTED - TBC, not target org%" OR
status like "%ACCEPTED -TBC, not target org%" OR
status like "%ACCEPTED > TENTATIVE%" OR
status like "%accepted calendar%" OR
status like "%Accepted calendar%" OR
status like "%Accepted Calendar%" OR
status like "%Accepted Calendar  [Y]%" OR
status like "%Accepted Calendar [Target]%" OR
status like "%Accepted Calendar [Target] Colleague%" OR
status like "%Accepted Calendar [Y]%" OR
status like "%ACCEPTED- TBC, not target org%" OR
status like "%Adobe%" OR
status like "%Advise of colleagues (below)%" OR
status like "%colleague%" OR
status like "%Colleague%" OR
status like "%Already advised no colleagues%" OR
status like "%Already Attending/ Presenting%" OR
status like "%Already in invite list%" OR
status like "%already registered- check%" OR
status like "%Alvien colleague%" OR
status like "%America RECORDING%" OR
status like "%Andrew colleague%" OR
status like "%Andy Williams%" OR
status like "%Annual leave%" OR
status like "%Ask for Colleague%" OR
status like "%Asked about events in shanghai%" OR
status like "%Asked about price fu 23/9%" OR
status like "%asked agenda%" OR
status like "%Asked agenda%" OR
status like "%Asked details%" OR
status like "%ASKED DETAILS%" OR
status like "%asked for colleague%" OR
status like "%ASKED FOR COLLEAGUE%" OR
status like "%asked for colleague 9/4%" OR
status like "%ASKED FOR DETAILS%" OR
status like "%Asked for further info%" OR
status like "%Asked how got details%" OR
status like "%Asked how got details - wrong profile%" OR
status like "%Asked if meant for them%" OR
status like "%Asked if mel event too%" OR
status like "%asked more details%" OR
status like "%Asked more details%" OR
status like "%asked more info%" OR
status like "%Asked more information%" OR
status like "%Asked price%" OR
status like "%Asked Price%" OR
status like "%ASked price%" OR
status like "%ASKED PRICE%" OR
status like "%Asked Q%" OR
status like "%Asked Question%" OR
status like "%ASKED question%" OR
status like "%ASKED Question%" OR
status like "%ASKED QUESTIOn%" OR
status like "%ASKED QUESTION%" OR
status like "%ASKED QUESTION - EMAIL%" OR
status like "%asked questions%" OR
status like "%Asked questions%" OR
status like "%Asked relevance%" OR
status like "%Asked the Cost: Awaiting confirmation (FU 12/11)%" OR
status like "%asked when invitatoin was sent%" OR
status like "%Asked who other rep was%" OR
status like "%at capacity%" OR
status like "%At Perth til 6/3%" OR
status like "%ATTENDEE CAMPAIGN 11/3%" OR
status like "%Attending Brekkie%" OR
status like "%Attending the Sydney Event%" OR
status like "%Attending with CIO%" OR
status like "%Attending with Information Management Specialist%" OR
status like "%Attention Vicky:%" OR
status like "%Awaiting Confirmation%" OR
status like "%Away%" OR
status like "%AWAY%" OR
status like "%away until sep%" OR
status like "%BASED IN AMSTERDAM%" OR
status like "%Based in KL%" OR
status like "%Based in Singapore%" OR
status like "%Based in USA until Jan 2020%" OR
status like "%Believes emails aren%" OR
status like "%Bigmarker%" OR
status like "%Bigmarker rego%" OR
status like "%BLOCKED%" OR
status like "%Blocked us%" OR
status like "%Brisbane based%" OR
status like "%Brisbane Based%" OR
status like "%BRISBANE BASED%" OR
status like "%BUSY%" OR
status like "%Calendar [N] Final Info [C]%" OR
status like "%Calendar [N] Final Info [Y]%" OR
status like "%Calendar [N] Recon [ ]%" OR
status like "%Calendar [N] Recon [Y]%" OR
status like "%Calendar [Y]%" OR
status like "%Calendar [Y] Recon [Y]%" OR
status like "%Calendar accepted%" OR
status like "%Calendar Conflict%" OR
status like "%Came from an OOO response%" OR
status like "%Can no longer attend%" OR
status like "%Can No Longer Attend%" OR
status like "%t make it YES (invited by colleague)%" OR
status like "%CANNOT ACCEPT%" OR
status like "%CANNOT ATTEND%" OR
status like "%CEO colleague%" OR
status like "%Chalinya colleague%" OR
status like "%CHANGED ROLES%" OR
status like "%Check Target Dept.%" OR
status like "%CHECK WITH CATHERINE - GOV%" OR
status like "%Check with Michaela%" OR
status like "%Checking price%" OR
status like "%Checking with HR Department%" OR
status like "%colleague%" OR
status like "%Christopher Seller%" OR
status like "%Colleagues%" OR
status like "%Cloudflare%" OR
status like "%Cloudflare Confirmed%" OR
status like "%colleague%" OR
status like "%Colleague%" OR
status like "%Colleague of Bora%" OR
status like "%Colleague of Chelsia%" OR
status like "%Colleague of Sean McDonnell%" OR
status like "%Colleague of Sharmini%" OR
status like "%Collegue of Adelle%" OR
status like "%COMPETITOR%" OR
status like "%CONFIRMED%" OR
status like "%Confirming%" OR
status like "%Confirming Role%" OR
status like "%Confirming with team%" OR
status like "%Confirming: Today%" OR
status like "%Conflict of Interest%" OR
status like "%Currentlty in France%" OR
status like "%colleague%" OR
status like "%DECLINDED%" OR
status like "%Decline: Dropout%" OR
status like "%Declined%" OR
status like "%DECLINED%" OR
status like "%DECLINED - Linkedin%" OR
status like "%DECLINED CALENDAR%" OR
status like "%Declined Calender%" OR
status like "%DECLINED No FU 17th AM%" OR
status like "%DECLINED not target org%" OR
status like "%DECLINED not target title%" OR
status like "%Declinedemail then accepted calendar%" OR
status like "%Defo coming%" OR
status like "%DEFO Coming%" OR
status like "%DELCINED%" OR
status like "%Dell confirmed%" OR
status like "%Dell Confirmed%" OR
status like "%Dell Confirmed: Venue Confirmed%" OR
status like "%Dennis%" OR
status like "%Dip Colleague ***%" OR
status like "%drop out%" OR
status like "%Drop out%" OR
status like "%Drop Out%" OR
status like "%DROP out%" OR
status like "%DROP OUT%" OR
status like "%DROP OUT - COVID 19%" OR
status like "%DROP OUT - Fires%" OR
status like "%Drop Out - Last Minute%" OR
status like "%DROP OUT - Requested colleague%" OR
status like "%DROP OUT : SENT REPRESENTATIVE%" OR
status like "%DROP OUT (COVID-19)%" OR
status like "%Drop Out 3/10%" OR
status like "%Drop Out: Off Sick%" OR
status like "%Drop-out%" OR
status like "%DROPBOX%" OR
status like "%DROPBOX - DROP OUT%" OR
status like "%Dropped out%" OR
status like "%Either he or another executive will attend%" OR
status like "%Either Head of HR or Head of FInance%" OR
status like "%England  - RECORDING%" OR
status like "%Enquired Further on Linkedin%" OR
status like "%EUROPE BASED%" OR
status like "%Eva Colleague%" OR
status like "%EXCLUSION%" OR
status like "%Extended leave%" OR
status like "%EXTENDED LEAVE%" OR
status like "%Extended to colleagues%" OR
status like "%Extending to colleagues%" OR
status like "%Fujitsu Confirmed%" OR
status like "%General Manager Colleague%" OR
status like "%colleague%" OR
status like "%GERMANY BASED%" OR
status like "%Germany RECORDING%" OR
status like "%Germany YES%" OR
status like "%GitHub confirmed%" OR
status like "%going%" OR
status like "%GRADUATE= NO%" OR
status like "%HK%" OR
status like "%HK _ Recoridng%" OR
status like "%HK Recording%" OR
status like "%HK RECORDING%" OR
status like "%HK YES%" OR
status like "%HPE Confirmed%" OR
status like "%HQ%" OR
status like "%in LEUI OF CIO%" OR
status like "%In Leui of head%" OR
status like "%In lie of Mudit%" OR
status like "%In Lieu CFO FU 10/9%" OR
status like "%in lieu of CIO%" OR
status like "%In Lieu of CSO%" OR
status like "%in lieu of GM IT%" OR
status like "%In lieu of Group Manager%" OR
status like "%In lieu of IT Director Troy McKenna%" OR
status like "%in lieu of MD%" OR
status like "%In lieu of Senior Audit Manager, Group Technology%" OR
status like "%in lieu of VP Technology%" OR
status like "%IN WAITING ROOM%" OR
status like "%INDIA YES%" OR
status like "%inquired%" OR
status like "%Inquired%" OR
status like "%Interested%" OR
status like "%Interested (Awaiting confirmation)%" OR
status like "%INTERESTED IN RECORDING?%" OR
status like "%Interstate%" OR
status like "%JAPAN BASED%" OR
status like "%Colleague%" OR
status like "%John Colleague%" OR
status like "%colleague%" OR
status like "%colleague%" OR
status like "%colleague%" OR
status like "%leave%" OR
status like "%Leave%" OR
status like "%leaving%" OR
status like "%Leaving early%" OR
status like "%LEFR%" OR
status like "%left%" OR
status like "%Left%" OR
status like "%LEFT%" OR
status like "%Left VM%" OR
status like "%LEFT: Key Target%" OR
status like "%LI - Not Interested%" OR
status like "%listen later%" OR
status like "%Listen Later%" OR
status like "%listen later - register%" OR
status like "%Live%" OR
status like "%live recording pls%" OR
status like "%Luke Butcher%" OR
status like "%Colleague%" OR
status like "%Mat Leave%" OR
status like "%MATERNITY LEAVE%" OR
status like "%melbourne based%" OR
status like "%Melbourne based%" OR
status like "%Melbourne Based%" OR
status like "%MELBOURNE BASED%" OR
status like "%Melbourne based - referred sydney colleague%" OR
status like "%Melbourne Based Key Target%" OR
status like "%Michael Yung colleague%" OR
status like "%Colleague%" OR
status like "%might not come%" OR
status like "%colleague%" OR
status like "%MOVED TO USA%" OR
status like "%Moving to new organisation%" OR
status like "%N0%" OR
status like "%Needs to leave around 9%" OR
status like "%Needs to leave at 1:30%" OR
status like "%Needs to see if he is the right person%" OR
status like "%Neil Colleague%" OR
status like "%New NO%" OR
status like "%New NO, away%" OR
status like "%New NO, no recording%" OR
status like "%no" OR
status like "%nO" OR
status like "%No" OR
status like "%NO" OR
status like "%NO%"

')



q5_pt2=sqldf('select * from bounce where 
status like "%on holiday%" OR
status like "%On leave%" OR
status like "%On leave til June%" OR
status like "%On Secondment%" OR
status like "%On-Demand%" OR
status like "%ooo%" OR
status like "%OoO%" OR
status like "%OOO%" OR
status like "%OOO - 1/5%" OR
status like "%OOO - 13/2%" OR
status like "%OOO - 14/6%" OR
status like "%OOO - 15/2%" OR
status like "%OOO - 15/4%" OR
status like "%OOO - 18/2%" OR
status like "%OOO - 29th April%" OR
status like "%OOO - 3/4%" OR
status like "%OOO - 5/4%" OR
status like "%OOO - 6/8%" OR
status like "%OOO - 8/4%" OR
status like "%OOO - 9th April%" OR
status like "%ooo - after event%" OR
status like "%OOO - after event%" OR
status like "%OOO - AFTER EVENT%" OR
status like "%OOO - after event - sending colleague%" OR
status like "%OOO - April 18%" OR
status like "%OOO - April 23%" OR
status like "%OOO - April 29%" OR
status like "%OOO - HK%" OR
status like "%OOO - Late March%" OR
status like "%OOO - leave%" OR
status like "%OOO - LONG SERVICE LEAVE%" OR
status like "%OOO - March 20%" OR
status like "%OOO - March 25%" OR
status like "%OOO - March 28%" OR
status like "%ooo - mat leave%" OR
status like "%OOO - Mat leave%" OR
status like "%OOO - Mat Leave%" OR
status like "%OOO - May 6%" OR
status like "%OOO - no date%" OR
status like "%OOO - no date (personal leave)%" OR
status like "%ooo - NOT TARGET%" OR
status like "%OOO - Oct 16%" OR
status like "%OOO - parental leave%" OR
status like "%OOO - Until Novemeber%" OR
status like "%ooo -after event%" OR
status like "%OOO 1/10%" OR
status like "%ooo 10/1%" OR
status like "%OOO 10/6%" OR
status like "%OOO 10/7%" OR
status like "%OOO 11/11%" OR
status like "%ooo 11/3%" OR
status like "%OOO 11/6%" OR
status like "%OOO 11/7%" OR
status like "%OOO 11/9%" OR
status like "%ooo 12/1%" OR
status like "%OOO 12/3%" OR
status like "%OOO 12/4%" OR
status like "%OOO 12/6%" OR
status like "%OOO 12/7%" OR
status like "%ooo 12/9%" OR
status like "%OOO 12/9%" OR
status like "%ooo 13/1%" OR
status like "%OOO 13/1%" OR
status like "%OOO 13/8%" OR
status like "%ooo 13/9%" OR
status like "%OOO 13/9%" OR
status like "%OOO 14/10%" OR
status like "%OOO 14/6%" OR
status like "%OOO 14/7%" OR
status like "%ooo 15/1%" OR
status like "%ooo 15/11%" OR
status like "%ooo 15/4%" OR
status like "%OOO 15/4%" OR
status like "%OOO 15/5%" OR
status like "%OOO 15/7%" OR
status like "%OoO 15/8%" OR
status like "%OOO 16/4%" OR
status like "%OOO 16/9%" OR
status like "%OOO 17/7%" OR
status like "%ooo 18/11%" OR
status like "%OOO 18/2%" OR
status like "%OOO 2/10%" OR
status like "%OOO 2/12%" OR
status like "%OoO 2/8%" OR
status like "%ooo 20/11%" OR
status like "%ooo 21/1%" OR
status like "%OOO 21/10%" OR
status like "%ooo 21/11%" OR
status like "%OOO 21/2%" OR
status like "%ooo 22/11%" OR
status like "%OOO 22/2%" OR
status like "%OOO 22/7%" OR
status like "%OOO 23/5%" OR
status like "%OOO 23/9%" OR
status like "%ooo 25/11%" OR
status like "%OOO 25/2%" OR
status like "%OOO 25/3%" OR
status like "%OOO 25/9%" OR
status like "%OOO 29/11%" OR
status like "%OOO 29/7%" OR
status like "%OOO 3/12%" OR
status like "%OOO 30/3%" OR
status like "%OOO 30/9%" OR
status like "%OoO 31/7%" OR
status like "%OoO 5/8%" OR
status like "%OOO 6/10%" OR
status like "%OOO 6/5%" OR
status like "%OOO 7/10%" OR
status like "%OOO 7/5%" OR
status like "%OOO 7/6%" OR
status like "%ooo 7/8%" OR
status like "%OOO 8/10%" OR
status like "%ooo 8/8%" OR
status like "%OOO 9/1%" OR
status like "%OOO 9/7%" OR
status like "%OOO adfter event%" OR
status like "%ooo after event%" OR
status like "%ooo after EVENT%" OR
status like "%OOO after event%" OR
status like "%OOO AFter event%" OR
status like "%OOO AFTER EVENT%" OR
status like "%OOO AFTER EVENT - invited colleagues%" OR
status like "%OOO in the jungle%" OR
status like "%OOO til 10/2%" OR
status like "%OOO til 10/3%" OR
status like "%OOO til 16/3%" OR
status like "%OOO til 19/2%" OR
status like "%OOO til 2/2%" OR
status like "%OOO til 20/1%" OR
status like "%OOO til 21/1%" OR
status like "%OOO til 21st%" OR
status like "%OOO til 22/1%" OR
status like "%OOO til 23/1%" OR
status like "%OOO til 24/1%" OR
status like "%OOO til 28/1%" OR
status like "%OOO til 28/1; with AIA group%" OR
status like "%OOO til 29/1%" OR
status like "%OOO til 29/5%" OR
status like "%OOO til 29/6%" OR
status like "%OOO til 3/2%" OR
status like "%OOO til 3/3%" OR
status like "%OOO til 30/1%" OR
status like "%OOO til 31/1%" OR
status like "%OOO til 4/3%" OR
status like "%OOO til 9/9%" OR
status like "%OOO til day of event%" OR
status like "%OOO til Feb. 3%" OR
status like "%OOO til October%" OR
status like "%OOO till 1/1/2021%" OR
status like "%OOO till 10/3%" OR
status like "%OOO till 11th March%" OR
status like "%OOO till 13th%" OR
status like "%OOO till 9 March%" OR
status like "%OOO- AFTER EVENT%" OR
status like "%OOO- DELAYED RESPONSES%" OR
status like "%OOO- EXTENDED LEAVE%" OR
status like "%OOO- MATERNITY LEAVE%" OR
status like "%OOO- PARENTAL LEAVE%" OR
status like "%OOO; directed one person%" OR
status like "%ooo. -FU 20/7%" OR
status like "%OoO31/7%" OR
status like "%OVERSUB%" OR
status like "%OVERSUBBED%" OR
status like "%Oversubed%" OR
status like "%Oversubscribe%" OR
status like "%OVERSUBSCRIBE%" OR
status like "%Oversubscribed%" OR
status like "%OVERSUBSCRIBED%" OR
status like "%Oversubscribed (matt_egan)%" OR
status like "%Oversubscribed 14/1 - ME@%" OR
status like "%OVERSUBSCRIBED 16/1%" OR
status like "%Oversubscribed 2/12 (dani@)%" OR
status like "%Oversubscribed 2/12 (hollyr@)%" OR
status like "%OVERSUBSCRIBED 20/1 - Received%" OR
status like "%Oversubscribed 3/2%" OR
status like "%OVERSUBSCRIBED 3/2%" OR
status like "%Oversubscribed 30/1%" OR
status like "%OVERSUBSCRIBED 4/12%" OR
status like "%OVERSUBSCRIBED 4/12 - acknowledged%" OR
status like "%OVERSUBSCRIBED 4/12 - L.J%" OR
status like "%OVERSUBSCRIBED 4/12 - L.J - achknowledged%" OR
status like "%OVERSUBSCRIBED 4/12 - ME - achknoweldfged%" OR
status like "%OVERSUBSCRIBED 4/2%" OR
status like "%Pak Alvin & Pak Henry%" OR
status like "%Panel Invitation%" OR
status like "%Panel: Invited%" OR
status like "%PANEL*%" OR
status like "%Parental leave%" OR
status like "%PARENTAL LEAVE%" OR
status like "%PARIS BASED%" OR
status like "%Passed it onto the team%" OR
status like "%Passing on to a colleague%" OR
status like "%PAST ATTENDEE%" OR
status like "%Past Attendees ICYMI 1st FU 16th AM%" OR
status like "%Past event NO - 2nd FU%" OR
status like "%PAST NO%" OR
status like "%PAST NO - Shared with colleagues%" OR
status like "%PAST YES%" OR
status like "%Possible%" OR
status like "%POST%" OR
status like "%POST CAMPAIGN?%" OR
status like "%POST EVENT%" OR
status like "%Post event recording%" OR
status like "%Post-event%" OR
status like "%Post-Event%" OR
status like "%Post-Event Recording%" OR
status like "%POST-EVENT self reg%" OR
status like "%postevent RECORDING%" OR
status like "%Postponed YES%" OR
status like "%POTENTIAL%" OR
status like "%POTENTIAL LATE ARRIVAL%" OR
status like "%Potentially Available [ME.AU]%" OR
status like "%PREVIOUS ATTENDEE%" OR
status like "%Previous Attendee (FU 18/5)%" OR
status like "%Previous Attendee: 1st fu 26/10%" OR
status like "%Previous Attendee: FU 28/10%" OR
status like "%Previous Attendee: Invited 22/10%" OR
status like "%Previous attendees%" OR
status like "%Previous Attendees: Invited 7/10%" OR
status like "%Previous Confirmed%" OR
status like "%Previous JKT attendee sent to team%" OR
status like "%Previous No%" OR
status like "%Previous Registration%" OR
status like "%Previous Registration: FU 30/6%" OR
status like "%Previous Y%" OR
status like "%Question%" OR
status like "%Questioned agenda%" OR
status like "%Questioned agende%" OR
status like "%Questioned location%" OR
status like "%Questioned price%" OR
status like "%QUESTIONS%" OR
status like "% Colleague%" OR
status like "%Re-Invited 19/6%" OR
status like "%REC%" OR
status like "%REC, already reg%" OR
status like "%Recently Perth Based%" OR
status like "%Recommended (invited 18/11%" OR
status like "%RECONF%" OR
status like "%RECONF | No #%" OR
status like "%Reconfirmed%" OR
status like "%Reconnect 8/4%" OR
status like "%Reconnected 15/1  Asked for attendee list%" OR
status like "%Reconnected 15/1  Asked who other rep was%" OR
status like "%Reconnected 15/1  Check if has IT Colleague%" OR
status like "%Reconnected 15/1  Sending Colleagues%" OR
status like "%Reconnected 15/1 Asked about the agenda - interested%" OR
status like "%Reconnected 4/2%" OR
status like "%Reconnected 5/2%" OR
status like "%RECORCING%" OR
status like "%Record%" OR
status like "%RECORD%" OR
status like "%RECORD - Competitor%" OR
status like "%RECORDIDNG%" OR
status like "%recording%" OR
status like "%Recording%" OR
status like "%ReCORDING%" OR
status like "%REcording%" OR
status like "%RECORDing%" OR
status like "%RECORDING%" OR
status like "%RECORDING - International%" OR
status like "%RECORDING - IT Company%" OR
status like "%RECORDING - IT Company?%" OR
status like "%Recording - L%" OR
status like "%RECORDING - Personal Email%" OR
status like "%RECORDING - refered by Sara%" OR
status like "%RECORDING - REGISTERD%" OR
status like "%Recording - Registered%" OR
status like "%RECORDING - registered%" OR
status like "%Recording - sent link for colleagues%" OR
status like "%Recording - sent link for colleagues too%" OR
status like "%RECORDING - taken from Senior tab%" OR
status like "%RECORDING - Target Org%" OR
status like "%RECORDING - Tech difficulties%" OR
status like "%RECORDING - UK Based%" OR
status like "%RECORDING - USA Based?%" OR
status like "%RECORDING (AU)%" OR
status like "%RECORDING (india)%" OR
status like "%RECORDING (Public Sector)%" OR
status like "%Recording [APAC]%" OR
status like "%Recording = L%" OR
status like "%recording pls%" OR
status like "%RECORDING Registered%" OR
status like "%Recording- Registered%" OR
status like "%Recording, Bad title%" OR
status like "%RECORDING, bad title%" OR
status like "%RECORDING, Bad title%" OR
status like "%Recording:%" OR
status like "%Recording?%" OR
status like "%Recordng%" OR
status like "%RECORDRecording [APAC]%" OR
status like "%Recorfinh%" OR
status like "%Recoridng%" OR
status like "%RECORIDNG%" OR
status like "%RECORING%" OR
status like "%Refer a Colleague%" OR
status like "%Referred%" OR
status like "%Referred by cIO%" OR
status like "%Referred by CIO%" OR
status like "%referred by Director Invited 20/1%" OR
status like "%Referred by GM digital%" OR
status like "%referred by Manager, Enterprise Architecture%" OR
status like "%Referred by: VP. Head of HR Services & Organization Developmen%" OR
status like "%Referred colleague%" OR
status like "%Referred to by COO - Heads Up Operations%" OR
status like "%Reffered bu EGM%" OR
status like "%Reffered by Head of CX - invited 8/10%" OR
status like "%REFORDING%" OR
status like "%Register%" OR
status like "%REGISTER%" OR
status like "%registered%" OR
status like "%Registered%" OR
status like "%REGISTERED%" OR
status like "%Registered - Recording%" OR
status like "%Registered 18/7%" OR
status like "%Registered Post-event%" OR
status like "%Registered RECORDING%" OR
status like "%Registered YES%" OR
status like "%Registered- Final Info Sent%" OR
status like "%Registered- send audio%" OR
status like "%Registered- Send Post Event%" OR
status like "%Registration%" OR
status like "%Registration Modal%" OR
status like "%Registrations Closed%" OR
status like "%Reinvited%" OR
status like "%Relocated to Canberra%" OR
status like "%REOCRDING%" OR
status like "%REOCRINDG%" OR
status like "%REORDING%" OR
status like "%Replied but didnt actually answer%" OR
status like "%Replied but didnt answer%" OR
status like "%Replied ccing colleague%" OR
status like "%replied via linkedin%" OR
status like "%Reply%" OR
status like "%Representative sent%" OR
status like "%Requested Attendance%" OR
status like "%requested invite be sent again%" OR
status like "%Retired%" OR
status like "%RETIRED%" OR
status like "%Ricci Colleague%" OR
status like "%Rigistered- send audio%" OR
status like "%Rohan Colleague%" OR
status like "%ROnald colleague 1%" OR
status like "%RSVP No%" OR
status like "%RSVP NO%" OR
status like "%SELF REG%" OR
status like "%SELF-REG%" OR
status like "%SELF-REGISTERED%" OR
status like "%Self-rego%" OR
status like "%Send recording/ Registered%" OR
status like "%Send recording/Registered%" OR
status like "%sending colleague%" OR
status like "%Sending Colleague%" OR
status like "%Sending colleagues- waiting for response%" OR
status like "%sending delegare%" OR
status like "%Sending Proxy%" OR
status like "%sending representative%" OR
status like "%Sending team%" OR
status like "%Sendung colleauges%" OR
status like "%senidng colleague%" OR
status like "%SENT 20/10%" OR
status like "%Sent at capacity%" OR
status like "%Sent by Business Development Director - Technology%" OR
status like "%Sent by CFO%" OR
status like "%Sent by COO - typeform sent%" OR
status like "%Sent by Corporate Human Resources Manager%" OR
status like "%Sent by CTO%" OR
status like "%Sent by CTO (target organisation)%" OR
status like "%Sent by Director of Data&Information%" OR
status like "%Sent by Global Operations%" OR
status like "%SENT BY: GM Risk / Head of Risk%" OR
status like "%sent calendar%" OR
status like "%sent calendar ** COULDN%" OR
status like "%sent calendar ** WILL CONFIRM ASAP%" OR
status like "%Sent Colleagne%" OR
status like "%sent colleague%" OR
status like "%Sent colleague%" OR
status like "%Sent Colleague%" OR
status like "%Sent to Colleague%" OR
status like "%SENT TO COLLEAGUE%" OR
status like "%Series Registration%" OR
status like "%SHARED DETAILS%" OR
status like "%Shared with colleague%" OR
status like "%Sick Leave Today%" OR
status like "%Singapore Based%" OR
status like "%SINGAPORE BASED%" OR
status like "%speaker%" OR
status like "%SPEAKER%" OR
status like "%Speaker inv 14/2%" OR
status like "%speaker invite%" OR
status like "%Sponsor/NO%" OR
status like "%Stuart Harrison Team%" OR
status like "%Sue Cooke Colleague%" OR
status like "%Suggested%" OR
status like "%Suggested by Executive Director, ICT Shared Services%" OR
status like "%Suggested colleagues:%" OR
status like "%Sutheshan colleague%" OR
status like "%Suzanna colleague%" OR
status like "%Suzanna colleague ***%" OR
status like "%Switzerland based%" OR
status like "%SYDNEY BASED%" OR
status like "%Tanya Whitey%" OR
status like "%TBC%" OR
status like "%TBC - RECORDING%" OR
status like "%TBC: Consultant%" OR
status like "%TENATIVE%" OR
status like "%tenative- waiting for response%" OR
status like "%TENT - Reconnected%" OR
status like "%tentative%" OR
status like "%Tentative%" OR
status like "%TENTATIVE%" OR
status like "%TENTATIVE - Malaysia%" OR
status like "%Tentative - recording%" OR
status like "%Tentative - will confirm Wed%" OR
status like "%TENTATIVE (FU 28/11)%" OR
status like "%TENTATIVE RECORDING%" OR
status like "%Tentative Yes%" OR
status like "%TENTATIVE YES%" OR
status like "%TENTATIVE- FU 2/5%" OR
status like "%TENTATIVE, will confirm closer%" OR
status like "%TENTATIVE, will confirm in morning%" OR
status like "%TENTATIVE: Confirming this afternoon%" OR
status like "%Tentative: Did not Confirm%" OR
status like "%Tentative: Recon 15/5%" OR
status like "%Tentative: Recon 15/6%" OR
status like "%TEssa colleague%" OR
status like "%Their REgo%" OR
status like "%Told Oversubscribe%" OR
status like "%Transcript%" OR
status like "%travelling%" OR
status like "%Travelling%" OR
status like "%Travels to HK for Work%" OR
status like "%UK BASED%" OR
status like "%unaub%" OR
status like "%Unavaialbe%" OR
status like "%unavailable%" OR
status like "%Unavailable%" OR
status like "%UNAVAILABLE%" OR
status like "%Unavailable - asked colleagues%" OR
status like "%Unavailable - forwarded to team%" OR
status like "%Unavailable - invited colleague%" OR
status like "%Unavailable - invited colleagues%" OR
status like "%Unavailable - Mel-based%" OR
status like "%Unavailable - Previous Yes%" OR
status like "%Unavailable - shared with colleagues%" OR
status like "%Unavailable (3 weeks)%" OR
status like "%Unavailable (No colleagues)%" OR
status like "%Unavailable [all events during this stage]%" OR
status like "%UNAVAILABLE & NO COLLEAGUE%" OR
status like "%Unavailable + No Colleague%" OR
status like "%Unavailable + No longer at Tempus%" OR
status like "%unavailable to attend%" OR
status like "%Unavailable- Sent Colleague%" OR
status like "%UNAVAILABLE- Sent Colleague%" OR
status like "%Unavailable:%" OR
status like "%Unavailable: No colleague%" OR
status like "%Unavailable: Suggested colleagues%" OR
status like "%Unavailable/Leaving%" OR
status like "%Unavailablr%" OR
status like "%Unavailablw%" OR
status like "%Unavailalbe%" OR
status like "%unavaliable%" OR
status like "%Unavaliable%" OR
status like "%UNAVALIABLE%" OR
status like "%Unavaukabke%" OR
status like "%Uninvite%" OR
status like "%Uninvited%" OR
status like "%UNINVITED @M.Sal%" OR
status like "%UNINVITED 10:50am 6/11%" OR
status like "%UNINVITED 3/12%" OR
status like "%UNINVITED 30/8%" OR
status like "%Uninvited 5/11%" OR
status like "%UNINVITED 5/11%" OR
status like "%Univinted 10/1%" OR
status like "%UNIVITED 24/2%" OR
status like "%univited 5/11 8am%" OR
status like "%UNSB%" OR
status like "%unsub%" OR
status like "%unSUB%" OR
status like "%Unsub%" OR
status like "%UNSub%" OR
status like "%UNSUB%" OR
status like "%UNSUB - Answered with this email%" OR
status like "%UNSUB - changed role%" OR
status like "%UNSUB - Left%" OR
status like "%UNSUB - LEFT%" OR
status like "%UNSUB - Not KELLY%" OR
status like "%UNSUB - Not Sam%" OR
status like "%Unsub - WP%" OR
status like "%UNSUB - WP%" OR
status like "%UNSUB - Wrong field%" OR
status like "%UNSUB not in IT%" OR
status like "%UNSUB NOT RELEVANT%" OR
status like "%UNSUB WP%" OR
status like "%UNSUB wrong person%" OR
status like "%UNSUB WRONG PERSON%" OR
status like "%UNSUB- Left%" OR
status like "%UNSUB: RETIRED%" OR
status like "%unsubscribe%" OR
status like "%Unsubscribe%" OR
status like "%UNSUBSCRIBE%" OR
status like "%UNVITED%" OR
status like "%US BASED%" OR
status like "%Vivian Colleague%" OR
status like "%Vivian Lee Colleague%" OR
status like "%WA BASED%" OR
status like "%Waiting for Response%" OR
status like "%Wanted to bring 2 colleagues%" OR
status like "%Wants to be called%" OR
status like "%was in emea sheet RECORDING%" OR
status like "%Will Be on AL%" OR
status like "%will confirm%" OR
status like "%Will confirm%" OR
status like "%Will confirm by COB%" OR
status like "%will confirm closer%" OR
status like "%WIll confirm closer -may come but leave early%" OR
status like "%Will try come%" OR
status like "%Will try to make it%" OR
status like "%Woramon Colleague%" OR
status like "%WP%" OR
status like "%wrong address%" OR
status like "%WRONG DAVID%" OR
status like "%Wrong EA%" OR
status like "%wrong email%" OR
status like "%Wrong GIGI%" OR
status like "%wrong person%" OR
status like "%Wrong person%" OR
status like "%Wrong Person%" OR
status like "%WRONG person%" OR
status like "%WRONG PERSON%" OR
status like "%WRONG PERSON - Occupational Health Supervisor%" OR
status like "%WRONG PROFILE%" OR
status like "%wrong title%" OR
status like "%WRONGPERSON%" OR
status like "%x NO%" OR
status like "%x NO > DOESNT work in IT%" OR
status like "%x NO > not in IT%" OR
status like "%y" OR
status like "%Y" OR
status like "%Y-  TBC%" OR
status like "%yes%" OR
status like "%Yes%" OR
status like "%YES%" OR
status like "%YES  - IT%" OR
status like "%YES  - Malaysia%" OR
status like "%YES  >RECORDING%" OR
status like "%YES _ INDONESIA%" OR
status like "%YES -  (In Lieu of Rachel Mok- LJ*)%" OR
status like "%YES -  COLLEAGUE%" OR
status like "%YES - (In Lieu of Masitoh        Ramli- LJ*)%" OR
status like "%YES - (In Lieu of Rachel Mok- LJ*)%" OR
status like "%YES - 30 mins late%" OR
status like "%YES - ACCEPTED%" OR
status like "%Yes - Assumed Close%" OR
status like "%Yes - Attended Last Week%" OR
status like "%YES - bad title%" OR
status like "%YES - Calednar accept%" OR
status like "%YES - %" OR
status like "%YES - check for colleague%" OR
status like "%YES - COLLEAGUE%" OR
status like "%YES - competitor%" OR
status like "%YES - CTIO?%" OR
status like "%Yes - Decline on Reconfirmation%" OR
status like "%YES - Don%" OR
status like "%YES - don%" OR
status like "%YES - entire series%" OR
status like "%YES - Exclusion%" OR
status like "%YES - Fujitsu%" OR
status like "%YES - Happy to be asked 1 question - What do you think is the role of information governance in driving an application specific IT strategy?%" OR
status like "%Yes - In lieu of Head [Architect Level]%" OR
status like "%YES - IT (O/S)%" OR
status like "%YES - IT Comp%" OR
status like "%YES - IT Company%" OR
status like "%YES - Linkedin%" OR
status like "%YES - MALAYSIA%" OR
status like "%Yes - Matt Egan%" OR
status like "%YES - ME (Very senior FYI)%" OR
status like "%YES - ME Confirmed%" OR
status like "%YES - ME Confirmed > he%" OR
status like "%YES - N%" OR
status like "%YES - needs to leave at 12:30%" OR
status like "%YES - No on Conf%" OR
status like "%YES - No to speaking%" OR
status like "%YES - not relevant?%" OR
status like "%YES - Not super relevant, but senior. I think will be ok%" OR
status like "%YES - not target title%" OR
status like "%YES - Oversubscribe%" OR
status like "%YES - panel declined%" OR
status like "%YES - Panel Declined%" OR
status like "%YES - panel fu 30/7%" OR
status like "%YES - panel invite 19/7%" OR
status like "%YES - panel member%" OR
status like "%YES - Past Attendee 2nd FU%" OR
status like "%YES - Pitney target%" OR
status like "%YES - Recording%" OR
status like "%YES - registered%" OR
status like "%YES - REPLIED NO IN 2nd EMAIL%" OR
status like "%YES - Requested by Fujitsu%" OR
status like "%YES - requested by Rubrik (but on exclusion list)%" OR
status like "%Yes - Self registration%" OR
status like "%YES - self rego%" OR
status like "%YES - Sent by Asistant director%" OR
status like "%YES - sent by CTO%" OR
status like "%YES - Sent by Head of Business Improvement%" OR
status like "%YES - sent by Head of Systems Engineering%" OR
status like "%YES - sent link for colleagues%" OR
status like "%Yes - Singapore%" OR
status like "%YES - singapore%" OR
status like "%YES - Singapore%" OR
status like "%YES - SINGAPORE%" OR
status like "%YES - speaker invite 29/7%" OR
status like "%Yes - Target Org%" OR
status like "%YES - TBC%" OR
status like "%YES - TBC - UK Based%" OR
status like "%YES - TBC (SELF-REGISTERED)%" OR
status like "%YES - TBC UK Based%" OR
status like "%YES - TBC?%" OR
status like "%YES - TECH CO%" OR
status like "%YES - TENTATIVE%" OR
status like "%YES - Title not good%" OR
status like "%YES - Uninvite%" OR
status like "%YES - used link%" OR
status like "%YES - used platform%" OR
status like "%YES - VIA LINK%" OR
status like "%YES - via link [unavailable]%" OR
status like "%Yes - WANTS TO BRING ""CLIENT"" - Simon@%" OR
status like "%YES - will be 10min late%" OR
status like "%YES - will be late 4:00pm%" OR
status like "%YES - WIll be Late; wants to dial in%" OR
status like "%YES - will be there at 12:20%" OR
status like "%YES - will try and make it%" OR
status like "%YES (AC)%" OR
status like "%YES (also asked for recording)%" OR
status like "%YES (and asked to bring colleague-he%" OR
status like "%YES (Assumed Close)%" OR
status like "%Yes (colleague)%" OR
status like "%YES (colleague)%" OR
status like "%YES (Exclusion list)%" OR
status like "%YES (he%" OR
status like "%YES (invited by colleague)%" OR
status like "%YES (invited by colleague) Registered%" OR
status like "%YES (Inviting +5 colleagues)%" OR
status like "%YES (KEY TARGET)%" OR
status like "%YES (May be interstate)%" OR
status like "%YES (Need to print name tag)%" OR
status like "%YES (no longer works at RAC)%" OR
status like "%YES (Panel invite 20/11)%" OR
status like "%YES (Public Sector)%" OR
status like "%YES (Referred by CDO)%" OR
status like "%YES (said NO in second email)%" OR
status like "%YES (TBC)%" OR
status like "%YES (tentative)%" OR
status like "%YES (will arrive late)%" OR
status like "%YES (will join late)%" OR
status like "%YES [TAREGT]%" OR
status like "%Yes [via Link]%" OR
status like "%YES [via Link]%" OR
status like "%YES {TARGET}%" OR
status like "%YES *** sent by Head of Operations Development%" OR
status like "%YES = L%" OR
status like "%YES ACCEPTED%" OR
status like "%YES Accepted- Renea%" OR
status like "%YES and RECORD%" OR
status like "%YES AUS%" OR
status like "%YES based in India%" OR
status like "%YES by Cloudflare%" OR
status like "%Yes Cloudflare 28 MAY%" OR
status like "%YES Cloudflare Digital Roundtable (DACH), 18th June%" OR
status like "%YES Cloudflare EMEA,  16th September (Joe Sullivan)%" OR
status like "%YES Cloudflare EMEA, 7th September (John Cumming)%" OR
status like "%YES Cloudflare Healthcare Webinar EMEA, 24th SEPT%" OR
status like "%YES HK%" OR
status like "%YES in Lieu of CTO%" OR
status like "%YES In Lieu of Director ICT%" OR
status like "%YES Invited by Se√°n%" OR
status like "%YES June, The Future after Crisis: Building a Process Around Digital Innovation%" OR
status like "%YES L%" OR
status like "%YES NEW%" OR
status like "%YES recording%" OR
status like "%YES Registered%" OR
status like "%YES TBC - COLLEAGUE%" OR
status like "%YES with +1%" OR
status like "%YES- ACCEPTED%" OR
status like "%YES- Accepted Calendar%" OR
status like "%YES- No to speaking%" OR
status like "%YES- Reconfirmed%" OR
status like "%YES- WILL LEAVE EARLY%" OR
status like "%YES-ACCEPTED%" OR
status like "%YES, bad titles%" OR
status like "%Yes, Head of Origination and Sector Coverage +1%" OR
status like "%YES, MY based%" OR
status like "%YES; EA to??%" OR
status like "%Yes:%" OR
status like "%YES:%" OR
status like "%YES: Arrivng 12:15/12:30%" OR
status like "%YES: attending with Belinda Campbell%" OR
status like "%YES: Attending with Christine%" OR
status like "%YES: Attending with Neil%" OR
status like "%YES: Calendar%" OR
status like "%YES: Call again%" OR
status like "%YES: Chris colleague%" OR
status like "%YES: Chris%" OR
status like "%YES: Colleague%" OR
status like "%YES: COlleague of Anton%" OR
status like "%YES: colleague of Don Pedro%" OR
status like "%YES: colleague of Executive Director%" OR
status like "%YES: Colleague of Matthew%" OR
status like "%YES: coming at 10%" OR
status like "%YES: coming at 10 [CIO Colleague]%" OR
status like "%YES: Confirm%" OR
status like "%YES: Confirm ORG%" OR
status like "%YES: Connection to Capillary Technologies (Target)%" OR
status like "%YES: Declined%" OR
status like "%YES: Fujitsu%" OR
status like "%YES: HAS TO LEAVE AT 1:45%" OR
status like "%YES: HPE Confirmed%" OR
status like "%YES: In Lieu GM%" OR
status like "%YES: In Lieu of CFO%" OR
status like "%YES: In Lieu of Head of IT Procurement%" OR
status like "%YES: Innovatus Media%" OR
status like "%YES: Invited for panel 23/7%" OR
status like "%YES: Key Target%" OR
status like "%YES: New%" OR
status like "%YES: NEW%" OR
status like "%YES: Not avaialbe - On-Demand%" OR
status like "%YES: Not Construction%" OR
status like "%YES: not staying for dinner%" OR
status like "%YES: On Behalf of CIO%" OR
status like "%YES: On behalf of the CRO%" OR
status like "%YES: Panel Decline%" OR
status like "%YES: Print on Night (Find last night)%" OR
status like "%YES: Pritida colleague%" OR
status like "%Yes: Recording%" OR
status like "%YES: Requsted no marketing meetings%" OR
status like "%YES: SELF-REG%" OR
status like "%YES: Sent by CDO%" OR
status like "%YES: Sent by CEO%" OR
status like "%YES: Sent by Cheryl-Ann Moy%" OR
status like "%YES: sent by Chief Data Officer%" OR
status like "%YES: Sent by Head of Governance and Risk%" OR
status like "%YES: Sent by Head of Network Infrastructure - invitation extended%" OR
status like "%YES: sent by HO FInance%" OR
status like "%YES: SL to Share%" OR
status like "%YES: SPEAKER%" OR
status like "%YES: Through Link%" OR
status like "%YES: Unavailable on call%" OR
status like "%YES: Unavailable on Reconf%" OR
status like "%Yes: Used Link%" OR
status like "%Yes: Used Link - Recording%" OR
status like "%YES: Will confirm by Friday (FU 12/11)%" OR
status like "%YES: Yudi%" OR
status like "%yes?%" OR
status like "%YES? [has a +1]%" OR
status like "%Yes? [not in invitation list]%" OR
status like "%YES.  No Competitor%" OR
status like "%YES. IT Company%" OR
status like "%Yes...%" OR
status like "%YES**%" OR
status like "%YES| HPE%" OR
status like "%yy%"             

')

q5complete=rbind(q5,q5_pt2)


write_csv(q5complete,paste("~/Desktop/crm/","crm2019_2020legit.csv", sep=""))

t1=read.csv('attendeeemail.csv')


t2=sqldf('select * from q5complete where email not in t1')

write_csv(t2,paste("~/Desktop/crm/","t2.csv", sep=""))