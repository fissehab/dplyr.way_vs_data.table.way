
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(compare)

spending=fromJSON("https://data.medicare.gov/api/views/nrth-mfg3/rows.json?accessType=DOWNLOAD")
names(spending)

meta=spending$meta
hospital_spending=data.frame(spending$data)
colnames(hospital_spending)=make.names(meta$view$columns$name)
hospital_spending=select(hospital_spending,-c(sid:meta))

glimpse(hospital_spending)

cols = 6:11; # These are the columns to be changed to numeric.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], as.numeric)

cols = 12:13; # These are the columns to be changed to dates.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], ymd_hms)

sapply(hospital_spending,class)

hospital_spending_DT = data.table(hospital_spending)

class(hospital_spending_DT)

from_dplyr = select(hospital_spending, Hospital.Name)
from_data_table = hospital_spending_DT[,.(Hospital.Name)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = select(hospital_spending, -Hospital.Name)
from_data_table = hospital_spending_DT[,!c("Hospital.Name"),with=FALSE]
compare(from_dplyr,from_data_table, allowAll=TRUE)

DT=copy(hospital_spending_DT)
DT=DT[,Hospital.Name:=NULL]
"Hospital.Name"%in%names(DT)

DT=copy(hospital_spending_DT)
DT=DT[,c("Hospital.Name","State","Measure.Start.Date","Measure.End.Date"):=NULL]
c("Hospital.Name","State","Measure.Start.Date","Measure.End.Date")%in%names(DT)

from_dplyr = select(hospital_spending, Hospital.Name,State,Measure.Start.Date,Measure.End.Date)
from_data_table = hospital_spending_DT[,.(Hospital.Name,State,Measure.Start.Date,Measure.End.Date)]
compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = select(hospital_spending, -c(Hospital.Name,State,Measure.Start.Date,Measure.End.Date))
from_data_table = hospital_spending_DT[,!c("Hospital.Name","State","Measure.Start.Date","Measure.End.Date"),with=FALSE]
compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = select(hospital_spending,contains("Date"))
from_data_table = subset(hospital_spending_DT,select=grep("Date",names(hospital_spending_DT)))
compare(from_dplyr,from_data_table, allowAll=TRUE)

names(from_dplyr)

setnames(hospital_spending_DT,c("Hospital.Name", "Measure.Start.Date","Measure.End.Date"),
                           c("Hospital","Start_Date","End_Date"))
names(hospital_spending_DT)

hospital_spending = rename(hospital_spending,Hospital= Hospital.Name, Start_Date=Measure.Start.Date,End_Date=Measure.End.Date)

compare(hospital_spending,hospital_spending_DT, allowAll=TRUE)

from_dplyr = filter(hospital_spending,State=='CA') # selecting rows for California
from_data_table = hospital_spending_DT[State=='CA']
compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = filter(hospital_spending,State=='CA' & Claim.Type!="Hospice") 
from_data_table = hospital_spending_DT[State=='CA' & Claim.Type!="Hospice"]
compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = filter(hospital_spending,State %in% c('CA','MA',"TX")) 
from_data_table = hospital_spending_DT[State %in% c('CA','MA',"TX")]
unique(from_dplyr$State)

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = arrange(hospital_spending, State)

from_data_table = setorder(hospital_spending_DT, State)

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = arrange(hospital_spending, desc(State))

from_data_table = setorder(hospital_spending_DT, -State)

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = arrange(hospital_spending, State,desc(End_Date))

from_data_table = setorder(hospital_spending_DT, State,-End_Date)

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = mutate(hospital_spending, diff=Avg.Spending.Per.Episode..State. - Avg.Spending.Per.Episode..Nation.)

from_data_table = hospital_spending_DT
from_data_table = from_data_table[,diff := Avg.Spending.Per.Episode..State. - Avg.Spending.Per.Episode..Nation.]

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = mutate(hospital_spending, diff1=Avg.Spending.Per.Episode..State. - Avg.Spending.Per.Episode..Nation.,
                                        diff2=End_Date-Start_Date)

from_data_table = copy(hospital_spending_DT)
from_data_table = from_data_table[,c("diff1","diff2") := list(Avg.Spending.Per.Episode..State. - Avg.Spending.Per.Episode..Nation.,
                                                             diff2=End_Date-Start_Date)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

summarize(hospital_spending,mean=mean(Avg.Spending.Per.Episode..Nation.)) 

hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Nation.))]

summarize(hospital_spending,mean=mean(Avg.Spending.Per.Episode..Nation.),
                            maximum=max(Avg.Spending.Per.Episode..Nation.),
                            minimum=min(Avg.Spending.Per.Episode..Nation.),
                            median=median(Avg.Spending.Per.Episode..Nation.)) 

hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Nation.),
                        maximum=max(Avg.Spending.Per.Episode..Nation.),
                        minimum=min(Avg.Spending.Per.Episode..Nation.),
                        median=median(Avg.Spending.Per.Episode..Nation.))]

head(hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),by=.(Hospital)])

mygroup= group_by(hospital_spending,Hospital) 

from_dplyr = summarize(mygroup,mean=mean(Avg.Spending.Per.Episode..Hospital.))
from_data_table=hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                                     by=.(Hospital)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

head(hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                          by=.(Hospital,State)])

mygroup= group_by(hospital_spending,Hospital,State)

from_dplyr = summarize(mygroup,mean=mean(Avg.Spending.Per.Episode..Hospital.))
from_data_table=hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                                     by=.(Hospital,State)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr=hospital_spending%>%group_by(Hospital,State)%>%summarize(mean=mean(Avg.Spending.Per.Episode..Hospital.))

from_data_table=hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                                     by=.(Hospital,State)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

hospital_spending%>%group_by(State)%>%summarize(mean=mean(Avg.Spending.Per.Episode..Hospital.))%>%
arrange(desc(mean))%>%head(10)%>%
        mutate(State = factor(State,levels = State[order(mean,decreasing =TRUE)]))%>%
          ggplot(aes(x=State,y=mean))+geom_bar(stat='identity',color='darkred',fill='skyblue')+
          xlab("")+ggtitle('Average Spending Per Episode by State')+
          ylab('Average')+ coord_cartesian(ylim = c(3800, 4000))



hospital_spending_DT[,.(mean=mean(Avg.Spending.Per.Episode..Hospital.)),
                                     by=.(State)][order(-mean)][1:10]%>% 
            mutate(State = factor(State,levels = State[order(mean,decreasing =TRUE)]))%>%
           ggplot(aes(x=State,y=mean))+geom_bar(stat='identity',color='darkred',fill='skyblue')+
          xlab("")+ggtitle('Average Spending Per Episode by State')+
          ylab('Average')+ coord_cartesian(ylim = c(3800, 4000))
