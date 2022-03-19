library(readxl)
library(dplyr)
library(httr)

url<-"https://www2.illinois.gov/idoc/reportsandstatistics/Documents/December%202021%20Prison%20Stock.xls"

GET(url, write_disk(tf <-tempfile(fileext = ".xls")))
df<-read_excel(tf, skip = 5)%>%
  rename(DOCNUM = `IDOC #`, DOB = `Date of Birth`, Veteran = `Veteran Status`, Curr_adm_dt = `Current Admission Date`,
         Admit_type = `Admission Type`, Parent_Institution = `Parent Institution`, MSR_dt = `Projected Mandatory Supervised Release (MSR) Date3`,
         Proj_Disc_dt = `Projected Discharge Date3`, Custody_dt = `Custody Date`, Sent_dt = `Sentence Date`,
         Crime_Class = `Crime Class`, Holding_Offense = `Holding Offense`, HO_Cat = `Holding Offense Category`, 
         Offense_Type = `Offense Type`, Sent_yrs = `Sentence Years`, Sent_mon = `Sentence Months`,
         Truth_in_Sentencing = `Truth in Sentencing`, Sent_County = `Sentencing County`)

life<-df%>%
  filter(Sent_yrs == "LIFE")

SDP<-df%>%
  filter(Sent_yrs == "SDP")

Standard<-df%>%
  filter(!Sent_yrs %in% c("LIFE", "SDP"))%>%
  filter(!is.na(Name))%>%
  mutate(Sent_yrs = as.numeric(Sent_yrs), 
         Sent_mon = as.numeric(Sent_mon))

Race<-life%>%
  group_by(Race)%>%
  summarise(Count = n())

RaceSDP<-SDP%>%
  group_by(Race)%>%
  summarise(Count = n())

RaceStandard<-Standard%>%
  group_by(Race)%>%
  summarise(Count=n())
