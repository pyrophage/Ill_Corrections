library(readxl)
library(dplyr)
library(httr)

url<-"https://www2.illinois.gov/idoc/reportsandstatistics/Documents/December%202021%20Prison%20Stock.xls"

GET(url, write_disk(tf <-tempfile(fileext = ".xls")))
df<-read_excel(tf, skip = 5)

life<-df%>%
  filter(`Sentence Years` == "LIFE")

SDP<-df%>%
  filter(`Sentence Years` == "SDP")

Standard<-df%>%
  filter(!`Sentence Years` %in% c("LIFE", "SDP"))%>%
  mutate(`Sentence Years` = as.numeric(`Sentence Years`))


Race<-life%>%
  group_by(Race)%>%
  summarise(Count = n())

RaceSDP<-SDP%>%
  group_by(Race)%>%
  summarise(Count = n())
