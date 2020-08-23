library(tidyverse)
library(lubridate)


mydata = read.csv("ptf-smf.csv")
head(mydata)

names(mydata)[2:5] = c("MCP", "SMP", "PIP", "NIP") 

names(mydata)

mydata %>% glimpse()

((mydata$SMP.Direction))

mydata = mydata %>%
  mutate(SMP.Direction=case_when(SMP.Direction=="?Energy Surplus" ~ "Surplus", SMP.Direction=="? Energy Deficit"~ "Deficit", SMP.Direction=="? In Balance" ~ "Balance")) %>%
  mutate(Date=as.POSIXct(Date, format="%d.%m.%y %H:%M")) %>%
  mutate(Day=day(Date),Hour=hour(Date),WeekDay=wday(Date,label = 0))


days = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
for (i in 1:7){
  mydata$WeekDay <- gsub(c(i),days[i],mydata$WeekDay)
}

mydata$WeekDay <- factor(mydata$WeekDay, levels= days)


mydata$Diff = mydata$SMP-mydata$MCP

mydata %>% glimpse()

ggplot(mydata, aes(x=SMP, y=MCP)) + 
  geom_point() + 
  theme_minimal() + 
  ggtitle("SMP vs MCP on July, 2020") + 
  labs(x = "SMP", y = "MCP") + 
  geom_abline(slope=1, intercept=0)


AvgofWeekDays =
  mydata %>%
  group_by(WeekDay) %>%
  summarise(AvgMCP = mean(MCP), AvgSMP = mean(SMP), AvgDiff = mean(Diff))

AvgofHours =
  mydata %>%
  group_by(Hour) %>%
  summarise(AvgMCP = mean(MCP), AvgSMP = mean(SMP), AvgDiff = mean(Diff))

pivotAvgofWeekDays <-
  AvgofWeekDays %>%
  ungroup() %>%
  pivot_longer(cols=c(AvgMCP, AvgSMP), names_to="SMPorMCP", values_to="Price")


ggplot(pivotAvgofWeekDays,aes(x=WeekDay,y=Price,fill=SMPorMCP))+
  geom_bar(stat="Identity",position = "dodge")+
  scale_fill_manual(values=c("darkblue", "darkgreen"), labels = c("SMP", "MCP"))+
  theme_minimal()+
  labs(x="Week Day", y="Price",title="Avg Prices by Week Day")

pivotAvgofHours <-
  AvgofHours %>%
  ungroup() %>%
  pivot_longer(cols=c(AvgMCP, AvgSMP), names_to="SMPorMCP", values_to="Price")

ggplot(pivotAvgofHours,aes(x=Hour,y=Price,fill=SMPorMCP))+
  geom_bar(stat="Identity",position = "dodge")+
  scale_fill_manual(values=c("darkblue", "darkgreen"), labels = c("SMP", "MCP"))+
  theme_minimal()+
  labs(x="Hour", y="Price",title="Avg Prices by Hour")

ggplot(pivotAvgofWeekDays, aes(x=WeekDay,y=AvgDiff))+
  geom_line(aes(x=WeekDay,y=AvgDiff))+
  labs(x="Days of July", y="TL",title="Daily average SMP(System Marginal Price) and MCP(Market Clearing Price) in July 2020")+
  scale_color_manual(name="",values = c("mean SMP"="darkred","mean MCP"="navy"))+
  theme_minimal()+
  theme(title=element_text(size=9.5))

ggplot(data=pivotAvgofWeekDays, aes(x=WeekDay, y=AvgDiff, group=1)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="Week Day", y="Average Difference",title="Avg Differences by Week Day")

ggplot(data=pivotAvgofHours, aes(x=Hour, y=AvgDiff)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  labs(x="Hour", y="Average Difference",title="Avg Differences by Hour")


