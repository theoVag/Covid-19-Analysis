# COVID-19 Analysis
# Author: Theodoros - Panagiotis Vagenas
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("date")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("gridExtra")
#install.packages("ggrepel")
#install.packages("arules")
#install.packages("ggExtra")
library(data.table)
library(date)
library(lubridate)
library(dplyr)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(ggrepel)
library(arules)
library(ggExtra)
cases_data <- fread('time_series_covid19_confirmed_global.csv')
deaths_data <- fread('time_series_covid19_deaths_global.csv')
# Preprocess
# 1
cases_data$'Province/State' <- NULL
cases_data$Lat <- NULL
cases_data$Long <- NULL

deaths_data$'Province/State' <- NULL
deaths_data$Lat <- NULL
deaths_data$Long <- NULL

# 2
cases.long <- melt(cases_data)
deaths.long <- melt(deaths_data)

# 3
setnames(cases.long,"Country/Region","Country")
setnames(deaths.long,"Country/Region","Country")

# 4
setnames(cases.long,"value","confirmed")
setnames(deaths.long,"value","deaths")

# 5
cases.long$variable <- cases.long[,lapply(.SD,mdy),.SDcols="variable"]
setnames(cases.long,"variable","date")
deaths.long$variable <- deaths.long[,lapply(.SD,mdy),.SDcols="variable"]
setnames(deaths.long,"variable","date")

# 6

cases.long2 <- cases.long[,sum(confirmed),by = .(Country,date)] # edw thelei h sum h max den kserw analoga an einai cummulative
setnames(cases.long2,"V1","confirmed")

deaths.long2 <- deaths.long[,sum(deaths),by = .(Country,date)]
setnames(deaths.long2,"V1","deaths")

# 7
com <- merge(cases.long2,deaths.long2,by=c("Country","date"))

# 8
world <- com[,.(confirmed = sum(confirmed),deaths = sum(deaths)), by=date] # change the 326 to last element
world$confirmed[length(world$confirmed)]
world$deaths[length(world$deaths)]

# 9
com <- com[order(Country,date)]

# 10
day1 <-min(com$date)

com <- com %>% mutate(confirmed.inc = ifelse(date==day1, NA, confirmed- lag(confirmed, n=1)))
com <- com %>% mutate(deaths.inc = ifelse(date==day1, NA, deaths- lag(deaths, n=1)))


# ANALYSIS

max_date <- max(com$date)
world_data_per_day <- com[,.(cases=sum(confirmed),deaths=sum(deaths),world.confirmed.inc=sum(confirmed.inc),world.deaths.inc=sum(deaths.inc)),by=date]

# Set English as language for date labels
Sys.setlocale("LC_TIME", "C")


# Plot worldwide cases and deaths simple and in log scale
g1 <- ggplot()+
  geom_line(data=world_data_per_day,aes(x=date,y=cases),color="Blue") + 
  labs(x = "Date", y="Cases",title=paste0("Worldwide cases - ",max_date))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(legend.title=element_blank(),
        legend.position='bottom',
        plot.title =element_text(size = 13,hjust = 0.5))

g2 <- ggplot()+
  geom_line(data=world_data_per_day,aes(x=date,y=deaths),color="Blue")+
  labs(x = "Date", y="Deaths",title=paste0("Worldwide deaths - ",max_date))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(legend.title=element_blank(),
        legend.position='bottom',
        plot.title = element_text(size = 13,hjust = 0.5))

plot_grid(g1, g2, labels=c("", "", ""), ncol = 2, nrow = 1, align = "v")

g2 <- ggplot()+
  geom_line(data=world_data_per_day,aes(x=date,y=cases,color='cases')) + 
  geom_line(data=world_data_per_day,aes(x=date,y=deaths,color='deaths'))+
  labs(x = "Date", y="Cases",title="Worldwide number of cases and deaths in log scale")+
  theme(legend.title=element_blank(), 
        legend.position='right',
        plot.title =element_text(hjust=0.5))+
  scale_y_continuous(trans='log10')+
  scale_x_date(date_breaks="1 month",date_labels = "%b")
plot(g2)

# Worldwide daily cases and death
g3 <- ggplot(data=world_data_per_day,aes(x=date,y=world.confirmed.inc),color='blue')+
  geom_point() + 
  geom_smooth()+
  labs(x = "Date", y="Cases",title="Worldwide daily cases")+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(plot.title =element_text(hjust=0.5))

g4 <- ggplot(data=world_data_per_day,aes(x=date,y=world.deaths.inc),color='blue')+
  geom_point() + 
  geom_smooth()+
  labs(x = "Date", y="Cases",title="Worldwide daily deaths")+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(plot.title =element_text(hjust=0.5))

grid.arrange(g3, g4, ncol=2)

# add death rate according to current confirmed cases (not daily)
world_data_per_day <- world_data_per_day %>% mutate(death_rate=100*deaths/cases)
g5 <- ggplot()+
  geom_line(data=world_data_per_day,aes(x=date,y=death_rate)) + 
  labs(x = "Date", y="Death rate",title="Worldwide death rate")+
  scale_x_date(date_breaks="1 month",date_labels = "%b")+
  theme(legend.title=element_blank(), 
        legend.position='bottom',
        plot.title = element_text(hjust=0.5))
plot(g5)
#ggsave("death_rate.jpg", dpi=300)

# Top 20 countries with the most confirmed cases, the other countries are grouped in row other
top_countries <- com%>% filter(date== max(date))
top_countries$confirmed.inc <- NULL
top_countries$deaths.inc <- NULL
top_countries$date <- NULL
top_countries <- top_countries[order(-confirmed)]
top_countries <- top_countries %>% mutate(death_rate=100*deaths/confirmed)
other_countries <- top_countries[21:dim(top_countries)[1],lapply(.SD, sum),.SDcols = c("confirmed","deaths")]
other_countries_death_rate <- top_countries[21:dim(top_countries)[1],lapply(.SD, mean),.SDcols = c("death_rate")]
top_countries_20 <- rbind(top_countries[1:20,],cbind(Country="other",other_countries,other_countries_death_rate))
top_countries_20$Country <- factor(top_countries_20$Country, levels = top_countries_20$Country)

# bar plot for comparison between confirmed cases, deaths and death rate
g6 <- ggplot(top_countries_20,aes(x = Country,y=confirmed,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=confirmed, y=confirmed), vjust=0,size=2)+
  labs(x = element_blank(), y="Cases",title="Top 20 Countries with most cases: Cases")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text(size = 8))

g7 <- ggplot(top_countries_20,aes(x = Country,y=deaths,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=deaths, y=deaths), vjust=0,size=2)+
  labs(x = element_blank(), y="Deaths",title="Top 20 Countries with most cases: Deaths")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text( size = 8))

g8 <- ggplot(top_countries_20,aes(x = Country,y=death_rate,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(death_rate,2), y=death_rate), vjust=0,size=3)+
  labs(x = "Country", y="Death rate",title="Top 20 Countries with most cases: Death rate")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text(size = 8))

#grid.arrange(g6, g7,g8, nrow=3,top="Top 20 countries with the most confirmed cases")
plot_grid(g6, g7, g8, labels=c("", "", ""), ncol = 1, nrow = 3, align = "v")

g9 <-  ggplot(data=top_countries_20,aes(x=confirmed,y=deaths,group=Country))+
  geom_point(aes(color=Country,size=death_rate)) + 
  labs(x = "Confirmed cases", y="Deaths",title="Top 20 countries with most cases: Deaths vs confirmed",color="Country",size="Death rate")+
  geom_text_repel(aes(label = Country))+
  theme(legend.title=element_text(),legend.position='bottom',plot.title =element_text(hjust = 0.5,size=12),axis.text.x=element_text(size=10),axis.text.y=element_text(size = 10))
  
plot(g9)


# Construct world map with confirmed cases
# Load again dataset without dropping columns
full_world <- map_data("world")
cases_data <- fread('time_series_covid19_confirmed_global.csv')
deaths_data <- fread('time_series_covid19_deaths_global.csv')
cols_vector = c('Province/State','Country/Region','Lat','Long',names(cases_data)[length(names(cases_data))])
cases_data <- cases_data[,..cols_vector]

cases_data$'Province/State' <- NULL

setnames(cases_data,"Country/Region","region")

cases_data <- cases_data[,.(lat=Lat,long=Long,confirmed=sum(`1/12/21`)),by=.(region)] # here last date

sequence_break <- c(1,100,1000,10000,100000,1000000)

full_world$region[full_world$region=="USA"] <- "US"
full_world$region[full_world$region=="Myanmar"] <- "Burma"
full_world$region[full_world$region=="Macedonia"] <- "North Macedonia"
full_world$region[full_world$region=="South Korea"] <- "Korea, South"
full_world$region[full_world$region=="UK"] <-"United Kingdom"
full_world$region[full_world$region=="Republic of Congo"] <- "Congo (Brazzaville)"
full_world$region[full_world$region=="Democratic Republic of the Congo"] <- "Congo (Kinshasa)"
full_world$region[full_world$region=="Ivory Coast"] <- "Cote d'Ivoire"
full_world$region[full_world$region=="Taiwan"] <- "Taiwan*"
full_world$region[full_world$region=="Czech Republic"] <- "Czechia"
full_world$region[full_world$region=="French Guiana"] <- "Czechia"

nn <- left_join(cases_data, full_world, by = "region")

gworldmap <- ggplot()+
  geom_polygon(data = full_world, aes(x=long,y=lat,group=group),alpha=0.2)+
  geom_polygon(data = nn, aes(x=long.y,y=lat.y,group=group,fill=confirmed),alpha=1)+
  scale_fill_gradientn(colours = rev(heat.colors(7)),breaks = sequence_break,trans = "log10")+
  annotate(geom="text", x=150, y=-100, label="Grey countries' measurements are not available",color="red",alpha=0.7)+
  theme(
    plot.title =element_text(hjust = 0.5),
    legend.position = "right",
    legend.background = element_rect(fill = "#ffffff", color = NA)
  )

plot(gworldmap)

# European Union countries

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")


europe_com <- com %>% filter(com$Country %in% europeanUnion)

euro_countries <- europe_com%>% filter(date== max(date))
euro_countries$confirmed.inc <- NULL
euro_countries$deaths.inc <- NULL
euro_countries$date <- NULL
euro_countries <- euro_countries[order(-confirmed)]
euro_countries <- euro_countries %>% mutate(death_rate=100*deaths/confirmed)

euro_countries$Country <- factor(euro_countries$Country, levels = euro_countries$Country)
temp <- europe_com %>% filter(Country %in% euro_countries$Country)
euro_countries <- euro_countries %>% mutate(categ = discretize(euro_countries$death_rate, method = "frequency", breaks = 3))

# European confirmed cases,deaths and death rates ordered by cases (descent)
g18 <- ggplot(euro_countries,aes(x = Country,y=confirmed,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(confirmed, 2), y=confirmed), size=2, vjust=0)+
  labs(x = "", y="Cases",title="Europe: Confirmed cases")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text(size = 7))

g19 <- ggplot(euro_countries,aes(x = Country,y=deaths,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(deaths, 2), y=deaths), size=2, vjust=0)+
  labs(x = "", y="Deaths",title="Europe: Deaths")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text(size = 7))

g20 <- ggplot(euro_countries,aes(x = Country,y=death_rate,group=Country))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(death_rate, 2), y=death_rate), size=2, vjust=0)+
  labs(x = "Country", y="Death rate",title="Europe: Death rate")+
  theme(plot.title =element_text(hjust = 0,size=12),axis.text.x=element_text(size = 7))
# align vertical for more clear results
plot_grid(g18, g19, g20, labels=c("", "", ""), ncol = 1, nrow = 3, align = "v")


g21 <-  ggplot(data=temp,aes(x=confirmed,y=deaths,group=Country))+
  geom_line(aes(color=Country)) + 
  labs(x = "Confirmed cases", y="Deaths",title="Europe: Deaths vs Confirmed cases")+
  theme(legend.title=element_blank(), legend.position='right',plot.title =element_text(hjust = 0.5))
plot(g21)

g22 <-  ggplot(data=euro_countries,aes(x=confirmed,y=deaths,group=Country))+
  geom_point(aes(color=Country,size=death_rate)) + 
  labs(x = "Confirmed cases", y="Deaths",title="Europe: Deaths vs Confirmed cases",color="Country",size="Death rate")+
  geom_text_repel(aes(label = Country))+
  theme(legend.title=element_text(),legend.position='bottom',plot.title =element_text(hjust = 0.5,size=12),axis.text.x=element_text(size=10),axis.text.y=element_text(size = 10))

plot(g22)

# Diverging bars Europe confirmed and deaths
euro_countries$confirmed_norm <- round((euro_countries$confirmed - mean(euro_countries$confirmed))/sd(euro_countries$confirmed), 2)
euro_countries$confirmed_type <- ifelse(euro_countries$confirmed_norm < 0, "below","above")
euro_countries <- euro_countries[order(euro_countries$confirmed_norm), ] # sort

g25 <- ggplot(euro_countries, aes(x=Country, y=confirmed_norm, label=confirmed_norm)) +
  geom_bar(stat='identity', aes(fill=confirmed_type), width=.5) +
  scale_fill_manual(name="Confirmed cases",labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(subtitle="Normalised confirmed cases",
       title= "Diverging Bars for Europe") +
  theme( legend.position='bottom')+
  coord_flip()

euro_countries$deaths_norm <- round((euro_countries$deaths - mean(euro_countries$deaths))/sd(euro_countries$deaths), 2)
euro_countries$deaths_type <- ifelse(euro_countries$deaths_norm < 0, "below","above")
euro_countries <- euro_countries[order(euro_countries$deaths_norm), ] # sort

g26 <- ggplot(euro_countries, aes(x=Country, y=deaths_norm, label=deaths_norm)) +
  geom_bar(stat='identity', aes(fill=deaths_type), width=.5) +
  scale_fill_manual(name="Deaths",labels = c("Above Average", "Below Average"),
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  labs(subtitle="Normalised deaths",
       title= "Diverging Bars for Europe") +
  theme( legend.position='bottom')+
  coord_flip()
grid.arrange(g25,g26, ncol=2)

# Ranking for Europe
g27 <- ggplot(euro_countries, aes(y=Country, x=confirmed)) +
  geom_point(col="tomato2", size=3) + # Draw points
  geom_segment(aes(y=Country,
                   yend=Country,
                   x=min(confirmed),
                   xend=max(confirmed)),
                   linetype="dashed",
                   size=0.1)+
  labs(title="Dot Plot for Europe",subtitle="Country vs Confirmed") 

g28 <- ggplot(euro_countries, aes(y=Country, x=deaths)) +
  geom_point(col="tomato2", size=3) + # Draw points
  geom_segment(aes(y=Country,
                   yend=Country,
                   x=min(deaths),
                   xend=max(deaths)),
                   linetype="dashed",
                   size=0.1)+
  labs(title="Dot Plot for Europe",subtitle="Country vs Deaths") 
grid.arrange(g27,g28, ncol=2)

# ---Greece statistics---
greece <- com %>% filter(Country=="Greece")
g11 <- ggplot(greece, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Greece", x= "Date", y= "Confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

g12 <- ggplot(greece, aes(x=date, y=deaths)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Deaths in Greece", x= "Date", y= "Deaths") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(g11, g12, ncol=2)

# Cases-deaths relationships
g13 <-  ggplot(data=greece,aes(x=confirmed,y=deaths))+
  geom_line() + 
  labs(x = "Confirmed cases", y="Deaths",title="Greece: Confirmed cases vs Deaths")+
  theme(legend.title=element_blank(), legend.position='bottom',plot.title =element_text(hjust = 0.5))
plot(g13)

gg_daily1 <- ggplot(greece, aes(x=date, y=confirmed.inc)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Daily Confirmed Cases in Greece", x= "Date", y= "Confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")

gg_daily2 <- ggplot(greece, aes(x=date, y=deaths.inc)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Daily Confirmed Deaths in Greece", x= "Date", y= "Confirmed deaths") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_date(date_breaks="1 month",date_labels = "%b")

grid.arrange(gg_daily1,gg_daily2, ncol=2)

# Greece results per day of week
days_all <- greece %>% mutate(daysofweek=weekdays(date))
days_all <- na.omit(days_all)
days_all <- days_all[,.(confirmed = mean(confirmed.inc),deaths=mean(deaths.inc)),by=daysofweek] 
days_all <- days_all %>% mutate(death_rate=100*deaths/confirmed)
days_all$daysofweek <- factor(days_all$daysofweek, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

g14 <- ggplot(days_all,aes(x = daysofweek,y=confirmed,fill=confirmed))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(confirmed, 2), y=confirmed), size=2, vjust=0)+
  labs(x = "Day", y="Confirmed cases",title="Greece: Mean of confirmed cases per day of week")+
  scale_fill_gradientn(colours = rev(heat.colors(7)))+
  theme(plot.title =element_text(hjust=0.5),axis.text.x=element_text(hjust=0.5))

g15 <- ggplot(days_all,aes(x = daysofweek,y=deaths,fill=deaths))+
  geom_bar(stat='identity')+
  geom_text(aes(label=round(deaths, 2), y=deaths), size=2, vjust=0)+
  labs(x = "Day", y="Deaths",title="Greece: Mean of deaths per day of week")+
  scale_fill_gradientn(colours = rev(heat.colors(7)))+
  theme(plot.title =element_text(hjust=0.5),axis.text.x=element_text(hjust=0.5))

grid.arrange(g14,g15, nrow=2)

# Distribution for Greece
g29 <- ggplot(greece, aes(Country, confirmed.inc))+
  geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Box plot for Greece",
       subtitle="Confirmed cases per day",
       x="",
       y="Confirmed cases")+
  background_grid()+
  theme(panel.grid.minor = element_line(colour="white", size=0.2))+
  scale_y_continuous(minor_breaks = seq(0 , 4000, 20), breaks = seq(0, 4000, 100))

g30 <- ggplot(greece, aes(Country, deaths.inc))+
  geom_boxplot(varwidth=T, fill="plum") +
  labs(title="Box plot for Greece",
       subtitle="Confirmed deaths per day",
       x="",
       y="Confirmed deaths")+
  background_grid()+
  scale_y_continuous(minor_breaks = seq(0 , 130, 5), breaks = seq(0, 130, 10))
grid.arrange(g29,g30, ncol=2)


