library(jagsUI)
library(readr)
library(dplyr)
library(lubridate)

#all data from https://api.squiggle.com.au/

#this gets the 2020 future fixture and results for season thus far to base the predictions on.
#download.file("https://api.squiggle.com.au/?q=games;year=2020;format=csv",
#							"Data/Results2020.csv")

#get the variables of interest
results2016<-read_csv("Data/Results2016.csv") %>%
	select(ateam, hteam, ateamid, hteamid, winnerteamid, round, year, date, ascore, hscore)
results2017<-read_csv("Data/Results2017.csv") %>%
	select(ateam, hteam, ateamid, hteamid, winnerteamid, round, year, date, ascore, hscore)
results2018<-read_csv("Data/Results2018.csv") %>%
	select(ateam, hteam, ateamid, hteamid, winnerteamid, round, year, date, ascore, hscore)
results2019<-read_csv("Data/Results2019.csv") %>%
	select(ateam, hteam, ateamid, hteamid, winnerteamid, round, year, date, ascore, hscore)
results2020<-read_csv("Data/Results2020.csv")  %>%
	select(ateam, hteam, ateamid, hteamid, winnerteamid, round, year, date, ascore, hscore)

#bind the data together
results<-bind_rows(results2016, results2017, results2018, results2019, results2020) %>%
	      mutate(margin = ifelse(date<today(), hscore-ascore, NA)) %>%
	      mutate(Time = 1+as.duration(interval(min(date), date)) %/% as.duration(months(1)))

#list of teams with their numerical codes for reference
team_list<-results %>% select(ateam, ateamid) %>% distinct() %>%
	arrange(ateamid)
	
#three parameters - 
# d  - inherent quality of each team
# p  - probability the home team wins each game
# home_advantage  - additive effect on log-odds of winning if at home.
params=c("d", "p", "home_advantage")

#fit the model
jagsdat<-list(
	ateam=results$ateamid,
	hteam=results$hteamid,
	homewin=as.numeric(results$hteamid==results$winnerteamid),
#	Tmonths=results$Time,
	games=nrow(results),
	teams=max(results$hteamid),
#	maxT=max(results$Time)
)

#fits the Bayesian Bradley-Terry model to the data, and predicts the results of future matches
mod <- jags(data = jagsdat,
									 parameters.to.save = params,
									 model.file = "R/foot_mod.txt",
									 n.chains = 3,
									 n.adapt = 1000,
									 n.iter = 5000,
									 n.burnin = 1000,
									 n.thin = 5)

#present team quality scores
pres_quality<-team_list %>% 
	mutate(quality= mod$mean$d) %>%
	arrange(desc(quality))
pres_quality

round_to_predict=1

results %>%
	mutate(prwin=mod$q50$p, lwr=mod$q2.5$p, upp=mod$q97.5$p) %>%
	filter(year==2020) %>%
	filter(round==round_to_predict) %>%
	mutate(Predicted_winner=ifelse(prwin>0.5, hteam, ateam)) %>%
	arrange(date) %>%
	select(ateam, hteam, Predicted_winner, prwin, prwin, lwr, upp, round)


