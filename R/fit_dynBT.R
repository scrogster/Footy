library(jagsUI)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

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
params=c("d", "p", "home_advantage", "sigma")

#fit the model
jagsdat<-list(
	ateam=results$ateamid,
	hteam=results$hteamid,
	homewin=as.numeric(results$hteamid==results$winnerteamid),
	time=results$Time,
	games=nrow(results),
	teams=max(results$hteamid),
	times=max(results$Time)
)

#fits the Bayesian Bradley-Terry model to the data, and predicts the results of future matches
mod <- jags(data = jagsdat,
									 parameters.to.save = params,
									 model.file = "R/foot_mod_dyn.txt",
									 n.chains = 3,
									 n.adapt = 1000,
									 n.iter = 30000,
									 n.burnin = 20000,
									 n.thin = 5, parallel=TRUE)
traceplot(mod,"sigma", pages=1)
hist(mod$sims.list$sigma, breaks=50)
traceplot(mod,"home_advantage", pages=1)
hist(mod$sims.list$home_advantage, breaks=50)


#present team quality scores
pres_quality<-team_list %>% 
	mutate(quality= mod$mean$d[,40]) %>%
	arrange(desc(quality))
pres_quality

round_to_predict=1
tips_out<-file.path("Tips", paste0("round_", round_to_predict, ".csv"))

tips<-results %>%
	mutate(prwin=mod$q50$p, lwr=mod$q2.5$p, upp=mod$q97.5$p) %>%
	filter(year==2020) %>%
	filter(round==round_to_predict) %>%
	mutate(Predicted_winner=ifelse(prwin>0.5, hteam, ateam)) %>%
	arrange(date) %>%
	select(ateam, hteam, Predicted_winner, prwin, lwr, upp, round)

readr::write_csv(tips, tips_out)

#Calibration of margin###########################################################
A<-results %>%
	mutate(prwin=mod$q50$p, lwr=mod$q2.5$p, upp=mod$q97.5$p) %>%
	mutate(Predicted_winner=ifelse(prwin>0.5, hteam, ateam)) %>%
	arrange(date) %>%
	select(ateam, hteam, Predicted_winner, prwin, lwr, upp, round, margin, year,Time)

library(ggplot2)
ggplot(A, aes(x=prwin, y=margin))+
	geom_point()+
	xlim(0, 1)+
	geom_smooth(method="lm")+
  facet_wrap(~cut_interval(Time, 6))+
	theme_bw()
##################################################################################

library(tidyr)
skill_trend<-mod$mean$d %>% data.frame() %>%
	bind_cols(team_list) %>%
	select(ateam, everything(), -ateamid) %>%
	pivot_longer(-ateam, names_to="Time", values_to = "skill") %>%
	mutate(Time = parse_number(Time))

skill_trend_lwr<-mod$q2.5$d %>% data.frame() %>%
	bind_cols(team_list) %>%
	select(ateam, everything(), -ateamid) %>%
	pivot_longer(-ateam, names_to="Time", values_to = "lwr") %>%
	mutate(Time = parse_number(Time))

skill_trend_upp<-mod$q97.5$d %>% data.frame() %>%
	bind_cols(team_list) %>%
	select(ateam, everything(), -ateamid) %>%
	pivot_longer(-ateam, names_to="Time", values_to = "upp") %>%
	mutate(Time = parse_number(Time))

skill_trend<-skill_trend %>%
	left_join(skill_trend_lwr) %>%
	left_join(skill_trend_upp)

library(ggplot2)

#Temporal trends in skill
ggplot(skill_trend, aes(x=Time, y=skill, group=ateam))+
	geom_line()+
	geom_ribbon(aes(ymin=lwr, ymax=upp), fill="red", col=NA, alpha=0.5)+
	geom_vline(xintercept=48)+
	facet_wrap(~ateam)+
	theme_bw()
