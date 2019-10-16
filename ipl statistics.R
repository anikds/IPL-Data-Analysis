library(tidyverse)
setwd('D:/datasets/R Datasets/ipldata')

match <- read.csv("matches.csv", stringsAsFactors = FALSE)
innings <- read.csv("deliveries.csv",stringsAsFactors = FALSE)

#how many matches in a dataset
match%>%summarize(no_of_matches = n())

#which team has won by the maximum runs?
max_runs <- match[which.max(match$win_by_runs),]
max_runs%>%select(winner,win_by_runs)

#which team has won by maximum wickets?
match%>% 
  filter(win_by_wickets == max(win_by_wickets))%>%
  select(winner,win_by_wickets)

#Closest margin for victory(minimum runs)
#mumbai indian tops the list with 3 close victory in terms of runs
match%>%
  filter(win_by_runs != 0)%>%
  filter(win_by_runs == min(win_by_runs)) %>%
  select(winner, win_by_runs,season)%>%
  ggplot(aes(winner,fill = factor(season)))+
  geom_bar()+
  coord_flip()

#Closest margin of victory(minimum wickets)
#only 3 teams have won by 1 wicket
match%>%
  filter(win_by_wickets !=0)%>%
  filter(win_by_wickets == min(win_by_wickets))%>%
  select(winner,win_by_runs,season)%>%
  ggplot(aes(winner, fill = factor(season)))+
  geom_bar()+
  coord_flip()+
  scale_y_continuous("Wickets")

# most successful team
match%>%
  group_by(winner)%>%
  summarize(most_win = n())%>%
  filter(most_win == max(most_win))

# matches won by each team
match%>%
  group_by(winner)%>%
  summarize(most_win = n())%>%
  ggplot(aes(x = winner,y = most_win,fill = winner))+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous("Matches won")

# no of matches each season
match%>%
  group_by(season)%>%
  summarize(match_count = n())%>%
  filter(match_count == max(match_count))

# which player has won most man of the match award
match%>%
  group_by(player_of_match)%>%
  summarize(most_award = n())%>%
  top_n(10)%>%
  ggplot(aes(x =  reorder(player_of_match, most_award), y = most_award))+
  geom_bar(stat = "identity", fill = "light blue")+
  coord_flip()+
  xlab("Players")

############## Individul batting performance EDA & Visualization ###########
# top 10 batsman who has most number of sixes
innings%>%
  group_by(batsman)%>%
  filter(batsman_runs == 6)%>%
  summarize(sixes = n())%>%
  top_n(10)%>%
  ggplot(aes(x = reorder(batsman, sixes),y = sixes))+
  geom_bar(stat = "identity",fill = "light green")+
  coord_flip()+
  xlab("Players")+
  ggtitle("Top ten players with most sixes in IPL career")+
  geom_text(aes(label = sixes),hjust =1.25)

# top 5 batsman who has most no. of fours
innings %>%
  group_by(batsman)%>%
  filter(batsman_runs == 4)%>%
  summarize(fours = n())%>%
  top_n(10)%>%
  ggplot(aes(x = reorder(batsman,fours),y = fours))+
  geom_bar(stat = "identity",fill = "slategray4", width =  0.75)+
  coord_flip()+
  xlab("Players")+
  ggtitle("Top 5 batsmans with most fours in IPL career")+
  geom_text(aes(label = fours), hjust =1.25)

# Top 10 batsman with most runs in IPL
innings%>%
  group_by(batsman)%>%
  summarize(runs = sum(batsman_runs))%>%
  top_n(n = 10,wt = runs)%>%
  ggplot(aes(x = reorder(batsman,runs),y = runs,fill = batsman))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  coord_flip()+
  labs(x = "Players", y ="Runs", title = "Top 10 batsman with most runs in IPL career")+
  geom_text(aes(label = runs),hjust = 1.25)

# highest individual score
highest_individual_score <- innings%>%
  group_by(match_id,batsman)%>%
  summarize(individual_score = sum(batsman_runs))%>%
  arrange(desc(individual_score))%>%
  ungroup()%>%
  top_n(10, individual_score)%>%
  mutate(id = as.factor(row_number()))

highest_individual_score%>%
  ggplot(aes(x= id, y = individual_score,fill = batsman))+
  geom_bar(stat= "identity",show.legend = FALSE)+
  scale_x_discrete(labels = highest_individual_score$batsman, name = "Batsman")+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y = "Individual Score", title ="Top 10 highest individual scorers")+
  geom_text(aes(label = individual_score), vjust = 2.0)

# highest run scorer each season
match_full <- innings%>%
  left_join(match, by = c("match_id" = "id"))

match_full%>%
  group_by(season,batsman)%>%
  summarize(total_runs = sum(batsman_runs))%>%
  filter(total_runs == max(total_runs))%>%
  ggplot(aes(x = season,y = total_runs,fill = batsman))+
  geom_bar(stat ="identity")+
  ggtitle("Highest run scorer each season")+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  geom_text(aes(label= total_runs, vjust= 1.50))+
  scale_y_continuous(name="Total Runs", breaks = c(0,250,500,750,1000,1250))

# Batting comparision of Chris Gayle, V Kohli, AB de Villiers IPL performance
match_full%>%
  group_by(season,batsman)%>%
  summarise(runs = sum(batsman_runs))%>%
  filter(batsman %in% c("CH Gayle","V Kohli","AB de Villiers"))%>%
  ggplot(aes(x = season, y = runs, fill = batsman))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Total Runs")

##############Individul bowling performance EDA & Visualization ###########
# top 10 leading wicket takers in IPL
bowler_wicket <- c("caught","caught and bowled", "bowled","hit wicket", "lbw","stumped","obstructing the field	")

innings%>%
  group_by(bowler)%>%
  summarise(wickets =  sum(dismissal_kind %in% bowler_wicket))%>%
  arrange(desc(wickets))%>%
  top_n(10)%>%
  ggplot(aes(x= reorder(bowler, -wickets), y = wickets, fill = bowler))+
  geom_bar(stat = "identity",show.legend =  FALSE)+
  labs(x = "Bowlers" , y = "Wickets", title = "Top ten leading wicket takers in IPL")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(label = wickets, vjust =1.25))

# leading wicket takers in a season
match_full%>%
  group_by(season, bowler)%>%
  summarise(wickets =  sum(dismissal_kind %in% bowler_wicket))%>%
  filter(wickets == max(wickets))%>%
  ggplot(aes(x= season, y = wickets, fill = bowler))+
  geom_bar(stat = "identity")+
  labs(y = "Wickets" ,title = "Leading wicket takers in a season")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(aes(label = wickets, vjust =1.50))+
  scale_x_discrete(name="Season", limits=c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019))

# Clean Bowled
innings%>%
  filter(dismissal_kind == "bowled")%>%
  count(bowler)%>%
  arrange(desc(n))%>%
  top_n(5)%>%
  ggplot(aes(reorder(x = bowler,-n),y =n, fill = bowler))+
  geom_bar(stat="identity", show.legend = FALSE)+
  labs(x = "Bowlers",y = "Wickets", title = "Top 5 bowled wickets")+
  geom_text(aes(label = n), vjust = 1.5)

# Best Death bowlers
innings%>%
  filter(over >= 15)%>%
  group_by(bowler)%>%
  summarise(total_wickets = sum(dismissal_kind %in% bowler_wicket))%>%
  arrange(desc(total_wickets))%>%
  top_n(10)%>%
  ggplot(aes(reorder(x =bowler,total_wickets),y= total_wickets,fill = bowler))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  labs(x = "Bowlers", y = "Wickets",title = "Top Death bowlers")+
  geom_text(aes(label = total_wickets),hjust =1.5)+
  coord_flip()

# Best powerplay bowlers
innings%>%
  filter(over < 7)%>%
  group_by(bowler)%>%
  summarise(total_wickets = sum(dismissal_kind %in% bowler_wicket))%>%
  arrange(desc(total_wickets))%>%
  top_n(10)%>%
  ggplot(aes(reorder(x =bowler,total_wickets),y= total_wickets,fill = bowler))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  labs(x = "Bowlers", y = "Wickets",title = "Top powerplay bowlers")+
  geom_text(aes(label = total_wickets),hjust =1.5)+
  coord_flip()
