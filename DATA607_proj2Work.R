#install.packages('XML')
library(XML)
library(dplyr)
library(stringr)
library(magrittr)
library(tidyr)
library(ggplot2)

#read in data from github repository
mvp.data <- read.csv('Project2_tbl1.csv', header=T, stringsAsFactors = F)

#rename columns
str(mvp.data)
colnames(mvp.data) <- c('superbowlID', 'player', 'stats')
mvp.data

# parse column 2
mvp.data %<>% separate(player,c('name', 'position','team'),sep=",") 

#trim artefacts from separate()
# mvp.data$team <- str_trim(mvp.data$team, side="both")
# mvp.data$position <- str_trim(mvp.data$position, side="both")

# standardize words - 
mvp.data$stats %<>% str_replace_all('touchdown[s]?', 'TD') %>% 
  str_replace_all('[Tt]wo', '2')

### try to parse stats
# mvp.data %<>% separate(stats, into=c('stat.1','stat.2','stat.3'),sep=',')
# mvp.data$`stat.1` <- str_trim(mvp.data$`1`, side="both")
# mvp.data$`stat.2` <- str_trim(mvp.data$`2`, side="both")
# mvp.data$`stat.3` <- str_trim(mvp.data$`3`, side="both")
mvp.data$TD <- NA
mvp.data$yards<-NA


# this works but neecs to be generalized
for(i in 1:53){

    mvp.data$TD[i] <- str_extract(mvp.data$stats[i], '[[:digit:]] TD') %>%  str_replace('TD','')
    mvp.data$yards[i] <- str_extract(mvp.data$stats[i], '[[:digit:]]{2,3} yards') %>%  str_replace('yards','')
}
mvp.data

# Data Analysis finding the players with the most MVP's, player with most 
#total touchdown and passing or rushing yardage, as well as a summary of 
#frequencies of which positions win the award most.

most.mvps <- mvp.data %>%  count(name) %>% arrange(desc(n)) %>% filter(n>1)
most.mvps






##################### TABLE 2 ######################################
site <- "http://www.espn.com/nfl/superbowl/history/winners"
SB.results = readHTMLTable(site, header=T, which=1,stringsAsFactors=F) %>% as_tibble()
colnames(SB.results) <- c('superbowlID','date','site','result')

SB.results2 <- SB.results %>%  slice(-1) %>% 
    separate(date, c('month','day','year')) %>%  
    separate(result, c('winner','loser'), sep=',')

SB.results2 %>% separate(winner, c('team','score'), regex=("([[:alpha:]]{1,3})[[:space:]]*([[:digit:]]*)"))

#good for score 
SB.results2 %>% separate(winner, c('team','score'), sep="[^[:digit:]]+")


#closer
SB.results2 %>% separate(winner, c('team','score'), sep=" +[:digit:]")


str_split(SB.results2$winner, "[^[:digit:]]+")
unlist(str_match_all(SB.results$winner, '[:alpha:]+'))

#  Create columns for 
#Super Bowl ID, MVP name, position, Team, TDs scored/yards passing//yards rushing/sacks or turnovers

#####################################################################################################
##############################TABLE 2################################################################
#-overall current distribution, trends by state, overall trends
raw.dat <- read.csv('Project2_tbl2.csv', header=T, stringsAsFactors = F) %>% as_tibble()
colnames(raw.dat) <- c('State','SizeRank','1','2','3','4','5','6','7','8','9','10','11','12')
raw.dat2 <- raw.dat %>%  gather('MOY', 'n',3:14)
raw.dat2

# Look at seasonality effects
test <- raw.dat2 %>% group_by(MOY) %>%  
            summarize(n=mean(n)) %>% arrange(as.numeric(MOY))
# why the fuck does the x axis            
ggplot(test,aes(x=MOY,y=n)) + geom_bar(stat='identity') 




# Trends by state 
# look at 5 largest states 
tt <- raw.dat2 %>% filter(SizeRank %in% 1:5)
tt  %>% group_by(MOY,State) %>%  
  summarize(n=mean(n)) %>% arrange(as.numeric(MOY))


