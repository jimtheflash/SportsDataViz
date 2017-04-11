library(dplyr)
library(XML)

rbURL <- 'http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=2016&year_max=2016&season_start=1&season_end=-1&age_min=0&age_max=0&pos=rb&c1stat=rush_att&c1comp=gt&c1val=50&c2stat=choose&c2comp=gt&c3stat=choose&c3comp=gt&c4stat=choose&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=rush_att&draft=0&draft_year_min=1936&draft_year_max=2016&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos=qb&draft_pos=rb&draft_pos=wr&draft_pos=te&draft_pos=e&draft_pos=t&draft_pos=g&draft_pos=c&draft_pos=ol&draft_pos=dt&draft_pos=de&draft_pos=dl&draft_pos=ilb&draft_pos=olb&draft_pos=lb&draft_pos=cb&draft_pos=s&draft_pos=db&draft_pos=k&draft_pos=p'

rbATTYD <- readHTMLTable(rbURL, stringsAsFactors = FALSE) %>% 
  as.data.frame()
names(rbATTYD) <- gsub('results.', '', names(rbATTYD), fixed = TRUE)
viz.in <- rbATTYD %>%  filter(Rk != 'Rk') %>%
  select(Player, Att, Yds) %>%
  mutate(Att = as.numeric(scale(as.numeric(as.character(Att, na.rm = TRUE)))),
         Yds = as.numeric(scale(as.numeric(as.character(Yds, na.rm = TRUE)))))

write.csv(viz.in, file = 'rbATTYD.csv')
getwd()
