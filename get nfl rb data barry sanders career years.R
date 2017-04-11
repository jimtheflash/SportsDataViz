rm(list = ls())

library(dplyr)
library(ggplot2)
library(rvest)

url <- 'http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=1989&year_max=1998&season_start=1&season_end=-1&age_min=0&age_max=0&pos=rb&c1stat=rush_yds&c1comp=gt&c1val=1000&c2stat=choose&c2comp=gt&c3stat=choose&c3comp=gt&c4stat=choose&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=rush_yds&draft=0&draft_year_min=1936&draft_year_max=2016&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos=qb&draft_pos=rb&draft_pos=wr&draft_pos=te&draft_pos=e&draft_pos=t&draft_pos=g&draft_pos=c&draft_pos=ol&draft_pos=dt&draft_pos=de&draft_pos=dl&draft_pos=ilb&draft_pos=olb&draft_pos=lb&draft_pos=cb&draft_pos=s&draft_pos=db&draft_pos=k&draft_pos=p&offset='

df1 <- read_html(paste0(url, 0)) %>% 
  html_table(header = FALSE) %>% 
  as.data.frame()

names(df1) <- df1[2, ]
df1 <- df1[-c(1:2), ]

df2 <- read_html(paste0(url, 100)) %>%
  html_table(header = FALSE) %>%
  as.data.frame()

names(df2) <- df2[2, ]
df2 <- df2[-c(1:2), ]

BarrySanders <- rbind(df1, df2) %>%
  select(Player, Year, Yds) %>%
  mutate(Year = as.numeric(Year),
         Yds = as.numeric(Yds))

ggplot(data = BarrySanders, mapping = aes(x = Year, y = Yds, color = Player)) +
  geom_line()


rm(df1, df2, url)

