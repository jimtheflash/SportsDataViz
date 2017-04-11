
library(directlabels)
library(dplyr)
library(ggplot2)
library(rvest)
library(XML)

# years <- 1986:2016
# 
# draftList <- c()
# 
# for (y in years) {
#   Sys.sleep(3)
#   url <- paste0("http://www.pro-football-reference.com/play-index/draft-finder.cgi?request=1&year_min=", y, "&year_max=", y, "&draft_round_min=1&draft_round_max=30&draft_slot_min=1&draft_slot_max=500&pick_type=overall&pos=qb&pos=rb&pos=wr&pos=te&pos=e&pos=t&pos=g&pos=c&pos=ol&pos=dt&pos=de&pos=dl&pos=ilb&pos=olb&pos=lb&pos=cb&pos=s&pos=db&pos=k&pos=p&college_id=all&conference_id=any&show=all&order_by=default")
#   tbl <- read_html(url) %>% html_table() %>% as.data.frame(stringsAsFactors = FALSE)
#   draftList[[length(draftList)+1]] <- tbl
# }  
# 
# drafts <- do.call(rbind, draftList)
# 
# drafts.clean <- select(drafts, 
#                        Year, Rnd, Pick, PlayerName = Var.5, College.Univ, Pos, DrAge, Tm) %>%
#   mutate(Year = as.numeric(Year),
#          Rnd = as.numeric(Rnd),
#          Pick = as.numeric(Pick),
#          DrAge = as.numeric(DrAge),
#          Pos2 = ifelse(Pos %in% c('C', 'G', 'OL', 'T'), 'OL',
#                        ifelse(Pos %in% c('CB', 'DB', 'FS', 'S', 'SS'), 'DB',
#                               ifelse(Pos %in% c('FB', 'RB'), 'RB',
#                                      ifelse(Pos %in% c('DL', 'DT', 'NT'), 'DL',
#                                             ifelse(Pos %in% c('ILB', 'LB', 'OLB'), 'LB', 
#                                                    ifelse(Pos %in% c('LS', 'K', 'P'), 'SpT', Pos))))))) %>%
#   filter(!is.na(Year)) %>%
#   group_by(Year, Pos) %>%
#   summarise(draftedPlayers = n_distinct(PlayerName))
  

direct.label(ggplot(data = filter(drafted.positions.by.year, Pos2 %in% c('QB', 'RB', 'TE', 'WR')), mapping = aes(x = Year, y = draftedPlayers, color = Pos2)) +
  geom_line() +
  geom_point() +
  ylab("Players Drafted") +
  ggtitle("Number Of Offensive Skill Position Players Drafted By Year, 1996-2016") +
  theme_minimal(),
    list(dl.trans(x = x + .1), "last.bumpup"))

# WRs always seem to get drafted more often than other positions; RB's seems to beat QB's and TE's
# lets see how players turn out when by round




