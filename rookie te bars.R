rm(list = ls())

library(dplyr)
library(ggplot2)
library(XML)


wrList <- c()
for (n in c(0, 100, 200, 300)) {
  u <- paste0('http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=1920&year_max=2016&season_start=1&season_end=1&age_min=14&age_max=48&pos=te&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=targets&c2comp=gt&c2val=20&c3stat=choose&c3comp=gt&c4stat=choose&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=g&draft=1&draft_year_min=1936&draft_year_max=2016&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos=qb&draft_pos=rb&draft_pos=wr&draft_pos=te&draft_pos=e&draft_pos=t&draft_pos=g&draft_pos=c&draft_pos=ol&draft_pos=dt&draft_pos=de&draft_pos=dl&draft_pos=ilb&draft_pos=olb&draft_pos=lb&draft_pos=cb&draft_pos=s&draft_pos=db&draft_pos=k&draft_pos=p&offset=', n)
  
  df <- as.data.frame(readHTMLTable(u, stringsAsFactors = FALSE))
  wrList[[length(wrList) + 1]] <- df
  rm(df, n, u)
}

teDF <- do.call(rbind, wrList)
names(teDF) <- gsub('results.', '', names(teDF), fixed = TRUE)

teDF.clean <- subset(teDF, Rk != 'Rk')
teDF.clean$Player <- gsub('*', '', teDF.clean$Player, fixed = TRUE)


teDF.clean.chars <- teDF.clean %>%
  select(Player, Draft, Tm, Lg, Ctch.)

teDF.clean.nums <- teDF.clean %>%
  select(-Player, -Draft, -Tm, -Lg, -Ctch.)

teDF.clean.nums <- as.data.frame(apply(teDF.clean.nums, 2, as.numeric))

teDF.fixedcols <- cbind(teDF.clean.chars, teDF.clean.nums)
teDF.fixedcols <- filter(teDF.fixedcols, Player != 'Calvin Johnson')

# gc
rm(list = ls(pattern = 'teDF.clean'))

# make some labels, which will be ordered for plot
teDF.fixedcols$orderedTemp <- paste0(teDF.fixedcols$Player,
                                     ', ',
                                     teDF.fixedcols$Year)
# to make your bars follow a specific order, you need to use an ordered factor
teDF.fixedcols$orderedPlayers <- 
  factor(teDF.fixedcols$orderedTemp,
         levels = teDF.fixedcols$orderedTemp[order(teDF.fixedcols$Tgt)])

# only plotting a handful of data, specifically the top 10 22 year old target-getters
data.viz <- filter(teDF.fixedcols, Age == 22) %>% top_n(10, Tgt)

# make the plot
dv <- ggplot(data = data.viz, mapping = aes(x = orderedPlayers, y = Tgt)) +
    geom_col(width = 2/3) +
    ylab("Total Targets, Rookie Season, 22 Year Old NFL Tight Ends") +
    scale_y_continuous(limits = c(0, 132), expand = c(0, .01)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()

dv
ggsave(dv, file = 'dv.jpg', units = 'in', height = 2, width = 5)


