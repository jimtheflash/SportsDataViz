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

rm(list = ls(pattern = 'teDF.clean'))

teDF.fixedcols$touches <- teDF.fixedcols$Att + teDF.fixedcols$Tgt

maxTgts <- c()
for (i in unique(teDF.fixedcols$Age)) {
  ageDF <- subset(teDF.fixedcols, Age == i)
  pn <- ageDF$Player[ageDF$Tgt == max(ageDF$Tgt)]
  maxTgts[[length(maxTgts) + 1]] <- pn
  rm(ageDF, pn)
}


# make the plot
g <- ggplot(data = teDF.fixedcols, 
            mapping = aes(x = Age, y = Tgt, label = paste0(Player, 
                                                           ' (', Tgt, 
                                                           ')'))) +
  scale_x_continuous(breaks = min(teDF.fixedcols$Age):max(teDF.fixedcols$Age),
                     expand = c(.1, .1)) +
  geom_jitter(alpha = .2, size = 2, width = 0.05) +
  geom_text(data = subset(teDF.fixedcols, Player %in% maxTgts), nudge_y = 5, size = rel(3)) +
  geom_boxplot(aes(group = Age), fill = NA, outlier.shape = NA, width = 0.25) +
  xlab('Rookie Season Age') +
  ylab('Targets') +
  ggtitle(label = 'Total Targets, Rookie Season, NFL Tight Ends, By Rookie Season Age', subtitle = '(Minimum 32 Targets; Ages Are Years; Labeled Points Are Players With Highest Rookie Targets For Age)') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
# see it
g
# save it
ggsave('NFL_TE_RookieTargetsAge.jpg', plot = g, units = 'in',
       width = 10, height = 5)  
