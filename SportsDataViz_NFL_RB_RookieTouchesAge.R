rm(list = ls())

library(dplyr)
library(ggplot2)
library(XML)

# pull a list of rookie RBs with at least 20 carries per game, get their touches, 
rbList <- c()
for (n in c(0, 100, 200, 300)) {
  u <- paste0('http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=1920&year_max=2016&season_start=1&season_end=1&age_min=14&age_max=48&pos=rb&c1stat=rush_att&c1comp=gt&c1val=20&c2stat=targets&c2comp=gt&c3stat=choose&c3comp=gt&c4stat=choose&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=g&draft=1&draft_year_min=1936&draft_year_max=2016&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos=qb&draft_pos=rb&draft_pos=wr&draft_pos=te&draft_pos=e&draft_pos=t&draft_pos=g&draft_pos=c&draft_pos=ol&draft_pos=dt&draft_pos=de&draft_pos=dl&draft_pos=ilb&draft_pos=olb&draft_pos=lb&draft_pos=cb&draft_pos=s&draft_pos=db&draft_pos=k&draft_pos=p&offset=', n)
  
  df <- as.data.frame(readHTMLTable(u, stringsAsFactors = FALSE))
  rbList[[length(rbList) + 1]] <- df
  rm(df, n, u)
}

# inspect!
rbDF <- do.call(rbind, rbList)
# View(rbDf) ## if using RStudio

# clean up names
names(rbDF) <- gsub('results.', '', names(rbDF), fixed = TRUE)

# clean up by removing anyone who's row rank is 'Rk'; probably a more elegant way to do this
rbDF.clean <- subset(rbDF, Rk != 'Rk')
rbDF.clean$Player <- gsub('*', '', rbDF.clean$Player, fixed = TRUE)

# fix column classes
rbDF.clean.chars <- rbDF.clean %>%
  select(Player, Draft, Tm, Lg, Ctch.)

rbDF.clean.nums <- rbDF.clean %>%
  select(-Player, -Draft, -Tm, -Lg, -Ctch.)

rbDF.clean.nums <- as.data.frame(apply(rbDF.clean.nums, 2, as.numeric))

rbDF.fixedcols <- cbind(rbDF.clean.chars, rbDF.clean.nums)

# gc
rm(list = ls(pattern = 'rbDF.clean'))

# compute touches
rbDF.fixedcols$touches <- rbDF.fixedcols$Att + rbDF.fixedcols$Tgt

# find RBs with max touches for their respective ages
maxAtts <- c()
for (i in unique(rbDF.fixedcols$Age)) {
  ageDF <- subset(rbDF.fixedcols, Age == i)
  pn <- ageDF$Player[ageDF$touches == max(ageDF$touches)]
  maxAtts[[length(maxAtts) + 1]] <- pn
  rm(ageDF, pn)
}

# clean out anyone who wouldn't make the final figure interesting/useful
rbDF.fixedcols <- subset(rbDF.fixedcols, touches >= 50)

# make the plot
g <- ggplot(data = rbDF.fixedcols, 
             mapping = aes(x = Age, y = touches, label = paste0(Player, 
                                                                ' (', touches, 
                                                                ')'))) +
  scale_x_continuous(breaks = min(rbDF.fixedcols$Age):max(rbDF.fixedcols$Age),
                     expand = c(.1, .1)) +
  geom_jitter(alpha = .2, size = 2, width = 0.05) +
  geom_text(data = subset(rbDF.fixedcols, Player %in% maxAtts), nudge_y = 20, size = rel(3)) +
  geom_boxplot(aes(group = Age), fill = NA, outlier.shape = NA, width = 0.25) +
  xlab('Rookie Season Age') +
  ylab('Touches (Rushing Attempts + Passing Targets)') +
  ggtitle(label = 'Total Touches In Rookie Season By NFL Running Backs, By Rookie Season Age', subtitle = '(Minimum 50 Touches; Labeled Points Are Players With Highest Rookie Touches For Age Group)') +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
# see it
g
# save it
ggsave('NFL_RB_RookieTouchesAge.jpg', plot = g, units = 'in',
       width = 10, height = 5)  
  