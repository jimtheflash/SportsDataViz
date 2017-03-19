rm(list = ls())

library(dplyr)
library(ggplot2)
library(XML)


wrList <- c()
for (n in c(0, 100, 200, 300)) {
  u <- paste0('http://www.pro-football-reference.com/play-index/psl_finder.cgi?request=1&match=single&year_min=1920&year_max=2016&season_start=1&season_end=1&age_min=14&age_max=48&pos=wr&c1stat=rush_att&c1comp=gt&c1val=0&c2stat=targets&c2comp=gt&c2val=32&c3stat=choose&c3comp=gt&c4stat=choose&c4comp=gt&c5comp=choose&c5gtlt=lt&c6mult=1.0&c6comp=choose&order_by=g&draft=1&draft_year_min=1936&draft_year_max=2016&draft_slot_min=1&draft_slot_max=500&draft_pick_in_round=pick_overall&conference=any&draft_pos=qb&draft_pos=rb&draft_pos=wr&draft_pos=te&draft_pos=e&draft_pos=t&draft_pos=g&draft_pos=c&draft_pos=ol&draft_pos=dt&draft_pos=de&draft_pos=dl&draft_pos=ilb&draft_pos=olb&draft_pos=lb&draft_pos=cb&draft_pos=s&draft_pos=db&draft_pos=k&draft_pos=p&offset=', n)
  
  df <- as.data.frame(readHTMLTable(u, stringsAsFactors = FALSE))
  wrList[[length(wrList) + 1]] <- df
  rm(df, n, u)
}

wrDF <- do.call(rbind, wrList)
names(wrDF) <- gsub('results.', '', names(wrDF), fixed = TRUE)

wrDF.clean <- subset(wrDF, Rk != 'Rk')
wrDF.clean$Player <- gsub('*', '', wrDF.clean$Player, fixed = TRUE)


wrDF.clean.chars <- wrDF.clean %>%
  select(Player, Draft, Tm, Lg, Ctch.)

wrDF.clean.nums <- wrDF.clean %>%
  select(-Player, -Draft, -Tm, -Lg, -Ctch.)

wrDF.clean.nums <- as.data.frame(apply(wrDF.clean.nums, 2, as.numeric))

wrDF.fixedcols <- cbind(wrDF.clean.chars, wrDF.clean.nums)


rm(list = ls(pattern = 'wrDF.clean'))

wrDF.fixedcols$touches <- wrDF.fixedcols$Att + wrDF.fixedcols$Tgt

maxTgts <- c()
for (i in unique(wrDF.fixedcols$Age)) {
  ageDF <- subset(wrDF.fixedcols, Age == i)
  pn <- ageDF$Player[ageDF$Tgt == max(ageDF$Tgt)]
  maxTgts[[length(maxTgts) + 1]] <- pn
  rm(ageDF, pn)
}


gg <- ggplot(data = wrDF.fixedcols, 
             mapping = aes(x = Age, y = Tgt, label = paste0(Player, 
                                                                ' (', touches, 
                                                                ')'))) +
  scale_x_continuous(breaks = min(wrDF.fixedcols$Age):max(wrDF.fixedcols$Age)) +
  geom_jitter(alpha = 0.4, size = 3, width = 0.1) +
  geom_text(data = subset(wrDF.fixedcols, Player %in% maxTgts), nudge_y = 5) +
  # geom_label_repel(data = subset(wrDF.fixedcols, Player %in% maxAtts)) +
  geom_boxplot(aes(group = Age), fill = NA, outlier.shape = NA) +
  xlab('Rookie Season Age') +
  ylab('Passing Targets') +
  ggtitle(label = 'Season Total Targets, By NFL Rookie Wide Receivers, By Rookie Season Age', subtitle = '(Player With Maximum Touches By Age Called Out; Minimum 32 Targets)') +
  theme_minimal()
gg

