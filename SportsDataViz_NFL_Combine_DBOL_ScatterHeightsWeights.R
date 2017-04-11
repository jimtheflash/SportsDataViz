library(dplyr)
library(ggplot2)
library(XML)

load(file.choose()) # open combineData.RData
pulled <- combine.data
names(pulled) <- gsub('results.', '', names(pulled), fixed = TRUE)                      
# make weights numeric; since all invitees are weighed, removing NAs cleans out the repeated row headers
pulled$Wt <- as.numeric(as.character(pulled$Wt))
cleaned <- subset(pulled, !is.na(pulled$Wt))

# fix heights (turn this into a function)
heights <- strsplit(as.character(cleaned$Height), '-')
ft <- as.numeric(lapply(heights, '[[', 1))
inches <- as.numeric(lapply(heights, '[[', 2))
cleaned$ht.numeric <- (ft * 12) + inches

cleaned$Pos <- as.character(cleaned$Pos)

# rename position groups  (could certainly do this more efficiently)
cleaned$Pos2 <- ifelse(cleaned$Pos %in% c('FS', 'SS'), 'S', ifelse(
  cleaned$Pos %in% c('ILB', 'OLB'), 'LB', ifelse(
    cleaned$Pos %in% c('LS','P', 'K'), 'SpT', ifelse(
      cleaned$Pos %in% c('C', 'OG'), 'C/OG', ifelse(
        cleaned$Pos == 'FB', 'RB', cleaned$Pos
      )))))

# then make position a factor, ordered in some kind of sensible way
cleaned$PositionGroup <- factor(cleaned$Pos2, 
                                levels = c('WR', 'RB', 'TE', 'QB',
                                           'C/OG', 'OT', 'DT', 'DE',
                                           'CB', 'S', 'LB', 'SpT'))

cleaned$pos3 <- ifelse(cleaned$Pos %in% c('OT', 'C', 'OG'), 'OL',
                       ifelse(cleaned$Pos %in% c('CB', 'FS', 'SS'), 'DB', 
                              cleaned$Pos))

# now plot 
g <- ggplot(data = filter(cleaned, Pos2 %in% c('OT', 'C/OG', 'CB', 'S')), 
            mapping = aes(x = Wt, y = ht.numeric, color = pos3)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_grey() +
  xlab('Weight (lbs)') +
  ylab('Height (inches)') +
  ggtitle(label = 'NFL Combine Heights and Weights By Position',
          subtitle = 'Defensive Backs Vs. Offensive Linemen') +
  theme_minimal()

dl <- direct.label(g, method = list("chull.grid", cex = 1.5))
dl
ggsave('SportsDataViz_NFL_Combine_DBOL_ScatterHeightsWeights.jpg', 
       plot = dl, units = 'in', width = 5, height = 5)  


