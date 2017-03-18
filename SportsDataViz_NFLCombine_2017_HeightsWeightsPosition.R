library(dplyr)
library(ggplot2)
library(XML)

# pulled data
u <- 'http://www.pro-football-reference.com/draft/2017-combine.htm'
pulled <- as.data.frame(readHTMLTable(u, stringsAsFactors = FALSE))
names(pulled) <- gsub('combine.', '', names(pulled), perl = TRUE)                      
# make weights numeric; since all invitees are weighed, removing NAs cleans out the repeated row headers
pulled$Wt <- as.numeric(pulled$Wt)
cleaned <- subset(pulled, !is.na(pulled$Wt))

# fix heights (turn this into a function)
heights <- strsplit(cleaned$Ht, '-')
ft <- as.numeric(lapply(heights, '[[', 1))
inches <- as.numeric(lapply(heights, '[[', 2))
cleaned$ht.numeric <- (ft * 12) + inches

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

# now plot 
g <- ggplot(data = cleaned, mapping = aes(x = Wt, y = ht.numeric)) +
  facet_wrap(facets = 'PositionGroup') +
  geom_point() +
  xlab('Weight (lbs)') +
  ylab('Height (inches)') +
  ggtitle('NFL Combine 2017 Heights and Weights By Position')

g

ggsave('NFLCombine_2017_HeightsWeightsPosition.jpg', plot = g, units = 'in',
       width = 10, height = 5)  


