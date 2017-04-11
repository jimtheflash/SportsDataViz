rm(list = ls())

library(directlabels)
library(dplyr)
library(ggplot2)
library(ggrepel)

ms <- read.csv(file.choose(), stringsAsFactors = FALSE)
summary(ms)


# distributions in msYD by player age; get max ms by age for labels
maxmsyd <- ms %>% group_by(floor(Age)) %>% 
  summarise(maxMSYD = max(msYD, na.rm = TRUE))
maxmsyd$fil <- paste0(as.character(maxmsyd$`floor(Age)`), 
                      as.character(maxmsyd$maxMSYD))
label.df <- filter(ms, paste0(floor(Age), msYD) %in% maxmsyd$fil)

# make plot
bp <-
ggplot(data = ms, mapping = aes(x = floor(Age), y = msYD)) +
  geom_boxplot(mapping = aes(group = floor(Age)), size = 1.5,
               width = .5) +
  geom_point(data = label.df, aes(x = floor(Age), y = msYD)) +
  geom_text(data = label.df, 
            mapping = aes(label = paste0(Player, ', ', round(msYD *100), '% (', Year, ')')),
            nudge_y = .02,
            size = 4) +
  xlab("Age") +
  scale_x_continuous(breaks = 18:23, labels = 18:23, expand = c(.15, .1)) +
  ylab("Market Share Receiving Yards") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Market Share Receiving Yards\nBy Player Age", subtitle = "2017 NFL Wide Receiver Prospects\n(Season In Parentheses)") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
bp

# summarise by player
player.rsi <- ms %>% group_by(Player) %>% 
  summarise(rsi = max(RSI_Score),
            maxAge = max(Age, na.rm = TRUE),
            avgRecYds = mean(Yds, na.rm = TRUE),
            avgMSYD = mean(msYD, na.rm = TRUE),
            avgMSTD = mean(msTD, na.rm = TRUE))
player.rsi <- player.rsi[order(-player.rsi$rsi, -player.rsi$avgRecYds), ]
player.rsi$rank <- 1:nrow(player.rsi)
player.rsi$PlayerFactor <- factor(player.rsi$Player,
                                  levels = player.rsi$Player[order(player.rsi$avgMSYD)])


# plot average ms's by player
avg.msYDs <- ggplot(data = player.rsi, mapping = aes(x = PlayerFactor, y = avgMSYD)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent, position = "top") +
  ylab("Average Market Share Receiving Yards, College Career") +
  # ggtitle(label = "Average Market Share Receiving Yards, College Career") +
  coord_flip() +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        text = element_text(size = 15))
  
ggsave(avg.msYDs, file = 'avgMSYDS.png', unit = 'cm', height = 20, width = 20)


# RSI quintiles
tierone.rsi <- filter(player.rsi, rank <= 8)
tiertwo.rsi <- filter(player.rsi, rank > 8 & rank <= 16)
tierthree.rsi <- filter(player.rsi, rank > 16 & rank <= 24)
tierfour.rsi <- filter(player.rsi, rank > 24 & rsi > 0)
tierfive.rsi <- filter(player.rsi, rsi == 0)

#ms.tiers
t1 <- filter(ms, Player %in% tierone.rsi$Player) %>% mutate(tier = 'one')
t2 <- filter(ms, Player %in% tiertwo.rsi$Player) %>% mutate(tier = 'two')
t3 <- filter(ms, Player %in% tierthree.rsi$Player) %>% mutate(tier = 'three')
t4 <- filter(ms, Player %in% tierfour.rsi$Player) %>% mutate(tier = 'four')
t5 <- filter(ms, Player %in% tierfive.rsi$Player) %>% mutate(tier = 'five')


all.tiers <- rbind(t1, t2, t3, t4, t5) %>%
  mutate(flAge = floor(Age)) %>%
  group_by(tier, flAge) %>%
  summarise(meanMSYD = mean(msYD))

tier.summary <- rbind(t1, t2, t3, t4, t5) %>%
  mutate(flAge = floor(Age)) %>%
  group_by(flAge) %>%
  summarise(meanMSYD = mean(msYD),
            sdMSYD = sd(msYD))

colorz <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99')

# tier one plot
t1plot <- ggplot() +
  geom_line(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            linetype = "33", color = 'black', size = 2, alpha = .3) +
  geom_point(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            color = 'black', size = 4, alpha = .3) +
  geom_errorbar(data = tier.summary,
                aes(x = flAge, ymin = meanMSYD - sdMSYD, ymax = meanMSYD + sdMSYD),
                color = 'black', size = 2, width = 1/5, alpha = .05) +
  geom_line(data = t1,
            aes(x = floor(Age), y = msYD, group = Player, color = Player),
            size = 1, alpha = .5) +
  geom_point(data = t1, 
            aes(x = floor(Age), y = msYD, color = Player), size = 2) +
  scale_x_continuous(breaks = 18:24, labels = 18:24, expand = c(.1, .1)) +
  xlab('Age (Years)') +
  scale_y_continuous(labels = scales::percent) +
  ylab('Average MS Receiving Yards') +
  ggtitle('MS Receiving Yards, First Tier 2017 Wide Receiver Prospects',
          subtitle = 'RotoViz Scouting Index Scores 124 To 198') +
  scale_color_brewer(type = 'div', palette = 'Dark2') +
  annotate(geom = 'text', x = 20.5, y = .11, 
           label = 'Points On Dashed Line Represent Mean MS Receiving Yards\nAcross All 2017 Wide Receiver Prospects For That Age\n\nError Bars Are +/- 1 Standard Deviation Around The Mean', 
           color = 'black', alpha = .5, hjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 12))

direct.label(t1plot, method = list(dl.trans(x=x+.1, y=y+.5), 'last.bumpup'))


# tier two plot
t2plot <- ggplot() +
  geom_line(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            linetype = "33", color = 'black', size = 2, alpha = .3) +
  geom_point(data = tier.summary, 
             aes(x = flAge, y = meanMSYD),
             color = 'black', size = 4, alpha = .3) +
  geom_errorbar(data = tier.summary,
                aes(x = flAge, ymin = meanMSYD - sdMSYD, ymax = meanMSYD + sdMSYD),
                color = 'black', size = 2, width = 1/5, alpha = .05) +
  geom_line(data = t2,
            aes(x = floor(Age), y = msYD, group = Player, color = Player),
            size = 1, alpha = .5) +
  geom_point(data = t2, 
             aes(x = floor(Age), y = msYD, color = Player), size = 2) +
  scale_x_continuous(breaks = 18:24, labels = 18:24, expand = c(.1, .1)) +
  xlab('Age (Years)') +
  scale_y_continuous(labels = scales::percent) +
  ylab('Average MS Receiving Yards') +
  ggtitle('MS Receiving Yards, Second Tier 2017 Wide Receiver Prospects',
          subtitle = 'RotoViz Scouting Index Scores 100 To 124') +
  scale_color_brewer(type = 'div', palette = 'Dark2') +
  annotate(geom = 'text', x = 21.5, y = .1, 
           label = 'Points On Dashed Line Represent Mean MS Receiving Yards\nAcross All 2017 Wide Receiver Prospects For That Age\n\nError Bars Are +/- 1 Standard Deviation Around The Mean', 
           color = 'black', alpha = .5, hjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 12))

direct.label(t2plot, method = list(dl.trans(x=x+.1, y=y+.5), 'last.bumpup'))

# tier three plot
t3plot <- ggplot() +
  geom_line(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            linetype = "33", color = 'black', size = 2, alpha = .3) +
  geom_point(data = tier.summary, 
             aes(x = flAge, y = meanMSYD),
             color = 'black', size = 4, alpha = .3) +
  geom_errorbar(data = tier.summary,
                aes(x = flAge, ymin = meanMSYD - sdMSYD, ymax = meanMSYD + sdMSYD),
                color = 'black', size = 2, width = 1/5, alpha = .05) +
  geom_line(data = t3,
            aes(x = floor(Age), y = msYD, group = Player, color = Player),
            size = 1, alpha = .5) +
  geom_point(data = t3, 
             aes(x = floor(Age), y = msYD, color = Player), size = 2) +
  scale_x_continuous(breaks = 18:24, labels = 18:24, expand = c(.1, .1)) +
  xlab('Age (Years)') +
  scale_y_continuous(labels = scales::percent) +
  ylab('Average MS Receiving Yards') +
  ggtitle('MS Receiving Yards, Third Tier 2017 Wide Receiver Prospects',
          subtitle = 'RotoViz Scouting Index Scores 52 To 81') +
  scale_color_brewer(type = 'div', palette = 'Dark2') +
  annotate(geom = 'text', x = 18, y = .35, 
           label = 'Points On Dashed Line Represent Mean MS Receiving Yards\nAcross All 2017 Wide Receiver Prospects For That Age\n\nError Bars Are +/- 1 Standard Deviation Around The Mean', 
           color = 'black', alpha = .5, hjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 12))

direct.label(t3plot, method = list(dl.trans(x=x+.1, y=y+.5), 'last.bumpup'))

# tier four plot
t4plot <- ggplot() +
  geom_line(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            linetype = "33", color = 'black', size = 2, alpha = .3) +
  geom_point(data = tier.summary, 
             aes(x = flAge, y = meanMSYD),
             color = 'black', size = 4, alpha = .3) +
  geom_errorbar(data = tier.summary,
                aes(x = flAge, ymin = meanMSYD - sdMSYD, ymax = meanMSYD + sdMSYD),
                color = 'black', size = 2, width = 1/5, alpha = .05) +
  geom_line(data = t4,
            aes(x = floor(Age), y = msYD, group = Player, color = Player),
            size = 1, alpha = .5) +
  geom_point(data = t4, 
             aes(x = floor(Age), y = msYD, color = Player), size = 2) +
  scale_x_continuous(breaks = 18:24, labels = 18:24, expand = c(.1, .1)) +
  xlab('Age (Years)') +
  scale_y_continuous(labels = scales::percent) +
  ylab('Average MS Receiving Yards') +
  ggtitle('MS Receiving Yards, Fourth Tier 2017 Wide Receiver Prospects',
          subtitle = 'RotoViz Scouting Index Scores 27 To 48') +
  scale_color_brewer(type = 'div', palette = 'Dark2') +
  annotate(geom = 'text', x = 18, y = .35, 
           label = 'Points On Dashed Line Represent Mean MS Receiving Yards\nAcross All 2017 Wide Receiver Prospects For That Age\n\nError Bars Are +/- 1 Standard Deviation Around The Mean', 
           color = 'black', alpha = .5, hjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 12))

direct.label(t4plot, method = list(dl.trans(x=x+.1, y=y+.5), 'last.bumpup'))

# tier five plot
t5plot <- ggplot() +
  geom_line(data = tier.summary, 
            aes(x = flAge, y = meanMSYD),
            linetype = "33", color = 'black', size = 2, alpha = .3) +
  geom_point(data = tier.summary, 
             aes(x = flAge, y = meanMSYD),
             color = 'black', size = 4, alpha = .3) +
  geom_errorbar(data = tier.summary,
                aes(x = flAge, ymin = meanMSYD - sdMSYD, ymax = meanMSYD + sdMSYD),
                color = 'black', size = 2, width = 1/5, alpha = .05) +
  geom_line(data = t5,
            aes(x = floor(Age), y = msYD, group = Player, color = Player),
            size = 1, alpha = .5) +
  geom_point(data = t5, 
             aes(x = floor(Age), y = msYD, color = Player), size = 2) +
  scale_x_continuous(breaks = 18:24, labels = 18:24, expand = c(.1, .1)) +
  xlab('Age (Years)') +
  scale_y_continuous(labels = scales::percent) +
  ylab('Average MS Receiving Yards') +
  ggtitle('MS Receiving Yards, Fifth Tier 2017 Wide Receiver Prospects',
          subtitle = 'No RotoViz Scouting Index Score Available') +
  annotate(geom = 'text', x = 18, y = .45, 
           label = 'Points On Dashed Line Represent Mean MS Receiving Yards\nAcross All 2017 Wide Receiver Prospects For That Age\n\nError Bars Are +/- 1 Standard Deviation Around The Mean', 
           color = 'black', alpha = .5, hjust = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 12))

direct.label(t5plot, method = list(dl.trans(x=x+.1, y=y+.5), 'last.bumpup'))





ggplot(data = t1, aes(x = floor(Age), y = msYD, facets = Player)) +
  geom_line() +
  facet_grid(Player ~ .)



make.plot <- function(viz.data, tier, title.ranks, dl = TRUE, sp.title = NA) {
  
  # select input data
  input.data <- filter(ms, Player %in% viz.data$Player)
  # specify a colorblind friendly palette
  cbPalette <- c("#999999", 
                 "#E69F00", 
                 "#56B4E9", 
                 "#009E73", 
                 "#000000", 
                 "#0072B2", 
                 "#D55E00", 
                 "#CC79A7",
                 "#999999")
  
  # call plot
  
  plot.call <-
    ggplot(data = input.data, mapping = aes(x = floor(Age), y = msYD, color = Player)) +
    geom_point(size = 1) +
    geom_line(alpha = 1, size = 1) +
    scale_x_continuous(limits = c(18, 24), expand = c(.1, .1)) +
    xlab("Age (Years)") +
    scale_y_continuous(labels = scales::percent, limits = c(0, .5)) +
    ylab("Market Share Receiving Yards") +
    scale_color_manual(values = cbPalette) +
    ggtitle(label = "Market Share Receiving Yards By Age", 
            subtitle = ifelse(is.na(sp.title), paste0("2017 Wide Receiver Prospects, Tier ", tier, "\nRotoViz Scouting Index Scores From ", 
                                                      min(input.data$RSI_Score), " To ", max(input.data$RSI_Score)), sp.title)) +
    theme_minimal() +
    theme(panel.grid = element_line(colour = "lightgrey"))
  
  if (dl) {
   ret <- direct.label(plot.call, method = list(cex = 1, dl.trans(x=x+0.1, y=y*1.01), "last.bumpup"))
   return(ret)
  } else {
    return(plot.call)
  }
}

#### NOTE: CHANGE "AGE" TO FLOOR AGE


t1 <- make.plot(tierone.rsi, "One", "1-8")
t2 <- make.plot(tiertwo.rsi, "Two", "9-16")
t3 <- make.plot(tierthree.rsi, "Three", "17-24")
t4 <- make.plot(tierfour.rsi, "Four", "25-32")
t5 <- make.plot(tierfive.rsi, "Five", "33-40", sp.title = paste("2017 Wide Receiver Prospects, Tier Five, RotoViz Scouting Index Scores Unavailable"))

ggsave(plot = t1, file = 't1_floored.png', units = 'in', height = 5, width = 8)
ggsave(plot = t2, file = 't2.png', units = 'in', height = 5, width = 8)
ggsave(plot = t3, file = 't3.png', units = 'in', height = 5, width = 8)
ggsave(plot = t4, file = 't4.png', units = 'in', height = 5, width = 8)
ggsave(plot = t5, file = 't5.png', units = 'in', height = 5, width = 8)



# tier summaries
tier.summaries <- rbind(data.frame(filter(ms, Player %in% tierone.rsi$Player), tier = 'one'),
                        data.frame(filter(ms, Player %in% tiertwo.rsi$Player), tier = 'two'),
                        data.frame(filter(ms, Player %in% tierthree.rsi$Player), tier = 'three'),
                        data.frame(filter(ms, Player %in% tierfour.rsi$Player), tier = 'four'),
                        data.frame(filter(ms, Player %in% tierfive.rsi$Player), tier = 'five')) %>%
  mutate(ag = floor(Age)) %>%
  group_by(tier, ag) %>%
  summarise(MSYD = mean(msYD, na.rm = TRUE),
            cases = n())

total.summary <- ms %>% mutate(ag = floor(Age)) %>% group_by(ag) %>%
  summarise(MSYD = mean(msYD, na.rm = TRUE))

tiers <- ggplot(data = tier.summaries, mapping = aes(x = ag, y = MSYD, color = tier, size = cases)) +
  geom_point() +
  geom_line(size = .5)
tiers

gp <- ggplot() +
  geom_line(data = filter(tier.summaries, tier == 'one'), mapping = aes(x = ag, y = MSYD), color = 'red', size = 2) +
  geom_text(data = filter(tier.summaries, tier == 'one'), mapping = aes(labels = 'tier one')) +
  geom_line(data = filter(tier.summaries, tier == 'two'), mapping = aes(x = ag, y = MSYD), color = 'lightgrey') +
  geom_line(data = filter(tier.summaries, tier == 'three'), mapping = aes(x = ag, y = MSYD), color = 'lightgrey') +
  geom_line(data = filter(tier.summaries, tier == 'four'), mapping = aes(x = ag, y = MSYD), color = 'lightgrey') +
  geom_line(data = filter(tier.summaries, tier == 'five'), mapping = aes(x = ag, y = MSYD), color = 'lightgrey') +
  geom_line(data = total.summary, mapping = aes(x = ag, y = MSYD), color = 'darkgrey', size = 2, label = tier) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

direct.label(gp, method = "smart.grid")

mean.ms <- ms %>% group_by(Player) %>% summarise(meanMSYD = mean(msYD, na.rm = TRUE),
                                                 RSI = max(RSI_Score, na.rm = TRUE))
mean.ms$PlayerF <- factor(mean.ms$Player, levels = mean.ms$Player[order(mean.ms$meanMSYD)])

ggplot(data = mean.ms, mapping = aes(x = PlayerF, y = meanMSYD)) +
  geom_bar(stat = 'identity') +
  scale_x_discrete(breaks = mean.ms$PlayerF, labels = paste0(as.character(mean.ms$PlayerF), ' (', mean.ms$RSI, ')')) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal()
# 
# ['#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#005a32']

# ggsave(plot = t1, file = 't1.jpg', unit = 'in', width = 6, height = 4)


# 
#   # geom_line(size = 2, mapping = aes(color = Player)) +
#   geom_smooth(method = 'loess', color = 'darkgray', fill = '#f0f0f0') +
#   geom_text(mapping = aes(label = ifelse(msYD > bound, paste0(Player,', ', Year), '')), nudge_y = .015) +
#   scale_x_continuous(breaks = 18:25, minor_breaks = NULL, label = 18:25) +
#   scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5), minor_breaks = NULL) +
#   theme_minimal()


# 
# ggplot(data = best_year, mapping = aes(x = Age, y = bestMSYD)) +
#   geom_point(size = 2) +
#   # geom_text(mapping = aes(label = ifelse(msYD > bound, paste0(Player,', ', Year), '')), nudge_y = .015) +
#   scale_x_continuous(breaks = 18:25, minor_breaks = NULL, label = 18:25) +
#   scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5), minor_breaks = NULL) +
#   theme_minimal() +
#   geom_smooth(data = ms, color = 'darkgray', fill = '#f0f0f0')
# 
# mod <- loess(data = ms, formula = msYD ~ Age)
# preds <- predict(mod, ms$Age, se = TRUE)
# 
# ms$upper.bound <- preds$fit + 2.575 * preds$se.fit
# ms$residuals <- mod$residuals
# 
# best.year <- ms %>% group_by(Player, Year) %>% 
#   summarise(bestMSYD = max(msYD, na.rm = TRUE),
#             bestMSTD = max(msTD, na.rm = TRUE)) %>%
#   filter(bestMSYD == max(bestMSYD))
# 
# filtered.best.year <- 
#   ms[(paste(ms$Player, ms$Year) %in% paste(best.year$Player, best.year$Year)), ]
# 
# 
# ggplot(data = ms, mapping = aes(x = Age, y = msYD)) +
#   geom_point(data = ms, mapping = aes(x = Age, y = msYD), size = 3, alpha = .5) +
#   geom_smooth(data = ms, mapping = aes(x = Age, y = msYD), method = 'loess', color = 'darkgray', fill = '#f0f0f0') +
#   geom_text(data = filtered.best.year, mapping = aes(label = ifelse(msYD >= upper.bound, paste0(Player, Year), '')), nudge_y = .015) +
#   theme_minimal()
# 
