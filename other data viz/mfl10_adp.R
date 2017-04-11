library(dplyr)
library(ggplot2)
library(ggrepel)
library(plotly)


mfl10 <- read.csv(file.choose())
min10 <- filter(mfl10, drafts >= 10)


ggplot(data = min10, mapping = aes(x = pos, y = adp, facets = pos, group = pos)) +
  geom_jitter(alpha = .33, width = .05) +
  geom_violin(fill = NA) +
  scale_y_reverse()


rb10 <- filter(min10, pos == 'RB')
wr10 <- filter(min10, pos == 'WR')

hist(rb10$adp)
qplot(rb10$adp)
qplot(x = 1:length(rb10$adp), y = rb10$adp[order(rb10$adp)]) + geom_text_repel(aes(label = ))

ggplot(data = rb10, mapping = aes(x = pos.rk, y = adp, label = name)) +
  geom_point() +
  geom_text_repel()


ggplot(data = wr10, mapping = aes(x = pos.rk, y = adp, label = name)) +
  geom_point() +
  geom_dl(method = 'smart.grid')

direct.label()
