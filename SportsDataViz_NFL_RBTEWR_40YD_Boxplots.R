library(dplyr)
library(ggplot2)

load(file.choose()) # find combineData.RData

rb <- combine.data %>% filter(results.Pos == 'RB')
qplot(as.numeric(as.character(rb$results.40YD)), bins = 10) +
  xlab("40-Yard Dash Times (Seconds)") +
  ylab("Count") +
  theme_minimal()


gp <-
ggplot(data = filter(combine.data, results.Pos %in% c("RB", "TE", "WR")),
       mapping = aes(group = results.Pos, x = results.Pos, y = as.numeric(as.character(results.40YD)))) +
  geom_boxplot(size = 1.05, width = 1/3) +
  xlab("Position") +
  ylab("40-Yard Dash Times (Seconds)") +
  ggtitle(label = "40-Yard Dash Times By Selected Position Groups", 
          subtitle = "NFL Combine Results, 2000-2016") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))
gp

ggsave('posgroups_boxes.jpg', plot = gp, units = 'in', width = 10, height = 5)
ggsave()
