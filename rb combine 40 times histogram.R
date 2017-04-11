rm(list = ls())
library(dplyr)
library(ggplot2)
library(XML)

yrList <- 2000:2016
posList <- c('QB', 'WR', 'TE', 'RB', 'FB', 'OT', 'OG', 'C', 'DE', 'DT', 'ILB', 'OLB', 'SS', 'FS', 'CB', 'LS', 'K', 'P')

outList <- c()
for (x in yrList) {
  yrOut <- c()
  for (y in posList) {
    URL <- paste0('http://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=', x, '&year_max=', x, '&height_min=65&height_max=82&weight_min=149&weight_max=375&pos=', y, '&show=all&order_by=year_id')
    print(paste0('grabbing data for ', y, 's from ', x))
    # THIS IS IMPORTANT; Sys.sleep() ensures you don't violate pfr's terms of service; set > 3
    Sys.sleep(6)
    yrPos.out <- try(as.data.frame(readHTMLTable(URL)), silent = TRUE)
    if(class(yrPos.out) != 'try-error') {
      yrOut[[y]] <- yrPos.out
    }
  }
  
  yrOut.out <- do.call(rbind, yrOut)
  outList[[x]] <- yrOut.out
}

combine.data <- do.call(rbind, outList)
save(combine.data, file = 'combineData.RData')

clean <- data.frame(Year = as.numeric(as.character(combine.data$results.Year)),
                    Player = as.character(combine.data$results.Player),
                    Pos = as.character(combine.data$results.Pos),
                    AV = as.numeric(as.character(combine.data$results.AV)),
                    School = as.character(combine.data$results.School),
                    Height = as.character(combine.data$results.Height),
                    Weight = as.numeric(as.character(combine.data$results.Wt)),
                    Forty = as.numeric(as.character(combine.data$results.40YD)),
                    Vertical = as.numeric(as.character(combine.data$results.Vertical)),
                    BenchReps = as.numeric(as.character(combine.data$results.BenchReps)),
                    Broad = as.numeric(as.character(combine.data$results.Broad.Jump)),
                    Cone = as.numeric(as.character(combine.data$results.3Cone)),
                    Shuttle = as.numeric(as.character(combine.data$results.Shuttle)),
                    DPOS = as.character(combine.data$results.Drafted..tm.rnd.yr.),
                    stringsAsFactors = FALSE)
rm(posList, URL, x, y, yrList, yrOut)

# fix height
height.split <- strsplit(clean$Height, '-', fixed = TRUE)
feet <- as.numeric(lapply(height.split, '[', 1))
inches <- as.numeric(lapply(height.split, '[', 2))
height.fixed <- inches + (feet * 12)
clean$Height.fixed <- height.fixed
clean$Height <- NULL
rm(height.split, feet, inches, height.fixed)

# fix draft detail
detail <- strsplit(clean$DPOS, '/', fixed = TRUE)
team <- lapply(detail, '[', 1)
team.fixed <- unlist(team)
team.fixed <- trimws(team.fixed)
clean$Team.fixed <- team.fixed

round <- lapply(detail, '[', 2)
round.fixed <- unlist(round)
round.fixed <- trimws(round.fixed)
round.fixed <- gsub(pattern = 'st|nd|th|rd', replacement = '', round.fixed)
round.fixed <- as.numeric(round.fixed)
clean$Round.fixed <- round.fixed

pick <- lapply(detail, '[', 3)
pick.fixed <- unlist(pick)
pick.fixed <- trimws(pick.fixed)
pick.fixed <- gsub('st|nd|th|rd| pick', '', pick.fixed)
pick.fixed <- as.numeric(pick.fixed)
clean$Pick.fixed <- pick.fixed

rm(list = ls(pattern = 'team|pick|round|team'))

clean$playedInNFL <- ifelse(is.na(clean$AV), 0, 1)
clean$draftedInd <- ifelse(is.na(clean$Pick.fixed), 0, 1)
cleaner <- subset(clean, Pos != 'Pos')

# make plots!
# rb40yd dash
rb <- cleaner[cleaner$Pos == 'RB', ]
wr <- cleaner[cleaner$Pos == 'WR', ]

qplot(rb$Forty, bins = 10, xlab = '40 Yard Dash Time (seconds)') +
  ggtitle('NFL Combine 40 Yard Dash Times, Running Backs, 2000-2016') +
  theme_minimal()
