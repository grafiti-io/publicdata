#### LOAD LIBRARIES
library(fiftystater)
library(ggplot2)
library(plyr)
library(RColorBrewer)

#### READ IN DATA
setwd('.../YouthVoting');
df.raw <- read.csv('Voting-By-Age-State-2012-MOD.csv', stringsAsFactors = FALSE); df.raw$YEAR <- 'Y2012'; df.raw$POP_TOT <- as.numeric(gsub(',', '', df.raw$POP_TOT)); df.raw$POP_CIT <- as.numeric(gsub(',', '', df.raw$POP_CIT)); df.raw$VOTES_TOT <- as.numeric(gsub(',', '', df.raw$VOTES_TOT)); df.ed.12 <- df.raw;
df.raw <- read.csv('Voting-By-Age-State-2014-MOD.csv', stringsAsFactors = FALSE); df.raw$YEAR <- 'Y2014'; df.raw$POP_TOT <- as.numeric(gsub(',', '', df.raw$POP_TOT)); df.raw$POP_CIT <- as.numeric(gsub(',', '', df.raw$POP_CIT)); df.raw$VOTES_TOT <- as.numeric(gsub(',', '', df.raw$VOTES_TOT)); df.ed.14 <- df.raw;
df.raw <- read.csv('Voting-By-Age-State-2016-MOD.csv', stringsAsFactors = FALSE); df.raw$YEAR <- 'Y2016'; df.raw$POP_TOT <- as.numeric(gsub(',', '', df.raw$POP_TOT)); df.raw$POP_CIT <- as.numeric(gsub(',', '', df.raw$POP_CIT)); df.raw$VOTES_TOT <- as.numeric(gsub(',', '', df.raw$VOTES_TOT)); df.ed.16 <- df.raw;
df.comb <- rbind.fill(df.ed.12, df.ed.14, df.ed.16);
states <- cbind.data.frame(state.abb, tolower(state.name)); names(states) <- c('state.abb', 'state.name');

#### CREATE SUMMARY DATASET FOR TWO AGE BINS: 18-24 & 25+
df <- df.comb;
df$AGE.NEW <- ifelse(df$AGE == 'AG1_18_24', 'AB1_18_24',
              ifelse(df$AGE %in% c('AG2_25_34', 'AG3_35_44', 'AG4_45_64', 'AG5_65+'), 'AB2_25+', NA));
df.s <- aggregate(cbind(POP_TOT, POP_CIT, VOTES_TOT) ~ YEAR + STATE + AGE.NEW, data = df, function(x) sum(x)); df.s <- df.s[with(df.s, order(YEAR, STATE, AGE.NEW)), ];
df.s$VRATE.TOT <- round(100 * (df.s$VOTES_TOT / df.s$POP_TOT), 2); df.s$VRATE.CIT <- round(100 * (df.s$VOTES_TOT / df.s$POP_CIT), 2); df.s$STATE <- tolower(df.s$STATE);
df.fin <- merge(df.s, states, by.x = 'STATE', by.y = 'state.name'); df.fin <- df.fin[with(df.fin, order(YEAR, STATE, AGE.NEW)), ];

#### MAKE MAP
df.plot <- subset(df.fin); df.plot$plotvar <- df.plot$VRATE.TOT; numcolor <- 5; breaks <- quantile(df.plot$plotvar, c(seq(0, 1, (1/numcolor)))); breaks; df.plot$colornum <- findInterval(df.plot$plotvar, breaks, all.inside = T);
plotcolor <- brewer.pal(numcolor, 'YlGn'); df.plot$colorcode <- plotcolor[df.plot$colornum]; plotcolor; table(df.plot$colornum);
breaks.vector <- round(as.numeric(breaks)[2:(length(breaks)-1)], 0); legend.vector <- rep(NA, numcolor); legend.vector[1] <- paste0('<', breaks.vector[1]); legend.vector[numcolor] <- paste0('>', breaks.vector[length(breaks.vector)]);
for (i in 2:(length(legend.vector)-1)){legend.vector[i] <- paste0(breaks.vector[i-1], '-', breaks.vector[i])}; legend.vector <- paste0('  ', legend.vector, '%');
df.plot.actual <- subset(df.plot, YEAR == 'Y2012' & AGE.NEW == 'AB1_18_24'); nrow(df.plot.actual);
g1 <- ggplot(df.plot.actual, aes(map_id = STATE)) + coord_map() +
  geom_map(data = df.plot.actual, map = fifty_states, aes(fill = df.plot.actual$colorcode), colour = 'gray') +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  scale_fill_identity(breaks = plotcolor, limits = plotcolor, labels = legend.vector, guide = 'legend') +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) + labs(x = '', y = '') +
  theme(legend.position = 'right', 
        legend.title = element_blank(), legend.text = element_text(size = 80), legend.text.align = 0.5,
        legend.key.size = unit(2, 'in'), panel.background = element_blank()); g1;
png(paste0('map_', 'printed_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '.png'), 
    width = 4000, height = 2000, units = 'px'); g1; dev.off();