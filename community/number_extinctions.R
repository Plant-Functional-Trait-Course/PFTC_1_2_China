set.seed(42)
data.frame(x = rep(1:3, each = 10), y = sample(letters[1:10], replace = TRUE, size = 30)) %>% 
  

#immigrants and extinction

#find number of immigrant/extinct

source("community/start_here.R")
head(cover_thin)
#plot
minY <- min(cover_thin$year)
maxY <- max(cover_thin$year)

pextinct <- cover_thin %>%
  select(-speciesName, -originBlockID, -destBlockID, -destPlotID) %>%
  group_by(turfID, TTtreat, destSiteID) %>%
  do({
    plyr::ldply((minY + 1):maxY, function(y) {
      c0 <- .$species[.$year == minY]
      c1 <- .$species[.$year == y]
      value <- length(setdiff(c0, c1))/length(c0)
      value[length(c1) == 0] <- NA
      data.frame(year = y, value = value)
    })
  })
    

##find final
controlturf <- turfs %>%  
  filter(TTtreat == "control") %>%
  select(turfID, destSiteID, destBlockID) %>%
  rename(controlTurfID = turfID)

finalextinct <-
  cover_thin %>% 
    filter(year == minY) %>%
    select(-speciesName, -originBlockID) %>%
    merge(controlturf) %>% 
    group_by(turfID, TTtreat, destSiteID) %>%
    summarise(final = length(
      setdiff(
      species,
      cover_thin$species[cover_thin$turfID == controlTurfID[1] & cover_thin$year == maxY]
    ))/length(species))
ggplot(finalextinct, aes(x = TTtreat, y  = final, colour = destSiteID)) + geom_boxplot()


plot_extinctImmigrant <- function(x, final, ylab = ""){
  turfcolour = "grey60"
  g <- ggplot(x, aes(x = year, y = value, group = turfID)) +
    geom_line(colour = turfcolour) +
    stat_summary(geom = "line", fun.y  = mean, mapping = aes(x = year, y = value, colour  = destSiteID), inherit.aes = FALSE, size = 1.2) + 
    facet_wrap(~TTtreat, ncol = 2) + 
    labs(x = "Year CE", ylab = ylab, colour = "Destination site") +
    theme_bw()
  
  
  if (missing(final)) {
    g <- g + scale_x_continuous(breaks = min(x$year):max(x$year))
  } else{
    offset <- 0.3
    final <- cbind(final, x = max(x$year) + offset)
    g <- g + geom_point(data  = final,
                        mapping = aes(x = x, y = final),
                        colour = turfcolour) +
      stat_summary(
        data  = final,
        mapping = aes(x = x, y = final, colour  = destSiteID),
        inherit.aes = FALSE,
        fun.y = mean,
        geom = "point"
      ) +
      scale_x_continuous(
        breaks = c(min(x$year):max(x$year), max(x$year) + offset),
        labels = c(min(x$year):max(x$year), expression(infinity))
      )
    
  }
  g
}
plot_extinctImmigrant(pextinct, finalextinct, ylab = "Proportion extinct")
