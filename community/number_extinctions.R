#immigrants and extinction

#find number of immigrant/extinct

source("community/start_here.R")
head(cover_thin)
#plot
minY <- min(cover_thin$year)
maxY <- max(cover_thin$year)

ex_im <- cover_thin %>%
  select(-speciesName, -originBlockID, -destBlockID, -destPlotID) %>%
  group_by(turfID, TTtreat, destSiteID) %>%
  do({
    plyr::ldply((minY + 1):maxY, function(y) {
      c0 <- .$species[.$year == minY]
      c1 <- .$species[.$year == y]
      extinct <- length(setdiff(c0, c1))/length(c0)
      extinct[length(c1) == 0] <- NA
      immigrant <- length(setdiff(c1, c0))
      immigrant[length(c1) == 0] <- NA
      
      data.frame(year = y, extinct, immigrant)
    })
  })
    

##find final
controlturf <- turfs %>%  
  filter(TTtreat == "control") %>%
  select(turfID, destSiteID, destBlockID) %>%
  rename(controlTurfID = turfID)

final_ex_im <- # how many spp need to go extinct to match ctrl turf
  cover_thin %>%
  filter(year == minY) %>%
  select(-speciesName,-originBlockID) %>%
  left_join(controlturf) %>%
  group_by(turfID, TTtreat, destSiteID) %>%
  summarise(
    extinct = length(
      setdiff(species,
              cover_thin$species[cover_thin$turfID == controlTurfID[1] &
                                   cover_thin$year == maxY])) / length(species),
    immigrant = length(
      setdiff(cover_thin$species[cover_thin$turfID == controlTurfID[1] &
                                                 cover_thin$year == maxY], 
      species))
  )

ggplot(final_ex_im, aes(x = TTtreat, y  = extinct, colour = destSiteID)) + 
  geom_boxplot()


plot_extinctImmigrant <- function(x, final, what, ylab = ""){
  turfcolour = "grey60"
  g <- ggplot(x, aes_string(x = "year", y = what, group = "turfID")) +
    geom_line(colour = turfcolour) +
    stat_summary(geom = "line", fun.y  = mean, mapping = aes_string(x = "year", y = what, colour  = "destSiteID"), inherit.aes = FALSE, size = 1.2) + 
    facet_wrap(~TTtreat, ncol = 2) + 
    labs(x = "Year CE", ylab = ylab, colour = "Destination site") +
    theme_bw()
  
  
  if (missing(final)) {
    g <- g + scale_x_continuous(breaks = min(x$year):max(x$year))
  } else{
    offset <- 0.3
    final$x <- max(x$year) + offset
    g <- g + geom_point(data  = final,
                        mapping = aes_string(x = "x", y = what),
                        colour = turfcolour) +
      stat_summary(
        data  = final,
        mapping = aes_string(x = "x", y = what, colour  = "destSiteID"),
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

plot_extinctImmigrant(ex_im, final_ex_im, what = "extinct", ylab = "Proportion extinct")
plot_extinctImmigrant(ex_im, final_ex_im, what = "immigrant", ylab = "Number of immigrants")
