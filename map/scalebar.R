scalebar <- function (data = NULL, location = "bottomright", dist, height = 0.02, 
                      st.dist = 0.02, st.bottom = TRUE, st.size = 5, dd2km = NULL, 
                      model, x.min, x.max, y.min, y.max, anchor = NULL, facet.var = NULL, 
                      facet.lev = NULL) 
{
  if (is.null(data)) {
    if (is.null(x.min) | is.null(x.max) | is.null(y.min) | 
        is.null(y.max)) {
      stop("If data is not defined, x.min, x.max, y.min and y.max must be.")
    }
    data <- data.frame(long = c(x.min, x.max), lat = c(y.min, 
                                                       y.max))
  }
  if (location == "bottomleft") {
    if (is.null(anchor)) {
      x <- min(data$long)
      y <- min(data$lat)
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "bottomright") {
    if (is.null(anchor)) {
      x <- max(data$long)
      y <- min(data$lat)
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (location == "topleft") {
    if (is.null(anchor)) {
      x <- min(data$long)
      y <- max(data$lat)
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- 1
  }
  if (location == "topright") {
    if (is.null(anchor)) {
      x <- max(data$long)
      y <- max(data$lat)
    }
    else {
      x <- as.numeric(anchor["x"])
      y <- as.numeric(anchor["y"])
    }
    direction <- -1
  }
  if (!st.bottom) {
    st.dist <- y + (max(data$lat) - min(data$lat)) * (height + 
                                                        st.dist)
  }
  else {
    st.dist <- y - (max(data$lat) - min(data$lat)) * st.dist
  }
  height <- y + (max(data$lat) - min(data$lat)) * height
  if (!is.null(dd2km)) {
    break1 <- maptools::gcDestination(lon = x, lat = y, bearing = 90 * 
                                        direction, dist = dist, dist.units = "km", model = model)[1, 
                                                                                                  1]
    break2 <- maptools::gcDestination(lon = x, lat = y, bearing = 90 * 
                                        direction, dist = dist * 2, dist.units = "km", model = model)[1, 
                                                                                                      1]
  }
  else {
    if (location == "bottomleft" | location == "topleft") {
      break1 <- x + dist * 1000
      break2 <- x + dist * 2000
    }
    else {
      break1 <- x - dist * 1000
      break2 <- x - dist * 2000
    }
  }
  box1 <- data.frame(x = c(x, x, rep(break1, 2), x), y = c(y, 
                                                           height, height, y, y), group = 1)
  box2 <- data.frame(x = c(rep(break1, 2), rep(break2, 2), 
                           break1), y = c(y, rep(height, 2), y, y), group = 1)
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    box1[, facet.var] <- facet.lev
    box2[, facet.var] <- facet.lev
  }
  
  gg.box1 <- geom_polygon(data = box2, aes(x, y), fill = "white", 
                          color = NA)
  #gg.box2 <- geom_polygon(data = box2, aes(x, y), fill = "black", 
                          #color = "black")
  x.st.pos <- c(box1[c(1, 3), 1], box2[3, 1])
  if (location == "bottomright" | location == "topright") {
    x.st.pos <- rev(x.st.pos)
  }
  legend2 <- cbind(data[1:3, ], x = x.st.pos, y = st.dist, 
                   label = c(0,  paste0(" ", c(dist, dist * 2), "km")))
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    legend2[, facet.var] <- facet.lev
    legend2[, facet.var] <- facet.lev
  }
  if (!is.null(facet.var) & !is.null(facet.lev)) {
    gg.legend <- geom_text(data = legend2, aes(x, y, label = label), 
                           size = st.size)
  }
  else {
    gg.legend <- annotate("text", label = legend2$label[1:2], x = x.st.pos[1:2], y = st.dist, size = st.size, colour = "white")
  }
  return(list(gg.box1, gg.legend))
}