autoplot.prcWithoutSP <- function(object, select, xlab, ylab,
                           title = NULL, subtitle = NULL, caption = NULL,
                           legend.position = "top", ...) {
  ## fortify the model object
  fobj <- fortify(object, ...)
  
  ## levels of factors - do this now before we convert things
  TimeLevs <- levels(fobj$Time)
  TreatLevs <- levels(fobj$Treatment)
  
  ## convert Time to a numeric
  fobj$Time <- as.numeric(as.character(fobj$Time))
  
  ## process select
  ind <- fobj$Score != "Sample"
  if(missing(select)) {
    select <- rep(TRUE,sum(ind))
  } else {
    stopifnot(isTRUE(all.equal(length(select), sum(ind))))
  }
  
  ## samples and species "scores"
  samp <- fobj[!ind, ] 
  spp <- fobj[ind,][select, ]
  
  ## base plot
  plt <- ggplot(data = samp,
                aes_string(x = 'Time', y = 'Response', group = 'Treatment',
                           colour = 'Treatment', linetype = 'Treatment'))
  ## add the control
  plt <- plt + geom_hline(yintercept = 0)

  ## add the coefficients
  plt <- plt + geom_line() +
    theme(legend.position = legend.position) +
    scale_x_continuous(breaks = as.numeric(TimeLevs), minor_breaks = NULL)
  
  ## add labels
  if(missing(xlab)) {
    xlab <- 'Time'
  }
  if(missing(ylab)) {
    ylab <- 'Treatment'
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                    caption = caption)
  
  ## return
  plt
}