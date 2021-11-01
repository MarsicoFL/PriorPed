library(ggplot2)
library(patchwork)

PLOTHINTS = list(
  "3/4-siblings" = list(order = 1:7, spouse = rbind(c(3,5,0), c(5,4,0))),
  "3/4-siblings + child" = list(order = 1:8, spouse = rbind(c(3,5,0), c(5,4,0)))
)

plotped = function(ped, ids, id.missing, col, title, pedname = NULL) {
  ids <- unlist(ids) %>% unique()
  colvec = case_when(labels(ped) %in% ids ~ col,
                     labels(ped) == id.missing ~ 2,
                     TRUE ~ 1)
  hints = PLOTHINTS[[pedname]]
  suppressWarnings(
    plot(ped, hatched = c(ids, id.missing), col = colvec, title = title, cex.main = 1.5, cex = 1.25, 
         margin = c(.5, .5, 3.5, .5), hints = hints)
  )
}

parsePlotError = function(e) {
  msg = conditionMessage(e)
  if(grepl("reduce cex", msg))
    msg = "Pedigree is too big for plotting.\n(You may still perform simulations.)"
  msg
}



