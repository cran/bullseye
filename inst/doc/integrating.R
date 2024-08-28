## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message=FALSE, warning=FALSE,
  fig.width=4, fig.height=4, fig.align = "center"
)



## ----setup, message=FALSE-----------------------------------------------------
# install.packages("palmerpenguins")

library(bullseye)
library(dplyr)
library(ggplot2)
peng <-
  rename(palmerpenguins::penguins, 
           bill_length=bill_length_mm,
           bill_depth=bill_depth_mm,
           flipper_length=flipper_length_mm,
           body_mass=body_mass_g)

## ----echo=FALSE---------------------------------------------------------------
library(ggiraph)
set_girafe_defaults(opts_sizing= opts_sizing(rescale=FALSE, width=.5))

## ----corrplot_assoc-----------------------------------------------------------
sc <- pairwise_scores(peng) # includes factors, unlike `cor`
  
corrplot::corrplot(as.matrix(sc), diag=FALSE)
# corrplot::corrplot(as.matrix(sc, default=1)) # to show 1 along the diagoonal


## ----fig.width=7, fig.height=7------------------------------------------------
linkspotter::linkspotterGraphOnMatrix(as.data.frame(as.matrix(sc)),minCor=0.7)

## -----------------------------------------------------------------------------
# install.packages("correlation")
library(correlation)
sc_cor <- correlation(peng, method = "distance")
plot(as.pairwise(sc_cor))

## -----------------------------------------------------------------------------
sc_multi<- bind_rows(
  as.pairwise(correlation(peng, method = "pearson")),
  as.pairwise(correlation(peng, method = "biweight")))
plot(sc_multi)

## -----------------------------------------------------------------------------
pm <- pairwise_multi(peng)
tidyr::pivot_wider(pm, names_from=score, values_from = value) |>
  ggplot(aes(x=nmi, y=ace))+ geom_point()


