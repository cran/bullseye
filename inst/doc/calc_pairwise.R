## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
# install.packages("palmerpenguins")

library(bullseye)
library(palmerpenguins)
library(dplyr)

## ----echo=FALSE---------------------------------------------------------------

kableExtra::kbl(pair_methods,booktabs = T,caption = "List of the functions available in the package for calculating different association measures along with the packages used for calculation.") %>% kableExtra::kable_styling(latex_options = "scale_down")

## -----------------------------------------------------------------------------
sc_dcor <- pair_dcor(penguins)
str(sc_dcor)

## -----------------------------------------------------------------------------
sc_nmi <- pair_nmi(penguins)
sc_nmi

## -----------------------------------------------------------------------------
pairwise_scores(penguins) |> distinct(score, pair_type)

## -----------------------------------------------------------------------------
pair_control()

## -----------------------------------------------------------------------------
bind_rows(sc_dcor, sc_nmi) |> arrange(x,y)

## -----------------------------------------------------------------------------
pairwise_multi(penguins)
dcor_nmi <- pairwise_multi(penguins, c("pair_dcor", "pair_nmi"))

## -----------------------------------------------------------------------------
pairwise_by(penguins, by="species", pair_cor)

## -----------------------------------------------------------------------------
sc_sex <- pairwise_scores(penguins, by="species")

## -----------------------------------------------------------------------------
sc_sex |> distinct(group)

## -----------------------------------------------------------------------------
pc <- pair_control(nn="pairwise_multi", nnargs= c("pair_dcor", "pair_ace"), fn=NULL, ff=NULL)
sc_sex <- pairwise_scores(penguins, by="species", control=pc, ungrouped=FALSE) 

## -----------------------------------------------------------------------------
pair_scagnostics(penguins[,1:5], scagnostic=c("Stringy", "Clumpy"))

## -----------------------------------------------------------------------------
pairwise_by(penguins[,1:5], by="species",function(x) pair_scagnostics(x, scagnostic=c("Stringy", "Clumpy")))


## -----------------------------------------------------------------------------
x <- cor(penguins[, c("bill_length_mm", "bill_depth_mm" ,"flipper_length_mm" ,"body_mass_g")], 
         use= "pairwise.complete.obs")
pairwise(x, score="pearson", pair_type = "nn")

## -----------------------------------------------------------------------------
as.matrix(sc_dcor)

## -----------------------------------------------------------------------------
correlation::correlation(penguins)

## -----------------------------------------------------------------------------
x <- correlation::correlation(penguins)
pairwise(as.matrix(x)) 

