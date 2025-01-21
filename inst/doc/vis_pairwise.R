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

## -----------------------------------------------------------------------------
plot(pair_cor(peng))

## -----------------------------------------------------------------------------
filter(pair_methods,nn&ff&fn)

## -----------------------------------------------------------------------------
plot(pairwise_scores(peng), interactive=TRUE)

## -----------------------------------------------------------------------------
scores <- pairwise_scores(peng, by="species")
plot(scores, interactive=TRUE) 

## -----------------------------------------------------------------------------
plot(scores, var_order="seriate_max_diff", interactive=TRUE) 

## -----------------------------------------------------------------------------
mscores <- 
bind_rows(
  pair_cor(peng),
  pair_cor(peng, method="spearman"),
  pair_dcor(peng),
  pair_ace(peng)
) |> filter(pair_type=="nn") |>
  mutate(value=abs(value)) # convert all scores to 0-1

plot(mscores, interactive=TRUE) 

## -----------------------------------------------------------------------------
sc <- pair_scagnostics(peng)
plot(sc, interactive=TRUE)

## ----fig.width=5--------------------------------------------------------------
plot(sc, type="linear")

## -----------------------------------------------------------------------------
sc |> filter(y != "year")|>
plot(type="linear", geom="point", add_lines=TRUE)

## ----echo=FALSE---------------------------------------------------------------
description <- c("Annual income",
                 "Employment status with categories not in labor force, unemployed, employed",
                 "Hours worked per week",
                 "Race of the participant with categories white, black, asian or other",
                 "Age of the participant in years",
                 "Gender with categories male or female",
                 "Whether the person is a U.S. citizen",
                 "Travel time to work, in minutes",
                 "Language spoken at home with categories english or other",
                 "Whether the person is married",
                 "Education level with categories hs or lower, college, grad",
                 "Whether the person is disabled",
                 "The quarter of the year that the person was born with categories jan thru mar,
                 apr thru jun , jul thru sep, oct thru dec")

df <- data.frame(Variable=names(openintro::acs12),
                 Description=description
                 )

kableExtra::kbl(df, booktabs = T, caption = "Variable description of the acs12 dataset") |>
  kableExtra::kable_styling(latex_options = "scale_down")

## -----------------------------------------------------------------------------
acs12 <- openintro::acs12

scores <- pairwise_multi(acs12)

## -----------------------------------------------------------------------------
mutate(scores, valmax = max(abs(value)), .by=c(x,y))|>
  filter(valmax > .25) |>
  plot(type="linear",geom="point", interactive=TRUE)



## ----eval=FALSE---------------------------------------------------------------
# a <- ace_cor(acs12$income, acs12$employment)
# plot(a$x, a$tx)

## -----------------------------------------------------------------------------
group_scores <- pairwise_scores(acs12, by = "race")

# filtering variable pairs with a range of 0.25 or greater
rng <- function(vals){
  if (all(is.na(vals))) 0 else max(vals, na.rm=TRUE)- min(vals,na.rm=TRUE)
}

mutate(group_scores, valrange = rng(value),valmax = max(abs(value)), .by=c(x,y))|>
  filter(valrange > .25 | valmax > .4) |>
  plot(type="linear", geom="point", pair_order = "seriate_max_diff")+ 
   theme(legend.text = element_text(size = rel(.5)), legend.title = element_text(size = rel(.5))
  )

## ----fig.width=8--------------------------------------------------------------
ggplot(data=acs12, aes(x=employment, y=hrs_work))+
  geom_boxplot()+
  facet_grid(cols=vars(race)) +scale_x_discrete(na.translate = FALSE)

## ----fig.height=3-------------------------------------------------------------
ggplot(data=acs12, aes(x=gender, y=time_to_work))+
  geom_boxplot()+
  facet_grid(cols=vars(race)) 

## ----fig.height=3-------------------------------------------------------------
ggplot(data=acs12, aes(x=gender, y=income))+
  geom_boxplot()+
  facet_grid(cols=vars(race))

