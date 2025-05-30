% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_scores.R
\name{pairwise_scores}
\alias{pairwise_scores}
\title{Calculates scores or conditional scores for a dataset}
\usage{
pairwise_scores(
  d,
  by = NULL,
  ungrouped = TRUE,
  control = pair_control(),
  handle.na = TRUE,
  warnings = TRUE,
  add.nobs = FALSE
)
}
\arguments{
\item{d}{a dataframe}

\item{by}{a character string for the name of the conditioning variable. Set to \code{NULL} by default.}

\item{ungrouped}{Ignored if \code{by} is \code{NULL}.
If TRUE calculates the ungrouped score in addition to grouped scores.}

\item{control}{a list for the measures to be calculated for different variable types. The default is
\code{\link[=pair_control]{pair_control()}} which calculates Pearson's correlation if the variable pair is numeric,
canonical correlation for factor or mixed pairs, and polychoric correlation for two ordered factors.}

\item{handle.na}{If TRUE uses pairwise complete observations to calculate measure of association.}

\item{warnings}{If TRUE, generates a warning for datasets of one row, one column, or with constant variables.}

\item{add.nobs}{If TRUE, adds an extra column containing the number of observations used for each score.}
}
\value{
A tibble with class \code{pairwise}.
}
\description{
Calculates scores for every variable pair in a dataset when \code{by} is \code{NULL}. If \code{by}
is a name of a variable in the dataset, conditional scores for every
variable pair at different levels of the grouping variable are calculated.
}
\details{
Returns a \code{pairwise} tibble structure.
}
\examples{
irisc <- pairwise_scores(iris)
irisc <- pairwise_scores(iris, control=pair_control(nnargs= c(method="spearman")))
irisc <- pairwise_scores(iris, control=pair_control(fn="pair_ace"))

#Lots of numerical measures
irisc <- pairwise_scores(iris, control=pair_control(nn="pairwise_multi", fn=NULL))
irisc <- pairwise_scores(iris, 
             control=pair_control(nn="pairwise_multi",  nnargs="pair_cor", fn=NULL))
#conditional measures
cond_iris <- pairwise_scores(iris, by = "Species") 
cond_iris_wo <- pairwise_scores(iris, by = "Species",ungrouped=FALSE) # without overall
irisc <- pairwise_scores(iris, control=pair_control(nn="pairwise_multi", fn=NULL))
irisc <- pairwise_scores(iris, by = "Species",control=pair_control(nn="pairwise_multi", fn=NULL))

#scagnostics
sc <- pairwise_scores(iris, control=pair_control(nn="pair_scagnostics", fn=NULL)) # ignore fn pairs
sc <- pairwise_scores(iris, by = "Species",
                  control=pair_control(nn="pair_scagnostics", fn=NULL)) # ignore fn pairs
}
