\name{gomms}
\alias{gomms}
\title{
GLM-Based Ordination Method for Microbiome Samples
}
\description{
estimate factor loadings and scores.
}
\usage{
gomms(X, n.factors = 2, min.prop.nonzeros = 0.05, show.max.delta = FALSE)
}
\arguments{
  \item{X}{raw count data.}
  \item{n.factors}{number of factors. Default value is 2.}
  \item{min.prop.nonzeros}{minimum proportion of nonzeros allowed in analysis.}
  \item{show.max.delta}{display the maximum different between jth and (j+1)th iterations.}
}
\value{estimated factor scores.}
\references{
Sohn, M.B. and Li, H. (2017). A GLM-Based Latent Variable Ordination Method for Microbiome Samples (Submitted).
}
\author{
Michael B. Sohn

Maintainer: Michael B. Sohn <msohn@mail.med.upenn.edu>
}