\name{getRanks}
\alias{getRanks}
\title{
Calculate Friedman-style ranks for a matrix in which multiple entities are represented by columns and the domains they were evaluated on by rows.
}
\description{
For each domain, entities are ranked according to their performance from highest to lowest value. In case of a tie, the mean of all ranks with the same value is used.
}
\usage{
getRanks(rawVector, ndigits)
}
\arguments{
  \item{rawVector}{
Matrix with columns representing the entities to be compared and rows the domains they were evaluated on.
}
  \item{ndigits}{
Identifying rank ties for floats is facilitated by rounding the ranks to n digits.
}
}
\value{
Returns a matrix of the same shape with former values replaced by Friedman-style ranks.
}
\references{
Japkowicz/Shah (2011), Evaluating Learning Algorithms. Pages 255 ff.
}
\author{
Christian Guckelsberger
}
\section{Warning}{Please ensure that the input matrix has the correct format!}

