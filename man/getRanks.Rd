\name{getRanks}
\alias{getRanks}
\title{
Calculate Friedman-style ranks for a matrix in which columns represent different groups with their respective samples.
}
\description{
For each row, the samples of individual groups are ranked from 1 to n. In case of a tie, the mean of all ranks with the same value is used. E.g. if there is no rank 2 assigned yet and there are 3 groups with the same sample value, they will be assigned the rank (2+3+4)/3=3.
}
\usage{
getRanks(rawVector, ndigits)
}
\arguments{
  \item{rawVector}{
Matrix with columns representing the groups to be compared and rows the individual samples.
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

