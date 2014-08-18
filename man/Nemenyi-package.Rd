\name{Nemenyi}
\docType{package}
\title{Implementation of the Nemenyi post-hoc test to find the groups of data that differ after a statistical test of multiple comparisons, e.g. Friedman test.}
\description{Use to performs pairwise comparisons of >2 groups. The studentized range (q)-distribution is used from the Tukey package.}
\details{
\tabular{ll}{
Package: \tab Nemenyi\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-08-18\cr
License: \tab GPL-2\cr
}
}
\author{
Christian Guckelsberger <cguckels@students.uni-mainz.de>
}
\references{
Japkowicz/Shah (2011), Evaluating Learning Algorithms. Pages 255 ff.
}
\keyword{package}
\examples{
library(Nemenyi)

#Example from: Japkowicz/Shah (2011), Evaluating Learning Algorithms. Pages 273 ff.
classifierA<-c (85.83 , 85.91 , 86.12 , 85.82 , 86.28 , 86.42 , 85.91 , 86.10 , 85.95 , 86.12) /100
classifierB<-c (75.86 , 73.18 , 69.08 , 74.05 , 74.71 , 65.90 , 76.25 , 75.10 , 70.50 , 73.95) /100
classifierC<-c (84.19 , 85.91 , 83.83 , 85.11 , 86.38 , 81.20 , 86.38 , 86.75 , 88.03 , 87.18) /100
m<-matrix(c(classifierA,classifierB,classifierC),nrow=10, byrow=FALSE)

pairwise.nemenyi.test(m,3)
}
