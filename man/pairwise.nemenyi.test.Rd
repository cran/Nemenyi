\name{pairwise.nemenyi.test}
\alias{pairwise.nemenyi.test}

\title{
Calculate the Nemenyi post-hoc test to find pairs of samples that differ after the Friedman test.
}
\description{Performs pairwise comparisons of multiple groups and returns the corresponding p-values. The studentized range (q)-distribution is used from the Tukey package.}
\usage{
pairwise.nemenyi.test(matrix,digits)
}
\arguments{
  \item{matrix}{
Matrix with performance measures with columns=groups (columns) and rows=samples per group
}
\item{digits}{
Number of digits to keep when rounding the samples
}
}
\value{
Object of class "pairwise.htest":

\item{method}{
A character string indicating what type of test was performed.
}

\item{parameter}{
Degrees of freedom used in the test
}

\item{data.name}{
A character string giving the name(s) of the data.
}

\item{statistic}{
Object of class "pairwise.table": Table of Nemenyi-values in lower triangular form.
}

\item{p.value}{	
Object of class "pairwise.table": Table of p values in lower triangular form.
}

\item{p.adjust.method}{
Adjustment method used for multiple testing.
}
}
\references{
Japkowicz/Shah (2011), Evaluating Learning Algorithms. Pages 255 ff.
}
\author{
Christian Guckelsberger
}
\section{Warning}{Please ensure that the input matrix has the correct format!}

