\name{pairwise.nemenyi.test}
\alias{pairwise.nemenyi.test}

\title{
Calculate the Nemenyi post-hoc test to find the groups of data that differ after a statistical test of multiple comparisons, e.g. the Friedman test.
}
\description{Performs pairwise comparisons of multiple entities over multiple domains and returns the corresponding p-values. The studentized range (q)-distribution is used from the Tukey-test package.}
\usage{
pairwise.nemenyi.test(matrix)
}
\arguments{
  \item{matrix}{
Matrix with performance measures for multiple entities (columns) over multiple domains (rows)
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

