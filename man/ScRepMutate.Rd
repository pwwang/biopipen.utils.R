% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sctcr-marcos.R
\name{ScRepMutate}
\alias{ScRepMutate}
\title{Mutate scRepertorie object}
\usage{
ScRepMutate(screp, mutaters)
}
\arguments{
\item{screp}{The scRepertorie object. It is either a Seurat object or a list of data.frames}

\item{mutaters}{A list of expressions (in characters) to mutate the data}
}
\value{
The mutated scRepertorie object
}
\description{
Mutate scRepertorie object
}
\examples{
\donttest{
data(contig_list, package = "scRepertoire")
screp <- scRepertoire::combineTCR(contig_list)
head(ScRepMutate(screp, list(CTaa_len = "nchar(CTaa)"))[[1]])
}
}
