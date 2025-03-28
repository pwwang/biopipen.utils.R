% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/casing.R
\name{case_info}
\alias{case_info}
\title{Create information for a casename}
\usage{
case_info(name, outdir, is_dir = TRUE, create = FALSE)
}
\arguments{
\item{name}{A casename
Sections are separated by \code{::} in the casename
When section is specified, the case will be created in the section directory}

\item{outdir}{The output directory, where the case directory will be created or prefix will be under}

\item{is_dir}{Whether to create a directory for the case
Otherwise, a prefix will be returned.}

\item{create}{Create the directory if not exists when \code{is_dir} is TRUE, otherwise, create the parent directory
of the prefix.}
}
\value{
A list of information, including:
\itemize{
\item \code{name}: The name of the case, without the sections
\item \code{section}: The sections of the case as a vector
\item \code{slug}: The slugified name
\item \code{section_slug}: The slugified sections
\item \code{prefix}: The prefix of the case, either the directory or the file path prefix
}
}
\description{
Create information for a casename
}
