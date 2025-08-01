% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_edges_from_map.R
\name{get_edges_from_map}
\alias{get_edges_from_map}
\title{Brief: Convert Dependency Map to Edge List}
\usage{
get_edges_from_map(dep_map)
}
\arguments{
\item{dep_map}{A named list of data frames, where each data frame contains the dependencies of a user-defined function.}
}
\value{
A data frame representing the edge list, with columns from and to, suitable for plotting with visNetwork.
}
\description{
Description: Converts a dependency map (as produced by build_dependency_map(), which is a list of data frames
representing function dependencies, into a unified edge list with from and to columns. This format is required
for visualizing the function relationships using visNetwork.
}
\details{
Author: Antonio Fratamico
Date: 10/07/2025
}
