% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Constraints.R
\name{get_constraints}
\alias{get_constraints}
\title{Get Constraints}
\usage{
get_constraints(cobj, type = NULL, id = NULL)
}
\arguments{
\item{cobj}{constraints object}

\item{type}{type of constraint. valid types are symbol, cardinality, group
and performance}

\item{id}{id of constraint. Equivalent to location in constraints list}
}
\value{
list of constraints
}
\description{
Getter function to return constraints object contraints. Subsetable by type
or id
}
