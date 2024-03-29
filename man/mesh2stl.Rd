% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh2stl.R
\name{mesh2stl}
\alias{mesh2stl}
\title{Converts a 3d object with x, y, z coordinates to a stl file.}
\usage{
mesh2stl(x, y, z, plot = TRUE, file.path = NULL, file.name = NULL,
  object.name = "object")
}
\arguments{
\item{x}{a matrix}

\item{y}{a matrix}

\item{z}{a matrix}

\item{plot}{should the object be plotted as well}

\item{file.path}{where should the file be saved}

\item{file.name}{name for file, should end with ".stl"}

\item{object.name}{what is the name of the 3d object}
}
\value{
an stl file
}
\description{
Converts a 3d object with x, y, z coordinates to a stl file.
}
\examples{
a = 0.94; d = 0.56; c = 0.34; b = sqrt(a^2 - c^2)
uv <- mesh(x=seq(0,2*pi,length.out = 30),y=seq(0,2*pi,length.out = 30))
x <- (d*(c - a*cos(uv$x)*cos(uv$y)) + b^2*cos(uv$x)) / (a - c*cos(uv$x)*cos(uv$y))
y <- (b*sin(uv$x)*(a - d*cos(uv$y))) / (a - c*cos(uv$x)*cos(uv$y))
z <- (b*sin(uv$y)*(c*cos(uv$x) - d)) / (a - c*cos(uv$x)*cos(uv$y))
mesh2stl(x, y, z)

}
