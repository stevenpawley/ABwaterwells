% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tblAWWID.R
\name{tblAwwid}
\alias{tblAwwid}
\title{AWWID table class}
\value{
a `tblAwwid` R6 class
}
\description{
AWWID table class

AWWID table class
}
\keyword{internal}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{name}}{name of the table}

\item{\code{data}}{a data.table of the table data}

\item{\code{request}}{the URL request that was made to obtain the table}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-tblAwwid-new}{\code{tblAwwid$new()}}
\item \href{#method-tblAwwid-print}{\code{tblAwwid$print()}}
\item \href{#method-tblAwwid-metricate}{\code{tblAwwid$metricate()}}
\item \href{#method-tblAwwid-clone}{\code{tblAwwid$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tblAwwid-new"></a>}}
\if{latex}{\out{\hypertarget{method-tblAwwid-new}{}}}
\subsection{Method \code{new()}}{
Create a new 'tblAwwid' object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tblAwwid$new(x, name, request)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{a data.table of the table data}

\item{\code{name}}{name of the table}

\item{\code{request}}{the URL string that was used to request the table}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tblAwwid-print"></a>}}
\if{latex}{\out{\hypertarget{method-tblAwwid-print}{}}}
\subsection{Method \code{print()}}{
Print method for the 'tblAwwid' class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tblAwwid$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tblAwwid-metricate"></a>}}
\if{latex}{\out{\hypertarget{method-tblAwwid-metricate}{}}}
\subsection{Method \code{metricate()}}{
Standardize a tblAwwid object by converting all measurement columns
from imperial units to metric equivalents, and renaming some column
names to avoid name collisions when joining tables
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tblAwwid$metricate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tblAwwid-clone"></a>}}
\if{latex}{\out{\hypertarget{method-tblAwwid-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{tblAwwid$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
