\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ euclidean
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(digit1, digit2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{digit1}{
%%     ~~Describe \code{digit1} here~~
}
  \item{digit2}{
%%     ~~Describe \code{digit2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (digit1, digit2) 
{
    stopifnot(is.numeric(digit1), is.numeric(digit2), length(digit1) == 
        1, length(digit2) == 1)
    if (digit1 == 0) {
        return(digit2)
    }
    else {
        return(euclidean(digit2\%\%digit1, digit1))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
