\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{dijkstra
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
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
function (graph, init_node) 
{
    stopifnot(is.data.frame(graph), length(graph) == 3, names(graph[1]) == 
        "v1", names(graph[2]) == "v2", names(graph[3]) == "w", 
        length(init_node) == 1, is.numeric(init_node))
    newgraph = makegraph(graph)
    visited = vector()
    j = 1
    res = vector()
    to = unlist(graph[1])
    for (i in 1:length(to)) {
        if ((to[i] \%in\% visited) == FALSE) {
            res[j] = suppressMessages(get_distance_pair(newgraph, 
                init_node, to[i], algorithm = "bi"))
            j = j + 1
            visited = append(visited, to[i])
        }
    }
    return(res)
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
