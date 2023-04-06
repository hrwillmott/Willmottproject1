#' ChiSquare Plot Test for MVN
#'
#' @param x a matrix of data (at least 2 variables)
#'
#' @return a chisq plot and proportion of squared distances within qchisq(0.5,p)
#' @export
#'
#' @examples mychisqplot(psych[,1:5])
mychisqplot <- function(x){
  #checking input
  if (is.data.frame(x)==FALSE | ncol(x) < 2){stop("x must be a data frame that has at least 2 variables")}
  for (i in 1:ncol(x)){if (is.numeric(x[,i])==FALSE){stop("x must contain only numerical values")}}

  #calculating helpful quantities
  n <- nrow(x)
  p <- ncol(x)
  Sinv <- solve(cov(x))
  xbar <- matrix(colMeans(x),nrow=p,ncol=1)

  #create sorted list of djsq
  djsq <- NULL
  for (j in 1:n){djsq[j] <- t(t(x[j,])-xbar) %*% Sinv %*% (t(x[j,]) - xbar)}
  djsq <- sort(djsq)

  #create quantiles
  problevels <- ((1:n)-0.5)/n
  quantiles <- qchisq(problevels,p)

  #create chisq plot
  df <- data.frame(quantiles,djsq)
  g <- ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=quantiles,y=djsq))
  g <- g + ggplot2::geom_point(color="blue") + ggplot2::labs(x="Theoretical Chi-Sq Quantiles", y = "Squared Distances", title="My Chi-Square Plot")
  g <- g + ggplot2::geom_abline(data=df, ggplot2::aes(slope=1,intercept=0), color="darkred", linewidth=1 )

  #evaluating normality
  propdj <- length(djsq[djsq <= qchisq(0.5,p)])/n

  #list output
  invisible(list(plot=g, propdj=propdj, expprop=0.5, djsq=djsq, quantiles=quantiles))
}
