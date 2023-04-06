#' Steps for Detecting Outliers
#'
#' @param x matrix of data
#' @importFrom graphics pairs
#'
#' @return marginal dotplots, bivariate scatterplots, coordinates of outliers
#' @export
#'
#' @examples mydetectout(psych[,1:5])
mydetectout <- function(x){
  #checking input
  if (is.data.frame(x)==FALSE | ncol(x) < 2){stop("x must be a matrix that has at least 2 variables")}
  for (i in 1:ncol(x)){if (is.numeric(x[,i])==FALSE){stop("x must contain only numerical values")}}

  #calculating helpful quantities
  n <- nrow(x)
  p <- ncol(x)
  xbar <- matrix(colMeans(x),nrow=p,ncol=1)
  Sinv <- solve(cov(x))

  #create a dot plot of each variable
  df <- data.frame(x)
  value <- NULL
  ggplot2::ggplot(tidyr::gather(df),ggplot2::aes(value)) +  # Plot the values
    ggplot2::facet_wrap(~ key, scales = "free") +   # In separate panels
    ggplot2::geom_dotplot(binwidth=1) -> dotplots


  #make a scatter plot for each pair of variables
  scatters <- pairs(x)

  #calculate standardized values for each column
  z <- matrix(NA, nrow=n,ncol=p)
  for (j in 1:n){
    for (k in 1:p){
      z[j,k] <- (x[j,k] - mean(x[,k]))/sd(x[,k])
    }
  }
  zoutliers <- which(z < -3 | z > 3, arr.ind=TRUE)

  #calculate generalized sq distances
  gsd <- NULL
  for (j in 1:n){
    gsd[j] <- t(t(x[j,])-xbar) %*% Sinv %*% (t(x[j,]) - xbar)
  }
  gsdoutliers <- which(gsd > qchisq(0.95,p),arr.ind=TRUE)

  invisible(list(dotplots=dotplots, scatters=scatters, zoutliers=zoutliers, gsdoutliers=gsdoutliers))
}
