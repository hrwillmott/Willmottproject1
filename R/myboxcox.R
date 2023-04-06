#' Box Cox Transformation
#'
#' @param dat a vector of data
#' @param lambda vector of possible values for lambda
#' @param iter number of iterations
#' @param alpha confidence level
#' @importFrom stats quantile
#'
#' @return best lambda + confidence interval, plot of log likelihood
#' @export
#'
#' @examples myboxcox(psych$Conform)
myboxcox <- function(dat, lambda = seq(-2,2,1/10), iter=1000, alpha=0.05){
  #conditionals to check input
  if (length(dat)==1 | is.numeric(dat)==FALSE | is.vector(dat)==FALSE){stop("dat must be a vector")}
  if (length(lambda)==1 | is.numeric(lambda)==FALSE | is.vector(lambda)==FALSE){stop("lambda must be a vector")}
  if (length(iter) != 1 | iter!=floor(iter) | is.numeric(iter)==FALSE | iter < 0){stop("iter must be a positive integer")}
  if (iter < 100){message("You may want to use more iterations for better results")}
  if (length(alpha)!=1 | 0 >= alpha | alpha >= 1){stop("alpha must be a real number between 0 and 1")}
  if (alpha > 0.10){message("alpha is your significance level, so it should be small")}

  #creating log lik function where x=lambda value
  n <- length(dat)
  samp <- dat
  x <- NULL
  loglik <- function(x){
    n <- length(samp)
    if (x != 0) {xjlam <- (samp^x - 1)/x}
    if (x == 0) {xjlam <- log(samp)}
    (-n/2)*log((1/n)*sum((xjlam - mean(xjlam))^2)) + (x-1)*sum(log(samp))
  }

  #finding best lambda
  lams <- NULL
  values <- NULL
  for (i in 1:iter){
    samp <- sample(dat,n,replace=TRUE)
    for (j in 1:length(lambda)){values[j] <- loglik(x=lambda[j])}
    ind <- which.max(values)
    lams[i] <- lambda[ind]
  }
  bestlam <- mean(lams)
  cilam <- quantile(lams,c(alpha/2,1-alpha/2))
  logvalue <- loglik(bestlam)

  #plotting function
  samp <- dat
  df <- data.frame(x=lambda)
  g <- ggplot2::ggplot(df, ggplot2::aes(x)) + ggplot2::stat_function(fun=Vectorize(loglik),size=1.5)
  g <- g + ggplot2::geom_vline(xintercept=bestlam, color="darkgreen",size=1.5)
  g <- g + ggplot2::geom_vline(xintercept=cilam[1], color="darkred",size=1.5) + ggplot2::geom_vline(xintercept=cilam[2], color="darkred",size=1.5)
  g <- g + ggplot2::ggtitle(paste0(100*(1-alpha),"% Confidence Interval for Lambda")) + ggplot2::xlab("lambda") + ggplot2::ylab("Log Likelihood Function")
  g <- g + ggplot2::geom_text(label=round(bestlam,2), x=bestlam, y=1.25*logvalue, color="darkgreen", size=6) + ggplot2::geom_text(label=round(cilam[1],2), x=cilam[1]-.25, y=1.5*logvalue, color="darkred", size=6) + ggplot2::geom_text(label=round(cilam[2],2), x=cilam[2]+.25, y=1.5*logvalue, color="darkred", size=6)
  #list output
  list(plot=g, bestlam=bestlam, cilam=cilam,logvalue=logvalue, lams=lams, values=values)
}
