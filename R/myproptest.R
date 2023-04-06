#' Proportion Test for Univariate Normality
#'
#' @param x a vector of univariate data
#' @importFrom stats sd
#'
#' @return proportion of data, expected proportion, and confidence intervals for 1 and 2 std deviations, and a histogram
#' @export
#'
#' @examples myproptest(psych$Indep)
myproptest <- function(x){
  #conditionals to check input
  if (length(x)==1 | is.vector(x)==FALSE | is.numeric(x)==FALSE){stop("x must be a vector of numerical values")}

  #calculating helpful quantities
  mu <- mean(x)
  s <- sd(x)
  n <- length(x)

  #calculating phats
  onesd <- NULL
  twosd <- NULL
  for (i in 1:n){
    if (mu - s < x[i] & x[i] < mu + s){onesd[i]=1} else {onesd[i]=0}
    if (mu - 2*s < x[i] & x[i] < mu + 2*s){twosd[i]=1} else {twosd[i]=0}
  }
  phat1 <- sum(onesd)/n
  phat2 <- sum(twosd)/n

  #calculating confidence intervals for reference
  comp1 <- 1.396/sqrt(n)
  comp2 <- 0.628/sqrt(n)
  ci1sd <- 0.683 + c(-1,1)*comp1
  ci2sd <- 0.954 + c(-1,1)*comp2

  #stating result
  abs1 <- abs(phat1-0.683)
  abs2 <- abs(phat2-0.954)
  if (abs1 > comp1 | abs2 > comp2){ result <- "not consistent with normality"}else {result <- "consistent with normality"}

  #creating graphic
  df <- data.frame(x)
  g <- ggplot2::ggplot(df, ggplot2::aes(x)) + ggplot2::geom_histogram(color="lightblue", bins=10)
  g <- g + ggplot2::geom_vline(xintercept=c(mu-s, mu+s), color="red")
  g <- g + ggplot2::geom_vline(xintercept=c(mu-2*s,mu+2*s), color="blue")
  g <- g + ggplot2::geom_text(label=paste0("phat1=",round(phat1,digits=2)), x=mu, y=n/10, color="red", size=8)
  g <- g + ggplot2::geom_text(label=paste0("phat2=",round(phat2,digits=2)), x=mu, y=n/20, color="blue", size=8)
  g <- g + ggplot2::ggtitle("Percentage of Data in 1 (red) and 2 (blue) Standard Deviations")

  invisible(list(phat1 = phat1, expprop1=0.683, ci1sd=ci1sd, phat2 = phat2, expprop2=0.954, ci2sd=ci2sd, result=result, plot=g))
}
