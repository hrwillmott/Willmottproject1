#' Result 4.7 Bivariate Normality Test
#'
#' @param x1 a vector of data (variable 1)
#' @param x2 a vector of data (variable 2)
#' @importFrom stats cov qchisq
#'
#' @return expected proportion, actual proportion, and bivariate scatter plot
#' @export
#'
#' @examples myresult47(psych$Indep,psych$Conform)
myresult47 <- function(x1, x2){
  #conditionals to check input
  if (length(x1)==1 | is.vector(x1)==FALSE |is.numeric(x1)==FALSE){stop("x1 must be a vector")}
  if (length(x2)==1 | is.vector(x2)==FALSE |is.numeric(x2)==FALSE){stop("x2 must be a vector")}
  if (length(x1) != length(x2)){stop("x1 and x2 must have the same length")}

  #setting up helpful values
  x <- matrix(data=c(x1,x2), nrow=length(x1), ncol=2,byrow=FALSE)
  xbar <- matrix(colMeans(x),nrow=2,ncol=1)
  Sinv <- solve(cov(x))

  #calculating proportion inside chisq(.5) ellipse
  inellipse <- NULL
  for (i in 1:length(x1)){
    point <- matrix(data=x[i,],nrow=2,ncol=1)
    if (t(point-xbar) %*% Sinv %*% (point-xbar) <= qchisq(0.5,2)) {inellipse[i] = 1}
    if (t(point-xbar) %*% Sinv %*% (point-xbar) > qchisq(0.5,2)) {inellipse[i] = 0}
  }
  actprop <- sum(inellipse)/length(x1)

  #creating graphic
  df <- data.frame(x1,x2)
  g <- ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=x1,y=x2))
  g <- g + ggplot2::geom_point(color="blue",alpha=0.5,size=2.5) + ggplot2::labs(x="x1 variable", y = "x2 variable", title="Bivariate Data")
  g <- g + ggplot2::stat_ellipse(level=0.5, color="red",linewidth=1) #adding in ellipse
  g <- g + ggplot2::geom_text(label=paste0(round(actprop*100,digits=1),"%"), x=mean(x1), y=mean(x2), color="red", size=6)

  #list output
  list(expected_prop=0.5, actual_prop = actprop, plot = g)
}
