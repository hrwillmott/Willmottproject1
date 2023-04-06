#' My rQ Function
#'
#' @param x a vector of univariate data
#' @importFrom stats cor
#'
#' @return a Q-Q plot and the value of rQ
#' @export
#'
#' @examples myrq(psych$Supp)
myrq <- function(x){
  #conditionals to check input
  if (length(x)==1 | is.vector(x)==FALSE|is.numeric(x)==FALSE){stop("x must be a vector")}

  #calculating helpful quantities
  ordx <- sort(x) #ordering data in ascending order
  n <- length(x)
  problevels <- ((1:n)-0.5)/n #calculating prob levels
  quantiles <- qnorm(problevels,0,1) #calculating quantiles
  #calculating rQ
  rq <- cor(x=quantiles,y=ordx)

  #creating plot
  df <- data.frame(ordx,quantiles)
  g <- ggplot2::ggplot(data=df, mapping=ggplot2::aes(x=quantiles,y=ordx))
  g <- g + ggplot2::geom_point(color="darkblue") + ggplot2::labs(x="Theoretical Quantiles", y = "Sample Quantiles", title="My Q-Q Plot")
  g <- g + ggplot2::geom_smooth(method="lm", color="darkred")
  g <- g + ggplot2::geom_text(label=paste0("rQ=",round(rq,digits=3)), x=1, y=mean(ordx), color="darkred", size=6)


  #list output
  invisible(list(rq=rq, plot=g, ordx=ordx, quantiles=quantiles))
}
