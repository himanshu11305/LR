#' Performs linear regression.
#'
#' This is truly a great and much-needed function
#'
#' @param Y vector of outcomes.
#' @param covData list of subjects (data corresponding to covariates)
#' @return Coefficients \code{coeff} and p-values \code{pval}.
#' @export
#' @examples
#' myLinearRegression(y,covData)


myLinearRegression <- function(Y , covData){

  if(ncol(covData) < 6 ){ # one column to account for Y
    p <- GGally::ggpairs(covData,
                    columns = 2:ncol(covData))
    print(p)

  }else
    print("Too many variables to plot")

  LR <- lm(Y ~ . , data = covData)
  LS <- summary(LR)

  pval <- LS$coefficients[ ,4]
  pcoef <- LR$coefficients

  return(list ("coefficients" = cbind(pcoef), "p-values" = cbind(pval)))
}
