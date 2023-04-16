#' Predict method for MARS
#' @description Predict with an mars model for new data, returns the predicted basis function.
#'
#'
#' @param object An object of class mars
#' @param newdata A data frame to predict with
#'
#'
#' @examples
#' mc = mars.control()
#' mm = mars(y ~ .,data=marstestdata,control=mc)

#' @author Ajay Behal Lei Liu Linden Gueck
predict.mars <- function(object,newdata)
{

  if(missing(newdata) || is.null(newdata)){
    B <- as.matrix(object$B)
  }


  else
  {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf,"terms")
    X <- model.matrix(mt,mf)[,-1]


    B <- make_B_splits(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}


make_B_splits <- function(X, Bfuncs) {
  Bf <- Bfuncs[-1]
  B <- matrix(1, nrow = nrow(X), ncol = length(Bf))
  for(i in 1:length(Bf)) {
    j <- Bf[[i]]
    for(t in 1:nrow(j)) B[,i] <- B[,i] * h(j[t,1],X[,j[t,2]], j[t,3])
  }
  return (cbind(1,B))
}
