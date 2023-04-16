#' summary.mars
#'
#' @description prints a summary of the mars object with the function call.
#' @param object of class mars, which is obtained from calling mars()
#' @param digits the number of significant digits i.e. SIG FIG
#'
#'
#' @examples mm<-mars(y ~.,data=mars::marstestdata)
#' summary(mm)
#' @author Ajay Behal Lei Liu Linden Gueck
summary.mars <- function(object,digits)
{
  for(i in 2:length(object$Bfuncs))
  {
    cat("Coefficient ",names(object$coefficients)[i],":\n")
    for(j in 1:nrow(object$Bfuncs[[i]]))
    {
      cat("covariates:",object$x_names[object$Bfuncs[[i]][j,2]],"split at value t:",object$Bfuncs[[i]][j,3],"\n")
    }
  }
}
