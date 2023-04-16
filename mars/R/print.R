#' Prints out a mars object
#'
#' @description print mars object
#' @return values of the coefficients
#'
#' @details Prints intercept and coefficient of mars object
#'
#' @examples mm <- mars(y~x1+x2,data=marstestdata)
#' print(mm)
#' @author Ajay Behal Lei Liu Linden Gueck

print.mars <- function(marsobject,...)
{

  print(marsobject$call)
  print(coefficients(marsobject))



  model = "Y = B0"
  for(i in 2:length(marsobject$coefficients))
  {
    model = paste(model, names(marsobject$coefficients)[i], sep="+") #
  }

  print("The model is: ", model)

  for(k in 1:length(marsobject$coefficients)){
    cat(marsobject$coefficients[k], "is the coefficient of",names(marsobject$coefficients)[k],"\n" )
  }

}


