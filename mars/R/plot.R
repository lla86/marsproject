#' Plot mars
#'
#' @param description plots the fitted basis function
#' @param OBJECT a mars object
#'

#'
#' @examples mm <- mars(y~x1+x2,data=marstestdata,mars.control(Mmax=4))
#' @author  Ajay Behal Lei Liu Linden Gueck
plot.mars<-function(OBJECT,...)
{

  NEWDATA<-eval(OBJECT$call$data)


  tt<-terms(OBJECT$formula,data=NEWDATA)
  tt<-delete.response(tt)
  mf<-model.frame(tt,NEWDATA)
  mt<-attr(mf,"terms")
  X<-model.matrix(mt,mf)[,-1]
  Bf<-x$Bfuncs

  margin_1<-which(sapply(Bf,function(x) NROW(x)==1))
  margin_2<-which(sapply(Bf,function(x) NROW(x)==2))

  nn<-ceiling(sqrt(length(margin_1)+length(margin_2)))


  opar<-graphics::par(mfrow=c(nn,nn),mar=c(2,2,2,2))


  on.exit(graphics::par(opar))#

  for(i in margin_1)
  {
    vv<-Bf[[i]][1,"v"];variable_name<-x$x_names[[vv]]


    x_coord<-seq(from=min(X[,vv]),to=max(X[,vv]),length=100)
    y_coord<-h(x,Bf[[i]][1,"s"],Bf[[i]][1,"t"])
    plot(x_coord,y_coord,type="l",xlab=variable_name,main=variable_name,...)
  }
}
