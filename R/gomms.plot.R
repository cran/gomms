gomms.plot <- function(X, Y, col.markers=NULL, pch.markers=NULL, ...){
  if(!is.matrix(Y)) stop("Y must be in matrix format!", call.=FALSE);
  n.col.Y <- ncol(Y);
  default.pchs <- c(21:25);
  if(nrow(X)!=nrow(Y)){
    Y <- Y[-which(!(rownames(Y) %in% rownames(X))),];
  }
  if(n.col.Y==1){
    if(is.null(col.markers)){
      n.col.markers <- length(unique(Y))
      markers.col <- rainbow(n.col.markers)[as.factor(Y)]
    } else{
      markers.col <- col.markers[Y]
    }
    markers.pch <- 21
  } else{
    if(is.null(col.markers)){
      n.col.markers <- length(unique(Y[,1]))
      markers.col <- rainbow(n.col.markers)[as.factor(Y[,1])]
    } else{
      markers.col <- col.markers[as.factor(Y[,1])]
    }
    if(is.null(pch.markers)){
      n.pch.markers <- length(unique(Y[,2]))
      if(n.pch.markers > 5) stop("Please provide values for pch.markers!", call.=FALSE);
      markers.pch <- default.pchs[as.factor(Y[,2])]
    } else{
      markers.pch <- pch.markers[as.factor(Y[,2])]
    }
  }
  rslt.fig <- plot(X, pch=markers.pch, bg=markers.col, ...)
  return(rslt.fig)
}
