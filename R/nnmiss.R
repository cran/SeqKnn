"nnmiss" <-
function (x, xmiss, ismiss, K) 
{
    xd <- scale(x, xmiss, FALSE)[, !ismiss]
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    od <- order(dd)[seq(K)]
    
    od<-od[!is.na(od)]
    K<-length(od)
   
    distance<-dd[od]
    s<-sum(1/distance)
    weight<-(1/distance)/s
    xmiss[ismiss] <- drop(weight %*% x[od, ismiss, drop = FALSE]) ## weighted mean
##  xmiss[ismiss] <- drop(rep(1/K, K) %*% x[od, ismiss, drop = FALSE])  ## mean
    xmiss
}

