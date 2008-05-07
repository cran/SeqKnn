.packageName <- "SeqKnn"
"SeqKNN" <-
function (data, k) 
{
    x <- as.matrix(data)
    N <- dim(x)
    p <- N[2]
    N <- N[1]
    nas <- is.na(drop(x %*% rep(1, p)))
    xcomplete <- x[!nas, ]
    xbad <- x[nas, , drop = FALSE]
    missing<-c()

    for (i in seq(nrow(xbad))) {
        missing[i]<-sum(is.na(xbad[i,]))
    }
    missingorder<-order(missing)

    xnas <- is.na(xbad)
    xbadhat <- xbad
    cat(nrow(xbad), fill = TRUE)
    for (i in seq(nrow(xbad))) {
        j<-order(missingorder[i])
        xinas <- xnas[missingorder[i], ]
        xbadhat[missingorder[i], ] <- nnmiss(xcomplete, xbad[missingorder[i], ], xinas, K = k)
        xcomplete<-rbind(xcomplete, xbadhat[missingorder[i],]) 
    }
    x[nas, ] <- xbadhat
    x
}

"nnmiss" <-
function (x, xmiss, ismiss, K) 
{
    xd <- as.matrix(scale(x, xmiss, FALSE)[, !ismiss])
    dd <- drop(xd^2 %*% rep(1, ncol(xd)))
    od <- order(dd)[seq(K)]
    
    od<-od[!is.na(od)]
    K<-length(od)
   
    distance<-dd[od]
    s<-sum(1/(distance+0.000000000000001))
    weight<-(1/(distance+0.000000000000001))/s
    xmiss[ismiss] <- drop(weight %*% x[od, ismiss, drop = FALSE]) ## weighted mean
##  xmiss[ismiss] <- drop(rep(1/K, K) %*% x[od, ismiss, drop = FALSE])  ## mean
    xmiss
}

