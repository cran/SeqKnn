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

