leu<-function(x){if (ncol(x)!=nrow(x)) stop("X Kare Matris Deðil")
  if(!is.numeric(x)) stop("X numerik bir matris deðil")
  n<-nrow(x)
  l<-matrix(0,nrow=n,ncol=n)
  u<-matrix(0,nrow=n,ncol=n)
  diag(l)<-rep(1,n)
  
  u[1,] <- x[1,]
  
  for (i in 2:n) {   
    
    l[i,1] <- x[i,1]/x[1,1]
    
    if ( l[i,1] >=0 ) { u[i,] <- -l[i,1]*x[1,]+x[i,]
    }
    
    else { u[i,] <- l[i,1]*x[1,]+x[i,]
    }

  }
  
  for (k in 3:n) { for (j in 2:n) { if (j<k) {
    
    l[k,j] <- u[k,j]/u[j,j]
    
    if ( u[j,j] == 0 )  
      
      stop("Singular Matris")
    
    if ( l[k,j] >= 0 ) { u[k,] <- -l[k,j]*u[j,]+u[k,]
    }
    
    else { u[k,] <- l[k,j]*u[j,]+u[k,] }
    
    }
   }
  }
  result <- list( L=l, U=u )
  return( result )
}