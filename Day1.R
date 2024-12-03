distance2lists <-
  function (L1,L2) {
    L1 <- sort(L1)
    L2 <- sort(L2)
    distance <- sum(abs(L1-L2))
    return(distance)
  }

L1 <- input[,1]
L2 <- input[,2]

distance2lists(L1,L2)

similarity2Lists <- 
  function(L1,L2) {
    s <- 0
    for (i in 1:(length(L1))) {
      s <- s+L1[i]*sum(L2 == L1[i])
    }
    return(s)
  }

similarity2Lists(L1,L2)
  
