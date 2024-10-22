    alpha <- 0.9 # Percentile 
    eps <- 1e-4 # Tolleranza
    a <- -4     
    b <- 4     

    f <- function(x) {
      pnorm(x) - alpha # Cerco x per cui pnorm(x) = alpha
    }

    # Algoritmo di bisezione
    bisection <- function(a, b, f, eps) {
      repeat {
        c <- (a + b) / 2 # Punto medio
        if (abs(f(c)) < eps) {
          break 
        }
        if (f(c) * f(a) < 0) {
          b <- c 
        } else {
          a <- c 
        }
      }
      return(c) 
    }


    percentile <- bisection(a, b, f, eps)
    percentile

    ## [1] 1.28125

    qnorm(0.9)

    ## [1] 1.281552

Lo rifaccio con epsilon macchina

    alpha <- 0.9 # Percentile 
    eps <- .Machine$double.eps # Tolleranza
    a <- -4     
    b <- 4     

    f <- function(x) {
      pnorm(x) - alpha # Cerco x per cui pnorm(x) = alpha
    }

    # Algoritmo di bisezione
    bisection <- function(a, b, f, eps) {
      repeat {
        c <- (a + b) / 2 # Punto medio
        if (abs(f(c)) < eps) {
          break 
        }
        if (f(c) * f(a) < 0) {
          b <- c 
        } else {
          a <- c 
        }
      }
      return(c) 
    }


    percentile <- bisection(a, b, f, eps)
    percentile

    ## [1] 1.281552
