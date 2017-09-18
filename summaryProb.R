summaryProb <- function(m) {
    
    # marginal density
    # sum the probability of one outcome without regard for the other
    mdX <- apply(m, 1, sum)
    mdY <- apply(m, 2, sum)
    
    # expected value
    # initiating expected value of X
    eX <- 0
    # for loop summing the expected probability of X
    for(i in 1:dim(m)[1]) {
        eX <- mdX[i]*x[i] + eX
    }
    
    eY <- 0
    for(i in 1:dim(m)[2]) {
        eY <- mdY[i]*x[i] + eY
    }
    
    # variance
    # initiating variance of X
    vX <- 0
    # for loop summing variance of X
    for(i in 1:dim(m)[1]) {
        vX <- mdX[i] * (x[i] - eX)^2 + vX
    }
    
    vY <- 0
    for(i in 1:dim(m)[2]) {
        vY <- mdY[i] * (y[i] - eY)^2 + vY
    }
    
    # standard deviation
    sdX <- sqrt(vX)
    
    sdY <- sqrt(vY)
    
    # writing to a data frame
    x <- data.frame("Expected Value" = c(eX, eY),
                    "Variance" = c(vX, vY),
                    "Standard Deviation" = c(sdX, sdY))
    rownames(x) <- c("X", "Y")
    return(x)
}

# example
x <- c(0, 1)
y <- c(0, 1)
pf <- matrix(c(.20, .05, .15, .60),
             nrow = 2,
             ncol = 2,
             byrow = T,
             dimnames = list(c("Fail", "Pass"),
                             c("Don't Study", "Study")))
