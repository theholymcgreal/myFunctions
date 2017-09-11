mySum <- function(x) {
    # Computes the sum of a vector
    #
    # Args:
    #   x: vector whose sum is to be calculated
    #       
    # Returns: 
    #   sum of vector x
    
    # initialize mySum with a value
    mySum <- 0
    
    # for loop to sum the vector
    for (i in x) {
            mySum <- mySum + i
    }
    return(round(mySum, 2))
}

myMean <- function(x, includeOutliers = T) {
    # Computes the mean of a vector with added argument includeOutliers
    #
    # Args:
    #   x: vector whose mean is to be calculated
    #   includeOutliers: logical parameter for outliers
    #
    # Returns: 
    #   mean of vector x
    
    # compute mean with outliers
    if (includeOutliers == T) {
        
                myMean <- mySum(x) / length(x)
                
    }
    
    # compute mean without outliers
    # outlier is > 2 SD away from mean
    else {
        
        # compute original mean & SD
        origMean <- mySum(x) / length(x)
        origSD <- mySD(x)
        
        # logical vector which determines outliers
        outlier <- (x > origMean + 2 * origSD) | 
                   (x < origMean - 2 * origSD)
        
        # new vector for computing mean of vector
        xNoOut <- x[outlier == F]
        
        # compute mean
        myMean <- mySum(xNoOut) / length(xNoOut)
    }
    
    return(round(myMean, 2))
}

myMedian <- function(x) {
    # Computes the median of a vector
    #
    # Args: 
    #   x: vector whose median is calculated
    #
    # Returns: median of vector x
    
    # sort the vector 
    xSort <- sort(x, decreasing = F)
    # compute length of the vector
    lengthSort <- length(xSort)
    
    # finding middle number in a vector
    # even vector length, find average of middle two numbers
    if (lengthSort %% 2 == 0) {  
        x1 <- xSort[lengthSort / 2]
        x2 <- xSort[(lengthSort / 2) + 1]
        myMedian <- myMean(x1:x2)
    }
    # odd vector length, find middle number
    else {
        myMedian <- xSort[ceiling(lengthSort / 2)]
    }
    
    return(round(myMedian, 2))
}

myMode <- function(x) {
    # Computes the number occuring most often in a vector
    #
    # Args:
    #   x: vector whose mode is calculated
    #
    # Returns:
    #   mode of vector x
    myMode <- as.numeric(names(which(table(x) == max(table(x)))))
    return(round(myMode, 2))
}

myMax <- function(x) {
    # computes maximum number in a vector
    #
    # Args:
    #   x: vector
    #
    # Returns:
    #   max value of vector x
    myMax <- -1/0.000000000000000000000000001
    for (i in x) {
        if (i <= myMax) {
            next
        }
        else {
            myMax <- i
        }
    }
    return(round(myMax, 2))
}

myMin <- function(x) {
    # computes minimum number in a vector
    #
    # Args:
    #   x: vector
    #
    # Returns:
    #   min value of vector x
    myMin <- 1/0.000000000000000000000000001
    for (i in x) {
        if (i >= myMin) {
            next
        }
        else {
            myMin <- i
        }
    }
    return(round(myMin, 2))
}

myRange <- function(x) {
    # computes range of a vector
    #
    # Args: 
    #   x: vector for computing range
    #
    # Returns:
    #   range of vector x
    myRange <- myMax(x) - myMin(x)
    return(round(myRange, 2))
}

myVariance <- function(x) {
    # computes variance of a vector
    #
    # Args:
    #   x: vector for computing variance
    #
    # Returns:
    #   variance of vector x
    myMean <- myMean(x)
    numerator <- 0
    for (i in x) {
        numerator <- (i - myMean)^2 + numerator
    }
    myVariance <- numerator / (length(x) - 1)
    return(round(myVariance, 2))
}

mySD <- function(x) {
    # compute standard deviation of a vector
    #
    # Args: 
    #   x: vector for computing SD
    #
    # Returns:
    #   SD of vector x
    mySD <- sqrt(myVariance(x))
    return(round(mySD, 2))
}

mySummaryStat <- function(x, includeOutliers = T) {
    # compute a summary statistic dataframe for a vector
    # 
    # Args:
    #   x: vector for computing summary stat
    #   includeOutliers: logical parameter for outliers
    #
    # Returns:
    #   data frame of summary statistics
    if (includeOutliers == T) {
    
    summary <- data.frame(Min = myMin(x), Max = myMax(x), Range = myRange(x),
                          Sum = mySum(x), Mean = myMean(x), Median = myMedian(x), 
                          Mode = myMode(x), Variance = myVariance(x), SD = mySD(x))
    }
    
    else {
        
        origMean <- myMean(x)
        origSD <- mySD(x)
        outlier <- (x > origMean + 2 * origSD) | 
                   (x < origMean - 2 * origSD)
        x <- x[outlier == F]
        
        summary <- data.frame(Min = myMin(x), Max = myMax(x), Range = myRange(x),
                              Sum = mySum(x), Mean = myMean(x), Median = myMedian(x), 
                              Mode = myMode(x), Variance = myVariance(x), SD = mySD(x))
    }
    return(summary)
}
