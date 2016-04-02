
# PREDICT 454 Programming Challenges
# ----------------------------------


# Week 1 - Change Machine
#------------------------

# Build the function
change.machine <- function(x)
{
    # Validate that the number is between 0 and 100
    if (x < 0 | x > 100)
    {
        print("The input value must be between 0 and 100.")
        
    }    else
    
    {
    
    # Calculate the number of quarter, dimes, nickles, and pennies
    # quart <- x/.25
    # dime <- x/.10
    # nickle <- x/.05
    # penny <- x/.01
    
    
    # create list of the change
    # change <- list(quart, dime, nickle, penny)
    
    
    # Calculate the number of quarter, dimes, nickles, and pennies
    
    quart <- as.integer(x/25)
    remainder <- x%%25
    
    dime <- as.integer(remainder/10)
    remainder <- remainder%%10
    
    nickle <- as.integer(remainder/5)
    
    penny <- remainder%%5
    

    # create list of the change
    change <- list(quart, dime, nickle, penny)
    
    
    
    return(change)
    }
}


