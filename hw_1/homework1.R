# Final submission code

main <- function()
{

    set.seed(0)
}

# Function for reading the data matrix and fitting a linear regressor for the depression level
linreg <- function(){

    # Load table
    d <- read.table("JobsNoMissCont.tab", header = TRUE)

    # Cast to matrix
    m <- as.matrix(d)

    # Get the number of training samples for our regression model
    number_of_train_samples <- dim(m)[1]

    input_variables <- m[, 1:11]

    regressed_variable <- m[,12]

    # Create ones column that is necessary for X matrix
    ones <- as.matrix(rep(c(1), times=number_of_train_samples))

    # Add the column with ones to create the x wave matrix from the slides
    X_wave <- cbind(ones, input_variables)

    # Solve regression problem using clsoed form equation
    A <- solve(t(X_wave) %*% X_wave) %*% t(X_wave) %*% regressed_variable
    
    return(A)
}

# Code for constructing confidence intervals around parameter of interest.

df_make <- function(mat){
    
    input_frame <- as.data.frame(mat)

    column_number <- dim(input_frame)[2]
    
    # Put the names of columns
    colnames(input_frame) <- paste0(c(rep("x", column_number)), 1:column_number)
    
    return(input_frame)
}


df_resample <- function(df){
    
    train_data_size = dim(df)[1]
    
    resample_index <- sample(1:train_data_size, replace = TRUE)
    
    new_sample <- res_frame[resample_index,]
    
    return(new_sample)
}

bootstrap_ci <- function(df, k, f, q)
{
    
    # Get the number of training samples
    number_of_train_samples = dim(df)[1]
    
    # Estimated statistics of the orignal data
    estimated_statistics <- f(df)
    
    # Array for storing the statistics
    samples_statistics <- c()
    
    for (i in 1:k)
    {
        
        # Get new index by sampling with replacement
        new_index <- sample(1:number_of_train_samples, replace = TRUE)
        new_sample <- df[new_index,]
        
        new_statistics <- estimated_statistics - f(new_sample)

        samples_statistics <- c(samples_statistics,  new_statistics)
    }
    
    lower_quantile <- q
    
    upper_quantile <- 1 - q
    
    quantiles <- quantile(samples_statistics, c(lower_quantile, upper_quantile))
    
    return(c(estimated_statistics, estimated_statistics + quantiles[1], estimated_statistics + quantiles[2]))

}

library(MASS)

mean_x_1_func <- function(df){ mean(df$x1) }
mean_x_2_func <- function(df){ mean(df$x2) }
median_x_3_func <- function(df){ median(df$x3) }
cov_x_4_x_5_func <- function(df){ cov(df$x4, df$x5) }

confidence_ci <- function()
{
    mu <- c(1, 2, 3, 0, 0)

    sigma <- rbind(c(1, 1, 1, 1, 1), c(1, 3, 1, 1, 1), c(1, 1, 5, 1, 1), c(1, 1, 1, 2, 0), c(1, 1, 1, 0, 2))

    gaussian_samples <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)

    gaussian_samples_formatted <- df_make(gaussian_samples)
    
    print(bootstrap_ci(gaussian_samples_formatted, 100, mean_x_1_func, 0.025))
    print(bootstrap_ci(gaussian_samples_formatted, 100, mean_x_2_func, 0.025))
    print(bootstrap_ci(gaussian_samples_formatted, 100, median_x_3_func, 0.025))
    print(bootstrap_ci(gaussian_samples_formatted, 100, cov_x_4_x_5_func, 0.025))
    
}

main()
