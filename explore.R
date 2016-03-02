data(diamonds)
data(mtcars)
require("ggplot2")
require("grid")
diamonds$vs <- rep((mtcars$vs==1)[3:22],2697) #replicate vs into diamonds

hist <- function(df,bins) #function for #1. density and count hists
{ var_num <- df[which(lapply(df, is.numeric) == TRUE)]
  for (i in 1:ncol(var_num)){ #loop through each column
    for (n in bins){          #loop through each binsize
      width <- (max(var_num[[i]])-min(var_num[[i]]))/n
      col_mean <- mean(var_num[[i]])
      hist_plot <- ggplot(var_num,aes(x=var_num[[i]]), environment=environment())
      hist_plot <- hist_plot + geom_histogram(colour = 'blue', fill = 'blue', binwidth = width )
      hist_plot <- hist_plot + labs(x=names(var_num)[[i]]) + geom_vline(xintercept=col_mean, colour='red')
      hist_plot2 <- hist_plot + aes(y=..density..) + labs(y="density")
      print(hist_plot)  #regular plot
      print(hist_plot2)}} #density plot
}


bar <- function(df) #function for #2. make bar graphs for factor and logical columns
{ var_fac <- df[which(lapply(df, is.factor) == TRUE)]
  var_logic <-df[which(lapply(df, is.logical) == TRUE)]
  if (ncol(var_fac)>=1) {
    for (i in 1:ncol(var_fac)) {  #loop through each factor
    bar_fac <- ggplot(df, aes(x=var_fac[[i]]), environment=environment())
    bar_fac <- bar_fac + geom_bar(fill="gray", colour="gray") +
      labs(x=names(var_fac[[i]]))
    print(bar_fac)} 
  }
  
  if (ncol(var_logic)>=1) {
      for (i in 1:ncol(var_logic)) {  #loop through each logical
        bar_logic <- ggplot(df, aes(x=var_logic[[i]]), environment=environment())
        bar_logic <- bar_logic + geom_bar(fill="gray", colour="gray") +
        labs(x=names(var_logic[[i]]))
        print(bar_logic)} 
    }
}

# function for 4b.ii creates dataframe with column name pairings and corresponding r-squared value
rsquared <- function(df) {
  var_num <- df[which(lapply(df, is.numeric) == TRUE)]
  rs <- NULL    #create empty vectors to place data into
  name <- NULL
  for (i in 1:(ncol(var_num)-1)){
    for (j in (i+1):ncol(var_num)) {  #loop through each pair
      rs <- c(rs,summary(lm(var_num[[i]]~var_num[[j]], df))$r.squared)  #calculate the r-squared
      name <- c(name, paste(names(var_num[i]),names(var_num[j]),sep="-"))}  #create the hyphenated name
  }
  data <- data.frame(name,rs)   #return dataframe with the name and rs list
  return(data)
}

#Function for #4a.Takes dataframe as input and outputs a frequency table for all factor and logical variables
freq_table <- function(df) 
{   var_fac <- df[which(lapply(df, is.factor) == TRUE)]
    var_logic <-df[which(lapply(df, is.logical) == TRUE)]
    vectortable <- vector(mode="list", length=(ncol(var_fac)+ncol(var_logic)))  #create vector to place tables in
  
  if (ncol(var_fac)>=1) {  
    for (i in 1:ncol(var_fac)) {
      vectortable[[i]] <- table(list(var_fac[[i]])) #place each new vector in the list
    }}
  if (ncol(var_logic)>=1) {
    for (j in 1:ncol(var_logic)) {
      vectortable[[ncol(var_fac)+j]] <- table(list(var_logic[[j]]))  #place the logic vectors at the end of the list
    }}
    
    return(vectortable)
    
}

#return summary statistics for all numerical variables
sumstat <- function(df)
{ var_num <- df[which(lapply(df, is.numeric) == TRUE)]
  statistics <- vector(mode="list", length=ncol(var_num)) #create list to place each summary statistic
  for (i in 1:ncol(var_num)) {
    statistics[[i]] <- summary(var_num[[i]])  #place sum stats for variables in the list
    
  }
  return(statistics)
}


#create numerical vector pairs and their pearson coefficients above a certain threshold
pearsons <- function(df,thresh)
{ name <- NULL
coeff <- NULL
var_num <- df[which(lapply(df, is.numeric) == TRUE)]

for (n in 1:(ncol(var_num)-1)){ #add the hyphenated pairings to a vector
  for (m in (n+1):ncol(var_num)) {
    if (abs(cor(var_num[[n]],var_num[[m]],method="pearson"))>thresh) {
      name <- c(name, paste(names(var_num[n]),names(var_num[m]),sep="-"))}}}

for (n in 1:(ncol(var_num)-1)) { #create a vector with the pearson coeffs 
  for (m in (n+1):ncol(var_num)) {
    if (abs(cor(var_num[[n]],var_num[[m]],method="pearson"))>thresh) {
      coeff <- c(coeff, cor(var_num[[n]],var_num[[m]],method="pearson"))}}}


data2 <- data.frame(name,coeff) #create a dataframe with the names and coeffs
return(data2)
}

explore <- function(df,bins,thresh)
#the big function for this homework
#takes as input a dataset, list of bin sizes, and correlation threshold 
#it will print histograms and bar graphs.
#it will return list of frequency tables, summary statistics, r^2 values, and correlations
{ hist(df,bins) #prints regular and density histograms for numerical variables
  bar(df)       #creates bargraphs for logical and factor variables
  freqtab <- freq_table(df) #creates frequency table for factor and logical variables
  stats <-sumstat(df)       #creates summary statistics for numerical variables
  rsdata <-rsquared(df)     #creates list of r^2 values for pairs of numerical variables
  corrdata <- pearsons(df,thresh) #lists pearson correlations for numerical values above threshold
  biglist <- list(freqtab, stats, rsdata, corrdata) #returns list of outputs from last 4 functions
  return(biglist)
}
explore(diamonds,c(5,20,50),.25)
explore(mtcars,c(5,20,50),.25)

