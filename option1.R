library("tidyr")
library("dplyr")

#merge the two datasets by the first two columns (company code & event date)
merge_data <- function(returndata, pardata){
  returndata$id <- 1:nrow(returndata)
  result <- merge(returndata, pardata, 
                      by.x=c(colnames(returndata)[1],colnames(returndata)[2]),
                      by.y=c(colnames(pardata)[1],colnames(pardata)[2]))
  return(result)
}

filter_data <- function(mergedata, rangedf){
  #findInterval: 0-smaller, 1-within range, 2-greater
  #calculate the product of the value (0,1 or 2), and attach it as a new column in the merged data
  cond <- rowProds(sapply(seq(7,length(colnames(mergedata)),2), function(i) {
    findInterval(mergedata[,i], c(rangedf$From[as.integer((i-5)/2)],
                 rangedf$To[as.integer((i-5)/2)]),
                 rightmost.closed = TRUE)
  }))
  mergedatawithcond <- cbind(mergedata,cond)

  #get the subset of all rows in which the product equals 1 (all values of all characteristics are within range)
  filtered <- mergedatawithcond[cond==1,]
  filtered <- filtered[order(filtered$id),]
  return(filtered)
}

initiate_original_range <- function(pardata){
  df <- data.frame(
    Characteristic = colnames(pardata)[seq(3,length(colnames(pardata)),by=2)],
    From = sapply(seq(3,length(colnames(pardata)),by=2),function(i){
      #min(pardata[,i])
      median(pardata[pardata[,i+1]==1,i])}),
    To = sapply(seq(3,length(colnames(pardata)),by=2),function(i){
      #max(pardata[,i])
      median(pardata[pardata[,i+1]==100,i])}))
  return(df)
}

initiate_selected_range <- function(pardata){
  df <- data.frame(
    Characteristic = colnames(pardata)[seq(3,length(colnames(pardata)),by=2)],
    From = sapply(seq(3,length(colnames(pardata)),by=2),function(i){
      1}),
    To = sapply(seq(3,length(colnames(pardata)),by=2),function(i){
     100}))
  return(df)
}

get_original_range <- function(pardata,selectedrange){
  df <- data.frame(
    Characteristic = selectedrange$Characteristic,
    From = sapply(seq(1,length(selectedrange$Characteristic),by=1),function(i){
      quantile(pardata[,(i*2+2)], selectedrange$From/100)}),
      #pardata[pardata[,(i*2+2)]==rangedf$From,(i*2+1)][1]
    To = sapply(seq(1,length(selectedrange$Characteristic),by=1),function(i){
      quantile(pardata[,(i*2+2)], selectedrange$To/100)}))
      #pardata[pardata[,(i*2+2)]==rangedf$To,(i*2+1)][1]}))
  return(df)
}

calculate_data <- function(filteredmergedata){
  tempreturn <- data.table(companycode=filteredmergedata[,1],
                           eventdate=filteredmergedata[,2],
                           daterelative=filteredmergedata[,3],
                           dailyret=filteredmergedata[,4])
  averagereturn <- aggregate(filteredmergedata[,4], 
                             list(filteredmergedata[,3]), mean)
  #cummeanreturn <- cumprod(averagereturn$x + 1) - 1
  tempreturn <- tempreturn[,list(cumreturn=cumprod(dailyret+1)-1,daterelative,dailyret),
                           by=list(companycode,eventdate)]
  return(tempreturn)
}

get_meancumreturn <- function(tempreturn){
  meancumreturn <- aggregate(tempreturn$cumreturn,list(tempreturn$daterelative), mean)
  return(meancumreturn)
}

get_summary <- function(tempreturn){
  summarydf <- data.frame(measure = c("Number of events","min","p10","p25",
                                       "median","p75","p90","max","sd"), 
                           value = c(sum(tempreturn$daterelative==0),
                                     min(tempreturn$cumreturn),
                                     quantile(tempreturn$cumreturn, c(.10,.25)),
                                     median(tempreturn$cumreturn),
                                     quantile(tempreturn$cumreturn, c(.75,.90)),
                                     max(tempreturn$cumreturn),
                                     sd(tempreturn$cumreturn)))
  return(summarydf)
}

option1 <- function(returndata, pardata, rangedf){
  return(calculate_data(filter_data(merge_data(returndata,pardata),rangedf)))
}