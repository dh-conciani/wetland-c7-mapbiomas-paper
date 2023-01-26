## assess cerrado wetlands
## dhemerson.costa@ipam.org.br

## read tables
data <- read.csv('./table/col7_transitions_wetland_cerrado.csv')
data <- data[, !names(data) %in% c('system.index', '.geo')]    ## drop undesired columns from LCLUC

## translate transitions
data$from <- round(data$class_id/1000)
data$to <- round((data$class_id/1000 - floor(data$class_id/1000)) * 1000)

## read mapbiomas dicitonary
dict <- read.csv('./dict/mapbiomas-dict-en2.csv', sep= ';')

## translate mapbiomas classes
data2 <- as.data.frame(NULL)
## for each [from] class
for (i in 1:length(unique(data$from))) {
  ## for each unique value
  y <- subset(dict, id == unique(data$from)[i])
  ## select matched class
  z <- subset(data, from == unique(data$from)[i])
  ## apply LCLUC translation
  z$from_lab <- gsub(paste0('^',y$id,'$'), y$level_wet_trans, z$from)
  
  ## bind into recipe
  data2 <- rbind(data2, z)
  rm(y, z)
}

data3 <- as.data.frame(NULL)
## translate each [to] class
for (j in 1:length(unique(data2$to))) {
  ## for each unique value
  y <- subset(dict, id == unique(data2$to)[j])
  ## select matched class
  z <- subset(data2, to == unique(data2$to)[j])
  ## apply LCLUC translation
  z$to_lab <- gsub(paste0('^',y$id,'$'), y$level_wet_trans, z$to)
  
  ## bind into recipe
  data3 <- rbind(data3, z)
  rm(y, z)
}
data <- data3
rm(data2, data3, dict, i, j)

## get transitions per time-point
for (k in 1:length(unique(data$year))) {
  ## get year i
  x <- subset(data, year == unique(data$year[k]))
  ## get only changes
  x <- subset(x, from_lab != to_lab)
  ## compute aggregation 
  ##aggregate(x= list(area= x$area), by=list())
  
  
}

k=1

