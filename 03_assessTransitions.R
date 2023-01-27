## assess cerrado wetlands
## dhemerson.costa@ipam.org.br

library(ggplot2)

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

## insert paired trajectories into dataset
data$traj <- paste0(data$from_lab, ' -> ', data$to_lab)

## aggregate trajectories
x <- aggregate(x= list(area= data$area),
               by=list(
                 from= data$from_lab,
                 to= data$to_lab,
                 year= data$year, 
                 traj= data$traj), 
               FUN= 'sum')


## retain only changes
x <- subset(x, from != to)
x <- subset(x, area > 50)
x <- subset(x, from != 'Ignore' & to != 'Ignore')

## retain only wetlands changes
x_loss <- subset(x, from == 'Wetland')
x_gain <- subset(x, to == 'Wetland')

## get class involved in the transition
x_loss$class <- x_loss$to
x_gain$class <- x_gain$from

## insert label
x_loss$label <- 'Loss To'
x_gain$label <- 'Gain From'

## assign negative values to loss
x_loss$area <- x_loss$area * -1

## bind
xi <- rbind(x_loss, x_gain); rm(x_loss, x_gain)

## remove first and last years
xij <- subset(xi, year > 1985 & year <2021)

## reorder factors
xij$class <- factor(xij$class, levels=c('Other Non Vegetated Areas', 'Agriculture', 'Pasture', 'Mosaic of Uses',
                                        'Forest Plantation', 'Forest Formation', 'Other Non Forest Formations', 'Water'))






ggplot(data= xij, mapping= aes(x= year, y= area/1000, fill= class)) +
  geom_bar(stat='identity', position= 'stack', alpha=0.9) +
  scale_fill_manual(
    'Class', values=c('#af2a2a', '#e974ed', '#ffd966', '#fff3bf', '#935132', '#006400', '#bdb76b', '#0000ff')
    ) + 
  geom_hline(yintercept = 0, size=1, col= 'red') +
  theme_classic()  +
  xlab(NULL) +
  ylab('Area (Kha)')

# compute net-balance per class
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(xij$class)))  {
  ## get class
  a <- subset(xij, class == unique(xij$class)[i])
  ## get loss
  loss <- subset(a, label == 'Loss To')
  ## convert areas to positive
  loss$area <- loss$area * -1
  ## get gains
  gain <- subset(a, label == 'Gain From')
  ## get diff
  gain$diff <- gain$area - loss$area
  ## get final balance
  gain$balance <- sum(gain$diff)
  ## bind
  recipe <- rbind(recipe, gain)
  rm(a, loss, gain)
}

## plot
names(recipe)

ggplot(data= recipe, mapping= aes(x= year, y= diff/1000)) +
  geom_line(mapping= aes(col= class), size=1.5) +
  scale_colour_manual(
    'Class', values=c('#af2a2a', '#e974ed', '#ffd966', '#fff3bf', '#935132', '#006400', '#bdb76b', '#0000ff')
  ) + 
  facet_wrap(~class, nrow=1) +
  geom_hline(yintercept = 0, size=0.5, col= 'red', linetype= 'dashed') +
  theme_bw() +
  xlab(NULL) +
  ylab('Area (Kha)')
  
