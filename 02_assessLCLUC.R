## assess cerrado wetlands
## dhemerson.costa@ipam.org.br

## read libraries
library(sf)
library(ggplot2)
library(reshape2)
library(ggpmisc)
library(tidyverse)
library(ggpubr)
library(tibble)
library(dplyr)
library(quantreg)
library(ggsflabel)
library(ggrepel)
library(treemapify)
library(cartogram)

## set options
options(scipen= 999)

## read ecoregion shapefile
vec <- read_sf('./vector/ecoregions.shp')

## read LCLUC data
lcluc <- read.csv('./table/mapbiomas_collection70_integration_v2_ecoregion.csv')
lcluc <- lcluc[, !names(lcluc) %in% c('system.index', '.geo')]    ## drop undesired columns from LCLUC

## read LCLUC dicitonary
dict <- read.csv('./dict/mapbiomas-dict-en2.csv', sep= ';')

## translate LCLUC classes
data <- as.data.frame(NULL)
## for each class id
for (i in 1:length(unique(lcluc$class_id))) {
  ## for each unique value
  y <- subset(dict, id == unique(lcluc$class_id)[i])
  ## select matched class
  z <- subset(lcluc, class_id == unique(lcluc$class_id)[i])
  ## apply LCLUC translation
  z$level_1 <- gsub(paste0('^',y$id,'$'), y$level_1, z$class_id)
  z$level_1.2 <- gsub(paste0('^',y$id,'$'), y$level_1.2, z$class_id)
  z$level_2 <- gsub(paste0('^',y$id,'$'), y$level_2, z$class_id)
  z$level_3 <- gsub(paste0('^',y$id,'$'), y$level_3, z$class_id)
  z$level_4 <- gsub(paste0('^',y$id,'$'), y$level_4, z$class_id)
  z$level_wet <- gsub(paste0('^',y$id,'$'), y$level_wet, z$class_id)
  
  ## bind into recipe
  data <- rbind(data, z)
  rm(y, z)
}
rm(lcluc, dict, i)

## build territory dict
dict <- as.data.frame(cbind(
  id= vec$ID,
  ecoregion= vec$NAME)
  )

## translate 
data2 <- as.data.frame(NULL)
## for each ecoregion id
for (i in 1:length(unique(data$territory))) {
  ## for each unique value
  y <- subset(dict, id == unique(data$territory)[i])
  ## select matched class
  z <- subset(data, territory == unique(data$territory)[i])
  ## apply tenure translation for each level
  z$ecoregion <- gsub(paste0('^',y$id,'$'), paste0(y$id, '. ', y$ecoregion), z$territory)
  ## bind into recipe
  data2 <- rbind(data2, z)
  rm(y, z)
}

data <- data2
rm(data2, dict, i)

## get only 2021 year
cy <- subset(data, year == 2021)
## aggregate data
cy <- aggregate(x=list(area= cy$area), by=list(class= cy$level_2), FUN= 'sum')
## quanto de área úmida resta no cerrado?
subset(cy, class== 'Wetland')
## isso representa quanto de todo o território?
subset(cy, class== 'Wetland')$area / sum(cy$area) * 100
rm(cy)

## get lcluc by ecoregion
cy <- subset(aggregate(x=list(area= data$area), by=list(class= data$level_2, year= data$year, region= data$ecoregion), FUN= 'sum'),
             year == 2021)

## compute the wetland relative area ofr each ecoregion
data2 <- as.data.frame(NULL)
for (i in 1:length(unique(cy$region))) {
  ## get region 
  x <- subset(cy, region== unique(cy$region)[i])
  ## insert relative wetland
  x$rel_wet <- subset(x, class == 'Wetland')$area / sum(x$area) * 100
  ## bind
  data2 <- rbind(data2, subset(x, class== 'Wetland'))
  rm(x)
}
rm(cy)

## cast metrics (columns) as lines
data3 <- as.data.frame(rbind(cbind(region= data2$region, value= data2$area/1e3, metric= 'Area (Mha)'), 
                             cbind(region= data2$region, value= data2$rel_wet, metric= 'Relative Area (%)')))

## get only aboslute area
z <- subset(data3, metric == 'Area (Mha)')

## compute percnt i relation the total
z$perc <- round(as.numeric(z$value) / sum(as.numeric(z$value)) * 100, digits=1)

## plot "onde estao as wetlands do cerrado?" - grafico 
ggplot(z, mapping=aes(x= reorder(region, as.numeric(value)), y= as.numeric(value), fill= as.numeric(value))) +
  geom_bar(stat= 'identity', col= 'black') +
  scale_fill_fermenter('Area (Kha)', breaks=c(0, 20, 50, 200, 500, 1000, 3000), palette = 'BrBG', direction= 1) +
  geom_text(mapping=aes(label= paste0(round(as.numeric(value), digits=0),' Kha - ', perc, '%')),
            hjust=-0.1, size=4) +
  coord_flip(clip= 'off', ylim=c(0, 4000)) + 
  theme_classic() +
  theme(text = element_text(size = 14)) +
  xlab(NULL) +
  ylab('Área (Kha)')

## join data with shapefile
z$ID <- as.numeric(sapply(strsplit(z$region, split='.', fixed=TRUE), function(x) (x[1]))) ## parse ID from region names
vec <- left_join(vec, z, by= 'ID')

## get ID labels to plot on the map
points <- as.data.frame(st_coordinates(st_centroid(vec)))
points$ID <- vec$ID

## plot maps
ggplot() +
  geom_sf(data= vec, mapping= aes(fill= as.numeric(value)), col= 'gray50') +
  geom_text_repel(data = points, aes(X, Y, label = ID), size = 6, col='black') +
  scale_fill_fermenter('Area (Kha)', breaks=c(0, 20, 50, 200, 500, 1000, 3000), palette = 'BrBG', direction= 1) +
  theme_void() +
  xlab(NULL) +
  ylab(NULL)

## build cartogram
vec$value <- as.numeric(vec$value)
# construct a cartogram 
wet_cart <- cartogram(st_transform(vec, 3857), "value", itermax=30)

# plot cartogram
ggplot() +
  geom_sf(data= wet_cart, mapping= aes(fill= as.numeric(value)), col= 'gray50') +
  #geom_text_repel(data = points, aes(X, Y, label = ID), size = 6, col='black') +
  scale_fill_fermenter('Area (Kha)', breaks=c(0, 20, 50, 200, 500, 1000, 3000), palette = 'BrBG', direction= 1) +
  theme_void() +
  xlab(NULL) +
  ylab(NULL)

## get land tenure
tenure <- read.csv('./table/fundiario_wet_ecoregion.csv')
tenure <- tenure[, !names(tenure) %in% c('system.index', '.geo')]    ## drop undesired columns from LCLUC

## build territory dict
dict <- as.data.frame(cbind(
  id= vec$ID,
  ecoregion= vec$NAME)
)

## translate ecoregion
data2 <- as.data.frame(NULL)
## for each ecoregion id
for (i in 1:length(unique(tenure$territory))) {
  ## for each unique value
  y <- subset(dict, id == unique(tenure$territory)[i])
  ## select matched class
  z <- subset(tenure, territory == unique(tenure$territory)[i])
  ## apply tenure translation for each level
  z$ecoregion <- gsub(paste0('^',y$id,'$'), paste0(y$id, '. ', y$ecoregion), z$territory)
  ## bind into recipe
  data2 <- rbind(data2, z)
  rm(y, z)
}

tenure <- data2
rm(data2, dict, i)

## translate land tenure
dict <- read.csv('./dict/tenure-dict-en.csv', sep=';')

## create recipe to translate each land tenure
recipe <- as.data.frame(NULL)
## for each tenure id
for (j in 1:length(unique(tenure$class_id))) {
  ## for each unique value, get mean in n levels
  y <- subset(dict, Value == unique(tenure$class_id)[j])
  ## select matched land tenure 
  z <- subset(tenure, class_id == unique(tenure$class_id)[j])
  ## apply tenure translation for each level
  z$tenure <- gsub(paste0('^',y$Value,'$'), y$tenure.l1, z$class_id)
  ## bind into recipe
  recipe <- rbind(recipe, z)
}
rm(y,z,j)
tenure <- recipe; rm(recipe)

## get only 2021 tenure data
tenureY <- subset(tenure, year == 2021)

## aggregate
tenureYY <- aggregate(x=list(area= tenureY$area), by= list(tenure= tenureY$tenure), FUN= 'sum')
## compute proportions
tenureYY$perc <- round(tenureYY$area / sum(tenureYY$area) * 100, digits=1)

## build label
tenureYY$lab <- paste0(tenureYY$tenure,'\n', round(tenureYY$area/1e6, digits=1),' Mha - ', tenureYY$perc,'%')

## plot
ggplot(tenureYY, aes(area = area, fill = tenure, label = lab)) +
  geom_treemap(alpha=0.5, col='black') +
  geom_treemap_text(size=15, grow= FALSE, place= 'center') 

## get relative area per tenure
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(tenureY$tenure))) {
  ## get tenure x
  x <- subset(tenureY, tenure== unique(tenureY$tenure)[i])
  ## aggregate
  x <- aggregate(x=list(area= x$area), by=list(ecoregion= x$ecoregion, tenure= x$tenure), FUN= 'sum')
  ## compute percents
  x$perc <- round(x$area / sum(x$area) * 100, digits=1)
  ## insert zeros (regions without this tenure)
  ## pegar etiquetas onde o fundiario nao ocorre
  a  <- unique(tenureY$ecoregion) [- which(unique(tenureY$ecoregion) %in% unique(x$ecoregion)) ]
  ## organizar dataframe
  b  <- rep(unique(x$tenure), length(a))
  c <- rep(0, length(a))
  d <- rep(0, length(a))
  ## merge
  f <- as.data.frame(cbind(
    ecoregion= a,
    tenure= b,
    area= c,
    perc= d
  ))
  ## bind
  x <- rbind(x, f)
  ## bind
  recipe <- rbind(recipe, x)
  rm(x, a, b, c, d, f)
  }

## read ecoregion shapefile
vec <- read_sf('./vector/ecoregions.shp')

## parse ID 
recipe$ID <- as.numeric(sapply(strsplit(recipe$ecoregion, split='.', fixed=TRUE), function(x) (x[1]))) ## parse ID from region names
vec <- left_join(vec, recipe, by= 'ID')

## plot
x11()
ggplot(data= vec) +
  geom_sf(data= vec, mapping= aes(fill= as.numeric(perc)), col= 'gray70', size=0.5) +
  geom_text_repel(data = points, aes(X, Y, label = ID), size = 3, col='black') +
  scale_fill_fermenter('Relative Area (%)', breaks=c(0, 5, 10, 20, 40, 80), palette = 'YlOrRd', direction= 1) +
  facet_wrap(~tenure) + 
  theme_void() +
  theme(text = element_text(size = 14)) +
  xlab(NULL) +
  ylab(NULL)


####################
## stability analysis
## read trajectories
traj <- read.csv('./table/col7_trajectories_wetland_cerrado.csv')
traj <- traj[, !names(traj) %in% c('system.index', '.geo')]    ## drop undesired columns from LCLUC

## translate trajectory
traj$class_id <- gsub('^1$', 'Pr-Ab Ch=1',
                      gsub('^2$', 'Ab-Pr Ch=1',
                           gsub('^3$', 'Pr-Ab Ch>2',
                                gsub('^4$', 'Ab-Pr Ch>2',
                                     gsub('^5$', 'Ab-Ab or Pr-Pr Ch>1',
                                          gsub('^6$', 'Pr-Pr Ch=0',
                                               gsub('^7$', 'Ab-Ab Ch=0',
                                                    traj$class_id)))))))

## read ecoregion shapefile
vec <- read_sf('./vector/ecoregions.shp')

## build territory dict
dict <- as.data.frame(cbind(
  id= vec$ID,
  ecoregion= vec$NAME)
)

## translate 
data2 <- as.data.frame(NULL)
## for each ecoregion id
for (i in 1:length(unique(traj$territory))) {
  ## for each unique value
  y <- subset(dict, id == unique(traj$territory)[i])
  ## select matched class
  z <- subset(traj, territory == unique(traj$territory)[i])
  ## apply tenure translation for each level
  z$ecoregion <- gsub(paste0('^',y$id,'$'), paste0(y$id, '. ', y$ecoregion), z$territory)
  ## bind into recipe
  data2 <- rbind(data2, z)
  rm(y, z)
}

## aggregate
x <- aggregate(x=list(area= traj$area), by=list(traj= traj$class_id), FUN= 'sum')
#3 remove Ab-Ab traj
x <- subset(x, traj != 'Ab-Ab Ch=0')

## compute percents
x$perc <- round(x$area/sum(x$area)*100, digits=1)

## plot trajectories
ggplot(data= x, mapping= aes(area= area, fill= traj)) +
  geom_treemap() +
  geom_treemap_text(mapping= aes(label= paste0(traj, '\n', 
                                               round(area/1e6, digits=2), ' Mha - ', perc, '%'))) +
  scale_fill_manual(values=c("#ffff00", "#020e7a", "#14a5e3", "#941004", "#f5261b", "#666666")) +
  theme(legend.position="none") +
  xlab('Trajectories (1985 - 2021)')


## get nPresence
np <- read.csv('./table/col7_nYearPresence_wetland_cerrado.csv')
np <- np[, !names(np) %in% c('system.index', '.geo')]    ## drop undesired columns from LCLUC
## aggregate
np <- aggregate(x=list(area=np$area), by=list(freq=np$class_id), FUN='sum')

## plot freq
ggplot(data= subset(np, freq != 37), mapping=aes(x=freq, y= area/1000)) +
  geom_line(stat= 'identity', size=1, col= 'red', alpha=0.6) +
  geom_point(alpha=0.7, pch=7, size=2) +
  theme_classic() +
  xlab('n. years of presence') +
  ylab('Area (Kha)')

##  calc perc
np$perc <- np$area/sum(np$area)*100

## insert labels
data2$label <- 
  gsub('Pr-Pr Ch=0', 'Stable',
     gsub('Pr-Ab Ch=1', 'Loss',
          gsub('Pr-Ab Ch>2', 'Loss',
               gsub('Ab-Pr Ch=1', 'Gain',
                    gsub('Ab-Pr Ch>2', 'Gain',
                         gsub('Ab-Ab or Pr-Pr Ch>1', 'All Alternation',
                              data2$class_id))))))

## remove absence
net <- subset(data2, label != 'Ab-Ab Ch=0')

## aggregate
net_ag <- aggregate(x=list(area= net$area), by=list(ecoregion= net$ecoregion, label=net$label), FUN= 'sum')

## compute net (gain - loss)
recipe <- as.data.frame(NULL)
for (i in 1:length(unique(net_ag$ecoregion))) {
  x <- subset(net_ag, ecoregion == unique(net_ag$ecoregion)[i])
  ## compute percents
  x$perc <-round(x$area/sum(x$area)*100, digits=1)
  
  ## compute net balance
  y <- as.data.frame(
          cbind(
            ecoregion= unique(x$ecoregion),
            label= 'Net Balance',
            area= subset(x, label == 'Gain')$area - subset(x, label == 'Loss')$area
          )
  )
  
  ## compute perc of net balance
  y$perc <- round(as.numeric(y$area)/sum(x$area) * 100, digits=1)
  
  ## bind
  x <- rbind(x, y)
  recipe <- rbind(recipe, x)
  rm(x, y)
}

## read ecoregion shapefile
vec <- read_sf('./vector/ecoregions.shp')

## parse ID 
recipe$ID <- as.numeric(sapply(strsplit(recipe$ecoregion, split='.', fixed=TRUE), function(x) (x[1]))) ## parse ID from region names
vec <- left_join(vec, recipe, by= 'ID')


#re-order factor levels for trajectories
vec$label <- factor(vec$label, levels=c('Net Balance', 'Gain', 'Loss', 'All Alternation', 'Stable'))

## Plot Maps
x11()
ggplot() +
  geom_sf(data= vec, mapping= aes(fill= as.numeric(perc)), col= 'gray70', size=0.5) +
  geom_text_repel(data = points, aes(X, Y, label = ID), size = 3, col='black') +
  scale_fill_fermenter('Relative Area (%)', breaks=c(-10, 0, 5, 10, 20, 40, 80), palette = 'Spectral', direction= 1) +
  facet_wrap(~label, nrow=1) + 
  theme_void() +
  theme(text = element_text(size = 14)) +
  xlab(NULL) +
  ylab(NULL)

## plot graphs
ggplot(data= vec, mapping= aes(x= reorder(ecoregion, -perc), y= perc, fill= perc)) +
  geom_bar(stat='identity', col='black') +
  scale_fill_fermenter('Relative Area (%)', breaks=c(-10, 0, 5, 10, 20, 40, 80), palette = 'Spectral', direction= 1) +
  geom_text(mapping=aes(label= paste0(perc, '%', ' | ', round(as.numeric(area)/1000), ' Kha')), hjust="inward") +
  facet_wrap(~label, nrow=1) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = 14),
        legend.position = "none") +
  xlab(NULL) +
  ylab('Relative Area (%)')


names(vec)



## cast table into columns
data_cast <- dcast(data, year + ecoregion ~ level_wet, value.var='area', sum)

## plot analysis
ggplot(data= data_cast, mapping=aes(x= Water/1e3 , y= Wetland/1e3)) +
  geom_point(alpha=0.5) +
  stat_correlation(method= 'spearman', col= 'blue', size= 5) + 
  facet_wrap(~ecoregion, scales= 'free') + 
  theme_bw() +
  xlab('Water (Kha)') + ylab ('Wetland (Kha)')

## compute spearmans correlation per ecoregion
for (i in 1:length(unique(data_cast$ecoregion))) {
  ## subset ecoregion
  x <- subset(data_cast, ecoregion == unique(data_cast$ecoregion)[i])
  ## compute correlation
  as.data.frame(cbind(ecoregion= unique(data_cast$ecoregion)[i], 
                      cor= cor(x= x$Water, y= x$Wetland, method= 'spearman')))
  
}



## get only 2021 data
#last <- subset(data, year== 2021)

## subset (get only water and wetlands)
#data2 <- subset(data, level_wet!= 'Others')
#ggplot(data= data2, mapping= aes(x= year, y=area/1e6, col= level_wet)) +
#  stat_summary(geom='line', fun= 'sum', size= 1.5) +
#  scale_colour_manual('Class', values=c('#0000ff', '#45c2a5')) + 
#  facet_wrap(~ecoregion, scales= 'free_y') +
#  theme_bw() +
#  xlab('Year') + ylab('Area (Mha)')

#ggplot() +
#  geom_sf(data= vec, fill= 'white', col= 'black') 
