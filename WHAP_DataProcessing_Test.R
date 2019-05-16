# Test script and tools for processing data collected for the WHAP

# Current Protocol: https://docs.google.com/document/d/1IGem9VtdVqSxq3cCjuG4W_RsARtbStm5lJ8XmgZwgiU/edit?usp=sharing
# Data management Plan: https://docs.google.com/presentation/d/1iUz5JzUBPaa2qy-z_KTvLdoHFavO8PeUkzN9LYi1i84/edit?usp=sharing
# Kara Moore-O'leary
# Sarah Byer

########################################################################
# Load packages and import data
Packages <- c("raster", "ggplot2", "reshape2", "foreign", "sp", "rgdal", "plyr", "measurements")
lapply(Packages, library, character.only = TRUE)
setwd("Z:/I_M/Projects/CVKB_Zone_Biology/GIS/Geodatabase")

# vantage points
vgdb <- "Vantage_Pt_Export_20180627.gdb"
subset(ogrDrivers(), grepl("GDB", name))
vgdb_list <- ogrListLayers(vgdb)
vantage_pt <- readOGR(dsn=vgdb, vgdb_list[1])

# quadrats
qgdb <- "Quadrat_Pt_Export_20180627.gdb"
subset(ogrDrivers(), grepl("GDB", name))
qgdb_list <- ogrListLayers(qgdb)
quadrat_pt <- readOGR(dsn=qgdb, qgdb_list[1])
dim(quadrat_pt)

# management units  -- Grab this off the AGOL site too: SLW_HabManagementUnits
setwd("Z:/I_M/Projects/WRIA/Stone Lakes/Geodatabase/SLW_RLGIS_Zone10")
mgdb <- "SLW_RLGIS_Zone10.gdb"
subset(ogrDrivers(), grepl("GDB", name))
mgdb_list <- ogrListLayers(mgdb)
mgmt_unit <- readOGR(dsn=mgdb, mgdb_list[grep('MU_Refuge_System_Land', mgdb_list)])


########################################################################
# Calculate Area of of the focal species for the SPI equation
# Consolidate vantage point data

# SWAMP TIMOTHY
# Average cover classes by unit name:
v.df <- data.frame(vantage_pt) # Convert spatialpointsdataframe
mytbl <- ddply(v.df, .(Unit_Name, Target_Plant1), summarise, 
               Emergent_N    = sum(!is.na(Cover_Em)),
               Emergent_mean = mean(Cover_Em, na.rm=TRUE),
               Emergent_sd   = sd(Cover_Em, na.rm=TRUE),
               Emergent_se   = sd(Cover_Em, na.rm=TRUE) / sqrt(sum(!is.na(Cover_Em))), 
               Plant_N    = sum(!is.na(Target_Cover1)),
               Plant_mean = mean(Target_Cover1, na.rm=TRUE),
               Plant_sd   = sd(Target_Cover1, na.rm=TRUE),
               Plant_se   = sd(Target_Cover1, na.rm=TRUE) / sqrt(sum(!is.na(Target_Cover1))))
mytbl # Average emergent vegetation cover and target plant cover for each management unit

#### NEED REAL ACREAGES FROM ACTUAL MGMT UNITS ####

# Join summary table to management unit table by Unit Name
# use merge() function

# for now, creating fake acre values in mytbl
mytbl$Unit_Acres <- c(5, 10)
mytbl$Acres_Em <- (mytbl$Emergent_mean*0.01)*mytbl$Unit_Acres
mytbl$Acres_Plant1 <- (mytbl$Plant_mean*0.01)*mytbl$Acres_Em
mytbl # Acres of emergent veg and acres of swamp timothy in the management unit

########################################################################
# Manage quadrat data to calculate seed density

## Count of inflorescences per 30cm:
count30.fun <- function(quad_size, inflo_count) {
  count30 <- vector()
  if (quad_size == 30)
    count30 <- inflo_count
  else
    count30 <- inflo_count*4
}
# count30.fun() takes into account the quadrat size - if the quadrat is 30cm2, the recorded inflo count is saved,
# if the quadrat is 15cm, the recorded inflo count is multipled by 4 to get the inflo counts per 30cm2.
inflo_count_30 <- mapply(count30.fun, quad_size=quadrat_pt$Quad_Size, inflo_count=quadrat_pt$Count_Inflo1) # function applied to Count_Inflo1
quadrat_pt$Count_Inflo1_30 <- inflo_count_30 # Add values to quadrat data frame in new column
quadrat_pt@data # View attribute table


## Spatial join quadrats to mgmt units to get mgmt unit name, etc. in quadrat data:
mgmt_unit <- spTransform(mgmt_unit, projection(quadrat_pt)) # same crs
plot(mgmt_unit)
plot(quadrat_pt, add=T, col="blue")

## Calculate average inflorescence density from quadrats in each management unit's focal species area:
# Ideally, only want to do this for those management units that were surveyed.
# Earlier in the script, make a list of the surveyed management units and their names.
unit_inflo_dens <- cbind(mgmt_unit$MU_Name, over(mgmt_unit, quadrat_pt[,35], fn = mean, na.rm=TRUE)) # returns data frame of management unit name and average inflo count in that unit
conv_cm2_acre <- conv_unit(1, from = "acre", to = "cm2")/30 # number of 30cm2 units per acre
unit_inflo_dens$Avg_Inflo_Acre <- unit_inflo_dens$Count_Inflo1_30*conv_cm2_acre # Average number of inflorescences per acre
unit_inflo_dens


# Get test results so far:
x <- unit_inflo_dens[c(2:3),]
x$`mgmt_unit$MU_Name` <- mytbl$Unit_Name
colnames(x) <- c("Unit_Name", "Avg_Inflo_30m", "Avg_Inflo_Acre")
m <- merge(mytbl, x, by = "Unit_Name")
m


 $ plot.title  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 15
  ..$ hjust        : num -1
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(plot.title=element_text(size=15))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 8
 $ plot.title  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 15
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(plot.title=element_text(size=15))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(plot.title=element_text(size=10))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 8
 $ plot.title  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 10
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(plot.title=element_text(size=6))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 8
 $ plot.title  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 6
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(plot.title=element_text(size=4))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 8
 $ plot.title  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 4
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(plot.title=element_text(size=4))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(plot.title=element_text(size=2))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(plot.title=element_text(size=1))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(plot.title=element_text(size=.5))+
+   theme(axis.title.x =element_text(size=30))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(axis.title.x =element_text(size=20))+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=20))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 7
 $ axis.title.x:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=10))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 7
 $ axis.title.x: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 10
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=5))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
List of 7
 $ axis.title.x: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ axis.title.y: list()
  ..- attr(*, "class")= chr [1:2] "element_blank" "element"
 $ legend.title:List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ legend.text :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 30
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.x :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 5
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : num 90
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ axis.text.y :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 $ strip.text  :List of 11
  ..$ family       : NULL
  ..$ face         : NULL
  ..$ colour       : NULL
  ..$ size         : num 20
  ..$ hjust        : NULL
  ..$ vjust        : NULL
  ..$ angle        : NULL
  ..$ lineheight   : NULL
  ..$ margin       : NULL
  ..$ debug        : NULL
  ..$ inherit.blank: logi FALSE
  ..- attr(*, "class")= chr [1:2] "element_text" "element"
 - attr(*, "class")= chr [1:2] "theme" "gg"
 - attr(*, "complete")= logi FALSE
 - attr(*, "validate")= logi TRUE
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=5))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=10))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_blank())+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=15))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_text(size=20))+
+   labs(title="2017 Seed Production Index (SPI) By Unit", y="SPI")+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=15))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
Error: Don't know how to add RHS to a theme object
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_text(size=20))+
+   labs(title="2017 Seed Production Index (SPI) By Unit", y="SPI")+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(axis.text.x = element_text(angle=90, size=15))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
> plot
> breaks=SPI_2017$order,
Error: unexpected ',' in "breaks=SPI_2017$order,"
>     labels=SPI_2017$UnitCell)+
Error: unexpected ')' in "    labels=SPI_2017$UnitCell)"
>   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_text(size=20))+
+   labs(title="2017 Seed Production Index (SPI) By Unit", y="SPI")+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(plot.title=element_text(size=30, hjust=.5))+
+   theme(axis.text.x = element_text(angle=90, size=15))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
Error: Don't know how to add RHS to a theme object
> plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
+   geom_col()+
+   facet_wrap(~LIT, scales='free') +
+   scale_x_continuous(
+     breaks=SPI_2017$order,
+     labels=SPI_2017$UnitCell)+
+   theme(axis.title.x =element_blank())+
+   theme(axis.title.y =element_text(size=20))+
+   labs(title="2017 Seed Production Index (SPI) By Unit", y="SPI")+
+   theme(legend.title =element_text(size=30))+
+   theme(legend.text =element_text(size=30))+
+   theme(plot.title=element_text(size=30, hjust=.5))+
+   theme(axis.text.x = element_text(angle=90, size=15))+
+   theme(axis.text.y =element_text(size=20))+
+   theme(strip.text= element_text(size=20))
Error in ggplot(data = SPI_2017, mapping = aes(x = order, y = SPI, fill = Species)) : 
  object 'SPI_2017' not found
> SPI_2017<-read.csv('WHAPHabitatSurvey_FY17.csv')
> SPI_2017<-SPI_2017 %>% 
+   filter(!


# Graphing 2017 data ------------------------------------------------------

library(ggplot2); library(dplyr)
SPI_2017<-read.csv('WHAPHabitatSurvey_FY17.csv')
SPI_2017<-SPI_2017 %>% 
  filter(! is.na(SPI)) %>% 
  arrange(LIT, SPI) %>%
  mutate(order = row_number())





plot<-ggplot(data=SPI_2017, mapping=aes(x=order, y=SPI, fill=Species))+
  geom_col()+
  facet_wrap(~LIT, scales='free_x') +
  scale_x_continuous(
    breaks=SPI_2017$order,
    labels=SPI_2017$UnitCell)+
  theme(axis.title.x =element_blank())+
  theme(axis.title.y =element_text(size=20))+
  labs(title="2017 Seed Production Index (SPI) By Unit", y="SPI")+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.text.x = element_text(angle=90, size=10))+
  theme(axis.text.y =element_text(size=20))+
  theme(strip.text= element_text(size=20))

plot

