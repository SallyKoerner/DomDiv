###data for pulling in climate, aridity & SPEI data for grassland dominance/diversity MS
###EJ Forrestel, revised 20 November 2019 ; Komatsu revised Nov 2020

###loading packages
require(sp);require(raster);require(SPEI);require(readxl);require(rgdal);require(maptools);require(viridis);library(ggplot2);require(hexbin);require(httr);require(rasterVis);require(maps);require(RColorBrewer);require(SDMTools)

setwd('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\GEx working groups\\DomDiv_Workshop\\DomDivClimate')


######CLEANING STEPS FOR ORIGINAL DATA FROM SALLY####
###reading in original Excel file and cleaning up the coordinates and spitting back out as a csv
dat <- read_excel('GPS_AllSites_update11122020.xlsx')
dat <- as.data.frame(dat)
dat$GPS_long <- as.numeric(dat$GPS_long)
dat$GPS_lat <- as.numeric(dat$GPS_lat)

###Step 1, flipping coorindates for the Kenya sites
temp <- dat$GPS_lat[which(dat$Block=="Kenya")]
dat$GPS_lat[which(dat$Block=="Kenya")] <- dat$GPS_long[which(dat$Block=="Kenya")]
dat$GPS_long[which(dat$Block=="Kenya")] <- temp

###Step 2, making the Brazil coordinates negative
dat$GPS_lat[which(dat$Block=="Brazil")] <- -dat$GPS_lat[which(dat$Block=="Brazil")]
dat$GPS_long[which(dat$Block=="Brazil")] <- -dat$GPS_long[which(dat$Block=="Brazil")]

###writing back out csv files with fixed coordinates
write.csv(dat,file="GPS_AllSites_FIXED_112020.csv")

####START HERE TO READ IN OUTPUTTED CLEAN DATA####
###reading in fixed coordinates
dat <- read.csv('GPS_AllSites_FIXED_112020.csv')

###subsetting data to include only sites of interest for the map plot
country_list <- c('Argentina','Australia','Brazil','Inner Mongolia, China','India','South Africa','Tibet, China','USA')
dat <- dat[which(dat$Block%in%country_list),]


###choosing color palette for biplots of MAP and MAT

scale_color_manual(values=c("#E41A1C", "#999999","#4DAF4A","#FF7F00","#984EA3", "#A65628" ,"#F781BF","#377EB8"))
color_scale <- as.data.frame(matrix(,data=NA,nrow=8,ncol=2))
colnames(color_scale) <- c('country','color')
color_scale$country <- country_list
color_scale$color <- c("#E41A1C", "#999999","#4DAF4A", "#984EA3","#FF7F00", "#A65628" ,"#F781BF","#377EB8")
color_scale2 <- as.data.frame(matrix(,data=NA,nrow=8,ncol=2))
colnames(color_scale2) <- c('country','color')
color_scale2$country <- country_list
color_scale2$color<- c("#E41A1C", "#999999","#4DAF4A", "#FF7F00", "#984EA3","#A65628" ,"#F781BF","#377EB8")

###reading in bioclim variable layers 
r.bio <- getData('worldclim',var='bio',res=5)

###reading in coordaintes for all sites
coords <- data.frame(as.numeric(dat$GPS_long),as.numeric(dat$GPS_lat))
points <- SpatialPoints(coords,proj4string = r.bio@crs)
dat.bio <- extract(r.bio,points)
###reading in AI data
AI <- raster('C:\\Users\\lapie\\Dropbox (Smithsonian)\\working groups\\GEx working groups\\DomDiv_Workshop\\DomDivClimate\\AI_annual\\AI_annual\\ai_yr')
values(AI) <- log(values(AI))
dat.AI <- extract(AI,points)

###combining climate data 
rownames(dat.bio) <- dat$Site_name
dat.clim <- cbind(dat.bio,dat.AI)
write.csv(dat.clim,file='dat_clim_NEW_2020.csv')
dat.clim <- cbind(dat,dat.bio)
dat.clim$color <- color_scale$color[match(dat$Block,color_scale$country)]

###checking plotting color scale
ggplot(data.frame(x = rnorm(10000), y = rnorm(10000)), aes(x = x, y = y)) +
  geom_hex() + coord_fixed() +
  scale_fill_viridis() + theme_bw() + theme(legend.position="bottom")

#image(AI,col=alpha(rev(viridis(256)),.9),asp=1,axes=FALSE,xaxs="i",xaxt='n',yaxt='n',ann=FALSE)
map("world",fill=TRUE,col=alpha("gray",.2),lwd=.7,ylim=c(-60,90),mar=c(0,0,0,0),border=alpha("gray10",alpha=.6))
points(points,cex=.8,col=dat.clim$color,pch=16)
box()
#pnts <- cbind(x=c(-160,-155,-155,-160),y=c(10,10,-30,-30))
#cols <- rev(viridis(12)) 
#legend.gradient(pnts,cols,title="Aridity Index",cex=.6,limits=c(0,13.5))
# legend(x=-180,y=40,legend=c('Argentina','Australia','Brazil','Inner Mongolia, China','India','South Africa','Tibet, China','USA'),cex=.7,col=color_scale$color,bty="n",pt.cex=1.2,pch=16)
#text()

###generating MAP and MAT biplot
theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=20, vjust=-0.35), axis.text.x=element_text(size=16),
             axis.title.y=element_text(size=20, angle=90, vjust=0.5), axis.text.y=element_text(size=16),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))

ggplot(data=dat.clim, aes(x=bio12, y=bio1/10)) +
  geom_point(aes(color=Block), size=3) +
  scale_color_manual(values=color_scale2$color) +
  xlab('Mean Annual Precipitation (mm)') + ylab('Mean Annual Temperature (C)') +
  theme(legend.position='bottom')
#export at 900x600

# plot(dat.clim$bio12,dat.clim$bio1/10,col=dat.clim$color,pch=16,cex=1.2,xlab="Mean Annual Precipitation (mm)",ylab="Mean Annual Temperature (C)",las=1)

