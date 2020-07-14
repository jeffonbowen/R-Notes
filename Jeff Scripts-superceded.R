## Quick Reference ##

## Favourite Resources ----

# R for Data Science. https://r4ds.had.co.nz/transform.html
# Ecology in R (Udemy)
# Data Camp seems very good for guided training
# Cookbook for R is handy, though wish it used tidy functions more.
# Also the companion ggplot2 cookbook, http://www.cookbook-r.com/Graphs/

## Import/Read Data ----

getwd()
setwd("c:/      ")
list.files()      # Useful for copying a filename   

# read_csv
indat <- read_csv('CEM_Oct10_13.csv', 
                  col_types = cols(.default = "c",
                                   'Shape_Area' = col_double(),
                                   'SDEC_1' = col_integer(), 
                                   'SDEC_2' = col_integer(), 
                                   'SDEC_3' = col_integer()))
# Default for all columns
col_types = cols(.default = "c")

# Interactive file selection
file.choose()

## View and clean data ----
Dim() ## dim gives dimensions
Print()
head()
Tail()
merge() 
str()
summary()
tibble()
rm() ## remove from memory

# from Janitor package
get_dupes(stations, 'Survey Name')
tabyl(stations, Footprint)

## Data mManipulation ----

library(tidyverse)
library(tibble)               # For tables
library(dplyr)                # Tidyverse
library(lubridate)

# Rename columns
colnames(x) <- c("a", "b")    # Base R
rename(x, newname = oldname)  # Tidy way

# Subsetting data
tmp <- dat[dat$SERAL_1 == "AS" & !is.na(dat$SERAL_1), ] # second part deals with NA rows

# To sort, use
arrange()
# Create variables
mutate()  # Benefit over base: more efficient with more than one variable).

# Filter out NA values
x <- x %>% filter(!is.na(BHC20_2))

# Recode values
dat[dat$SERAL_1 == "AS" & !is.na(dat$SERAL_1), ]$SERAL_1 <- "as"

# To  convert to factor
StationIDxEnvTest$CountOfStationID <- as.factor(StationIDxEnvTest$CountOfStationID)

# Count and tally

# Group_by and Summarize
sumdat <- data %>% group_by(GRID_ID, PRVUD, PRVUDOF, Accessible, Ownership, Candidate2, Region, BHC20_1) %>% 
  summarise(Total = sum(Shape_Area/10000)) %>% 
  top_n(1, Total) %>% 
  filter(Candidate2 == "Yes")


## Display and Formatting ----

# Format %
densplot$Perc <- round(densplot$Perc, digits = 2)
densplot$Perc <- paste(densplot$Perc, "%")


## Visualization ----
library(ggplot2)              # For graphics
library(cowplot)              # addon to ggplot

g2 <- ggplot(emmE.plot, aes(x=xEasting, y=mean, group=Habitat))+
  geom_line(aes(colour=Habitat))+    
  scale_size_manual(10)+
  scale_colour_manual(values=c("#1D4D18", "#5BC355", "#ABEB22","#EBB57C"))+
  xlab(label = "<<< West          -Location-          East >>>") +
  ylab(label = "Estimated mean density (males/ha) with 95% CI")+
  labs(title=spp)+
  theme_bw()+
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill=Habitat, colour=NA), alpha=0.1, linetype=0)
g2


## Linear Models ----

library(vcd)                  # Fits a discrete (count data) distribution for goodness-of-fit tests.

# Common packages for regression-type analyses 
library(lme4)                 # GLMMs
library(MASS)                 # Negative Binomial GLM
library(glmmTMB)              # for nb and zi, but can do everyhting esle too
library(pscl)                 # zero-inflated models
library(mgcv)                 # GAM

# Packages for evaluating model fit
library(DHARMa)               # Analysis of residuals for mixed models

# Common packages for interpretting results
library(visreg)               # regression visualization
library(MuMIn)                # model selection
library(effects)              # for extracting effects terms. Use effects or Alleffects
library(emmeans)              # Estimate marginal means

# Helpers
library(broom)                # Tidy results tables for exporting and printing 
library(broom.mixed)          # Same as above, but for mixed models

library(corrplot)             # Come back to this and find home

## Typical steps for regression ----

# Step 1. Visualize distribution
# Make a histogram using hist() or ggplot(); visually inspect
# For count data, use package 'vcd' to fit possible disubutions
fit.p <- goodfit(dat$Richness, type = "poisson")  # also binomial and nbinomial
summary(fit.p)
rootogram(fit.p)

# STEP 2. Fit model


# STEP 3. Compare models
# Comare models using package 'MuMin'. 
model.sel(m1, m2, m3, m4)
# ** Figure out weights and model averaging

# STEP 4. Assess model fit. 
# If glm, view standard diagnostic plots
plot(model1)  # or/
# Examine Residuals for Gaussian
qqnorm(residuals(bestmod))
qqline(residuals(bestmod))
plot(bestmod,MonYear~resid(.))

# Visreg?

# For mixed models, use package 'DHARMa' to simulate residuals.
res <- simulateResiduals(fittedModel = bestmod)
plot(res)
testDispersion(res)
# There are other test in 'DHAMRa' to explore.

# Test for overdispersion. Unlear of these work for mixed models. 
deviance(bestmod)/df.residual(bestmod)
# or
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(bestmod)

# STEP 5. Estimate Means and Plots 
# Plot all effects
ae <- allEffects(bestmod)
plot(ae, residuals="TRUE")
ae
# Or plot individual effects
e <- predictorEffect("BHC7", bestmod)
plot(e)

# Package EMMEANS for marginal means

# To make predictions, may need to create a grid.
# expand.grip, tidyr:: expand_grid, modelr::data_grid

# What a out predictions for continuous variables?
emmE <- as.data.frame(emmeans(pois.m2, specs="xEasting", 
                              at=list(xEasting=seq(-2.03, 1.13, length.out=50))))



## Extract the coefficients data frame
x <-summary.glm(Count.model1)$coefficients 

## Extract the coefficients data frame
results.effects <-as.data.frame(effect('Habitat.Class', Count.model1)) 






## Biodiversity analyses ----
library(vegan)
library(dismo)                # Species distribution models
library(BiodiversityR)
library(indicspecies)         # Indicator species analysis


RankAbun.1 <- rankabuncomp(stxsp, y=st.year, factor='Survey.Name', scale='abundance', 
                           legend=F, ylim = c(0,600), rainbow=T)
RankAbun.1 <- rankabuncomp(stxsp, y=st.year, factor='Survey.Name', scale='proportion',
                           legend=T, rainbow=T)
RankAbun.1 <- rankabuncomp(stXspAbunSMSA, y=stXyearSMSA, factor='Survey.Name', scale='proportion',
                           legend=T, rainbow=T)

data(mtcars)

library(iNEXT)
out <- iNEXT(spider, q=c(0,1,2), datatype="abundance", size=m)
ggiNEXT(x, type=1, se=TRUE, facet.var="order", color.var="site", grey=FALSE)

DataInfo(spXyearSMSA)
out <- iNEXT(spXyearSMSAn, q=0, datatype="incidence_freq",endpoint=336)
out <- iNEXT(spXyearSMSAn, q=c(0,1,2), datatype="incidence_freq",endpoint=336)
ggiNEXT(out,type=1,se=TRUE)
ggiNEXT(out,type=2,se=TRUE)
ggiNEXT(out, type=1, facet.var="order")

library(SpadeR)
SimilarityMult(spxyear.1i, datatype = "incidence_freq", q = 0, nboot = 200)


### Mapping ----

library(sp)         ## Key package. Though being replaced by sf.
library(sf)         ## Replaces sp. sp still needed for some things.
library(rgdal)      ## Bindings for GDAL(?) seems to provide helper functions

library(raster)     ## This is key for raster manipulation.
library(rasterVis)
library(maptools)   ## Haven't used this yet

library(maps)

library(rgeos)      # ? Has some data. Geometry Engine
library(tmap)
library(tmaptools)
library(ggmap)      ## Spatial Visualization with ggplot2

library(rspatial)

## For dynamic map bases
library(leaflet)    # This is very popular. 
library(mapview)    # Another popular package for interactive maps

library(htmlwidgets)
library(shiny)
library(webshot)


# Mapview, good for quick view
mapview(x)

# Leaflet
m <- leaflet() %>% 
  addTiles() 

# Lat/long NAD83 is EPSG 4269
# Lat/long WGS84 is EPSG 4326
# UTM NAD83 zone 10N is EPSG 26911 

# How to reproject
newcrs = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
footprint <- spTransform(footprint, newcrs)

# Read/Write shapefile
library(rgdal)
writeOGR(footprint, dsn = ".", "fp_wgs84", driver="ESRI Shapefile")

# Read UTM cooridnates and write same datafile with lat/long 
dat <- read_csv("WOODto2019.csv") 
dat <- SpatialPointsDataFrame(coords = cbind(woodat$UTM_Northing, woodat$UTM_Easting), 
                              data = woodat, proj4string=CRS("+proj=utm +zone=10 +north +datum=NAD83"))  
dat <- spTransform(wooutm, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
datout <- (cbind(dat@data, dat@coords)) %>% 
                  rename(Longitude = coords.x1, Latitude = coords.x2)

# Rasters
GDALinfo("") # raster file attributes before reading. 