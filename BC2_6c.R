#Db-RDA for BC2 for bacteria only in leaved and roots 
#4 oct 17

#load vegan
library(vegan)

# set working directory (where your input files live on your computer)
setwd("Documents/BC2_Analyses/")

#load commuity data matrix as object. 
BC2_6c_OTU<-read.csv("BC2_6c_OTU.csv", header=TRUE, row.names=1, colClasses = c("factor", rep("numeric",21)))

# Make Bray Curtis dissimilarity matrix 
BC26cBrayCurtisMatrix<-vegdist(BC2_6c_OTU, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,na.rm = FALSE)

str(BC2_6c_OTU)
is.na(BC2_6c_OTU)

is.finite(BC2_6c_OTU)

#load environmental data as an object
BC2_6c_Enviro<- read.csv("BC2_6c_Enviro.csv", header=TRUE, row.names=1)

#dbrda with all environmental data
#FullModel
dbRDA_all_6c<-capscale(BC26cBrayCurtisMatrix ~ Site + Date + avg_sal + max_sal + avg_water_lev + min_water_lev + max_water_lev + avg_temp + min_temp + max_temp + Tide_Amp + avg_waterlevel2 + Avg_Flooding, data=BC2_6c_Enviro)

#Null Model
#generate one model with NOTHING to explain the unifrac dm matrix
dbRDAnull_BC26c<-capscale(BC26cBrayCurtisMatrix~1,data=BC2_6c_Enviro)
dbRDAnull_BC26c

#use forward selection to choose which elements of the full model explain a significant amount of variaiton in the unifrac dm by comparing it to the null
#I have played around with changing the pstep # as I am unsure what is appropriate
dbRDA_forsel6c<-ordistep(dbRDAnull_BC26c,scope=formula(dbRDA_all_6c),direction="forward",Pin=.1,trace=T,pstep=200000)
dbRDA_forsel6c
# note: this model selected: max_sal and avg_water_level2 as the variables that best explained symbiont community
# I am still confused about the output results in the Inertia, Proportion, Rank table, as well as the Eigen Values. 
# I think this has something to do with the % of the varation explained by the model/ partitioning the variance?

plot(dbRDA_forsel6c)
#this makes a very ugly plot

anova(dbRDA_forsel6c, by="margin")
#This ANOVA shows the significance per variable that contributed to the model? So for Max_water_level p=.005 and avg_waterlevel2 p=.033.

# DO I need to condition?