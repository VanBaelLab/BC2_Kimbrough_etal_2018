#Db-RDA for BC2 for all fungi and bacteria in leaves and roots 

#load vegan
library(vegan)

# set working directory 
setwd("Documents/BC2_Analyses/")

#load commuity data matrix as object. Mine has 38 obs of 44 variables. make sample names row names and make sure OTU counts are read as numeric
BC2_OTU<-read.csv("BC2_OTU.csv", header=TRUE, row.names=1, colClasses = c("factor", rep("numeric",44)))

#dissimilarity matrix script. make output an object by putting BC2BrayCurtisMatrix<- at beginning
BC2BrayCurtisMatrix<-vegdist(BC2_OTU, method="bray", binary=FALSE, diag=FALSE, upper=FALSE,na.rm = FALSE)


#load environmental data as an object
BC2_Env<- read.csv("BC2_Enviro.csv", header=TRUE, row.names=1)


#dbrda with all env and pc
#FullModel
dbRDA_all_LRcombo<-capscale(BC2BrayCurtisMatrix ~ Site + Date + avg_sal + max_sal + avg_water_lev + min_water_lev + max_water_lev + avg_temp + min_temp + max_temp + Tide_Amp + avg_waterlevel2 + Avg_Flooding, data=BC2_Env)


#Null Model
#generate one model with NOTHING to explain the unifrac dm matrix
dbRDAnull_BC2<-capscale(BC2BrayCurtisMatrix~1,data=BC2_Env)
dbRDAnull_BC2

#use forward selection to choose which elements of the full model explain a significant amount of variaiton in the unifrac dm by comparing it to the null
dbRDA_forsel<-ordistep(dbRDAnull_BC2,scope=formula(dbRDA_all_LRcombo),direction="forward",Pin=.1,trace=T,pstep=200000)
dbRDA_forsel
#note: this model selected :max_water_level, max_temperature, max_salinity, avg_water_level in different combos each time as explaining significant amount of variation

plot(dbRDA_forsel)
anova(dbRDA_forsel, by="margin")
#This ANOVA showed the significance per variable- 

#conditioning: agknowleges that these are correlated, condition for site (and tissue)?
dbRDA_all_LRcombo_conditionforwater<-capscale(BC26bBrayCurtisMatrix ~ max_water_lev + avg_waterlevel2 + Condition(Site), data =BC2_6b_Enviro)
plot(dbRDA_all_LRcombo_conditionforwater)
dbRDA_all_LRcombo_conditionforwater
