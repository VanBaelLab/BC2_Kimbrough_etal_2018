#Db-RDA for BC2 for fungi only in roots 
Dec 15 2017
Root Fungi Only
#load vegan
library(vegan)


#load commuity data matrix as object. 
BC2_6f_OTU<-read.csv("BC2_6f_OTU.csv", header=TRUE, row.names=1, colClasses = c("factor", rep("numeric",21)))


str(BC2_6f_OTU)
BC2_6f_OTU_NA <- BC2_6f_OTU[, c(-ncol(BC2_6f_OTU), -ncol(BC2_6f_OTU)-1)]
BC2_6f_OTU_NA2 <- BC2_6f_OTU_NA[, c(-ncol(BC2_6f_OTU_NA), -ncol(BC2_6f_OTU_NA)-1)]

# Make Bray Curtis dissimilarity matrix 
BC26fBrayCurtisMatrix<-vegdist(BC2_6f_OTU_NA2, method="bray", binary=FALSE, diag=FALSE, upper=FALSE)


#load environmental data as an object
BC2_6f_Enviro<- read.csv("BC2_6f_Enviro.csv", header=TRUE, row.names=1)

#dbrda with all environmental data
#FullModel
dbRDA_all_6f<-capscale(BC26fBrayCurtisMatrix ~ Site + Date + avg_sal + max_sal + avg_water_lev + min_water_lev + max_water_lev + avg_temp + min_temp + max_temp + Tide_Amp + avg_waterlevel2 + Avg_Flooding, data=BC2_6f_Enviro)

#Null Model
#generate one model with NOTHING to explain the unifrac dm matrix
dbRDAnull_BC26f<-capscale(BC26fBrayCurtisMatrix~1,data=BC2_6f_Enviro)
dbRDAnull_BC26f

#use forward selection to choose which elements of the full model explain a significant amount of variaiton in the unifrac dm by comparing it to the null
#I have played around with changing the pstep # as I am unsure what is appropriate
dbRDA_forsel6f<-ordistep(dbRDAnull_BC26f,scope=formula(dbRDA_all_6f),direction="forward",Pin=.1,trace=T,pstep=200000)
dbRDA_forsel6f
# note: this model selected: max_sal and avg_water_level2 as the variables that best explained symbiont community
# I am still confused about the output results in the Inertia, Proportion, Rank table, as well as the Eigen Values. 
# I think this has something to do with the % of the varation explained by the model/ partitioning the variance?

plot(dbRDA_forsel6f)
#this makes a very ugly plot

anova(dbRDA_forsel6f, by="margin")
#This ANOVA shows the significance per variable that contributed to the model? So for Max_water_level p=.005 and avg_waterlevel2 p=.033.

# DO I need to condition?