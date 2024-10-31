#Within and between country diversity

#Plotted bar plots of pairwise SNP distances vs counts for each country. Example:

library(ggplot2)

# Density histogram of pairwise SNP distance counts

UK_all=read.csv("UK_SNPdistances.csv",header=T)
ggplot(UK_all,aes(x=UK.SNP.distances))+
geom_histogram(binwidth=10)

# Complete for each country

# Between country diversity:

# Combined vcf imported into R using vcfR straight into genind object

library(vcfR)
library(adegenet)
library(hierfstat)

Ti_vcf=read.vcfR("Ti.vcf",verbose=F)
Ti_genind=vcfR2genind(Ti_vcf)
metadata=read.csv("Ti_isolates.metadata.csv",header=T)
Ti_genind@pop=as.factor(metadata$Country)

# Between country diversity tests:

all=genet.dist(Ti_genind,method="WC84)

all %>% round(digits=3)