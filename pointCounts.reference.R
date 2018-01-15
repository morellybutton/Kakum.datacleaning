#code for creating species reference list and analyzing raw datasheets

setwd("/Volumes/ELDS/ECOLIMITS/Ghana/Kakum/")
#site="Kakum"
spp<-read.csv(paste0(getwd(),"/Biodiversity/Birds_spplist.csv"))

#remove blank rows
spp<-spp[spp$Common.Name!="",]

#remove notes below names
spp<-spp[grep("http",spp$Family,invert=T),]

#extract habitat values
base<-spp[,1:4]

#identify species that visit Primaryforests
index<-grep("PrimaryForest",as.character(spp[,8]))
base[index,5]<-1
base[is.na(base[,5]),5]<-0

#identify species that visit Secondaryforests
index<-grep("SecondaryForest",as.character(spp[,8]))
base[index,6]<-1
base[is.na(base[,6]),6]<-0

#Identify species along forest edges
index<-grep("Edge",as.character(spp[,8]))
base[index,7]<-1
base[is.na(base[,7]),7]<-0

#Identify species in Woodland/Savanna
index1<-grep("Woodland",as.character(spp[,8]))
index2<-grep("Savanna",as.character(spp[,8]))
base[index1,8]<-1
base[index2,8]<-1
base[is.na(base[,8]),8]<-0

#Identify species in Cultivations,Plantations
index1<-grep("Cultivations",as.character(spp[,8]))
index2<-grep("Plantations",as.character(spp[,8]))
base[index1,9]<-1
base[index2,9]<-1
base[is.na(base[,9]),9]<-0

#Identify species in Settlements,Gardens
index1<-grep("Settlements",as.character(spp[,8]))
index2<-grep("Gardens",as.character(spp[,8]))
base[index1,10]<-1
base[index2,10]<-1
base[is.na(base[,10]),10]<-0

#Identify structure values
#Canopy and Sub-canopy
index1<-grep("Canopy",as.character(spp[,9]))
index2<-grep("Sub",as.character(spp[,9]))
base[index1,11]<-1
base[index2,11]<-1
base[is.na(base[,11]),11]<-0

#MidStory
index1<-grep("Mid",as.character(spp[,9]))
base[index1,12]<-1
base[is.na(base[,12]),12]<-0

#UnderStory
index1<-grep("Understor",as.character(spp[,9]))
index2<-grep("UnderGrowth",as.character(spp[,9]))
base[index1,13]<-1
base[index2,13]<-1
base[is.na(base[,13]),13]<-0

#Ground
index1<-grep("Ground",as.character(spp[,9]))
base[index1,14]<-1
base[is.na(base[,14]),14]<-0

#AllLevels
index1<-grep("AllLevels",as.character(spp[,9]))
index2<-grep("Generalist",as.character(spp[,9]))

#and CanopytoGround text
index3<-base[,11]==1&base[,14]==1
base[index1,15]<-1
base[index2,15]<-1
base[index3,15]<-1
base[is.na(base[,15]),15]<-0
rm(index,index1,index2,index3)

#Pull out Diet IDs
#ID frugivores
index1<-grep("Fruit",as.character(spp[,10]))
base[index1,16]<-1
base[is.na(base[,16]),16]<-0
rm(index1)

#ID Nectar eaters
index1<-grep("Nectar",as.character(spp[,10]))
base[index1,17]<-1
base[is.na(base[,17]),17]<-0
rm(index1)

#ID Seed eaters
index1<-grep("Seed",as.character(spp[,10]))
base[index1,18]<-1
base[is.na(base[,18]),18]<-0
rm(index1)

#ID Omnivores
index<-grep("SmallVertebrate",as.character(spp[,10]))
index1<-grep("Omnivor",as.character(spp[,10]))
index2<-grep("SmallAnimal",as.character(spp[,10]))
index3<-grep("Egg",as.character(spp[,10]))
index4<-grep("Amphibian",as.character(spp[,10]))
index5<-grep("Lizard",as.character(spp[,10]))
index6<-grep("Reptile",as.character(spp[,10]))

base[index,19]<-1
base[index1,19]<-1
base[index2,19]<-1
base[index3,19]<-1
base[index4,19]<-1
base[index5,19]<-1
base[index6,19]<-1
base[is.na(base[,19]),19]<-0
rm(index,index1,index2,index3,index4,index5,index6)

#ID Insectivore
index<-grep("Insect",as.character(spp[,10]))
base[index,20]<-1
base[is.na(base[,20]),20]<-0
rm(index)

#ID Insect Orders eaten
#Orthoptera (Grasshoppers,Crickets)
index<-grep("Orthoptera",as.character(spp[,11]))
index1<-grep("Gryll",as.character(spp[,11]))
index2<-grep("Acrid",as.character(spp[,11]))

base[index,21]<-1
base[index1,21]<-1
base[index2,21]<-1
base[is.na(base[,21]),21]<-0
rm(index,index1,index2)

#Hemiptera (Bugs)
index<-grep("Hemiptera",as.character(spp[,11]))
index1<-grep("Aphid",as.character(spp[,11]))
index2<-grep("Cicad",as.character(spp[,11]))
index3<-grep("Coccoid",as.character(spp[,11]))

base[index,22]<-1
base[index1,22]<-1
base[index2,22]<-1
base[index3,22]<-1
base[is.na(base[,22]),22]<-0
rm(index,index1,index2,index3)

#Araneae (Spiders)
index<-grep("Araneae",as.character(spp[,11]))
base[index,23]<-1
base[is.na(base[,23]),23]<-0
rm(index)

#Coleoptera (Beetles)
index<-grep("Coleopt",as.character(spp[,11]))
base[index,24]<-1
base[is.na(base[,24]),24]<-0
rm(index)

#Lepidoptera (Butterflies,Moths,etc)
index<-grep("Lepidopt",as.character(spp[,11]))
base[index,25]<-1
base[is.na(base[,25]),25]<-0
rm(index)

#Diptera (Mosquitoes,Midges)
index<-grep("Dipt",as.character(spp[,11]))
index1<-grep("Simulii",as.character(spp[,11]))
base[index,26]<-1
base[index1,26]<-1
base[is.na(base[,26]),26]<-0
rm(index,index1)

#Isoptera (Termites)
index<-grep("Isoptera",as.character(spp[,11]))
base[index,27]<-1
base[is.na(base[,27]),27]<-0
rm(index)

#Chilopoda ad Diplopoda (Centipedes and Millipedes)
index<-grep("Chilopod",as.character(spp[,11]))
index1<-grep("Diplopod",as.character(spp[,11]))
base[index,28]<-1
base[index1,28]<-1
base[is.na(base[,28]),28]<-0
rm(index,index1)

#Odonata (Dragon and Damselflies)
index<-grep("Odonata",as.character(spp[,11]))
base[index,29]<-1
base[is.na(base[,29]),29]<-0
rm(index)

#Hymenoptera (Ants,Wasps,Bees)
index<-grep("Hymenop",as.character(spp[,11]))
index1<-grep("Formic",as.character(spp[,11]))
base[index,30]<-1
base[index1,30]<-1
base[is.na(base[,30]),30]<-0
rm(index,index1)

colnames(base)<-c("Common Name","Family","Genus","Species","PrimaryForest","SecondaryForest","ForestEdge","Woodland/Savanna","Agriculture","Settlements","StructCanopy","StructMidStorey","StructUnderStory","StructGround","StructAllLevels","Frugivore","NectarEater","SeedEater","Omnivore","Insectivore","Order:Orthoptera","Order:Hemiptera","Order:Araneae","Order:Coleoptera","Order:Lepidoptera","Order:Diptera","Order:Isoptera","Order:Chilopoda/Diplopoda","Order:Odonata","Order:Hymenoptera")

#make sure new species with empty rows do not have entries
base[match(spp[spp$Habitats=="","Common.Name"],base$`Common Name`),5:30]<-NA

#write spp reference list
write.csv(base,paste0(getwd(),"/Biodiversity/bird_reflist.csv"))
