#### Global path ####
#setwd("/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
setwd("/home/lucas.dugerdil/Documents/Recherche/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/R_stats") 
# DB.path = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/Data_Bases/"
DB.path = "/home/lucas.dugerdil/Documents/Recherche/Data_Bases/"

#### Libraries ####
# suppressPackageStartupMessages()
library(ggplot2)
library(ggpp) 
library(ggpubr) # stat_cor
library(ggpmisc) # stat_poly_line
library(RColorBrewer)
library(patchwork)
library(dplyr) # mutate
library(maps)
library(mapproj)
library(gstat)
library(reshape)
library(ggnewscale) # function new_scale_color
library("readr")
library(ggthemes)
library(tibble) # as_tibble
library(scales)       # quoi du type d'echelle sur les graphs ggplot
library(lubridate)
library(rgdal)

# usethis::use_git_config(user.name="Lucas Dugerdil", user.email="lucas.dugerdil@lebib.org")
# library(usethis)

#### Function ####
Clean.trait.discon <- function(MD, Trait.ID, Keep.string, No.aggregation = F){
  if(missing(Keep.string)){Keep.string <- NULL}
  MD <- MD[MD$TraitID %in% Trait.ID, c("AccSpeciesName", "TraitID", "OrigValueStr")]
  names(MD)[1] <- c("species")
  MD$ID <- seq(1:nrow(MD))
  #MD$TraitID <- as.numeric(MC$TraitID)
  MD <- as_tibble(reshape2::dcast(MD, species + ID ~ TraitID, value.var = "OrigValueStr"))
  
  #### Photosynthesis pathway ####
  if(is.null(MD[["22"]]) == F){
    MD[["22"]] <- gsub("c3", "C3", MD[["22"]])
    MD[["22"]] <- gsub("C3\\?", "C3", MD[["22"]])
    MD[["22"]] <- gsub("C4\\?", "C4", MD[["22"]])
    MD[["22"]] <- gsub("c4", "C4", MD[["22"]])
    
    MD[["22"]] <- gsub("C3/C4", 0.5, MD[["22"]])
    MD[["22"]] <- gsub("C3/CAM", -0.5, MD[["22"]])
    MD[["22"]] <- gsub("C3", 0, MD[["22"]])
    MD[["22"]] <- gsub("C4", 1, MD[["22"]])
    MD[["22"]] <- gsub("CAM", -1, MD[["22"]])
    MD[["22"]] <- as.numeric(MD[["22"]])
  }
  
  #### Evergreenness ####
  if(is.null(MD[["37"]]) == F){
    MD[["37"]][which(MD[["37"]] == "Evergreen")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "exchanger")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "yes")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "3")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "?")] <- NA
    MD[["37"]][which(MD[["37"]] == "n.d.")] <- NA
    MD[["37"]][which(MD[["37"]] == "aphyllous")] <- NA
    MD[["37"]][which(MD[["37"]] == "E")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "evergreen type 2")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "evergreen  type 1")] <- "evergreen"
    MD[["37"]][which(MD[["37"]] == "always persistent green")] <- "evergreen"
    
    MD[["37"]][which(MD[["37"]] == "Deciduous")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "aestival")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "vernal")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "hibernal")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "aestival")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "no")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "D")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "Db")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "W")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "winter deciduous")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "deciduous type 1")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "deciduous type 2")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "deciduous type 3")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "5")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "1")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "megaphanerophyte")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "Nonevergreen")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "always summer green")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "always overwintering green")] <- "deciduous"
    MD[["37"]][which(MD[["37"]] == "always spring green")] <- "deciduous"
    
    MD[["37"]][which(MD[["37"]] == "2")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "SEMI")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "ED")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "deciduous/evergreen")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "semi-deciduous")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "semideciduous")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "winter semi-deciduous")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "drought semi-deciduous")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "drought-deciduous")] <- "semi-evergreen"
    MD[["37"]][which(MD[["37"]] == "variable")] <- 0.5
    MD[["37"]][which(MD[["37"]] == "semi-evergreen")] <- 0.5
    MD[["37"]][which(MD[["37"]] == "deciduous")] <- 0
    MD[["37"]][which(MD[["37"]] == "evergreen")] <- 1
    MD[["37"]] <- as.numeric(MD[["37"]])}
  
  #### Woodiness ####
  if(is.null(MD[["38"]]) == F){
    MD[["38"]][which(MD[["38"]] == "Semi-woody")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "semi-woody")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "wood at base")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "woody at base")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "Suffrutex")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "non-woody/woody")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "Variable")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "variable")] <- 0.5
    MD[["38"]][which(MD[["38"]] == "Herb")] <- 0
    MD[["38"]][which(MD[["38"]] == "h")] <- 0
    MD[["38"]][which(MD[["38"]] == "H")] <- 0
    MD[["38"]][which(MD[["38"]] == "Herbaceous")] <- 0
    MD[["38"]][which(MD[["38"]] == "Grass&Sedges")] <- 0
    MD[["38"]][which(MD[["38"]] == "non-woody")] <- 0
    MD[["38"]][which(MD[["38"]] == "non woody")] <- 0
    MD[["38"]][which(MD[["38"]] == "Woody")] <- 1
    MD[["38"]][which(MD[["38"]] == "W")] <- 1
    MD[["38"]][which(MD[["38"]] == "Y")] <- 1
    MD[["38"]][which(MD[["38"]] == "w")] <- 1
    MD[["38"]][which(MD[["38"]] == "3")] <- 1
    MD[["38"]][which(MD[["38"]] == "2")] <- 1
    MD[["38"]][which(MD[["38"]] == "woody")] <- 1
    MD[["38"]] <- as.numeric(MD[["38"]])
  }
  
  #### Growth forme ####
  if(is.null(MD[["48"]]) == F){
    MD[["48"]][which(MD[["48"]] == "Shrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == " shrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "shrug")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "shrub*")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "shrub*****")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "twining shrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "twiner")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "twiner*")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "woody*")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "woody")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "Woody")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "succulent shrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "climbing shrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "climbing shrub*")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "semi-shrub")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "sub-shrub")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "sub shrub")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "sub shrub*")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "woody herb")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "cushion grass")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "cushion plant")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "creeper")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "creeper*")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "woody creeper")] <- "subshrub"
    MD[["48"]][which(MD[["48"]] == "forb*")] <- "forb"
    MD[["48"]][which(MD[["48"]] == "crucifer")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "herb*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "Herb")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "Herb, Pere")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "per grass")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "climbing herb*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "climbing herb")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "twining herb*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "twining herb")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "herbaceous twiner")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "herbaceous climber*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "tussock grass")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "Grass")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "bunchgrass")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "moss")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "rosette")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "succulent*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "leaf succulent")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "succulent")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "succulent herb")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "neophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "neophyte*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "halophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "epiphyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "Non-woody epiphyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "non-woody epiphyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "geophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "geophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "geophyte*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "hemicryptophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "therophyte")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "cyperoid")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "cyperoid*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "grass")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "grass*")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "Graminoid")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "bamboo")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "shrubby bamboo")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "Tree")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "small tree")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "Small_Tree")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "tree ")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "treelet*")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "tree*")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "tree**** (means find other one in original, add *)")] <- "tree"
    MD[["48"]][which(MD[["48"]] == "woody vine")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "woody vine*")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "Liana")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "liana")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "liana*")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "climber*")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "Vine")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "vine*")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "vine")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "climbing legume")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "herbaceous vine")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "herbaceous climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "hook climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "leaning climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "tendril climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "shrubby climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "woody climber")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "woody climber*")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "hemiepiphyte")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "epiphyte")] <- "climber"
    MD[["48"]][which(MD[["48"]] == "epiphyte*")] <- "climber"
    
    MD[["48"]][which(MD[["48"]] == "hemiparasite")] <- "parasite"
    MD[["48"]][which(MD[["48"]] == "hemiparasite*")] <- "parasite"
    MD[["48"]][which(MD[["48"]] == "Parasite")] <- "parasite"
    MD[["48"]][which(MD[["48"]] == "parasite*")] <- "parasite"
    MD[["48"]][which(MD[["48"]] == "Sedge")] <- "sedge"
    MD[["48"]][which(MD[["48"]] == "Aquatic")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "aquatic sedge")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "aquatic sedge*")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "aquatic herb")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "aquatic herb*")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "aquatic*")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "hydrophyte")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "hydrophyte")] <- "aquatic"
    MD[["48"]][which(MD[["48"]] == "marsh")] <- "aquatic"
    
    
    MD[["48"]][which(MD[["48"]] == "1")] <- NA
    MD[["48"]][which(MD[["48"]] == "10")] <- NA
    MD[["48"]][which(MD[["48"]] == "2")] <- NA
    MD[["48"]][which(MD[["48"]] == "3")] <- NA
    MD[["48"]][which(MD[["48"]] == "4")] <- NA
    MD[["48"]][which(MD[["48"]] == "5")] <- NA
    MD[["48"]][which(MD[["48"]] == "Reed")] <- NA
    MD[["48"]][which(MD[["48"]] == "Pam")] <- NA
    MD[["48"]][which(MD[["48"]] == "scrambler")] <- NA
    MD[["48"]][which(MD[["48"]] == "scrambling herb")] <- NA
    
    MD[["48"]][which(MD[["48"]] == "subshrub")] <- "shrub"
    MD[["48"]][which(MD[["48"]] == "forb")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "sedge")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "graminoid")] <- "herb"
    MD[["48"]][which(MD[["48"]] == "parasite")] <- "Other"
    MD[["48"]][which(MD[["48"]] == "climber")] <- "Other"
    MD[["48"]][which(MD[["48"]] == "aquatic")] <- "Other"
    MD[["48"]][which(MD[["48"]] == "other")] <- "Other"
    MD[["48"]][which(MD[["48"]] == "herb")] <- "Herb"
    MD[["48"]][which(MD[["48"]] == "tree")] <- "Tree"
    MD[["48"]][which(MD[["48"]] == "shrub")] <- "Shrub"
  }
  
  #### Select unique value of GF ####  
  if(is.null(Keep.string) == F & any(names(MD) %in% as.character(Keep.string)) == T){
    MD.str <- MD[names(MD) %in% c("species", as.character(Keep.string))]
    names(MD.str)[2] <- "To.agg"
    library(plyr)
    MD.str <- plyr::count(MD.str, c("species", "To.agg"))
    MD.str <- MD.str[is.na(MD.str$To.agg)==F,]
    Duplicated.species <- MD.str$species[duplicated(MD.str$species)]
    # Selected.type <- MD.str[0]
    for(i in Duplicated.species){
      Sub.df <- MD.str[MD.str$species == i,]
      
      # print(Sub.df$To.agg[Sub.df$freq == max(Sub.df$freq)][1])
      MD.str$To.agg[MD.str$species == i] <- Sub.df$To.agg[Sub.df$freq == max(Sub.df$freq)][1]
      # print(MD.str$To.agg[MD.str$species == i])
    }
    MD.str <- MD.str[-c(3)]
    MD.str <- MD.str[!duplicated(MD.str),]
    names(MD.str)[2] <- as.character(Keep.string)
    Go = T
  }
  else{Go = F}
  
  #### Merge MD/MC ####
  MD <- MD[-2]
  MD <- MD[!names(MD) %in% as.character(Keep.string)]
  if(No.aggregation == F){
    MD <- aggregate(MD, list(MD[["species"]]), FUN = mean, na.action = na.omit)
    MD <- MD[-2]
    names(MD)[1] <- c("species")
  }
  
  if(is.null(Keep.string) == F & Go == T){
    MD <- full_join(MD, MD.str, by = "species")
  }
  
  return(MD)
}

Clean.trait.con <- function(MC, Trait.ID, Average){
  if(missing(Average)){Average = F}
  MC <- MC[MC$TraitID %in% Trait.ID, c("AccSpeciesName", "TraitID", "StdValue")]#, "UnitName", "Comment")]
  names(MC)[1] <- c("species")
  MC <- MC[which(!is.na(MC$species)),]
  
  # PB <- unique(MC[c(2,4,5)])
  # PB <- PB[sort(PB$TraitID),]
  # View(PB)
  
  MC$StdValue <- as.numeric(MC$StdValue)
  MC$TraitID <- as.numeric(MC$TraitID)
  MC <- aggregate(MC, list(MC$species, MC$TraitID), FUN = mean)
  MC <- MC[-c(2,3)]
  names(MC)[c(1,2)] <- c("species", "TraitID")
  MC <- reshape2::dcast(MC[-4], species ~ TraitID, na.rm = F)
  
  #### Valeur moyenne des traits équivalents ####
  if(Average == T){
    MC[["3108"]] <- rowMeans(MC[as.character(3108:3114)], na.rm = T)
    MC <- MC[-match(as.character(3109:3114), names(MC))]  
    
    MC[["3115"]] <- rowMeans(MC[as.character(3115:3117)], na.rm = T)
    MC <- MC[-match(as.character(3116:3117), names(MC))]  
    # print(MC)
  }
  
  return(MC)
}
Auto.extract.trait.agg.table <- function(MP, Table.Taxon, TNRS.check, Show.missing = F, Save.path){
  #### Settings ####
  if(missing(MP)){warning("Import a surface pollen matrix.")}
  if(missing(Table.Taxon)){warning("Import a Conversion Taxon Table.")}
  if(missing(TNRS.check)){TNRS.check = F}
  if(missing(Save.path)){Save.path = NULL}
  
  #### Extract UZ conversion table from ACA ####
  MP <- gsub("\\.", " ", row.names(MP))
  TPT <- gsub("\\.", " ", Table.Taxon$Nom)
  
  if(Show.missing == T){
    print("The following pollen-types are not in the Taxa Table.")
    print(setdiff(MP, TPT))
    }
  
  TPT <- Table.Taxon[match(MP, TPT),]
  row.names(TPT) <- MP
  TPT <- TPT[order(TPT$Genre),]
  TPT <- TPT[order(TPT$Famille),]
  
  #### Clean with TNRS ####
  if(TNRS.check == T){
    library(TNRS)
    Espece.cor <- TNRS(TPT$Espece)
    TPT$Espece[which(TPT$Espece %in% Espece.cor$Name_submitted)] <- Espece.cor$Accepted_species[match(TPT$Espece[which(TPT$Espece %in% Espece.cor$Name_submitted)], Espece.cor$Name_submitted)]
    Espece.cor <- TNRS(TPT$Genre)
    TPT$Genre[which(TPT$Genre %in% Espece.cor$Name_submitted)] <- Espece.cor$Accepted_name[match(TPT$Genre[which(TPT$Genre %in% Espece.cor$Name_submitted)], Espece.cor$Name_submitted)]
    Espece.cor <- TNRS(TPT$Famille)
    TPT$Famille[which(TPT$Famille %in% Espece.cor$Name_submitted)] <- Espece.cor$Accepted_name[match(TPT$Famille[which(TPT$Famille %in% Espece.cor$Name_submitted)], Espece.cor$Name_submitted)]
    }
  
  #### Pollen type coarse and fine ####
  TPT$PT_sl <- NA
  TPT$PT_sl[which(TPT$Level == 5)] <- TPT$Espece[which(TPT$Level == 5)] 
  TPT$PT_sl[which(TPT$Level == 4)] <- TPT$Subgenus[which(TPT$Level == 4)] 
  TPT$PT_sl[which(TPT$Level == 3)] <- TPT$Genre[which(TPT$Level == 3)] 
  TPT$PT_sl[which(TPT$Level == 2)] <- TPT$Subfamille[which(TPT$Level == 2)] 
  TPT$PT_sl[which(TPT$Level == 1)] <- TPT$Famille[which(TPT$Level == 1)] 
  TPT$PT_ss <- NA
  TPT$PT_ss[which(!TPT$Espece == "")] <- TPT$Espece[which(!TPT$Espece == "")] 
  TPT$PT_ss[intersect(which(!TPT$Subgenus == ""), which(is.na(TPT$PT_ss)))] <- TPT$Subgenus[intersect(which(!TPT$Subgenus == ""), which(is.na(TPT$PT_ss)))] 
  TPT$PT_ss[intersect(which(!TPT$Genre == ""), which(is.na(TPT$PT_ss)))] <- TPT$Genre[intersect(which(!TPT$Genre == ""), which(is.na(TPT$PT_ss)))] 
  TPT$PT_ss[intersect(which(!TPT$Subfamille == ""), which(is.na(TPT$PT_ss)))] <- TPT$Subfamille[intersect(which(!TPT$Subfamille == ""), which(is.na(TPT$PT_ss)))] 
  TPT$PT_ss[intersect(which(!TPT$Famille == ""), which(is.na(TPT$PT_ss)))] <- TPT$Famille[intersect(which(!TPT$Famille == ""), which(is.na(TPT$PT_ss)))] 
  
  #### Add labels PT coarse and fine ####
  TPT["PT_ss.label"] <- TPT$PT_ss
  TPT$PT_ss.label[TPT$PT_ss %in% TPT$Genre] <- paste(TPT$PT_ss[TPT$PT_ss %in% TPT$Genre], "spp.", sep = " ")
  TPT$PT_ss.label[grep(" t", row.names(TPT))] <- paste(TPT$PT_ss[grep(" t", row.names(TPT))], "type", sep = "-")
  
  TPT["PT_sl.label"] <- TPT$PT_sl
  TPT$PT_sl.label[TPT$PT_sl %in% TPT$Genre] <- paste(TPT$PT_sl[TPT$PT_sl %in% TPT$Genre], "spp.", sep = " ")
  TPT$PT_sl.label[grep(" t", row.names(TPT))] <- paste(TPT$PT_sl[grep(" t", row.names(TPT))], "type", sep = "-")
  
  #### Export ####
  if(is.null(Save.path) == F){write.csv(TPT, Save.path)}
  return(TPT)
}

CWT.calculation <- function(MT, MP, Mclim = NULL, MPS.ACA.Biom = NULL, Accep.seuil, Remove.biom){
  #### Settings ####
  if(missing(Remove.biom)){Remove.biom = NULL}
  #### Clean matrix ####
  # Keep.real.names <- row.names(MP)
  # MP <- data.frame(t(MP))
  # names(MP) <- Keep.real.names
  MP$id <- row.names(MP)
  names(MT) <- gsub("X", "", names(MT))
  if(any(grepl("TRY", names(MT)) == T) == F){names(MT) <- paste("TRY", names(MT), sep = "_")}
  names(MT)[1] <- "id"
  MT <- MT[which(MT$id %in% MP$id),]
  # /!\ missing taxa ! 
  Missing.taxa <- setdiff(MP$id, MT$id)
  if(length(Missing.taxa) > 0){
    print("The following taxa are missing from the trait matrix:")
    print(Missing.taxa)
    }
  
  Missing.raw <- MT[setdiff(MP$id, MT$id),]
  Missing.raw$id <- setdiff(MP$id, MT$id)
  MT <- rbind(MT, Missing.raw)
  MT <- as_tibble(lapply(MT, function(x){x[is.nan(x)] <- NA ; x}))
  
  #### Merge MP + MT ####
  MPT <- merge(MT, MP, by = "id", all = T)
  MPT <- melt(MPT, id = names(MT))
  names(MPT)[names(MPT) == "variable"] <- "Site"
  names(MPT)[names(MPT) == "value"] <- "FA"
  
  #### Calculation of the CWT ####
  MCWT <- data.frame(Site = (unique(MPT["Site"])))
  MCWT.stat <- setNames(data.frame(NA,NA,NA), c("Trait", "Pour.sites.kept", "N.site"))
  MCWT.stat <- MCWT.stat[-1,]
  Tot.site.nb <- length(levels(MPT$Site))
  
  #### Verbose ####
  pb = txtProgressBar(min = 1, 
                      max = length(grep("TRY", names(MPT))),
                      width = 40,
                      initial = 0,  style = 3) 
  
  init <- numeric(length(grep("TRY", names(MPT))))
  end <- numeric(length(grep("TRY", names(MPT))))
  
  #### Main loop ####
  for(i in grep("TRY", names(MPT))){
    init[i] <- Sys.time()
    Trait.treat.i <- names(MPT)[i]
    A <- MPT[c("id", Trait.treat.i, "Site", "FA")]
    A$FA[is.na(A[[Trait.treat.i]])] <- NA
    all_abund = aggregate(FA ~ Site, A, sum)
    colnames(all_abund)[2] = "tot_abund"
    Keep.sites <- all_abund[all_abund[2] > Accep.seuil,]
    # print(all_abund[2])
    # print(Trait.treat.i)
    # print(nrow(Keep.sites))
    if(nrow(Keep.sites) > 0){
      N.site <- length(Keep.sites$tot_abund)
      Pourc.site.up.seuil <- round(length(Keep.sites$tot_abund)/Tot.site.nb, digits = 2)
      MCWT.stat[i,] <- c(Trait.treat.i, Pourc.site.up.seuil, N.site)
      A <- A[which(A$Site %in% Keep.sites$Site),]
      A <- merge(A, all_abund, by = "Site")
      # A$FA <- scale(A$FA/A$tot_abund)
      A$FA <- A$FA/A$tot_abund
      XX <- aggregate(FA * eval(parse(text = Trait.treat.i)) ~ Site, A, sum, na.rm = T)
      # print(typeof(XX$Site))
    }
    else{
      XX <- data.frame(Site = NA, X = "")
    }
    names(XX)[2] <- Trait.treat.i
    # XX$Site <- as.character(XX$Site)
    # print(typeof(XX$Site))
    MCWT <- left_join(MCWT, XX, by = "Site")
    
    end[i] <- Sys.time()
    setTxtProgressBar(pb, i)
    time <- round(seconds_to_period(sum(end - init)), 0)
    est <- length(grep("TRY", names(MPT))) * (mean(end[end != 0] - init[init != 0])) - time
    remainining <- round(seconds_to_period(est), 0)
    cat(paste(" // Execution time:", time,
              " // Estimated time remaining:", remainining), "")
  }
  close(pb)
  
  MCWT.stat <- MCWT.stat[-1,]
  MCWT.stat[nrow(MCWT.stat)+1,] <- c("Average", round(mean(as.numeric(MCWT.stat$Pour.sites.kept)), digits = 2), round(mean(as.numeric(MCWT.stat$N.site)), digits = 0))
  
  #### Merge CWT + climat ####
  if(is.null(Mclim) == F){
    Mclim[["Site"]] <- row.names(Mclim)
    MCWT.clim = merge(MCWT, Mclim, by = "Site")    # On fusionne les matrices CWM et CLIMAT
    
    if(is.null(MPS.ACA.Biom) == F){
      MPS.ACA.Biom[["Site"]] <- row.names(MPS.ACA.Biom)
      MCWT.clim = merge(MCWT.clim, MPS.ACA.Biom, by = intersect(names(MCWT.clim), names(MPS.ACA.Biom)))    # On fusionne les matrices CWM et CLIMAT
      
    }
    if(is.null(Remove.biom) == F){MCWT.clim <- MCWT.clim[setdiff(seq(1,nrow(MCWT.clim)), which(MCWT.clim$Biome %in% Remove.biom)),]}
    }
  
  #### Export ####
  if(is.null(Mclim) == F){
    Lexport <- list(MCWT = MCWT.clim, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)}
  if(is.null(Mclim) == T){
    Lexport <- list(MCWT = MCWT, MCWT.stat = MCWT.stat, Missing.taxon = Missing.taxa)}
  
  return(Lexport)
}

LR.CWM <- function(MP, MV, Scale, Max_seuil, Keep.taxa = NULL, Cluster = NULL, Save.plotly = F, Dot.size = 5, Dot.opac = 1,
                   Legend.position = "left", Num.facet = NULL, R2.pos, Return.plot = F, H, W, Save.plot = NULL){
  #### Settings ####
  if(missing(Max_seuil)){Max_seuil = NULL}
  if(missing(R2.pos)){R2.pos = "bottomleft"}
  if(missing(Scale)){Scale = NULL}
  library(reshape2)
  
  #### Correspondance and clean ####
  # Sites.MP <- MP$Site
  # Sites.MV <- MV$Site
  # MP <- data.frame(t(MP))
  # MV <- data.frame(t(MV))
  # names(MP) <- Sites.MP
  # names(MV) <- Sites.MV
 
  
  Keep.taxa <- c("Site", Keep.taxa)
  MP <- MP[intersect(names(MP), names(MV))]
  MV <- MV[intersect(names(MP), names(MV))]
  #### Fusion taxons rares #### 
  
  if(is.null(Keep.taxa) == F){
    if(is.null(Cluster) == F){Keep.taxa <- c(Cluster, Keep.taxa)}
    MP <- MP[match(Keep.taxa,names(MP))]
    MV <- MV[match(Keep.taxa,names(MV))]
  }
  else{
    # print(names(MV))
    # print(names(MP))
    Keep.taxa <- c(unique(union(names(MV), names(MP))))
  }
  
  #### Traits labels ####
  names(MV)[names(MV) == "TRY_LeafN"] <- "TRY_N[leaf_]"
  names(MP)[names(MP) == "TRY_LeafN"] <- "TRY_N[leaf_]"
  
  if(is.null(Num.facet) == F){
    Nb.traits <- length(names(MP)[grep("TRY_", names(MP))])
    New.names <- names(MP)[grepl("TRY_", names(MP))]
    for(i in 1:length(New.names)){
      New.names[i] <- paste(gsub(")", "", Num.facet), i, ")~", New.names[i], "[CWM]", sep = "")
    }
    names(MP) <- c(names(MP)[!grepl("TRY_", names(MP))], New.names)
    names(MV) <- c(names(MV)[!grepl("TRY_", names(MV))], New.names)
    
    
  }
  
  names(MP) <- gsub("TRY_", "", names(MP))
  names(MV) <- gsub("TRY_", "", names(MV))
  
  
  print(names(MP))  
  
  #### MP and MV melt ####
  if(is.null(Cluster) == F){
    names(MP)[1] <- "Cluster"
    names(MV)[1] <- "Cluster"
    MP <- reshape2::melt(MP, id = c("Site", "Cluster"))
    MV <- reshape2::melt(MV, id = c("Site", "Cluster"))
  }
  else{
    MP <- reshape2::melt(MP, id = "Site")
    MV <- reshape2::melt(MV, id = "Site")
  }
  
  MP$Type <- "Pollen" 
  MV$Type <- "Vegetation" 
  
  #### Plot LR MP vs. MV ####
  # MP <- subset(MP, select = -c(Type))
  # MV <- subset(MV, select = -c(Type))
  # MT <- left_join(MV, MP, by = c("Cluster", "variable"))
  MT <- rbind(MV, MP)
  MT <- dcast(MT, ...  ~ Type, value.var = "value")
  
  # print(head(MT))
  # names(MT)[c(3,4)] <- c("Vegetation", "Pollen")
  
  # MT$value <- as.double(MT$value)
  MT$Vegetation <- as.double(MT$Vegetation)
  MT$Pollen <- as.double(MT$Pollen)
  #### Add R2 ####
  if(R2.pos == "bottomleft"){
    R2.y = "bottom"
    R2.x = "left"}
  if(R2.pos == "bottomright"){
    R2.y = "bottom"
    R2.x = "right"}
  if(R2.pos == "topleft"){
    R2.y = "top"
    R2.x = "left"}
  if(R2.pos == "bottomright"){
    R2.y = "bottom"
    R2.x = "right"}
  if(R2.pos == "none"){
    R2.y = "none"
    R2.x = "none"}
  
  Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, color = "turquoise4", size = 3.5, small.r = F, na.rm = T,
                         aes(x = Vegetation, y = Pollen, label =  sprintf("%s*\", \"*%s" ,
                                              after_stat(rr.label),
                                              # after_stat(r.squared),
                                              after_stat(p.value.label)
                         )))
  
  #### Param graph ####
  Min.lim <- min(min(MT$Pollen, na.rm = T), min(MT$Vegetation, na.rm = T), na.rm = T)
  Max.lim <- max(max(MT$Pollen, na.rm = T), max(MT$Vegetation, na.rm = T), na.rm = T)
  
  #### Color and fill settings ####
  Value.bi <- c(
    "Boreal Forests/Taiga" = "#8FB8E6",
    "Deserts & Xeric Shrublands" = "#C88282",
    "Montane Grasslands & Shrublands" = "#D0C3A7",
    "Temperate Broadleaf & Mixed Forests" = "#3E8A70",
    "Temperate Conifer Forests" = "#6B9A88",
    "Temperate Grasslands, Savannas & Shrublands" = "#ECED8A",
    "N/A" = "#FFEAAF",
    "Tundra" = "#A9D1C2",
    "Tropical & Subtropical Coniferous Forests" = "#99CA81",
    "Mangroves" = "#FE01C4",
    "Flooded Grasslands & Savannas" = "#BEE7FF",
    "Light taiga" = "#1874CD",
    "Dark taiga" = "#658D94",
    "Steppe-desert" = "#DD5925",
    "Desert-steppe" = "#DD5925",
    "Desert" = "#CD2626",
    "Steppe" = "#EE8D25",
    "Alpine meadow" = "#FFC125",
    "Steppe" = "#1874CD",
    "Desert" = "firebrick3", 
    "Desert-steppe" = "darkorange", 
    "Mountain steppes meadows"= "darkorange", 
    "Forest-steppes"= "#6789CE", "Juniperus woodland"= "dodgerblue3",
    "Forest" = "darkgreen", "Riparian forest" = "darkgreen", 
    "Steppe" = "goldenrod1",
    "Forest-steppe" = "#38A700", 
    "Woodland" = "#9FBB67",
    "Alpine steppe" = "dodgerblue3",
    "Anthropic" = "grey10",
    "Steppe-forest" = "#B2A75C",
    "Forest-steppe" = "#B2A75C", 
    "Chol cold desert-steppes" = "#7916C4", 
    "Tugai riparian forest" = "#BB0268", 
    "Chol warm deserts" = "#bb0202", 
    "Adyr desert-steppes" = "#ff5400", 
    "Adyr steppes" = "#e6c607", 
    "Tau riparian forest" = "#2C9740", 
    "Tau thermophilous woodlands" = "#85682D", 
    "Tau juniper steppe-forest" = "#176E5B",
    "Tau steppes" = "#bab133",
    "Alau cryophilous steppe-forest" = "#54a697",
    "Alau meadows" = "#197CDA" 
  )
  
  Value.bi <- Value.bi[which(names(Value.bi) %in% unique(MT$Cluster))]
  My_fill <-  scale_color_manual(values = Value.bi, name = "") # "Biomes (Dinerstein et al., 2017)"#, labels = levels(ACA.biom.proj$BIOME_NAME)
  
  #### Plot ####
  # Plot.compa <- ggplot(MT, aes(x = Vegetation, y = Pollen, color = Cluster, label = Site)) +
  # Plot.compa <- ggplot(MT, aes(x = Vegetation, y = Pollen)) +
  Plot.compa <- ggplot(MT) +
    geom_ribbon(data = data.frame(x = c(-Inf,+Inf), ymin = c(-Inf,-Inf), ymax = c(-Inf, +Inf)), aes(x=x, ymin=ymin, ymax=ymax), fill="#6D956F", alpha=0.25)+
    geom_ribbon(data = data.frame(x = c(-Inf,+Inf), ymax = c(+Inf,+Inf), ymin = c(-Inf, +Inf)), aes(x=x, ymin=ymin, ymax=ymax), fill="#BEA33A", alpha=0.25)+
    geom_point(data = MT, mapping =  aes(x = Vegetation, y = Pollen, color = Cluster), size = Dot.size, alpha = Dot.opac, na.rm = T) + 
    # facet_wrap(vars(variable), scales = Scale)+
    facet_grid(cols = vars(variable), scales = Scale, labeller = label_parsed)+
    Add.r2 + lims(x = c(Min.lim, Max.lim), y = c(Min.lim, Max.lim))+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey20")+
    geom_smooth(data = MT, mapping = aes(x = Vegetation, y = Pollen), method = "lm", se = F, span = 1000, na.rm = T, linetype = "dashed", linewidth = .5, formula = y ~ x)+
    My_fill +
    labs(x = "Vegetation fractional abundance (%)", y = "Pollen fractional \n abundance (%)", fill = "Sample Type")+
    theme(
      axis.text.y = element_text(vjust = 1, hjust = 1, size = 9),
      axis.text.x = element_text(size = 9), axis.title = element_text(size=12),
      axis.line.x = element_line(lineend = "butt", color = "grey70"), legend.position = Legend.position,
      axis.ticks = element_line(lineend = "butt", color = "grey70"), 
      panel.background=element_blank(), legend.key = element_blank(), panel.spacing = unit(0.2, "lines"),
      panel.border = element_rect(fill = NA), strip.background = element_blank(),
      strip.placement = "outside", strip.text = element_text(size = 13))
  
  #### Plotly interactive graph ####
  if(Save.plotly == T){
    library(plotly)
    library(htmlwidgets)
    
    Ylim = c(0,1)
    My_title <- NULL
    # print(MGDGT[Pclim])
    # MGDGT <- cbind(MGDGT)
    # fig <- Ptot %>% plot_ly()
    # print(MGDGT)
    # fig <- plot_ly(MGDGT, x = MGDGT[Pclim], y = MGDGT[Age.select], type = 'scatter',
    #                # color = MGDGT[[Select.type]], 
    #                # symbol = "ka",
    #                symbol = MGDGT$Top,
    #                mode = 'markers',
    #                text = ~paste('Depth: ', Top)#, #textposition = NULL,
    #                # text = ~paste('Site: ', row.names(MGDGT), "\n Biome:", MGDGT[[Select.type]], sep = "")#, #textposition = NULL,
    #                # textfont = list(color = '#000000', size = 16)
    # )
    # fig <- fig %>% layout(title = My_title,
    #                       xaxis = list(#title = My_title,
    #                         zeroline = F,
    #                         range = Xlim),
    #                       yaxis = list(color = "grey30", #title = 'Brain Weight (g)',
    #                                    range = Ylim)
    #                       )
    # print("pouet")
    fig <- ggplotly(Plot.compa,)
    #### Save html ####
    Save.plot.html <- gsub("pdf", "html", Save.plot)
    print(Save.plot.html)
    Keep.name <- gsub(".*\\/", "", Save.plot.html)
    Path.root <- paste(gsub(Keep.name, "", Save.plot.html), "HTML_files/", sep = "")
    if(file.exists(Path.root) == F){dir.create(Path.root)}
    Save.plot.html <- paste(Path.root, Keep.name, sep = "")
    saveWidget(ggplotly(fig), file = Save.plot.html)
  }
  # print(Plot.compa)
  # 
  #### Save plot and export ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(Plot.compa, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
    else{ggsave(Save.plot)}}
  
  if(Return.plot == T){return(Plot.compa)}
  else{return(MT)}
}

LR.CWM.clim <- function(MT, Meco, Keep.taxa, Keep.clim, 
                          Strip.lab, R2.pos, H, W, Save.plot){
  #### Libraries ####
  library(tibble)
  library(ggplot2)
  library(ggpmisc)
  library(dplyr)
  library(reshape2)
  
  #### Settings ####
  # if(missing(Select.plot)){Select.plot = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Keep.taxa)){Keep.taxa = NULL}
  if(missing(Keep.clim)){Keep.clim = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Strip.lab)){Strip.lab = F}
  if(missing(R2.pos)){R2.pos = "bottomleft"}
  # if(missing(Plotly)){Plotly = T}
  # if(missing(Plotly.all)){Plotly.all = F}
  
  # print(class(MT))
  if(class(MT) == "list"){
    # print(MT[[1]])
    print(names(MT)[1])
  }
  else{
    MT <- list(My_site = MT)
    Meco <- list(My_site = Meco)
  }
  #### Select taxa and clim param ####
  Sites.MP <- MP$Site
  Sites.MV <- MV$Site
  MP <- data.frame(t(MP))
  MV <- data.frame(t(MV))
  names(MP) <- Sites.MP
  names(MV) <- Sites.MV
  # MP <- MP[, intersect(names(MP), names(MV))]
  # MV <- MV[, intersect(names(MP), names(MV))]   
  
  MT.clean <- data.frame(matrix(NA, nrow = 1, ncol = 6))
  names(MT.clean) <- c("rowname", "Veg.lab", "Veg.val", "variable", "value", "Country")
  MT.clean <- MT.clean[-1,]
  
  for(i in 1:length(MT)){
    MT.i <- MT[[i]]
    Meco.i <- Meco[[i]]
    
    
    if(is.null(Keep.taxa) == F){MT.i <- MT.i[Keep.taxa,]}
    else{
      MT.i <- MT.i[c(1:4),]
      print("Only the first 4 taxa have been displayed. Select ***Keep.taxa*** if you want.")
    }
    
    if(is.null(Keep.clim) == T){
      Keep.clim = c("MAP", "MAAT")
      print("Only the MAAT and MAP have been displayed. Select ***Keep.clim*** if you want.")}
    Meco.i <- Meco.i[, Keep.clim]
    
    
    
    MT.i <- left_join(rownames_to_column(Meco.i), rownames_to_column(data.frame(t(MT.i))), by = c("rowname"))
    MT.i <- melt(MT.i, id = c("rowname", Keep.clim))
    names(MT.i)[c(length(names(MT.i)), (length(names(MT.i))-1))] <- c("Veg.val", "Veg.lab")
    MT.i <- melt(MT.i, id = c("rowname", "Veg.lab", "Veg.val"))
    MT.i <- na.omit(MT.i)
    # print(MT.i)
    MT.i$Country <- names(MT)[i]
    
    MT.clean <- rbind(MT.clean, MT.i)
    
  }
  
  MT <- MT.clean
  # }if(class(MT) == "data.frame"){MT$Country <- "My-study"}
  
  # print(MT)
  
  #### Graphical settings ####
  values.bi = c("Azerbaijan" = "#98312eff",
                "WAST" = "#98312eff",
                "Mongolia" =  "royalblue",
                "COST" =  "royalblue",
                "Tajikistan" = "#3fa587ff",
                "Uzbekistan" = "#84761cff"
  )
  
  values.bi <- values.bi[which(names(values.bi) %in% unique(MT$Country))]
  Scale.fill <- scale_color_manual(values = values.bi, name = "Country", drop = T)
  #### Add R2 ####
  if(R2.pos == "bottomleft"){
    R2.y = "bottom"
    R2.x = "left"}
  if(R2.pos == "bottomright"){
    R2.y = "bottom"
    R2.x = "right"}
  if(R2.pos == "none"){
    R2.y = "none"
    R2.x = "none"}
  
  Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, 
                         size = 2.4, small.r = F, vstep = 0.07, p.digits = 1, na.rm = T,  
                         aes(label =  sprintf("%s*\", \"*%s" ,
                                              after_stat(rr.label),
                                              # after_stat(r.squared),
                                              after_stat(p.value.label)
                         )))
  
  #### Annotations names Strig.lab = F ####
  if(Strip.lab == F){
    Strip.lab.disp <- element_blank()
    S.trait <- setNames(data.frame(as.factor(unique(MT$variable)), rep(1,nlevels(MT$variable)), rep(1,nlevels(MT$variable))), c("Lab", "x","y"))
    S.clim <- setNames(data.frame(as.factor(unique(MT$Veg.lab)), rep(1,nlevels(MT$Veg.lab)), rep(1,nlevels(MT$Veg.lab))), c("Lab", "x","y"))
    
    Theme.null <- theme(axis.line = element_blank(), axis.title = element_blank(),
                        strip.text = element_blank(), axis.text = element_blank(),plot.margin = unit(c(0,0,0,0), 'cm'),
                        axis.ticks = element_blank(), plot.background = element_blank(),
                        panel.grid = element_blank(), panel.background = element_blank())
    
    p.up <- ggplot(S.trait, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", ncol = length(unique(MT$variable)))+
      geom_text(aes(label = Lab))+ Theme.null
    
    p.right <- ggplot(S.clim, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", nrow = length(unique(MT$variable)))+
      geom_text(aes(label = Lab), angle = 270,  hjust=0.5, vjust=1)+ Theme.null
  }
  else{Strip.lab.disp <- element_text(hjust = 0)}
  
  #### Plot ####
  pLR <- ggplot(MT, aes(x = Veg.val, y = value, color = Country))+
    geom_point(size = 1.5, alpha = 0.5, shape = 16)+
    geom_smooth(method = "lm", se = F, span = 1000, size = 0.7, linetype = "dashed",
                formula = y ~ x)+    
    Add.r2 + Scale.fill + 
    xlab("Plant fractional abundances (%)")+
    ylab("Climate parameters")+
    facet_wrap(Veg.lab ~ variable, scales = "free_y")+
    # theme_classic()+theme(panel.border = element_rect(fill = NA), panel.background = element_blank(), strip.background = element_blank())
    #### Theme ####
  theme(
    axis.line = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(fill = NA),
    plot.background = element_blank(), plot.margin = unit(c(0,0,0,0), 'cm'),
    legend.position = "bottom", 
    panel.grid = element_blank(), 
    strip.text = Strip.lab.disp,
    strip.background = element_blank(),
  )
  
  #### Export ####
  if(Strip.lab == F){pLR <- p.up + plot_spacer() + pLR + p.right + plot_layout(nrow = 2, heights = c(1/40,39/40), widths = c(39/40,1/40))}
  print(pLR)
  
  ggsave(pLR, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
}

Checklist.cleaning <- function(Import.checklist = NULL, TNRS.check = T, Remove.non.pollinic = T, Remove.subsp = T,
                               GIFT.shape.file = NULL, BIEN.shape.file = NULL, TL.APG.Save.RDS = NULL, TL.Save.RDS = NULL){
  #### Choice ####
  Remove.famille <- c("", NA, "Aspleniaceae", "Athyriaceae", "Blechnaceae", "Osmundaceae", "Equisetaceae", 
                      "Pteridaceae", "Polypodiaceae", "Woodsiaceae", "Unknown", "Selaginellaceae",
                      "Lycopodiaceae", "Dryopteridaceae", "Dennstaedtiaceae", "Cystopteridaceae",
                      "Davalliaceae", "Gleicheniaceae", "Hymenophyllaceae", "Lindsaeaceae", "Ophioglossaceae",
                      "Marsileaceae", "Salviniaceae", "Thelypteridaceae", "Tectariaceae", "Lygodiaceae", "Onocleaceae")
  
  #### TNRS function build ####
  TNRS_function <- function(M) {
    library(TNRS)
    print(paste("Checklist cleaned", deparse(substitute(M)), "by the TNRS API request:"))
    M <- M[!duplicated(M)]
    results <- data.frame()
    Step = 1000
    #### Loops pour bp de taxa ####
    for(i in 1:ceiling(length(M)/Step)){
      Min <- 1 + (i - 1)*Step
      Max <- i*Step
      if(Max > length(M)){Max = length(M)}
      print(paste("From species", Min, "to", Max))
      Add.list <- TNRS(M[Min:Max])
      results <- rbind(results, Add.list)
    }
    print(paste("Number of species cleaned by the TNRS:", nrow(results), sep = " "))
    results$Accepted_family[results$Taxonomic_status == "No opinion"] <- results$Name_matched_accepted_family[results$Taxonomic_status == "No opinion"] 
    results$Accepted_name[results$Taxonomic_status == "No opinion"] <- results$Name_matched[results$Taxonomic_status == "No opinion"] 
    results <- results[c(2,3,15,14,21,41,33,34,7)]
    names(results) <- c("Name_submitted", "Score", "Score_G", "TNRS_Genus", "Score_F", "TNRS_Family", "Status_taxo", "Accepted_name", "Rank")
    
    #### Clean data ####
    M <- left_join(data.frame(Name_submitted = M), results, by = "Name_submitted")
    M <- M[!duplicated(M),]
    M <- M[M$Score > 0.9,]
    M$Genera <- gsub("\\s.*", "", M$Accepted_name)
    M <- M[M$Rank == "species",] 
    M <- M[c(6,10,8)]
    names(M) <- c("family", "genus", "species")
    return(M)
    
  }
  
  #### Imported checklist ####
  if(is.null(Import.checklist) == F){
    if(TNRS.check == T){
      if(ncol(Import.checklist) > 1){
        Import.checklist <- Import.checklist[[ncol(Import.checklist)]]}
      print(paste("Number of species imported:", length(Import.checklist), sep = " "))
      ChL.import <- TNRS_function(Import.checklist)}
    else{
      if(ncol(Import.checklist) == 3){ChL.import <- Import.checklist}
      else(warning("If TNRS == F, you need to provide the following columns: family, genus, species"))
      }
    }
  else{
    ChL.import <- NULL
    if(TNRS.check == T){print("TNRS == T but not checklist imported")}}
  
  #### GIFT checklist extract ####
  if(is.null(GIFT.shape.file) == F){
    library("GIFT")
    library("sf")
    print("Checklist extraction from GIFT API request")
    aca_shp <- suppressMessages(st_read(GIFT.shape.file))
    aca_shp <- suppressWarnings(st_buffer(aca_shp, dist = 0))
    sf_use_s2(FALSE)
    aca <- GIFT_checklists(taxon_name = "Spermatophyta", shp = aca_shp)
    ChL.GIFT <- aca$checklists
    print(paste("Number of species extracted from GIFT:", length(unique(aca$checklists$work_species)), sep = " "))
    
    if(TNRS.check == T){ChL.GIFT <- TNRS_function(unique(aca$checklists$work_species))}
    else{
      ChL.GIFT <- ChL.GIFT[c("family","work_species")]
      ChL.GIFT$genus <- gsub("\\s.*", "", ChL.GIFT$work_species)
      ChL.GIFT <- ChL.GIFT[c(1,3,2)]
      names(ChL.GIFT) <- c("family", "genus", "species")
      }
    
    }
  
  #### BIEN occurance extract ####
  if(is.null(BIEN.shape.file) == F){
    library(BIEN)
    ChL.BIEN <- BIEN_occurrence_country(BIEN.shape.file)
    ChL.BIEN <- ChL.BIEN[,1:6]
    ChL.BIEN <- unique(ChL.BIEN$scrubbed_species_binomial)
    ChL.BIEN <- ChL.BIEN[!is.na(ChL.BIEN)]
    ChL.BIEN <- TNRS_function(ChL.BIEN)
    }
  
  #### Merging all extractions ####
  if(is.null(BIEN.shape.file) == T & is.null(GIFT.shape.file) == T & is.null(ChL.import) == F){TL.clean <- ChL.import}
  if(is.null(BIEN.shape.file) == T & is.null(GIFT.shape.file) == F & is.null(ChL.import) == T){TL.clean <- ChL.GIFT}
  if(is.null(BIEN.shape.file) == F & is.null(GIFT.shape.file) == T & is.null(ChL.import) == T){TL.clean <- ChL.BIEN}
  if(is.null(BIEN.shape.file) == F & is.null(GIFT.shape.file) == F & is.null(ChL.import) == T){TL.clean <- rbind(ChL.BIEN, ChL.GIFT)}
  if(is.null(BIEN.shape.file) == F & is.null(GIFT.shape.file) == T & is.null(ChL.import) == F){TL.clean <- rbind(ChL.BIEN, ChL.import)}
  if(is.null(BIEN.shape.file) == T & is.null(GIFT.shape.file) == F & is.null(ChL.import) == F){TL.clean <- rbind(ChL.GIFT, ChL.import)}
  if(is.null(BIEN.shape.file) == F & is.null(GIFT.shape.file) == F & is.null(ChL.import) == F){TL.clean <- rbind(ChL.BIEN, ChL.GIFT, ChL.import)}
  
  #### Remove varieties ####
  TL.clean <- TL.clean[!duplicated(TL.clean),]
  TL.clean <- na.omit(TL.clean)
  TL.clean <- TL.clean[TL.clean$family != "",]
  if(Remove.subsp == T){
    TL.clean$species <- gsub(" subsp.\\s.*", "", TL.clean$species)
    TL.clean$species <- gsub(" var.\\s.*", "", TL.clean$species)
    TL.clean$species <- gsub(" fo.\\s.*", "", TL.clean$species)
    TL.clean <- TL.clean[!duplicated(TL.clean),]
  }
  
  #### Check family fake names ####
  if(Remove.non.pollinic == T){TL.clean <- TL.clean[!TL.clean$family %in% Remove.famille,]}
  TL.clean$family[which(TL.clean$family == "Chenopodiaceae")] <- "Amaranthaceae"
  TL.clean$family[which(TL.clean$family == "Viburnaceae")] <- "Adoxaceae"
  TL.clean$family[which(TL.clean$family == "Petiveriaceae")] <- "Phytolaccaceae"
  TL.clean$family[which(TL.clean$family == "Corbichoniaceae")] <- "Aizoaceae"
  TL.clean$family[which(TL.clean$family == "Compositae")] <- "Asteraceae"
  TL.clean$family[which(TL.clean$family == "Leguminosae")] <- "Fabaceae"
  
  #### Check if genus in several families ####
  Pb.gen <- c()
  for(gen in unique(TL.clean$genus)){
    A <- TL.clean[TL.clean$genus == gen,]
    Fam <- unique(A$family)
    if(length(Fam) > 1){
      Pb.gen <- append(Pb.gen, gen)
    }
  }
  
  if(length(Pb.gen) > 0){stop("Some genus are in more than one family !!!")}
  
  #### Add the subfamily / subgenera ####
  Aster <- read.csv(file = "Import/World_DB/Taxonomie/Asteraceae.csv", sep = "\t", header = F)    # from NCBI Nat. Cent. Biotech. Info
  Pinus <- read.csv(file = "Import/World_DB/Taxonomie/Pinus_subgenre.csv", sep = ",", header = T) # from NCBI Nat. Cent. Biotech. Info
  Cerealia <- read.csv(file = "Import/World_DB/Taxonomie/Cerealia-type.csv", sep = ",", header = T)
  Quercus <- read.csv(file = "Import/World_DB/Taxonomie/Quercus_phenology.csv", sep = ",", header = T)
  Rosaceae <- data.frame(readRDS("Import/World_DB/Taxonomie/Rosaceae_type.Rds"))
  Ephedra <- read.csv(file = "Import/World_DB/Taxonomie/Ephedra.csv", sep = ",", header = T)
  
  A = Aster[match(TL.clean$genus, Aster$V1),2]
  B = Pinus[match(TL.clean$species, Pinus$Espece),2]
  D = Quercus[match(TL.clean$species, Quercus$Espece),3]
  E = Cerealia[match(TL.clean$genus, Cerealia$Genre),4]
  G = Cerealia[match(TL.clean$species, Cerealia$Espece),4]
  H = Rosaceae[match(TL.clean$species, Rosaceae$species),4]
  I = Ephedra[match(TL.clean$species, Ephedra$species),2]
  C = data.frame(A, B, D, E, G, H, I)
  C <- mutate(C, Other.clade = coalesce(A,B,D,E,G, H, I))
  TL.clean$Other.clade <- C$Other.clade
  
  #### APGIV ####
  APGIV <- read.csv(file = "Resultats/World_DB/Taxonomie/APG_IV_clean.csv", header = T, row.names = 1)
  
  TL.APG <- left_join(TL.clean, APGIV[-c(4)], by = "family", relationship = "many-to-many")
  TL.APG <- TL.APG[c(6,5,4,1,2,3)]
  TL.APG$subreign <- NA 
  TL.APG$subreign[!is.na(TL.APG$order)] <- "Angiospermes"
  
  TL.APG$order[TL.APG$family == "Cycadaceae"] <- "Cycadales"
  TL.APG$order[TL.APG$family == "Ginkgoaceae"] <- "Ginkgoales"
  TL.APG$order[TL.APG$family == "Araucariaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Cupressaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Pinaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Podocarpaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Ephedraceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Gnetaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Taxaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Sciadopityaceae"] <- "Pinales"
  TL.APG$order[TL.APG$family == "Asphodelaceae"] <- "Asparagales"
  
  TL.APG$subreign[is.na(TL.APG$kingdom)] <- "Gymnospermes"
  TL.APG$kingdom[is.na(TL.APG$kingdom)] <- TL.APG$order[is.na(TL.APG$kingdom)]
  TL.APG <- TL.APG[!duplicated(TL.APG),]
  TL.APG <- TL.APG[order(TL.APG$family, TL.APG$genus, TL.APG$species),]
  
  TL.APG <- TL.APG[c(7,1:6)]
  TL <- TL.APG[c(5,4,6,7)]
  print(paste("FINAL number of species in the checklist:", nrow(TL), sep = " "))
  
  #### Export ####
  if(is.null(TL.APG.Save.RDS) == F){
    Path.to.create <- gsub("(.*/).*\\.Rds.*","\\1", TL.APG.Save.RDS)
    dir.create(file.path(Path.to.create), showWarnings = F)
    saveRDS(TL.APG, TL.APG.Save.RDS)}
  
  if(is.null(TL.Save.RDS) == F){
    Path.to.create <- gsub("(.*/).*\\.Rds.*","\\1", TL.Save.RDS)
    dir.create(file.path(Path.to.create), showWarnings = F)
    saveRDS(TL, TL.Save.RDS)}
  
  Export <- list(TL = TL, TL.APG = TL.APG)
  return(Export)
  
}

DB.trait.extraction <- function(TL = NULL, Projet.name = "Projet1", Extract.BIEN = F, Extract.GIFT = F,
                                Extract.TRY = F, Extract.BROT = F, Chinese.trait.DB = F){
  #### Import BIEN  ####
  if(Extract.BIEN == T){
    library("BIEN")
    BIEN.tr.ACA <- BIEN_trait_species(species = TL$species)
    Save.path = paste(DB.path, "Vegetation/Occurences/BIEN/BIEN_", Projet.name, "_trait.Rds", sep = "")
    print(paste("BIEN traits for", Projet.name, "save at the path:", Save.path))
    saveRDS(BIEN.tr.ACA, Save.path)
  }
  
  #### Import TRY ####
  if(Extract.TRY == T){
    library("vroom")
    if(exists("TL.TRY") == F){TL.TRY <- readRDS(paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_plantlist.Rds", sep = ""))}
    if(exists("TRY") == F){TRY <- vroom::vroom(file = paste(DB.path, "Traits/TRY_dec_2019/8066.csv", sep = ""))}
    TL.TRY.ACA <- intersect(TL.TRY$AccSpeciesName, TL$species)
    TRY.ACA <- TRY[which(TRY$AccSpeciesName %in% TL.TRY.ACA),]
    
    Save.path = paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_taxa_", Projet.name, ".Rds", sep = "")
    print(paste("TRY traits for", Projet.name, "save at the path:", Save.path))
    saveRDS(TRY.ACA,  Save.path)
  }
  
  #### Import BROT 2.0 + homog ####
  if(Extract.BROT == T){
    brot <- read.csv("Import/World_DB/Traits/BROT2.0/BROT2_dat.csv", row.names = 1, stringsAsFactors = F)
    brot.sou <- read.csv("Import/World_DB/Traits/BROT2.0/BROT2_sou.csv", row.names = 1, stringsAsFactors = F, encoding = "UTF-8")
    brot.tax <- read.csv("Import/World_DB/Traits/BROT2.0/BROT2_tax.csv", row.names = 1, stringsAsFactors = F, encoding = "UTF-8")
    brot.syn <- read.csv("Import/World_DB/Traits/BROT2.0/BROT2_syn.csv", stringsAsFactors = F, encoding = "UTF-8")
    TL.BROT.ACA <- intersect(brot$Taxon, TL$species)
    brot.ACA <- brot[which(brot$Taxon %in% TL.BROT.ACA),]
    Save.path = paste("Import/World_DB/Traits/BROT2.0/BROT_", Projet.name, ".Rds", sep = "")
    print(paste("Brot traits for", Projet.name, "save at the path:", Save.path))
    saveRDS(brot.ACA, Save.path)
  }
  
  #### Import Chinese trait DB + homog ####
  if(Chinese.trait.DB == T){
    CPT.trait.conti <- as_tibble(read.csv("Import/China/Traits/China_Plant_TraitsDB_csv/Hard Traits.csv", row.names = 1, stringsAsFactors = F))
    CPT.trait.unconti <- as_tibble(read.csv("Import/China/Traits/China_Plant_TraitsDB_csv/Morphometric traits.csv", row.names = 1, stringsAsFactors = F))
    CPT.trait.pathway <- as_tibble(read.csv("Import/China/Traits/China_Plant_TraitsDB_csv/Photo Pathway.csv"))
    CPT.PFT <- as_tibble(read.csv("Import/China/Traits/China_Plant_TraitsDB_csv/PFT data.csv"))
    CPT.species.samp <- as_tibble(read.csv("Import/China/Traits/China_Plant_TraitsDB_csv/Species translations.csv"))
    CPT.species.samp$Species <- paste(CPT.species.samp$ACCEPTED.GENUS, CPT.species.samp$ACCEPTED.SPECIES, sep = " ")
    TL.CPT.ACA <- intersect(CPT.species.samp$Species, TL$species)
    CPT.species.samp <- CPT.species.samp[which(CPT.species.samp$Species %in% TL.CPT.ACA),]
    
    CPT.aggreg <- function(M, Aggeg.by, Aggreg){
      M <- merge(CPT.species.samp[c(Aggeg.by, "Species")], M[which(M[[Aggeg.by]] %in% CPT.species.samp[[Aggeg.by]]),], by = Aggeg.by)
      M <- M[-1]
      if(Aggreg == T){
        M <- aggregate(M, list(M$Species), FUN = mean, na.action = na.pass, na.rm = T)
        names(M)[1] <- "Species"
        M <- M[-2]}
      return(as_tibble(M))
    }
    
    CPT.trait.conti.ACA <- CPT.aggreg(CPT.trait.conti, "SAMPLE.ID", Aggreg = T)
    CPT.trait.unconti.ACA <- CPT.aggreg(CPT.trait.unconti, "SAMPLE.ID", Aggreg = F)
    CPT.PFT.ACA <- CPT.aggreg(CPT.PFT, "SAMPLE.ID", Aggreg = F)
    CPT.pathway.ACA <- CPT.aggreg(CPT.trait.pathway, "SPECIES.ID", Aggreg = F)
    CPT.ACA <- merge(CPT.trait.conti.ACA, CPT.trait.unconti.ACA, by = "Species", all = T)
    CPT.ACA <- merge(CPT.ACA, CPT.PFT.ACA, by = "Species", all = T)
    CPT.ACA <- merge(CPT.ACA, CPT.pathway.ACA, by = "Species", all = T)
    CPT.ACA <- CPT.ACA[!duplicated(CPT.ACA),]
    Save.path = paste("Import/China/Traits/China_Plant_TraitsDB_csv/CPT_", Projet.name, ".Rds", sep = "")
    print(paste("Chinese DB traits for", Projet.name, "save at the path:", Save.path))
    saveRDS(CPT.ACA, Save.path)
    }
  
  #### Import GIFT ####
  if(Extract.GIFT == T){
    library("GIFT")
    GIFT <- readRDS("Import/ACA/Traits/GIFT/20210617_central_asia_traits.rds")
    GIFT <- GIFT[GIFT$species %in% TL$species,]
    # Traitslistes <- names(GIFT.tr.Proj)[-c(1,ncol(GIFT.tr.Proj))]
    # Add.GIFT.API <- GIFT_traits(trait_IDs = Traitslistes)
    # Add.GIFT.API <- Add.GIFT.API[Add.GIFT.API$species %in% TL$species,]
    # GIFT.tr.Proj <- reshape2::melt(GIFT.tr.Proj, id = c("work_ID", "species"))
    # names(GIFT.tr.Proj) <- c("work_ID", "species", "trait_ID")
    
    Save.path = paste("Import/ACA/Traits/GIFT/20210617_central_asia_traits_", Projet.name, "_crop.Rds", sep = "")
    print(paste("GIFT traits for", Projet.name, "save at the path:", Save.path))
    saveRDS(GIFT, Save.path)
  }
}

DB.trait.clean <- function(BIEN.path = NULL, TRY.path = NULL, Projet.name = "Projet1"){
  #### TRY clean ####
  if(is.null(TRY.path) == F){
    TRY.ACA <- readRDS(TRY.path)
    TRY.ACA <- TRY.ACA[!(grepl("Guy, A. L., J. M. Mischkolz, and E. G. Lamb.", TRY.ACA$Reference) & TRY.ACA$TraitID %in% 46),]
    TRY.ACA <- TRY.ACA[!(grepl("Wright JP, Sutton", TRY.ACA$Reference) & TRY.ACA$TraitID %in% 46),]
    TRY.ACA[TRY.ACA$TraitID %in% 46 & TRY.ACA$AccSpeciesID %in% 387342,21][3,] <- TRY.ACA[TRY.ACA$TraitID %in% 46 & TRY.ACA$AccSpeciesID %in% 387342,21][3,]/10
    TRY.ACA[TRY.ACA$TraitID %in% 46 & TRY.ACA$AccSpeciesID %in% 31479,21] <- TRY.ACA[TRY.ACA$TraitID %in% 46 & TRY.ACA$AccSpeciesID %in% 31479,21]/10
    TRY.ACA$StdValue[(grepl("Li, Y. and Shipley", TRY.ACA$Reference) & TRY.ACA$TraitID %in% 4)] <- TRY.ACA$StdValue[(grepl("Li, Y. and Shipley", TRY.ACA$Reference) & TRY.ACA$TraitID %in% 4)]*10
    TRY.ACA$StdValue[(TRY.ACA$DatasetID == 415 & TRY.ACA$TraitID %in% 146)] <- TRY.ACA$StdValue[(TRY.ACA$DatasetID == 415 & TRY.ACA$TraitID %in% 146)]/10
    TRY.ACA <- TRY.ACA[setdiff(seq(1,nrow(TRY.ACA)), which(TRY.ACA$TraitID %in% 15 & TRY.ACA$StdValue <= 0.02)),] 
    TRY.ACA <- TRY.ACA[setdiff(seq(1,nrow(TRY.ACA)), which(TRY.ACA$TraitID %in% 14 & TRY.ACA$StdValue == 0)),] 
    TRY.ACA[which(TRY.ACA$TraitID %in% 14 & TRY.ACA$StdValue > 100),"StdValue"] <- TRY.ACA[which(TRY.ACA$TraitID %in% 14 & TRY.ACA$StdValue > 100), "StdValue"]/10
    TRY.ref <- unique(TRY.ACA[c(3,4,26)])
    TRY.ref <- TRY.ref[!duplicated(TRY.ref$DatasetID),]
    TRY.ref <- TRY.ref[!TRY.ref$Reference == "unpub.",]
    TRY.ref <- TRY.ref[order(TRY.ref$DatasetID),]
    write.table(TRY.ref, paste("Resultats/ACA/Traits/TRY_", Projet.name, "ACA_references.csv", sep = ""), sep = ",", row.names = F)
    Save.TRY = gsub(".Rds", "_clean.Rds", TRY.path)
    saveRDS(TRY.ACA)
    
  }
  
  #### BIEN clean ####
  if(is.null(BIEN.path) == F){
    BIEN.tr.ACA <- readRDS(BIEN.path)
    BIEN.tr.ACA <- BIEN.tr.ACA[!grepl("Albrectsen BR", BIEN.tr.ACA$project_pi),]
    BIEN.tr.ACA$trait_value[grepl("We sampled leaves from 5 to 20 random", BIEN.tr.ACA$method) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("We sampled leaves from 5 to 20 random", BIEN.tr.ACA$method) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])/10
    BIEN.tr.ACA$trait_value[grepl("Fu B", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Fu B", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*1000
    BIEN.tr.ACA$trait_value[grepl("Stevens JT", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Stevens JT", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*1000
    BIEN.tr.ACA$trait_value[grepl("Ameztegui", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Ameztegui", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Kurokawa", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Kurokawa", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Edwards EJ", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Edwards EJ", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Mason CM", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Mason CM", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Muir CD", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Muir CD", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Price CA", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Price CA", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Storkey J", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Storkey J", BIEN.tr.ACA$project_pi) & grepl("leaf area per leaf dry", BIEN.tr.ACA$trait_name)])*10^6
    BIEN.tr.ACA$trait_value[grepl("Muir CD", BIEN.tr.ACA$project_pi) & grepl("leaf dry mass per leaf fresh mass", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Muir CD", BIEN.tr.ACA$project_pi) & grepl("leaf dry mass per leaf fresh mass", BIEN.tr.ACA$trait_name)])*10^2
    BIEN.tr.ACA$trait_value[grepl("Milla R", BIEN.tr.ACA$project_pi) & grepl("leaf dry mass per leaf fresh mass", BIEN.tr.ACA$trait_name)] <- as.numeric(BIEN.tr.ACA$trait_value[grepl("Milla R", BIEN.tr.ACA$project_pi) & grepl("leaf dry mass per leaf fresh mass", BIEN.tr.ACA$trait_name)])*10^2
    Save.BIEN = gsub(".Rds", "_clean.Rds", BIEN.path)
    saveRDS(BIEN.tr.ACA, Save.BIEN)
  }
  
  }
  
Pollen.type.2.Checklist <- function(List.PT_sl, List.PT_ss, TL, TPT, Save.path = NULL){
  #### Convertion type pollen #### 
  List.PT_sl.simple <- gsub(" spp.", "", List.PT_sl)
  List.PT_sl.simple <- gsub("-type", "", List.PT_sl.simple)
  TP.sl <- setNames(data.frame(matrix(NA, ncol = length(List.PT_sl), nrow = nrow(TL))), List.PT_sl.simple)
  TP.sl <- cbind(TL, TP.sl)
  Compter = (length(names(TL))+1)
  for(i in Compter:ncol(TP.sl)){
    Type.p <- names(TP.sl)[i]
    if(Type.p %in% TPT$Species){TP.sl[which(TP.sl$species == Type.p),Type.p] <- T}
    if(Type.p %in% TPT$Subgenus){TP.sl[which(TP.sl$Other.clade == Type.p),Type.p] <- T}
    if(Type.p %in% TPT$Genus){TP.sl[which(TP.sl$genus == Type.p),Type.p] <- T}
    if(Type.p %in% TPT$Subfamille){TP.sl[which(TP.sl$Other.clade == Type.p),Type.p] <- T}
    if(Type.p %in% TPT$Family){TP.sl[which(TP.sl$family == Type.p),Type.p] <- T}
    }
  
  TP.sl[6:ncol(TP.sl)][is.na(TP.sl[6:ncol(TP.sl)])] <- F
  names(TP.sl) <- c(names(TL), List.PT_sl) 
  
  List.PT_ss.simple <- gsub(" spp.", "", List.PT_ss)
  List.PT_ss.simple <- gsub("-type", "", List.PT_ss.simple)
  TP.ss <- setNames(data.frame(matrix(NA, ncol = length(List.PT_ss), nrow = nrow(TL))), List.PT_ss.simple)
  TP.ss <- cbind(TL, TP.ss)
  for(i in Compter:ncol(TP.ss)){
    Type.p <- names(TP.ss)[i]
    
    if(grepl("-", Type.p)){Type.p <- strsplit(Type.p, "-")[[1]]}
    if(any(Type.p %in% TPT$Species)){TP.ss[which(TP.ss$species %in% Type.p),Type.p] <- T}
    if(any(Type.p %in% TPT$Subgenus)){TP.ss[which(TP.ss$Other.clade %in% Type.p),Type.p] <- T}
    if(any(Type.p %in% TPT$Genus)){
      TP.ss[which(TP.ss$genus %in% Type.p),Type.p] <- T
      if(length(Type.p) > 1){print(Type.p)}
      }
    if(any(Type.p %in% TPT$Subfamille)){TP.ss[which(TP.ss$Other.clade %in% Type.p),Type.p] <- T}
    if(any(Type.p %in% TPT$Family)){TP.ss[which(TP.ss$family %in% Type.p),Type.p] <- T}
    }
  
  TP.ss[Compter:ncol(TP.ss)][is.na(TP.ss[Compter:ncol(TP.ss)])] <- F
  names(TP.ss) <- c(names(TL), List.PT_ss) 
  
  #### Export new taxon ####
  if(is.null(Save.path) == F){
    Save.path1 <- gsub(".Rds", "_bool_ss.Rds", Save.path)
    Save.path2 <- gsub(".Rds", "_bool_sl.Rds", Save.path)
    saveRDS(TP.ss, file = Save.path1)
    saveRDS(TP.sl,file = Save.path2)
    }
  
  return(list(TP.ss = TP.ss, TP.sl = TP.sl))
}

DB.merge <- function(TRY = NULL, BIEN = NULL, BROT = NULL, GIFT = NULL, TL = NULL, TL.APG = NULL, TBT = NULL, Remove.outliers = T,
                     W = 1700, H = 700, SSD.interpol = F, Save.MT = NULL, Save.plot = NULL, Tab.stats= NULL, Plot.only.continuous = F){
  #### Init val ####
  TBT <- data.frame(read.csv(file=TBT,sep=",",dec=".", header=T))
  MT <- setNames(data.frame(matrix(ncol = c(ncol(TL) + 4))), c(names(TL), "GrowthForm", "variable", "value", "DB"))
  TBT$TRY_lab <- paste("TRY", TBT$TRY_lab, sep = "_")
  #### BIEN ####
  if(is.null(BIEN) == F){
    BIEN <- as_tibble(BIEN[which(BIEN$trait_name %in% TBT$BIEN_lab), c(1:3)]) 
    for(i in 1:nrow(TBT)){BIEN$trait_name[BIEN$trait_name == TBT$BIEN_lab[i]] <- TBT$ID_TRY[i]}
    names(BIEN) <- c("AccSpeciesName", "TraitID", "StdValue")
    MC.BIEN <- Clean.trait.con(BIEN, Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == T)])
    MC.BIEN$`47` <- MC.BIEN$`47`/1000
    MC.BIEN <- full_join(TL, MC.BIEN, by = "species")
    names(BIEN) <- c("AccSpeciesName", "TraitID", "OrigValueStr")
    MD.BIEN <- Clean.trait.discon(BIEN, Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == F)], Keep.string = c(48)) # , 155, 335 /!\ work in progress pour les deux derniers !)
    MC.BIEN <- full_join(MC.BIEN, MD.BIEN, by = c("species"))
    MC.BIEN <- melt(data.frame(MC.BIEN), by = c("species", "48"))
    MC.BIEN$DB <- "BIEN"
    # print(MC.BIEN)
    names(MC.BIEN) <- c(names(TL), "GrowthForm", "variable", "value", "DB")
    MT <- rbind(MT, MC.BIEN)
  }
  
  #### BROT ####
  if(is.null(BROT) == F){
    BROT <- as_tibble(BROT[which(BROT$Trait %in% TBT$BROT_lab), c(2:4)]) 
    for(i in 1:nrow(TBT)){BROT$Trait[BROT$Trait ==TBT$BROT_lab[i]] <- TBT$ID_TRY[i]}
    names(BROT) <- c("AccSpeciesName", "TraitID", "StdValue")
    MC.BROT <- Clean.trait.con(BROT, Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == T)])
    MC.BROT$`47` <- MC.BROT$`47`/1000
    MC.BROT <- full_join(TL, MC.BROT, by = "species")
    names(BROT) <- c("AccSpeciesName", "TraitID", "OrigValueStr")
    MD.BROT <- Clean.trait.discon(BROT, Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == F)]) # , 155, 335 /!\ work in progress pour les deux derniers !)
    MC.BROT <- full_join(MC.BROT, MD.BROT, by = "species")
    MC.BROT <- melt(data.frame(MC.BROT))
    MC.BROT$GrowthForm <- NA
    MC.BROT <- MC.BROT[c(1:4,7,5,6)] 
    MC.BROT$DB <- "BROT"
    names(MC.BROT) <- c(names(TL), "GrowthForm", "variable", "value", "DB")
    MT <- rbind(MT, MC.BROT)
    # print(MC.BROT)
  }
  
  #### TRY ####
  if(is.null(TRY) == F){
    MC.TRY <- Clean.trait.con(TRY, Trait.ID = TBT$ID_TRY[TBT$Trait_continu == T], Average = T)
    MC.TRY <- full_join(TL, MC.TRY, by = "species")
    MD.TRY <- Clean.trait.discon(TRY, Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == F)], Keep.string = c(48))
    MC.TRY <- full_join(MC.TRY, MD.TRY, by = "species")
    MC.TRY <- melt(data.frame(MC.TRY), by = c("species", "48"))
    if(ncol(MC.TRY) != 7){print("here")
      MC.TRY$GrowthForm <- NA
      MC.TRY <- MC.TRY[c(1:4,7,5,6)] 
    }
    MC.TRY$DB <- "TRY"
    names(MC.TRY) <- c(names(TL), "GrowthForm", "variable", "value", "DB")
    MT <- rbind(MT, MC.TRY)
  }
  
  #### GIFT ####
  if(is.null(GIFT) == F){
    GIFT <- GIFT[,c(which(names(GIFT) %in% c("species", TBT$ID_GIFT)))]
    GIFT <- reshape2::melt(GIFT, id = "species")
    GIFT$variable <- as.character(GIFT$variable)
    for(i in 1:nrow(TBT)){GIFT$variable[GIFT$variable == TBT$ID_GIFT[i]] <- TBT$ID_TRY[i]}
    names(GIFT) <- c("species", "TraitID", "StdValue")
    MC.GIFT <- GIFT[GIFT$TraitID %in% TBT$ID_TRY[TBT$Trait_continu == T],]
    MC.GIFT <-  reshape2::dcast(MC.GIFT, species ~ TraitID, na.rm = F)
    for(i in 2:ncol(MC.GIFT)){MC.GIFT[[i]] <- as.numeric(MC.GIFT[[i]])}
    MC.GIFT$`26` <- MC.GIFT$`26`*100
    MC.GIFT$`3115` <- MC.GIFT$`3115`/10
    MC.GIFT$`4` <- MC.GIFT$`4`/1000
    MC.GIFT$`47` <- MC.GIFT$`47`/1000
    MC.GIFT <- full_join(TL, MC.GIFT, by = "species")
    
    MD.GIFT <- GIFT[GIFT$TraitID %in% TBT$ID_TRY[which(TBT$Trait_continu == F)],]
    names(MD.GIFT) <- c("AccSpeciesName", "TraitID", "OrigValueStr")
    MD.GIFT <- Clean.trait.discon(as_tibble(MD.GIFT), Trait.ID = TBT$ID_TRY[which(TBT$Trait_continu == F)], Keep.string = c(48), No.aggregation = F) # , 155, 335 /!\ work in progress pour les deux derniers !)
    MC.GIFT <- full_join(MC.GIFT, MD.GIFT, by = "species")
    MC.GIFT <- melt(data.frame(MC.GIFT), by = c("species", "48"))
    MC.GIFT$DB <- "GIFT"
    names(MC.GIFT) <- c(names(TL), "GrowthForm", "variable", "value", "DB")
    MT <- rbind(MT, MC.GIFT)
  }
  
  #### Merge ####
  MT <- MT[-c(1),]
  MT$variable <- TBT$TRY_lab[match(gsub("X", "", MT$variable), TBT$ID_TRY)]
  MT <- data.table::setDT(MT)[, GrowthForm := zoo::na.locf(zoo::na.locf(GrowthForm, na.rm = FALSE), fromLast = TRUE), species]
  MT <- reshape2::dcast(MT, ... ~ variable, value.var = "value", fun.aggregate = mean,)
  MT$TRY_SLA <- MT$TRY_SLA*0.001
  MT <-  MT[order(MT$family, MT$genus, MT$species),]
  is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}
  MT[is.nan(MT)] <- NA
  
  if(Remove.outliers == T){
    for(i in 1:ncol(MT)){
      trait.i <- names(MT)[i]
      if(any(trait.i %in% TBT$TRY_lab) == T){
        if(is.na(TBT$Lim_max[TBT$TRY_lab == trait.i]) == F){
          MT[which(MT[trait.i] > TBT$Lim_max[TBT$TRY_lab == trait.i]), trait.i] <- NA
        }}}}
  
  #### Linear interpolation SSD from LDMC ####
  if(SSD.interpol == T){
    List.faba <- TL.APG[TL.APG$family == "Fabaceae","species"]
    List.dicot <- TL.APG[TL.APG$subreign == "Angiospermes","species"] 
    List.monoc <- TL.APG[TL.APG$kingdom == "monocots","species"] 
    List.dicot <- setdiff(List.dicot, c(List.monoc,List.faba))
    
    List.herb <- MT[MT$GrowthForm == "Herb", "species"]
    List.faba <- intersect(List.faba, List.herb)
    List.monoc <- intersect(List.monoc, List.herb)
    List.dicot <- intersect(List.dicot, List.herb)
    
    Interp.faba <- 0.692*MT[is.na(MT$TRY_SSD) & MT$species %in% List.faba, "TRY_LDMC"] + 0.048
    Interp.mono <- 0.888*MT[is.na(MT$TRY_SSD) & MT$species %in% List.monoc, "TRY_LDMC"] + 0.027
    Interp.dico <- 0.524*MT[is.na(MT$TRY_SSD) & MT$species %in% List.dicot, "TRY_LDMC"] + 0.096
    
    MT[is.na(MT$TRY_SSD) & MT$species %in% List.faba, "TRY_SSD"] <- Interp.faba
    MT[is.na(MT$TRY_SSD) & MT$species %in% List.monoc, "TRY_SSD"] <- Interp.mono
    MT[is.na(MT$TRY_SSD) & MT$species %in% List.dicot, "TRY_SSD"] <- Interp.dico
  }
  #### Stats sur les traits ####
  if(is.null(Tab.stats) == F){
    Table.trait.stats <- data.frame(read.csv(Tab.stats, sep = "\t", header = T, row.names = 1))
    Stats.traits <- data.frame(lapply(MT, function(x) signif(mean(x, na.rm = T), digits = 3)))
    Stats.traits <- rbind(Stats.traits, data.frame(lapply(MT, function(x) signif(sd(x, na.rm = T), digits = 3))))
    Stats.traits <- rbind(Stats.traits, data.frame(lapply(MT, function(x) min(x, na.rm = T))))
    Stats.traits <- rbind(Stats.traits, data.frame(lapply(MT, function(x) max(x, na.rm = T))))
    Stats.traits <- rbind(Stats.traits, data.frame(lapply(MT, function(x) round(length(which(is.na(x)==F)), digits = 0))))
    row.names(Stats.traits) <- c("Mean", "SD", "Min", "Max", "mathbb{N_{species}}")
    Stats.traits <- t(Stats.traits[which(names(Stats.traits) %in% row.names(Table.trait.stats))])
    Table.trait.stats <- Table.trait.stats[which(row.names(Table.trait.stats) %in% row.names(Stats.traits)),] 
    Table.trait.stats <- cbind(Table.trait.stats, Stats.traits)
    Table.trait.stats$Min <- signif(Table.trait.stats$Min, digits = 3)
    Table.trait.stats$Max <- signif(Table.trait.stats$Max, digits = 3)
    Save.Tab.stats <- gsub("Import", "Resultats", Tab.stats)
    Save.Tab.stats <- gsub(".csv", "_full.csv", Save.Tab.stats)
    write.csv(Table.trait.stats, Save.Tab.stats, row.names = F)
  }
  #### Scaling ####
  Scaling.traits <- function(M){
    for(i in 1:ncol(M)){
      # print(paste("Scaling trait:", trait.i))
      # print(shapiro.test(M[[i]]))
      trait.i <- names(M)[i]
      Trans.i <- TBT$Transformation[which(TBT$TRY_lab == trait.i)]
      if(any(Trans.i == "log10") == T){M[trait.i] <- log10(M[trait.i])}
      if(any(Trans.i == "log10+1") == T){M[trait.i] <- log10(M[trait.i]+1)}
      if(any(Trans.i == "sqrt") == T){M[trait.i] <- log10(M[trait.i])}
      # print(shapiro.test(M[[i]]))
    }
    
    is.na(M) <- sapply(M, is.infinite)
    M[,which(names(M) %in% TBT$TRY_lab[TBT$Trait_continu])] <- scale(M[,which(names(M) %in% TBT$TRY_lab[TBT$Trait_continu])]) 
    M[is.nan(M)] <- NA
    return(M)
  }
  MT.scaled <- Scaling.traits(MT)
  
  #### Plot histo ####
  if(is.null(Save.plot) == F){
    Plot.hist <- function(MT.hist, Save.plot, My.alpha, My.position, Log.scale = T){
      if(Plot.only.continuous == T){MT.hist <- MT.hist[MT.hist$variable %in% TBT$TRY_lab[TBT$Trait_continu],]}
      MT.hist.log <- MT.hist[MT.hist$variable %in% TBT$TRY_lab[TBT$Log_dist],]
      MT.hist <- MT.hist[!MT.hist$variable %in% TBT$TRY_lab[TBT$Log_dist],]
      
      
      if(Log.scale == T){My_scale = scale_x_continuous(trans = 'log10', breaks = trans_breaks('log10', function(x) 10^x), labels = trans_format('log10', math_format(10^.x)))}
      else{My_scale <- NULL}
      
      p1 <- ggplot(data = MT.hist.log, aes(x = value)) + 
        geom_histogram(aes(fill = My.fill), bins = 30, na.rm = T, alpha = My.alpha, position = My.position) + # dodge, identity, stack
        facet_wrap(vars(variable), scales = "free", nrow = 2)+
        My_scale +
        theme_par()+
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"))
      if(nrow(MT.hist) > 0){
        p2 <- ggplot(data = MT.hist, aes(x = value)) +
          geom_histogram(aes(fill = My.fill), bins = 30, na.rm = T, alpha = My.alpha, position = My.position) +
          facet_wrap(vars(variable), scales = "free", nrow = 2)+
          theme_par()+
          ylab("Number of observations")+
          theme(axis.title.x = element_blank(), plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"))
        
        p1 <- p2 + p1 + plot_layout(widths = c(2/5, 3/5), guides = "collect")
      }
      ggsave(filename = Save.plot, p1, width = W*0.026458333, height = H*0.026458333, units = "cm")
    }
    
    MT.hist <- melt(MT, by = c("family", "Other.clade", "genus", "species", "GrowthForm", "DB"))
    MT.scaled.hist <- melt(MT.scaled, by = c("family", "Other.clade", "genus", "species", "GrowthForm", "DB"))
    if(SSD.interpol == T){MT.hist[MT.hist$species %in% c(List.dicot,List.faba,List.monoc) & MT.hist$variable == "TRY_SSD" & is.na(MT.hist$value) == F, "DB"] <- "Extrapolated SSD"}
    MT.hist2 <- MT.hist
    names(MT.scaled.hist)[names(MT.scaled.hist) == "GrowthForm"] <- "My.fill"
    names(MT.hist2)[names(MT.hist2) == "GrowthForm"] <- "My.fill"
    names(MT.hist)[names(MT.hist) == "DB"] <- "My.fill"
    Save.plot1 <- gsub(".pdf", "_DB.pdf", Save.plot)
    Save.plot2 <- gsub(".pdf", "_Growthform.pdf", Save.plot)
    Save.plot3 <- gsub(".pdf", "_Growthform_scaled.pdf", Save.plot)
    Plot.hist(MT.hist, Save.plot = Save.plot1, My.alpha = 1, My.position = "stack")
    Plot.hist(MT.hist2, Save.plot = Save.plot2, My.alpha = .3, My.position = "identity")
    Plot.hist(MT.scaled.hist, Save.plot = Save.plot3, My.alpha = .3, My.position = "identity", Log.scale = F)
  }
  
  #### Export ####
  Export.MT <- function(M){
    GrothForm <- M[c("species", "GrowthForm")]
    GrothForm <- GrothForm[!duplicated(GrothForm$species),]
    TL.APG <- full_join(TL.APG, GrothForm, by = "species")
    M <- aggregate(M[names(M)[grep("TRY_", names(M))]], by = M[c("species")], FUN = mean, na.rm = T, na.action = na.pass)
    M <- full_join(TL.APG, M, by = "species")
    return(M)
    }
  
  MT <- Export.MT(MT)
  MT.scaled <- Export.MT(MT.scaled)
  print(MT.scaled)
  if(is.null(Save.MT) == F){
    saveRDS(MT, Save.MT)
    saveRDS(MT.scaled, gsub(".Rds", "_scaled.Rds", Save.MT))
  }
  return(MT)
}

CWM.FT.plot <- function(MFT = NULL, MT = NULL, Plot.x = NULL, Select.clim = NULL, Select.model = NULL, Limites,
                        Select.DB = NULL, Select.trait = NULL, Strat.plot = T, Temp.zone, Select.interv = 1000, Smooth.param = 0.25,
                        Name.zone = NULL, Zone.clim = NULL, Xlims = NULL, Return.plot = F, Dot.size = 1, Dot.alpha = 0.1, 
                        R2.pos = NULL, Save.plot = NULL, Display.legend = "bottom", W = 1000, H = 1000, Facet_order = NULL){
  #### Settings ####
  if(missing(Temp.zone)){Temp.zone = rep("C", length(Zone.clim))}
  
  #### Temp zone ####
  if(any(unique(grepl("#", Temp.zone))) == T){
    print("Manual color scale for climate zone activated.")
    Rect.color.scale <- Temp.zone
    names(Rect.color.scale) <- Rect.color.scale
    if(any(unique(grepl("#", Temp.zone2))) == T){
      Rect.color.scale <- unique(c(Temp.zone, Temp.zone2))
      names(Rect.color.scale) <- Rect.color.scale
    }
    
  }
  else{
    Rect.color.scale <- c('grey')
    # print("Auto color scale")
    if(length(unique(Temp.zone)) == 0){Rect.color.scale <- c("grey")}
    if(length(unique(Temp.zone)) == 3){Rect.color.scale <- c("#75AADB", "black", "#E76D51")}
    if(length(unique(Temp.zone)) == 1 & "G" %in% unique (Temp.zone)){Rect.color.scale <- c("grey")}
    if(length(unique(Temp.zone)) == 1 & "C" %in% unique(Temp.zone)){Rect.color.scale <- c("#75AADB")}
    if(length(unique(Temp.zone)) == 1 & "W" %in% unique(Temp.zone)){Rect.color.scale <- c("#E76D51")}
    if(length(unique(Temp.zone)) == 2 & "C" %in% unique(Temp.zone) & "W" %in% unique(Temp.zone)){Rect.color.scale <- c("#E76D51", "#75AADB")}
    if(length(unique(Temp.zone)) == 2 & "G" %in% unique(Temp.zone) & "W" %in% unique(Temp.zone)){Rect.color.scale <- c("black", "#E76D51")}
    if(length(unique(Temp.zone)) == 2 & "G" %in% unique(Temp.zone) & "C" %in% unique(Temp.zone)){Rect.color.scale <- c("#75AADB", "black")}
  }
  
  if(is.null(Zone.clim) == F){
    yo = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                    xmax = Zone.clim[seq(2,length(Zone.clim), by=2)], 
                    Temp.col = Temp.zone)} 
  else{yo = data.frame(xmin = 0, xmax = 0, Temp.col = "black")}
  
  if(is.null(Name.zone) == F){yo2 = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                                               xmax = Zone.clim[seq(2,length(Zone.clim), by=2)],
                                               Temp.col = Temp.zone,
                                               Title.zone = Name.zone,
                                               variable = "Height[CWM]"
                                               )}
  else{yo2 = data.frame(xmin = 0, xmax = 0, Temp.col = "", Title.zone = "", Categorie = "")}
  
  Zone.clim.to.plot <- geom_rect(data = yo, inherit.aes = F, na.rm = T,
                                 mapping = aes(ymin = -Inf, ymax= +Inf, xmin = xmin, xmax = xmax, fill = Temp.col),
                                 alpha=0.1, color = "grey", linewidth = 0.3, linetype = 2)
  
  #### Clean Matrix function transfert ####
  if(is.null(MFT) == F){
    MFT <- data.frame(MFT)
    if(is.null(Select.clim) == T){
      print("Too much curves ! Please select *Select.clim*.")
      Select.clim <- c("MAAT", "MAP")
    }
    Match.clim <- unique (grep(paste(Select.clim,collapse="|"), names(MFT), value=TRUE))
    MFT <- MFT[Match.clim]
    
    if(is.null(Select.model) == F){
      Match.model <- unique (grep(paste(Select.model,collapse="|"), names(MFT), value=TRUE))
      MFT <- MFT[Match.model]
    }
    
    if(is.null(Select.DB) == F){
      Match.DB <- unique(grep(paste(Select.DB,collapse="|"), names(MFT), value=TRUE))
      MFT <- MFT[Match.DB]
    }
    MFT <- cbind(Site = row.names(MFT), MFT)
  }
  
  #### Clean Matrix trait ####
  if(is.null(MT) == F){
    if(class(MT) == "list"){MT <- MT[[1]]}
    
    if(is.null(Plot.x) == T){
      Keep.x <- c("Age", "Top", "Bottom", "AgeBP2023")
      Plot.x <- names(MT)[names(MT) %in% Keep.x]
      }
    
    if(length(Plot.x)>0){Plot.x <- sort(Plot.x)[1]}
    if(is.null(Select.trait) == T){Select.trait <- names(MT)[grep("TRY_", names(MT))]}
    
    Select.trait <- c("Site", Plot.x, Select.trait)
    MT <- MT[Select.trait]
  }
  #### Merge Matrixes ####
  if(is.null(MT) == T){M <- MFT}
  if(is.null(MFT) == T){M <- MT}
  if(is.null(MFT) == F & is.null(MT) == F){M <- full_join(MT, MFT, by = intersect(names(MT), names(MFT)))}
  
  M <- subset(M, select = -c(Site))
  M <- M[order(M[[Plot.x]]),]
  names(M)[which(!grepl("SEP.", names(M)))]
  M.m <- reshape2::melt(M, names(M)[which(!grepl("TRY_.", names(M)))])
  names(M.m)[c(ncol(M.m)-1,ncol(M.m))] <- c("Traits", "Traits_values")
  M.m <- reshape2::melt(M.m, names(M.m)[which(!grepl("SEP", names(M.m)))])
  names(M.m)[c(ncol(M.m)-1,ncol(M.m))] <- c("SEP", "SEP_values")
  M.m <- reshape2::melt(M.m, c(names(M.m)[1], "Traits", "Traits_values", "SEP", "SEP_values"))
  M.m$Method <- sub("\\..*", "", M.m$variable)
  M.m$Clim.param <- sub(".*\\.", "", M.m$variable)
  
  M.m$variable <- gsub(paste(unique(M.m$Method), collapse = "|"), "", M.m$variable)
  M.m$variable <- gsub(paste(unique(M.m$Clim.param), collapse = "|"), "", M.m$variable)
  M.m$variable <- gsub("\\.", "", M.m$variable)
  M.m$Traits <- gsub("TRY_", "", M.m$Traits)
  M.m$Traits[M.m$Traits == "LeafN"] = "N[leaf-CWM]"
  M.m$Traits[M.m$Traits == "LeafArea"] = "LeafArea[CWM]"
  M.m$Traits[M.m$Traits == "Height"] = "Height[CWM]"
  M.m$Traits[M.m$Traits == "SSD"] = "SSD[CWM]"
  M.m$Traits[M.m$Traits == "SLA"] = "SLA[CWM]"
  M.m$Traits[M.m$Traits == "SeedMass"] = "SeedMass[CWM]"
  
  
  #### Add R2 ####
  if(R2.pos == "bottomleft"){
    R2.y = "bottom"
    R2.x = "left"}
  if(R2.pos == "bottomright"){
    R2.y = "bottom"
    R2.x = "right"}
  if(R2.pos == "none"){
    R2.y = "none"
    R2.x = "none"}
  
  R2.x <- 0.01
  # R2.x <- c(.95, .01, .95, .01, .01, .01)
  R2.y <- seq(from = 0.01, to = 0.5, length.out = 1+length(c(unique(M.m$variable), unique(M.m$Method))))
  
  # Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, color = "turquoise4", size = 3.5, small.r = F, na.rm = T,
  Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, size = 3, small.r = F, na.rm = T,
                         aes(label =  sprintf("%s*\", \"*%s" ,
                                              after_stat(rr.label),
                                              # after_stat(r.squared),
                                              after_stat(p.value.label)
                         )))
  
  if(length(unique(M.m$Clim.param)) == 1){
    Y.title <- unique(M.m$Clim.param)
    Y.title <- ylab(paste(Y.title, "(pollen-inferred)", sep = " "))
    }
  else{Y.title <- NULL}
  
  #### Color settings ####
  My_colors <- scale_colour_manual(values = c("WASTDB" = "#e2a064ff", "WAST" = "#e2a064ff", "NMSDB" = "#F3A481",  
                                              "ST" = "#91C4DD", "STDB" = "#91C4DD", "MDB" = "red",
                                              "COSTDB" = "#c19d4dff", "COST" = "#c19d4dff", "TUDB" = "#0094AF",
                                              "MEDTEMP" = "#F3A481", "TEMPSCAND" = "#91C4DD", Traits = "#2c9740ff",
                                              "EAPDB" = "#0F3361", "TAIGDB" = "#32156eff"), name = "Calibration \n databases")
  
  if(is.null(Xlims) == F){Xlims <- xlim(Xlims[1], Xlims[2])}
  
  if(is.null(Name.zone) == F){yo2$variable = unique(M.m$Traits)[1]}
  Zone.txt <- geom_text(data = yo2, inherit.aes = F, aes(x = (xmax+xmin)/2, y = Inf, label = Title.zone, color = Temp.col), size = 2, angle = 0, vjust = 1.5, fontface = "bold")
  
  
  #### Plot RL ####
  if(Strat.plot == F){
    Plot.CWM <- ggplot(M.m, aes(y = value, x = Traits_values, colour = variable, shape = Method))+
      facet_wrap(vars(Traits), scales = "fixed") +
      geom_point(size = Dot.size, alpha = Dot.alpha)+
      geom_smooth(method = "lm", se = F, span = 1000, na.rm = T, linewidth = 1, formula = y ~ x)+
      Add.r2 + My_colors + Xlims + Y.title + xlab("CWM-traits (z-scores)")+
      guides(shape = guide_legend(override.aes = list(size = Dot.size+3))) +
      theme(
        plot.background = element_rect(fill = NA, colour = "grey30"),
        panel.spacing = unit(0.1, "lines"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside", panel.border = element_rect(NA, "black", linewidth = 1), strip.clip = "off",
        strip.text = element_text(size = 11),
        legend.justification = c("left"),               # left, top, right, bottom
        # plot.margin = unit(c(0,0,0,0), "lines")
      )
  }
  
  #### Plot Strat plot ####
  if(Strat.plot == T){
    #### Remelting ####
    Keep.names.y <- names(M.m)[1]
    
    M1 <- M.m[c(1:3)]
    names(M1) <- c("xPlot", "variable", "value")
    M1$Method <- "Traits"
    M1$Clim.param <- "Traits"
    
    M2 <- M.m[c(1,9,7,8,6)]
    names(M2) <- c("xPlot", "variable", "value", "Method", "Clim.param")
    M.m <- rbind(M1,M2)
    
    My.lab <- "X"
    if(Keep.names.y %in% c("Top", "Bottom", "Depth")){My.lab <- c("Depth (cm)")}
    if(Keep.names.y == "Age"){My.lab <- c("Time (year cal. BP)")}
    if(missing(Limites)){Limites = c(round(min(M.m$xPlot), digits = -2), max(M.m$xPlot))}
    
    M.m$variable[M.m$variable == "MAAT"] = "MAAT~(degree~C)"
    M.m$variable[M.m$variable == "MAP"] = "MAP~(mm.yr^-1)"
    
    
    if(is.null(Facet_order) == T){Facet_order <- sort(unique(M.m$variable), decreasing = F)}
    
    M.m$variable <- factor(M.m$variable, levels = Facet_order, ordered = T)
    
    #### Plot ####
    Plot.CWM <- ggplot(M.m, aes(y = value, x = xPlot, colour = Clim.param, shape = Method))+
      scale_fill_manual(values = Rect.color.scale, guide = "none")+
      Zone.clim.to.plot + 
      # new_scale_fill()+
      facet_wrap(vars(variable), scales = "free_y", ncol = 1, drop = F, strip.position = "left", labeller = label_parsed) + xlab(My.lab)+
      geom_point(size = Dot.size, alpha = Dot.alpha)+ ylab(NULL)+
      geom_smooth(method = "loess", se = T, fullrange = T, span = Smooth.param, na.rm = T, linewidth = .5, formula = 'y ~ x')+
      My_colors + scale_shape(name = "Calibration \n databases") +
      scale_x_continuous(breaks = c(Limites[1], round(seq(0, Limites[2], by = Select.interv))))+
      scale_y_continuous(expand = c(0.05,0.05))+
      new_scale_color()+
      # scale_color_manual(values = my_orange, guide = "none")+
      scale_color_manual(values = Rect.color.scale, guide = "none", name = NULL, labels = NULL, breaks = NULL, na.translate = FALSE)+
      Zone.txt +
      guides(shape = guide_legend(nrow = 1), colour = guide_legend(nrow = 1, override.aes=list(fill=NA))) +
      theme(
        axis.title.x=element_text(size=12, colour = "grey20"),
        axis.title.y=element_text(size=12),              # AGE
        strip.text = element_text(size=12, angle = 45),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, colour = "grey30"),
        axis.text.y = element_text(size = 8, colour = "grey30"),
        axis.line = element_line(colour = "grey30"),
        axis.ticks = element_line(colour = "grey30"),
        legend.position = Display.legend,                        # permet de mettre le carre des legendes en bas
        legend.key.size = unit(8, "mm"),
        legend.direction = "horizontal", #
        panel.spacing = unit(0.1, "lines"),
        panel.background = element_blank(), legend.background = element_blank(),
        strip.background = element_blank(),
        strip.placement = "left", strip.clip = "off",
        legend.justification = c("left"),
        plot.margin = unit(c(0,0,0,0), 'pt')
      )
  }
  #### Save plot and export ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(Plot.CWM, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
    else{ggsave(Save.plot)}}
  
  if(Return.plot == F){return(M.m)}
  else{return(Plot.CWM)}
  
}



#### Applications (Uz) ####
Uzbekistan.surf = F
if(Uzbekistan.surf == T){
  #### Calculation to CWM ####
  Calculation.CWM.Uz = F
  if(Calculation.CWM.Uz == T){
    #### Build une Table de conversion Pollen-types vs. taxa ####
    Auto.extract.trait.agg.table.UZ = F
    if(Auto.extract.trait.agg.table.UZ == T){
      #### Import data ####
      source("Scripts/TNRS.R")
      Uz.MP  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Surface/MP_pour.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      Table.Taxon.Uz <- read.csv(file="Import/World_DB/Pollen/Odile_DB/Corresp_name_full_V11.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)
      
      #### Clean data ####
      row.names(Uz.MP) <- gsub(" ", "\\.", row.names(Uz.MP))
      Uz.MP <- data.frame(t(Uz.MP))
      Uz.MP$Thymelaceae <- Uz.MP$Thymelaceae + Uz.MP$Strigosella.t # Thymelaceae
      Uz.MP <- subset(Uz.MP, select = - c(Strigosella.t))
      Uz.MP <- data.frame(t(Uz.MP)) # Pourc check
      
      #### Function d'extraction auto ####
      TPT <- Auto.extract.trait.agg.table(MP = Uz.MP, Table.Taxon = Table.Taxon.Uz, TNRS.check = T, 
                                          Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ_raw.csv")
      }
    else{TPT <- read.csv(file = "Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)}
    
    #### Import data cleaned #### 
    # My data (TUSD)
    Uz.MP_sl  <- data.frame(read.csv(file="Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_coarse.csv",sep=",",dec=".", header=T, row.names=1))
    Uz.MP_ss  <- data.frame(read.csv(file="Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_fine.csv",sep=",",dec=".", header=T, row.names=1))
    Uz.MV_sl <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz_PT_sl.csv", sep = ",", header = T, row.names = 1)
    Uz.MV_ss <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz_PT_ss.csv", sep = ",", header = T, row.names = 1)
    Uz.MV_TN <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz.csv", sep = ",", header = T, row.names = 1)
    Uz.eco <- read.table("Import/Uzbekistan/Site/TUSD_surf_samples.csv", sep = ",", header = T, row.names = 1)
    Uz.index.TN <- read.table("Import/Uzbekistan/Vegetation/Indexes/Index_from_Uz_Herbier_auto.csv", sep = ",", header = T, row.names = 1)
  
    #### Keep only my samples (No Suzanne, No Odile) ####
    Uz.MP_ss  <- Uz.MP_ss[!grepl("SL", names(Uz.MP_ss)) & !grepl("OP", names(Uz.MP_ss))]
    Uz.MP_ss  <- Uz.MP_ss[rowSums(Uz.MP_ss) != 0,]
    Uz.MP_sl  <- Uz.MP_sl[!grepl("SL", names(Uz.MP_sl)) & !grepl("OP", names(Uz.MP_sl))]
    Uz.MP_sl  <- Uz.MP_sl[rowSums(Uz.MP_sl) != 0,]
    Uz.eco  <- Uz.eco[Uz.eco$Palynologues == "Lucas Dugerdil",]
    
    #### Climat extract / import #### 
    Climat.extract = F
    if(Climat.extract == T){
      #### Extract ####
      source("Scripts/Climat_extract.R")
      Uz.Cord <- read.table("Import/Uzbekistan/Site/TUSD_surf_samples.csv", sep = ",", header = T, row.names = 1)
      Uz.Cord <- Uz.Cord[!grepl("ZOP", row.names(Uz.Cord)) & !grepl("ZSL", row.names(Uz.Cord)),c(1:2)]
      
      Uz.Clim_chel <- Clim.param.extraction(M = Uz.Cord, All.param = F, Chelsa = T, Altitude = T, Aridity = T)
      Uz.Clim_wc <- Clim.param.extraction(M = Uz.Cord, All.param = F, Chelsa = F, Altitude = T, Aridity = T)
      Uz.Biom <- Clim.param.extraction(M = Uz.Cord, Clim.cal = F, Biome = T, All.param = F, Chelsa = T, Altitude = F, Aridity = F)
      Uz.Land <- Clim.param.extraction(M = Uz.Cord, Clim.cal = F, Biome = F, All.param = F, Chelsa = F, Land.cover = T, Altitude = F, Aridity = F)
      
      #### Clim merge ####
      Uz.climtot <- Uz.Clim_chel
      names(Uz.climtot)[4:9] <- paste(names(Uz.climtot)[4:9], "chel", sep = "_")
      Uz.climtot <- cbind(Uz.climtot, Uz.Clim_wc[4:9])
      names(Uz.climtot)[11:16] <- paste(names(Uz.climtot)[11:16], "wc", sep = "_")
      Uz.climtot <- Uz.climtot[order(names(Uz.climtot))]
      
      Uz.Biom <- left_join(rownames_to_column(Uz.Biom[c(1,2,4,5)]), rownames_to_column(Uz.Eco[c(1,2,3,7)]), by = c("rowname"))
      row.names(Uz.Biom) <- Uz.Biom$rowname
      Uz.Biom <- Uz.Biom[-c(1)]
      
      #### Export ####
      saveRDS(Uz.Clim_chel, "Resultats/Uzbekistan/Pollen/Surface/TUSD_Clim_chel.Rds")
      saveRDS(Uz.Clim_wc, "Resultats/Uzbekistan/Pollen/Surface/TUSD_Clim_wc.Rds")
      saveRDS(Uz.Land, "Resultats/Uzbekistan/Pollen/Surface/TUSD_LandCover.Rds")
      saveRDS(Uz.Biom, "Resultats/Uzbekistan/Pollen/Surface/TUSD_Biom.Rds")
      saveRDS(Uz.climtot, "Resultats/Uzbekistan/Pollen/Surface/TUSD_climtot.Rds")
      }
    else{
      Uz.Clim_chel <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUSD_Clim_chel.Rds")
      Uz.Clim_wc <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUSD_Clim_wc.Rds")
      Uz.Land <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUSD_LandCover.Rds")
      Uz.Biom <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUSD_Biom.Rds")
      Uz.climtot <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUSD_climtot.Rds")
    }
    
    
    #### Old version (tricks avec Trait.R) ####
    Old.v.UZ = F
    if(Old.v.UZ == T){
      #### Import ####
      # My data (ACA db)
      TL.ACA <- readRDS(paste(DB.path, "Vegetation/Occurences/Merge_DB/Taxon_list_ACA_V1.Rds", sep = ""))
      # GrowthForm.ACA <- readRDS(paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_GrowthForm_ACA.Rds", sep = ""))
      MT.ACA <- readRDS(paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_trait_ACA.Rds", sep = ""))
      MT.ACA.gf <- readRDS("Resultats/World_DB/Traits/MT_ACA_gapfilled.Rds")
      Tr.PT_ss <- readRDS("Resultats/ACA/Traits/Tr.PT_ss.Rds")
      Tr.PT_sl <- readRDS("Resultats/ACA/Traits/Tr.PT_sl.Rds")
      # Tr.MV_gf <- readRDS("Resultats/ACA/Traits/MT_MV_full_gf.Rds")
      Tr.MV <- readRDS("Resultats/ACA/Traits/MT_MV_full.Rds")
      Tr.PT_ss_gf <- readRDS("Resultats/ACA/Traits/Tr.PT_ss_gf.Rds")
      Tr.PT_sl_gf <- readRDS("Resultats/ACA/Traits/Tr.PT_sl_gf.Rds")
      Tr.PT_fam_gf <- readRDS("Resultats/ACA/Traits/Tr.PT_fam_gf.Rds")
      Tr.PT_fam <- readRDS("Resultats/ACA/Traits/Tr.PT_fam.Rds")
      Tr.PT_gen_gf <- readRDS("Resultats/ACA/Traits/Tr.PT_gen_gf.Rds")
      Tr.PT_gen <- readRDS("Resultats/ACA/Traits/Tr.PT_gen.Rds")
      
      #### Check if ACA checklist is complete or not ? ####
      Missing.species <- unique(TPT$Espece)[-1][!unique(TPT$Espece)[-1] %in% TL.ACA$species]
      Missing.genus <- unique(TPT$Genre)[-1][!unique(TPT$Genre)[-1] %in% TL.ACA$genus]
      Missing.fam <- unique(TPT$Famille)[!unique(TPT$Famille) %in% TL.ACA$family]
      if(length(Missing.species) > 0){print(paste("These species are missing:", Missing.species))}
      if(length(Missing.genus) > 0){print(paste("These genus are missing:", Missing.genus))}
      if(length(Missing.fam) > 0){print(paste("These family are missing:", Missing.fam))}
      # 
      # Missing.species.MV <- unique(TPT$Espece)[-1][!unique(TPT$Espece)[-1] %in% TL.ACA$species]
      # Missing.genus <- unique(TPT$Genre)[-1][!unique(TPT$Genre)[-1] %in% TL.ACA$genus]
      # Missing.fam <- unique(TPT$Famille)[!unique(TPT$Famille) %in% TL.ACA$family]
      # if(length(Missing.species) > 0){print(paste("These species are missing:", Missing.species))}
      # if(length(Missing.genus) > 0){print(paste("These genus are missing:", Missing.genus))}
      # if(length(Missing.fam) > 0){print(paste("These family are missing:", Missing.fam))}
      
      #### Extract traits for UZ from ACA ####
      Extract.trait.from.ACA <- function(My_taxa = NULL, gf = T, return.missing = F, PT = NULL, veget = F){
        if(veget == T){
          if(PT == "NT"){
            My_genus <- unique(My_taxa$Genus[grep(" spp.", My_taxa$Species)])
            My_species <- unique(My_taxa$Species[!grepl(" spp.", My_taxa$Species) & !is.na(My_taxa$Species)])
            My_family <- na.omit(unique(My_taxa$Family[is.na(My_taxa$Species)]))}
          if(PT == "sl"){
            My_genus <- unique(My_taxa$Genus[grepl(" spp.", My_taxa$PT.sl) | grepl("-type", My_taxa$PT.sl)])
            My_species <- unique(My_taxa$Species[!grepl(" spp.", My_taxa$PT.sl) & !is.na(My_taxa$PT.sl)])
            My_family <- na.omit(unique(My_taxa$Family[grep("eae", My_taxa$PT.sl)]))
            }
          if(PT == "ss"){
            My_genus <- unique(My_taxa$Genus[grepl(" spp.", My_taxa$PT.ss) | grepl("-type", My_taxa$PT.ss)])
            My_species <- unique(My_taxa$Species[!grepl(" spp.", My_taxa$PT.ss) & !is.na(My_taxa$PT.ss)])
            My_family <- na.omit(unique(My_taxa$Family[grep("eae", My_taxa$PT.ss)]))
            }
          
          # print(My_genus)
          # print(My_family)
            
          if(gf == T){
            MT.spe <- MT.ACA.gf[which(MT.ACA.gf$species %in% My_species),]
            MT.gen <- Tr.PT_gen_gf[which(Tr.PT_gen_gf$genus %in% My_genus),]
            MT.fam <- Tr.PT_fam_gf[which(Tr.PT_fam_gf$family %in% My_family),]
            MT.spe <- MT.spe[-c(1,2)]}
          else{
            MT.spe <- MT.ACA[which(MT.ACA$species %in% My_species),]
            MT.gen <- Tr.PT_gen[which(Tr.PT_gen$genus %in% My_genus),]
            MT.fam <- Tr.PT_fam[which(Tr.PT_fam$family %in% My_family),]
            MT.spe <- MT.spe[-c(1,2,3)]
            }
            
          names(MT.gen)[1] <- "species"
          names(MT.fam)[1] <- "species"
          MT.gen$species <- paste(MT.gen$species, "spp.", sep = " ")
          MT <- rbind(MT.spe, MT.fam, MT.gen)
          if(gf == T){
            Missing.fam <- setdiff(My_family, Tr.PT_fam_gf$family)
            Missing.gen <- setdiff(My_genus, Tr.PT_gen_gf$genus)}
          else{
            Missing.fam <- setdiff(My_family, Tr.PT_fam$family)
            Missing.gen <- setdiff(My_genus, Tr.PT_gen$genus)
            }
            
          Missing.spe <- setdiff(My_species, MT.ACA$species)
          
          if(return.missing == T){return(Missing.spe)}
          else{return(MT)}}
        
        if(veget == F){
          if(gf == T){
            MT1 <- MT.ACA.gf[-c(1:2)]
            Tr1 <- Tr.PT_ss_gf
            Tr2 <- Tr.PT_sl_gf
            }
          else{
            MT1 <- MT.ACA[-c(1:3)]
            Tr1 <- Tr.PT_ss
            Tr2 <- Tr.PT_sl
            }
            
          print(dim(MT1))
          MT <- MT1[MT1$species %in% My_taxa,]
          Missing.types <- setdiff(My_taxa, MT$species)
          
          Found.in.ss <- Tr1[Tr1$species %in% Missing.types,]
          MT <- rbind(MT, Found.in.ss)
          Missing.types <- setdiff(My_taxa, MT$species)
          
          Found.in.sl <- Tr2[Tr2$species %in% Missing.types,]
          MT <- rbind(MT, Found.in.sl)
          
          if(PT == "sl"){MT$species <- TPT$PT_sl.label[match(MT$species, My_taxa)]}
          if(PT == "ss"){MT$species <- TPT$PT_ss.label[match(MT$species, My_taxa)]}
          return(MT)
        }
        }
        
      
      MT.Uz_gf <- Extract.trait.from.ACA(Uz.index.TN, gf = T, return.missing = F, veget = T, PT = "NT")
      MT.Uz <- Extract.trait.from.ACA(Uz.index.TN, gf = F, return.missing = F, veget = T, PT = "NT")
      Missing.taxa <- Extract.trait.from.ACA(Uz.index.TN, gf = F, return.missing = T, veget = T, PT = "NT")
      # 
      MT.Uz.PT_sl_gf <- Extract.trait.from.ACA(Uz.index.TN, gf = T, return.missing = F, PT = "sl", veget = T)
      MT.Uz.PT_sl <- Extract.trait.from.ACA(Uz.index.TN, gf = F, return.missing = F, PT = "sl", veget = T)
      MT.Uz.PT_ss_gf <- Extract.trait.from.ACA(Uz.index.TN, gf = T, return.missing = F, PT = "ss", veget = T)
      MT.Uz.PT_ss <- Extract.trait.from.ACA(Uz.index.TN, gf = F, return.missing = F, PT = "ss", veget = T)
      # 
      # MT.Uz.MP_sl_gf <- Extract.trait.from.ACA(TPT$PT_sl, gf = T, return.missing = F, PT = "sl", veget = F)
      # MT.Uz.MP_sl <- Extract.trait.from.ACA(TPT$PT_sl, gf = F, return.missing = F, PT = "sl", veget = F)
      # MT.Uz.MP_ss_gf <- Extract.trait.from.ACA(TPT$PT_ss, gf = T, return.missing = F, PT = "ss", veget = F)
      # MT.Uz.MP_ss <- Extract.trait.from.ACA(TPT$PT_ss, gf = F, return.missing = F, PT = "ss", veget = F)
     
      #### CWM-traits calculation ####
      CWM.calculation = T
      if(CWM.calculation == T){print("eoeo")
        Seuil.pcover = 50
        MCWT.clim.Uz.MV.PT_sl_gf <- CWT.calculation(MT = MT.Uz.PT_sl_gf, MP = Uz.MV_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MV.PT_sl <- CWT.calculation(MT = MT.Uz.PT_sl, MP = Uz.MV_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MV.PT_ss_gf <- CWT.calculation(MT = MT.Uz.PT_ss_gf, MP = Uz.MV_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MV.PT_ss <- CWT.calculation(MT = MT.Uz.PT_ss, MP = Uz.MV_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MV.NT <- CWT.calculation(MT = MT.Uz, MP = Uz.MV_TN, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MV.NT_gf <- CWT.calculation(MT = MT.Uz_gf, MP = Uz.MV_TN, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MP_sl <- CWT.calculation(MT = MT.Uz.MP_sl, MP = Uz.MP_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MP_sl_gf <- CWT.calculation(MT = MT.Uz.MP_sl_gf, MP = Uz.MP_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MP_ss <- CWT.calculation(MT = MT.Uz.MP_ss, MP = Uz.MP_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        MCWT.clim.Uz.MP_ss_gf <- CWT.calculation(MT = MT.Uz.MP_ss_gf, MP = Uz.MP_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
        }
      
      
      }
  
    #### Uz checklist ####
    Checklist.building.Uz = F
    if(Checklist.building.Uz == T){
      TUSD.bo1 <- "../../../../../media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Projets/ACA/Borders_ACA/Tadj_uz/Tadj_uz_bo.shp"
      TUSD.bo2 <- c("Uzbekistan", "Tajikistan")
      ChL.Samarqand  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Samarqand_checklist.csv",sep=",",dec=".", header=T, row.names=1))
      ChL.Jizzak  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Jizzakh_checklist.csv",sep=",",dec=".", header=T, row.names=1))
      ChL.Bukhara  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Bukhara_checklist.csv",sep=",",dec=".", header=T, row.names=1))
      ChL.Kashkadarya  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Kashkadarya_checklist.csv",sep=",",dec=".", header=T, row.names=1))
      ChL.Nuratau  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Nuratau_checklist.csv",sep=",",dec=".", header=T, row.names=1))
      ChL.Chatkal  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Chatkal_checklist.csv",sep=",",dec=".", header=T, row.names=1))        
      ChL.TianShan  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/TianShan_checklist.csv",sep=",",dec=".", header=T, row.names=1))        
      ChL.Tajikistan  <- data.frame(read.csv(file="Import/Uzbekistan/Checklist/Tajikistan_checklist.csv",sep=",",dec=".", header=T, row.names=1))        
      ChL.TUSD  <- data.frame(read.csv(file="Resultats/Uzbekistan/Checklist/Checklist_MV_TUSD.csv",sep=",",dec=".", header=T, row.names=1))        
      
      ChL.Uz <- rbind(ChL.Bukhara, ChL.Jizzak, ChL.Samarqand, ChL.Kashkadarya, ChL.Nuratau, ChL.Chatkal, ChL.TianShan[c(1,2)], ChL.Tajikistan[c(1,2)])  
      ChL.Uz$Species <- paste(ChL.Uz$Genera, ChL.Uz$Species, sep = " ")
      ChL.Uz <- rbind(ChL.TUSD, ChL.Uz)
      ChL.Uz <- ChL.Uz[!duplicated(ChL.Uz),]
      
      A = Checklist.cleaning(ChL.Uz, TNRS.check = T, Remove.non.pollinic = T, Remove.subsp = T,
                            GIFT.shape.file = TUSD.bo1,
                            BIEN.shape.file = TUSD.bo2,
                            TL.APG.Save.RDS = "Resultats/Uzbekistan/Checklist/Checklist_MV_TUSD_APG_clean.Rds",
                            TL.Save.RDS = "Resultats/Uzbekistan/Checklist/Checklist_MV_TUSD_clean.Rds")
      ChL.TUSD.APG <- A$TL.APG
      ChL.TUSD <- A$TL 
      
    }
    else{
      ChL.TUSD.APG <- readRDS("Resultats/Uzbekistan/Checklist/Checklist_MV_TUSD_APG_clean.Rds")
      ChL.TUSD <- as_tibble(readRDS("Resultats/Uzbekistan/Checklist/Checklist_MV_TUSD_clean.Rds"))
      }
    
    #### Check if all MV in the ChL ####
    Verif.Uz.Taxa.Chl = F
    if(Verif.Uz.Taxa.Chl == T){
      print("Taxa not in the check list")
      # All.taxa.TUSD <- unique(c(row.names(Uz.MP_sl), row.names(Uz.MP_ss), row.names(Uz.MV_sl), row.names(Uz.MV_ss), row.names(Uz.MV_TN)))
      All.taxa.TUSD <- unique(row.names(Uz.MV_TN))
      # All.taxa.TUSD <- gsub("-type", "", All.taxa.TUSD)
      A <- All.taxa.TUSD[!All.taxa.TUSD %in% ChL.TUSD$species]
      A.fam <- A[grep("aceae", A)]
      A.gen <- A[grep("spp.", A)]
      A.sp <- setdiff(A, c(A.fam, A.gen))
      A.gen <- gsub(" spp.", "", A.gen)
      if(length(A.fam[! A.fam %in% ChL.TUSD$family]) > 0){print(A.fam[! A.fam %in% ChL.TUSD$family])}
      if(length(A.gen[! A.gen %in% ChL.TUSD$family]) > 0){print(A.gen[! A.gen %in% ChL.TUSD$genus])}
      if(length(A.sp[! A.sp %in% ChL.TUSD$family]) > 0){print(A.sp[! A.sp %in% ChL.TUSD$species])}
    }
    
    #### Table conversion pollen ####
    Table.conversion.pollen = T
    if(Table.conversion.pollen == T){
      A = Pollen.type.2.Checklist(
            List.PT_ss = unique(c(row.names(Uz.MP_ss), row.names(Uz.MV_ss))),
            List.PT_sl = unique(c(row.names(Uz.MP_sl), row.names(Uz.MV_sl))), 
            Save.path = "Resultats/Uzbekistan/Traits/Corresp_tax/Table_corresp_plant_type.Rds",
            TL = ChL.TUSD, TPT = TPT)

    }
    #### Traits extraction for the checklist ####
    Trait.extraction.Uz = F
    if(Trait.extraction.Uz == T){
      DB.trait.extraction(TL = ChL.TUSD, Projet.name = "TUSD", Extract.GIFT = F,
                          Extract.BIEN = F, Extract.TRY = F, Extract.BROT = F, Chinese.trait.DB = F)
      
      # A FAIRE: API request pour GIFT
      # le DB.trait.clean ne fonctionne pas pour TUSD
      DB.trait.clean(BIEN.path = paste(DB.path, "Vegetation/Occurences/BIEN/BIEN_TUSD_trait.Rds", sep = ""),
                     # TRY.path = paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_taxa_TUSD.Rds", sep = ""),
                     Projet.name = "TUSD")
      }
    
    #### Traits DB cleaning / merging ####
    Calculate.ext.trait = F
    if(Calculate.ext.trait == T){
      if(exists("BIEN.TUSD") == F){
        BIEN.TUSD <- readRDS(paste(DB.path, "Vegetation/Occurences/BIEN/BIEN_TUSD_trait_clean.Rds", sep = ""))
        TRY.TUSD <- readRDS(paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_taxa_TUSD.Rds", sep = ""))
        BROT.TUSD <- as_tibble(readRDS("Import/World_DB/Traits/BROT2.0/BROT_TUSD.Rds"))
        # CPT.TUSD <- as_tibble(readRDS("Import/China/Traits/China_Plant_TraitsDB_csv/CPT_TUSD.Rds"))
        GIFT.TUSD <- as_tibble(readRDS("Import/ACA/Traits/GIFT/20210617_central_asia_traits_TUSD_crop.Rds"))
        }
      
      MT.Uz = DB.merge(
               TRY = TRY.TUSD,
               BROT = BROT.TUSD,
               GIFT = GIFT.TUSD,
               BIEN = BIEN.TUSD,
               TL = ChL.TUSD, TL.APG = ChL.TUSD.APG, SSD.interpol = T, Plot.only.continuous = T, Remove.outliers = T, 
               Save.plot = "Figures/Uzbekistan/Traits/Distribution/Trait_dist_hist_TUSD.pdf",
               Tab.stats = "Import/Uzbekistan/Traits/Table_traits.csv",
               TBT = "Import/Uzbekistan/Traits/Trait_ID_names.csv",
               Save.MT = "Resultats/Uzbekistan/Traits/Traits_values/MT_TUSD.Rds")
      
    }
    
    #### Trait aggregation by pollen-types ####
    TPT.aggregation.Uz = T
    if(TPT.aggregation.Uz == T){
      #### Import ####
      MT.Uz.no.scale <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT_TUSD.Rds")
      MT.Uz_gf <- read.table(file = "Resultats/Uzbekistan/Traits/Traits_values/MT_TUSD_scale_gf.txt", sep="\t",dec=".", header=T, stringsAsFactors = F)
      MT.Uz <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT_TUSD_scaled.Rds")
      MT.Uz.sd <- read.table(file = "Resultats/Uzbekistan/Traits/Traits_values/MT_TUSD_scale_gf_sd.txt", sep = "\t", dec=".", header=T)
      Only.hiera <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/Hierarchie_gap-filling.Rds")
      Only.trait <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/TM_before_gap-filling.Rds")
      MT.bool.ss <- readRDS(file = "Resultats/Uzbekistan/Traits/Corresp_tax/Table_corresp_plant_type_bool_ss.Rds")
      MT.bool.sl <- readRDS(file = "Resultats/Uzbekistan/Traits/Corresp_tax/Table_corresp_plant_type_bool_sl.Rds")
      Error.display = T
      Fullfill.NA.by.fam = F
      
      #### GF cleaning #####
      MT.Uz.cv <- MT.Uz.sd/MT.Uz_gf
      MT.Uz_gf[MT.Uz.cv > 1 & is.na(Only.trait)] <- NA
      MT.Uz_gf <- as_tibble(cbind(Only.hiera[c(2)], MT.Uz_gf))
      MT.Uz_gf <- full_join(MT.Uz[names(MT.Uz)[!grepl("TRY", names(MT.Uz))]], MT.Uz_gf, by = "species")
      
      #### Functions ####
      print("Let's aggregate pollen-type, bro !")
      Trait.aggregate.by.class <- function(M, by = NULL){
        BY = list(M[[by]])
        By.names <- names(M[by])
        nums <- unlist(lapply(M, is.numeric))  
        M <- M[nums]
        M <- aggregate(M, BY, FUN = mean, na.action = na.pass, na.rm = T)
        names(M)[1] <- By.names
        return(M)
      }
      Trait.aggregate.by.type <- function(TP, MT, name.var){
        Fuck.off <- c("species", "family", "genus", "PT_ss", "PT_sl", "subreign", "GrowthForm", "kingdom", "order", "Other.clade")
        Col.to.keep <- setdiff(names(MT), Fuck.off)
        Row.to.keep <- setdiff(names(TP), Fuck.off)
        
        TP.work <- setNames(data.frame(matrix(NA, ncol = length(Col.to.keep)+1, nrow = length(Row.to.keep))), c(name.var,Col.to.keep))
        TP.work[[name.var]] <- Row.to.keep
        names(TP.work)[1] <- "species"
        
        T.m <- TP.work
        T.sd <- TP.work
        TP <- TP[c("species",Row.to.keep)]
        MT <- data.frame(MT[c("species",Col.to.keep)])
        pb = txtProgressBar(min = 0, max = ncol(TP), initial = 0) 
        print(paste("Aggregation with", name.var))
        for(i in 2:ncol(TP)){
          setTxtProgressBar(pb,i)
          S.to.pick <- TP$species[which(TP[i] == T)]
          for(j in 2:ncol(MT)){
            Val <- MT[which(MT$species %in% S.to.pick), j]
            Trait.mean <- mean(Val, na.rm = T)
            Trait.sd <- sd(Val, na.rm = T)
            T.m[i-1,j] <- Trait.mean
            T.sd[i-1,j] <- Trait.sd
          }
        }
        close(pb)
        # return(list(Mean = T.m, SD = T.sd, Nval = T.n))
        return(list(Mean = T.m, SD = T.sd))
      }
      Keep.no.gf <- function(M.to.fill, M.to.add, id){
        KN <- M.to.fill[[id]]
        M.to.fill[[id]] <- gsub(" spp.", "", M.to.fill[[id]])
        M.to.fill[[id]] <- gsub("-type", "", M.to.fill[[id]])
        Miss.type <- which(M.to.fill[[id]] %in% M.to.add[[id]])
        To.add <- which(M.to.add[[id]] %in% M.to.fill[[id]] )
        for(i in 2:ncol(M.to.fill)){
          for(j in 1:length(Miss.type)){
            if(is.na(M.to.add[To.add[j],i]) == F){
              M.to.fill[Miss.type[j], i] <- M.to.add[To.add[j],i]
            }
          }
        }
        M.to.fill[[id]] <- KN
        return(M.to.fill)}
      Fill.NA.from.fam.gen <- function(MT, Mgen, Mfam){
        print(paste("Fullfill the NAs by the average value for genus and family for the", deparse(substitute(MT))))
        Trait.list <- names(MT)[grep("TRY", names(MT))]
        pb = txtProgressBar(min = 0, max = nrow(MT), initial = 0)
        for(i in Trait.list){
          print(i)
          for(j in 1:nrow(MT)){
            setTxtProgressBar(pb,j)
            if(is.na(MT[which(MT$genus %in% Mgen$genus), i][j])){
              MT[which(MT$genus %in% Mgen$genus), i][j] <- Mgen[match(MT$genus, Mgen$genus), i][j]}
            if(is.na(MT[which(MT$family %in% Mfam$family), i][j])){
              MT[which(MT$family %in% Mfam$family), i][j] <- Mfam[match(MT$family, Mfam$family), i][j]}
          }}
        close(pb)
        return(MT)
      }
      
      #### Aggregate trait family / genus ####
      Tr.PT_fam <- Trait.aggregate.by.class(MT.Uz, by = "family")
      Tr.PT_gen <- Trait.aggregate.by.class(MT.Uz, by = "genus")
      Tr.PT_fam_gf <- Trait.aggregate.by.class(MT.Uz_gf, by = "family")
      Tr.PT_gen_gf <- Trait.aggregate.by.class(MT.Uz_gf, by = "genus")
    
      #### Aggregation pollen-type ####
      Tr.PT_sl.full <- Trait.aggregate.by.type(MT.bool.sl, MT.Uz, "PT_sl") # Normal sl
      Tr.PT_ss.full <- Trait.aggregate.by.type(MT.bool.ss, MT.Uz, "PT_ss") # Normal ss
      Tr.PT_sl.g.full <- Trait.aggregate.by.type(MT.bool.sl, MT.Uz_gf, "PT_sl") # Normal sl
      Tr.PT_ss.g.full <- Trait.aggregate.by.type(MT.bool.ss, MT.Uz_gf, "PT_ss") # Normal sl
      
      Tr.PT_sl.sd <- Tr.PT_sl.full$SD
      Tr.PT_ss.sd <- Tr.PT_ss.full$SD
      Tr.PT_sl_gf.sd <- Tr.PT_sl.g.full$SD
      Tr.PT_ss_gf.sd <- Tr.PT_ss.g.full$SD
      
      Tr.PT_sl <- Tr.PT_sl.full$Mean
      Tr.PT_ss <- Tr.PT_ss.full$Mean
      Tr.PT_sl_gf <- Tr.PT_sl.g.full$Mean
      Tr.PT_ss_gf <- Tr.PT_ss.g.full$Mean
      
      #### Replace par value non gf quand c'est possible (qui est normalement plus fiable que la gf) ####
      Tr.PT_ss_gf <- Keep.no.gf(Tr.PT_ss_gf, Tr.PT_fam, 1)
      Tr.PT_ss_gf <- Keep.no.gf(Tr.PT_ss_gf, Tr.PT_gen, 1)
      Tr.PT_sl_gf <- Keep.no.gf(Tr.PT_sl_gf, Tr.PT_fam, 1)
      Tr.PT_sl_gf <- Keep.no.gf(Tr.PT_sl_gf, Tr.PT_gen, 1)
      Tr.PT_fam_gf <- Keep.no.gf(Tr.PT_fam_gf, Tr.PT_fam, 1)
      Tr.PT_gen_gf <- Keep.no.gf(Tr.PT_gen_gf, Tr.PT_gen, 1)
      
      #### Clean pollen-types ####
      Tr.PT_ss_gf <- cbind(Rank = "PT_ss", GrowthForm = TPT$AP_NAP[match(Tr.PT_ss_gf$species, TPT$PT_ss)], Tr.PT_ss_gf)
      Tr.PT_ss_gf  <- Tr.PT_ss_gf[c(1,3,2,4:ncol(Tr.PT_ss_gf))]
      names(Tr.PT_ss_gf)[names(Tr.PT_ss_gf) == "species"] <- "taxa"
      
      Tr.PT_sl_gf <- cbind(Rank = "PT_sl", GrowthForm = TPT$AP_NAP[match(Tr.PT_sl_gf$species, TPT$PT_sl)], Tr.PT_sl_gf)
      Tr.PT_sl_gf  <- Tr.PT_sl_gf[c(1,3,2,4:ncol(Tr.PT_sl_gf))]
      names(Tr.PT_sl_gf)[names(Tr.PT_sl_gf) == "species"] <- "taxa"
      
      Tr.PT_ss <- cbind(Rank = "PT_ss", GrowthForm = TPT$AP_NAP[match(Tr.PT_ss$species, TPT$PT_ss)], Tr.PT_ss)
      Tr.PT_ss  <- Tr.PT_ss[c(1,3,2,4:ncol(Tr.PT_ss))]
      names(Tr.PT_ss)[names(Tr.PT_ss) == "species"] <- "taxa"
      
      Tr.PT_sl <- cbind(Rank = "PT_sl", GrowthForm = TPT$AP_NAP[match(Tr.PT_sl$species, TPT$PT_sl)], Tr.PT_sl)
      Tr.PT_sl  <- Tr.PT_sl[c(1,3,2,4:ncol(Tr.PT_sl))]
      names(Tr.PT_sl)[names(Tr.PT_sl) == "species"] <- "taxa"
      
      #### Fullfill the NA in the MT species by the genus / fam average values ####
      if(Fullfill.NA.by.fam == T){
        MT.Uz <- Fill.NA.from.fam.gen(MT.Uz, Tr.PT_gen, Tr.PT_fam)
        MT.Uz_gf <- Fill.NA.from.fam.gen(MT.Uz_gf, Tr.PT_gen_gf, Tr.PT_fam_gf)
      }
      
      #### Merge all datasets #### 
      MT.Uz.full <- cbind(Rank = "species", MT.Uz[!names(MT.Uz) %in% c("subreign","kingdom","order","Other.clade","family","genus")])
      names(MT.Uz.full)[names(MT.Uz.full) == "species"] <- "taxa"
      Tr.PT_fam  <- cbind(Rank = "family", Tr.PT_fam, GrowthForm = NA)
      names(Tr.PT_fam)[names(Tr.PT_fam) == "family"] <- "taxa"
      Tr.PT_gen  <- cbind(Rank = "genus", Tr.PT_gen, GrowthForm = NA)
      names(Tr.PT_gen)[names(Tr.PT_gen) == "genus"] <- "taxa"
      Tr.PT_gen$taxa <- paste(Tr.PT_gen$taxa, "spp.", sep = " ")
      Tr.PT_gen  <- rbind(Tr.PT_gen, Tr.PT_fam)
      Tr.PT_gen  <- Tr.PT_gen[c(1,2,ncol(Tr.PT_gen),3:(ncol(Tr.PT_gen)-1))]
      MT.Uz.full  <- as_tibble(rbind(MT.Uz.full, Tr.PT_gen, Tr.PT_sl, Tr.PT_ss))
      
      MT.Uz.full_gf <- cbind(Rank = "species", MT.Uz_gf[!names(MT.Uz_gf) %in% c("subreign","kingdom","order","Other.clade","family","genus")])
      names(MT.Uz.full_gf)[names(MT.Uz.full_gf) == "species"] <- "taxa"
      Tr.PT_fam_gf  <- cbind(Rank = "family", Tr.PT_fam_gf, GrowthForm = NA)
      names(Tr.PT_fam_gf)[names(Tr.PT_fam_gf) == "family"] <- "taxa"
      Tr.PT_gen_gf  <- cbind(Rank = "genus", Tr.PT_gen_gf, GrowthForm = NA)
      names(Tr.PT_gen_gf)[names(Tr.PT_gen_gf) == "genus"] <- "taxa"
      Tr.PT_gen_gf$taxa <- paste(Tr.PT_gen_gf$taxa, "spp.", sep = " ")
      Tr.PT_gen_gf  <- rbind(Tr.PT_gen_gf, Tr.PT_fam_gf)
      Tr.PT_gen_gf  <- Tr.PT_gen_gf[c(1,2,ncol(Tr.PT_gen_gf),3:(ncol(Tr.PT_gen_gf)-1))]
      MT.Uz.full_gf  <- as_tibble(rbind(MT.Uz.full_gf, Tr.PT_fam_gf, Tr.PT_gen_gf, Tr.PT_sl_gf, Tr.PT_ss_gf))
      
      #### Check if missing taxa in front of splot and surf pol DB ####
      if(Error.display == T){
        print(row.names(Uz.MP_sl)[!row.names(Uz.MP_sl) %in%  MT.Uz.full_gf$taxa[MT.Uz.full_gf$Rank == "PT_sl"]])
        print(row.names(Uz.MP_sl)[!row.names(Uz.MP_sl) %in%  MT.Uz.full$taxa[MT.Uz.full$Rank == "PT_sl"]])
        print(row.names(Uz.MP_ss)[!row.names(Uz.MP_ss) %in%  MT.Uz.full_gf$taxa[MT.Uz.full_gf$Rank == "PT_ss"]])
        print(row.names(Uz.MP_ss)[!row.names(Uz.MP_ss) %in%  MT.Uz.full$taxa[MT.Uz.full$Rank == "PT_ss"]])
        print(row.names(Uz.MV_ss)[!row.names(Uz.MV_ss) %in%  MT.Uz.full$taxa[MT.Uz.full$Rank == "PT_ss"]])
        print(row.names(Uz.MV_sl)[!row.names(Uz.MV_sl) %in%  MT.Uz.full$taxa[MT.Uz.full$Rank == "PT_sl"]])
        print(row.names(Uz.MV_TN)[!row.names(Uz.MV_TN) %in%  MT.Uz.full$taxa[MT.Uz.full$Rank %in% c("species", "genus", "family")]])
        }
    
      #### Export data ####
      saveRDS(MT.Uz.full_gf, "Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full_gf.Rds")
      saveRDS(MT.Uz.full, "Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full.Rds")
      }
    else{
      MT.Uz.full_gf <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full_gf.Rds")
      MT.Uz.full <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full.Rds")
    }
    
    #### CWM-traits calculation ####
    CWM.calculation = T
    if(CWM.calculation == T){
      Seuil.pcover = 80
      MT.Uz.MP_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_sl_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MV_sl_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MV_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MV_sl <- MT.Uz.full[MT.Uz.full$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MP_sl <- MT.Uz.full[MT.Uz.full$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MV_ss <- MT.Uz.full[MT.Uz.full$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MP_ss <- MT.Uz.full[MT.Uz.full$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MV_NT <- MT.Uz.full[MT.Uz.full$Rank %in% c("species", "genus", "family"), c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MV_NT_gf <- MT.Uz.full[MT.Uz.full$Rank %in% c("species", "genus", "family"), c(2,4:ncol(MT.Uz.full))]
      
      MCWT.clim.Uz.MV.PT_sl_gf <- CWT.calculation(MT = MT.Uz.MV_sl_gf, MP = Uz.MV_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MV.PT_sl <- CWT.calculation(MT = MT.Uz.MV_sl, MP = Uz.MV_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MV.PT_ss_gf <- CWT.calculation(MT = MT.Uz.MV_ss_gf, MP = Uz.MV_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MV.PT_ss <- CWT.calculation(MT = MT.Uz.MV_ss, MP = Uz.MV_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MV.NT <- CWT.calculation(MT = MT.Uz.MV_NT, MP = Uz.MV_TN, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MV.NT_gf <- CWT.calculation(MT = MT.Uz.MV_NT_gf, MP = Uz.MV_TN, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MP_sl <- CWT.calculation(MT = MT.Uz.MP_sl, MP = Uz.MP_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MP_sl_gf <- CWT.calculation(MT = MT.Uz.MP_sl_gf, MP = Uz.MP_sl, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MP_ss <- CWT.calculation(MT = MT.Uz.MP_ss, MP = Uz.MP_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Uz.MP_ss_gf <- CWT.calculation(MT = MT.Uz.MP_ss_gf, MP = Uz.MP_ss, Mclim = Uz.climtot, MPS.ACA.Biom = Uz.Biom, Accep.seuil = Seuil.pcover)
      
      saveRDS(MCWT.clim.Uz.MV.PT_sl_gf, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_sl_gf.Rds")
      saveRDS(MCWT.clim.Uz.MV.PT_sl, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_sl.Rds")
      saveRDS(MCWT.clim.Uz.MV.PT_ss_gf, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_ss_gf.Rds")
      saveRDS(MCWT.clim.Uz.MV.PT_ss, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_ss.Rds")
      saveRDS(MCWT.clim.Uz.MV.NT, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_NT.Rds")
      saveRDS(MCWT.clim.Uz.MV.NT_gf, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_NT_gf.Rds")
      saveRDS(MCWT.clim.Uz.MP_sl, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_sl.Rds")
      saveRDS(MCWT.clim.Uz.MP_sl_gf, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_sl_gf.Rds")
      saveRDS(MCWT.clim.Uz.MP_ss, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_ss.Rds")
      saveRDS(MCWT.clim.Uz.MP_ss_gf, "Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_ss_gf.Rds")
      
      
    }
  }
  
  #### Plots resultats (surface) ####
  Plot.CWM.Uz = F
  if(Plot.CWM.Uz == T){
    #### Import ####
    MCWT.clim.Uz.MV.PT_sl_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_sl_gf.Rds")
    MCWT.clim.Uz.MV.PT_sl <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_sl.Rds")
    MCWT.clim.Uz.MV.PT_ss_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_ss_gf.Rds")
    MCWT.clim.Uz.MV.PT_ss <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_PT_ss.Rds")
    MCWT.clim.Uz.MV.NT <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_NT.Rds")
    MCWT.clim.Uz.MV.NT_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MV_NT_gf.Rds")
    MCWT.clim.Uz.MP_sl <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_sl.Rds")
    MCWT.clim.Uz.MP_sl_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_sl_gf.Rds")
    MCWT.clim.Uz.MP_ss <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_ss.Rds")
    MCWT.clim.Uz.MP_ss_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/TUSD/MCWT_Uz_MP_ss_gf.Rds")
    
    MV <- MCWT.clim.Uz.MV.PT_ss_gf$MCWT
    MP <- MCWT.clim.Uz.MP_ss_gf$MCWT
    
    
    MP$ClassMAP <- "Hyper_Arid"
    MP$ClassMAP[MP$AI>=2500] <- "Wet"
    MV$ClassMAP <- "Hyper_Arid"
    MV$ClassMAP[MV$AI>=2500] <- "Wet"
    
    
    Extra.plots  <- data.frame(read.csv(file="Import/Uzbekistan/Vegetation/Releves/Uz_topcore_extrapolation.csv",sep=",",dec=".", header=T, row.names=1))
    # MV <- MV[!MV$Site %in% c("MUZT3M03", row.names(Extra.plots)),]
    # MP <- MP[!MP$Site %in% c("MUZT3M03", row.names(Extra.plots)),]
    
    MV$Type <- "Vegetation"
    MP$Type <- "Pollen"
    MT <- rbind(MV, MP)
    MT[grep("TRY", names(MT))] <- scale(MT[grep("TRY", names(MT))])
    MP <- MT[MT$Type == "Pollen",]
    MV <- MT[MT$Type == "Vegetation",]
    MP <- MP[MP$Site  %in%  MV$Site,]
    
    # MV$Extrapolate_VegPlot <- "No"
    # MV$Extrapolate_VegPlot[MV$Site %in% row.names(Extra.plots)] <- "Yes"
    # MP$Extrapolate_VegPlot <- "No"
    # MP$Extrapolate_VegPlot[MP$Site %in% row.names(Extra.plots)] <- "Yes"
    # 
    Trait.to.keep.pca1 <- c(5,7,8,13:15, 35)
    
    #### LR comparaison CWM-t MP / MV ####
    LR.comparaison = F
    if(LR.comparaison == T){
      LR.CWM.Uz <- LR.CWM(MP = MP, MV = MV,
                   # Keep.taxa = names(MV)[grep("TRY_", names(MV))][c(1:10)],                   Keep.taxa = names(MV)[grep("TRY_", names(MV))][c(1:6,8,10:12)], Save.plotly = F,
                   Keep.taxa = c("TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass", "TRY_SLA", "TRY_SSD"), Legend.position = "none",
                   H = 700, W = 900, Num.facet = "(A)", Dot.opac = 0.9, Dot.size = 1.8,
                   R2.pos = "bottomleft", Scale = "fixed", Cluster = "Ecosystem", Return.plot = T#,
                   # Save.plot = "Figures/Uzbekistan/Traits/Correlation/LR_CWM_comp.pdf"
                   )
      
      # LR.CWM.clim(MT = list(MP = MP, MV = MV), Meco = list(Uzbekistan = Uz.eco),
      #             Keep.taxa = c("TRY_SSD", "TRY_Height", "TRY_SLA", "TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass"),
      #             # Keep.taxa = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia spp."),
      #             Keep.clim = c("Altitude", "MAP", "MAAT"),
      #             H = 1200, W = 1200, Strip.lab = F, R2.pos = "none",
      #             Save.plot = "Figures/Uzbekistan/Traits/Correlation/LR_CWM_clim.pdf")
    }
    
    #### PCA CWM-t MP / MV ####
    PCA.trait = F
    if(PCA.trait == T){
      source("Scripts/Trait.R")
      PCA.MV <- PCA.bioclim(MV[,Trait.to.keep.pca1], Dot.opac = 0.9, Dot.size = 2.5, Cluster.core = "Ecosystem",
                            Ellipse = F, Cluster.core.lab = "",  Legend.size = 11, 
                            Density.contour = T, Opa.range = c(0.1,.4), Density.type = "polygon",
                            transp_OK = T, Scale.PCA = 6, return.pick = T, Num.facet = "(B)", Show.annot = T,
                            Site.name = "PCA CWM vegetation", Legend.position = "bottom", Show.centroid = F, Marg.density.plot = F,
                            Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png"#, #Symbol.pos = 
                            # Save.plot = "Figures/Uzbekistan/Traits/Multivariate/PCA/PCA_CWM_MV_Uz.pdf", H = 650, W = 650
                                       )
      PCA.MP <- PCA.bioclim(MP[,Trait.to.keep.pca1], Dot.opac = 0.9, Dot.size = 2.5, Cluster.core = "Ecosystem",
                            Ellipse = F, Cluster.core.lab = "", Legend.size = 6, 
                            Density.contour = T, Opa.range = c(0.1,.4), Density.type = "polygon", Show.annot = F,
                            transp_OK = T, Scale.PCA = 6, return.pick = T, Num.facet = "(C)", Reverse.dim = T,
                            Site.name = "PCA CWM pollen", Legend.position = "none", Show.centroid = F, Marg.density.plot = F,
                            Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png"#, #Symbol.pos = 
                                       # Save.plot = "Figures/Uzbekistan/Traits/Multivariate/PCA/PCA_CWM_MP_Uz.pdf", H = 650, W = 650
                                       )
      
    }
    
    #### Procuste Analysis MP / MV ####
    Procrustes.plot = F
    if(Procrustes.plot == T){
      row.names(MV) <- MV$Site
      row.names(MP) <- MP$Site
      Meco <- MV["Ecosystem"]
      MP.proc <- data.frame(t(MP[,Trait.to.keep.pca1[-c(length(Trait.to.keep.pca1))]]))
      MV.proc <- data.frame(t(MV[,Trait.to.keep.pca1[-c(length(Trait.to.keep.pca1))]]))
      
      source("Scripts/Veget.R")
      PCoI.CWM.Uz <- PCoI.vegetation(MP.proc, MV.proc, Meco = Uz.eco, Stats.pos = c(-1.8,2.5),
                                     W = 1200, H = 800, Return.plot = T, Show.errors = F, Reverse.dim = F,
                                     Title = "(D) Procrustes analysis (B) vs. (C)", Dot.opac = 0.9, Dot.size = 2.2,
                                     Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen_vs_veget.png",
                                     Save.plot = "Figures/Uzbekistan/Traits/Multivariate/Procrustian/CWM_tot_Uz.pdf"
      )
      
    }
    
    #### Merge all plots ####
    Merge.all.plots = F
    if(Merge.all.plots == T){
      pca.full <- LR.CWM.Uz / (PCA.MV + PCA.MP + PCoI.CWM.Uz) / guide_area() + plot_layout(heights = c(.35, .62, .06), guides = "collect")
      W = 1100
      H = 650
      ggsave(filename = "Figures/Uzbekistan/Traits/Multivariate/CWM_tot_Uz.pdf", pca.full, width = W*0.026458333, height = H*0.026458333, units = "cm")
     }
  }
  
  #### Paléo (Fazilman) ####
  Fazilm.CWM = F
  if(Fazilm.CWM == T){
    #### Import data ####
    Faz.MP_sl <- readRDS("Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_Fazilman_PT_coarse.Rds")
    Faz.MP_ss <- readRDS("Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_Fazilman_PT_fine.Rds")
    MT.Uz.full_gf <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full_gf.Rds")
    MT.Uz.full <- readRDS("Resultats/Uzbekistan/Traits/Traits_values/MT.Uz.full.Rds")
    Uz.TaxaCorresp  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
    Faz.MA <- data.frame(read.csv(file = "Resultats/Uzbekistan/Pollen/Cores/Fazilman/MA_Fazilman_pollen.csv",sep=",",dec=".", header=T, row.names=1))
    
    #### CWM-traits calculation ####
    CWM.calculation = T
    if(CWM.calculation == T){ 
      Seuil.pcover <- 70
      MT.Uz.MP_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_sl_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_sl <- MT.Uz.full[MT.Uz.full$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MP_ss <- MT.Uz.full[MT.Uz.full$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full))]
      
      MCWT.Faz.MP_sl <- CWT.calculation(MT = MT.Uz.MP_sl, MP = Faz.MP_sl, Mclim = Faz.MA, Accep.seuil = Seuil.pcover)
      MCWT.Faz.MP_sl_gf <- CWT.calculation(MT = MT.Uz.MP_sl_gf, MP = Faz.MP_sl, Mclim = Faz.MA, Accep.seuil = Seuil.pcover)
      MCWT.Faz.MP_ss <- CWT.calculation(MT = MT.Uz.MP_ss, MP = Faz.MP_ss, Mclim = Faz.MA, Accep.seuil = Seuil.pcover)
      MCWT.Faz.MP_ss_gf <- CWT.calculation(MT = MT.Uz.MP_ss_gf, MP = Faz.MP_ss, Mclim = Faz.MA, Accep.seuil = Seuil.pcover)
      saveRDS(MCWT.Faz.MP_sl, "Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_sl.Rds")
      saveRDS(MCWT.Faz.MP_sl_gf, "Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_sl_gf.Rds")
      saveRDS(MCWT.Faz.MP_ss, "Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_ss.Rds")
      saveRDS(MCWT.Faz.MP_ss_gf, "Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_ss_gf.Rds")
    }
    else{
      MCWT.Faz.MP_sl <- readRDS("Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_sl.Rds")
      MCWT.Faz.MP_sl_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_sl_gf.Rds")
      MCWT.Faz.MP_ss <- readRDS("Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_ss.Rds")
      MCWT.Faz.MP_ss_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/Fazilman/MCWT_Uz_MP_ss_gf.Rds")
    }
    
    #### Plot CWM vs. age BP ####
    Plot.Faz.CWM = F
    if(Plot.Faz.CWM == T){
      Fazilman.TUDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_TUDB.Rds")
      Fazilman.COSTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_COSTDB.Rds")
      Fazilman.WASTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_WASTDB.Rds")
      Fazilman.STDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_STDB.Rds")
      
      MAAT.plot <- CWM.FT.plot(MT = MCWT.Faz.MP_ss_gf, MFT = c(Fazilman.TUDB, Fazilman.COSTDB),
                               Select.clim = c("MAAT"),
                               Select.model = c("MAT", "WAPLS", "BRT"),
                               Select.trait = c("TRY_SLA", "TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                               Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = F,
                               H = 1000, W = 1500, R2.pos = "bottomleft", Display.legend = "none")
      
      MAP.plot <- CWM.FT.plot(MT = MCWT.Faz.MP_ss_gf, MFT = c(Fazilman.TUDB, Fazilman.COSTDB),
                              Select.clim = c("MAP"),
                              Select.model = c("MAT", "WAPLS", "BRT"),
                              Select.trait = c("TRY_SLA", "TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                              Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = F,
                              H = 1000, W = 1500, R2.pos = "bottomleft", Display.legend = "none"#,
                              # Save.plot = "Figures/Uzbekistan/Traits/Cores/Fazilman/CWM_Fazilman_age.pdf"
                              )
      
      pfull <- ((MAAT.plot / MAP.plot) | guide_area()) + plot_layout(guides = "collect", widths = c(0.9,0.1)) 
      W = 900
      H = 900
      ggsave(filename = "Figures/Uzbekistan/Traits/Cores/Fazilman/CWM_FT_Fazilman.pdf", pfull, width = W*0.026458333, height = H*0.026458333, units = "cm")
      
      MAP.plot <- CWM.FT.plot(MT = MCWT.Faz.MP_ss_gf, MFT = c(Fazilman.TUDB, Fazilman.COSTDB),
                              Select.clim = c("MAP", "MAAT"),
                              # Select.model = c("MAT", "WAPLS", "BRT"),
                              Select.model = c("MAT"),
                              # Select.trait = c("TRY_SLA", "TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                              Select.trait = c("TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                              Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = T, Dot.size = 1.3, Dot.alpha = .07,
                              H = 900, W = 400, R2.pos = "bottomleft", Display.legend = "bottom", Select.interv = 1000, Limites = c(0,10000),
                              Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3300, 3900, 4000, 4400, 5000, 7650, 7650, 8950, 9500 , max(MCWT.Faz.MP_ss_gf$MCWT$Age)),  #Feng et al., 2006
                              Temp.zone = c("C","W","C","W","C", "W", "C", "C", "G"),
                              Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky", "Lak1",  "Lak2", "LS"),
                              Save.plot = "Figures/Uzbekistan/Traits/Cores/Fazilman/CWM_Fazilman.pdf")}
  }
  
  #### Paléo (Aral86) ####
  Aral86.CWM = T
  if(Aral86.CWM == T){
    #### Import data ####
    Aral86.MP <- data.frame(t(read.csv(file="Import/Uzbekistan/Pollen/Cores/Neotoma/Aral86.csv",sep=",",dec=".",header=T, row.names=1, stringsAsFactors = FALSE)))
    Aral86.MA <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Cores/Neotoma/Aral86_MA.csv",sep="\t",dec=",",header=T, row.names=1, stringsAsFactors = FALSE))
    
    #### Clean data général ####
    Aral86.MP <- data.frame(t(Aral86.MP))
    row.names(Aral86.MP)[row.names(Aral86.MP) == "Juglans"] <- "Juglans regia"
    row.names(Aral86.MP)[row.names(Aral86.MP) == "Ephedra"] <- "Ephedra distachya"
    row.names(Aral86.MP)[row.names(Aral86.MP) == "Plantaginaceae"] <- "Plantago"
    row.names(Aral86.MP)[row.names(Aral86.MP) == "Rubiaceae"] <- "Galium"
    row.names(Aral86.MP)[row.names(Aral86.MP) == "Iridaceae"] <- "Liliaceae"
    
    #### Build une Table de conversion Pollen-types vs. taxa ####
    Auto.extract.trait.agg.table.UZ = F
    if(Auto.extract.trait.agg.table.UZ == T){
      source("Scripts/TNRS.R")
      Table.Taxon.Uz <- read.csv(file="Import/World_DB/Pollen/Odile_DB/Corresp_name_full_V12.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)
      TPT <- Auto.extract.trait.agg.table(MP = data.frame(t(Aral86.MP)), Table.Taxon = Table.Taxon.Uz, TNRS.check = T, Show.missing = F, Save.path = "Resultats/Uzbekistan/Pollen/Cores/Neotoma/Aral86/Corresp_pollen_Aral86.csv")
      }
    else{TPT <- read.csv(file = "Resultats/Uzbekistan/Pollen/Cores/Neotoma/Aral86/Corresp_pollen_Aral86.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)}
    
    #### Convert pollen-types in PT_sl et PT_ss ####
    Aral86.MP_sl <- cbind(PT.sl = TPT$PT_sl.label[match(row.names(Aral86.MP), row.names(TPT))], Aral86.MP)
    Aral86.MP_sl <- aggregate(Aral86.MP_sl[-1], by = list(Aral86.MP_sl[["PT.sl"]]), sum)
    row.names(Aral86.MP_sl) <- Aral86.MP_sl$Group.1
    Aral86.MP_sl <- Aral86.MP_sl[-1]
    
    Aral86.MP_ss <- cbind(PT.ss = TPT$PT_ss.label[match(row.names(Aral86.MP), row.names(TPT))], Aral86.MP)
    Aral86.MP_ss <- aggregate(Aral86.MP_ss[-1], by = list(Aral86.MP_ss[["PT.ss"]]), sum)
    row.names(Aral86.MP_ss) <- Aral86.MP_ss$Group.1
    Aral86.MP_ss <- Aral86.MP_ss[-1]
    
    Aral86.MP <- Aral86.MP/rowSums(Aral86.MP)
    Aral86.MP[is.na(Aral86.MP)] <- 0
    Aral86.MP_ss <- round(Aral86.MP_ss*100, digits = 2)
    Aral86.MP_sl <- round(Aral86.MP_sl*100, digits = 2)
    
    #### CWM-traits calculation ####
    CWM.calculation = T
    if(CWM.calculation == T){ 
      Seuil.pcover <- 70
      MT.Uz.MP_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_ss_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_sl_gf <- MT.Uz.full_gf[MT.Uz.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full_gf))]
      MT.Uz.MP_sl <- MT.Uz.full[MT.Uz.full$Rank == "PT_sl", c(2,4:ncol(MT.Uz.full))]
      MT.Uz.MP_ss <- MT.Uz.full[MT.Uz.full$Rank == "PT_ss", c(2,4:ncol(MT.Uz.full))]
      
      MCWT.Aral86.MP_sl <- CWT.calculation(MT = MT.Uz.MP_sl, MP = Aral86.MP_sl, Mclim = Aral86.MA, Accep.seuil = Seuil.pcover)
      MCWT.Aral86.MP_sl_gf <- CWT.calculation(MT = MT.Uz.MP_sl_gf, MP = Aral86.MP_sl, Mclim = Aral86.MA, Accep.seuil = Seuil.pcover)
      MCWT.Aral86.MP_ss <- CWT.calculation(MT = MT.Uz.MP_ss, MP = Aral86.MP_ss, Mclim = Aral86.MA, Accep.seuil = Seuil.pcover)
      MCWT.Aral86.MP_ss_gf <- CWT.calculation(MT = MT.Uz.MP_ss_gf, MP = Aral86.MP_ss, Mclim = Aral86.MA, Accep.seuil = Seuil.pcover)
      saveRDS(MCWT.Aral86.MP_sl, "Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_sl.Rds")
      saveRDS(MCWT.Aral86.MP_sl_gf, "Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_sl_gf.Rds")
      saveRDS(MCWT.Aral86.MP_ss, "Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_ss.Rds")
      saveRDS(MCWT.Aral86.MP_ss_gf, "Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_ss_gf.Rds")
    }
    else{
      MCWT.Aral86.MP_sl <- readRDS("Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_sl.Rds")
      MCWT.Aral86.MP_sl_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_sl_gf.Rds")
      MCWT.Aral86.MP_ss <- readRDS("Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_ss.Rds")
      MCWT.Aral86.MP_ss_gf <- readRDS("Resultats/Uzbekistan/Traits/CWM/Neotoma/Aral86/MCWT_Uz_MP_ss_gf.Rds")
    }
    
    #### Plot CWM vs. age BP ####
    Plot.Aral86.CWM = T
    if(Plot.Aral86.CWM == T){
      Aral86.TUDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_TUDB.Rds")
      Aral86.COSTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_COSTDB.Rds")
      MCWT.Aral86.MP_ss_gf$MCWT$Age <- MCWT.Aral86.MP_ss_gf$MCWT$Bottom
      # Aral86.WASTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_WASTDB.Rds")
      # Aral86.STDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_STDB.Rds")
      
      MAAT.plot <- CWM.FT.plot(MT = MCWT.Aral86.MP_ss_gf, MFT = c(Aral86.TUDB, Aral86.COSTDB),
                               Select.clim = c("MAAT"),
                               Select.model = c("MAT", "WAPLS", "BRT"),
                               Select.trait = c("TRY_SLA", "TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                               Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = F,
                               H = 1000, W = 1500, R2.pos = "bottomleft", Display.legend = "none")
      
      MAP.plot <- CWM.FT.plot(MT = MCWT.Aral86.MP_ss_gf, MFT = c(Aral86.TUDB, Aral86.COSTDB),
                              Select.clim = c("MAP"),
                              Select.model = c("MAT", "WAPLS", "BRT"),
                              Select.trait = c("TRY_SLA", "TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                              Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = F,
                              H = 1000, W = 1500, R2.pos = "bottomleft", Display.legend = "none"#,
                              # Save.plot = "Figures/Uzbekistan/Traits/Cores/Neotoma/Aral86/CWM_Aral86_age.pdf"
      )
      
      pfull <- ((MAAT.plot / MAP.plot) | guide_area()) + plot_layout(guides = "collect", widths = c(0.9,0.1)) 
      W = 900
      H = 900
      ggsave(filename = "Figures/Uzbekistan/Traits/Cores/Neotoma/Aral86/CWM_FT_Aral86.pdf", pfull, width = W*0.026458333, height = H*0.026458333, units = "cm")
      MAP.plot <- CWM.FT.plot(MT = MCWT.Aral86.MP_ss_gf, MFT = c(Aral86.TUDB, Aral86.COSTDB),
                              Select.clim = c("MAP", "MAAT"),
                              Select.model = c("MAT", "WAPLS"), 
                              Select.trait = c("TRY_Height", "TRY_LeafArea", "TRY_LeafN", "TRY_SSD", "TRY_SeedMass"),
                              Plot.x = "Age", Select.DB = NULL, Return.plot = T, Strat.plot = T, Dot.size = 1.3, Dot.alpha = .01, Smooth.param = 0.2,
                              H = 900, W = 400, R2.pos = "bottomleft", Display.legend = "bottom", Select.interv = 50, 
                              Zone.clim = c(0,30,115,155, 200, 230),
                              Temp.zone = c("G", "G", "G"),
                              Name.zone = c("E1", "E2", "E3"),
                              Save.plot = "Figures/Uzbekistan/Traits/Cores/Neotoma/Aral86/CWM_Aral86.pdf")}
  }
  
  
  
  }

#### Applications (Iran, Gol) ####
Iran.surf = F
if(Iran.surf == T){
  #### Calculation to CWM ####
  Calculation.CWM.Ir = T
  if(Calculation.CWM.Ir == T){
    #### Build une Table de conversion Pollen-types vs. taxa ####
    Auto.extract.trait.agg.table.Ir = F
    if(Auto.extract.trait.agg.table.Ir == T){
      #### Import data ####
      source("Scripts/TNRS.R")
      Ir.MP <- data.frame(read.csv(file="Import/Iran/Pollen/Surface/MP_count.csv",sep=",",dec=".", header=T, row.names=1))
      Table.Taxon.Ir <- read.csv(file="Import/World_DB/Pollen/Odile_DB/Corresp_name_full_V11.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)
      
      #### Clean data ####
      Ir.MP <- data.frame(t(Ir.MP))
      Ir.MP$Cousinia <- Ir.MP$Cousinia + Ir.MP$Carthamus
      Ir.MP <- subset(Ir.MP, select = - c(Carthamus))
      Ir.MP$Cichorioideae <- Ir.MP$Cichorioideae + Ir.MP$Lactuceae
      Ir.MP <- subset(Ir.MP, select = - c(Lactuceae))
      Ir.MP <- subset(Ir.MP, select = - c(Sparganium, Indetermined))
      Ir.MP <- data.frame(t(Ir.MP))
      # row.names(Ir.MP) <- gsub(" ", "\\.", row.names(Ir.MP))
      
      
      #### Function d'extraction auto ####
      TPT <- Auto.extract.trait.agg.table(MP = Ir.MP, 
                                          Table.Taxon = Table.Taxon.Ir, 
                                          TNRS.check = T, 
                                          Save.path = "Resultats/Iran/Pollen/Func_trans/Corresp_pollen_Ir_raw.csv")}
    else{TPT <- read.csv(file = "Import/Iran/Pollen/Func_trans/Corresp_pollen_Ir.csv", sep=",",dec=".", row.names = 1,  header=T, stringsAsFactors = F)}
    
    #### Import data cleaned #### 
    Ir.MP_sl  <- data.frame(read.csv(file="Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_coarse.csv",sep=",",dec=".", header=T, row.names=1))
    Ir.MP_ss  <- data.frame(read.csv(file="Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_fine.csv",sep=",",dec=".", header=T, row.names=1))
    Ir.MV_sl <- read.table("Resultats/Iran/Vegetation/MV_Ir_PT_sl.csv", sep = ",", header = T, row.names = 1)
    Ir.MV_ss <- read.table("Resultats/Iran/Vegetation/MV_Ir_PT_ss.csv", sep = ",", header = T, row.names = 1)
    Ir.MV_TN <- read.table("Resultats/Iran/Vegetation/MV_Ir.csv", sep = ",", header = T, row.names = 1)
    Ir.eco <- read.table("Import/Iran/Site/Ir_SS_simple.csv", sep = ",", header = T, row.names = 1)
    Ir.index.TN <- read.table("Import/Iran/Vegetation/Indexes/Index_from_Ir_Herbier_auto.csv", sep = ",", header = T, row.names = 1)
    
    #### Ir checklist ####
    Checklist.building.Ir = T
    if(Checklist.building.Ir == T){
      Gol.bo <- "../Stages/Golestan_Medhi/SIG/Golestan_border/Parc_nat_golestan.shp"
      # ChL.Samarqand  <- data.frame(read.csv(file="Import/Iran/Checklist/Samarqand_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Jizzak  <- data.frame(read.csv(file="Import/Iran/Checklist/Jizzakh_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Bukhara  <- data.frame(read.csv(file="Import/Iran/Checklist/Bukhara_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Kashkadarya  <- data.frame(read.csv(file="Import/Iran/Checklist/Kashkadarya_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Nuratau  <- data.frame(read.csv(file="Import/Iran/Checklist/Nuratau_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Chatkal  <- data.frame(read.csv(file="Import/Iran/Checklist/Chatkal_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.TianShan  <- data.frame(read.csv(file="Import/Iran/Checklist/TianShan_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.Tajikistan  <- data.frame(read.csv(file="Import/Iran/Checklist/Tajikistan_checklist.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # ChL.TUSD  <- data.frame(read.csv(file="Resultats/Iran/Checklist/Checklist_MV_TUSD.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
      # 
      # ChL.Ir <- rbind(ChL.Bukhara, ChL.Jizzak, ChL.Samarqand, ChL.Kashkadarya, ChL.Nuratau, ChL.Chatkal, ChL.TianShan[c(1,2)], ChL.Tajikistan[c(1,2)])  
      # ChL.Ir$Species <- paste(ChL.Ir$Genera, ChL.Ir$Species, sep = " ")
      # ChL.Ir <- rbind(ChL.TUSD, ChL.Ir)
      # ChL.Ir <- ChL.Ir[!duplicated(ChL.Ir),]
      # 
      A = Checklist.cleaning(#ChL.Ir$Species,
        TNRS.check = F, Remove.non.pollinic = F, Remove.subsp = F,
        # GIFT.shape.file = TUSD.bo,
        BIEN.shape.file = Gol.bo,
        # TL.APG.Save.RDS = "Resultats/Iran/Checklist/Checklist_MV_TUSD_APG_clean.Rds",
        # TL.Save.RDS = "Resultats/Iran/Checklist/Checklist_MV_TUSD_clean.Rds"
      )
      stop()
      ChL.TUSD.APG <- A$TL.APG
      ChL.TUSD <- A$TL 
      
    }
    else{
      ChL.TUSD.APG <- readRDS("Resultats/Iran/Checklist/Checklist_MV_TUSD_APG_clean.Rds")
      ChL.TUSD <- as_tibble(readRDS("Resultats/Iran/Checklist/Checklist_MV_TUSD_clean.Rds"))
    }
    
    #### Check if all MV in the ChL ####
    Verif.Ir.Taxa.Chl = F
    if(Verif.Ir.Taxa.Chl == T){
      print("Taxa not in the check list")
      # All.taxa.TUSD <- unique(c(row.names(Ir.MP_sl), row.names(Ir.MP_ss), row.names(Ir.MV_sl), row.names(Ir.MV_ss), row.names(Ir.MV_TN)))
      All.taxa.TUSD <- unique(row.names(Ir.MV_TN))
      # All.taxa.TUSD <- gsub("-type", "", All.taxa.TUSD)
      A <- All.taxa.TUSD[!All.taxa.TUSD %in% ChL.TUSD$species]
      A.fam <- A[grep("aceae", A)]
      A.gen <- A[grep("spp.", A)]
      A.sp <- setdiff(A, c(A.fam, A.gen))
      A.gen <- gsub(" spp.", "", A.gen)
      if(length(A.fam[! A.fam %in% ChL.TUSD$family]) > 0){print(A.fam[! A.fam %in% ChL.TUSD$family])}
      if(length(A.gen[! A.gen %in% ChL.TUSD$family]) > 0){print(A.gen[! A.gen %in% ChL.TUSD$genus])}
      if(length(A.sp[! A.sp %in% ChL.TUSD$family]) > 0){print(A.sp[! A.sp %in% ChL.TUSD$species])}
    }
    
    #### Table conversion pollen ####
    Table.conversion.pollen = F
    if(Table.conversion.pollen == T){
      A = Pollen.type.2.Checklist(
        List.PT_ss = unique(c(row.names(Ir.MP_ss), row.names(Ir.MV_ss))),
        List.PT_sl = unique(c(row.names(Ir.MP_sl), row.names(Ir.MV_sl))), 
        Save.path = "Resultats/Iran/Traits/Corresp_tax/Table_corresp_plant_type.Rds",
        TL = ChL.TUSD, TPT = TPT)
      
    }
    #### Traits extraction for the checklist ####
    Trait.extraction.Ir = F
    if(Trait.extraction.Ir == T){
      DB.trait.extraction(TL = ChL.TUSD, Projet.name = "TUSD", Extract.GIFT = F,
                          Extract.BIEN = F, Extract.TRY = F, Extract.BROT = F, Chinese.trait.DB = F)
      
      # A FAIRE: API request pour GIFT
      # le DB.trait.clean ne fonctionne pas pour TUSD
      DB.trait.clean(BIEN.path = paste(DB.path, "Vegetation/Occurences/BIEN/BIEN_TUSD_trait.Rds", sep = ""),
                     # TRY.path = paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_taxa_TUSD.Rds", sep = ""),
                     Projet.name = "TUSD")
    }
    
    #### Traits DB cleaning / merging ####
    Calculate.ext.trait = F
    if(Calculate.ext.trait == T){
      if(exists("BIEN.TUSD") == F){
        BIEN.TUSD <- readRDS(paste(DB.path, "Vegetation/Occurences/BIEN/BIEN_TUSD_trait_clean.Rds", sep = ""))
        TRY.TUSD <- readRDS(paste(DB.path, "Traits/TRY_dec_2019/Extraction/TRY_taxa_TUSD.Rds", sep = ""))
        BROT.TUSD <- as_tibble(readRDS("Import/World_DB/Traits/BROT2.0/BROT_TUSD.Rds"))
        # CPT.TUSD <- as_tibble(readRDS("Import/China/Traits/China_Plant_TraitsDB_csv/CPT_TUSD.Rds"))
        GIFT.TUSD <- as_tibble(readRDS("Import/ACA/Traits/GIFT/20210617_central_asia_traits_TUSD_crop.Rds"))
      }
      
      MT.Ir = DB.merge(
        TRY = TRY.TUSD,
        BROT = BROT.TUSD,
        GIFT = GIFT.TUSD,
        BIEN = BIEN.TUSD,
        TL = ChL.TUSD, TL.APG = ChL.TUSD.APG, SSD.interpol = T, Plot.only.continuous = T, Remove.outliers = T, 
        Save.plot = "Figures/Iran/Traits/Distribution/Trait_dist_hist_TUSD.pdf",
        Tab.stats = "Import/Iran/Traits/Table_traits.csv",
        TBT = "Import/Iran/Traits/Trait_ID_names.csv",
        Save.MT = "Resultats/Iran/Traits/Traits_values/MT_TUSD.Rds")
      
    }
    
    #### Trait aggregation by pollen-types ####
    TPT.aggregation.Ir = F
    if(TPT.aggregation.Ir == T){
      #### Import ####
      MT.Ir.no.scale <- readRDS("Resultats/Iran/Traits/Traits_values/MT_TUSD.Rds")
      MT.Ir_gf <- read.table(file = "Resultats/Iran/Traits/Traits_values/MT_TUSD_scale_gf.txt", sep="\t",dec=".", header=T, stringsAsFactors = F)
      MT.Ir <- readRDS("Resultats/Iran/Traits/Traits_values/MT_TUSD_scaled.Rds")
      MT.Ir.sd <- read.table(file = "Resultats/Iran/Traits/Traits_values/MT_TUSD_scale_gf_sd.txt", sep = "\t", dec=".", header=T)
      Only.hiera <- readRDS("Resultats/Iran/Traits/Traits_values/Hierarchie_gap-filling.Rds")
      Only.trait <- readRDS("Resultats/Iran/Traits/Traits_values/TM_before_gap-filling.Rds")
      MT.bool.ss <- readRDS(file = "Resultats/Iran/Traits/Corresp_tax/Table_corresp_plant_type_bool_ss.Rds")
      MT.bool.sl <- readRDS(file = "Resultats/Iran/Traits/Corresp_tax/Table_corresp_plant_type_bool_sl.Rds")
      Error.display = T
      Fullfill.NA.by.fam = F
      
      #### GF cleaning #####
      MT.Ir.cv <- MT.Ir.sd/MT.Ir_gf
      MT.Ir_gf[MT.Ir.cv > 1 & is.na(Only.trait)] <- NA
      MT.Ir_gf <- as_tibble(cbind(Only.hiera[c(2)], MT.Ir_gf))
      MT.Ir_gf <- full_join(MT.Ir[names(MT.Ir)[!grepl("TRY", names(MT.Ir))]], MT.Ir_gf, by = "species")
      
      #### Functions ####
      print("Let's aggregate pollen-type, bro !")
      Trait.aggregate.by.class <- function(M, by = NULL){
        BY = list(M[[by]])
        By.names <- names(M[by])
        nums <- unlist(lapply(M, is.numeric))  
        M <- M[nums]
        M <- aggregate(M, BY, FUN = mean, na.action = na.pass, na.rm = T)
        names(M)[1] <- By.names
        return(M)
      }
      Trait.aggregate.by.type <- function(TP, MT, name.var){
        Fuck.off <- c("species", "family", "genus", "PT_ss", "PT_sl", "subreign", "GrowthForm", "kingdom", "order", "Other.clade")
        Col.to.keep <- setdiff(names(MT), Fuck.off)
        Row.to.keep <- setdiff(names(TP), Fuck.off)
        
        TP.work <- setNames(data.frame(matrix(NA, ncol = length(Col.to.keep)+1, nrow = length(Row.to.keep))), c(name.var,Col.to.keep))
        TP.work[[name.var]] <- Row.to.keep
        names(TP.work)[1] <- "species"
        
        T.m <- TP.work
        T.sd <- TP.work
        TP <- TP[c("species",Row.to.keep)]
        MT <- data.frame(MT[c("species",Col.to.keep)])
        pb = txtProgressBar(min = 0, max = ncol(TP), initial = 0) 
        print(paste("Aggregation with", name.var))
        for(i in 2:ncol(TP)){
          setTxtProgressBar(pb,i)
          S.to.pick <- TP$species[which(TP[i] == T)]
          for(j in 2:ncol(MT)){
            Val <- MT[which(MT$species %in% S.to.pick), j]
            Trait.mean <- mean(Val, na.rm = T)
            Trait.sd <- sd(Val, na.rm = T)
            T.m[i-1,j] <- Trait.mean
            T.sd[i-1,j] <- Trait.sd
          }
        }
        close(pb)
        # return(list(Mean = T.m, SD = T.sd, Nval = T.n))
        return(list(Mean = T.m, SD = T.sd))
      }
      Keep.no.gf <- function(M.to.fill, M.to.add, id){
        KN <- M.to.fill[[id]]
        M.to.fill[[id]] <- gsub(" spp.", "", M.to.fill[[id]])
        M.to.fill[[id]] <- gsub("-type", "", M.to.fill[[id]])
        Miss.type <- which(M.to.fill[[id]] %in% M.to.add[[id]])
        To.add <- which(M.to.add[[id]] %in% M.to.fill[[id]] )
        for(i in 2:ncol(M.to.fill)){
          for(j in 1:length(Miss.type)){
            if(is.na(M.to.add[To.add[j],i]) == F){
              M.to.fill[Miss.type[j], i] <- M.to.add[To.add[j],i]
            }
          }
        }
        M.to.fill[[id]] <- KN
        return(M.to.fill)}
      Fill.NA.from.fam.gen <- function(MT, Mgen, Mfam){
        print(paste("Fullfill the NAs by the average value for genus and family for the", deparse(substitute(MT))))
        Trait.list <- names(MT)[grep("TRY", names(MT))]
        pb = txtProgressBar(min = 0, max = nrow(MT), initial = 0)
        for(i in Trait.list){
          print(i)
          for(j in 1:nrow(MT)){
            setTxtProgressBar(pb,j)
            if(is.na(MT[which(MT$genus %in% Mgen$genus), i][j])){
              MT[which(MT$genus %in% Mgen$genus), i][j] <- Mgen[match(MT$genus, Mgen$genus), i][j]}
            if(is.na(MT[which(MT$family %in% Mfam$family), i][j])){
              MT[which(MT$family %in% Mfam$family), i][j] <- Mfam[match(MT$family, Mfam$family), i][j]}
          }}
        close(pb)
        return(MT)
      }
      
      #### Aggregate trait family / genus ####
      Tr.PT_fam <- Trait.aggregate.by.class(MT.Ir, by = "family")
      Tr.PT_gen <- Trait.aggregate.by.class(MT.Ir, by = "genus")
      Tr.PT_fam_gf <- Trait.aggregate.by.class(MT.Ir_gf, by = "family")
      Tr.PT_gen_gf <- Trait.aggregate.by.class(MT.Ir_gf, by = "genus")
      
      #### Aggregation pollen-type ####
      Tr.PT_sl.full <- Trait.aggregate.by.type(MT.bool.sl, MT.Ir, "PT_sl") # Normal sl
      Tr.PT_ss.full <- Trait.aggregate.by.type(MT.bool.ss, MT.Ir, "PT_ss") # Normal ss
      Tr.PT_sl.g.full <- Trait.aggregate.by.type(MT.bool.sl, MT.Ir_gf, "PT_sl") # Normal sl
      Tr.PT_ss.g.full <- Trait.aggregate.by.type(MT.bool.ss, MT.Ir_gf, "PT_ss") # Normal sl
      
      Tr.PT_sl.sd <- Tr.PT_sl.full$SD
      Tr.PT_ss.sd <- Tr.PT_ss.full$SD
      Tr.PT_sl_gf.sd <- Tr.PT_sl.g.full$SD
      Tr.PT_ss_gf.sd <- Tr.PT_ss.g.full$SD
      
      Tr.PT_sl <- Tr.PT_sl.full$Mean
      Tr.PT_ss <- Tr.PT_ss.full$Mean
      Tr.PT_sl_gf <- Tr.PT_sl.g.full$Mean
      Tr.PT_ss_gf <- Tr.PT_ss.g.full$Mean
      
      #### Replace par value non gf quand c'est possible (qui est normalement plus fiable que la gf) ####
      Tr.PT_ss_gf <- Keep.no.gf(Tr.PT_ss_gf, Tr.PT_fam, 1)
      Tr.PT_ss_gf <- Keep.no.gf(Tr.PT_ss_gf, Tr.PT_gen, 1)
      Tr.PT_sl_gf <- Keep.no.gf(Tr.PT_sl_gf, Tr.PT_fam, 1)
      Tr.PT_sl_gf <- Keep.no.gf(Tr.PT_sl_gf, Tr.PT_gen, 1)
      Tr.PT_fam_gf <- Keep.no.gf(Tr.PT_fam_gf, Tr.PT_fam, 1)
      Tr.PT_gen_gf <- Keep.no.gf(Tr.PT_gen_gf, Tr.PT_gen, 1)
      
      #### Clean pollen-types ####
      Tr.PT_ss_gf <- cbind(Rank = "PT_ss", GrowthForm = TPT$AP_NAP[match(Tr.PT_ss_gf$species, TPT$PT_ss)], Tr.PT_ss_gf)
      Tr.PT_ss_gf  <- Tr.PT_ss_gf[c(1,3,2,4:ncol(Tr.PT_ss_gf))]
      names(Tr.PT_ss_gf)[names(Tr.PT_ss_gf) == "species"] <- "taxa"
      
      Tr.PT_sl_gf <- cbind(Rank = "PT_sl", GrowthForm = TPT$AP_NAP[match(Tr.PT_sl_gf$species, TPT$PT_sl)], Tr.PT_sl_gf)
      Tr.PT_sl_gf  <- Tr.PT_sl_gf[c(1,3,2,4:ncol(Tr.PT_sl_gf))]
      names(Tr.PT_sl_gf)[names(Tr.PT_sl_gf) == "species"] <- "taxa"
      
      Tr.PT_ss <- cbind(Rank = "PT_ss", GrowthForm = TPT$AP_NAP[match(Tr.PT_ss$species, TPT$PT_ss)], Tr.PT_ss)
      Tr.PT_ss  <- Tr.PT_ss[c(1,3,2,4:ncol(Tr.PT_ss))]
      names(Tr.PT_ss)[names(Tr.PT_ss) == "species"] <- "taxa"
      
      Tr.PT_sl <- cbind(Rank = "PT_sl", GrowthForm = TPT$AP_NAP[match(Tr.PT_sl$species, TPT$PT_sl)], Tr.PT_sl)
      Tr.PT_sl  <- Tr.PT_sl[c(1,3,2,4:ncol(Tr.PT_sl))]
      names(Tr.PT_sl)[names(Tr.PT_sl) == "species"] <- "taxa"
      
      #### Fullfill the NA in the MT species by the genus / fam average values ####
      if(Fullfill.NA.by.fam == T){
        MT.Ir <- Fill.NA.from.fam.gen(MT.Ir, Tr.PT_gen, Tr.PT_fam)
        MT.Ir_gf <- Fill.NA.from.fam.gen(MT.Ir_gf, Tr.PT_gen_gf, Tr.PT_fam_gf)
      }
      
      #### Merge all datasets #### 
      MT.Ir.full <- cbind(Rank = "species", MT.Ir[!names(MT.Ir) %in% c("subreign","kingdom","order","Other.clade","family","genus")])
      names(MT.Ir.full)[names(MT.Ir.full) == "species"] <- "taxa"
      Tr.PT_fam  <- cbind(Rank = "family", Tr.PT_fam, GrowthForm = NA)
      names(Tr.PT_fam)[names(Tr.PT_fam) == "family"] <- "taxa"
      Tr.PT_gen  <- cbind(Rank = "genus", Tr.PT_gen, GrowthForm = NA)
      names(Tr.PT_gen)[names(Tr.PT_gen) == "genus"] <- "taxa"
      Tr.PT_gen$taxa <- paste(Tr.PT_gen$taxa, "spp.", sep = " ")
      Tr.PT_gen  <- rbind(Tr.PT_gen, Tr.PT_fam)
      Tr.PT_gen  <- Tr.PT_gen[c(1,2,ncol(Tr.PT_gen),3:(ncol(Tr.PT_gen)-1))]
      MT.Ir.full  <- as_tibble(rbind(MT.Ir.full, Tr.PT_gen, Tr.PT_sl, Tr.PT_ss))
      
      MT.Ir.full_gf <- cbind(Rank = "species", MT.Ir_gf[!names(MT.Ir_gf) %in% c("subreign","kingdom","order","Other.clade","family","genus")])
      names(MT.Ir.full_gf)[names(MT.Ir.full_gf) == "species"] <- "taxa"
      Tr.PT_fam_gf  <- cbind(Rank = "family", Tr.PT_fam_gf, GrowthForm = NA)
      names(Tr.PT_fam_gf)[names(Tr.PT_fam_gf) == "family"] <- "taxa"
      Tr.PT_gen_gf  <- cbind(Rank = "genus", Tr.PT_gen_gf, GrowthForm = NA)
      names(Tr.PT_gen_gf)[names(Tr.PT_gen_gf) == "genus"] <- "taxa"
      Tr.PT_gen_gf$taxa <- paste(Tr.PT_gen_gf$taxa, "spp.", sep = " ")
      Tr.PT_gen_gf  <- rbind(Tr.PT_gen_gf, Tr.PT_fam_gf)
      Tr.PT_gen_gf  <- Tr.PT_gen_gf[c(1,2,ncol(Tr.PT_gen_gf),3:(ncol(Tr.PT_gen_gf)-1))]
      MT.Ir.full_gf  <- as_tibble(rbind(MT.Ir.full_gf, Tr.PT_fam_gf, Tr.PT_gen_gf, Tr.PT_sl_gf, Tr.PT_ss_gf))
      
      #### Check if missing taxa in front of splot and surf pol DB ####
      if(Error.display == T){
        print(row.names(Ir.MP_sl)[!row.names(Ir.MP_sl) %in%  MT.Ir.full_gf$taxa[MT.Ir.full_gf$Rank == "PT_sl"]])
        print(row.names(Ir.MP_sl)[!row.names(Ir.MP_sl) %in%  MT.Ir.full$taxa[MT.Ir.full$Rank == "PT_sl"]])
        print(row.names(Ir.MP_ss)[!row.names(Ir.MP_ss) %in%  MT.Ir.full_gf$taxa[MT.Ir.full_gf$Rank == "PT_ss"]])
        print(row.names(Ir.MP_ss)[!row.names(Ir.MP_ss) %in%  MT.Ir.full$taxa[MT.Ir.full$Rank == "PT_ss"]])
        print(row.names(Ir.MV_ss)[!row.names(Ir.MV_ss) %in%  MT.Ir.full$taxa[MT.Ir.full$Rank == "PT_ss"]])
        print(row.names(Ir.MV_sl)[!row.names(Ir.MV_sl) %in%  MT.Ir.full$taxa[MT.Ir.full$Rank == "PT_sl"]])
        print(row.names(Ir.MV_TN)[!row.names(Ir.MV_TN) %in%  MT.Ir.full$taxa[MT.Ir.full$Rank %in% c("species", "genus", "family")]])
      }
      
      #### Export data ####
      saveRDS(MT.Ir.full_gf, "Resultats/Iran/Traits/Traits_values/MT.Ir.full_gf.Rds")
      saveRDS(MT.Ir.full, "Resultats/Iran/Traits/Traits_values/MT.Ir.full.Rds")
    }
    else{
      MT.Ir.full_gf <- readRDS("Resultats/Iran/Traits/Traits_values/MT.Ir.full_gf.Rds")
      MT.Ir.full <- readRDS("Resultats/Iran/Traits/Traits_values/MT.Ir.full.Rds")
    }
    
    #### CWM-traits calculation ####
    CWM.calculation = F
    if(CWM.calculation == T){
      Seuil.pcover = 50
      MT.Ir.MP_ss_gf <- MT.Ir.full_gf[MT.Ir.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Ir.full_gf))]
      MT.Ir.MP_ss_gf <- MT.Ir.full_gf[MT.Ir.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Ir.full_gf))]
      MT.Ir.MP_sl_gf <- MT.Ir.full_gf[MT.Ir.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Ir.full_gf))]
      MT.Ir.MV_sl_gf <- MT.Ir.full_gf[MT.Ir.full_gf$Rank == "PT_sl", c(2,4:ncol(MT.Ir.full_gf))]
      MT.Ir.MV_ss_gf <- MT.Ir.full_gf[MT.Ir.full_gf$Rank == "PT_ss", c(2,4:ncol(MT.Ir.full_gf))]
      MT.Ir.MV_sl <- MT.Ir.full[MT.Ir.full$Rank == "PT_sl", c(2,4:ncol(MT.Ir.full))]
      MT.Ir.MP_sl <- MT.Ir.full[MT.Ir.full$Rank == "PT_sl", c(2,4:ncol(MT.Ir.full))]
      MT.Ir.MV_ss <- MT.Ir.full[MT.Ir.full$Rank == "PT_ss", c(2,4:ncol(MT.Ir.full))]
      MT.Ir.MP_ss <- MT.Ir.full[MT.Ir.full$Rank == "PT_ss", c(2,4:ncol(MT.Ir.full))]
      MT.Ir.MV_NT <- MT.Ir.full[MT.Ir.full$Rank %in% c("species", "genus", "family"), c(2,4:ncol(MT.Ir.full))]
      MT.Ir.MV_NT_gf <- MT.Ir.full[MT.Ir.full$Rank %in% c("species", "genus", "family"), c(2,4:ncol(MT.Ir.full))]
      
      MCWT.clim.Ir.MV.PT_sl_gf <- CWT.calculation(MT = MT.Ir.MV_sl_gf, MP = Ir.MV_sl, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MV.PT_sl <- CWT.calculation(MT = MT.Ir.MV_sl, MP = Ir.MV_sl, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MV.PT_ss_gf <- CWT.calculation(MT = MT.Ir.MV_ss_gf, MP = Ir.MV_ss, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MV.PT_ss <- CWT.calculation(MT = MT.Ir.MV_ss, MP = Ir.MV_ss, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MV.NT <- CWT.calculation(MT = MT.Ir.MV_NT, MP = Ir.MV_TN, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MV.NT_gf <- CWT.calculation(MT = MT.Ir.MV_NT_gf, MP = Ir.MV_TN, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MP_sl <- CWT.calculation(MT = MT.Ir.MP_sl, MP = Ir.MP_sl, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MP_sl_gf <- CWT.calculation(MT = MT.Ir.MP_sl_gf, MP = Ir.MP_sl, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MP_ss <- CWT.calculation(MT = MT.Ir.MP_ss, MP = Ir.MP_ss, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
      MCWT.clim.Ir.MP_ss_gf <- CWT.calculation(MT = MT.Ir.MP_ss_gf, MP = Ir.MP_ss, Mclim = Ir.climtot, MPS.ACA.Biom = Ir.Biom, Accep.seuil = Seuil.pcover)
    }
  }
  
  #### Plots results ####
  Plot.CWM.Ir = F
  if(Plot.CWM.Ir == T){
  #### LR comparaison CWM-t MP / MV ####
  LR.comparaison = F
  if(LR.comparaison == T){
    MV = MCWT.clim.Ir.MV.PT_ss_gf$MCWT
    MP = MCWT.clim.Ir.MP_ss_gf$MCWT
    
    LR.CWM(MP = MP, MV = MV,
           # Keep.taxa = names(MV)[grep("TRY_", names(MV))][c(1:10)],
           Keep.taxa = c("TRY_SSD", "TRY_Height", "TRY_SLA", "TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass"),
           H = 500, W = 700, R2.pos = "bottomright", Scale = "free",
           Save.plot = "Figures/Iran/Traits/LR_CWM_comp.pdf")
    
    LR.CWM.clim(MT = list(MP = MP, MV = MV), Meco = list(Iran = Ir.eco),
                Keep.taxa = c("TRY_SSD", "TRY_Height", "TRY_SLA", "TRY_LeafArea", "TRY_LeafN", "TRY_SeedMass"),
                # Keep.taxa = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia spp."),
                Keep.clim = c("Altitude", "MAP", "MAAT", "TS"),
                H = 1200, W = 1200, Strip.lab = F, R2.pos = "none",
                Save.plot = "Figures/Iran/Traits/LR_CWM_clim.pdf")
  }
  
  }
}
