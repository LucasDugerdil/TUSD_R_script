#### Global path ####
#setwd("/media/lucas.dugerdil/Maximator/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/home/lucas.dugerdil/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
# setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/R_stats") 
setwd("/home/lucas.dugerdil/Documents/Recherche/R_stats") 

#### Import ####
library("readODS")
library("dplyr")
library(patchwork)
library(tibble)
library(ggplot2)
library(ggpmisc)
library(reshape2)
library(ggtext) # Species en italic

if(exists("Herbier") == F){
  Herbier <- data.frame(read_ods("/home/lucas.dugerdil/Documents/Recherche/Herbier/Herbier_numerique/Index.ods"))
  }

#### FUNCTIONS ####
# Import des releves a 10m
# Il faut chaque releve dans un fichier csv, tous dans le meme dossier
# On choisit le chemin du dossier + le separateur dans le fichier csv (, . ; \t)
Import.releves.10m.multitable <- function(Veget.path, Csv.sep, Plus.convert){
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Plus.convert)){Plus.convert = 0.5}
  Lname.file <- dir(Veget.path, pattern = "csv")
  MV.10m <- list()
  for(i in Lname.file){
    Path.file <- paste(Veget.path, i, sep = "/")
    Name.tab <- gsub("\\.csv", "", i)
    Index = which(Lname.file == i)
    MV.10m[[Index]] <- read.table(Path.file, sep = Csv.sep, row.names = 1, check.names = F, header = T, na.strings = c("","NA"), stringsAsFactors = F)
    names(MV.10m)[Index] <- Name.tab
    RN <- row.names(MV.10m[[Index]])
    # print(RN)
    MV.10m[[Index]] <- replace(MV.10m[[Index]], MV.10m[[Index]] == "+", Plus.convert)
    MV.10m[[Index]] <- replace(MV.10m[[Index]], is.na(MV.10m[[Index]]), 0)
    MV.10m[[Index]] <- sapply(MV.10m[[Index]], function(x) as.double(as.character(x)))   # convert data in numeric
    row.names(MV.10m[[Index]]) <- RN
    }
  
  return(MV.10m)
}

# Import des relevés mais dans un unique fichier csv
Import.releves.10m.unitable <- function(Releve.path, Site.path, Com.path, Csv.sep, Plus.convert, Error.display){
  #### Settings ####
  if(missing(Releve.path)){print("Import the path of the unique vegetation table in csv.")}
  if(missing(Site.path)){print("Import the path of the site list in csv.")}
  if(missing(Com.path)){Com.path = NULL}
  if(missing(Csv.sep)){Csv.sep = ","}
  if(missing(Plus.convert)){Plus.convert = 0.5}
  if(missing(Error.display)){Error.display = T}
  
  
  #### Import data ####
  List.sites <- read.table(Site.path, sep = Csv.sep, check.names = F, header = T, na.strings = c("","NA"), stringsAsFactors = F)
  Releve.x <- read.table(Releve.path, sep = Csv.sep, check.names = F, header = F, na.strings = c("","NA"), stringsAsFactors = F)
  if(is.null(Com.path) == F){Com.x <- read.table(Com.path, sep = Csv.sep, check.names = F, header = F, na.strings = c("","NA"), stringsAsFactors = F)}
  
  #### Clean relevé ####
  Releve.x <- split(Releve.x, cumsum(1:nrow(Releve.x) %in% which(apply(Releve.x, 1, function(x) all(is.na(x)))))) 
  names(Releve.x) <- List.sites[[1]][1:length(names(Releve.x))]
  Releve.x[[1]] <- rbind(NA, Releve.x[[1]])
   
  Clean.releve <- function(x){
    x <- x[-c(1),]
    x <- x[!apply(x, 2, function(y) all(is.na(y)))]
    names(x) <- x[1,]
    x <- x[-c(1),]
    RN <- x[,1]
    x <- x[,-c(1)]
    x[is.na(x)] <- 0
    x[x == "1S" | x == "1E" | x == "1N" | x == "1W"] <- '1'
    x[x == "4S" | x == "4E" | x == "4N" | x == "4W"] <- '4'
    x <- replace(x, x =="+", Plus.convert)
    x <- sapply(x, function(x) suppressWarnings(as.numeric(as.character(x), fixed = T)))   # convert data in numeric
    row.names(x) <- RN
    return(x)
  }
  
  Releve.x <- lapply(Releve.x, function(x) Clean.releve(x))
  if(Error.display == T){
    print(paste("Nb sites:", dim(List.sites)[1]))
    print(paste("Nb relevés:", length(Releve.x)))
    }
  
  #### Add Community to relevé ####
  if(is.null(Com.path) == F){
    Com.x <- split(Com.x, cumsum(1:nrow(Com.x) %in% which(apply(Com.x, 1, function(x) all(is.na(x)))))) 
    names(Com.x) <- List.sites[[1]][1:length(names(Com.x))]
    if(Error.display == T){print(paste("Nb communauté:", length(names(Com.x))))}
    
    Com.x[[1]] <- rbind(NA, Com.x[[1]])
    Com.add <- function(x, y){
      y <- y[-c(1),]
      Keep.RN <- row.names(x)
      # print(duplicated(Keep.RN))
      if(length(Keep.RN[duplicated(Keep.RN)]) > 0 & Error.display == T){print(paste("*** ERROR:", Keep.RN[duplicated(Keep.RN)], "is duplicated in the releve ! ***"))}
      x <- data.frame(x)
      row.names(x) <- Keep.RN
      names(x) <- gsub("X", "", names(x))
      for(i in 1:nrow(y)){
        New.col.name <- y[i,1]
        Calc <- y[i,2]
        Calc <- unlist(strsplit(Calc, split = "\\+"))
        N = length(Calc)
        Calc <- paste("x$`", Calc, "`", sep = '')
        Calc <- paste("(", paste(Calc, collapse = '+ '), ")/", N, sep = '')
        Calc <- round(eval(parse(text = Calc)), digits = 1)
        if(length(Calc) > 0){
          x <- cbind(x, NEW = Calc)
          names(x)[ncol(x)] <- New.col.name}
        
        Com.list <- names(x)[grep("C", names(x)[1:ncol(x)])[-1]]
        Com.list <- Com.list[order(Com.list)]
        x[is.na(x)] <- 0
        
        x <- x[c(match(setdiff(names(x), Com.list),names(x)), match(Com.list, names(x)))]
        # print(x)
        
        }
      return(x)
      }
    
    Releve.x <- Releve.x[1:length(Com.x)]
    Releve.x <- mapply(Com.add, x = Releve.x, y = Com.x)}
  
  return(Releve.x)
  }

# On importe un index de vegetation : correspondance entre les types et les taxons, famille, pollen, nature et couche
# Si on indique les matrices de vegetation, on a une verification des types manquants.
Import.index <- function(Index.path, MV, Herbier, Herbier.country, Simplify.Vtype, Add.ubiquist,
                         TNRS.Accepted, Inerte.path, add.CF = F, add.subsp = F,
                         Save.path, Csv.sep, Error.display, Auto.add.miss){
  #### Settings ####
  Check.corresp = T
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Herbier)){Herbier = NULL}
  if(missing(Index.path)){Index.path = NULL}
  if(missing(Inerte.path)){Inerte.path = NULL}
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Add.ubiquist)){Add.ubiquist = F}
  if(missing(Error.display)){Error.display = T}
  if(missing(Auto.add.miss)){Auto.add.miss = F}
  if(missing(Simplify.Vtype)){Simplify.Vtype = F}
  if(missing(TNRS.Accepted)){TNRS.Accepted = F}
  if(missing(MV)){Check.corresp = F
                  print("Add vegetation matrix to check correlation with the index.")}
  
  
  #### Import index + check redondance types ####
  if(is.null(Herbier) == F){
    print("Import index from Herbier.")
    Index <- Herbier[which(Herbier$Country %in% Herbier.country),]
    if(TNRS.Accepted == T){
      
      if(add.CF == F & add.subsp == F){Index <- Index[,c(2:5,16,17,18,24:26)]}
      else{Index <- Index[,c(2:6,16,17,18,24:26)]}
      Index$Family <- Index$TNRS_Family
      Taxa.to.change <- grepl("Invalid", Index$Status_taxo) | grepl("Synon", Index$Status_taxo) | grepl("Illegitimate", Index$Status_taxo)
      Index$Genus[Taxa.to.change] <- gsub("\\s.*", "", Index$Accepted_name[Taxa.to.change])
      Index$Species[Taxa.to.change] <- gsub(".*\\s", "", Index$Accepted_name[Taxa.to.change])
      
      if(add.subsp == T){
        Index$Variety[grep("var.", Index$Accepted_name)] <- paste("var.", gsub(".*\\s", "", Index$Accepted_name[grep("var.", Index$Accepted_name)]), sep = " ")
        Index$Variety[grep("subsp.", Index$Accepted_name)] <- paste("subsp.", gsub(".*\\s", "", Index$Accepted_name[grep("subsp.", Index$Accepted_name)]), sep = " ")
        }
      
      if(add.CF == F | add.subsp == F |(add.CF == T | add.subsp == T)){Index <- Index[-c(9:11)]}
      if(add.CF == F & add.subsp == F){Index <- Index[-c(8:10)]}
      }
    else{
      if(add.CF == F & add.subsp == F){Index <- Index[,c(2:5,16,17,18)]}
      else{Index <- Index[,c(2:6,16,17,18)]}
      }
    
    Index$Species[!is.na(Index$Genus) & is.na(Index$Species)] <- "spp."
    if(add.CF == T){Index$Species[grepl("CF", Index$Variety)] <- paste("CF", Index$Species[grepl("CF", Index$Variety)], sep = " ")}
    if(add.subsp == T){
    Index$Species[grepl("subsp.", Index$Variety) | grepl("var.", Index$Variety)] <- paste(Index$Species[grepl("subsp.", Index$Variety) | grepl("var.", Index$Variety)], Index$Variety[grepl("subsp", Index$Variety) | grepl("var", Index$Variety)], sep = " ")}
    if(add.CF == T | add.subsp == T){Index <- Index[,-c(5)]}
    
    Index$Species <- paste(Index$Genus, Index$Species, sep = " ")
    Index$Species[grepl("NA NA", Index$Species)] <- NA
    VegI <- Index
    names(VegI)[1] <- "Vtype"
    VegI$Type <- "Veget"
    # print(names(VegI))
      }
  if(is.null(Index.path) == F){
    print("Import index from csv file.")
    VegI <- data.frame(read.table(Index.path, sep = Csv.sep, check.names = F, header = T, na.strings = c("","NA"), stringsAsFactors = F))
    }
  if(Error.display == T){
     Test <- VegI[which(duplicated(VegI[1]) == T),]
     Test <- Test[!is.na(Test$Vtype),]
     if(length(row.names(Test)) != 0){
       print("The following types are duplicated in the DB :")
       print(Test)
       print("(the TYPE column in the Herbier are duplicate for 2 differents entries.")
      }
     else{print("No duplicates in Index.")}
     
     # Test2 <- VegI[c(4:7)]
     # # print(Test2[duplicated(Test2$Species) & !duplicated(Test2[c(2:4)]),])
     }
  
  #### Check correspondance type releve / type index ####
  if(Check.corresp == T){
    All.veget.type <- c()
    
    #### Name simplification if TRUE ####
    if(Simplify.Vtype == T){
    Clean.Vtype <- function(x, what){
      if(what == "row.names"){N <- row.names(x)}
      if(what == "Vtype"){N <- x$Vtype}
      N <- gsub("sp.", "sp", N)
      N <- gsub("\\.", "_", N)
      N <- gsub(" ", "_", N)
      N <- gsub("è", "e", N)
      N <- gsub("é", "e", N)
      N <- gsub("?", "", N)
      N <- gsub("/", "_", N)
      N <- gsub("*^_", "", N)
      N <- gsub("__", "_", N)
      N <- tolower(N)
      # N <- toupper(letters(N)[1])
      
      if(what == "row.names"){row.names(x) <- N}
      if(what == "Vtype"){x$Vtype <- N}
      return(x)
      }
    
    MV <- lapply(MV, function(x) Clean.Vtype(x, "row.names"))
    VegI <- Clean.Vtype(VegI, "Vtype")
    }
    
    #### Add inertes-types ####
    if(is.null(Inerte.path) == F){
      Inertes <- data.frame(read.table(Inerte.path, sep = Csv.sep, check.names = F, header = T, na.strings = c("","NA"), stringsAsFactors = F))
      names(Inertes)[1] <- "Vtype"
      VegI <- full_join(VegI, Inertes,by = c("Vtype", "Type"))
      }
    
    #### Match/mismatch MV vs. VegI ####
    for(i in MV){All.veget.type <- c(All.veget.type, row.names(i))}
    All.veget.type <- unique(All.veget.type)
    All.veget.index <- VegI$Vtype
    
    Type.SD <- setNames(data.frame(All.veget.type[grepl(" SD", All.veget.type)]), "SD")
    Type.SD[["Vtype"]] <- gsub(" SD", "", Type.SD$SD)
    All.veget.type <- setdiff(All.veget.type, Type.SD$SD)
    
    Corres.check <- setdiff(All.veget.type,All.veget.index)
    Corres.check.SD <- setdiff(Type.SD$Vtype,All.veget.index)
    
    #### Add the cosmopolites + spp. ####
    if(Add.ubiquist == T){
      #### Cas des sp. ####
      Indiff.gen <- data.frame(Vtype = Corres.check[grep("sp.$", Corres.check)])
      Indiff.gen$Genus <- gsub(" sp.", "", Indiff.gen$Vtype)
      Herbier.Indiff.gen <- Herbier[Herbier$Genus %in% Indiff.gen$Genus, c(3,4,16:18)] 
      # print(Indiff.gen$Genus)
      Herbier.Indiff.gen <- Herbier.Indiff.gen[complete.cases(Herbier.Indiff.gen),]
      Herbier.Indiff.gen <- Herbier.Indiff.gen[!duplicated(Herbier.Indiff.gen),]
      
      P1 <- Herbier.Indiff.gen[duplicated(Herbier.Indiff.gen$Genus) | duplicated(Herbier.Indiff.gen$Genus, fromLast = T),]
      # print(P1)
      P1 <- P1[P1$GF != "Unknown",]
      P2 <- Herbier.Indiff.gen[!duplicated(Herbier.Indiff.gen$Genus) & !duplicated(Herbier.Indiff.gen$Genus, fromLast = T),]
               
      # print(Indiff.gen)
      Herbier.Indiff.gen <- rbind(P1, P2)
      # print(Herbier.Indiff.gen)
      # Still.missing <- setdiff(Indiff.gen$Genus, Herbier.Indiff.gen$Genus)
      # print(Still.missing)
      Indiff.gen <- full_join(Herbier.Indiff.gen, Indiff.gen, by = "Genus")
      Indiff.gen <- Indiff.gen[!duplicated(Indiff.gen$Genus),]
      Indiff.gen <- Indiff.gen[order(Indiff.gen$Genus),]
      Indiff.gen$Species <- paste(Indiff.gen$Genus, "spp.")
      Indiff.gen <- Indiff.gen[, c(6,1,2,7,3:5)]
      Indiff.gen$Type <- "Veget"
      Indiff.gen <- Indiff.gen[complete.cases(Indiff.gen),]
      Corres.check <- setdiff(Corres.check, Indiff.gen$Vtype)
      # print(Corres.check)
      # print(Indiff.gen)
      
      ### Cas des ubiquistes ####
      Ubi <- Corres.check[sapply(gregexpr("[[:alpha:]]+", Corres.check), function(x) sum(x > 0)) == 2]
      Ubi <- data.frame(Vtype =Ubi[!grepl("[1,2,3]", Ubi) & !grepl(" / ", Ubi) & !grepl(" t$", Ubi)])
      Ubi$Species <- Ubi$Vtype
      Herbier.ubi <- Herbier[which(paste(Herbier$Genus, Herbier$Species) %in% Ubi$Vtype), c(3,4,5,16:18)] 
      # print(Herbier.ubi)
      Herbier.ubi <- Herbier.ubi[!duplicated(Herbier.ubi),]
      Herbier.ubi$Species <- paste(Herbier.ubi$Genus, Herbier.ubi$Species) 
      Ubi <- full_join(Herbier.ubi, Ubi, by = "Species")
      Ubi <- Ubi[, c(7,1:6)]
      Ubi$Type <- "Veget"
      Ubi <- Ubi[complete.cases(Ubi),]
      # print(Ubi)
      Corres.check <- setdiff(Corres.check, Ubi$Vtype)
      
      #### Cas des familles ####
      Ubi.fam <- data.frame(Vtype = Corres.check[grep("eae$", Corres.check)])
      Ubi.fam$Family <- Ubi.fam$Vtype
      Ubi.fam$Genus <- NA
      Ubi.fam$Species <- NA
      Ubi.fam$PT.ss <- Ubi.fam$Vtype
      Ubi.fam$PT.sl <- Ubi.fam$Vtype
      Ubi.fam$GF <- "Unknown"
      Ubi.fam$Type <- "Veget"
      Corres.check <- setdiff(Corres.check, Ubi.fam$Vtype)
      # print(Corres.check)
      names(Ubi.fam) <- c("Vtype", "Family", "Genus", "Species", "PT.sl", "PT.ss", "GrowthForm", "Type")
      # print(names(Ubi.fam))
      # print(names(VegI))
      # print(names(Ubi))
      # print(names(Indiff.gen))
      VegI <- rbind(VegI, Ubi, Ubi.fam, Indiff.gen)
    }
    
    # print(VegI)
    #### Show the missing taxa ####
    VegI <- VegI[!is.na(VegI$Vtype),]
    if(length(Corres.check) == 0 & Error.display == T){print("The index perfectly match the datas !")}
    if(length(Corres.check) == 1 & Error.display == T){
      print(paste("The following type is missing from the index :", Corres.check))}
    
    # if(length(Corres.check) > 1){
      if(Error.display == T){
        print("The following types are missing from the index :")
        print(Corres.check)}
      if(Auto.add.miss == T){
        To.add <- matrix(NA, nrow = length(Corres.check), ncol = (ncol(VegI)-1))
        To.add <- data.frame(cbind(Corres.check, To.add))
        colnames(To.add) <- colnames(VegI)
        VegI.used <- VegI[which(VegI$Vtype %in% intersect(All.veget.type,All.veget.index)),]
        To.add <- data.frame(rbind(VegI.used, To.add))
        
        To.add.SD <- VegI[VegI$Vtype %in% intersect(VegI$Vtype, Type.SD$Vtype),]
        To.add.SD <- full_join(To.add.SD, Type.SD, by = "Vtype", keep = F)
        To.add.SD <- setNames(To.add.SD[c(9,2:8)], c("Vtype", "Family", "Genus", "Species", "PT.sl", "PT.ss", "GrowthForm", "Type")) 
        To.add <- rbind(To.add, To.add.SD)
        if(Add.ubiquist == T){To.add <- rbind(To.add, Indiff.gen, Ubi, Ubi.fam)}
        To.add <- To.add[order(To.add$Vtype),]
        Herb.not.used <- setdiff(Index$Type, To.add$Vtype)
        # Herb.not.used <- Herb.not.used[order(Herb.not.used)]
        # Herb.not.used <- Herbier[match(Herb.not.used, Herbier$Type), c(2:5,9)]
        if(Error.display == T){
          if(all(is.na(Herb.not.used)) == F){
            print("The following types are in the index but not used in the relevés :")
            Herb.not.used <- Index[match(Herb.not.used, Index$Type), c(1:5)]
            print(Herb.not.used)}
          }

        
        
        if(is.null(Index.path) == F){Index.path2 <- gsub("\\.csv", "_autoadd.csv", Index.path)}
        else{
          if(is.null(Save.path) == F){
            Index.path2 <- Save.path
            Index.path3 <- gsub("\\.csv", "_missing_in_herbier.csv", Index.path2)
            Index.path4 <- gsub("\\.csv", "_not_used_in_herbier.csv", Index.path2)
          }
          else{print("Please add the path to save the index.")}
          }
        write.table(To.add, file = Index.path2, row.names = F, sep = Csv.sep, dec = ".")
        write.table(Corres.check, file = Index.path3, row.names = F, sep = Csv.sep, dec = ".")
        write.table(Herb.not.used, file = Index.path4, row.names = F, sep = Csv.sep, dec = ".")
        return(To.add)
        # }
    }
    else{
      write.table(VegI, file = Save.path, row.names = F, sep = Csv.sep, dec = ".")
      }
    }
  #### Last check ####
  if(Error.display == T & "GrowthForm" %in% names(VegI)){
    Test2 <- VegI[c(1:7)]
    if(nrow(Test2[duplicated(Test2$Vtype),]) > 0){
      print("*** Attention les taxons suivants présentes des incongruences (PT.sl, PT.ss, GrowthForm to check)")
      print(Test2[duplicated(Test2$Vtype),])
      }}
  
  return(VegI)
  }

# Conversion des releves de terrain
# Rapport sur 100
# Possibilite d'enlever les abiotiques (pierre, mousse...)
Convert.releves.10m <- function(MV, Remove.abiot, Tree.Herb, Taxa.name, PT, Keep.unknown.type = F,
                                Nb.tree.layers = 2, Family.name, Pollen.type, Index, Merge.releve, Error.display){
  #### Settings ####
  if(missing(Remove.abiot)){Remove.abiot = F }
  if(missing(Tree.Herb)){Tree.Herb = F }
  if(missing(Taxa.name)){Taxa.name = F }
  if(missing(Family.name)){Family.name = F }
  if(missing(Pollen.type)){Pollen.type = F }
  
  if(missing(Index)){Remove.abiot = F
                    Taxa.name = F 
                    Family.name = F}
  if(missing(Merge.releve)){Merge.releve = T}
  if(missing(Error.display)){Error.display = T}
  if(missing(PT)){PT = "ss"}
  
  #### Fusion multi part releves ####
  if(Merge.releve == T){
    names(MV) <- gsub("_2","", names(MV)) 
    Count = c()
    for(i in 1:(length(MV)-1)){
      if(names(MV)[i+1] == names(MV)[i]){
        Count = c(Count, i)
        }
      
      NewMat <- list()
      K = 1
      for(i in Count){
        A = merge(data.frame(t(MV[[i]]), check.names = F), data.frame(t(MV[[i+1]]), check.names = F), all = T, by = 0, sort = F)
        row.names(A) <- gsub("X","",A[,1])
        A <- replace(A, is.na(A), 0)
        A <- subset(A, select = -c(Row.names))
        A <- data.frame(t(A), check.names = F)
        NewMat[[K]] <- A
        names(NewMat)[K] <- names(MV[i])
        K = K + 1  
        }
      }
    Keep <- 1:length(MV)
    B <- Count + 1
    Count = c(Count, B)
    Keep <- Keep[-Count]
    MV <- MV[Keep]
    MV <- c(MV, NewMat)
    }
  
  #### Séparation matrice Tree / herbs ####    
  if(Tree.Herb == T){
    #### List of Trees ####
    List.tree <- Index$Vtype[which(Index$GrowthForm == "Tree")]
    List.tree <- List.tree[!grepl("\\sSD", List.tree)]
    List.tree <- c(List.tree, "Ciel")
    
    if(Error.display == T){
      print("Mean of tree / herbs layers applied. The tree taxa are :")
      print(List.tree)}

    #### Splitting the 2 matrixes ####
    MV.tree <- MV
    for(j in 1:length(MV)){
      k <- MV[[j]]
      MV[[j]] <- k[setdiff(row.names(k), List.tree),]
      MV.tree[[j]] <- k[intersect(row.names(k), List.tree),]
      }
  
    #### Percentage correction avec INERTE ####
    for(i in 1:length(MV)){
      MV[[i]] <- t(MV[[i]])
      MV[[i]] <- MV[[i]]/rowSums(MV[[i]])*100
      MV[[i]] <- t(MV[[i]])
      Test <- as.double(unique(colSums(MV[[i]])))
      if(round(min(Test, na.rm = T),0)<100){
        print(paste("Erreur quadrat ", names(MV)[[i]],". La somme des taxons n'est pas egale a 100 mais a ", Test[1], ".", sep = ""))}
      
      if(is.null(nrow(MV.tree[[i]]))==0){
        if(nrow(MV.tree[[i]]) != 0){
          MV.tree[[i]] <- t(MV.tree[[i]])
          MV.tree[[i]] <- MV.tree[[i]]/rowSums(MV.tree[[i]])*100
          MV.tree[[i]] <- t(MV.tree[[i]])
          MV.tree[[i]][is.nan(MV.tree[[i]])] <- 0
        }
        else{
          MV.tree[[i]] <- rbind(MV.tree[[i]], rep(100, ncol(MV.tree[[i]])))
          row.names(MV.tree[[i]]) <- "Ciel"
          colnames(MV.tree[[i]]) <- colnames(MV[[i]])
        }
      }
    }
    
    #### Re-merging AP / NAP ####
    if(Tree.Herb == T){
      for(i in 1:length(MV)){
        MV[[i]] <- rbind(MV[[i]], MV.tree[[i]]*Nb.tree.layers)
      }
    }
  
  Divide.Nb.layer <- lapply(MV, function(A) colSums(A) != 100)
  }
  
  #### Remove abiotic values ####
  if(Remove.abiot == T){
    if("Nature" %in% names(Index)){names(Index)[match("Nature", names(Index))] <- "Type"}
    
    if(Error.display == T){
      print("Removing the following abiotic surfaces :")
      print(Index$Vtype[Index$Type %in% c("Inerte", "NPP")])
      }
      
    for(j in 1:length(MV)){ 
      k <- MV[[j]]
      Param <- length(setdiff(row.names(k),Index$Vtype[Index$Type  %in% c("Inerte", "NPP")]))
      if(Param > 1){
      MV[[j]] <- k[setdiff(row.names(k),Index$Vtype[Index$Type  %in% c("Inerte", "NPP")]),]}
      else{
        KN <- colnames(k)
        RN <- setdiff(row.names(k),Index$Vtype[Index$Type  %in% c("Inerte", "NPP")])
        MV[[j]] <- data.frame(t(k[setdiff(row.names(k),Index$Vtype[Index$Type  %in% c("Inerte", "NPP")]),]))
        names(MV[[j]]) <- KN
        row.names(MV[[j]]) <- RN
        }
      }
    }
  
  #### Percentage correction sans INERTE ####
  for(i in 1:length(MV)){
    MV[[i]] <- t(MV[[i]])
    MV[[i]] <- MV[[i]]/rowSums(MV[[i]])*100
    MV[[i]] <- t(MV[[i]])
    Test <- as.double(unique(colSums(MV[[i]])))
    if(round(min(Test, na.rm = T),0)<100){
      print(paste("Erreur quadrat ", names(MV)[[i]],". La somme des taxons n'est pas egale a 100 mais a ", Test[1], ".", sep = ""))}
    }
  

  #### Change to taxa name ####
  Choix.name = c(Taxa.name, Family.name, Pollen.type)
  if(any(Choix.name == T)){
    for(j in 1:length(MV)){
      k <- row.names(MV[[j]])
      # print(k)
      new.name <- c()
      if(Taxa.name == T){
        if("Species" %in% names(Index)){names(Index)[match("Species", names(Index))] <- "Vtaxa"}
        for(h in k){
          # print(h)
          if(sjmisc::is_empty(Index$Vtaxa[Index$Vtype == h]) == T){
            if(h %in% Index$Vtype){
              if(Keep.unknown.type == T){new.name <- c(new.name, h)}
              else{
                if(!is.na(Index$Family[Index$Vtype == h])){
                  new.name <- c(new.name, Index$Family[Index$Vtype == h])
                  }
                else{
                  # print(h)
                  if(Keep.unknown.type == T){new.name <- c(new.name, h)}
                  else{new.name <- c(new.name, "Incertae sedis")}
                  }
                }
              }
            else{new.name <- c(new.name, h)}
            }
          if(sjmisc::is_empty(Index$Vtaxa[Index$Vtype == h]) == F){
            new.name <- c(new.name, Index$Vtaxa[Index$Vtype == h])}
          }
        }
      
      if(Family.name == T){
        if("Familly" %in% names(Index)){names(Index)[match("Familly", names(Index))] <- "Family"}
        for(h in k){
          if(sjmisc::is_empty(Index$Family[Index$Vtype == h]) == T){
            
            # if(h %in% Index$Species){
            # print(h)
            # new.name <- c(new.name, Index$Species[Index$Vtype == h])
            # }
            # else{
              if(Error.display == T){print(paste("The following Vtype don't have family:", h, sep = " "))}
              # print(h)
              new.name <- c(new.name, "Incertae Sedis")
              # }
            }
          if(sjmisc::is_empty(Index$Family[Index$Vtype == h]) == F){
            new.name <- c(new.name, Index$Family[Index$Vtype == h])}}}
      
      if(Pollen.type == T){
        # print("Pollen types")
        
        if(PT == "ss" & "PT.ss" %in% names(Index)){names(Index)[match("PT.ss", names(Index))] <- "Ptype"}
        if(PT == "sl" & "PT.sl" %in% names(Index)){names(Index)[match("PT.sl", names(Index))] <- "Ptype"}
        
        for(h in k){
          new.name <- c(new.name, Index$Ptype[Index$Vtype == h])
          if(length(Index$Ptype[Index$Vtype == h]) == 0){
            print("Problem on this taxa in the Index !!!")
            print(h)
            new.name <- c(new.name, "Incertae Sedis")}
          }
      }
      
      if(Error.display == T){
        print("*** Old types ***")
        print(paste("*** Taille vecteur : ", length(row.names(MV[[j]]))))
        print(row.names(MV[[j]]))
        print("*** New names ***")
        print(paste("*** Taille vecteur : ", length(new.name)))
        print(new.name)
        print("*** Duplicated taxa in this list ***")
        Taxa.pb <- new.name[duplicated(new.name)]
        print(Index[which(Index$Vtaxa %in% Taxa.pb),c(1,5:7)])
        }
      
      row.names(MV[[j]]) <- new.name
      row.names(MV[[j]])[is.na(row.names(MV[[j]]))] <- "Incertae sedis"
      MV[[j]] <- aggregate.data.frame(MV[[j]], list(row.names(MV[[j]])), FUN = sum) # Fusion des taxons identiques
      row.names(MV[[j]]) <- MV[[j]][,1]
      MV[[j]] <- MV[[j]][-1]
      # print(names(MV)[j])
    }}
  else{for(j in 1:length(MV)){MV[[j]] <- as.data.frame(MV[[j]])}}
  # print("pouet")
  #### Return ####
  return(MV)
  
  }

# Fait la moyenne des quadrats pour chaque sites / releves
# On choisit le modele de distribution (D, D2, SP)
# On peut afficher le graphique des différents modeles %TV = f(D) (Displot = T)
Mean.veget.model <- function(MV, Model, Releve.design, Blind.com.merge, Digits = 5,
                             Csv.sep, Displot, Error.display, Remove.uncomplete){
  #### Settings ####
  if(missing(Model)){Model = "D"}
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Blind.com.merge)){Blind.com.merge = F}
  if(missing(Displot)){Displot = F}
  if(missing(Error.display)){Error.display = T}
  if(missing(Remove.uncomplete)){Remove.uncomplete = F}
  if(Remove.uncomplete == T){Seuil.remove = 95}
  if(Remove.uncomplete == F){Seuil.remove = 0}
  
  MV <- lapply(MV, function(x) x[,colSums(is.na(x))<nrow(x)]) # on enleve les colomns remplies de NaN
  ModelV.tot <- read.table(Releve.design, sep = Csv.sep, row.names = 1, check.names = F, header = T, na.strings = c("","NA"), stringsAsFactors = F)
  
  #### Subset models in Veget.model.csv ####
  # A terme il faudra faire un boucle sur les partie a val de ModelV$Design différentes
  #Mais là on va garder les val pour 10 m (ou 25 m le cas échéants)
  #ModelV <- ModelV[ModelV$Distance <= 25]
  if(!"Dist.pourc" %in% names(ModelV.tot)){
    ModelV.tot <- subset(ModelV.tot, ModelV.tot$Distance <= 250)
    ModelV.tot2 <- ModelV.tot[0]
    par(mfrow=c(1,length(unique(ModelV.tot$Design))))
    
    for(i in unique(ModelV.tot$Design)){
      ModelV <- subset(ModelV.tot, ModelV.tot$Design == i)
      
      #### Model calculation ####
      gamma <- 0.125
      B <- 0.75225277806368
      ModelV[["1/D"]]         = 1/ModelV$Distance
      ModelV[["Dist.pourc"]]  = ModelV[["1/D"]]/colSums(ModelV[3])
      ModelV[["1/D2"]]        = 1/(ModelV$Distance)^2
      ModelV[["Dist2.pourc"]] = ModelV[["1/D2"]]/colSums(ModelV[5])
      ModelV[["Sutton.Prentice"]] = B*gamma*(ModelV$Distance)^(gamma-1)*exp(-B*(ModelV$Distance)^(gamma))
      ModelV[["SP.pourc"]] = ModelV[["Sutton.Prentice"]]/colSums(ModelV[7])
      
      ModelV.tot2 <- rbind(ModelV.tot2, ModelV)
        
      #### Model plot ####
      if(Displot == T){
        ModelV.print <- subset(ModelV.tot2, ModelV.tot2$Design == i)
        plot(ModelV.print$Distance, ModelV.print$Dist2.pourc,
             xlab = "Distance to sample (m)",
             ylab = "Fractional influence of quadrat (%)",
             pch = i,
             type = "b", col = "royalblue")
        points(ModelV.print$Distance, ModelV.print$Dist.pourc, pch = i, type="b", col = "red")
        points(ModelV.print$Distance, ModelV.print$SP.pourc, pch = i, type="b", col = "orange")
        LabLeg <- c("Distance squarred", "Distance", "Sutton Prentice")
        ColLeg <- c("royalblue", "red", "orange")
        legend("topright",             
               legend = LabLeg,
               col = ColLeg,
               pch = i,
               y.intersp = 1,	                                       # espace entre y
               x.intersp = 1,                                      # espace entre x
               bty = "n" ) # enleve la boite autours de la legende
        }
    }}
  else{
    ModelV.tot2 <- ModelV.tot
    }
  
  #### Blind.com.merge ####
  if(Blind.com.merge == T){
    Com.BM <- function(x){
      Id.com <- grep("C", names(x))
      Id.com <- Id.com[-1]
      if(length(Id.com) > 1){
        Com.merged <- data.frame(rowMeans(x[,Id.com]))
        x <- x[,-Id.com]
        x <- cbind(x, Com.merged)
        }
      names(x)[ncol(x)] <- "Cmean"
      return(x)
    }
    # for(i in 1:length(MV)){MV[i] <- Com.BM(MV[i])}
    MV <- lapply(MV, function(x) Com.BM(x)) # ATTENTION problem sur MV parfois.
    }
  
  #### Checking match Releve.design & MV ####
  for(i in 1:length(MV)){
    Corres.check <- setdiff(colnames(MV[[i]]), row.names(ModelV.tot2))
    if(length(Corres.check)>0){
      if(Error.display == T){
        print(paste("In", names(MV)[i], "the distance information is missing, these quadrats are removed :", paste(Corres.check, collapse = ", "), sep = " "))
        }
      MV[[i]] <- MV[[i]][ , - which(colnames(MV[[i]]) %in% c(Corres.check))]
      }
    }
  
  #### Select model to apply ####
  if(Model == "A"){ # Average model, distance independent
    print("Independant to distance vegetation model")
    Choix.model <- which(names(ModelV.tot2) == "Indet.Dist")
    }
  
  if(Model == "D"){ # Distance dependent (1/D)
    print("Distance weighted vegetation model")
    Choix.model <- which(names(ModelV.tot2) == "Dist.pourc")
    }
      
  if(Model == "SQTD"){ # Distance dependent (1/D)
    print("1/sqrt(Distance) weighted vegetation model")
    Choix.model <- which(names(ModelV.tot2) == "Sqt.Dist.pourc")
    }
    
  if(Model == "D2"){ # 1/D2 model
    print("Distance-squarred weighted vegetation model")
    Choix.model <- which(names(ModelV.tot2) == "Dist2.pourc")
    }
  
  if(Model == "SP"){ # Sutton-Prentice model
    print("Sutton-Prentice vegetation model")
    Choix.model <- which(names(ModelV.tot2) == "SP.pourc")
    }
  
  #### Model application to matrix ####
  All.veget.type <- c()
  for(i in MV){All.veget.type <- c(All.veget.type, row.names(i))}
  All.veget.type <- unique(All.veget.type)
  FullMat <- data.frame(row.names = All.veget.type)
  
  for(i in 1:length(MV)){
    # print(names(MV[[i]]))
    Total.koef <- sum(ModelV.tot2[row.names(ModelV.tot2) %in% names(MV[[i]]), Choix.model])
    Pour.koef <- ModelV.tot2[row.names(ModelV.tot2) %in% names(MV[[i]]), Choix.model]/Total.koef
    for(j in colnames(MV[[i]])){
      Koef.cor <- Pour.koef[match(j,colnames(MV[[i]]))]
      MV[[i]][,colnames(MV[[i]])==j] = round(MV[[i]][,colnames(MV[[i]])==j]*Koef.cor, digits = 4)
      }
    MeanVal <- as.data.frame(rowSums(MV[[i]]))
    MeanVal <- MeanVal/sum(MeanVal)*100
    Test <- as.integer(sum(MeanVal))
    colnames(MeanVal) <- names(MV)[[i]]
    
    if(Test >= Seuil.remove){
      FullMat <- merge(FullMat, MeanVal, by = 0, all.x = T, all.y = T, sort = F)
      row.names(FullMat)<- FullMat$Row.names
      FullMat <- FullMat[-1]
      FullMat <- replace(FullMat, is.na(FullMat), 0)
      }
    
    if(Test != 100 & Error.display == T){print(paste("Erreur quadrat ", names(MV)[[i]],". La somme des taxons n'est pas egale a 100 mais a ", Test, ".", sep = ""))}
    }

  FullMat <- round(FullMat, digits = Digits)
  # print(FullMat)
  # write.table(MV, file = Releve.design, row.names=T, col.names=NA, sep=",", dec = ".")
  write.table(ModelV.tot2, file = Releve.design, row.names=T, col.names=NA, sep=",", dec = ".")
  return(FullMat)  
  }

# Trace le diagramme de vegetation
Hist.veget <- function(MV, Ordin.path, Index, Csv.sep, Sort, Max_seuil, AP.NAP, Save.plot, H, W){
  #### Settings ####
  library("rioja")
  UnSort = F
  if(missing(Sort)){UnSort = T}
  if(missing(AP.NAP)){AP.NAP = F}
  #if(missing(Index)){AP.NAP = F}
  if(missing(Index)){AP.NAP = F
    print("AP/NAP impossible : the index is missing.")}
  if(missing(Max_seuil)){Max_seuil = 3}
  if(missing(H)){H = 400}
  if(missing(W)){W = 800}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Ordin.path)){Ordin.path = NULL}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
 
  #### Fusion taxons rares #### 
  MV_max <- MV[0]
  MV_max[, "max"] <- apply(MV[,], 1, max)
  MV_dom <- MV[rowSums(MV_max)>Max_seuil,]
  MV_rare <- MV[rowSums(MV_max)<=Max_seuil,]
  MV_dom["Other" ,] <- colSums(MV_rare)
  MV_dom <- data.frame(t(MV_dom))
  
  #### Sort sites ####
  if(is.null(Ordin.path) == F){
    Ordin <- read.table(Ordin.path, sep = Csv.sep, check.names = F, row.names = 1, header = T, na.strings = c("","NA"), stringsAsFactors = F)
    MV.plot.data <- data.frame(t(MV))
    Inter <- intersect(row.names(Ordin), row.names(MV.plot.data))
    Ordin <- Ordin[which(row.names(Ordin) %in% Inter),]                                        # on enleve les taxons absents
    
    if(UnSort == T){Sort = "Ordination"
      Ordin <- Ordin[order(Ordin[[Sort]]),]}
    if(UnSort == F){Ordin <- Ordin[order(Ordin[[Sort]]),]}
    MV_dom <- MV_dom[match(row.names(Ordin),row.names(MV_dom)),]
    }
  else{
    Ordin <- data.frame(Plots = rep(1:nrow(MV)))
    Sort = "Plots"
    }
  
  #### Calcul AP et NAP  #### 
  if(AP.NAP == TRUE){
    #row.names(MV) <- gsub(" ",".", row.names(MV))
    #InfoPol$Nom <- gsub(" ",".", InfoPol$Nom)
    List.tree <- Index$Ptype[Index$Layer == "Tree"]
    
    #print(List.tree)
    #print(MV)
    AP <- colSums(MV[intersect(row.names(MV), List.tree),]) 
    List.herb <- Index$Ptype[Index$Layer == "Herb"]
    NAP <- colSums(MV[intersect(row.names(MV), List.herb),])
    print(AP)
    #AP <- colSums(MV[intersect(row.names(MV),InfoPol$Nom[InfoPol$AP.NAP == "AP"]),])
    #NAP <- colSums(MV[intersect(row.names(MV),InfoPol$Nom[InfoPol$AP.NAP == "NAP"]),])
    AP_nul <- sum(AP)
    print(MV[intersect(row.names(MV), List.tree),])
    if(AP_nul != 0){
      MV_dom <- rbind(AP=AP, NAP=NAP, t(MV_dom))
      MV_dom <- t(MV_dom)
      }
    
    print(MV_dom)
    #row.names(MV) <- gsub("\\."," ", row.names(MV))
    #InfoPol$Nom <- gsub("\\."," ", InfoPol$Nom)
  }  
  
  #### Graphic settings ####
  if(Sort == "Ordination"){Taille.bar = 20}
  else{Taille.bar = 10}
  #### Plot ####
  strat.plot(MV_dom,
             Ordin[[Sort]],
             y.rev = TRUE, 
             scale.percent = TRUE,     # True pour des pourcentages
             scale.minmax = TRUE,
             srt.xlabel = 45,           # Rotation de 45 des noms de taxon
             #title = paste("Pollen diagram of", nlake, "Lake"),
             ylabel = Sort,
             #y.tks = seq(-50,round(max(Age),digits=-2),200),
             #x.tks = seq(0,round(max(spec),digits=-2),round(max(spec),digits=-2)/4),
             plot.poly     = FALSE, 
             plot.bar      = TRUE,
             plot.line     = FALSE, 
             lwd.bar       = Taille.bar, 
             col.poly      = "#a2dbe1ff",         # bleu clair
             #col.bar       = couleur,            # Fait apparaitre les traits continus
             col.poly.line = "#8c92a1ff",         # gris fonce
             exag=FALSE,                # Fait apparaitre la zone x10
             col.exag="auto"#,           # Zone x10 couleur auto
             #clust = Clust_DP,
             #clust.width=0.075 
             #fun1=sm.fun                # application ou non du lissage
  )
  if(is.null(Save.plot) == F){dev.off()}
  return(MV_dom)
}

# PCA sur la veget
# CLUSTER NE MARCHE PAS (copier la PCA.pollen qui marche mieux)
PCA.veget <- function(MV, transp_OK, Site.name, Manu.lim, Leg.pos, Correspondence_Analysis,
                      Sort.eco, Cluster, Cluster.path, Csv.sep, Scale.PCA){
  #### Settings ####
  library(vegan)
  Clustering = T
  if(missing(Sort.eco)){Sort.eco = NULL}
  if(missing(Cluster.path)){Clustering = F}
  if(missing(Correspondence_Analysis)){Correspondence_Analysis = F}
  if(missing(Cluster)){Clustering = F}
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Scale.PCA)){Scale.PCA = 1}
  if(missing(Manu.lim)){Manu.lim = NULL}
  if(missing(Leg.pos)){Leg.pos = "bottomleft"}
  
  row.names(MV) <- gsub("aceae",".",row.names(MV))
  MV <- data.frame(t(MV))
  names(MV) <- gsub("type", "", names(MV))
  colnames(MV) <- gsub("sp\\.", "", colnames(MV))
  colnames(MV) <- gsub("\\.", " ", colnames(MV))
  
  #### Transforming the data ####
  if(transp_OK == FALSE){
    if(Correspondence_Analysis == T){MV.pca <- cca(MV, scale = T)}
    else{MV.pca <- rda(MV, scale = T)}
    MTitle = paste("PCA Vegetation -", Site.name)
    }
  else{
    if(Correspondence_Analysis == T){
      MV <- log1p(MV)
      MV.pca <- cca(MV, scale = TRUE)
      }
    else{MV.pca <- rda(log1p(MV), scale = TRUE)}
    MTitle = paste("PCA Vegetation, log-trans, ", Site.name)
    }
  
  #### Graphic parameters ####
  if(Correspondence_Analysis == F){
    site.score  <- scores(MV.pca, choices=c(1,2), display = "wa", scaling = 2)         # valeur PCA age
    taxa.score <- scores(MV.pca, choices=c(1,2), display = "species", scaling = 2)
    PC1.MV <- MV.pca[["CA"]][["eig"]][["PC1"]]/MV.pca[["tot.chi"]]*100              # calcul PC1
    PC2.MV <- MV.pca[["CA"]][["eig"]][["PC2"]]/MV.pca[["tot.chi"]]*100              # calcul PC2
    }
  else{
    site.score  <- MV.pca$CA$v[,c(1:2)]
    taxa.score <- MV.pca$CA$u[,c(1:2)]
    PC1.MV <- MV.pca[["CA"]][["eig"]][["CA1"]]/MV.pca[["tot.chi"]]*100              # calcul PC1
    PC2.MV <- MV.pca[["CA"]][["eig"]][["CA2"]]/MV.pca[["tot.chi"]]*100              # calcul PC2
    }
  
  par(mar=c(5,4,4,2)+0.1)#,xpd=TRUE)
  xmin <- min(c(min(site.score[,1]),min(taxa.score[,1])))                             # lim axe PC1
  xmax <- max(c(max(site.score[,1]),max(taxa.score[,1])))
  ymin <- min(c(min(site.score[,2]),min(taxa.score[,2])))                             # lim axe PC2
  ymax <- max(c(max(site.score[,2]),max(taxa.score[,2])))
  
  if(is.null(Manu.lim) == F){
    xmin <- Manu.lim[1]
    xmax <- Manu.lim[2]
    ymin <- Manu.lim[3]
    ymax <- Manu.lim[4]
    }
  
  #### PCA ####
  if(Correspondence_Analysis == F){
    #### Vector plot ####
    biplot(MV.pca,                                              # ajout des vecteurs taxa
           scaling = Scale.PCA,
           display = "species",                                  # 'species', vecteurs taxa
           type = "t",                                           # 't', txt aux vecteurs, 'n', vecteurs vides
           cex = .1,
           main = MTitle,
           sub = paste("n = ", nrow(MV)),
           xlim = c(xmin,xmax),
           ylim = c(ymin,xmax),
           xlab = paste("PC1 (",format(PC1.MV, digits=4),"% )"),
           ylab = paste("PC2 (",format(PC2.MV, digits=4),"% )")
           )
    
    
    #### Add plots ####
    Tsite <- gsub("MMNT","",row.names(MV))
    text(site.score, Tsite, cex=.5, pos= 3)                # textes sites
    
    if(Clustering == T){
      # colfunc    <- colorRampPalette(c("red","yellow", "green","royalblue"))
      # colfunc    <- colorRampPalette(c("royalblue","#d67700ff", "#f1e55cff","darkgreen", "#96e580ff"))
      colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
      # colfunc    <- colorRampPalette(c("white", "red","yellow2", "darkorange", "green","royalblue", "black"))
      Cluster.extract <- data.frame(read.csv(Cluster.path, sep = Csv.sep ,dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
      Inter <- intersect(row.names(Cluster.extract), row.names(site.score))
      Cluster.extract <- Cluster.extract[which(row.names(Cluster.extract) %in% Inter),]
      M.eco <- subset(Cluster.extract, select = c(Cluster))
      M.eco <- data.frame(M.eco[][row.names(site.score),])
      row.names(M.eco) <- row.names(site.score)
      num_colors <- nlevels(as.factor(M.eco[[1]]))
      diamond_color_colors <- colfunc(num_colors)
      Col.eco <- diamond_color_colors[as.factor(M.eco[[1]])]
    }
    else{Col.eco = 1}
    
    points(site.score,                                            # ajouts des points AGE
           pch = 19, cex = 1.3,
           col = Col.eco
           )
    
    #### Legend #####
    if(Clustering == T){
      legend(Leg.pos,             
             legend = levels(as.factor(M.eco[[1]])),
             col = diamond_color_colors,
             pch = 19,
             y.intersp = 0.7,	                                     # espace entre y
             x.intersp = 0.4,                                      # espace entre x
             bty = "n" ) 
      }
  }
  
  #### CA ####
  if(Correspondence_Analysis == T){
    
    library(FactoMineR)
    library(factoextra)
    res.ca <- CA(scale(MV, center = F), graph = FALSE)
    p <- fviz_ca_biplot(res.ca, alpha.geom="contrib",
                        repel = F, select.row = list(contrib = 7),
                        arrows = c(T,F), 
                        )
    print(p)
    # library(FactoMineR)
    # MV.pca <- CA(MV, graph = FALSE)
    # plot(MV.pca, autoLab = "yes")
    }
    
    
  #### Export data ####
  write.table(t(site.score), file="Resultats/PCA_Veget_Arkhangai.csv", col.names = FALSE, sep = "\t")
  write.table(t(taxa.score), file="Resultats/PCA_Veget_Arkhangai_taxa.csv", col.names = TRUE, sep = "\t")
  return(site.score)
}

PCA.vegetation <- function(MP, transp_OK, Site.name, Type.samples, Short.name, GDGT, Annot, Nb.contrib = NULL, Leg.size = 1,
                            Remove.7Me, Display.legends, Simple.title, Show.text, Symbol.loc, Symbol.path, Vector.show = NULL,
                            Cluster.path, Cluster.groups, Sort.eco, Csv.sep, Scale.PCA, Color.choice = NULL,
                            cluster.from.PCA, Leg.loc,Leg.loc2, Save.path, Manu.lim, Title.inside = F){
  #### Settings ####
  library(vegan)
  Clustering = T
  if(missing(cluster.from.PCA)){cluster.from.PCA = F}
  if(missing(Show.text)){Show.text = F}
  if(missing(Cluster.path)){Clustering = F}
  if(missing(Short.name)){Short.name = T}
  if(missing(Remove.7Me)){Remove.7Me = F}
  if(missing(Display.legends)){Display.legends = T}
  if(missing(Simple.title)){Simple.title = F}
  if(missing(Cluster.groups)){Cluster.groups = "Ecosystems"}
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Scale.PCA)){Scale.PCA = 1}
  if(missing(Annot)){Annot = NULL}
  if(missing(Symbol.path)){Symbol.path = NULL}
  if(missing(Symbol.loc)){Symbol.loc = NULL}
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Sort.eco)){Sort.eco = NULL}
  if(missing(Manu.lim)){Manu.lim = NULL}
  if(missing(Type.samples)){Type.samples = NULL}
  if(missing(Leg.loc)){Leg.loc = "bottomleft"}
  if(missing(Leg.loc2)){Leg.loc2 = "bottomright"}
  if(missing(Site.name)){Site.name = ""}
  if(missing(GDGT)){GDGT = NULL}
  
  #### Pour les GDGTs ####
  if(is.null(GDGT) == F){
    if(Remove.7Me == T){MP <- MP[setdiff(row.names(MP), row.names(MP)[grepl("7Me", row.names(MP))]),]}
    row.names(MP) <- gsub("f.", "", row.names(MP))
    row.names(MP) <- gsub("_5Me", "", row.names(MP))
    row.names(MP) <- gsub("_6Me", "\\'", row.names(MP))
    row.names(MP) <- gsub("_7Me", "\\''", row.names(MP))
  }
  
  #### Pour le pollen ####
  if(is.null(GDGT) == T){
    row.names(MP) <- gsub("aceae",".",row.names(MP))
    row.names(MP) <- gsub(".undiff","",row.names(MP))
    row.names(MP) <- gsub(".indet","",row.names(MP))
    row.names(MP) <- gsub("Po.","Poa.",row.names(MP))
    row.names(MP) <- gsub(".spp.","",row.names(MP))
    row.names(MP) <- gsub(".type","t",row.names(MP))
  }
  
  MP <- data.frame(t(MP), check.names = F)
  
  #### Transforming the data ####
  if(transp_OK == FALSE){MP.pca <- rda(MP, scale = F)
  MTitle = paste("PCA", Type.samples, "-", Site.name)}
  # else{MP.pca <- rda(log1p(MP), scale = T)
  else{MP.pca <- rda(MP, scale = T)
  MTitle = paste("PCA", Type.samples, "log-trans, ", Site.name)}
  if(Simple.title == T){MTitle = paste("PCA", Type.samples)}
  if(is.null(Annot) == F){MTitle <- paste(Annot, MTitle)}
  if(Title.inside == T){MTitle <- NULL}
  
  #### Test clustering from PCA ####
  if(cluster.from.PCA == T){
    # print(names(MP.pca))
    # View(MP.pca)
    library(FactoMineR)
    library(factoextra)
    # Compute PCA with ncp = 3
    res.pca <- PCA(MP, ncp = 5, graph = FALSE, scale.unit = F)
    # Compute hierarchical clustering on principal components
    res.hcpc <- HCPC(res.pca, graph = FALSE)
    # fit <- kmeans(pf$ind$coord[,1:4],2)
    print(res.hcpc$data.clust)
    
    pdend <- fviz_dend(res.hcpc, 
                       cex = 0.7,                     # Label size
                       palette = "jco",               # Color palette see ?ggpubr::ggpar
                       rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
                       rect_border = "jco",           # Rectangle color
                       labels_track_height = 0.8      # Augment the room for labels
    )
    
    pclu <- fviz_cluster(res.hcpc,
                         repel = TRUE,            # Avoid label overlapping
                         show.clust.cent = TRUE, # Show cluster centers
                         palette = "jco",         # Color palette see ?ggpubr::ggpar
                         ggtheme = theme_minimal(),
                         main = "Factor map"
    )
    print(pdend)
    print(pclu)}
  
  #### Graphic parameters ####
  site.score  <- scores(MP.pca, choices=c(1,2), display = "wa", scaling = 2)         # valeur PCA age
  taxa.score <- scores(MP.pca, choices=c(1,2), display = "species", scaling = Scale.PCA)
  
  par(mgp = c(1.7,0.6,0), mar=c(3,3,2,0.3)+0.1)
  
  PC1.MP <- MP.pca[["CA"]][["eig"]][["PC1"]]/MP.pca[["tot.chi"]]*100
  PC2.MP <- MP.pca[["CA"]][["eig"]][["PC2"]]/MP.pca[["tot.chi"]]*100
  xmin <- min(c(min(site.score[,1]),2*min(taxa.score[,1])))                             # lim axe PC1
  xmax <- max(c(max(site.score[,1]),2*max(taxa.score[,1])))
  ymin <- min(c(min(site.score[,2]),2*min(taxa.score[,2])))                             # lim axe PC2
  ymax <- max(c(max(site.score[,2]),2*max(taxa.score[,2])))
  xmin <- xmin + .08*xmin
  xmax <- xmax + .08*xmin
  ymin <- ymin + .08*ymin
  ymax <- ymax + .08*ymax
  
  if(is.null(Manu.lim) == F){
    xmin <- Manu.lim[1]
    xmax <- Manu.lim[2]
    ymin <- Manu.lim[3]
    ymax <- Manu.lim[4]
  }
  
  #### Contribution ####
  if(is.null(Vector.show) == F & is.null(Nb.contrib) == T){
    taxa.score <- taxa.score[row.names(taxa.score) %in% Vector.show,]
    MP.pca$CA$v <- MP.pca$CA$v[row.names(MP.pca$CA$v) %in% Vector.show,]
    }
  
  if(is.null(Nb.contrib) == F){
    Contribution <- scores(MP.pca, display = "sp", scaling = 0)[,1]^2*100
    Contribution <- Contribution[order(Contribution, decreasing = T)]
    Contribution <- names(Contribution[1:Nb.contrib])
    if(is.null(Vector.show) == F){
      Contribution <- unique(c(Contribution, Vector.show))
      print(paste("**** We will merge the", Nb.contrib, "most contributing taxa + the", length(Vector.show), "manually selected taxa.****" ))}
    
    taxa.score <- taxa.score[row.names(taxa.score) %in% Contribution,]
    MP.pca$CA$v <- MP.pca$CA$v[row.names(MP.pca$CA$v) %in% Contribution,]
    }
  
  #### Empty biplot ####
  biplot(MP.pca,                                              # ajout des vecteurs taxa
         # scaling = Scale.PCA,
         display = "species",                                  # 'species', vecteurs taxa
         type = "n",                                           # 't', txt aux vecteurs, 'n', vecteurs vides
         cex  = c(0.5, 0.8), 
         main = MTitle, #col = My.col,
         #sub = paste("n = ", nrow(MP)),
         xlim = c(xmin,xmax),
         ylim = c(ymin,xmax), 
         xlab = bquote(PCA[1] ~ "(" ~ .(format(PC1.MP, digits = 2)) ~ "%)"),
         ylab = bquote(PCA[2] ~ "(" ~ .(format(PC2.MP, digits = 2)) ~ "% )")
  )
  
  #### Add Arrows ####
  # text(MP.pca, scaling = Scale.PCA*1.5, display = "species", cex = .95, col = "#6e2115ff")
  text(MP.pca, scaling = Scale.PCA*1.5, display = "species", cex = .95, col = "#6e2115ff")
  # text(MP.pca, display = "species", cex = .95, col = "#6e2115ff")
  arrows(0,0, x1 = taxa.score[,1]*Scale.PCA, y1 = taxa.score[,2]*Scale.PCA, length = 0.05, lty = 1, col="#6e2115ff")
  # arrows(0,0, x1 = taxa.score[,1], y1 = taxa.score[,2], length = 0.05, lty = 1, col="#6e2115ff")
  
  if(Title.inside == T){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.01, 0.95, Annot, adj = 0, cex = 1.7)  # if that's what you want
    par(usr = usr) # restore original user coordinates
  }
  #### Add text ####
  if(Short.name == T){Tsite <- sub("M","",row.names(MP))}
  else{Tsite <- row.names(MP)}
  if(Show.text == T){text(site.score, Tsite, cex=.6, pos= 3)}  # textes sites

  #### Add Points ####
  if(Clustering == T){
    if(is.character(Cluster.path) == T){Cluster <- data.frame(read.csv(Cluster.path, sep = Csv.sep ,dec=".",header=T,row.names=1), stringsAsFactors = T)}
    if(is.data.frame(Cluster.path) == T){Cluster <- Cluster.path}
    Inter <- intersect(row.names(Cluster), row.names(site.score))
    Cluster <- Cluster[which(row.names(Cluster) %in% Inter),]
    M.eco <- subset(Cluster, select = Cluster.groups)
    M.eco <- data.frame(M.eco[][row.names(site.score),])
    row.names(M.eco) <- row.names(site.score)
    Fact.eco <- as.factor(M.eco[[1]])
    if(is.null(Sort.eco) == F){Fact.eco <- ordered(Fact.eco, levels = Sort.eco)}
    num_colors <- nlevels(Fact.eco)

    if(is.null(Color.choice) == F){diamond_color_colors <- Color.choice}
    if(is.null(Color.choice) == T){
      colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
      diamond_color_colors <- colfunc(num_colors)
    }
    Col.eco <- diamond_color_colors[Fact.eco]
    }
  else{Col.eco = "grey"}
  points(site.score, pch = 19, cex = 1.5, col = Col.eco)

  #### Legend #####
  if(Clustering == T & Display.legends == T){
    legend(Leg.loc,
           legend = levels(Fact.eco),
           col = diamond_color_colors, cex = Leg.size, pt.cex = Leg.size,
           pch = 19,
           y.intersp = 0.8,	                                     # espace entre y
           x.intersp = 0.8,                                      # espace entre x
           bty = "n" )
  }

  #### Legend 2 ####
  legend(Leg.loc2,
         legend = paste("n = ", nrow(MP)),
         pch = NA,
         y.intersp = 0.7,	                                     # espace entre y
         x.intersp = 0.4,                                      # espace entre x
         bty = "n" )

  #### Add symbol ####
  if(is.null(Symbol.loc) == F & is.null(Symbol.path) == F){
    library(png)
    # library(magick)
    # library(rsvg)
    pic <- readPNG(Symbol.path)
    # pic <- rsvg(Symbol.path)
    # print(pic)
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    rasterImage(pic,0.83,0.83,0.99,0.99)
    par(usr = usr) # restore original user coordinates
  }
  
  #### Export data ####
  if(is.null(Save.path) == F){
    Site.name <- gsub(" ","_",Site.name)
    Save.path.Site <- gsub("\\.csv", "_PCA_site.csv", Save.path)
    Save.path.Taxon <- gsub("\\.csv", "_PCA_taxa.csv", Save.path)
    Save.path.Cluster <- gsub("\\.csv", "_PCA_HCPC.csv", Save.path)
    
    write.table(t(site.score), file = Save.path.Site, col.names = FALSE, sep = ",")
    write.table(t(taxa.score), file = Save.path.Taxon, col.names = TRUE, sep = ",")
    
    if(cluster.from.PCA == T){
      write.table(res.hcpc$data.clust, file = Save.path.Cluster, col.names = FALSE, sep = ",")
    }
  }
  
  return(site.score)
}


# RDA sur la veget avec DB abiotique
# Soit Climat, soil...
# Param necessaire : MV, MClim 
# Param option : transp_OK, Site.name, Csv.sep, Scale.PCA
RDA.veget <- function(MV, MClim, Choose.clim, Cluster, Sort.eco,
                      Leg.pos, transp_OK, Site.name, Csv.sep, Scale.RDA){
  #### Settings ####
  library(vegan)
  Clustering = T
  if(missing(Sort.eco)){Sort.eco = NULL}
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Cluster)){Clustering = F}
  if(missing(Choose.clim)){print("Select the climat variable to perform the RDA.")}
  if(missing(Scale.RDA)){Scale.RDA = 1}
  if(missing(Leg.pos)){Leg.pos = "bottomleft"}
  row.names(MV) <- gsub("aceae",".",row.names(MV))
  MV <- data.frame(t(MV))
  
  #### Import Climat + verif match DB ####
  Clim <- data.frame(read.csv(MClim, sep = Csv.sep ,dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
  C <- colnames(Clim)
  Inter <- intersect(row.names(Clim), row.names(MV))
  Clim <- Clim[which(row.names(Clim) %in% Inter),]
  MV <- MV[which(row.names(MV) %in% Inter),]
  Clim <- data.frame(Clim[][row.names(MV),])
  row.names(Clim) <- row.names(MV)
  colnames(Clim) <- C
  Clim.rda <- subset(Clim, select = Choose.clim)
  
  #### Transforming the data ####
  if(transp_OK == FALSE){MV.pca <- rda(MV, Clim.rda, scale = TRUE)
  MTitle = paste("RDA Veget/Clim -", Site.name)}
  else{MV.pca <- rda(log1p(MV), Clim.rda, scale = TRUE)
  MTitle = paste("RDA Veget/Clim, log-trans, ", Site.name)}
  # print(summary(MV.pca))
  
  #### Calcul parameters ####
  C_PC.rda <- coef(MV.pca)    # récupère les coefficients canoniques (equivalents R en multivar (donc il faut faire au 2 pour avoir R2)) pour chaque variable clim
  PClim.sc <- scores(MV.pca, choices=1:2, scaling = Scale.RDA, display = "sp")
  PClim.sc.site <- scores(MV.pca, choices=1:2, scaling = Scale.RDA, display = "sites")
  clim.score <- MV.pca[["CCA"]][["biplot"]]
  R2 <- RsquareAdj(MV.pca)$r.squared  
  Tot.eig <- sum(MV.pca[["CCA"]]$eig) + sum(MV.pca[["CA"]]$eig)
  RDA1 <- MV.pca[["CCA"]]$eig[1]/Tot.eig*100
  RDA2 <- MV.pca[["CCA"]]$eig[2]/Tot.eig*100

  par(mar=c(5,4,4,2)+0.1)#,xpd=TRUE)
 
  #### Vector plot ####
  
  plot(MV.pca, 
       type = "n",
       scaling = Scale.RDA, 
       main = MTitle,
       sub = paste("R2 =", format(R2, digits = 2), ", n = ", nrow(MV)),
       xlab = paste("RDA1 (",format(RDA1, digits=4),"% )"),
       ylab = paste("RDA2 (",format(RDA2, digits=4),"% )")
       )
  

  #### Add plots ####
  if(Clustering == T){
    M.eco <- subset(Clim, select = Cluster)
    # colfunc    <- colorRampPalette(c("white", "red","yellow2", "darkorange", "green","royalblue", "black"))
    # colfunc    <- colorRampPalette(c("red","yellow", "green","royalblue"))
    # colfunc    <- colorRampPalette(c("royalblue","#d67700ff", "#f1e55cff","darkgreen", "#96e580ff"))
    # colfunc    <- colorRampPalette(c("red","yellow", "green","royalblue"))
    colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
    
    num_colors <- nlevels(as.factor(M.eco[[1]]))
    diamond_color_colors <- colfunc(num_colors)
    Col.eco <- diamond_color_colors[as.factor(M.eco[[1]])]
    
    # if(is.character(Cluster.path) == T){Cluster <- data.frame(read.csv(Cluster.path, sep = Csv.sep ,dec=".",header=T,row.names=1), stringsAsFactors = T)}
    # if(is.data.frame(Cluster.path) == T){Cluster <- Cluster.path}
    
    # Inter <- intersect(row.names(Cluster), row.names(site.score))
    # Cluster <- Cluster[which(row.names(Cluster) %in% Inter),]
    # M.eco <- subset(Cluster, select = Cluster.groups)
    # M.eco <- data.frame(M.eco[][row.names(site.score),])
    # row.names(M.eco) <- row.names(site.score)
    # Fact.eco <- as.factor(M.eco[[1]])
    # if(is.null(Sort.eco) == F){Fact.eco <- ordered(Fact.eco, levels = Sort.eco)}
    # num_colors <- nlevels(Fact.eco)
    # diamond_color_colors <- colfunc(num_colors)
    # Col.eco <- diamond_color_colors[Fact.eco]
    }
  else{Col.eco = 1}
  colnames(MV) <- gsub("type", "", colnames(MV))
  colnames(MV) <- gsub("sp\\.", "", colnames(MV))
  colnames(MV) <- gsub("\\.", " ", colnames(MV))
  
  points(PClim.sc.site, pch = 19, cex = 1.3, col = Col.eco)
  arrows(0,0,x1 = PClim.sc[,1], y1 = PClim.sc[,2], length = 0.05, lty = 1, col="#d96a07ff")
  arrows(0,0,x1 = clim.score[,1], y1 = clim.score[,2], length = 0.08, lty = 1, lwd = 2, col="royalblue")
  # text(PClim.sc.site, gsub("MMNT6","", row.names(MV)), cex=.4, pos = 3)            # textes sites
  text(PClim.sc, colnames(MV), cex=.7, pos = 3, col = "#d96a07ff")                # textes taxa
  text(clim.score, row.names(clim.score), cex=.9, pos = 3, col = "royalblue")      # textes clim
  
  #### Legend #####
  if(Clustering == T){
    legend(Leg.pos,             
           legend = levels(as.factor(M.eco[[1]])),
           col = diamond_color_colors,
           pch = 19,
           y.intersp = 0.7,	                                     # espace entre y
           x.intersp = 0.4,                                      # espace entre x
           bty = "n" ) 
  }
  
  #### Export data ####
  write.table(t(PClim.sc.site), file="Resultats/RDA_Veget_Arkhangai.csv", col.names = FALSE, sep = "\t")
  write.table(t(PClim.sc), file="Resultats/RDA_Veget_Arkhangai_taxa.csv", col.names = TRUE, sep = "\t")
  return(MV.pca)
}

RDA.vegetation <- function(MP, MClim, Choose.clim, Cluster.path, Type.samples, Sort.eco, Color.choice = NULL,
                            Cluster.groups, Display.legends, transp_OK, Manu.lim, GDGT, Annot, Title.inside = F,
                            Remove.7Me, Leg.loc, Leg.loc2, Simple.title, Show.text, Symbol.loc, Symbol.path,
                            Site.name, Csv.sep, Scale.taxa, Scale.sites, Save.path, Vector.show = NULL, Nb.contrib = NULL, Leg.size = 1){
  #### Settings ####
  library(vegan)
  Clustering = T
  if(missing(Type.samples)){Type.samples = NULL}
  if(missing(Csv.sep)){Csv.sep = "\t"}
  if(missing(Annot)){Annot = NULL}
  if(missing(Sort.eco)){Sort.eco = NULL}
  if(missing(Show.text)){Show.text = F}
  if(missing(Cluster.path)){Clustering = F}
  if(missing(Remove.7Me)){Remove.7Me = F}
  if(missing(Simple.title)){Simple.title = F}
  if(missing(Cluster.groups)){Cluster.groups = "Ecosystems"}
  if(missing(Display.legends)){Display.legends = T}
  if(missing(Choose.clim)){print("Select the climat variable to perform the RDA.")}
  if(missing(Scale.taxa)){Scale.taxa = 1}
  if(missing(Scale.sites)){Scale.sites = 2}
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Manu.lim)){Manu.lim = NULL}
  if(missing(GDGT)){GDGT = NULL}
  if(missing(Site.name)){Site.name = ""}
  if(missing(Leg.loc)){Leg.loc = "bottomleft"}
  if(missing(Leg.loc2)){Leg.loc2 = "bottomright"}
  if(missing(Symbol.path)){Symbol.path = NULL}
  if(missing(Symbol.loc)){Symbol.loc = NULL}
  
  #### Pour les GDGTs ####
  if(is.null(GDGT) == F){
    if(Remove.7Me == T){MP <- MP[setdiff(row.names(MP), row.names(MP)[grepl("7Me", row.names(MP))]),]}
    row.names(MP) <- gsub("f.", "", row.names(MP))
    row.names(MP) <- gsub("_5Me", "", row.names(MP))
    row.names(MP) <- gsub("_6Me", "\\'", row.names(MP))
    row.names(MP) <- gsub("_7Me", "\\''", row.names(MP))
  }
  
  #### Pour le pollen ####
  if(is.null(GDGT) == T){
    row.names(MP) <- gsub("aceae",".",row.names(MP))
    row.names(MP) <- gsub(".undiff","",row.names(MP))
    row.names(MP) <- gsub(".indet","",row.names(MP))
    row.names(MP) <- gsub("Po.","Poa.",row.names(MP))
    row.names(MP) <- gsub(".spp.","",row.names(MP))
    row.names(MP) <- gsub(".type","",row.names(MP))
  }
  
  MP <- data.frame(t(MP), check.names = F)
  
  #### Import Climat + verif match DB ####
  if(is.character(MClim) == T){Clim <- data.frame(read.csv(MClim, sep = Csv.sep, dec = ".", header=T, row.names=1), stringsAsFactors = T)}
  if(is.data.frame(MClim) == T){Clim <- MClim}
  C <- colnames(Clim)
  Inter <- intersect(row.names(Clim), row.names(MP))
  # print(Inter)
  Clim <- Clim[which(row.names(Clim) %in% Inter),]
  MP <- MP[which(row.names(MP) %in% Inter),]
  Clim <- data.frame(Clim[][row.names(MP),])
  row.names(Clim) <- row.names(MP)
  colnames(Clim) <- C
  Clim.rda <- subset(Clim, select = Choose.clim)
  # print(Clim.rda)
  #### Transforming the data ####
  # print(transp_OK)
  if(transp_OK == F){MP.pca <- rda(MP, Clim.rda, scale = T)
  MTitle = paste("RDA", Type.samples, "/Climate -", Site.name)}
  else{MP.pca <- rda(log1p(MP), Clim.rda, scale = T)
  MTitle = paste("RDA", Type.samples, "/Climate - log-trans, ", Site.name)}
  if(Simple.title == T){MTitle = paste("RDA", Type.samples, "/Climate")}
  # print(MP.pca)
  if(is.null(Annot) == F){MTitle <- paste(Annot, MTitle)}
  if(Title.inside == T){MTitle <- NULL}
  # par(mar = c(1, 1, 1, 0) + 0.1)
  
  # return(MP.pca)
  #### Calcul parameters ####
  C_PC.rda <- coef(MP.pca)    # récupère les coefficients canoniques (equivalents R en multivar (donc il faut faire au 2 pour avoir R2)) pour chaque variable clim
  PClim.sc <- scores(MP.pca, choices=1:2, scaling = Scale.taxa, display = "sp")
  PClim.sc.site <- scores(MP.pca, choices=1:2, scaling = Scale.sites, display = "sites")
  clim.score <- MP.pca[["CCA"]][["biplot"]]
  R2 <- RsquareAdj(MP.pca)$r.squared
  Tot.eig <- sum(MP.pca[["CCA"]]$eig) + sum(MP.pca[["CA"]]$eig)
  RDA1 <- MP.pca[["CCA"]]$eig[1]/Tot.eig*100
  RDA2 <- MP.pca[["CCA"]]$eig[2]/Tot.eig*100
  
  #### Graphical parameters ####
  xmin <- min(c(min(PClim.sc.site[,1]),2*min(PClim.sc[,1])))                             # lim axe PC1
  xmax <- max(c(max(PClim.sc.site[,1]),2*max(PClim.sc[,1])))
  ymin <- min(c(min(PClim.sc.site[,2]),2*min(PClim.sc[,2])))                             # lim axe PC2
  ymax <- max(c(max(PClim.sc.site[,2]),2*max(PClim.sc[,2])))
  
  xmin <- xmin + .08*xmin
  xmax <- xmax + .08*xmin
  ymin <- ymin + .08*ymin
  ymax <- ymax + .08*ymax
  
  if(is.null(Manu.lim) == F){
    xmin <- Manu.lim[1]
    xmax <- Manu.lim[2]
    ymin <- Manu.lim[3]
    ymax <- Manu.lim[4]
  }
  #par(mar=c(5,4,4,2)+0.1)#,xpd=TRUE)
  par(mgp = c(1.7,0.6,0), mar=c(3,3,2,0.3)+0.1)
  
  #### Contribution ####
  if(is.null(Vector.show) == F & is.null(Nb.contrib) == T){
    PClim.sc <- PClim.sc[row.names(PClim.sc) %in% Vector.show,]
    MP.pca$CCA$v <- MP.pca$CCA$v[row.names(MP.pca$CCA$v) %in% Vector.show,]
    MP.pca$CA$v <- MP.pca$CA$v[row.names(MP.pca$CA$v) %in% Vector.show,]
    MP <- MP[colnames(MP) %in% Vector.show]}
  
  if(is.null(Nb.contrib) == F){
    Contribution <- scores(MP.pca, display = "sp", scaling = 0)[,1]^2*100
    Contribution <- Contribution[order(Contribution, decreasing = T)]
    Contribution <- names(Contribution[1:Nb.contrib])
    if(is.null(Vector.show) == F){
      Contribution <- unique(c(Contribution, Vector.show))
      print(paste("**** We will merge the", Nb.contrib, "most contributing taxa + the", length(Vector.show), "manually selected taxa.****" ))}
    
    PClim.sc <- PClim.sc[row.names(PClim.sc) %in% Contribution,]
    MP.pca$CCA$v <- MP.pca$CCA$v[row.names(MP.pca$CCA$v) %in% Contribution,]
    MP.pca$CA$v <- MP.pca$CA$v[row.names(MP.pca$CA$v) %in% Contribution,]
    MP <- MP[colnames(MP) %in% Contribution]}
  
  #### Vector plot ####
  plot(MP.pca,
       type = "n",
       scaling = Scale.sites,
       main = MTitle,
       xlim = c(xmin,xmax),
       ylim = c(ymin,xmax),
       xlab = bquote(RDA[1] ~ "(" ~ .(format(RDA1, digits = 2)) ~ "%)"),
       ylab = bquote(RDA[2] ~ "(" ~ .(format(RDA2, digits = 2)) ~ "% )")
  )
  
  
  
  #### Clustering colors ####
  if(Clustering == T){
    if(is.character(Cluster.path) == T){Cluster <- data.frame(read.csv(Cluster.path, sep = Csv.sep ,dec=".",header=T,row.names=1), stringsAsFactors = T)}
    if(is.data.frame(Cluster.path) == T){Cluster <- Cluster.path}
    Inter <- intersect(row.names(Cluster), row.names(PClim.sc.site))
    Cluster <- Cluster[which(row.names(Cluster) %in% Inter),]
    M.eco <- subset(Cluster, select = Cluster.groups)
    M.eco <- data.frame(M.eco[][row.names(PClim.sc.site),])
    row.names(M.eco) <- row.names(PClim.sc.site)
    Fact.eco <- as.factor(M.eco[[1]])
    if(is.null(Sort.eco) == F){Fact.eco <- ordered(Fact.eco, levels = Sort.eco)}
    num_colors <- nlevels(Fact.eco)
    if(is.null(Color.choice) == F){diamond_color_colors <- Color.choice}
    else{
      colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
      diamond_color_colors <- colfunc(num_colors)
    }
    Col.eco <- diamond_color_colors[Fact.eco]
  }
  else{Col.eco = 1}
  
  #### Add plots ####
  arrows(0,0,x1 = PClim.sc[,1], y1 = PClim.sc[,2], length = 0.07, lty = 1, lwd = 1.5, col="#6e2115ff")
  arrows(0,0,x1 = clim.score[,1], y1 = clim.score[,2], length = 0.1, lty = 1, lwd = 2.5, col="#1c4871ff")
  if(Show.text == T){text(PClim.sc.site, sub("M","", row.names(MP)), cex=.5, pos = 3)}            # textes sites
  # text(PClim.sc, colnames(MP), cex=.95, pos = 3, col = "#6e2115ff")                # textes taxa
  text(PClim.sc, colnames(MP), cex=.95, pos = 3, col = "#6e2115ff")                # textes taxa
  text(clim.score, row.names(clim.score), cex=1.3, pos = 3, col = "#1c4871ff")      # textes clim
  points(PClim.sc.site, pch = 19, cex = 1.5, col = Col.eco)
  
  if(Title.inside == T){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.01, 0.95, Annot, adj = 0, cex = 1.7)  # if that's what you want
    par(usr = usr) # restore original user coordinates
  }
  
  
  #### Legend #####
  if(Clustering == T & Display.legends == T){
    legend("bottomleft",
           legend = levels(Fact.eco),
           col = diamond_color_colors,
           pch = 19,
           y.intersp = 0.7,	                                     # espace entre y
           x.intersp = 0.4,                                      # espace entre x
           bty = "n" )
  }
  
  #### Legend 2 ####
  legend(Leg.loc2,
         legend = bquote(italic(R)^2  == ~ .(format(R2, digits = 2)) ~ ", n = " ~ .(nrow(MP))),
         pch = NA,
         y.intersp = 0.7,	                                     # espace entre y
         x.intersp = 0.4,                                      # espace entre x
         bty = "n" )
  
  #### Add symbol ####
  if(is.null(Symbol.loc) == F & is.null(Symbol.path) == F){
    library(png)
    pic <- readPNG(Symbol.path)
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    rasterImage(pic,0.83,0.83,0.99,0.99)
    par(usr = usr) # restore original user coordinates
    }
  
  #### Export data ####
  if(is.null(Save.path) == F){
    Site.name <- gsub(" ","_",Site.name)
    Save.path.Site <- gsub("\\.csv", "_RDA_site.csv", Save.path)
    Save.path.Taxon <- gsub("\\.csv", "_RDA_taxa.csv", Save.path)
    write.table(t(PClim.sc.site), file = Save.path.Site, col.names = FALSE, sep = ",")
    write.table(t(PClim.sc), file = Save.path.Taxon, col.names = TRUE, sep = ",")
  }
  
  return(MP.pca)
}

PCoI.vegetation <- function(MV, MP, Meco = NULL, H, W,  Dot.opac = 0.6, Dot.size = 2.5, Legend.pos = "none",
                            Stats.pos = NULL, Show.outliers = F, Show.site.lab = F, Eco.col = NULL,
                            Symbol.path = NULL, Symbol.pos = NULL, Title = NULL,  Eco.lab = NULL,
                            Save.plot, Reverse.dim = F, Show.errors = T, Return.plot = F){
  #### Settings ####
  library(ggrepel) # nom des lignes à côté
  library(ggnewscale) # function new_scale_color
  if(missing(H)){H = 400}
  if(missing(W)){W = 800}
  if(missing(Save.plot)){Save.plot = NULL}
  
  library(missMDA)
  
  
 
  #### Fullfill NA's ####
  MP <- data.frame(t(data.frame(MP)))
  if(nlevels(as.factor(is.na(MP))) >= 2){
    print("NA's fullfilled for the PCA for MP.")
    nb <- estim_ncpPCA(MP, ncp.max=5) ## Time consuming, nb = 2
    MP.comp <- imputePCA(MP, ncp = nb[[1]])
    MP <- data.frame(t(data.frame(MP.comp$completeObs)))
    }
  else{MP <- data.frame(t(data.frame(MP)))}
  
  MV <- data.frame(t(data.frame(MV)))
  if(nlevels(as.factor(is.na(MV))) >= 2){
    print("NA's fullfilled for the PCA for MV.")
    nb <- estim_ncpPCA(MV, ncp.max=5) ## Time consuming, nb = 2
    MV.comp <- imputePCA(MV, ncp = nb[[1]])
    MV <- data.frame(t(data.frame(MV.comp$completeObs)))
    }
  else{MV <- data.frame(t(data.frame(MV)))}
    
  
  #### PCA calculations ####
  MP <- MP[,which(names(MP) %in% names(MV))]
  MV <- MV[,which(names(MV) %in% names(MP))]
  MP <- MP[,match(names(MV), names(MP))]
  par(mfrow=c(1,2))
  PCA.MV <- PCA.vegetation(MV, Csv.sep =",", transp_OK = T, Scale.PCA = 1, Site.name = "MV")
  PCA.MP <- PCA.vegetation(MP, Csv.sep =",", transp_OK = T, Scale.PCA = 1, Site.name = "MP")
  par(mfrow=c(1,1))
  
  #### Procrustes calculations ####
  pro <- procrustes(X = PCA.MV, Y = PCA.MP, symmetric = F)
  PROTEST <- protest(PCA.MV, PCA.MP)
  ctest <- data.frame(rda1 = pro$Yrot[,1],
                      rda2 = pro$Yrot[,2],
                      xrda1 = pro$X[,1],
                      xrda2 = pro$X[,2])
  
  if(is.null(Meco) == F){ctest$Ecosystem <- as.factor(Meco[which(row.names(Meco) %in% row.names(ctest)), "Ecosystem"])}
  else{ctest$Ecosystem <- "Sites"}
  
  #### Color ecosystems ####
  values.bi = c("Light taiga" = "#1874CD",
                "Sites" = "#658D94",
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
  
  ctest$Lab <- row.names(ctest)
 

  if(is.null(Eco.col) == F & is.null(Meco) == F){
    Eco.col <- Eco.col[unique(Meco$Ecosystem)]
    Scale.color <- scale_color_manual(values = Eco.col)
    Scale.fill <- scale_fill_manual(values = Eco.col)
    
    if(is.null(Eco.lab) ==F){
      Scale.color <- scale_color_manual(values = Eco.col, labels = Eco.lab, name = "Vegetation-types")
      Scale.fill <- scale_fill_manual(values = Eco.col, labels = Eco.lab, name = "Vegetation-types")
    }
  }
  else{
    Scale.color <- scale_color_manual(values = values.bi, guide = F)
    Scale.fill <- scale_fill_manual(values = values.bi, guide = F)
  }
  
  
  
  if(Show.outliers == T){Col.out = "grey60"}
  if(Show.outliers == F){Col.out = NA}
  
  #### Other param graph + stats ####
  if(Reverse.dim == T){
    # if(Show.annot == T){
    #   Note.n <- annotate("text", x = min(MP.pca$ind$coord[,2]), y = min(MP.pca$ind$coord[,1]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0)}
    # else{Note.n <- NULL}
    My_labs <- labs(x = substitute(paste("PCoI"[2])),y = substitute(paste("PCoI"[1])))
    names(ctest) <- names(ctest)[c(2,1,4,3,5,6)]
    }
  else{My_labs <- labs(x = substitute(paste("PCoI"[1])),y = substitute(paste("PCoI"[2])))}
  
  if(is.null(Title) == F){Mytit <- ggtitle(Title)}
  else{Mytit <- NULL}
  
  if(is.null(Stats.pos) == F){
    Note.n <- annotate("text", x = Stats.pos[1], y = Stats.pos[2], 
                       label = paste("m12^2 = ", round(pro$ss, digits = 2), ", n = ", ncol(MP),
                                     ",\nPROTEST r = ", round(PROTEST$t0, digits = 2), 
                                     ",\nPROTEST p-value > ", round(PROTEST$signif, digits = 3), 
                                     sep = ""), size = 3, color = "grey10", hjust = 0, vjust = 1)
  }
  else{Note.n <- NULL}
  
  if(Show.site.lab == T){Site.lab <- geom_text(aes(x=xrda1, y=xrda2, label = Lab, color = Ecosystem))}
  else{Site.lab <- NULL}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Ajout symbole ####
  if(is.null(Symbol.path) == F){
    if(is.null(Symbol.pos)== T){Symbol.pos <- c(.9, .9, .16)}
    library(png)
    library(grid)
    img <- readPNG(Symbol.path)
    g <- rasterGrob(x = Symbol.pos[1], y = Symbol.pos[2], width = Symbol.pos[3], height = Symbol.pos[3], img, interpolate = T)
    Logo <- annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  }
  else{Logo <- NULL}
  
  #### Plot 1 ####
  p <- ggplot(ctest) +
    geom_vline(xintercept = 0, lty = "dashed")+
    geom_hline(yintercept = 0, lty = "dashed")+
    geom_segment(aes(x=rda1, y=rda2, xend=xrda1, yend=xrda2, color = Ecosystem), alpha = 0.6, arrow = arrow(length=unit(0.25,"cm")))+
    geom_point(aes(x=rda1, y=rda2, color = Ecosystem), size = Dot.size, alpha = Dot.opac) +
    My_labs + Mytit + Site.lab +
    Scale.color+Scale.fill+
    new_scale_color() + 
    Logo + Note.n +
    theme_classic()+
    theme(axis.line = element_blank(), 
          strip.background = element_blank(),
          strip.placement = "outside", legend.position = Legend.pos,
          panel.border = element_rect(NA, "black", linewidth = 1)
          )
  
  #### Procrustes errors ####
  ctest$Procrustes_error <- sqrt((ctest$rda1-ctest$xrda1)^2+(ctest$rda2-ctest$xrda2)^2)
  ctest <- ctest[order(ctest$Procrustes_error,decreasing = T),]
  row.names(ctest) <- seq(1:nrow(ctest))
  ctest$ID <- as.numeric(row.names(ctest))
  
  p2 <- ggplot(ctest, aes(x = ID, y = Procrustes_error)) +
    geom_bar(aes(fill = Ecosystem), position = 'dodge', stat='identity')+
    geom_hline(yintercept = mean(ctest$Procrustes_error), color="grey30", lwd = 0.2)+
    geom_hline(yintercept = quantile(ctest$Procrustes_error)[c(2,4)], color="grey30", lwd = 0.2, linetype = "longdash")+
    coord_flip()+
    Scale.color+Scale.fill+ 
    theme_classic()+
    theme(axis.line.x = element_blank(), legend.position = Legend.pos, axis.ticks.x = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())
  
  #### Boxplot errors #### 
  p3 <- ggplot(ctest) +
    geom_boxplot(aes(Procrustes_error, y = Ecosystem, fill = Ecosystem), outlier.colour = Col.out)+
    geom_vline(xintercept = mean(ctest$Procrustes_error), color="grey30", lwd = 0.2)+
    geom_vline(xintercept = quantile(ctest$Procrustes_error)[c(2,4)], color="grey30", lwd = 0.2, linetype = "longdash")+
    Scale.color+Scale.fill+ 
    xlab("Procrustes error")+ 
    theme_classic()+
    theme(axis.title.y = element_blank(), legend.position = Legend.pos)
  
  #### Save and export ####
  if(Show.errors == T){p <- p|(p2 / p3 + plot_layout(widths = c(3/4, 1/4), guides = "collect")) + plot_layout(heights = c(3/5, 2/5))}
  
  if(is.null(Save.plot) == F){
    print(p)
    dev.off()}
  
  if(Return.plot == T){return(p)}
  else{return(ctest)}
  }

LDA.vegetation <- function(MV, H, W, Cluster, Color.choise = NULL, Connect.area = "Polygon", Limites = NULL,
                           Cluster.path, Sort.cluster, Save.plot){
  #### Settings ####
  library(MASS)
  if(missing(H)){H = 400}
  if(missing(W)){W = 800}
  if(missing(Sort.cluster)){Sort.cluster = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  
  #### LDA computation ####
  # MLDA <- scale(MV, center = F)
  MLDA <- log1p(MV)
  lda.eco <- Cluster.path[match(row.names(MLDA), row.names(Cluster.path)),]

  Veget.lda <- lda(MLDA, lda.eco[[Cluster]])
  LD_prop_veg <-prop.table(Veget.lda$svd^2)
  LD1_veg <- LD_prop_veg[1]
  LD2_veg <- LD_prop_veg[2]
  
  Pronos_veg <- predict(Veget.lda)$x
  Pronos_veg <- cbind(data.frame(Pronos_veg), lda.eco[Cluster])
  names(Pronos_veg)[ncol(Pronos_veg)] <- "Ecosystems"
  if(is.null(Sort.cluster) == F){Pronos_veg$Ecosystems <- factor(Pronos_veg$Ecosystems, levels = Sort.cluster)}
  else{Pronos_veg$Ecosystems <- factor(Pronos_veg$Ecosystems)}
  Pronos_veg$Label <- gsub("^M", "", row.names(Pronos_veg))
  Pronos2D_veg <- Pronos_veg[,1:2]
  
  #### Which kind of connecting area ####
  if(Connect.area == "Polygon"){
    LDA_hull <- 
      Pronos_veg %>% 
      group_by(Ecosystems) %>% 
      slice(chull(LD1, LD2))
    
    Connect.area.stuff <- geom_polygon(data = LDA_hull, aes(x = LD1, y = LD2, color = Ecosystems, fill = Ecosystems), alpha = .3)}
  if(Connect.area == "Ellypse"){
    Connect.area.stuff <- stat_ellipse(type = "t", level = .5)}
  # else{Connect.area.stuff <- NULL}
  
  #### Colors settings ####
  if(is.null(Color.choise) == F){
    values.bi <- Color.choise}
  else{
    values.bi = c("Desert" = "firebrick3", "Haloxylon ammodendron scrub" = "firebrick3", 
                  "Mountain steppes meadows"= "#6789CE", 
                  "Mountain Forest-steppes" = "#9FBB67", 
                  "Forest-steppes"= "dodgerblue3",
                  "Forest" = "darkgreen",
                  "Desert-steppe" = "darkorange", 
                  "Artemisia steppe" = "darkorange", 
                  "Steppe" = "goldenrod1",
                  "Forest-steppe" = "green",
                  "Riparian forest" = "darkgreen",
                  "Alpine steppe" = "dodgerblue3",
                  "Anthropic" = "grey10",
                  "Azerbaijan shrub desert and steppe" = "darkorange",
                  "Caucasus mixed forests" = "darkgreen",
                  "Caspian Hyrcanian mixed forests" = "dodgerblue3"
                  )}
  
  values.bi <- values.bi[which(names(values.bi) %in% levels(Pronos_veg$Ecosystems))]
  Scale.fill <- scale_fill_manual(values = values.bi, drop = T)
  Scale.color <- scale_color_manual(values = values.bi, drop = T)
  
  if(is.null(Limites) == F){My_lims <- lims(x = c(Limites[1], Limites[2]), y = c(Limites[3], Limites[4]))}
  else{My_lims <- NULL}
  
  #### Plot ####
  library(ggrepel)
  pLDA <- ggplot(data = data.frame(Pronos_veg), mapping = aes(x = LD1, y = LD2, color = Ecosystems))+
    Connect.area.stuff +
    geom_text_repel(aes(label = Label), size = 2.5, force = 10, nudge_y = 0.01, nudge_x = 0.01) +
    geom_point(shape = 16, size = 2.5)+
    Scale.fill + Scale.color + My_lims +
    guides(color = guide_legend(nrow = 2))+
    xlab(paste("LDA1 (",format(LD1_veg*100, digits=4),"%)"))+
    ylab(paste("LDA2 (",format(LD2_veg*100, digits=4),"%)"))+
    theme(legend.position = "top")+
    theme(plot.background = element_blank(), panel.background = element_blank(),
          legend.key = element_blank(),
          panel.border = element_rect(NA, "black", linewidth = 1),
          panel.grid = element_line(linetype = "dashed"),
          axis.line = element_blank())
  
  ggsave(pLDA, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
  
}

CA.vegetation <- function(MV, Cluster.path, Cluster.groups, Cluster.groups.lab, Sort.cluster = NULL,
                          Color.choise = NULL, Dot.size = 3, Reverse.dim = F, Show.annot = F, Manu.lim.x = NULL,
                          Manu.lim.y = NULL, Dot.opac = NULL, Lab.size = 5, Annot = NULL,
                          Legend.position = "right", X.pos = "bottom", Y.pos = "left", Pollen = NULL, Show.site.name = T,
                          Ellipse = F, Show.centroid = F, H, W, Nb.contrib = 15, Save.plot){
  #### Settings ####
  library(ggnewscale) # function new_scale_color
  library(FactoMineR)
  library(factoextra)
  if(missing(H)){H = 400}
  if(missing(W)){W = 800}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Cluster.groups.lab )){Cluster.groups.lab = "Biome (Dinerstein et al., 2017)"}
  if(Show.centroid == F){Centroide = "quali"}
  if(Show.centroid == T){Centroide = NULL}
  if(missing(Cluster.path)){Cluster.path = NULL}
  if(missing(Cluster.groups)){Cluster.groups = "Ecosystems"}
  Groupes = NULL
  
  #### Pour le pollen ####
  if(is.null(Pollen) == T){
    row.names(MV) <- gsub("aceae",".",row.names(MV))
    row.names(MV) <- gsub(".undiff","",row.names(MV))
    row.names(MV) <- gsub(".indet","",row.names(MV))
    row.names(MV) <- gsub("Po.","Poa.",row.names(MV))
    row.names(MV) <- gsub(".spp.","",row.names(MV))
    row.names(MV) <- gsub(".type","",row.names(MV))
  }
  
  #### CA calculation ####
  MCA <- MV
  names(MCA) <- gsub("^MAST", "", names(MCA))
  names(MCA) <- gsub("^M", "", names(MCA))
  res.ca <- CA(scale(MCA, center = F), graph = FALSE)
  
  if(is.null(Cluster.path) == F){
    Cluster.path <- Cluster.path[match(names(MV), row.names(Cluster.path)),]
    MV <- data.frame(t(MV))
    MV <- cbind(data.frame(MV), Cluster.path[Cluster.groups])
  }
  
  if(is.null(Dot.opac) == F){Opac <- Dot.opac}
  else{Opac <- 1}
  
  #### Color settings ####
  if(is.null(Groupes) == F){
    my_orange = c("Water" = "darkblue",
                  "Altitude" = "brown",
                  "Temperature" = "darkred")
    
    Scale.color.vec <- scale_color_manual(values = my_orange, name = "Proxies")
    
  }
  else{
    Groupes <- "Unique"
    my_orange <- data.frame(Unique = "royalblue")
    Scale.color.vec <- scale_color_manual(values = my_orange, guide = "none")
  }
  
  if(is.null(Cluster.path) == F){
    if(is.numeric(MV[[Cluster.groups]]) == T){
      my_orange2 = brewer.pal(n = 11, "RdYlBu")[Keep.col2[-c(3,4,5,7,8,9)]] 
      orange_palette2 = colorRampPalette(my_orange2)
      my_orange2 = rev(orange_palette2(length(seq(min(MV[[Cluster.groups]]), max(MV[[Cluster.groups]]), by = 200))))
      Scale.fill <- scale_fill_gradientn(colours = my_orange2, guide = "colourbar", 
                                         name = Cluster.groups.lab,
                                         breaks = seq(round(min(MV[[Cluster.groups]]),digits = -3), max(MV[[Cluster.groups]]), by = 1000),
                                         na.value = "white")}
    else{
      if(is.null(Color.choise) == F){
        values.bi <- Color.choise
        print(values.bi)
        }
      else{
        values.bi = c("Desert" = "firebrick3", "Dwarf shrubs" = "firebrick3", "Haloxylon ammodendron scrub" = "firebrick3", 
                      "Mountain steppes meadows"= "darkorange", 
                      "Mountain Forest-steppes" = "#9FBB67", "Quercus Acer-Juniperus woodland" = "#9FBB67", 
                      "Forest-steppes"= "#6789CE", "Juniperus woodland"= "dodgerblue3",
                      "Forest" = "darkgreen", "Riparian forest" = "darkgreen", 
                      "Caucasus mixed forests" = "darkgreen", "Hyrcanian forest" = "darkgreen",
                      "Desert-steppe" = "firebrick3", "Woodland" = "#9FBB67",
                      "Artemisia steppe" = "darkorange", 
                      "Steppe" = "goldenrod1", "Artemisia dwarf shrubs" = "goldenrod1",
                      "Forest-steppe" = "green", "Mountain Artemisia steppe" = "#FFC125",
                      "Alpine steppe" = "dodgerblue3","Mountain meadows" = "dodgerblue3","Acer monspessulanum scrub" = "dodgerblue3",
                      "Anthropic" = "grey10","Acer-Juniperus woodland" = "grey10","Acer cappadocicum thicket" = "grey10",
                      "Azerbaijan shrub desert and steppe" = "darkorange",
                      "Caspian Hyrcanian mixed forests" = "dodgerblue3"
        )}
      if(is.null(Sort.cluster) == F){
        MV[[Cluster.groups]] <- factor(MV[[Cluster.groups]], Sort.cluster, ordered = T)
      }
      values.bi <- values.bi[which(names(values.bi) %in% unique(MV[[Cluster.groups]]))]
      print(values.bi)
      Scale.fill <- scale_fill_manual(values = values.bi, name = Cluster.groups.lab, drop = T)
      Scale.color <- scale_color_manual(values = values.bi, name = Cluster.groups.lab, drop = T)
    }
    Col.select <- MV[[Cluster.groups]]
    Fill.select <- MV[[Cluster.groups]]
  }
  else{
    Scale.fill <- scale_fill_manual(values = "brown", name = "Species density")
    Scale.color <- scale_color_manual(values = "brown", name = "Species density")
    Opac = 0.2
    Col.select = "brown"
    Fill.select = "brown"
  }
  #### Other settings ####
  if(Show.site.name == F){My_geom = "point"}
  else{My_geom = c("point", "text")}
  print(res.ca$col)
  if(is.null(Annot) == F){Annot <- annotate("text", x = min(res.ca$col$coord[,1]), y = max(res.ca$col$coord[,2]), label = Annot, size = Lab.size*2.3, hjust = 0.2)}
  #### Reverse dim ####
  A = round(res.ca$eig[1,2], digits = 0)
  B = round(res.ca$eig[2,2], digits = 0)
  
  if(Reverse.dim == T){
    if(Show.annot == T){
      Note.n <- annotate("text", x = min(res.ca$col$coord[,2]), y = min(res.ca$col$coord[,1]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0)}
    else{Note.n <- NULL}
    Axes <- c(2,1)
    X.arrow <- c(2)
    Y.arrow <- c(1)
    Xlab <- labs(x = substitute(paste("CA"[2], ~ "(", B, " %)", sep = " " )),
                 y = substitute(paste("CA"[1], ~ "(", A, " %)", sep = " " )))
  }
  else{
    if(Show.annot == T){
      Note.n <- annotate("text", x = min(res.ca$col$coord[,1]), y = min(res.ca$col$coord[,2]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0, vjust = 0.5)}
    else{Note.n <- NULL}
    
    Axes = c(1,2)
    X.arrow <- c(1)
    Y.arrow <- c(2)
    Xlab <- labs(x = substitute(paste("CA"[1], ~ "(", A, " %)", sep = " " )),
                 y = substitute(paste("CA"[2], ~ "(", B, " %)", sep = " " )))
  }
  if(is.null(Manu.lim.x)==F){
    Lim.x <- xlim(Manu.lim.x)
    if(Show.annot == T){Note.n <- annotate("text", x = Manu.lim.x[1], y = min(res.ca$col$coord[,2]), label = paste("n = ", nrow(MP), sep = ""), size = 5, hjust = 0, vjust = 0.5)}
    else{Note.n <- NULL}
  }
  else{Lim.x <- NULL}
  if(is.null(Manu.lim.y)==F){Lim.y <- ylim(Manu.lim.y)}
  else{Lim.y <- NULL}
  
  #### Plot ####
  p <- fviz_ca_biplot(res.ca, repel = F, arrows = c(T,F), shape.col = 16, 
                      pointsize = Dot.size, geom.col = My_geom,
                      labelsize = Lab.size,
                      alpha.col = Opac, #alpha.var ="contrib",
                      col.col = Col.select, map = "symbiplot",
                      alpha.row = "contrib",
                      invisible = Centroide, # enlève ou ajoute le centroïde
                      addEllipses = Ellipse, ellipse.level = 0.75, ellipse.type = "convex", # convex or norm
                      ellipse.alpha = 0.2,
                      select.row = list(contrib = Nb.contrib))+
    Xlab + Annot + 
    Scale.color +
    scale_x_continuous(position = X.pos) +
    scale_y_continuous(position = Y.pos) +
    theme(plot.background = element_blank(),
          legend.position = Legend.position,  axis.ticks.length = unit(0.8,'mm'),
          axis.title = element_text(size = 7, margin = margin(t = 0, r = 0, b = 0, l = 0)),
          axis.ticks = element_line(colour = "grey70", linewidth = .4),
          axis.text = element_text(size = 6),
          legend.key = element_blank(), plot.title = element_blank(),
          panel.border = element_rect(NA, "grey70", linewidth = .4),
          panel.grid = element_line(linetype = "dashed"),
          axis.line = element_blank()
          )
  
  print(p)
  ggsave(p, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")
  return(p)
  }

Hclust.vegetation <- function(MV, H, W, Save.plot){
  #### Settings ####
  library(plotly)
  library(ggplot2)
  library(ggdendro)
  if(missing(H)){H = 400}
  if(missing(W)){W = 800}
  if(missing(Save.plot)){Save.plot = NULL}
  
  #### Computation + plot ####
  dd <- dist(scale(MV), method = "euclidean")
  hc <- hclust(dd, method = "ward.D2")
  
  phit <- fviz_dend(hc, k = 10, show_labels = T, rect = T, k_colors = "npg", lwd = .3, type = "rectangle", horiz = T)
  # print(phit)
  ggsave(phit, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
}

# Search the value of 1 taxa in all the releves
Search.releve <- function(MV, Taxa, Quadrat, Value){
  #### Settings ####
  S.T = T
  S.Q = T
  S.V = T
  if(missing(Taxa)){S.T = F}
  if(missing(Quadrat)){S.Q = F}
  if(missing(Value)){S.V = F}
  
  #### Search ####
  RR = NA
  # if(S.T == T){RR = lapply(MV, function(x) subset(x, row.names(x) == Taxa))}
  # if(S.Q == T){RR = lapply(MV, function(x) subset(x, colnames(x) == Quadrat))}
  
  #if(S.V == T){RR = lapply(MV, function(x) subset(x, row.names(x) == Value))}
  
  if(S.T == T){RR = lapply(MV, function(x) subset(x, rownames(x) %in% Taxa))}
  if(S.Q == T){RR = lapply(MV, function(x) subset(x, colnames(x) %in% Quadrat))}
  
  # RR = RR[lapply(RR,length)>0]
  RR <- RR[lapply(RR, function(x) length(row.names(x)))>0]
  # if(S.T == T & S.Q == T){
  #   R1 = lapply(MV, function(x) subset(x, row.names(x) == Taxa))
  #   RR = lapply(R1, function(x) subset(x, , colnames(x)== Quadrat))
  #   }
  # print(RR)
  return(RR)
  }

Diag.pol.veget <- function(MV = NULL, MP = NULL, Meco = NULL, Max_seuil = NULL, Display.legend = "none", Sp.lab.size = 7,
                           Show.site.name, Rotate = F, Nzone = 6, AP.NAP.ShP.col = F, Eco.col = NULL, Aggregation.ss = F,
                           Remove.sites.not.full = F, Eco.dot.size, Keep.taxa = NULL, CONISS.t, Abiot.plot = NULL, Leg.nb.row = 2,
                           Pollen.index = NULL, AP.NAP = F, ShP = F, Show.site.eco = F, CONISS.Lab.size = 4, Eco.lab = NULL,
                           Print.cluster.zone = F, Print.cluster.name = F, Save.CONISS = NULL, Ordin, H, W, Save.plot){
  #### Settings ####
  library(tibble)
  library(ggplot2)
  library(tidypaleo)
  library(tidyverse)
  library(patchwork)
  library("tableHTML")
  if(missing(Ordin)){Ordin = NULL}
  if(missing(Meco)){Meco = NULL}
  if(missing(CONISS.t)){CONISS.t = NULL}
  if(missing(Show.site.name)){Show.site.name = T}
  if(missing(Eco.dot.size)){Eco.dot.size = 2.5}
  Add.other = F
  
  #### Correspondance and clean ####
  if(Remove.sites.not.full == T){
    MP <- MP[, intersect(names(MP), names(MV))]
    MV <- MV[, intersect(names(MP), names(MV))]
    }
  else{
    Pollen.no.veget <- setdiff(names(MP), names(MV))
    Veget.no.pollen <- setdiff(names(MV), names(MP))
    Add.to.veget <- setNames(data.frame(matrix(NA, nrow(MV), length(Pollen.no.veget))), Pollen.no.veget)
    # MP <- MP[, union(names(MP), names(MV))]
    MV <- cbind(MV, Add.to.veget)
    }
  
  Mclim <- Meco
  
  #### AP / NAP ####
  if(AP.NAP == T | AP.NAP.ShP.col == T){
    if(is.null(Pollen.index) == T){
      print("**** Add a pollen taxa index containing AP and NAP informations. ****")
      print("**** Or change AP.NAP.ShP.col == F. ****")
      stop()
      MAP.NAP <- NULL}
    else{
      if(Aggregation.ss == F){Pollen.type.lab <- "PT_sl.label"}
      if(Aggregation.ss == T){Pollen.type.lab <- "PT_ss.label"}
      
      Pollen.index <- Pollen.index[!duplicated(Pollen.index[Pollen.type.lab]),c("AP_NAP", Pollen.type.lab)]
      
      if(ShP == F){Pollen.index$AP_NAP[Pollen.index$AP_NAP == "ShP"] <- "NAP"}
      if(ShP == F){Pollen.index$AP_NAP[Pollen.index$AP_NAP == "ShP"] <- "NAP"}
      
      MAP.NAP.calc <- function(M, Type){
        if(ShP == T){MAS <- colSums(M[intersect(row.names(M),Pollen.index[Pollen.index$AP_NAP == "ShP", Pollen.type.lab]),])}
        MAP <- data.frame(AP = colSums(M[intersect(row.names(M),Pollen.index[Pollen.index$AP_NAP == "AP", Pollen.type.lab]),]))
        MNAP <- data.frame(NAP = colSums(M[intersect(row.names(M),Pollen.index[Pollen.index$AP_NAP == "NAP", Pollen.type.lab]),]))
        MAP.NAP <- cbind(AP = MAP, NAP = MNAP)
        if(ShP == T){MAP.NAP <- cbind(MAP.NAP, ShP = MAS)}
        MAP.NAP <- MAP.NAP/rowSums(MAP.NAP)*100
        
        MAP.NAP$Type <- Type
        MAP.NAP$Site <- row.names(MAP.NAP)
        
        MAP.NAP <- melt(MAP.NAP, by = c("Type", "Site"))
        Missing.stuff <- row.names(M[setdiff(row.names(M), Pollen.index[Pollen.index$AP_NAP %in% c("NAP", "AP", "ShP"), Pollen.type.lab]),])
        
        if(is.null(Keep.taxa) == T & length(Missing.stuff)>0){
          print(paste("The following taxa don't have AP, NAP values in the ", Type, " matrix.", sep = ""))
          print(Missing.stuff)
          }
        if(is.null(Keep.taxa) == F & length(intersect(Missing.stuff, Keep.taxa))>1){
          print(paste("The following taxa don't have AP, NAP values in the ", Type, " matrix.", sep = ""))
          print(intersect(Missing.stuff, Keep.taxa))}
        
        return(MAP.NAP)
      }
      
      MAP.NAP.MP <- MAP.NAP.calc(MP, "Pollen")
      MAP.NAP.MV <- MAP.NAP.calc(MV, "Vegetation")
      MAP.NAP <- rbind(MAP.NAP.MV, MAP.NAP.MP)
      
      List.AP <- row.names(MP[intersect(row.names(MP),Pollen.index[Pollen.index$AP_NAP == "AP", Pollen.type.lab]),])
      List.NAP <- row.names(MP[intersect(row.names(MP),Pollen.index[Pollen.index$AP_NAP == "NAP", Pollen.type.lab]),])
      Max.by.tax.MP <- apply(MP, 1, max, na.rm = TRUE)
      Max.by.tax.MV <- apply(MV, 1, max, na.rm = TRUE)
      Max.by.tax <- rbind(Max.by.tax.MP, Max.by.tax.MV)
      Max.by.tax <- apply(Max.by.tax, 2, max, na.rm = TRUE)
      
      if(ShP == T){
        List.ShP <- row.names(MP[intersect(row.names(MP),Pollen.index[Pollen.index$AP_NAP == "ShP", Pollen.type.lab]),])
        if(any(Keep.taxa == "Other")){List.ShP <- c(List.ShP, 'Other')}
        My_title <- "AP vs. NAP \n vs. ShP (%)"}
      else{
        if(any(Keep.taxa == "Other")){List.NAP <- c(List.NAP, 'Other')}
        My_title <- "AP vs. NAP (%)"
        }
    }
    }
  else{MAP.NAP <- NULL}
  
  #### Fusion taxons rares #### 
  if(is.null(Max_seuil) == F){
    MP_max <- MP[0]
    MP_max[, "max"] <- apply(MP[,], 1, max)
    MP_dom <- MP[rowSums(MP_max)>Max_seuil,]
    MP_rare <- MP[rowSums(MP_max)<=Max_seuil,]
    MP_dom["Other" ,] <- colSums(MP_rare)
    MP <- data.frame(MP_dom)
    
    MV_rare <- MV[row.names(MV) %in% row.names(MP_rare),]
    MV_rare <- rbind(MV_rare, MV["Incertae sedis",])
    MV <- MV[!row.names(MV) %in% row.names(MP_rare),]
    MV <- MV[row.names(MV) !="Incertae sedis",]
    MV["Other" ,] <- colSums(MV_rare)
    }
  
  if(is.null(Keep.taxa) == F){
    if(any(Keep.taxa == "Other")){
      Keep.taxa <- Keep.taxa[Keep.taxa != "Other"]
      Other.MP <- colSums(MP[setdiff(row.names(MP), Keep.taxa),])
      Other.MV <- colSums(MV[setdiff(row.names(MV), Keep.taxa),])
      Add.other = T
      }
        
    MP <- MP[match(Keep.taxa,row.names(MP)),]
    MV <- MV[match(Keep.taxa,row.names(MV)),]
    
    if(Add.other == T){
      MP <- rbind(MP, Other = Other.MP)
      MV <- rbind(MV, Other = Other.MV)
      Keep.taxa <- c(Keep.taxa, "Other")
      Max.Other <- max(max(MP[row.names(MP) == "Other",], na.rm = T), max(MV[row.names(MV) == "Other",], na.rm = T), na.rm = T)
      if(AP.NAP == T | AP.NAP.ShP.col == T){Max.by.tax <- c(Max.by.tax, Other = Max.Other)}
    }
  }
  
  #### MP and MV melt ####
  MP$Taxa <- row.names(MP)
  MP <- reshape2::melt(MP, id = "Taxa")
  MP$Type <- "Pollen"
  MV$Taxa <- row.names(MV)
  MV <- reshape2::melt(MV, id = "Taxa")
  MV$Type <- "Vegetation"
  MT <- rbind(MV, MP)
  
  #### Grey zone ####
  if(Remove.sites.not.full == F){
    MT$Point <- NA
    MT$Point[is.na(MT$value)] <- 0
    # MT$Taxa[is.na(MT$value)] <- "Missing MV"
    Points.NA <- geom_point(inherit.aes = F, data = MT, mapping = aes(x = Point, y = variable), color = "grey10", size = .3, na.rm = T)
    }
  else{Points.NA <- NULL}
  
  #### Graph settings ####
  if(is.null(Keep.taxa) == F){MT$Taxa <- factor(MT$Taxa, levels = Keep.taxa)}
  if(Show.site.name == T){
    if(is.null(Abiot.plot) == F){
      if(is.null(Meco) == F & Show.site.eco == T){
        Site.name4 <- element_text(angle = 0, size = 5, margin = margin(r = 0))
        Site.name2 <-  element_blank()
      }
      else{
        Site.name2 <-  element_text(angle = 0, vjust = 0.5, hjust = 1, size = 5)
        Site.name4 <- element_blank()   
      } 
      Site.name <- element_blank()
      My_ticks1 <- element_blank()
      My_ticks2 <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
      My_ticks3 <- element_blank()
      Site.name3 <- element_blank()
      Vertical.axe <- element_blank()
    }
    else{
      if(AP.NAP == T){
        if(is.null(Meco) == F & Show.site.eco == T){
          Site.name4 <- element_text(angle = 0, vjust = 0.5, hjust = 0, size = 5, margin = margin(r = 0))
          Site.name3 <-  element_blank()}
        else{
          Site.name3 <-  element_text(angle = 0, vjust = 0.5, hjust = 1, size = 5)
          Site.name4 <-  element_blank()}
      
      Site.name <-  element_blank()
      Vertical.axe <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
      Site.name <-  element_blank()
      Site.name2 <-  element_blank()
      My_ticks1 <- element_blank()
      My_ticks2 <- element_blank()
      My_ticks3 <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
      }
      else{
        Site.name <-  element_text(angle = 0, vjust = 0.5, hjust = 1, size = 5)}
        Site.name2 <-  element_blank()
        Site.name3 <-  element_blank()
        Site.name4 <-  element_blank()
        My_ticks1 <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
        My_ticks2 <- element_blank()
    }
      }
  if(Show.site.name == F){
    Site.name <-  element_blank()
    Site.name2 <-  element_blank()
    Site.name3 <-  element_blank()
    Site.name4 <-  element_blank()
    Vertical.axe <- element_blank()
    My_ticks3 <- element_blank()
    if(is.null(Abiot.plot) == F){
      My_ticks1 <- element_blank()
      My_ticks2 <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
      }
    else{
      My_ticks1 <- element_line(lineend = "butt", color = "grey70", linewidth = .1)
      My_ticks2 <- element_blank()
      }
    }
  
  if(is.null(CONISS.t) == T & is.null(Meco) == T){My.margin <- unit(c(0,1.5,0,0), "cm")}
  else{My.margin <- unit(c(0,0.2,0,0), "line")}
  
  
  #### Sort by Manual ordination ####
  if(is.null(Ordin) == F){
    Inter <- intersect(row.names(Ordin), levels(MT$variable))
    Manque.ds.ordin <- setdiff(levels(MT$variable), row.names(Ordin))
    if(length(Manque.ds.ordin) > 0){print(paste("Le site", Manque.ds.ordin, "est manquant dans le fichier d'ordination."))}
    
    Ordin.inter <- setNames(data.frame(Ordin[which(row.names(Ordin) %in% Inter),]), "Ordination")
    row.names(Ordin.inter) <- row.names(Ordin)[which(row.names(Ordin) %in% Inter)]
    
    #### Si on veut tous les releves même les vides
    if(Remove.sites.not.full == F){Ordin.inter <- Ordin}
    names(Ordin.inter) <- "Ordination"
    # order <- as.factor(row.names(Ordin.inter)[rev(order(Ordin.inter$Ordination))])
    order <- factor(row.names(Ordin.inter)[rev(order(Ordin.inter$Ordination))], ordered = T, levels = row.names(Ordin.inter)[rev(order(Ordin.inter$Ordination))])
    MT$variable <- factor(MT$variable, ordered = T, levels = row.names(Ordin.inter)[rev(order(Ordin.inter$Ordination))])
  }
  else{
    order <- factor(unique(MT$variable))
  }
  
  
  #### CONISS cluster ####
  if(is.null(CONISS.t) == F){
    if(AP.NAP == T){
      MT2 <- MAP.NAP[c(3,2,4,1)]
      names(MT2) <- c("Taxa", "variable", "value", "Type")
      if(Remove.sites.not.full == F){MT2$Point <- NA}
      MT2 <- rbind(MT, MT2)
    }
    else{MT2 <- MT}
    
    MT2$value <- sqrt(MT2$value/100)^2
    MT_clust <- MT2 %>%
      dplyr::filter(Type == CONISS.t) %>% # Vegetation vs Pollen
      nested_data(qualifiers = variable, key = Taxa, value = value, fill = 0)
    
    if(is.null(Nzone) == F){MT_clust <- tidypaleo::nested_chclust_coniss(MT_clust, n_groups = Nzone)}
    else{MT_clust <- tidypaleo::nested_chclust_coniss(MT_clust)}
   
    if(Print.cluster.zone == T){
      df <- dplyr::select(MT_clust[[12]][[1]], variable, hclust_zone)
      df <- na.omit(df)
      df <- dplyr::group_by(df, hclust_zone) 
      CONISS.bound <- dplyr::summarize(df, Min.CONISS = min(variable, na.rm = T), Max.CONISS = max(variable, na.rm = T))
      CONISS.bound$hclust_zone <- paste("P", rev(CONISS.bound$hclust_zone), sep = "")
      CONISS.bound$MY_col <- rep("A", length(CONISS.bound$hclust_zone))
      # CONISS.bound$MY_col[which(odd(1:length(CONISS.bound$hclust_zone)))] <- "B"
      CONISS.bound$MY_col[odd(1:length(CONISS.bound$hclust_zone))] <- "B"
      CONISS.bound$Min <- match(CONISS.bound$Min.CONISS, order)-1
      CONISS.bound$Max <- match(CONISS.bound$Max.CONISS, order)
      
      Line.coniss <- geom_hline(yintercept = CONISS.bound$Min+0.5, color = "grey30", linewidth = .3, linetype = "dashed")
      Line.coniss2 <- geom_vline(xintercept = CONISS.bound$Min+0.5, color = "grey30", linewidth = .3, linetype = "dashed")
      # print("CONISS layer boundaries :")
      # print(CONISS.bound)
      }  
    else{
      Line.coniss <- NULL
      Line.coniss2 <- NULL
      }
    
    if(is.null(Save.CONISS) == F){
      CONISS.bound$hclust_zone <- paste("L", CONISS.bound$hclust_zone, sep = "")
      Fine.line <- c(match(CONISS.bound$Max.CONISS, MS.keep[[variable]]), match(CONISS.bound$Min.CONISS, MS.keep[[variable]]))
      Fine.line <- sort(Fine.line[!is.na(Fine.line)])
      Fine.line <- Fine.line[1:nrow(CONISS.bound)]
      saveRDS(CONISS.bound, Save.CONISS)}}
  else{
    Line.coniss <- NULL
    Line.coniss2 <- NULL
    }
  
  #### Plot ####
  Plot.by.PT <- function(MT, My_colors = c("#d9a15eff", "#3f6b3dff"), 
                         My_lab = c("Pollen", "Vegetation"), Tit = NULL, Sam.tit = ""){
    Plot.compa <- ggplot(MT, aes(x = value, y = variable, fill = Type)) +
      Line.coniss +
      geom_vline(xintercept = 0, color = "grey70", size = .2)+
      Points.NA +
      geom_colh(width = 0.85, position = "dodgev", na.rm = T) +
      facet_abundanceh(vars(Taxa), rotate_facet_labels = 45, dont_italicize = c("spp?\\.", "type", "[Oo]ther", "^(.*?)aceae", "tree", "deciduous")) +
      scale_fill_manual(values = My_colors, labels = My_lab)+
      scale_y_discrete(limits = order) +
      scale_x_continuous(breaks = seq(10,100,10), expand = c(0,0))+
      guides(fill = guide_legend(title.position = "top", nrow = Leg.nb.row)) +
      labs(x = Tit, y = NULL, fill = Sam.tit)+
      theme(
        axis.text.y = Site.name,
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1),
        axis.title = element_text(size=9),
        axis.line.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
        axis.ticks.y = My_ticks1,
        axis.ticks.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
        legend.position = Display.legend, legend.background = element_blank(),
        legend.key = element_blank(), legend.box.background = element_blank(),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(0,0,0,0),
        panel.spacing = unit(0.2, "lines"),
        legend.justification = "right",
        legend.direction = "horizontal",
        strip.background = element_blank(),
        strip.text = element_text(size = Sp.lab.size),
        plot.margin = My.margin
        )
    return(Plot.compa)
    }
  
  if(AP.NAP.ShP.col == F | is.null(Pollen.index) == T){
    Plot.compa <- Plot.by.PT(MT, Tit = "Pollen & vegetation relative abundances (%)", Sam.tit = "Sample Type")}
  else{
    MT.AP <- MT[MT$Taxa %in% List.AP,]
    MT.AP$Taxa <- factor(unique(MT.AP$Taxa))
    if(Remove.sites.not.full == F){Points.NA <- geom_point(inherit.aes = F, data = MT.AP, mapping = aes(x = Point, y = variable), color = "grey10", size = .3, na.rm = T)}
    Plot.compa.AP <- Plot.by.PT(MT.AP, My_colors =  c("#258721ff", "grey50"),  
                                My_lab = c("Arboreal Pollen (AP)", "Vegetation taxa"), Sam.tit = "Sample Type")
  
    MT.NAP <- MT[MT$Taxa %in% List.NAP,]
    MT.NAP$Taxa <- factor(unique(MT.NAP$Taxa))
    if(Remove.sites.not.full == F){Points.NA <- geom_point(inherit.aes = F, data = MT.NAP, mapping = aes(x = Point, y = variable), color = "grey10", size = .3, na.rm = T)}
    Plot.compa.NAP <- Plot.by.PT(MT.NAP, My_colors = c("#f27041ff", "grey50"), My_lab = c("Non-Arboreal Pollen (NAP)", ""), 
                                 Tit = "Pollen & vegetation relative abundances (%)")
    
    Np1 = sum(Max.by.tax[names(Max.by.tax) %in% levels(MT.AP$Taxa)])
    Np2 = sum(Max.by.tax[names(Max.by.tax) %in% levels(MT.NAP$Taxa)])
    
    if(ShP == T){
      MT.ShP <- MT[MT$Taxa %in% List.ShP,]
      MT.ShP$Taxa <- factor(unique(MT.ShP$Taxa))
      if(Remove.sites.not.full == F){Points.NA <- geom_point(inherit.aes = F, data = MT.ShP, mapping = aes(x = Point, y = variable), color = "grey10", size = .3, na.rm = T)}
      Plot.compa.ShP <- Plot.by.PT(MT.ShP, 
                                   My_colors = c("#d3d269ff", "grey50"), My_lab = c("Shrubs Pollen (ShP)", ""))
      
      Np3 = sum(Max.by.tax[names(Max.by.tax) %in% levels(MT.ShP$Taxa)])
      Plot.compa <- Plot.compa.AP + Plot.compa.NAP + Plot.compa.ShP + 
        plot_layout(widths = c(Np1, Np2, Np3), guides = "collect", axes = "collect") & 
        theme(legend.position = Display.legend, legend.justification = "right")
      }
    else{Plot.compa <- (Plot.compa.AP | Plot.compa.NAP) + plot_layout(widths = c(Np1,Np2), guides = "collect") & theme(legend.position = Display.legend)}
    }
  
  #### Add CONISS to plot ####
  if(is.null(CONISS.t) == F){
    Theme_CONISS <- theme(axis.text.y.left = element_blank(),
                          panel.background=element_blank(),
                          plot.background = element_blank(),
                          axis.title = element_text(size = 9),
                          axis.line.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),                   # fait apparaitre seulement l'axe des y
                          axis.ticks.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
                          axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1),
                          axis.ticks.y.left = element_blank(), plot.margin = unit(c(0,0,0,0), "lines"))
    
    Pclust <- ggplot() +
      layer_dendrogram(MT_clust, aes(y = variable), colour = "grey60", size = .1) +
      scale_y_discrete(limits = order) +
      scale_x_continuous(expand = c(0,0)) +
      labs(x = "CONISS", y = NULL) + Theme_CONISS
      
    if(Print.cluster.name == T){
      Rect_core <- geom_rect(data = CONISS.bound, inherit.aes = F,
                             mapping = aes(xmin = 0, xmax = 2, ymin = Min, ymax = Max, fill = MY_col),
                             alpha=0.6, color = "grey10", linewidth = .5, show.legend = F, na.rm = T
                             )
      
      Text_core <- geom_text(data = CONISS.bound, inherit.aes = F,
                             aes(x = 1, y = (Min+Max)/2, label = hclust_zone),
                             vjust = 0.5, hjust = 0.5, color = "grey10" , na.rm = T, size = CONISS.Lab.size)
      
      pLab <- ggplot(MT, aes(x = value, y = variable))+
        scale_y_continuous(limits = c(0,length(order)), expand = c(0,0), name = NULL)+
        scale_fill_manual(values = c("A" = "white", "B" = "grey70"))+# guides(fill = "none")+
        Rect_core + Text_core + 
        theme(panel.background=element_blank(), plot.margin = unit(c(0,0,0,0), "lines"),
              plot.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), 
              axis.title = element_blank(), axis.line = element_blank(),legend.position = "none")
      
      Pclust <- pLab + Pclust + plot_layout(widths = c(40,60))
      }
    
    if(AP.NAP.ShP.col == F | is.null(Pollen.index) == T){Plot.compa <- ((Plot.compa + Pclust) + plot_layout(widths = c(37/40, 3/40)))}
    else{
      if(ShP == T){
        NpC = (Np1+Np2+Np3)*0.07
        Plot.compa <- (Plot.compa + Pclust + plot_layout(widths = c(Np1, Np2, Np3, NpC)))}
      else{Plot.compa <- (Plot.compa + Pclust + plot_layout(widths = c(Np1, Np2, 60)))}
  }}
  
  #### Add AP / NAP ####
  if(AP.NAP == T & is.null(Pollen.index) == F){
    values.bi2 = c("NAP" = "#f27041ff","AP" = "#6db38fff", "ShP" = "#cfce90ff")
    
    if(AP.NAP.ShP.col == T & is.null(Pollen.index) == F){
      My.po = "none"
    }
    else{My.po = guide_legend(title.position = "top")}  
    
    PAP.NAP  <-  ggplot(MAP.NAP, mapping = aes(x = Site, y = value, fill = variable, group = variable))+
      geom_area(fill = "grey80", color = NA, size = .2)+ 
      geom_area(aes(alpha = Type), color = "grey30", size = .2)+ 
      geom_hline(yintercept = 0, color = "grey30", size = .2)+
      coord_flip() + Line.coniss2 +
      labs(y = My_title, x = NULL)+
      scale_x_discrete(limits = order)+
      scale_fill_manual(values = values.bi2, name = "Pollen types")+
      facet_grid(cols = vars(Type), scales = "free")+
      guides(fill = My.po, alpha = "none") +
      scale_alpha_manual(values = c("Pollen" = 1, "Vegetation" = 0.4))+
      theme(axis.text.y = Site.name3, 
            panel.background=element_blank(),
            plot.background = element_blank(), 
            axis.title = element_text(size=9),
            strip.text = element_text(angle = 45, size = Sp.lab.size, hjust = 0, vjust = 0),
            axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1),
            strip.background = element_blank(),
            legend.justification = "right", legend.position = Display.legend,
            panel.grid = element_blank(), legend.margin=margin(0,0,0,0),
            legend.key = element_blank(), axis.line.y = Vertical.axe,
            axis.line.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
            axis.ticks.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
            axis.ticks.y = My_ticks3, panel.spacing = unit(0.05, "lines"),
            plot.margin = unit(c(0,0,0,0), "lines")#,
            )
      Plot.compa <- (PAP.NAP + Plot.compa) + plot_layout(widths = c(3/30, 23/25), guides = "collect") #& theme(legend.position = Display.legend)
  }
  
  #### Color ecosystems ####
  values.bi = c("Light taiga" = "#1874CD",
                "Dark taiga" = "#658D94",
                "Steppe-desert" = "#DD5925",
                "Desert-steppe" = "#DD5925",
                "Desert" = "#CD2626",
                "Steppe" = "#EE8D25",
                "Alpine meadow" = "#FFC125",
                "Steppe" = "#1874CD",
                "MAP" = "#ADD8E6", "AI" = "#ADD8E6",
                "MAAT" = "#F08080",
                "Altitude" = "#CDBA96",
                "Desert" = "firebrick3", 
                "Desert-steppe" = "darkorange", 
                "Steppe" = "goldenrod1",
                "Forest-steppe" = "#38A700",
                "Riparian forest" = "darkgreen",
                "Alpine steppe" = "dodgerblue3",
                "Anthropic" = "grey10",
                "Steppe-forest" = "#B2A75C",
                "Forest-steppe" = "#B2A75C",
                "Cold desert" ="#6128c6", 
                "Tugai" = "#b77bb6", 
                "Desert" = "#ae1616", 
                "Steppe" = "darkorange", 
                "Desert-steppe" = "#efd01b", 
                "Forest-steppe" = "#42c500", 
                "Alpine steppe" = "#366a4b", 
                "Riparian forest" = "dodgerblue3", 
                "Anthropic" = "grey20"
  )
  if(is.null(Eco.col) == F & is.null(Meco) == F){
    Eco.col <- Eco.col[unique(Meco$Ecosystem)]
    my_col <- scale_color_manual(values = Eco.col)
    
    if(is.null(Eco.lab) ==F){
      my_col <- scale_color_manual(values = Eco.col, labels = Eco.lab, name = "Vegetation-types")
    }
  }
  else{
    my_col <- scale_color_manual(values = values.bi, name = "Ecosystems")
  }
  
  #### Add Abiot plots ####
  if(is.null(Abiot.plot) == F){
    Mclim <- subset(Mclim, select = Abiot.plot)
    Mclim[Mclim==""]<-NA
    Zero.lin <- data.frame(hline = sapply(Mclim, min, na.rm = TRUE))
    
    Zero.lin$variable<- row.names(Zero.lin)
    To.zero <- c("MAP", "Altitude")
    Zero.lin$hline[which(Zero.lin$variable %in% To.zero)] <- 0
    Mclim$Site <- row.names(Mclim)
    Mclim <- Mclim[complete.cases(Mclim),]
    Mclim <- Mclim[match(order, Mclim$Site),]
    Mclim <- melt(Mclim, id = c("Site"))
    Mclim$MinVal <- Zero.lin$hline[match(Mclim$variable, Zero.lin$variable)]
    
    New.lab <- c("MAAT" = "MAAT", "MAP" = "MAP~(mm.yr^-1)", "Altitude" = "Altitude", "AI" = "AI")
    My.unit <- c("MAAT" = "°C", "MAP" = NA, "Altitude" = "m a.s.l.", "AI" = NA)
    Pclim  <-  ggplot(Mclim, mapping = aes(x = Site, y = value, fill = variable, group = variable))+
      geom_line(color = "grey10")+ coord_flip() +
      geom_ribbon(aes(ymin = MinVal, ymax = value), position = "identity") +
      geom_point(size = .5, color = "grey30")+
      facet_grid(cols = vars(variable), scales = "free")+
      tidypaleo::facet_geochem_gridh(vars(variable), renamers = New.lab, units = My.unit, default_units = "", rotate_axis_labels = 45) +
      labs(y = "Climate parameters", x = NULL)+
      scale_x_discrete(limits = order)+
      scale_y_continuous(expand = c(0,0))+
      guides(fill = "none")+ Line.coniss2 +
      scale_fill_manual(values = values.bi, name = NULL)+
      geom_hline(data = Zero.lin, aes(yintercept = hline), color = "grey30", size = .2)+
      theme(axis.text.y = Site.name2, 
            panel.background=element_blank(),
            plot.background = element_blank(), 
            strip.text = element_text(angle = 45, size = Sp.lab.size, hjust = 0, vjust = 0),
            axis.text.x = element_text(size = 5),
            strip.background = element_blank(), legend.margin=margin(0,0,0,0),
            axis.title = element_text(size=9),
            legend.position = "none", panel.grid = element_blank(),
            legend.key = element_blank(), panel.spacing = unit(0.2, "lines"),
            axis.line = element_line(lineend = "butt", color = "grey70", linewidth = .1),
            axis.ticks.x = element_line(lineend = "butt", color = "grey70", linewidth = .1),
            axis.ticks.y = My_ticks2, strip.clip = "off",
            plot.margin = unit(c(0,0,0,0), "lines"))
    Plot.compa <- Pclim + Plot.compa + plot_layout(widths = c(4/30, 26/30), guides = "collect") & theme(legend.position = Display.legend)
  }
  
  #### Add Ecosystem ####
  if(is.null(Meco) == F & Show.site.eco == T){
    Meco <- Meco['Ecosystem'] 
    Meco[Meco==""]<-NA
    Meco$variable <- row.names(Meco)
    Meco <- Meco[complete.cases(Meco),]
    Meco <- Meco[match(order, Meco$variable),]
    Peco  <-  ggplot(Meco, mapping = aes(x = 1, y = variable, colour = Ecosystem))+
      geom_point(size = Eco.dot.size, shape = 16, na.rm = T)+
      scale_y_discrete(limits = order) +
      scale_x_continuous(expand = c(0,0))+
      guides(colour = guide_legend(title.position = "top", nrow = Leg.nb.row)) +
      my_col +
      theme(axis.text.y = Site.name4, 
            strip.clip = "off",
            axis.text.x = element_blank(),
            axis.title = element_blank(), 
            panel.background=element_blank(), panel.border = element_blank(),
            plot.background = element_blank(),
            legend.justification = "left", legend.margin=margin(0,0,0,0),
            legend.direction = "horizontal", legend.text = element_markdown(),
            legend.key = element_blank(), legend.key.spacing = unit(0.01, 'line'),
            panel.grid = element_blank(),
            plot.margin = unit(c(0,0,0,0), "lines"),
            axis.ticks = element_blank()
      )
    
    Plot.compa <- Peco + Plot.compa + plot_layout(widths = c(1/100, 24/25)) & 
      theme(legend.position = Display.legend, legend.margin=margin(0,0,0,0),
            legend.box.margin=margin(0,0,0,0))
  }
  #### Save plot and export ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){
      ggsave(Plot.compa, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
    else{ggsave(Save.plot)}}
  
  
}

Plot.twinspan <- function(MV, Max.species, Site.lab.size, Meco = NULL, Eco.dot.size = 2, Select.species = "both", Annot = NULL,
                          Symbol.path = NULL, Eco.col = NULL, Nb.cluster = 1, Cut.levels = NULL, Save.Indicator = NULL, Multipage.nrow = NULL,
                          Add.dendro = F, Show.indicators = T, Return.plot = F, Sp.lab.size, H, W, Save.plot = NULL){
  #### Initial settings ####  
  library(twinspan)
  # detach("package:vegan", unload=TRUE)
  # unloadNamespace("twinspan")
  # library(twinspanR)
  library(RColorBrewer)
  if(missing(Max.species)){Max.species = nrow(MV)}
  if(missing(Sp.lab.size)){Sp.lab.size = NULL}
  if(missing(Site.lab.size)){Site.lab.size = NULL}
  
  #### Twinspan calculation ####  
  Tw <- data.frame(t(MV))
  
  if(is.null(Cut.levels) == F){Tw <- twinspan(Tw, cutlevels = Cut.levels)}
  else{Tw <- twinspan(Tw)}
  
  Ind.spe.sum <- summary(Tw)
  if(Select.species == "both"){Tab <- twintable(Tw, maxspp = Max.species)}
  else{Tab <- twintable(Tw, goodspecies = Select.species)}
  
  #### Clean Species names ####
  Species.list <- gsub(".*([[:upper:]])", "\\1", colnames(Tab))
  Order <- gsub("([[:upper:]]).*", "", colnames(Tab))
  Order <- gsub("\\s", "", Order)
  Site.list <- gsub(".*\\s", "", row.names(Tab))
  Species.list <- gsub("\\.", " ", Species.list)
  Species.list <- gsub("spp ", "spp.", Species.list)
  
  #### Find list of indicator taxa ####
  inds <- abs(Tw$quadrat$indicators)
  inds <- inds[inds > 0]
  inds <- Tw$quadrat$pseudo2species[inds]
  inds <- sort(unique(inds))
  List.ind <- Tw$species$labels[inds]
  List.ind <- gsub(".*\\s", "", List.ind)
  List.ind <- gsub("\\.", " ", List.ind)
  List.ind <- gsub("spp ", "spp.", List.ind)
  Match.ind <- which(Species.list %in% List.ind)
  
  #### Melt ####
  Tab <- as.data.frame(t(Tab))
  names(Tab) <- Site.list
  Tab <- cbind(ID = seq(1:nrow(Tab)), Order = Order, Species = Species.list, Tab)
  Tab.ind <- Tab[Match.ind,]
  Tw.m <- melt(Tab, id = c("ID", "Order", "Species"))
  Tw.ind.m <- melt(Tab.ind, id = c("ID", "Order", "Species"))
  
  #### Graphical settings (labels) ####  
  My.labs <- Tw.m$Species
  My.labs[!grepl("eae", My.labs)] <- paste0("<i>", My.labs[!grepl("eae", My.labs)], "</i>")
  My.labs[grepl("spp.", My.labs)] <- paste0("<i>", gsub("spp.", "", My.labs[grepl("spp.", My.labs)]), "</i>", "spp.")
  My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)] <- paste0("<i>", gsub(" type", "", My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)]), "</i>", "-type")
  My.labs[grepl("unidentified", My.labs)] <- paste0("<i>", gsub("unidentified", "", My.labs[grepl("unidentified", My.labs)]), "</i>", "unidentified")
  My.labs <- gsub("<i><i>", "<i>", My.labs)
  My.labs <- gsub("</i></i>", "</i>", My.labs)
  Tw.m$Species <- factor(Tw.m$Species, levels = Species.list)
  
  My_blank_theme <- theme(
    legend.position = "none", axis.line = element_line(colour = "grey70", linewidth = .4), panel.border = element_rect(fill = NA, colour = "grey70", linewidth = .4),
    axis.text.y = element_markdown(size = Sp.lab.size), axis.ticks = element_line(colour = "grey70", linewidth = .4),
    axis.text.x = element_text(size = Site.lab.size),
    panel.grid = element_blank(), plot.margin = unit(c(0,0,0,0), "cm"),
    axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), plot.background = element_blank()
  )
  
  My_blank_theme2 <- theme(
    legend.position = "none", panel.background = element_blank(),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.clip = "off",  
    axis.text.y = element_markdown(size = Sp.lab.size), axis.ticks.y = element_line(colour = "grey70", linewidth = .4),
    axis.title.x = element_blank(), axis.line.y = element_line(colour = "grey70", linewidth = .4),
    axis.title.y = element_blank(),  plot.background = element_blank(), plot.margin = unit(c(0,0,0.2,0), "cm")
  )
   
   if(is.null(Eco.col) == F & is.null(Meco) == F){
     Eco.col <- Eco.col[unique(Meco$Ecosystem)]
     my_col <- scale_color_manual(values = Eco.col)
    }
  else{my_col <- NULL}
  
  if(is.null(Annot) == F){Annot <- annotate("text", x = length(Site.list), y = length(Species.list), label = Annot, size = 5, hjust = 1, vjust = 1)}
  
  
  #### Show indicators ####
  if(Show.indicators == T){
    Indicators <- geom_tile(data = Tw.ind.m, aes(color = as.factor(value)), width = .95, height = .95, linewidth = .4, fill = NA, alpha = .5)
    Indicators.col <- scale_color_manual(values = c("white", rep("black",(length(unique(Tw.ind.m$value))-1))))
    List.ind2 <- paste0("<b><i>", List.ind, "</i>")
    List.ind2[grepl("spp.", List.ind)] <- paste0("<b><i>", gsub("spp.", "", List.ind[grepl("spp.", List.ind)]), "</i>", "spp.")
    List.ind2[grepl("type", List.ind)] <- paste0("<b><i>", gsub(" type", "", List.ind[grepl("type", List.ind)]), "</i>", "-type")
    List.ind2[grepl("unidentified", List.ind)] <- paste0("<b><i>", gsub("unidentified", "", List.ind[grepl("unidentified", List.ind)]), "</i>", "unidentified")
    List.ind2 <- paste0(List.ind2, "</b>")
    List.ind[grepl("spp.", List.ind)] <- paste0("<i>", gsub("spp.", "", List.ind[grepl("spp.", List.ind)]), "</i>", "spp.")
    List.ind[grepl("type", List.ind)] <- paste0("<i>", gsub(" type", "", List.ind[grepl("type", List.ind)]), "</i>", "-type")
    List.ind[grepl("unidentified", List.ind)] <- paste0("<i>", gsub("unidentified", "", List.ind[grepl("unidentified", List.ind)]), "</i>", "unidentified")
    
    for(i in 1:length(List.ind)){My.labs[grepl(List.ind[i], My.labs)] <- List.ind2[i]}
    }
  else{
    Indicators <- NULL
    Indicators.col <- NULL
    }
  
  if(is.null(Save.Indicator) == F){saveRDS(Tab.ind$Species, Save.Indicator)}
  
  #### Add dendro ####
  if(Add.dendro == T){
    #### Library and settings ####
    library(ggdendro)
    library(ggnewscale)
    DT <- as.dendrogram(Tw)
    data <- dendro_data(DT, type = "rectangle")
    
    #### Ajout des points ####
    if(is.null(Meco) == F){
      data$label <- data.frame(data$label)
      data$label <- cbind(data$label, Ecosystem = Meco$Ecosystem[match(data$label$label, row.names(Meco))])
      Col.tab <- unlist(Eco.col[match(data$label$Ecosystem, names(Eco.col))], use.names = F)
      My_dots <- geom_point(data = data$label, aes(x = x, y = y, color = Ecosystem), size = Eco.dot.size, na.rm = T)
      My_txt <- NULL
      }
    else{My_dots <- NULL}
    
    
    #### Plots ####
    if(Nb.cluster == 1){
      pden <- ggplot() + 
        geom_segment(data = segment(data), aes(x = x, y = y, xend = xend, yend = yend), linewidth = .2) + 
        scale_x_discrete(expand = c(0.02, 0.02))+
        scale_y_discrete(expand = c(0.02, 0.02))+
        My_dots + my_col + My_txt + My_blank_theme2
      }
    else{
      library(magrittr)
      library(dendextend)
      dend <- as.dendrogram(Tw) %>%
        set("branches_k_color", k = Nb.cluster) %>%
        # set("branches_k_color", k = 7) %>%
        # set("branches_k_color", value = c("grey10", "blue"), k = 2) %>%
        set("branches_lwd", 0.35) %>%
        set("labels_cex", 0) %>%
        set("leaves_pch", 19) %>% 
        set("leaves_cex", Eco.dot.size) %>%
        set("leaves_col", Col.tab) #%>%
      
      pden <- ggplot(as.ggdend(dend), horiz = F, theme = NULL) + 
        scale_x_discrete(expand = c(0.01, 0.01)) +
        coord_cartesian(clip = 'off') +
        My_blank_theme2
        }  
    }
   
  #### Add symbol ####
  if(is.null(Symbol.path) == F){
    library(cowplot)
    library(png)
    library(grid)
    g <- rasterGrob(readPNG(Symbol.path), interpolate = T)
    pLog <- ggplot()+annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + theme_nothing()
  }
  
  #### Plots ####
  if(is.null(Multipage.nrow) == F){
    Nrow <- Multipage.nrow
    if(is.null(Save.plot) == F){
      Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
      dir.create(file.path(Path.to.create), showWarnings = FALSE)
      if(is.null(W) == F & is.null(H) == F){
        pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
      else{pdf(file = Save.plot)}}
    }
  else{Nrow <- nlevels(Tw.m$Species)}
  
  for(i in 1:(round(nlevels(Tw.m$Species)/Nrow, digits = 0))-1){
    if((Nrow*(i+1)-1) < nlevels(Tw.m$Species)){Nsup <- (Nrow*(i+1))}
    else{Nsup <- nlevels(Tw.m$Species)}
    Species.pool <- rev(levels(Tw.m$Species))
    Keep.tax.i <- Species.pool[(Nrow*i):Nsup]
    
    Tw.m.i <- Tw.m[Tw.m$Species %in% Keep.tax.i,]
    Tw.ind.m.i <- Tw.ind.m[Tw.ind.m$Species %in% Keep.tax.i,]
    
    My.labs.i <- My.labs[which(Tw.m$Species %in% Keep.tax.i)]
    if(Show.indicators == T){
      Indicators <- geom_tile(data = Tw.ind.m.i, aes(color = as.factor(value)), width = .95, height = .95, linewidth = .4, fill = NA, alpha = .5)
    }
    
    pTW <- ggplot(Tw.m.i, aes(x = variable, y = Species)) +
      geom_tile(aes(fill = value)) +
      Indicators + Indicators.col +
      scale_fill_gradient(low = "white", high = "#974401ff")+ #CD7024
      scale_x_discrete(position = "top", expand = c(0.01, 0.01), guide = guide_axis(angle = 90)) +
      scale_y_discrete(label = My.labs.i) + Annot +
      My_blank_theme
    
    if((i+1)>1){print(pTW)}
    if(Add.dendro == T & is.null(Symbol.path) == T){pTW <- pden / pTW / plot_layout(nrow = 2, heights = c(1,10))}
    if(Add.dendro == T & is.null(Symbol.path) == F){pTW <- (pden + inset_element(pLog, left = -0.2, bottom = 0.08, right = 0, top = .92, on_top = T)) / pTW / plot_layout(nrow = 2, heights = c(1,10))}
    if(Add.dendro == F & is.null(Symbol.path) == F){pTW <- pTW + inset_element(pLog, left = 0, bottom = 0, right = 0, top = 1, on_top = T)}
    if((i+1)==1){print(pTW)}
    }
  
  if(is.null(Multipage.nrow) == F){dev.off()}
  else{
    if(is.null(Save.plot) == F){ggsave(pTW, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
  }
  
  if(Return.plot == T){return(pTW)}
  else{return(Species.list)}
  }

Veget.piechart <- function(MV, Select.plot, Plotly, Plotly.all, H, W, Save.plot){
  #### Settings ####
  if(missing(Select.plot)){Select.plot = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Plotly)){Plotly = T}
  if(missing(Plotly.all)){Plotly.all = F}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  
 
  #### Select plots ####
  if(is.null(Select.plot) == F){
    MV <- MV[Select.plot]
    Row.to.keep <- which(rowSums(MV) > 0)
    MV$Species <- row.names(MV)
    MV <- data.frame(MV[Row.to.keep,])
  }
  
  #### Clean data ####
  MV$Species <- row.names(MV)
  MV <- melt(MV, id = "Species")
  MV$perc <- round(MV$value, digits = 0)
  MV$perc[MV$perc == 0] <- NA
  
  # Get the positions
  df2 <- MV %>% 
    mutate(csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  
  
  #### ggplot ####
  p <- ggplot(MV, aes(x = 1, y = value, fill = as.factor(Species))) +
    # geom_bar(width = 1, stat = "identity")+
    geom_col(width = 1, color = 1, linewidth = .1) +
    facet_wrap(vars(variable))+
    guides(fill = "none")+
    # coord_polar(theta = "y", start = 0)+
    coord_polar(theta = "y")+
    geom_text(aes(label = perc),
               position = position_stack(vjust = 0.5),
              na.rm = T) +
    # scale_y_continuous(breaks = df2$pos, labels = df$group) +
    # scale_y_continuous(breaks = MV$value, labels = as.factor(MV$Species)) +
    scale_y_continuous(breaks = df2$pos, labels = MV$Species) +
    theme_void()+
    theme(axis.title.x = element_blank(),
    #       axis.text.x  = element_blank(),
    #       axis.ticks.x = element_blank(),
    #       axis.title.y = element_blank(),
          # axis.text.y  = element_blank(),
          axis.text  = element_text(size = 5),
          axis.ticks.y = element_blank())
  
  #### Plotly ####
  if(Plotly == T){
    #### Libs ####
    library(plotly)
    library(htmlwidgets)
    
    #### Dir creat ####
    Save.plot.html <- gsub("pdf", "html", Save.plot)
    Keep.name <- gsub(".*\\/", "", Save.plot.html)
    Path.root <- paste(gsub(Keep.name, "", Save.plot.html), "HTML_files/", sep = "")
    if(file.exists(Path.root) == F){dir.create(Path.root)}
    Save.plot.html <- paste(Path.root, Keep.name, sep = "")
    
    print(Save.plot.html)
    
    #### Only 1 plot ####
    if(Plotly.all == F){

    # # Create fake data
    df <- data.frame(
      genre=c("Pop", "HipHop", "Latin", "Jazz"),
      values = c(33,22,25,20)
    )
    print(MV$variable)
    # 
    p1_ly <- plot_ly(data = MV, labels =~Species, values = ~value, type="pie") %>%
    # p1_ly <- plot_ly(data = MV, labels =~Species, values = ~value, color = ~variable, type="pie") %>%
      # subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
      layout(title = unique(MV$variable),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    # 
    # p1_ly <- ggplotly(p)
    # p1_ly <- p1_ly %>% layout(boxmode = "group", boxpoints = F)
    # options(warn = - 1) 
    saveWidget(p1_ly, file = Save.plot.html)}
    #### Multi plot ####
    else{
      List.cam <- unique(MV$variable)
      for(i in List.cam){
        Save.cam.i <- gsub("\\.html", paste("_", i, "\\.html", sep = ""), Save.plot.html)
        MV.i <- MV[MV$variable == i,]
        MV.i <- MV.i[MV.i$value != 0,]
        
        # MV.i$value <- log(MV.i$value + 0.01)
        # MV.i$value <- MV.i$value/sum(MV.i$value)
      
        p1_ly <- plot_ly(data = MV.i, labels =~Species, values = ~value, type="pie") %>%
          layout(title = unique(MV.i$variable),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        saveWidget(p1_ly, file = Save.cam.i)
      }
    }
  
  }
 
  
  #### Export ####
  print(p)
  if(is.null(Save.plot) == F){dev.off()}
}

LR.clim.veget <- function(MT, Meco, Keep.taxa, Keep.clim, 
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
  
  if(class(MT) == "list"){
    # print(names(MT)[1])
    }
  else{
    MT <- list(My_site = MT)
    Meco <- list(My_site = Meco)
  }
  #### Select taxa and clim param ####
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
    MT.i$Country <- names(MT)[i]
    
    MT.clean <- rbind(MT.clean, MT.i)
    
    }
  
  MT <- MT.clean
  # }if(class(MT) == "data.frame"){MT$Country <- "My-study"}
  
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
  if(R2.pos == "topright"){
    R2.y = "top"
    R2.x = "right"}
  if(R2.pos == "topleft"){
    R2.y = "top"
    R2.x = "left"}
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
  
  ggsave(pLR, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
}

LR.pol.veget <- function(MP, MV, Scale, Max_seuil, Keep.taxa, R2.size = 3.5,
                         R2.pos, Return.plot = F, H, W, Save.plot = NULL){
  #### Settings ####
  if(missing(Max_seuil)){Max_seuil = NULL}
  if(missing(R2.pos)){R2.pos = "bottomleft"}
  if(missing(Scale)){Scale = NULL}
  
  #### Correspondance and clean ####
  MP <- MP[, intersect(names(MP), names(MV))]
  MV <- MV[, intersect(names(MP), names(MV))]
  
  #### Fusion taxons rares #### 
  if(is.null(Max_seuil) == F){
    MP_max <- MP[0]
    MP_max[, "max"] <- apply(MP[,], 1, max)
    MP_dom <- MP[rowSums(MP_max)>Max_seuil,]
    MP_rare <- MP[rowSums(MP_max)<=Max_seuil,]
    MP_dom["Other" ,] <- colSums(MP_rare)
    MP <- data.frame(MP_dom)
    
    MV_rare <- MV[row.names(MV) %in% row.names(MP_rare),]
    MV_rare <- rbind(MV_rare, MV["Incertae sedis",])
    MV <- MV[!row.names(MV) %in% row.names(MP_rare),]
    MV <- MV[row.names(MV) !="Incertae sedis",]
    MV["Other" ,] <- colSums(MV_rare)
  }
  
  if(is.null(Keep.taxa) == F){
    MP <- MP[match(Keep.taxa,row.names(MP)),]
    MV <- MV[match(Keep.taxa,row.names(MV)),]
  }
  
  #### MP and MV melt ####
  MP$Taxa <- row.names(MP)
  MP <- reshape2::melt(MP, id = "Taxa")
  # MP$Type <- "Pollen"
  MV$Taxa <- row.names(MV)
  MV <- reshape2::melt(MV, id = "Taxa")
  # MV$Type <- "Vegetation"
  # MT <- rbind(MV, MP)
  
  #### Plot LR MP vs. MV ####
  # MP <- subset(MP, select = -c(Type))
  # MV <- subset(MV, select = -c(Type))
  MT <- left_join(MV, MP, by = c("Taxa", "variable"))
  names(MT)[c(3,4)] <- c("Vegetation", "Pollen")
  
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
  
  Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, color = "turquoise4", size = R2.size, small.r = F,
                         aes(label =  sprintf("%s*\", \"*%s" ,
                                              after_stat(rr.label),
                                              # after_stat(r.squared),
                                              after_stat(p.value.label)
                         )))
  
  #### Plot ####
  Plot.compa <- ggplot(MT, aes(x = Vegetation, y = Pollen)) +
    geom_point()+ 
    facet_wrap(vars(Taxa), scales = Scale)+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70")+
    geom_smooth(method = "lm", se = F, span = 1000, linetype = "dashed", linewidth = .5,
                formula = y ~ x)+
    Add.r2 + 
    labs(x = "Vegetation fractional abundance (%)", y = "Pollen fractional abundance (%)", fill = "Sample Type")+
    theme(
      axis.text.y = element_text(angle = -45, vjust = 1, hjust = 1, size = 8),
      axis.text.x = element_text(size = 7),
      axis.title = element_text(size=12),
      axis.line = element_blank(),
      axis.ticks = element_line(lineend = "butt", color = "grey30", size = .3),                   # fait apparaitre seulement l'axe des y
      panel.background=element_blank(),
      legend.key = element_blank(), panel.spacing = unit(0.2, "lines"),
      panel.border = element_rect(fill = NA, colour = "grey30"),
      strip.background = element_blank(), strip.placement = "outside",
      strip.text = element_text(size = 9)
    )
  
  #### Save plot and export ####
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(Plot.compa, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
    else{ggsave(Save.plot)}}

  if(Return.plot == T){return(Plot.compa)}
  else{return(MT)}
}

Davis.Index <- function(MP, MV, Add.group = F, Remove.taxa = NULL, Add.Rval = F, R.val.rel = NULL,
                        Bar.width = 1, Sp.lab.size = 10, Grp.Lab.size = 3.5, Leg.pos = "bottom",
                        Save.path = NULL, H = 500, W = 500, Save.plot = NULL){
  #### Import and settings ####
  library(ggtext) # Species en italic
  
  #### Binarization ####
  if(is.null(Remove.taxa) == F){
    MP <- MP[-Remove.taxa,]
    MV <- MV[-Remove.taxa,]
    }
  
  Keep.taxa.names <- row.names(MP)  
  NB.sites <- ncol(MP)
  MP <- data.frame(t(MP))
  MV <- data.frame(t(MV))
  
  MV_freq <- data.frame(ifelse(MV > 0,-1,100))
  MP_freq <- data.frame(ifelse(MP > 0,1,100))
  
  MV_freq <- cbind(Sites = row.names(MV_freq), MV_freq)
  MP_freq <- cbind(Sites = row.names(MP_freq), MP_freq)
  Mtot_freq <- rbind(MP_freq, MV_freq)
  Mtot_freq <- aggregate(Mtot_freq[-1], by = list(Mtot_freq[["Sites"]]), sum)
  row.names(Mtot_freq) <- Mtot_freq$Group.1
  Mtot_freq <- Mtot_freq[-1]
  
  #### Counting of binary values ####
  B0 <- apply(Mtot_freq, 2, function(x) length(which(x == 0)))
  P0 <- apply(Mtot_freq, 2, function(x) length(which(x == 101)))
  P1 <- apply(Mtot_freq, 2, function(x) length(which(x == 99)))
  Mfull <- rbind(P0, P1, B0) 
  Mfull <- data.frame(t(Mfull))
  
  #### Calculation of Davis index ####
  Mfull$A <- round(100 * Mfull$B0 / (Mfull$P0 + Mfull$P1 + Mfull$B0), digits = 0)
  Mfull$U <- round(100 * Mfull$P1 / (Mfull$P1 + Mfull$B0), digits = 0)
  Mfull$O <- round(100 * Mfull$P0 / (Mfull$P0 + Mfull$B0), digits = 0)
  row.names(Mfull) <- Keep.taxa.names
  Mfull[is.na(Mfull)] <- 0
  Mfull[["Davis"]] <- NA
  Mfull$Davis[Mfull$A >= 65] <- "Strongly-associated taxa"
  Mfull$Davis[Mfull$A < 65] <- "Associated \ntaxa"
  Mfull$Davis[Mfull$A < 50] <- "Weakly Associated taxa"
  Mfull$Davis[Mfull$A == 0 & Mfull$O > 50] <- "Unasso. \ntaxa"
  Mfull$Davis[Mfull$U == 0 & Mfull$A < 50] <- "Over-represented taxa"
  Mfull <- Mfull[rowSums(Mfull[c(1:3)]) >= 0.10*NB.sites,]
  Mfull <- Mfull[order(Mfull$O, decreasing = T),]
  Mfull <- Mfull[order(Mfull$A),]
  Mfull$Davis <- factor(Mfull$Davis, levels = c("Over-represented taxa", "Strongly-associated taxa", "Associated \ntaxa", "Weakly Associated taxa", "Unasso. \ntaxa"))
  Mfull <- Mfull[order(Mfull$Davis),]
  
  #### Melting and ggplot formatage ####
  MD <- Mfull[4:7]
  MD <- MD[order(MD$A),]
  MD <- MD[order(MD$U),]
  MD <- MD[order(MD$Davis),]
  MD$Species <- row.names(MD)
  Species.list <- row.names(MD)
  Mgroup <- MD
  MD <- melt(MD, id = c("Davis", "Species"))
  
  #### Graphical settings ####
  My_blank_theme <- theme(
    legend.position = Leg.pos, legend.background = element_blank(), legend.margin = unit(c(0,0,0,0), "cm"), legend.text = element_text(size = Sp.lab.size),
    axis.line = element_line(colour = "grey70", linewidth = .4), panel.border = element_rect(fill = NA, colour = "grey70", linewidth = .4),
    axis.text.y = element_markdown(size = Sp.lab.size), axis.ticks = element_line(colour = "grey70", linewidth = .4),
    panel.grid = element_blank(), plot.margin = unit(c(0,0,0,0), "cm"),
    panel.background = element_blank(), plot.background = element_blank()
    )
  
  My_blank_theme2 <- My_blank_theme + theme(axis.line = element_blank(), panel.border = element_blank(),
                                            axis.title = element_blank(), axis.text.x = element_blank(),
                                            axis.ticks = element_blank(), axis.text.y = element_blank())
  
  My_blank_theme3 <- My_blank_theme + theme(axis.line.y = element_blank(), panel.border = element_blank(),
                                            axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  
  #### Graphical settings (labels) ####  
  My.labs <- MD$Species
  My.labs[!grepl("eae", My.labs)] <- paste0("<i>", My.labs[!grepl("eae", My.labs)], "</i>")
  My.labs[grepl("spp.", My.labs)] <- paste0("<i>", gsub("spp.", "", My.labs[grepl("spp.", My.labs)]), "</i>", "spp.")
  My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)] <- paste0("<i>", gsub("-type", "", My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)]), "</i>", "-t.")
  My.labs[grepl("unidentified", My.labs)] <- paste0("<i>", gsub("unidentified", "", My.labs[grepl("unidentified", My.labs)]), "</i>", "unidentified")
  My.labs <- gsub("<i><i>", "<i>", My.labs)
  My.labs <- gsub("</i></i>", "</i>", My.labs)
  My.labs <- gsub("-type", "-t.", My.labs)
  MD$Species <- factor(MD$Species, levels = Species.list)
  
  #### Plot graph ####
  pDI <- ggplot(MD, aes(x = value, y = Species, fill = variable))+
    geom_bar(position = "dodge", stat ="identity", width = Bar.width)+ 
    ylab(NULL)+
    xlab("Index values (%)")+ 
    scale_x_continuous(expand = c(0.01,2)) +
    scale_y_discrete(label = My.labs) + 
    # geom_vline(xintercept = 20, lty="dashed")+
    geom_vline(xintercept = 50, lty="dashed")+
    geom_vline(xintercept = 65, lty="dashed")+
    guides(fill = guide_legend(nrow = 2), colour = guide_legend(nrow = 2))+
    scale_fill_manual(values = c("A" = "#c9b503ff", "U" = "#4796E2", "O" = "#C93434"), name = NULL, labels = c("Associated (A)", "Under-associated (U)", "Over-associated (O)"))+ 
    My_blank_theme
  
  #### Add groups ####
  if(Add.group == T){
    #### Tab build ####
    Tab.grp <- data.frame(Up = NA, Down = NA)
    for(i in unique(Mgroup$Davis)){
      Up <- min(which(Mgroup$Davis == i))
      Down <- max(which(Mgroup$Davis == i))
      Tab.grp[i,1] <- Up#-0.4
      Tab.grp[i,2] <- Down+0.4
    }
    Tab.grp$x <- 1
    Tab.grp$y <- (Tab.grp$Up + Tab.grp$Down)/2
    Tab.grp$Lab <- row.names(Tab.grp)
    Tab.grp <- Tab.grp[-1,]
    Tab.grp$Lab <- gsub("\\s", "\n", Tab.grp$Lab)
    Tab.grp$Lab <- gsub("-", "\n", Tab.grp$Lab)
    Tab.grp$Lab <- gsub("\ntaxa", " taxa", Tab.grp$Lab)
    Line.DB <- Tab.grp$Up-0.5
    Line.DB <- Line.DB[-1]
    
    #### plot ####
    p2 <- ggplot(Tab.grp, aes(x = x, y = y))+
      geom_rect(mapping = aes(ymin = Up-0.4, ymax = Down, xmin = 0, xmax = 2), alpha=0.6, color = "grey30", linewidth = .5, show.legend = F, fill = "grey80", na.rm = T) +
      geom_text(aes(x = x, y = y, label = Lab), size = Grp.Lab.size, vjust = 0.5, hjust = 0.5, color = "grey20", angle = 270, na.rm = T, fontface = "bold") +
      scale_y_discrete(label = My.labs, expand = c(0,0)) + 
      My_blank_theme2
      
    pDI <- pDI + geom_hline(yintercept = Line.DB, lty="dashed", linewidth = .6, colour = "grey50")
    pDI <- pDI + p2 + plot_layout(widths = c(7,1))
  }
  
  #### Add R value (plot) ####
  if(Add.Rval == T){
    MP[MP == 0] <- NA
    MV[MV == 0] <- NA
    names(MP) <- Keep.taxa.names
    names(MV) <- Keep.taxa.names
    MP <- MP[match(Species.list,names(MP))]
    MV <- MV[match(Species.list,names(MV))]
    MV <- data.frame(t(MV))
    MP <- data.frame(t(MP))
    R.local <- round(MP/MV, digits = 1)
    
    if(is.null(R.val.rel) == F){
      Keep.name <- row.names(R.local)
      R.local <- data.frame(t(R.local))
      R.local <- R.local/R.local[[R.val.rel]]
      R.local <- data.frame(t(R.local))
      row.names(R.local) <- Keep.name
    }
    
    R.local$NB.val <- ncol(R.local)-rowSums(is.na(R.local))
    R.local$Mean <- rowMeans(R.local[-c(ncol(R.local)-1, ncol(R.local))], na.rm = T)
    R.local$Median <- apply(R.local, 1, median, na.rm=T)
    R.local$Species <- factor(row.names(R.local), levels(MD$Species), ordered = T)
    R.local$Seuil <- "Over." 
    R.local$Seuil[R.local$Mean < 1] <- "Under." 
    R.local$Seuil[R.local$Mean == 1] <- "OK" 
    print(R.local["NB.val"])
    
    R.local.m <- melt(R.local, id = c("NB.val", "Species", "Mean", "Median", "Seuil"))
    
    pBox <- ggplot(R.local.m, aes(y = Species, x = value, color = Seuil)) +
      scale_color_manual(values = c("Under." = "#3F6B3D", "Over." = "#D9A15E", "OK" = "grey10"), label = c( "Under." = "µ(R)<1",  "Over." = "µ(R)>1", "OK" = "STD"), name = NULL)+
      guides(fill = guide_legend(nrow = 2), colour = guide_legend(nrow = 2))+
      geom_boxplot(outliers = F, fill = "grey90") + My_blank_theme3 + xlab("R-values")
    pDI <- ((pDI) | pBox) + plot_layout(widths = c(2.5,1))
    }
  #### Export #### 
  if(is.null(Save.path) == F){write.csv(Mfull, file="Resultats/Indice_Davis.csv")}
  # if(is.null(Save.plot) == F){dev.off()}
  if(is.null(Save.plot) == F){ggsave(pDI, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
  
  return(Mfull)
  
}

Check.taxa.diff <- function(MP, MV, type = NULL, 
                            Display.message = T,
                            Auto.homogeneise = F){
  Diff.MV.MP <- setdiff(row.names(MP), row.names(MV))
  Diff.MP.MV <- setdiff(row.names(MV), row.names(MP))
  if(Display.message == T){
    if(is.null(type) == F){print(paste("**** Taxon differences", type, "****", sep = " "))}
    print(Diff.MV.MP[order(Diff.MV.MP)])
    print(Diff.MP.MV[order(Diff.MP.MV)])}
  
  if(Auto.homogeneise == T){
    print("Add missing rows for data.frame homogeneity.")
    MP[Diff.MP.MV,] <- 0
    MP <- MP[order(row.names(MP)),]
    MV[Diff.MV.MP,] <- 0
    MV <- MV[order(row.names(MV)),]
    return(list(MV, MP, Diff.MV.MP[order(Diff.MV.MP)], Diff.MP.MV[order(Diff.MP.MV)]))
  }
}

Map.samples <- function(Samples.list, Samples.pollen, Taxa.italic = F, Ncol = NULL, R2.size = 2.8,
                        Display.taxa, Label.tab, Insert.LR = NULL, Insert.loc = c(1,1,1,1),
                        Save.plot, H, W, Map.extend, Crop.samples, Limites, Check.DB.diff){
  #### Initial settings ####
  library(ggplot2)
  library(ggtext) # Species en italic
  library(sp)
  library(raster)
  library(maps)
  library(sf)
  library(gstat)
  library(stars)
  library(rgdal)
  if(missing(Check.DB.diff)){Check.DB.diff = F}
  if(missing(Save.plot)){Save.plot = F}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Limites)){Limites = NULL}
  if(missing(Label.tab)){Label.tab = NULL}
  if(missing(Samples.pollen)){Samples.pollen = NULL}
  if(missing(Display.taxa)){Display.taxa = NULL}
  if(missing(Map.extend)){Map.extend = "Eurasia"}
  if(missing(Crop.samples)){Crop.samples = F}
  Ligne.niveau = NULL
  My_river = NULL
  DEM.p = NULL
  
  #### Graph settings ####
  if(Map.extend == "Eurasia"){
    Map.Eurasia <- c("Mongolia", "Russia", "Spain", "France", "Italy", "Greece", "Germany", "Finland",
                     "Czech Republic", "Denmark", "Kosovo", "Vietnam", "Laos", "Japan",
                     "Latvia","Lithuania", "Estony", "Belarus", "Romania", "Bulgaria", "Hungary", "Austria", "Croatia",
                     "Albania", "Serbia", "Slovenia", "Slovakia", "Bosnia", "Montenegro", "UK", "Ireland", "Moldova", "Macedonia",
                     "Norway", "Sweden", "Turkey", "Belgium", "Uzbekistan", "Tajikistan", "Syria", "Israel", "Jordan",
                     "Kazakhstan","Turkmenistan", "Ukraine", "Poland", "Portugal", "Switzerland", "Kyrgyzstan", "Morocco",
                     "China", "Iran", "Armenia", "Georgia", "Afghanistan", "Iraq", "Azerbaijan")
    Eurasia_map <- map_data("world", region = Map.Eurasia)
    Map_area <- Eurasia_map}
  if(Map.extend == "ACA"){
    if(exists("ACA.bo.co") == F){
      Path.ACA.border.co = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Projets/ACA/Borders_ACA/Full/ACA_border.shp"
      ACA.bo.co = readOGR(Path.ACA.border.co)
      proj4string(ACA.bo.co) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      ACA.bo.co.proj = fortify(ACA.bo.co)
      }
    Map_area <- ACA.bo.co
    }
  if(Map.extend == "China"){Map_area <- map_data("world", region = c("China", "Mongolia", "Laos", "Vietnam"))}
  if(Map.extend == "TUSD"){Map_area <- map_data("world", region = c("Uzbekistan", "Tajikistan"))}
  if(Map.extend == "Golestan"){
    if(exists("Gol.bo.co.proj") == F){
      Path.Gol.border.co = "/home/lucas.dugerdil/Documents/Recherche/Stages/Golestan_Medhi/SIG/Golestan_border/Parc_nat_golestan.shp"
      Gol.bo.co = readOGR(Path.Gol.border.co)
      proj4string(Gol.bo.co) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      Gol.bo.co.proj = fortify(Gol.bo.co)
      
      Path.River = "/home/lucas.dugerdil/Documents/Recherche/Stages/Golestan_Medhi/SIG/Réseau_hydrographique/river.shp"
      River = readOGR(Path.River)
      River = fortify(River)
        
      Path.DEM = "/home/lucas.dugerdil/Documents/Recherche/Stages/Golestan_Medhi/SIG/DEM_Aster_Golestan/Clip_DEM_PN_Golestan/Courbes_niveaux/Courbe_niveau_Golestan/contour.shp"
      DEM = readOGR(Path.DEM)
      proj4string(DEM) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      DEM = fortify(DEM)
      
      # Path.DEM.low = "/home/lucas.dugerdil/Documents/Recherche/Stages/Golestan_Medhi/SIG/DEM_Aster_Golestan/Clip_DEM_PN_Golestan/Part1.tif"
      # DEM.low.ACA = raster(Path.DEM.low)
      # DEM.low.ACA.df <- as(DEM.low.ACA, "SpatialPixelsDataFrame")
      # DEM.low.ACA.df <- as.data.frame(DEM.low.ACA.df)
      # colnames(DEM.low.ACA.df) <- c("DEM.low", "x", "y")
      # DEM.p <- geom_tile(data = DEM.low.ACA.df, aes(x = x, y = y, fill = DEM.low, color = DEM.low), alpha = 1)
      My_river = geom_path(data=River, aes(x=long, y=lat), colour="royalblue", linetype = 1, size = .6)
      Ligne.niveau = geom_path(data=DEM, aes(x=long, y=lat, group = group),  colour="grey50", linetype = 2, size = .1)
    }
    Map_area <- Gol.bo.co
  }
  
  
  
  #### Data surface treatment ####
  Coord.list <- c("Lat", "Long")
  #Samples.list <-sapply(Coord.list, function(x) as.data.frame(Samples.list[grepl(x, names(Samples.list))]), simplify = F)
  Melt.data <- data.frame(NA, NA, NA)
  names(Melt.data) <- c("Lat", "Long", "DB")
  for(i in 1:length(Samples.list)){
    LAT <- Samples.list[[i]][grepl("Lat", names(Samples.list[[i]]))]
    LONG <- Samples.list[[i]][grepl("Long", names(Samples.list[[i]]))]
    A = data.frame(Lat = LAT, Long = LONG, DB = rep(names(Samples.list)[i], nrow(LAT)))
    Melt.data <- rbind(Melt.data, A)
    }
  Melt.data <- Melt.data[-1,]
  if(Check.DB.diff == F){
    myshapes <- NULL
    Surf.point <- geom_point(inherit.aes = F, data = Melt.data, mapping = aes(x = Long, y = Lat,  colour = DB), alpha = .4, size = 1, shape = 16)}
  else{
    Surf.point <- geom_point(inherit.aes = F, data = Melt.data, mapping = aes(x = Long, y = Lat,  shape = DB, colour = DB), alpha = .6, size = 1, stroke = .2)
    myshapes <- c(1, 3, 4, 0)
    names(myshapes) <- unique(Melt.data$DB)
    myshapes <- myshapes[1:length(unique(Melt.data$DB))]
  }
  
  #### Taxa map ####
  if(is.null(Samples.pollen)==F & is.null(Display.taxa)==F){
    Melt.data <- setNames(data.frame(NA, NA, NA,NA,NA, NA), c("Site", "Lat", "Long", "DB", "variable", "value"))
    for(i in 1:length(Samples.pollen)){
      Tax.names.keep <- row.names(Samples.pollen[[i]])
      Samples.pollen[[i]] <- data.frame(t(Samples.pollen[[i]]))
      names(Samples.pollen[[i]]) <- Tax.names.keep
      My.loc <- Samples.list[[names(Samples.pollen)[i]]]
      Take <- Samples.pollen[[i]][Display.taxa]
      Take$DB <-  names(Samples.pollen)[i]
      Take$Site <-  row.names(Take)
      My.loc <- My.loc[match(row.names(Take), row.names(My.loc)), Coord.list]
      Take <-  cbind(Take, My.loc)
      Take <- reshape::melt(Take, id = c("Lat", "Long", "DB", "Site"))
      Melt.data <- rbind(Melt.data, Take)
    }
    Melt.data <- na.omit(Melt.data)
    names(Melt.data)[5] <- "Taxa"
    
    #### Graphical settings (labels) ####  
    if(Taxa.italic == T){
      My.labs <- Melt.data$Taxa
      My.labs[!grepl("eae", My.labs)] <- paste0("<i>", My.labs[!grepl("eae", My.labs)], "</i>")
      My.labs[grepl("deciduous", My.labs)] <- paste0("<i>", gsub("deciduous", "", My.labs[grepl("deciduous", My.labs)]), "</i>", "deciduous")
      My.labs[grepl("spp.", My.labs)] <- paste0("<i>", gsub("spp.", "", My.labs[grepl("spp.", My.labs)]), "</i>", "spp.")
      My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)] <- paste0("<i>", gsub("type", "", My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)]), "</i>", "type")
      
      Melt.data$Taxa <- factor(My.labs)}
      
    if(is.null(Label.tab) == F){
      Melt.data$Taxa <- Label.tab[match(Melt.data$Taxa, Label.tab$Type_total), "Label"]
    }
    else{Taxa.lab <- NULL}
    
    Melt.data$value[Melt.data$value == 0] <- NA
    Empty.data <- Melt.data[is.na(Melt.data$value),]
    myshapes <- NULL
    Melt.data$DB <- gsub("\\.", " ", Melt.data$DB)
    Surf.point <- geom_point(inherit.aes = F, data = Melt.data, mapping = aes(x = Long, y = Lat,  colour = DB, size = value), alpha = .5, shape = 16)
    Empty.point <- geom_point(inherit.aes = F, data = Empty.data, mapping = aes(x = Long, y = Lat), colour = "grey10", size = 0.7, alpha = 1, shape = 16)
    Facet.taxa <- facet_wrap(vars(Taxa), ncol = Ncol)
  }
  else{
    Facet.taxa <- NULL
    Empty.point <- NULL}
  
  #print(Melt.data)
  #### Crop samples ####
  if(Crop.samples == T){
    crop.Melt.data <- Melt.data
    Map_area_projected <- st_as_sf(Map_area)
    coordinates(crop.Melt.data) <- ~ Long + Lat 
    proj4string(crop.Melt.data) <- CRS("+proj=longlat")
    crop.Melt.data <- spTransform(crop.Melt.data, crs(Map_area_projected))
    #print(crop.Melt.data)
    #print(over(crop.Melt.data, Map_area_projected))
  }
  
  #ACA.bo = fortify(ACA.bo, warning = F)
  #Map_area = fortify(Map_area, warning = F)
  
  #### Insert LR ####
  if(is.null(Insert.LR) == F){
    library(purrr)
    #### Graphical settings (labels) ####  
    if(Taxa.italic == T){
      My.labs <- Insert.LR$Taxa
      My.labs[!grepl("eae", My.labs)] <- paste0("<i>", My.labs[!grepl("eae", My.labs)], "</i>")
      My.labs[grepl("spp.", My.labs)] <- paste0("<i>", gsub("spp.", "", My.labs[grepl("spp.", My.labs)]), "</i>", "spp.")
      My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)] <- paste0("<i>", gsub("type", "", My.labs[grepl("type", My.labs) & !grepl("eae", My.labs)]), "</i>", "type")
      
      My.labs[grepl("deciduous", My.labs)] <- paste0("<i>", gsub("deciduous", "", My.labs[grepl("deciduous", My.labs)]), "</i>", "deciduous")
      
      Insert.LR$Taxa <- factor(My.labs)}
    
    #### Add R2 ####
    R2.y = "top"
    R2.x = "right"
    Add.r2 <- stat_poly_eq(label.y = R2.y, label.x = R2.x, color = "turquoise4", size = R2.size, small.r = F,
                           aes(label =  sprintf("%s*\", \"*%s" ,
                                                after_stat(rr.label),
                                                # after_stat(r.squared),
                                                after_stat(p.value.label)
                           )))
    
    #### Functions annotation #### 
    annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){
                          layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                                geom = ggplot2:::GeomCustomAnn,
                                inherit.aes = TRUE, params = list(grob = grob, 
                                                                  xmin = xmin, xmax = xmax, 
                                                                  ymin = ymin, ymax = ymax))}
    
    get_inset <- function(MT){
      Plot.compa <- ggplot(MT, aes(x = Vegetation, y = Pollen)) +
        geom_point(size = 1, color = "grey25")+ 
        Add.r2+
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70")+
        geom_smooth(method = "lm", se = F, span = 1000, linetype = "dashed", linewidth = .5,
                    formula = y ~ x)+
        theme(
          axis.text = element_text(size = 5),
          axis.title = element_blank(),              # AGE
          axis.line = element_blank(),
          axis.ticks = element_line(lineend = "butt", color = "grey30", size = .3),                   # fait apparaitre seulement l'axe des y
          panel.background=element_rect(fill = "grey95"),
          legend.key = element_blank(), panel.grid = element_blank(),
          panel.spacing = unit(0.2, "lines"),
          panel.border = element_rect(fill = NA, colour = "grey30"),
          strip.text.x = element_markdown(size = 12),
          axis.ticks.length = unit(0.4,'mm'),
          strip.background = element_blank(), plot.background = element_blank(),
          strip.placement = "inside",
          strip.text = element_text(size = 9)#,
        )
      return(Plot.compa)
    }
    
    #### Insert mapping #### 
    My_insert <- get_inset(Insert.LR)
    My_insert <- Insert.LR %>%
      split(f = .$Taxa) %>%
      purrr::map(~annotation_custom2(
        grob = ggplotGrob(get_inset(.) +
                            scale_x_continuous(limits=c(0,80))+
                            scale_y_continuous(limits=c(0,60))
                          ),
        data = data.frame(Taxa = unique(.$Taxa)),
        xmin = Insert.loc[1], xmax = Insert.loc[2],
        ymin = Insert.loc[3], ymax = Insert.loc[4]
        )
      )
  }
  else{My_insert = NULL}
  
  #### Plot ####
  pmap <- ggplot() + DEM.p +
    geom_polygon(data = Map_area, aes(x = long, y = lat, group = group), alpha = 1, fill = "grey97", color = "grey30", size = 0.3)+
    Ligne.niveau + My_river +
    Surf.point +
    Empty.point +
    # scale_colour_manual(values = c("#d6d81eff", "#32156eff"))+
    scale_colour_manual(values = c("#D9A15E", "#3F6B3D"), name = "Sampling method")+
    # scale_colour_manual(values = c("#944b24ff", "#3F6B3D"), name = "Sampling method")+
    # scale_fill_gradientn(colours = c("#1a9641", "#ccea8f", "#ffffc0", "#fed18a", "#f89957", "#ed6e43", "#ececec"), guide = "legend", name = "Elevation (m a.s.l.)")+
    # scale_color_gradientn(colours = c("#1a9641", "#ccea8f", "#ffffc0", "#fed18a", "#f89957", "#ed6e43", "#ececec"), guide = "legend", name = "Elevation (m a.s.l.)")+
    
    scale_x_continuous(name = "Longitude (°)", limits = c(Limites[1],Limites[2]))+
    scale_y_continuous(name = "Latitude (°)", limits = c(Limites[3],Limites[4]))+
    scale_shape_manual(values = myshapes) +
    scale_size_continuous(range = c(0.1, 10), name =  "Fractional Abundances (%)") +
    guides(colour = guide_legend(override.aes = list(size = 8, alpha = .5, nrow = 1)), size = guide_legend(nrow = 1))+
    coord_quickmap()+
    Facet.taxa +
    #### Theme ####
  theme(
    axis.line= element_line(colour = "grey", lineend = "butt"),
    axis.ticks = element_line(colour = "grey"),
    legend.key = element_blank(), axis.title = element_text(size = 12),
    legend.text = element_text(size = 12), legend.title = element_text(face = "bold", size = 14),
    panel.background = element_blank(), legend.direction = "horizontal", legend.background = element_blank(),
    panel.spacing = unit(0.2, "lines"), legend.position = "top",
    strip.text.x = element_markdown(size = 13), strip.placement = "inside", plot.background = element_blank(),
    strip.background = element_blank(),plot.margin=unit(c(0,0,0,0),"cm")
  ) + My_insert
  
  #### Export / save plot ####
  pmap
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(pmap, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm", limitsize = F)}
    else{ggsave(Save.plot)}}
  
  
  # return(ACA.bo.co)
  }


#### ARKHANGAI ####
Arkhangai = F
if(Arkhangai == T){
  #### Import & convert vegetation types ####
  Khangai.veget.10m <- Import.releves.10m.multitable(Veget.path = "Import/Mongolia/Vegetation/RPP_Arkhangai/Releve_veget", Csv.sep = ",", Plus.convert = 0.1)
  
  # Khangai.index <- Import.index("Import/Mongolia/Vegetation/RPP_Arkhangai/Indexes/Vegetation_taxa_index_autoadd.csv",
  #                               Khangai.veget.10m,
  #                               Csv.sep = ",",
  #                               Error.display = T,
  #                               Auto.add.miss = F)
  
  Khangai.index <- Import.index(Herbier = Herbier,
                              Herbier.country = c("Mongolie"),
                              MV = Khangai.veget.10m,
                              Csv.sep = ",", Auto.add.miss = T, Simplify.Vtype = F, Add.ubiquist = F,
                              Inerte.path = "Import/Mongolia/Vegetation/RPP_Arkhangai/Indexes/List_inerte.csv",
                              Save.path = "Import/Mongolia/Vegetation/RPP_Arkhangai/Indexes/Index_from_Arkhangai_Herbier_auto.csv",
                              Error.display = T)
  
  A = Search.releve(Khangai.veget.10m, Taxa = "Astragalus t1")
  # A = Search.releve(Khangai.veget.10m, Taxa = "Taraxacum officinalis")
  # 
  # Khangai.veget.10m <- Convert.releves.10m(Khangai.veget.10m,
  #                                          Remove.abiot = T,
  #                                          Tree.Herb = F,
  #                                          Taxa.name = T, Family.name = F, Pollen.type = F,
  #                                          Merge.releve = T, Error.display = T,
  #                                          Index = Khangai.index)
  # # 
  
  # library("readODS")
  # if(exists("Herbier") == F){Herbier <- data.frame(read_ods("/home/lucas.dugerdil/Documents/Recherche/Herbier/Herbier numérique/Index.ods"))}
  # Index.Mongolie <- Herbier[which(Herbier$Sampling_place == "Mon. Arkhangai"),]
  # Index.Mongolie <- Index.Mongolie[,c(2:5)]
  # Index.Mongolie$Species[is.na(Index.Mongolie$Species)] <- "spp."
  # Index.Mongolie$Species <- paste(Index.Mongolie$Genus, Index.Mongolie$Species, sep = " ")
  # Index.Mongolie <- Index.Mongolie[,-c(3)]
  
  # print(Index.Mongolie)

  #### Calculation vegetation matrix ####
  # Khangai.veget.10m.mean <- Mean.veget.model(MV = Khangai.veget.10m,
  #                                            Model = "SP",
  #                                            Releve.design = "Import/Mongolia/Vegetation/RPP_Arkhangai/Indexes/Vegetation_model.csv",
  #                                            Displot = F,
  #                                            Error.display = F,
  #                                            Csv.sep = ",")
  # print(colSums(Khangai.veget.10m.mean))
  
  #### Diagramme veget #####
  Diag.veg = F
  if(Diag.veg == T){
    par(mfrow=c(1,1))
    
    Plot.Khangai <- Hist.veget(MV = Khangai.veget.10m.mean,
                               Ordin.path = "Import/Mongolia/Site/RPP_Arkhangai/RPP_Arkhan_surf_clim.csv",
                               Save.plot = "Figures/Mongolia/Vegetation/RPP_Arkhangai/Hist_veget.pdf",
                               Index = Khangai.index,
                               Csv.sep = ",",
                               Max_seuil = 1,
                               AP.NAP = F, H = 800, W = 1200,
                               Sort = "Ordination2"  # Altitude, MAP, MAAT, Latitude, Longitude, Ordination
      )
    }
    
  #### PCA / RDA ####
  PCA.disp = F
  if(PCA.disp == T){
    H = 700
    W = 1200
    Save.plot = "Figures/Mongolia/Vegetation/RPP_Arkhangai/PCA_RDA_RPP_Arkhangai.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,2))
    PCA.Khangai <- PCA.veget(data.frame(t(Plot.Khangai)),               # Import Matrice pour la PCA
              Cluster.path = "Import/Mongolia/Site/RPP_Arkhangai/RPP_Arkhan_surf_clim.csv", 
              Csv.sep =",",
              transp_OK = F,                         # Log trans (T), (F) sinon
              Scale.PCA = ,                         # 1 or 2
              Leg.pos = "bottomleft",
              Site.name = "Arkhangai, Mongolia")     # Nom du site

    RDA.Khangai <- RDA.veget(data.frame(t(Plot.Khangai)),               # Import Matrice pour la PCA
              MClim = "Import/Mongolia/Site/RPP_Arkhangai/RPP_Arkhan_surf_clim.csv",
              Choose.clim = c("Altitude", "MAAT", "MAP"),#, "Latitude", "Longitude"),
              Cluster = c("Ecosystem"),
              Csv.sep =",",
              transp_OK = F,                         # Log trans (T), (F) sinon
              Scale.RDA = 3,                         # 1 or 2
              Leg.pos = "topright",
              Site.name = "Arkhangai, Mongolia")     # Nom du site
    
    
    dev.off()
    par(mfrow=c(1,1))
    }
  
  
  
  }

#### MONGOLIA PART1 ####
Mong.part1 = F
if(Mong.part1 == T){
  #### Calculation veget matrix ####
  Calculations = F
  if(Calculations == T){
    Mong.veget.10m.P1 <- Import.releves.10m.multitable(Veget.path = "Import/Mongolia/Vegetation/AB2016/Releve_veget", Csv.sep = ",", Plus.convert = 0.1)
    Mong.index.P1 <- Import.index("Import/Mongolia/Vegetation/AB2016/Indexes/Veget_taxa_index_AB16.csv",
                                  Mong.veget.10m.P1,
                                  Csv.sep = ",", Save.path = "Import/Mongolia/Vegetation/AB2016/Indexes/Veget_taxa_index_AB16_V2.csv",
                                  Error.display = T)
    
    Mong.veget.10m.P1 <- Convert.releves.10m(MV = Mong.veget.10m.P1,
                                             Remove.abiot = T,
                                             Tree.Herb = F,
                                             Taxa.name = T, Family.name = F, Pollen.type = F,
                                             Merge.releve = F, Error.display = F,
                                             Index = Mong.index.P1)
  
    Mong.veget.10m.P1.mean <- Mean.veget.model(MV = Mong.veget.10m.P1,
                                               Model = "SP",
                                               Releve.design = "Import/Mongolia/Vegetation/AB2016/Indexes/Veget_model_AB16.csv",
                                               Displot = F,
                                               Error.display = F,
                                               Remove.uncomplete = F,
                                               Csv.sep = ",")
    
    Add.Siberia = T
    if(Add.Siberia == T){
      Sib.veget.10m.P1 <- Import.releves.10m.multitable(Veget.path = "Import/Russia/Vegetation/AB2016/Releve_veget", Csv.sep = ",", Plus.convert = 0.1)
      A = Search.releve(Mong.veget.10m.P1, Taxa = "Faba 3")
      
      Sib.index.P1 <- Import.index("Import/Russia/Vegetation/AB2016/Indexes/Veget_taxa_index_AB16.csv",
                                   Sib.veget.10m.P1, 
                                   Save.path = "Import/Russia/Vegetation/AB2016/Indexes/Veget_taxa_index_AB16_V2.csv",
                                   Csv.sep = ",", Auto.add.miss = F,
                                   Error.display = T)
      
      Sib.veget.10m.P1 <- Convert.releves.10m(MV = Sib.veget.10m.P1, 
                                               Remove.abiot = T, 
                                               Tree.Herb = F,
                                               Taxa.name = F, Family.name = F, Pollen.type = T,
                                               Merge.releve = F, Error.display = T,
                                               Index = Sib.index.P1)
      
      Sib.veget.10m.P1.mean <- Mean.veget.model(MV = Sib.veget.10m.P1,
                                                 Model = "SP",
                                                 Releve.design = "Import/Russia/Vegetation/AB2016/Indexes/Vegetation_model.csv",
                                                 Displot = F,
                                                 Error.display = F,
                                                 Remove.uncomplete = F,
                                                 Csv.sep = ",")
      
      Mong.veget.10m.P1.mean <- merge(Mong.veget.10m.P1.mean, Sib.veget.10m.P1.mean, by = 0, all = T, sort = F)
      row.names(Mong.veget.10m.P1.mean) <- Mong.veget.10m.P1.mean$Row.names
      Mong.veget.10m.P1.mean <- Mong.veget.10m.P1.mean[-1]
      Mong.veget.10m.P1.mean[is.na(Mong.veget.10m.P1.mean)]<-0
      }
    
    #### Statistique ####
    print(paste("Nb Vtypes:", length(unique(Mong.index.P1$Vtype))))
    print(paste("Nb Family:", length(unique(Mong.index.P1$Familly))))
    # print(paste("Nb Genus:", length(unique(Mong.index.P1$Genus))))
    # print(paste("Nb Species:", length(unique(Mong.index.P1$Species))))
    print(paste("Nb PT-sl:", length(unique(Mong.index.P1$Ptype))))
    # print(paste("Nb PT-ss:", length(unique(Mong.index.P1$PT.ss))))
    # write.table(Mong.veget.10m.P1.mean, "Resultats/Mongolia/Vegetation/MV_Mong_p1.csv", sep = ",", col.names = NA)
  }
  else{
    Mong.veget.10m.P1.mean <- read.table("Resultats/Mongolia/Vegetation/MV_Mong_p1.csv", sep = ",", header = T, row.names = 1)
    Mong.eco <- read.table("Import/Mongolia/Site/Surface_samples_climat.csv", sep = ",", header = T, row.names = 1)
  }
  
  #### Diag veget ####
  Diag.veg = F
  if(Diag.veg == T){
    par(mfrow=c(1,1))
    Plot.Mong <- Hist.veget(MV = Mong.veget.10m.P1.mean,
                               Ordin.path = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                               Save.plot = "Figures/Mongolia/Vegetation/AB2016/Hist_veget_AB2016.pdf",  
                               Index = Mong.index.P1,
                               Csv.sep = ",", H = 1500, W = 1800,
                               Max_seuil = 1,
                               AP.NAP = F,
                               Sort = "Ordination"  # Alt_GPS, Alt_DEM, MAP, MAAT, Latitude, Longitude, Ordination
    )
  }
  
  #### LR climate ####
  LR.climat = F
  if(LR.climat == T){
    LR.clim.veget(MT = list(Mongolia = Mong.veget.10m.P1.mean), Meco = list(Mongolia = Mong.eco),
                  Keep.taxa = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia"),
                  Keep.clim = c("Altitude", "MAP", "MAAT"),
                  H = 1200, W = 1200, Strip.lab = F, R2.pos = "bottomleft",
                  Save.plot = "Figures/Mongolia/Vegetation/LRveg_clim_Mong.pdf")
  }
  
  #### Compare pollen and veget ####
  Compare.pollen.veget = F
  if(Compare.pollen.veget == T){
    MPS_mongPol <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Surface/Mongolia_surfP1_MPfrac.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
    MPS_eco <- data.frame(read.csv(file="Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
    library(tibble)
    library(ggplot2)
    library(tidypaleo)
    library(tidyverse)
    H = 600
    W = 400
    Save.plot = "Figures/Mongolia/Vegetation/AB2016/Compar_hist_MV_MP_AB2016.pdf"
    
    row.names(MPS_mongPol)[row.names(MPS_mongPol) == "Ranunculus undiff"] <- "Ranunculaceae"
    # row.names(MPS_mongPol)[row.names(MPS_mongPol) == "Pinus indet"] <- "Pinus indet."
    row.names(MPS_mongPol)[row.names(MPS_mongPol) == "Senecio t"] <- "Asteraceae"
    row.names(MPS_mongPol)[row.names(MPS_mongPol) == "Liliaceae allium t"] <- "Allium"
    MPS_mongPol[row.names(MPS_mongPol) == "Pinus sylvestris",] <- MPS_mongPol[row.names(MPS_mongPol) == "Pinus sylvestris",] + MPS_mongPol[row.names(MPS_mongPol) == "Pinus indet",]
    MPS_mongPol[row.names(MPS_mongPol) == "Poaceae",] <- MPS_mongPol[row.names(MPS_mongPol) == "Poaceae",] + MPS_mongPol[row.names(MPS_mongPol) == "Phragmites",]
    MPS_mongPol[row.names(MPS_mongPol) == "Asteraceae",] <- MPS_mongPol[row.names(MPS_mongPol) == "Asteraceae",] + MPS_mongPol[row.names(MPS_mongPol) == "Carthamus t",]
    MPS_mongPol[row.names(MPS_mongPol) == "Artemisia",] <- MPS_mongPol[row.names(MPS_mongPol) == "Artemisia",] + MPS_mongPol[row.names(MPS_mongPol) == "Artemisia gros t",]
    MPS_mongPol <- MPS_mongPol[!row.names(MPS_mongPol) == "Artemisia gros t",]
    MPS_mongPol <- MPS_mongPol[!row.names(MPS_mongPol) == "Phragmites",]
    MPS_mongPol <- MPS_mongPol[!row.names(MPS_mongPol) == "Pinus indet",]
    
    Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Pinus sylvestris",] <- Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Pinus sylvestris",] + Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Pinus indet.",]
    Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Poaceae",] <- Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Poaceae",] + Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Phragmites",]
    Mong.veget.10m.P1.mean <- Mong.veget.10m.P1.mean[!row.names(Mong.veget.10m.P1.mean) == "Phragmites",]
    Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Asteraceae",] <- Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Asteraceae",] + Mong.veget.10m.P1.mean[row.names(Mong.veget.10m.P1.mean) == "Asteroideae",]
    Mong.veget.10m.P1.mean <- Mong.veget.10m.P1.mean[!row.names(Mong.veget.10m.P1.mean) == "Asteroideae",]
    Mong.veget.10m.P1.mean <- Mong.veget.10m.P1.mean[!row.names(Mong.veget.10m.P1.mean) == "Pinus indet.",]
    Mong.veget.10m.P1.mean <- Mong.veget.10m.P1.mean[,!names(Mong.veget.10m.P1.mean) == "MMNT2M06p"]
    
    MP <- MPS_mongPol
    MV <- Mong.veget.10m.P1.mean
    Max_seuil = NULL
    Display.legend = "top"
    # Keep.taxa = c("Pinus sylvestris", "Pinus sibirica", "Betula", "Larix", "Ericaceae", "Rosaceae", "Artemisia", "Poaceae", "Fabaceae", "Plantago", "Brassicaceae", "Cyperaceae", "Caryophyllaceae", "Amaranthaceae")
    Keep.taxa = c("Pinus sylvestris", "Pinus sibirica", "Betula", "Artemisia", "Poaceae", "Cyperaceae", "Amaranthaceae")
    # Keep.taxa = c("Pinus sylvestris", "Pinus sibirica", "Betula", "Alnus.4p", "Picea", "Larix", "Salix", "Artemisia", "Poaceae", "Cyperaceae", "Amaranthaceae")
    # Keep.taxa = NULL
    CONISS.t = NULL # Vegetation vs Pollen
    # CONISS.t = "Vegetation" #
    Ordin <- MPS_eco["Bioclim.ordin"]
    # Ordin = NULL
    
    
    #### Correspondance and clean ####
    MP <- MP[, intersect(names(MP), names(MV))]
    MV <- MV[, intersect(names(MP), names(MV))]
    MP <- MP*100
    
    #### Fusion taxons rares #### 
    if(is.null(Max_seuil) == F){
      MP_max <- MP[0]
      MP_max[, "max"] <- apply(MP[,], 1, max)
      MP_dom <- MP[rowSums(MP_max)>Max_seuil,]
      MP_rare <- MP[rowSums(MP_max)<=Max_seuil,]
      MP_dom["Other" ,] <- colSums(MP_rare)
      MP <- data.frame(MP_dom)
      
      MV_rare <- MV[row.names(MV) %in% row.names(MP_rare),]
      MV_rare <- rbind(MV_rare, MV["Incertae sedis",])
      MV <- MV[!row.names(MV) %in% row.names(MP_rare),]
      MV <- MV[row.names(MV) !="Incertae sedis",]
      MV["Other" ,] <- colSums(MV_rare)
      }
    if(is.null(Keep.taxa) == F){
      MP <- MP[match(Keep.taxa,row.names(MP)),]
      MV <- MV[match(Keep.taxa,row.names(MV)),]
    }
    #### MP and MV melt ###
    MP$Taxa <- row.names(MP)
    MP <- reshape2::melt(MP, id = "Taxa")
    MP$Type <- "Pollen"
    MV$Taxa <- row.names(MV)
    MV <- reshape2::melt(MV, id = "Taxa")
    MV$Type <- "Vegetation"
    MT <- rbind(MV, MP)
    
    #### Graph settings ####
    if(is.null(Keep.taxa) == F){MT$Taxa <- factor(MT$Taxa, levels = Keep.taxa)}
    
    #### CONISS cluster ####
    if(is.null(CONISS.t) == F){
      MT_clust <- MT %>%
        dplyr::filter(Type == CONISS.t) %>% # Vegetation vs Pollen
        nested_data(qualifiers = variable, key = Taxa, value = value) %>%
        nested_hclust(method = "average")
      
      dendro_order <- MT_clust %>%
        tidyr::unnest(c(qualifiers, dendro_order)) %>%
        dplyr::arrange(dendro_order) %>% 
        dplyr::pull(variable)
      
      order <- dendro_order
      }
    else{order <- levels(MT$variable)}
    
    #### Sort by Manual ordination ####
    if(is.null(Ordin) == F){
    Inter <- intersect(row.names(Ordin), levels(MT$variable))
    Manque.ds.ordin <- setdiff(levels(MT$variable), row.names(Ordin))
    if(length(Manque.ds.ordin) > 0){print(paste("Le site", Manque.ds.ordin, "est manquant dans le fichier d'ordination."))}
    
    Ordin.inter <- setNames(data.frame(Ordin[which(row.names(Ordin) %in% Inter),]), "Ordination")
    row.names(Ordin.inter) <- row.names(Ordin)[which(row.names(Ordin) %in% Inter)]
    
    order <- as.factor(row.names(Ordin.inter)[rev(order(Ordin.inter$Ordination))])
    }

    
    #### Plot ####
    
    Plot.compa <- ggplot(MT, aes(x = value, y = variable, fill = Type)) +
      labs(x = "Relative abundance (%)", y = NULL, fill = "Sample Type")+
      geom_colh(width = 0.85, position = "dodgev") +
      facet_abundanceh(vars(Taxa)) +
      scale_fill_manual(values = c("#d9a15eff", "#3f6b3dff"))+
      scale_y_discrete(limits = order) +
      geom_vline(xintercept = 0, color = "grey70", size = .2)+
      # layer_zone_boundaries(MT_clust, aes(y = variable), colour = "grey60")+
      theme(
        # axis.title.x=element_text(size=17),              # XRF
        axis.text.y = element_text(angle = -45, vjust = 1, hjust = 1, size = 7),
        axis.text.x = element_text(size = 5),
        axis.title = element_text(size=11),              # AGE
        axis.line.x = element_line(lineend = "butt", color = "grey70"),                   # fait apparaitre seulement l'axe des y
        axis.ticks = element_line(lineend = "butt", color = "grey70"),                   # fait apparaitre seulement l'axe des y
        # axis.line.y = element_line(lineend = "butt"),    # fait apparaitre seulement l'axe des x, coupe au bout
        panel.background=element_blank(),                # cache le fond
        legend.position = Display.legend,
        legend.key = element_blank(),  # carré autours du symbole
        # legend.direction = "horizontal",
        panel.spacing = unit(0.2, "lines"),
        # panel.border = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 7),
        # legend.justification = c("left"),               # left, top, right, bottom
        #legend.text = element_blank(),
        #plot.margin = unit(c(0.2,3,0.2,0.2), "lines")
        # plot.margin = unit(c(0.2,0,0.2,0.2), "lines")
      )
    
    #### Add CONISS to plot ####
    library(patchwork)
    Pclust <- ggplot() +
      layer_dendrogram(MT_clust, aes(y = variable), colour = "grey60") +
      scale_y_discrete(limits = dendro_order) +
      labs(x = "CONISS", y = NULL) +
      theme(axis.text.y.left = element_blank(), 
            panel.background=element_blank(),
            plot.background = element_blank(),
            axis.ticks.y.left = element_blank())#,
    
    #### Color ecosystems ####
    values.bi = c("Light taiga" = "#1874CD",
                  "Dark taiga" = "#658D94",
                  "Steppe-desert" = "#DD5925",
                  "Desert" = "#CD2626",
                  "Steppe" = "#EE8D25",
                  "Alpine meadow" = "#FFC125",
                  # "Steppe" = "#1874CD",
                  "Steppe-forest" = "#B2A75C"
                  )
    
    #### Add Ecosystem ####
    Meco <- MPS_eco['Ecosystem'] 
    Meco[Meco==""]<-NA
    Meco$variable <- row.names(Meco)
    Meco <- Meco[complete.cases(Meco),]
    Peco  <-  ggplot(Meco, mapping = aes(x = 1, y = variable, colour = Ecosystem))+ geom_point(size = 2.5)+
      scale_y_discrete(limits = dendro_order) +
      scale_color_manual(values = values.bi, name = "Ecosystems")+
      theme(axis.text = element_blank(), 
            axis.title = element_blank(),
            panel.background=element_blank(),
            plot.background = element_blank(),
            legend.position = Display.legend,
            legend.key = element_blank(),
            plot.margin = unit(c(0,0,0,0), "lines"),
            axis.ticks = element_blank()
            )#,
       
    # Plot.compa <- Plot.compa + Peco + Pclust + plot_layout(widths = c(17/20, 1/20, 2/20), guides = "collect") & theme(legend.position = Display.legend)
    #### Save plot and export ####
    if(is.null(Save.plot) == F){
      if(is.null(W) == F & is.null(H) == F){ggsave(Plot.compa, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")}
      else{ggsave(Save.plot)}}
    
    }
  
  #### PCA / RDA ####
  PCA.disp = F
  if(PCA.disp == T){
    par(mfrow=c(1,2))
    PCA.Mong.P1 <- PCA.veget(data.frame(t(Plot.Mong)),               # Import Matrice pour la PCA
                             Cluster.path = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                             Csv.sep =",",
                             transp_OK = F,                         # Log trans (T), (F) sinon
                             Scale.PCA = 1,                         # 1 or 2
                             # Leg.loc = "bottomright",
                             #Manu.lim = 
                             Site.name = "Arkhangai, Mongolia")     # Nom du site
    
    RDA.Mong.P1 <- RDA.veget(data.frame(t(Plot.Mong)),               # Import Matrice pour la PCA
                             MClim = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                             Choose.clim = c("Altitude", "MAAT", "MAP"),#, "Latitude", "Longitude"),
                             Cluster = c("Ecosystem"),
                             Csv.sep =",",
                             transp_OK = F,                         # Log trans (T), (F) sinon
                             Scale.RDA = 1,                         # 1 or 2
                             Site.name = "Arkhangai, Mongolia")     # Nom du site
    par(mfrow=c(1,1))
  }
  
  #### Procrustean Co-innertia Analysis ####
  PCoI = F
  if(PCoI == T){
    library(ggrepel) # nom des lignes à côté
    
    MPS_mongPol <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Surface/Mongolia_surfP1_MPfrac.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
    MPS_eco <- data.frame(read.csv(file="Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
    
    MPS_mongVeg <- data.frame(t(Plot.Mong))
    MPS_mongPol <- MPS_mongPol[, which(names(MPS_mongPol) %in% names(MPS_mongVeg))]
    MPS_mongVeg <- MPS_mongVeg[, which(names(MPS_mongVeg) %in% names(MPS_mongPol))]
    
    par(mfrow=c(1,2))
    PCA.Mong.P1 <- PCA.veget(MPS_mongVeg,               # Import Matrice pour la PCA
                             Cluster.path = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                             Csv.sep =",", transp_OK = F, Scale.PCA = 1, 
                             Site.name = "AB2016, Mongolia")     # Nom du site
    
    PCA.Mong.pollen.P1 <- PCA.veget(MPS_mongPol,               # Import Matrice pour la PCA
                             Cluster.path = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                             Csv.sep =",", transp_OK = F, Scale.PCA = 1,
                             Site.name = "AB2016, Mongolia")     # Nom du site
    par(mfrow=c(1,1))
    
    
    pro <- procrustes(X = PCA.Mong.P1, Y = PCA.Mong.pollen.P1, symmetric = FALSE)
    ctest <- data.frame(rda1 = pro$Yrot[,1],
                        rda2 = pro$Yrot[,2],
                        xrda1 = pro$X[,1],
                        xrda2 = pro$X[,2])
    ctest$Ecosystem <- as.factor(MPS_eco[which(row.names(MPS_eco) %in% row.names(ctest)), "Ecosystem"])
    
    # ctest <- data.frame(PCoI_1 = pro$Yrot[,1],
    #                     PCoI_2 = pro$Yrot[,2])
    # ctest$site <- row.names(ctest)
    # ctest$type <- "pollen"
    # ctest$Ecosystem <- as.factor(MPS_eco[which(row.names(MPS_eco) %in% row.names(ctest)), "Ecosystem"])
    # 
    # ctest2 <- data.frame(PCoI_1 = pro$X[,1],
    #                     PCoI_2 = pro$X[,2])
    # ctest2$site <- row.names(ctest2)
    # ctest2$type <- "vegetation"
    # ctest2$Ecosystem <- as.factor(MPS_eco[which(row.names(MPS_eco) %in% row.names(ctest2)), "Ecosystem"])
    # 
    
    # ctest <- rbind(ctest, ctest2)
    
    #### Color ecosystems ####
    values.bi = c("Light taiga" = "#1874CD",
                  "Dark taiga" = "#658D94",
                  "Steppe-desert" = "#DD5925",
                  "Desert-steppe" = "#DD5925",
                  "Desert" = "#CD2626",
                  "Steppe" = "#EE8D25",
                  "Alpine meadow" = "#FFC125",
                  "Steppe" = "#1874CD",
                  "Desert" = "firebrick3", 
                  "Desert-steppe" = "darkorange", 
                  "Steppe" = "goldenrod1",
                  "Forest-steppe" = "#38A700",
                  "Riparian forest" = "darkgreen",
                  "Alpine steppe" = "dodgerblue3",
                  "Anthropic" = "grey10",
                  "Steppe-forest" = "#B2A75C",
                  "Forest-steppe" = "#B2A75C"
    )
    
    print(ctest)
    ctest$Lab <- row.names(ctest)
    Scale.color <- scale_color_manual(values = values.bi, guide = F)
    Scale.fill <- scale_fill_manual(values = values.bi, guide = F)
    
    p <- ggplot(ctest) +
    # p <- ggplot(ctest, aes(x = PCoI_1, y = PCoI_2, group = site)) +
      # geom_point(aes(color = Ecosystem, shape = type), size =2.5) +
      geom_segment(aes(x=rda1,y=rda2,xend=xrda1,yend=xrda2), color = "grey30", alpha = 0.5, 
                   arrow = arrow(length=unit(0.25,"cm")))+
      geom_point(aes(x=rda1, y=rda2, color = Ecosystem), size =2.5) +
      geom_point(aes(x=xrda1, y=xrda2, color = Ecosystem), size = 2, shape = 8) +
      geom_text_repel(mapping = aes(x = xrda1, y = xrda2, label = Lab, color = Ecosystem), nudge_y = 0.5,
                      force = 4, #nudge_x  = 20000, 
                      direction = "y", hjust = 1,  alpha = 1,
                      size = 3, face = "bold", parse = F, segment.size = 0.18, segment.colour = "grey70")+
      # geom_line(aes(group = as.factor(site)), color = "grey30", alpha = 0.5, arrow = arrow(length=unit(0.25,"cm")))+
      # geom_line()+
      # facet_wrap(vars(Ecosystem))+
      xlab("PCoI 1")+
      ylab("PCoI 2")+
      Scale.color+
      scale_shape_manual(name = "Sample",
                         labels = c("Pollen", "Vegetation"),
                         values = c(16, 8)) +
      theme_classic()+
      theme(axis.line = element_blank(), 
            strip.background = element_blank(),
            strip.placement = "outside",
            panel.border = element_rect(colour = "grey50", fill = NA))
    
    W = 500
    H = 300
    Save.plot = "Figures/Mongolia/Vegetation/AB2016/PCoI_veget_pollen_AB2016.pdf"
    ggsave(p, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
    
    #### Procrustes errors ####
    ctest$Procrustes_error <- sqrt((ctest$rda1-ctest$xrda1)^2+(ctest$rda2-ctest$xrda2)^2)
    ctest <- ctest[order(ctest$Procrustes_error,decreasing = T),]
    row.names(ctest) <- seq(1:nrow(ctest))
    ctest$ID <- as.numeric(row.names(ctest))
    
    p2 <- ggplot(ctest, aes(x = ID, y = Procrustes_error)) +
      geom_bar(aes(fill = Ecosystem), position = 'dodge', stat='identity')+
      geom_hline(yintercept = mean(ctest$Procrustes_error), color="grey30", lwd = 0.2)+
      geom_hline(yintercept = quantile(ctest$Procrustes_error)[c(2,4)], color="grey30", lwd = 0.2, linetype = "longdash")+
      coord_flip()+
      Scale.color+
      Scale.fill+
      theme_classic()+
      theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())
    
    #### Boxplot errors #### 
    p3 <- ggplot(ctest) +
      geom_boxplot(aes(Procrustes_error, y = Ecosystem, fill = Ecosystem), outlier.color = "grey70")+
      geom_vline(xintercept = mean(ctest$Procrustes_error), color="grey30", lwd = 0.2)+
      geom_vline(xintercept = quantile(ctest$Procrustes_error)[c(2,4)], color="grey30", lwd = 0.2, linetype = "longdash")+
      # coord_flip()+
      Scale.fill+
      xlab("Procrustes error")+
      theme_classic()+
      theme(axis.title.y = element_blank())
    
    #### Save and export ####
    # p2 <- p|(p2 / p3 + plot_layout(widths = c(3/4, 1/4), guides = "collect")) + plot_layout(heights = c(3/5, 2/5))
    # W = 1200
    # H = 800
    # Save.plot = "Figures/Mongolia/Vegetation/AB2016/Procrustes_errors_veget_pollen_AB2016.pdf"
    # ggsave(p2, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")
    
    
    # Pfull <- Plot.compa + p2 +  plot_layout(heights = c(2/3, 1/3))
    
    layout <- "
    AAAAAAAAAAAAAAABCC
    AAAAAAAAAAAAAAABCC
    AAAAAAAAAAAAAAABCC
    AAAAAAAAAAAAAAABCC
    AAAAAAAAAAAAAAABCC
    AAAAAAAAAAAAAAABCC
    DDDDDDDDDEEEEEEEEE
    DDDDDDDDDEEEEEEEEE
    DDDDDDDDDFFFFFFFFF
    "
    Pfull <-  Plot.compa + Peco + Pclust + p + p2 + p3 + plot_layout(design = layout, guides = "collect")& theme(legend.position = Display.legend)
    
    H = 1000
    W = 1300
    Save.plot = "Figures/Mongolia/Vegetation/AB2016/Compar_hMV_MP_AB2016_total.pdf"
    ggsave(Pfull, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")#
    
    
  }
  
  
}

#### Uzbekistan ####
Uzbek.veget = T
if(Uzbek.veget == T){
  #### Calculations ####
  Calculations = F
  if(Calculations == T){
    All.Error.to.show = F
    Uz.veget.10m <- Import.releves.10m.unitable(Releve.path = "Import/Uzbekistan/Vegetation/Releves/Uzbek_veget_p1.csv",
                                                Site.path = "Import/Uzbekistan/Vegetation/Releves/Uz_site_releve.csv",
                                                Com.path = "Import/Uzbekistan/Vegetation/Releves/Uzbek_community_p1.csv",
                                                Csv.sep = ",", Error.display = All.Error.to.show,
                                                Plus.convert = "0.5")

    Uz.index.TN <- Import.index(Herbier = Herbier,
                                Herbier.country = c("Tajikistan", "Uzbekistan"),
                                MV = Uz.veget.10m, TNRS.Accepted = T, add.CF = F, add.subsp = F,
                                Csv.sep = ",", Auto.add.miss = T, Simplify.Vtype = F, Add.ubiquist = T,
                                Inerte.path = "Import/Uzbekistan/Vegetation/Indexes/List_inerte.csv",
                                Save.path = "Import/Uzbekistan/Vegetation/Indexes/Index_from_Uz_Herbier_auto.csv",
                                Error.display = All.Error.to.show)
    
    Uz.index.TN$Species[which(Uz.index.TN$Species == "Oxytropis glabra")] <- "Oxytropis spp."
    
    # # Recherche des Vtypes manquants ?
    # A <- as.vector(read.table("Import/Uzbekistan/Vegetation/Indexes/Index_from_Uzeri_Herbier_auto_missing_in_herbier.csv"))
    # Search.releve(Uz.veget.10m,Taxa = "Draba sp.1")
    # Search.releve(Uz.veget.10m,Taxa = "Galium sp. blanc t2")
    # print(lapply(Uz.veget.10m, function(A) row.names(A)[row.names(A) %in% Still.miss$x]))
    # print(lapply(Uz.veget.10m, function(A) row.names(A)[row.names(A) %in% "Alopecurus droit t"]))
    # print(row.names(Uz.veget.10m$MUZT6C02))
    
    Nb.tree.layers = 3
    Tree.Herb = T
    Uz.veget.10m.TN <- Convert.releves.10m(MV = Uz.veget.10m, Remove.abiot = T, Tree.Herb = Tree.Herb, Nb.tree.layers = Nb.tree.layers,
                                             Taxa.name = T, Family.name = F, Pollen.type = F, Keep.unknown.type = F,
                                             Merge.releve = F, Error.display = All.Error.to.show, Index = Uz.index.TN)
    # print(row.names(Uz.veget.10m.TN$MUZT2M08))

    Uz.veget.10m.PT_ss <- Convert.releves.10m(MV = Uz.veget.10m, Remove.abiot = T, Tree.Herb = Tree.Herb, Nb.tree.layers = Nb.tree.layers,
                                             Taxa.name = F, Family.name = F, Pollen.type = T, PT = "ss",
                                             Merge.releve = F, Error.display = F, Index = Uz.index.TN)

    Uz.veget.10m.PT_sl <- Convert.releves.10m(MV = Uz.veget.10m, Remove.abiot = T, Tree.Herb = Tree.Herb, Nb.tree.layers = Nb.tree.layers,
                                             Taxa.name = F, Family.name = F, Pollen.type = T, PT = "sl",
                                             Merge.releve = F, Error.display = F, Index = Uz.index.TN)

    Uz.veget.10m.mean.TN <- Mean.veget.model(MV = Uz.veget.10m.TN, Model = "SQTD", # SP SQTD
                                             Digits = 2, Releve.design = "Import/Uzbekistan/Vegetation/Indexes/Vegetation_model.csv",
                                             Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")
  
    Uz.veget.10m.mean.PT_ss <- Mean.veget.model(MV = Uz.veget.10m.PT_ss, Model = "SQTD", # SP SQTD
                                                Digits = 2, Releve.design = "Import/Uzbekistan/Vegetation/Indexes/Vegetation_model.csv",
                                                Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")

    Uz.veget.10m.mean.PT_sl <- Mean.veget.model(MV = Uz.veget.10m.PT_sl, Model = "SQTD", # SP SQTD
                                                Digits = 2, Releve.design = "Import/Uzbekistan/Vegetation/Indexes/Vegetation_model.csv",
                                                Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")

   
    #### Extrapolates vegetation plots from surroundings ####
    Extrapolate = F
    if(Extrapolate == T){
      Add.extra.plots.to.MV <- function(M, Extra.list){
        Extre.to.add <- M[match(Extra.list$Veget_plot_close, names(M))]
        names(Extre.to.add) <- row.names(Extra.list)
        M <- cbind(M, Extre.to.add)
        return(M)
        } 
      Extra.plots  <- data.frame(read.csv(file="Import/Uzbekistan/Vegetation/Releves/Uz_topcore_extrapolation.csv",sep=",",dec=".", header=T, row.names=1))
      Uz.veget.10m.mean.TN <- Add.extra.plots.to.MV(Uz.veget.10m.mean.TN, Extra.plots)
      Uz.veget.10m.mean.PT_sl <- Add.extra.plots.to.MV(Uz.veget.10m.mean.PT_sl, Extra.plots)
      Uz.veget.10m.mean.PT_ss <- Add.extra.plots.to.MV(Uz.veget.10m.mean.PT_ss, Extra.plots)
      }
    
    #### Envoie Nowak Tajikistan ####
    envoie.Nowak = F
    if(envoie.Nowak == T){
      Uz.index.TN <- Import.index(Herbier = Herbier,
                                  Herbier.country = c("Tajikistan", "Uzbekistan"),
                                  MV = Uz.veget.10m, TNRS.Accepted = T, add.CF = T, add.subsp = T,
                                  Csv.sep = ",", Auto.add.miss = T, Simplify.Vtype = F, Add.ubiquist = T,
                                  Inerte.path = "Import/Uzbekistan/Vegetation/Indexes/List_inerte.csv",
                                  Save.path = "Import/Uzbekistan/Vegetation/Indexes/Index_from_Uz_Herbier_auto.csv",
                                  Error.display = All.Error.to.show)
      
      Uz.veget.10m.TN <- Convert.releves.10m(MV = Uz.veget.10m, Remove.abiot = T, Tree.Herb = F,
                                             Taxa.name = T, Family.name = F, Pollen.type = F, Keep.unknown.type = T,
                                             Merge.releve = F, Error.display = All.Error.to.show, Index = Uz.index.TN)
      
      Uz.veget.10m.mean.TN <- Mean.veget.model(MV = Uz.veget.10m.TN, Model = "SQTD",
                                               Releve.design = "Import/Uzbekistan/Vegetation/Indexes/Vegetation_model.csv",
                                               Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")
      
      Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[,grep("MTA", names(Uz.veget.10m.mean.TN))]
      Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[which(rowSums(Uz.veget.10m.mean.TN) != 0),]
      write.table(Uz.veget.10m.mean.TN, "Send_Nowak.csv", sep = ",", col.names = NA)
    }
    #### Statistique ####
    # print(row.names(Uz.veget.10m.mean.TN[rowSums(Uz.veget.10m.mean.TN) == 0,]))
    # print(row.names(Uz.veget.10m.mean.PT_sl[rowSums(Uz.veget.10m.mean.PT_sl) == 0,]))
    # print(row.names(Uz.veget.10m.mean.PT_ss[rowSums(Uz.veget.10m.mean.PT_ss) == 0,]))
    print(paste("Nb vegetation plots:", ncol(Uz.veget.10m.mean.TN)))
    print(paste("Nb Vtypes:", length(unique(Uz.index.TN$Vtype))))
    print(paste("Nb Family:", length(unique(Uz.index.TN$Family))))
    print(paste("Nb Genus:", length(unique(Uz.index.TN$Genus))))
    print(paste("Nb Species:", length(unique(Uz.index.TN$Species))))
    print(paste("Nb PT-sl:", length(unique(Uz.index.TN$PT.sl))))
    print(paste("Nb PT-ss:", length(unique(Uz.index.TN$PT.ss))))
    Main.taxa <- sapply(Uz.veget.10m.mean.TN, function(x) paste(row.names(Uz.veget.10m.mean.TN[x > 5,]), collapse = ", "))
    
    write.table(Main.taxa, "Resultats/Uzbekistan/Vegetation/Main_taxa_uz.csv", sep = ",", col.names = NA)
    write.table(Uz.veget.10m.mean.TN, "Resultats/Uzbekistan/Vegetation/MV_uz.csv", sep = ",", col.names = NA)
    write.table(Uz.veget.10m.mean.PT_ss, "Resultats/Uzbekistan/Vegetation/MV_uz_PT_ss.csv", sep = ",", col.names = NA)
    write.table(Uz.veget.10m.mean.PT_sl, "Resultats/Uzbekistan/Vegetation/MV_uz_PT_sl.csv", sep = ",", col.names = NA)
    
    }
  else{
    Uz.veget.10m.mean.TN <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz.csv", sep = ",", header = T, row.names = 1)
    Uz.veget.10m.mean.PT_sl <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz_PT_sl.csv", sep = ",", header = T, row.names = 1)
    Uz.veget.10m.mean.PT_ss <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz_PT_ss.csv", sep = ",", header = T, row.names = 1)
    }
  
  #### Ecological import ####
  Uz.eco <- read.table("Import/Uzbekistan/Site/TUSD_surf_samples.csv", sep = ",", header = T, row.names = 1)
  MP.Uz.filter <- read.table("Resultats/Uzbekistan/Pollen/MP_Uz_filtered.csv", sep = ",", header = T, row.names = 1)
  Uz.MP_sl  <- data.frame(read.csv(file="Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_coarse.csv",sep=",",dec=".", header=T, row.names=1))
  Uz.MP_ss  <- data.frame(read.csv(file="Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_fine.csv",sep=",",dec=".", header=T, row.names=1))
  
  Sort.eco.Uz = c("Chol cold desert-steppes", "Tugai riparian forest", "Chol warm deserts", "Adyr desert-steppes", 
                  "Adyr steppes", "Tau riparian forest", "Tau thermophilous woodlands", 
                  "Tau juniper steppe-forest", "Tau steppes", "Alau cryophilous steppe-forest", "Alau meadows")
  Uz.col = c("#7916C4", "#BB0268", "#bb0202", "#ff5400", 
             "#e6c607", "#2C9740", "#85682D", "#176E5B", "#bab133", "#54a697", "#197CDA")
  Sort.eco.Uz.lab <-  c("*Chol* cold desert-steppes", "*Tugai* riparian forest", "*Chol* warm deserts", "*Adyr* desert-steppes", 
                  "*Adyr* steppes", "*Tau* riparian forest", "*Tau* thermophilous woodlands", 
                  "*Tau* juniper steppe-forest", "*Tau* steppes", "*Alau* cryophilous steppe-forest", "*Alau* meadows")
  My_eco_col <- setNames(data.frame(t(Uz.col)), Sort.eco.Uz)
  
  #### Verif metadata list samples ####
  Check.metada = F
  if(Check.metada == T){
    Meco.all <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_V2.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$GDGT == "x"] <- paste(Meco.all$Proxy[Meco.all$GDGT == "x"], "G", sep = "")
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    List.samp.Veg.plot.T <- names(Uz.veget.10m.mean.TN)
    List.samp.Veg.plot.expected <- row.names(Meco.all[Meco.all$Veg.plot == "x",])
    List.samp.Veg.plot.expected <- List.samp.Veg.plot.expected[grepl("UZ", List.samp.Veg.plot.expected) | grepl("TA", List.samp.Veg.plot.expected)]
    
    if(length(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T)) >= 1){
      print("The following samples are missing from the Veg.plot matrix :")
      print(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T))}
    
    if(length(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)) >= 1){
      print("The following samples are missing from the metadata :")    
      print(sort(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)))}
    }
  
  #### Twinspan ####
  Twinspan.Uz = F
  if(Twinspan.Uz == T){
    Best = T
    if(Best == T){
      # Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("spp\\.", row.names(Uz.veget.10m.mean.TN))&!grepl("eae", row.names(Uz.veget.10m.mean.TN)),]
      Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("Poaceae", row.names(Uz.veget.10m.mean.TN)),]
      Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("Incertae sedis", row.names(Uz.veget.10m.mean.TN)),]
      # Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("Gagea", row.names(Uz.veget.10m.mean.TN)),]
      # Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("Astragalus spp", row.names(Uz.veget.10m.mean.TN)),]
      Uz.veget.10m.mean.TN <- Uz.veget.10m.mean.TN[!grepl("Allium spp", row.names(Uz.veget.10m.mean.TN)),]
      Uz.Tw.TN.best <- Plot.twinspan(MV = Uz.veget.10m.mean.TN, #Select.species = "indicator", # indicator ou leading
                                Max.species = 100,
                                # Max.species = 300,
                                Show.indicators = T, Cut.levels = c(0.01, 1, 3, 5, 25, 50, 70),
                                Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Nb.cluster = 4,
                                H = 900, W = 700, Return.plot = F,
                                # H = 2000, W = 700,
                                Sp.lab.size = 5.4, Site.lab.size = 7, Eco.col = My_eco_col,
                                Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                Save.Indicator = "Resultats/Uzbekistan/Vegetation/Indicator_species.Rds",
                                Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_Taxa_100_best.pdf")}
    
    All.tw = T
    if(All.tw == T){
      Uz.Tw.PT_ss <- Plot.twinspan(MV = Uz.veget.10m.mean.PT_ss, Nb.cluster = 4,
                                   Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Show.indicators = T,
                                   H = 950, W = 700,  Max.species = 50, Sp.lab.size = 8, Site.lab.size = 8, Eco.col = My_eco_col,
                                   Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                   Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_PT_ss.pdf")
      
      Uz.Tw.PT_sl <- Plot.twinspan(MV = Uz.veget.10m.mean.PT_sl, Nb.cluster = 4,
                                   Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Show.indicators = T, Eco.col = My_eco_col,
                                   Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                   H = 950, W = 700,  Max.species = 50, Sp.lab.size = 8, Site.lab.size = 8,
                                   Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_PT_sl.pdf")
      
      Uz.Tw.TN <- Plot.twinspan(MV = Uz.veget.10m.mean.TN,
                                Sp.lab.size = 6.5, Site.lab.size = 12.5, Nb.cluster = 4, Cut.levels = c(0.01, 1, 3, 5, 25, 50, 70),
                                H = 1500, W = 1100, Multipage.nrow = 200,
                                Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 4, Show.indicators = T, Eco.col = My_eco_col,
                                Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_Taxa_total.pdf")
      
      Uz.Tw.TN.ind <- Plot.twinspan(MV = Uz.veget.10m.mean.TN, Select.species = "indicator", # indicator ou leading
                                    Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Show.indicators = F, Nb.cluster = 4, 
                                    Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                    H = 950, W = 700, Sp.lab.size = 8, Site.lab.size = 8, Eco.col = My_eco_col,
                                    Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_Taxa_indicator_sp.pdf")
      
      Uz.Tw.TN.lead <- Plot.twinspan(MV = Uz.veget.10m.mean.TN, Select.species = "leading", # indicator ou leading
                                     Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Show.indicators = T, Nb.cluster = 4, 
                                     Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                     H = 950, W = 700, Sp.lab.size = 8, Site.lab.size = 8, Eco.col = My_eco_col,
                                     Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_Taxa_leading_sp.pdf")
    }
    
    MV.Uz.filter <- Uz.veget.10m.mean.TN[row.names(Uz.veget.10m.mean.TN) %in% Uz.Tw.TN.best,]
    write.table(MV.Uz.filter, "Resultats/Uzbekistan/Vegetation/MV_Uz_TN_filter_twinspan.csv", sep = ",", col.names = NA)
    }
  else{MV.Uz.filter <- read.table("Resultats/Uzbekistan/Vegetation/MV_Uz_TN_filter_twinspan.csv", sep = ",", header = T, row.names = 1)}
  Uz.IndTax <- readRDS("Resultats/Uzbekistan/Vegetation/Indicator_species.Rds")
  
  #### CA + dendrogramme ####
  CCA = F
  if(CCA == T){
    #### Correspondance analysis (CA) ####
    CA.MV.Uz <- CA.vegetation(MV = log1p(MV.Uz.filter), H = 600, W = 600, Nb.contrib = 15, Show.site.name = F, Legend.position = "none",
                              Cluster.path = Uz.eco, Cluster.groups = "Ecosystem", Color.choise = setNames(Uz.col, Sort.eco.Uz),
                              Dot.size = 3, Dot.opac = .8, Lab.size = 2, Pollen = F,  Annot = "(B)", X.pos = "top", Y.pos = "right",
                              Save.plot = "Figures/Uzbekistan/Vegetation/CA_MV_Uz.pdf")
    
    #### Add CA to Twinspan ####
    Add.to.tw = T
    if(Add.to.tw == T){
      Uz.Tw.TN.best <- Plot.twinspan(MV = Uz.veget.10m.mean.TN, Max.species = 100, Annot = "(A)", 
                                     Show.indicators = T, Cut.levels = c(0.01, 1, 3, 5, 25, 50, 70),
                                     Add.dendro = T, Meco = Uz.eco, Eco.dot.size = 2, Nb.cluster = 4, Return.plot = T,
                                     Sp.lab.size = 5.4, Site.lab.size = 7, Eco.col = My_eco_col,
                                     Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png")}
    H = 900
    W = 700
    Save.plot = "Figures/Uzbekistan/Vegetation/Twinspan_Uz_Taxa_100_best.pdf"
    CA.MV.Uz <- Uz.Tw.TN.best + inset_element(CA.MV.Uz, left = 0, bottom = 0, right = 0.43, top = 0.43)
    ggsave(file = Save.plot, CA.MV.Uz, width = W*0.01041666666667, height = H*0.01041666666667)
    
    #### Dendrogramme #### 
    Hclust.MV.Uz <- Hclust.vegetation(MV = data.frame(t(MV.Uz.filter)), H = 1600, W = 800,
                                      Save.plot = "Figures/Uzbekistan/Vegetation/Dendro_veget_Uz.pdf")
  }
  
  #### Camembert ####
  Cam.Uz = F
  if(Cam.Uz == T){
    Uz.MV.piechart <- Veget.piechart(Uz.veget.10m.mean.TN, 
                                     # Select.plot = grepl("MUZT3S0",names(Uz.veget.10m.mean)),
                                     # Select.plot = c("MUZT3S01"),
                                     # Select.plot = c("MUZT3M11", "MUZT5M03", "MUZT3S01"),
                                     Plotly = T,
                                     Plotly.all = T,
                                     H = 1000, W = 1000, Save.plot = "Figures/Uzbekistan/Vegetation/MV.pdf")}
  
  #### Diag veget ####
  Diag.veget.uz = F
  if(Diag.veget.uz == T){
    MV.Uz.filter <- Hist.veget(MV = Uz.veget.10m.mean.PT_ss,
                               Ordin.path = "Import/Uzbekistan/Site/Uz_surf_samples.csv",
                               # Ordin.path = "Resultats/Uzbekistan/Vegetation/MV_uz_PCA.csv",
                               Save.plot = "Figures/Uzbekistan/Vegetation/Hist_veget_uz.pdf",
                               Index = Uz.index.P1,
                               Csv.sep = ",",
                               Max_seuil = 15,
                               AP.NAP = F, H = 800, W = 1200,
                               Sort = "MAP"  # Altitude, MAP, MAAT, Latitude, Longitude, Ordin1, Ordin2
    )
    
    MV.Uz.filter <- data.frame(t(MV.Uz.filter))
    write.table(MV.Uz.filter, "Resultats/Uzbekistan/Vegetation/MV_Uz_TN_filter_hist.csv", sep = ",", col.names = NA)
  }
  # else{MV.Uz.filter <- read.table("Resultats/Uzbekistan/Vegetation/MV_Uz_TN_filter_hist.csv", sep = ",", header = T, row.names = 1)}
  
  
  #### PCA / RDA ####
  PCA.disp = F
  if(PCA.disp == T){
    #### Multiplot settings ####
    H = 1000
    W = 1000
    Save.plot = "Figures/Uzbekistan/Vegetation/PCA_RDA_Uz.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    
    par(mfrow = c(2, 2),     # 2x2 layout
        oma = c(0, 0, 0, 0), # two rows of text at the outer left and bottom margin
        mar = c(0, 0, 0, 0), # space for one row of text at ticks and to separate plots
        mgp = c(0, 0, 0),    # axis label at 2 rows distance, tick labels at 1 row
        xpd = F
        )            # allow content to protrude into outer margin (and beyond)
    
    My_clus <- "Ecosystem"
    #### PCA veget ####
    # MV.Uz.filter <- MV.Uz.filter[row.names(MV.Uz.filter) %in% Uz.IndTax,]
    # row.names(MV.Uz.filter) <- vegan::make.cepnames(row.names(MV.Uz.filter))
    # Uz.IndTax <- vegan::make.cepnames(Uz.IndTax)
    PCA.MV.Uz <- PCA.vegetation(scale(MV.Uz.filter),
                             Cluster.path = Uz.eco, Cluster.groups = "Ecosystem", Annot = "(A)",
                             Csv.sep =",", Simple.title = T, Show.text = F, Scale.PCA = 1, Manu.lim = c(-3,2.6,-5.8,1.6),
                             transp_OK = T, cluster.from.PCA = F, Display.legends = T, 
                             # Nb.contrib = 5,
                             Vector.show = Uz.IndTax,
                             Save.path = "Resultats/Uzbekistan/Vegetation/MV_Uz.csv", Color.choice = Uz.col,
                             Sort.eco = Sort.eco.Uz, Leg.loc = "bottomleft", Symbol.loc = c(4, 4, 1.5), Leg.size = 1.2,
                             Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                             # Symbol.path = "Figures/ACA/Trait/Workflow/Symbole_pollen.svg",
                             Type.samples = "", Title.inside = T)
  
    # Contrib <- res.pca$ind$contrib[,c(1,2)]
    # Contrib$Mean <- rowMeans(Contrib)
    # Contrib$Mean > sort(Contrib$Mean, decreasing = T)[[Nb.contrib]]
    
    #### RDA veget ####
    RDA.MV.Uz <- RDA.vegetation(scale(MV.Uz.filter), MClim = Uz.eco,
                             # Choose.clim = c("MTWAQ", "MPWAQ", "MPCOQ", "Altitude"), 
                             Choose.clim = c("MAAT", "MAP", "PS", "TS", "MTCOQ", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                             Cluster.path = Uz.eco, Cluster.groups = "Ecosystem", Annot = "(B)",
                             Display.legends = F, Simple.title = T, Color.choice = Uz.col, 
                             Csv.sep =",",Leg.loc2 = "bottomleft", Title.inside = T,
                             transp_OK = T, Scale.sites = 3, Scale.taxa = 3, Vector.show = Uz.IndTax,
                             Manu.lim = c(-2.3,1.5,-2.1,1.8), Symbol.loc = c(1.25, 1.08, .52),
                             Save.path = "Resultats/Uzbekistan/Pollen/Surface/MP_Uz.csv",
                             Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                             Type.samples = "Vegetation plots",
                             Sort.eco = Sort.eco.Uz)
    
    #### PCA pollen ####
    source("Scripts/Pollen_surf.R")
    rownames(Plot.Uz.pollen.surf)[rownames(Plot.Uz.pollen.surf) == "Chenopodiaceae"] <- "Amaranthaceae"
    PCA.pollen.Uz <- PCA.pollen.surf(scale(Plot.Uz.pollen.surf),
    # PCA.pollen.Uz <- PCA.pollen.surf(Plot.Uz.pollen.surf,
                                     Cluster.path = Meco, Csv.sep =",", Simple.title = T, Show.text = F,
                                     transp_OK = T, cluster.from.PCA = F, Display.legends = F, 
                                     Scale.PCA = 2, Manu.lim = c(-1.5,2.7,-1.7,2.2), Annot = "(C)", Title.inside = T,
                                     # Cluster.groups = "Vegetation", Color.choice = Uz.col.2, Sort.eco = Sort.eco.Uz.2,
                                     Cluster.groups = "Ecosystem", Color.choice = Uz.col, Sort.eco = Sort.eco.Uz,
                                     Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png",  Nb.contrib = 15,
                                     Save.path = "Resultats/Uzbekistan/Pollen/Surface/MP_Uz.csv", Symbol.loc = c(1.25, 1.08, .52),
                                     Leg.loc = "bottomleft", Type.samples = "Pollen")
    
    #### RDA  pollen ####
    RDA.Uz.P1 <- RDA.pollen.surf(scale(Plot.Uz.pollen.surf), MClim = Meco, Cluster.path = Meco,
                                 # RDA.Uz.P1 <- RDA.pollen.surf(Plot.Uz.pollen.surf, MClim = Meco, Cluster.path = Meco,
                                 Choose.clim = c("MAAT", "MAP", "PS", "TS", "MTCOQ", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                                 Show.text = F, Display.legends = F, Simple.title = T, transp_OK = T,
                                 # Cluster.groups = "Vegetation", Color.choice = Uz.col.2, Sort.eco = Sort.eco.Uz.2,
                                 Cluster.groups = "Ecosystem", Color.choice = Uz.col, Sort.eco = Sort.eco.Uz,
                                 Scale.sites = 3, Scale.taxa = 2, Annot = "(D)", Title.inside = T,
                                 Manu.lim = c(-1.5,1.8,-1.75,1.6), Leg.loc = "bottomright", Nb.contrib = 15,
                                 Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png", Symbol.loc = c(1.25, 1.08, .52),
                                 Save.path = "Resultats/Uzbekistan/Pollen/Surface/MP_Uz.csv",
                                 Csv.sep =",",Leg.loc2 = "bottomleft", Type.samples = "Pollen")
    
    #### Results ####
    PC1.ordin <- data.frame(PCA.MV.Uz)
    PC1.ordin$Ordin2 <- match(PC1.ordin$PC1, sort(PC1.ordin$PC1))
    write.table(PC1.ordin, "Resultats/Uzbekistan/Vegetation/MV_uz_PCA.csv", sep = ",", col.names = NA)
    
    dev.off()
    par(mfrow=c(1,1))
  }
  
  #### LDA ####
  LDA.Uz = F 
  if(LDA.Uz == T){
    LDA.MV.Uz <- LDA.vegetation(MV = data.frame(t(MV.Uz.filter)), Color.choise = setNames(Uz.col, Sort.eco.Uz),
                                Cluster = "Ecosystem", Cluster.path = Uz.eco, Sort.cluster = Sort.eco.Uz,
                                Limites = c(-4.6,1,-5.2,4),
                                H = 900, W = 900, Save.plot = "Figures/Uzbekistan/Vegetation/LDA_Uz.pdf")}
  
  #### LR climate ####
  LR.climat = F
  if(LR.climat == T){
    LR.clim.veget(MT = list(Uzbekistan = Uz.veget.10m.mean.PT_ss), Meco = list(Uzbekistan = Uz.eco),
                  Keep.taxa = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia spp."),
                  Keep.clim = c("Altitude", "MAP", "MAAT", "TS"),
                  H = 1200, W = 1200, Strip.lab = F, R2.pos = "topright",
                  Save.plot = "Figures/Uzbekistan/Vegetation/LRveg_clim_Uz.pdf")
    
  }
  
  #### Compare pollen and veget ####
  Compare.pollen.veget = T
  if(Compare.pollen.veget == T){
    #### Import data ####
    Uz.pol.ind  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv",sep=",",dec=".", header=T, row.names=1))
    Uz.MV_sl <- Uz.veget.10m.mean.PT_sl
    Uz.MV_ss <- Uz.veget.10m.mean.PT_ss
    Clean.taxo <- Check.taxa.diff(Uz.MP_sl, Uz.MV_sl, type = "coarse", Display.message = F, Auto.homogeneise = T)
    Uz.MV_sl <- Clean.taxo[[1]]
    Uz.MP_sl <- Clean.taxo[[2]]
    write.table(Clean.taxo[[4]], "Resultats/Uzbekistan/Vegetation/VegTaxa_not_in_pollen_sl.csv")
    
    Clean.taxo <- Check.taxa.diff(Uz.MP_ss, Uz.MV_ss, type = "fine", Display.message = F, Auto.homogeneise = T)
    Uz.MV_ss <- Clean.taxo[[1]]
    Uz.MP_ss <- Clean.taxo[[2]]
    write.table(Clean.taxo[[4]], "Resultats/Uzbekistan/Vegetation/VegTaxa_not_in_pollen_ss.csv")
    
    #### Clean DB ####
    Keep.common.site <- intersect(names(Uz.MP_sl), names(Uz.MV_sl))
    TUSD.coord <- Uz.eco[c(1,2, 55, 10)]
    TUSD.coord <- TUSD.coord[row.names(TUSD.coord) %in% Keep.common.site,]
    names(TUSD.coord) <- c("Long", "Lat", "Alt", "Eco")
    
    Uz.MP_sl <- Uz.MP_sl[!grepl("MUZT5M03", names(Uz.MP_sl))]
    Uz.MP_ss <- Uz.MP_ss[!grepl("MUZT5M03", names(Uz.MP_ss))]
    Uz.MV_sl <- Uz.MV_sl[!grepl("MUZT5M03", names(Uz.MV_sl))]
    Uz.MV_ss <- Uz.MV_ss[!grepl("MUZT5M03", names(Uz.MV_ss))]
    
    Keep.taxa = c("Betula spp.", "Juniperus spp.", "Rosaceae", "Caprifoliaceae",
                  "Fraxinus spp.", "Salix spp.", "Populus spp.", "Juglans regia", "Elaeagnus spp.",
                  "Epilobium-type", "Potentilla-type", "Lamiaceae", "Polygonum-type", "Apiaceae",
                  "Plantago spp.", "Ranunculaceae", "Caryophyllaceae", "Cardueae", "Cousinia spp.", "Scrophulariaceae-type",
                  "Cyperaceae", "Fabaceae", "Poaceae", "Artemisia spp.", "Amaranthaceae", "Asteroideae", "Brassicaceae", "Cichorioideae", 
                  "Convolvulus-type", "Ephedra distachya-type", "Ephedra fragilis-type", "Calligonum-type", "Tamarix spp.")
    
    Keep.taxa.2 = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia spp.", "Asteroideae", 
                    "Fabaceae", "Rosaceae", "Juniperus spp.", "Juglans regia")
    
    Uz.pol.ind$AP_NAP[Uz.pol.ind$PT_sl.label == "Rosaceae"] <- c("AP", "AP")
    Uz.pol.ind$AP_NAP[Uz.pol.ind$PT_sl.label == "Caprifoliaceae"] <- c("AP", "AP", "AP", "AP")
    
    #### Plots diag pol / veg ####
    Diag.po.ve = F
    if(Diag.po.ve == T){
      #### Extrapolates vegetation plots from surroundings ####
      Extrapolate = T
      if(Extrapolate == T){
        Add.extra.plots.to.MV <- function(M, Extra.list){
          Extre.to.add <- M[match(Extra.list$Veget_plot_close, names(M))]
          names(Extre.to.add) <- row.names(Extra.list)
          M <- cbind(M, Extre.to.add)
          return(M)
        } 
        Extra.plots  <- data.frame(read.csv(file="Import/Uzbekistan/Vegetation/Releves/Uz_topcore_extrapolation.csv",sep=",",dec=".", header=T, row.names=1))
        Uz.MV_ss_DP <- Add.extra.plots.to.MV(Uz.MV_ss, Extra.plots)
        Uz.MV_sl_DP <- Add.extra.plots.to.MV(Uz.MV_sl, Extra.plots)
      }
      Keep.taxa <- c(Keep.taxa, "Other")
      
      #### Plots ####
      A = Diag.pol.veget(MP = Uz.MP_sl, MV = Uz.MV_sl_DP, 
                     Meco = Uz.eco, Keep.taxa = Keep.taxa, Pollen.index = Uz.pol.ind,
                     CONISS.t = "Pollen", Nzone = 9, Print.cluster.zone = T, Print.cluster.name = T,
                     Ordin = Uz.eco["Ordin2"], Aggregation.ss = F,
                     Max_seuil = NULL, Display.legend = "bottom",
                     Remove.sites.not.full = F, AP.NAP = T,  ShP = T, AP.NAP.ShP.col = T,
                     Show.site.name = T, Leg.nb.row = 3,
                     Show.site.eco = T, Eco.dot.size = 2.2, Eco.col = My_eco_col, Eco.lab = Sort.eco.Uz.lab,
                     Abiot.plot = c("MAP", "MAAT", "Altitude"),
                     H = 850, W = 1350,
                     Save.plot = "Figures/Uzbekistan/Vegetation/Compar_hist_MV_MP_uz.pdf")
      }
      
    #### Plots LR pol / veg ####
    LR.po.ve = F
    if(LR.po.ve == T){
      LR.insert <- LR.pol.veget(MP = Uz.MP_sl, MV = Uz.MV_sl,
                   Keep.taxa = Keep.taxa.2,
                   H = 600, W = 900, R2.pos = "bottomright",
                   Save.plot = "Figures/Uzbekistan/Vegetation/Compar_hist_MV_MP_uz_linear_all.pdf")
      
      LR.pol.veget(MP = Uz.MP_sl, MV = Uz.MV_sl,
                   Keep.taxa = c("Juniperus spp.", "Betula spp.", "Salix spp.", "Artemisia spp."),
                   H = 400, W = 400, R2.pos = "bottomright", Scale = "free",
                   Save.plot = "Figures/Uzbekistan/Vegetation/Compar_hist_MV_MP_uz_linear_over.pdf")
      
      LR.pol.veget(MP = Uz.MP_sl, MV = Uz.MV_sl,
                   Keep.taxa = c("Rosaceae", "Cyperaceae", "Fabaceae", "Tamarix spp."),
                   H = 400, W = 400, R2.pos = "bottomright", Scale = "free",
                   Save.plot = "Figures/Uzbekistan/Vegetation/Compar_hist_MV_MP_uz_linear_low.pdf")
      
      LR.insert.full <- LR.pol.veget(MP = Uz.MP_sl, MV = Uz.MV_sl,
                   Keep.taxa = Keep.taxa[-c(11,28,length(Keep.taxa))], R2.size = 3.5,
                   H = 1300, W = 1500, R2.pos = "bottomright", Scale = "free", 
                   Save.plot = "Figures/Uzbekistan/Vegetation/Compar_hist_MV_MP_uz_linear_full.pdf")
      }
    
    #### Procrustean Co-innertia Analysis ####
    PCoI = T
    if(PCoI == T){
      PCoI.Uz <- PCoI.vegetation(MV = Uz.MV_sl, MP = Uz.MP_sl, Meco = Uz.eco, Show.errors = T, Show.outliers = T, 
                                 Symbol.pos = c(.9,.9,.15), Stats.pos = c(-5.2,6), Show.site.lab = F,
                                 Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen_vs_veget.png",
                                 W = 900, H = 400, Save.plot = "Figures/Uzbekistan/Vegetation/Procrustes_errors_veget_pollen_uz.pdf")}
    
    #### Davis Index ####
    Davis.Uz = F
    if(Davis.Uz == T){
      Uz.MP_sl <- Uz.MP_sl[names(Uz.MP_sl) %in% Keep.common.site]
      Uz.MV_sl <- Uz.MV_sl[names(Uz.MV_sl) %in% Keep.common.site]
      Uz.DavisIdx <- Davis.Index(MP = Uz.MP_sl, MV = Uz.MV_sl, Add.group = T, Add.Rval = T, R.val.rel = "Poaceae", #Fabaceae
                                 Remove.taxa = c(50, 51, 82, 84, 86, 94), Bar.width = .75, Sp.lab.size = 13,Grp.Lab.size = 5,
                                 Save.path = "Resultats/Uzbekistan/Pollen/Surface/Uz_Davis_index.csv", 
                                 H = 1400, W = 700, Save.plot = "Figures/Uzbekistan/Pollen/Surface/Uz_Davis_index.pdf")}
    
    #### Mapping MV vs. MP ####
    Mapping.MV.MP = F
    if(Mapping.MV.MP == T){
      Map1 <- Map.samples(
        Samples.list = list(Vegetation.plots = TUSD.coord, Pollen.samples = TUSD.coord),
        Samples.pollen =  list(Pollen.samples = Uz.MP_sl, Vegetation.plots = Uz.MV_sl),
        Limites = c(55.9,75.2,36.5,45.7), Insert.loc = c(54.76,63,35.9,42),
        Map.extend = "TUSD", Insert.LR = LR.insert,
        Display.taxa = Keep.taxa.2, Taxa.italic = T,
        Save.plot = "Figures/Uzbekistan/Vegetation/Map_TUSD_pollen_veget_all.pdf", W = 1200, H = 850)
    
      Map2 <- Map.samples(
        Samples.list = list(Vegetation.plots = TUSD.coord, Pollen.Samples = TUSD.coord),
        Samples.pollen =  list(Vegetation.plots = Uz.MV_sl, Pollen.Samples = Uz.MP_sl),
        Limites = c(55.5,76,36.5,45.7),
        Map.extend = "TUSD",
        Display.taxa = c("Fabaceae", "Cyperaceae", "Rosaceae", "Tamarix spp.",  "Juglans regia", "Poaceae"),
        Save.plot = "Figures/Uzbekistan/Vegetation/Map_TUSD_pollen_veget_low.pdf", W = 1400, H = 700)

      Map3 <- Map.samples(
        Samples.list = list(Vegetation.plots = TUSD.coord, Pollen.Samples = TUSD.coord),
        Samples.pollen =  list(Pollen.Samples = Uz.MP_sl, Vegetation.plots = Uz.MV_sl),
        Limites = c(55.5,76,36.5,45.7),
        Map.extend = "TUSD",
        Display.taxa = c("Artemisia spp.", "Betula spp.", "Juniperus spp.", "Salix spp.", "Amaranthaceae", "Ephedra fragilis-type"),
        Save.plot = "Figures/Uzbekistan/Vegetation/Map_TUSD_pollen_veget_high.pdf", W = 1400, H = 700)
      
      Map3 <- Map.samples(
        Samples.list = list(Vegetation.plots = TUSD.coord, Pollen.Samples = TUSD.coord),
        Samples.pollen =  list(Pollen.Samples = Uz.MP_sl, Vegetation.plots = Uz.MV_sl),
        Limites = c(55.5,76,36.5,45.7), Ncol = 4, Taxa.italic = T, R2.size = 2,
        Map.extend = "TUSD", Display.taxa = Keep.taxa[-c(28,length(Keep.taxa))],
        Insert.LR = LR.insert.full, Insert.loc = c(54.5,63,35.9,42),
        Save.plot = "Figures/Uzbekistan/Vegetation/Map_TUSD_pollen_veget_full.pdf", W = 1350, H = 1800)
      }
    }
  }

#### Azerbaijan ####
Azer.veget = F
if(Azer.veget == T){
  #### Calculations ####
  Calculations = T
  if(Calculations == T){
    Errors = F
    Az.veget.10m <- Import.releves.10m.unitable(Releve.path = "Import/Azerbaijan/Vegetation/Releves/Azer_veget_p1.csv", 
                                                Site.path = "Import/Azerbaijan/Vegetation/Releves/Azer_site_releve.csv", 
                                                Com.path = "Import/Azerbaijan/Vegetation/Releves/Azer_community_p1.csv",
                                                Csv.sep = ",", Error.display = Errors,
                                                Plus.convert = "0.5")
    Az.index.P1 <- Import.index(Herbier = Herbier,
                                  Herbier.country = "Azerbaijan",
                                  MV = Az.veget.10m, TNRS.Accepted = T,
                                  Csv.sep = ",", Auto.add.miss = T, Simplify.Vtype = F, Add.ubiquist = T,
                                  Inerte.path = "Import/Azerbaijan/Vegetation/Indexes/List_inerte.csv",
                                  Save.path = "Import/Azerbaijan/Vegetation/Indexes/Index_from_Azeri_Herbier_auto.csv",
                                  Error.display = T)
  
    # # Recherche des Vtypes manquants ?
    # A <- as.vector(read.table("Import/Azerbaijan/Vegetation/Indexes/Index_from_Azeri_Herbier_auto_missing_in_herbier.csv"))
    # Search.releve(Az.veget.10m,Taxa = "Astrantia geante")
    # print(lapply(Az.veget.10m, function(A) row.names(A)[row.names(A) %in% Still.miss$x]))
    print(row.names(Az.veget.10m$MAZT5M03))
  
    Az.veget.10m.TN <- Convert.releves.10m(MV = Az.veget.10m, Remove.abiot = T, Tree.Herb = F,
                                        Taxa.name = T, Family.name = F, Pollen.type = F, Merge.releve = F, 
                                        Error.display = Errors, Index = Az.index.P1)
    
    
    Az.veget.10m.PT_ss <- Convert.releves.10m(MV = Az.veget.10m, Remove.abiot = T, Tree.Herb = F,
                                        Taxa.name = F, Family.name = F, Pollen.type = T, Merge.releve = F,
                                        Error.display = F, Index = Az.index.P1, PT = "ss")
    
    Az.veget.10m.PT_sl <- Convert.releves.10m(MV = Az.veget.10m, Remove.abiot = T, Tree.Herb = F,
                                        Taxa.name = F, Family.name = F, Pollen.type = T, Merge.releve = F,
                                        Error.display = F, Index = Az.index.P1, PT = "sl")
    
    Az.veget.10m.mean.TN <- Mean.veget.model(MV = Az.veget.10m.TN, Model = "SQTD", # SP SQTD
                                          Releve.design = "Import/Azerbaijan/Vegetation/Indexes/Vegetation_model.csv",
                                          Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")
    
    Az.veget.10m.mean.PT_ss <- Mean.veget.model(MV = Az.veget.10m.PT_ss, Model = "SQTD", # SP SQTD
                                          Releve.design = "Import/Azerbaijan/Vegetation/Indexes/Vegetation_model.csv",
                                          Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")
    
    Az.veget.10m.mean.PT_sl <- Mean.veget.model(MV = Az.veget.10m.PT_sl, Model = "SQTD", # SP SQTD
                                          Releve.design = "Import/Azerbaijan/Vegetation/Indexes/Vegetation_model.csv",
                                          Displot = F, Blind.com.merge = T, Error.display = F, Csv.sep = ",")
    
    #### Statistique ####
    # print(row.names(Az.veget.10m.mean.TN[rowSums(Az.veget.10m.mean.TN) == 0,]))
    print(paste("Nb vegetation plots:", ncol(Az.veget.10m.mean.TN)))
    print(paste("Nb Vtypes:", length(unique(Az.index.P1$Vtype))))
    print(paste("Nb Family:", length(unique(Az.index.P1$Family))))
    print(paste("Nb Genus:", length(unique(Az.index.P1$Genus))))
    print(paste("Nb Species:", length(unique(Az.index.P1$Species))))
    print(paste("Nb PT-sl:", length(unique(Az.index.P1$PT.sl))))
    print(paste("Nb PT-ss:", length(unique(Az.index.P1$PT.ss))))
    Main.taxa <- sapply(Az.veget.10m.mean.TN, function(x) paste(row.names(Az.veget.10m.mean.TN[x > 5,]), collapse = ", "))
    
    write.table(Main.taxa, "Resultats/Azerbaijan/Vegetation/Main_taxa_Az.csv", sep = ",", col.names = NA)
    write.table(Az.veget.10m.mean.TN, "Resultats/Azerbaijan/Vegetation/MV_Az.csv", sep = ",", col.names = NA)
    write.table(Az.veget.10m.mean.PT_ss, "Resultats/Azerbaijan/Vegetation/MV_Az_PT_ss.csv", sep = ",", col.names = NA)
    write.table(Az.veget.10m.mean.PT_sl, "Resultats/Azerbaijan/Vegetation/MV_Az_PT_sl.csv", sep = ",", col.names = NA)
    }
  else{
    Az.veget.10m.mean.TN <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az.csv", sep = ",", header = T, row.names = 1)
    Az.veget.10m.mean.PT_ss <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az_PT_ss.csv", sep = ",", header = T, row.names = 1)
    Az.veget.10m.mean.PT_sl <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az_PT_sl.csv", sep = ",", header = T, row.names = 1)
    Az.eco <- read.table("Import/Azerbaijan/Site/Az_surf_samples.csv", sep = ",", header = T, row.names = 1)
  }
  Sort.eco.Az <-  c("Azerbaijan shrub desert and steppe", "Caucasus mixed forests", "Caspian Hyrcanian mixed forests")
  #### Verif metadata list samples ####
  Check.metada = T
  if(Check.metada == T){
    Meco.all <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_V2.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$GDGT == "x"] <- paste(Meco.all$Proxy[Meco.all$GDGT == "x"], "G", sep = "")
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    List.samp.Veg.plot.T <- names(Az.veget.10m.mean.TN)
    List.samp.Veg.plot.expected <- row.names(Meco.all[Meco.all$Veg.plot == "x",])
    List.samp.Veg.plot.expected <- List.samp.Veg.plot.expected[grepl("AZ", List.samp.Veg.plot.expected)]
    
    if(length(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T)) >= 1){
      print("The following samples are missing from the Veg.plot matrix :")
      print(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T))}
    
    if(length(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)) >= 1){
      print("The following samples are missing from the metadata :")    
      print(sort(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)))}
    
  }
  
  
  #### Twinspan ####
  Twinspan.az = F
  if(Twinspan.az == T){
    Az.Tw.PT <- Plot.twinspan(MV = Az.veget.10m.mean.PT_ss,
                  H = 950, W = 700,  Max.species = 50, Sp.lab.size = 8, Site.lab.size = 8,
                  Save.plot = "Figures/Azerbaijan/Vegetation/Twinspan_Az_PT.pdf")

    Az.Tw.TN <- Plot.twinspan(MV = Az.veget.10m.mean.TN,
                           # Max.species = 30,
                           H = 3500, W = 700, Sp.lab.size = 4,
                           Save.plot = "Figures/Azerbaijan/Vegetation/Twinspan_Az_Taxa_total.pdf")
    
    Az.Tw.TN <- Plot.twinspan(MV = Az.veget.10m.mean.TN,
                           Max.species = 50,
                           H = 950, W = 700, Sp.lab.size = 8, Site.lab.size = 8,
                           Save.plot = "Figures/Azerbaijan/Vegetation/Twinspan_Az_Taxa_50_best.pdf")
    
    
    Az.Tw.TN <- Az.veget.10m.mean.TN[row.names(Az.veget.10m.mean.TN) %in% Az.Tw.TN,]
    write.table(Az.Tw.TN, "Resultats/Azerbaijan/Vegetation/MV_Az_TN_filter_twinspan.csv", sep = ",", col.names = NA)
    }
  else{MV.Az.filter <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az_TN_filter_twinspan.csv", sep = ",", header = T, row.names = 1)}
  
  #### Diag veget ####
  Diag.veget.Az = F
  if(Diag.veget.Az == T){
    MV.Az.filter <- Hist.veget(MV = Az.veget.10m.mean.TN,
                             Ordin.path = "Import/Azerbaijan/Site/Az_surf_samples.csv",
                             # Ordin.path = "Resultats/Azerbaijan/Vegetation/MV_Az_PCA.csv",
                             Save.plot = "Figures/Azerbaijan/Vegetation/Hist_veget_Az.pdf",
                             Index = Az.index.P1,
                             Csv.sep = ",",
                             Max_seuil = 15,
                             AP.NAP = F, H = 800, W = 1200,
                             Sort = "MAP"  # Altitude, MAP, MAAT, Latitude, Longitude, Ordin1, Ordin2
                             )
    
    MV.Az.filter <- data.frame(t(MV.Az.filter))
    write.table(MV.Az.filter, "Resultats/Azerbaijan/Vegetation/MV_Az_TN_filter_hist.csv", sep = ",", col.names = NA)
    }
  # else{MV.Az.filter <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az_TN_filter_hist.csv", sep = ",", header = T, row.names = 1)}
  
  #### PCA / CA / RDA ####
  PCA.disp = F
  if(PCA.disp == T){
    #### Facet settings ####
    H = 900
    W = 1600
    Save.plot = "Figures/Azerbaijan/Vegetation/PCA_RDA_Az.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,2))
    
    #### PCA veget ####
    PCA.MV.Az <- PCA.vegetation(MV.Az.filter,
                                Cluster.path = Az.eco, Cluster.groups = "Ecosystem", Annot = "(A)",
                                Csv.sep =",", Simple.title = T, Show.text = T, Scale.PCA = 1,
                                transp_OK = T, cluster.from.PCA = F, Display.legends = T,
                                Save.path = "Resultats/Azerbaijan/Vegetation/MV_Az.csv",
                                Sort.eco = Sort.eco.Az,
                                Leg.loc = "bottomleft", Symbol.loc = c(-1.3, 2.5, .8),
                                Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                Type.samples = "Vegetation plots")
    
    #### RDA veget ####
    RDA.MV.Az <- RDA.vegetation(MV.Az.filter, MClim = Az.eco,  Cluster.path = Az.eco,
                                Choose.clim = c("Altitude", "MAAT", "MAP", "TS"),
                                Cluster.groups = "Ecosystem", Annot = "(B)",
                                Display.legends = F, Simple.title = T,
                                Csv.sep =",",Leg.loc2 = "bottomleft",
                                transp_OK = F, Scale.sites = 3, Scale.taxa = 3,
                                # Manu.lim = c(-1.6,1.3,-1.6,1.3),
                                Symbol.loc = c(-1.5, 1.08, .55),
                                Save.path = "Resultats/Azerbaijan/Vegetation/MV_Az.csv",
                                Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                Type.samples = "Vegetation plots",
                                Sort.eco = Sort.eco.Az
                                )

    #### Results ####
    PC1.ordin <- data.frame(PCA.MV.Az)
    PC1.ordin$Ordin2 <- match(PC1.ordin[[c(1)]], sort(PC1.ordin[[c(2)]]))
    write.table(PC1.ordin, "Resultats/Azerbaijan/Vegetation/MV_Az_PCA.csv", sep = ",", col.names = NA)

    dev.off()
    par(mfrow=c(1,1))
  }
  
  #### LDA ####
  LDA.Az = F
  if(LDA.Az == T){
    LDA.MV.Az <- LDA.vegetation(MV = data.frame(t(MV.Az.filter)), Cluster = "Ecosystem", Cluster.path = Az.eco,
                                H = 600, W = 600, Save.plot = "Figures/Azerbaijan/Vegetation/LDA_Az.pdf")}
  
  #### CA + dendrogramme ####
  CCA = F
  if(CCA == T){
    #### Correspondance analysis (CA) ####
    CA.MV.Az <- CA.vegetation(MV = MV.Az.filter, H = 1000, W = 1000,
                              Save.plot = "Figures/Azerbaijan/Vegetation/CA_MV_Az.pdf")
  
    #### Dendrogramme #### 
    Hclust.MV.Az <- Hclust.vegetation(MV = data.frame(t(MV.Az.filter)), H = 1600, W = 800,
                                     Save.plot = "Figures/Azerbaijan/Vegetation/Dendro_veget_Az.pdf")
  }
  
  #### LR climate ####
  LR.climat = F
  if(LR.climat == T){
    LR.clim.veget(MT = list(Azerbaijan = Az.veget.10m.mean), Meco = list(Azerbaijan = Az.eco),
                  Keep.taxa = c("Poaceae", "Amaranthaceae", "Cyperaceae", "Artemisia sp."),
                  Keep.clim = c("Altitude", "MAP", "MAAT", "TS"),
                  H = 1200, W = 1200, Strip.lab = F, R2.pos = "bottomleft",
                  Save.plot = "Figures/Azerbaijan/Vegetation/LRveg_clim_Az.pdf")
  }
}

#### Iran ####
Iran.veget = F
if(Iran.veget == T){
  #### Calculations ####
  Calculations = F
  if(Calculations == T){
    library(TNRS)
    #### Taxa list cleaning ####
    Compar.medhi.Lucas = F
    if(Compar.medhi.Lucas == T){
      TL.Lucas  <- data.frame(read.csv(file="Import/Iran/Vegetation/Indexes/working_files/Taxa_list_Golestan_Lucas.csv",sep=",",dec=".", header=T, row.names=1))
      TL.Medhi  <- data.frame(read.csv(file="Import/Iran/Vegetation/Indexes/working_files/Taxa_list_Golestan.csv",sep=",",dec=".", header=T, row.names=1))
      Common.types <- intersect(TL.Lucas$Species, TL.Medhi$Species)
      From.Lucas.not.used.by.Medhi <- setdiff(TL.Lucas$Species, TL.Medhi$Species)
      TL.Lucas.TNRS <- TNRS(TL.Lucas$Species)
      TL.Mehdi.TNRS <- TNRS(TL.Medhi$Species)
      TL.Mehdi.TNRS <- TL.Mehdi.TNRS[c(1:3,5:7,12:15,33:35,41)]
      write.table(TL.Lucas.TNRS, "Import/Iran/Vegetation/Indexes/working_files/Taxa_list_Golestan_Lucas_clean.csv", sep = ",", col.names = NA)
      write.table(TL.Mehdi.TNRS, "Import/Iran/Vegetation/Indexes/working_files/Taxa_list_Golestan_Mehdi_clean.csv", sep = ",", col.names = NA)
      }
    
    Building.index.corresp.pollen = T
    if(Building.index.corresp.pollen == T){
      TL.Ir  <- data.frame(read.csv(file="Import/Iran/Vegetation/Indexes/Taxa_list_Golestan.csv",sep=",",dec=".", header=T, row.names=1))
      TL.Ir.TNRS <- TNRS(TL.Ir$Species)
      TL.Ir.TNRS <- TL.Ir.TNRS[c(1:3,5:7,12:15,33:35,41)]
      TL.Ir.TNRS <- TL.Ir.TNRS[c(2,14,9,12)]
      names(TL.Ir.TNRS)[1] <- "Species"
      TL.Ir <- left_join(TL.Ir, TL.Ir.TNRS, by = "Species")
      H.Ir <- Herbier[Herbier$Accepted_name %in% TL.Ir$Accepted_name, c("Accepted_name", "PT.ss", "PT.sl", "GrowthForm")]
      H.Ir <- H.Ir[!duplicated(H.Ir),]
      Not.in.H <- TL.Ir[!TL.Ir$Accepted_name %in% Herbier$Accepted_name,]
      H.Ir.gen <- Herbier[Herbier$Genus %in% unique(Not.in.H$Genus_matched), c("Genus", "PT.ss", "PT.sl", "GrowthForm")]
      names(H.Ir.gen)[1] <- "Accepted_name"
      H.Ir <- rbind(H.Ir, H.Ir.gen)
      H.Ir <- H.Ir[!duplicated(H.Ir$Accepted_name),]
      TL.Ir <- left_join(TL.Ir, H.Ir, by = "Accepted_name")
      
      names(TL.Ir) <- c("Vtype", "Names", "Variety", "Family", "Genus", "Accepted_name", "PT.ss", "PT.sl", "GrowthForm")
      TL.Ir$Type <- "Veget"
      TL.Ir$Type[TL.Ir$Vtype == "moss"] <- "Inerte"
      TL.Ir$Species <- gsub(".*\\s", "", TL.Ir$Accepted_name)
      TL.Ir <- TL.Ir[c(1,2,4,5,11,3,7:9,10,6)]
      
      H.Ir.gen2 <- H.Ir.gen[H.Ir.gen$Accepted_name %in% TL.Ir$Genus[is.na(TL.Ir$PT.ss)],]
      H.Ir.gen2 <- H.Ir.gen2[!duplicated(H.Ir.gen2$Accepted_name),]
      TL.Ir[is.na(TL.Ir$PT.ss),c("PT.ss", "PT.sl", "GrowthForm")] <- H.Ir.gen2[match(TL.Ir$Genus[is.na(TL.Ir$PT.ss)], H.Ir.gen2$Accepted_name),c(2:4)]
      TL.Ir$Family[grep("aceae", TL.Ir$Species)] <- TL.Ir$Species[grep("aceae", TL.Ir$Species)]
      TL.Ir$Genus[grep("aceae", TL.Ir$Species)] <- NA
      TL.Ir$Species[grep("aceae", TL.Ir$Species)] <- NA
      
      Sp.corect <- TL.Ir$Names[grepl("subsp", TL.Ir$Variety)| grepl("var", TL.Ir$Variety)]
      Sp.corect <- gsub("\\s[^.]*$", "", Sp.corect)
      Sp.corect <- gsub(" subsp.", "", Sp.corect)
      Sp.corect <- gsub(" var.", "", Sp.corect)
      Sp.corect <- gsub(".*\\s", "", Sp.corect)
      TL.Ir$Species[grepl("subsp", TL.Ir$Variety)| grepl("var", TL.Ir$Variety)] <- Sp.corect
      
      GrowthForm.ACA <- readRDS("/home/lucas.dugerdil/Documents/Recherche/Data_Bases/Traits/TRY_dec_2019/Extraction/TRY_GrowthForm_ACA.Rds")
      GrowthForm.Ir <- GrowthForm.ACA[GrowthForm.ACA$species %in% TL.Ir$Accepted_name[is.na(TL.Ir$GrowthForm)],]
      GrowthForm.Ir <- GrowthForm.Ir[GrowthForm.Ir$GrowthForm != "Unknown",]
      TL.Ir$GrowthForm[is.na(TL.Ir$GrowthForm)] <- GrowthForm.Ir$GrowthForm[match(TL.Ir$Accepted_name[is.na(TL.Ir$GrowthForm)], GrowthForm.Ir$species)]
      write.table(TL.Ir, "Import/Iran/Vegetation/Indexes/Index_from_Ir_Herbier_auto.csv", sep = ",", col.names = NA)
    }
    else{TL.Ir  <- data.frame(read.csv(file="Import/Iran/Vegetation/Indexes/Index_from_Ir_Herbier_clean.csv",sep=",",dec=".", header=T, row.names=1))}
    
    #### Vegetation matrix cleaning ####
    All.Error.to.show = F
    Ir.veget <- read.table("Import/Iran/Vegetation/MV_Ir.csv", sep = ",", header = T, row.names = 1)
    Ir.veget[Ir.veget == 0] <- 0.01
    Ir.veget[is.na(Ir.veget)] <- 0
    Ir.veget$G47 <- rowSums(Ir.veget[grepl("G47", names(Ir.veget))])
    Ir.veget <- Ir.veget[!grepl("G47.", names(Ir.veget))]
    Ir.veget <- list(A = Ir.veget)
    Ir.index.TN <- TL.Ir[c(1,3,4,2,8,7,9,10)]
    names(Ir.index.TN)[4] <- "Species"
    
    Ir.veget.TN <- Convert.releves.10m(MV = Ir.veget, Remove.abiot = T, Tree.Herb = F,
                                           Taxa.name = T, Family.name = F, Pollen.type = F, Keep.unknown.type = T,
                                           Merge.releve = F, Error.display = All.Error.to.show, Index = Ir.index.TN)
    
    Ir.veget.PT_ss <- Convert.releves.10m(MV = Ir.veget, Remove.abiot = T, Tree.Herb = F, Keep.unknown.type = F,
                                              Taxa.name = F, Family.name = F, Pollen.type = T, PT = "ss",
                                              Merge.releve = F, Error.display = F, Index = Ir.index.TN)
    
    Ir.veget.PT_sl <- Convert.releves.10m(MV = Ir.veget, Remove.abiot = T, Tree.Herb = F, Keep.unknown.type = F, 
                                              Taxa.name = F, Family.name = F, Pollen.type = T, PT = "sl",
                                              Merge.releve = F, Error.display = F, Index = Ir.index.TN)
    
    Ir.veget.TN <- round(Ir.veget.TN$A, digits = 2)
    Ir.veget.PT_sl <- round(Ir.veget.PT_sl$A, digits = 2)
    Ir.veget.PT_ss <- round(Ir.veget.PT_ss$A, digits = 2)
    
    #### Statistique ####
    print(row.names(Ir.veget.TN[rowSums(Ir.veget.TN) == 0,]))
    print(paste("Nb vegetation plots:", ncol(Ir.veget.TN)))
    print(paste("Nb Vtypes:", length(unique(Ir.index.TN$Vtype))))
    print(paste("Nb Family:", length(unique(Ir.index.TN$Family))))
    print(paste("Nb Genus:", length(unique(Ir.index.TN$Genus))))
    print(paste("Nb Species:", length(unique(Ir.index.TN$Species))))
    print(paste("Nb PT-sl:", length(unique(Ir.index.TN$PT.sl))))
    print(paste("Nb PT-ss:", length(unique(Ir.index.TN$PT.ss))))

    write.table(Ir.veget.TN, "Resultats/Iran/Vegetation/MV_Ir.csv", sep = ",", col.names = NA)
    write.table(Ir.veget.PT_sl, "Resultats/Iran/Vegetation/MV_Ir_PT_sl.csv", sep = ",", col.names = NA)
    write.table(Ir.veget.PT_ss, "Resultats/Iran/Vegetation/MV_Ir_PT_ss.csv", sep = ",", col.names = NA)
    saveRDS(Ir.veget.TN, "Resultats/Iran/Vegetation/MV_Ir.Rds")
    saveRDS(Ir.veget.PT_sl, "Resultats/Iran/Vegetation/MV_Ir_PT_sl.Rds")
    saveRDS(Ir.veget.PT_ss, "Resultats/Iran/Vegetation/MV_Ir_PT_ss.Rds")
    }
  else{
    Ir.veget.TN <- readRDS("Resultats/Iran/Vegetation/MV_Ir.Rds")
    Ir.veget.PT_sl <- readRDS("Resultats/Iran/Vegetation/MV_Ir_PT_sl.Rds")
    Ir.veget.PT_ss <- readRDS("Resultats/Iran/Vegetation/MV_Ir_PT_ss.Rds")
    }
  
  Ir.eco <- read.table("Import/Iran/Site/Ir_SS_simple.csv", sep = ",", header = T, row.names = 1)
  # Sort.eco.Ir = c("Desert-steppe", "Mountain steppes meadows", "Mountain Forest-steppes", "Forest-steppes", "Forest")
  # Iran.col = c("firebrick3", "goldenrod1", "#6789CE", "#9FBB67", "darkgreen")
  Sort.eco.Ir = c("Desert-steppe", "Mountain steppe-meadow", "Woodland", "Forest-steppe", "Forest")
  Iran.col = c("#CD2626", "#FFC125", "#9FBB67", "#6789CE", "#006400")
  MP.Ir.filter <- read.table("Resultats/Iran/Pollen/Surface/MP_Ir_filtered.csv", sep = ",", header = T, row.names = 1)
  Ir.MP_sl  <- data.frame(read.csv(file="Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_coarse.csv",sep=",",dec=".", header=T, row.names=1))
  Ir.MP_ss  <- data.frame(read.csv(file="Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_fine.csv",sep=",",dec=".", header=T, row.names=1))
  
  #### Verif metadata list samples ####
  Check.metada = F
  if(Check.metada == T){
    Meco.all <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_V2.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$GDGT == "x"] <- paste(Meco.all$Proxy[Meco.all$GDGT == "x"], "G", sep = "")
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    List.samp.Veg.plot.T <- names(Ir.veget.TN)
    List.samp.Veg.plot.expected <- row.names(Meco.all[Meco.all$Veg.plot == "x",])
    List.samp.Veg.plot.expected <- List.samp.Veg.plot.expected[grepl("Ir", List.samp.Veg.plot.expected) | grepl("TA", List.samp.Veg.plot.expected)]
    
    if(length(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T)) >= 1){
      print("The following samples are missing from the Veg.plot matrix :")
      print(setdiff(List.samp.Veg.plot.expected, List.samp.Veg.plot.T))}
    
    if(length(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)) >= 1){
      print("The following samples are missing from the metadata :")    
      print(sort(setdiff(List.samp.Veg.plot.T,List.samp.Veg.plot.expected)))}
    
  }
  
  #### Twinspan ####
  Twinspan.Ir = F
  if(Twinspan.Ir == T){
    Only.best = T
    if(Only.best == F){
      Ir.Tw.PT_ss <- Plot.twinspan(MV = Ir.veget.PT_ss, 
                                   Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2, Show.indicators = T,
                                   H = 950, W = 700,  Max.species = 50, Sp.lab.size = 8, Site.lab.size = 8,
                                   Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                   Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_PT_ss.pdf")
      
      Ir.Tw.PT_sl <- Plot.twinspan(MV = Ir.veget.PT_sl, 
                                   Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2, Show.indicators = T,
                                   H = 950, W = 700,  Max.species = 50, Sp.lab.size = 8, Site.lab.size = 8,
                                   Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                   Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_PT_sl.pdf")
      
      Ir.Tw.TN <- Plot.twinspan(MV = Ir.veget.TN,
                                # Max.species = 30,
                                H = 3500, W = 700, Sp.lab.size = 4, 
                                Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2, Show.indicators = T,
                                Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_Taxa_total.pdf")
      Ir.Tw.TN.ind <- Plot.twinspan(MV = Ir.veget.TN, Select.species = "indicator", # indicator ou leading
                                    Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2, Show.indicators = F,
                                    H = 950, W = 700, Sp.lab.size = 8, Site.lab.size = 8, Eco.col = setNames(data.frame(t(Iran.col)), Sort.eco.Ir),
                                    Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_Taxa_indicator_sp.pdf")
      
      Ir.Tw.TN.lead <- Plot.twinspan(MV = Ir.veget.TN, Select.species = "leading", # indicator ou leading
                                     Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2, Show.indicators = T,
                                     H = 950, W = 700, Sp.lab.size = 8, Site.lab.size = 8, Eco.col = setNames(data.frame(t(Iran.col)), Sort.eco.Ir),
                                     Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_Taxa_leading_sp.pdf")
    }
    
    Ir.Tw.TN <- Plot.twinspan(MV = Ir.veget.TN, #Select.species = "indicator", # indicator ou leading
                              Max.species = 100, Show.indicators = T, Nb.cluster = 2,
                              Add.dendro = T, Meco = Ir.eco, Eco.dot.size = 2,
                              H = 950, W = 700, Sp.lab.size = 6, Site.lab.size = 8, Eco.col = setNames(data.frame(t(Iran.col)), Sort.eco.Ir),
                              Save.Indicator = "Resultats/Iran/Vegetation/Indicator_species.Rds",
                              Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                              Save.plot = "Figures/Iran/Vegetation/Twinspan_Ir_Taxa_100_best.pdf")
    
    
    MV.Ir.filter <- Ir.veget.TN[row.names(Ir.veget.TN) %in% Ir.Tw.TN,]
    write.table(MV.Ir.filter, "Resultats/Iran/Vegetation/MV_Ir_TN_filter_twinspan.csv", sep = ",", col.names = NA)
  }
  else{MV.Ir.filter <- read.table("Resultats/Iran/Vegetation/MV_Ir_TN_filter_twinspan.csv", sep = ",", header = T, row.names = 1)}
  Ir.IndTax <- readRDS("Resultats/Iran/Vegetation/Indicator_species.Rds")
  
  #### Camembert ####
  Cam.Ir = F
  if(Cam.Ir == T){
    Ir.MV.piechart <- Veget.piechart(Ir.veget.TN, 
                                     # Select.plot = grepl("MIrT3S0",names(Ir.veget)),
                                     # Select.plot = c("MIrT3S01"),
                                     # Select.plot = c("MIrT3M11", "MIrT5M03", "MIrT3S01"),
                                     Plotly = T,
                                     Plotly.all = T,
                                     H = 1000, W = 1000, Save.plot = "Figures/Iran/Vegetation/MV.pdf")}
  
  #### Diag veget ####
  Diag.veget.Ir = F
  if(Diag.veget.Ir == T){
    MV.Ir.filter <- Hist.veget(MV = Ir.veget.PT_ss,
                               Ordin.path = "Import/Iran/Site/Ir_SS_simple.csv",
                               # Ordin.path = "Resultats/Iran/Vegetation/MV_Ir_PCA.csv",
                               Save.plot = "Figures/Iran/Vegetation/Hist_veget_Ir.pdf",
                               Index = Ir.index.P1,
                               Csv.sep = ",",
                               Max_seuil = 7,
                               AP.NAP = F, H = 800, W = 1200,
                               Sort = "MAP"  # Altitude, MAP, MAAT, Latitude, Longitude, Ordin1, Ordin2
    )
    
    MV.Ir.filter <- data.frame(t(MV.Ir.filter))
    write.table(MV.Ir.filter, "Resultats/Iran/Vegetation/MV_Ir_TN_filter_hist.csv", sep = ",", col.names = NA)
  }
  # else{MV.Ir.filter <- read.table("Resultats/Iran/Vegetation/MV_Ir_TN_filter_hist.csv", sep = ",", header = T, row.names = 1)}
  
  
  #### PCA / RDA ####
  PCA.disp = F
  if(PCA.disp == T){
    #### Settings ####
    H = 1000
    W = 1000
    Save.plot = "Figures/Iran/Vegetation/PCA_RDA_Ir.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(2,2))
    My_clus <- "Ecosystem"
    #### PCA veget ####
    # row.names(MV.Ir.filter) <- vegan::make.cepnames(row.names(MV.Ir.filter))
    PCA.MV.Ir <- PCA.vegetation(MV.Ir.filter, 
                                Vector.show = Ir.IndTax,
                                Nb.contrib = 21,
                                Cluster.path = Ir.eco, Cluster.groups = "Ecosystem", Annot = "(A)",
                                Csv.sep =",", Simple.title = T, Show.text = F, Scale.PCA = 1,  Manu.lim = c(-2.7,1.9,-2.5,2.5),
                                transp_OK = T, cluster.from.PCA = F, Display.legends = T, Leg.size = 1.2,
                                Save.path = "Resultats/Iran/Vegetation/MV_Ir.csv", Color.choice =  Iran.col,
                                Sort.eco = Sort.eco.Ir, Leg.loc = "bottomleft", Symbol.loc = c(-1.3, 2.5, .8),
                                # Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                Type.samples = "Vegetation plots")

    #### RDA veget ####
    RDA.MV.Ir <- RDA.vegetation(MV.Ir.filter, MClim = Ir.eco,
                                Choose.clim = c("MAAT", "MAP", "AI", "Altitude"), 
                                Cluster.path = Ir.eco, Cluster.groups = "Ecosystem", Annot = "(B)",
                                Display.legends = F, Simple.title = T,
                                Vector.show = Ir.IndTax, Nb.contrib = 21,
                                Csv.sep =",",Leg.loc2 = "bottomright",
                                transp_OK = F, Scale.sites = 3, Scale.taxa = 3,
                                Manu.lim = c(-1.6,1.4,-1.6,1.5), Symbol.loc = c(-1.5, 1.08, .55),
                                Save.path = "Resultats/Iran/Pollen/Surface/MP_Ir.csv", Color.choice =  Iran.col,
                                # Symbol.path = "Figures/ACA/Trait/Workflow/export/symbole_Veget.png",
                                Type.samples = "Vegetation plots",
                                Sort.eco = Sort.eco.Ir)
    
    #### PCA pollen ####
    source("Scripts/Pollen_surf.R")
    PCA.pollen.Ir <- PCA.pollen.surf(MP.Ir.filter,               # Import Matrice pour la PCA
                                     Cluster.path = Ir.eco, Cluster.groups = "Ecosystem",
                                     Csv.sep =",", Simple.title = T, Show.text = F, Annot = "(C)",
                                     transp_OK = T, cluster.from.PCA = F, Scale.PCA = 1, Color.choice =  Iran.col,
                                     Manu.lim = c(-.8,2.4,-2,3.2), Display.legends = F,
                                     Symbol.loc = c(-2.42, 1.26, .9), Nb.contrib = 30,
                                     Save.path = "Resultats/Iran/Pollen/Surface/MP_Ir.csv",
                                     # Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png",
                                     Sort.eco = Sort.eco.Ir, Leg.loc2 = "bottomleft",
                                     Type.samples = "Pollen")
    
    #### RDA pollen ####
    RDA.Ir.P1 <- RDA.pollen.surf(MP.Ir.filter, MClim = Ir.eco,
                                 Choose.clim = c("MAAT", "MAP", "AI", "Altitude"), 
                                 Cluster.path = Ir.eco, Cluster.groups = "Ecosystem",
                                 Display.legends = F, Simple.title = T, Color.choice =  Iran.col,
                                 Csv.sep =",",Leg.loc2 = "bottomleft", Annot = "(D)",
                                 Manu.lim = c(-0.55,0.8,-0.9,0.5), Nb.contrib = 30,
                                 transp_OK = F, Scale.sites = 1, Scale.taxa = 2,
                                 Save.path = "Resultats/Iran/Pollen/Surface/MP_Ir.csv",
                                 Type.samples = "Pollen", 
                                 # Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png",
                                 Sort.eco = Sort.eco.Ir)
    
    #### Results ####
    PC1.ordin <- data.frame(PCA.MV.Ir)
    PC1.ordin$Ordin2 <- match(PC1.ordin$PC1, sort(PC1.ordin$PC1))
    write.table(PC1.ordin, "Resultats/Iran/Vegetation/MV_Ir_PCA.csv", sep = ",", col.names = NA)
    
    dev.off()
    par(mfrow=c(1,1))
  }
  
  #### LDA ####
  LDA.Ir = F
  if(LDA.Ir == T){
    LDA.MV.Ir <- LDA.vegetation(MV = data.frame(t(MV.Ir.filter)), 
                                Cluster = "Ecosystem", Cluster.path = Ir.eco, Sort.cluster = Sort.eco.Ir,
                                H = 600, W = 600, Save.plot = "Figures/Iran/Vegetation/LDA_Ir.pdf")}
  
  #### CA + dendrogramme ####
  CCA = F
  if(CCA == T){
    #### Correspondance analysis (CA) ####
    CA.MV.Ir <- CA.vegetation(MV = MV.Ir.filter, H = 700, W = 1000, Nb.contrib = 25, Show.site.name = F,
                              Cluster.groups = "Ecosystem", Cluster.path = Ir.eco, Sort.cluster = Sort.eco.Ir,
                              Save.plot = "Figures/Iran/Vegetation/CA_MVfilt_Ir.pdf")
    
    row.names(Ir.veget.TN) <- vegan::make.cepnames(row.names(Ir.veget.TN))
    
    CA.MV.Ir <- CA.vegetation(MV = Ir.veget.TN, H = 700, W = 1000, Nb.contrib = 40, Show.site.name = F,
                              Cluster.groups = "Ecosystem", Cluster.path = Ir.eco, Sort.cluster = Sort.eco.Ir,
                              Save.plot = "Figures/Iran/Vegetation/CA_MV_Ir.pdf")
    
    #### Dendrogramme #### 
    Hclust.MV.Ir <- Hclust.vegetation(MV = data.frame(t(MV.Ir.filter)), H = 1600, W = 800,
                                      Save.plot = "Figures/Iran/Vegetation/Dendro_veget_Ir.pdf")
  }
  
  #### LR climate ####
  LR.climat = F
  if(LR.climat == T){
    LR.clim.veget(MT = list(Iran = Ir.veget.PT_ss), Meco = list(Iran = Ir.eco),
                  Keep.taxa = c("Amaranthaceae", "Quercus deciduous", "Artemisia spp.", "Carpinus betulus"),
                  Keep.clim = c("Altitude", "MAP", "MAAT", "TS"),
                  H = 900, W = 900, Strip.lab = F, R2.pos = "bottomright",
                  Save.plot = "Figures/Iran/Vegetation/LRveg_clim_Ir.pdf")
    
  }
  #### Compare pollen and veget ####
  Compare.pollen.veget = F
  if(Compare.pollen.veget == T){
    #### Import data ####
    Ir.pol.ind  <- data.frame(read.csv(file="Import/Iran/Pollen/Func_trans/Corresp_pollen_Ir_clean.csv",sep=",",dec=".", header=T, row.names=1))
    Ir.MV_sl <- Ir.veget.PT_sl
    Ir.MV_ss <- Ir.veget.PT_ss
    Clean.taxo <- Check.taxa.diff(Ir.MP_sl, Ir.MV_sl, type = "coarse", Display.message = F, Auto.homogeneise = T)
    Ir.MV_sl <- Clean.taxo[[1]]
    Ir.MP_sl <- Clean.taxo[[2]]
    Clean.taxo <- Check.taxa.diff(Ir.MP_ss, Ir.MV_ss, type = "fine", Display.message = F, Auto.homogeneise = T)
    Ir.MV_ss <- Clean.taxo[[1]]
    Ir.MP_ss <- Clean.taxo[[2]]
    
    
    #### Clean DB ####
    Keep.common.site <- intersect(names(Ir.MP_sl), names(Ir.MV_sl))
    Gol.coord <- Ir.eco[c(1,2, 50, 5)]
    Gol.coord <- Gol.coord[row.names(Gol.coord) %in% Keep.common.site,]
    names(Gol.coord) <- c("Long", "Lat", "Alt", "Eco")
    
    Ir.MP_sl <- Ir.MP_sl[!grepl("MIrT5M03", names(Ir.MP_sl))]
    Ir.MP_ss <- Ir.MP_ss[!grepl("MIrT5M03", names(Ir.MP_ss))]
    Ir.MV_sl <- Ir.MV_sl[!grepl("MIrT5M03", names(Ir.MV_sl))]
    Ir.MV_ss <- Ir.MV_ss[!grepl("MIrT5M03", names(Ir.MV_ss))]
    
    Keep.taxa = c("Quercus deciduous", "Carpinus betulus", 
                  "Rosaceae (shrub)", "Rosaceae (tree)",
                  "Alnus spp.", "Ulmus spp.", 
                  "Acer spp.", "Juniperus spp.",  
                  "Ephedra fragilis-type", 
                  "Apiaceae", 
                  "Cyperaceae", 
                  "Liliaceae", 
                  "Poaceae", 
                  "Cerealia-type", 
                  "Cardueae", "Galium-type",
                  "Rumex spp.", "Fabaceae", "Lamiaceae", "Boraginaceae", 
                  "Typha-type",
                  "Brassicaceae", "Cirsium-type", "Caryophyllaceae", "Convolvulus spp.", "Matricaria-type",
                  "Artemisia spp.", "Amaranthaceae", "Cousinia spp.", "Other")
    
    Keep.taxa.2 = c("Quercus deciduous", "Acer spp.", "Poaceae", "Juniperus spp.", "Artemisia spp.", "Amaranthaceae")
    #### Plots diag pol / veg ####
    Diag.po.ve = F
    if(Diag.po.ve == T){
      Diag.pol.veget(MP = Ir.MP_ss, MV = Ir.MV_ss, Meco = Ir.eco,
                     Keep.taxa = Keep.taxa, Aggregation.ss = T,
                     Pollen.index = Ir.pol.ind,
                     CONISS.t = "Pollen", Nzone = 4, Print.cluster.name = T,
                     Ordin = Ir.eco["Ordin2"],
                     Max_seuil = NULL, 
                     Display.legend = "bottom", 
                     H = 700, W = 1300, 
                     Sp.lab.size = 6,
                     Show.site.name = T, Print.cluster.zone = T, Leg.nb.row = 2,
                     Show.site.eco = T, Eco.dot.size = 4, Eco.col = setNames(data.frame(t(Iran.col)), Sort.eco.Ir),
                     Remove.sites.not.full = F, AP.NAP = T,  ShP = T, AP.NAP.ShP.col = T,
                     
                     Abiot.plot = c("MAAT", "Altitude", "AI"),
                     Save.plot = "Figures/Iran/Vegetation/Compar_hist_MV_MP_Ir.pdf")
    }
    
    #### Plots LR pol / veg ####
    LR.po.ve = F
    if(LR.po.ve == T){
      LR.insert <- LR.pol.veget(MP = Ir.MP_sl, MV = Ir.MV_sl,
                                Keep.taxa = Keep.taxa.2,
                                H = 600, W = 900, R2.pos = "bottomright",
                                Save.plot = "Figures/Iran/Vegetation/Compar_hist_MV_MP_Ir_linear_all.pdf")
      
      LR.pol.veget(MP = Ir.MP_sl, MV = Ir.MV_sl,
                   Keep.taxa = c("Juniperus spp.", "Carpinus betulus", "Poaceae", "Artemisia spp."),
                   H = 400, W = 400, R2.pos = "bottomright", Scale = "free",
                   Save.plot = "Figures/Iran/Vegetation/Compar_hist_MV_MP_Ir_linear_over.pdf")
      
      LR.pol.veget(MP = Ir.MP_sl, MV = Ir.MV_sl,
                   Keep.taxa = c("Cousinia spp.", "Cyperaceae", "Fabaceae", "Alnus spp."),
                   H = 400, W = 400, R2.pos = "bottomright", Scale = "free",
                   Save.plot = "Figures/Iran/Vegetation/Compar_hist_MV_MP_Ir_linear_low.pdf")
    }
    
    #### Procrustean Co-innertia Analysis ####
    PCoI = F
    if(PCoI == T){
      Ir.eco2 <- Ir.eco 
      Ir.eco2$Ecosystem[Ir.eco2$Ecosystem == "Mountain steppe-meadow"] <- "Mountain \nsteppe- \nmeadow"
      Ir.eco2$Ecosystem[Ir.eco2$Ecosystem == "Forest-steppe"] <- "Forest- \nsteppe"
      Ir.eco2$Ecosystem[Ir.eco2$Ecosystem == "Desert-steppe"] <- "Desert- \nsteppe"
      Lab2 <- c("Desert- \nsteppe", "Mountain \nsteppe- \nmeadow", "Woodland","Forest- \nsteppe", "Forest")

      PCoI.Ir <- PCoI.vegetation(MV = Ir.MV_sl, MP = Ir.MP_sl, Meco = Ir.eco2, W = 900, H = 450,
                                 Show.errors = T, Show.outliers = T,
                                 Eco.col = setNames(data.frame(t(Iran.col)), Lab2),
                                 Symbol.pos = c(.12,.88,.2), Stats.pos = c(-6.35,-2.1), Show.site.lab = F,
                                 Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen_vs_veget.png",
                                 Save.plot = "Figures/Iran/Vegetation/Procrustes_errors_veget_pollen_Ir.pdf")
    }
    
    #### Davis Index ####
    Davis.Ir = F
    if(Davis.Ir == T){
      Ir.MP_sl <- Ir.MP_sl[names(Ir.MP_sl) %in% Keep.common.site]
      Ir.MV_sl <- Ir.MV_sl[names(Ir.MV_sl) %in% Keep.common.site]
      Ir.DavisIdx <- Davis.Index(MP = Ir.MP_sl, MV = Ir.MV_sl,
                                 Add.group = T, Add.Rval = T, R.val.rel = "Poaceae", #Fabaceae
                                 # Remove.taxa = c(50, 51, 82, 84, 86, 94), 
                                 Bar.width = .75, Sp.lab.size = 13,Grp.Lab.size = 2,
                                 Save.path = "Resultats/Iran/Pollen/Surface/Ir_Davis_index.csv", 
                                 H = 1200, W = 800, Save.plot = "Figures/Iran/Pollen/Surface/Ir_Davis_index.pdf"
      )
    }
    
    #### Mapping MV vs. MP ####
    Mapping.MV.MP = F
    if(Mapping.MV.MP == T){
      Map1 <- Map.samples(
        Samples.list = list(Vegetation.plots = Gol.coord, Pollen.Samples = Gol.coord),
        Samples.pollen =  list(Pollen.Samples = Ir.MP_sl, Vegetation.plots = Ir.MV_sl),
        # Limites = c(55.5,76,36.5,45.7), 
        Insert.loc = c(55.7,55.95,37.265,37.44),
        Map.extend = "Golestan", Insert.LR = LR.insert,
        Display.taxa = Keep.taxa.2, Taxa.italic = T,
        Save.plot = "Figures/Iran/Vegetation/Map_Gol_pollen_veget_all.pdf", W = 1300, H = 700)
    }
    
  }
  
  
  
}


#### ACA MV ####
ACA = F
if(ACA == T){
  #### Import all datas ####
  Mong.veget.10m.P1.mean <- read.table("Resultats/Mongolia/Vegetation/MV_Mong_p1.csv", sep = ",", header = T, row.names = 1)
  Mong.eco <- read.table("Import/Mongolia/Site/Surface_samples_climat.csv", sep = ",", header = T, row.names = 1)
  Az.veget.10m.mean.PT <- read.table("Resultats/Azerbaijan/Vegetation/MV_Az_PT.csv", sep = ",", header = T, row.names = 1)
  Az.eco <- read.table("Import/Azerbaijan/Site/Az_surf_samples.csv", sep = ",", header = T, row.names = 1)
  Uz.veget.10m.mean.PT <- read.table("Resultats/Uzbekistan/Vegetation/MV_uz_PT.csv", sep = ",", header = T, row.names = 1)
  Uz.eco <- read.table("Import/Uzbekistan/Site/Uz_surf_samples.csv", sep = ",", header = T, row.names = 1)
  row.names(Mong.veget.10m.P1.mean)[row.names(Mong.veget.10m.P1.mean) == "Artemisia"] <- "Artemisia spp."
  
  #### Plot LR clim full ACA ####
  LR.clim.veget(MT = list(Azerbaijan = Az.veget.10m.mean.PT, 
                          Uzbekistan = Uz.veget.10m.mean.PT,
                          Mongolia = Mong.veget.10m.P1.mean
                          ), 
                Meco = list(Azerbaijan = Az.eco, 
                            Uzbekistan = Uz.eco,
                            Mongolia = Mong.eco
                            ),
                Keep.taxa = c("Artemisia spp.", "Amaranthaceae", "Cyperaceae"),
                Keep.clim = c("Altitude", "MAP", "MAAT"),
                H = 800, W = 850, Strip.lab = F, R2.pos = "bottomright",
                Save.plot = "Figures/ACA/Vegetation/LRveg_clim_ACA_full.pdf")
  
  
  
}
