#### Global path + VERSION DB ####
#setwd("/media/marjorie/Maximator/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/marjorie/Samsung_T5/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
setwd("/home/lucas.dugerdil/Documents/Recherche/R_stats") 
#setwd("/media/marjorie/Samsung_T5/Documents/Recherche/R_stats") 

# Version.DB <- "DB5498"
# Version.DB <- "DB3717"
Version.DB <- "DB3373"
print(paste("All calculations made for the version: ", Version.DB))
#### Librairies ####
# library(fields)
library(rioja)
library(palaeoSig)
# library(maptools)
#library(colorRamps)
library(randomForest)
library(dismo) # BRT

#### Functions ####
Biome.Pol.map <- function(Site.loc, Site.loc2, Site.loc3, Biome.select, Save.plot, H, W){
  #### Settings ####
  library(maps)
  if(missing(Biome.select)){Biome.select = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Site.loc)){Site.loc = NULL}
  if(missing(Site.loc2)){Site.loc2 = NULL}
  if(missing(Site.loc3)){Site.loc3 = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Definition des couleurs ####
  List.biom <- unique(DB.odile.B$BIOMPOL_) # New DB /!\ les biomes ne sont pas organisés dans le même ordre
  # List.biom <- unique(DB3058Biome$BIOMPOL) # Old DB
  Couleur.Prentice <- data.frame(read.csv(text = "Biomes, Couleurs,
  WAMX,#185699FF,
  TEDE,#72a72bff,
  XERO,#cf0156ff,
  COMX,#c1b646ff,
  HODE,#fcf8b6ff,
  WAST,#e2a064ff,
  CLMX,#ee9b2fff,
  PION,#0e3056ff,
  TAIG,#32156eff,
  TUND,#caa6c2ff,
  COCO,#9d2e58ff,
  COST,#f3c768ff,
  ANTH,#6e1d2cff,
  CODE,#d6d81eff,
  CLDE,#b1d5f0ff",
  sep = ",", 
  header = T)) # ANTH, PION, COST, CODE sont faux ? Les autres viennent de Prentice 2000.
  Couleur.Prentice <- subset(Couleur.Prentice, select = -c(X))
  Col2 <- as.character(Couleur.Prentice$Couleurs)
  Leg.biom <- as.character(Couleur.Prentice$Biomes)
  
  #### Plot fond de map ####
  map("world", regions = c("Mongolia", "Russia", "Spain", "France", "Italy", "Greece", "Germany", "Finland", "Czech Republic", "Denmark", "Kosovo",
                           "Latvia","Lithuania", "Estony", "Belarus", "Romania", "Bulgaria", "Hungary", "Austria", "Croatia", "Switzerland",
                           "Albania", "Serbia", "Slovenia", "Slovakia", "Bosnia", "Montenegro", "UK", "Ireland", "Moldova", "Macedonia",
                           "Norway", "Sweden", "Turkey", "Belgium", "Uzbekistan", "Tajikistan", "Syria", "Israel", "Jordan",
                           "Kazakhstan","Turkmenistan", "Ukraine", "Poland", "Portugal", "Morocco", "Kyrgyzstan", "Azerbaijan",
                           "China", "Iran", "Armenia", "Georgia"), #proj = "orthographic", orientation = c(20,50,-10),
           col="grey95", fill = T, border = "grey60",  mar = c(0,0,0,0))
  
  #### Legends settings ####
  # Type.leg <- unique(DB3058Biome$BIOMPOL)
  Type.leg <- unique(DB.odile.B$BIOMPOL_)
  Type.leg[which(List.biom == Biome.select)] <- (19+4)
  Type.leg[which(List.biom != Biome.select)] <- 19
  Type.leg <- as.numeric(Type.leg)
  
  # Size.leg <- unique(DB3058Biome$BIOMPOL)
  Size.leg <- unique(DB.odile.B$BIOMPOL)
  Size.leg[which(List.biom == Biome.select)] <- 1.5
  Size.leg[which(List.biom != Biome.select)] <- .95
  Size.leg <- as.numeric(Size.leg)
  
  # N0 <- paste("n[EAPDB] == ", length(DB3191Coord$LONG) + length(Site.loc2$LONG), sep = "")
  N0 <- paste("n[EAPDB] == ", length(DB.odile.Co$LONG) + length(Site.loc2$LONG), sep = "")
  Lab.nb <- c(parse(text = N0))
  # BG.list <- rep(0, length(unique(DB3058Biome)))
  BG.list <- rep(0, length(unique(DB.odile.B)))
  
  #### Add points par biome ####
  for (i in 1:length(List.biom)){
    Biome.site <- row.names(DB.odile.B)[which(DB.odile.B == List.biom[i])]
    # DB3058PolClim.Biome <- DB3191Clim[Biome.site,]
    DB3058PolClim.Biome <- DB.odile.Cl[Biome.site,]
    # DB3058Coord.Biome <- DB3191Coord[Biome.site,]
    DB3058Coord.Biome <- DB.odile.Co[Biome.site,]

    if(i == which(List.biom == Biome.select)){ # COST
      N1 <- paste("n[", Biome.select,  "DB] == ", length(DB3058Coord.Biome$LONG) + length(Site.loc2$LONG), sep = "")
      Lab.nb <- c(parse(text = N0), parse(text = N1))
      BG = Col2[i]
      Save = Col2[i]
      Col2[i] <- "grey35"
      param.forme = 4}
    else{
      BG = NA
      param.forme = 0}
    BG.list[i] <- BG 
    # points(DB3058Coord.Biome$LONG, DB3058Coord.Biome$LAT, pch=(19+param.forme), cex=(0.5+param.forme/4), 
    points(DB3058Coord.Biome$LONG, DB3058Coord.Biome$LAT, pch=(19+param.forme), cex=(0.5+param.forme/4), 
           col = Col2[i],
           bg = BG
           )
  }
  #### Add sub DB ####
  if(is.null(Site.loc) == F){
    Col2 <- append(Col2, "black")
    Leg.biom <- append(Leg.biom, " MDB")
    BG.list <- append(BG.list, NA)
    Type.leg <- append(Type.leg, 1)
    N1 <- paste("n[MDB] == ", length(Site.loc$LONG), sep = "")
    Lab.nb <- append(Lab.nb, parse(text = N1))
    points(Site.loc$LONG, Site.loc$LAT, pch = 1, cex = 1.1, col = "black")}
  
  if(is.null(Site.loc2) == F){
    Col2 <- append(Col2, "green")
    Leg.biom <- append(Leg.biom, " NMSDB")
    N2 <- paste("n[NMSDB] == ", length(Site.loc2$LONG), sep = "")
    Lab.nb <- append(Lab.nb, parse(text = N2))
    points(Site.loc2$LONG, Site.loc2$LAT, pch = 19, col = "green", cex = .6)}
  
  
  #### Legends ####  
  legend("bottomright",                                           # ajout LEGENDES
         col = Col2, pt.bg = BG.list, pt.cex = Size.leg,
         pch = Type.leg, #c(rep(19, 16), 1, 23),
         cex = .95, 
         border = "white",                                     # pas cadre
         bty = 'n',                                            # pas bordure 
         title = "Biomes",                                          # titre
         title.adj = 0.35,
         y.intersp = 1.1,	                                       # espace entre y
         x.intersp = 0.2,                                      # espace entre x
         legend = Leg.biom
  )
  #### Insert ####  
  legend("topleft",                                           # ajout LEGENDES
         legend = Lab.nb,
         cex = .9,
         border = "white",                                     # pas cadre
         bty = 'n',                                            # pas bordure 
         title="Data set size",                                          # titre
         title.adj = 0.35,
         y.intersp=1.1,	                                       # espace entre y
         x.intersp = 0.2
  )
  if(is.null(Save.plot) == F){dev.off()}
  }

Surf.MAT.prep <- function(MP_surf, Type_MAT, Show.message, Replace.dot = T, Corresp_name){
  if(missing(Show.message)){Show.message = T}
  if(missing(Corresp_name)){warning("Missing the matrix of the pollen type / transfert function type correspondance.")}
  #### Donnees en %TP ou en [C] ####
  if(rowSums(MP_surf[1,]) < 2){MP_surf[MP_surf <= 0.01] <- 0
  MP_surf <- (MP_surf/rowSums(MP_surf))}                      # Fossil data in %TP
  else{                                
    MP_surf[MP_surf <= 0.5] <- 0
    MP_surf <- (MP_surf/rowSums(MP_surf))}                    # Fossile pollen [C]
  
  #### Nom taxon correspondant MAT model / fossils samples ####
  a <- Corresp_name[Type_MAT]
  Keep.taxa <- subset(Corresp_name, a != "ABS")                                                    # Valeurs Absente de la DB_surface
  if(Replace.dot == T){names(MP_surf) <- gsub("\\."," ",names(MP_surf))}                                                 # remplace . en espace
  MP_surf.cor <- MP_surf[, which(names(MP_surf) %in% Keep.taxa$Nom)]                               # on enleve les taxons absents
  Keep.taxa <- Keep.taxa[match(intersect(names(MP_surf.cor),Keep.taxa$Nom), Keep.taxa$Nom),]       # on remplace les noms par ceux de la DB_surf
  colnames(MP_surf.cor)<- Keep.taxa[[Type_MAT]]
  MP_surf.cor <- as.data.frame(do.call(cbind, by(t(MP_surf.cor),INDICES=names(MP_surf.cor),FUN=colSums)))   # on somme les taxons qui appartiennent au meme type
  MP_surf.cor <- (MP_surf.cor/rowSums(MP_surf.cor))
  i <- (colSums(MP_surf.cor, na.rm=T) != 0)                         # si les col sont vides
  MP_surf.cor <- MP_surf.cor[, i]                                   # alors on les vire

  if(Show.message == T){
    print(paste("Les taxons suivants ont été enlevés de la base, faute de correspondance dans le fichier :", 
                paste(setdiff(names(MP_surf), Keep.taxa$Nom), collapse = ", " )))}
  
  #### Return algo ####
  return(MP_surf.cor)
}

Surf.Clim.Pol <- function(MP_surf, MClim_surf){
  Remove.NA.from.clim <- unique(row.names(which(is.na(MClim_surf), arr.ind = T)))
  MClim_surf <- MClim_surf[!row.names(MClim_surf) %in% Remove.NA.from.clim,]
  
  inter <- intersect(row.names(MP_surf), row.names(MClim_surf))
  MClim_surf <- MClim_surf[inter,]
  return(MClim_surf)}

Surf.check.clim <- function(MP_surf, MClim_surf, Error.display = T){
  inter <- intersect(row.names(MP_surf), row.names(MClim_surf))
  Error1 <- setdiff(row.names(MP_surf), row.names(MClim_surf))
  Error2 <- setdiff(row.names(MClim_surf), row.names(MP_surf))
  
  if(length(Error1)>0 & Error.display == T){
    print("**** Missings sites in Mclim. ****")
    print(Error1)}
  
  if(length(Error2)>0 & Error.display == T){
    print("**** Missings sites in MP. ****")
    print(Error2)}
  
  MP_surf <- MP_surf[inter,]
  return(MP_surf)}

Fossil.MAT.prep <- function(MP_fossil, MAge, Type_MAT, Displot, 
                            H = 400, W = 700, Save.plot = NULL, Corresp_name){
  #### Settings ####
  if(missing(Displot)){Displot = F}
  if(missing(Corresp_name)){warning("Missing the matrix of the pollen type / transfert function type correspondance.")}
  
  #### Donnees en %TP ou en [C] ####
  MP_fossil[is.na(MP_fossil)] <- 0
  if(rowSums(MP_fossil[1,]) < 2){
    print("Pollen data in %.")
    MP_fossil[MP_fossil <= 0.01] <- 0
    MP_fossil <- (MP_fossil/rowSums(MP_fossil))                     } # Fossil data in %TP
  else{              
    print("Pollen data in concentration.")
    MP_fossil[MP_fossil <= 0.5] <- 0
    MP_fossil <- (MP_fossil/rowSums(MP_fossil))}                    # Fossile pollen [C]
  
  #### Nom taxon correspondant MAT model / fossils samples ####
  a <- Corresp_name[Type_MAT]
  # print(setdiff(names(MP_fossil),Corresp_name$Nom))
  Keep.taxa <- subset(Corresp_name, a != "ABS")                                                                   # Valeurs Absente de la DB_surface
  names(MP_fossil) <- gsub("\\."," ",names(MP_fossil))                                                            # remplace . en espace
  # write.csv(setdiff(names(MP_fossil),Keep.taxa$Nom), "Resultats/Aaaaaa.csv")
  MP_fossil.cor <- MP_fossil[, which(names(MP_fossil) %in% Keep.taxa$Nom)]                                        # on enleve les taxons absents
  Keep.taxa <- Keep.taxa[match(intersect(names(MP_fossil.cor),Keep.taxa$Nom), Keep.taxa$Nom),]                    # on remplace les noms par ceux de la DB_surf
  colnames(MP_fossil.cor)<- Keep.taxa[[Type_MAT]]                                                            
  MP_fossil.cor <- as.data.frame(do.call(cbind, by(t(MP_fossil.cor),INDICES=names(MP_fossil.cor),FUN=colSums)))   # on somme les taxons qui appartiennent au meme type
  MP_fossil.cor <- (MP_fossil.cor/rowSums(MP_fossil.cor))
  
  #### Vérif si les échantillons sont bien dans le bon sens (depth ou age) ####
  Common.samples <- intersect(row.names(MP_fossil.cor), row.names(MAge))
  MP_fossil.cor <- MP_fossil.cor[na.omit(match(row.names(MAge), Common.samples)),]
  MAge <- MAge[na.omit(match(row.names(MAge), Common.samples)),]
  
  #### Plot verif ####
  if(Displot == T){
    #### Save plots ####
    if(is.null(Save.plot) == F){
      Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
      dir.create(file.path(Path.to.create), showWarnings = FALSE)
      if(is.null(W) == F & is.null(H) == F){
        pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
      else{pdf(file = Save.plot)}}
    
    #### Plot ####
    mx <- apply(MP_fossil.cor, 2, max)
    MP_fossil.sub <- MP_fossil.cor[, mx > 0.01]   # seuil %TP > 5%
    
    Plot.x <- "Age"
    if(any(is.na(MAge[[Plot.x]]))){Plot.x = "Top"}
    strat.plot(MP_fossil.sub*100, yvar = MAge[[Plot.x]], scale.percent=T, y.rev=T, plot.poly=T, col.poly.line=NA, exag=T, col.exag="auto", col.poly="darkgreen")
    # strat.plot(MP_fossil.sub*100, yvar=MAge$Age)
    if(is.null(Save.plot) == F){dev.off()}
    
  }
  
  #### Return algo ####
  return(MP_fossil.cor)
}

Fossil.corresp.surface <- function(MP_fossil, MP_surf){
  inter <- intersect(names(MP_fossil), names(MP_surf))
  MP_fossil <- MP_fossil[,inter]
  
  Taxa.miss <- setdiff(names(MP_surf),names(MP_fossil))
  A <- setNames(data.frame(matrix(ncol = length(Taxa.miss), nrow = nrow(MP_fossil))), Taxa.miss) 
  row.names(A) <- row.names(MP_fossil)
  A[is.na(A)] <- 0
  A <- cbind(MP_fossil, A)
  MP_fossil <- A[,sort(names(A))]
  
  return(MP_fossil)
}

Remove.taxa.surf.non.analogue <- function(MP_surf, MP_fossil){
  inter <- intersect(names(MP_fossil), names(MP_surf))
  MP_surf <- MP_surf[,inter]
  return(MP_surf)
}

FT.quantif <- function(MPol, MClim, Model, Min.NB,
                              Save.path, Save.RDS, Mcoord, Nb.arg){
  #### Settings ####
  if(missing(Model)){Model = "WAPLS"
    print("Default model choice : WAPLS")}
  Save.tab = T
  Autocorrel.check = T
  if(missing(Save.path)){Save.tab = F}
  if(missing(Save.RDS)){Save.RDS = F}
  if(missing(Nb.arg)){Nb.arg = NULL}
  if(missing(Min.NB)){Min.NB = NULL}
  if(missing(Mcoord)){Autocorrel.check = F}
  tableWAPLS <- vector("list", length(MClim))
  Tab.impor.taxa <- vector("list", length(MClim))
  Tab.result <- list()
  Best.param <- list()

  #### model running ####
  print(paste(Model, "modellisation in running on the", substitute(MPol), "database.", sep = " "))
  for(i in 1:length(MClim)){
    #### WAPLS ####
    if(Model == "WAPLS"){
      if(is.null(Min.NB) == T){Min.NB = 2}
      if(is.null(Nb.arg) == T){Nb.arg = 5}
      print(paste(round((i/length(MClim))*100), "% done for the ", names(MClim)[i], sep = ""))
      Fit.i = WAPLS(MPol, MClim[[i]], npls = Nb.arg, lean=F)
      
      Fit.CV = crossval(Fit.i, k = Fit.i$k, cv.method="bootstrap", verbose=F, ngroups=5, nboot=500, h.cutoff=0, h.dist=NULL)
      tableWAPLS[[i]] <- Fit.i
      }
    
    #### MAT ####
    if(Model == "MAT"){
      if(is.null(Min.NB) == T){Min.NB = 4}
      if(is.null(Nb.arg) == T){Nb.arg = 10}
      print(paste(round((i/length(MClim))*100), "% done for the ", names(MClim)[i], sep = ""))
      Fit.i = MAT(MPol, MClim[[i]], k = Nb.arg, lean=F)
      
      Fit.CV = crossval(Fit.i, k = Fit.i$k, cv.method="bootstrap", verbose=F, ngroups=5, nboot=500, h.cutoff=0, h.dist=NULL)
      tableWAPLS[[i]] <- Fit.i
      }
    
    #### Random Forest ####
    if(Model == "RF"){
      print(paste(round((i/length(MClim))*100), "% done for the ", names(MClim)[i], sep = ""))
      DB <- cbind(param.climat = MClim[[i]], MPol)
      Fit.i <- randomForest(param.climat ~., data = DB, ntree = 100, mtry = 2, na.action = na.roughfix)
      Fit.CV <- rfcv(MPol, MClim[[i]], cv.fold = 3)
      Best.CV.error <-  names(Fit.CV$error.cv)[match(min(Fit.CV$error.cv), Fit.CV$error.cv)]
      Residual.RF <- Fit.CV$predicted[[Best.CV.error]]
      Residual.RF <- Residual.RF - MClim[[i]]
      Tab.impor.taxa[[i]] <- round(Fit.i$importance, digits = 0)
      tableWAPLS[[i]] <- Fit.i
      }
    
    #### Boosted Regression Tree ####
    if(Model == "BRT"){
      print(paste(round((i/length(MClim))*100), "% done for the ", names(MClim)[i], sep = ""))
      MPol <- MPol[,colSums(MPol)>0.1]                               # ATTENTION : pour BRT les taxons <0.1 ne fonctionnent pas
      DB <- cbind(param.climat = MClim[[i]], MPol)
      Fit.i <- gbm.step(data = DB, gbm.x = 2:ncol(DB), gbm.y = 1, family = "gaussian", 
                        verbose = F, call = F, 
                        tree.complexity = 6, tolerance.method= "fixed", tolerance = 0.1,
                        # learning.rate = 0.02,
                        # bag.fraction = 0.5,
                        max.trees = 7000
                        )
      Tab.impor.taxa[[i]] <- round(Fit.i$contributions[2], digits = 2)
      tableWAPLS[[i]] <- Fit.i    
      }   
    
    #### Tableau synthese des resultats Cross Val ####
    if(Model == "WAPLS"){
      AddParam <- performance(Fit.CV)$crossval
      AddParam <- AddParam[c(Min.NB: Nb.arg),] # Pas valide en dessous de Min.NB params
      AddParam2 <- AddParam[,1:2] # on garde juste R2, RMSE
      AddParam2 <- cbind(NPLS = seq(Min.NB, Nb.arg), AddParam2, Moran.s.I = seq(Min.NB, Nb.arg), Moran.p.val = seq(Min.NB, Nb.arg))

      if(Autocorrel.check == T){
        Clim.reconstruct.sites <- Fit.CV$residuals.cv
        for(x in 1:(Nb.arg-Min.NB)){
          Spa.auto.i <- Spatial.autocor.check(Mcoord, data.frame(Clim.reconstruct.sites[,x]))
          AddParam2[x,4] <- as.numeric(Spa.auto.i[[1]])
          AddParam2[x,5] <- as.numeric(Spa.auto.i[[4]])
        }}}
    
    if(Model == "MAT"){
      AddParam <- performance(Fit.CV)$crossval[(Nb.arg+1):length(performance(Fit.CV)$crossval[,1]),]
      AddParam <- AddParam[c(Min.NB: Nb.arg),] # Pas valide en dessous de Min.NB
      AddParam2 <- AddParam[,1:2] # on garde juste R2, RMSE
      AddParam2 <- data.frame(cbind(Nb.analogue = seq(Min.NB,Nb.arg), AddParam2, Moran.s.I = seq(Min.NB,Nb.arg), Moran.p.val = seq(Min.NB,Nb.arg)))
      if(Autocorrel.check == T){
        Clim.reconstruct.sites <- Fit.CV$residuals.cv
        for(x in 1:(Nb.arg-Min.NB)){
          Spa.auto.i <- Spatial.autocor.check(Mcoord, data.frame(Clim.reconstruct.sites[,x]))
          AddParam2[x,4] <- as.numeric(Spa.auto.i[[1]])
          AddParam2[x,5] <- as.numeric(Spa.auto.i[[4]])
        }}
      
      }
    
    if(Model == "RF"){
      MSE <- Fit.i$mse
      MSE <- MSE[length(MSE)]
      RMSE <- sqrt(MSE)
      R2 <- Fit.i$rsq
      R2 <-  R2[length(R2)]
      AddParam <- cbind(RMSE = RMSE, pseudo.R2 = R2)
      AddParam2 <- cbind(Nb.tree = 100, RMSE = RMSE, pseudo.R2 = R2, Moran.s.I = NA, Moran.p.val = NA)

      if(Autocorrel.check == T){
        Clim.reconstruct.sites <- data.frame(Residual.RF) #Fit.CV$residuals.cv
        row.names(Clim.reconstruct.sites) <- row.names(MPol)
        Spa.auto.i <- Spatial.autocor.check(Mcoord, data.frame(Clim.reconstruct.sites))
        AddParam2[4] <- as.numeric(Spa.auto.i[[1]])
        AddParam2[5] <- as.numeric(Spa.auto.i[[4]])
        # for(x in 1:Nb.arg){
        #   Spa.auto.i <- Spatial.autocor.check(Mcoord, data.frame(Clim.reconstruct.sites[,x]))
        #   AddParam2[x,4] <- as.numeric(Spa.auto.i[[1]])
        #   AddParam2[x,5] <- as.numeric(Spa.auto.i[[4]])
        #   }
        }}
    
    if(Model == "BRT"){
      R2 <- Fit.i$cv.statistics$correlation.mean
      MSE <- Fit.i$cv.statistics$deviance.mean    # Je suis pas sur que c'est la RMSE ca
      MSE <- MSE[length(MSE)]
      RMSE <- sqrt(MSE)
      AddParam <- cbind(RMSE = RMSE, pseudo.R2 = R2)
      Nb.tree <- Fit.i$gbm.call$best.trees
      AddParam2 <- cbind(Nb.tree = Nb.tree, RMSE = RMSE, pseudo.R2 = R2, Moran.s.I = NA, Moran.p.val = NA)
      
      if(Autocorrel.check == T){
        Clim.reconstruct.sites <- data.frame(Fit.i$residuals) #Fit.CV$residuals.cv
        row.names(Clim.reconstruct.sites) <- row.names(MPol)
        Spa.auto.i <- Spatial.autocor.check(Mcoord, data.frame(Clim.reconstruct.sites))
        AddParam2[4] <- as.numeric(Spa.auto.i[[1]])
        AddParam2[5] <- as.numeric(Spa.auto.i[[4]])
      }}
    
    Param.clim.lab <- rep(names(MClim)[[i]], length(AddParam2[,1]))
    Param.clim.lab.num = paste(Param.clim.lab, 1:length(AddParam2[,1]), sep = "_")
    Keep.Nx.name <- row.names(AddParam2)
    row.names(AddParam2) <- Param.clim.lab.num
    Tab.result <- rbind(Tab.result, AddParam2)
    
    #### Choix meilleurs Composant, selon CV ####
    if(Model %in% c("MAT", "WAPLS")){
      if(Min.NB == 0){Add.nb.param = 0}
      else{Add.nb.param = Min.NB - 1}
      }
    else{Add.nb.param = 0}
    
    MinRMSE <- min(AddParam[,1])
    MaxR2 <- max(AddParam[,2])
    BestR2 <- which(AddParam[,2]==MaxR2)
    BestRMSE <- which(AddParam[,1]==MinRMSE)
    Lastcheck <- min(BestR2, BestRMSE)
    R2.choice <- AddParam[Lastcheck,2]
    RMSE.choice <- AddParam[Lastcheck,1]
    Vam <- data.frame(Best.R2 = BestR2[[1]]+Add.nb.param, Best.RMSE = BestRMSE[[1]]+Add.nb.param, Param.choice = Lastcheck, R2.choice = R2.choice, RMSE.choice = RMSE.choice)
    row.names(Vam)<- names(MClim)[[i]]
    
    #### Calcul Moran's I autocorrel ####
    if(Autocorrel.check == T & Model %in% c("MAT", "WAPLS")){
      Nx.name.best <- Keep.Nx.name[Lastcheck]
      Clim.reconstruct.sites <- data.frame(Clim.reconstruct.sites)
      Clim.reconstruct.sites.best <- data.frame(Clim.reconstruct.sites[[Nx.name.best]])
      row.names(Clim.reconstruct.sites.best) <- row.names(Clim.reconstruct.sites)
      names(Clim.reconstruct.sites.best) <- paste(names(MClim)[i], "-residual", sep = "")
      Spa.auto.i <- Spatial.autocor.check(Mcoord, Clim.reconstruct.sites.best)
      Vam2 <- data.frame(Moran.I = Spa.auto.i[1], Moran.p.val = Spa.auto.i[4])
      row.names(Vam2)<- names(MClim)[[i]]
      Vam.merge <- cbind(Vam, Vam2)
      Best.param <-rbind(Best.param, Vam.merge)
      }
    else{Best.param <-rbind(Best.param, Vam)}
    }
  
  if(Model %in% c("RF","BRT")){Best.param <- Best.param[,4:5]}

  yo <- c(names(MClim),"Best.Param")
  tableWAPLS[[length(MClim)+1]] <- Best.param
  names(tableWAPLS) <- yo
  
  #### Save / export data ####
  Best.param <- as.matrix(Best.param[with(Best.param, order(-R2.choice)),])
  if(Save.tab == T){
    Save.path2 <- gsub("\\.csv", "_BestParam.csv", Save.path)
    write.table(Tab.result, file=Save.path, row.names=T, col.names=NA, sep=",", dec = ".")
    write.table(Best.param, file=Save.path2, row.names=T, col.names=NA, sep=",", dec = ".")
    
    if(Model %in% c("RF","BRT")){
      Tab.impor.taxa <- as.data.frame(Tab.impor.taxa)
      names(Tab.impor.taxa) <- names(MClim)
      Save.path3 <- gsub("\\.csv", "_Taxa_importance.csv", Save.path)
      write.table(Tab.impor.taxa, file=Save.path3, row.names=T, col.names=NA, sep=",", dec = ".")
      }
    
    if(Save.RDS == T){
      Save.path.RDS <- gsub("\\.csv", ".Rds", Save.path)
      saveRDS(tableWAPLS, Save.path.RDS)}
    
  }
  if(length(Best.param[,1])>50){print(head(Best.param, n = 50))}
  else{print(Best.param)}
  return(tableWAPLS)
  }

FT.core <- function(MCore, MAge, Model.WAPLS, Model.MAT, Fit.val, Model.RF, Model.BRT,
                    Save.tab, Save.plot, Save.RDS, H, W, Only.fit, LakeName, Select.clim,
                    Ecartype.curve, Model.param.show, Displot, Verbose,
                    Zone.Clim.span, Zone.Temp, Save.path){
  #### Init param ####
  if(missing(Verbose)){Verbose = T}
  if(missing(Save.tab)){Save.tab = T}
  if(missing(Zone.Clim.span)){Zone.Clim.span = 2}
  if(missing(Zone.Temp)){Zone.Temp = rep("U", length(Zone.Clim.span)/2)}
  Zone.Temp <- rep(Zone.Temp,each=2)
  if(Zone.Clim.span == c(0) | identical(Zone.Temp,character(0))){Zone.OK = F}
  else{Zone.OK = T}
  if(missing(Select.clim)){Select.clim = NULL}
  if(missing(Model.WAPLS)){Model.WAPLS = NULL}
  if(missing(Model.MAT)){Model.MAT = NULL}
  if(missing(Model.RF)){Model.RF = NULL}
  if(missing(Model.BRT)){Model.BRT = NULL}
  if(missing(Save.path)){Save.tab = F}
  if(missing(Displot)){Displot = T}
  if(missing(Zone.Clim.span)){Zone.OK = F}
  if(missing(Model.param.show)){Model.param.show = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Save.RDS)){Save.RDS = F}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Only.fit)){Only.fit = F}
  if(missing(LakeName)){LakeName = "Lake"}
  if(missing(Fit.val)){Fit.val = 0}
  if(missing(Ecartype.curve)){Ecartype.curve = c(F,F,F,F)}
  if(missing(MAge)){MAge = paste(LakeName, seq(1:nrow(MCore)), sep = "_")}
  
  #### Select le model type ####
  Keep.WAPLS <- deparse(substitute(Model.WAPLS))
  Keep.MAT <- deparse(substitute(Model.MAT))
  Keep.RF <- deparse(substitute(Model.RF))
  Keep.BRT <- deparse(substitute(Model.BRT))
  
  if(is.null(Model.WAPLS)== F){Model.type <- Model.WAPLS}
  if(is.null(Model.MAT)== F){Model.type <- Model.MAT}
  if(is.null(Model.RF)== F){Model.type <- Model.RF}
  if(is.null(Model.BRT)== F){Model.type <- Model.BRT}
  
  #### Save plots ####
  if(is.null(Save.plot) == F & Displot == T){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Select param clim ####
  if(is.null(Select.clim) == F){
    Select.clim <- c(Select.clim, "Best.Param")
    Model.type <- Model.type[names(Model.type) %in% Select.clim]
    Model.type$Best.Param <- Model.type$Best.Param[row.names(Model.type$Best.Param) %in% Select.clim,]
    
    if(is.null(Model.BRT) == F){
      Model.BRT <- Model.BRT[names(Model.BRT) %in% Select.clim]
      Model.BRT$Best.Param <- Model.BRT$Best.Param[row.names(Model.BRT$Best.Param) %in% Select.clim,]
    }
    
    if(is.null(Model.MAT) == F){
      Model.MAT <- Model.MAT[names(Model.MAT) %in% Select.clim]
      Model.MAT$Best.Param <- Model.MAT$Best.Param[row.names(Model.MAT$Best.Param) %in% Select.clim,]
    }
    
    if(is.null(Model.WAPLS) == F){
      Model.WAPLS <- Model.WAPLS[names(Model.WAPLS) %in% Select.clim]
      Model.WAPLS$Best.Param <- Model.WAPLS$Best.Param[row.names(Model.WAPLS$Best.Param) %in% Select.clim,]
    }
    
    if(is.null(Model.RF) == F){
      Model.RF <- Model.RF[names(Model.RF) %in% Select.clim]
      Model.RF$Best.Param <- Model.RF$Best.Param[row.names(Model.RF$Best.Param) %in% Select.clim,]
    }
  }
  
  #### Graphical settings ####
  Tailleplot <-length(Model.type)-1
  if (Tailleplot <= 3){par(mfrow = c(1,Tailleplot))}
  if (Tailleplot == 4){par(mfrow = c(2,2))}
  if (Tailleplot >= 5 & Tailleplot <= 6){par(mfrow = c(2,3))}
  if (Tailleplot >= 7 & Tailleplot <= 9){par(mfrow = c(3,3))}
  Mmodel <- data.frame(Age = MAge)
  MModel.MAT <- data.frame(Age = MAge)
  MModel.RF <- data.frame(Age = MAge)
  MModel.BRT <- data.frame(Age = MAge)
  M.errors.WAPLS <- data.frame(Age = MAge)
  M.errors.MAT <- data.frame(Age = MAge) 
  Full.MAT.RDS <- list()
  Full.WAPLS.RDS <- list()
  Full.RF.RDS <- list()
  Full.BRT.RDS <- list()
  

  #### Loop on the climat param ####
  if(Verbose == F){
    library(lubridate)
    pb = txtProgressBar(min = 1, 
                        max = (length(Model.type)-1), 
                        # width = 4*(length(Model.type)-1), 
                        width = 40,
                        initial = 0,  style = 3) 
    
    init <- numeric((length(Model.type)-1))
    end <- numeric((length(Model.type)-1))
    }
  
  print(paste("Prediction for ", LakeName, " with the following models :", Keep.WAPLS, ", ", Keep.MAT, ", ", Keep.BRT, ", ",  Keep.RF, ".", sep = ""))
  for (i in 1:(length(Model.type)-1)){
    if(Verbose == F){init[i] <- Sys.time()}
    #### WAPLS ####
    if(is.null(Model.WAPLS) == F){
    LabParamlim <- as.character(gsub("\\p{P}","", deparse(names(Model.type)[i]), perl = TRUE ))
      NComp.WAPLS = Model.WAPLS$Best.Param[[3]][i]
      if(NComp.WAPLS < 2){NComp.WAPLS <- 2}
      if(Verbose == T){print(paste(round(i/(length(Model.WAPLS)), digits = 2)*100, "% done. The ", LabParamlim, " is modelling with the WAPLS and ", NComp.WAPLS, " parameters.", sep = ""))}
      Cor.WAPLS=predict(Model.WAPLS[[i]], MCore, npls = NComp.WAPLS, sse=T, nboot = 1000, verbose = F)
      Mmodel[i+1] <- cbind(Cor.WAPLS$fit[,NComp.WAPLS])
      colnames(Mmodel)[i+1] <- LabParamlim
      M.errors.WAPLS[i+1] <- cbind(Cor.WAPLS$SEP.boot[,NComp.WAPLS])
      colnames(M.errors.WAPLS)[i+1] <- LabParamlim
      Full.WAPLS.RDS[[i]] <- Cor.WAPLS
      names(Full.WAPLS.RDS)[[i]] <- LabParamlim}
    
    #### MAT ####
    if(is.null(Model.MAT) == F){
      NComp.MAT = Model.MAT$Best.Param[[3]][i]
      if(NComp.MAT < 4){NComp.MAT <- 4}
      if(Verbose == T){print(paste("The ", LabParamlim, " is modelling with the MAT and ", NComp.MAT, " analogues.", sep = ""))}
      Cor.MAT=predict(Model.MAT[[i]], MCore, k = NComp.MAT, sse = T, nboot = 1000, verbose = F)
      MModel.MAT[i+1] <- cbind(Cor.MAT$fit[,2])     # 2 = value-wm (weighted mean), 1 = normal-value
      M.errors.MAT[i+1] <- cbind(Cor.MAT$SEP.boot[,2]) # 2 = value-wm (weighted mean)
      colnames(MModel.MAT)[i+1] <- LabParamlim
      colnames(M.errors.MAT)[i+1] <- LabParamlim
      Full.MAT.RDS[[i]] <- Cor.MAT
      names(Full.MAT.RDS)[[i]] <- LabParamlim}

    #### Random forest ####
    if(is.null(Model.RF) == F){
      if(Verbose == T){print(paste("The ", LabParamlim, " is modelling with the RF.", sep = ""))}
      Cor.RF = predict(Model.RF[[i]], MCore, see = T)
      MSE <- Model.RF[[i]]$mse             # Pb dans le mse. Manquant pou Mongolie ?
      RMSE.RF <- sqrt(MSE[length(MSE)])
      # RMSE.RF <- Model.RF[[i]]$Best.Param[row.names(Model.RF[[i]]$Best.Param)==LabParamlim,2]  # Marche pr Mong ms pas Arm
      MModel.RF <- cbind(MModel.RF, Cor.RF)
      colnames(MModel.RF)[i+1] <- LabParamlim
      Full.RF.RDS[[i]] <- Cor.RF
      names(Full.RF.RDS)[[i]] <- LabParamlim}
    
    #### Boosted Regression Tree (BRT) ####
    if(is.null(Model.BRT) == F){
      if(Verbose == T){print(paste("The ", LabParamlim, " is modelling with the BRT.", sep = ""))}
      MCore.i <- MCore[, names(MCore) %in% colnames(Model.BRT[[i]]$data$x.order)] # ATTENTION il manque les taxon < 0.1 pour les BRT
      Cor.BRT <- gbm::predict.gbm(Model.BRT[[i]], MCore.i, n.trees = Model.BRT[[i]]$gbm.call$best.trees, type="response")
      RMSE.BRT <- Model.BRT$Best.Param[i,2]
      
      MModel.BRT<- cbind(MModel.BRT, Cor.BRT)
      colnames(MModel.BRT)[i+1] <- LabParamlim
      Full.BRT.RDS[[i]] <- Cor.BRT
      names(Full.BRT.RDS)[[i]] <- LabParamlim
      }
    
    #### Plot résultats graphiques ####
    if(Displot == T){
      #### Val Min / Max ####
      ymin = 0
      ymax = 0
      if(Ecartype.curve[1] == F){
        
        if(is.null(Model.WAPLS) == F){
          ymin = min(Cor.WAPLS$fit[,NComp.WAPLS])
          ymax = max(Cor.WAPLS$fit[,NComp.WAPLS])}
        
        if(is.null(Model.MAT) == F & Ecartype.curve[2] == F){
          ymin = min(ymin, Cor.MAT$fit[,2])
          ymax = max(ymax, Cor.MAT$fit[,2])}
        
        if(is.null(Model.RF) == F & Ecartype.curve[3] == F){
          ymin = min(ymin, min(Cor.RF))
          ymax = max(ymax, max(Cor.RF))}
        
        if(is.null(Model.BRT) == F & Ecartype.curve[4] == F){
          ymin = min(ymin, min(Cor.BRT))
          ymax = max(ymax, max(Cor.BRT))}
        
        fullY <- abs(ymax) - abs(ymin)
        ymax = ymax + 0.05*fullY
        }
      else{
        if(is.null(Model.WAPLS) == F){
          ymin = min(Cor.WAPLS$fit[,NComp.WAPLS] - Cor.WAPLS$SEP.boot[,1])
          ymax = max(Cor.WAPLS$fit[,NComp.WAPLS] + Cor.WAPLS$SEP.boot[,1])}
        
        if(is.null(Model.MAT) == F){
          ymin = min(ymin, na.omit(Cor.MAT$fit[,2] - Cor.MAT$SEP.boot[,1]))
          ymax = max(ymax, na.omit(Cor.MAT$fit[,2] + Cor.MAT$SEP.boot[,1]))}
  
        if(is.null(Model.RF) == F){
          ymin = min(ymin, min(Cor.RF - RMSE.RF))
          ymax = max(ymax, max(Cor.RF + RMSE.RF))}
        
        if(is.null(Model.BRT) == F){
          ymin = min(ymin, c(Cor.BRT - RMSE.BRT))
          ymax = max(ymax, max(Cor.BRT + RMSE.BRT))}
  
        fullY <- abs(ymax) - abs(ymin)
        ymax = ymax + 0.05*fullY
        }
      
      #### Plot Mean value ####
      if(is.null(Model.WAPLS) == F & Only.fit == F){Y <- Cor.WAPLS$fit[,NComp.WAPLS]}
      else{Y <- rep(NA, length(MAge))}
      
      plot(MAge, Y, 
           xlim=c(min(MAge),max(MAge)), 
           ylim = c(ymin, ymax), 
           type="l", 
           ylab= LabParamlim, 
           xlab="Time (yr cal BP)", 
           col=1, 
           las=0, lwd=1, bty="n")
      
      #### Legend ####
      if(Model.param.show == T){
        fullT <- abs(min(MAge)) + abs(max(MAge))
        x1 = 0.3*fullT
        x2 = 0.4*fullT
        x4 = 0.55*fullT
        x5 = 0.65*fullT
        
        ymax.lab1 <- ymax 
        ymax.lab2 <- ymax - 0.05*ymax
        
        #### Legend WAPLS ####
        if(is.null(Model.WAPLS) == F){
        mylabel1 = Keep.WAPLS
        mylabel2 = bquote(npls == .(Model.WAPLS[[1]][["npls"]]) ~ "," ~
                          k == .(Model.WAPLS[[length(Model.WAPLS)]][i,3]) ~ "," ~
                          italic(R)^2 == .(format(Model.WAPLS[[length(Model.WAPLS)]][i,4], digits = 2)) ~ "," ~
                          RMSE == .(format(Model.WAPLS[[length(Model.WAPLS)]][i,5], digits = 2)))
        
        text(x = x1, y = ymax.lab1, labels = mylabel1, font = 2, cex = 0.8, pos = 1)
        text(x = x2, y = ymax.lab1, labels = mylabel2, cex = 0.8, pos = 1)
        }
        
        #### Legend MAT ####
        if(is.null(Model.MAT) == F){
          mylabel1b = Keep.MAT
          mylabel3b = bquote(k == .(Model.MAT[[length(Model.MAT)]][i,3]) ~ "," ~
                             italic(R)^2 == .(format(Model.MAT[[length(Model.MAT)]][i,4], digits = 2)) ~ "," ~
                             RMSE == .(format(Model.MAT[[length(Model.MAT)]][i,5], digits = 2)))
          
          
          text(x = x1, y = ymax.lab2, labels = mylabel1b, col = "royalblue", font = 2, cex = 0.8)
          text(x = x2, y = ymax.lab2, labels = mylabel3b, cex = 0.8)
          }
        
        #### Legend RF ####
        if(is.null(Model.RF) == F){
          mylabel1b = Keep.RF
          mylabel2b = bquote(#Nb.tree == .(Model.RF[[i]][["call"]][["ntree"]]) ~ "," ~
                             italic(R)^2 == .(format(Model.RF[[length(Model.RF)]][i,1], digits = 2)) ~ "," ~
                             RMSE == .(format(Model.RF[[length(Model.RF)]][i,2], digits = 2)))
          
          
          text(x = x4, y = ymax.lab2, labels = mylabel1b, col = "darkorange", font = 2, cex = 0.8, pos = 1)
          text(x = x5, y = ymax.lab2, labels = mylabel2b, cex = 0.8, pos = 1)
          }
      
      
        #### Legend BRT ####
        if(is.null(Model.BRT) == F){
          mylabel1b = Keep.BRT
          mylabel2b = bquote(Nb.tree == .(Model.BRT[[i]][["call"]][["ntree"]]) ~ "," ~
                               italic(R)^2 == .(format(Model.BRT[[length(Model.BRT)]][i,1], digits = 2)) ~ "," ~
                               RMSE == .(format(Model.BRT[[length(Model.BRT)]][i,2], digits = 2)))
          
          
          text(x = x4, y = ymax.lab1, labels = mylabel1b, col = "darkgreen", font = 2, cex = 0.8, pos = 1)
          text(x = x5, y = ymax.lab1, labels = mylabel2b, cex = 0.8, pos = 1)
      }
    }
    
      #### Plot Model MAT add ####
      if (is.null(Model.MAT) == F & Only.fit == F){
        lines(MAge, Cor.MAT$fit[,2], col = "royalblue", lwd=1, las=0)}
      
      #### Plot Model RF add ####
      if(is.null(Model.RF) == F & Only.fit == F){
        lines(MModel.RF[[1]], Cor.RF, col = "darkorange", lwd=1, las=0)}
      
      #### Plot Model BRT add ####
      if(is.null(Model.BRT) == F & Only.fit == F){
        lines(MModel.BRT[[1]], Cor.BRT, col = "darkgreen", lwd=1, las=0)}
      
      #### Plot fitting ####
      if(Fit.val > 0){
        if (is.null(Model.WAPLS) == F){
          Curve.fit = lowess(Cor.WAPLS$fit[,NComp.WAPLS], f = Fit.val)
          lines(MAge, Curve.fit$y, col=1, lwd=2)}
        if (is.null(Model.MAT) == F){
          Curve.fit.MAT = lowess(Cor.MAT$fit[,2], f = Fit.val)
          lines(MAge, Curve.fit.MAT$y, col= "royalblue", lwd=2)}
        if (is.null(Model.RF) == F){
          Curve.fit.MAT = lowess(Cor.RF, f = Fit.val)
          lines(MAge, Curve.fit.MAT$y, col = "darkorange", lwd=2)}
        if (is.null(Model.BRT) == F){
          Curve.fit.MAT = lowess(Cor.BRT, f = Fit.val)
          lines(MAge, Curve.fit.MAT$y, col = "darkgreen", lwd=2)}
          }
      
      #### Interval WAPLS ####
      if (Ecartype.curve[1] == T & is.null(Model.WAPLS) == F){
        lines(MAge, Cor.WAPLS$fit[,NComp.WAPLS] + Cor.WAPLS$SEP.boot[,1], lwd=.4, lty = "dashed")
        lines(MAge, Cor.WAPLS$fit[,NComp.WAPLS] - Cor.WAPLS$SEP.boot[,1], lwd=.4, lty = "dashed")
      }
      
      #### Interval MAT ####
      if (Ecartype.curve[2] == T & is.null(Model.MAT) == F){
        lines(MAge, Cor.MAT$fit[,2] + Cor.MAT$SEP.boot[,1], lwd=.4, col = "royalblue", lty = "dashed")
        lines(MAge, Cor.MAT$fit[,2] - Cor.MAT$SEP.boot[,1], lwd=.4, col = "royalblue", lty = "dashed")
      }
      
      #### Interval RF ####
      if (Ecartype.curve[3] == T & is.null(Model.RF) == F){
        lines(MAge, Cor.RF + RMSE.RF, lwd=.4, col = "darkorange", lty = "dashed")
        lines(MAge, Cor.RF - RMSE.RF, lwd=.4, col = "darkorange", lty = "dashed")
      }
       
      #### Interval BRT ####
      if (Ecartype.curve[4] == T & is.null(Model.BRT) == F){
        lines(MAge, Cor.BRT + RMSE.BRT, lwd=.4, col = "darkgreen", lty = "dashed")
        lines(MAge, Cor.BRT - RMSE.BRT, lwd=.4, col = "darkgreen", lty = "dashed")
      }
      
      #### Plot climate zones ####
      if (Zone.OK == T){
        for(j in 1:length(Zone.Clim.span)){
          if(as.logical(j%%2) == T){        # seulement les j impairs
            a <- which(MAge>Zone.Clim.span[j])
            b <- which(MAge>Zone.Clim.span[j+1])
            #Ymax <- max(Cor.WAPLS$fit[a[1]:b[1],NComp] + Cor.WAPLS$SEP.boot[a[1]:b[1],NComp])
            #Ymin <- min(Cor.WAPLS$fit[a[1]:b[1],NComp] - Cor.WAPLS$SEP.boot[a[1]:b[1],NComp])
            chaud = rgb(1, 0, 0, 0.08)
            froid = rgb(0, 0, 1, 0.08)
            unknow = rgb(0.3, 0.3, 0.3, 0.08)
            if(Zone.Temp[j]=="C"){colTemp <- froid}
            if(Zone.Temp[j]=="W"){colTemp <- chaud}
            if(Zone.Temp[j]=="U"){colTemp <- unknow}
            polygon(c(Zone.Clim.span[j],Zone.Clim.span[j+1], Zone.Clim.span[j+1],Zone.Clim.span[j]), c(ymin, ymin, ymax, ymax), border = NA, col = colTemp)
          }}}
      }
    if(Verbose == F){
      end[i] <- Sys.time()
      setTxtProgressBar(pb, i)
      time <- round(seconds_to_period(sum(end - init)), 0)
      est <- (length(Model.type)-1) * (mean(end[end != 0] - init[init != 0])) - time
      remainining <- round(seconds_to_period(est), 0)
      cat(paste(" // Execution time:", time,
                " // Estimated time remaining:", remainining), "")}
    
    }
  
  if(Verbose == F){
    close(pb)
    library(beepr)
    # beep(3)
    } 
  
  #### Save / export data ####
  row.names(Mmodel) <- row.names(MCore)
  par(mfrow = c(1,1))
  if(Save.tab == T){
    Path.to.create.csv <- gsub("(.*/).*\\.csv.*","\\1", Save.path)
    dir.create(file.path(Path.to.create.csv), showWarnings = FALSE)
    
  if(is.null(Model.WAPLS) == F){
    #### Save WAPLS ####
    Save.Model.WAPLS <- gsub("\\.", "_", Keep.WAPLS)
    add.to.path <- paste("_", Save.Model.WAPLS, ".csv", sep = "")
    add.to.path.error <- paste("_SEP_", Save.Model.WAPLS, ".csv", sep = "")
    Save.path1 <- gsub("\\.csv", add.to.path, Save.path)
    Save.path.error <- gsub("\\.csv", add.to.path.error, Save.path)
    write.table(Mmodel, file = Save.path1, row.names=T, col.names=NA, sep=",", dec = ".")
    write.table(M.errors.WAPLS, file = Save.path.error, row.names=T, col.names=NA, sep=",", dec = ".")
    }}
  
  if(is.null(Model.MAT) == F){
    #### Save MAT ####
    if(Save.tab == T){
      Save.Model.WAPLS <- gsub("\\.", "_", Keep.MAT)
      add.to.path <- paste("_", Save.Model.WAPLS, ".csv", sep = "")
      add.to.path.error <- paste("_SEP_", Save.Model.WAPLS, ".csv", sep = "")
      Save.path2 <- gsub("\\.csv", add.to.path, Save.path)
      Save.path2.error <- gsub("\\.csv", add.to.path.error, Save.path)
      write.table(MModel.MAT, file = Save.path2, row.names=T, col.names=NA, sep=",", dec = ".")
      write.table(M.errors.MAT, file = Save.path2.error, row.names=T, col.names=NA, sep=",", dec = ".")
    }
    
    #### Add MAT to function return ####
    Mmodel = list(Mmodel, MModel.MAT, M.errors.WAPLS, M.errors.MAT)
    Save.DB.name <- gsub(".*\\.","", Keep.WAPLS)
    labtot <- c(Keep.WAPLS, Keep.MAT, paste("SEP.WAPLS", Save.DB.name, sep = "."), paste("SEP.MAT", Save.DB.name, sep = "."))
    names(Mmodel) <- labtot}
    
  if(is.null(Model.RF) == F){
    #### Save RF ####
    if(Save.tab == T){
      Save.Model.WAPLS <- gsub("\\.", "_", Keep.RF)
      add.to.path <- paste("_", Save.Model.WAPLS, ".csv", sep = "")
      Save.path2 <- gsub("\\.csv", add.to.path, Save.path)
      write.table(MModel.RF, file = Save.path2, row.names=T, col.names=NA, sep=",", dec = ".")
    }
    
    #### Add RF to function return ####
    Mmodel <- append(Mmodel, list(MModel.RF))
    names(Mmodel)[length(Mmodel)] <- Keep.RF
    }

  if(is.null(Model.BRT) == F){
    #### Save BRT ####
    if(Save.tab == T){
      # print(Keep.BRT)
      Save.Model.BRT <- gsub("\\.", "_", Keep.BRT)
      add.to.path <- paste("_", Save.Model.BRT, ".csv", sep = "")
      Save.path2 <- gsub("\\.csv", add.to.path, Save.path)
      write.table(MModel.BRT, file = Save.path2, row.names=T, col.names = NA, sep=",", dec = ".")
    }
    
    #### Add BRT to function return ####
    Mmodel <- append(Mmodel, list(MModel.BRT))
    names(Mmodel)[length(Mmodel)] <- Keep.BRT
  }
  
  #### Save format RDS ####
  if(Save.RDS == T & is.null(Save.path) == F){
    Total.model <- list(WAPLS = Full.WAPLS.RDS, MAT = Full.MAT.RDS, BRT = Full.BRT.RDS, RF = Full.RF.RDS)
    
    if(is.null(Model.BRT) == F){Save.DB.name <- gsub(".*\\.","", Keep.BRT)}
    if(is.null(Model.WAPLS) == F){Save.DB.name <- gsub(".*\\.","", Keep.WAPLS)}
    if(is.null(Model.MAT) == F){Save.DB.name <- gsub(".*\\.","", Keep.MAT)}
    if(is.null(Model.RF) == F){Save.DB.name <- gsub(".*\\.","", Keep.RF)}
    
    Save.path.RDS <- paste(gsub("\\.csv", paste("_", Save.DB.name, sep = ""), Save.path), ".Rds", sep = "")
    Save.path.RDS.full <- paste(gsub("\\.csv", paste("_", Save.DB.name, "_full", sep = ""), Save.path), ".Rds", sep = "")
    Path.to.create2 <- gsub("(.*/).*\\.Rds.*","\\1", Save.path.RDS)
    dir.create(file.path(Path.to.create2), showWarnings = F)
    
    saveRDS(Total.model, Save.path.RDS.full)
    saveRDS(Mmodel, Save.path.RDS)}
  
  #### End ####
  if(is.null(Save.plot) == F){dev.off()}
  #return(Mmodel)
  return(Total.model)
}

Plot.relation.Surf.Clim <- function(MPol, MClim, Nb.max, Label2, Label3, L.position,
                                    Save.plot, H, W, Name.taxa, Plot.manual, Lab.clim
                                    ){
  #### Init Val ####
  if(missing(Nb.max)){Nb.max = 9}
  if(missing(Label2)){Label2 = NULL}
  if(missing(Label3)){Label3 = NULL}
  if(missing(Plot.manual)){Plot.manual = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Name.taxa)){Name.taxa = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(L.position)){L.position = rep("bottom", Nb.max)}

  #### Definitions nom taxa ####
  # Cor.name <- Name.taxa$Label[match(names(MPol), Name.taxa[[4]])]
  Cor.name <- Name.taxa$Label[match(names(MPol), Name.taxa["Nom"])]
  print(Cor.name)
  Yo <- matrix(0, ncol = ncol(MClim), nrow = ncol(MPol))
  colnames(Yo) <- names(MClim)
  row.names(Yo) <- Cor.name

  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Graphical settings ####
  if (Nb.max <= 3){par(mfrow = c(1, Nb.max), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max == 4){par(mfrow = c(2,2), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max >= 5 & Nb.max <= 6){par(mfrow = c(2,3), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max >= 7 & Nb.max <= 9){par(mfrow = c(3,3), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max >= 10 & Nb.max <= 12){par(mfrow = c(3,4), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max >= 13 & Nb.max <= 16){par(mfrow = c(4,4), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}
  if (Nb.max >= 17 & Nb.max <= 32){par(mfrow = c(8,4), mar = c(3, 3, 0.5, 0.5), mgp = c(1.7,0.6,0))}

  #### Matrice de correlation ####
  for(i in 1:ncol(MPol)){
    for(j in 1:ncol(MClim)){
      Data <- cbind(MPol[i], MClim[j])
      yo <- paste(names(Data[1]), "~", names(Data[2]))
      RelLin <- lm(yo, data = Data)
      Yo[i,j] = round(summary(RelLin)$r.squared, digits = 6)
      }}
  
  #### Color ####
  if(is.null(Label2) == F){
    if(is.null(Label3) == T){
      Lab <- row.names(MPol)
      a <- paste(".*", Label2, ".*", sep = "")
      Collab <- gsub(a,"RoyalBlue", Lab)
      Collab[Collab != "RoyalBlue"] = "Black"
      }
    
    else{
      Lab <- row.names(MPol)
      a <- paste(".*", Label2, ".*", sep = "")
      Collab <- gsub(a,"RoyalBlue", Lab)
      b <- paste(".*", Label3, ".*", sep = "")
      Collab <- gsub(b,"Orange", Collab)
      Collab[Collab != "RoyalBlue" & Collab != "Orange"] = "Black"
      }
    }
  
  else{Collab <- rep("Black", length(row.names(MPol)))}
  


  #### Selection des Nb.max R2 ####
  Nbval <- dim(Yo)[1]*dim(Yo)[2]
  for(i in 0:(Nb.max-1)){
    MaxI <- sort(Yo, partial = Nbval - i)[Nbval - i]
    a <-which(Yo == MaxI, arr.ind = T)

    for(j in length(a[,1])){ # cas des execos
      if(Plot.manual == F){
        RowMax <- a[j,1]
        ColMax <- a[j,2]}
      
      if(Plot.manual == T){
        i = i+1
        ColMax <- ceiling(i/ncol(MPol))
        RowMax <- i - (ColMax-1)*ncol(MPol)
        }

    #### Plot Mean value ####
    if(missing(Lab.clim)){Lab.disp = names(MClim[ColMax])} 
    else{Lab.disp <- Lab.clim[i]}
    XLAB <- Name.taxa$Label[match(names(MPol[RowMax]), Name.taxa[[4]])]
    plot(MPol[[RowMax]], MClim[[ColMax]], 
         type = "p", 
         ylab = Lab.disp, 
         xlab = XLAB,
         col = Collab 
         )
      
    #### Plot Regression Lineaire ####
    Data1 <- cbind(MPol[RowMax], MClim[ColMax])
    formula <- paste(names(MClim[ColMax]), "~", names(MPol[RowMax]))
    reg1 <- lm(formula, data = Data1)
    abline(reg1)
    Sum1<- summary(reg1)
    
    #### Plot Regression Lineaire SUBPLOT BLEU ####
    if(is.null(Label2) == F){
      Allsite <- row.names(MPol)
      SubSet2.lab <- Allsite[which(grepl(Label2, Allsite) == T)]
      SubSet2.data <- subset(MPol, row.names(MPol) %in% SubSet2.lab)
      SubSet2.clim <- subset(MClim, row.names(MClim) %in% SubSet2.lab)
      Data2 <- cbind(SubSet2.data[RowMax], SubSet2.clim[ColMax])
      formula2 <- paste(names(SubSet2.clim[ColMax]), "~", names(SubSet2.data[RowMax]))
      reg2 <- lm(formula2, data = Data2)
      abline(reg2, col = "Royalblue")
      Sum2<- summary(reg2)
      }
    
    #### Plot Regression Lineaire SUBPLOT ORANGE ####
    if(is.null(Label3) == F){
      Allsite <- row.names(MPol)
      SubSet3.lab <- Allsite[which(grepl(Label3, Allsite) == T)]
      SubSet3.data <- subset(MPol, row.names(MPol) %in% SubSet3.lab)
      SubSet3.clim <- subset(MClim, row.names(MClim) %in% SubSet3.lab)
      
      Data3 <- cbind(SubSet3.data[RowMax], SubSet3.clim[ColMax])
      formula3 <- paste(names(SubSet3.clim[ColMax]), "~", names(SubSet3.data[RowMax]))
      reg3 <- lm(formula3, data = Data3)
      abline(reg3, col = "Orange")
      Sum3<- summary(reg3)
    }
      
    #### Legende regression ####
    if(is.null(Label2) == T & is.null(Label3) == T){
      rp = vector('expression',2)
      rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                         list(MYVALUE = format(Sum1[["r.squared"]], digits = 2)))[2]
      
      if(Sum1$coefficients[2,4] > 0.005){my.p = Sum1$coefficients[2,4]
      rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      else{my.p = 0.005
      rp[2] = substitute(expression(italic(p) <= MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      legend("topright", #bottom
             bty="n", 
             cex = .6, 
             legend = rp)
      }
    
    if(is.null(Label2) == F & is.null(Label3) == T){
      
      rp = vector('expression',2)
      rp[1] = substitute(expression(italic(R)^2 == MYVALUE), list(MYVALUE = format(Sum1[["r.squared"]], digits = 2)))[2]
      if(Sum1$coefficients[2,4] > 0.005){my.p = Sum1$coefficients[2,4]
      rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      else{my.p = 0.005
      rp[2] = substitute(expression(italic(p) <= MYOTHERVALUE), list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      rp2 = vector('expression',2)
      rp2[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                         list(MYVALUE = format(Sum2[["r.squared"]], digits = 2)))[2]
      
      if(Sum2$coefficients[2,4] > 0.005){my.p = Sum2$coefficients[2,4]
      rp2[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      else{my.p = 0.005
      rp2[2] = substitute(expression(italic(p) <= MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      legend("topright", #bottom 
             bty="n", 
             cex = .7, 
             text.font = 4,
             legend = rp)
      
      legend("topright", 
             bty="n", 
             cex = .7,
             text.font = 4,
             text.col = "Royalblue",
             legend = rp2)
    }
    
    if(is.null(Label2) == F & is.null(Label3) == F){
      rp = vector('expression',2)
      rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                         list(MYVALUE = format(Sum1[["r.squared"]], digits = 2)))[2]
      
      if(Sum1$coefficients[2,4] > 0.005){my.p = Sum1$coefficients[2,4]
      rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      else{my.p = 0.005
      rp[2] = substitute(expression(italic(p) <= MYOTHERVALUE), 
                         list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      rp2 = vector('expression',2)
      rp2[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                          list(MYVALUE = format(Sum2[["r.squared"]], digits = 2)))[2]
      
      if(Sum2$coefficients[2,4] > 0.005){my.p = Sum2$coefficients[2,4]
      rp2[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                          list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      else{my.p = 0.005
      rp2[2] = substitute(expression(italic(p) <= MYOTHERVALUE), 
                          list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      rp3 = vector('expression',2)
      rp3[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                          list(MYVALUE = format(Sum3[["r.squared"]], digits = 2)))[2]
      
      if(Sum3$coefficients[2,4] > 0.005){my.p = Sum3$coefficients[2,4]
      rp3[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                          list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      else{my.p = 0.005
      rp3[2] = substitute(expression(italic(p) <= MYOTHERVALUE), 
                          list(MYOTHERVALUE = format(my.p, digits = 4)))[2]}
      
      
      legend(paste(L.position[i], "left", sep = ""), 
             bty="n", 
             cex = .7, 
             text.font = 4,
             legend = rp)
      
      legend(paste(L.position[i], "right", sep = ""), 
             bty="n", 
             cex = .7,
             text.font = 4,
             text.col = "Royalblue",
             legend = rp2)
      
      legend(L.position[i], 
             bty="n", 
             cex = .7,
             text.font = 4,
             text.col = "Orange",
             legend = rp3)
    }
    
    }}
  #### Export ####
  par(mfrow = c(1,1))
  if(is.null(Save.plot) == F){dev.off()}
  return(Yo)
  }

Analogue.map <- function(Model.WAPLS, MCore, MCoord, MAge, 
                         Age.choice, Zone.plot, Clim.choix, H, W, Save.plot, Save.path){
  #### Settings ####
  Age.recup <- names(MAge)[grep("A", names(MAge))]
  library(maps)
  if(missing(Save.path)){Save.plot = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Zone.plot)){Zone.plot = "."}
  if(missing(Age.choice)){
    Nb.max = 4
    Duree.core <- length(MCore[,1])
    levels <- c(1, Duree.core*1/3, Duree.core*2/3, Duree.core)}
  else{
    Nb.max = length(Age.choice)
    levels <- which(MAge[[Age.recup]] %in% Age.choice)
    }
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Graphical settings ####
  if (Nb.max <= 3){par(mfrow = c(1, Nb.max), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max == 4){par(mfrow = c(2,2), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max >= 5 & Nb.max <= 6){par(mfrow = c(2,3), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max >= 7 & Nb.max <= 9){par(mfrow = c(3,3), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max >= 10 & Nb.max <= 12){par(mfrow = c(3,4), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max >= 13 & Nb.max <= 16){par(mfrow = c(4,4), mai = c(0.6, 0.6, 0.2, 0.2))}
  if (Nb.max >= 17 & Nb.max <= 32){par(mfrow = c(8,4), mai = c(0.6, 0.6, 0.2, 0.2))}
  colfunc  <- colorRampPalette(c("red", "royalblue"))  # degrade couleur par Nb analogue
  
  
  #### Extract analogues ####
  Cor.MAT=predict(Model.WAPLS[[Clim.choix]], MCore, sse = T, nboot = 1000, verbose = T)
  analogues <- Cor.MAT$match.name
  
  for (i in 1:Nb.max) {
    level <- levels[i]
    names <- sapply(analogues[level, ], as.character)
    ColAnal <- colfunc(length(names))
    coords <- MCoord[names, ]
    map("world",  regions = Zone.plot , col="grey", mar = c(0,0,0,0))  
    points(MCoord$LONG, MCoord$LAT, pch=19, cex=0.3, col="lightgreen")
    points(coords$LONG, coords$LAT, pch=19, cex=1, col = ColAnal)
    title(paste(round(MAge[level, Age.recup], 0), "BP"))
    }
  
  #### Labels Analogues Order ####
  legend("bottomright",
         legend = colnames(analogues),
         pch = 19,
         cex = .8,
         text.width = .8,
         col = ColAnal,
         bty = 'n'                                            # pas bordure 
  )
  
  #### Export ####
  par(mfrow = c(1,1))
  if(is.null(Save.plot) == F){dev.off()}
  if(is.null(Save.path) == F){write.table(analogues, file=Save.path, row.names=T, col.names=NA, sep=",", dec = ".")}
  #print(analogues)
  return(analogues)
}

# Check the spacial auto-correlation of a database (Latitude / Longitude) 
Spatial.autocor.check <- function(Mcoord, Mvariable){
  #### Initialization value ####
  if(missing(Mcoord)){print("Import the Latitude and the Longitude of the sites.")}
  if(missing(Mvariable)){print("Import the matric of the variables you want to check the autocorrelation.")}
  library("geosphere")
  library("ape")
  
  #### Check size of DB ####
  if(length(setdiff(row.names(Mcoord), row.names(Mvariable))) >=1){
    print(paste("The following site coord doesn't match with the variable :", setdiff(row.names(Mcoord), row.names(Mvariable))))
    Mcoord <- Mcoord[-which(row.names(Mcoord) %in% setdiff(row.names(Mcoord), row.names(Mvariable))),]
  }
  if(length(setdiff(row.names(Mvariable), row.names(Mcoord))) >=1){
    Mvariable <- Mvariable[-which(row.names(Mvariable) %in% setdiff(row.names(Mvariable), row.names(Mcoord))),]}

  #### Calcul inverse distance matrix ####
  Lat <- Mcoord[,grep("LAT|lat|Lat", names(Mcoord))]
  Long <- Mcoord[,grep("LONG|long|Long", names(Mcoord))]
  M <- cbind(Long, Lat)
  row.names(M) <- row.names(Mcoord)
  M.dists <- distm(M, fun = distVincentyEllipsoid)
  M.dists <- 1/M.dists                         # on inverse
  diag(M.dists) <- 0                   # on mets 0 sur la diagonal
  colnames(M.dists) <- row.names(Mcoord)
  row.names(M.dists) <- row.names(Mcoord)
  
  # /!\ is.finite(M.dists) /!\
  M.dists[is.finite(M.dists)==F] <- 1
  
  #### Calcule de l'indice Moiran's I #### (Telfort and Birk 2005)
  # test significant if (p < 0.01), SD -> 0 peu de variance au sein DB
  # MI.expected = -1/(n-1)
  # MI.observed > MI.exp -> + auto-correlation spatial positive
  Moran.I <- data.frame(I=0, E=0, SD=0, p.val=0)#, z.score=0)
  for(i in 1:length(names(Mvariable))){
    Moran.I.i <- as.matrix(t(Moran.I(Mvariable[,i], M.dists)))
    z.score = (Moran.I.i$I - Moran.I.i$E)/Moran.I.i$SD
    colnames(Moran.I.i) <- names(Moran.I)
    row.names(Moran.I.i) <- names(Mvariable)[i]
    Moran.I <- rbind(Moran.I, Moran.I.i)
    }
  
  Moran.I <- Moran.I[-1,]
  return(Moran.I)
}

MAT.WAPLS.Ncomp.influ <- function(Model.WAPLS, MCore, MAge, NComp, Param.clim){
  #### Settings ####
  library(ggplot2)      # permet de faire des graphiques esthetiques
  library(reshape)      # permet d'utiliser la fonction melt utile pour afficher les graphs
  library(RColorBrewer) # choix des couleurs perso
  library(ggrepel)      # label à la fin des lignes
  library(ggnewscale)

  #### Graphic settings #### 
  my_orange = brewer.pal(n = 9, "Oranges")[3:9]
  orange_palette = colorRampPalette(c(my_orange[1], my_orange[4], my_orange[6]), space = "Lab")
  my_orange2 = orange_palette(NComp)
  
  #### Prediction FT ####
  Mmodel <- data.frame(Age = MAge)
  Mfull <- list()

  for(i in 1:NComp){
    print(paste("Calcul :", round(i/NComp*100), "%", sep = " "))
    Cor.MAT=predict(Model.WAPLS[[Param.clim]], MCore, k = i, sse=T, nboot=1000, verbose = F)
    Mmodel[i+1] <- Cor.MAT$fit[,2]
    }
  names(Mmodel) <- c("Age", paste("k", 1:NComp, sep = ""))
  Mmodel<- melt(Mmodel, id = "Age")
  
  #### Plot FT vs. number k ####
  
  p <- ggplot(data = Mmodel, mapping = aes(x = Age, y = value, color = variable)) +
    geom_line(linetype = "solid", size=0.6, alpha = 4)    +  # dashted, dotted, solid
    geom_point(size = 1.8, alpha = .4)+ #shape = 20
    scale_colour_manual("", values=my_orange2)+
    facet_grid(rows = vars(variable))  
  
  print(Mmodel)
  print(p)
  return(Mmodel)
}

Total.FT.EAPDB <- function(x, Lake.name, Select.clim, Select.calib, Select.model, Save.path, Save.plot){
  #### Initial settings ####
  if(missing(Select.calib)){warning("**** Select a calibration please ! ****")}
  if(missing(Select.model)){Select.model <- c("WAPLS", "BRT")}
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  print("*** Please check if the modern calibration set is loaded or not ! ***")
  MAT = NULL
  BRT = NULL
  RF = NULL
  WAPLS = NULL
  
  #### Global clean ####
  Abscisse <- c("Age","Depth", "agebp", "MAge")
  Age <- x[,names(x) %in% Abscisse]
  x[,names(x) %in% Abscisse] <- NA
  x <- x[, !names(x) %in% Abscisse]
  x <- data.frame(sapply(as.data.frame(x), as.double))
  x <- data.frame(x/rowSums(x))
  print(paste("FT calculation for:", Lake.name))
  
  
  #### Loop on the calibs ####
  for(i in Select.calib){
    Save.path = paste(Save.path, Lake.name, "/", Lake.name, "_", i, ".csv", sep = "")
    Save.plot = paste(Save.plot, Lake.name, "/", Lake.name, "_", i, ".pdf", sep = "")
    
    if("WAPLS" %in% Select.model){WAPLS <- eval(parse(text = (paste("WAPLS.", i, sep = ""))))}
    if("BRT" %in% Select.model){BRT <- eval(parse(text = (paste("BRT.", i, sep = ""))))}
    if("RF" %in% Select.model){RF <- eval(parse(text = (paste("RF.", i, sep = ""))))}
    if("MAT" %in% Select.model){MAT <- eval(parse(text = (paste("MAT.", i, sep = ""))))}
    
    if(i == "EAPDB"){x2 <- Fossil.corresp.surface(x, DB.odile.Po)}
    if(i == "COSTDB"){x2 <- Fossil.corresp.surface(x, DB.odile.Po.COST)}
    if(i == "WASTDB"){x2 <- Fossil.corresp.surface(x, DB.odile.Po.WAST)}
    if(i == "MEDTEMP"){x2 <- Fossil.corresp.surface(x, DB.MEDTEMP.Po)}
    if(i == "TEMPSCAND"){x2 <- Fossil.corresp.surface(x, DB.TEMPSCAND.Po)}
    
    x2 <- data.frame(x2/rowSums(x2))
    
    x2 <- FT.core(
      Model.WAPLS = WAPLS,
      Model.MAT = MAT,
      Model.RF = RF,
      Model.BRT = BRT,
      MCore = x2,
      MAge = Age,
      Fit.val = 0.25,
      Select.clim = Select.clim,
      LakeName = Lake.name,
      Only.fit = T, Verbose = F,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T, Displot = T,
      # Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
      # Zone.Temp = c("C","W","C","W","C", "W"),
      Save.RDS = T,
      Save.path = Save.path,
      Save.plot = Save.plot,
      H = 1100, W = 1900
    )
  }
  return(x2)
}

#### Import Maps ####
if(exists("ACA.bo") == F){
  library(rgdal)
  Path.ACA.border = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Projets/ACA/Borders_ACA/Extern_border/Out_border_ACA.shp"
  ACA.bo = readOGR(Path.ACA.border)
  proj4string(ACA.bo) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  ACA.bo.proj = fortify(ACA.bo)
  
  Eurasia_map <- c("Mongolia", "Russia", "Spain", "France", "Italy", "Greece", "Germany", "Finland", "Bhutan", "Bangladesh",
                   "Czech Republic", "Denmark", "Kosovo", "Vietnam", "Laos", "Japan", "Nepal", "India", "Myanmar",
                   "Latvia","Lithuania", "Estonia", "Belarus", "Romania", "Bulgaria", "Hungary", "Austria", "Croatia",
                   "Albania", "Serbia", "Slovenia", "Slovakia", "Bosnia", "Montenegro", "UK", "Ireland", "Moldova", "Macedonia",
                   "Norway", "Sweden", "Turkey", "Belgium", "Uzbekistan", "Tajikistan", "Syria", "Israel", "Jordan", "Pakistan",
                   "Kazakhstan","Turkmenistan", "Ukraine", "Poland", "Portugal", "Switzerland", "Kyrgyzstan", "Morocco",
                   "China", "Iran", "Armenia", "Georgia", "Afghanistan", "Iraq", "Azerbaijan")
  Eurasia_map <- map_data("world", region = Eurasia_map)
  
  CircumCasp_map <- c("Russia", "Turkey", "Uzbekistan", "Tajikistan", "Syria", "Israel", "Jordan", "Pakistan",
                      "Kazakhstan","Turkmenistan", "Kyrgyzstan", "China", "Iran", "Armenia", "Georgia", "Afghanistan", "Iraq", "Azerbaijan")
  CircumCasp_map <- map_data("world", region = CircumCasp_map)
  
  TUSD_map <- c("Uzbekistan", "Tajikistan")
  TUSD_map <- map_data("world", region = TUSD_map)
  TUSD_map = fortify(TUSD_map)
  
  Couleur.Prentice <- c(
    "WAMX"="#185699FF","WAST"="#e2a064ff","XERO"="#cf0156ff","CLMX"="#ee9b2fff","COMX"="#c1b646ff","TEDE"="#72a72bff","HODE"="#fcf8b6ff",
    "TAIG"="#32156eff","TUND"="#caa6c2ff","PION"="#0e3056ff","ANTH"="#6e1d2cff","COCO"="#9d2e58ff","COST"="#f3c768ff","CODE"="#d6d81eff","CLDE"="#b1d5f0ff")
  
  
  Map_theme_grid <- theme(
    axis.title = element_blank(), title = element_text(size = 20),
    legend.title = element_text(size = 12), legend.key = element_blank(), legend.justification = c("center"), legend.text = element_text(size = 10), 
    legend.position = c(0.95, 0.35), legend.background = element_rect(fill = "#EBEBEB"),
    panel.background = element_rect(fill = "#EBEBEB", colour = "black"), panel.border = element_rect(fill = NA, colour = "black"),
    plot.background = element_blank(), plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")
  )
  
  map_empty  <-  ggplot()+
    geom_polygon(data = Eurasia_map, aes(x=long, y=lat, group = group), alpha = 1, fill = "grey85", color = "grey30", size = 0.3)+
    scale_color_manual(values = Couleur.Prentice, name = "Biomization")+
    coord_quickmap(xlim = c(-3,175), ylim = c(10, 80)) +
    guides(colour = guide_legend(override.aes = list(size = 5.5))) + Map_theme_grid
  
}
#### Import Database ####
DB.Odile.calc = F
if(DB.Odile.calc == T){
  Old.V = F
  if(Old.V == T){
    #### Import old DB Odile ####
    DB3058PolClim <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3058/DB_Odile_3058PolClim.txt", sep="\t", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    DB3058Biome   <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3058/DB_Odile_3058Biom.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    DB3191Clim    <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3191/DB_Odile_3191Clim.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    DB3191Pol     <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3191/DB_Odile_3191Pol.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    DB3191Coord   <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3191/DB_Odile_3191Coord.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    DBMong.Coord  <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/Sites/DB_Odile_MongCoord.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    DB3191Clim[which(DB3191Clim$PANN < 0),"PANN"] <- DB3191Clim[which(DB3191Clim$PANN < 0),"SUMMERPR"]
    DB3058PolClim[which(DB3058PolClim$PANN < 0),"PANN"] <- DB3058PolClim[which(DB3058PolClim$PANN < 0),"SUMMERPR"]
    
    }
  else{
    #### Import DB Odile ####
    # ATTENTION : les données sont déjà en pourcentage ! Donc vérifier que ca passe pas en pourmilles
    # ATTENTION : choisir le CLIMAT -- Odile NNR / WC / CHELSA
    DB.odile.B  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "b.Rds", sep = ""))
    DB.odile.Po  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "po.Rds", sep = ""))
    DB.odile.Co  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "co.Rds", sep = ""))
    if(as.integer(gsub("DB", "", Version.DB)) > 3500){
      DB.odile.Cl_wc  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "cl_wc.Rds", sep = ""))
      DB.odile.Cl_chel  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "cl_chel.Rds", sep = ""))
      DB.odile.Cl <- DB.odile.Cl_wc}
    else{DB.odile.Cl  <- readRDS(paste("Resultats/World_DB/Pollen/Odile_DB/", Version.DB, "/ss", gsub("DB", "", Version.DB), "cl.Rds", sep = ""))}
    
    #### Prep/check datas ####
    DB.odile.Po[DB.odile.Po <= 0.01] <- 0             # remove taxa < 1 %
    DB.odile.Po <- (DB.odile.Po/rowSums(DB.odile.Po)) 
    DB.odile.Cl <- Surf.Clim.Pol(DB.odile.Po, DB.odile.Cl)
    DB.odile.Po <- Surf.check.clim(DB.odile.Po, DB.odile.Cl, Error.display = F)
    DB.odile.Cl[which(DB.odile.Cl$MAP < 0),"MAP"] <- DB.odile.Cl[which(DB.odile.Cl$MAP < 0),"SUMMERPR"]
    DB.odile.Co <-  DB.odile.Co[row.names(DB.odile.Co) %in% row.names(DB.odile.Po),]
    DB.odile.B <-  DB.odile.B[which(row.names(DB.odile.B) %in% row.names(DB.odile.Po)),1, drop = F]
    if(as.integer(gsub("DB", "", Version.DB)) > 3500){
      DB.odile.Cl <- subset(DB.odile.Cl, select = c(MAAT, Tspr, Tsum, MTWAQ, MTCOQ, TS, MAP, Pspr, Pwin, AI, Latitude, Longitude))
    }
    else{
      names(DB.odile.Co)[names(DB.odile.Co) == "LATI"] <- "LAT"
    }
    
    #### Extention spatiale DB ####
    Map.Eurasia <- c("Mongolia", "Russia", "Spain", "France", "Italy", "Greece", "Germany", "Finland",
                     "Czech Republic", "Denmark", "Kosovo",
      "Latvia","Lithuania", "Estony", "Belarus", "Romania", "Bulgaria", "Hungary", "Austria", "Croatia",
      "Albania", "Serbia", "Slovenia", "Slovakia", "Bosnia", "Montenegro", "UK", "Ireland", "Moldova", "Macedonia",
      "Norway", "Sweden", "Turkey", "Belgium", "Uzbekistan", "Tajikistan", "Syria", "Israel", "Jordan",
      "Kazakhstan","Turkmenistan", "Ukraine", "Poland", "Portugal", "Switzerland", "Kyrgyzstan", "Morocco",
      "China", "Iran", "Armenia", "Georgia", "Afghanistan", "Iraq", "Azerbaijan")
    
    #### Extractions par biomes ####
    Extract = T
    if(Extract == T){
      #### EAPDB ####
      EAPDB.pick = T
      if(EAPDB.pick == T){
        DB.odile.Co$LONG[DB.odile.Co$LONG == -178] <- 182
        DB.odile.Po <- DB.odile.Po[colSums(DB.odile.Po)!=0]   # remove taxa empty
        DB.odile.Po <- DB.odile.Po[,sort(names(DB.odile.Po))] # sort taxa alphabetically
      }
      #### TAIGDB ####
      TAIGDB.pick = T
      if(TAIGDB.pick == T){
        DB.odile.B.TAIGDB <- subset(DB.odile.B, BIOMPOL_ == "TAIG")
        DB.odile.Cl.TAIGDB <- subset(DB.odile.Cl, row.names(DB.odile.Cl) %in% row.names(DB.odile.B.TAIGDB))
        DB.odile.Co.TAIGDB <- subset(DB.odile.Co, row.names(DB.odile.Co) %in% row.names(DB.odile.B.TAIGDB))
        DB.odile.Po.TAIGDB <- subset(DB.odile.Po, row.names(DB.odile.Po) %in% row.names(DB.odile.B.TAIGDB))
        DB.odile.Po.TAIGDB <- DB.odile.Po.TAIGDB[colSums(DB.odile.Po.TAIGDB)!=0] # remove taxa empty
        
        DB.odile.Co.TAIGDB.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_TAIGDB_Co.csv"
        write.table(DB.odile.Co.TAIGDB, file = DB.odile.Co.TAIGDB.path, row.names=T, col.names=NA, sep=",", dec = ".")
        DB.odile.Po.TAIGDB.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_TAIGDB_Po.csv"
        write.table(DB.odile.Po.TAIGDB, file = DB.odile.Po.TAIGDB.path, row.names=T, col.names=NA, sep=",", dec = ".")
        }      
      
      #### COSTDB ####
      COST.pick = T
      if(COST.pick == T){
        DB.odile.B.COST <- subset(DB.odile.B, BIOMPOL_ == "COST")
        DB.odile.Cl.COST <- subset(DB.odile.Cl, row.names(DB.odile.Cl) %in% row.names(DB.odile.B.COST))
        DB.odile.Co.COST <- subset(DB.odile.Co, row.names(DB.odile.Co) %in% row.names(DB.odile.B.COST))
        DB.odile.Po.COST <- subset(DB.odile.Po, row.names(DB.odile.Po) %in% row.names(DB.odile.B.COST))
        DB.odile.Po.COST <- DB.odile.Po.COST[colSums(DB.odile.Po.COST)!=0] # remove taxa empty
        
        DB.odile.Co.COST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_COST_Co.csv"
        write.table(DB.odile.Co.COST, file = DB.odile.Co.COST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        DB.odile.Po.COST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_COST_Po.csv"
        write.table(DB.odile.Po.COST, file = DB.odile.Po.COST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        stop()
        }
      
      #### WASTDB ####
      WAST.pick = T
      if(WAST.pick == T){
        DB.odile.B.WAST <- subset(DB.odile.B, BIOMPOL_ == "WAST")
        DB.odile.Cl.WAST <- subset(DB.odile.Cl, row.names(DB.odile.Cl) %in% row.names(DB.odile.B.WAST))
        DB.odile.Co.WAST <- subset(DB.odile.Co, row.names(DB.odile.Co) %in% row.names(DB.odile.B.WAST))
        DB.odile.Po.WAST <- subset(DB.odile.Po, row.names(DB.odile.Po) %in% row.names(DB.odile.B.WAST))
        DB.odile.Po.WAST <- DB.odile.Po.WAST[colSums(DB.odile.Po.WAST)!=0] # remove taxa empty
        
        DB.odile.Co.WAST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_WAST_Co.csv"
        write.table(DB.odile.Co.WAST, file = DB.odile.Co.WAST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        DB.odile.Po.WAST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_WAST_Po.csv"
        write.table(DB.odile.Po.WAST, file = DB.odile.Po.WAST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        }
      
      #### STDB ####
      ST.pick = T
      if(ST.pick == T){
        DB.odile.B.ST <- subset(DB.odile.B, BIOMPOL_ %in% c("WAST", "COST"))
        DB.odile.Cl.ST <- subset(DB.odile.Cl, row.names(DB.odile.Cl) %in% row.names(DB.odile.B.ST))
        DB.odile.Co.ST <- subset(DB.odile.Co, row.names(DB.odile.Co) %in% row.names(DB.odile.B.ST))
        DB.odile.Po.ST <- subset(DB.odile.Po, row.names(DB.odile.Po) %in% row.names(DB.odile.B.ST))
        DB.odile.Po.ST <- DB.odile.Po.ST[colSums(DB.odile.Po.ST)!=0] # remove taxa empty
        
        DB.odile.Co.ST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_ST_Co.csv"
        write.table(DB.odile.Co.ST, file = DB.odile.Co.ST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        DB.odile.Po.ST.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_ST_Po.csv"
        write.table(DB.odile.Po.ST, file = DB.odile.Po.ST.path, row.names=T, col.names=NA, sep=",", dec = ".")
        }
      
      #### MEDTEMP ####
      MEDTEMP.pick = T
      if(MEDTEMP.pick == T){
        DB.MEDTEMP.Cl <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_Med.TempEuro_cl.csv", 
                                  row.names = 1, header = T, sep = ';', dec = ".") # Coordon?es DB Julien
        DB.MEDTEMP.Po <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_Med.TempEuro_po.csv", 
                                  row.names = 1, header = T, sep = ';', dec = ".") # Coordon?es de Julien
        DB.MEDTEMP.Co <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_Med.TempEuro_co.csv", 
                                  row.names = 1, header = T, sep = ';', dec = ".") # Coordon?es de Julien
        
        DB.MEDTEMP.Po <- subset(DB.MEDTEMP.Po, row.names(DB.MEDTEMP.Po) %in% row.names(DB.MEDTEMP.Co)) # Recoupe les sites de EAPDB Odile (pollen) en fonction des sites Julien 
        DB.MEDTEMP.Co <- subset(DB.MEDTEMP.Co, row.names(DB.MEDTEMP.Co) %in% row.names(DB.MEDTEMP.Po)) # Recoupe la DB coordonn?es Julien en fonction du nouveau nombre de site (pr?c?dent)  
        DB.MEDTEMP.Cl <- subset(DB.MEDTEMP.Cl, row.names(DB.MEDTEMP.Cl) %in% row.names(DB.MEDTEMP.Co)) # Recoupe la DB clim Julien en fonction du nouveau nombre de site (pr?c?dent)  
        DB.MEDTEMP.Po <- DB.MEDTEMP.Po[colSums(DB.MEDTEMP.Po)!=0] # remove taxa empty
        
      }
      #### TEMPSCAND ####
      TEMPSCAND.pick = T
      if(TEMPSCAND.pick == T){
        DB.TEMPSCAND.Cl <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_TempEuro.Scand_cl.csv", 
                                    row.names = 1, header = T, sep = ';', dec = ".") # Coordon?es DB Julien
        DB.TEMPSCAND.Po <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_TempEuro.Scand_po.csv", 
                                    row.names = 1, header = T, sep = ';', dec = ".", stringsAsFactors = F) # pollen DB Julien
        DB.TEMPSCAND.Co <- read.csv(file = "Import/World_DB/Pollen/Lea_DB/DB_TempEuro.Scand_co.csv", 
                                    row.names = 1, header = T, sep = ';', dec = ".", stringsAsFactors = F) # pollen DB Julien
        
        DB.TEMPSCAND.Po <- subset(DB.TEMPSCAND.Po, row.names(DB.TEMPSCAND.Po) %in% row.names(DB.TEMPSCAND.Co)) # Recoupe les sites de EAPDB Odile (pollen) en fonction des sites Julien 
        DB.TEMPSCAND.Co <- subset(DB.TEMPSCAND.Co, row.names(DB.TEMPSCAND.Co) %in% row.names(DB.TEMPSCAND.Po)) # Recoupe la DB coordonn?es Julien en fonction du nouveau nombre de site (pr?c?dent)  
        DB.TEMPSCAND.Cl <- subset(DB.TEMPSCAND.Cl, row.names(DB.TEMPSCAND.Cl) %in% row.names(DB.TEMPSCAND.Co)) # Recoupe la DB clim Julien en fonction du nouveau nombre de site (pr?c?dent)  
        DB.TEMPSCAND.Po <- DB.TEMPSCAND.Po[colSums(DB.TEMPSCAND.Po)!=0] # remove taxa empty
      }
      
      #### ACADB ####
      ACADB.pick = T
      if(ACADB.pick == T){
        DB.odile.Co <- DB.odile.Co[which(is.na(DB.odile.Co$LONG) == F),]
        DB.odile.Co <- SpatialPointsDataFrame(coords = DB.odile.Co[,3:2], data = DB.odile.Co, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        ACADB.Co <- data.frame(DB.odile.Co[ACA.bo,])
        ACADB.Co <- ACADB.Co[c(1:4)]
        
        Test.dup <- ACADB.Co[duplicated(ACADB.Co[c(1:3)])|duplicated(ACADB.Co[c(1:3)], fromLast = T),]
        Test.dup <- Test.dup[order(Test.dup$LAT, Test.dup$LONG),]
        
        ACADB.Cl <-  DB.odile.Cl[row.names(DB.odile.Cl) %in% row.names(ACADB.Co),]
        ACADB.B <-  DB.odile.B[which(row.names(DB.odile.B) %in% row.names(ACADB.Co)),1, drop = F]
        ACADB.Po <-  DB.odile.Po[row.names(DB.odile.Po) %in% row.names(ACADB.Co),]
        ACADB.Po <- ACADB.Po[colSums(ACADB.Po)!=0]
        saveRDS(ACADB.Po, "Resultats/ACA/Pollen/Func_trans/Surface/ACA_MP_FT_clean.Rds")
        saveRDS(ACADB.Cl, "Resultats/ACA/Pollen/Func_trans/Surface/ACA_MP_FT_clim.Rds")
        saveRDS(ACADB.Co, "Resultats/ACA/Pollen/Func_trans/Surface/ACA_MP_FT_coord.Rds")
        
        MT <- cbind(ACADB.Co, ACADB.B)
        MT <- MT[MT$BIOMPOL_  %in%  c("COST", "TEDE", "CODE", "TAIG"),]
        
        Plot.map = F
        if(Plot.map == T){
          source("Scripts/Merge_DB.R")
          pmap.bioms <- map_empty +
            geom_polygon(data = ACA.bo.proj, aes(x=long, y=lat),colour="black", alpha = 1, fill = "grey75", size = 0.5)+
            geom_polygon(data = TUSD_map, aes(x=long, y=lat, group = group),colour="black", alpha = 1, fill = "grey50", size = 0.5)+
            geom_point(MT, mapping = aes(x = LONG, y = LAT), shape = 1, size = 3, alpha = .8)+
            facet_wrap(vars(BIOMPOL_))+
            geom_point(MT, mapping = aes(x = LONG, y = LAT, colour = BIOMPOL_), shape = 16, size = 2.5, alpha = .8)
          W = 1150*2
          H = 650*2
          ggsave(filename = "Figures/ACA/Pollen/Func_trans/Maps/MAP_DBACA_pol.pdf", pmap.bioms, width = W*0.026458333, height = H*0.026458333, units = "cm")
        }
        }
      
      #### Map des sites de surface ####
      map.biom = F
      if(map.biom == T){
        Biome.Pol.map(#Site.loc = DB.odile.Co.COST,
                      # Site.loc2 = DB.odile.Co.WAST,
                      # Site.loc3 = DB.odile.Co.ST,
                      H = 600, W = 1200,  Biome.select = "COST",
                      Save.plot = "Figures/World_DB/Pollen/Func_trans/Maps/Map_eurasian_pollen_DB.pdf"
        )
        
        
        }
      }
    #### Surface FT models par biomes ####
    Surface.calc = F
    if(Surface.calc == T){
      #### TUDB (Tajik + Uzbek) ####
      TUDB.FT = F
      if(TUDB.FT == T){
        #### Import data ####
        Corresp_name  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
        Uz.MP  <- data.frame(read.csv(file="Resultats/Uzbekistan/Pollen/Surface/MP_Uz_clean.csv",sep=",",dec=".", header=T, row.names=1))
        Uz.eco  <- data.frame(read.csv(file="Resultats/Uzbekistan/Pollen/Surface/TUSD_all_eco.csv",sep=",",dec=".", header=T, row.names=1))
        
        #### Clean data ####
        Uz.eco <- subset(Uz.eco, select = c(MAAT, Tspr, Tsum, MTWAQ, MTCOQ, TS, MAP, Pspr, Pwin, AI, Latitude, Longitude))
        Uz.MP <- data.frame(t(Uz.MP))
        
        Uz.MP <- Surf.MAT.prep(Uz.MP, Type_MAT = "Type_total", Corresp_name = Corresp_name)
        Uz.clim <- Surf.Clim.Pol(Uz.MP, Uz.eco)
        Uz.MP <- Surf.check.clim(Uz.MP, Uz.eco)
        
        Uz.coord <- subset(Uz.clim, select = c(Latitude, Longitude))
        names(Uz.coord) <- c("LAT", "LONG")
        Uz.clim <- subset(Uz.clim, select = -c(Latitude, Longitude))
        write.table(Uz.MP, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clean.csv", row.names=T, col.names=NA, sep=",", dec = ".")
        write.table(Uz.clim, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clim.csv", row.names=T, col.names=NA, sep=",", dec = ".")
        write.table(Uz.coord, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_coord.csv", row.names=T, col.names=NA, sep=",", dec = ".")
        saveRDS(Uz.MP, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clean.Rds")
        saveRDS(Uz.clim, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clim.Rds")
        saveRDS(Uz.coord, "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_coord.Rds")
       
        
        #### Calibration surface (WAPLS, MAT, RF et BRT) ####
        Calculate.FT = F
        if(Calculate.FT == T){
          MAT.TUDB.bool = T
          if(MAT.TUDB.bool == T){
            MAT.TUDB <- FT.quantif(Uz.MP, Uz.clim,
                                    Mcoord = Uz.coord,
                                    Model = "MAT",
                                    Save.RDS = T, 
                                    Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Surface/MAT_TUDB.csv")}
          
          WAPLS.TUDB.bool = T
          if(WAPLS.TUDB.bool == T){
            WAPLS.TUDB <- FT.quantif(Uz.MP, Uz.clim,
                                      Mcoord = Uz.coord,
                                      Model = "WAPLS",
                                      Save.RDS = T,
                                      Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Surface/WAPLS_TUDB.csv")}
          
          RF.TUDB.bool = T
          if(RF.TUDB.bool == T){
            RF.TUDB <- FT.quantif(Uz.MP, Uz.clim,
                                   Mcoord = Uz.coord,
                                   Model = "RF",
                                   Save.RDS = T,
                                   Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Surface/RF_TUDB.csv")}
          
          BRT.TUDB.bool = T
          if(BRT.TUDB.bool == T){
            BRT.TUDB <- FT.quantif(Uz.MP, Uz.clim,
                                    Mcoord = Uz.coord,
                                    Model = "BRT",
                                    Save.RDS = T,
                                    Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Surface/BRT_TUDB.csv")}
        }
        
        #### Plot LR pollen / Param. clim ####
        # Uz.MP  <- data.frame(read.csv(file="Resultats/Uzbekistan/Pollen/Surface/MP_Uz_clean.csv",sep=",",dec=".", header=T, row.names=1))
        LinRelClim.Uz <- Plot.relation.Surf.Clim(Uz.MP, Uz.clim, Nb.max = 9,
                                                 H = 900, W = 900,
                                                 # Name.taxa = Corresp_name,
                                                 Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Calib/LR_TUDB.pdf")
        
      }
      
      #### COSTDB ####
      COST.FT = T
      if(COST.FT == T){
        #### Cas sans Cicho #### 
        Sans.noC.ok = F
        if(Sans.noC.ok == T){
          DB.odile.Po.COST.noC <- subset(DB.odile.Po.COST, select = -c(COMPLIGU))
          New.sum <- rowSums(DB.odile.Po.COST.noC)
          DB.odile.Po.COST <- DB.odile.Po.COST.noC/New.sum
        }
        
        #### Test COSTDB ####
        Test.correl.COST = F
        if(Test.correl.COST == T){
          set.seed(1234)
          data1 <- sample(2, nrow(DB.odile.Po.COST), 
                          replace = T, 
                          prob = c(0.6, 0.4))
          DB.odile.Po.COST_test <- DB.odile.Po.COST[data1 == 2,]
          DB.odile.Po.COST <- DB.odile.Po.COST[data1 == 1,]
          DB.odile.Po.COST <- DB.odile.Po.COST[,colSums(DB.odile.Po.COST) != 0]
          
          DB.odile.Cl.COST_test <- DB.odile.Cl.COST[which(row.names(DB.odile.Cl.COST) %in% row.names(DB.odile.Po.COST_test)),]
          DB.odile.Co.COST_test <- DB.odile.Co.COST[which(row.names(DB.odile.Co.COST) %in% row.names(DB.odile.Po.COST_test)),]
          DB.odile.Cl.COST <- DB.odile.Cl.COST[which(row.names(DB.odile.Cl.COST) %in% row.names(DB.odile.Po.COST)),]
          DB.odile.Co.COST <- DB.odile.Co.COST[which(row.names(DB.odile.Co.COST) %in% row.names(DB.odile.Po.COST)),]
          write.csv(DB.odile.Cl.COST_test, file = "Resultats/Armenia/Pollen/Func_trans/Surface_correl/DB_Odile_COST_test_correl_clim.csv")
          write.csv(DB.odile.Co.COST_test, file = "Resultats/Armenia/Pollen/Func_trans/Surface_correl/DB_Odile_COST_test_correl_coord.csv")
          }
        #### Calcul COSTDB ####
        MAT.COSTDB.T = T
        if(MAT.COSTDB.T == T){
          MAT.COSTDB <- FT.quantif(DB.odile.Po.COST, DB.odile.Cl.COST,
                                              Mcoord = DB.odile.Co.COST,
                                              Model = "MAT",
                                              Save.RDS = T,
                                              Save.path = paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_COSTDB.csv", sep = "/"))}
      
        WAPLS.COSTDB.T = T
        if(WAPLS.COSTDB.T == T){
          WAPLS.COSTDB <- FT.quantif(DB.odile.Po.COST, DB.odile.Cl.COST,
                                           Mcoord = DB.odile.Co.COST,
                                           Nb.arg = 6,
                                           Model = "WAPLS",
                                           Save.RDS = T,
                                           Save.path = paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_COSTDB.csv", sep = "/"))}
      
        RF.COSTDB.T = T
        if(RF.COSTDB.T == T){
          RF.COSTDB <- FT.quantif(DB.odile.Po.COST, DB.odile.Cl.COST,
                                        Mcoord = DB.odile.Co.COST,
                                        Model = "RF",
                                        Save.RDS = T,
                                        Save.path = paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_COSTDB.csv", sep = "/"))}
        
        BRT.COSTDB.T = T
        if(BRT.COSTDB.T == T){
          BRT.COSTDB <- FT.quantif(DB.odile.Po.COST, DB.odile.Cl.COST,
                                       Mcoord = DB.odile.Co.COST,
                                       Model = "BRT",
                                       Save.RDS = T,
                                       Save.path = paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_COSTDB.csv", sep = "/"))}
        
        #### Apply test on surface COSTDB ####
        if(Test.correl.COST == T){
          DB.odile.Po.COST_test <- Fossil.corresp.surface(DB.odile.Po.COST_test, DB.odile.Po.COST) 
          plot.COST.correl.surf <- FT.core(
            Model.WAPLS = WAPLS.COSTDB,
            Model.MAT = MAT.COSTDB,
            Model.RF = RF.COSTDB,
            Model.BRT = BRT.COSTDB,
            MCore = DB.odile.Po.COST_test,
            MAge = DB.odile.Co.COST_test$LATI,
            #Fit.val = 0.25,
            Ecartype.curve = c(F,F,F,F),
            Only.fit = F,
            Model.param.show = T,
            Zone.Clim.span = c(0),
            #Zone.Temp = c("W","C","W","C","W"),
            Save.RDS = T,
            Save.path = "Resultats/Armenia/Pollen/Func_trans/Surface_correl/COST_surf_correl.csv",
            Save.plot = "Figures/Armenia/Pollen/Func_trans/Surface_validation/COST_surf_correl.pdf",
            H = 1300, W = 1700
          )
          }
        }
      #### ACADB (all ACA) ####
      ACADB.FT = F
      if(ACADB.FT == T){
        #### Calibration surface (WAPLS, MAT, RF et BRT) ####
        Calculate.FT = T
        if(Calculate.FT == T){
          MAT.ACADB.bool = T
          if(MAT.ACADB.bool == T){
            MAT.ACADB <- FT.quantif(ACADB.Po, ACADB.Cl,
                                    Mcoord = ACADB.Co,
                                    Model = "MAT",
                                    Save.RDS = T, #Version = Version.ACADB,
                                    Save.path = "Resultats/ACA/Pollen/Func_trans/Surface/MAT_ACADB.csv")}
          
          WAPLS.ACADB.bool = T
          if(WAPLS.ACADB.bool == T){
            WAPLS.ACADB <- FT.quantif(ACADB.Po, ACADB.Cl,
                                      Mcoord = ACADB.Co,
                                      Model = "WAPLS",
                                      Save.RDS = T, 
                                      Save.path = "Resultats/ACA/Pollen/Func_trans/Surface/WAPLS_ACADB.csv")}
          
          RF.ACADB.bool = T
          if(RF.ACADB.bool == T){
            RF.ACADB <- FT.quantif(ACADB.Po, ACADB.Cl,
                                   Mcoord = ACADB.Co,
                                   Model = "RF",
                                   Save.RDS = T,
                                   Save.path = "Resultats/ACA/Pollen/Func_trans/Surface/RF_ACADB.csv")}
          
          BRT.ACADB.bool = T
          if(BRT.ACADB.bool == T){
            BRT.ACADB <- FT.quantif(ACADB.Po, ACADB.Cl,
                                    Mcoord = ACADB.Co,
                                    Model = "BRT",
                                    Save.RDS = T,
                                    Save.path = "Resultats/ACA/Pollen/Func_trans/Surface/BRT_ACADB.csv")}
        }
        
        #### Plot LR pollen / Param. clim ####
        # ACADB.Po  <- data.frame(read.csv(file="Resultats/ACA/Pollen/Surface/MP_ACA_clean.csv",sep=",",dec=".", header=T, row.names=1))
        LinRelClim.ACA <- Plot.relation.Surf.Clim(ACADB.Po, ACADB.Cl, Nb.max = 9,
                                                 H = 900, W = 900,
                                                 # Name.taxa = Corresp_name,
                                                 Save.plot = "Figures/ACA/Pollen/Func_trans/Calib/LR_ACADB.pdf")
        
      }
      #### TAIGDB ####
      TAIGDB.FT = F
      if(TAIGDB.FT == T){
        MAT.TAIGDB = T
        if(MAT.TAIGDB == T){
          MAT.TAIGDB <- FT.quantif(DB.odile.Po.TAIGDB, DB.odile.Cl.TAIGDB,
                                              Mcoord = DB.odile.Co.TAIGDB,
                                              Model = "MAT",
                                              Save.RDS = T,
                                              Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MAT_TAIGDB.csv")}
      
        WAPLS.TAIGDB = T
        if(WAPLS.TAIGDB == T){
          WAPLS.TAIGDB <- FT.quantif(DB.odile.Po.TAIGDB, DB.odile.Cl.TAIGDB,
                                           Mcoord = DB.odile.Co.TAIGDB,
                                           Nb.arg = 6,
                                           Model = "WAPLS",
                                           Save.RDS = T,
                                           Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/WAPLS_TAIGDB.csv")}
      
        RF.TAIGDB = T
        if(RF.TAIGDB == T){
          RF.TAIGDB <- FT.quantif(DB.odile.Po.TAIGDB, DB.odile.Cl.TAIGDB,
                                        Mcoord = DB.odile.Co.TAIGDB,
                                        Model = "RF",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/RF_TAIGDB.csv")
          }
        
        BRT.TAIGDB = T
        if(BRT.TAIGDB == T){
          BRT.TAIGDB <- FT.quantif(DB.odile.Po.TAIGDB, DB.odile.Cl.TAIGDB,
                                       Mcoord = DB.odile.Co.TAIGDB,
                                       Model = "BRT",
                                       Save.RDS = T,
                                       Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/BRT_TAIGDB.csv")}}
      
      #### WASTDB ####
      WAST.FT = F
      if(WAST.FT == T){
        MAT.WASTDB = T
        if(MAT.WASTDB == T){
          MAT.WASTDB <- FT.quantif(DB.odile.Po.WAST, DB.odile.Cl.WAST,
                                        Mcoord = DB.odile.Co.WAST,
                                        Nb.arg = 6,
                                        Model = "MAT",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MAT_WASTDB.csv")}
          
        WAPLS.WASTDB = F
        if(WAPLS.WASTDB == T){
          WAPLS.WASTDB <- FT.quantif(DB.odile.Po.WAST, DB.odile.Cl.WAST,
                                            Nb.arg = 6,
                                            Mcoord = DB.odile.Co.WAST,
                                            Model = "WAPLS",
                                            Save.RDS = T,
                                            Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/WAPLS_WASTDB.csv")}
        
        RF.WASTDB = F
        if(RF.WASTDB == T){
          RF.WASTDB <- FT.quantif(DB.odile.Po.WAST, DB.odile.Cl.WAST,
                                         Mcoord = DB.odile.Co.WAST,
                                         Model = "RF",
                                         Save.RDS = T,
                                         Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/RF_WASTDB.csv")}
        
        BRT.WASTDB = F
        if(BRT.WASTDB == T){
          BRT.WASTDB <- FT.quantif(DB.odile.Po.WAST, DB.odile.Cl.WAST,
                                       Mcoord = DB.odile.Co.WAST,
                                       Model = "BRT",
                                       Save.RDS = T,
                                       Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/BRT_WASTDB.csv")}}
        
      #### EAPDB (Odile DB) ####
      EAPDB.FT = F
      if(EAPDB.FT == T){
        RF.EAPDB = T
        if(RF.EAPDB == T){
          RF.EAPDB <- FT.quantif(DB.odile.Po, DB.odile.Cl,
                                        Mcoord = DB.odile.Co,
                                        Model = "RF",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/RF_EAPDB.csv")}
      
        BRT.EAPDB = T
        if(BRT.EAPDB == T){
          BRT.EAPDB <- FT.quantif(DB.odile.Po, DB.odile.Cl,
                                        Mcoord = DB.odile.Co,
                                        Model = "BRT",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/BRT_EAPDB.csv")}
        
        WAPLS.EAPDB = T
        if(WAPLS.EAPDB == T){
          WAPLS.EAPDB <- FT.quantif(DB.odile.Po, DB.odile.Cl,
                                           Nb.arg = 10,
                                           Mcoord = DB.odile.Co,
                                           Model = "WAPLS",
                                           Save.RDS = T,
                                           Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/WAPLS_EAPDB.csv")}
        
        MAT.EAPDB = T
        if(MAT.EAPDB == T){
          MAT.EAPDB <- FT.quantif(DB.odile.Po, DB.odile.Cl,
                                         Mcoord = DB.odile.Co,
                                         Nb.arg = 6,
                                         Model = "MAT",
                                         Save.RDS = T,
                                         Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MAT_EAPDB.csv")}
        
        Modif.MAT.MAP = F
        if(Modif.MAT.MAP == T){ # remove 32 sites corrompus
          Corrupt.sites <- row.names(DB.odile.Cl[5])[which(DB.odile.Cl[5] < 0)]
          Corrupt.sites2 <- row.names(DB.odile.Cl[5])[which(DB.odile.Cl[5] < DB.odile.Cl[8])]
          Corrupt.sites <- unique(c(Corrupt.sites, Corrupt.sites2))
          DB.odile.Po <- DB.odile.Po[setdiff(row.names(DB.odile.Po), Corrupt.sites),]
          DB.odile.Cl <- DB.odile.Cl[setdiff(row.names(DB.odile.Cl), Corrupt.sites),]
          
          MAP.MAT.EAPDB = MAT(DB.odile.Po, DB.odile.Cl$MAP, k = 10, lean = F)
          MAP.MAT.EAPDB.CV = crossval(MAP.MAT.EAPDB, k = 10, cv.method="bootstrap", 
                                      verbose = T, ngroups = 5, nboot = 500, h.cutoff = 0, h.dist = NULL)
          }
        
        }
          
      #### STDB (COST + WAST) ####
      ST.FT = F
      if(ST.FT == T){
        MAT.STDB = T
        if(MAT.STDB == T){
          MAT.ST <- FT.quantif(DB.odile.Po.ST, DB.odile.Cl.ST,
                                        Mcoord = DB.odile.Co.ST,
                                        Nb.arg = 6,
                                        Model = "MAT",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MAT_STDB.csv")}
        
        WAPLS.STDB = T
        if(WAPLS.STDB == T){
          WAPLS.ST <- FT.quantif(DB.odile.Po.ST, DB.odile.Cl.ST,
                                          Mcoord = DB.odile.Co.ST,
                                          Nb.arg = 6,
                                          Model = "WAPLS",
                                          Save.RDS = T,
                                          Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/WAPLS_STDB.csv")}
    
        RF.STDB = F
        if(RF.STDB == T){
          RF.ST <- FT.quantif(DB.odile.Po.ST, DB.odile.Cl.ST,
                                         Mcoord = DB.odile.Co.ST,
                                         Model = "RF",
                                         Save.RDS = T,
                                         Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/RF_STDB.csv")}
        
        BRT.STDB = F
        if(BRT.STDB == T){
          BRT.ST <- FT.quantif(DB.odile.Po.ST, DB.odile.Cl.ST,
                                     Mcoord = DB.odile.Co.ST,
                                     Model = "BRT",
                                     Save.RDS = T,
                                     Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/BRT_STDB.csv")}}
          
      #### MEDTEMPDB ####
      MEDTEMPDB.FT = F
      if(MEDTEMPDB.FT == T){
        MAT.MEDTEMPDB = T
        if(MAT.MEDTEMPDB == T){
          MAT.MEDTEMP <- FT.quantif(MPol = DB.MEDTEMP.Po, 
                                      MClim = DB.MEDTEMP.Cl,
                                      Mcoord = DB.MEDTEMP.Co, 
                                      Model = "MAT", 
                                      Save.RDS = T,
                                      Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/MAT_MEDTEMP.csv")
        }
        
        WAPLS.MEDTEMPDB = T
        if(WAPLS.MEDTEMPDB == T){
          WAPLS.MEDTEMP <-  FT.quantif(MPol = DB.MEDTEMP.Po, 
                                       MClim = DB.MEDTEMP.Cl,
                                       Mcoord = DB.MEDTEMP.Co,
                                       Nb.arg = 6,
                                       Model = "WAPLS",
                                       Save.RDS = T,
                                       Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/WAPLS_MEDTEMP.csv")
        }
        
        RF.MEDTEMPDB = T
        if(RF.MEDTEMPDB == T){
          RF.MEDTEMP <- FT.quantif(MPol = DB.MEDTEMP.Po, 
                                   MClim = DB.MEDTEMP.Cl,
                                   Mcoord = DB.MEDTEMP.Co,
                                   Model = "RF",
                                   Save.RDS = T,
                                   Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/RF_MEDTEMP.csv")
        }
        
        BRT.MEDTEMPDB = T
        if(BRT.MEDTEMPDB == T){
          BRT.MEDTEMP <- FT.quantif(MPol = DB.MEDTEMP.Po, 
                                    MClim = DB.MEDTEMP.Cl,
                                    Mcoord = DB.MEDTEMP.Co,
                                    Model = "BRT",
                                    Save.RDS = T,
                                    Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/BRT_MEDTEMP.csv")
        }
      }
      
      #### TEMPSCANDDB ####
      TEMPSCAND.FT = F
      if(TEMPSCAND.FT == T){
        MAT.TEMPSCANDDB = T
        if(MAT.TEMPSCANDDB == T){
          MAT.TEMPSCAND <- FT.quantif(MPol = DB.TEMPSCAND.Po, 
                                      MClim = DB.TEMPSCAND.Cl,
                                      Mcoord = DB.TEMPSCAND.Co,
                                      Model = "MAT",
                                      Save.RDS = T,
                                      Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/MAT_TEMPSCAND.csv")
          
        }
        
        WAPLS.TEMPSCAND = T
        if(WAPLS.TEMPSCAND == T){
          WAPLS.TEMPSCAND <- FT.quantif(MPol = DB.TEMPSCAND.Po, 
                                        MClim = DB.TEMPSCAND.Cl,
                                        Mcoord = DB.TEMPSCAND.Co,
                                        Nb.arg = 6,
                                        Model = "WAPLS",
                                        Save.RDS = T,
                                        Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/WAPLS_TEMPSCAND.csv")
        }
        
        RF.TEMPSCAND = T
        if(RF.TEMPSCAND == T){
          RF.TEMPSCAND <- FT.quantif(MPol = DB.TEMPSCAND.Po, 
                                     MClim = DB.TEMPSCAND.Cl,
                                     Mcoord = DB.TEMPSCAND.Co,
                                     Model = "RF",
                                     Save.RDS = T,
                                     Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/RF_TEMPSCAND.csv")
        }
        
        BRT.TEMPSCAND = T
        if(BRT.TEMPSCAND == T){
          BRT.TEMPSCAND <- FT.quantif(MPol = DB.TEMPSCAND.Po, 
                                      MClim = DB.TEMPSCAND.Cl,
                                      Mcoord = DB.TEMPSCAND.Co,
                                      Model = "BRT",
                                      Save.RDS = T,
                                      Save.path = "Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/BRT_TEMPSCAND.csv")
        }
      }
      }}}

Import.RDS.world.FT = T
if(Import.RDS.world.FT == T){
  if(exists("MAT.EAPDB") == F){
    #### Import EAPDB ####
    DB.odile.Po <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3266/ss3266po.csv", sep=",", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    MAT.EAPDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_EAPDB.Rds", sep = "/"))
    WAPLS.EAPDB <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_EAPDB.Rds", sep = "/"))
    RF.EAPDB    <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_EAPDB.Rds", sep = "/"))
    BRT.EAPDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_EAPDB.Rds", sep = "/"))
    
    #### Import TAIGDB ####
    DB.odile.Po.TAIGDB <- data.frame(read.csv(file="Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_TAIGDB_Po.csv", sep=",", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    MAT.TAIGDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_TAIGDB.Rds", sep = "/"))
    WAPLS.TAIGDB <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_TAIGDB.Rds", sep = "/"))
    RF.TAIGDB    <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_TAIGDB.Rds", sep = "/"))
    BRT.TAIGDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_TAIGDB.Rds", sep = "/"))
    
    #### Import COSTDB ####
    DB.odile.Po.COST <- data.frame(read.csv(file="Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_COST_Po.csv", sep=",", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    MAT.COSTDB       <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_COSTDB.Rds", sep = "/"))
    WAPLS.COSTDB     <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_COSTDB.Rds", sep = "/"))
    RF.COSTDB        <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_COSTDB.Rds", sep = "/"))
    BRT.COSTDB       <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_COSTDB.Rds", sep = "/"))
    
    #### Import WASTDB ####
    DB.odile.Po.WAST <- data.frame(read.csv(file="Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_WAST_Po.csv", sep=",", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    MAT.WASTDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_WASTDB.Rds", sep = "/"))
    WAPLS.WASTDB <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_WASTDB.Rds", sep = "/"))
    RF.WASTDB    <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_WASTDB.Rds", sep = "/"))
    BRT.WASTDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_WASTDB.Rds", sep = "/"))
     
    #### Import STDB ####
    DB.odile.Po.STDB <- data.frame(read.csv(file="Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_ST_Po.csv", sep=",", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
    MAT.STDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "MAT_STDB.Rds", sep = "/"))
    WAPLS.STDB <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "WAPLS_STDB.Rds", sep = "/"))
    RF.STDB    <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "RF_STDB.Rds", sep = "/"))
    BRT.STDB   <- readRDS(paste("Resultats/World_DB/Pollen/Func_trans/Surface", Version.DB, "BRT_STDB.Rds", sep = "/"))
    
    #### Imports TUDB ####
    MAT.TUDB   <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/MAT_TUDB.Rds")
    WAPLS.TUDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/WAPLS_TUDB.Rds")
    RF.TUDB    <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/RF_TUDB.Rds")
    BRT.TUDB   <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/BRT_TUDB.Rds")
    
    #### Imports MEDTEMP ####
    MAT.MEDTEMP   <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/MAT_MEDTEMP.Rds")
    WAPLS.MEDTEMP <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/WAPLS_MEDTEMP.Rds")
    BRT.MEDTEMP   <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/BRT_MEDTEMP.Rds")
    RF.MEDTEMP    <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/MEDTEMP/RF_MEDTEMP.Rds")
    
    #### Imports TEMPSCAND ####
    MAT.TEMPSCAND   <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/MAT_TEMPSCAND.Rds")
    WAPLS.TEMPSCAND <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/WAPLS_TEMPSCAND.Rds")
    BRT.TEMPSCAND   <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/BRT_TEMPSCAND.Rds")
    RF.TEMPSCAND    <- readRDS("Resultats/World_DB/Pollen/Func_trans/Surface/TEMPSCAND/RF_TEMPSCAND.Rds")
    }}

#### Mongolia ####
Mongolia = F
if(Mongolia == T){
  #### Import Data ####
  # Surface Mongolia #
  Corresp_name  <- data.frame(read.csv(file="Import/Mongolia/Pollen/Func_trans/Corresp_pollen_MAT_Mongolia.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
  MPS_mongPol   <- data.frame(t(read.csv(file="Resultats/Mongolia/Pollen/Surface/Mongolia_surfP1_MPfrac.csv",sep=",",dec=".", header = T, stringsAsFactors = T, row.names = 1)))
  MPS_mongClim  <- data.frame(read.csv(file="Import/Mongolia/Site/Surface_samples_climat.csv",sep=",",dec=".",header=T,row.names=1))
  MPS_mongCoord <- subset(MPS_mongClim, select = c(Latitude, Longitude))
  names(MPS_mongCoord) <- c("LAT", "LONG")
  
  # Cores #
  Ayrag.import = T
  if(Ayrag.import == T){
    source("Scripts/Age.R")
    MP.Ayrag     <- data.frame(t(read.csv(file="Resultats/Mongolia/Pollen/Ayrag/MP_Ayrag_MPfrac.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE)))                   # GDGT mesure Ayrag
    MAge.raw     <- data.frame(read.csv(file="Import/Mongolia/Pollen/Ayrag/Ayrag_prof_date.csv",sep="\t",dec=".",header=T, stringsAsFactors = F)) # Correspondance des profondeurs/echantillons GAY
    #GDGT.Ayrag  <- data.frame(read.csv(file="Resultats/GDGT/GDGT_Ayrag_ClimMod.csv",sep=",",dec=".",header=T,row.names=1))        # GDGT MAAT Ayrag
    MP.Ayrag.Age <- Age.convert(MAge.raw, Proxy.select = "Pollen", Depth.select = c("Top", "Bottom", "Age"))
    #GDGTprof    <- Age.convert(MAge.raw, Proxy.select = "GDGT", Depth.select = c("Top", "Bottom", "Age"))
    #Age.GDGT    <- Age.convert(MAge.raw, Proxy.select = "GDGT", Depth.select = c("Age"))
    }

  Altai.cores.imp = T
  if(Altai.cores.imp == T){
    source("Scripts/Age.R")
    Corresp_name.Gottingen <- data.frame(read.csv(file="Import/Mongolia/Pollen/Func_trans/Corresp_pollen_MAT_Mongolia_Gottingen.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
    MP.D1L1       <- data.frame(t(read.csv(file="Resultats/Mongolia/Pollen/D1L1/MP_D1L1_MPfrac.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE)))                   # GDGT mesure Ayrag
    MP.D3L6       <- data.frame(t(read.csv(file="Resultats/Mongolia/Pollen/D3L6/MP_D3L6_MPfrac.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE)))                   # GDGT mesure Ayrag
    MAge.D1L1.raw <- data.frame(read.csv(file="Import/Mongolia/Pollen/Cores/Altai_D1L1_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F)) # Correspondance des profondeurs/echantillons GAY
    MAge.D3L6.raw <- data.frame(read.csv(file="Import/Mongolia/Pollen/Cores/Altai_D3L6_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F)) # Correspondance des profondeurs/echantillons GAY
    MAge.D1L1.raw <- Age.convert(MAge.D1L1.raw, Proxy.select = "Pollen", Depth.select = c("Top", "Bottom", "Age"))
    MAge.D3L6.raw <- Age.convert(MAge.D3L6.raw, Proxy.select = "Pollen", Depth.select = c("Top", "Bottom", "Age"))
  }
  
  #### Neotoma import ####
  neotoma.cores.imp = T
  if(neotoma.cores.imp == T){
    Corresp_name_neotoma <- data.frame(read.csv(file="Import//World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
    MP.Hubsugul_nuur <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Hubsugul_nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Hubsugul_nuur <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Hubsugul_nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MP.Hoton_Nuur    <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Hoton_Nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Hoton_Nuur    <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Hoton_Nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MP.Dood_Nuur     <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Dood_Nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Dood_Nuur     <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Dood_Nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MP.Daba_Nuur     <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Daba_Nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Daba_Nuur     <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Daba_Nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MP.Gun_Nuur      <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Gun_Nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Gun_Nuur      <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Gun_Nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MP.Achit_Nuur    <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Achit_Nuur_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    MA.Achit_Nuur    <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Cores/Achit_Nuur_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
    }
  
  #### Time crop ####
  Time.crop = F
  if(Time.crop == T){
    Seuil <- min(max(MP.Ayrag.Age$Age), max(MAge.D1L1.raw$Age), max(MAge.D3L6.raw$Age))
    MP.Ayrag.Age <- MP.Ayrag.Age[1:length(MP.Ayrag.Age$Age[MP.Ayrag.Age$Age <= Seuil]),]
    MAge.D1L1.raw <- MAge.D1L1.raw[1:length(MAge.D1L1.raw$Age[MAge.D1L1.raw$Age <= Seuil]),]
    MAge.D3L6.raw <- MAge.D3L6.raw[1:length(MAge.D3L6.raw$Age[MAge.D3L6.raw$Age <= Seuil]),]
    MP.D3L6 <- MP.D3L6[1:nrow(MAge.D3L6.raw),]
    MP.D1L1 <- MP.D1L1[1:nrow(MAge.D1L1.raw),]
    MP.Ayrag <- MP.Ayrag[1:nrow(MP.Ayrag.Age),]
    
  }
  
  #### NMSDB  : Calib Mongolia AsiaBiome 2016 ####
  NMSDB = T
  if(NMSDB == T){
      #### Select climat param ####
      MPS_mongClim <- subset(MPS_mongClim, select = c(MAAT, Tspr, MTWAQ, MAP, Pspr, Psum))
      #### Surface Calibration ####
      MPS_mongPol <- Surf.MAT.prep(MPS_mongPol, Type_MAT = "Type_MAT_Mong", Corresp_name = Corresp_name)
      DB_Mong_Clim <- Surf.Clim.Pol(MPS_mongPol, MPS_mongClim)
      MPS_mongPol <- Surf.check.clim(MPS_mongPol, MPS_mongClim)
      #MPS_mongCoord <- Surf.check.clim(MPS_mongPol, MPS_mongCoord)
      
      
      #### Calcul MAT WAPLS ####
      Calculate.FT = F
      if(Calculate.FT == T){
        MAT.NMSDB.bool = F
        if(MAT.NMSDB.bool == T){
          MAT.NMSDB <- FT.quantif(MPS_mongPol, DB_Mong_Clim,
                                         Mcoord = MPS_mongCoord,
                                         Nb.arg = 15,
                                         Model = "MAT",
                                         Save.RDS = T,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_NMSDB.csv")}
        
        WAPLS.NMSDB.bool = F
        if(WAPLS.NMSDB.bool == T){
          WAPLS.NMSDB <- FT.quantif(MPS_mongPol, DB_Mong_Clim,
                                           Mcoord = MPS_mongCoord,
                                           Nb.arg = 15,
                                           Model = "WAPLS",
                                           Save.RDS = T,
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_NMSDB.csv")}
    
        RF.NMSDB.bool = F
        if(RF.NMSDB.bool == T){
          RF.NMSDB <- FT.quantif(MPS_mongPol, DB_Mong_Clim,
                                           Mcoord = MPS_mongCoord,
                                           Model = "RF",
                                           Save.RDS = T,
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/RF_NMSDB.csv")}
        
        BRT.NMSDB.bool = T
        if(BRT.NMSDB.bool == T){
          BRT.NMSDB <- FT.quantif(MPS_mongPol, DB_Mong_Clim,
                                        Mcoord = MPS_mongCoord,
                                        Model = "BRT",
                                        Save.RDS = T,
                                        Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_NMSDB.csv")}
        }
      else{
        MAT.NMSDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_NMSDB.Rds")
        WAPLS.NMSDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_NMSDB.Rds")
        RF.NMSDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/RF_NMSDB.Rds")
        BRT.NMSDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_NMSDB.Rds")
        }
      
      #### Check congruence ####
      MP_Ayrag.conv <- Fossil.MAT.prep(MP.Ayrag, MP.Ayrag.Age, Type_MAT = "Type_MAT_Mong",  Corresp_name = Corresp_name, Displot = F)
      MP_Ayrag.conv <- Fossil.corresp.surface(MP_Ayrag.conv, MPS_mongPol)
      MP.D1L1.conv <- Fossil.MAT.prep(MP.D1L1, MAge.D1L1.raw, Type_MAT = "Type_MAT_Mong", Displot = T,  Corresp_name = Corresp_name.Gottingen)
      MP.D1L1.conv <- Fossil.corresp.surface(MP.D1L1.conv, MPS_mongPol)
      MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Mong", Displot = T,  Corresp_name = Corresp_name.Gottingen)
      MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, MPS_mongPol)
      
      MP.Hubsugul_nuur.conv <- Fossil.MAT.prep(MP.Hubsugul_nuur, MA.Hubsugul_nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      MP.Hoton_Nuur.conv    <- Fossil.MAT.prep(MP.Hoton_Nuur, MA.Hoton_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      MP.Dood_Nuur.conv     <- Fossil.MAT.prep(MP.Dood_Nuur, MA.Dood_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      MP.Daba_Nuur.conv     <- Fossil.MAT.prep(MP.Daba_Nuur, MA.Daba_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      MP.Gun_Nuur.conv      <- Fossil.MAT.prep(MP.Gun_Nuur, MA.Gun_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      MP.Achit_Nuur.conv    <- Fossil.MAT.prep(MP.Achit_Nuur, MA.Achit_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
      
      MP.Hubsugul_nuur.conv <- Fossil.corresp.surface(MP.Hubsugul_nuur.conv, MPS_mongPol)
      MP.Hoton_Nuur.conv <- Fossil.corresp.surface(MP.Hoton_Nuur.conv, MPS_mongPol)
      MP.Dood_Nuur.conv <- Fossil.corresp.surface(MP.Dood_Nuur.conv, MPS_mongPol)
      MP.Daba_Nuur.conv <- Fossil.corresp.surface(MP.Daba_Nuur.conv, MPS_mongPol)
      MP.Gun_Nuur.conv <- Fossil.corresp.surface(MP.Gun_Nuur.conv, MPS_mongPol)
      MP.Achit_Nuur.conv <- Fossil.corresp.surface(MP.Achit_Nuur.conv, MPS_mongPol)
      
      #### Plot Modelling past - AYRAG ####
      plot.Ayrag.ft = F
      if(plot.Ayrag.ft == T){
        Ayrag.NMSDB <- FT.core(
                        Model.WAPLS = WAPLS.NMSDB,
                        Model.MAT = MAT.NMSDB,
                        Model.RF = RF.NMSDB,
                        Model.BRT = BRT.NMSDB,
                        MCore = MP_Ayrag.conv,
                        MAge = MP.Ayrag.Age$Age,
                        Fit.val = 0.25,
                        LakeName = "Ayrag",
                        Only.fit = T,
                        Ecartype.curve = c(T, T, F, F),
                        Model.param.show = T,
                        Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                        #Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                        Zone.Temp = c("C","W","C","W"),
                        #Zone.Temp = c("C","W"),
                        Save.RDS = T,
                        Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag.csv",
                        Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_NMSDB.pdf",
                        H = 1100, W = 1900
        )}
      
      test.randomTF = T
      if(test.randomTF == T){
        rlghr <- randomTF(spp = sqrt(MPS_mongPol), 
                          env = DB_Mong_Clim[c(4,1)],
                          fos = sqrt(MP_Ayrag.conv), 
                          n = 99, fun = MAT, col = 1)
        
        rlghr$sig
        plot(rlghr)
        }
  
      Analog.map.NMSDB = F
      if(Analog.map.NMSDB == T){
        Anal.map.Ayrag.NMSDB <- Analogue.map(Model.WAPLS = MAT.NMSDB,
                                             MAge = MP.Ayrag.Age,
                                             MCore = MP_Ayrag.conv,
                                             MCoord = MPS_mongCoord,
                                             Age.choice = c(-57, 182, 806, 1645, 2901, 3172),
                                             Zone.plot = c("Mongolia"),
                                             Clim.choix = "MAAT",
                                             H = 600, W = 900,
                                             Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Analogues/MAT_Ayrag_NMSDB.csv",
                                             Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Anal_maps/AnalMap_Ayrag_NMSDB.pdf")
                                             }
      
      LR.1 = F
      if(LR.1 == T){
         LinRelClim.Mongolie <- Plot.relation.Surf.Clim(as, DB_Mong_Clim, Nb.max = 9,
                                                        H = 900, W = 900,
                                                        Name.taxa = Corresp_name,
                                                        Save.plot = "Figures/Mongolia/Pollen/Func_trans/Calib/LR_NMSDB.pdf"
         )}
      
      #### Plot Modelling past - D1L1 ####
      plot.D1L1.ft = F
      if(plot.D1L1.ft == T){
        D1L1.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.D1L1.conv,
          MAge = MAge.D1L1.raw$Age,
          Fit.val = 0.25,
          LakeName = "D1L1",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/D1L1/D1L1_NMSDB.pdf",
          H = 1100, W = 1900
        )}
      
      #### Plot Modelling past - D3L6 ####
      plot.D3L6.ft = F
      if(plot.D3L6.ft == T){
        D3L6.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.D3L6.conv,
          MAge = MAge.D3L6.raw$Age,
          Fit.val = 0.25,
          LakeName = "D3L6",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/D3L6_NMSDB.pdf",
          H = 1100, W = 1900
        )}
      
      plot.D3L6.ft.ncomp.WAPLS = F
      if(plot.D3L6.ft.ncomp.WAPLS == T){
        D3L6.NMSDB.Ncomp <- MAT.WAPLS.Ncomp.influ(Model.WAPLS = MAT.COSTDB,
                                                  MCore = MP.D3L6.conv,
                                                  MAge = MAge.D3L6.raw$Age,
                                                  Param.clim = "MAP",
                                                  NComp = 6)}
      
      plot.D3L6.ft.ncomp.MAP = F
      if(plot.D3L6.ft.ncomp.MAP == T){
        D3L6.NMSDB.Ncomp <- MAT.WAPLS.Ncomp.influ(Model.WAPLS = MAT.NMSDB,
                                                  MCore = MP.D3L6.conv,
                                                  MAge = MAge.D3L6.raw$Age,
                                                  Param.clim = "MAAT",
                                                  NComp = 10)
      }
      
      #### Plot Mongolian neotoma cores ####
      plot.mong.ft = F
      if(plot.mong.ft == T){
        #### Hubsugul_nuur ####
        Hubsugul_nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Hubsugul_nuur.conv,
          MAge = MA.Hubsugul_nuur$Age,
          Fit.val = 0.25,
          LakeName = "Hubsugul_nuurv",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Hoton_Nuur ####
        Hoton_Nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Hoton_Nuur.conv,
          MAge = MA.Hoton_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Hoton_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Dood_Nuur ####
        Dood_Nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Dood_Nuur.conv,
          MAge = MA.Dood_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Dood_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur_NMSDB.pdf",
          H = 1100, W = 1900)
        
        
        #### Daba_Nuur ####
        Daba_Nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Daba_Nuur.conv,
          MAge = MA.Daba_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Daba_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Achit_Nuur ####
        Achit_Nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Achit_Nuur.conv,
          MAge = MA.Achit_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Achit_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur_NMSDB.pdf",
          H = 1100, W = 1900
        )
        #### Gun_Nuur ####
        Gun_Nuur.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Gun_Nuur.conv,
          MAge = MA.Gun_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Gun_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
      }
      
      #### Plot Russian ERAPDB cores ####
      ACA.ERAPDB = F
      if(ACA.ERAPDB == T){
        Corresp_name_ERAPDB  <- data.frame(read.csv(file="Import/World_DB/Pollen/ERAPDB/Indexes/Corresp_pollen_FT_ERAPDB.csv",sep=",",dec=".",header=T, row.names = 1)) 
        Corresp_name_ERAPDB <- Corresp_name_ERAPDB[Corresp_name_ERAPDB$Type == "P",]
        files.ERAPDB <- list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = T)
        ERAPDB.pollen <- lapply(files.ERAPDB, read.csv, header=TRUE, stringsAsFactors=FALSE, row.names = 1)
        names(ERAPDB.pollen) <-gsub(".csv", "", list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = F))
        ERAPDB.pollen <- ERAPDB.pollen[-9] # Remove Kotopel
        
        Total.conv <- function(x){
          y <- x[row.names(x) %in% c("Age","Depth"),]
          x[row.names(x) %in% c("Age","Depth"),] <- NA
          x <- na.omit(x)
          x <- data.frame(t(t(x)/rowSums(t(x))))
          print(names(x)[1])
          x <- Fossil.MAT.prep(data.frame(t(x)), data.frame(t(y)), Type_MAT = "Corresp_FT_Mong", Displot = T, Corresp_name = Corresp_name_ERAPDB)
          x <- Fossil.corresp.surface(x, MPS_mongPol)
          x <- list(x, y)
          return(x)
        }
        ERAPDB.pollen.conv <- purrr::map(ERAPDB.pollen, Total.conv)
        
        Total.FT <- function(x){
          Age <- data.frame(t(x[[2]]))
          x <- x[[1]]
          Lake.name <- gsub(".Ech_1", "", row.names(x)[1])
          print(Lake.name)
          
          x <- FT.core(
            Model.WAPLS = WAPLS.NMSDB,
            Model.MAT = MAT.NMSDB,
            Model.RF = RF.NMSDB,
            Model.BRT = BRT.NMSDB,
            MCore = x,
            MAge = Age$Age,
            Fit.val = 0.25,
            LakeName = Lake.name,
            Only.fit = T,
            Ecartype.curve = c(T, T, T, T),
            Model.param.show = T,
            Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
            Zone.Temp = c("C","W","C","W","C", "W"),
            Save.RDS = T,
            Save.path = paste("Resultats/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
            Save.plot = paste("Figures/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_NMSDB.pdf", sep = ""),
            H = 1100, W = 1900
          )
          return(x)
        }
        ERAPDB.FT <- purrr::map(ERAPDB.pollen.conv[9:18], Total.FT)
        # /!\ problème sur le 9 (Khendyrkul)
        
        }
      #### Plot Russian neotoma cores ####
      plot.russ.ft = F
      if(plot.russ.ft == T){
        #### Preparation ####
        MP.Baikal.conv <- Fossil.MAT.prep(MP.Baikal, MA.Baikal, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Baikal.conv <- Fossil.corresp.surface(MP.Baikal.conv, MPS_mongPol)
        
        MP.Derput.conv <- Fossil.MAT.prep(MP.Derput, MA.Derput, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Derput.conv <- Fossil.corresp.surface(MP.Derput.conv, MPS_mongPol)
        
        MP.Kotokel.conv <- Fossil.MAT.prep(MP.Kotokel, MA.Kotokel, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Kotokel.conv <- Fossil.corresp.surface(MP.Kotokel.conv, MPS_mongPol)
        
        MP.Nuochaga.conv <- Fossil.MAT.prep(MP.Nuochaga, MA.Nuochaga, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Nuochaga.conv <- Fossil.corresp.surface(MP.Nuochaga.conv, MPS_mongPol)
        
        MP.Suollakh.conv <- Fossil.MAT.prep(MP.Suollakh, MA.Suollakh, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Suollakh.conv <- Fossil.corresp.surface(MP.Suollakh.conv, MPS_mongPol)
        
        #### Baikal ####
        Baikal.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Baikal.conv,
          MAge = MA.Baikal$Age,
          Fit.val = 0.25,
          LakeName = "Baikal",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Russia/Pollen/Func_trans/Baikal/Baikal.csv",
          Save.plot = "Figures/Russia/Pollen/Func_trans/Baikal/Baikal_NMSDB.pdf",
          H = 1100, W = 1900
          )
        #/!\ ne marche pas !
        #### Derput #### 
        Derput.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Derput.conv,
          MAge = MA.Derput$Age,
          Fit.val = 0.25,
          LakeName = "Derput",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Russia/Pollen/Func_trans/Derput/Derput.csv",
          Save.plot = "Figures/Russia/Pollen/Func_trans/Derput/Derput_NMSDB.pdf",
          H = 1100, W = 1900
          )  
        
        
        #### Kotokel ####
        Kotokel.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Kotokel.conv,
          MAge = MA.Kotokel$Age,
          Fit.val = 0.25,
          LakeName = "Kotokel",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel.csv",
          Save.plot = "Figures/Russia/Pollen/Func_trans/Kotokel/Kotokel_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Nuochaga ####
        Nuochaga.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Nuochaga.conv,
          MAge = MA.Nuochaga$Age,
          Fit.val = 0.25,
          LakeName = "Nuochaga",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga.csv",
          Save.plot = "Figures/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_NMSDB.pdf",
          H = 1100, W = 1900
        )
        #### Suollakh ####
        Suollakh.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Suollakh.conv,
          MAge = MA.Suollakh$Age,
          Fit.val = 0.25,
          LakeName = "Suollakh",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh.csv",
          Save.plot = "Figures/Russia/Pollen/Func_trans/Suollakh/Suollakh_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
      }
      #### Plot Iran neotoma cores ####
      plot.iran.ft = F
      if(plot.iran.ft == T){
        #### Preparation ####
        MP.Parishan.conv <- Fossil.MAT.prep(MP.Parishan, MA.Parishan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Parishan.conv <- Fossil.corresp.surface(MP.Parishan.conv, MPS_mongPol)
        
        MP.Maharlou.conv <- Fossil.MAT.prep(MP.Maharlou, MA.Maharlou, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Maharlou.conv <- Fossil.corresp.surface(MP.Maharlou.conv, MPS_mongPol)
        
        MP.Gomishan.conv <- Fossil.MAT.prep(MP.Gomishan, MA.Gomishan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Gomishan.conv <- Fossil.corresp.surface(MP.Gomishan.conv, MPS_mongPol)
        
        #### Parishan ####
        Parishan.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Parishan.conv,
          MAge = MA.Parishan$Age,
          Fit.val = 0.25,
          LakeName = "Parishanv",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Iran/Pollen/Func_trans/Parishan/Parishan.csv",
          Save.plot = "Figures/Iran/Pollen/Func_trans/Parishan/Parishan_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Maharlou ####
        Maharlou.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Maharlou.conv,
          MAge = MA.Maharlou$Age,
          Fit.val = 0.25,
          LakeName = "Maharlou",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou.csv",
          Save.plot = "Figures/Iran/Pollen/Func_trans/Maharlou/Maharlou_NMSDB.pdf",
          H = 1100, W = 1900
          )  
        
        #### Gomishan ####
        Gomishan.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Gomishan.conv,
          MAge = MA.Gomishan$Age,
          Fit.val = 0.25,
          LakeName = "Gomishan",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan.csv",
          Save.plot = "Figures/Iran/Pollen/Func_trans/Gomishan/Gomishan_NMSDB.pdf",
          H = 1100, W = 1900
          )
      }
      
      #### Plot Chinese neotoma cores ####
      plot.china.ft = F
      if(plot.china.ft == T){
        #### Preparation ####
        MP.Zoige.conv <- Fossil.MAT.prep(MP.Zoige, MA.Zoige, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Zoige.conv <- Fossil.corresp.surface(MP.Zoige.conv, MPS_mongPol)
        
        MP.Wenquan.conv <- Fossil.MAT.prep(MP.Wenquan, MA.Wenquan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Wenquan.conv <- Fossil.corresp.surface(MP.Wenquan.conv, MPS_mongPol)
        
        MP.Tianchi_Liupan.conv <- Fossil.MAT.prep(MP.Tianchi_Liupan, MA.Tianchi_Liupan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Tianchi_Liupan.conv <- Fossil.corresp.surface(MP.Tianchi_Liupan.conv, MPS_mongPol)
        
        MP.Tianchi_Gaoligong.conv <- Fossil.MAT.prep(MP.Tianchi_Gaoligong, MA.Tianchi_Gaoligong, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Tianchi_Gaoligong.conv <- Fossil.corresp.surface(MP.Tianchi_Gaoligong.conv, MPS_mongPol)
        
        MP.Kanas.conv <- Fossil.MAT.prep(MP.Kanas, MA.Kanas, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Kanas.conv <- Fossil.corresp.surface(MP.Kanas.conv, MPS_mongPol)
        
        MP.Kakitu.conv <- Fossil.MAT.prep(MP.Kakitu, MA.Kakitu, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Kakitu.conv <- Fossil.corresp.surface(MP.Kakitu.conv, MPS_mongPol)
        
        MP.Hurleg.conv <- Fossil.MAT.prep(MP.Hurleg, MA.Hurleg, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Hurleg.conv <- Fossil.corresp.surface(MP.Hurleg.conv, MPS_mongPol)
        
        MP.Bayanchagan.conv <- Fossil.MAT.prep(MP.Bayanchagan, MA.Bayanchagan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Bayanchagan.conv <- Fossil.corresp.surface(MP.Bayanchagan.conv, MPS_mongPol)
        
        #### Zoige ####
        Zoige.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Zoige.conv,
          MAge = MA.Zoige$Age,
          Fit.val = 0.25,
          LakeName = "Zoige",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Zoige/Zoige.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Zoige/Zoige_NMSDB.pdf",
          H = 1100, W = 1900
          )
      
        #### Wenquan ####
        Wenquan.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Wenquan.conv,
          MAge = MA.Wenquan$Age,
          Fit.val = 0.25,
          LakeName = "Wenquan",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Wenquan/Wenquan.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Wenquan/Wenquan_NMSDB.pdf",
          H = 1100, W = 1900
          )
  
        #### Tianchi_Liupan ####
        Tianchi_Liupan.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Tianchi_Liupan.conv,
          MAge = MA.Tianchi_Liupan$Age,
          Fit.val = 0.25,
          LakeName = "Tianchi_Liupan",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan_NMSDB.pdf",
          H = 1100, W = 1900)
        

        #### Tianchi_Gaoligong ####
        Tianchi_Gaoligong.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Tianchi_Gaoligong.conv,
          MAge = MA.Tianchi_Gaoligong$Age,
          Fit.val = 0.25,
          LakeName = "Tianchi_Gaoligong",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Kanas ####
        Kanas.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Kanas.conv,
          MAge = MA.Kanas$Age,
          Fit.val = 0.25,
          LakeName = "Kanas",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Kanas/Kanas.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Kanas/Kanas_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Kakitu ####
        Kakitu.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Kakitu.conv,
          MAge = MA.Kakitu$Age,
          Fit.val = 0.25,
          LakeName = "Kakitu",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Kakitu/Kakitu.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Kakitu/Kakitu_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Hurleg ####
        Hurleg.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Hurleg.conv,
          MAge = MA.Hurleg$Age,
          Fit.val = 0.25,
          LakeName = "Hurleg",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Hurleg/Hurleg.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Hurleg/Hurleg_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Bayanchagan ####
        Bayanchagan.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Bayanchagan.conv,
          MAge = MA.Bayanchagan$Age,
          Fit.val = 0.25,
          LakeName = "Bayanchagan",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/China/Pollen/Func_trans/Bayanchagan/Bayanchagan.csv",
          Save.plot = "Figures/China/Pollen/Func_trans/Bayanchagan/Bayanchagan_NMSDB.pdf",
          H = 1100, W = 1900
        )
        
        }
      #### Plot Kirghizistan neotoma cores ####
      plot.Kirghizistan.ft = F
      if(plot.Kirghizistan.ft == T){
        #### Preparation ####
        MP.Kichikol.conv <- Fossil.MAT.prep(MP.Kichikol, MA.Kichikol, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Kichikol.conv <- Fossil.corresp.surface(MP.Kichikol.conv, MPS_mongPol)
        
        MP.Karakol.conv <- Fossil.MAT.prep(MP.Karakol, MA.Karakol, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
        MP.Karakol.conv <- Fossil.corresp.surface(MP.Karakol.conv, MPS_mongPol)
        
        #### Kichikol ####
        Kichikol.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Kichikol.conv,
          MAge = MA.Kichikol$Age,
          Fit.val = 0.25,
          LakeName = "Kichikolv",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol.csv",
          Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_NMSDB.pdf",
          H = 1100, W = 1900
          )
        
        #### Karakol ####
        Karakol.NMSDB <- FT.core(
          Model.WAPLS = WAPLS.NMSDB,
          Model.MAT = MAT.NMSDB,
          Model.RF = RF.NMSDB,
          Model.BRT = BRT.NMSDB,
          MCore = MP.Karakol.conv,
          MAge = MA.Karakol$Age,
          Fit.val = 0.25,
          LakeName = "Karakol",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol.csv",
          Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_NMSDB.pdf",
          H = 1100, W = 1900
          )  
      }
      
      
      }
  
  #### MDB    : Fusion Matrice locale / DB Odile MONGOLIE ####
  MDB = F
  if(MDB == T){
    #### Extraction des donnees propres ####
    Mong.site <- row.names(DBMong.Coord)
    DB3058PolClim.Mong <- DB3058PolClim[Mong.site,]

    #### Preparation data Pollen ####
    Keep.name <- c(Mong.site, row.names(MPS_mongCoord))
    DBMong.tot.Coord <- merge(DBMong.Coord, MPS_mongCoord, all = T, sort = F)
    row.names(DBMong.tot.Coord) <- Keep.name
    PollenSurf.Mong.tot <- DB3058PolClim.Mong[, 1:103]  # pollen, enleve climat
    PollenSurf.Mong.tot[PollenSurf.Mong.tot <= 0.5] <- 0
    MPS_mongPol_Globcal <- Surf.MAT.prep(MPS_mongPol, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)

    DB.odile.Co.MDB.path = "Resultats/World_DB/Pollen/Func_trans/Surface/SIG/Odile_MDB_Co.csv"
    write.table(DBMong.tot.Coord, file = DB.odile.Co.MDB.path, row.names=T, col.names=NA, sep=",", dec = ".")
    
    #### Map of the subsamples ####
    map.biom = F
    if(map.biom == T){
      Biome.Pol.map(Site.loc = DBMong.tot.Coord, # MDB
                    Site.loc2 = MPS_mongCoord, # NMSDB
                    #Site.loc3 = DBMong.tot.Coord,
                    Biome.select = "COST",
                    H = 600, W = 1200,
                    Save.plot = "Figures/Mongolia/Maps/Map_eurasian_pollen_DB.pdf"
      )}

    #### Merge Matrice Pollen Mongolia totale ####
    azzer <- c(row.names(PollenSurf.Mong.tot), row.names(MPS_mongPol_Globcal))
    PollenSurf.COST.MMN <- merge(PollenSurf.Mong.tot, MPS_mongPol_Globcal, all = T, sort = F)
    PollenSurf.COST.MMN[is.na(PollenSurf.COST.MMN)] <- 0
    row.names(PollenSurf.COST.MMN) <- azzer
    PollenSurf.COST.MMN <- PollenSurf.COST.MMN[colSums(PollenSurf.COST.MMN)!=0]
    PollenSurf.COST.MMN <- (PollenSurf.COST.MMN/rowSums(PollenSurf.COST.MMN))
    
    #### Merge Matrice Climat ####
    Clim.Mong.tot <- DB3058PolClim.Mong[, 104:length( DB3058PolClim.Mong)]  # climat, enleve pollen
    Clim1 <- subset(MPS_mongClim, select = c(MAAT, MAP, Psum, MTCOQ, MTWAQ))
    Clim.Mong.tot <- subset(Clim.Mong.tot, select = -c(GDD5, RUNOFF, AETPET))
    colnames(Clim1) <- c("MAAT", "MAP", "Psum","MTCO","MTWA")
    colnames(Clim.Mong.tot ) <- c("MTCO", "MTWA", "MAAT","MAP","Psum")
    MClim.merge <- merge(Clim.Mong.tot, Clim1, all = T, sort = F)
    MClim.merge[is.na(MClim.merge)] <- 0
    azzer2 <- c(row.names(Clim.Mong.tot), row.names(Clim1))
    row.names(MClim.merge) <- azzer2
    
    #### Calcul MAT WAPLS RF ####
    MClim.merge <- Surf.Clim.Pol(PollenSurf.COST.MMN, MClim.merge)
    PollenSurf.COST.MMN <- Surf.check.clim(PollenSurf.COST.MMN, MClim.merge)
    
    Calculate.FT = F
    if(Calculate.FT == T){
      MAT.MDB.bool = T
      if(MAT.MDB.bool == T){
        MAT.MDB <- FT.quantif(PollenSurf.COST.MMN, MClim.merge,
                                       Mcoord = DBMong.tot.Coord[1:2],
                                       Model = "MAT",
                                       Nb.arg = 15,
                                       Save.RDS = T,
                                       Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_MDB.csv")}
  
      WAPLS.MDB.bool = T
      if(WAPLS.MDB.bool == T){
        WAPLS.MDB <- FT.quantif(PollenSurf.COST.MMN, MClim.merge,
                                         Mcoord = DBMong.tot.Coord[1:2],
                                         Model = "WAPLS",
                                         Nb.arg = 15,
                                         Save.RDS = T,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_MDB.csv")}
    
      RF.MDB.bool = F
      if(RF.MDB.bool == T){
        RF.MDB <- FT.quantif(PollenSurf.COST.MMN, MClim.merge,
                                      Mcoord = DBMong.tot.Coord[1:2],
                                      Model = "RF",
                                      Save.RDS = T,
                                      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/RF_MDB.csv")}
      
      BRT.MDB.bool = F
      if(BRT.MDB.bool == T){
        BRT.MDB <- FT.quantif(PollenSurf.COST.MMN, MClim.merge,
                                    Mcoord = DBMong.tot.Coord[1:2],
                                    Model = "BRT",
                                    Save.RDS = T,
                                    Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_MDB.csv")}}
    else{
      MAT.MDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_MDB.Rds")
      WAPLS.MDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_MDB.Rds")
      RF.MDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/RF_MDB.Rds")
      BRT.MDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_MDB.Rds")
    }
    
    #### RL surface / climat ####
    LR = F
    if(LR == T){
      Tax.Princ <- subset(PollenSurf.COST.MMN, select = c(CHENOPOD, BETULA, POACEAE, ARTEMISI, CYPERACE, RANUNCUL, LARIX, PINUSHAP, PINUSDIP))
      Clim.Princ <- subset(MClim.merge, select = c(MAP))
      LinRelClim.MongolieTOT <- Plot.relation.Surf.Clim(PollenSurf.COST.MMN, MClim.merge, Nb.max = 9,
                                                        Label2 = "MMN",
                                                        H = 1100, W = 1100,
                                                        Name.taxa = Corresp_name,
                                                        Save.plot = "Figures/Mongolia/Pollen/Func_trans/Calib/LR_MongTOTCal.pdf")}
    
    #### Plots Ayrag ####
    plot.Ayrag.ft2 = F
    if(plot.Ayrag.ft2 == T){
      MP_Ayrag.conv <- Fossil.MAT.prep(MP.Ayrag, MP.Ayrag.Age, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name, Displot = F)
      MP_Ayrag.conv <- Fossil.corresp.surface(MP_Ayrag.conv, PollenSurf.COST.MMN)
      
      Ayrag.model.stepB <- FT.core(Model.WAPLS = WAPLS.MDB,
                                           Model.MAT = MAT.MDB,
                                           Model.RF = RF.MDB,
                                           Model.BRT = BRT.MDB,
                                           MCore = MP_Ayrag.conv,
                                           MAge = MP.Ayrag.Age$Age,
                                           Fit.val = 0.25,
                                           Only.fit = T,
                                           Ecartype.curve = c(T,T,T,T),
                                           LakeName = "Ayrag",
                                           Model.param.show = T,
                                           #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                           #Zone.Temp = c("C","W","C","W"),
                                           Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                           Zone.Temp = c("C","W"),
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag.csv",
                                           Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_MDB.pdf",
                                           Save.RDS = T,
                                           H = 1100, W = 1900)}

    Analog.map.Ayrag.MDB = F
    if(Analog.map.Ayrag.MDB == T){
      Analogue.Ayrag.MDB <- Analogue.map(Model.WAPLS = MAT.MDB,
                                         MAge = MP.Ayrag.Age,
                                         MCore = MP_Ayrag.conv,
                                         MCoord = DBMong.tot.Coord,
                                         Clim.choix = "MAAT",
                                         Age.choice = c(-57, 182, 806, 1645, 2901, 3172),
                                         Zone.plot = c("Mongolia"),
                                         H = 600, W = 900,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Analogues/MAT_Ayrag_MDB.csv",
                                         Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Anal_maps/AnalMap_Ayrag_MDB.pdf"
                                         )}
  
    #### Plots Altai D1L1 ####
    plot.D1L1.MDB = F
    if(plot.D1L1.MDB == T){
      MP.D1L1.conv <- Fossil.MAT.prep(MP.D1L1, MAge.D1L1.raw, Type_MAT = "Type_MAT_Odile", Displot = T,  Corresp_name = Corresp_name.Gottingen)
      MP.D1L1.conv <- Fossil.corresp.surface(MP.D1L1.conv, PollenSurf.COST.MMN)
      
      D1L1.model.MDB <- FT.core(Model.WAPLS = WAPLS.MDB,
                                           Model.MAT = MAT.MDB,
                                           Model.RF = RF.MDB,
                                           Model.BRT = BRT.MDB,
                                           MCore = MP.D1L1.conv,
                                           MAge = MAge.D1L1.raw$Age,
                                           Fit.val = 0.25,
                                           Only.fit = T,
                                           Ecartype.curve = c(T,T,T,T),
                                           LakeName = "D1L1",
                                           Model.param.show = T,
                                           #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                           #Zone.Temp = c("C","W","C","W"),
                                           Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                           Zone.Temp = c("C","W"),
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1.csv",
                                           Save.plot = "Figures/Mongolia/Pollen/Func_trans/D1L1/D1L1_MDB.pdf",
                                           Save.RDS = T,
                                           H = 1100, W = 1900)}
    
    Analog.map.D1L1.MDB = F
    if(Analog.map.D1L1.MDB == T){
      Analogue.D1L1.MDB <- Analogue.map(Model.WAPLS = MAT.MDB,
                                         MAge = MAge.D1L1.raw,
                                         MCore = MP.D1L1.conv,
                                         MCoord = DBMong.tot.Coord,
                                         Clim.choix = "MAAT",
                                         Age.choice = c(-57, 182, 806, 1645, 2901, 3172),
                                         Zone.plot = c("Mongolia"),
                                         H = 600, W = 900,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Analogues/MAT_Ayrag_MDB.csv",
                                         Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Anal_maps/AnalMap_Ayrag_MDB.pdf"
      )}
    
    #### Plots Altai D3L6 ####
    plot.D3L6.MDB = F
    if(plot.D3L6.MDB == T){
      MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Odile", Displot = T,  Corresp_name = Corresp_name.Gottingen)
      MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, PollenSurf.COST.MMN)
      
      D3L6.model.MDB <- FT.core(Model.WAPLS = WAPLS.MDB,
                                        Model.MAT = MAT.MDB,
                                        Model.RF = RF.MDB,
                                        Model.BRT = BRT.MDB,
                                        MCore = MP.D3L6.conv,
                                        MAge = MAge.D3L6.raw$Age,
                                        Fit.val = 0.25,
                                        Only.fit = T,
                                        Ecartype.curve = c(T,T,T,T),
                                        LakeName = "D3L6",
                                        Model.param.show = T,
                                        #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                        #Zone.Temp = c("C","W","C","W"),
                                        Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                        Zone.Temp = c("C","W"),
                                        Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6.csv",
                                        Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/D3L6_MDB.pdf",
                                        Save.RDS = T,
                                        H = 1100, W = 1900)}
    
    #### Plot Russian ERAPDB cores ####
    ACA.ERAPDB = T
    if(ACA.ERAPDB == T){
      Corresp_name_ERAPDB  <- data.frame(read.csv(file="Import/World_DB/Pollen/ERAPDB/Indexes/Corresp_pollen_FT_ERAPDB.csv",sep=",",dec=".",header=T, row.names = 1)) 
      Corresp_name_ERAPDB <- Corresp_name_ERAPDB[Corresp_name_ERAPDB$Type == "P",]
      files.ERAPDB <- list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = T)
      ERAPDB.pollen <- lapply(files.ERAPDB, read.csv, header=TRUE, stringsAsFactors=FALSE, row.names = 1)
      names(ERAPDB.pollen) <-gsub(".csv", "", list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = F))
      ERAPDB.pollen <- ERAPDB.pollen[-9] # Remove Kotopel
      
      Total.conv <- function(x){
        y <- x[row.names(x) %in% c("Age","Depth"),]
        x[row.names(x) %in% c("Age","Depth"),] <- NA
        x <- na.omit(x)
        x <- data.frame(t(t(x)/rowSums(t(x))))
        print(names(x)[1])
        x <- Fossil.MAT.prep(data.frame(t(x)), data.frame(t(y)), Type_MAT = "Corresp_FT_Mong", Displot = T, Corresp_name = Corresp_name_ERAPDB)
        x <- Fossil.corresp.surface(x, PollenSurf.COST.MMN)
        x <- list(x, y)
        return(x)
      }
      ERAPDB.pollen.conv <- purrr::map(ERAPDB.pollen, Total.conv)
      
      Total.FT <- function(x){
        Age <- data.frame(t(x[[2]]))
        x <- x[[1]]
        Lake.name <- gsub(".Ech_1", "", row.names(x)[1])
        print(Lake.name)
        
        x <- FT.core(
          Model.WAPLS = WAPLS.MDB,
          Model.MAT = MAT.MDB,
          Model.RF = RF.MDB,
          Model.BRT = BRT.MDB,
          MCore = x,
          MAge = Age$Age,
          Fit.val = 0.25,
          LakeName = Lake.name,
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
          Zone.Temp = c("C","W","C","W","C", "W"),
          Save.RDS = T,
          Save.path = paste("Resultats/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
          Save.plot = paste("Figures/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_MDB.pdf", sep = ""),
          H = 1100, W = 1900
        )
        return(x)
      }
      ERAPDB.FT <- purrr::map(ERAPDB.pollen.conv[7], Total.FT) # Only Dulikha
      # /!\ problème sur le 9 (Khendyrkul)
      
    }
  }
  
  #### COSTDB : Fusion Matrice locale / globale STEPPE FROID ####
  Calib.COST = F
  if(Calib.COST == T){
      #### Extraction des donnees propres ####
      COST.site <- row.names(DB3058Biome)[which(DB3058Biome == "COST")]
      DB3058PolClim.COST <- DB3058PolClim[COST.site,]
      DB3058Coord.COST <- DB3191Coord[COST.site,]
      
      # Keep.name <- c(COST.site, row.names(MPS_mongCoord))
      # DB3058Coord.COST <- merge(DB3058Coord.COST, MPS_mongCoord, all = F, sort = F)
      # row.names(DB3058Coord.COST) <- Keep.name
      
      #### Preparation data Pollen ####
      #Biome.Pol.map()
      #points(DB3058Coord.COST$LONG, DB3058Coord.COST$LAT)
  
      PollenSurf.COST <- DB3058PolClim.COST[, 1:103]  # pollen
      PollenSurf.COST[PollenSurf.COST <= 0.5] <- 0
      PollenSurf.COST <- (PollenSurf.COST/rowSums(PollenSurf.COST))
      MPS_mongPol_Globcal <- Surf.MAT.prep(MPS_mongPol, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)
      
      #### Merge Matrice Pollen ####
      azzer <- c(row.names(PollenSurf.COST), row.names(MPS_mongPol_Globcal))
      PollenSurf.COST.MMN <- merge(PollenSurf.COST, MPS_mongPol_Globcal, all = T, sort = F)
      PollenSurf.COST.MMN[is.na(PollenSurf.COST.MMN)] <- 0
      row.names(PollenSurf.COST.MMN) <- azzer
      PollenSurf.COST.MMN2 <- PollenSurf.COST.MMN[colSums(PollenSurf.COST.MMN)!=0]
      
      #### Merge Matrice Climat ####
      Clim.COST <- DB3058PolClim.COST[, 104:length( DB3058PolClim.COST)]  # climat
      Clim1 <- subset(MPS_mongClim, select = c(MAAT, MAP, Psum, MTCOQ, MTWAQ))
      Clim.COST <- subset(Clim.COST, select = -c(GDD5, RUNOFF, AETPET))
      colnames(Clim1) <- c("MAAT", "MAP", "Psum","MTCO","MTWA")
      colnames(Clim.COST ) <- c("MTCO", "MTWA", "MAAT","MAP","Psum")
      MClim.merge <- merge(Clim.COST, Clim1, all = T, sort = F)
      MClim.merge[is.na(MClim.merge)] <- 0
      azzer2 <- c(row.names(Clim.COST), row.names(Clim1))
      row.names(MClim.merge) <- azzer2
      
      #### Calcul MAT WAPLS RF ####
      MClim.merge <- Surf.Clim.Pol(PollenSurf.COST.MMN, MClim.merge)
      PollenSurf.COST.MMN <- Surf.check.clim(PollenSurf.COST.MMN, MClim.merge)
      PollenSurf.COST.MMN2 <- Surf.check.clim(PollenSurf.COST.MMN2, MClim.merge)
      
      Calculate.FT = F
      if(Calculate.FT == T){
        MAT.Ayrag.COST.check = T
        if(MAT.Ayrag.COST.check == T){
          Remove.corrupt = T  # Bug with some negative MAP value in WC2
          if(Remove.corrupt == T){
            Site.corrupt = c("SU_828", "SU_832", "SU_833", "SU_834", "SU_837")
            Corrupt.sites <- row.names(MClim.merge[4])[which(MClim.merge[4] < 0)]
            PollenSurf.COST.MMN2 <- PollenSurf.COST.MMN2[setdiff(row.names(PollenSurf.COST.MMN2), Corrupt.sites),]
            PollenSurf.COST.MMN2 <- PollenSurf.COST.MMN2[setdiff(row.names(PollenSurf.COST.MMN2), Site.corrupt),]
            MClim.merge <- MClim.merge[setdiff(row.names(MClim.merge), Corrupt.sites),]
            MClim.merge <- MClim.merge[setdiff(row.names(MClim.merge), Site.corrupt),]
            DB3058Coord.COST <- DB3058Coord.COST[setdiff(row.names(DB3058Coord.COST), Corrupt.sites),]
            DB3058Coord.COST <- DB3058Coord.COST[setdiff(row.names(DB3058Coord.COST), Site.corrupt),]
          }
          
          MAT.COSTDB <- FT.quantif(PollenSurf.COST.MMN2, MClim.merge,
                                         Mcoord = DB3058Coord.COST[1:2],
                                         Model = "MAT",
                                         Nb.arg = 10,
                                         Save.RDS = T,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_COSTDB.csv")}
        
        WAPLS.Ayrag.COST.check = F
        if(WAPLS.Ayrag.COST.check == T){
          WAPLS.COSTDB <- FT.quantif(PollenSurf.COST.MMN2, MClim.merge,
                                           Mcoord = DB3058Coord.COST[1:2],
                                           Model = "WAPLS",
                                           Nb.arg = 10,
                                           Save.RDS = T,
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_COSTDB.csv")}
  
        RF.Ayrag.COST.check = F
        if(RF.Ayrag.COST.check == T){
          RF.COSTDB <- FT.quantif(PollenSurf.COST.MMN2, MClim.merge,
                                            Mcoord = DB3058Coord.COST[1:2],
                                            Model = "RF",
                                            Save.RDS = T,
                                            Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/RF_COSTDB.csv")}
        
        RF.Ayrag.COST.check = F
        if(RF.Ayrag.COST.check == T){
          BRT.COSTDB <- FT.quantif(PollenSurf.COST.MMN2, MClim.merge,
                                       Mcoord = DB3058Coord.COST[1:2],
                                       Model = "BRT",
                                       Save.RDS = T,
                                       Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_COSTDB.csv")}}
      else{
        MAT.COSTDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_COSTDB.Rds")
        WAPLS.COSTDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_COSTDB.Rds")
        RF.COSTDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/RF_COSTDB.Rds")
        BRT.COSTDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_COSTDB.Rds")
        }
      
      #### Plots Ayrag ####
      plot.Ayrag.ft3 = F
      if(plot.Ayrag.ft3 == T){
        MP_Ayrag.conv <- Fossil.MAT.prep(MP.Ayrag, MP.Ayrag.Age, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)
        MP_Ayrag.conv <- Fossil.corresp.surface(MP_Ayrag.conv, PollenSurf.COST.MMN2)
        
        Ayrag.COST <- FT.core(Model.WAPLS = WAPLS.COSTDB,
                                            Model.MAT = MAT.COSTDB,
                                            Model.RF = RF.COSTDB,
                                            Model.BRT = BRT.COSTDB,
                                            MCore = MP_Ayrag.conv,
                                            MAge = MP.Ayrag.Age$Age,
                                            Fit.val = 0.25,
                                            Ecartype.curve = c(T,T,T,F),
                                            Only.fit = T,
                                            LakeName = "Ayrag",
                                            Model.param.show = F, 
                                            #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                            #Zone.Temp = c("C","W","C","W"),
                                            Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                            Zone.Temp = c("C","W"),
                                            Save.RDS = T,
                                            Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag.csv",
                                            Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_COSTDB.pdf",
                                            H = 1100, W = 1900)}
                        
      
      test.randomTF = T
      if(test.randomTF == T){
        rlghr <- randomTF(spp = sqrt(PollenSurf.COST.MMN2), 
                          env = MClim.merge[c(3,4)],
                          fos = sqrt(MP_Ayrag.conv), 
                          n = 99, fun = MAT, col = 1)
        
        rlghr$sig
        plot(rlghr)
      }
      
      LR.COST = T
      if(LR.COST == T){
        # source("Scripts/GDGT_calib.R")
        # Mat.corel.LR.pol <- Mat.corel(MClim.merge, PollenSurf.COST.MMN2, I.confiance = 0.95,
        #                               Display.pval = "blank",
        #                               Disp.R = "number",
        #                               Label.simple = T,
        #                               Title = "Correlation between br-GDGT and climate parameters.",
        #                               Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/Matcor_clim_pollen.csv",
        #                               Save.plot = "Figures/Mongolia/Pollen/Func_trans/Calib/Matcor_clim_pollen.pdf",
        #                               H = 200,
        #                               W = 2500)
        
        Tax.Princ <- subset(PollenSurf.COST.MMN2, select = c(PINUSDIP, BETULA, PICEA, LARIX, ARTEMISI, POACEAE, CYPERACE, CHENOPOD))
        Clim.Princ <- subset(MClim.merge, select = c(MAP, MAAT))
        #LinRelClim.MongolieTOT <- Plot.relation.Surf.Clim(Tax.Princ, Clim.Princ, Nb.max = 9, Label2 = "MMN", Label3 = 'SU_')
        LinRelClim.COST <- Plot.relation.Surf.Clim(Tax.Princ, Clim.Princ, Nb.max = 16,
                                                   Plot.manual = T,
                                                   Label2 = "MMN",
                                                   Label3 = 'SU_',
                                                   H = 1000, W = 1000, 
                                                   L.position = c(rep("bottom", 8), rep("top", 8)),
                                                   Lab.clim = c(expression(paste(MAP, (mm.yr^-1))), NA, NA, NA, 
                                                                expression(paste(MAP, (mm.yr^-1))), NA, NA, NA, 
                                                                "MAAT (°C)", NA, NA, NA, 
                                                                "MAAT (°C)", NA, NA, NA),
                                                   Name.taxa = Corresp_name,
                                                   Save.plot = "Figures/Mongolia/Pollen/Func_trans/Calib/LR_MongCOST_V2.pdf")}
      
      Analog.map.3 = F
      if(Analog.map.3 == T){
        Analogue.Ayrag.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                            MAge = MP.Ayrag.Age,
                                            MCore = MP_Ayrag.conv,
                                            MCoord = DBMong.tot.Coord,
                                            Zone.plot = c("Mongolia"),#, "Kazakhstan"),
                                            Clim.choix = "MAAT")}
      
      #### Plots D1L1 ####
      plot.D1L1.ft3 = F
      if(plot.D1L1.ft3 == T){
        MP.D1L1.conv <- Fossil.MAT.prep(MP.D1L1, MAge.D1L1.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
        MP.D1L1.conv <- Fossil.corresp.surface(MP.D1L1.conv, PollenSurf.COST.MMN2)
        
        Ayrag.COST <- FT.core(Model.WAPLS = WAPLS.COSTDB,
                                      Model.MAT = MAT.COSTDB,
                                      Model.RF = RF.COSTDB,
                                      Model.BRT = BRT.COSTDB,
                                      MCore = MP.D1L1.conv,
                                      MAge = MAge.D1L1.raw$Age,
                                      Fit.val = 0.25,
                                      Ecartype.curve = c(T,T,T,F),
                                      Only.fit = T,
                                      LakeName = "D1L1",
                                      Model.param.show = F, 
                                      #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                      #Zone.Temp = c("C","W","C","W"),
                                      Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                      Zone.Temp = c("C","W"),
                                      Save.RDS = T,
                                      Save.path = "Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1.csv",
                                      Save.plot = "Figures/Mongolia/Pollen/Func_trans/D1L1/D1L1_COSTDB.pdf",
                                      H = 1100, W = 1900)}
      
      #### Plots D3L6 ####
      plot.D3L6.ft3 = F
      if(plot.D3L6.ft3 == T){
        MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
        MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, PollenSurf.COST.MMN2)
        
        D3L6.COST <- FT.core(Model.WAPLS = WAPLS.COSTDB,
                                      Model.MAT = MAT.COSTDB,
                                      Model.RF = RF.COSTDB,
                                      Model.BRT = BRT.COSTDB,
                                      MCore = MP.D3L6.conv,
                                      MAge = MAge.D3L6.raw$Age,
                                      Fit.val = 0.25,
                                      Ecartype.curve = c(T,T,T,F),
                                      Only.fit = T,
                                      LakeName = "D3L6",
                                      Model.param.show = F, 
                                      #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                      #Zone.Temp = c("C","W","C","W"),
                                      Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                      Zone.Temp = c("C","W"),
                                      Save.RDS = T,
                                      Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6.csv",
                                      Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/D3L6_COSTDB.pdf",
                                      H = 1100, W = 1900)}
      
      Analog.map.D3L6.COST = F
      if(Analog.map.D3L6.COST == T){
        MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
        MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, PollenSurf.COST.MMN2)
        
        Analogue.D3L6.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                            MAge = MAge.D3L6.raw,
                                            MCore = MP.D3L6.conv,
                                            MCoord = DB3058Coord.COST,
                                           Age.choice = c(-66.0, 990.7, 1171.6, 1328.6, 2234.7, 3815.8),
                                           Zone.plot = c("Mongolia", "Russia",
                                                         "Uzbekistan", "Tajikistan",
                                                         "Kazakhstan","Turkmenistan",
                                                         "China"),
                                           Clim.choix = "MAP",
                                           H = 800, W = 1600,
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/Analogues/MAT_D3L6_COST_MAP.csv",
                                           Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/Anal_maps/AnalMap_D3L6_COST_MAP.pdf")}
      
      #### Plot Mongolian neotoma cores ####
      plot.mong.ft.COST = F
      if(plot.mong.ft.COST == T){
        #### Preparation ####
        MP.Hubsugul_nuur.conv <- Fossil.corresp.surface(MP.Hubsugul_nuur.conv, PollenSurf.COST.MMN2)
        MP.Hoton_Nuur.conv <- Fossil.corresp.surface(MP.Hoton_Nuur.conv, PollenSurf.COST.MMN2)
        MP.Dood_Nuur.conv <- Fossil.corresp.surface(MP.Dood_Nuur.conv, PollenSurf.COST.MMN2)
        MP.Daba_Nuur.conv <- Fossil.corresp.surface(MP.Daba_Nuur.conv, PollenSurf.COST.MMN2)
        MP.Gun_Nuur.conv <- Fossil.corresp.surface(MP.Gun_Nuur.conv, PollenSurf.COST.MMN2)
        MP.Achit_Nuur.conv <- Fossil.corresp.surface(MP.Achit_Nuur.conv, PollenSurf.COST.MMN2)
        
        #### Hubsugul_nuur ####
        Hubsugul_nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Hubsugul_nuur.conv,
          MAge = MA.Hubsugul_nuur$Age,
          Fit.val = 0.25,
          LakeName = "Hubsugul_nuurv",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur_COSTDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Hoton_Nuur ####
        Hoton_Nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Hoton_Nuur.conv,
          MAge = MA.Hoton_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Hoton_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur_COSTDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Dood_Nuur ####
        Dood_Nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Dood_Nuur.conv,
          MAge = MA.Dood_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Dood_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur_COSTDB.pdf",
          H = 1100, W = 1900)
        
        
        #### Daba_Nuur ####
        Daba_Nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Daba_Nuur.conv,
          MAge = MA.Daba_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Daba_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur_COSTDB.pdf",
          H = 1100, W = 1900
        )
        
        #### Achit_Nuur ####
        Achit_Nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Achit_Nuur.conv,
          MAge = MA.Achit_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Achit_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur_COSTDB.pdf",
          H = 1100, W = 1900
        )
        #### Gun_Nuur ####
        Gun_Nuur.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Gun_Nuur.conv,
          MAge = MA.Gun_Nuur$Age,
          Fit.val = 0.25,
          LakeName = "Gun_Nuur",
          Only.fit = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          Zone.Clim.span = c(160, 420, 730, 1030),
          Zone.Temp = c("C","W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur.csv",
          Save.plot = "Figures/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_COSTDB.pdf",
          H = 1100, W = 1900
        )
        
      }
      
      }
  
  #### TAIGA ####
  Calib.TAIG = F
  if(Calib.TAIG == T){
    #### Preparation ####
    MP.Hubsugul_nuur.conv <- Fossil.MAT.prep(MP.Hubsugul_nuur, MA.Hubsugul_nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    MP.Hoton_Nuur.conv    <- Fossil.MAT.prep(MP.Hoton_Nuur, MA.Hoton_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    MP.Dood_Nuur.conv     <- Fossil.MAT.prep(MP.Dood_Nuur, MA.Dood_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    MP.Daba_Nuur.conv     <- Fossil.MAT.prep(MP.Daba_Nuur, MA.Daba_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    MP.Gun_Nuur.conv      <- Fossil.MAT.prep(MP.Gun_Nuur, MA.Gun_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    MP.Achit_Nuur.conv    <- Fossil.MAT.prep(MP.Achit_Nuur, MA.Achit_Nuur, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
    
    MP.Hubsugul_nuur.conv <- Fossil.corresp.surface(MP.Hubsugul_nuur.conv, DB.odile.Po.TAIGDB)
    MP.Hoton_Nuur.conv <- Fossil.corresp.surface(MP.Hoton_Nuur.conv, DB.odile.Po.TAIGDB)
    MP.Dood_Nuur.conv <- Fossil.corresp.surface(MP.Dood_Nuur.conv, DB.odile.Po.TAIGDB)
    MP.Daba_Nuur.conv <- Fossil.corresp.surface(MP.Daba_Nuur.conv, DB.odile.Po.TAIGDB)
    MP.Gun_Nuur.conv <- Fossil.corresp.surface(MP.Gun_Nuur.conv, DB.odile.Po.TAIGDB)
    MP.Achit_Nuur.conv <- Fossil.corresp.surface(MP.Achit_Nuur.conv, DB.odile.Po.TAIGDB)
    
    #### Hubsugul_nuur ####
    Hubsugul_nuur.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Hubsugul_nuur.conv,
      MAge = MA.Hubsugul_nuur$Age,
      Fit.val = 0.25,
      LakeName = "Hubsugul_nuurv",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur.csv",
      Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Hoton_Nuur ####
    Hoton_Nuur.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Hoton_Nuur.conv,
      MAge = MA.Hoton_Nuur$Age,
      Fit.val = 0.25,
      LakeName = "Hoton_Nuur",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur.csv",
      Save.plot = "Figures/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Dood_Nuur ####
    Dood_Nuur.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Dood_Nuur.conv,
      MAge = MA.Dood_Nuur$Age,
      Fit.val = 0.25,
      LakeName = "Dood_Nuur",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur.csv",
      Save.plot = "Figures/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur_TAIGDB.pdf",
      H = 1100, W = 1900)
    
    
    #### Daba_Nuur ####
    Daba_Nuur.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Daba_Nuur.conv,
      MAge = MA.Daba_Nuur$Age,
      Fit.val = 0.25,
      LakeName = "Daba_Nuur",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur.csv",
      Save.plot = "Figures/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Achit_Nuur ####
    Achit_Nuur.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Achit_Nuur.conv,
      MAge = MA.Achit_Nuur$Age,
      Fit.val = 0.25,
      LakeName = "Achit_Nuur",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur.csv",
      Save.plot = "Figures/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    #### Gun_Nuur ####
      Gun_Nuur.TAIGDB <- FT.core(
        Model.WAPLS = WAPLS.TAIGDB,
        Model.MAT = MAT.TAIGDB,
        Model.RF = RF.TAIGDB,
        Model.BRT = BRT.TAIGDB,
        MCore = MP.Gun_Nuur.conv,
        MAge = MA.Gun_Nuur$Age,
        Fit.val = 0.25,
        LakeName = "Gun_Nuur",
        Only.fit = T,
        Ecartype.curve = c(T, T, T, T),
        Model.param.show = T,
        Zone.Clim.span = c(160, 420, 730, 1030),
        Zone.Temp = c("C","W"),#,"C","W"),
        Save.RDS = T,
        Save.path = "Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur.csv",
        Save.plot = "Figures/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_TAIGDB.pdf",
        H = 1100, W = 1900
      )
      
    }
  
  #### EAPDB  : Total DB Odile 3058 ####
  EAPDB = F
  if(EAPDB == T){
    #### Preparation data Pollen ####
    MP.EAPDB <- DB3058PolClim[, 1:103]  # pollen
    MP.EAPDB[MP.EAPDB <= 0.5] <- 0
    MP.EAPDB <- (MP.EAPDB/rowSums(MP.EAPDB))
    MPS_mongPol_Globcal <- Surf.MAT.prep(MPS_mongPol, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)
    
    #### Merge Matrice Pollen ####
    azzer <- c(row.names(MP.EAPDB), row.names(MPS_mongPol_Globcal))
    MP.EAPDB <- merge(MP.EAPDB, MPS_mongPol_Globcal, all = T, sort = F)
    MP.EAPDB[is.na(MP.EAPDB)] <- 0
    row.names(MP.EAPDB) <- azzer
    MP.EAPDB <- MP.EAPDB[colSums(MP.EAPDB)!=0]
    
    #### Merge Matrice Climat ####
    MC.EAPDB <- DB3058PolClim[, 104:length(DB3058PolClim)]  # climat
    Clim1 <- subset(MPS_mongClim, select = c(MAAT, MAP, Psum, MTCOQ, MTWAQ))
    MC.EAPDB <- subset(MC.EAPDB, select = -c(GDD5, RUNOFF, AETPET))
    colnames(Clim1) <- c("MAAT", "MAP", "Psum","MTCO","MTWA")
    colnames(MC.EAPDB ) <- c("MTCO", "MTWA", "MAAT","MAP","Psum")
    azzer2 <- c(row.names(MC.EAPDB), row.names(Clim1))
    MC.EAPDB <- merge(MC.EAPDB, Clim1, all = T, sort = F)
    row.names(MC.EAPDB) <- azzer2
    MC.EAPDB[is.na(MC.EAPDB)] <- 0
    
    #### Merge matrix coords ####
    DB3191Coord.simp <- DB3191Coord[1:2]
    Sites.3058 <- intersect(row.names(DB3191Coord.simp), row.names(MP.EAPDB))
    EAPDB.tot.Coord <- merge(DB3191Coord.simp[Sites.3058,], MPS_mongCoord, all = T, sort = F)
    row.names(EAPDB.tot.Coord) <- c(row.names(DB3191Coord.simp[Sites.3058,]), row.names(MPS_mongCoord))
    
    #### Export data merged ####
    write.table(MPS_mongPol_Globcal, file = "Resultats/Mongolia/Pollen/Func_trans/Surface/DBMongoliaPol.csv", row.names=T, col.names=NA, sep=",", dec = ".")
    write.table(MC.EAPDB, file = "Resultats/Mongolia/Pollen/Func_trans/Surface/DB3104_LucasPol.csv", row.names=T, col.names=NA, sep=",", dec = ".")
    
    #### Calcul MAT WAPLS ####
    MC.EAPDB <- Surf.Clim.Pol(MP.EAPDB, MC.EAPDB)
    MP.EAPDB <- Surf.check.clim(MP.EAPDB, MC.EAPDB)

    Calculate.FT = T
    if(Calculate.FT == T){
      MAT.EAPDB.check = T
      if(MAT.EAPDB.check == T){
        MAT.EAPDB <- FT.quantif(MP.EAPDB, MC.EAPDB,
                                       Mcoord = EAPDB.tot.Coord,
                                       Model = "MAT",
                                       Save.RDS = T,
                                       Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_EAPDB.csv")}
  
      WAPLS.EAPDB.check = T
      if(WAPLS.EAPDB.check == T){
        WAPLS.EAPDB <- FT.quantif(MP.EAPDB, MC.EAPDB,
                                       Mcoord = EAPDB.tot.Coord,
                                       Model = "WAPLS",
                                       Save.RDS = T,
                                       Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_EAPDB.csv")}
      
      RF.EAPDB.check = T
      if(RF.EAPDB.check == T){
        RF.EAPDB <- FT.quantif(MP.EAPDB, MC.EAPDB,
                                         Mcoord = EAPDB.tot.Coord,
                                         Model = "RF",
                                         Save.RDS = T,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/RF_EAPDB.csv")}
      
      BRT.EAPDB.check = T
      if(BRT.EAPDB.check == T){
        BRT.EAPDB <- FT.quantif(MP.EAPDB, MC.EAPDB,
                                         Mcoord = EAPDB.tot.Coord,
                                         Model = "BRT",
                                         Save.RDS = T,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_EAPDB.csv")}
    }
    else{
      MAT.EAPDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/MAT_EAPDB.Rds")
      WAPLS.EAPDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/WAPLS_EAPDB.Rds")
      RF.EAPDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/RF_EAPDB.Rds")
      BRT.EAPDB   <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Surface/BRT_EAPDB.Rds")
    }

    plot.Ayrag.ft4 = T
    if(plot.Ayrag.ft4 == T){
      MP_Ayrag.conv <- Fossil.MAT.prep(MP.Ayrag, MP.Ayrag.Age, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)
      MP_Ayrag.conv <- Fossil.corresp.surface(MP_Ayrag.conv, MP.EAPDB)
      
      Ayrag.model.stepB <- FT.core(Model.WAPLS = WAPLS.EAPDB,
                                           Model.MAT = MAT.EAPDB,
                                           MCore = MP_Ayrag.conv,
                                           MAge = MP.Ayrag.Age$Age,
                                           Fit.val = 0.25,
                                           Ecartype.curve = c(T,T,T,F),
                                           Only.fit = T,
                                           LakeName = "Ayrag",
                                           Model.param.show = F, 
                                           #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                           #Zone.Temp = c("C","W","C","W"),
                                           Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                           Zone.Temp = c("C","W"),
                                           Save.RDS = T,
                                           Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_EAPDB.csv",
                                           Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_EAPDB.pdf",
                                           H = 1100, W = 1700
                                           )}
    
    LR.4 = F
    if(LR.4 == T){
      Tax.Princ <- subset(PollenSurf.steppe.MMN2, select = c(CHENOPOD, BETULA, POACEAE, ARTEMISI, CYPERACE, BETULA, LARIX, PINUSHAP, PINUSDIP))
      Clim.Princ <- subset(MClim.merge, select = c(MAP))
      LinRelClim.MongolieTOT <- Plot.relation.Surf.Clim(Tax.Princ, Clim.Princ, Nb.max = 9, Label2 = "MMN", Label3 = 'SU_')}
  }
  
  EAPDB.V2 = F
  if(EAPDB.V2 == T){
    #### Plots Ayrag ####
    plot.Ayrag.EAPDB = F
    if(plot.Ayrag.EAPDB == T){
      MP_Ayrag.conv <- Fossil.MAT.prep(MP.Ayrag, MP.Ayrag.Age, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name)
      MP_Ayrag.conv <- Fossil.corresp.surface(MP_Ayrag.conv, DB.odile.Po)
      
      Ayrag.EAPDB <- FT.core(Model.WAPLS = WAPLS.EAPDB,
                                    Model.MAT = MAT.EAPDB,
                                    Model.RF = RF.EAPDB,
                                    Model.BRT = BRT.EAPDB,
                                    MCore = MP_Ayrag.conv,
                                    MAge = MP.Ayrag.Age$Age,
                                    Fit.val = 0.25,
                                    Ecartype.curve = c(T,T,T,F),
                                    Only.fit = T,
                                    LakeName = "Ayrag",
                                    Model.param.show = F, 
                                    #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                    #Zone.Temp = c("C","W","C","W"),
                                    Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                    Zone.Temp = c("C","W"),
                                    Save.RDS = T,
                                    Save.path = "Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag.csv",
                                    Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_EAPDB.pdf",
                                    H = 1100, W = 1900)}
    
    #### Plots D1L1 ####
    plot.D1L1.ft3 = F
    if(plot.D1L1.ft3 == T){
      MP.D1L1.conv <- Fossil.MAT.prep(MP.D1L1, MAge.D1L1.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
      MP.D1L1.conv <- Fossil.corresp.surface(MP.D1L1.conv, DB.odile.Po)
      
      Ayrag.EAPDB <- FT.core(Model.WAPLS = WAPLS.EAPDB,
                                    Model.MAT = MAT.EAPDB,
                                    Model.RF = RF.EAPDB,
                                    Model.BRT = BRT.EAPDB,
                                    MCore = MP.D1L1.conv,
                                    MAge = MAge.D1L1.raw$Age,
                                    Fit.val = 0.25,
                                    Ecartype.curve = c(T,T,T,F),
                                    Only.fit = T,
                                    LakeName = "D1L1",
                                    Model.param.show = F, 
                                    #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                    #Zone.Temp = c("C","W","C","W"),
                                    Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                    Zone.Temp = c("C","W"),
                                    Save.RDS = T,
                                    Save.path = "Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1.csv",
                                    Save.plot = "Figures/Mongolia/Pollen/Func_trans/D1L1/D1L1_EAPDB.pdf",
                                    H = 1100, W = 1900)}
    
    #### Plots D3L6 ####
    plot.D3L6.ft3 = T
    if(plot.D3L6.ft3 == T){
      MAT.EAPDB$MAP <- MAP.MAT.EAPDB.CV # Correction MAP
      MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
      MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, DB.odile.Po)
      
      D3L6.EAPDB <- FT.core(Model.WAPLS = WAPLS.EAPDB,
                                   Model.MAT = MAT.EAPDB,
                                   Model.RF = RF.EAPDB,
                                   Model.BRT = BRT.EAPDB,
                                   MCore = MP.D3L6.conv,
                                   MAge = MAge.D3L6.raw$Age,
                                   Fit.val = 0.25,
                                   Ecartype.curve = c(T,T,T,F),
                                   Only.fit = T,
                                   LakeName = "D3L6",
                                   Model.param.show = F, 
                                   #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                                   #Zone.Temp = c("C","W","C","W"),
                                   Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                                   Zone.Temp = c("C","W"),
                                   Save.RDS = T,
                                   Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6.csv",
                                   Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/D3L6_EAPDB.pdf",
                                   H = 1100, W = 1900)}
    
    # Correction MAP
    
    if(Modif.MAT.MAP == T){
      D3L6.EAPDB.MAP.MAT = predict(MAP.MAT.EAPDB.CV, MP.D3L6.conv, k = 10, sse = T, nboot = 1000, verbose = T)
      Old.D3L6.EAPDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_EAPDB.Rds")
      Old.D3L6.EAPDB$MAT.EAPDB$MAP <- D3L6.EAPDB.MAP.MAT$fit[,1]
      saveRDS(Old.D3L6.EAPDB, "Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_EAPDB.Rds")
      }
    
    
    Analog.map.D3L6.EAPDB = T
    if(Analog.map.D3L6.EAPDB == T){
      MAT.EAPDB$MAP <- MAP.MAT.EAPDB.CV # Correction MAP
      MP.D3L6.conv <- Fossil.MAT.prep(MP.D3L6, MAge.D3L6.raw, Type_MAT = "Type_MAT_Odile", Corresp_name = Corresp_name.Gottingen)
      MP.D3L6.conv <- Fossil.corresp.surface(MP.D3L6.conv, DB.odile.Po)
      
      Analogue.D3L6.EAPDB <- Analogue.map(Model.WAPLS = MAT.EAPDB,
                                         MAge = MAge.D3L6.raw,
                                         MCore = MP.D3L6.conv,
                                         MCoord = DB3191Coord,
                                         Age.choice = c(-66.0, 990.7, 1171.6, 1328.6, 2234.7, 3815.8),
                                         Zone.plot = c("Mongolia", "Russia",
                                                       "Uzbekistan", "Tajikistan",
                                                       "Kazakhstan","Turkmenistan",
                                                       "China"),
                                         Clim.choix = "MAP",
                                         H = 800, W = 1600,
                                         Save.path = "Resultats/Mongolia/Pollen/Func_trans/D3L6/Analogues/MAT_D3L6_EAPDB_MAP.csv",
                                         Save.plot = "Figures/Mongolia/Pollen/Func_trans/D3L6/Anal_maps/AnalMap_D3L6_EAPDB_MAP.pdf")}
    
  }
  
  #### Plot GDGT meme style ####
  GDGT.plot.OK = F
  if(GDGT.plot.OK == T){
    Zone.OK = T
    par(mfrow = c(3,2))
    Fit.val = 0.2
    Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500)
    Zone.Temp = c("C","W","C","W")
    
    GDGT.AyragClim <- subset(GDGT.AyragClim, select = c(MAAT_mr_Mong, MAAT_mr_DJ,MAATL_Sun, MAAT_5Me_DJ, MAAT_soil5Me_Naaf, MAP_mr_Mong2))
    Leg.Y <- c(expression("MAAT"[mr_Mong]),
               expression("MAAT"[mr_DeJonge]),
               expression("MAAT"[lake_Sun]),
               expression("MAAT"[5*Me_DeJonge]),
                  expression("MAAT"[5*Me_Naaf]),
               expression("MAP"[mr_Mong]))
    
    Zone.Temp <- rep(Zone.Temp,each=2)
    for(i in 1:ncol(GDGT.AyragClim)){
      plot(Age.GDGT$Age, GDGT.AyragClim[,i], 
           xlim=c(min(Age.GDGT$Age),max(Age.GDGT$Age)), 
           #ylim = c(ymin, ymax), 
           type="l", 
           ylab= Leg.Y[i], 
           xlab="Time (yr cal BP)", 
           col= "#c96f1dff", 
           las=0, lwd=1, bty="n")
      abline(h = -0.37083, lty="dotted")
      Curve.fit = lowess(GDGT.AyragClim[,i], f = Fit.val)
      lines(Age.GDGT$Age, Curve.fit$y, col="Orange", lwd=2)
      
    
    if (Zone.OK == T){
      for(j in 1:length(Zone.Clim.span)){
        if(as.logical(j%%2) == T){        # seulement les j impairs
          a <- which(Age.GDGT$Age>Zone.Clim.span[j])
          b <- which(Age.GDGT$Age>Zone.Clim.span[j+1])
          #Ymax <- max(GDGT.AyragClim[a[1]:b[1],i])
          Ymax <- max(GDGT.AyragClim[,i])
          #Ymin <- min(GDGT.AyragClim[a[1]:b[1],i])
          Ymin <- min(GDGT.AyragClim[,i])
          chaud = rgb(1, 0, 0,0.08)
          froid = rgb(0, 0, 1,0.08)
          print(Zone.Temp[j])
          if(Zone.Temp[j]=="C"){colTemp <- froid}
          if(Zone.Temp[j]=="W"){colTemp <- chaud}
          polygon(c(Zone.Clim.span[j],Zone.Clim.span[j+1], Zone.Clim.span[j+1],Zone.Clim.span[j]), c(Ymin, Ymin, Ymax, Ymax), border = NA, col = colTemp)
        }}}}
    par(mfrow = c(1,1))
    }
  }

#### Uzbekistan ####
Uzbekistan = T
if(Uzbekistan == T){
  #### Import Data ####
  MP_Faz <- data.frame(t(read.csv(file="Resultats/Uzbekistan/Pollen/Cores/Fazilman/MP_Fazilman_pollen_clean.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE)))
  MA_Faz    <- data.frame(t(readRDS("Resultats/Uzbekistan/Pollen/Cores/Fazilman/MA_Fazilman_pollen")))
  MA_Faz <- data.frame(t(MA_Faz[na.omit(match(row.names(MP_Faz), names(MA_Faz)))]))
  Uz.MP <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clean.Rds")
  Uz.clim <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clim.Rds")
  Uz.coord <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_coord.Rds")
  Corresp_name  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
  
  MP_Fazilman.conv <- Fossil.MAT.prep(MP_Faz, MA_Faz, Type_MAT = "Type_Odile",  Corresp_name = Corresp_name, Displot = T,
                                      Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_DP_check.pdf")
  
  MP_Aral86 <- data.frame(t(read.csv(file="Import/Uzbekistan/Pollen/Cores/Neotoma/Aral86.csv",sep=",",dec=".",header=T, row.names=1, stringsAsFactors = FALSE)))
  MA_Aral86 <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Cores/Neotoma/Aral86_MA.csv",sep="\t",dec=",",header=T, row.names=1, stringsAsFactors = FALSE))
  Corresp_name_neotoma  <- data.frame(read.csv(file="Import/World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP_Aral86.conv <- Fossil.MAT.prep(MP_Aral86, MA_Aral86, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma, Save.plot = "Figures/Uzbekistan/Pollen/Cores/Neotoma/DP_Aral86.pdf")
  
  #### TUDB: Calib Uzbekistan + Tajikistan database ####
  TUDB = T
  if(TUDB == T){
    #### Check congruence ####
    Uz.MP.FT <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Surface/Uz_MP_FT_clean.Rds")
    MP_Fazilman.conv <- Fossil.corresp.surface(MP_Fazilman.conv, Uz.MP.FT)
    MP_Aral86.conv <- Fossil.corresp.surface(MP_Aral86.conv, Uz.MP.FT)
    
    #### Plot Modelling past - Fazilman ####
    plot.Fazilman.ft.TUDB = T
    if(plot.Fazilman.ft.TUDB == T){
      Fazilman.TUDB <- FT.core(
        Model.WAPLS = WAPLS.TUDB,
        Model.MAT = MAT.TUDB,
        Model.RF = RF.TUDB,
        Model.BRT = BRT.TUDB,
        MCore = MP_Fazilman.conv,
        MAge = MA_Faz$Age,
        Fit.val = 0.25,
        LakeName = "Fazilman",
        Only.fit = T,
        Ecartype.curve = c(T, T, F, F),
        Model.param.show = T,
        # Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
        #Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
        # Zone.Temp = c("C","W","C","W"),
        #Zone.Temp = c("C","W"),
        Save.RDS = T,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_TUDB.pdf",
        H = 1100, W = 1900
      )}
    
    #### Test et vérifications #### 
    test.randomTF = F
    if(test.randomTF == T){
      rlghr <- randomTF(spp = sqrt(Uz.MP), 
                        env = Uz.clim[c(4,1)],
                        # env = Uz.clim,
                        fos = sqrt(MP_Fazilman.conv), 
                        n = 99, fun = MAT, col = 1)
      
      rlghr$sig
      # X11()
      plot(rlghr)
    }
    
    Analog.map.TUDB = F
    if(Analog.map.TUDB == T){
      Anal.map.Fazilman.TUDB <- Analogue.map(Model.WAPLS = MAT.TUDB,
                                           MAge = MA_Faz,
                                           MCore = MP_Fazilman.conv,
                                           MCoord = Uz.coord,
                                           Age.choice = c(-28, 650, 1951, 5157, 7959, 9958),
                                           Zone.plot = c("Uzbekistan"),
                                           Clim.choix = "MAAT",
                                           H = 600, W = 900,
                                           Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Analogues/MAT_Fazilman_TUDB.csv",
                                           Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Anal_maps/AnalMap_Fazilman_TUDB.pdf")}
    
    #### Plot Modelling past - Aral 86 ####
    plot.Aral86.ft.TUDB = T
    if(plot.Aral86.ft.TUDB == T){
      Aral86.TUDB <- FT.core(
        Model.WAPLS = WAPLS.TUDB,
        Model.MAT = MAT.TUDB,
        Model.RF = RF.TUDB,
        Model.BRT = BRT.TUDB,
        MCore = MP_Aral86.conv,
        MAge = MA_Aral86$Top,
        Fit.val = 0.25,
        LakeName = "Aral86",
        Only.fit = T,
        Ecartype.curve = c(T, T, F, F),
        Model.param.show = T,
        Save.RDS = T,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_TUDB.pdf",
        H = 1100, W = 1900
      )}
    
    
  }
  
  #### COSTDB: steppe froide ####
  Calib.COST = T
  if(Calib.COST == T){
    #### Plots Fazilman ####
    plot.Fazilman.ft.COST = T
    if(plot.Fazilman.ft.COST == T){
      MP_Fazilman.conv <- Fossil.corresp.surface(MP_Fazilman.conv, DB.odile.Po.COST)
      Fazilman.COST <- FT.core(Model.WAPLS = WAPLS.COSTDB,
                            Model.MAT = MAT.COSTDB,
                            Model.RF = RF.COSTDB,
                            Model.BRT = BRT.COSTDB,
                            MCore = MP_Fazilman.conv,
                            MAge = MA_Faz$Age,
                            Fit.val = 0.25,
                            Ecartype.curve = c(T,T,T,F),
                            Only.fit = T,
                            LakeName = "Fazilman",
                            Model.param.show = F, 
                            #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                            #Zone.Temp = c("C","W","C","W"),
                            # Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                            # Zone.Temp = c("C","W"),
                            Save.RDS = T,
                            Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman.csv",
                            Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_COSTDB.pdf",
                            H = 1100, W = 1900)}
    
    #### Test et vérifs ####
    test.randomTF = F
    if(test.randomTF == T){
      rlghr <- randomTF(spp = sqrt(PollenSurf.COST.MMN2), 
                        env = MClim.merge[c(3,4)],
                        fos = sqrt(MP_Fazilman.conv), 
                        n = 99, fun = MAT, col = 1)
      
      rlghr$sig
      plot(rlghr)
    }
    
    LR.COST = F
    if(LR.COST == T){
      # source("Scripts/GDGT_calib.R")
      # Mat.corel.LR.pol <- Mat.corel(MClim.merge, PollenSurf.COST.MMN2, I.confiance = 0.95,
      #                               Display.pval = "blank",
      #                               Disp.R = "number",
      #                               Label.simple = T,
      #                               Title = "Correlation between br-GDGT and climate parameters.",
      #                               Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Surface/Matcor_clim_pollen.csv",
      #                               Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Calib/Matcor_clim_pollen.pdf",
      #                               H = 200,
      #                               W = 2500)
      
      Tax.Princ <- subset(PollenSurf.COST.MMN2, select = c(PINUSDIP, BETULA, PICEA, LARIX, ARTEMISI, POACEAE, CYPERACE, CHENOPOD))
      Clim.Princ <- subset(MClim.merge, select = c(MAP, MAAT))
      #LinRelClim.UzolieTOT <- Plot.relation.Surf.Clim(Tax.Princ, Clim.Princ, Nb.max = 9, Label2 = "MMN", Label3 = 'SU_')
      LinRelClim.COST <- Plot.relation.Surf.Clim(Tax.Princ, Clim.Princ, Nb.max = 16,
                                                 Plot.manual = T,
                                                 Label2 = "MMN",
                                                 Label3 = 'SU_',
                                                 H = 1000, W = 1000, 
                                                 L.position = c(rep("bottom", 8), rep("top", 8)),
                                                 Lab.clim = c(expression(paste(MAP, (mm.yr^-1))), NA, NA, NA, 
                                                              expression(paste(MAP, (mm.yr^-1))), NA, NA, NA, 
                                                              "MAAT (°C)", NA, NA, NA, 
                                                              "MAAT (°C)", NA, NA, NA),
                                                 Name.taxa = Corresp_name,
                                                 Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Calib/LR_UzCOST_V2.pdf")}
    
    Analog.map.3 = F
    if(Analog.map.3 == T){
      Analogue.Fazilman.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                          MAge = MA_Faz,
                                          MCore = MP_Fazilman.conv,
                                          MCoord = DBUz.tot.Coord, 
                                          Age.choice = c(-28, 650, 1951, 5157, 7959, 9958),
                                          Zone.plot = c("Uzbekistan"),#, "Kazakhstan"),
                                          Clim.choix = "MAAT")}
    
    #### Plots Aral86 ####
    plot.Aral86.ft.COST = T
    if(plot.Aral86.ft.COST == T){
      MP_Aral86.conv <- Fossil.corresp.surface(MP_Aral86.conv, DB.odile.Po.COST)
      Aral86.COST <- FT.core(Model.WAPLS = WAPLS.COSTDB,
                               Model.MAT = MAT.COSTDB,
                               Model.RF = RF.COSTDB,
                               Model.BRT = BRT.COSTDB,
                               MCore = MP_Aral86.conv,
                               MAge = MA_Aral86$Top,
                               Fit.val = 0.25,
                               Ecartype.curve = c(T,T,T,F),
                               Only.fit = T,
                               LakeName = "Aral86",
                               Model.param.show = F, 
                               #Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500),
                               #Zone.Temp = c("C","W","C","W"),
                               # Zone.Clim.span = c(160, 420, 730, 1030), ### LIA et MWP Behling et al.
                               # Zone.Temp = c("C","W"),
                               Save.RDS = T,
                               Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86.csv",
                               Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Neotoma/Aral86/Aral86_COSTDB.pdf",
                               H = 1100, W = 1900)}
    
  }
  

  #### WASTDB: steppe chaude ####
  Calib.WAST = F
  if(Calib.WAST == T){
    #### Plots Fazilman ####
    plot.Fazilman.WAST.ft = F
    if(plot.Fazilman.WAST.ft == T){
      MP_Fazilman.conv <- Fossil.corresp.surface(MP_Faz, DB.odile.Po.WAST) 
      plot.Fazilman.WAST.ft <- FT.core(
        Model.WAPLS = WAPLS.WASTDB,
        Model.MAT = MAT.WASTDB,
        Model.RF = RF.WASTDB,
        Model.BRT = BRT.WASTDB,
        MCore = MP_Fazilman.conv,
        MAge = MA_Faz$Age,
        Fit.val = 0.25,
        Only.fit = T,
        Ecartype.curve = c(T,T,T,F),
        Model.param.show = T,
        Save.RDS = T, Displot = F,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_WAST.pdf",
        H = 1300, W = 1700
      )}
  
    #### Analogue map ####
    Analog.map.Fazilman.WAST = F
    if(Analog.map.Fazilman.WAST == T){
      Analog.map.Fazilman.WAST <- Analogue.map(Model.WAPLS = MAT.WASTDB,
                                            MAge = MA.Fazilman,
                                            MCore = MP_Faz,
                                            MCoord = DB.odile.Co.WAST,
                                            Age.choice = c(-28, 650, 1951, 5157, 7959, 9958),
                                            Zone.plot = Map.Eurasia,
                                            Clim.choix = "MAAT",
                                            H = 350, W = 900,
                                            Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Analogues/MAT_Fazilman_WASTDB.csv",
                                            Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Anal_maps/AnalMap_Fazilman_WASTDB.pdf")
    }
    
    #### Plots Aral86 ####
    plot.Aral86.WAST.ft = T
    if(plot.Aral86.WAST.ft == T){
      MP_Aral86.conv <- Fossil.corresp.surface(MP_Aral86.conv, DB.odile.Po.WAST) 
      plot.Aral86.WAST.ft <- FT.core(
        Model.WAPLS = WAPLS.WASTDB,
        Model.MAT = MAT.WASTDB,
        Model.RF = RF.WASTDB,
        Model.BRT = BRT.WASTDB,
        MCore = MP_Aral86.conv,
        MAge = MA_Aral86$Top,
        Fit.val = 0.25,
        Only.fit = T,
        Ecartype.curve = c(T,T,T,F),
        Model.param.show = T,
        Save.RDS = T, Displot = F,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Aral86/Aral86.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Aral86/Aral86_WAST.pdf",
        H = 1300, W = 1700
      )}
    
    }
  
  #### STDB: all steppes ####
  Calib.STDB = F
  if(Calib.STDB == T){
    plot.Fazilman.ST.ft = T
    if(plot.Fazilman.ST.ft == T){
      MP_Fazilman.conv <- Fossil.corresp.surface(MP_Faz, DB.odile.Po.ST) 
      plot.Fazilman.ST.ft <- FT.core(
        Model.WAPLS = WAPLS.STDB,
        Model.MAT = MAT.STDB,
        Model.RF = RF.STDB,
        Model.BRT = BRT.STDB,
        MCore = MP_Fazilman.conv,
        MAge = MA_Faz$Age,
        Fit.val = 0.25,
        Only.fit = F,
        Ecartype.curve = c(T,T,T,F),
        Model.param.show = T, Save.RDS = T, Displot = F,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_ST.pdf",
        H = 1300, W = 1700
      )}
    
    #### Analogue map ####
    Analog.map.Fazilman.ST = F
    if(Analog.map.Fazilman.ST == T){
      Analog.map.Fazilman.ST <- Analogue.map(Model.WAPLS = MAT.ST,
                                          MAge = MA.Fazilman,
                                          MCore = MP_Faz,
                                          MCoord = DB.odile.Co.ST,
                                          Age.choice = c(-28, 650, 1951, 5157, 7959, 9958),
                                          Zone.plot = Map.Eurasia,
                                          Clim.choix = "MAAT",
                                          H = 350, W = 900,
                                          Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Analogues/MAT_Fazilman_STDB.csv",
                                          Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Anal_maps/AnalMap_Fazilman_STDB.pdf")
    }}
  
  #### EAPDB: Total DB Odile ####
  EAPDB = F
  if(EAPDB == T){
    #### Fazilman ####
    plot.Fazilman.EAPDB.ft = T
    if(plot.Fazilman.EAPDB.ft == T){
      MP_Fazilman.conv <- Fossil.corresp.surface(MP_Faz, DB.odile.Po) 
      plot.Fazilman.EAPDB.ft <- FT.core(
        Model.WAPLS = WAPLS.EAPDB,
        Model.MAT = MAT.EAPDB,
        Model.RF = RF.EAPDB,
        Model.BRT = BRT.EAPDB,
        MCore = MP_Fazilman.conv,
        MAge = MA_Faz$Age,
        Fit.val = 0.25,
        Ecartype.curve = c(T,T,T,F),
        Only.fit = T, 
        Model.param.show = T,
        # Zone.Clim.span = c(796, 2700, 3000, 4800),
        # Zone.Temp = c("W","C"),
        Save.RDS = T, Displot = F,
        Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman.csv",
        Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_EAPDB.pdf",
        H = 1300, W = 1700
      )}
    
    #### Analogue map ####
    Analog.map.Fazilman.EAPDB = T
    if(Analog.map.Fazilman.EAPDB == T){
      Analog.map.Fazilman.EAPDB <- Analogue.map(Model.WAPLS = MAT.EAPDB,
                                               MAge = MA.Fazilman,
                                               MCore = MP.Fazilman,
                                               MCoord = DB.odile.Co,
                                               Age.choice = c(-28, 650, 1951, 5157, 7959, 9958),
                                               Zone.plot = Map.Eurasia,
                                               Clim.choix = "MAAT",
                                               H = 350, W = 900,
                                               Save.path = "Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Analogues/MAT_Fazilman_EAPDBDB.csv",
                                               Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Anal_maps/AnalMap_Fazilman_EAPDBDB.pdf")
    }
  }
  }

#### China ####
China = F
if(China == T){
  #### Import Data ####
  Corresp_name_neotoma  <- data.frame(read.csv(file="Import//World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP.Zoige     <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Zoige_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Zoige     <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Zoige_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Wenquan        <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Wenquan_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Wenquan        <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Wenquan_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Tianchi_Liupan <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Tianchi_Liupan_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Tianchi_Liupan <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Tianchi_Liupan_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Tianchi_Gaoligong <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Tianchi_Gaoligong_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Tianchi_Gaoligong <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Tianchi_Gaoligong_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Kanas     <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Kanas_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Kanas     <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Kanas_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Kakitu    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Kakitu_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Kakitu    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Kakitu_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Hurleg    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Hurleg_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Hurleg    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Hurleg_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Bayanchagan    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Bayanchagan_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Bayanchagan    <- data.frame(read.csv(file="Resultats/China/Pollen/Cores/Bayanchagan_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  
  #### Convert pollen type ####
  MP.Zoige.conv <- Fossil.MAT.prep(MP.Zoige, MA.Zoige, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Wenquan.conv <- Fossil.MAT.prep(MP.Wenquan, MA.Wenquan, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Tianchi_Liupan.conv <- Fossil.MAT.prep(MP.Tianchi_Liupan, MA.Tianchi_Liupan, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Tianchi_Gaoligong.conv <- Fossil.MAT.prep(MP.Tianchi_Gaoligong, MA.Tianchi_Gaoligong, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Kanas.conv <- Fossil.MAT.prep(MP.Kanas, MA.Kanas, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Kakitu.conv <- Fossil.MAT.prep(MP.Kakitu, MA.Kakitu, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Hurleg.conv <- Fossil.MAT.prep(MP.Hurleg, MA.Hurleg, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Bayanchagan.conv <- Fossil.MAT.prep(MP.Bayanchagan, MA.Bayanchagan, Type_MAT = "Corresp_FT_Odile", Displot = T,  Corresp_name = Corresp_name_neotoma)
  
  #### COST DB ####
  plot.china.ft.COST = F
  if(plot.china.ft.COST == T){
    #### Preparation ####
    MP.Zoige.conv <- Fossil.corresp.surface(MP.Zoige.conv, DB.odile.Po.COST)
    MP.Wenquan.conv <- Fossil.corresp.surface(MP.Wenquan.conv, DB.odile.Po.COST)
    MP.Tianchi_Liupan.conv <- Fossil.corresp.surface(MP.Tianchi_Liupan.conv, DB.odile.Po.COST)
    MP.Tianchi_Gaoligong.conv <- Fossil.corresp.surface(MP.Tianchi_Gaoligong.conv, DB.odile.Po.COST)
    MP.Kanas.conv <- Fossil.corresp.surface(MP.Kanas.conv, DB.odile.Po.COST)
    MP.Kakitu.conv <- Fossil.corresp.surface(MP.Kakitu.conv, DB.odile.Po.COST)
    MP.Hurleg.conv <- Fossil.corresp.surface(MP.Hurleg.conv, DB.odile.Po.COST)
    MP.Bayanchagan.conv <- Fossil.corresp.surface(MP.Bayanchagan.conv, DB.odile.Po.COST)
    
    #### Zoige ####
    Zoige.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Zoige.conv,
      MAge = MA.Zoige$Age,
      Fit.val = 0.25,
      LakeName = "Zoige",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Zoige/Zoige.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Zoige/Zoige_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Wenquan ####
    Wenquan.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Wenquan.conv,
      MAge = MA.Wenquan$Age,
      Fit.val = 0.25,
      LakeName = "Wenquan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Wenquan/Wenquan.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Wenquan/Wenquan_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Tianchi_Liupan ####
    Tianchi_Liupan.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Tianchi_Liupan.conv,
      MAge = MA.Tianchi_Liupan$Age,
      Fit.val = 0.25,
      LakeName = "Tianchi_Liupan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan_COSTDB.pdf",
      H = 1100, W = 1900)
    
    
    #### Tianchi_Gaoligong ####
    Tianchi_Gaoligong.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Tianchi_Gaoligong.conv,
      MAge = MA.Tianchi_Gaoligong$Age,
      Fit.val = 0.25,
      LakeName = "Tianchi_Gaoligong",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Kanas ####
    Kanas.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Kanas.conv,
      MAge = MA.Kanas$Age,
      Fit.val = 0.25,
      LakeName = "Kanas",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Kanas/Kanas.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Kanas/Kanas_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Kakitu ####
    Kakitu.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Kakitu.conv,
      MAge = MA.Kakitu$Age,
      Fit.val = 0.25,
      LakeName = "Kakitu",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Kakitu/Kakitu.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Kakitu/Kakitu_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Hurleg ####
    Hurleg.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Hurleg.conv,
      MAge = MA.Hurleg$Age,
      Fit.val = 0.25,
      LakeName = "Hurleg",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Hurleg/Hurleg.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Hurleg/Hurleg_COSTDB.pdf",
      H = 1100, W = 1900
      ) 
    
    #### Bayanchagan ####
    Bayanchagan.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Bayanchagan.conv,
      MAge = MA.Bayanchagan$Age,
      Fit.val = 0.25,
      LakeName = "Bayanchagan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/China/Pollen/Func_trans/Bayanchagan/Bayanchagan.csv",
      Save.plot = "Figures/China/Pollen/Func_trans/Bayanchagan/Bayanchagan_COSTDB.pdf",
      H = 1100, W = 1900
    )
    }
  
}

#### Iran ####
Iran = F
if(Iran == T){
  #### Import Data ####
  Corresp_name_neotoma  <- data.frame(read.csv(file="Import//World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP.Gomishan     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Gomishan_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Gomishan     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Gomishan_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Maharlou     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Maharlou_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Maharlou     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Maharlou_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Parishan     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Parishan_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Parishan     <- data.frame(read.csv(file="Resultats/Iran/Pollen/Cores/Parishan_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 

  #### Convert pollen type ####
  MP.Parishan.conv <- Fossil.MAT.prep(MP.Parishan, MA.Parishan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Maharlou.conv <- Fossil.MAT.prep(MP.Maharlou, MA.Maharlou, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Gomishan.conv <- Fossil.MAT.prep(MP.Gomishan, MA.Gomishan, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  
  #### COST DB ####
  plot.iran.ft.COST = F
  if(plot.iran.ft.COST == T){
    #### Preparation ####
    MP.Parishan.conv <- Fossil.corresp.surface(MP.Parishan.conv, DB.odile.Po.COST)
    MP.Maharlou.conv <- Fossil.corresp.surface(MP.Maharlou.conv, DB.odile.Po.COST)
    MP.Gomishan.conv <- Fossil.corresp.surface(MP.Gomishan.conv, DB.odile.Po.COST)
    
    #### Parishan ####
    Parishan.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Parishan.conv,
      MAge = MA.Parishan$Age,
      Fit.val = 0.25,
      LakeName = "Parishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Parishan/Parishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Parishan/Parishan_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Maharlou ####
    Maharlou.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Maharlou.conv,
      MAge = MA.Maharlou$Age,
      Fit.val = 0.25,
      LakeName = "Maharlou",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Maharlou/Maharlou_COSTDB.pdf",
      H = 1100, W = 1900
    )  
    
    #### Gomishan ####
    Gomishan.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Gomishan.conv,
      MAge = MA.Gomishan$Age,
      Fit.val = 0.25,
      LakeName = "Gomishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Gomishan/Gomishan_COSTDB.pdf",
      H = 1100, W = 1900
    )
  }
  
  #### WAST DB ####
  plot.iran.ft.WAST = F
  if(plot.iran.ft.WAST == T){
    #### Preparation ####
    MP.Parishan.conv <- Fossil.corresp.surface(MP.Parishan.conv, DB.odile.Po.WAST)
    MP.Maharlou.conv <- Fossil.corresp.surface(MP.Maharlou.conv, DB.odile.Po.WAST)
    MP.Gomishan.conv <- Fossil.corresp.surface(MP.Gomishan.conv, DB.odile.Po.WAST)
    
    #### Parishan ####
    Parishan.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Parishan.conv,
      MAge = MA.Parishan$Age,
      Fit.val = 0.25,
      LakeName = "Parishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Parishan/Parishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Parishan/Parishan_WASTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Maharlou ####
    Maharlou.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Maharlou.conv,
      MAge = MA.Maharlou$Age,
      Fit.val = 0.25,
      LakeName = "Maharlou",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Maharlou/Maharlou_WASTDB.pdf",
      H = 1100, W = 1900
    )  
    
    #### Gomishan ####
    Gomishan.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Gomishan.conv,
      MAge = MA.Gomishan$Age,
      Fit.val = 0.25,
      LakeName = "Gomishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Gomishan/Gomishan_WASTDB.pdf",
      H = 1100, W = 1900
    )
  }
  
  #### EAPDB ####
  plot.iran.ft.EAPDB = F
  if(plot.iran.ft.EAPDB == T){
    #### Preparation ####
    MP.Parishan.conv <- Fossil.corresp.surface(MP.Parishan.conv, DB.odile.Po)
    MP.Maharlou.conv <- Fossil.corresp.surface(MP.Maharlou.conv, DB.odile.Po)
    MP.Gomishan.conv <- Fossil.corresp.surface(MP.Gomishan.conv, DB.odile.Po)
    
    #### Parishan ####
    Parishan.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Parishan.conv,
      MAge = MA.Parishan$Age,
      Fit.val = 0.25,
      LakeName = "Parishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Parishan/Parishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Parishan/Parishan_EAPDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Maharlou ####
    Maharlou.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Maharlou.conv,
      MAge = MA.Maharlou$Age,
      Fit.val = 0.25,
      LakeName = "Maharlou",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Maharlou/Maharlou_EAPDB.pdf",
      H = 1100, W = 1900
    )  
    
    #### Gomishan ####
    Gomishan.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Gomishan.conv,
      MAge = MA.Gomishan$Age,
      Fit.val = 0.25,
      LakeName = "Gomishan",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan.csv",
      Save.plot = "Figures/Iran/Pollen/Func_trans/Gomishan/Gomishan_EAPDB.pdf",
      H = 1100, W = 1900
    )
    }
  }

#### Russia ####
Russia = F
if(Russia == T){
  #### Import Data ####
  Corresp_name_neotoma  <- data.frame(read.csv(file="Import/World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP.Baikal     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Baikal_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Baikal     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Baikal_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Derput     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Derput_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Derput     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Derput_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Kotokel     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Kotokel_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Kotokel     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Kotokel_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Nuochaga     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Nuochaga_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Nuochaga     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Nuochaga_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Suollakh     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Suollakh_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Suollakh     <- data.frame(read.csv(file="Resultats/Russia/Pollen/Cores/Suollakh_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 

  #### ERAPDB ####
  ACA.ERAPDB = F
  if(ACA.ERAPDB == T){
    Corresp_name_ERAPDB  <- data.frame(read.csv(file="Import/World_DB/Pollen/ERAPDB/Indexes/Corresp_pollen_FT_ERAPDB.csv",sep=",",dec=".",header=T, row.names = 1)) 
    Corresp_name_ERAPDB <- Corresp_name_ERAPDB[Corresp_name_ERAPDB$Type == "P",]
    files.ERAPDB <- list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = T)
    ERAPDB.pollen <- lapply(files.ERAPDB, read.csv, header=TRUE, stringsAsFactors=FALSE, row.names = 1)
    names(ERAPDB.pollen) <-gsub(".csv", "", list.files(path = "Resultats/World_DB/Pollen/ERAPDB_sites/", pattern = ".csv", full.names = F))
    ERAPDB.pollen <- ERAPDB.pollen[-9] # Remove Kotopel*
    #ERAPDB.no.MAD <- lapply(ERAPDB.pollen, function(x) {x[row.names(x) %in% c("Age","Depth"),] <- NA ;na.omit(x)})
    #ERAPDB.MAD <- lapply(ERAPDB.pollen, function(x) {y <- x[row.names(x) %in% c("Age","Depth"),]; y})
    
    #### COSTDB ####
    Total.conv <- function(x){
      y <- x[row.names(x) %in% c("Age","Depth"),]
      x[row.names(x) %in% c("Age","Depth"),] <- NA
      x <- na.omit(x)
      x <- data.frame(t(t(x)/rowSums(t(x))))
      print(gsub(".Ech_1", "", names(x)[1]))
      x <- Fossil.MAT.prep(data.frame(t(x)), data.frame(t(y)), Type_MAT = "Corresp_FT_Mong", Displot = T, Corresp_name = Corresp_name_ERAPDB)
      x <- Fossil.corresp.surface(x, DB.odile.Po.COST)
      x <- list(x, y)
      return(x)
      }
    # ERAPDB.pollen.conv <- purrr::map(ERAPDB.pollen, Total.conv)
    
    Total.FT.COST <- function(x){
      Age <- data.frame(t(x[[2]]))
      x <- x[[1]]
      #Lake.name <- gsub("\\..*", "", row.names(x)[1])
      Lake.name <- gsub(".Ech_1", "", row.names(x)[1])
      print(Lake.name)
      x <- FT.core(
        Model.WAPLS = WAPLS.COSTDB,
        Model.MAT = MAT.COSTDB,
        Model.RF = RF.COSTDB,
        Model.BRT = BRT.COSTDB,
        MCore = x,
        MAge = Age$Age,
        Fit.val = 0.25,
        LakeName = Lake.name,
        Only.fit = T,
        Ecartype.curve = c(T, T, T, T),
        Model.param.show = T,
        Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
        Zone.Temp = c("C","W","C","W","C", "W"),
        Save.RDS = T,
        Save.path = paste("Resultats/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
        Save.plot = paste("Figures/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_COSTDB.pdf", sep = ""),
        H = 1100, W = 1900
      )
      return(x)
    }
    # ERAPDB.FT.COST <- purrr::map(ERAPDB.pollen.conv[c(1:7,9:18)], Total.FT.COST)
    # bug sur Khendyrkul [8]
    
    #### TAIGDB ####
    Total.conv <- function(x){
      y <- x[row.names(x) %in% c("Age","Depth"),]
      x[row.names(x) %in% c("Age","Depth"),] <- NA
      x <- na.omit(x)
      x <- data.frame(t(t(x)/rowSums(t(x))))
      print(gsub(".Ech_1", "", names(x)[1]))
      x <- Fossil.MAT.prep(data.frame(t(x)), data.frame(t(y)), Type_MAT = "Corresp_FT_Mong", Displot = T, Corresp_name = Corresp_name_ERAPDB)
      x <- Fossil.corresp.surface(x, DB.odile.Po.TAIGDB)
      x <- list(x, y)
      return(x)
    }
    # ERAPDB.pollen.conv <- purrr::map(ERAPDB.pollen, Total.conv)
    
    Total.FT.TAIG <- function(x){
      Age <- data.frame(t(x[[2]]))
      x <- x[[1]]
      Lake.name <- gsub(".Ech_1", "", row.names(x)[1])
      
      x <- FT.core(
        Model.WAPLS = WAPLS.TAIGDB,
        Model.MAT = MAT.TAIGDB,
        Model.RF = RF.TAIGDB,
        Model.BRT = BRT.TAIGDB,
        MCore = x,
        MAge = Age$Age,
        Fit.val = 0.25,
        LakeName = Lake.name,
        Only.fit = T,
        Ecartype.curve = c(T, T, T, T),
        Model.param.show = T,
        Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
        Zone.Temp = c("C","W","C","W","C", "W"),
        Save.RDS = T,
        Save.path = paste("Resultats/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
        Save.plot = paste("Figures/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_TAIGDB.pdf", sep = ""),
        H = 1100, W = 1900
      )
      return(x)
    }
    # ERAPDB.FT.TAIG <- purrr::map(ERAPDB.pollen.conv[c(9:18)], Total.FT.TAIG)
    # bug sur Khendyrkul [8]
    
    #### EAPDB ####
    Total.conv <- function(x){
      y <- x[row.names(x) %in% c("Age","Depth"),]
      x[row.names(x) %in% c("Age","Depth"),] <- NA
      x <- na.omit(x)
      x <- data.frame(t(t(x)/rowSums(t(x))))
      print(gsub(".Ech_1", "", names(x)[1]))
      x <- Fossil.MAT.prep(data.frame(t(x)), data.frame(t(y)), Type_MAT = "Corresp_FT_Mong", Displot = T, Corresp_name = Corresp_name_ERAPDB)
      x <- Fossil.corresp.surface(x, DB.odile.Po)
      x <- list(x, y)
      return(x)
    }
    ERAPDB.pollen.conv <- purrr::map(ERAPDB.pollen, Total.conv)
    
    Total.FT.EAP <- function(x){
      Age <- data.frame(t(x[[2]]))
      x <- x[[1]]
      Lake.name <- gsub(".Ech_1", "", row.names(x)[1])
      
      x <- FT.core(
        Model.WAPLS = WAPLS.EAPDB,
        Model.MAT = MAT.EAPDB,
        Model.RF = RF.EAPDB,
        Model.BRT = BRT.EAPDB,
        MCore = x,
        MAge = Age$Age,
        Fit.val = 0.25,
        LakeName = Lake.name,
        Only.fit = T,
        Ecartype.curve = c(T, T, T, T),
        Model.param.show = T,
        Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
        Zone.Temp = c("C","W","C","W","C", "W"),
        Save.RDS = T,
        Save.path = paste("Resultats/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
        Save.plot = paste("Figures/Russia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_EAPDB.pdf", sep = ""),
        H = 1100, W = 1900
      )
      return(x)
    }
    ERAPDB.FT.EAP <- purrr::map(ERAPDB.pollen.conv[c(7)], Total.FT.EAP)
    # bug sur Khendyrkul [8]
    
    }
  
  #### Analogues maps Dulikha ####
  Analogue.Dulikha.map.COST = F
  if(Analogue.Dulikha.map.COST == T){
    Analogue.Dulikha.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                          MAge = data.frame(t(ERAPDB.pollen.conv[[7]][[2]][2,])),
                                          MCore = ERAPDB.pollen.conv$`Dulikha Bog`[[1]],
                                          MCoord = DB.odile.Co,
                                          Zone.plot = c("Mongolia", "Kazakhstan", "Russia"),
                                          Clim.choix = "MAAT", 
                                          Save.path = "Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha_MAT_COSTDB_analogues.csv",
                                          Save.plot = "Figures/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_COSTDB_analog_map.pdf", 
                                          H = 1000, W = 3000, 
                                          Age.choice = as.numeric(ERAPDB.pollen.conv$`Dulikha Bog`[[2]][2,])[c(1,20,40,60,80,113)]
                                          )
      }
  
  Analogue.Dulikha.map.TAIG = F
  if(Analogue.Dulikha.map.TAIG == T){
    Analogue.Dulikha.TAIG <- Analogue.map(Model.WAPLS = MAT.TAIGDB,
                                          MAge = data.frame(t(ERAPDB.pollen.conv[[7]][[2]][2,])),
                                          MCore = ERAPDB.pollen.conv$`Dulikha Bog`[[1]],
                                          MCoord = DB.odile.Co,
                                          Zone.plot = c("Mongolia", "Kazakhstan", "Russia"),
                                          Clim.choix = "MAAT", 
                                          Save.path = "Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha_MAT_TAIGDB_analogues.csv",
                                          Save.plot = "Figures/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_TAIGDB_analog_map.pdf", 
                                          H = 1000, W = 3000, 
                                          Age.choice = as.numeric(ERAPDB.pollen.conv$`Dulikha Bog`[[2]][2,])[c(1,20,40,60,80,113)]
    )
  }
  #### Convertion pollen type ####
  MP.Baikal.conv <- Fossil.MAT.prep(MP.Baikal, MA.Baikal, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Derput.conv <- Fossil.MAT.prep(MP.Derput, MA.Derput, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Kotokel.conv <- Fossil.MAT.prep(MP.Kotokel, MA.Kotokel, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Nuochaga.conv <- Fossil.MAT.prep(MP.Nuochaga, MA.Nuochaga, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Suollakh.conv <- Fossil.MAT.prep(MP.Suollakh, MA.Suollakh, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  
  #### COST DB ####
  plot.russ.ft.COST = F
  if(plot.russ.ft.COST == T){
    #### Preparation ####
    MP.Baikal.conv <- Fossil.corresp.surface(MP.Baikal.conv, DB.odile.Po.COST)
    MP.Derput.conv <- Fossil.corresp.surface(MP.Derput.conv, DB.odile.Po.COST)
    MP.Kotokel.conv <- Fossil.corresp.surface(MP.Kotokel.conv, DB.odile.Po.COST)
    MP.Nuochaga.conv <- Fossil.corresp.surface(MP.Nuochaga.conv, DB.odile.Po.COST)
    MP.Suollakh.conv <- Fossil.corresp.surface(MP.Suollakh.conv, DB.odile.Po.COST)
    
    #### Baikal ####
    Baikal.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Baikal.conv,
      MAge = MA.Baikal$Age,
      Fit.val = 0.25,
      LakeName = "Baikal",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Baikal/Baikal.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Baikal/Baikal_COSTDB.pdf",
      H = 1100, W = 1900
    )
    #### Derput #### 
    Derput.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Derput.conv,
      MAge = MA.Derput$Age,
      Fit.val = 0.25,
      LakeName = "Derput",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Derput/Derput.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Derput/Derput_COSTDB.pdf",
      H = 1100, W = 1900
    )  
    
    
    #### Kotokel ####
    Kotokel.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Kotokel.conv,
      MAge = MA.Kotokel$Age,
      Fit.val = 0.25,
      LakeName = "Kotokel",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Kotokel/Kotokel_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Nuochaga ####
    Nuochaga.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Nuochaga.conv,
      MAge = MA.Nuochaga$Age,
      Fit.val = 0.25,
      LakeName = "Nuochaga",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_COSTDB.pdf",
      H = 1100, W = 1900
    )
    #### Suollakh ####
    Suollakh.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Suollakh.conv,
      MAge = MA.Suollakh$Age,
      Fit.val = 0.25,
      LakeName = "Suollakh",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Suollakh/Suollakh_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    }
  #### TAIG DB ####
  plot.russ.ft.TAIG = F
  if(plot.russ.ft.TAIG == T){
    #### Preparation ####
    MP.Baikal.conv <- Fossil.corresp.surface(MP.Baikal.conv, DB.odile.Po.TAIGDB)
    MP.Derput.conv <- Fossil.corresp.surface(MP.Derput.conv, DB.odile.Po.TAIGDB)
    MP.Kotokel.conv <- Fossil.corresp.surface(MP.Kotokel.conv, DB.odile.Po.TAIGDB)
    MP.Nuochaga.conv <- Fossil.corresp.surface(MP.Nuochaga.conv, DB.odile.Po.TAIGDB)
    MP.Suollakh.conv <- Fossil.corresp.surface(MP.Suollakh.conv, DB.odile.Po.TAIGDB)
    
    #### Baikal ####
    Baikal.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Baikal.conv,
      MAge = MA.Baikal$Age,
      Fit.val = 0.25,
      LakeName = "Baikal",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Baikal/Baikal.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Baikal/Baikal_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    #### Derput #### 
    Derput.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Derput.conv,
      MAge = MA.Derput$Age,
      Fit.val = 0.25,
      LakeName = "Derput",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Derput/Derput.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Derput/Derput_TAIGDB.pdf",
      H = 1100, W = 1900
    )  
    
    
    #### Kotokel ####
    Kotokel.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Kotokel.conv,
      MAge = MA.Kotokel$Age,
      Fit.val = 0.25,
      LakeName = "Kotokel",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Kotokel/Kotokel_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Nuochaga ####
    Nuochaga.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Nuochaga.conv,
      MAge = MA.Nuochaga$Age,
      Fit.val = 0.25,
      LakeName = "Nuochaga",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    #### Suollakh ####
    Suollakh.TAIGDB <- FT.core(
      Model.WAPLS = WAPLS.TAIGDB,
      Model.MAT = MAT.TAIGDB,
      Model.RF = RF.TAIGDB,
      Model.BRT = BRT.TAIGDB,
      MCore = MP.Suollakh.conv,
      MAge = MA.Suollakh$Age,
      Fit.val = 0.25,
      LakeName = "Suollakh",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Suollakh/Suollakh_TAIGDB.pdf",
      H = 1100, W = 1900
    )
    
  }
  #### EAPDB ####
  plot.russ.ft.EAPDB = F
  if(plot.russ.ft.EAPDB == T){
    #### Preparation ####
    MP.Baikal.conv   <- Fossil.corresp.surface(MP.Baikal.conv, DB.odile.Po)
    MP.Derput.conv   <- Fossil.corresp.surface(MP.Derput.conv, DB.odile.Po)
    MP.Kotokel.conv  <- Fossil.corresp.surface(MP.Kotokel.conv, DB.odile.Po)
    MP.Nuochaga.conv <- Fossil.corresp.surface(MP.Nuochaga.conv, DB.odile.Po)
    MP.Suollakh.conv <- Fossil.corresp.surface(MP.Suollakh.conv, DB.odile.Po)
    
    #### Baikal ####
    Baikal.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Baikal.conv,
      MAge = MA.Baikal$Age,
      Fit.val = 0.25,
      LakeName = "Baikal",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Baikal/Baikal.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Baikal/Baikal_EAPDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Derput #### 
    Derput.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Derput.conv,
      MAge = MA.Derput$Age,
      Fit.val = 0.25,
      LakeName = "Derput",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Derput/Derput.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Derput/Derput_EAPDB.pdf",
      H = 1100, W = 1900
    )  
    
    
    #### Kotokel ####
    Kotokel.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Kotokel.conv,
      MAge = MA.Kotokel$Age,
      Fit.val = 0.25,
      LakeName = "Kotokel",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Kotokel/Kotokel_EAPDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Nuochaga ####
    Nuochaga.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Nuochaga.conv,
      MAge = MA.Nuochaga$Age,
      Fit.val = 0.25,
      LakeName = "Nuochaga",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_EAPDB.pdf",
      H = 1100, W = 1900
    )
    #### Suollakh ####
    Suollakh.EAPDB <- FT.core(
      Model.WAPLS = WAPLS.EAPDB,
      Model.MAT = MAT.EAPDB,
      Model.RF = RF.EAPDB,
      Model.BRT = BRT.EAPDB,
      MCore = MP.Suollakh.conv,
      MAge = MA.Suollakh$Age,
      Fit.val = 0.25,
      LakeName = "Suollakh",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh.csv",
      Save.plot = "Figures/Russia/Pollen/Func_trans/Suollakh/Suollakh_EAPDB.pdf",
      H = 1100, W = 1900
    )
    
  }
  }

#### Kirghizistan ####
Kirghizistan = F
if(Kirghizistan == T){
  #### Import Data ####
  Corresp_name_neotoma  <- data.frame(read.csv(file="Import//World_DB/Pollen/Neotoma/Indexes/Corresp_pollen_FT_Neotoma.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP.Kichikol     <- data.frame(read.csv(file="Resultats/Kirghizistan/Pollen/Cores/Kichikol_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Kichikol     <- data.frame(read.csv(file="Resultats/Kirghizistan/Pollen/Cores/Kichikol_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MP.Karakol     <- data.frame(read.csv(file="Resultats/Kirghizistan/Pollen/Cores/Karakol_MP.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Karakol     <- data.frame(read.csv(file="Resultats/Kirghizistan/Pollen/Cores/Karakol_MA.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  
  #### Convert pollen type ####
  MP.Kichikol.conv <- Fossil.MAT.prep(MP.Kichikol, MA.Kichikol, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  MP.Karakol.conv <- Fossil.MAT.prep(MP.Karakol, MA.Karakol, Type_MAT = "Corresp_FT_Mong", Displot = T,  Corresp_name = Corresp_name_neotoma)
  
  #### COSTDB ####
  plot.Kirghizistan.ft.COST = F
  if(plot.Kirghizistan.ft.COST == T){
    #### Preparation ####
    MP.Kichikol.conv <- Fossil.corresp.surface(MP.Kichikol.conv, DB.odile.Po.COST)
    MP.Karakol.conv <- Fossil.corresp.surface(MP.Karakol.conv, DB.odile.Po.COST)
    
    #### Kichikol ####
    Kichikol.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Kichikol.conv,
      MAge = MA.Kichikol$Age,
      Fit.val = 0.25,
      LakeName = "Kichikolv",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol.csv",
      Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
    #### Karakol ####
    Karakol.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Karakol.conv,
      MAge = MA.Karakol$Age,
      Fit.val = 0.25,
      LakeName = "Karakol",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol.csv",
      Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_COSTDB.pdf",
      H = 1100, W = 1900
    )  
    }
  
  #### WASTDB ####
  plot.Kirghizistan.ft.WASTDB = F
  if(plot.Kirghizistan.ft.WASTDB == T){
    #### Preparation ####
    MP.Kichikol.conv <- Fossil.corresp.surface(MP.Kichikol.conv, DB.odile.Po.WAST)
    MP.Karakol.conv <- Fossil.corresp.surface(MP.Karakol.conv, DB.odile.Po.WAST)
    
    #### Kichikol ####
    Kichikol.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Kichikol.conv,
      MAge = MA.Kichikol$Age,
      Fit.val = 0.25,
      LakeName = "Kichikolv",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol.csv",
      Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_WASTDB.pdf",
      H = 1100, W = 1900
      )
    
    #### Karakol ####
    Karakol.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Karakol.conv,
      MAge = MA.Karakol$Age,
      Fit.val = 0.25,
      LakeName = "Karakol",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol.csv",
      Save.plot = "Figures/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_WASTDB.pdf",
      H = 1100, W = 1900
      )  
    }
}

#### Kazakhstan ####
Kazakhstan = F
if(Kazakhstan == T){
  #### Import Data ####
  Corresp_name_aral  <- data.frame(read.csv(file="Import/Kazakhstan/Pollen/Cores/Corresp_pollen_FT_Aral.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) 
  MP.Aral     <- data.frame(read.csv(file="Import/Kazakhstan/Pollen/Cores/Aral_PoC.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  MA.Aral     <- data.frame(read.csv(file="Import/Kazakhstan/Pollen/Cores/MA_Aral.csv",sep=",",dec=".",header=T, stringsAsFactors = F, row.names = 1)) 
  
  #### Convert pollen type ####
  MP.Aral <- (MP.Aral/rowSums(MP.Aral))    # pourcentage pollen
  MP.Aral[MP.Aral <= 0.01] <- 0             # remove taxa < 1 %
  MP.Aral <- (MP.Aral/rowSums(MP.Aral)) 
  MP.Aral.conv <- Fossil.MAT.prep(MP.Aral, MA.Aral, Type_MAT = "Type_MAT_Odile", Displot = T,  Corresp_name = Corresp_name_aral)

  #### COSTDB ####
  plot.Kazakhstan.ft.COST = F
  if(plot.Kazakhstan.ft.COST == T){
    #### Preparation ####
    MP.Aral.conv.COST <- Fossil.corresp.surface(MP.Aral.conv, DB.odile.Po.COST)

    #### Aral ####
    Aral.COSTDB <- FT.core(
      Model.WAPLS = WAPLS.COSTDB,
      Model.MAT = MAT.COSTDB,
      Model.RF = RF.COSTDB,
      Model.BRT = BRT.COSTDB,
      MCore = MP.Aral.conv.COST,
      MAge = MA.Aral$Age,
      Fit.val = 0.25,
      LakeName = "Aral",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kazakhstan/Pollen/Func_trans/Aral/Aral.csv",
      Save.plot = "Figures/Kazakhstan/Pollen/Func_trans/Aral/Aral_COSTDB.pdf",
      H = 1100, W = 1900
    )
    
  }
  
  #### WASTDB ####
  plot.Kazakhstan.ft.WASTDB = F
  if(plot.Kazakhstan.ft.WASTDB == T){
    #### Preparation ####
    MP.Aral.conv.WAST <- Fossil.corresp.surface(MP.Aral.conv, DB.odile.Po.WAST)

    #### Aral ####
    Aral.WASTDB <- FT.core(
      Model.WAPLS = WAPLS.WASTDB,
      Model.MAT = MAT.WASTDB,
      Model.RF = RF.WASTDB,
      Model.BRT = BRT.WASTDB,
      MCore = MP.Aral.conv.WAST,
      MAge = MA.Aral$Age,
      Fit.val = 0.25,
      LakeName = "Aral",
      Only.fit = T,
      Ecartype.curve = c(T, T, T, T),
      Model.param.show = T,
      Zone.Clim.span = c(160, 420, 730, 1030),
      Zone.Temp = c("C","W"),#,"C","W"),
      Save.RDS = T,
      Save.path = "Resultats/Kazakhstan/Pollen/Func_trans/Aral/Aral.csv",
      Save.plot = "Figures/Kazakhstan/Pollen/Func_trans/Aral/Aral_WASTDB.pdf",
      H = 1100, W = 1900
    )
    
  }
}


#### Armenia ####
Armenia = F
if(Armenia == T){
  #### Import Data ####
  # Cores #
  Cores.import = T
  if(Cores.import == T){
    source("Scripts/Age.R")
    MP.Shenka <- data.frame(read.csv(file="Import/Armenia/Pollen/Shenkani/shenpo.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    MA.Shenka <- data.frame(read.csv(file="Import/Armenia/Pollen/Shenkani/MA_shenk.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    MP.Kanli  <- data.frame(read.csv(file="Import/Armenia/Pollen/Kanli/kanlCpo.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    MA.Kanli  <- data.frame(read.csv(file="Import/Armenia/Pollen/Kanli/MA_Kanli.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    MP.Zarishat <- data.frame(read.csv(file="Import/Armenia/Pollen/Zarishat/Zarishat_PoC.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE, fill = T))
    MA.Zarishat <- data.frame(read.csv(file="Import/Armenia/Pollen/Zarishat/MA_Zarishat.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    Corresp_Armenia <-data.frame(read.csv(file="Import/Armenia/Pollen/Zarishat/Corresp_pollen_FT_Armenia.csv",sep=",",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
    }
  
  #### Climate reconstruction Kanli ####
  Kanli = F
  if(Kanli == T){
    #### Check congruance surface / cores ####
    MP.Kanli <- (MP.Kanli/rowSums(MP.Kanli))    # pourcentage pollen
    MP.Kanli[MP.Kanli <= 0.01] <- 0             # remove taxa < 1 %
    MP.Kanli <- (MP.Kanli/rowSums(MP.Kanli)) 

    #### Climate reconstruction KANLI ####
    Kanli.FT = F
    if(Kanli.FT == T){
      #### COSTDB ####
      plot.Kanli.COST.ft = F
      if(plot.Kanli.COST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Kanli, DB.odile.Po.COST) 
        plot.Kanli.COST.ft <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Kanli,
          MAge = MA.Kanli$AGE,
          Fit.val = 0.25,
          Ecartype.curve = c(T,T,T,F),
          Only.fit = T,
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Kanli_COST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Kanli.COST = F
      if(Analog.map.Kanli.COST == T){
        Analog.map.Kanli.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                               MAge = MA.Kanli,
                                               MCore = MP.Kanli,
                                               MCoord = DB.odile.Co.COST,
                                               Age.choice = c(796, 1973, 3971, 6482, 8315, 9253),
                                               Zone.plot = Map.Eurasia,
                                               Clim.choix = "MAAT",
                                               H = 350, W = 900,
                                               Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Analogues/MAT_Kanli_COSTDB.csv",
                                               Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Anal_maps/AnalMap_Kanli_COSTDB.pdf")
      }
      
      #### COSTDB no Cicho ####
      plot.Kanli.COST.ft.no.cicho= F
      if(plot.Kanli.COST.ft.no.cicho == T){
        MP.Kanli.noC <- subset(MP.Kanli, select = -c(COMPLIGU))
        New.sum <- rowSums(MP.Kanli.noC)
        MP.Kanli.noC <- MP.Kanli.noC/New.sum
        
        plot.Kanli.COST.ft <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Kanli.noC,
          MAge = MA.Kanli$AGE,
          Fit.val = 0.25,
          Ecartype.curve = c(T,T,T,F),
          Only.fit = T,
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/FT_Kanli_COST_noCicho.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/FT_Kanli_COST_no.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Kanli.COST = F
      if(Analog.map.Kanli.COST == T){
        Analog.map.Kanli.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                              MAge = MA.Kanli,
                                              MCore = MP.Kanli,
                                              MCoord = DB.odile.Co.COST,
                                              Age.choice = c(796, 1973, 3971, 6482, 8315, 9253),
                                              Zone.plot = Map.Eurasia,
                                              Clim.choix = "MAAT",
                                              H = 350, W = 900,
                                              Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Analogues/MAT_Kanli_COSTDB.csv",
                                              Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Anal_maps/AnalMap_Kanli_COSTDB.pdf")
      }
      #### WASTDB ####
      plot.Kanli.WAST.ft = F
      if(plot.Kanli.WAST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Kanli, DB.odile.Po.WAST) 
        plot.Kanli.WAST.ft <- FT.core(
          Model.WAPLS = WAPLS.WASTDB,
          Model.MAT = MAT.WASTDB,
          Model.RF = RF.WASTDB,
          Model.BRT = BRT.WASTDB,
          MCore = MP.Kanli,
          MAge = MA.Kanli$AGE,
          Fit.val = 0.25,
          Only.fit = T,
          Ecartype.curve = c(T,T,T,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Kanli_WAST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Kanli.WAST = F
      if(Analog.map.Kanli.WAST == T){
        Analog.map.Kanli.WAST <- Analogue.map(Model.WAPLS = MAT.WASTDB,
                                              MAge = MA.Kanli,
                                              MCore = MP.Kanli,
                                              MCoord = DB.odile.Co.WAST,
                                              Age.choice = c(796, 1973, 3971, 6482, 8315, 9253),
                                              Zone.plot = Map.Eurasia,
                                              Clim.choix = "MAAT",
                                              H = 350, W = 900,
                                              Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Analogues/MAT_Kanli_WASTDB.csv",
                                              Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Anal_maps/AnalMap_Kanli_WASTDB.pdf")
      }
      
      #### STDB ####
      plot.Kanli.ST.ft = F
      if(plot.Kanli.ST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Kanli, DB.odile.Po.ST) 
        plot.Kanli.ST.ft <- FT.core(
          Model.WAPLS = WAPLS.ST,
          Model.MAT = MAT.ST,
          Model.RF = RF.ST,
          Model.BRT = BRT.ST,
          MCore = MP.Kanli,
          MAge = MA.Kanli$AGE,
          Fit.val = 0.25,
          Only.fit = F,
          Ecartype.curve = c(T,T,T,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Kanli_ST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Kanli.ST = F
      if(Analog.map.Kanli.ST == T){
        Analog.map.Kanli.ST <- Analogue.map(Model.WAPLS = MAT.ST,
                                              MAge = MA.Kanli,
                                              MCore = MP.Kanli,
                                              MCoord = DB.odile.Co.ST,
                                              Age.choice = c(796, 1973, 3971, 6482, 8315, 9253),
                                              Zone.plot = Map.Eurasia,
                                              Clim.choix = "MAAT",
                                              H = 350, W = 900,
                                              Save.path = "Resultats/Armenia/Pollen/Func_trans/Kanli/Analogues/MAT_Kanli_STDB.csv",
                                              Save.plot = "Figures/Armenia/Pollen/Func_trans/Kanli/Anal_maps/AnalMap_Kanli_STDB.pdf")
      }
      
    }}
  
  #### Climate reconstruction Shenkani ####
  Shenkani = F
  if(Shenkani == T){
    #### Check congruance surface / cores ####
    MP.Shenka <- (MP.Shenka/rowSums(MP.Shenka))    # pourcentage pollen
    MP.Shenka[MP.Shenka <= 0.01] <- 0             # remove taxa < 1 %
    MP.Shenka <- (MP.Shenka/rowSums(MP.Shenka)) 
    
    #### Climate reconstruction ####
    Shenka.FT = F
    if(Shenka.FT == T){
      #### COSTDB ####
      plot.Shenka.COST.ft = F
      if(plot.Shenka.COST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Shenka, DB.odile.Po.COST) 
        plot.Shenka.COST.ft <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Shenka,
          MAge = MA.Shenka$AGE,
          Fit.val = 0.25,
          Only.fit = T,
          Ecartype.curve = c(T,T,F,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Shenka_COST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Shenka.COST = T
      if(Analog.map.Shenka.COST == T){
        Analog.map.Shenka.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                             MAge = MA.Shenka,
                                             MCore = MP.Shenka,
                                             MCoord = DB.odile.Co.COST,
                                             Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                             Zone.plot = Map.Eurasia,
                                             Clim.choix = "MAAT",
                                             H = 400, W = 900,
                                             Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Analogues/MAT_Shenka_COSTDB.csv",
                                             Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Anal_maps/AnalMap_Shenka_COSTDB.pdf")
      }
      
      #### WASTDB ####
      plot.Shenka.WAST.ft = F
      if(plot.Shenka.WAST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Shenka, DB.odile.Po.WAST) 
        plot.Shenka.WAST.ft <- FT.core(
          Model.WAPLS = WAPLS.WASTDB,
          Model.MAT = MAT.WASTDB,
          Model.RF = RF.WASTDB,
          Model.BRT = BRT.WASTDB,
          MCore = MP.Shenka,
          MAge = MA.Shenka$AGE,
          Fit.val = 0.25,
          Only.fit = F,
          Ecartype.curve = c(T,T,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Shenka_WAST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Shenka.WAST = T
      if(Analog.map.Shenka.WAST == T){
        Analog.map.Shenka.WAST <- Analogue.map(Model.WAPLS = MAT.WASTDB,
                                             MAge = MA.Shenka,
                                             MCore = MP.Shenka,
                                             MCoord = DB.odile.Co.WAST,
                                             Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                             #Zone.plot = c("Mongolia"),
                                             Clim.choix = "MAAT",
                                             H = 400, W = 900,
                                             Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Analogues/MAT_Shenka_WASTDB.csv",
                                             Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Anal_maps/AnalMap_Shenka_WASTDB.pdf")
      }
      
      #### STDB ####
      plot.Shenka.ST.ft = F
      if(plot.Shenka.ST.ft == T){
        MP.Kanli <- Fossil.corresp.surface(MP.Shenka, DB.odile.Po.ST) 
        plot.Shenka.ST.ft <- FT.core(
          Model.WAPLS = WAPLS.ST,
          Model.MAT = MAT.ST,
          Model.RF = RF.ST,
          Model.BRT = BRT.ST,
          MCore = MP.Shenka,
          MAge = MA.Shenka$AGE,
          Fit.val = 0.25,
          Only.fit = T,
          Ecartype.curve = c(T,T,T),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Shenka_ST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Shenka.ST = F
      if(Analog.map.Shenka.ST == T){
        Analog.map.Shenka.ST <- Analogue.map(Model.WAPLS = MAT.ST,
                                             MAge = MA.Shenka,
                                             MCore = MP.Shenka,
                                             MCoord = DB.odile.Co.ST,
                                             Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                             #Zone.plot = c("Mongolia"),
                                             Clim.choix = "MAAT",
                                             H = 400, W = 900,
                                             Save.path = "Resultats/Armenia/Pollen/Func_trans/Shenka/Analogues/MAT_Shenka_STDB.csv",
                                             Save.plot = "Figures/Armenia/Pollen/Func_trans/Shenka/Anal_maps/AnalMap_Shenka_STDB.pdf")
      }
    }}
  
  #### Climate reconstruction Zarishat ####
  Zarishat = T
  if(Zarishat == T){
    #### Check congruance surface / cores ####
    MP.Zarishat[is.na(MP.Zarishat)] <- 0
    MP.Zarishat <- (MP.Zarishat/rowSums(MP.Zarishat))    # pourcentage pollen
    MP.Zarishat[MP.Zarishat <= 0.01] <- 0             # remove taxa < 1 %
    MP.Zarishat <- (MP.Zarishat/rowSums(MP.Zarishat)) 
    MP.Zarishat.conv <- Fossil.MAT.prep(MP.Zarishat, MA.Zarishat, Type_MAT = "Type_MAT_Odile",  Corresp_name = Corresp_Armenia, Displot = T)

    #### Climate reconstruction ####
    Zarishat.FT = F
    if(Zarishat.FT == T){
      #### COSTDB ####
      plot.Zarishat.COST.ft = T
      if(plot.Zarishat.COST.ft == T){
        MP.Zarishat.conv.COST <- Fossil.corresp.surface(MP.Zarishat.conv, DB.odile.Po.COST) 
        plot.Zarishat.COST.ft <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Zarishat.conv.COST,
          MAge = MA.Zarishat$Age,
          Fit.val = 0.25,
          Only.fit = T,
          Ecartype.curve = c(T,T,F,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Zarishat_COST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Zarishat.COST = F
      if(Analog.map.Zarishat.COST == T){
        Analog.map.Zarishat.COST <- Analogue.map(Model.WAPLS = MAT.COSTDB,
                                               MAge = MA.Zarishat,
                                               MCore = MP.Zarishat,
                                               MCoord = DB.odile.Co.COST,
                                               Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                               Zone.plot = Map.Eurasia,
                                               Clim.choix = "MAAT",
                                               H = 400, W = 900,
                                               Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Analogues/MAT_Zarishat_COSTDB.csv",
                                               Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Anal_maps/AnalMap_Zarishat_COSTDB.pdf")
      }
      
      #### WASTDB ####
      plot.Zarishat.WAST.ft = T
      if(plot.Zarishat.WAST.ft == T){
        MP.Zarishat.conv.WAST <- Fossil.corresp.surface(MP.Zarishat.conv, DB.odile.Po.WAST) 
        plot.Zarishat.WAST.ft <- FT.core(
          Model.WAPLS = WAPLS.WASTDB,
          Model.MAT = MAT.WASTDB,
          Model.RF = RF.WASTDB,
          Model.BRT = BRT.WASTDB,
          MCore = MP.Zarishat.conv.WAST,
          MAge = MA.Zarishat$Age,
          Fit.val = 0.25,
          Only.fit = F,
          Ecartype.curve = c(T,T,F),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Zarishat_WAST.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Zarishat.WAST = F
      if(Analog.map.Zarishat.WAST == T){
        Analog.map.Zarishat.WAST <- Analogue.map(Model.WAPLS = MAT.WASTDB,
                                               MAge = MA.Zarishat,
                                               MCore = MP.Zarishat,
                                               MCoord = DB.odile.Co.WAST,
                                               Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                               #Zone.plot = c("Mongolia"),
                                               Clim.choix = "MAAT",
                                               H = 400, W = 900,
                                               Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Analogues/MAT_Zarishat_WASTDB.csv",
                                               Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Anal_maps/AnalMap_Zarishat_WASTDB.pdf")
      }
      
      #### STDB ####
      plot.Zarishat.ST.ft = T
      if(plot.Zarishat.ST.ft == T){
        MP.Zarishat.conv.ST <- Fossil.corresp.surface(MP.Zarishat.conv, DB.odile.Po.ST) 
        plot.Zarishat.ST.ft <- FT.core(
          Model.WAPLS = WAPLS.STDB,
          Model.MAT = MAT.STDB,
          Model.RF = RF.STDB,
          Model.BRT = BRT.STDB,
          MCore = MP.Zarishat.conv.ST,
          MAge = MA.Zarishat$Age,
          Fit.val = 0.25,
          Only.fit = T,
          Ecartype.curve = c(T,T,T),
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800, 5200, 7800, 8000, 8700, 8900, 9695),
          Zone.Temp = c("W","C","W","C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat.csv",
          Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Zarishat_ST.pdf",
          H = 1300, W = 1700
        )
        }
      
      Analog.map.Zarishat.ST = F
      if(Analog.map.Zarishat.ST == T){
        Analog.map.Zarishat.ST <- Analogue.map(Model.WAPLS = MAT.ST,
                                             MAge = MA.Zarishat,
                                             MCore = MP.Zarishat,
                                             MCoord = DB.odile.Co.ST,
                                             Age.choice = c(-55, 1996, 3991, 6528, 8380, 9153),
                                             #Zone.plot = c("Mongolia"),
                                             Clim.choix = "MAAT",
                                             H = 400, W = 900,
                                             Save.path = "Resultats/Armenia/Pollen/Func_trans/Zarishat/Analogues/MAT_Zarishat_STDB.csv",
                                             Save.plot = "Figures/Armenia/Pollen/Func_trans/Zarishat/Anal_maps/AnalMap_Zarishat_STDB.pdf")
      }
    }}
  }

#### Italia ####
Italia = F
if(Italia == T){
  #### Italia mono sites ####
  Italia.mono.sites = F
  if(Italia.mono.sites == T){
    #### Import Data ####
    # Cores #
    Cores.import = T
    if(Cores.import == T){
      MP.Eufemia <- data.frame(read.csv(file="Import/Italia/Pollen/Cores/SEufemiacmPO.csv",sep=";",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
      MP.Matese <- data.frame(read.csv(file="Import/Italia/Pollen/Cores/MatesePO.csv",sep=";",dec=".",header=T,row.names=2, stringsAsFactors = FALSE))
      MA.Matese <- MP.Matese[1]
      names(MA.Matese)[1] <- "Age"
      MP.Matese <- MP.Matese[-c(1:4)]
      MP.Matese <- MP.Matese[-c(34)]
      MP.Matese <- MP.Matese[-c(95:ncol(MP.Matese))]
      MP.Matese <- MP.Matese/100
    }
    
    
    #### Preparation data cores ####
    MA.Eufemia <- MP.Eufemia[1]
    MP.Eufemia <- MP.Eufemia[-1]
    MP.Eufemia[is.na(MP.Eufemia)]<- 0
    MP.Eufemia <- MP.Eufemia[,sort(names(MP.Eufemia))]
    
    #### Check congruance surface / cores ####
    MP.Eufemia <- (MP.Eufemia/rowSums(MP.Eufemia))    # pourcentage pollen
    MP.Eufemia[MP.Eufemia <= 0.01] <- 0               # remove taxa < 1 %
    MP.Eufemia <- (MP.Eufemia/rowSums(MP.Eufemia)) 
    
    
    #### Climate reconstruction ####
    Eufemia.FT = F
    if(Eufemia.FT == T){
      #### EAPDB ####
      plot.Eufemia.EAPDB.ft = F
      if(plot.Eufemia.EAPDB.ft == T){
        MP.Eufemia <- Fossil.corresp.surface(MP.Eufemia, DB.odile.Po) 
        plot.Eufemia.EAPDB.ft <- FT.core(
          Model.WAPLS = WAPLS.EAPDB,
          Model.MAT = MAT.EAPDB,
          Model.RF = RF.EAPDB,
          Model.BRT = BRT.EAPDB,
          MCore = MP.Eufemia,
          MAge = MA.Eufemia$Age,
          Fit.val = 0.25,
          Ecartype.curve = c(T,T,T,F),
          Only.fit = T,
          Model.param.show = T,
          Zone.Clim.span = c(796, 2700, 3000, 4800),
          Zone.Temp = c("W","C"),
          Save.RDS = T,
          Save.path = "Resultats/Italia/Pollen/Func_trans/Eufemia/Eufemia.csv",
          Save.plot = "Figures/Italia/Pollen/Func_trans/Eufemia/Eufemia_EAPDB.pdf",
          H = 1300, W = 1700
        )}
      
      Analog.map.Eufemia.EAPDB = T
      if(Analog.map.Eufemia.EAPDB == T){
        Analog.map.Eufemia.EAPDB <- Analogue.map(Model.WAPLS = MAT.EAPDB,
                                                 MAge = MA.Eufemia,
                                                 MCore = MP.Eufemia,
                                                 MCoord = DB.odile.Co,
                                                 Age.choice = c(429, 1033, 2003, 3046, 4040, 5318),
                                                 Zone.plot = Map.Eurasia,
                                                 Clim.choix = "MAAT",
                                                 H = 350, W = 900,
                                                 Save.path = "Resultats/Italia/Pollen/Func_trans/Eufemia/Analogues/MAT_Eufemia_EAPDBDB.csv",
                                                 Save.plot = "Figures/Italia/Pollen/Func_trans/Eufemia/Anal_maps/AnalMap_Eufemia_EAPDBDB.pdf")
      }
      }
    
    Matese.FT = F
    if(Matese.FT == T){
      Corresp_name  <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/Corresp_name_full_V9.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # Correspondance des profondeurs/echantillons GAY
      
      MP.Matese.conv <- Fossil.MAT.prep(MP.Matese, MA.Matese, Type_MAT = "Type_Odile", Displot = T,  Corresp_name = Corresp_name)
      
      #### EAPDB ####
      Matese.EAPDB = F
      if(Matese.EAPDB == T){
        MP.Matese.conv.eapd <- Fossil.corresp.surface(MP.Matese.conv, DB.odile.Po)
        Matese.EAPDB <- FT.core(
          Model.WAPLS = WAPLS.EAPDB,
          # Model.MAT = MAT.EAPDB,
          # Model.RF = RF.EAPDB,
          # Model.BRT = BRT.EAPDB,
          MCore = MP.Matese.conv.eapd,
          MAge = MA.Matese$Age,
          Fit.val = 0.25,
          LakeName = "Matese",
          Only.fit = T, Displot = T,
          # Ecartype.curve = c(T, T, T, T),
          Model.param.show = T,
          # Zone.Clim.span = c(160, 420, 730, 1030),
          # Zone.Temp = c("C","W"),#,"C","W"),
          Select.clim = c("MAAT", "MAP"),
          Save.RDS = T,
          Save.path = "Resultats/Italia/Pollen/Func_trans/Matese/Matese.csv",
          Save.plot = "Figures/Italia/Pollen/Func_trans/Matese/Matese_EAPDB.pdf",
          H = 1100, W = 1900
        )}
      
      #### COSTDB ####
      Matese.COST = F
      if(Matese.COST == T){
        MP.Matese.conv.cost <- Fossil.corresp.surface(MP.Matese.conv, DB.odile.Po.COST)
        Matese.COSTDB <- FT.core(
          Model.WAPLS = WAPLS.COSTDB,
          Model.MAT = MAT.COSTDB,
          Model.RF = RF.COSTDB,
          Model.BRT = BRT.COSTDB,
          MCore = MP.Matese.conv.cost,
          MAge = MA.Matese$Age,
          Fit.val = 0.18,
          LakeName = "Matese",
          Select.clim = c("MAAT", "MAP", "MTCO", "MTWA"),
          Only.fit = T, Displot = T,
          Ecartype.curve = c(T, T, T, T),
          Model.param.show = T, Verbose = F,
          # Zone.Clim.span = c(420, 500),
          # Zone.Temp = c("C"),#"W"),#,"C","W"),
          Save.RDS = T,
          Save.path = "Resultats/Italia/Pollen/Func_trans/Matese/Matese.csv",
          Save.plot = "Figures/Italia/Pollen/Func_trans/Matese/Matese_COSTDB.pdf",
          H = 1100, W = 1900
        )
      }
      }
    
    Matese.merge.DB = F
    if(Matese.merge.DB == T){
      files.it  <- list.files(path = c("Import/Italia/Pollen/Func_trans/Fonction_transfer_Mary"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
      Biome.Matese <- data.frame(read.csv(file="Import/Italia/Pollen/Func_trans/Biome_Mary/Matese_biome.csv",sep=";",dec=".",header=T,row.names=1, stringsAsFactors = FALSE))
      Biome.Matese$Age <- as.numeric(gsub("M", "", row.names(Biome.Matese)))
      Lake.names.it <- gsub(".*\\/", "", files.it)
      Lake.names.it <- gsub("\\..*", "", Lake.names.it)
      Lake.names.it <- gsub("Matese_", "", Lake.names.it)
      it.pollen <- lapply(files.it, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ",", dec = ".")
      names(it.pollen) <- Lake.names.it
      
      for(i in 1:length(it.pollen)){
        it.pollen[[i]]$DB <- gsub(".*_", "", names(it.pollen)[i])
        it.pollen[[i]]$DB <- gsub("DB", "", it.pollen[[i]]$DB)
        it.pollen[[i]]$method <- gsub("_.*", "", names(it.pollen)[i])
      }
      
      Methodes = c("MAT", "WAPLS", "BRT", "RF")
      for(i in Methodes){
        it.pol.BRT <- do.call("rbind", it.pollen[grep(i, names(it.pollen))])
        it.pol.BRT <- it.pol.BRT[(it.pol.BRT$Age == Biome.Matese$Age & it.pol.BRT$DB == Biome.Matese$BIOME),]
        it.pol.BRT <- it.pol.BRT[order(it.pol.BRT$Age),]
        Export.data = paste("Resultats/Italia/Pollen/Func_trans/Func_trans_Mary/", i, "_Matese.csv", sep = "")
        write.table(it.pol.BRT, file = Export.data, col.names = NA)
      }
      
      
    }
    
  }
  #### Italia Marion ####
  Italia.Marion = F
  if(Italia.Marion == T){
    #### Import data #####
    files.it  <- list.files(path = c("Import/Italia/Pollen/Cores/Lake_italia_Marion"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
    Lake.names.it <- gsub(".*\\/", "", files.it)
    Lake.names.it <- gsub("\\..*", "", Lake.names.it)
    Lake.names.it <- gsub(".*\\) ", "", Lake.names.it)
    it.pollen <- lapply(files.it, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ";", dec = ",")
    names(it.pollen) <- Lake.names.it
    
    #### Calculations ####
    Total.FT.EAPDB <- function(x, Lake.name){
      print(x)
      Abscisse <- c("Age","Depth", "agebp")
      Age <- x[,names(x) %in% Abscisse]
      x[,names(x) %in% Abscisse] <- NA
      x <- x[, !names(x) %in% Abscisse]
      
      x <- data.frame(sapply(as.data.frame(x), as.double))
      x <- data.frame(x/rowSums(x))
      x <- Fossil.corresp.surface(x, DB.odile.Po)
      x <- data.frame(x/rowSums(x))
      print(x)
      # x <- FT.core(
      #   Model.WAPLS = WAPLS.EAPDB,
      #   Model.MAT = MAT.EAPDB,
      #   Model.RF = RF.EAPDB,
      #   Model.BRT = BRT.EAPDB,
      #   MCore = x,
      #   MAge = Age,
      #   Fit.val = 0.25,
      #   LakeName = Lake.name,
      #   Only.fit = T,
      #   Ecartype.curve = c(T, T, T, T),
      #   Model.param.show = T,
      #   # Zone.Clim.span = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4300),
      #   # Zone.Temp = c("C","W","C","W","C", "W"),
      #   Save.RDS = T,
      #   Save.path = paste("Resultats/Italia/Pollen/Func_trans/", Lake.name, "/", Lake.name, ".csv", sep = ""),
      #   Save.plot = paste("Figures/Italia/Pollen/Func_trans/", Lake.name, "/", Lake.name, "_EAPDB.pdf", sep = ""),
      #   H = 1100, W = 1900
      #   )
      return(x)
    }
    
    # it.pollen <- it.pollen[3:4]
    
    for(i in 1:length(names(it.pollen))){
      print(names(it.pollen)[i])
      Total.FT.EAPDB(it.pollen[[i]], Lake.name = names(it.pollen)[i])
    }
      
    }
  
  #### Italia Léa ####
  Italia.Lea = F
  if(Italia.Lea == T){
    #### Import data #####
    files.it  <- list.files(path = c("Import/Italia/Pollen/Cores/Lakes_Lea"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
    Lake.names.it <- gsub(".*\\/", "", files.it)
    Lake.names.it <- gsub("\\..*", "", Lake.names.it)
    Lake.names.it <- gsub(".*\\) ", "", Lake.names.it)
    it.pollen <- lapply(files.it, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ",", dec = ".")
    names(it.pollen) <- Lake.names.it
    
    #### Calcul FT EAPDB ####
    Med.EAPDB = F
    if(Med.EAPDB == T){
      for(i in 1:length(names(it.pollen))){
        Pouet <- Total.FT.EAPDB(it.pollen[[i]], 
                                Lake.name = names(it.pollen)[i],
                                Select.clim = c("MAAT", "MAP"),
                                Select.model = c("BRT", "WAPLS", "MAT"),
                                Select.calib = c("EAPDB"),
                                Save.path = "Resultats/Italia/Pollen/Func_trans/Func_trans_Lea/",
                                Save.plot = "Figures/Italia/Pollen/Func_trans/Func_trans_Lea/")
      }}
    
    #### Calcul FT MEDTEMP ####
    Med.Medtemp = F
    if(Med.Medtemp == T){
      for(i in 1:length(names(it.pollen))){
        Pouet <- Total.FT.EAPDB(it.pollen[[i]], 
                                Lake.name = names(it.pollen)[i],
                                Select.clim = c("MAAT", "MAP"),
                                Select.model = c("BRT", "WAPLS", "MAT"),
                                Select.calib = c("MEDTEMP"),
                                Save.path = "Resultats/Italia/Pollen/Func_trans/Func_trans_Lea/",
                                Save.plot = "Figures/Italia/Pollen/Func_trans/Func_trans_Lea/")
      }}
    
    #### Calcul FT TEMPSCAND ####
    Med.Tempscand = F
    if(Med.Tempscand == T){
      for(i in 1:length(names(it.pollen))){
        Pouet <- Total.FT.EAPDB(it.pollen[[i]], 
                                Lake.name = names(it.pollen)[i],
                                Select.clim = c("MAAT", "MAP"),
                                Select.model = c("BRT", "WAPLS", "MAT"),
                                Select.calib = c("TEMPSCAND"),
                                Save.path = "Resultats/Italia/Pollen/Func_trans/Func_trans_Lea/",
                                Save.plot = "Figures/Italia/Pollen/Func_trans/Func_trans_Lea/")
      }}
 
  }
}

#### Sud France #### 
Sud.France = F
if(Sud.France == T){
  #### Import ####
  files.fr  <- list.files(path = c("Import/France/Pollen/Cores"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
  Lake.names.fr <- gsub(".*\\/", "", files.fr)
  Lake.names.fr <- gsub("\\..*", "", Lake.names.fr)
  Lake.names.fr <- gsub(".*\\) ", "", Lake.names.fr)
  fr.pollen <- lapply(files.fr, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ",", dec = ".")
  names(fr.pollen) <- Lake.names.fr
  
  #### Calcul FT EAPDB ####
  Med.EAPDB = F
  if(Med.EAPDB == T){
    for(i in 1:length(names(fr.pollen))){
      Pouet <- Total.FT.EAPDB(fr.pollen[[i]], 
                              Lake.name = names(fr.pollen)[i],
                              Select.clim = c("MAAT", "MAP"),
                              Select.model = c("BRT", "WAPLS", "MAT"),
                              Select.calib = c("EAPDB"),
                              Save.path = "Resultats/France/Pollen/Func_trans/",
                              Save.plot = "Figures/France/Pollen/Func_trans/")
    }}
      
  #### Calcul FT MEDTEMP ####
  Med.Medtemp = F
  if(Med.Medtemp == T){
  fr.pollen <- fr.pollen[c(2)]
    for(i in 1:length(names(fr.pollen))){
      Pouet <- Total.FT.EAPDB(fr.pollen[[i]], 
                              Lake.name = names(fr.pollen)[i],
                              Select.clim = c("MAAT", "MAP"),
                              Select.model = c("BRT", "WAPLS", "MAT"),
                              Select.calib = c("MEDTEMP"),
                              Save.path = "Resultats/France/Pollen/Func_trans/",
                              Save.plot = "Figures/France/Pollen/Func_trans/")
    }}
      
  #### Calcul FT TEMPSCAND ####
  Med.Tempscand = F
  if(Med.Tempscand == T){
    for(i in 1:length(names(fr.pollen))){
      Pouet <- Total.FT.EAPDB(fr.pollen[[i]], 
                              Lake.name = names(fr.pollen)[i],
                              Select.clim = c("MAAT", "MAP"),
                              Select.model = c("BRT", "WAPLS", "MAT"),
                              Select.calib = c("TEMPSCAND"),
                              Save.path = "Resultats/France/Pollen/Func_trans/",
                              Save.plot = "Figures/France/Pollen/Func_trans/")
    }}
}
#### ACA collection ####
ACA.full = F
if(ACA.full == T){
  TopCores <- list(MP.Zoige.conv, MP.Wenquan.conv, MP.Tianchi_Liupan.conv, MP.Tianchi_Gaoligong.conv, MP.Kanas.conv, MP.Kakitu.conv, MP.Hurleg.convMP.Bayanchagan.conv, 
                   MP.Parishan.conv, MP.Gomishan.conv, MP.Maharlou.conv, MP.Baikal.conv, MP.Derput.conv, MP.Kotokel.conv, MP.Nuochaga.conv, MP.Suollakh.conv,
                   MP.Kichikol.conv, MP.Karakol.conv, MP.Kanli, MP.Zarishat, MP.Shenka, )
  
}
