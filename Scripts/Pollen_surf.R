#### Global path ####
#setwd("/media/lucas.dugerdil/Maximator/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/home/lucas.dugerdil/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/R_stats") 
setwd("/home/lucas.dugerdil/Documents/Recherche/R_stats") 

#### Library ####
library(dplyr)
library(tibble)

#### Functions ######
# Trace le diagramme de vegetation
Diag.pol.surf <- function(MP, Ordin.path, Name.zone, Y.legend, Index, Csv.sep, Taille.bar = 13,
                          Sort, Sort.taxon, Abiot.plot, Sort.eco, GDGT, My_Ecosystem, Calc.stat = F,
                          Save.path = NULL, Save.plot, H, W, Label.eco, TaxonLabel, Taxa.angle, Color.choice = NULL,
                          Manual.sort, Max_seuil, AP.NAP, ShP, CONISS, Nzone){
  #### Settings ####
  library("rioja")
  UnSort = F
  Coloring = T
  if(missing(Sort)){
    UnSort = T
    Sort = NULL}
  if(missing(Sort.eco)){Sort.eco = NULL}
  if(missing(TaxonLabel)){TaxonLabel = NULL}
  if(missing(Ordin.path)){Ordin.path = NULL}
  if(missing(Label.eco)){Label.eco = NULL}
  if(missing(Name.zone)){Name.zone = NULL}
  if(missing(Y.legend)){Y.legend = Sort}
  if(missing(AP.NAP)){AP.NAP = F}
  if(missing(CONISS)){CONISS = FALSE}
  if(missing(Nzone)){Nzone = FALSE}
  if(missing(Index)){AP.NAP = F
  Coloring = F
  }
  if(missing(AP.NAP)){AP.NAP = FALSE}
  if(missing(ShP)){ShP = FALSE}
  if(missing(Taxa.angle)){Taxa.angle = 45}
  if(missing(Max_seuil)){Max_seuil = 0.05}
  if(missing(Sort.taxon)){Sort.taxon = "None"}
  if(missing(My_Ecosystem)){My_Ecosystem = "Ecosystem"}
  if(missing(Abiot.plot)){Abiot.plot = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  
  #### Save plots ####
  if(is.null(Save.plot) == F){
    Path.to.create <- gsub("(.*/).*\\.pdf.*","\\1", Save.plot)
    dir.create(file.path(Path.to.create), showWarnings = FALSE)
    if(is.null(W) == F & is.null(H) == F){
      pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)}
    else{pdf(file = Save.plot)}}
  
  #### Index taxon ####
  if(Coloring == T){
    InfoPol  <- read.csv(file = Index, sep=",",dec=".", header = T, stringsAsFactors = F, row.names = 1, check.names = FALSE)
    if(names(InfoPol)[1] != "Index" & ncol(InfoPol) == 5){
      InfoPol  <- read.csv(file = Index, sep=",",dec=".", header = F, stringsAsFactors = F, check.names = F)
      names(InfoPol) <- c("Index", "Nom", "Type", "Label", "AP.NAP")
      InfoPol$Nom <- gsub(" ", "\\.", InfoPol$Nom)
    }
    if(ncol(InfoPol) > 6){
      if("AP.NAP" %in% names(InfoPol) == T){InfoPol <- InfoPol[c("Nom", "Type_Odile", "Label", "AP.NAP",)]}
      if("AP_NAP" %in% names(InfoPol) == T){InfoPol <- InfoPol[c("Nom", "Type_Odile", "Label", "AP_NAP")]}
      InfoPol <- cbind(seq(1:nrow(InfoPol)), InfoPol, NA)
      names(InfoPol) <- c("Index", "Nom", "Type", "Label", "AP.NAP", "Cortege")
      InfoPol$Nom <- gsub("\\.", " ", InfoPol$Nom)
      }
    
    
    if(length(grep("\\.", row.names(MP))) > 0){InfoPol$Nom <- gsub(" ", ".", InfoPol$Nom)}
    
    InfoPol  <- cbind(InfoPol, Couleur=rep(InfoPol$AP.NAP))
    InfoPol$Couleur<- gsub('NAP', '#f27041ff', InfoPol$Couleur)
    InfoPol$Couleur<- gsub('AP', '#6db38fff', InfoPol$Couleur)
    InfoPol$Couleur<- gsub('ShP', '#cfce90ff', InfoPol$Couleur)
    InfoPol$Couleur<- gsub('NaN', '#cfce90ff', InfoPol$Couleur)
    InfoPol$Couleur<- gsub('Algue', '#cfce90ff', InfoPol$Couleur)
    InfoPol$Couleur<- gsub('Sum', '#cfce90ff', InfoPol$Couleur)
    if("Variable" %in% unique(InfoPol$Couleur)){InfoPol$Couleur<- gsub('Variable', '#cfce90ff', InfoPol$Couleur)}
    # InfoPol$Couleur<- gsub('Other', 'grey60', InfoPol$Couleur)
    InfoPol <- rbind(InfoPol, c("Other", "Other", "p", "Other", NaN, NaN, "grey80"))
    
    # print(InfoPol)
    Tax.manquant.ds.index <- setdiff(row.names(MP), InfoPol$Nom)
    if(length(Tax.manquant.ds.index) > 0){print(paste("Les taxons suivants sont manquants dans l'index :", paste(Tax.manquant.ds.index, collapse = ", "), sep = " "))}
    }
  else{InfoPol <- data.frame()}
  #### Fusion taxons rares #### 
  MP_max <- MP[0]
  MP_max[, "max"] <- apply(MP[,], 1, max)
  MP_dom <- MP[rowSums(MP_max)>Max_seuil,]
  MP_rare <- MP[rowSums(MP_max)<=Max_seuil,]
  MP_dom["Other" ,] <- colSums(MP_rare)
  MP_dom <- data.frame(t(MP_dom))
  #### Sort Taxon ####
  Sort.col <- function(){
    InfoPol <- InfoPol[order(InfoPol$Couleur),]
    inter <- intersect(InfoPol$Nom, row.names(MP_dom))
    return(inter)}
  
  if(Sort.taxon == 'None'){print("Not sorted")}
  if(Sort.taxon == 'Auto'){
    MP_dom <- data.frame(t(MP_dom))
    MP_dom <- MP_dom[match(Sort.col(),row.names(MP_dom)),]
    MP_dom <- data.frame(t(MP_dom))
  }
  if(Sort.taxon == 'Manual'){
    if(missing(Manual.sort)){print("**ERROR** The sorting vector is not working. We applied an auto-sorting.")
      MP_dom <- data.frame(t(MP_dom))
      
      A = Sort.col()
      # print(A)
      MP_dom <- MP_dom[match(Sort.col(),row.names(MP_dom)),]
      MP_dom <- data.frame(t(MP_dom))
    }
    else{

      MP_dom <- MP[row.names(MP) %in% Manual.sort,]
      MP_rare <- MP[!row.names(MP) %in% Manual.sort,]
      MP_dom["Other",] <- colSums(MP_rare)
      # MP_dom <- data.frame(t(MP_dom))
            
      # MP_dom <- data.frame(t(MP_dom))
      MP_dom <- MP_dom[match(Manual.sort,row.names(MP_dom)),]
      MP_dom <- data.frame(t(MP_dom))
    }}
  
  #### Calcul AP et NAP  #### 
  if(AP.NAP == TRUE){
    row.names(MP) <- gsub(" ",".", row.names(MP))
    InfoPol$Nom <- gsub(" ",".", InfoPol$Nom)
    AP <- colSums(MP[intersect(row.names(MP),InfoPol$Nom[InfoPol$AP.NAP == "AP"]),])
    NAP <- colSums(MP[intersect(row.names(MP),InfoPol$Nom[InfoPol$AP.NAP == "NAP"]),])
    AP_nul <- sum(AP)
    if(ShP == T){
      MAS <- colSums(MP[intersect(row.names(MP),InfoPol$Nom[InfoPol$AP.NAP == "ShP"]),])
      }

    if(AP_nul != 0){
      MP_dom <- t(MP_dom)
      MP_dom <- rbind(AP=AP, NAP=NAP, MP_dom )
      if(ShP == T){MP_dom <- rbind(ShP=MAS, MP_dom )}
      MP_dom <- t(MP_dom)
      }
    row.names(MP) <- gsub("\\."," ", row.names(MP))
    InfoPol$Nom <- gsub("\\."," ", InfoPol$Nom)
    if(!"AP" %in% InfoPol$Nom){
      print("*** We automatically had AP and NAP in the color matrix. ***")
      InfoPol <- rbind(InfoPol, c(NA, "NAP", "p", "NAP", NaN, NaN, "#f27041ff"))
      InfoPol <- rbind(InfoPol, c(NA, "AP", "p", "AP", NaN, NaN, "#6db38fff"))
      }
    InfoPol <- rbind(InfoPol, c(NA, "ShP", "p", "ShP", NaN, NaN, "#cfce90ff"))
  }  

  #### Sort sites ####
  if(is.null(Ordin.path) == F){
    Ordin <- read.table(Ordin.path, sep = Csv.sep, check.names = F, row.names = 1, header = T, na.strings = c("","NA"), stringsAsFactors = F)
    MP.plot.data <- data.frame(t(MP))
    Inter <- intersect(row.names(Ordin), row.names(MP.plot.data))
    Manque.ds.ordin <- setdiff(row.names(MP.plot.data), row.names(Ordin))
    if(length(Manque.ds.ordin) > 0){print(paste("Le site", Manque.ds.ordin, "est manquant dans le fichier d'ordination."))}
    Ordin <- Ordin[which(row.names(Ordin) %in% Inter),]                                        # on enleve les taxons absents
    
    if(UnSort == T){
      if("Ordination" %in% names(Ordin)){Sort = "Ordination"}
      if("Ordin1" %in% names(Ordin)){Sort = "Ordin1"}
      if("Ordin.clust" %in% names(Ordin)){Sort = "clust.ordin"}
      Ordin <- Ordin[order(Ordin[[Sort]]),]
      }
    if(UnSort == F){
      if(Sort %in% names(Ordin)){Ordin <- Ordin[order(Ordin[[Sort]]),]}
      else{print("*** The SORT parameters is not in the ordination matrix. ***")}
      }
    
    Triage = Ordin[[Sort]]
    New.name = row.names(Ordin)
    Sort.data.plot <- gsub("\\.ordin", "", Sort)
  }
  if(is.null(Ordin.path) == T){
    Triage <- 1: length(MP)
    New.name = row.names(MP_dom)
    Sort = "Ordination"
  }
  #### Graphic settings ####
  # if(Sort == "Ordination" | Sort == "Eco.ordination"){Taille.bar = 13}
  # else{Taille.bar = 13}
  Ytk.lab = seq(min(Triage), max(Triage), 1)

  if(is.null(TaxonLabel) == T){TaxonLabel<- gsub("\\.", " ", colnames(MP_dom))}
  else{
    if(length(TaxonLabel) == 1){
      Lab.to.fin <- gsub("\\.", " ", colnames(MP_dom))
      TaxonLabel <- InfoPol[match(Lab.to.fin, InfoPol$Nom), TaxonLabel]
    }
  }
  
  if(is.null(Abiot.plot) == F){
    x1 = 0.23
    x2 = 0.95
    x3 = 0.07
    if(is.null(Name.zone) == F){T2 = paste("Surface pollen diagram of", Name.zone)}
    else{T2 = NULL}
    T1 = NULL
  }
  else{
    x1 = 0.07
    x2 = 0.95
    if(is.null(Name.zone) == F){T1 = paste("Surface pollen diagram of", Name.zone)}
    else{T1 = NULL}
  }

  #### Color settings ####
  if(Coloring == T){
    InfoPol$Nom <- gsub(" ",".", InfoPol$Nom)
    C <- subset(InfoPol, select=c(Couleur))
    row.names(C)<- InfoPol$Nom
    MP_dom.t <- data.frame(t(MP_dom))
    MP_color <- cbind(MP_dom.t[0], C[rownames(MP_dom.t),])
    couleur = as.character(MP_color[[1]])
  }
  else{couleur = "darkgrey"}

  #### Color ecosystems ####
  if(is.null(Sort.eco) == F){
    Fact.eco <- as.factor(Ordin[[My_Ecosystem]])
    num_colors <- nlevels(Fact.eco)
    if(is.null(Color.choice) == F){diamond_color_colors <- Color.choice}
    else{
      colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
      diamond_color_colors <- colfunc(num_colors)
      }
    Fact.eco <- ordered(Fact.eco, levels = Sort.eco)
    Col.eco <- diamond_color_colors[Fact.eco]
    Pt.eco = 19
    }
  else{
    Pt.eco = NULL
    Col.eco = 1}

  #### Export data ####
  MP_dom <- MP_dom[match(New.name,row.names(MP_dom)),]
  MP_dom <- MP_dom*100

  #### CONISS Clustering ####
  if(CONISS == TRUE){
    #### CONISS calcul ####
    MP_coniss <- MP_dom
    diss <- dist(sqrt(MP_coniss/100)^2)
    Clust_DP <- chclust(diss, method = "coniss")
    
    #### Stat extraction and mise en forme ####
    if(Calc.stat == T){
      cc <- cutree(Clust_DP, k = Nzone)
      Tab.stat <- data.frame(t(rbind(CONISS = cc, data.frame(t(MP_coniss)))))
      Tab.stat <- aggregate(Tab.stat, list(Tab.stat[["CONISS"]]), FUN = mean, na.action = na.omit)
      Tab.stat <- Tab.stat[-c(1)]
      row.names(Tab.stat) <- paste("U", Tab.stat$CONISS, sep = "_")
      Tab.stat <- data.frame(t(round(Tab.stat[-c(1)], digits = 2)))
      
      Seuil = 5
      Resume <- c()
      for(i in 1:ncol(Tab.stat)){
        NN = row.names(Tab.stat[Tab.stat[i] > Seuil,])
        VV = Tab.stat[Tab.stat[i] > Seuil,i]
        NN = NN[order(VV, decreasing = T)]
        VV = VV[order(VV, decreasing = T)]
        JJ = paste(NN, " (", VV, " %)", sep = '', collapse = ', ')
        Resume[i] <- JJ
      }
      Tab.stat <- data.frame(t(Tab.stat))
      Tab.stat <- cbind(Tab.stat, Resume)
      print(Resume)
      
      if(is.null(Save.path) == F){
        Save.path.div <- gsub("\\.csv", "_CONISS_units.csv", Save.path)
        write.table(cc, file=Save.path.div, row.names=T, col.names=NA, sep=",", dec = ".")
        
        Save.path.div <- gsub("\\.csv", "_CONISS_stats.csv", Save.path)
        write.table(Tab.stat, file=Save.path.div, row.names=T, col.names=NA, sep=",", dec = ".")
        
        }
    }
    else{Tab.stat <- NULL}
    
  }
  else{Clust_DP = NULL}

  #### Plot ####
  # print(Triage)
  p <- strat.plot(MP_dom,
                  Triage,
                  y.rev = TRUE,
                  scale.percent = TRUE,     # True pour des pourcentages
                  scale.minmax = TRUE,
                  srt.xlabel = Taxa.angle,           # Rotation de 45 des noms de taxon
                  x.names = TaxonLabel,      # Nom taxon sans point
                  title = T1,
                  xSpace = 0.007,
                  mgp = c(0, 0.5, 0.3),
                  #ylabel = Y.legend,
                  y.tks = 1000, # hide the ticks
                  plot.poly     = F,
                  plot.bar      = T,
                  plot.line     = F,
                  lwd.bar       = Taille.bar,
                  col.poly      = couleur,         # bleu clair
                  col.bar       = couleur,            # Fait apparaitre les traits continus
                  col.poly.line = "#8c92a1ff",         # gris fonce
                  exag = T,                # Fait apparaitre la zone x10
                  col.exag = "auto",           # Zone x10 couleur auto
                  clust = Clust_DP,
                  xLeft = x1,
                  xRight= x2,
                  #orig.fig = c(0.006,1,0,1),
                  clust.width=0.05
  )
  #### Plot ORDIN ####
  if(is.null(Abiot.plot) == F){
    Sorting.data <- subset(Ordin, select = Abiot.plot)
    Lab.unit <- c("MAP (mm.yr-1)", "MAAT (°C)", "Altitude (m.a.s.l)")
    p2 <- strat.plot(Sorting.data,
                    Triage,
                    y.rev = TRUE,
                    scale.percent = F,     # True pour des pourcentages
                    scale.minmax = F,
                    srt.xlabel = Taxa.angle, 
                    x.names = Lab.unit,      # Nom taxon sans point
                    title = T2,
                    xSpace = 0.005,
                    mgp = c(0, 0.5, 0.3),
                    y.tks = 1000, # hide the ticks
                    # ylabel = Y.legend,
                    # y.tks = Ytk.lab,
                    # y.tks = names(Sorting.data),
                  
                    plot.poly     = T,
                    plot.bar      = F,
                    plot.line     = T,
                    lwd.bar       = 1,
                    col.poly      = c("lightblue", "lightcoral", "wheat3"),         # bleu clair
                    col.bar       = "royalblue",            # Fait apparaitre les traits continus
                    col.poly.line = "darkgrey",         # gris fonce
                    exag = F,                # Fait apparaitre la zone x10
                    #col.exag = "auto",           # Zone x10 couleur auto
                    #clust = Clust_DP,
                    xLeft = x3,
                    xRight= x1,
                    #orig.fig = c(0.006,1,0,1),
                    add = T
    )}
  #### Etiquette sites ####
  Table.sites <- matrix(row.names(MP_dom))
  Table.sites <- gsub("p", "\\'", Table.sites)
  par(fig=c(0, 1, 0, 1), mar=c(0,0,0,0), mgp=c(0, 1, 0), new = T)
  plot.new()
  # Haut = 0.84
  # Bas = 0.01
  Haut = 0.835
  Bas = -0.24
  Size.word <- (Haut - Bas)/length(Table.sites)
  legend(x= c(-0.027,.065), y=c(Bas, Haut),
         Table.sites,
         y.intersp = Size.word*66.5,
         pch = Pt.eco,
         col = Col.eco,
         bty = "n",
         pt.cex = 1.4,
         cex = .7)

  #### CONISS plotting ####
  if(CONISS == TRUE & Nzone > 0){
    Ligne <- addClustZone(p, Clust_DP, Nzone,col="grey25", lwd = 1.4, lty = 2)
    #print(p$figs)
    if(is.null(Abiot.plot) == F){addClustZone(p2, Clust_DP, Nzone,col="grey25", lwd = 1.4, lty = 2)}}

  #### Etiquette Ecosystems ####
  if(is.null(Label.eco) == F){
    Table.eco<- matrix(row.names(MP_dom))
    Table.eco <- gsub("p", "\\'", Table.eco)
    Table.eco <- Label.eco
    par(fig=c(0.95, 1, 0, 1), mar=c(.5,0,.5,.5), mgp=c(0, 1, 0), new = T)
    # par(fig = c(0.95, 1, 0, 1), mar=c(1,0,1,1), mgp=c(0, 1, 0), new = T)
    plot.new()
    Haut = .9
    Bas = 0.046
    Size.word <- (Haut - Bas)/length(Table.eco)
    legend(x= c(-0.35,.065), y=c(Bas, Haut), Table.eco,
           y.intersp = Size.word*70,
           bty = "n",
           cex = .6, x.intersp = 3
           )
  }
  #### Return data ####
  if(is.null(Save.plot) == F){dev.off()}
  return(MP_dom)
}

PCA.pollen.surf <- function(MP, transp_OK, Site.name, Type.samples, Short.name, GDGT, Annot, Nb.contrib = NULL,
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
    row.names(MP) <- gsub(".type","",row.names(MP))
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
  taxa.score <- scores(MP.pca, choices=c(1,2), display = "species", scaling = Scale.PCA*1.2)
  
  par(mgp = c(1.7,0.6,0), mar=c(3,3,2,0.3)+0.1)
  
  PC1.MP <- round(MP.pca[["CA"]][["eig"]][["PC1"]]/MP.pca[["tot.chi"]]*100, digits = 0)
  PC2.MP <- round(MP.pca[["CA"]][["eig"]][["PC2"]]/MP.pca[["tot.chi"]]*100, digits = 0)
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
  
  #### Vector plot ####
  biplot(MP.pca,                                              # ajout des vecteurs taxa
         # scaling = Scale.PCA,
         display = "species",                                  # 'species', vecteurs taxa
         # display = "site",                                  # 'species', vecteurs taxa
         type = "n",                                           # 't', txt aux vecteurs, 'n', vecteurs vides
         # type = "t",                                           # 't', txt aux vecteurs, 'n', vecteurs vides
         cex = 2,
         main = MTitle,
         #sub = paste("n = ", nrow(MP)),
         xlim = c(xmin,xmax),
         ylim = c(ymin,xmax),
         xlab = bquote(PCA[1] ~ "(" ~ .(format(PC1.MP, digits = 2)) ~ "%)"),
         ylab = bquote(PCA[2] ~ "(" ~ .(format(PC2.MP, digits = 2)) ~ "% )")
         # xlab = paste("PC1 (",format(PC1.MP, digits=4),"% )"),
         # ylab = paste("PC2 (",format(PC2.MP, digits=4),"% )")
  )
  
  #### Color points setting ####
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
    else{
      colfunc    <- colorRampPalette(c("firebrick3", "darkorange", "goldenrod1", "#38A700", "darkgreen", "dodgerblue3", "grey10"))
      diamond_color_colors <- colfunc(num_colors)
    }
    Col.eco <- diamond_color_colors[Fact.eco]
  }
  else{Col.eco = 1}
  points(site.score, pch = 19, cex = 1.5, col = Col.eco)
  # arrows(0,0,x1 = taxa.score[,1], y1 = taxa.score[,2], length = 0.05, lty = 1, col="firebrick2")
  # text(taxa.score, colnames(MP), cex=.7, pos = 3, col = "firebrick2")                # textes taxa
  text(MP.pca, scaling = Scale.PCA, display = "species", cex = .95, col = "#6e2115ff")
  arrows(0,0, x1 = taxa.score[,1], y1 = taxa.score[,2], length = 0.05, lty = 1, col="#6e2115ff")
  if(Title.inside == T){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.01, 0.95, Annot, adj = 0, cex = 1.7)  # if that's what you want
    par(usr = usr) # restore original user coordinates
  }  
  #### Add plots ####
  if(Short.name == T){Tsite <- sub("M","",row.names(MP))}
  else{Tsite <- row.names(MP)}
  if(Show.text == T){text(site.score, Tsite, cex=.6, pos= 3)}  # textes sites
  
  #### Legend #####
  if(Clustering == T & Display.legends == T){
    legend(Leg.loc,
           legend = levels(Fact.eco),
           col = diamond_color_colors,
           pch = 19, cex = 0.9,
           y.intersp = 0.85,	          # espace entre y
           x.intersp = 0.6,           # espace entre x
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
    pic <- readPNG(Symbol.path)
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
# Param necessaire : MP, MClim
# Param option : transp_OK, Site.name, Csv.sep, Scale.PCA
RDA.pollen.surf <- function(MP, MClim, Choose.clim, Cluster.path, Type.samples, Sort.eco, Color.choice = NULL,
                           Cluster.groups, Display.legends, transp_OK, Manu.lim, GDGT, Annot, Vector.show = NULL, Nb.contrib = NULL, Leg.size = 1,
                           Remove.7Me, Leg.loc, Leg.loc2, Simple.title, Show.text, Symbol.loc, Symbol.path,
                           Site.name, Csv.sep, Scale.taxa, Scale.sites, Save.path, Title.inside = F){
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
  if(Show.text == T){text(PClim.sc.site, sub("M","", row.names(MP)), cex=.5, pos = 3)}            # textes sites
  arrows(0,0,x1 = PClim.sc[,1], y1 = PClim.sc[,2], length = 0.05, lty = 1, col="#6e2115ff")
  points(PClim.sc.site, pch = 19, cex = 1.5, col = Col.eco)
  arrows(0,0,x1 = clim.score[,1], y1 = clim.score[,2], length = 0.08, lty = 1, lwd = 2, col="#1c4871ff")
  text(PClim.sc, colnames(MP), cex=.95, pos = 3, col = "#6e2115ff")                # textes taxa
  text(clim.score, row.names(clim.score), cex=1.3, pos = 3, col = "#1c4871ff")      # textes clim
  if(Title.inside == T){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.01, 0.95, Annot, adj = 0, cex = 1.7)  # if that's what you want
    par(usr = usr) # restore original user coordinates
  }  
  
  #### Legend #####
  if(Clustering == T & Display.legends == T){
    legend(Leg.loc,
           legend = levels(Fact.eco),
           col = diamond_color_colors,
           pch = 19, cex = 0.9,
           y.intersp = 0.85,	          # espace entre y
           x.intersp = 0.6,           # espace entre x
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
# Calculation of ordination
# A FINIR
# L idee est de ne faire plus qu un fichier qui fusionne Surface_samples et Ordination
Ordin.bioclim <- function(Mclim, Meco, Select.param, Save.path){
  #### Settings ####
  if(missing(Save.path)){Save.path = NULL}
  if(missing(Select.param)){Select.param = colnames(Mclim)[1]}
  if(missing(Meco)){Meco = NULL}
  if(missing(Mclim)){Mclim = NULL}

  #### Clim ordination ####
  if(is.null(Mclim) == F){
    In.clim <- intersect(Select.param, names(Mclim))
    Mclim <- subset(Mclim, select = In.clim)
    for(i in names(Mclim)){
      Mclim <- Mclim[order(Mclim[[i]]),]
      Mclim[[i]] <- 1:nrow(Mclim)
    }
    names(Mclim) <- paste(names(Mclim), ".ordin", sep = "")
    Mclim <- Mclim[order(row.names(Mclim)),]

  }

  #### Eco ordination ####
  if(is.null(Meco) == F){
    In.eco <- intersect(Select.param, names(Meco))
    Meco <- subset(Meco, select = In.eco)
    for(i in names(Meco)){
      Meco <- Meco[order(Meco[[i]]),]
      Meco[[i]] <- 1:nrow(Meco)
    }
    names(Meco) <- paste(names(Meco), ".ordin", sep = "")
    Meco <- Meco[order(row.names(Meco)),]
  }

  M <- cbind(Meco, Mclim)

  print(M)

}

LR.ratio <- function(MP, TabRa, Meco = NULL, Keep.clim = F,
                     Strip.lab = F, R2.pos = "bottomleft", Smooth.method = "lm",
                     H = 400, W = 500, Save.plot = NULL){
  
  #### Settings ####
  if(length(unique(Keep.clim %in% names(Meco))) == 1){Meco <- Meco[, Keep.clim]}
  else{
    "One climate parameter is not existing."
    Keep.clim = NULL
    }
  
  if(is.null(Keep.clim) == T){
    Keep.clim = c("MAP", "MAAT")
    print("Only the MAAT and MAP have been displayed. Select ***Keep.clim*** if you want.")}
  
  #### Calcul ratio ####
  MR <- MP[0]
  for(i in 1:length(TabRa)){
    TaxonDeno <- c(as.character(unique(TabRa[[i]]$Deno)))
    ColDeno <- subset(MP_cal, select = TaxonDeno)
    TaxonNomi <- c(as.character(unique(TabRa[[i]]$Nomi)))
    ColNomi <- subset(MP_cal, select = TaxonNomi)
    MR[i] <- rowSums(ColDeno)/rowSums(ColNomi)
    InitLDeno <- paste(substring(TaxonDeno, 1, 2), collapse = "+")
    InitLNomi <- paste(substring(TaxonNomi, 1, 2), collapse = "+")
    colnames(MR)[i] <- paste(InitLDeno,InitLNomi,sep = "/")
  }
  MR[MR == Inf] <- NA
  
  MR <- merge(MR, Meco, by = 0)
  row.names(MR) <- MR$Row.names
  MR <- MR[-1]
  MR <- melt(MR, Keep.clim)
  names(MR)[c(ncol(MR)-1, ncol(MR))] <- c("Ratio.lab", "Ratio.val")
  MR <- melt(MR, c("Ratio.lab", "Ratio.val"))
  names(MR)[c(ncol(MR)-1, ncol(MR))] <- c("Clim.lab", "Clim.val")
  
  #### Graphical settings ####
  values.bi = c("Azerbaijan" = "#98312eff",
                "WAST" = "#98312eff",
                "Mongolia" =  "royalblue",
                "COST" =  "royalblue",
                "Tajikistan" = "#3fa587ff",
                "Uzbekistan" = "#84761cff"
  )
  
  values.bi <- values.bi[which(names(values.bi) %in% unique(MR$Country))]
  Scale.fill <- scale_color_manual(values = values.bi, name = "Country", drop = T)
  
  #### Add R2 ####
  if(length(R2.pos) == 1){
    if(R2.pos == "bottomleft"){
      R2.y = "bottom"
      R2.x = "left"} 
    if(R2.pos == "topright"){
      R2.y = "top"
      R2.x = "right"}
    if(R2.pos == "bottomright"){
      R2.y = "bottom"
      R2.x = "right"}
    if(R2.pos == "topleft"){
      R2.y = "top"
      R2.x = "left"}
    if(R2.pos == "none"){
      R2.y = "none"
      R2.x = "none"}}
  else{
    R2.x = R2.pos[0]
    R2.y = R2.pos[0]
    for(i in 1:length(R2.pos)){
      if(R2.pos[i] == 1){
        R2.x[i] = "left"
        R2.y[i] = "top"}
      if(R2.pos[i] == 2){
        R2.x[i] = "right"
        R2.y[i] = "top"}
      if(R2.pos[i] == 3){
        R2.x[i] = "left"
        R2.y[i] = "bottom"}
      if(R2.pos[i] == 4){
        R2.x[i] = "right"
        R2.y[i] = "bottom"}
    }
  }
  
  Add.r2 <- stat_poly_eq(label.y = R2.y,
                         label.x = R2.x, method = Smooth.method,
                         size = 2.4, small.r = F, vstep = 0.07, p.digits = 1, na.rm = T,
                         aes(label =  sprintf("%s*\", \"*%s" ,
                                              after_stat(rr.label),
                                              # after_stat(r.squared),
                                              after_stat(p.value.label)
                         )))
  
  
  #### Annotations names Strig.lab = F ####
  if(Strip.lab == F){
    Strip.lab.disp <- element_blank()
    S.trait <- setNames(data.frame(as.factor(unique(MR$Ratio.lab)), rep(1,nlevels(MR$Ratio.lab)), rep(1,nlevels(MR$Ratio.lab))), c("Lab", "x","y"))
    S.clim <- setNames(data.frame(as.factor(unique(MR$Clim.lab)), rep(1,nlevels(MR$Clim.lab)), rep(1,nlevels(MR$Clim.lab))), c("Lab", "x","y"))
    
    Theme.null <- theme(axis.line = element_blank(), axis.title = element_blank(),
                        strip.text = element_blank(), axis.text = element_blank(),plot.margin = unit(c(0,0,0,0), 'cm'),
                        axis.ticks = element_blank(), plot.background = element_blank(),
                        panel.grid = element_blank(), panel.background = element_blank())
    
    p.up <- ggplot(S.trait, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", ncol = length(unique(MR$Ratio.lab)))+
      geom_text(aes(label = Lab))+ Theme.null
    
    p.right <- ggplot(S.clim, mapping = aes(x = x, y = y))+
      facet_wrap(vars(Lab), scales = "free_x", nrow = length(unique(MR$Ratio.lab)))+
      geom_text(aes(label = Lab), angle = 270,  hjust=0.5, vjust=1)+ Theme.null
  }
  else{Strip.lab.disp <- element_text(hjust = 0)}
  
  #### Plot ####
  # pLR <- ggplot(MR, aes(x = Clim.val, y = Ratio.val, group = Clim.lab, label = Ratio.lab))+
  pLR <- ggplot(MR, aes(x = Clim.val, y = Ratio.val))+
    geom_point(size = 1.5, alpha = 0.5, shape = 16)+
    geom_smooth(method = Smooth.method, se = F, span = 1000, size = 0.7, linetype = "dashed",
                formula = y ~ x)+
    Add.r2 + Scale.fill +
    xlab("Plant fractional abundances (%)")+
    ylab("Climate parameters")+
    facet_wrap(Clim.lab ~ Ratio.lab, scales = "free")+
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
  
  ggsave(pLR, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm")
  return(MR)
  
}

Pol.Ratio <- function(MP = NULL, Select.ratio = NULL) {
  #### Choix des ratios #####
  TabRa <- list(data.frame(Deno = c("Poaceae"), Nomi = c("Artemisia")),                  # A/Ch El-Moslimany, (1990) -> humidite
                data.frame(Deno = c("Poaceae"), Nomi = c("Amaranthaceae")),                          # A/Cy Herzschuh et al. (2006) -> Tsum
                data.frame(Deno = c("Artemisia"), Nomi = c("Amaranthaceae")),                        # A/Cy Herzschuh et al. (2006) -> Tsum
                data.frame(Deno = c("Amaranthaceae"), Nomi = c("Artemisia")),                        # A/Cy Herzschuh et al. (2006) -> Tsum
                data.frame(Deno = c("Artemisia","Amaranthaceae"), Nomi = c("Poaceae")),              # A+Ch/Po Fowell et al. (2003) -> humidite, R>5 desert
                data.frame(Deno = c("AP"), Nomi = c("NAP")),                                         # A/Cy Herzschuh et al. (2006) -> Tsum
                data.frame(Deno = c("Artemisia","Amaranthaceae"), Nomi = c("Poaceae", "Thalictrum")),
                data.frame(Deno = c("Artemisia", "Artemisia", "Amaranthaceae"), Nomi = c("Poaceae", "Thalictrum", "Cyperaceae")),
                data.frame(Deno = c("Artemisia"), Nomi = c("Cyperaceae"))
  )
  names(TabRa) <- c("Po/Ar", "Po/Am", "Ar/Am", "Am/Ar", "Ar/Am+Ar","AP/NAP", "Ar+Am/Po.Tha", "Ar+Am/Po+Tha+Cy", "Ar/Cy")
  if(is.null(Select.ratio) == T){
    print("*** You can choose pollen ratio among: ***")
    print(names(TabRa))}
  else{TabRa <- TabRa[Select.ratio]}
  
  #### Calcul ratio ####
  if(is.null(MP) == T){
    print("*** Please add a pollen matrix with pollen as columns, samples as row. ***")
    return(NULL)}
  else{
    MR <- MP[0]
    for(i in 1:length(TabRa)){
      TaxonDeno <- c(as.character(unique(TabRa[[i]]$Deno)))
      ColDeno <- subset(MP, select = TaxonDeno)
      TaxonNomi <- c(as.character(unique(TabRa[[i]]$Nomi)))
      ColNomi <- subset(MP, select = TaxonNomi)
      MR[i] <- rowSums(ColDeno)/rowSums(ColNomi)
      InitLDeno <- paste(substring(TaxonDeno, 1, 3), collapse = "_")
      InitLNomi <- paste(substring(TaxonNomi, 1, 3), collapse = "_")
      colnames(MR)[i] <- paste(InitLDeno,InitLNomi,sep = "/")
    }
    
    MR <- merge(MR, Meco, by = 0)
    row.names(MR) <- MR$Row.names
    MR <- MR[-1]
    return(MR)}
}

#### Mongolia ####
Mong = F
if(Mong == T){
  #### Import table ####
  MPS_mongPol <- data.frame(read.csv(file="Resultats/Mongolia/Pollen/Surface/Mongolia_surfP1_MPfrac.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  Mclim       <- data.frame(read.csv(file="Import/Mongolia/Site/Surface_samples_climat.csv",sep=",",dec=".",header=T,row.names=1))
  Meco        <- data.frame(read.csv(file="Import/Mongolia/Site/Surface_samples.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag


  # Ordin.bioclim(Meco = Meco, Mclim = Mclim, Select.param = c("Altitude", "MAAT", "MAP", "Sample.type", "Vegetation.community"),
  #               Save.path = "Import/Mongolia/Site/Surface_samples_test.csv"
  #               )

  #### Poaceae
  MPS_mongPol <- data.frame(t(MPS_mongPol))
  MPS_mongPol$Poaceae <- MPS_mongPol$Poaceae + MPS_mongPol$Phragmites
  MPS_mongPol <- subset(MPS_mongPol, select = - Phragmites)

  ### Artemisia
  MPS_mongPol$Artemisia <- MPS_mongPol$Artemisia + MPS_mongPol$Artemisia.gros.t
  MPS_mongPol <- subset(MPS_mongPol, select = - Artemisia.gros.t)

  ### Trifolium
  names(MPS_mongPol)[grep("Trifol", names(MPS_mongPol))][1] <- "Trifolium.Indet"

  ### Pinus
  # MPS_mongPol[["Pinus.sp"]] = MPS_mongPol$Pinus.indet + MPS_mongPol$Pinus.sibirica + MPS_mongPol$Pinus.sylvestris #+ MPS_mongPol$Picea
  MPS_mongPol[["Pinus.sylvestris"]] = MPS_mongPol$Pinus.indet + MPS_mongPol$Pinus.sylvestris
  MPS_mongPol <- subset(MPS_mongPol, select=-c(Pinus.indet))

  ### Pourc check

  MPS_mongPol <- data.frame(t(MPS_mongPol))
  # row.names(MPS_mongPol)[nrow(MPS_mongPol)] <- "Pinus.indet"

  #### Clim, eco subset ####
  Site.commun <- intersect(names(MPS_mongPol), row.names(Meco))
  Site.non.commun <- c(setdiff(row.names(Meco), names(MPS_mongPol)), setdiff(names(MPS_mongPol), row.names(Meco)))
  Meco.retro <- Meco[Site.non.commun,]
  Meco <- Meco[Site.commun,]
  Mclim <- Mclim[Site.commun,]
  Mtype <- subset(Meco, select = c(Sample.type))
  Sort.eco.mong = c("Desert", "Steppe-desert", "Steppe", "Alpine meadow", "Steppe-forest", "Light taiga", "Dark taiga")

  #### Export clean data ####
  Clean.names.pollen <- data.frame(read.csv(file="Import/Mongolia/Pollen/Func_trans/Corresp_pollen_MAT_Mongolia.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  A <- gsub("\\.", " ", row.names(MPS_mongPol))
  A <- cbind(Pollen_type = Clean.names.pollen$Label[match(A, Clean.names.pollen$Nom)], MPS_mongPol)
  A <- aggregate(x = A[,-1], by = list(A$Pollen_type), FUN = sum)
  row.names(A) <- A$Group.1
  A <- A[-1]
  write.csv(A, "Resultats/Mongolia/Export_pangaea/Pollen_pourcentage_NMSDB.csv")

  #### DP ####
  DP.surf = T
  if(DP.surf == T){
    row.names(MPS_mongPol) <- gsub("\\.", " ", row.names(MPS_mongPol))
    Plot.Mong.pollen.surf <- Diag.pol.surf(MP = MPS_mongPol,
                             Ordin.path = "Import/Mongolia/Pollen/Indexes/Ordination_pol_surf.csv",
                             Index = "Import/Mongolia/Pollen/Indexes/Index_pollen_Mongolia.csv",
                             Name.zone = "Mongolia - Bioclimate ordinated",
                             Y.legend = "Sample sites",
                             Sort.taxon = "Manual", # Auto, Manual
                             Manual.sort = c("Pinus.sylvestris", "Pinus.sibirica", "Betula","Alnus", "Picea", "Abies",  "Larix", "Salix",
                                             "Artemisia", "Poaceae",
                                             "Cyperaceae", "Brassicaceae", "Convolvulus",
                                             "Rumex",  "Amaranthaceae", "Caryophyllaceae","Thalictrum",
                                             "Other"),
                             TaxonLabel = c("AP", "NAP", "Pinus sylvestris", "Pinus sibirica", "Betula sp.", "Alnus sp.", "Picea obovata", "Abies sibirica",
                             "Larix sibirica", "Salix sp.", "Artemisia sp.", "Poaceae", "Cyperaceae", "Brassicaceae", "Convolvulus-type",
                             "Rumex sp.", "Amaranthaceae", "Caryophyllaceae", "Thalictrum sp.", "Other taxa"),
                             Csv.sep = ",",
                             Max_seuil = 0.03,
                             AP.NAP = T,
                             CONISS = T,
                             Abiot.plot = c("MAP", "MAAT", "Altitude"), # Fonction à faire ! Transform MAAT en MAAT.ordin (1, 2, 3... et pas 1.0° 2.3° 3.4°...)
                             Sort = "Bioclim.ordin",  # Altitude, MAP.ordin, MAAT, Latitude, Longitude, Ordination
                             Nzone = 7,
                             Label.eco = c("Light taiga", "Dark taiga-","birch subtaiga", "Forest-steppe", "Steppe", "Alpine meadow", "Steppe-desert", "Desert"),
                             Sort.eco = Sort.eco.mong,
                             H = 1000, W = 1900,
                             Save.plot = "Figures/Mongolia/Pollen/Surface/DPsurf_Mong_P2.pdf")

    Plot.Mong.pollen.surf <- subset(Plot.Mong.pollen.surf, select = -c(AP,NAP))
    }
  #### PCA ####
  PCA.pollen = T
  if(PCA.pollen == T){
    #### Graphical settings ####
    W = 1100
    H = 550
    Save.plot = "Figures/Mongolia/Pollen/Surface/PCA_Mong_P1.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,2))

    #### PCA ####
    PCA.pollen.Mong <- PCA.pollen.surf(data.frame(t(Plot.Mong.pollen.surf)),               # Import Matrice pour la PCA
                             Cluster.path = Meco,
                             Cluster.groups = "Vegetation.community",
                             Csv.sep =",", Simple.title = T,
                             transp_OK = T,                         # Log trans (T), (F) sinon
                             Scale.PCA = 1,                         # 1 or 2
                             Save.path = "Resultats/Mongolia/Pollen/Surface/MP_mong.csv",
                             #Site.name = "Mongolia surface sample",
                             Sort.eco = Sort.eco.mong,
                             Type.samples = "Pollen"
                             )     # Nom du site
    #### RDA ####
    RDA.Mong.P1 <- RDA.pollen.surf(data.frame(t(Plot.Mong.pollen.surf)),               # Import Matrice pour la PCA
                             MClim = Mclim,
                             Choose.clim = c("MAAT", "MAP", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                             Cluster.path = Meco,
                             Cluster.groups = "Vegetation.community",
                             Display.legends = F, Simple.title = T,
                             Csv.sep =",",
                             transp_OK = T,                         # Log trans (T), (F) sinon
                             Scale.sites = 2,                         # 1 or 2
                             Scale.taxa = 1,                         # 1 or 2
                             Manu.lim = c(-2.2,2.2,-2.2,2.2),
                             Save.path = "Resultats/Mongolia/Pollen/Surface/MP_mong.csv",
                             Type.samples = "Pollen",
                             Sort.eco = Sort.eco.mong,
                             #Site.name = "Mongolia surface sample"
                             )     # Nom du site

    dev.off()
  }
  #### Pollen ratio test ####
  Pollen.Ratio.Test = F
  if(Pollen.Ratio.Test == T){
    ##### Choix des ratios #####
    TabRa <- list(#data.frame(Deno = c("Artemisia"), Nomi = c("Amaranthaceae")),                                     # A/Ch El-Moslimany, (1990) -> humidite
      data.frame(Deno = c("Artemisia"), Nomi = c("Cyperaceae")),                                          # A/Cy Herzschuh et al. (2006) -> Tsum
      data.frame(Deno = c("Artemisia"), Nomi = c("Amaranthaceae","Artemisia")),
      #data.frame(Deno = c("Artemisia","Amaranthaceae"), Nomi = c("Poaceae")),                           # A+Ch/Po Fowell et al. (2003) -> humidite, R>5 desert
      #data.frame(Deno = c("Artemisia","Amaranthaceae"), Nomi = c("Poaceae", "Thalictrum")),
      data.frame(Deno = c("Artemisia", "Artemisia", "Amaranthaceae"), Nomi = c("Poaceae", "Thalictrum", "Cyperaceae"))
      )

    #### Graphical settings ####
    W = 900
    H = 330
    Save.plot = "Figures/Mongolia/Pollen/Surface/PolRatio_climate_CV.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,3))


    #### Calcul ratio ####
    MP_cal <- data.frame(Plot.Mong.pollen.surf)
    MR <- MP_cal[0]
    for(i in 1:length(TabRa)){
      TaxonDeno <- c(as.character(unique(TabRa[[i]]$Deno)))
      ColDeno <- subset(MP_cal, select = TaxonDeno)
      TaxonNomi <- c(as.character(unique(TabRa[[i]]$Nomi)))
      ColNomi <- subset(MP_cal, select = TaxonNomi)
      MR[i] <- rowSums(ColDeno)/rowSums(ColNomi)
      InitLDeno <- paste(substring(TaxonDeno, 1, 2), collapse = "+")
      InitLNomi <- paste(substring(TaxonNomi, 1, 2), collapse = "+")
      colnames(MR)[i] <- paste(InitLDeno,InitLNomi,sep = "/")
      }


    MR <- merge(MR, Mclim, by = 0)
    row.names(MR) <- MR$Row.names
    MR <- MR[-1]

    plot(MR$`Ar/Am+Ar`, MR$Tspr, xlab = "Ar/Am+Ar", ylab = expression(T[spring]~ ("\u00B0C")), main = "(A)")
    reg1 <- lm(Tspr ~ `Ar/Am+Ar`, data = MR)
    abline(reg1)
    Sum1<- summary(reg1)

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
           cex = 1,
           legend = rp)

    plot(MR$`Ar+Am/Po+Th+Cy`, MR$Pspr, xlab = "Ar+Am/Po+Th+Cy", ylab = expression(P[spring] ~ (mm)), main = "(B)")

    reg1 <- lm(Pspr ~ `Ar+Am/Po+Th+Cy`, data = MR)
    abline(reg1)
    Sum1<- summary(reg1)

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
           cex = 1,
           legend = rp)

    plot(MR$`Ar/Cy`, MR$MAP, xlab = "Ar/Cy", ylab = expression(MAP ~ (mm.yr^-1)), main = "(C)")
    MR[is.infinite(MR$`Ar/Cy`),"Ar/Cy"] <- 0
    reg1 <- lm(MAP ~ `Ar/Cy`, data = MR)
    abline(reg1)
    Sum1<- summary(reg1)

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
           cex = 1,
           legend = rp)
    # library(corrplot)
    # M = cor(MR)
    # CP <- corrplot(M, type = "lower", order = "hclust", low = MR[1:3], upp = MR[3:10],
    #                # col = colorRampPalette(c("royalblue","white", "darkorange"))(10),
    #                tl.col="black", tl.srt=45, tl.cex = .5, method = "number", number.cex = 0.2,
    #                # method = Disp.R,
    #                sig.level = 0.95, insig = "blank", pch.cex = 2)
    # title(main = Title, sub = Subt)
    # Mfull <- list(R2 = Mcor, Var = Mvar, p.val = PV)
    # print(CP)

    dev.off()



}}

#### Uzbekistan ####
Uz.pol = F
if(Uz.pol == T){
  #### Import table ####
  MPuz  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Surface/MP_pour.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  MPuz.AlC  <- read.csv(file="Import/Uzbekistan/Pollen/Surface/Malg_conc.csv",sep=",",dec=".", header = T, stringsAsFactors = T, row.names = 1, check.names = FALSE)
  MPuz.AlF  <- read.csv(file="Import/Uzbekistan/Pollen/Surface/Malg_pour_alg.csv",sep=",",dec=".", header = T, stringsAsFactors = T, row.names = 1, check.names = FALSE)
  MPuz.C    <- read.csv(file="Import/Uzbekistan/Pollen/Surface/MPol_conc.csv",sep=",",dec=".", header = T, stringsAsFactors = T, row.names = 1, check.names = FALSE)
  MPuz.NPPF <- read.csv(file="Import/Uzbekistan/Pollen/Surface/MNPP_pour.csv",sep=",",dec=".", header = T, stringsAsFactors = T, row.names = 1, check.names = FALSE)
  MPuz.PoC <- MPuz.C[which(!row.names(MPuz.C) %in% row.names(MPuz.NPPF)),]
  MPuz.NPPC <- MPuz.C[match(row.names(MPuz.NPPF),row.names(MPuz.C)),]
  Pol.Sum.MPuz <- colSums(MPuz.PoC)
  
  Meco.all       <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_V2.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
  Main.taxa       <- data.frame(read.csv(file = "Resultats/Uzbekistan/Vegetation/Main_taxa_uz.csv", sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
  Uz.TaxaCorresp  <- data.frame(read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Corresp_pollen_UZ.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  
  #### Clim, eco subset ####
  Clean.clim = T
  if(Clean.clim == T){
    Meco.all$Main.vegetation[which(row.names(Meco.all) %in% row.names(Main.taxa))] <- Main.taxa$x[na.omit(match(row.names(Meco.all), row.names(Main.taxa)))]
    
    Meco <- data.frame(read.csv(file="Import/Uzbekistan/Site/Uz_surf_samples.csv",sep=",",dec=".",header=T,row.names=1))
    Meco <- Meco[c(1:4)]
    Mbiom.all       <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_biom.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
    Mclim.all       <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_clim.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
    
    names(Mbiom.all)[3] <- "Ecozone"
    Meco <- left_join(rownames_to_column(Meco), rownames_to_column(Mbiom.all), by = ("rowname"))
    Meco$Geography <- NA
    Meco <- Meco[c(1,6,7,2,3,8,13,4,5,9,10)]
    Meco <- left_join(Meco, rownames_to_column(Mclim.all[-c(1,2)]), by = ("rowname"))
    Meco <- left_join(Meco, rownames_to_column(Meco.all[c("Lake")]), by = ("rowname"))
    row.names(Meco) <- Meco$rowname
    Meco <- Meco[-c(1)]
    
    Site.commun <- intersect(names(MPuz), row.names(Meco))
    Site.non.commun <- c(setdiff(row.names(Meco), names(MPuz)), setdiff(names(MPuz), row.names(Meco)))
    Meco.retro <- Meco[Site.non.commun,]
    Meco <- Meco[Site.commun,]
    write.table(Meco, "Resultats/Uzbekistan/Pollen/Surface/Uz_surf_samples_raw.csv", row.names=T, col.names=NA, sep=",", dec = ".")
  }
  
  
  
  Sort.eco.Uz = c("Chol cold desert-steppes", "Tugai riparian forest", "Chol warm deserts", "Adyr desert-steppes", 
                  "Adyr steppes", "Tau riparian forest", "Tau thermophilous woodlands", 
                  "Tau juniper steppe-forest", "Tau steppes", "Alau cryophilous steppe-forest", "Alau meadows")
  Uz.col = c("#7916C4", "#BB0268", "#bb0202", "#ff5400", 
             "#e6c607", "#2C9740", "#85682D", 
             "#176E5B", "#bab133", "#54a697", "#197CDA")
  
  Sort.eco.Uz.2 = c("Cold desert", "Temperate desert", "Steppes","Forest","Mountains")
  Uz.col.2 = c("#7916c4ff", "#bb0202ff", "#e6c607ff", "#2c9740ff", "#197cdaff")
  
  #### Add other pollen surf from Uz, Tajk ####
  Add.other.SS = T
  if(Add.other.SS == T){
    print("Add data from Odile DB and Suzanne Leroy")
    #### Keep only data in Tajik-Uzbek area ####
    Crop.TUDB = F
    if(Crop.TUDB == T){
      #### Import shape file ####
      if(exists("TUDB.bo") == F){
        library(rgdal)
        Path.TUDB.border = "/media/lucas.dugerdil/Extreme SSD/Documents/Recherche/SIG/Projets/ACA/Borders_ACA/Tadj_uz/Tadj_uz_bo.shp"
        TUDB.bo = readOGR(Path.TUDB.border)
        proj4string(TUDB.bo) <- CRS("+init=epsg:4326")
        # proj4string(TUDB.bo) <- st_crs(4326)
      }
      
      #### Suzanne crop ####
      Leroy.coord <- data.frame(read.csv("Import/World_DB/Pollen/Leroy_DB/Leroy_metadata.csv"))
      # Leroy.coord.TU <- SpatialPointsDataFrame(coords = Leroy.coord[,c(7,6)], data = Leroy.coord, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      Leroy.coord.TU <- SpatialPointsDataFrame(coords = Leroy.coord[,c(7,6)], data = Leroy.coord, proj4string = CRS("+init=epsg:4326"))
      Leroy.coord.TU <- Leroy.coord.TU[TUDB.bo,]
      print("here")
      Leroy.coord.TU <- data.frame(Leroy.coord.TU)
      row.names(Leroy.coord.TU) <- paste("MUZSL", seq(1: nrow(Leroy.coord.TU)), sep ="")
      Leroy.coord.TU <- Leroy.coord.TU[c(7,6,8,11,2,12)]
      Leroy.coord.TU$Ref <- "Suzanne Leroy"
      Leroy.coord.TU$sampling.date <- as.numeric(gsub(".*/", "",Leroy.coord.TU$sampling.date))
      names(Leroy.coord.TU) <- c("Longitude", "Latitude", "Altitude", "Sample.type", "Label", "Date", "Palynologues")
      
      saveRDS(Leroy.coord.TU, "Resultats/Uzbekistan/Pollen/Surface/Leroy_TUDB.Rds")
      write.csv(Leroy.coord.TU, "Resultats/Uzbekistan/Pollen/Surface/Leroy_TUDB.csv")
      
      #### Odile crop ####
      DB.Odile.Co <- data.frame(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3373/ss3373co.csv", sep=";", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)) # DB Odile, world
      DB.Odile.Co.TU <- SpatialPointsDataFrame(coords = DB.Odile.Co[,c(1,2)], data = DB.Odile.Co, proj4string = CRS("+init=epsg:4326"))
      DB.Odile.Co.TU <- DB.Odile.Co.TU[TUDB.bo,]
      DB.Odile.Co.TU <- data.frame(DB.Odile.Co.TU)
      
      
      DB.Odile.Co.TU <- DB.Odile.Co.TU[c(1:3)]
      DB.Odile.Co.TU$Sample.type <- "Unknown"
      DB.Odile.Co.TU$Label <- row.names(DB.Odile.Co.TU)
      row.names(DB.Odile.Co.TU) <- paste("MUZOP", seq(1: nrow(DB.Odile.Co.TU)), sep ="")
      DB.Odile.Co.TU$Date <- NA
      DB.Odile.Co.TU$Ref <- "Odile Peyron"
      names(DB.Odile.Co.TU) <- c("Longitude", "Latitude", "Altitude", "Sample.type", "Label", "Date", "Palynologues")
      DB.Odile.Co.TU <- DB.Odile.Co.TU[row.names(DB.Odile.Co.TU) != "MUZOP1",]
      saveRDS(DB.Odile.Co.TU, "Resultats/Uzbekistan/Pollen/Surface/DB_odile_TUDB.Rds")
      write.csv(DB.Odile.Co.TU, "Resultats/Uzbekistan/Pollen/Surface/DB_odile_TUDB.csv")
      
    }
    else{
      Leroy.coord.TU <- readRDS("Resultats/Uzbekistan/Pollen/Surface/Leroy_TUDB.Rds")
      DB.Odile.Co.TU <- readRDS("Resultats/Uzbekistan/Pollen/Surface/DB_odile_TUDB.Rds")
    }
    TUDB.add.coord <- rbind(Leroy.coord.TU, DB.Odile.Co.TU)
    
    #### Clim merge & extract for the new data ####  
    Extract.clim.ACA = F
    if(Extract.clim.ACA == T){
      source("Scripts/Climat_extract.R")
      TUDB.add.Clim <- Clim.param.extraction(M = TUDB.add.coord[c(1,2)], Season = T, Seasonality = T,
                                             All.param = T, Chelsa = F,
                                             Altitude = T, Aridity = T)
      
      
      TUDB.add.Biom <- Clim.param.extraction(M = TUDB.add.coord[c(1,2)], Clim.cal = F,
                                             All.param = F, Aridity = F, Altitude = F, Season = F, Biome = T, Map.display = F, Clim.display = F, 
                                             Land.cover = F)
      
      TUDB.add.eco <- cbind(TUDB.add.Biom, TUDB.add.Clim[c(3:ncol(TUDB.add.Clim))])
      names(TUDB.add.eco)[names(TUDB.add.eco) == "Ecosystem"] <- "Ecozone"
      saveRDS(TUDB.add.eco, "Resultats/Uzbekistan/Pollen/Surface/TUDB_add_eco.Rds")
      }
    else{
      TUDB.add.eco <- readRDS("Resultats/Uzbekistan/Pollen/Surface/TUDB_add_eco.Rds")
      }
    
    #### Merge old + new data TUDB ECO ####
    TUDB.add.eco <- cbind(TUDB.add.eco, TUDB.add.coord[c(5,7)])
    names(TUDB.add.eco)[names(TUDB.add.eco) == "Label"] <- "Lake"
    Meco$Sites <- row.names(Meco)
    Meco$Palynologues <- "Lucas Dugerdil"
    TUDB.add.eco$Sites <- row.names(TUDB.add.eco)
    TUDB.add.eco$Lake <- gsub(" ", ".", TUDB.add.eco$Lake)
    TUDB.add.eco$Lake <- gsub("-", ".", TUDB.add.eco$Lake)
    Meco <- suppressMessages(dplyr::full_join(Meco, TUDB.add.eco, )) 
    row.names(Meco) <- Meco$Sites
    Meco <- subset(Meco, select = -c(Sites))
    Meco$Biome[which(Meco$Biome == "N/A")] <- "Montane Grasslands & Shrublands"
    
    TUDB.add.type <- TUDB.add.coord[c(1,2,4:7)]
    names(TUDB.add.type)[names(TUDB.add.type) == "Label"] <- "Lake"
    TUDB.add.type$Pollen <- "x"
    TUDB.add.type$GDGT <- ""
    TUDB.add.type$Veg.plot <- ""
    TUDB.add.type$Country <- "Uzbekistan"
    Meco.all$Sites <- row.names(Meco.all)
    Meco.all$Palynologues <- "Lucas Dugerdil"
    TUDB.add.type$Sites <- row.names(TUDB.add.type)
    Meco.all <- suppressMessages(dplyr::full_join(Meco.all, TUDB.add.type))
    row.names(Meco.all) <- Meco.all$Sites
    Meco.all <- subset(Meco.all, select = -c(Sites, GDGT))
    
    Meco.all <- Meco.all[Meco.all$Country %in% c("Uzbekistan", "Tajikistan"),]
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    #### Merge old + Peyron TUDB Pollen ####
    DB.odile.Po <- data.frame(t(read.csv(file="Import/World_DB/Pollen/Odile_DB/DB3373/ss3373po.csv", sep=";", dec=".",header=T, row.names = 1, stringsAsFactors = FALSE)))
    DB.odile.Po.Uz <- DB.odile.Po[, names(DB.odile.Po) %in% TUDB.add.eco$Lake]
    names(DB.odile.Po.Uz) <- row.names(TUDB.add.eco[match(names(DB.odile.Po.Uz), TUDB.add.eco$Lake),])
    DB.odile.Po.Uz <- DB.odile.Po.Uz[rowSums(DB.odile.Po.Uz) != 0,]
    DB.odile.Po.Uz <- DB.odile.Po.Uz/100
    Corresp_name <- read.csv(file="Import/Uzbekistan/Pollen/Func_trans/Peyron_TUDB_correspTab_clean.csv", sep=",",dec=".",header=T, stringsAsFactors = F)
    row.names(DB.odile.Po.Uz) <- Corresp_name$Taxa_Lucas[match(row.names(DB.odile.Po.Uz), Corresp_name$Taxa_Odile)] 
    # row.names(DB.odile.Po.Uz) <- gsub(" ", ".", row.names(DB.odile.Po.Uz))
    row.names(DB.odile.Po.Uz) <- row.names(DB.odile.Po.Uz)
    
    #### Merge old + Leroy TUDB Pollen ####
    # Clean sites
    Leroy.MP <- data.frame(read.csv("Import/World_DB/Pollen/Leroy_DB/Leroy_DB_pol.csv", sep = "\t", dec = ",", header = T, row.names = 1, stringsAsFactors = F))
    Leroy.MP.Uz <- Leroy.MP[c(1:243), names(Leroy.MP) %in% TUDB.add.eco$Lake]
    names(Leroy.MP.Uz) <- row.names(TUDB.add.eco[match(names(Leroy.MP.Uz), TUDB.add.eco$Lake),])
    Taxa.names <- row.names(Leroy.MP.Uz)
    Leroy.MP.Uz <- data.frame(lapply(Leroy.MP.Uz, as.numeric))
    row.names(Leroy.MP.Uz) <- Taxa.names
    Leroy.MP.Uz[is.na(Leroy.MP.Uz)] <- 0
    Leroy.MP.Uz <- Leroy.MP.Uz[rowSums(Leroy.MP.Uz) != 0,]
    
    # Clean taxa
    Leroy.TaxaCorresp <- data.frame(Taxa_Suzanne = row.names(Leroy.MP.Uz))
    Leroy.TaxaCorresp <- cbind(Leroy.TaxaCorresp, Uz.TaxaCorresp[match(Leroy.TaxaCorresp$Taxa_Suzanne, row.names(Uz.TaxaCorresp)),])
    write.table(Leroy.TaxaCorresp, "Resultats/Uzbekistan/Pollen/Surface/Leroy_TUDB_correspTab_row.csv")
    Leroy.TaxaCorresp <- data.frame(read.csv("Import/Uzbekistan/Pollen/Func_trans/Leroy_TUDB_correspTab_clean.csv", sep = ",", dec = ".", header = T))
    Leroy.MP.Uz <- Leroy.MP.Uz[!row.names(Leroy.MP.Uz) %in% Leroy.TaxaCorresp$Taxa_Suzanne[Leroy.TaxaCorresp$Type == "Aquatic"],]
    
    # Aggregate synonyms
    Taxa.names <- Leroy.TaxaCorresp$Taxa_Lucas[match(row.names(Leroy.MP.Uz), Leroy.TaxaCorresp$Taxa_Suzanne)]
    Leroy.MP.Uz <- cbind(Species = Taxa.names, Leroy.MP.Uz)
    Leroy.MP.Uz <- aggregate(Leroy.MP.Uz[-1], by = list(Leroy.MP.Uz[["Species"]]), sum)
    Taxa.names <- Leroy.MP.Uz$Group.1 
    Leroy.MP.Uz <- Leroy.MP.Uz[-c(1)]
    
    # Pourcentage
    Leroy.MP.Uz <- data.frame(t(Leroy.MP.Uz))
    Leroy.MP.Uz <- Leroy.MP.Uz / rowSums(Leroy.MP.Uz)
    Leroy.MP.Uz <- data.frame(t(Leroy.MP.Uz))
    row.names(Leroy.MP.Uz) <- Taxa.names
    # row.names(Leroy.MP.Uz) <- gsub(" ", ".", Taxa.names)
    
    # Merge with MPuz
    library(dplyr)
    library(tibble)
    # print(row.names(MPuz))
    # print(row.names(Leroy.MP.Uz))
    K = full_join(rownames_to_column(Leroy.MP.Uz), rownames_to_column(MPuz), by = ("rowname" = "rowname"))
    K = full_join(K, rownames_to_column(DB.odile.Po.Uz), by = ("rowname" = "rowname"))
    row.names(K) <- K$rowname
    K <- K[-c(1)]
    K[is.na(K)] <- 0
    K <- K[rowSums(K) != 0,]
    
    
    #### Export ####
    MPuz <- K
    saveRDS(Meco.all, "Resultats/Uzbekistan/Pollen/Surface/TUSD_all_types.Rds")
    saveRDS(Meco, "Resultats/Uzbekistan/Pollen/Surface/TUSD_all_eco.Rds")
    write.table(Meco.all, "Resultats/Uzbekistan/Pollen/Surface/TUSD_all_types.csv", row.names=T, col.names=NA, sep=",", dec = ".")
    write.table(Meco, "Resultats/Uzbekistan/Pollen/Surface/TUSD_all_eco.csv", row.names=T, col.names=NA, sep=",", dec = ".")
    Meco <- data.frame(read.csv("Import/Uzbekistan/Site/TUSD_surf_samples.csv", sep = ",", dec = ".", header = T, row.names = 1))
    
    #### Export PANGEAE ####
    Meco.PANGAEA <- suppressMessages(dplyr::full_join(rownames_to_column(Meco.all[-c(8)]), rownames_to_column(Meco[-c(1,2,56,57,58)]), by = ("rowname" = "rowname")))
    row.names(Meco.PANGAEA) <- Meco.PANGAEA$rowname
    # print(names(Meco.PANGAEA))
    Meco.PANGAEA <- Meco.PANGAEA[c(5,18,20,7,8,66,4,2,12,13,11)]
    Meco.PANGAEA <- Meco.PANGAEA[Meco.PANGAEA$Proxy != "",]
    # Meco.PANGAEA$Geography[is.na(Meco.PANGAEA$Geography) & Meco.PANGAEA$Latitude > 44.3] <- "Aralkum"
    # Meco.PANGAEA$Geography[is.na(Meco.PANGAEA$Geography) & Meco.PANGAEA$Latitude < 44.3] <- "Karakalpakstan"
    Meco.PANGAEA <- Meco.PANGAEA[order(Meco.PANGAEA$Country, Meco.PANGAEA$Geography, Meco.PANGAEA$Ecosystem),]
    # Sort.geograph <- c("Aralkum", "Karakalpakstan", "Kyzilkum", )
    saveRDS(Meco.PANGAEA, "Resultats/Uzbekistan/Export_pangaea/Sites_metadata_TUSD.Rds")
    write.table(Meco.PANGAEA, "Resultats/Uzbekistan/Export_pangaea/Sites_metadata_TUSD.csv", row.names=T, col.names=NA, sep=",", dec = ".")
  }
  
  #### Export clean data ####
  MPuz <- data.frame(t(MPuz))
  MPuz$Thymelaceae <- MPuz$Thymelaceae + MPuz$Strigosella.t # Thymelaceae
  MPuz$Tamarix <- MPuz$Tamarix + MPuz$Zygophyllum
  MPuz <- subset(MPuz, select = - c(Strigosella.t, Zygophyllum))
  MPuz <- data.frame(t(MPuz)) # Pourc check
  row.names(MPuz) <- gsub("\\.", " ", row.names(MPuz))
  
  Uz.MP_ss <- cbind(PT.ss = Uz.TaxaCorresp$PT_ss.label[match(row.names(MPuz), row.names(Uz.TaxaCorresp))], MPuz)
  Uz.MP_ss <- aggregate(Uz.MP_ss[-1], by = list(Uz.MP_ss[["PT.ss"]]), sum)
  row.names(Uz.MP_ss) <- Uz.MP_ss$Group.1
  Uz.MP_ss <- Uz.MP_ss[-1]
  
  Uz.MP_sl <- cbind(PT.sl = Uz.TaxaCorresp$PT_sl.label[match(row.names(MPuz), row.names(Uz.TaxaCorresp))], MPuz)
  Uz.MP_sl <- aggregate(Uz.MP_sl[-1], by = list(Uz.MP_sl[["PT.sl"]]), sum)
  row.names(Uz.MP_sl) <- Uz.MP_sl$Group.1
  Uz.MP_sl <- Uz.MP_sl[-1]

  KRN <- row.names(Uz.MP_sl)
  Uz.MP_sl <- data.frame(t(Uz.MP_sl))
  Uz.MP_sl <- Uz.MP_sl/rowSums(Uz.MP_sl)*100
  Uz.MP_sl <- data.frame(t(Uz.MP_sl))
  row.names(Uz.MP_sl) <- KRN

  KRN <- row.names(Uz.MP_ss)
  Uz.MP_ss <- data.frame(t(Uz.MP_ss))
  Uz.MP_ss <- Uz.MP_ss/rowSums(Uz.MP_ss)*100
  Uz.MP_ss <- data.frame(t(Uz.MP_ss))
  row.names(Uz.MP_ss) <- KRN
  
  MPuz <- data.frame(t(MPuz))
  MPuz$Chenopodiaceae <- MPuz$Chenopodiaceae + MPuz$Haloxylon.t
  MPuz$Apiaceae <- MPuz$Apiaceae + MPuz$Ferula.sp # Apiaceae
  MPuz$Asteroideae <- MPuz$Asteroideae + MPuz$Aster.t + MPuz$Senecio.t + MPuz$Matricaria.t # Asteroideae
  MPuz$Plantago <- MPuz$Plantago.coronopus.t + MPuz$Plantago.alpina.t + MPuz$Plantago.lanceolata + MPuz$Plantago.major + MPuz$Plantago.media # Plantago
  MPuz <- subset(MPuz, select = - c(Aster.t, Senecio.t, Matricaria.t, Haloxylon.t, Ferula.sp,
                                    Plantago.coronopus.t, Plantago.alpina.t,
                                    Plantago.lanceolata, Plantago.major, Plantago.media))
  MPuz <- data.frame(t(MPuz)) # Pourc check
  write.csv(MPuz, "Resultats/Uzbekistan/Pollen/Surface/MP_Uz_clean.csv")
  write.csv(Uz.MP_sl, "Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_coarse.csv")
  write.csv(Uz.MP_ss, "Resultats/Uzbekistan/Export_pangaea/Pollen_pourcentage_TUSD_PT_fine.csv")
  
  #### Verif metadata list samples ####
  Check.metada = F
  if(Check.metada == T){
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$GDGT == "x"] <- paste(Meco.all$Proxy[Meco.all$GDGT == "x"], "G", sep = "")
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    List.samp.pollen.T <- names(MPuz)
    List.samp.pollen.expected <- row.names(Meco.all[Meco.all$Pollen == "x",])
    
    if(length(setdiff(List.samp.pollen.expected, List.samp.pollen.T)) >= 1){
      print("The following samples are missing from the pollen matrix :")
      print(setdiff(List.samp.pollen.expected, List.samp.pollen.T))}
    
    if(length(setdiff(List.samp.pollen.T,List.samp.pollen.expected)) >= 1){
      print("The following samples are missing from the metadata :")    
      print(sort(setdiff(List.samp.pollen.T,List.samp.pollen.expected)))}
    }
  
  #### Stats Uz surface ####
  Stats.show = F
  if(Stats.show == T){
    print(paste("Nombre échantillons Uz-surf:", ncol(MPuz)))
    print(paste("Nombre type-polliniques Uz-surf:", nrow(MPuz)))
    print(paste("Nombre algues Uz-surf:", nrow(MPuz.AlC)))
    print(paste("Nombre fungal spores Uz-surf:", nrow(MPuz.NPPC)))
    print(paste("Taxon dominant Uz-surf (valeur moyenne sur toute la carotte):"))
    print(sort(round(rowMeans(MPuz), digits = 2), decreasing = T)[1:10])
  }
  
  #### Cluster aveugle test ####
  Cluster.aveugle = T
  if(Cluster.aveugle == T){
    library(plotly)
    library(ggplot2)
    library(ggdendro)

    MPuz.st = t(MPuz)
    MPuz.dd <- dist(scale(MPuz.st), method = "euclidean")

    # MPuz.st = decostand(t(MPuz),"normalize")
    # MPuz.dd <- dist(MPuz.st)

    hc <- hclust(MPuz.dd, method = "ward.D")
    Clust.test <- setNames(data.frame(hc$order), "Ordin.clust")
    row.names(Clust.test) <- names(MPuz)#[hc$order]
    # print(Clust.test)
    
    Meco <- merge(Meco, Clust.test, by="row.names",all.x=TRUE)
    row.names(Meco) <- Meco$Row.names
    Meco <- Meco[-c(1)]
    # print(Meco)
    write.csv(Meco, "Resultats/Uzbekistan/Pollen/Surface/Clust_MP_ordin.csv")

    # p <- ggdendrogram(hc, rotate = T, size = 2)
    # ggplotly(p)
  }
  #### DP ####
  DP.surf = F
  if(DP.surf == T){
    Plot.Uz.pollen.surf <- Diag.pol.surf(MP = MPuz,
                                           # Ordin.path = "Import/Uzbekistan/Site/Uz_surf_samples.csv",
                                           # Ordin.path = "Import/Uzbekistan/Site/archives/Uz_surf_samples_V1.csv",
                                           # Ordin.path = "Resultats/Uzbekistan/Pollen/Surface/Uz_surf_samples_raw.csv",
                                           # Ordin.path = "Resultats/Uzbekistan/Pollen/Surface/",
                                           Ordin.path = "Import/Uzbekistan/Site/TUSD_surf_samples.csv",
                                           Index = "Import/Uzbekistan/Pollen/Func_trans/Bibli_pollen_ACA_clean.csv",
                                           Y.legend = "Sample sites", Sort.taxon = "Manual", Sort = "Ordin2",
                                           Manual.sort = c("Juniperus", "Betula",
                                                           "Ephedra.distachia", "Potentilla.t", "Ranunculaceae", "Lamiaceae",
                                                           "Rosaceae.shrub", "Fraxinus","Salix", "Juglans", "Apiaceae",
                                                           "Polygonum.t", "Plantago", "Cichorioideae",  "Cousinia.sp", "Asteroideae",
                                                           "Cyperaceae", "Poaceae", "Artemisia", "Cardueae",
                                                           "Fabaceae", "Brassicaceae", "Caryophyllaceae", "Chenopodiaceae",
                                                           "Calligonum.t",  "Ephedra.fragilis", "Convolvulus", "Tamarix", "Elaeagnus", #"Zygophyllum",
                                                           "Other"),
                                           TaxonLabel = "Type", Csv.sep = ",", Color.choice = Uz.col,
                                           AP.NAP = T,  ShP = T, CONISS = T, Taille.bar = 6, Calc.stat = T,
                                           Abiot.plot = c("MAP", "MAAT", "Altitude"),
                                           Nzone = 9, #Max_seuil = 0.00,
                                           Label.eco = c("Cold \n desert", "Tugai", "Desert", "Desert \n -steppe", "Steppe", "Riparian \n forest \n -anthropic", "Forest \n -steppe",  "Alpine \n steppe"),
                                           Sort.eco = Sort.eco.Uz, H = 1300, W = 3000,
                                           Save.path = "Resultats/Uzbekistan/Pollen/Surface/TUSD.csv",
                                           Save.plot = "Figures/Uzbekistan/Pollen/Surface/DPsurf_uz.pdf")
    
    # Plot.Uz.pollen.surf <- subset(Plot.Uz.pollen.surf, select = -c(Other, Elaeagnus, Artemisia, Caryophyllaceae,
    #                                                                Fraxinus, Potentilla.t, Apiaceae, Ranunculaceae))
    # Plot.Uz.pollen.surf <- subset(Plot.Uz.pollen.surf,
    #                               select = -c(AP,NAP,ShP, Other, Apiaceae, Fraxinus, Potentilla.t, Artemisia, Calligonum.t,
    #                                           Juglans, Rosaceae.shrub, Elaeagnus, Salix, Ranunculaceae, Poaceae))
    # Plot.Uz.pollen.surf <- subset(Plot.Uz.pollen.surf, select = -c(AP, NAP, ShP, Other, Apiaceae, Fraxinus, Salix))
    Plot.Uz.pollen.surf <- subset(Plot.Uz.pollen.surf, select = -c(AP, NAP, ShP, Other, Salix, Apiaceae, Calligonum.t,
                                                                   Brassicaceae, Potentilla.t, Ranunculaceae, Artemisia))
    Plot.Uz.pollen.surf <- data.frame(t(Plot.Uz.pollen.surf))
    write.table(Plot.Uz.pollen.surf, "Resultats/Uzbekistan/Pollen/MP_Uz_filtered.csv", sep = ",", col.names = NA)
  }
  else{Plot.Uz.pollen.surf <- read.table("Resultats/Uzbekistan/Pollen/MP_Uz_filtered.csv", sep = ",", header = T, row.names = 1)}
  
  #### DP NPP surf ####
  DP.NPP = F
  if(DP.NPP == T){
    source("Scripts/Pollen_DP.R")
    MPuz.ordin <- data.frame(t(Meco))
    MPuz.ordin <- MPuz.ordin[grep("Ordi", row.names(MPuz.ordin)),]
    row.names(MPuz.ordin)[1] <- "Top"
    row.names(MPuz.ordin)[5] <- "Age"
    MPuz.NPPC <- MPuz.NPPC[intersect(names(MPuz.NPPC), names(MPuz.ordin))]
    MPuz.ordin <- MPuz.ordin[intersect(names(MPuz.NPPC), names(MPuz.ordin))]
    DP_plot_Fungal(MPuz.NPPC, MPuz.ordin, Conc.Plot = F,
                   Pol.Sum = Pol.Sum.MPuz, Auto.lab.NPP = F,
                   Cortege = F, Col.cortege = F, Log.trans = F, Time.window = NULL,
                   Path.index = "Import/Uzbekistan/Pollen/Func_trans/Bibli_pollen_ACA_clean.csv",
                   CONISS = F, nlake = "Uzbekistan surfaces", Nzone = 6, Seuil.NPP = 1, Sort.taxon = "Auto.mean",
                   Spore.influx = F, Spore.diversity = F, Diversity.pollen.ratio = F, Richesse.type = F, 
                   Save.plot = "Figures/Uzbekistan/Pollen/Surface/Fazilman_DNPP_tot.pdf",
                   H = 1000, W = 2600
    )
  }
  #### PCA ####
  PCA.pollen = F
  if(PCA.pollen == T){
    #### Graphical settings ####
    # W = 2000
    # H = 1000
    W = 1100
    H = 550
    Save.plot = "Figures/Uzbekistan/Pollen/Surface/PCA_Uz_surfpol.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,2))
    
    #### PCA ####
    PCA.pollen.Uz <- PCA.pollen.surf(scale(Plot.Uz.pollen.surf),
    # PCA.pollen.Uz <- PCA.pollen.surf(Plot.Uz.pollen.surf,
                                       Cluster.path = Meco, Csv.sep =",", Simple.title = T, Show.text = F,
                                       transp_OK = T, cluster.from.PCA = F, 
                                       Scale.PCA = 3, Manu.lim = c(-2.9,1.9,-1.2,3), 
                                       # Cluster.groups = "Vegetation", Color.choice = Uz.col.2, Sort.eco = Sort.eco.Uz.2,
                                       Cluster.groups = "Ecosystem", Color.choice = Uz.col, Sort.eco = Sort.eco.Uz,
                                       Save.path = "Resultats/Uzbekistan/Pollen/Surface/MP_Uz.csv",
                                       Leg.loc = "bottomleft", Type.samples = "Pollen")
    
    #### RDA ####
    RDA.Uz.P1 <- RDA.pollen.surf(scale(Plot.Uz.pollen.surf), MClim = Meco, Cluster.path = Meco,
    # RDA.Uz.P1 <- RDA.pollen.surf(Plot.Uz.pollen.surf, MClim = Meco, Cluster.path = Meco,
                                   Choose.clim = c("MAAT", "MAP", "PS", "TS", "MTCOQ", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                                   Show.text = F, Display.legends = F, Simple.title = T, transp_OK = T,
                                   # Cluster.groups = "Vegetation", Color.choice = Uz.col.2, Sort.eco = Sort.eco.Uz.2,
                                   Cluster.groups = "Ecosystem", Color.choice = Uz.col, Sort.eco = Sort.eco.Uz,
                                   Scale.sites = 3, Scale.taxa = 2,
                                   Manu.lim = c(-1.2,1,-1.75,0.6),
                                   Save.path = "Resultats/Uzbekistan/Pollen/Surface/MP_Uz.csv",
                                   Csv.sep =",",Leg.loc2 = "bottomright", Type.samples = "Pollen")
    
    dev.off()
  }
  #### Pollen ratio test ####
  Pollen.Ratio.Test = F
  if(Pollen.Ratio.Test == T){
    ##### Choix des ratios #####
    TabRa <- list(data.frame(Deno = c("Artemisia"), Nomi = c("Chenopodiaceae")),                                     # A/Ch El-Moslimany, (1990) -> humidite
      data.frame(Deno = c("Artemisia"), Nomi = c("Cyperaceae")),                                          # A/Cy Herzschuh et al. (2006) -> Tsum
      data.frame(Deno = c("Artemisia"), Nomi = c("Chenopodiaceae","Artemisia")),
      data.frame(Deno = c("Artemisia","Chenopodiaceae"), Nomi = c("Poaceae"))                           # A+Ch/Po Fowell et al. (2003) -> humidite, R>5 desert
      )
    
    #### Graphical settings ####
    Ratio.Uz <- LR.ratio(MP = data.frame(t(Plot.Uz.pollen.surf)), TabRa = TabRa, Meco = Meco, 
                         Keep.clim = c("MAAT", "MAP", "MTWAQ", "MPWAQ"), R2.pos = "topright", Smooth.method = "lm",
                         H = 1000, W = 1000, Strip.lab = F, #R2.pos = c(1,2,1,3,4,3,1,2,1),
                         Save.plot = "Figures/Uzbekistan/Pollen/Surface/PolRatio_climate_CV.pdf")
    
             }
    
    
  #### Mat correl pollen ####
  Mat.corel = F
  if(Mat.corel == T){
    source("Scripts/Trait.R")
    Plot.Uz.pollen.surf <- subset(Plot.Uz.pollen.surf, select = -c(Calligonum.t, Caryophyllaceae, Asteroideae, Plantago, Juglans))
    # Meco <- subset(Meco, select = -c(AI, Pwin))
    
    MC.full <- Mat.corel.CWT.clim(Plot.Uz.pollen.surf, Plot.Uz.pollen.surf, # full M.cor.full[c(22:65)], M.cor.full[c(1,7:20)], 
                                  I.confiance = 0.95, 
                                  Display.pval = "blank",
                                  Disp.R = "number",
                                  Title = "",
                                  Save.path = "Resultats/Uzbekistan/Pollen/Surface/Matcorr_pollen_surf_uz.csv",
                                  Save.plot = "Figures/Uzbekistan/Pollen/Surface/Matcor_pol_surf_uz.pdf",
                                  H = 1900,
                                  W = 1000)
    
    
    MC.full <- Mat.corel.CWT.clim(Plot.Uz.pollen.surf, Meco[c(1,2,14:20,47:55)], # full M.cor.full[c(22:65)], M.cor.full[c(1,7:20)], 
                                  I.confiance = 0.95, 
                                  Display.pval = "blank",
                                  Disp.R = "number",
                                  Title = "",
                                  Save.path = "Resultats/Uzbekistan/Pollen/Surface/Matcorr_pollen_surf_clim_uz.csv",
                                  Save.plot = "Figures/Uzbekistan/Pollen/Surface/Matcor_pol_surf_clim_uz.pdf",
                                  H = 1900,
                                  W = 1000)
    
    
      }
  
  #### Stat sur Pollen units ####
  Stats.clust = F
  if(Stats.clust == T){
    KRN <- row.names(Uz.MP_sl)
    Uz.MP_sl <- data.frame(t(Uz.MP_sl))
    
    Uz.MP_sl <- left_join(rownames_to_column(Uz.MP_sl), rownames_to_column(Meco["Pollen_units"]), by = "rowname")
    
    TAB <- Uz.MP_sl %>%
      group_by(Pollen_units) %>%
      summarise_at(vars(names(Uz.MP_sl)[-c(1,ncol(Uz.MP_sl))]), mean)
    TAB <- data.frame(TAB)
    row.names(TAB) <- TAB$Pollen_units
    TAB <- TAB[-c(1)]
    TAB <- data.frame(t(TAB))
  
    }
  
  #### TWINSPAN sur pollen ? ####
  TW.pol = F
  if(TW.pol == T){
    Uz.Tw.TN <- Plot.twinspan(MV = Uz.MP_sl, Only.indicators = F,
                              # Meco = Meco["Ecosystem"], 
                              Meco = Meco["Pollen_units"], 
                              H = 500, W = 800, Sp.lab.size = 6, Site.lab.size = 6, Dot.size = 1.5,
                              # Color.choice = setNames(Uz.col, Sort.eco.Uz), 
                              Save.plot = "Figures/Uzbekistan/Pollen/Surface/Twinspan_MP_TUDS.pdf")
    
   
  }
  
  }

#### Iran (Golestan) ####
Ir.pol = F
if(Ir.pol == T){
  #### Import table ####
  MPIr  <- data.frame(read.csv(file="Import/Iran/Pollen/Surface/MP_count.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  Meco <- data.frame(read.csv(file="Import/Iran/Site/Ir_SS.csv",sep="\t",dec=".",header=T,row.names=1))
  # Mcluster.PCA <- data.frame(read.csv(file="Resultats/Iran/Pollen/Surface/MP_mong_PCA_HCPC.csv",sep=",",dec=".",header=T,row.names=1))
  # Meco.all       <- data.frame(read.csv(file="Import/ACA/Site/My_data/SS_ACA_V2.csv",sep=",",dec=".",header=T,row.names=1), stringsAsFactors = T)        # GDGT indexe Ayrag
  
  #### Extract climat ####
  Extract.Clim.WC = F
  if(Extract.Clim.WC == T){
    source("Scripts/Climat_extract.R")
    Mclim <- Clim.param.extraction(M = Meco, 
                                   All.param = T, Aridity = T, Altitude = T, Season = T, MAF = T,
                                   Seasonality = T, Biome = F, Map.display = T, Clim.display = F, 
                                   Save.path = "Import/Iran/Site/Ir_SS_clim.csv", Land.cover = F, 
                                   Save.plot = "Figures/Iran/Maps/Surface_samples_sites.pdf")    
    
    Mclim.chsa <- Clim.param.extraction(M = Meco, Chelsa = T,
                                        Aridity = T,
                                        All.param = F, 
                                        Altitude = T,
                                        Season = F, Biome = F,  
                                        #Map.display = F, 
                                        #Clim.display = F, 
                                        #Nb.map = 2, W = 2000, H = 1000,
                                        Save.path = "Import/Iran/Site/Ir_SS_clim_chel.csv",
                                        Save.plot = "Figures/Iran/Maps/Surface_samples_sites.pdf")
    
    Mbiom <- Clim.param.extraction(M = Meco, Clim.cal = F,
                                   All.param = F, Aridity = F, Altitude = F, Season = F, Biome = T, Map.display = F, Clim.display = F, 
                                   Land.cover = T,
                                   Save.path = "Import/Iran/Site/Ir_SS_biom.csv")
  }
  else{
    Mclim <- data.frame(read.csv(file="Import/Iran/Site/Ir_SS_clim.csv",sep=",",dec=".",header=T,row.names=1))
    Mbiom <- data.frame(read.csv(file="Import/Iran/Site/Ir_SS_biom.csv",sep=",",dec=".",header=T,row.names=1))
    Meco <- data.frame(read.csv(file="Import/Iran/Site/Ir_SS_simple.csv",sep=",",dec=".",header=T,row.names=1))
    }

  Site.commun <- intersect(names(MPIr), row.names(Meco))
  Site.non.commun <- c(setdiff(row.names(Meco), names(MPIr)), setdiff(names(MPIr), row.names(Meco)))
  Meco.retro <- Meco[Site.non.commun,]
  Meco <- Meco[Site.commun,]
  Mclim <- Mclim[Site.commun,]
  Sort.eco.Ir = c("Desert-steppe", "Mountain steppe-meadow", "Woodland", "Forest-steppe", "Forest")
  Iran.col = c("firebrick3", "goldenrod1", "#9FBB67", "#6789CE", "darkgreen")
  
  #### Export clean data ####
  MPIr[is.na(MPIr)] <- 0
  
  Ir.TaxaCorresp  <- data.frame(read.csv(file="Import/Iran/Pollen/Func_trans/Corresp_pollen_Ir_clean.csv",sep=",",dec=".", header=T, row.names=1))        # GDGT indexe Ayrag
  MPIr <- data.frame(t(MPIr))
  MPIr <- MPIr / rowSums(MPIr)
  
  MPIr$Ephedra.fragilis.type <- MPIr$Ephedra.fragilis.type + MPIr$Ephedra.distachya.type
  MPIr <- subset(MPIr, select = - c(Ephedra.distachya.type))
  MPIr$Fabaceae <- MPIr$Fabaceae + MPIr$Astragalus.type
  MPIr <- subset(MPIr, select = - c(Astragalus.type))
  MPIr$Carpinus.betulus <- MPIr$Carpinus.betulus + MPIr$Carpinus
  MPIr <- subset(MPIr, select = - c(Carpinus))
  MPIr$Cousinia <- MPIr$Cousinia + MPIr$Carthamus
  MPIr <- subset(MPIr, select = - c(Carthamus))
  MPIr$Cichorioideae <- MPIr$Cichorioideae + MPIr$Lactuceae
  MPIr <- subset(MPIr, select = - c(Lactuceae))
  MPIr <- subset(MPIr, select = - c(Sparganium, Indetermined))
  MPIr <- data.frame(t(MPIr)) # Pourc check
  row.names(MPIr) <- gsub("\\.", " ", row.names(MPIr))
  
  Ir.MP_sl <- cbind(PT.sl = Ir.TaxaCorresp$PT_sl.label[match(row.names(MPIr), row.names(Ir.TaxaCorresp))], MPIr)
  Ir.MP_sl <- aggregate(Ir.MP_sl[-1], by = list(Ir.MP_sl[["PT.sl"]]), sum)
  row.names(Ir.MP_sl) <- Ir.MP_sl$Group.1
  Ir.MP_sl <- Ir.MP_sl[-1]

  Ir.MP_ss <- cbind(PT.ss = Ir.TaxaCorresp$PT_ss.label[match(row.names(MPIr), row.names(Ir.TaxaCorresp))], MPIr)
  Ir.MP_ss <- aggregate(Ir.MP_ss[-1], by = list(Ir.MP_ss[["PT.ss"]]), sum)
  row.names(Ir.MP_ss) <- Ir.MP_ss$Group.1
  Ir.MP_ss <- Ir.MP_ss[-1]

  Ir.MP_ss <- round(Ir.MP_ss*100, digits = 2)
  Ir.MP_sl <- round(Ir.MP_sl*100, digits = 2)
  
  write.csv(Ir.MP_sl, "Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_coarse.csv")
  write.csv(Ir.MP_ss, "Resultats/Iran/Export_pangaea/Pollen_pourcentage_IR_PT_fine.csv")
  
  
  row.names(MPIr)[row.names(MPIr) == "Chenopodiaceae"] <- "Amaranthaceae"
  # 
  # MPIr <- data.frame(t(MPIr))
  # Chenopodiaceae 
  # MPIr$Apiaceae <- MPIr$Apiaceae + MPIr$Ferula.sp # Apiaceae
  # MPIr$Asteroideae <- MPIr$Asteroideae + MPIr$Aster.t + MPIr$Senecio.t + MPIr$Matricaria.t # Asteroideae
  # MPIr$Plantago <- MPIr$Plantago.coronopus.t + MPIr$Plantago.alpina.t + MPIr$Plantago.lanceolata + MPIr$Plantago.major + MPIr$Plantago.media # Plantago
  # MPIr <- subset(MPIr, select = - c(Aster.t, Senecio.t, Matricaria.t, Haloxylon.t, Ferula.sp,
  #                                   Plantago.coronopus.t, Plantago.alpina.t,
  #                                   Plantago.lanceolata, Plantago.major, Plantago.media))
  # MPIr <- data.frame(t(MPIr)) # Pourc check
  # 
  
  #### Verif metadata list samples ####
  Check.metada = F
  if(Check.metada == T){
    Meco.all$Proxy <- ""
    Meco.all$Proxy[Meco.all$Pollen == "x"] <- "P"
    Meco.all$Proxy[Meco.all$GDGT == "x"] <- paste(Meco.all$Proxy[Meco.all$GDGT == "x"], "G", sep = "")
    Meco.all$Proxy[Meco.all$Veg.plot == "x"] <- paste(Meco.all$Proxy[Meco.all$Veg.plot == "x"], "V", sep = "")
    
    List.samp.pollen.T <- names(MPIr)
    List.samp.pollen.expected <- row.names(Meco.all[Meco.all$Pollen == "x",])
    
    if(length(setdiff(List.samp.pollen.expected, List.samp.pollen.T)) >= 1){
      print("The following samples are missing from the pollen matrix :")
      print(setdiff(List.samp.pollen.expected, List.samp.pollen.T))}
    
    if(length(setdiff(List.samp.pollen.T,List.samp.pollen.expected)) >= 1){
      print("The following samples are missing from the metadata :")    
      print(sort(setdiff(List.samp.pollen.T,List.samp.pollen.expected)))}
    
  }
  
  #### Stats Ir surface ####
  Stats.show = T
  if(Stats.show == T){
    print(paste("Nombre échantillons Golestan surface:", ncol(MPIr)))
    print(paste("Nombre type-polliniques Golestan surface:", nrow(MPIr)))
    print(paste("Taxon dominant Golestan surface (valeur moyenne sur tous les échantillons):"))
    print(sort(round(rowMeans(MPIr), digits = 2), decreasing = T)[1:10])
  }
  
  #### DP ####
  DP.surf = F
  if(DP.surf == T){
    Plot.Ir.pollen.surf <- Diag.pol.surf(MP = MPIr,
                                         Ordin.path = "Import/Iran/Site/Ir_SS_simple.csv",
                                         Index = "Import/Iran/Pollen/Func_trans/Corresp_pollen_Ir_clean.csv",
                                         Y.legend = "Sample sites",
                                         Sort = "Ordin2",  # Altitude, MAP.ordin, MAAT, Latitude, Longitude, Ordination
                                         TaxonLabel = "Label",  Color.choice =  Iran.col,
                                         Max_seuil = 0.01, My_Ecosystem = "Ecosystem",
                                         AP.NAP = T,  ShP = F, CONISS = T, Nzone = 5, #Taxa.angle = -90,
                                         Abiot.plot = c("MAP", "MAAT", "Altitude"),
                                         Label.eco = c("Hyrcanian \n forest", "Acer- \n Juniperus \n forest",
                                                       "Quercus \n forest", "Poaceae \n steppe",
                                                       "Artemisia \n Amaranthaceae \n steppe"),
                                         Sort.eco = Sort.eco.Ir,
                                         H = 1300, W = 3000, Csv.sep = ",",
                                         Save.plot = "Figures/Iran/Pollen/Surface/DPsurf_Ir.pdf")
    
    stop()
    MAP.NAP <- subset(Plot.Ir.pollen.surf, select = c(AP,NAP))
    Plot.Ir.pollen.surf <- subset(Plot.Ir.pollen.surf, select = -c(AP,NAP,Other))
    # Plot.Ir.pollen.surf <- subset(Plot.Ir.pollen.surf, select = -c(Other, Apiaceae, Fraxinus, Salix))
    Plot.Ir.pollen.surf <- data.frame(t(Plot.Ir.pollen.surf))
    write.table(Plot.Ir.pollen.surf, "Resultats/Iran/Pollen/Surface/MP_Ir_filtered.csv", sep = ",", col.names = NA)
    write.table(MAP.NAP, "Resultats/Iran/Pollen/Surface/MP_AP_NAP.csv", sep = ",", col.names = NA)
  }
  else{
    Plot.Ir.pollen.surf <- read.table("Resultats/Iran/Pollen/Surface/MP_Ir_filtered.csv", sep = ",", header = T, row.names = 1)
    MAP.NAP <- read.table("Resultats/Iran/Pollen/Surface/MP_AP_NAP.csv", sep = ",", header = T, row.names = 1)
    }
  
  #### Twinspan ####
  Twinspan.Ir = F
  if(Twinspan.Ir == T){
    source("Scripts/Veget.R")
    Add.spp <- !grepl("eae", row.names(MPIr)) & !grepl(" type", row.names(MPIr))  & !grepl("\\s", row.names(MPIr))
    row.names(MPIr)[Add.spp] <- paste(row.names(MPIr)[Add.spp], "spp.")
    MPIr.tw <- Plot.twinspan(MV = MPIr*100, 
                             Add.dendro = T, Meco = Ir.eco, 
                             Eco.dot.size = 3, Nb.cluster = 2,
                             Show.indicators = T, #Cut.levels = c(0,2,4,10,20,30,40,60,80),
                             Eco.col = setNames(data.frame(t(Iran.col)), Sort.eco.Ir),
                             H = 950, W = 700,  Sp.lab.size = 8, Site.lab.size = 8, #Max.species = 50, 
                             Save.Indicator = "Resultats/Iran/Pollen/Indicator_species.Rds",
                             Symbol.path = "Figures/ACA/Trait/Workflow/export/Symbole_pollen.png",
                             Save.plot = "Figures/Iran/Pollen/Surface/Twinspan_pollen_Ir.pdf")}
  #### PCA ####
  PCA.pollen = F
  if(PCA.pollen == T){
    #### Graphical settings ####
    W = 1100
    H = 550
    Save.plot = "Figures/Iran/Pollen/Surface/PCA_Ir_surfpol.pdf"
    pdf(file = Save.plot, width = W*0.01041666666667, height = H*0.01041666666667)
    par(mfrow=c(1,2))
    
    #### PCA ####
    # PCA.pollen.Ir <- PCA.pollen.surf(scale(Plot.Ir.pollen.surf),               # Import Matrice pour la PCA
    PCA.pollen.Ir <- PCA.pollen.surf(Plot.Ir.pollen.surf,               # Import Matrice pour la PCA
                                     Cluster.path = Meco, Cluster.groups = "Ecosystem",
                                     # Cluster.path = Mcluster.PCA, Cluster.groups = "X3",
                                     Csv.sep =",", Simple.title = T, Show.text = T,
                                     transp_OK = T, cluster.from.PCA = F, 
                                     Scale.PCA = 1, Color.choice =  Iran.col,
                                     Save.path = "Resultats/Iran/Pollen/Surface/MP_Ir.csv",
                                     #Site.name = "Iran surface sample",
                                     Sort.eco = Sort.eco.Ir, Leg.loc = "bottomleft",
                                     Type.samples = "Pollen" 
    )     # Nom du site
    #### RDA ####
    RDA.Ir.P1 <- RDA.pollen.surf(Plot.Ir.pollen.surf,               # Import Matrice pour la PCA
                                 MClim = Meco,
                                 # Choose.clim = c("MAAT", "MAP", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                                 Choose.clim = c("MTWAQ", "MPWAQ", "MPCOQ", "Altitude"),#, "Latitude", "Longitude" pH_soil),
                                 Cluster.path = Meco,
                                 Cluster.groups = "Ecosystem", Show.text = T,
                                 Display.legends = F, Simple.title = T, 
                                 Csv.sep =",",Leg.loc2 = "bottomleft",
                                 transp_OK = F,  Color.choice =  Iran.col,
                                 Scale.sites = 3,                         # 1 or 2
                                 Scale.taxa = 2,                         # 1 or 2
                                 # Manu.lim = c(-1.5,1.5,-1.5,1.5),
                                 Save.path = "Resultats/Iran/Pollen/Surface/MP_Ir.csv",
                                 Type.samples = "Pollen",
                                 Sort.eco = Sort.eco.Ir,
                                 #Site.name = "Iran surface sample"
    )     # Nom du site
    
    dev.off()
  }
  #### Pollen ratio test ####
  Pollen.Ratio.Test = F
  if(Pollen.Ratio.Test == T){
    MR <- Pol.Ratio(MP = data.frame(t(rbind(data.frame(Plot.Ir.pollen.surf), data.frame(t(MAP.NAP))))), 
                    Select.ratio = c(1:6)
                    )
    MR <- MR[row.names(MR)!= "G43",]
    
    #### Plot strat ####
    Ratio.plot = F
    if(Ratio.plot == T){
      source("Scripts/Geoch_core.R")
      CONISS.pol <- c(0,4.5, 4.5, 10.5, 10.5, 12.5, 12.5, 22.5, 22.5, 33.5)
      
      Plot.ratio <- Plot.geoch.core(XRF = MR, MS = MR[c(8,9,15:23,55:57)+1], 
                                    Plot.x = "Ordin2",
                                    Keep.XRF = c("AP/NAP", "Poa/Art", "Poa/Ama", "Ama/Art", "Art_Ama/Poa"),
                                    Keep.MS = c("MAAT", "AI"), Change.titles = c("Pollen Ratios", "Climat parameters"),
                                    Select.interv.x = 1, CONISS = F, Panel.param = 1.5, Smooth.proxy = c("XRF"), Smooth.sd = T, Smooth.param = 0.3,
                                    Groupes = list(
                                      Wet = c("Ama/Art", "AI"),
                                      Warm = c("Art_Ama/Poa", "MAAT"),
                                      Forest = c("AP/NAP", "Poa/Art", "Poa/Ama")),
                                    Zone.clim = CONISS.pol, Manual.line = CONISS.pol, Arrows = T, Reverse.arrow = c(1,2),
                                    Name.zone = c("Hyrcanian \nforest", "Acer-\n Juniperus forest", "Quercus forest", "Poaceae \nsteppe", "Artemisia \nsteppe"),
                                    Temp.zone = rev(c("firebrick3", "goldenrod1", "#9FBB67", "#6789CE", "darkgreen")),
                                    H = 700, W = 1200, Save.plot = "Figures/Iran/Pollen/Surface/Pollen_ratio.pdf")}
    
    
    #### Mat correl ####
    Mat.cor.plot = F
    if(Mat.cor.plot == T){
      source("Scripts/Trait.R")
      MV.gf.cor <- Mat.corel.CWT.clim(MR[c(1:5)], MR[c(15:23,55:57)],
                                      I.confiance = 0.95,
                                      # Display.pval = "pch",
                                      Disp.R = "number", #Display = "lower", 
                                      Average = F, Label = F, Bar.pos = "n",
                                      # Save.pval = "Resultats/ACA/Traits/Permutations/Traits_ACAV_perm.csv",
                                      Permutation.test = F, Nb.permutations = 10000, 
                                      # Title = "Correlation between CWT-ACASP-ss and bioclimate parameters.",
                                      Save.plot = "Figures/Iran/Pollen/Surface/Matcor_PolRatio.pdf",
                                      H = 400,
                                      W = 800)}
  }
  
  
  #### Mat correl pollen ####
  Mat.corel = F
  if(Mat.corel == T){
    source("Scripts/Trait.R")
    # Plot.Ir.pollen.surf <- subset(Plot.Ir.pollen.surf, select = -c(Calligonum.t, Caryophyllaceae, Asteroideae, Plantago, Juglans))
    # Meco <- subset(Meco, select = -c(AI, Pwin))
    MC.full <- Mat.corel.CWT.clim(data.frame(t(Plot.Ir.pollen.surf)), data.frame(t(Plot.Ir.pollen.surf)),
                                  I.confiance = 0.95, Display = "lower", Bar.pos = "n", 
                                  Display.pval = "pch", Disp.R = "number", Title = "", 
                                  # Permutation.test = T, Nb.permutations = 10000, 
                                  Save.path = "Resultats/Iran/Pollen/Surface/Matcorr_pollen_surf_Ir.csv",
                                  Save.plot = "Figures/Iran/Pollen/Surface/Matcor_pol_surf_Ir.pdf",
                                  H = 1400, W = 1500)
    
    ##### "everything", "all.obs", "complete.obs", "na.or.complete",  "pairwise.complete.obs"
    My_use.cor <- "all.obs"
    #### "pearson", "kendall", "spearman"
    My_method.cor <- "pearson"
    
    MC.full <- Mat.corel.CWT.clim(Meco[c(1,2,10:20,43:52)], data.frame(t(Plot.Ir.pollen.surf[c(1,2,4,7,11,15,18,19,22,26,28),])),
                                  I.confiance = 0.95, Display.pval = "pch", Disp.R = "number", Title = "",
                                  Save.path = "Resultats/Iran/Pollen/Surface/Matcorr_pollen_surf_clim_Ir.csv",
                                  Save.plot = "Figures/Iran/Pollen/Surface/Matcor_pol_surf_clim_Ir.pdf",
                                  Use.cor = My_use.cor, Method.cor = My_method.cor, Label = F, Average = F,  Bar.pos = "n",
                                  H = 1300, W = 650
                                  )
    }
  
}
