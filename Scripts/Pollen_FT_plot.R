#### Global path ####
#setwd("/media/lucas.dugerdil/Maximator/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/Stages/Stage_M2_Mongolie/R_stats") 
setwd("/home/lucas.dugerdil/Documents/Recherche/R_stats") 
#setwd("/media/lucas.dugerdil/Samsung_T5/Documents/Recherche/R_stats") 

#### Librairies ####
library(rioja)
library(ggplot2)      # permet de faire des graphiques esthetiques
library(reshape)      # permet d'utiliser la fonction melt utile pour afficher les graphs
library(cowplot)      # permet aligner les graphs  
library(scales)       # quoi du type d'echelle sur les graphs ggplot
library(colorRamps)   # faire une échelle de couleur perso
library(ggnewscale)
library(zoo) # use function na.locf
library(stringr)
library(RColorBrewer)
library(patchwork)
library(dplyr)
library(tibble) # as_tibble



#### Fonctions ####
Plot.FT.summary <- function(FT, Cores, Pclim, Model, Select.interv, Cores.lab, Facet.T, Repel.T, Zoom.box = NULL,
                            Temp.col.alpha = 0.1, Surf.val, Anomaly, GDGT.plot.merge, Label.group, Smooth.param, Phase.clim,
                            Save.Rds, Save.plot, W, H, Limites, Add.lim.space, Mono.core, Condensed, Mean.models,
                            Manual.vlines = NULL, Smooth.sd, Only.fit, Zone.clim, Name.zone, Temp.zone, Clim.lab, Manual.y.val){
  #### Init Val ####
  if(missing(FT)){warning("Import the transfer function for plotting.")}
  if(missing(Cores)){warning("Select and sort the core(s) to plot.")}
  if(missing(Pclim)){warning("Select and sort the climate parameter(s) to plot.")}
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(ggthemes)
  library(ggrepel) # nom des lignes à côté
  
  if(missing(Surf.val)){
    Surf.val.j.i = NULL
    Surf.val = NULL}
  if(missing(Mean.models)){Mean.models = F}
  if(missing(Anomaly)){Anomaly = F}
  if(missing(Condensed)){Condensed = F}
  if(missing(Mono.core)){Mono.core = F}
  if(missing(Add.lim.space)){Add.lim.space = T}
  if(missing(Manual.y.val)){Manual.y.val = NULL}
  if(missing(Cores.lab)){Cores.lab = NULL}
  if(missing(GDGT.plot.merge)){GDGT.plot.merge = F}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Save.Rds)){Save.Rds = NULL}
  if(missing(Label.group)){
    Label.group <- c("NMSDB", "MDB", "COST", "COSTDB", "EAPDB", "TUDB", "WASTDB", "STDB", "ACADB")}
  if(missing(Zone.clim)){Zone.clim = NULL}
  if(missing(Name.zone)){Name.zone = NULL}
  if(missing(Temp.zone)){Temp.zone = rep("C", length(Zone.clim))}
  if(missing(Clim.lab)){Clim.lab = NULL}
  if(missing(Limites)){Limites = NULL}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(missing(Select.interv)){Select.interv = 1000}
  if(missing(Smooth.param)){Smooth.param = 0.6}
  if(missing(Only.fit)){Only.fit = F}
  if(missing(Facet.T)){Facet.T = T}
  if(missing(Repel.T)){Repel.T = F}
  if(missing(Phase.clim)){Phase.clim = F}
  if(missing(Smooth.sd)){Smooth.sd = T}
  
  #### Graphical settings ####
  Plot.list.col <- list()
  Rect.color.scale <- c("W" = "#E76D51",
                        "C" = "#75AADB",
                        "G" = "grey40",
                        "D" = "#c67f05",
                        "Wt" = "#004266")
  Rect.color.scale <- Rect.color.scale[unique(Temp.zone)]
  
  if(GDGT.plot.merge == T){
    Title.age = NULL
    Legende.age.chart <- rep(0, length(Cores))
    if(Add.lim.space == T){
      FT <- lapply(FT, function(x) x[which(x$Age <= Limites[2]),])
      Limites[2] <- (Limites[2] + Limites[2]*0.23)}
    Legende.Core.name.chart <- c(17, rep(0, (length(Pclim)-1)))
  }
  if(GDGT.plot.merge == F){
    Legende.age.chart <- c(rep(0, (length(Cores)-1)), 11)
    Title.age = "Age (yr BP)"
    Legende.Core.name.chart <- c(16, rep(0, (length(Pclim)-1)))
  }
  Legende.position.chart <- c("top", rep("none", (length(Cores)-1)))
  Legende.title.chart <- c(16, rep(0.5, (length(Cores)-1)))
  Legende.title.col.chart <- c("black", rep("white", (length(Cores)-1)))
  Legende.axis.chart <- c(rep("white", (length(Cores)-1)), "grey")
  if(length(Smooth.param) != length(Cores)){Smooth.param <- rep(Smooth.param, length(Cores)/length(Smooth.param))}
  if(is.null(Zone.clim) ==F){
    yo = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                    xmax = Zone.clim[seq(2,length(Zone.clim), by=2)], 
                    Temp.col = Temp.zone)} 
  else{yo = data.frame(xmin = 0, xmax = 0, Temp.col = "black")}
  if(is.null(Name.zone) == F){yo2 = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                                               xmax = Zone.clim[seq(2,length(Zone.clim), by=2)],
                                               Temp.col = Temp.zone,
                                               Title.zone = Name.zone,
                                               Model = Model[length(Model)])}
  else{yo2 = data.frame(xmin = 0, xmax = 0, Temp.col = "", Title.zone = "")}
  
  if(Facet.T == T){
    Geom.point <- geom_point(shape = 20, size = 1.9)
    Facet.disp <- facet_grid(rows = vars(Model), switch = "y")}
  else{
    Geom.point <- geom_point(size = 2)
    Facet.disp <- facet_grid(rows = NULL, switch = "y")}
  
  #### Case only some Surface Values ####
  if(is.null(Surf.val) == F){A <- setNames(data.frame(matrix(ncol = length(setdiff(Pclim,names(Surf.val))), nrow = nrow(Surf.val))), setdiff(Pclim,names(Surf.val))) 
  row.names(A) <- row.names(Surf.val)
  A[is.na(A)] <- NA
  A <- cbind(Surf.val, A)
  Surf.val <- A[,sort(names(A))]}
  
  #### Reorganized the surface values according to cores ####
  
  #### Main Loop ####
  FT.crop <-sapply(Cores, function(x) as.data.frame(FT[grepl(x, names(FT))]), simplify = F)
  if(Phase.clim == T){Select.models.tot <- list()}
  for(j in 1:length(Pclim)){
    #### Settle surf val j ####
    print(paste("Ploting the", Pclim[j]))
    Plot.list <- list()
    if(is.null(Surf.val) == F){Surf.val.j <- subset(Surf.val, select = c(Pclim[j]))}
    if(Phase.clim == T){
      if(is.null(Limites) == T){stop("The limites have to be settled to apply Phase.clim = T")}
      if(Limites[2] > 100){Select.models <- data.frame(Age = seq(Limites[1], Limites[2], by = 10))}
      if(Limites[2] <= 100){Select.models <- data.frame(Age = seq(Limites[1], Limites[2], by = .01))}
      }
    
    #### Verbose ####
    library(lubridate)
    if(length(Cores) > 1){
      pb = txtProgressBar(min = 1, 
                          max = length(Cores), 
                          width = 40,
                          initial = 0,  style = 3) 
      
      init <- numeric(length(Cores))
      end <- numeric(length(Cores))}
    
    #### Loop 2 ####
    for(i in 1:length(Cores)){
      #### Settle surf val i ####
      if(length(Cores) > 1){init[i] <- Sys.time()}
      Name.core <- Cores[i]
      if(is.null(Surf.val) == F){
        Surf.val.j.i <- Surf.val.j[Name.core,]
        if(is.na(Surf.val.j.i) == T){Surf.val.j.i = NULL}}
      
      if(is.null(Cores.lab) == F){Name.core <- Cores.lab}
      
      #### Fusion des list en 1 unique matrice par Carotte ####
      Mtest <- FT.crop[[i]]
      Mtest <- Mtest[,!grepl("SEP", colnames(Mtest))]
      colnames(Mtest)[1]<-"Age"
      Mtest <- Mtest[,!grepl("\\.Age", colnames(Mtest))]
      
      #### Selection des modèles / Param.clim ####
      names(Mtest) <- sub("Psum", "SUMMERPR", names(Mtest)) 
      Ttete <- unlist(c("Age", sapply(Model, function(x) names(Mtest)[grepl(x, names(Mtest))])))
      Ttete <- c("Age", sapply(Pclim[[j]], function(x) Ttete[grepl(x, Ttete)]))
      Mtest <- Mtest[Ttete]
  
      #### Calcul en anomalies ####
      if(Anomaly == T & is.null(Surf.val) == F){
        Keep.Age <- Mtest[,1]
        Msurf <- setNames(data.frame(matrix(ncol = ncol(Mtest), nrow = nrow(Mtest), Surf.val.j.i)), names(Mtest))
        row.names(Msurf)<- row.names(Mtest)
        Mtest <- (Mtest - Msurf)/Msurf
        Surf.val.j.i <- 0
        Lim.ano <- c(min(Mtest[2], na.rm = T), max(Mtest[2], na.rm = T))
        Lim.ano <- c(min(Lim.ano[1], Surf.val.j.i, na.rm = T), max(Lim.ano[2], Surf.val.j.i, na.rm = T))
        Mtest <- cbind(Age = Keep.Age, Mtest)
      }
      else{Lim.ano = NULL}

      #### Melt des données ####
      Mtot.melt <- melt(Mtest, id = "Age")
      Categorie <- t(data.frame(strsplit(as.character(Mtot.melt$variable),split="\\.")))
      Mtot.melt <- cbind(Mtot.melt, Categorie)
      Mtot.melt <- Mtot.melt[,-2]
      colnames(Mtot.melt) <- c("Age", "variable", "Lake", "Model", "DB", "Param.clim")
      Mtot.melt$DB <- factor(Mtot.melt$DB, levels = Label.group)

      row.names(Mtot.melt) <- paste("P", 1:nrow(Mtot.melt), sep = "")
      Mtot.melt <- na.omit(Mtot.melt)

      #### Last graphical settings ####
      if(Condensed == T){
        if(i < length(Cores)){
          Xtick <- element_blank()
          Xline <- element_blank()}
        else{
          Xtick <- element_line(colour = "grey55")
          Xline <- element_line(colour = "grey55", lineend = "butt")}
        if(i > 1){yo2$Title.zone.i <- ""}
        else{yo2$Title.zone.i <- yo2$Title.zone}
        }
      else{
        Xtick <- element_line(colour = "grey55")
        Xline <- element_line(colour = "grey55", lineend = "butt")
        yo2$Title.zone.i <- yo2$Title.zone}
      if(GDGT.plot.merge == T){
        Xtick <- element_blank()
        Xline <- element_blank()
        if(is.null(Cores.lab) == T){
          if(Mono.core == T){Name.core <- "Pollen-based \n climate reconstructions"}
          else{Name.core <- paste(Name.core, " (pollen)", sep = "")}}
        }
      
      if(length(Name.core) == 1){Name.core.plot <- Name.core}
      else{Name.core.plot <- Name.core[[i]]}
      
      #### Limites ####
      if(is.null(Limites) == T){Limites = c(min(Mtot.melt$Age, na.rm = T), max(Mtot.melt$Age, na.rm = T))}
      else{
        Mtot.melt <- Mtot.melt[Mtot.melt$Age <= Limites[2],]
        Mtot.melt <- Mtot.melt[Mtot.melt$Age >= Limites[1],]
        }
      
      #### Repels ####
      if(Repel.T == T){
        Mtot.melt$Lab <- NA
        Mtot.melt$Lab[which(Mtot.melt$Age %in% max(Mtot.melt$Age, na.rm = T))] <- paste(as.character(Mtot.melt$Param.clim[which(Mtot.melt$Age %in% max(Mtot.melt$Age, na.rm = T))]), "[",
                                                                             as.character(Mtot.melt$Model[which(Mtot.melt$Age %in% max(Mtot.melt$Age, na.rm = T))]), "-",
                                                                             as.character(Mtot.melt$DB[which(Mtot.melt$Age %in% max(Mtot.melt$Age, na.rm = T))]), "]", sep ="")
        if(length(na.omit(unique(Mtot.melt$Lab))) == 1){NY = 10}
        else{NY = 0}
        Repel <-  geom_text_repel(mapping = aes(x = Age, label = Lab),  nudge_y = NY, # ETIQUETTE MODELS
                                  force = 4, nudge_x  = 20000, direction = "y", hjust = 1,
                                  size = 4.5, parse = T, segment.size = 0.18, segment.colour = "grey70")
        }
      else{Repel <- NULL}
      
      #### Breaks ####
      if(is.null(Manual.y.val) == T){
        Kmax <- max(Mtot.melt$variable, Surf.val.j.i, na.rm = T)
        Kmin <- min(Mtot.melt$variable, Surf.val.j.i, na.rm = T)
        if(Kmax >= 40){
          Kmin <- round(Kmin, digits = -1)
          Kmin = floor(Kmin / 50) * 50
          Kmax <- round(Kmax, digits = -1)
          Kmax = ceiling(Kmax / 50) * 50
          Kmid <- Kmin + (Kmax - Kmin)*1/3
          Kmid = ceiling(Kmid / 50) * 50
          Kmid2 <- Kmin + (Kmax - Kmin)*2/3
          Kmid2 = ceiling(Kmid2 / 50) * 50
          }
        if(Anomaly == T & is.null(Surf.val) == F){
          Kmid <- Kmin + (Kmax - Kmin)*1/3
          Kmid2 <- Kmin + (Kmax - Kmin)*2/3
          }
        else{
          Kmin <- round(Kmin, digits = 1)
          Kmin = floor(Kmin / 0.5) * 0.5
          Kmax <- round(Kmax, digits = 1)
          Kmax = ceiling(Kmax / 0.5) * 0.5
          Kmid <- Kmin + (Kmax - Kmin)*1/3
          Kmid = ceiling(Kmid / 0.5) * 0.5
          Kmid2 <- Kmin + (Kmax - Kmin)*2/3
          Kmid2 = ceiling(Kmid2 / 0.5) * 0.5
          }
        Range.round <- c(Kmin, Kmid, Kmid2, Kmax)
        if(Kmax >= 40){
          Range.round <- round(Range.round, digits = -1)
          }
        if(Anomaly == T & is.null(Surf.val) == F){
          Range.round <- round(Range.round, digits = 2)
          }
        else{
          Range.round <- round(Range.round, digits = 1)
          }
        }
      else{
        Clim.break <- gsub("\\..*", "", names(Manual.y.val))
        Core.break <- gsub(".*\\.", "", names(Manual.y.val))
        Match.clim <- which(Clim.break %in% Pclim[j])
        Match.core <- which(Core.break %in% Cores[i])
        Match.tot <- unique(intersect(Match.clim, Match.core), intersect(Match.core, Match.clim))
        Range.round <- Manual.y.val[[Match.tot]]
        }
      
      #### Only fit ####
      if(Only.fit == F){FT.line <- geom_line(linetype = "solid", size=0.5, alpha = 0.5)}
      else{
        Geom.point <- NULL
        FT.line <- NULL}
      
      #### Average lol ####
      if(Mean.models == T){
        # print(names(Mtot.melt))
        # Mtot.melt["Smooth"] <- lowess(x = Mtot.melt$variable, y = Mtot.melt$Age, f = 0.25)
        # print(Mtot.melt)
        # Mean.line <- geom_line(inherit.aes = F, data = Mtot.melt, aes(x = Age, y = Smooth), color="#770000ff", size=2, alpha = .8, linetype="solid")
        Mean.line <- stat_summary(inherit.aes = F, data = Mtot.melt, aes(x = Age, y = variable), geom="line", fun = "mean", color="#770000ff", linewidth = 1.8, alpha = .8, linetype="solid")
      }
      else{Mean.line <- NULL}
      #### PLOTS ####
      Plot.list[[i]] <- ggplot(data = Mtot.melt, 
              mapping = aes(x = Age, y = variable, color = DB, shape = Model)) +
              # mapping = aes(x = Age, y = variable)) +
         #### Rectangles ####
         scale_fill_manual(values = Rect.color.scale, guide = "none")+
         geom_rect(data = yo, inherit.aes = F, 
                  mapping = aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf, fill = Temp.col), 
                  alpha = Temp.col.alpha, color = "grey", size = 0.3, linetype = 2, na.rm = T) +
         geom_vline(xintercept = Manual.vlines, col = "grey30", lty = 2, alpha = 0.7)+
        
         #### Plot items ####
         new_scale_fill()+
         scale_shape_manual(values = c("MAT" = 16, "WAPLS" = 1, "RF" = 3, "BRT" = 18)) +
         scale_fill_manual(values = c("WASTDB" = "#e2a064ff", "WAST" = "#e2a064ff", 
                                     "NMSDB" = "#F3A481", "MDB" = "red", "TUDB" = "#0094AF",
                                     "ST" = "#91C4DD", "STDB" = "#91C4DD",  
                                     "COSTDB" = "#f3c768ff", "COST" = "#f3c768ff",
                                     "MEDTEMP" = "#F3A481", "TEMPSCAND" = "#91C4DD", 
                                     "EAPDB" = "#0F3361", "TAIGDB" = "#32156eff"), label = Label.group)+
         scale_color_manual(values = c("WASTDB" = "#e2a064ff", "WAST" = "#e2a064ff", "NMSDB" = "#F3A481",  
                                      "ST" = "#91C4DD", "STDB" = "#91C4DD", "MDB" = "red",
                                      "COSTDB" = "#f3c768ff", "COST" = "#f3c768ff", "TUDB" = "#0094AF",
                                      "MEDTEMP" = "#F3A481", "TEMPSCAND" = "#91C4DD", 
                                      "EAPDB" = "#0F3361", "TAIGDB" = "#32156eff"), label = Label.group)+
        
         FT.line + Geom.point + Facet.disp + 
         geom_smooth(method = "loess", se = Smooth.sd, fullrange = F, level = 0.95, linetype="solid", formula = "y ~ x",
                     size = .8, show.legend = F, aes(fill = DB), span = Smooth.param[i], alpha = 0.2)+
         Mean.line + 
         ggtitle(Clim.lab[[j]])+
         guides(colour = guide_legend(nrow = 1))+        # force les legendes a salligner sur une unique ligne
         #scale_y_continuous(name = Name.core.plot,  breaks = scales::breaks_extended(n = 4))+
         scale_y_continuous(name = Name.core.plot, breaks = Range.round) +
         scale_x_continuous(name = Title.age, breaks = round(seq(0, Limites[2], by = Select.interv)), expand = c(0.02, 0.02))+
         coord_cartesian(xlim = Limites, ylim = Lim.ano, clip = "off")+

         #### Annotations ####
         Repel +
         geom_hline(yintercept = Surf.val.j.i, linetype = "dotdash", size = 0.6, color = "black", alpha = 0.6)+
         
         new_scale_color()+
         scale_color_manual(values = Rect.color.scale, guide = "none", name = NULL, labels = NULL, breaks = NULL, na.translate = FALSE)+
         geom_text(data = yo2, inherit.aes = F,
                 aes(x = (xmax+xmin)/2, y = Inf, label = Title.zone.i, color = Temp.col), size = 3.5, vjust = 1.5, fontface = "bold") +
         geom_rangeframe(data = data.frame(A = Range.round[c(1,length(Range.round))], B = Limites), inherit.aes = F, mapping = aes(x = B, y = A), colour = "grey50", sides = "l", size = 0.5) +
        
         #### Theme ####
         theme(
          plot.title =  element_text(size = Legende.title.chart[i], hjust = 0.5, color = Legende.title.col.chart[i]),
          axis.text.x = element_text(angle = 45, hjust = 1, size = Legende.age.chart[i], colour = "grey55"),
          axis.text.y = element_text(size = 10.5, colour = "grey55"),
          axis.title.x = element_text(size = Legende.age.chart[i], colour = "grey20"),              # taile police du titre de l'axe
          axis.title.y = element_text(size = Legende.Core.name.chart[j], colour = "grey20"),              # taile police du titre de l'axe
          #axis.line.y = element_line(colour = "grey", lineend = "butt"),
          axis.line.y = element_blank(),
          axis.line.x = Xline,
          axis.ticks.x.bottom = Xtick,
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position = Legende.position.chart[i],                   # Legendes DB
          legend.justification = c("center"),               # left, top, right, bottom
          legend.direction = "horizontal",
          legend.text.align = 0,
          legend.text = element_text(size = 10, color = "grey20"),
          panel.background = element_blank(),
          panel.spacing = unit(0.08, "cm"),
          panel.grid = element_blank(),
          strip.text.y = element_text(size = 11.5, angle = 180),
          strip.placement = "outside",
          strip.background = element_blank(),
          plot.background = element_blank(),
          plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm")
         )
      #### Calcul période climatique ####
      if(Phase.clim == T){
        # print(Pclim[j])
        # print(Cores[[i]])
        # print(Pclim)
        Continue = NULL
        if(length(unique(Mtot.melt$Model)) == 1 & length(unique(Mtot.melt$DB)) == 1){
          # print("Extract the selected smoothed model.")
          Save.fit <- ggplot_build(Plot.list[[i]])$data[[4]]
          Continue = T}
        if(Mean.models == T){
          # print("Calculate the mean")
          Save.fit <- ggplot_build(Plot.list[[i]])$data[[5]]#[,1:6]
          # print(Save.fit)
          Save.fit <- Save.fit[c("x", "y")]
          Continue = T
          }
        if(is.null(Continue) == T){
          # print("To calculate the climatic phase, please only select one model for each cores / param. clim.")
          Continue = F}
        
        if(Continue == T){
          Save.fit <- Save.fit[c("x", "y")]
          Oldest.val <- Save.fit[nrow(Save.fit),1]
          Save.fit <- Save.fit[Save.fit[1] <= Limites[2],]
          
          if(max(Save.fit[1]) > 100){Save.fit[1] <- round(Save.fit[1], digits = -1)}
          if(max(Save.fit[1]) <= 100){Save.fit[1] <- round(Save.fit[1], digits = 2)}
          
          Save.fit[3] <- Save.fit[2] > colMeans(Save.fit[2])
          Partial.deriv.bool <- diff(Save.fit[[2]]) / diff(Save.fit[[1]]) < 0
          Partial.deriv <- diff(Save.fit[[2]]) / diff(Save.fit[[1]])
          Partial.deriv[length(Partial.deriv)+1] <- Partial.deriv[length(Partial.deriv)]
          Partial.deriv.bool[length(Partial.deriv.bool)+1] <- Partial.deriv.bool[length(Partial.deriv.bool)]
          Save.fit[4] <- Partial.deriv
          Save.fit[5] <- Save.fit[4]*20+(Save.fit[2]-colMeans(Save.fit[2]))/sd(Save.fit[[2]])
          Save.fit[6] <- (Save.fit[2]-colMeans(Save.fit[2]))/sd(Save.fit[[2]])
          Save.fit[7] <- Partial.deriv.bool
          Save.fit[8] <- 2*(Save.fit[2]-min(Save.fit[2]))/(max(Save.fit[2])-min(Save.fit[2]))-1
          Save.fit[9] <- 2*(Save.fit[6]-min(Save.fit[6]))/(max(Save.fit[6])-min(Save.fit[6]))-1

          Save.fit[10]  <- lowess(x = Save.fit$x, y = Save.fit$y, f = 0.35)$y
          Save.fit[11] <- 2*(Save.fit[10]-min(Save.fit[10]))/(max(Save.fit[10])-min(Save.fit[10]))-1
          
          # Fit <- loess("y ~ x", x = y, y = x, data = Save.fit)
          # print(Fit)
          Save.fit <- Save.fit[,c(1,11)] # smooth de la moyenne de tous les models retenus z-scores entre -1 et 1
          # Save.fit <- Save.fit[,c(1,10)] # smooth de la moyenne de tous les models retenus
          # Save.fit <- Save.fit[,c(1,9)] # z-score standardisé entre -1 et 1
          # Save.fit <- Save.fit[,c(1,8)] # value standardisé entre -1 et 1
          # Save.fit <- Save.fit[,c(1,7)] # Tangente booléreen
          # Save.fit <- Save.fit[,c(1,6)] # z-score (centré réduite)
          # Save.fit <- Save.fit[,c(1,5)] # keep deriv x z-score
          #Save.fit <- Save.fit[,c(1,4)] # keep deriv
          # Save.fit <- Save.fit[,c(1,3)] # keep anomaly mean
          # Save.fit <- Save.fit[,c(1,2)] # keep anomaly mean
          
          if(length(unique(Mtot.melt$Model)) == 1 & length(unique(Mtot.melt$DB)) == 1){
            colnames(Save.fit) <- c("Age", paste(Cores[[i]], Pclim[[j]], unique(Mtot.melt$Model), unique(Mtot.melt$DB), sep = "."))
            # Merge.mat <- merge(Select.models, Save.fit, by ="Age", all.x = T)
            # Merge.mat <- cbind(Merge.mat[1:(ncol(Merge.mat)-1)], na.locf(Merge.mat[ncol(Merge.mat)], na.rm = F))
            # Merge.mat[Merge.mat$Age > Oldest.val, ncol(Merge.mat)] <- NA
            # Select.models <- Merge.mat
            }
          
          if(Mean.models == T){
            colnames(Save.fit) <- c("Age", paste(Cores[i], Pclim[[j]], "combined_models", sep = "."))
            }
          Merge.mat <- merge(Select.models, Save.fit, by ="Age", all.x = T)
          Merge.mat <- cbind(Merge.mat[1:(ncol(Merge.mat)-1)], na.locf(Merge.mat[ncol(Merge.mat)], na.rm = F))
          Merge.mat[Merge.mat$Age > Oldest.val, ncol(Merge.mat)] <- NA
          Select.models <- Merge.mat
          
        }

        
      }
     
      #### Verbose fin ####
      if(length(Cores) > 1){
        end[i] <- Sys.time()
        setTxtProgressBar(pb, i)
        time <- round(seconds_to_period(sum(end - init)), 0)
        est <- length(Cores) * (mean(end[end != 0] - init[init != 0])) - time
        remainining <- round(seconds_to_period(est), 0)
        cat(paste(" - Execut. time:", time,
                  " - Estim. time remain.:", remainining), "")}
      }
    if(length(Cores) > 1){close(pb)}
    #### Facet plot clim / cores ####
    Formula.patch <- paste(paste("Plot.list[[", seq(length(Cores)), "]]", sep = ""), collapse = " / ")
    Plot.list.col[[j]] <- eval(parse(text = Formula.patch))
    if(Phase.clim == T){Select.models.tot[[j]] <- Select.models}
    }
  #### Save plots and export ####
  Formula.patch <- paste(paste("Plot.list.col[[", seq(length(Pclim)), "]]", sep = ""), collapse =  " | ")
  Ptot <- eval(parse(text = Formula.patch))
  
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(Ptot, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm", limitsize = F)}
    else{ggsave(Save.plot)}}
   
  if(is.null(Save.Rds) == F & Phase.clim == T){saveRDS(Select.models.tot, Save.Rds)}
  return(Ptot)
  
  }

Plot.clim.phase <- function(MFT, MGDGT, Site.order, Limites, Save.Rds, Save.plot, W, H, 
                            Zone.clim, Name.zone, Temp.zone, Diff.MAAT.MAP, Meta.data,
                            Show.proxy.t){
  #### Init Val ####
  if(missing(MFT)){MFT = NULL}
  if(missing(MGDGT)){MGDGT = NULL}
  if(missing(Save.Rds)){Save.Rds = NULL}
  if(missing(Save.plot)){Save.plot = NULL}
  if(missing(Zone.clim)){Zone.clim = NULL}
  if(missing(Name.zone)){Name.zone = NULL}
  if(missing(Site.order)){Site.order = names(MFT[[1]][-1])}
  if(missing(Meta.data)){Meta.data = NULL}
  if(missing(Diff.MAAT.MAP)){Diff.MAAT.MAP = T}
  if(missing(Show.proxy.t)){Show.proxy.t = T}
  
  if(missing(Temp.zone)){Temp.zone = rep("C", length(Zone.clim))}
  if(missing(W)){W = NULL}
  if(missing(H)){H = NULL}
  if(is.null(Name.zone) == F){Site.order <- c("Climate Periods", Site.order)}
  
  #### Zone clim graphical settings ####
  if(length(unique(Temp.zone)) == 0){Title.color.scale <- c("grey")}
  if(length(unique(Temp.zone)) == 1){Title.color.scale <- c("grey")}
  if(length(unique(Temp.zone)) == 3){Title.color.scale <- c("#75AADB", "black", "#E76D51")}
  if(length(unique(Temp.zone)) == 2 & "C" %in% unique(Temp.zone) & "W" %in% unique(Temp.zone)){Title.color.scale <- c("#75AADB","#E76D51")}
  if(length(unique(Temp.zone)) == 2 & "G" %in% unique(Temp.zone) & "W" %in% unique(Temp.zone)){Title.color.scale <- c("black", "#E76D51")}
  if(length(unique(Temp.zone)) == 2 & "G" %in% unique(Temp.zone) & "C" %in% unique(Temp.zone)){Title.color.scale <- c("#75AADB", "black")}
  
  Rect.color.scale <- c("grey40", "grey5")
  
  if(is.null(Zone.clim) ==F){
    yo = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                    xmax = Zone.clim[seq(2,length(Zone.clim), by=2)], 
                    Temp.col = Temp.zone)
    
    My_climate_zone <- geom_rect(data = yo, inherit.aes = F, 
              mapping = aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=+Inf, fill = Temp.col), 
              alpha=0.2, color = "grey15", size = 0.3, linetype = 2)
    } 
  else{
    yo = data.frame(xmin = 0, xmax = 0, Temp.col = "black")
    My_climate_zone = NULL
    }
  if(is.null(Name.zone) == F){yo2 = data.frame(xmin = Zone.clim[seq(1,length(Zone.clim), by=2)], 
                                               xmax = Zone.clim[seq(2,length(Zone.clim), by=2)],
                                               Temp.col = Temp.zone,
                                               Title.zone = Name.zone,
                                               L1 = 2.5,
                                               Lake = factor("Climate Periods", levels = Site.order))
                              My_climate_name <- geom_text(data = yo2, inherit.aes = F,
                                                           aes(x = (xmax+xmin)/2, y = L1, label = Title.zone, color = Temp.col), size = 3, vjust = 0.5, fontface = "bold")
                                
  
  }
  else{
    yo2 = data.frame(xmin = 0, xmax = 0, Temp.col = "", Title.zone = "")
    My_climate_name = NULL}
  
  #### Merge pollen, gdgt and other ####
  if(is.null(MFT) == T & is.null(MGDGT) == T){stop("Import models to plot.")}
  if(is.null(MFT) == F & is.null(MGDGT) == T){MModel <- MFT}
  if(is.null(MFT) == T & is.null(MGDGT) == F){MModel <- MGDGT}
  if(is.null(MFT) == F & is.null(MGDGT) == F){
    if(length(MGDGT) == length(MFT)){
      MModel <- MGDGT[0]
      for(i in 1:length(MGDGT)){
        names(MGDGT[[i]])[-1] <- paste(names(MGDGT[[i]])[-1], "brGDGT", sep = "")
        names(MFT[[i]])[-1] <- paste(names(MFT[[i]])[-1], "Pollen", sep = ".")
        MModel[[i]] <- merge(MGDGT[[i]], MFT[[i]], by = "Age", all = T)
        }
      }
    else{stop("The 2 proxies should have been modelled both the same number of climate parameters.")}
    }

  #### Calcul difference param ####
  if(Diff.MAAT.MAP == T){
    MModel[[3]] <- MModel[[1]] + MModel[[2]]
    MModel[[3]]$Age <- MModel[[1]]$Age
    names(MModel[[3]]) <- gsub("MAAT", "MAAT + MAP", names(MModel[[1]]))}
  
  #### Recup des stats par période ####
  Stats.phase.clim.param <- list()
  if(is.null(My_climate_name) == F){
    Stats.phase.clim <- data.frame(matrix(nrow = nrow(yo2), ncol = (ncol(MModel[[1]])-1), NA))
    row.names(Stats.phase.clim) <- yo2$Title.zone
    
    for(i in 1:length(MModel)){
      Mp <- MModel[[i]]
      colnames(Stats.phase.clim) <- gsub("\\..*", "", names(MModel[[i]])[2:ncol(MModel[[i]])])
      
      for(j in 1:nrow(yo2)){
        Stats.phase.clim[j,] <- colMeans(Mp[which(Mp$Age <= yo2$xmax[j] & Mp$Age >= yo2$xmin[j]),2:ncol(Mp)], na.rm = T)
  
      }
      
      Stats.phase.clim.param[[i]] <- t(Stats.phase.clim)
      if(is.null(Meta.data) == F){
        Keep <- Stats.phase.clim.param[[i]]
        Common.site <- intersect(row.names(Keep), row.names(Meta.data))
        Stats.phase.clim.param[[i]] <- cbind(Meta.data[Common.site,], Keep[Common.site,])
        }
      print(Stats.phase.clim.param)
    }
    if(Diff.MAAT.MAP == T){names(Stats.phase.clim.param) <- c("MAAT", "MAP", "DeltaParam")}
    if(Diff.MAAT.MAP == F){names(Stats.phase.clim.param) <- c("MAAT", "MAP")}}
  
  
  #### Data preparations ####
  MModel <- melt(MModel, id = "Age")
  MModel$Lake <- gsub("\\..*", "", MModel$variable)
  MModel$Lake <- factor(MModel$Lake, levels = Site.order)
  MModel$Param <- gsub("\\.", "", str_match(MModel$variable, "\\.\\s*(.*?)\\s*\\."))[,1]
  MModel$Proxy <- gsub(".*\\.", "", MModel$variable)
  MModel$L1[MModel$Proxy == "Pollen"] <- 1
  MModel$L1[MModel$Proxy == "brGDGT"] <- 2
  
  #### Graph settings ####
  if(missing(Limites)){Limites = c(min(MModel$Age), max(MModel$Age))}
  Ylab.size <- c(rep(0, (length(unique(MModel$Param))-1)),8)
  A = 1:11
  my_orange = list(brewer.pal(n = 11, "RdBu")[A[-c(4,5,6,7,8)]],#[A[-c(4,5,6,7,8)]],
                rev(brewer.pal(n = 11, "PuOr")[A[-c(4,5,6,7,8)]]),
                brewer.pal(n = 11, "Spectral")[A[-c(1,3,4,5,7,8,9,11)]])

  #### Main loop ####
  Plot.list <- list()
  for(i in 1:length(unique(MModel$Param))){
    Param.selected <- unique(MModel$Param)[i]
    Mplot <- MModel[MModel$Param == Param.selected,]
    #print(Mplot$Param)
    Alf <- c("(A) :", "(B) :" , "(C) :")
    Title.subplot <- paste(Alf[i], Param.selected, sep = " ")
    #### Color plot ####
    orange_palette = colorRampPalette(my_orange[[i]])
    my_orange.p = rev(orange_palette(length(seq(-10, 10, by = 1))))
    
    #### Plot ####
    sig <- (Mplot$Age[2] - Mplot$Age[1])/2
    Plot.list[[i]]  <-  ggplot(Mplot)+
                  #### Items ####
                  geom_rect(mapping = aes(xmin = Age - sig, xmax = Age + sig, ymin = L1-0.5, ymax = L1+0.5, colour = value), na.rm = T)+
                  # geom_rect(mapping = aes(xmin = Age - sig, xmax = Age + sig, ymin = L1-0.5, ymax = L1+0.5, fill = value), na.rm = T)+
                  ggtitle(Title.subplot)+
                  facet_grid(Lake ~ ., scales = "free") +
                  scale_color_gradientn(colours = my_orange.p,  na.value = "gray90")+
                  # scale_fill_gradientn(colours = my_orange.p,  na.value = "gray90")+
                  scale_y_continuous(name = NULL)+
                  scale_x_continuous(name = "Time (year cal. BP)", limits = c(Limites[1], Limites[2]))+
      
                  #### Zone clim ####
                  new_scale_color()+
                  new_scale_fill()+
                  scale_fill_manual(values = Rect.color.scale, guide = "none")+
                  scale_color_manual(values = Title.color.scale, guide = "none", name = NULL, labels = NULL, breaks = NULL, na.translate = FALSE)+
                  
                  My_climate_zone +
                  My_climate_name +
                  #### Theme ####
                  theme_bw()+
                  theme(axis.ticks.y = element_blank(), 
                        axis.text.y = element_blank(),
                        plot.title = element_text(face = "bold"),
                        legend.title = element_blank(),
                        panel.spacing = unit(0, "cm"),
                        strip.background = element_blank(),
                        panel.border = element_blank(),
                        panel.grid = element_blank(),
                        legend.direction = "horizontal",
                        legend.position = "top",
                        legend.justification = c("center"),               # left, top, right, bottom
                        legend.text = element_text(size = 8),
                        strip.text.y = element_text(angle = 0, hjust = 0, size = Ylab.size[i]),
                        plot.margin = unit(c(0,0,0,0), "lines")
                  )
  }

  #### Save plots and export ####
  Formula.patch <- paste(paste("Plot.list[[", seq(length(unique(MModel$Param))), "]]", sep = ""), collapse = " + ")
  Ptot <- eval(parse(text = Formula.patch))
  
  #### Add plot annotation ####
  Mplot <- Mplot[,4:7]
  Mplot <- Mplot[which(duplicated.data.frame(Mplot)==F),]
  
  if(is.null(My_climate_name) == F){
    Mplot <- rbind(Mplot, c(1, "Climate Periods", NA, NA))}
  if(Show.proxy.t == T){
    Padd  <-  ggplot(Mplot)+ geom_point(mapping = aes(x = 1, y = L1, colour = Proxy, shape = Proxy), size = 3)+
      #### Items ####
      facet_grid(Lake ~ ., scales = "free") +
      scale_y_discrete(name = NULL)+
      scale_x_continuous(name = NULL)+
      ggtitle("(D) : Proxy type")+
      #### Theme ####
      theme_bw()+
      theme(axis.ticks = element_blank(), 
            axis.text = element_blank(),
            legend.title = element_blank(),
            panel.spacing = unit(0, "cm"),
            plot.title = element_text(face = "bold"),
            plot.title.position = "plot",
            strip.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            legend.direction = "horizontal",
            legend.position = "top",
            legend.justification = c("center"),               # left, top, right, bottom
            legend.text = element_text(size = 8),
            strip.text.y = element_blank(),
            plot.margin = unit(c(0,0,0,0), "lines")
            
      )
    
    
    Ptot <- Ptot + Padd + plot_layout(nrow = 1, widths = c(3/10, 3/10, 3/10, 1/10))
    }

  #### Export plots #### 
  if(is.null(Save.plot) == F){
    if(is.null(W) == F & is.null(H) == F){ggsave(Ptot, file = Save.plot, width = W*0.026458333, height = H*0.026458333, units = "cm", limitsize = F)}
    else{ggsave(Save.plot)}}
  
  if(is.null(Save.Rds) == F){saveRDS(Stats.phase.clim.param, Save.Rds)}
  # print(Stats.phase.clim.param)
  return(Stats.phase.clim.param)
  
}

  
#### Application ####
Uzbekistan = T
if(Uzbekistan == T){
  #### Import datas ####
  Fazilman.TUDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_TUDB.Rds")
  Fazilman.COSTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_COSTDB.Rds")
  Fazilman.WASTDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_WASTDB.Rds")
  Fazilman.STDB <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_STDB.Rds")
  # Uz.eco <- read.table("Import/Uzbekistan/Site/Uz_surf_samples.csv", sep = ",", header = T, row.names = 1)
  # Mclim.core <- Uz.eco[Uz.eco$Lake_name != "",]
  # row.names(Mclim.core) <- Mclim.core$Lake_name 
  Mclim.core <- readRDS("Resultats/Uzbekistan/Export_pangaea/Fazilman_actual_param.Rds")
  
  
  names(Mclim.core)[names(Mclim.core) == "Lake_name"] <- "Name" 
  names(Mclim.core)[names(Mclim.core) == "Longitude"] <- "Long" 
  names(Mclim.core)[names(Mclim.core) == "Latitude"] <- "Lat" 
    
  #### Plots ####
  Full.models = F
  if(Full.models == T){
    FT.Uzbekistan <- Plot.FT.summary(FT = c(Fazilman = c(Fazilman.TUDB, Fazilman.COSTDB, Fazilman.WASTDB, Fazilman.STDB)),
                                 Cores = c("Fazilman"), Label.group = c("TUDB", "COSTDB", "WASTDB", "STDB"),
                                 Pclim = c("MAAT", "MTWAQ", "MAP", "Pspr"),
                                 Clim.lab = c("MAAT (°C)", "MTWAQ (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[spring] , (mm.yr^1)))),
                                 Model = c("WAPLS", "RF", "BRT", "MAT"),
                                 Select.interv = 1000, 
                                 Surf.val = Mclim.core,
                                 Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_all_FT.pdf",
                                 H = 750, W = 1400)}
  
  Selected.models.papier.Faz = T
  if(Selected.models.papier.Faz == T){
    FT.Fazilman <- Plot.FT.summary(FT = c(Fazilman = c(Fazilman.TUDB, Fazilman.COSTDB)),
                                Cores = c("Fazilman"),
                                Pclim = c("MAAT", "MAP"),
                                Label.group = c("TUDB", "COSTDB"),
                                Surf.val = Mclim.core,
                                Anomaly = F, Mono.core = T,
                                GDGT.plot.merge = T, Add.lim.space = F,
                                Clim.lab = c("MAAT (°C)", 
                                             expression(paste(MAP, (mm.yr^1))), 
                                             "MTWAQ (°C)", 
                                             expression(paste(P[spring] , (mm.yr^1)))),
                                Model = c("WAPLS", "MAT", "BRT"), 
                                Select.interv = 1000, Limites = c(-100, 10000),
                                Manual.y.val = list(MAAT.Fazilman = c(-3, 0, 3, 6, 9, 12), MAP.Fazilman = c(200, 250, 300, 350, 400, 450)),
                                Zone.clim = c(-100, 50, 50, 550, 650, 950, 1350, 1650, 1900, 2500, 3300, 3700),  #Feng et al., 2006 , 4000, 4400
                                Temp.zone = c("W", "D","Wt","D","Wt","C"), Manual.vlines = c(4200, 5820, 6300, 8200),
                                Zoom.box = c(-100, 500), Temp.col.alpha = 0.2,
                                Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_select_FT.pdf",
                                H = 800, W = 1000)
    saveRDS(FT.Fazilman, "Resultats/Uzbekistan/Climat/Reconstructions/FT_Fazilman.Rds")
  }
  
  Selected.models.papier.Faz.LateHol = F
  if(Selected.models.papier.Faz.LateHol == T){
    FT.Fazilman.LH <- Plot.FT.summary(FT = c(Fazilman = c(Fazilman.TUDB, Fazilman.COSTDB)),
                                Cores = c("Fazilman"),
                                Pclim = c("MAAT", "MAP"),
                                Label.group = c("TUDB", "COSTDB"),
                                Surf.val = Mclim.core,
                                Anomaly = F, 
                                GDGT.plot.merge = T, Add.lim.space = F,
                                Clim.lab = c("MAAT (°C)", 
                                             expression(paste(MAP, (mm.yr^1))), 
                                             "MTWAQ (°C)", 
                                             expression(paste(P[spring] , (mm.yr^1)))),
                                # Model = c("MAT", "BRT"),
                                Model = c("BRT"),
                                Select.interv = 1000, Limites = c(-100, 4400),
                                # Manual.y.val = list(MAAT.Ayrag = c(-4, -2, 0, 2), MAP.Ayrag = c(200, 250, 300, 350, 400, 450)),
                                Zone.clim = c(50, 550, 650, 950, 1350, 1650, 1900, 2500, 3300, 3900),  #Feng et al., 2006 , 4000, 4400
                                Temp.zone = c("C","W","C","W","C"), # , "W"
                                Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky"), #, "4.2ky"
                                Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_select_FT_LH.pdf",
                                H = 600, W = 1000)
    saveRDS(FT.Fazilman.LH, "Resultats/Uzbekistan/Climat/Reconstructions/FT_Fazilman_LH.Rds")
    }
  
  Selected.models.papier.Faz.LIA = F
  if(Selected.models.papier.Faz.LIA == T){
    FT.Fazilman.LIA <- Plot.FT.summary(FT = c(Fazilman = c(Fazilman.TUDB, Fazilman.COSTDB)),
                                      Cores = c("Fazilman"),
                                      Pclim = c("MAAT", "MAP"),
                                      Label.group = c("TUDB", "COSTDB"),
                                      Surf.val = Mclim.core,
                                      Anomaly = F, 
                                      GDGT.plot.merge = T, Add.lim.space = F,
                                      Clim.lab = c("MAAT (°C)", 
                                                   expression(paste(MAP, (mm.yr^1))), 
                                                   "MTWAQ (°C)", 
                                                   expression(paste(P[spring] , (mm.yr^1)))),
                                      # Model = c("MAT", "BRT"),
                                      Model = c("BRT"),
                                      Select.interv = 1000, Limites = c(-100, 300),
                                      # Manual.y.val = list(MAAT.Ayrag = c(-4, -2, 0, 2), MAP.Ayrag = c(200, 250, 300, 350, 400, 450)),
                                      Zone.clim = c(50, 550, 650, 950, 1350, 1650, 1900, 2500, 3300, 3900),  #Feng et al., 2006 , 4000, 4400
                                      Temp.zone = c("C","W","C","W","C"), # , "W"
                                      Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky"), #, "4.2ky"
                                      Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Fazilman/Fazilman_select_FT_500yr.pdf",
                                      H = 600, W = 1000)
    saveRDS(FT.Fazilman.LIA, "Resultats/Uzbekistan/Climat/Reconstructions/FT_Fazilman_500yr.Rds")
    }
  
  Full.Uzbekistan.FT = F
  if(Full.Uzbekistan.FT == T){
    FT.Synthe.ita <- Plot.FT.summary(#### Import ####
                                     FT = Ital.FT,#[-15],#[1:5],
                                     #### Settings ####
                                     Cores = row.names(Mclim.core),#[-19],
                                     # Cores = gsub(".MAT.EAPDB", "", names(Ital.FT[1:5])),
                                     Pclim = c("MAAT", "MAP"),
                                     Label.group = c("EAPDB"),
                                     Model = c("MAT"), #"WAPLS", 
                                     Surf.val = Mclim.core,
                                     Facet.T = F, Condensed = T, Anomaly = F, Phase.clim = T, Mono.core = F, Repel.T = F,
                                     GDGT.plot.merge = F, Add.lim.space = "F",
                                     Smooth.param = c(0.18, 0.25, 0.2, 0.15, 0.25, #5
                                                      0.2, 0.3, 0.15, 0.25, 0.25, #10
                                                      0.25, 0.38, 0.22, 0.20, 0.4, #15
                                                      0.18, 0.2, 0.2, 0.2), #19
                                     # Smooth.param = 0.6,
                                     Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^-1)))),
                                     Limites = c(0, 16000), Select.interv = 1000,
                                     # Zone.clim = c(0, 4500, 5000, 11000, 11700,15000),  #Feng et al., 2006
                                     # Temp.zone = c("C", "W","C"),
                                     # Name.zone = c("Late Hol.", "Early Hol.", "YD"),
                                     Zone.clim = c(0, 4200, 4200, 8200, 8200, 11700, 11700, 12900, 12900, 14700, 14700, 16000),  #Feng et al., 2006
                                     Temp.zone = c("C", "W","W", "C", "C", "C"),
                                     Name.zone = c("Late Hol.", "Mid. Hol.", "Early Hol.", "YD", "BA", "OD"),
                                     Save.plot = "Figures/Uzbekistan/Pollen/Func_trans/Full_Uzbekistan/FT_climat_Uzbekistan_V2.pdf",
                                     Save.Rds  = "Resultats/Uzbekistan/Pollen/Func_trans/Regional_synthese/FT_Uzbekistan_full_V2.Rds",
                                     H = 4500, W = 1500)
  }
  
  # PhaseClim.Uz <- readRDS("Resultats/Uzbekistan/Pollen/Func_trans/Regional_synthese/FT_Uzbekistan_full_V2.Rds")
  Bande.pollen.Uz = F
  if(Bande.pollen.Uz == T){
    Bande.ACA.pollen <- Plot.clim.phase(MFT = PhaseClimUz, Site.order = row.names(Mclim.core),
                                        Limites = c(0, 16000),
                                        Diff.MAAT.MAP = F,
                                        Meta.data = Mclim.core, Show.proxy.t = F,
                                        # Zone.clim = c(0, 4500, 5000, 11000, 11700,15000),  #Feng et al., 2006
                                        # Temp.zone = c("C", "W","C"),
                                        # Name.zone = c("Late Hol.", "Early Hol.", "YD"),
                                        Zone.clim = c(0, 4200, 4200, 8200, 8200, 11700, 11700, 12900, 12900, 14700, 14700, 16000),  #Feng et al., 2006
                                        Temp.zone = c("C", "W","W", "C", "C", "C"),
                                        Name.zone = c("Late Hol.", "Mid. Hol.", "Early Hol.", "YD", "BA", "OD"),
                                        Save.plot = "Figures/Uzbekistan/Climat/Reconstruction/Bande_clim_Uzbekistan_V2.pdf",
                                        Save.Rds  = "Resultats/Uzbekistan/Pollen/Func_trans/Regional_synthese/Phase_clim_Uzbekistan_FT_fit_V2.Rds",
                                        H = 800, W = 1200)}
}