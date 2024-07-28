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
Mongolia = F
if(Mongolia == T){
  #### Import Mongolian datas ####
  Ayrag.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_NMSDB.Rds")
  Ayrag.MDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_MDB.Rds")
  Ayrag.COSTDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_COST.Rds")
  Ayrag.EAPDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_EAPDB.Rds")
  
  D1L1.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1_NMSDB.Rds")
  D1L1.MDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1_MDB.Rds")
  D1L1.COSTDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1_COST.Rds")
  D1L1.EAPDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D1L1/D1L1_EAPDB.Rds")
  
  D3L6.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_NMSDB.Rds")
  D3L6.NMSDB.full  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_NMSDB_full.Rds")
  D3L6.NMSDB$MAT.NMSDB$MAAT <- D3L6.NMSDB.full$MAT$MAAT$fit.boot[,1]
  D3L6.MDB    <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_MDB.Rds")
  D3L6.COSTDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_COST.Rds")
  D3L6.EAPDB <- readRDS("Resultats/Mongolia/Pollen/Func_trans/D3L6/D3L6_EAPDB.Rds")
  
  Hubsugul_nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur_NMSDB.Rds")
  Hoton_Nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur_NMSDB.Rds")
  Dood_Nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur_NMSDB.Rds")
  Daba_Nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur_NMSDB.Rds")
  Gun_Nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_NMSDB.Rds")
  Achit_Nuur.NMSDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur_NMSDB.Rds")
  
  Hubsugul_nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Hubsugul_nuur/Hubsugul_nuur_COSTDB.Rds")
  Hoton_Nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Hoton_Nuur/Hoton_Nuur_COSTDB.Rds")
  Dood_Nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Dood_Nuur/Dood_Nuur_COSTDB.Rds")
  Daba_Nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Daba_Nuur/Daba_Nuur_COSTDB.Rds")
  Gun_Nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_COSTDB.Rds")
  Achit_Nuur.COSTDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Achit_Nuur/Achit_Nuur_COSTDB.Rds")
  
  Gun_Nuur.TAIGDB  <- readRDS("Resultats/Mongolia/Pollen/Func_trans/Gun_Nuur/Gun_Nuur_TAIGDB.Rds")
  #### Import Chinese datas ####
  Hurleg.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Hurleg/Hurleg_NMSDB.Rds")
  Kakitu.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Kakitu/Kakitu_NMSDB.Rds")
  Kanas.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Kanas/Kanas_NMSDB.Rds")
  Tianchi_Gaoligong.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong_NMSDB.Rds")
  Tianchi_Liupan.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan_NMSDB.Rds")
  Wenquan.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Wenquan/Wenquan_NMSDB.Rds")
  Zoige.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Zoige/Zoige_NMSDB.Rds")
  Bayanchagan.NMSDB  <- readRDS("Resultats/China/Pollen/Func_trans/Bayanchagan/Bayanchagan_NMSDB.Rds")
  
  Hurleg.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Hurleg/Hurleg_COSTDB.Rds")
  Kakitu.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Kakitu/Kakitu_COSTDB.Rds")
  Kanas.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Kanas/Kanas_COSTDB.Rds")
  Tianchi_Gaoligong.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Tianchi_Gaoligong/Tianchi_Gaoligong_COSTDB.Rds")
  Tianchi_Liupan.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Tianchi_Liupan/Tianchi_Liupan_COSTDB.Rds")
  Wenquan.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Wenquan/Wenquan_COSTDB.Rds")
  Zoige.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Zoige/Zoige_COSTDB.Rds")
  Bayanchagan.COSTDB  <- readRDS("Resultats/China/Pollen/Func_trans/Bayanchagan/Bayanchagan_COSTDB.Rds")
  
  #### Import Russian datas ####
  Baikal_Posolskoe.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Baikal/Baikal_NMSDB.Rds")
  Suollakh.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh_NMSDB.Rds")
  #Derput.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Derput/Derput_NMSDB.Rds") # /!\ ne marche pas !
  Kotokel.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel_NMSDB.Rds")
  Nuochaga.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_NMSDB.Rds")
  Akkol.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Akkol/Akkol_NMSDB.Rds")
  Chivyrkui.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Chivyrkui/Chivyrkui_NMSDB.Rds")
  Duguldzery.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Duguldzery/Duguldzery_NMSDB.Rds")
  Dulikha.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_NMSDB.Rds")
  Dzhangyskol.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dzhangyskol/Dzhangyskol_NMSDB.Rds")
  Grusha.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Grusha/Grusha_NMSDB.Rds")
  Kendegelukol.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kendegelukol/Kendegelukol_NMSDB.Rds")
  Kuchelga.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kuchelga.Bog/Kuchelga.Bog_NMSDB.Rds")
  Baikal_96.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.St..GC.3.96/Lake.Baikal.St..GC.3.96_NMSDB.Rds")
  Baikal_93.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.VER93.2.st.24GC/Lake.Baikal.VER93.2.st.24GC_NMSDB.Rds")
  Okunaika.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Okunaika/Okunaika_NMSDB.Rds")
  Tashkol.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Tashkol/Tashkol_NMSDB.Rds")
  Teletskoye.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Teletskoye.Lake.northern.Altai/Teletskoye.Lake.northern.Altai_NMSDB.Rds")
  Ukta_1.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.1/Ukta.1_NMSDB.Rds")
  Ukta_2.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.2/Ukta.2_NMSDB.Rds")
  Utinoye.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Utinoye/Utinoye_NMSDB.Rds")
  Uzunkol.NMSDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Uzunkol/Uzunkol_NMSDB.Rds")
  
  Baikal_Posolskoe.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Baikal/Baikal_COSTDB.Rds")
  Suollakh.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh_COSTDB.Rds")
  #Derput.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Derput/Derput_COSTDB.Rds") /!\ ne marche pas !
  Kotokel.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel_COSTDB.Rds")
  Nuochaga.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_COSTDB.Rds")
  Akkol.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Akkol/Akkol_COSTDB.Rds")
  Chivyrkui.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Chivyrkui/Chivyrkui_COSTDB.Rds")
  Duguldzery.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Duguldzery/Duguldzery_COSTDB.Rds")
  Dulikha.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_COSTDB.Rds")
  Dzhangyskol.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dzhangyskol/Dzhangyskol_COSTDB.Rds")
  Grusha.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Grusha/Grusha_COSTDB.Rds")
  Kendegelukol.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kendegelukol/Kendegelukol_COSTDB.Rds")
  #Khendyrkul.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Khendyrkul/Khendyrkul_COSTDB.Rds")
  Kuchelga.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kuchelga.Bog/Kuchelga.Bog_COSTDB.Rds")
  Baikal_96.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.St..GC.3.96/Lake.Baikal.St..GC.3.96_COSTDB.Rds")
  Baikal_93.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.VER93.2.st.24GC/Lake.Baikal.VER93.2.st.24GC_COSTDB.Rds")
  Okunaika.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Okunaika/Okunaika_COSTDB.Rds")
  Tashkol.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Tashkol/Tashkol_COSTDB.Rds")
  Teletskoye.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Teletskoye.Lake.northern.Altai/Teletskoye.Lake.northern.Altai_COSTDB.Rds")
  Ukta_1.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.1/Ukta.1_COSTDB.Rds")
  Ukta_2.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.2/Ukta.2_COSTDB.Rds")
  Utinoye.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Utinoye/Utinoye_COSTDB.Rds")
  Uzunkol.COSTDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Uzunkol/Uzunkol_COSTDB.Rds")
  
  Baikal_Posolskoe.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Baikal/Baikal_TAIGDB.Rds")
  Suollakh.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Suollakh/Suollakh_TAIGDB.Rds")
  #Derput.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Derput/Derput_TAIGDB.Rds") #/!\ ne marche pas !
  Kotokel.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kotokel/Kotokel_TAIGDB.Rds")
  Nuochaga.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Nuochaga/Nuochaga_TAIGDB.Rds")
  Akkol.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Akkol/Akkol_TAIGDB.Rds")
  Chivyrkui.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Chivyrkui/Chivyrkui_TAIGDB.Rds")
  Duguldzery.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Duguldzery/Duguldzery_TAIGDB.Rds")
  Dulikha.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_TAIGDB.Rds")
  Dzhangyskol.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dzhangyskol/Dzhangyskol_TAIGDB.Rds")
  Grusha.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Grusha/Grusha_TAIGDB.Rds")
  Kendegelukol.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kendegelukol/Kendegelukol_TAIGDB.Rds")
  #Khendyrkul.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Khendyrkul/Khendyrkul_TAIGDB.Rds")
  Kuchelga.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Kuchelga.Bog/Kuchelga.Bog_TAIGDB.Rds")
  Baikal_96.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.St..GC.3.96/Lake.Baikal.St..GC.3.96_TAIGDB.Rds")
  Baikal_93.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Lake.Baikal.VER93.2.st.24GC/Lake.Baikal.VER93.2.st.24GC_TAIGDB.Rds")
  Okunaika.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Okunaika/Okunaika_TAIGDB.Rds")
  Tashkol.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Tashkol/Tashkol_TAIGDB.Rds")
  Teletskoye.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Teletskoye.Lake.northern.Altai/Teletskoye.Lake.northern.Altai_TAIGDB.Rds")
  Ukta_1.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.1/Ukta.1_TAIGDB.Rds")
  Ukta_2.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Ukta.2/Ukta.2_TAIGDB.Rds")
  Utinoye.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Utinoye/Utinoye_TAIGDB.Rds")
  Uzunkol.TAIGDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Uzunkol/Uzunkol_TAIGDB.Rds")
  
  Dulikha.EAPDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_EAPDB.Rds")
  Dulikha.MDB  <- readRDS("Resultats/Russia/Pollen/Func_trans/Dulikha.Bog/Dulikha.Bog_MDB.Rds")
  
  
  #### Import Iran / stan datas ####
  Parishan.NMSDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Parishan/Parishan_NMSDB.Rds")
  Gomishan.NMSDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan_NMSDB.Rds")
  Maharlou.NMSDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou_NMSDB.Rds")
  Karakol.NMSDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_NMSDB.Rds")
  Kichikol.NMSDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_NMSDB.Rds")
  
  Parishan.COSTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Parishan/Parishan_COSTDB.Rds")
  Gomishan.COSTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan_COSTDB.Rds")
  Maharlou.COSTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou_COSTDB.Rds")
  Karakol.COSTDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_COSTDB.Rds")
  Kichikol.COSTDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_COSTDB.Rds")
  Aral.COSTDB  <- readRDS("Resultats/Kazakhstan/Pollen/Func_trans/Aral/Aral_COSTDB.Rds")
  
  Parishan.WASTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Parishan/Parishan_WASTDB.Rds")
  Gomishan.WASTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Gomishan/Gomishan_WASTDB.Rds")
  Maharlou.WASTDB  <- readRDS("Resultats/Iran/Pollen/Func_trans/Maharlou/Maharlou_WASTDB.Rds")
  Karakol.WASTDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Karakol/Karakol_WASTDB.Rds")
  Kichikol.WASTDB  <- readRDS("Resultats/Kirghizistan/Pollen/Func_trans/Kichikol/Kichikol_WASTDB.Rds")
  
  Shenkani.COSTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_COST.Rds")
  Shenkani.STDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_ST.Rds")
  Shenkani.WASTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_WAST.Rds")
  Kanli.COSTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_COST.Rds")
  Kanli.STDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_ST.Rds")
  Kanli.WASTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_WAST.Rds")
  Zarishat.COSTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat_COSTDB.Rds")
  Zarishat.STDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat_STDB.Rds")
  Zarishat.WASTDB  <- readRDS("Resultats/Armenia/Pollen/Func_trans/Zarishat/Zarishat_WASTDB.Rds")
  
  #### Extract data clim surface ####
  Extract.Clim = F
  if(Extract.Clim == T){
    Mcoord.core  <- data.frame(read.csv(file="Import/ACA/Site/Cores_coordinates.csv",sep=",",dec=".",header=T, row.names = 1))
    Cores.dist.ERAPDB  <- data.frame(read.csv(file="Import/World_DB/Pollen/ERAPDB/ACA/ERAPDB_ACA_site.csv", sep=",",dec=".",header=T,row.names=2))        # GDGT indexe Ayrag
    
    debugSource("Scripts/Climat_extract.R")
    Mclim.core <- Clim.param.extraction(M = Mcoord.core,
                                       All.param = T,
                                       Altitude = F,
                                       Season = T,
                                       Map.display = F,
                                       Clim.display = F,
                                       Nb.map = 2, W = 2000, H = 1000,
                                       Save.path = "Import/ACA/Site/Cores_coordinates_climat.csv"#,
                                       #Save.plot = "Figures/Mongolia/Maps/Surface_samples_sites.pdf"
                                       )
    
    }
   
  Import.Clim = T
  if(Import.Clim == T){
    Mcoord.core <- data.frame(read.csv(file = "Import/ACA/Site/DB/Cores_coordinates.csv",sep=",",dec=".",header=T,row.names=1))
    Mcoord.core.WC2 <- data.frame(read.csv(file = "Import/ACA/Site/DB/Cores_coordinates_climat.csv",sep=",",dec=".",header=T,row.names=1))
    Actual.val <- data.frame(MAAT = Mcoord.core[,4], MAP = Mcoord.core[,5], Psum = NA)
    #Actual.val <- cbind(Actual.val, Psum = Mcoord.core[,37])
    
    Actual.val[is.na(Actual.val$MAAT),1]<- Mcoord.core.WC2[is.na(Actual.val$MAAT),3]
    Actual.val[is.na(Actual.val$MAP),2] <- Mcoord.core.WC2[is.na(Actual.val$MAP),4]
    Actual.val$Psum <- Mcoord.core.WC2$Psum
    #Actual.val[["MAP"]] <- Mcoord.core[["MAP"]]
    #Actual.val[["SUMMERPR"]] <- Mcoord.core[["Psum"]]
    Actual.val[["Lat"]] <- Mcoord.core[["Latitude"]]
    Actual.val[["Long"]] <- Mcoord.core[["Longitude"]]
    row.names(Actual.val) <- row.names(Mcoord.core)
    
    }
  
  #### Plots ####
  Full.models = F
  if(Full.models == T){
    names(Ayrag.COSTDB) <- gsub("COST", "COSTDB", names(Ayrag.COSTDB))
    names(D3L6.COSTDB) <- gsub("COST", "COSTDB", names(D3L6.COSTDB))
    names(D1L1.COSTDB) <- gsub("COST", "COSTDB", names(D1L1.COSTDB))
    FT.Mongolia <- Plot.FT.summary(FT = c(Ayrag = c(Ayrag.NMSDB[c(2)], Ayrag.MDB, Ayrag.COSTDB, Ayrag.EAPDB),
                                          D1L1  = c(D1L1.NMSDB[c(1)], D1L1.MDB, D1L1.COSTDB, D1L1.EAPDB),
                                          D3L6  = c(D3L6.NMSDB[c(1)], D3L6.MDB, D3L6.COSTDB, D3L6.EAPDB),
                                          Dulikha  = c(Dulikha.NMSDB[c(1)], Dulikha.COSTDB, Dulikha.EAPDB, Dulikha.MDB)
                                          ),
                                   Surf.val = Actual.val,
                                   Cores = c("Dulikha", "Ayrag", "D3L6", "D1L1"),
                                   Cores.lab = c("Dulikha (Baikal)", "Ayrag (Khangai)", "D3L6 (Altai)", "D1L1 (Altai)"),
                                   Pclim = c("MAAT", "MAP"), Facet.T = F, GDGT.plot.merge = T, Repel.T = T,
                                   Label.group = c("NMSDB"), 
                                   Smooth.param = 0.4, Condensed = T, Add.lim.space = F,
                                   Clim.lab = c("(A) MAAT (°C)", expression(paste((B) ~~ MAP, (mm.yr^-1))), expression(paste(P[summer] , (mm.yr^1)))),
                                   Model = c("WAPLS", "MAT"), # "RF", "BRT",
                                   Manual.y.val = list(MAAT.Dulikha = c(-0.75, -0.5, -0.25, 0), MAP.Dulikha = c(400, 425, 450, 475),
                                                       MAAT.Ayrag = c(-0.75, -0.25, 0.25, 0.75), MAP.Ayrag = c(250,300,350),
                                                       MAAT.D3L6 = c(0, 0.5, 1, 1.5), MAP.D3L6 = c(150, 200, 250, 300),
                                                       MAAT.D1L1 = c(0.5, 1, 1.5, 2), MAP.D1L1 = c(150, 200, 250)
                                                       ),
                                   Select.interv = 500,
                                   Limites = c(0, 4200),
                                   Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3300, 3900, 4000, 4400),  #Feng et al., 2006
                                   Temp.zone = c("C","W","C","W","C", "W"),
                                   # Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                                   Save.plot = "Figures/Mongolia/Pollen/Func_trans/Regional_synthese/Full_models_Mongolia.pdf",
                                   H = 800, W = 1000)}
  
  Selected.models.papier2 = F
  if(Selected.models.papier2 == T){
    FT.Ayrag <- Plot.FT.summary(FT = c(Ayrag = c(Ayrag.NMSDB, Ayrag.MDB, Ayrag.COSTDB, Ayrag.EAPDB),
                                                D1L1  = c(D1L1.NMSDB, D1L1.MDB, D1L1.COSTDB),
                                                D3L6  = c(D3L6.NMSDB, D3L6.MDB, D3L6.COSTDB, D3L6.EAPDB)),
                                         Cores = c("Ayrag"),
                                         Pclim = c("MAAT", "MAP"),
                                         Label.group = c("NMSDB", "COST"),
                                         Surf.val = Actual.val,
                                         Anomaly = F,
                                         GDGT.plot.merge = T, Add.lim.space = F,
                                         Smooth.param = 0.35,
                                         #Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^1)))),
                                         Clim.lab = c("(A) MAAT (°C)",
                                                      expression(paste(~ (B), MAP, (mm.yr^-1))), 
                                                      expression(paste(P[summer ], (mm.yr^-1)))),
                                         Model = c("WAPLS", "MAT", "BRT"),
                                         Select.interv = 500,
                                         Limites = c(0, 4000),
                                         Manual.y.val = list(MAAT.Ayrag = c(-4, -2, 0, 2), MAP.Ayrag = c(200, 250, 300, 350, 400, 450)),
                                         #Zone.clim = c(160, 420, 730, 1030),  # Behling
                                         # Zone.clim = c(50, 550, 650, 950),     # Dugerdil
                                         # Name.zone = c("LIA", "WMP"),
                                         # Temp.zone = c("C","W"),
                                         Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3300, 3900),  #Feng et al., 2006 , 4000, 4400
                                         Temp.zone = c("C","W","C","W","C"), # , "W"
                                         Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky"), #, "4.2ky"
                                         Save.plot = "Figures/Mongolia/Pollen/Func_trans/Ayrag/Ayrag_func_trans.pdf",
                                         H = 800, W = 1000)}
  
  Selected.models.fig = F
  if(Selected.models.fig == T){
    names(Ayrag.COSTDB) <- gsub("COST", "COSTDB", names(Ayrag.COSTDB))
    names(D3L6.COSTDB) <- gsub("COST", "COSTDB", names(D3L6.COSTDB))
    FT.Mongolia.selec.fig <- Plot.FT.summary(FT = c(Ayrag = c(Ayrag.NMSDB, Ayrag.MDB, Ayrag.COSTDB, Ayrag.EAPDB),
                                                D1L1  = c(D1L1.NMSDB, D1L1.MDB, D1L1.COSTDB),
                                                D3L6  = c(D3L6.NMSDB, D3L6.MDB, D3L6.COSTDB, D3L6.EAPDB),
                                                Dulikha  = c(Dulikha.NMSDB, Dulikha.COSTDB, Dulikha.EAPDB, Dulikha.MDB, Dulikha.TAIGDB),
                                                Gun_nuur  = c(Gun_Nuur.NMSDB, Gun_Nuur.COSTDB)),
                                         Cores = c("Dulikha", "D3L6"),
                                         Cores.lab = c("Dulikha (Baikal)", "D3L6 (Altai)"),
                                         Pclim = c("MAAT", "MAP"),
                                         Surf.val = Actual.val,
                                         Anomaly = F,
                                         Label.group = c("NMSDB", "COSTDB", "MDB", "EAPDB"),
                                         GDGT.plot.merge = T, Smooth.param = c(0.45, 0.45), Add.lim.space = T,
                                         Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^-1)))),
                                         #Clim.lab = c("MAAT (°C)", expression(paste(P[summer ], (mm.yr^1)))),
                                         Model = c("WAPLS", "MAT"),
                                         Select.interv = 500,
                                         Limites = c(-60, 5000),
                                         Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3300, 3900, 4000, 4400),  #Feng et al., 2006
                                         Temp.zone = c("C","W","C","W","C", "W"),
                                         Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                                         Save.plot = "Figures/Mongolia/Pollen/Func_trans/Regional_synthese/Selected_models_Mongolia_papier1_V2.pdf",
                                         H = 1200, W = 1600)} #avant H = 800, a terme H = 1200
  
  Selected.models.hol = F
  if(Selected.models.hol == T){
    FT.Mongolia.selec.hol <- Plot.FT.summary(FT = c(Ayrag = c(Ayrag.NMSDB, Ayrag.MDB, Ayrag.COSTDB),
                                                    D1L1  = c(D1L1.NMSDB, D1L1.MDB, D1L1.COSTDB),
                                                    D3L6  = c(D3L6.NMSDB, D3L6.MDB, D3L6.COSTDB)),
                                             Cores = c("D3L6", "Ayrag"),
                                             Pclim = c("MAAT", "MAP", "Psum"),
                                             Surf.val = Actual.val,
                                             Anomaly = F,
                                             GDGT.plot.merge = T,
                                             Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[summer ], (mm.yr^1)))),
                                             #Clim.lab = c("MAAT (°C)", expression(paste(P[summer ], (mm.yr^1)))),
                                             Model = c("RF", "WAPLS", "BRT", 'MAT'),
                                             Select.interv = 500,
                                             Limites = c(0, 9500),
                                             #Zone.clim = c(160, 420, 730, 1030),  # Behling
                                             # Zone.clim = c(50, 550, 650, 950),     # Dugerdil
                                             # Name.zone = c("LIA", "WMP"),
                                             # Temp.zone = c("C","W"),
                                             Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 5000, 8000),  #Feng et al., 2006
                                             Temp.zone = c("C","W","C","W", "W"),
                                             Name.zone = c("LIA", "WMP", "DACP", "RWP", "Hol.Opt"),
                                             Save.plot = "Figures/Mongolia/Pollen/Func_trans/Regional_synthese/Mongolia_Holocene.pdf",
                                             H = 2200, W = 2800)}
  
  Synthe.ACA = F
  if(Synthe.ACA == T){
    FT.Synthe.ACA <- Plot.FT.summary(#### Import ####
                                      FT = c(Baikal_Posolskoe = c(Baikal_Posolskoe.TAIGDB[c(1)]), #Baikal_Posolskoe.COSTDB[5], Baikal_Posolskoe.NMSDB[6], 
                                      Suollakh = c(Suollakh.TAIGDB[c(1)]), #Suollakh.NMSDB, Suollakh.COSTDB
                                      Kotokel = c(Kotokel.TAIGDB[c(1)]), #, Kotokel.COSTDB[3:5]Kotokel.NMSDB[6], 
                                      Nuochaga = c(Nuochaga.TAIGDB[c(6)]), # Nuochaga.COSTDB, Nuochaga.NMSDB[6], 
                                      Akkol = c(Akkol.COSTDB[6]), #Akkol.NMSDB, , Akkol.TAIGDB[c(1,5)]
                                      Chivyrkui = c(Chivyrkui.TAIGDB[1]), #Chivyrkui.NMSDB, Chivyrkui.COSTDB
                                      Duguldzery = c(Duguldzery.TAIGDB[c(6)]), #Duguldzery.NMSDB, Duguldzery.COSTDB, 
                                      Dulikha = c(Dulikha.NMSDB[c(5)]), #Dulikha.COSTDB, Dulikha.NMSDB, Dulikha.TAIGDB[c(5)], 
                                      Dzhangyskol = c(Dzhangyskol.COSTDB[1]), # Dzhangyskol.NMSDB, Dzhangyskol.TAIGDB[1]
                                      Grusha = c(Grusha.COSTDB[6]), #Grusha.NMSDB, Grusha.TAIGDB
                                      Kendegelukol = c(Kendegelukol.TAIGDB[c(6)]), #Kendegelukol.NMSDB, Kendegelukol.COSTDB
                                      Kuchelga = c(Kuchelga.NMSDB[c(6)]), # Kuchelga.COSTDB, Kuchelga.NMSDB, Kuchelga.TAIGDB[c(1)], 
                                      Baikal_96 = c(Baikal_96.NMSDB[c(1)]), # Baikal.96.COSTDB, Baikal.96.NMSDB, Baikal_96.TAIGDB[c(6)],
                                      Baikal_93 = c(Baikal_93.NMSDB[c(6)]), #Baikal.93.NMSDB,  Baikal_93.COSTDB, Baikal_93.TAIGDB[6]
                                      Okunaika = c(Okunaika.TAIGDB[c(5)]), #Okunaika.NMSDB, Okunaika.COSTDB, 
                                      Teletskoye = c(Teletskoye.TAIGDB[c(5)]), #Teletskoye.NMSDB, Teletskoye.COSTDB, 
                                      Ukta_1 = c(Ukta_1.TAIGDB[5]), #Ukta.1.NMSDB , Ukta.1.TAIGDB, Ukta_1.COSTDB[1]
                                      Ukta_2 = c(Ukta_1.TAIGDB[5]), #Ukta.2.NMSDB,  Ukta.2.TAIGDB, Ukta_2.COSTDB[1]
                                      Utinoye = c(Utinoye.NMSDB[c(1)]),# Utinoye.COSTDB), Utinoye.TAIGDB
                                      Uzunkol = c(Uzunkol.TAIGDB[2]), #Uzunkol.NMSDB, Uzunkol.COSTDB[c(1,5)], 
                                      Tashkol = c(Tashkol.TAIGDB[1]), #Tashkol.NMSDB, Tashkol.COSTDB[1], 
                                      
                                      Achit_nuur = c(Achit_Nuur.NMSDB[6]), # Achit_Nuur.COSTDB[6],
                                      Gun_nuur = c(Gun_Nuur.NMSDB[1]), #, Gun_Nuur.COSTDB
                                      Ayrag = c(Ayrag.NMSDB[2]), # Ayrag.COSTDB
                                      #Hubsugul = c(Hubsugul_nuur.NMSDB, Hubsugul_nuur.COSTDB),
                                      Dood_nuur = c(Dood_Nuur.COSTDB[6]), #Dood_Nuur.NMSDB,
                                      Daba_nuur = c(Daba_Nuur.NMSDB[2]), #, Daba_Nuur.COSTDB
                                      Hoton_nuur = c(Hoton_Nuur.COSTDB[6]), #Hoton_Nuur.NMSDB[6],
                                      D1L1  = c(D1L1.NMSDB[1]), # , D1L1.COSTDB
                                      D3L6  = c(D3L6.NMSDB[1]),#  D3L6.COSTDB
                                      Kanas = c(Kanas.COSTDB[2]), # Kanas.NMSDB
                                      Karakol = c(Karakol.NMSDB[5]), # Karakol.WASTDB , Karakol.COSTDB[5]
                                      Kichikol = c(Kichikol.COSTDB[c(6)]), # Kichikol.WASTDB, Kichikol.NMSDB,
                                      Wenquan = c(Wenquan.COSTDB[c(6)]), #Wenquan.NMSDB,
                                      Kakitu = c(Kakitu.NMSDB, Kakitu.COSTDB),
                                      Hurleg = c(Hurleg.COSTDB[2]), #Hurleg.NMSDB[2],
                                      Zoige = c(Zoige.NMSDB[5]), #, Zoige.COSTDB[c()]
                                      Bayanchagan = c(Bayanchagan.NMSDB[6]), #, Bayanchagan.COSTDB
                                      Tianchi_Liupan = c(Tianchi_Liupan.COSTDB[c(2)]), #Tianchi_Liupan.NMSDB,
                                      Tianchi_Gaoligong = c(Tianchi_Gaoligong.COSTDB), #Tianchi_Gaoligong.NMSDB
                                      Parishan = c(Parishan.WASTDB[1]), # Parishan.NMSDB, Parishan.COSTDB,
                                      Maharlou = c(Maharlou.WASTDB[2]), # Maharlou.NMSDB, Maharlou.COSTDB,
                                      Gomishan = c(Gomishan.WASTDB[6]), # Gomishan.NMSDB, Gomishan.COSTDB
                                      Aral = c(Aral.COSTDB[c(6)]),
                                      Zarishat = c(Zarishat.COSTDB[1]), #Zarishat.WASTDB, Zarishat.STDB
                                      Kanli = c(Kanli.COSTDB[c(6)]), #, Kanli.STDB, Kanli.WASTDB
                                      Shenkani = c(Shenkani.COSTDB[2]) #, Shenkani.STDB, Shenkani.WASTDB
                                      ),
                                     #### Settings ####
                                     Cores = c("Nuochaga", "Suollakh", # Derput,
                                               "Okunaika", "Ukta_1", "Ukta_2",
                                               "Duguldzery", "Baikal_Posolskoe", "Chivyrkui", "Baikal_96", # Khendyrkul,
                                               "Kotokel", "Kuchelga", "Dulikha", "Baikal_93",
                                               "Dood_nuur", "Gun_nuur",# "Hubsugul",
                                               "Ayrag", "Daba_nuur", "Achit_nuur",
                                               "Teletskoye", "Akkol", "Grusha",
                                               "Kendegelukol", "Uzunkol", "Tashkol", "Dzhangyskol",
                                               "Hoton_nuur", "D3L6", "D1L1",
                                               "Kanas", "Wenquan",
                                               "Karakol", "Kichikol", # Kakitu ?
                                               "Hurleg", "Zoige", "Tianchi_Liupan",
                                               "Utinoye", "Bayanchagan", #"Tianchi_Gaoligong",
                                               "Aral",
                                               "Zarishat", "Shenkani", "Kanli",
                                               "Gomishan", "Maharlou", "Parishan"
                                               ),
                                     Pclim = c("MAAT", "MAP", "SUMMERPR"),
                                     Label.group = c("NMSDB", "COSTDB", "COST", "WASTDB", "TAIGDB", "ST", "STDB"),
                                     Model = c("MAT", "RF", "BRT", "WAPLS"), #"WAPLS", 
                                     Surf.val = Actual.val, 
                                     Facet.T = F, Condensed = T, Anomaly = F, Phase.clim = T, Mono.core = F, Repel.T = F,
                                     GDGT.plot.merge = F, Add.lim.space = "F", Smooth.param = c(0.32, 0.15,              # Kantchaka
                                                                                                0.25, 0.30, 0.30,        # Nord Baikal
                                                                                                0.18, 0.18, 0.18, 0.25,  # Centre Baikal
                                                                                                0.18, 0.22, 0.18, 0.16,  # Sud Baikal
                                                                                                0.20, 0.20,              # Khentii
                                                                                                0.40, 0.20, 0.25,        # Arkhangai
                                                                                                0.30, 0.18, 0.14,        # Altai russie
                                                                                                0.20, 0.18, 0.15, 0.21,  # Altai Russie
                                                                                                0.20, 0.30, 0.28,        # Altai Mongole
                                                                                                0.18, 0.20,              # Altai Chine
                                                                                                0.30, 0.28,              # Tian Shan
                                                                                                0.17, 0.18, 0.30,        # Qaidam
                                                                                                0.20, 0.18,              # Chine Océanique
                                                                                                0.15,
                                                                                                0.18, 0.18, 0.20,        #Arménie
                                                                                                0.25, 0.25, 0.25         # Iran
                                                                                                ),
                                     Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[sum], (mm.yr^1)))),
                                     Limites = c(-100, 5000), Select.interv = 500,
                                     Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4400),  #Feng et al., 2006
                                     Temp.zone = c("C","W","C","W","C", "W"),
                                     Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                                     Save.plot = "Figures/ACA/Climat/Reconstruction/FT_climat_ACA.pdf",
                                     Save.Rds  = "Resultats/ACA/Pollen/Func_trans/Regional_synthese/FT_ACA_select_sites.Rds",
                                     H = 7000, W = 1000)
    }
  
  ACA.phase.clim = F
  if(ACA.phase.clim == T){
    #### Import data ####
    PhaseClimAca <- readRDS("Resultats/ACA/Pollen/Func_trans/Regional_synthese/FT_ACA_select_sites.Rds")
    PhaseClimAca.gdgt <- readRDS("Resultats/ACA/GDGT/Regional_synthese/Phase_clim_ACA_gdgt_fit.Rds")
    PhaseClimAca <-PhaseClimAca[c(1,2)]
    Cores.ordin = c("Nuochaga", "Suollakh", "Utinoye", "Bayanchagan",# Derput,
                    "Okunaika", "Ukta_1", "Ukta_2",
                    "Duguldzery", "Baikal_Posolskoe", "Chivyrkui", "Baikal_96", # Khendyrkul,
                    "Kotokel", "Kuchelga", "Dulikha", "Baikal_93",
                    "Dood_nuur", "Gun_nuur",# "Hubsugul",
                    "Ayrag", "Daba_nuur", "Achit_nuur",
                    "Teletskoye", "Akkol", "Grusha",
                    "Kendegelukol", "Uzunkol", "Tashkol", "Dzhangyskol",
                    "Hoton_nuur", "D3L6", "D1L1",
                    "Kanas", "Wenquan",
                    "Karakol", "Kichikol", # Kakitu ?
                    "Hurleg", "Zoige", "Tianchi_Liupan",
                      #"Tianchi_Gaoligong",
                    "Aral",
                    "Zarishat", "Shenkani", "Kanli",
                    "Gomishan", "Maharlou", "Parishan"
    )
    
    #### Calcul axe parallèle à l'EASM ####
    x1 = 55
    x2 = 125
    y1 = 30
    y2 = 60
    Vect = c((x2-x1), (y2-y1))
    BH = ((Actual.val$Long-x1)*Vect[1] + (Actual.val$Lat - y1)*Vect[2])/(sqrt(Vect[1]^2 + Vect[2]^2))
    xH = x1 + (BH*Vect[1])/(sqrt(Vect[1]^2 + Vect[2]^2))
    yH = y1 + (BH*Vect[2])/(sqrt(Vect[1]^2 + Vect[2]^2))
    Actual.val$Lat.proj <- xH
    Actual.val$Long.proj <- yH
    
    #Cores.ordin <- row.names(Actual.val[order(Actual.val$Latitude, decreasing = T),])
    #Cores.ordin <- row.names(Actual.val[order(Actual.val$Long.proj, decreasing = T),])
    #Cores.ordin <- c(row.names(Cores.dist.EASML)[order(Cores.dist.EASML$Dist_to_EASLM)], "Achit_nuur")
    Limites = c(-100, 5000)
    
    #### Apply ####
    Bande.pollen = F
    if(Bande.pollen == T){
      Bande.ACA.pollen <- Plot.clim.phase(MFT = PhaseClimAca, Site.order = Cores.ordin, Limites = Limites, Diff.MAAT.MAP = T,
                                   Meta.data = Actual.val[,4:5],
                      Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3500, 3900, 4000, 4400),  #Feng et al., 2006
                      Temp.zone = c("C","W","C","W","C", "W"),
                      Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                      Save.plot = "Figures/ACA/Climat/Reconstruction/Bande_clim_ACA.pdf",
                      Save.Rds  = "Resultats/ACA/Pollen/Func_trans/Regional_synthese/Phase_clim_ACA_FT_fit.Rds",
                      H = 800, W = 1200)}
    
    Bande.gdgt = F
    if(Bande.gdgt == T){
      Bande.ACA.gdgt <- Plot.clim.phase(MGDGT = PhaseClimAca.gdgt, Site.order = Cores.ordin, Limites = Limites, Diff.MAAT.MAP = T,
                                   Meta.data = Actual.val[,4:5],
                                   Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3500, 3900, 4000, 4400),  #Feng et al., 2006
                                   Temp.zone = c("C","W","C","W","C", "W"),
                                   Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                                   Save.plot = "Figures/ACA/Climat/Reconstruction/Bande_clim_ACA_gdgt.pdf",
                                   Save.Rds  = "Resultats/ACA/Pollen/Func_trans/Regional_synthese/Phase_clim_ACA_gdgt_fit.Rds",
                                   H = 400, W = 1200)}
      
    Bande.both = T
    if(Bande.both == T){
      Bande.ACA.both <- Plot.clim.phase(MFT = PhaseClimAca,  MGDGT = PhaseClimAca.gdgt, Site.order = Cores.ordin, Limites = Limites, Diff.MAAT.MAP = T,
                                        Meta.data = Actual.val[,4:5],
                                        Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3300, 3900, 4000, 4400),  #Feng et al., 2006
                                        Temp.zone = c("C","W","C","W","C", "W"),
                                        Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky"),
                                        Save.plot = "Figures/ACA/Climat/Reconstruction/Bande_clim_ACA_gdgt_ft.pdf",
                                        Save.Rds  = "Resultats/ACA/Pollen/Func_trans/Regional_synthese/Phase_clim_ACA_gdgt_ft_fit.Rds",
                                        H = 1200, W = 1200)}
  }
  
  #### Plot FT Baikal Siberia ####
  Baikal.fig = F
  if(Baikal.fig == T){ #### Papier Mariam / Cheima ####
    FT.Mongolia.selec.fig <- Plot.FT.summary(FT = c(
                                                    Kuchelga = c(Kuchelga.NMSDB[c(2)]), #Kuchelga.COSTDB, , Kuchelga.TAIGDB[c(2,6)] Kuchelga.COSTDB, Kuchelga.NMSDB, Kuchelga.TAIGDB[c(1)], 
                                                    Kotokel = c(Kotokel.NMSDB[c(6)]), #,Kotokel.TAIGDB[c(6)],  Kotokel.COSTDB[3:5]Kotokel.NMSDB[6], , Kotokel.COSTDB[3:5], Kotokel.NMSDB[6]
                                                    Baikal_93 = c(Baikal_93.TAIGDB[c(6)]), #Baikal.93.NMSDB, Baikal_93.NMSDB[c(2,6)], Baikal_93.COSTDB, Baikal_93.TAIGDB[6]
                                                    Dulikha  = c(Dulikha.NMSDB, Dulikha.COSTDB, Dulikha.TAIGDB), # Dulikha.NMSDB[c(2)] Dulikha.COSTDB, Dulikha.EAPDB, Dulikha.MDB, , Dulikha.TAIGDB
                                                    Gun_nuur  = c(Gun_Nuur.NMSDB[c(2)])),  #, Gun_Nuur.COSTDB
                                             Cores = c("Dulikha"),
                                             #Cores.lab = c("Kuchelga", "Kotokel", "Baikal_93", "Dulikha", "Gun_nuur"),
                                             #Cores.lab = c("Dulikha (Baikal)", "D3L6 (Altai)"),
                                             Pclim = c("MAAT", "MAP"),
                                             Surf.val = Actual.val,
                                             Anomaly = F, Facet.T = T,
                                             Label.group = c("NMSDB", "TAIGDB", "COSTDB"),
                                             GDGT.plot.merge = F, #Smooth.param = c(0.45, 0.45), Add.lim.space = T,
                                             Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^-1)))),
                                             #Clim.lab = c("MAAT (°C)", expression(paste(P[summer ], (mm.yr^1)))),
                                             Model = c("MAT", "WAPLS", "BRT"), 
                                             Select.interv = 1500,
                                             Limites = c(-60, 11000),
                                             Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4400, 5000, 8000, 10000, 11000),  #Feng et al., 2006
                                             Temp.zone = c("C","W","C","W","C", "W", "W", "C"),
                                             Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky", "Hol Opt", "TG"),
                                             Save.plot = "Figures/Russia/Pollen/Regional_synthese/Baikal_FT_V2.pdf",
                                             H = 1600, W = 1600)} #avant H = 800, a terme H = 1200
  
  Baikal.fig.V3 = F
  if(Baikal.fig.V3 == T){ #### Papier Mariam / Cheima ####
    FT.Dulikha.Gun.selec.fig <- Plot.FT.summary(FT = c(
                                Kuchelga = c(Kuchelga.NMSDB[c(2)]), #Kuchelga.COSTDB, , Kuchelga.TAIGDB[c(2,6)] Kuchelga.COSTDB, Kuchelga.NMSDB, Kuchelga.TAIGDB[c(1)], 
                                Kotokel = c(Kotokel.NMSDB[c(6)]), #,Kotokel.TAIGDB[c(6)],  Kotokel.COSTDB[3:5]Kotokel.NMSDB[6], , Kotokel.COSTDB[3:5], Kotokel.NMSDB[6]
                                Baikal_93 = c(Baikal_93.TAIGDB[c(6)]), #Baikal.93.NMSDB, Baikal_93.NMSDB[c(2,6)], Baikal_93.COSTDB, Baikal_93.TAIGDB[6]
                                Dulikha  = c(Dulikha.NMSDB, Dulikha.COSTDB, Dulikha.TAIGDB), # Dulikha.NMSDB[c(2)] Dulikha.COSTDB, Dulikha.EAPDB, Dulikha.MDB, , Dulikha.TAIGDB
                                Gun_nuur  = c(Gun_Nuur.COSTDB, Gun_Nuur.NMSDB, Gun_Nuur.TAIGDB)),  #, Gun_Nuur.COSTDB
                                Cores = c("Dulikha", "Gun_nuur"),
                                #Cores.lab = c("Kuchelga", "Kotokel", "Baikal_93", "Dulikha", "Gun_nuur"),
                                #Cores.lab = c("Dulikha (Baikal)", "D3L6 (Altai)"),
                                Pclim = c("MAAT", "MAP"),
                                Surf.val = Actual.val,
                                Anomaly = F, Facet.T = T,
                                Label.group = c("NMSDB", "COSTDB", "TAIGDB"),
                                GDGT.plot.merge = F, #Smooth.param = c(0.45, 0.45), Add.lim.space = T,
                                Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^-1)))),
                                #Clim.lab = c("MAAT (°C)", expression(paste(P[summer ], (mm.yr^1)))),
                                Model = c("MAT", "WAPLS", "BRT"), 
                                Select.interv = 1500,
                                Limites = c(-60, 11000),
                                Zone.clim = c(50, 550, 650, 950, 1600, 1800, 1900, 2500, 3600, 3800, 4000, 4400, 5000, 8000, 10000, 11000),  #Feng et al., 2006
                                Temp.zone = c("C","W","C","W","C", "W", "W", "C"),
                                Name.zone = c("LIA", "WMP", "DACP", "RWP", "3.5ky", "4.2ky", "Hol Opt", "TG"),
                                Save.plot = "Figures/Russia/Pollen/Regional_synthese/Baikal_FT_V3.pdf",
                                H = 2000, W = 1600)} #avant H = 800, a terme H = 1200
  
}

Armenia = F
if(Armenia == T){
  #### Import datas ####
  Shenkani.WAST <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_WAST.Rds")
  Shenkani.ST   <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_ST.Rds")
  Shenkani.COST <- readRDS("Resultats/Armenia/Pollen/Func_trans/Shenka/Shenka_COST.Rds")
  
  Kanli.WAST <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_WAST.Rds")
  Kanli.ST   <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_ST.Rds")
  Kanli.COST <- readRDS("Resultats/Armenia/Pollen/Func_trans/Kanli/Kanli_COST.Rds")
  
  #### Extract data clim surface ####
  Extract.Clim = F
  if(Extract.Clim == T){
    Mcoord.core  <- data.frame(read.csv(file="Import/Mongolia/Site/Cores_coordinates.csv",sep=",",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE))
    source("Scripts/Climat_extract.R")
    Mclim.core <- Clim.param.extraction(M = as.matrix(Mcoord.core), 
                                        All.param = F, 
                                        Altitude = F,
                                        Season = F, 
                                        Map.display = F, 
                                        Clim.display = F, 
                                        Nb.map = 2, W = 2000, H = 1000,
                                        Save.path = "Import/Mongolia/Site/Cores_coordinates_climat.csv"#,
                                        #Save.plot = "Figures/Mongolia/Maps/Surface_samples_sites.pdf"
    )}
  Import.Clim = F
  if(Import.Clim == T){
    Mcoord.core <- data.frame(read.csv(
      file = "Import/Mongolia/Site/Cores_coordinates_climat.csv",
      sep=",",dec=".",header=T,row.names=1))
  }
  #### Plots ####
  Full.models = T
  if(Full.models == T){
    FT.Armenia <- Plot.FT.summary(FT = c(Kanli = c(Kanli.WAST, Kanli.ST, Kanli.COST),
                                         Shenkani = c(Shenkani.WAST, Shenkani.ST, Shenkani.COST)),
                                   Cores = c("Kanli", "Shenkani"),
                                   Pclim = c("MAAT", "MTWA", "MAP", "SUMMERPR"),
                                   Clim.lab = c("MAAT (°C)", "MTWA (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[summer] , (mm.yr^1)))),
                                   Model = c("WAPLS", "RF", "BRT", "MAT"),
                                   Select.interv = 1000,
                                   Limites = c(-55, 10239),
                                   Temp.zone = c("G","W","C","W","G"),
                                   Zone.clim = c(796, 2300, 3000, 4800, 5200, 7800, 8000, 8700, 9000, 9696),
                                   Name.zone = c("Z1", "Z2", "Z3", "Z4", "Z5"),
                                   Save.plot = "Figures/Armenia/Pollen/Func_trans/Regional_synthese/Full_models_Armenia.pdf",
                                   H = 1700, W = 1400)}
  
  Kanli.models = T
  if(Kanli.models == T){
    FT.Armenia.Kanli <- Plot.FT.summary(FT = c(Kanli = c(Kanli.WAST, Kanli.ST, Kanli.COST),
                                         Shenkani = c(Shenkani.WAST, Shenkani.ST, Shenkani.COST)),
                                  Cores = c("Kanli"),
                                  Pclim = c("MAAT", "MAP"), 
                                  Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[summer] , (mm.yr^1)))),
                                  Model = c("WAPLS", "MAT", "RF", "BRT"),
                                  Select.interv = 1000,
                                  Limites = c(0, 10000),
                                  Temp.zone = c("G","W","C","W","G"),
                                  Zone.clim = c(796, 2300, 3000, 4800, 5200, 7800, 8000, 8700, 9000, 9696),
                                  Name.zone = c("Z1", "Z2", "Z3", "Z4", "Z5"),
                                  Save.plot = "Figures/Armenia/Pollen/Func_trans/Regional_synthese/Kanli_models_Armenia.pdf",
                                  H = 1000, W = 900)}
  
  Shenkani.models = T
  if(Shenkani.models == T){
    FT.Armenia.Shenkani <- Plot.FT.summary(FT = c(Kanli = c(Kanli.WAST, Kanli.ST, Kanli.COST),
                                               Shenkani = c(Shenkani.WAST, Shenkani.ST, Shenkani.COST)),
                                        Cores = c("Shenkani"),
                                        Pclim = c("MAAT", "MAP"), 
                                        Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[summer] , (mm.yr^1)))),
                                        Model = c("WAPLS", "MAT", "RF", "BRT"),
                                        Select.interv = 1000,
                                        Limites = c(0, 10000),
                                        Temp.zone = c("G","W","C","W","G"),
                                        Zone.clim = c(796, 2300, 3000, 4800, 5200, 7800, 8000, 8700, 9000, 9696),
                                        Name.zone = c("Z1", "Z2", "Z3", "Z4", "Z5"),
                                        Save.plot = "Figures/Armenia/Pollen/Func_trans/Regional_synthese/Shenkani_models_Armenia.pdf",
                                        H = 1000, W = 900)}
  
  
  
}

Italia = F
if(Italia == T){
  #### Import datas ####
  Eufemia.EAPDB <- readRDS("Resultats/Italia/Pollen/Func_trans/Eufemia/Eufemia_EAPDB.Rds")

  files.marion  <- list.files(path = c("Import/Italia/Pollen/Func_trans/Fonction_transfert_Blache_Marion/"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
  files.marion.MAAT <- files.marion[grep("TANN", files.marion)]
  files.marion.MAP <- files.marion[grep("PANN", files.marion)]
  
  Lake.names.MAP <- gsub(".*\\/", "", files.marion.MAP)
  Lake.names.MAP <- gsub("\\..*", "", Lake.names.MAP)
  Lake.names.MAP <- gsub("\\s\\(.*", "", Lake.names.MAP)
  Lake.names.MAP <- gsub("\\s", "_", Lake.names.MAP)
  Lake.names.MAAT <- gsub(".*\\/", "", files.marion.MAAT)
  Lake.names.MAAT <- gsub("\\..*", "", Lake.names.MAAT)
  Lake.names.MAAT <- gsub("\\s\\(.*", "", Lake.names.MAAT)
  Lake.names.MAAT <- gsub("\\s", "_", Lake.names.MAAT)
  
 
  
  Ital.FT.MAAT <- lapply(files.marion.MAAT, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ";", dec = ",")
  Ital.FT.MAP <- lapply(files.marion.MAP, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ";", dec = ",")
  
  names(Ital.FT.MAAT) <- Lake.names.MAAT
  names(Ital.FT.MAP) <- Lake.names.MAP
  
  Ital.FT.MAAT <- lapply(Ital.FT.MAAT, function(x){
    x <- x[2]
    x[2] <- as.numeric(row.names(x))
    names(x) <- c("MAAT", "Age") 
    row.names(x) <- paste("S", seq(1:nrow(x)), sep = "-")
    x <- x[c(2,1)]
    return(x)
    })
  Ital.FT.MAP <- lapply(Ital.FT.MAP, function(x){
    x <- x[2]
    x[2] <- row.names(x)
    names(x) <- c("MAP", "Age") 
    row.names(x) <- paste("S", seq(1:nrow(x)), sep = "-")
    x <- x[c(2,1)]
    return(x)
    })
  
  #Ital.FT.MAP <- c(Ital.FT.MAP, Lago_del_Segrino = data.frame(rep(NA, 95)))
  # names(Ital.FT.MAP)[length(names(Ital.FT.MAP))] <- "Lago_del_Segrino"
  Ital.FT.MAP <- Ital.FT.MAP[names(Ital.FT.MAAT)]
  
  Ital.FT <- Map(data.frame, Ital.FT.MAAT, Ital.FT.MAP)                # Merge two lists
  Ital.FT <- lapply(Ital.FT, function(x) x[,-c(3)])
  # Ital.FT = list(MAT.EAPDB = Ital.FT)
  
  # # test
  # Lake.names.MAAT[grep("Accesa", Lake.names.MAAT)] <- "Lago_Accesa"
  # Lake.names.MAP[grep("Accesa", Lake.names.MAP)] <- "Lago_Accesa"
  # names(Ital.FT) <- Lake.names.MAP
  # 
  
  names(Ital.FT) <- paste(names(Ital.FT), "MAT.EAPDB", sep = ".") 
  
  #### Extract data clim surface ####
  Extract.Clim = F
  if(Extract.Clim == T){
    Mcoord.core  <- data.frame(read.csv(file="Import/Italia/Pollen/Sites/ss302coordonnees.csv",sep="\t",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE))
    names(Mcoord.core) <- c("Long", "Lat", "Elevation")
    source("Scripts/Climat_extract.R")
    Mclim.core <- Clim.param.extraction(M = as.matrix(Mcoord.core), 
                                        All.param = T, 
                                        Altitude = F,
                                        Season = T, 
                                        Map.display = F, 
                                        Clim.display = F, 
                                        Nb.map = 2, W = 2000, H = 1000,
                                        Save.path = "Import/Italia/Pollen/Sites/ss302coordonnees_wc2.csv"#,
                                        #Save.plot = "Figures/Mongolia/Maps/Surface_samples_sites.pdf"
    )}
  
  Import.Clim = T
  if(Import.Clim == T){
    Mcoord.core <- data.frame(read.csv(file = "Import/Italia/Pollen/Func_trans/Fonction_transfert_Blache_Marion/Valeurs latitude-longitude.csv", sep=";",dec=",",header=T,row.names=1))
    Mcoord.core$Name <- row.names(Mcoord.core)
    Mclim.core <- data.frame(read.csv(file = "Import/Italia/Pollen/Func_trans/Fonction_transfert_Blache_Marion/Valeurs_climatique_actuelle.csv", sep=";",dec=",",header=T,row.names=1))
    Mclim.core <- merge(Mclim.core, Mcoord.core, by = "Name")
    Mclim.core$Name <- gsub("\\s", "_", Mclim.core$Name)
    Mclim.core$Long <- as.numeric(Mclim.core$Long)
    Mclim.core$Lat <- as.numeric(Mclim.core$Lat)
    
    # Mclim.core$Name[!gsub(".MAT.EAPDB", "", names(Ital.FT)) %in% Mclim.core$Name]
    row.names(Mclim.core) <- Mclim.core$Name
    
    Mclim.core <- Mclim.core[order(Mclim.core$Lat, decreasing = T),]
    
    }
  #### Plots ####
  Full.models = T
  if(Full.models == T){
    FT.Italia <- Plot.FT.summary(FT = c(Eufemia = c(Eufemia.EAPDB)),
                                  Cores = c("Eufemia"),
                                  Pclim = c("MAAT", "MTWA", "MAP", "SUMMERPR"),
                                  Clim.lab = c("MAAT (°C)", "MTWA (°C)", expression(paste(MAP, (mm.yr^1))), expression(paste(P[summer] , (mm.yr^1)))),
                                  Model = c("WAPLS", "RF", "BRT", "MAT"),
                                  Select.interv = 500,
                                  Save.plot = "Figures/Italia/Pollen/Func_trans/Eufemia/Eufemia_EAPDB_sorted.pdf",
                                  H = 750, W = 1400)}
  
  Full.Italia.FT = T
  if(Full.Italia.FT == T){
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
                                     Save.plot = "Figures/Italia/Pollen/Func_trans/Full_Italia/FT_climat_Italia_V2.pdf",
                                     Save.Rds  = "Resultats/Italia/Pollen/Func_trans/Regional_synthese/FT_Italia_full_V2.Rds",
                                     H = 4500, W = 1500)
  }
  
  PhaseClimIt <- readRDS("Resultats/Italia/Pollen/Func_trans/Regional_synthese/FT_Italia_full_V2.Rds")
  Bande.pollen.it = T
  if(Bande.pollen.it == T){
    Bande.ACA.pollen <- Plot.clim.phase(MFT = PhaseClimIt, Site.order = row.names(Mclim.core), 
                                        Limites = c(0, 16000), 
                                        Diff.MAAT.MAP = F,
                                        Meta.data = Mclim.core, Show.proxy.t = F,
                                        # Zone.clim = c(0, 4500, 5000, 11000, 11700,15000),  #Feng et al., 2006
                                        # Temp.zone = c("C", "W","C"),
                                        # Name.zone = c("Late Hol.", "Early Hol.", "YD"),
                                        Zone.clim = c(0, 4200, 4200, 8200, 8200, 11700, 11700, 12900, 12900, 14700, 14700, 16000),  #Feng et al., 2006
                                        Temp.zone = c("C", "W","W", "C", "C", "C"),
                                        Name.zone = c("Late Hol.", "Mid. Hol.", "Early Hol.", "YD", "BA", "OD"),
                                        Save.plot = "Figures/Italia/Climat/Reconstruction/Bande_clim_italia_V2.pdf",
                                        Save.Rds  = "Resultats/Italia/Pollen/Func_trans/Regional_synthese/Phase_clim_italia_FT_fit_V2.Rds",
                                        H = 800, W = 1200)}
}

Mediterranee.nord = F
if(Mediterranee.nord == T){
  #### Import datas ####
  files.lea.it  <- list.files(path = c("Resultats/Italia/Pollen/Func_trans/Func_trans_Lea"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
  files.lea.fr  <- list.files(path = c("Resultats/France/Pollen/Func_trans"), pattern = ".csv", full.names = T, all.files = T, recursive = T)
  files.lea <- c(files.lea.fr, files.lea.it)
  # files.lea <- files.lea.fr
  files.lea <- files.lea[!grepl("_SEP_", files.lea)]
  
  Lake.names <- gsub(".*\\/", "", files.lea)
  Lake.names <- gsub("_i.csv", ".csv", Lake.names)
  Lake.names <- gsub("\\..*", "", Lake.names)
  Lake.names <- stringi::stri_replace_last_regex(Lake.names, "_", ".")
  Model.keep <- gsub(".*\\.", "", Lake.names)
  Lake.names <- gsub("\\..*", "", Lake.names)
  Lake.names <- stringi::stri_replace_last_regex(Lake.names, "_", ".")
  Calib.keep <- gsub(".*\\.", "", Lake.names)
  Lake.names <- gsub("\\..*", "", Lake.names)
  Lake.names <- paste(Lake.names, Model.keep, Calib.keep, sep = ".")
  Lake.names <- gsub("-", "_", Lake.names)
  
  Med.FT <- lapply(files.lea, read.csv, header = T, stringsAsFactors = F, row.names = 1, sep = ",", dec = ".")
  names(Med.FT) <- Lake.names
  
  #### Extract data clim surface ####
  Extract.Clim = F
  if(Extract.Clim == T){
    Mcoord.core.fr  <- data.frame(read.csv(file="Import/France/Pollen/Sites/Pollen_coord_Zone1_Lea.csv",sep=";",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE))
    Mcoord.core.it  <- data.frame(read.csv(file="Import/Italia/Pollen/Sites/Pollen_coord_Zone2_Lea.csv",sep=";",dec=".",header=T, row.names = 1, stringsAsFactors = FALSE))
    
    names(Mcoord.core.fr)[c(3,4,5)] <- c("Lat", "Long", "Elevation")
    row.names(Mcoord.core.fr) <- Mcoord.core.fr$Name
    Mcoord.core <- rbind(Mcoord.core.it, Mcoord.core.fr[c(3:5)])
    
    write.table(Mcoord.core, file = "Import/Med/Pollen/Sites/Pollen_coord_med_Lea.csv", row.names=T, col.names=NA, sep=",", dec = ".")
  
    source("Scripts/Climat_extract.R")
    Mclim.core <- Clim.param.extraction(M = as.matrix(Mcoord.core), 
                                        All.param = T, Chelsa = F, MAF = F, Biome = F, Aridity = T, Seasonality = T,   
                                        Altitude = T, Land.cover = F, 
                                        Season = T, 
                                        Map.display = T, 
                                        Clim.display = T, 
                                        Nb.map = 1, W = 2000, H = 1000,
                                        Save.path = "Import/Med/Pollen/Sites/Pollen_coord_med_Lea_wc.csv",
                                        Save.plot = "Figures/Med/Maps/Surface_samples_sites.pdf"
    )}
  
  Import.Clim = T
  if(Import.Clim == T){
    Mcoord.core <- data.frame(read.csv(file = "Import/Med/Pollen/Sites/Pollen_coord_med_Lea.csv", sep=",",dec=".",header=T,row.names=1))
    Mcoord.clim <- data.frame(read.csv(file = "Import/Med/Pollen/Sites/Pollen_coord_med_Lea_wc.csv", sep=",",dec=".",header=T,row.names=1))
    Mclim.core <- left_join(rownames_to_column(Mcoord.clim), rownames_to_column(Mcoord.core), by = c("rowname" = "rowname"))
    Mclim.core <- subset(Mclim.core, select = -c(Lat, Long))
    Mclim.core$rowname <- gsub("\\s", "_", Mclim.core$rowname)
    Mclim.core$Longitude <- as.numeric(Mclim.core$Longitude)
    Mclim.core$Latitude <- as.numeric(Mclim.core$Latitude)
    # Mclim.core <- Mclim.core[order(Mclim.core$Latitude),]
    Mclim.core <- Mclim.core[order(Mclim.core$Elevation),]
    
    
    # Mclim.core <- Mclim.core[c(1,5,8,7,6,3,2)]
    # names(Mclim.core) <- c("Name", "MAAT", "MTCOM", "MTWAM", "MAP", "Lat", "Long")
    names(Mclim.core)[1] <- c("Name")
    Mclim.core$Name <- gsub("-", "_", Mclim.core$Name)
    row.names(Mclim.core) <- Mclim.core$Name
    
    
    
  }
  
  #### Mise-a-jour MA ####
  Maj.MA = F
  if(Maj.MA == T){
    files.MA  <- list.files(path = c("Import/France/Pollen/MA_France"), pattern = "ages.txt", full.names = T, all.files = T, recursive = T)
    lMA <- lapply(files.MA, read.table, header = T, stringsAsFactors = F, row.names = 1, sep = "\t", dec = ".")
    Lake.names.MA <- gsub(".*\\/", "", files.MA)
    Lake.names.MA <- gsub("_ages\\..*", "", Lake.names.MA)
    Lake.names.MA <- gsub("_[[:digit:]]", "", Lake.names.MA)
    Lake.names.MA <- gsub("[[:digit:]]", "", Lake.names.MA)
    names(lMA) <- Lake.names.MA
    
    for(j in 1:length(names(lMA))){
      for(i in 1:length(names(Med.FT))){
        if(grepl(Lake.names.MA[j], Lake.names[i]) == T){
          Med.FT[[i]]$Age <- lMA[[j]]$mean
          print(Lake.names.MA[j])
          print(Lake.names[i])
          # print(Med.FT[[i]]$Age)
          # print(lMA[[j]]$mean)
        }}}
    }
  
  #### Plots FTs ####
  Full.Italia.FT = T
  if(Full.Italia.FT == T){
    # Med.FT <- Med.FT[c(1,7,31,57,71,81)]
    # Med.FT <- Med.FT[!grepl("TEMPSCAND", names(Med.FT))]
    # Med.FT <- Med.FT[!grepl("WAPLS", names(Med.FT))]
    
    Cores = gsub("\\.[^.]*$", "", names(Med.FT))
    Cores = unique(gsub("\\.[^.]*$", "", Cores))
    
    for(i in 1:length(names(Med.FT))){
      if(max(Med.FT[[i]]$Age) > 100){
          Med.FT[[i]]$Age <- Med.FT[[i]]$Age/1000
      }
      }
    
    
    FT.Synthe.med <- Plot.FT.summary(#### Import ####
                                     FT = Med.FT,#[1:5],#[-15],#[1:5],
                                     Cores = unique(Cores),
                                     Pclim = c("MAAT", "MAP"),
                                     Label.group = c("EAPDB", "MEDTEMP", "TEMPSCAND"),
                                     Model = c("WAPLS", "BRT", "MAT"), #"WAPLS", 
                                     #### Settings ####
                                     Surf.val = Mclim.core,
                                     Facet.T = F, Condensed = F, Anomaly = F, Phase.clim = F,
                                     Mono.core = F, Repel.T = F, Mean.models = T,
                                     GDGT.plot.merge = F, Add.lim.space = "F",
                                     Only.fit = F, Smooth.sd = F,
                                     # Smooth.param = c(0.18, 0.25, 0.2, 0.15, 0.25, #5
                                     #                  0.2, 0.3, 0.15, 0.25, 0.25, #10
                                     #                  0.25, 0.38, 0.22, 0.20, 0.4, #15
                                     #                  0.18, 0.2, 0.2, 0.2), #19
                                     Smooth.param = 0.30,
                                     Clim.lab = c("MAAT (°C)", expression(paste(MAP, (mm.yr^-1)))),
                                     Limites = c(0, 12),
                                     Select.interv = 1,
                                     # Zone.clim = c(0, 4.5, 5, 11, 11.7,15),  #Feng et al., 2006
                                     # Temp.zone = c("C", "W","C"),
                                     # Name.zone = c("Late Hol.", "Early Hol.", "YD"),
                                     # Zone.clim = c(0, 4200, 4200, 8200),  #Feng et al., 2006
                                     # Temp.zone = c("C", "W"), 
                                     # Name.zone = c("Late Hol.", "Mid. Hol."),
                                     Save.plot = "Figures/Med/Pollen/Func_trans/FT_Med_full.pdf",
                                     Save.Rds  = "Resultats/Med/Pollen/Func_trans/Regional_synthese/FT_Med_full.Rds",
                                     H = 7000, W = 1500)
                                     # H = 1000, W = 1500)
  }
  
  PhaseClimIt <- readRDS("Resultats/Med/Pollen/Func_trans/Regional_synthese/FT_Med_full.Rds")
  for(i in 1:length(PhaseClimIt)){
    PhaseClimIt[[i]]$Age <- PhaseClimIt[[i]]$Age*1000
    # print(i)
  }

  #### Plots code barre climat ####
  Bande.pollen.it = F
  if(Bande.pollen.it == T){
    Bande.ACA.pollen <- Plot.clim.phase(MFT = PhaseClimIt, Site.order = row.names(Mclim.core), 
                                        # Limites = c(0, 15),
                                        Diff.MAAT.MAP = F,
                                        Meta.data = Mclim.core, Show.proxy.t = F,
                                        # Zone.clim = c(0, 2000, 5000, 8000),  #Feng et al., 2006
                                        Zone.clim = c(0, 1450, 1450, 2750, 2750, 4100, 4100, 7850),  #Harding 2013
                                        Temp.zone = c("C", "W", "C", "W"),
                                        Name.zone = c("Modern times", "Roman empire", "Bronze Age", "Neolithic"),
                                        # Zone.clim = c(0, 4.2, 4.2, 8.2, 8.2, 11.7, 11.7, 12.9, 12.9, 14.7, 14.7, 15),  #Feng et al., 2006
                                        # Temp.zone = c("C", "W","W", "C", "C", "C"),
                                        # Name.zone = c("Late Hol.", "Mid. Hol.", "Early Hol.", "YD", "BA", "OD"),
                                        Save.plot = "Figures/Med/Climat/Reconstruction/Bande_clim_italia_V2.pdf",
                                        Save.Rds  = "Resultats/Med/Pollen/Func_trans/Regional_synthese/Phase_clim_Med_FT_fit.Rds",
                                        H = 700, W = 1200)
    
    
    Check <- PhaseClimIt[[1]]
    Check <- melt(Check, id = "Age")
    
    p <- ggplot(Check, aes(x = Age, y = value)) + geom_point()+ facet_wrap(vars(variable), ncol = 1)
    
    W = 500
    H = 4000
    ggsave("Figures/Med/Climat/Reconstruction/Check_smooth.pdf", width = W*0.026458333, height = H*0.026458333, units = "cm", limitsize = F)
    
    }
  
}


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