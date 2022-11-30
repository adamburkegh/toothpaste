library(dplyr)
library(pals)
library(stringi)
library(ggplot2)
library(gridExtra)

exportPic <- TRUE

prepfig <- function(fprefix,logname, width=30, height=20, mar=c(1,1,1,1))
{
  if (exportPic){
    par(mar=mar)
    png( paste(workingPath,fprefix,"_",logname,".png",sep=""), 
         res=300, width=width, height=height, units='cm')
  }
}

postfig <- function()
{
  if (exportPic){
    dev.off()
  }
}




em_vs_ap_graph_plot <- function(workingPath, picName, rundata, ctLog){
  emcolName = "EARTH_MOVERS_LIGHT_COVERAGE"
  apcolName = "ALPHA_PRECISION_UNRESTRICTED"
  xlim=c(0,1)
  ylim=c(0,1)
  
  bpo <- rundata %>% 
      filter (baseLog == ctLog) #  %>% 
      # filter (shortId == 'rsd' | shortId == 'tmh')
  emtb <- as.numeric(bpo[[emcolName]])
  bpo[[emcolName]] <- emtb
  aptb <- as.numeric(bpo[[apcolName]])
  bpo[[apcolName]] <- aptb
  
  
  # byLog <- bpo %>% group_by (shortId)

  #   
  # sembyLog <- byLog %>% summarise(
  #   n     = n(),
  #   max   = max( .data[[emcolName]] ),
  #   mean 	= mean( .data[[emcolName]] )
  # )
  # 
  # sapbyLog <- byLog %>% summarise(
  #   n     = n(),
  #   max   = max( .data[[apcolName]] ),
  #   mean 	= mean( .data[[apcolName]] )
  # )
  # 

  # bnames <- sbyLog$shortId
  prepfig(picName,ctLog)
  
  pch=19

  plot(
    x = emtb,
    y = aptb,
    # main= paste("Earth Movers vs Alpha-Precision ", ctLog),
    main= paste(ctLog),
    # names.arg = bnames,
    xlab="Earth Movers",
    ylab="Alpha Precision",
    # horiz=FALSE, cex.names=0.8, las=2,
    xlim = xlim, ylim = ylim,
    col=bpo$col,
    pch=pch)

  postfig()
}

# ggplot version
em_vs_ap_graph <- function(workingPath, picName, rundata, ctLog){
  emcolName = "EARTH_MOVERS_LIGHT_COVERAGE"
  apcolName = "ALPHA_PRECISION_UNRESTRICTED"

  bpo <- rundata %>% 
    filter (baseLog == ctLog)   %>% 
    filter (shortId == 'rsd' | shortId == 'tmh')
  emtb <- as.numeric(bpo[[emcolName]])
  bpo[[emcolName]] <- emtb
  aptb <- as.numeric(bpo[[apcolName]])
  bpo[[apcolName]] <- aptb
  
  res <- ggplot(bpo, 
                aes(y=EARTH_MOVERS_LIGHT_COVERAGE, 
                    x=ALPHA_PRECISION_UNRESTRICTED,
                    col=ShortId)) + 
    geom_point() +
    xlim(0,1) + ylim(0,1) + 
    ggtitle(ctLog) +
    ylab("tEMSC0.8") + xlab("alpha precision")
  # + stat_ellipse()
  res
}


export_graph <- function (workingPath, picName, bpo, 
		ctMeasure, ctLog, ctShortMeasure, colName, ylim)
{
	
	bpo <- rundata %>% filter (baseLog == ctLog)
	tb <- as.numeric(bpo[[colName]])
	bpo[[colName]] <- tb

	byLog <- bpo %>% group_by (shortId)

	sbyLog <- byLog %>% summarise(
			n     = n(),
			max   = max( .data[[colName]] ),
	  		mean 	= mean( .data[[colName]] )
		)

	bnames <- sbyLog$shortId
	prepfig(picName,ctLog)

	barplot( 
		sbyLog$mean, 
		main= paste(ctMeasure, ctLog), 
		names.arg = bnames, 
		ylab=ctShortMeasure,
		horiz=FALSE, cex.names=0.8, las=2,
		ylim = ylim,
		col=alphabet())
	postfig()
}

em_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Earth Movers", 
		  ctLog = ctLog,
		  ctShortMeasure = "tEMSC 0.8",
		  colName = "EARTH_MOVERS_LIGHT_COVERAGE",
		  ylim=c(0,1))
}



entct_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entity Count", 
		  ctLog = ctLog,
		  ctShortMeasure = "|P| + |T|",
		  colName = "MODEL_ENTITY_COUNT",
		  ylim=c(0,3000))
}

edgect_graph <- function(workingPath, picName, rundata, ctLog){
	export_graph( workingPath, picName = picName, 
		  rundata, 
		  ctMeasure = "Entity Count", 
		  ctLog = ctLog,
		  ctShortMeasure = "|F|",
		  colName = "MODEL_EDGE_COUNT",
		  ylim=c(0,90))
}

workingPath = "c:/Users/burkeat/bpm/toothpaste-public/toothpaste/var/"

rundata = read.csv( paste(workingPath,"2022_jn.psv", sep=""), 
			sep ="|", strip.white=TRUE)

names(rundata) <- gsub("\\.", "", names(rundata))
rundata <- rundata %>% select (-X)

clncreators <- recode(rundata$ShortId,
			     "bce-fodina" = "fork-fodina",
           "bce-inductive" = "fork-inductive",
           "bce-split" = "fork-split",
           "rssmt" = "rsd")

rundata$shortId <- clncreators

baseLogs <- stri_sub(rundata$Log,0,-4)

rundata$baseLog = baseLogs

rundata <- rundata %>% filter (shortId != "split")


# logs <- unique(baseLogs)
# logs <- c("BPIC2013 closed")
# logs <- c("BPIC2018 control")
# logs <- c("BPIC2018 reference")
logs <- c("rtfm")
# logs <- c("teleclaims")

emappicName   <- paste("emap", gsub(" ","_", tolower(logs)), sep="") 


rundata$col = factor(rundata$shortId)

# count <- 1
# for (log in logs){
  em_vs_ap_graph( workingPath, picName = empicName[count], 
			  rundata, ctLog = log )
#	entct_graph( workingPath, picName = entcpicName[count], 
#		  rundata, ctLog = log )
#	edgect_graph( workingPath, picName = edgepicName[count], 
#		  rundata, ctLog = log )
	#duration_graph( workingPath, picName = durpicName[count], 
	#	  rundata, ctLog = log )
#	count= count +1
#}
g1 <- em_vs_ap_graph( workingPath, picName = "", 
                  rundata, ctLog = "rtfm" )
g2 <- em_vs_ap_graph( workingPath, picName = "", 
                rundata, ctLog = "teleclaims" )
g3 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2013 closed" )
g4 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2018 control" )
g5 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2018 reference" )
g6 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "sepsis" )
fullgrid <- grid.arrange(g1,g2,g3,g4,g5,g6) # ,ncol=2,nrow=2)


if (exportPic){
  ggsave(file=paste(workingPath,'tpres.png'),fullgrid)
}


