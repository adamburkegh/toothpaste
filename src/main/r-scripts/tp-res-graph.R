library(dplyr)
library(tidyr)
library(pals)
library(stringi)
library(ggplot2)
library(gridExtra)
library(readr)
library(rPref)

exportPic <- FALSE
# Export doesn't work - crude save from display in this script

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
    xlab="Earth Movers (EM)",
    ylab="Existential Precision (XPU)",
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
    filter (baseLog == ctLog)    %>% 
    filter (ShortId == 'GDT_SPN' | ShortId == 'toothpaste' | ShortId == 'trace' | 
            ShortId == 'walign-inductive' | ShortId == 'wfreq-split' | 
            ShortId == 'wpairscale-split' )
  emtb <- as.numeric(bpo[[emcolName]])
  bpo[[emcolName]] <- emtb
  aptb <- as.numeric(bpo[[apcolName]])
  bpo[[apcolName]] <- aptb
  
  res <- ggplot(bpo, 
                aes(y=EARTH_MOVERS_LIGHT_COVERAGE, 
                    x=ALPHA_PRECISION_UNRESTRICTED,
                    col=ShortId, shape=ShortId)) + 
    geom_point() +
    xlim(0,1) + ylim(0,1) + 
    ggtitle(ctLog) +
    ylab("Earth Movers (EM)") + xlab("Existential Precision (XPU)") 
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
		  ctShortMeasure = "EM",
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

# workingPath = "c:/Users/Adam/bpm/toothpaste3/toothpaste/var/"
workingPath = "c:/Users/burkeat/bpm/toothpaste-public/toothpaste/var/"

rundata = read.csv( paste(workingPath,"2022_jn.psv", sep=""), 
			sep ="|", strip.white=TRUE)

names(rundata) <- gsub("\\.", "", names(rundata))
rundata <- rundata %>% select (-X)

clncreators <- recode(rundata$ShortId,
           "align-inductive" = "walign-inductive",
			     "bce-fodina" = "fork-fodina",
           "bce-inductive" = "fork-inductive",
           "bce-split" = "fork-split",
			     "fe-split" = "wfreq-split",
			     "msaprh-split" = "wpairscale-split",
           "rssmt" = "GDT_SPN",
			     "tmh" = "toothpaste")


rundata$ShortId <- clncreators

baseLogs <- sub(' k.','', rundata$Log) 

baseLogs <- recode(baseLogs, "sepsis" = "Sepsis",
                             "rtfm"   = "Road Traffic Fines",
                             "teleclaims" = "Teleclaims")

rundata$baseLog = baseLogs

rundata$TotalDuration = rundata$MinerDuration + rundata$EstimatorDuration

# rundata <- rundata %>% filter (shortId != "split")


# logs <- unique(baseLogs)
# logs <- c("BPIC2013 closed")
# logs <- c("BPIC2018 control")
# logs <- c("BPIC2018 reference")
# logs <- c("rtfm")
# logs <- c("teleclaims")

# deletable?
# rundata$col = factor(rundata$shortId)

# count <- 1
# for (log in logs){
#  em_vs_ap_graph( workingPath, picName = empicName[count], 
#			  rundata, ctLog = log )
#	entct_graph( workingPath, picName = entcpicName[count], 
#		  rundata, ctLog = log )
#	edgect_graph( workingPath, picName = edgepicName[count], 
#		  rundata, ctLog = log )
	#duration_graph( workingPath, picName = durpicName[count], 
	#	  rundata, ctLog = log )
#	count= count +1
#}
g1 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2013 closed" )
g2 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2018 control" )
g3 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "BPIC2018 reference" )
g4 <- em_vs_ap_graph( workingPath, picName = "", 
                   rundata, ctLog = "Road Traffic Fines" )
g5 <- em_vs_ap_graph( workingPath, picName = "", 
                      rundata, ctLog = "Sepsis" )
g6 <- em_vs_ap_graph( workingPath, picName = "", 
                 rundata, ctLog = "Teleclaims" )
fullgrid <- grid.arrange(g1,g2,g3,g4,g5,g6) # ,ncol=2,nrow=2)
#fullgrid <- grid.arrange(g5) # ,ncol=2,nrow=2)


if (exportPic){
  ggsave(file=paste(workingPath,'tpres.png'),fullgrid)
}


rf <- function(an,rnddigits=2){
  format(round(an, digits=rnddigits), nsmall = rnddigits) 
}


# Runtime and entity count  export
# 
rundata %>%
  select(
    ShortId,
    MinerDuration,
    EstimatorDuration,
    TotalDuration,
    MODEL_ENTITY_COUNT,
    EARTH_MOVERS_LIGHT_COVERAGE,
    ALPHA_PRECISION_UNRESTRICTED,
    baseLog
  ) %>%
  group_by(baseLog, ShortId) %>%
  summarize(
    exmean = rf(mean(TotalDuration)),
    exsd = rf(sd(TotalDuration)),
    scmean = rf(mean(MODEL_ENTITY_COUNT)),
    scsd = rf(sd(MODEL_ENTITY_COUNT)),
    emmean = rf(mean(
      as.numeric(EARTH_MOVERS_LIGHT_COVERAGE), na.rm = TRUE
    )),
    apmean = rf(mean(
      as.numeric(ALPHA_PRECISION_UNRESTRICTED), na.rm = TRUE
    ))
  ) %>%
  write_csv(paste(workingPath, 'runtimes.csv', sep = ''))


## Pareto

forcenum <- function(df){
  as.numeric(as.character(df))
}

rd <- rundata
rd$EARTH_MOVERS_LIGHT_COVERAGE <- forcenum(rd$EARTH_MOVERS_LIGHT_COVERAGE)
rd$ALPHA_PRECISION_UNRESTRICTED <- forcenum(rd$ALPHA_PRECISION_UNRESTRICTED)
rd$MODEL_ENTITY_COUNT <- forcenum(rd$MODEL_ENTITY_COUNT)

rdn <- rd %>% drop_na()
## ======= Looks like a separate script, poor man's code reuse ======

rundata %>% select(ShortId,MinerDuration,EstimatorDuration,TotalDuration,MODEL_ENTITY_COUNT,baseLog) %>%
              group_by(baseLog,ShortId) %>%
              summarize(exmean=round(mean(TotalDuration),2),
                       exsd=round(sd(TotalDuration),2),
                       scmean=round(mean(MODEL_ENTITY_COUNT),2),
                       scsd=round(sd(MODEL_ENTITY_COUNT),2)) %>%
              write_csv(paste(workingPath,'runtimes.csv',sep=''))

tonum <- function(df){
  res <- as.numeric(as.character(df))
}

rd <- rundata
rd$EARTH_MOVERS_LIGHT_COVERAGE <- tonum(rd$EARTH_MOVERS_LIGHT_COVERAGE)
rd$ALPHA_PRECISION_UNRESTRICTED <- tonum(rd$ALPHA_PRECISION_UNRESTRICTED)
rd$MODEL_ENTITY_COUNT <- tonum(rd$MODEL_ENTITY_COUNT)

rdn <-
  rd %>% drop_na() %>% 
  select(
    baseLog,
    ShortId,
    EARTH_MOVERS_LIGHT_COVERAGE,
    ALPHA_PRECISION_UNRESTRICTED,
    MODEL_ENTITY_COUNT
  )

paretoWide <- psel(
  group_by(rdn,baseLog),
  high(EARTH_MOVERS_LIGHT_COVERAGE) * high(ALPHA_PRECISION_UNRESTRICTED) *
    low(MODEL_ENTITY_COUNT) ,
  show_level = TRUE,
  top_level=1
) 

paretoWide %>% write_csv(paste(workingPath,'pareto.csv',sep=''))
  
rdMeans <- rdn %>%
  group_by(baseLog,ShortId) %>%
  summarize(scmean=round(mean(MODEL_ENTITY_COUNT),2),
            apmean=round(mean(ALPHA_PRECISION_UNRESTRICTED),2),
            emmean=round(mean(EARTH_MOVERS_LIGHT_COVERAGE),2)
            )  

paretoMean <- 
  psel(
    group_by(rdMeans,baseLog),
    high(emmean) * high(apmean) *
    low(scmean) ,
    show_level = TRUE,
    top_level=1
  ) 

paretoMean %>% write_csv(paste(workingPath,'paretoMean.csv',sep=''))
