
# Settings function ----------------------------------------------------------------
#' Concatenates two models created by bam package
#'
#' This function takes as arguments list of calculated BAM models and binds them to one object. It is useful when
#' one want to present togeher baseline model and any number of scenarios.
#'
#' @param x list of objects of class "BamModel" (calculated models)
#'
#' @return Object of class BamModel
#'
#' @note Please not that model attributes are copied from the first "BamModle" class object in the list.
#'
#'
#'
#'@export
ConcatenateModels<-function(x)
{
  res<-rbindlist(x)
  mostattributes(res)<-attributes(x[[1]])
  class(res)<-c("BamModel","data.table","data.frame")
  return(res)
}

install_bam_dependencies<-function()
{
  cat("Checking for package dependencies\n")
  pckgs<-c("data.table","signal","ggplot2","scales",
           "foreach","ggthemes","DiagrammeR","doParallel",
           "jsonlite","knitr","openxlsx","knitr","kableExtra","pifpaf")
  
  for (n in pckgs)
  {
    x<-require(paste(n), character.only = T)
    if (x==F) 
    {
      cat("Installing needed package: ",n,"\n")
      install.packages(n)
    }
  }
  
}

#' Prepares settings for bam package calculations
#'
#' This function prepares the list variable with all settings needed by bam package functions.
#' If settings are not specified bam uses default values as below.
#'
#'
#' @return list of settings
#'
#'
#' @details
#' List of values:
#' INPUTS_VER - version of inputs data
#' CODE_VER - version of bam code
#' USE_CORES - number of processor cores to use for multithread computation. By default number of detected physical cores.
#' PARALLEL.PACKS - number of threads calculating model
#' PARALLEL.PACKSIZE - number of iterations calculated by one thread.
#'
#' PARALLEL.PACKS * PARALLEL_SIZE = number of final iterations of stochastic model.
#'
#' LOG.TO.SOCKET - enables logging to socket connection for parallel computations
#' LOG.TO.FILES - enables logging to file. Log files are named as date and time and are stored in ./BAM/Log.
#' LOG.TO.CONSOLE - enables logging to console (only for single threaded parts)
#' OUTPUT_PATH - sets paths where output files will be stored.
#'
#' @export
#' @import foreach data.table parallel knitr

#'@export

default.settings<-function(ask=T)
{
  
  install_bam_dependencies()
  
  library(parallel)
  library(knitr)
  SETTINGS<-list(
    INPUTS_VER="11",
    CODE_VER=packageVersion("bam"),
    USE.CORES=detectCores(logical=F),
    PARALLEL.PACKS=2,
    PARALLEL.PACKSIZE=10,
    LOG.TO.SOCKET=F,
    LOG.TO.FILE=T,
    LOG.TO.CONSOLE=T,
    DEB=F,
    startYear=2005,
    finalYear=2060)
  SETTINGS$OUTPUT_PATH<-paste0(getwd(),"/Outputs/",SETTINGS$CODE_VER,"_",SETTINGS$INPUTS_VER,"/")
  SETTINGS$LOG_PATH<-paste0(getwd(),"/Log/")
  
  if (SETTINGS$LOG.TO.FILE) SETTINGS$LOG.FILE.NAME<-paste0(make.names(Sys.time()),".log")
  #if (SETTINGS$LOG.TO.SOCKET) SETTINGS$SOCKET <- make.socket(port=4000)
  
  if (dir.exists(SETTINGS$OUTPUT_PATH)==F)
  {
    
    if (ask) a1<-readline(prompt=paste0("Output directory: '",SETTINGS$OUTPUT_PATH,"' does not exist. Create? (y/n)")) else a1="Y"
    
    if (a1 %in% c("Y","y")) dir.create(SETTINGS$OUTPUT_PATH, recursive = T) else stop("Execution stopped.")
  }
  
  if (dir.exists(SETTINGS$LOG_PATH)==F)
  {
    if (ask) a2<-readline(prompt=paste0("Log directory: '",SETTINGS$LOG_PATH,"' does not exist. Create? (y/n)")) else a2="Y"
    
    if (a2 %in% c("Y","y")) dir.create(SETTINGS$LOG_PATH, recursive = T) else stop("Execution stopped.")
  }
  
  
  if (file.exists(paste0(SETTINGS$LOG_PATH,"/",SETTINGS$LOG.FILE.NAME))==F)
  {
    file.create(paste0(SETTINGS$LOG_PATH,"/",SETTINGS$LOG.FILE.NAME), recursive=T)
    Log("Log file has been created: '%s'", SETTINGS$LOG.FILE.NAME, settings=SETTINGS )
  }
  
  Log(kable(t(as.data.frame(SETTINGS)), col.names = F, format="simple"), settings=SETTINGS)
  
  return(SETTINGS)
}


#'@export
Log <- function(text, settings=default.settings(), ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text), ...)
  if (settings$LOG.TO.CONSOLE) cat(paste0(msg,"\n"))
  if (settings$LOG.TO.FILE)   write(paste0(msg),paste0(settings$LOG_PATH,"/",settings$LOG.FILE.NAME),append=TRUE)
  if (settings$LOG.TO.SOCKET)
  {
    con<-make.socket(port=4000)
    write.socket(con, paste0(msg,"#\n"))
    close.socket(con)
  }
}

#'@export

lcl<-function(x, lev=0.025)
{
  return(quantile(x, lev))
}


#'@export
ucl<-function(x, lev=0.975)
{
  return(quantile(x, lev))
}

#'@export

view<-function(dat)
  
{
  tempFile = paste0(tempfile(), ".csv")
  tempPath = dirname(tempFile)
  
  write.csv(dat, file=tempFile)
  shell.exec(tempFile)
}


#'@export
showExcel<-function(df)
{
  UseMethod("showExcel",df)
}


#'@export
showExcel.default<-function(df)
{
  file<-paste(tempdir(),"\\","ModelResult.csv",sep="")
  write.csv(df,file)
  shell.exec(file)
}

#'@export
pyramid<-function(x, year, state)
{
  UseMethod("pyramid",x)
}

pyramid.default<-function(x, year, state)
{
  
}



#'@export
showTable<-function(df)
{
  library(RGtk2)
  model <- rGtkDataFrame(df)
  view <- gtkTreeView(model)
  mapply(view$insertColumnWithAttributes,  -1, colnames(model),
         list(gtkCellRendererText()),
         text = seq_len(ncol(model)) - 1)
  
  sw <- gtkScrolledWindow()
  sw$add(view)
  
  win <- gtkWindow(show=FALSE)
  win$add(sw)
  gtkWindowMaximize(win)
  win$show()
}


#'This function allow to export big data frames to clipboard
#'@export

toClipboard<-function(df)
{
  zz <- textConnection("xpxp", "w")
  write.table(df, zz,  sep="\t")
  writeClipboard(textConnectionValue(zz) )
  unlockBinding("xpxp", .GlobalEnv)
}

fix.csv <- function(file, new.name=TRUE, sep=",", comment.char="") {
  tmpframe <- read.csv(file, sep=sep,quote="", colClasses="character",
                       stringsAsFactors=FALSE, comment.char="",
                       blank.lines.skip=FALSE, na.strings="")
  tmpframe <- edit(tmpframe)
  if(is.character(new.name)) {
    out.name <- new.name
  } else if(new.name <- TRUE) {
    out.name <- readline(prompt="Enter file name to save (Hit enter to use original):")
  } else {
    out.name <- file
  }
  if(out.name=="") out.name <- filex
  write.table(tmpframe, file=out.name, append=FALSE, quote=FALSE, sep=sep,
              row.names=FALSE)
}

#this function removes all NAs from data.table and change them to zero.
#'@export
na.rm.data.table<-function(dt)
{
  for (n in names(dt))
  {
    dt[eval(parse(text=paste0("is.na(",n,")"))), eval(parse(text=paste0(n,":=0")))]
  }
}

#' Shows data.frame or data.table in rstudio html viewer
#'@export
kview <- function(x, ...){
  library(kableExtra)
  
  o<-kable(x, format="html")
  st<-"<style>
      table, td, tr, th {
      border: 1px solid black;
      border-collapse: collapse;
      }
      </style>"
  
  tab <- paste(st,capture.output(o), collapse = '\n')
  tf <- tempfile(fileext = ".html")
  writeLines(tab, tf)
  rstudioapi::viewer(tf)
}

# Prepare data ------------------------------------------------------------
first.year = 2015
final.year = 2050
CI.calendar.effect = 0
FI.calendar.effect = -4.00
CVD_calendar = 0 # each cvd，改成0试试
Pre_china = 'D:\\Obj 3 model Yuyang\\CAM 0511\\R data\\Prevalence for IMPACT-CAM eight states_20230401_60dementia.RDS'   #注意路径和文件
method_pre = 'Estimation'# method_pre = 'Beta distribution'
exact_prevalence = T
gen.random.num = 200
TPs_data = 'D:\\Obj 3 model Yuyang\\CAM 0511\\R data\\CHARLS - Incidence for IMPACT-CAM_20230403_Harm.RDS'
TP_interval = 2
Adjust_prop_year = NULL
Mortality_prop = 'D:\\Obj 3 model Yuyang\\CAM 0511\\R data\\prop_BAM_HR_nweight_0317.RDS'
Mortality_SE = F
Mortality = CVD_calendar.Mortality
Current_Pop = 'D:\\Obj 3 model Yuyang\\CAM 0511\\R data\\Pop structure (China UN 2022 ref).RDS'
Project.Pop = 'D:\\Obj 3 model Yuyang\\CAM 0511\\R data\\Pop structure (China UN 2022 ref).RDS'
Load_china <- function(first.year = 2015,
                       final.year = 2050,
                       CI.calendar.effect = 0,
                       FI.calendar.effect = 0,
                       CVD_calendar = 'R data/CVD_nCVD mortality in China 1990-2050_boot1000_UN2022_GBD2050rw2.RDS',
                       Pre_china = 'R data/Prevalence for IMPACT-CAM eight states_20221026_60dementia.RDS',
                       method_pre = 'Estimation',exact_prevalence = FALSE,gen.random.num = 200,
                       TPs_data = 'R data/CLHLS - Incidence for IMPACT-CAM_20211220.RDS',
                       TP_interval = 2,
                       Adjust_prop_year = c(2015),# Adjust by which year?
                       Mortality_prop = 'R data/CVD death proprtion from clhls_20221113_weight.RDS',
                       Mortality = 'R data/CVD_nCVD mortality in China 1990-2050_boot1000_UN2022_GBD2050rw2.RDS',
                       Mortality_SE = T,
                       Current_Pop = 'R data/Pop structure (China UN 2022 ref).RDS',
                       Project.Pop = 'R data/Pop structure (China UN 2022 ref).RDS'
)
{
  iters=c(0:(settings$PARALLEL.PACKS*settings$PARALLEL.PACKSIZE))
  Log("Starting LoadData_China() function with iterations %s-%s", 
      paste(min(iters)), paste(max(iters)), settings=settings)
  # CI and FI calendar effect 
  # pct is the value
  calEff<-function(pct=0, startYear=first.year, finalYear=final.year)
  {
    years<-c(startYear:finalYear)
    genders<-c("Men","Women")
    ages<-c(35:100)
    
    cf<-as.data.table(expand.grid(years, genders, ages))
    setnames(cf,c("Var1","Var2","Var3"), c("Year","Gender","Age"))
    cf[,caleff:=(100+pct)/100]
    cf[Year==startYear, caleff:=1]
    return(cf)
  }
  
  
  # CVD calendar effect 
  if (!is.null(CVD_calendar) & 
      is.character(CVD_calendar)){
    mortality_cvd <- data.table(readRDS(CVD_calendar))
    mrt <- mortality_cvd[mortality_cvd$Disease == 'CVD' & mortality_cvd$Year >= first.year,]
    setkey(mrt, Gender, Disease, Age, Year)
    mrt<-mrt[Disease=="CVD"]
    mrt[,Rate1:=shift(Rate),by=list(Gender, Disease, Age)]
    mrt[,a:=Rate1 - Rate,by=list(Gender, Disease, Age)]
    mrt[,caleff:=1-((Rate1-Rate)/Rate)]
    mrt[is.na(caleff), caleff:=1]
    mrt<-mrt[,list(Year, Age, Gender, caleff)]
    cvd_caleff_file = mrt[order(mrt$Year,mrt$Age)]
    
    Log("CVD calendar effect with iterations %s-%s", 
        paste(min(iters)), paste(max(iters)), settings=settings)
  }else if (!is.null(CVD_calendar) & 
            is.numeric(CVD_calendar)){
    cvd_caleff_file <- calEff(pct = CVD_calendar,
                               startYear=first.year, finalYear=final.year)
    Log(paste0("CVD calendar effect: ",
               CVD_calendar," with iterations %s-%s"), 
        paste(min(iters)), paste(max(iters)), settings=settings)
    
  }
  # else if (is.null(CVD_calendar)){
  #   cvd_caleff_file <- data.table(data.frame(
  #     Year = rep(first.year:final.year,each = 66 * 2),
  #     Age = rep(c(35:100),length(first.year:final.year)*2),
  #     Gender = rep(rep(c('Men','Women'),each = 66),
  #                  length(first.year:final.year)),
  #     caleff = 1
  #   ))
  #   Log("CVD calendar effect equal = 0 with iterations %s-%s", 
  #       paste(min(iters)), paste(max(iters)), settings=settings)
  # }

  cind_caleff_file <- calEff(pct = CI.calendar.effect,startYear=first.year, finalYear=final.year)
  Log(paste0("CI calendar effect: ",
             CI.calendar.effect," with iterations %s-%s"), 
      paste(min(iters)), paste(max(iters)), settings=settings)
  
  p10_caleff_file <- calEff(pct = FI.calendar.effect,startYear=first.year, finalYear=final.year)
  Log(paste0("FI calendar effect: ",
             FI.calendar.effect ,
             " with iterations %s-%s"), 
      paste(min(iters)), paste(max(iters)), settings=settings)
  
  # input all baseline things
  ## 1. iteration 
  #import transition probabilities
  ## 2. import transition probabilities 
  
  tps <- data.table(readRDS(TPs_data))
  tps<-tps[, list(Gender, X, Age, P, SE)]
  tps[, X:=paste0("P",substr(X,2,10))]
  tps[is.na(P),`:=`(P=0, SE=0)]
  
  ## 3. import prevalence 
  ## China prevalence 
  
  if (method_pre == 'Estimation'){
    Pre_state <- readRDS(Pre_china)
    if (exact_prevalence == FALSE){ # 95
      for (c in 3:10){
        Pre_state[Pre_state$rage>= 95 & Pre_state$gender_c == 'Male',c] <-
          Pre_state[Pre_state$rage == 95 & Pre_state$gender_c == 'Male',c]
        Pre_state[Pre_state$rage>= 95 & Pre_state$gender_c == 'Female',c] <-
          Pre_state[Pre_state$rage == 95 & Pre_state$gender_c == 'Female',c]
      }
    }else if (exact_prevalence == TRUE){
      Pre_state <- readRDS(Pre_china)
    }
    Pre_state <- data.table(Pre_state)
    # Cycle 10
    gen.random.num <- gen.random.num # random data
    sample.num <- (settings$PARALLEL.PACKS*settings$PARALLEL.PACKSIZE) # general sample
    
    set.seed(20211123)
    while_sample <- function(size,mean,sd){
      
      pre_data <- data.table(Boot. = rnorm(size*(sample.num+1),mean,sd),
                             Boot.num = rep(1:size,(sample.num+1)),
                             Resample  =rep(0:sample.num,e = size))
      return(pre_data)
    }
    resamples.dem.pre <- Pre_state[,list(Boot.dem = while_sample(gen.random.num,
                                                                 dem.fit,
                                                                 dem.se)$Boot.,
                                         Boot.num = while_sample(gen.random.num,
                                                                 dem.fit,
                                                                 dem.se)$Boot.num,
                                         Resample = rep(0:sample.num,e = gen.random.num)),
                                   by = list(rage,gender_c)]
    
    resamples.cind.pre <- Pre_state[,list(Boot.cind = while_sample(gen.random.num,
                                                                   cind.fit,
                                                                   cind.se)$Boot.,
                                          Boot.num = while_sample(gen.random.num,
                                                                  cind.fit,
                                                                  cind.se)$Boot.num,
                                          Resample = rep(0:sample.num,e = gen.random.num)),
                                    by = list(rage,gender_c)]
    resamples.find.pre <- Pre_state[,list(Boot.find = while_sample(gen.random.num,
                                                                   find.fit,
                                                                   find.se)$Boot.,
                                          Boot.num = while_sample(gen.random.num,
                                                                  find.fit,
                                                                  find.se)$Boot.num,
                                          Resample = rep(0:sample.num,e = gen.random.num)),
                                    by = list(rage,gender_c)]
    resamples.cvd.pre <- Pre_state[,list(Boot.cvd = while_sample(gen.random.num,
                                                                 cvd.fit,
                                                                 cvd.se)$Boot.,
                                         Boot.num = while_sample(gen.random.num,
                                                                 cvd.fit,
                                                                 cvd.se)$Boot.num,
                                         Resample = rep(0:sample.num,e = gen.random.num)),
                                   by = list(rage,gender_c)]
    
    resample.pre.dem.cind <- merge(resamples.dem.pre,resamples.cind.pre,id = c('Boot.num','Resample','rage','gender_c'))
    resample.pre.dem.cind.find <- merge(resample.pre.dem.cind,resamples.find.pre,id = c('Boot.num','Resample','rage','gender_c'))
    resample.pre.dem.cind.find.cvd <- merge(resample.pre.dem.cind.find,resamples.cvd.pre,id = c('Boot.num','Resample','rage','gender_c'))
    resample.pre.order <- resample.pre.dem.cind.find.cvd[order(resample.pre.dem.cind.find.cvd$Boot.num,
                                                               resample.pre.dem.cind.find.cvd$Resample),]
    # Caculate the prevalence for 8 states
    
    resample.pre.order$Pre_DEM  <- resample.pre.order$Boot.dem
    resample.pre.order$Pre_CIND <- resample.pre.order$Boot.cind
    resample.pre.order$Pre_FIND <- resample.pre.order$Boot.find
    resample.pre.order$Pre_HealthND <- 1 - resample.pre.order$Pre_CIND- resample.pre.order$Pre_FIND - resample.pre.order$Pre_DEM 
    resample.pre.order$Pre_HealthND[(resample.pre.order$Pre_HealthND) <= 0] <- 0
    ### Consider to CVD 
    resample.pre.order$Pre_CVD <- resample.pre.order$Boot.cvd
    # Pre(CIND & CVD) = Pre(CIND) * Pre(FIND) 
    resample.pre.order$Pre_CIND_CVD <- resample.pre.order$Pre_CIND * resample.pre.order$Pre_CVD
    # Pre(CIND only) = Pre(CIND) - Pre(CIND & CVD) 
    resample.pre.order$Pre_CIND_only <- resample.pre.order$Pre_CIND - resample.pre.order$Pre_CIND_CVD
    # Pre(FIND & CVD) = Pre(FIND) * Pre(CVD) 
    resample.pre.order$Pre_FIND_CVD <- resample.pre.order$Pre_FIND * resample.pre.order$Pre_CVD
    # Pre(FIND only) = Pre(FIND) - Pre(FIND & CVD) 
    resample.pre.order$Pre_FIND_only <- resample.pre.order$Pre_FIND - resample.pre.order$Pre_FIND_CVD
    # Pre(Dem & CVD) = Pre(Dem) * Pre(CVD) 
    resample.pre.order$Pre_DEM_CVD <- resample.pre.order$Pre_DEM * resample.pre.order$Pre_CVD
    # Pre(FIND only) = Pre(FIND) - Pre(FIND & CVD) 
    resample.pre.order$Pre_DEM_only <- resample.pre.order$Pre_DEM - resample.pre.order$Pre_DEM_CVD
    # Pre(CVD only) = Pre(Health without CI or FI) * Pre(CVD) 
    resample.pre.order$Pre_CVD_only <- resample.pre.order$Pre_HealthND * resample.pre.order$Pre_CVD
    # Pre(Health) =  1 - Pre(Health without CI or FI) * Pre(CVD) 
    resample.pre.order$Pre_Health <- 1 - resample.pre.order$Pre_CVD_only - 
      resample.pre.order$Pre_CIND_CVD -
      resample.pre.order$Pre_CIND_only - 
      resample.pre.order$Pre_FIND_CVD -
      resample.pre.order$Pre_DEM_CVD - 
      resample.pre.order$Pre_DEM_only - 
      resample.pre.order$Pre_FIND_only 
    resample.pre.order$Pre_Health[(resample.pre.order$Pre_Health) <= 0] <- 0
    # resample.pre.order$Pre_Health[(resample.pre.order$Pre_Health) <= 0] <- 0
    resample.pre.meansd <- resample.pre.order[,list(rage,gender_c,Boot.num,Resample,
                                                    Pre_Health,Pre_CVD_only,
                                                    Pre_CIND_CVD,Pre_CIND_only,
                                                    Pre_FIND_CVD,Pre_DEM_CVD,
                                                    Pre_DEM_only,Pre_FIND_only)]
    resample.pre.meansd[,`:=`(Iter = Resample,Age = rage,
                              Gender = ifelse(gender_c == 'Male','Men','Women') )]
    
    pre_china <- resample.pre.meansd[,list(s1 = mean(Pre_Health),
                                           s2 = mean(Pre_CVD_only),
                                           s3 = mean(Pre_CIND_CVD),
                                           s4 = mean(Pre_CIND_only),
                                           s5 = mean(Pre_FIND_CVD),
                                           s6 = mean(Pre_DEM_CVD),
                                           s7 = mean(Pre_DEM_only),
                                           s10 = mean(Pre_FIND_only)),
                                     by = list(Iter,Age,Gender)]
    
    ## scalling
    # prev<-prev.scalling(prev)
    pre_china[s2<0, s2:=0]
    pre_china[s3<0, s3:=0]
    pre_china[s4<0, s4:=0]
    pre_china[s5<0, s5:=0]
    pre_china[s6<0, s6:=0]
    pre_china[s7<0, s7:=0]
    pre_china[s10<0, s10:=0]
    pre_china[,s1:=1-(s2+s3+s4+s5+s6+s7+s10)]
    
    # Scaling to unit length using Eucledian vector length (if negative s1)
    pre_china[, pr:=0]
    pre_china[s1<0,pr:=sqrt(s2^2+s3^2+s4^2+s5^2+s6^2+s7^2+s10^2)]
    pre_china[s1<0,s1:=0]
    pre_china[pr!=0,s2:=(s2/pr)^2]
    pre_china[pr!=0,s3:=(s3/pr)^2]
    pre_china[pr!=0,s4:=(s4/pr)^2]
    pre_china[pr!=0,s5:=(s5/pr)^2]
    pre_china[pr!=0,s6:=(s6/pr)^2]
    pre_china[pr!=0,s7:=(s7/pr)^2]
    pre_china[pr!=0,s10:=(s10/pr)^2]
    pre_china[,pr:=NULL]
    pre_china2 <- pre_china
    names(pre_china) <- c("Iter","Age","Gender",paste0('s.',c(1:7,10)))
    prev <- copy(pre_china)
    Log("Prevalence loaded (estimation): '%s'", 
        as.character(Pre_china),
        # "CHARLS pooled data (estimation): '%s'", 
        settings = settings)
    
  }else if (method_pre == 'Beta distribution'){
    pre_beta <- readRDS('R data/CAM prevalence beta distribution_0401.RDS')
    
    {
      #cases
      cases<- (pre_beta)
      cases$Age.group <-  cases$agegroup_c
      cases$Gender <-  ifelse(cases$gender_c == 'Male','Men','Women')
      # setnames(cases, names(cases), make.names(names(cases)))
      # cases.old<-copy(cases)
      cases[, 'centerage'] <- NA
      cases[cases$Age.group=="35~39", 'centerage'] <- 37
      cases[cases$Age.group=="40~44", 'centerage'] <- 42
      cases[cases$Age.group=="45~49", 'centerage'] <- 47
      cases[cases$Age.group=="50~54", 'centerage'] <- 52
      cases[cases$Age.group=="55~59", 'centerage'] <- 57
      cases[cases$Age.group=="60~64", 'centerage'] <- 62
      cases[cases$Age.group=="65~69", 'centerage'] <- 67
      cases[cases$Age.group=="70~74", 'centerage'] <- 72
      cases[cases$Age.group=="75~79", 'centerage'] <- 77
      cases[cases$Age.group=="80~84", 'centerage'] <- 82
      cases[cases$Age.group=="85~89", 'centerage'] <- 87
      cases[cases$Age.group=="90~", 'centerage'] <- 93
      
      ages<-c(37,42,47,52,57,62,67,72,77,82,87,93)
      
      p <- 12 #number of age groups
      sample.num <- (settings$PARALLEL.PACKS*settings$PARALLEL.PACKSIZE) # general sample
      iters <- 0:sample.num
      t<-iters
      store<-data.table::data.table(Gender=rep(c(rep("Men", times=p),
                                                 rep("Women", times=p)),
                                               times=length(iters)),
                                    centerage=rep(ages, times=length(iters)*2),
                                    iter=sort(rep(iters, times=p*2)))
      
      cases <- data.table::data.table(cases)
      data.table::setkey(store, centerage, Gender)
      data.table::setkey(cases, centerage, Gender )
      store<-store[cases]
      uncert.prevalences=T
      if (uncert.prevalences)
      {
        store[,live_state2:=rbeta(length(a_state2),a_state2, b_state2)]
        store[,live_state3:=rbeta(length(a_state3),a_state3, b_state3)]
        store[,live_state4:=rbeta(length(a_state4),a_state4, b_state4)]
        store[,live_state5:=rbeta(length(a_state5),a_state5, b_state5)]
        store[,live_state6:=rbeta(length(a_state6),a_state6, b_state6)]
        store[,live_state7:=rbeta(length(a_state7),a_state7, b_state7)]
        store[,live_state10:=rbeta(length(a_state10),a_state10, b_state10)]
      } else
      {
        store[, live_state2:=a_state2/(a_state2+b_state2)]
        store[, live_state3:=a_state3/(a_state3+b_state3)]
        store[, live_state4:=a_state4/(a_state4+b_state4)]
        store[, live_state5:=a_state5/(a_state5+b_state5)]
        store[, live_state6:=a_state6/(a_state6+b_state6)]
        store[, live_state7:=a_state7/(a_state7+b_state7)]
        store[, live_state10:=a_state10/(a_state10+b_state10)]
      }
      
      data.table::setkey(store,iter,Gender,centerage)
      #d[,x:=c(37,42,47,52,57,62,67,72,77,82,87,95)]
      xout<-c(35:95)
      
      library(signal)
      smth<-function(x, value, xout)
      {
        r1<-pchip(x,value,xout)
        r1<-c(r1,rep(tail(r1, n=1),times=5)) # all values for ages 95-100 are the same like for 94
        return(r1)
      }
      
      
      # tx<-store[,list(live_state2=smth(centerage,live_state2,xout),
      #                 live_state3=smth(centerage,live_state3,xout),
      #                 live_state4=smth(centerage,live_state4,xout),
      #                 live_state5=smth(centerage,live_state5,xout),
      #                 live_state6=smth(centerage,live_state6,xout),
      #                 live_state7=smth(centerage,live_state7,xout),
      #                 live_state10=smth(centerage,live_state10,xout),
      #                 age=c(xout, 96:100)), 
      #           by=list(Gender,iter)]
      xout<-c(35:100)
      tx<-store[,list(live_state2=pchip(centerage,live_state2,xout),
                      live_state3=pchip(centerage,live_state3,xout),
                      live_state4=pchip(centerage,live_state4,xout),
                      live_state5=pchip(centerage,live_state5,xout),
                      live_state6=pchip(centerage,live_state6,xout),
                      live_state7=pchip(centerage,live_state7,xout),
                      live_state10=pchip(centerage,live_state10,xout),
                      age=c(35:100)), 
                by=list(Gender,iter)]
      
      
      data.table::setnames(tx,c("iter","age","live_state2","live_state3","live_state4","live_state5","live_state6","live_state7","live_state10"),
                           c("Iter","Age","s.2","s.3","s.4","s.5","s.6","s.7","s.10"))
      
      tx[s.2<0, s.2:=0]
      tx[s.3<0, s.3:=0]
      tx[s.4<0, s.4:=0]
      tx[s.5<0, s.5:=0]
      tx[s.6<0, s.6:=0]
      tx[s.7<0, s.7:=0]
      tx[s.10<0, s.10:=0]
      
      
      tx[,s.1:=1-(s.2+s.3+s.4+s.5+s.6+s.7+s.10)]
      prevalence<-tx[,c("Iter","Gender","Age","s.1","s.2","s.3","s.4","s.5","s.6","s.7","s.10"), 
                     with=F]
      
      
      # Scaling to unit length using Eucledian vector length (if negative s.1)
      prevalence[, pr:=0]
      prevalence[s.1<0,pr:=sqrt(s.2^2+s.3^2+s.4^2+s.5^2+s.6^2+s.7^2+s.10^2)]
      prevalence[s.1<0,s.1:=0]
      prevalence[pr!=0,s.2:=(s.2/pr)^2]
      prevalence[pr!=0,s.3:=(s.3/pr)^2]
      prevalence[pr!=0,s.4:=(s.4/pr)^2]
      prevalence[pr!=0,s.5:=(s.5/pr)^2]
      prevalence[pr!=0,s.6:=(s.6/pr)^2]
      prevalence[pr!=0,s.7:=(s.7/pr)^2]
      prevalence[pr!=0,s.10:=(s.10/pr)^2]
      prevalence[,pr:=NULL]
      
    }
    
    
    prev <- copy(prevalence)
    Log("Prevalence loaded (beta distribution): '%s'", 
        # 'CHARLS pooled data (beta distribution)', 
        Pre_china, settings = settings)
  }
  

  ## import mortality proportions 
  
  mortprop <- data.table(readRDS(Mortality_prop))
  setnames(mortprop, names(mortprop), make.names(names(mortprop)))
  Log("Mortality proportion loaded: '%s'", Mortality_prop, settings = settings)
  
  ## import initial population 
  inipop_0<-data.table(readRDS(Current_Pop))
  inipop <- melt(inipop_0[which(inipop_0$Year == first.year & inipop_0$Age >= 35),], 
               id.vars = c("Year", "Age"), variable.name = "Gender", value.name = "Population")
  inipop$Gender <- as.character(inipop$Gender)
  
  if (('Male' %in% unique(inipop$Gender)) | 
       ('Female' %in% unique(inipop$Gender))){
    inipop$Gender[inipop$Gender == 'Male'] <- 'Men'
    inipop$Gender[inipop$Gender == 'Female'] <- 'Women'
  }
  
  Log("Initial population data imported. Soruce file: '%s'", Current_Pop, settings = settings)
  
  ## import population projections 
  projpop_0 <- data.table(readRDS(Project.Pop))
  projpop <- projpop_0[projpop_0$Age == 35,]
  if (('Male' %in% names(projpop)) | 
      ('Female' %in% names(projpop))){
    names(projpop)[which(names(projpop) == 'Female')] <- "Women"
    names(projpop)[which(names(projpop) == 'Male')] <- "Men"
  }

  projpop[,`:=`(Men=as.double(Men), Women=as.double(Women))]
  projpop <- melt(projpop, id.vars = c("Year",'Age'),
                  value.name = "Population",variable.name = "Gender")
  projpop <- data.table(projpop)
  projpop<-projpop[Year %in% c(settings$startYear:settings$finalYear),]
  
  Log("Population projection data imported. Soruce file: '%s'", Project.Pop, settings = settings)
  
  ##   import calendar effects 
  
  if (class(cvd_caleff_file)[1]=="character")
  {
    calendar.effect.cvd<-fread(paste0(cvd_caleff_file))
    calendar.effect.cvd[Sex==1, Gender:="Women"]
    calendar.effect.cvd[Sex==0, Gender:="Men"]
    calendar.effect.cvd[, Sex:=NULL]
    Log(paste0("CVD calendar effect file loaded. File name: '%s'", CVD_calendar), settings = settings)
  } else
  {
    calendar.effect.cvd<-cvd_caleff_file
    Log(paste0("CVD calendar effect loaded: ", 'Non-CVD mortality trend'), settings = settings)
  }
  
  if (class(cind_caleff_file)[1]=="character")
  {
    calendar.effect.cind<-fread(paste0(cind_caleff_file))
    calendar.effect.cind[Sex==1, Gender:="Women"]
    calendar.effect.cind[Sex==0, Gender:="Men"]
    calendar.effect.cind[, Sex:=NULL]
    Log(paste0("CIND calendar effect file loaded. File name: '%s'", 
               CI.calendar.effect), settings = settings)
  } else
  {
    calendar.effect.cind<-cind_caleff_file
    Log(paste0("CIND calendar effect loaded:",CI.calendar.effect), settings = settings)
  }
  
  
  if (class(p10_caleff_file)[1]=="character")
  {
    calendar.effect.P10<-fread(paste0(p10_caleff_file))
    calendar.effect.P10[Sex==1, Gender:="Women"]
    calendar.effect.P10[Sex==0, Gender:="Men"]
    calendar.effect.P10[, Sex:=NULL]
    Log(paste0("P10 (Non-CVD Non-CIND related disability) calendar effect file loaded. File name: '%s'", FI.calendar.effect), settings = settings)
  } else
  {
    calendar.effect.P10<-p10_caleff_file
    Log(paste0("P10 (Non-CVD Non-CIND related disability) calendar effect loaded:",
               FI.calendar.effect), settings = settings)
  }
  
  calendar.effect.cvd<-calendar.effect.cvd[Year %in% c(settings$startYear:settings$finalYear)]
  calendar.effect.cind<-calendar.effect.cind[Year %in% c(settings$startYear:settings$finalYear)]
  calendar.effect.P10<-calendar.effect.P10[Year %in% c(settings$startYear:settings$finalYear)]
  
  res<-list()
  res[["prev"]]<-prev
  res[["mortprop"]]<-mortprop
  res[["tps"]]<-tps
  res[["inipop"]]<-inipop
  res[["projpop"]]<-projpop
  res[["cvd_caleff"]]<-calendar.effect.cvd
  res[["cind_caleff"]]<-calendar.effect.cind
  res[["p10_caleff"]]<-calendar.effect.P10
  

  tps_prepare <- function(dat=dat,
                          Mortality_SE =  Mortality_SE ,
                          TPs_data_from = TPs_data,
                          Mortality = Mortality){
    
    iters=c(0:(settings$PARALLEL.PACKS*settings$PARALLEL.PACKSIZE))
    ix<-data.table(Iter=iters, n=1)
    # Creating iterations
    
    dat$tps[,n:=1]
    
    setkey(ix, n)
    setkey(dat$tps, n)
    
    dat$tps<-dat$tps[ix, allow.cartesian=T]
    # draw TPs to non-absorbing states
    dat$tps[Iter>0, Live:=rnorm(length(P), mean=P, sd=SE)] # stochastic live value
    # TODO change to lognorm?
    dat$tps[Iter==0, Live:=P] # deterministic value stored as iter==0
    
    dat$tps[Live<0,Live:=0]  #discuss with Maria
    dat$tps[Live>1,Live:=1]

    tp_period <- TP_interval
    
    if (grepl('CHARLS',TPs_data)){
      #convert 2-year TPs to 1-year TPs from CHARLS
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5'),P:=1-exp(-(-log(1-P)/tp_period))]
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5'),Live:=1-exp(-(-log(1-Live)/tp_period))]
      # # convert 2-year TPs to 1-year TPs from CLHLS
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5') == FALSE,P:=1-exp(-(-log(1-P)/tp_period))]
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5') == FALSE,Live:=1-exp(-(-log(1-Live)/tp_period))]
      Log("Two year TPs (CHARLS) was changed to One-year.", settings = settings)
    }else if(grepl('CLHLS',TPs_data)){
      #convert 2-year TPs to 1-year TPs from CHARLS
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5'),P:=1-exp(-(-log(1-P)/2))]
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5'),Live:=1-exp(-(-log(1-Live)/2))]
      # # convert 3-year TPs to 1-year TPs from CLHLS
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5') == FALSE,P:=1-exp(-(-log(1-P)/3))]
      dat$tps[dat$tps$X %in% c('PCVD','P4_3','P10_5') == FALSE,Live:=1-exp(-(-log(1-Live)/3))]
      Log("Three-year TPs (CLHLS) was changed to One-year.", settings = settings)
    }
    
    #transpose table
    x.1<-dcast.data.table(dat$tps, formula=Iter + Gender+Age~X, value.var = "Live")
    
    #making sure that PCIND==0 for age below 50
    x.1[Age<50, PCIND:=0]
    x.1[Age<50, `:=`(P2_3 = 0,P4_3 = 0,
                     P4_7 = 0,P3_6 = 0,
                     P7_4 = 0,P6_3 = 0,
                     P10_7 = 0)]
    #Add calendar effect
    
    setnames(dat$cvd_caleff, "caleff","cvdcaleff")
    setnames(dat$cind_caleff, "caleff","cindcaleff")
    setnames(dat$p10_caleff, "caleff","p10caleff")
    
    dat$cvd_caleff[,cvdcaleff2:=cumprod(cvdcaleff),by=list(Gender, Age)]
    dat$cind_caleff[,cindcaleff2:=cumprod(cindcaleff),by=list(Gender, Age)]
    dat$p10_caleff[,p10caleff2:=cumprod(p10caleff),by=list(Gender, Age)]
    
    
    class(dat$cvd_caleff$Gender)
    class(x.1$Gender)
    
    setkey(x.1, Gender, Age)
    setkey(dat$cvd_caleff, Gender, Age)
    x.1<-x.1[dat$cvd_caleff,allow.cartesian=T]
    
    
    setkey(x.1, Gender, Age, Year)
    setkey(dat$cind_caleff, Gender, Age, Year)
    x.1<-x.1[dat$cind_caleff]
    
    setkey(x.1, Gender, Age, Year)
    setkey(dat$p10_caleff, Gender, Age, Year)
    x.1<-x.1[dat$p10_caleff]
    
    #adding CVD calendar effect
    x.1[,PCVD:=PCVD*cvdcaleff2, by=list(Iter,Gender, Age )]
    x.1[,P10_5:=P10_5*cvdcaleff2, by=list(Iter,Gender, Age )]  #add CVD cal eff also for P10_5
    x.1[,P4_3:=P4_3*cvdcaleff2, by=list(Iter,Gender, Age )]
    
    #adding CIND calendar effect
    x.1[,PCIND:=PCIND*cindcaleff2, by=list(Iter,Gender, Age )]
    x.1[,P2_3:=P2_3*cindcaleff2, by=list(Iter,Gender, Age )]
    x.1[,P10_7:=P10_7*cindcaleff2, by=list(Iter,Gender, Age )]  #add CIND cal eff also for P10_7
    
    #adding P10 calendar effect
    x.1[,P1_10:=P1_10*p10caleff2, by=list(Iter,Gender, Age )]
    
    Log("Calendar effects applied.", settings = settings)
    
    x.1[,`:=`(cvdcaleff=NULL, cindcaleff=NULL)]
    
    # CVD is two genderal
    # calculating tps coming from TPCIND and TPCVD
    x.1[,P1_3:=PCVD*PCIND ]
    x.1[,P1_2:=PCVD-P1_3]
    x.1[,P1_4:=PCIND-P1_3]
    
    # calculating tps coming from P10_5 and P10_7
    x.1[,P10_6:=P10_5*P10_7]
    x.1[,P10_5:=P10_5-P10_6]
    x.1[,P10_7:=P10_7-P10_6]
    
    
    # assuming no dementia risk in subjects below age of 50
    x.1[Age<50, P1_3:=0]
    x.1[Age<50, P1_4:=0]
    x.1[Age<50, P10_7:=0]
    x.1[Age<50, P10_6:=0]
    
    #read & prepare tps to deaths 
    stochiters<-iters[iters!=0]
    
    # Log("Preparing mortality sample...", settings = settings)
    use.sample.mortality=F
    uncert.tp.deaths = T
    
    x<-NULL
    x0<-NULL
    
    if (Mortality_SE == T)
    {
      dt <- data.table(readRDS(Mortality))
      dt <- dt[,c('Year','Age','Gender','Disease','Rate','se')]
      #if (settings$test)
      
      # 构建多个随机的概率
      qtls<-runif(stochiters)
      # 在确定Rate se后按照不同概率随机抽取数字
      # list(Vx=qnorm(qtls, dt$Rate, dt$se),Iter=stochiters, Rate=dt$Rate)
      # 即随机死亡的概率
      x<-dt[, list(Vx=qnorm(qtls, Rate, se),Iter=stochiters, Rate=Rate), 
            by=list(Year, Age, Gender, Disease)]
      
      # 设置第一年为0
      x[,Rate:=NULL]
      setcolorder(x, c("Iter", "Disease", "Year", "Gender", "Age", "Vx" ))
      if (0 %in% iters)
      {
        setnames(dt, "Rate","Vx")
        dt[,Iter:=0] 
        dt[,se:=NULL]
        x<-rbind(dt, x)
      }
    }else{
      dt <- data.table(readRDS(Mortality))
      dt <- dt[,c('Year','Age','Gender','Disease','Rate','upper','lower')]
      #if (settings$test)
      
      # 构建多个随机的概率
      # 根据iteration次数抽取多次死亡率数据
      mort_sample <- function(qtls,lower,upper){
        mort_data <- data.table(Rate = runif(stochiters,min = lower, max = upper), # 多大的概率落到95%CI上下限差值
                               stochiters  = 1:length(stochiters))
        return(mort_data)
      }

      x<-dt[, list(Vx = mort_sample(qtls,lower,upper)$Rate,
                   Iter = mort_sample(qtls,lower,upper)$stochiters),
            by=list(Year, Age, Gender, Disease)]
      
      # 设置第一年为0
      setcolorder(x, c("Iter", "Disease", "Year", "Gender", "Age", "Vx" ))
      if (0 %in% iters)
      {
        setnames(dt, "Rate","Vx")
        dt[,Iter:=0] 
        dt[,upper:=NULL]
        dt[,lower:=NULL]
        x<-rbind(dt, x)
      }
    }
    
    x<-x[Year %in% c(settings$startYear:settings$finalYear)]
    if (nrow(x[Vx<0])>0) 
    {
      #(warning(paste0(nrow(x[Vx<0]), " mortality values are <0")))
      Log("%s mortality values are <0. Changing these values to zero", nrow(x[Vx<0]), settings = settings)
    }
    x[Vx<0, Vx:=0]
    Log("Preparing mortality sample done.", settings = settings)
    
    # return(x)
    
    xx <- copy(x)
    rm(x)
    setkey(xx, Age, Gender, Disease)
    setkey(dat$mortprop,Age, Gender, Disease)
    
    
    x.2a <- dat$mortprop[xx[Disease=="CVD"]]
    x.2a[,':='(P1_8=X1*Vx,P2_8=X2*Vx,P3_8=X3*Vx,P4_8=X4*Vx,
               P5_8=X5*Vx,P6_8=X6*Vx,P7_8=X7*Vx,P10_8=X10*Vx)]
    x.2a[,':='(X1=NULL,X2=NULL,X3=NULL,X4=NULL,
               X5=NULL,X6=NULL,X7=NULL,X10=NULL, Vx=NULL, Disease=NULL)]
    
    x.2b<-dat$mortprop[xx[Disease=="nCVD"]]
    x.2b[,':='(P1_9=X1*Vx,P2_9=X2*Vx,P3_9=X3*Vx,P4_9=X4*Vx,
               P5_9=X5*Vx,P6_9=X6*Vx,P7_9=X7*Vx,P10_9=X10*Vx)]
    x.2b[,':='(X1=NULL,X2=NULL,X3=NULL,X4=NULL,X5=NULL,
               X6=NULL,X7=NULL, X10=NULL, Vx=NULL, Disease=NULL)]
    
    
    Log("Binding mortality TPs and other TPs together...", settings = settings)
    #Binding all TPs together
    
    setkey(x.2a, Iter, Gender, Year, Age)
    setkey(x.2b, Iter, Gender, Year, Age)
    
    x.2<-x.2a[x.2b]
    
    setkey(x.1, Iter, Gender, Year, Age)
    setkey(x.2, Iter, Gender, Year, Age)
    
    x<-x.1[x.2]
    Log("Done.", settings = settings)
    
    Log("Calculating recurrent TPs...", settings = settings)
    # x<-RecurrentStates(tp=x)
    # RecurrentStates
    # RecurrentStates<-function(tp, scaling=T)
    
    #tps can not be negative
    # chinese version
    tpnames<-c("1_2","1_3","1_4","1_8","1_9","1_10",# health
               "2_3","2_5","2_8","2_9",
               "3_6","3_8","3_9",
               "4_3","4_7","4_8","4_9",
               "5_2","5_8","5_9",
               "6_3","6_8","6_9",
               "7_4","7_8","7_9",
               "10_1","10_5","10_6","10_7","10_8","10_9")
    tp <- copy(x)
    for (tn in tpnames)
    {
      tp[eval(parse(text=paste0("P",tn,"<0"))),
         eval(parse(text=paste0("P",tn,":=0")))]
    }
    
    #recurrent states
    
    tp[,P1_1:=1-(P1_2+P1_3+P1_4+P1_8+P1_9+P1_10)]
    tp[,P2_2:=1-(P2_3+P2_5+P2_8+P2_9)]
    tp[,P3_3:=1-(P3_6+P3_8+P3_9)]
    tp[,P4_4:=1-(P4_3+P4_7+P4_8+P4_9)]
    tp[,P5_5:=1-(P5_2+P5_8+P5_9)]
    tp[,P6_6:=1-(P6_3+P6_8+P6_9)]
    tp[,P7_7:=1-(P7_4+P7_8+P7_9)]
    tp[,P10_10:=1-(P10_1+P10_5+P10_6+P10_7+P10_8+P10_9)]
    tp[,P8_8:=1]
    tp[,P9_9:=1]

    scaling = T # must!
    if (scaling)
    {
      while(sum(tp[,c('P1_1','P2_2','P3_3','P4_4',
                      'P5_5','P6_6','P7_7','P10_10')] < 0) >= 1){
        #scaling to unit lenght
        #P1_1
        tp[,EVL:=NA]
        class(tp$EVL)<-"double"
        tp[P1_1< 0,EVL:= sqrt(P1_2 ^ 2 + P1_3 ^ 2 + P1_4 ^ 2 + P1_8 ^ 2 +
                                P1_9 ^ 2 +P1_10 ^ 2)]
        tp[P1_1< 0,`:=`(
          P1_1 = 0,
          P1_2 = (P1_2 / EVL) ^ 2,
          P1_3 = (P1_3 / EVL) ^ 2,
          P1_4 = (P1_4 / EVL) ^ 2,
          P1_8 = (P1_8 / EVL) ^ 2,
          P1_9 = (P1_9 / EVL) ^ 2,
          P1_10 =(P1_10 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        
        #P2_2
        tp[P2_2 < 0,EVL:= sqrt(P2_3 ^ 2 + P2_5 ^ 2 + P2_8 ^ 2 + P2_9 ^ 2)]
        tp[P2_2 < 0,`:=`(
          P2_2 = 0,
          P2_3 = (P2_3 / EVL) ^ 2,
          P2_5 = (P2_5 / EVL) ^ 2,
          P2_8 = (P2_8 / EVL) ^ 2,
          P2_9 = (P2_9 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        #P3_3
        tp[P3_3 < 0,EVL:= sqrt(P3_6 ^ 2 + P3_8 ^ 2 + P3_9 ^ 2)]
        tp[P3_3 < 0,`:=`(
          P3_3 = 0,
          P3_6 = (P3_6 / EVL) ^ 2,
          P3_8 = (P3_8 / EVL) ^ 2,
          P3_9 = (P3_9 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        #P4_4
        tp[P4_4 < 0,EVL:= sqrt(P4_3 ^ 2 + P4_7 ^ 2 + P4_8 ^ 2 + P4_9 ^ 2)]
        tp[P4_4 < 0,`:=`(
          P4_4 = 0,
          P4_3 = (P4_3 / EVL) ^ 2,
          P4_7 = (P4_7 / EVL) ^ 2,
          P4_8 = (P4_8 / EVL) ^ 2,
          P4_9 = (P4_9 / EVL) ^ 2
        )]
        tp[,EVL := NA]
        
        #P5_5
        tp[P5_5 < 0,EVL:= sqrt(P5_2 ^ 2 + P5_8 ^ 2 + P5_9 ^ 2)]
        tp[P5_5 < 0,`:=`(
          P5_5 = 0,
          P5_2 = (P5_2 / EVL) ^ 2,
          P5_8 = (P5_8 / EVL) ^ 2,
          P5_9 = (P5_9 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        #P6_6
        tp[P6_6 < 0,EVL:= sqrt(P6_3 ^ 2 + P6_8 ^ 2 + P6_9 ^ 2)]
        tp[P6_6 < 0,`:=`(
          P6_6 = 0,
          P6_3 = (P6_3 / EVL) ^ 2,
          P6_8 = (P6_8 / EVL) ^ 2,
          P6_9 = (P6_9 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        #P7_7
        tp[P7_7 < 0,EVL:= sqrt(P7_4 ^ 2 + P7_8 ^ 2 + P7_9 ^ 2)]
        tp[P7_7 < 0,`:=`(
          P7_7 = 0,
          P7_4 = (P7_4 / EVL) ^ 2,
          P7_8 = (P7_8 / EVL) ^ 2,
          P7_9 = (P7_9 / EVL) ^ 2
        )]
        tp[,EVL:= NA]
        
        #P10_10
        tp[P10_10 < 0,EVL:= sqrt(P10_1 ^ 2 + P10_5 ^ 2 + P10_6 ^ 2 + P10_7 ^ 2 + P10_8 ^ 2 + P10_9 ^ 2)]
        tp[P10_10 < 0,`:=`(
          P10_10 = 0,
          P10_1 = (P10_1 / EVL) ^ 2,
          P10_5 = (P10_5 / EVL) ^ 2,
          P10_6 = (P10_6 / EVL) ^ 2,
          P10_7 = (P10_7 / EVL) ^ 2,
          P10_8 = (P10_8 / EVL) ^ 2,
          P10_9 = (P10_9 / EVL) ^ 2
        )]
        tp[,EVL:= NULL]
        
        
        #recalculating recurrent states
        tp[,P1_1:=1-(P1_2+P1_3+P1_4+P1_8+P1_9+P1_10)]
        tp[,P2_2:=1-(P2_3+P2_5+P2_8+P2_9)]
        tp[,P3_3:=1-(P3_6+P3_8+P3_9)]
        tp[,P4_4:=1-(P4_3+P4_7+P4_8+P4_9)]
        tp[,P5_5:=1-(P5_2+P5_8+P5_9)]
        tp[,P6_6:=1-(P6_3+P6_8+P6_9)]
        tp[,P7_7:=1-(P7_4+P7_8+P7_9)]
        tp[,P10_10:=1-(P10_1+P10_5+P10_6+P10_7+P10_8+P10_9)]
        tp[,P8_8:=1]
        tp[,P9_9:=1]
      }
    }
    
    tp.final <- copy(tp)

    
    tpnames<-names( tp.final )[(names( tp.final ) %in% c("Iter","Year","Age","Gender")==F)]
    setcolorder( tp.final ,c("Iter","Year","Age","Gender",sort(tpnames)))
    
    
    return( tp.final )
  }
  
  dat<-copy(res)
  dat[["tps"]] <- tps_prepare(dat = dat,
                              Mortality_SE =  Mortality_SE ,
                              TPs_data_from = TPs_data,
                              Mortality = Mortality)
  
  if (is.null(Adjust_prop_year)){
    dat <- copy(dat)
    Log("Calculating recurrent TPs - Done.", settings = settings)
  }else if (!is.null(Adjust_prop_year) & 
            is.numeric(Adjust_prop_year)){
    # Adjust_prop_year must be numeric 
    # for example, Adjust_prop_year = c(2015:2019)
    # Mortality in 
    Mortality_gbd <- readRDS(Mortality) 
    Mortality_gbd2015 <- Mortality_gbd[Mortality_gbd$Year %in% c(Adjust_prop_year),]
    
    # Prevalence in 2015 
    total2015 <- dat$inipop
    pre2015 <- dat$prev; 
    pre20150 <- pre2015[pre2015$Iter == 0,]
    pre20150m <- melt(data = pre20150[,-1], id = c('Age','Gender'))
    # weight in each states
    prop2015 <- dat$mortprop
    
    # Ideal death: all pop and all mortality 
    
    fact_death <- merge(total2015,Mortality_gbd2015,id = c('Year','Age','Gender'))
    fact_death$idea_death <- fact_death$Population*fact_death$Rate
    
    # True death: based on IMPACT-Model
    pre_state <- merge(total2015,pre20150,id.vars = c('Age','Gender'))
    true_death <- merge(pre_state,Mortality_gbd2015,by = c('Year','Age','Gender'))
    
    true_deathm <- melt(true_death[,c('Year','Age','Gender',
                                      'Disease','Rate',
                                      'Population',paste0('s.',c(1:7,10)))],
                                  id.vars = c('Year','Age','Gender',
                                              'Disease','Rate','Population'),
                        value.name = 'prevalence')
    # add weight
    names(prop2015)[4:11] <- c(paste0('s.',c(1:7,10)))
    prop2015m <- melt(data = prop2015,id = c('Disease','Age','Gender'),
                      value.name = 'weight.death')
    
    # addjust process
    weight <- merge(true_deathm,prop2015m,
                    by =c('Disease','Age',
                          'Gender','variable') )
    
    weight <- weight[order(weight$Age,weight$Gender,
                           weight$Disease,weight$variable),]
    
    weight$true_death <- weight$Population * weight$prevalence * 
      weight$Rate * weight$weight.death
    
    weight_true <- aggregate(data = weight, 
                             true_death ~ Age+Gender+Disease,
                             FUN = sum)
    # add fact
    
    weight_adjust <- merge(fact_death[,c('Age','Gender','Disease','idea_death')],
                           weight_true[,c('Age','Gender','Disease','true_death')],
                           id =c('Age','Gender','Disease') )
    
    weight_adjust$adj_wieght <- weight_adjust$idea_death/weight_adjust$true_death
    
    prop_mortality <- dat$mortprop
    prop_adjust <- merge(weight_adjust[,c('Age','Gender','Disease','adj_wieght')],
                         prop_mortality,by = c('Age','Gender','Disease'))
    
    prop_adjust[,`:=`(X1 = X1*adj_wieght,
                      X2 = X2*adj_wieght,
                      X3 = X3*adj_wieght,
                      X4 = X4*adj_wieght,
                      X5 = X5*adj_wieght,
                      X6 = X6*adj_wieght,
                      X7 = X7*adj_wieght,
                      X10 = X10*adj_wieght)]
    
    prop_adjust[,adj_wieght := NULL]
    rm(dat)
    dat<-copy(res)
    dat[["mortprop"]] <- prop_adjust
    dat[["tps"]] <- tps_prepare(dat = dat,
                                TPs_data_from = TPs_data,
                                Mortality = Mortality)
    Log("Calculating recurrent TPs - Done by adjust: %d-%d",
        min(Adjust_prop_year),max(Adjust_prop_year),
        settings = settings)
  }
  return(dat)
}

# Tetsing 
# iters <- 1
# scenario=NULL
# model="Baseline"
# settings=settings
# scenario.rf=NULL
# scenario.cont.RR=NULL
# scenario.cont.rf.baseline=NULL
# scenario.cont.func="subtraction"
# save.pif.table=T
# pif.output.file="pif_intermediate_scenario.csv"
# pif.scenario.file=NULL
# remove.tps=T
# inputs=dat


# RunModelMultithrd_China()
# Run core
RunModelMultithrd_China<-function(model="Baseline",
                                  inputs=dat,
                                  scenario="Baseline",
                                  scenario.rf=NULL,
                                  scenario.cont.RR=NULL, 
                                  scenario.cont.rf.baseline=NULL, 
                                  pif.scenario.file=NULL,
                                  pif.output.file="pif_intermediate_output.csv",
                                  save.pif.table=T,
                                  scenario.cont.func=function(X){X-5},
                                  #mort=system.file("extdata/10","mortality_33_1100i.csv",package="bam"),
                                  #calib.data.mort=NA,
                                  remove.tps=T,
                                  settings=settings)

{
  # model="Baseline"
  # inputs=dat
  # scenario="Baseline"
  # scenario.rf=NULL
  # scenario.cont.RR=NULL
  # scenario.cont.rf.baseline=NULL
  # pif.scenario.file=NULL
  # pif.output.file="pif_intermediate_output.csv"
  # save.pif.table=T
  # scenario.cont.func=function(X){X-5}
  # mort=system.file("extdata/10","mortality_33_1100i.csv",package="bam"),
  # calib.data.mort=NA,
  # remove.tps=T
  # settings=settings
  # # set parameter
  require(foreach)
  require(data.table)
  require(doParallel)
  require(ggplot2)
  
  model=model
  inputs=inputs
  
  Log("\n\nStarting model calculations, scenario: '%s'",model, settings = settings)
  
  set.seed(787)
  packs<-settings$PARALLEL.PACKS
  packSize<-settings$PARALLEL.PACKSIZE
  
  is<-list()
  is[[1]]<-c(0:packSize)
  
  if (packs>1) for (px in c(2:packs))
  {
    is[[px]]<-(px-1)*packSize+c(1:packSize)
  }
  # settings$USE.CORES <- settings$PARALLEL.PACKS
  # cl <- makeCluster(detectCores())
  registerDoParallel(settings$USE.CORES)
  Log(paste0("No of parallels workers registered: ", 
             getDoParWorkers(), ", ", settings$USE.CORES, " cores used."),
      settings=settings)
  
  Log(paste0("Parallel backend: ", getDoParName()), settings=settings)


  RunModel<-function(dat)
  {
    tps<-dat$tps
    prev<-dat$prev
    inipop<-dat$inipop
    proj<-dat$projpop
    # extract names of tps variables from input data set using regexp
    tpnames <- names(tps)[grep(pattern="^P[0-9]+_[0-9]+$", x=names(tps), perl=T)]
    
    #
    to<-as.numeric(sapply(strsplit(tpnames, split = "_"), "[", 2))
    from<-  as.numeric(substr(sapply(strsplit(tpnames, split = "_"), "[", 1),2,10))
    
    statenames<-names(prev)[grep(pattern="^s.[0-9]+$", x=names(prev), perl=T)]
    
    all.states<-paste0("s.",unique(c(from,to)))
    state.numbers<-unique(c(from,to))
    
    
    #filling with zero number of subject in states which are not defined in dataset but are existing in tp table
    empty.states<-all.states[(all.states %in% statenames) ==F]
    for (st in empty.states) prev[,eval(st):=0]
    
    start.year<-min(tps$Year)
    final.year<-max(tps$Year)
    min.age<-min(tps$Age)
    
    setkey(prev, Gender, Age)
    setkey(inipop, Gender, Age )
    
    startpop<-prev[inipop]
    for (st in all.states) startpop[,eval(st):=eval(parse(text=paste0(st,"* Population")))]
    
    startpop[,Population:=NULL]
    startpop[,Year:=NULL]
    startpop[,eval(parse(text=(paste0("`:=`(",paste0("MOV",from,"_",to,"=NA", collapse=","),")"))))]
    
    # if(y == start.year) out_first <- copy(startpop)

    out<-list()
    for (y in start.year:(final.year)) #Iterating though years
    {
      tpy<-tps[Year==y]
      tpy[,Year:=NULL]
      
      if ("Iter" %in% names(startpop))
      {
        setkey(tpy,Iter, Gender, Age)
        setkey(startpop,Iter, Gender, Age)
        
      } else
      {
        setkey(tpy,Gender, Age)
        setkey(startpop,Gender, Age)
      }
      prev1<-copy(tpy[startpop]) # 准备impact过程的数据集
      rm(startpop)
      
      
      for (ns in unique(to)) prev1[,paste0("ns.",ns):=0]
      for (i in 1:length(from)) # Iterating though all tps
      {
        prev1[,paste0("MOVx",from[i],"_",to[i]):=eval(parse(text=paste0("P",from[i],"_",to[i],"* s.",from[i])))]
        prev1[,paste0("ns.",to[i]):=eval(parse(text=paste0("ns.",to[i],"+MOVx",from[i],"_",to[i])))]
      }
      # 运行完数据集
      
      # now ns.x are s.x & MOVx are MOV for next year
      startpop<-copy(prev1[,c("Iter","Gender", "Age", 
                              paste0("ns.",unique(to)),paste0("MOVx",from,"_",to)), 
                           with=F])
      
      setnames(startpop, paste0("ns.",unique(to)), paste0("s.",unique(to)))
      setnames(startpop,paste0("MOVx",from,"_",to),paste0("MOV",from,"_",to))
      
      startpop[, Age:=Age+1]
      startpop[Age==101, Age:=35]
      startpop[Age==35,eval(parse(text=(paste0("`:=`(",paste0("s.",unique(to),"=0", collapse=","),")"))))]
      startpop[Age==35,eval(parse(text=(paste0("`:=`(",paste0("MOV",from,"_",to,"=0", collapse=","),")"))))]
      
      #now set new values from age 35 from population projection
      px<-proj[Year==y+1]
      
      setkey(px, Gender, Age)
      setkey(startpop, Gender, Age)
      
      startpop<-px[startpop,]
      startpop[Age==35,eval(parse(text=(paste0("`:=`(",paste0("s.",unique(to),"=0", collapse=","),")"))))]
      startpop[Age==35,s.1:=Population]
      startpop[,`:=`(Year=NULL, Population=NULL)]
      
      prev1<-prev1[,c("Iter","Gender","Age", paste0("s.",unique(to)), 
                      paste0("MOVx",from,"_",to), # orgin is MOV, now change to the movx,record this year death
                      tpnames), with=F]
      setnames(prev1,paste0("MOVx",from,"_",to),paste0("MOV",from,"_",to))
      prev1[,Year:=y]
      
      out[[paste(y)]]<-prev1[,c("Iter","Gender","Year","Age", paste0("s.",unique(to)), paste0("MOV",from,"_",to), tpnames), with=F]
      rm(prev1)
    }
    out<-rbindlist(out)
    # modified: 20220622 - add 2015 start year data
    # out_startyear<-list()
    
    rm(prev, prev1, startpop, px, tpy, tps)
    gc()
    
    # setkey(tps, Iter, Gender, Year, Age)
    # setkey(res, Iter, Gender, Year, Age)
    
    # res<-res[tps]
    
    return(out[Year>=start.year])
  }
  
  SummarizeGender<-function(m)
  {
    
    m<-m[Gender %in% c("Men","Women")]  
    
    toSum<-c("s.1","s.10","s.2","s.3","s.4","s.8","s.9","s.5","s.6","s.7",
             "MOV1_1","MOV1_10","MOV1_2","MOV1_3","MOV1_4","MOV1_8","MOV1_9",
             "MOV10_1","MOV10_10","MOV10_5","MOV10_6","MOV10_7","MOV10_8",
             "MOV10_9","MOV2_2","MOV2_3","MOV2_5","MOV2_8","MOV2_9","MOV3_3",
             "MOV3_6","MOV3_8","MOV3_9","MOV4_3","MOV4_4","MOV4_7","MOV4_8",
             "MOV4_9","MOV5_2","MOV5_5","MOV5_8","MOV5_9","MOV6_3","MOV6_6",
             "MOV6_8","MOV6_9","MOV7_4","MOV7_7","MOV7_8","MOV7_9","MOV8_8","MOV9_9"
             # "utility.s.1","utility.s.10","utility.s.2","utility.s.3","utility.s.4","utility.s.5",
             # "utility.s.6","utility.s.7","utility.allstates",
             # "total.hc.cost.s.1","total.hc.cost.s.10","total.hc.cost.s.2",
             # "total.hc.cost.s.3","total.hc.cost.s.4","total.hc.cost.s.5",
             # "total.hc.cost.s.6","total.hc.cost.s.7","total.hc.cost.allstates",
             # "total.soc.care.cost.s.1","total.soc.care.cost.s.10","total.soc.care.cost.s.2",
             # "total.soc.care.cost.s.3","total.soc.care.cost.s.4","total.soc.care.cost.s.5",
             # "total.soc.care.cost.s.6","total.soc.care.cost.s.7","total.soc.care.cost.allstates",
             # "variable","cost","utility.lost.Dementia","utility.lost.CVD","utility.lost.DementiaAndCVD"
    ) # "variable" is used sometimes as temporary
    
    # costnames<-c("sc.excl.daycare","sc.daycare", "hc.prescribing", "hc.hes.inpatient", "hc.hes.outpatient", 
    #              "hc.hes.ae", "hc.other", "ic.total", "sc.total","hc.total", "hcscic.total","utility","utility.weight",
    #              "qaly.value.total","qalyhcscic.value.total","sc.noninst.total", "sc.institutional.total" ) 
    states<-c(paste0("s.",c(1:7,10)),"allstates","CVD","Dementia","DementiaAndCVD","Disability.no.dementia")
    # coststates<-apply(expand.grid(costnames,".",states),1, paste0, collapse="")
    
    # toSum<-c(toSum,coststates)
    toSum<-c(toSum)
    exNames<-names(m)
    
    
    #sum only variables existing in m which are listed in toSum
    nx<-exNames[exNames %in% toSum]
    
    nxx<-paste0("list(",paste0(paste0(nx,"=sum(",nx,")"),collapse=","),")")
    m_cop <- copy(m)
    if ("AgeGrp" %in% names(m))  mA<-m[,eval(parse(text=nxx)),by=list(Model, Iter, Year, Age, AgeGrp)] else
      mA<-m[,eval(parse(text=nxx)),by=list(Model, Iter, Year, Age)]
    mA[,Gender:="All"]
    
    m <- (rbindlist(list(m_cop,mA), use.names=T, fill=T))
    return(m)
  }
  
    
  RunProcess <- function(iters,
                         inputs=dat, 
                         scenario=NULL, 
                         model="Baseline", 
                         settings=settings, 
                         scenario.rf=NULL, 
                         scenario.cont.RR=NULL, 
                         scenario.cont.rf.baseline=NULL, 
                         scenario.cont.func="subtraction",
                         save.pif.table=T,
                         pif.output.file="pif_intermediate_scenario.csv",
                         pif.scenario.file=NULL,
                         remove.tps=T){
    
    # inputs=dat
    # scenario=NULL
    # model="Baseline"
    # settings=settings
    # scenario.rf=NULL
    # scenario.cont.RR=NULL
    # scenario.cont.rf.baseline=NULL
    # scenario.cont.func="subtraction"
    # save.pif.table=T
    # pif.output.file="pif_intermediate_scenario.csv"
    # pif.scenario.file=NULL
    # remove.tps=T
    # iters = 1
    
    require(data.table)
    
    prev<-NULL
    # Log("(%d-%d): Starting model run...",min(iters),max(iters), settings=settings)
    
    subsetInputs<-function(d, iters)
    {
      dx<-copy(d)
      dx$prev<-dx$prev[Iter %in% iters]
      dx$tps<-dx$tps[Iter %in% iters]
      return(dx)
    }
    
    # Log("(%d-%d): Loading pre-prepared input data.",min(iters),max(iters), settings=settings)
    d<-subsetInputs(inputs, iters)
    
    dnames<-names(d$tps)
    
    ### Applying discrete risk factors scenario 
    if (is.null(scenario)==F)
    {
      Log <- function(text, settings=default.settings(), ...) 
      {
        msg <- sprintf(paste0(as.character(Sys.time()), ": ", text), ...)
        if (settings$LOG.TO.CONSOLE) cat(paste0(msg,"\n"))
        if (settings$LOG.TO.FILE)   write(paste0(msg),paste0(settings$LOG_PATH,"/",settings$LOG.FILE.NAME),append=TRUE)
        if (settings$LOG.TO.SOCKET)
        {
          con<-make.socket(port=4000)
          write.socket(con, paste0(msg,"#\n"))
          close.socket(con)
        }
      }
      
      RecurrentStates<-function(tp, scaling=T)
      {
        #tps can not be negative
        tpnames<-c("1_2","1_3","1_4","1_8","1_9","1_10","2_3","2_5","2_8","2_9","3_6","3_8","3_9","4_3",
                   "4_7","4_8","4_9","5_2","5_8","5_9","6_3","6_8","6_9","7_4","7_8","7_9",
                   "10_1","10_5","10_6","10_7","10_8","10_9")
        for (tn in tpnames)
        {
          tp[eval(parse(text=paste0("P",tn,"<0"))),eval(parse(text=paste0("P",tn,":=0")))]
        }
        
        #recurrent states
        
        tp[,P1_1:=1-(P1_2+P1_3+P1_4+P1_8+P1_9+P1_10)]
        tp[,P2_2:=1-(P2_3+P2_5+P2_8+P2_9)]
        tp[,P3_3:=1-(P3_6+P3_8+P3_9)]
        tp[,P4_4:=1-(P4_3+P4_7+P4_8+P4_9)]
        tp[,P5_5:=1-(P5_2+P5_8+P5_9)]
        tp[,P6_6:=1-(P6_3+P6_8+P6_9)]
        tp[,P7_7:=1-(P7_4+P7_8+P7_9)]
        tp[,P10_10:=1-(P10_1+P10_5+P10_6+P10_7+P10_8+P10_9)]
        tp[,P8_8:=1]
        tp[,P9_9:=1]
        
        
        if (scaling)
        {
          #scaling to unit lenght
          
          #P1_1
          tp[,EVL:=NA]
          class(tp$EVL)<-"double"
          tp[P1_1< 0,EVL:= sqrt(P1_2 ^ 2 + P1_3 ^ 2 + P1_4 ^ 2 + P1_8 ^ 2 +
                                  P1_9 ^ 2 +P1_10 ^ 2)]
          tp[P1_1< 0,`:=`(
            P1_1 = 0,
            P1_2 = (P1_2 / EVL) ^ 2,
            P1_3 = (P1_3 / EVL) ^ 2,
            P1_4 = (P1_4 / EVL) ^ 2,
            P1_8 = (P1_8 / EVL) ^ 2,
            P1_9 = (P1_9 / EVL) ^ 2,
            P1_10 =(P1_10 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          
          #P2_2
          tp[P2_2 < 0,EVL:= sqrt(P2_3 ^ 2 + P2_5 ^ 2 + P2_8 ^ 2 + P2_9 ^ 2)]
          tp[P2_2 < 0,`:=`(
            P2_2 = 0,
            P2_3 = (P2_3 / EVL) ^ 2,
            P2_5 = (P2_5 / EVL) ^ 2,
            P2_8 = (P2_8 / EVL) ^ 2,
            P2_9 = (P2_9 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          #P3_3
          tp[P3_3 < 0,EVL:= sqrt(P3_6 ^ 2 + P3_8 ^ 2 + P3_9 ^ 2)]
          tp[P3_3 < 0,`:=`(
            P3_3 = 0,
            P3_6 = (P3_6 / EVL) ^ 2,
            P3_8 = (P3_8 / EVL) ^ 2,
            P3_9 = (P3_9 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          #P4_4
          tp[P4_4 < 0,EVL:= sqrt(P4_3 ^ 2 + P4_7 ^ 2 + P4_8 ^ 2 + P4_9 ^ 2)]
          tp[P4_4 < 0,`:=`(
            P4_4 = 0,
            P4_3 = (P4_3 / EVL) ^ 2,
            P4_7 = (P4_7 / EVL) ^ 2,
            P4_8 = (P4_8 / EVL) ^ 2,
            P4_9 = (P4_9 / EVL) ^ 2
          )]
          tp[,EVL := NA]
          
          #P5_5
          tp[P5_5 < 0,EVL:= sqrt(P5_2 ^ 2 + P5_8 ^ 2 + P5_9 ^ 2)]
          tp[P5_5 < 0,`:=`(
            P5_5 = 0,
            P5_2 = (P5_2 / EVL) ^ 2,
            P5_8 = (P5_8 / EVL) ^ 2,
            P5_9 = (P5_9 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          #P6_6
          tp[P6_6 < 0,EVL:= sqrt(P6_3 ^ 2 + P6_8 ^ 2 + P6_9 ^ 2)]
          tp[P6_6 < 0,`:=`(
            P6_6 = 0,
            P6_3 = (P6_3 / EVL) ^ 2,
            P6_8 = (P6_8 / EVL) ^ 2,
            P6_9 = (P6_9 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          #P7_7
          tp[P7_7 < 0,EVL:= sqrt(P7_4 ^ 2 + P7_8 ^ 2 + P7_9 ^ 2)]
          tp[P7_7 < 0,`:=`(
            P7_7 = 0,
            P7_4 = (P7_4 / EVL) ^ 2,
            P7_8 = (P7_8 / EVL) ^ 2,
            P7_9 = (P7_9 / EVL) ^ 2
          )]
          tp[,EVL:= NA]
          
          #P10_10
          tp[P10_10 < 0,EVL:= sqrt(P10_1 ^ 2 + P10_5 ^ 2 + P10_6 ^ 2 + P10_7 ^ 2 + P10_8 ^ 2 + P10_9 ^ 2)]
          tp[P10_10 < 0,`:=`(
            P10_10 = 0,
            P10_1 = (P10_1 / EVL) ^ 2,
            P10_5 = (P10_5 / EVL) ^ 2,
            P10_6 = (P10_6 / EVL) ^ 2,
            P10_7 = (P10_7 / EVL) ^ 2,
            P10_8 = (P10_8 / EVL) ^ 2,
            P10_9 = (P10_9 / EVL) ^ 2
          )]
          tp[,EVL:= NULL]
          
          
          #recalculating recurrent states
          tp[,P1_1:=1-(P1_2+P1_3+P1_4+P1_8+P1_9+P1_10)]
          tp[,P2_2:=1-(P2_3+P2_5+P2_8+P2_9)]
          tp[,P3_3:=1-(P3_6+P3_8+P3_9)]
          tp[,P4_4:=1-(P4_3+P4_7+P4_8+P4_9)]
          tp[,P5_5:=1-(P5_2+P5_8+P5_9)]
          tp[,P6_6:=1-(P6_3+P6_8+P6_9)]
          tp[,P7_7:=1-(P7_4+P7_8+P7_9)]
          tp[,P10_10:=1-(P10_1+P10_5+P10_6+P10_7+P10_8+P10_9)]
          tp[,P8_8:=1]
          tp[,P9_9:=1]
          
        }
        return(tp)
      }
      
      rRR<-function(n,RR, LCL, UCL)
      {
        log.LCL=log(LCL)
        log.UCL=log(UCL)
        log.RR=log(RR)
        SE.ln.RR=(log.UCL-log.LCL)/(2*1.96)
        
        if (SE.ln.RR>0)
        {
          Mean1=log(RR)-(SE.ln.RR^2)/2
          SD=sqrt(exp(2*Mean1+2*SE.ln.RR^2)-exp(2*Mean1+SE.ln.RR^2))
          
          Corr.SE.ln.RR=sqrt(log(SD^2+exp(2*log(RR)))-2*log(RR))
          
          F.RR=exp(rnorm(n=n, mean=log(RR)-(Corr.SE.ln.RR^2)/2,sd=Corr.SE.ln.RR))
          return(F.RR)
        }
        else
        {
          # if SE.ln.RR ==0 assume no error
          return(rep(RR,n))
        }
      }
      
      read.RRandLag<-function(rf,iters=c(0:50), xlsx=F)
      {
        
        library(openxlsx)
        #rr<-as.data.table(read.xlsx(paste0(path,"Policy_layer/SmokingRR_CIs.xlsx")))
        #saveRDS(rr, file="./Inputs/Policy_layer/SmokingRR.rds")
        
        if (xlsx) rr<-as.data.table(read.xlsx(xlsxFile =rf,sheet = "RR")) else
          rr<-fread(rf)
        
        #rr<-fread(system.file("extdata/10/Policy_layer",paste0(rf,"RR.csv"), package="bam"))
        # rr<-rr[,list(Category, TP, Gender, minAge, maxAge, RR, LCL, UCL,Lag,Lag.sd, Group)]
        rr<-rr[,list(Category, TP, Gender, minAge, maxAge, RR, LCL, UCL,Lag,Lag.sd)]
        
        rr.x<-copy(rr)
        rr.x[,`:=`(LCL=NULL, UCL=NULL, Iter=0)]
        
        
        #relative risk distribution version
        #rr<-rr[,list(Iter=iters[iters!=0],RR=rRR(length(iters[iters!=0]),RR,LCL,UCL)),
        #      by=list(Category,TP,Gender,minAge,maxAge, Lag, Lag.sd, Group)]
        
        rr<-rr[,list(Iter=iters[iters!=0],RR=rRR(length(iters[iters!=0]),RR,LCL,UCL)),
               by=list(Category,TP,Gender,minAge,maxAge, Lag, Lag.sd)]
        
        if (0 %in% iters) rr<-rbind(rr.x,rr)
        
        rr[,AgeGroup:=paste0(minAge,"-",maxAge)]
        
        
        
        res<-list()
        for (i in c(1:7,10))
        {
          x1<-rr[TP=="CVD_death"]
          x1[,TP:=paste0("P",i,"_8")]
          x2<-rr[TP=="nCVD_death"]
          x2[,TP:=paste0("P",i,"_9")]
          res[[paste0(i)]]<-rbind(x1,x2)
          
        }
        x<-rbindlist(res)
        
        rr<-rr[(TP %in% c("CVD_death","nCVD_death"))==F]
        rr<-rbind(rr,x)
        
        #rr<-rr[,list(Age=c(minAge:maxAge),RR=RR),
        #       by=list(Category,Iter,TP,Gender,AgeGroup,Lag,Lag.sd, Group)]
        
        rr<-rr[,list(Age=c(minAge:maxAge),RR=RR),
               by=list(Category,Iter,TP,Gender,AgeGroup,Lag,Lag.sd)]
        rr[,Lag:=rnorm(length(Lag),Lag,Lag.sd)]
        
        rr[,Lag.sd:=NULL]
        rr[,AgeGroup:=NULL]
        
        return(rr)
      }
      
      read.Lag<-function(rf,iters)
      {
        rr<-read.RRandLag(rf,iters)
        rr<-dcast.data.table(rr, formula=Category+Iter+Gender+Age~TP, value.var = "Lag")
        
        old.names<-grep("^P[0-9]",names(rr), value = T,perl=T)
        new.names<-paste0("RR",substring(old.names,2))
        
        setnames(rr, old.names, new.names)
        
        return(rr)
      }
      
      read.RR2<-function(rf,iters)
      {
        rr<-read.RRandLag(rf,iters)
        rr<-dcast.data.table(rr, formula=Category+Iter+Gender+Age~TP, value.var = "RR")
        
        old.names<-grep("^P[0-9]",names(rr), value = T,perl=T)
        new.names<-paste0("RR",substring(old.names,2))
        
        setnames(rr, old.names, new.names)
        
        return(rr)
      }
      
      read.rf.prev2<-function(scenario.file)
      {
        res<-fread(scenario.file)
        res<-melt.data.table(res,id.vars = c("Year","Gender","Age","Scenario"),value.name = "Prev",variable.name = "Category")
        res<-dcast.data.table(res, formula = Year+Gender+Age+Category~Scenario, value.var = "Prev")
        setnames(res, c("Baseline","Scenario"),c("PrevBaseline","Prev"))
        
        return(res)
      }
      
      calc.DP2<-function(pr,RR,Lag)
      {
        # pr <- fread("./Log/prev.csv")
        # RR <- fread("./Log/rr.csv")
        # Lag<-fread("./Log/lag.csv")
        
        sce<-merge(pr, RR, by=c("Gender","Age","Category"), allow.cartesian = T)
        setkey(sce, Iter,Gender, Age, Year, Category)
        
        ### calculate PARFs ####
        #  tst<-sce[,list(Prev=sum(Prev),PrevBaseline=sum(PrevBaseline)),by=list(Iter,Gender,Age,Year)]
        #if (nrow(tst[round(Prev,digits=8)!=1])) stop("Scenario file contains final prevalences not summing to 100%")
        #if (nrow(tst[round(PrevBaseline,digits=8)!=1])) stop("Scenario file contains baseline prevalences not summing to 100%")
        
        nx<-names(sce)
        nx<-grep("^RR[0-9]",nx, value = T,perl=T)
        nx_suff<-substring(nx,3)
        
        for (i in nx_suff)
        {
          sce[,eval(paste0("X_base_",i)):=eval(parse(text=paste0("PrevBaseline*(RR",i,"-1)")))]
          sce[,eval(paste0("X_final_",i)):=eval(parse(text=paste0("Prev*(RR",i,"-1)")))]
          sce[,eval(paste0("RR",i)):=NULL]
        }
        
        
        t1<-paste0("X_base_",nx_suff,"=sum(X_base_",nx_suff,")", collapse=",")
        t2<-paste0("X_final_",nx_suff,"=sum(X_final_",nx_suff,")", collapse=",")
        
        sce<-sce[,eval(parse(text=paste0("list(",t1,",",t2,")"))),by=list(Iter,Gender,Age,Year)]
        
        for (i in nx_suff)
        {
          sce[,eval(paste0("PARF_base_",i)):=eval(parse(text=paste0("(X_base_",i,")/(1+X_base_",i,")")))]
          sce[,eval(paste0("PARF_final_",i)):=eval(parse(text=paste0("(X_final_",i,")/(1+X_base_",i,")")))]
          sce[eval(parse(text=paste0("PARF_base_",i,">PARF_final_",i))),eval(paste0("DP",i)):=eval(parse(text=paste0("1-abs(PARF_base_",i,"-PARF_final_",i,")")))]
          sce[eval(parse(text=paste0("PARF_base_",i,"<=PARF_final_",i))),eval(paste0("DP",i)):=eval(parse(text=paste0("1+abs(PARF_base_",i,"-PARF_final_",i,")")))]
          sce[eval(parse(text=paste0("is.nan(DP",i,")"))),eval(paste0("DP",i)):=1]
        }
        
        #fwrite(sce, file="sce.csv")
        
        for (i in nx_suff)
        {
          sce[,eval(paste0("PARF_base_",i)):=NULL]
          sce[,eval(paste0("PARF_final_",i)):=NULL]
          sce[,eval(paste0("X_base_",i)):=NULL]
          sce[,eval(paste0("X_final_",i)):=NULL]
        }
        
        #introducing lags
        nx<-names(Lag)
        nx<-grep("^RR[0-9]",nx, value = T,perl=T)
        nx_DP<-paste0("DP",substring(nx,3))
        setnames(Lag,nx,nx_DP)
        
        Lag<-Lag[Category==unique(Category)[1]]
        Lag[,Category:=NULL]
        
        Lag2<-melt(Lag, id.vars = c("Iter","Gender","Age"), variable.name = "TP",value.name = "Lag")
        Lag2[Lag<0, Lag:=0]
        
        sce2<-melt(sce, id.vars = c("Iter","Gender","Age","Year"), variable.name = "TP",
                   value.name = "DP")
        
        sce3<-merge(sce2,Lag2, by=c("Iter","Gender","Age","TP"))
        sce3[,Age:=Age+round(Lag)]
        sce3[,Year:=Year+round(Lag)]
        sce3[,Lag:=NULL]
        sce3<-sce3[ Age<=100 & Year<=2060 ]
        
        sce<-dcast.data.table(sce3,formula=Iter+Gender+Age+Year~TP, value.var = "DP", fun.aggregate = mean)
        
        for (i in nx_suff)
        {
          sce[eval(parse(text=paste0("is.nan(DP",i,")"))), eval(paste0("DP",i)):=1]
        }
        
        return(sce)
      }
      
      #RR.file="./Inputs/Policy_layer/Scenario_1.csv"
      Log("(%d-%d): Preparing scenario '%s'...",min(iters),max(iters), scenario, settings=settings)
      rr<-read.RR2(rf=scenario.rf, iters=iters)
      Log("(%d-%d): RR2 function done",min(iters),max(iters), settings=settings)
      Lag<-read.Lag(rf=scenario.rf, iters=iters)
      Log("(%d-%d): RRs read, file: '%s'...", min(iters),max(iters), paste0(scenario.rf), settings=settings)
      
      prev<-read.rf.prev2(scenario)
      #if (settings$DEB) fwrite(prev, file = "./Log/prev.csv")
      Log(paste0("(%d-%d): prevs read"),min(iters),max(iters), settings=settings)
      #if (settings$DEB) fwrite(rr, file = "./Log/rr.csv")
      #if (settings$DEB) fwrite(Lag, file = "./Log/lag.csv")
      
      RRs <- calc.DP2(prev, rr, Lag)
      #if (settings$DEB) fwrite(RRs, file = "./Log/RRs.csv")
      #fwrite(RRs,"RRs.csv")
      Log("(%d-%d): Preparing scenario '%s' done",min(iters),max(iters), scenario, settings=settings)
      
      RRs[,5:33] <- 1
      
      RRs[ , c("DP3_6", "DP4_7")] <- 0.8
      
      Log("(%d-%d): Applying scenario to tps...",min(iters),max(iters), settings=settings)
      tps.names<-names(RRs)
      nx<-grep("^DP[0-9]",names(RRs), value = T,perl=T)
      nx<-substring(nx,3)
      
      
      d$tps<-merge(d$tps, RRs, by=c("Iter","Year","Gender","Age"), all.x=T)
      
      #if (settings$DEB) fwrite(d$tps, file = "./Log/d_tps_with_DP.csv")
      
      for (i in nx)
      {
        d$tps[eval(parse(text=paste0("is.na(DP",i,")"))),eval(paste0("DP",i)):=1]
        d$tps[,eval(paste0("P_NEW",i)):=eval(parse(text=paste0("P",i,"*DP",i)))]
      }
      
      #fwrite(d$tps[Iter==0],"~/cloudDocs/BAM/Log/TPs-scenario.csv")
      
      
      #fwrite(d$tps[Iter==0], file=paste0("./Log/",model,"_tps.csv"))
      #fwrite(d$prev, file=paste0("./Log/",model,"_prev.csv"))
      #fwrite(d$inipop, file=paste0("./Log/",model,"_inipop.csv"))
      #fwrite(d$projpop, file=paste0("./Log/",model,"_popproj.csv"))
      
      for (i in nx)
      {
        d$tps[,eval(paste0("P",i)):=NULL]
        setnames(d$tps, paste0("P_NEW",i), paste0("P",i))
        d$tps[,eval(paste0("DP",i)):=NULL]
        
        
        negs <- nrow(d$tps[eval(parse(text=paste0("P",i, "<0")))])
        if (negs>0) Log("(%d-%d): Found %d negative values in TP %s.",min(iters),max(iters),negs,i, settings=settings)  
        
        d$tps[eval(parse(text=paste0("P",i,"<0"))),eval(parse(text=paste0("P",i,":=0")))]
      }
      
      Log("(%d-%d): Recalculating recurrent states after adding policy effect...", min(iters), max(iters), settings = settings)
      
      d$tps<-RecurrentStates(d$tps)
      
      setcolorder(d$tps,dnames)
      fwrite(d$tps[Iter==0],"./Log/TPs-final.csv")
      #d$tps<-d$tps[,tps.names, with=F]
      
      Log("(%d-%d): Applying scenario to tps done.",min(iters),max(iters), settings=settings)
    }
    
    # calc2.LE(m, method=2)
    calc2.LE<-function(m, method=1) #Outputs.R 中仅输出method=2的结果，method=1无效。修正 补充method 1代码
    {
      c.lx<-function(qx, startpop=100000)#基于给定的死亡概率和初始人群
      {
        lx<-rep(NA, times=length(qx))#qx指的是年龄的死亡概率
        lx[1]<-startpop
        for (i in 2:length(lx))
        {
          lx[i]<-lx[i-1]*(1-qx[i])
        }
        return(lx)
      }
      
      c.Lx<-function(lx, mx)
      {
        Lx<-rep(NA, times=length(lx))
        for (i in 1:length(lx))
        {
          if (i<length(lx))
          {
            Lx[i]<-0.5*(lx[i]+lx[i+1])
          }
          else
          {
            if (mx[i]==0) stop("Division by zero in function: c.lx()")
            Lx[i]<-lx[i]/mx[i]
          }
        }
        return(Lx)
      }
      gc()
      # 计算不同状态下的人数，对应不同状态的期望寿命计算
      m[,LY:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10] #这一步指的是所有存活的人的数量
      m[,LY_no_dementia:=s.1+s.2+s.3+s.4+s.5+s.10] #s.6=CVD&CI&FI s.7=Ci&FI
      m[,LY_no_disab:=s.1+s.2+s.3+s.4] #s.5.6.7=FI
      m[,LY_with_disab:=s.5+s.6+s.7+s.10]
      m[,LY_with_dementia:=s.6+s.7]
      m[,LY_healthy:=s.1]#s.1 =healthy
      m[,LY_no_cvd:=s.1+s.4+s.7+s.10]
      m[,LY_no_ci:=s.1+s.2+s.5+s.10]
      
      if (method==1)
      {

        m[,Dem_free_LY:=s.1+s.2+s.5+s.10]
        m[,Dis_free_LY:=s.1+s.2+s.3+s.4]
        m[,LY_plus:=c(LY[2:length(LY)],NA)]
        m[Age==100,LY_plus:=LY*0.97]


        m[,TotalPLY:=rev(cumsum(rev(LY))),by=list(Iter,Model,Gender,Year)]
        m[,LE:=TotalPLY/LY]


        m[,LY_plus_no_dementia:=(LY_no_dementia+c(LY_no_dementia[2:length(LY_no_dementia)],NA))/2]
        m[Age==100,LY_plus_no_dementia:=LY_no_dementia]

        m[,TotalPLY_no_dementia:=rev(cumsum(rev(LY_no_dementia))),by=list(Iter,Model,Gender,Year)]
        m[,LE_no_dementia:=TotalPLY_no_dementia/LY_no_dementia]


        # No CVD
        m[,LY_plus_no_cvd:=(LY_no_cvd+c(LY_no_cvd[2:length(LY_no_cvd)],NA))/2]
        m[Age==100,LY_plus_no_cvd:=LY_no_cvd]
        m[,TotalPLY_no_cvd:=rev(cumsum(rev(LY_no_cvd))),by=list(Iter, Model,Gender,Year)]
        m[,LE_no_cvd:=TotalPLY_no_cvd/LY_no_cvd]

        # No CI
        m[,LY_plus_no_ci:=(LY_no_ci+c(LY_no_ci[2:length(LY_no_ci)],NA))/2]
        m[Age==100,LY_plus_no_ci:=LY_no_ci]
        m[,TotalPLY_no_ci:=rev(cumsum(rev(LY_no_ci))),by=list(Iter, Model,Gender,Year)]
        m[,LE_no_ci:=TotalPLY_no_ci/LY_no_ci]

        # No disability

        m[,LY_plus_no_disab:=(LY_no_disab+c(LY_no_disab[2:length(LY_no_disab)],NA))/2]
        m[Age==100,LY_plus_no_disab:=LY_no_disab]
        m[,TotalPLY_no_disab:=rev(cumsum(rev(LY_no_disab))),by=list(Iter, Model,Gender,Year)]
        m[,LE_no_disab:=TotalPLY_no_disab/LY_no_disab]



        m[,LY_plus_healthy:=(LY_healthy+c(LY_healthy[2:length(LY_healthy)],NA))/2]
        m[Age==100,LY_plus_healthy:=LY_healthy]

        m[,TotalPLY_healthy:=rev(cumsum(rev(LY_healthy))),by=list(Iter, Model,Gender,Year)]
        m[,LE_healthy:=TotalPLY_healthy/LY_healthy]


        #LE with disability
        m[,LY_plus_with_disab:=(LY_with_disab+c(LY_with_disab[2:length(LY_with_disab)],NA))/2]
        m[Age==100,LY_plus_with_disab:=LY_with_disab]

        m[,TotalPLY_with_disab:=rev(cumsum(rev(LY_with_disab))),by=list(Iter, Model,Gender,Year)]
        m[,LE_with_disab:=TotalPLY_with_disab/LY_with_disab]
      }
      
      if (method==2)
      {
        #该步骤是为了计算死亡概率
        m[,LY:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10] #population
        m[,TO9:=MOV1_9+MOV2_9+MOV3_9+MOV4_9+MOV5_9+MOV6_9+MOV7_9+MOV10_9] #non-CVD deaths
        m[,TO8:=MOV1_8+MOV2_8+MOV3_8+MOV4_8+MOV5_8+MOV6_8+MOV7_8+MOV10_8] #CVD deaths
        m[,mx:=(TO8+TO9)/LY]      #central death rate
        m[,qx:=mx/(1+(0.5*mx))]   #conditional probability of death
        
        m0 <- m[mx<=0|is.na(mx),] # Yuyang added? Why? 出现死亡率小于0的情况?
        m <- m[mx>0,] # Yuyang added? Why? 出现死亡率小于0的情况?
        #每一个年龄都有死亡概率，而且根据不同的时间
        m[, lx:=c.lx(qx), by=list(Iter, Model, Gender, Year)] #从m0 <- m[mx<=0|is.na(mx),]移动到此处,根据一定分组进行单独计算
        m[, Lx:=c.Lx(lx,mx), by=list(Iter, Model, Gender, Year)] 
        m[,Tx:=rev(cumsum(rev(Lx))),by=list(Iter,Model,Gender,Year)]
        m[,LE:=Tx/Lx]
        
        #Disability-free life expectancy
        
        m[,pix:=(s.5+s.6+s.7+s.10)/LY] #corrected...
        m[,PYlwd:=Lx*(1-pix)]
        m[,TYlwd:=rev(cumsum(rev(PYlwd))),by=list(Iter,Model,Gender,Year)]
        m[,LE_no_disab:=TYlwd/Lx]
        
        #Dementia-free life expectancy
        m[,pixdem:=(s.6+s.7)/LY]
        m[,PYlwdem:=Lx*(1-pixdem)]
        m[,TYlwdem:=rev(cumsum(rev(PYlwdem))),by=list(Iter,Model,Gender,Year)]
        m[,LE_no_dementia:=TYlwdem/Lx]
        
        #Life expectancy of healthy population
        m[,pix_anydis:=(s.2+s.3+s.4+s.5+s.6+s.7+s.10)/LY]
        m[,PYlwd_anydis:=Lx*(1-pix_anydis)]
        m[,TYlwd_anydis:=rev(cumsum(rev(PYlwd_anydis))),by=list(Iter,Model,Gender,Year)]
        m[,LE_healthy:=TYlwd_anydis/Lx]
        
        #Life expectancy with disability
        m[,pix_disab:=(s.1+s.2+s.3+s.4)/LY]
        m[,PYlwd_disab:=Lx*(1-pix_disab)]
        m[,TYlwd_disab:=rev(cumsum(rev(PYlwd_disab))),by=list(Iter,Model,Gender,Year)]
        m[,LE_with_disab:=TYlwd_disab/Lx]
        
        #Life expectancy with dementia
        m[,pix_dementia:=(s.1+s.2+s.3+s.4+s.5+s.10)/LY]
        m[,PYlwd_dementia:=Lx*(1-pix_dementia)]
        m[,TYlwd_dementia:=rev(cumsum(rev(PYlwd_dementia))),by=list(Iter,Model,Gender,Year)]
        m[,LE_with_dementia:=TYlwd_dementia/Lx]
        
        #Life expectancy free of CVD
        m[,pix_cvd:=(s.2+s.3+s.5+s.6)/LY]
        m[,PYlwd_cvd:=Lx*(1-pix_cvd)]
        m[,TYlwd_cvd:=rev(cumsum(rev(PYlwd_cvd))),by=list(Iter,Model,Gender,Year)]
        m[,LE_no_cvd:=TYlwd_cvd/Lx]
        
        #Life expectancy free of CI (cognitive impairment)
        m[,pix_ci:=(s.3+s.4+s.6+s.7)/LY]
        m[,PYlwd_ci:=Lx*(1-pix_ci)]
        m[,TYlwd_ci:=rev(cumsum(rev(PYlwd_ci))),by=list(Iter,Model,Gender,Year)]
        m[,LE_no_ci:=TYlwd_ci/Lx]
        
        
        m[,`:=`(TO9=NULL,TO8=NULL,mx=NULL,qx=NULL,lx=NULL,Lx=NULL,Tx=NULL,pix=NULL,PYlwd=NULL,
                TYlwd=NULL,pixdem=NULL,PYlwdem=NULL,TYlwdem=NULL,pix_anydis=NULL,PYlwd_anydis=NULL,
                TYlwd_anydis=NULL,pix_disab=NULL,PYlwd_disab=NULL,TYlwd_disab=NULL,
                pix_cvd=NULL,PYlwd_cvd=NULL,TYlwd_cvd=NULL,pix_ci=NULL,PYlwd_ci=NULL,
                TYlwd_ci=NULL, pix_dementia=NULL, PYlwd_dementia=NULL, TYlwd_dementia=NULL)]
        m <- rbind(m0,m,fill = TRUE) # Yuyang added? Why fill?
        
        #names(m)
      }
      return(m)
    }
    
    ### Running model 
    # Log("(%d-%d): Running model...",min(iters),max(iters), settings=settings)
    m <- RunModel(dat = d)
    # Log("(%d-%d): Running model finished.",min(iters),max(iters), settings=settings)
    
    # Log("(%d-%d): Summarizing gender...",min(iters),max(iters), settings=settings)
    m[,Model:=model]
    
    m<-SummarizeGender(m)
    # Log("(%d-%d): Summarizing gender finished.",min(iters),max(iters), settings=settings)
    
    # Log("(%d-%d): Calculating life expectancies...",min(iters),max(iters), settings=settings)
    
    m <- calc2.LE(m, method=2)
    # Log("(%d-%d):Calculating life expectancies finished.",min(iters),max(iters), settings=settings)
    
    if (remove.tps) {
      # Log("(%d-%d): Removing tp's from final data set...",min(iters),max(iters), settings=settings)
      rem_p<-function(m)
      {
        m[,`:=`(P1_1=NULL,P1_10=NULL,P1_2=NULL,P1_3=NULL,P1_4=NULL,
                P1_8=NULL,P1_9=NULL,P10_1=NULL,
                P10_10=NULL,P10_5=NULL,P10_6=NULL,
                P10_7=NULL,P10_8=NULL,P10_9=NULL,
                P2_2=NULL,P2_3=NULL,P2_5=NULL,P2_8=NULL,
                P2_9=NULL,P3_3=NULL,P3_6=NULL,P3_8=NULL,
                P3_9=NULL,P4_3=NULL,P4_4=NULL,P4_7=NULL,
                P4_8=NULL,P4_9=NULL,P5_2=NULL,P5_5=NULL,
                P5_8=NULL,P5_9=NULL,P6_3=NULL,P6_6=NULL,
                P6_8=NULL,P6_9=NULL,P7_4=NULL,P7_7=NULL,
                P7_8=NULL,P7_9=NULL,P8_8=NULL,P9_9=NULL)]
      }
      rem_p(m)
      # Log("(%d-%d): Removing tp's done.",min(iters),max(iters), settings=settings)
      
    }
    
    # setting attributes to model dataset
    
    attributes(m)$scenario.prevalences<-"NULL"
    if (is.null(prev)==F) attributes(m)$scenario.prevalences<-prev
    # Log(paste0("(%d-%d):",length(prev)) ,min(iters),max(iters), settings=settings)
    
    return(m)
  }
  
  if (settings$PARALLEL.PACKS>1)
  {
    m<-foreach(i=is) %dopar%
      {
        print(paste0(Sys.time(),"Foreach loop started"))
        print(paste0("Code version: ",settings$CODE.VER))
        print(paste0("Inputs version: ",settings$INPUT.VER))

        RunProcess(iters=i, 
                   scenario=scenario, 
                   model=model, 
                   settings=settings, 
                   scenario.rf=scenario.rf, 
                   inputs=inputs,
                   scenario.cont.RR=scenario.cont.RR, scenario.cont.rf.baseline=scenario.cont.rf.baseline, 
                   scenario.cont.func=scenario.cont.func,save.pif.table=save.pif.table,
                   pif.scenario.file=pif.scenario.file, 
                   pif.output.file=paste0("pif_intermediate_output_",model,"_",min(i),"-",max(i),".csv"))
      }
  } else
  {
    m<-foreach(i=is) %do%
      {
        Log("Foreach loop started", settings=settings)
        Log(paste0("Code version: ",settings$CODE.VER), settings=settings)
        Log(paste0("Inputs version: ",settings$INPUT.VER), settings=settings)
        
        RunProcess(iters=i, 
                   scenario=scenario,
                   model=model, 
                   settings=settings, 
                   scenario.rf=scenario.rf, 
                   inputs=inputs,
                   scenario.cont.RR=scenario.cont.RR, scenario.cont.rf.baseline=scenario.cont.rf.baseline, 
                   scenario.cont.func=scenario.cont.func,save.pif.table=save.pif.table,
                   pif.scenario.file=pif.scenario.file, 
                   pif.output.file = paste0("pif_intermediate_output_",model,"_",min(i),"-",max(i),".csv"))
      }
  }
  # stopCluster(cl)

  prev<-attributes(m[[1]])$scenario.prevalences
  
  Log("Binding tasks...", settings = settings)
  m<-rbindlist(m)
  setcolorder(m,c("Model", names(m)[names(m) != "Model"]))
  setkey(m,Model, Iter, Gender, Year, Age)
  attributes(m)$scenario.prevalences<-prev

  #info.Code<-as.data.table(file.info(dir(paste0(getwd(),"/Code/",code.ver,"/R"), full.names = T )))
  # info.Inputs<-as.data.table(file.info(dir(paste0(getwd(),"/Inputs/",settings$INPUT.VER), full.names = T )))
  # fInputs<-system.file("extdata/",settings$INPUTS_VER,"/version.info.txt",package="bam")
  # attributes(m)$inputs.files<-info.Inputs
  # attributes(m)$code.version.description<-packageDescription("bam")
  # attributes(m)$inputs.version.description<-readChar(fInputs, file.info(fInputs)$size)
  attributes(m)$created<-date()
  # attributes(m)$code.version<-packageVersion("bam")
  attributes(m)$inputs.version<-settings$INPUTS_VER
  attributes(m)$scenario.name<-scenario
  class(m)<-append("CAMModel",class(m))
  
  # Log("Model calculation finished,\n", settings = settings)
  
  return(m)
}


# Output function ---------------------------------------------------------
## Prevalence --------------

Prevalences_china <- function(
m,
models = "",
gender = c("All", "Men", "Women"),
ages = list(c(65:100),c(60:100),c(50:100)),
years = c(2015:2050),
states = c(
  "Disease free",
  "Any disease",
  "CVD",
  "CD",
  "Dementia",
  "Any disability",
  "Total population",
  "Disability no dementia",
  "CVD disability",
  "s.1",
  "s.2",
  "s.3",
  "s.4",
  "s.5",
  "s.6",
  "s.7",
  "s.10"),
type = c('cases',"prevalence"),
ref.scenario = "No reference",
ref.year = 0,
ver = "absolute",
standard.pop = 'R data/2010_urban_rural.RDS',
standardized = list(0,1),
output = c("Median", "LCL", "UCL", "Mean", "Deterministic"),
settings=settings)
{
  #assuming iter==0 is deterministic, all other are stochastic
  
  # standard population for providing standardized prevalences
  # use model 2015 population data as standard by default
  # m <- copy(final_result)
  if (length(models) == 1 & sum(models == "") == 0 ){
    models = unique(m$Model)
  }
  
  if (is.na(standard.pop))
  {
    inipop_0<-data.table(readRDS('D:\\Obj 3_scenario analysis\\Pop structure (China UN 2022 ref).RDS'))
    inipop_std <- inipop_0[inipop_0$Year == 2015,]
    inipop_stdm <- melt(inipop_std,id = c('Year','Age'),
                        variable.name = 'Gender',
                        value.name = 'std.pop')
    inipop_stdm$Gender  <- ifelse(inipop_stdm$Gender == 'Male','Men','Women')
    inipop_stdmd <- dcast(inipop_stdm,Age~Gender,value.var = 'std.pop')
    inipop_stdmd$std.pop <- inipop_stdmd$Women + inipop_stdmd$Men
    inipop_stdmd$Gender <- 'All'
    inipop_stdm[,Year := NULL]
    
    inipop_stdall <- rbind(inipop_stdm[,c('Age','Gender','std.pop')],
                           inipop_stdmd[,c('Age','Gender','std.pop')])
    std <- copy(data.table(inipop_stdall))
  }else {
    std <- data.table(readRDS(standard.pop))
  }
  setkey(m, Gender, Age)
  setkey(std, Gender, Age)
  m <- m[J(std)]
  rm(std)
  
  x <- m[Gender %in% gender & Model %in% models & Year %in% years,
         list(Iter,
              Model,
              Year,
              Age,
              Gender,
              s.1,
              s.2,
              s.3,
              s.4,
              s.5,
              s.6,
              s.7,
              s.8,
              s.9,
              s.10,
              std.pop)]
  rm(m)
  gc()
  
  #x[,states:=make.names(states)]
  states <- make.names(states)
  
  if (class(ages) == "integer") {
    x <-
      x[Age >= min(ages) &
          Age <= max(ages), AgeRange := paste(min(ages), max(ages), sep = "-")]
  }
  if (class(ages) == "list")
  {
    xx <- list()
    i <- 1
    for (ar in ages)
    {
      i <- i + 1
      xx[[paste(i)]] <- copy(x[Age >= min(ar) & Age <= max(ar)])
      xx[[paste(i)]][, AgeRange := paste(min(ar), max(ar), sep = "-")]
    }
    x <- rbindlist(xx)
    rm(xx)
  }
  
  gc()
  
  
  x[, TotalPopulation := s.1 + s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
  xx <- list()
  for (state in states)
  {
    xc <- copy(x)
    xc[, State := state]
    
    if ("CVD.disab" == state) {
      xc[, Value := s.5 + s.6]
    }
    if ("CVD.and.dementia.disab" == state) {
      xc[, Value := s.5 + s.6 + s.7]
    }
    if ("Dementia.disab" == state) {
      xc[, Value := s.7 + s.6]
    }
    if ("Disease.free" == state) {
      xc[, Value := s.1]
    }
    if ("Any.disease" == state) {
      xc[, Value := s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
    }
    if ("CVD" == state) {
      xc[, Value := s.2 + s.3 + s.5 + s.6]
    }
    if ("CD" == state) {
      xc[, Value := s.3 + s.4 + s.6 + s.7]
    }
    if ("CI" == state) {
      xc[, Value := s.3 + s.4]
    }
    if ("Dementia" == state) {
      xc[, Value := s.6 + s.7]
    }
    if ("Any.disability" == state) {
      xc[, Value := s.5 + s.6 + s.7 + s.10]
    }
    if ("Disability.no.dementia" == state) {
      xc[, Value := s.5 + s.10]
    }
    if ("CVD.disability" == state) {
      xc[, Value := s.6 + s.5]
    }
    if ("Total.population" == state) {
      xc[, Value := s.1 + s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
    }
    if ("s.1" == state) {
      xc[, Value := s.1]
    }
    if ("s.2" == state) {
      xc[, Value := s.2]
    }
    if ("s.3" == state) {
      xc[, Value := s.3]
    }
    if ("s.4" == state) {
      xc[, Value := s.4]
    }
    if ("s.5" == state) {
      xc[, Value := s.5]
    }
    if ("s.6" == state) {
      xc[, Value := s.6]
    }
    if ("s.7" == state) {
      xc[, Value := s.7]
    }
    if ("s.8" == state) {
      xc[, Value := s.8]
    }
    if ("s.9" == state) {
      xc[, Value := s.9]
    }
    if ("s.10" == state) {
      xc[, Value := s.10]
    }
    
    xx[[state]] <- xc
  }
  
  x <- rbindlist(xx)
  rm(xx)
  gc()
  
  
  x[, `:=`(
    s.1 = NULL,
    s.2 = NULL,
    s.3 = NULL,
    s.4 = NULL,
    s.5 = NULL,
    s.6 = NULL,
    s.7 = NULL,
    s.8 = NULL,
    s.9 = NULL,
    s.10 = NULL
  )]
  
  
  x[, prevalence := 100 * Value / TotalPopulation]
  
  x1 <- copy(x)
  
  x <- x[is.na(AgeRange) == F]
  x <-
    x[, list(prevalence = weighted.mean(prevalence, w = std.pop),
             pop = sum(TotalPopulation)),
      by = list(Iter, Model, State, Year, Gender, AgeRange)]
  x[, Stand := "standardized"]
  
  x1 <- x1[is.na(AgeRange) == F]
  x1 <-
    x1[, list(
      prevalence = weighted.mean(prevalence, w = TotalPopulation),
      pop = sum(TotalPopulation)
    ),
    by = list(Iter, Model, State, Year, Gender, AgeRange)]
  x1[, Stand := "non-standardized"]
  
  x <- rbind(x, x1)
  
  rm(x1)
  gc()
  
  x[, cases := (prevalence / 100) * pop]

  
  if (length(type) == 1 & type[1] == "cases")
  {
    x[, Value := cases]
    x[, Type := "cases"]
  }
  if (length(type) == 1 & type[1] == "prevalence")
  {
    x[, Value := prevalence]
    x[, Type := "prevalence"]
  }
  if ("cases" %in% type & "prevalence" %in% type)
  {
    x1 <- copy(x)
    
    x[, Value := cases]
    x[, Type := "cases"]
    x1[, Value := prevalence]
    x1[, Type := "prevalence"]
    
    x <- rbind(x, x1)
    rm(x1)
    gc()
  }
  
  x[, `:=`(prevalence = NULL,
           pop = NULL,
           cases = NULL)]
  
  
  if (ref.year > 0)
  {
    x[, Model := make.names(Model)]
    x[, Year := paste0("y", Year)]
    ref.year <- paste0("y", ref.year)
    Years = unique(x$Year)
    xt <-
      as.data.table(
        dcast(
          x,
          formula = Iter + Gender + Model + AgeRange + State + Type + Stand ~ Year,
          value.var = "Value"
        )
      )
    OtherYears <- Years[Years != ref.year]
    
    tt = 10 # TODO
    for (yrs in OtherYears)
    {
      if (ver == "absolute")
        xt[, eval(paste0(yrs)) := eval(parse(text = paste0(yrs, "-", ref.year)))]
      if (ver == "relative")
        xt[, eval(paste0(yrs)) := eval(parse(text = paste0(
          "100*(", yrs, "-", ref.year, ")/", ref.year
        )))]
      if (ver == "annrel")
        xt[, eval(paste0(yrs)) := eval(parse(text = paste0(
          "((", yrs, "/", ref.year, ")**(1/", tt, "))-1"
        )))]
    }
    xt <-
      melt(xt,
           id.vars = c(
             "Iter",
             "Gender",
             "Model",
             "AgeRange",
             "State",
             "Type",
             "Stand"
           ))
    setnames(xt, c("variable", "value"), c("Year", "Value"))
    xt[Year == ref.year, Value := 0]
    xt[, Year := as.numeric(substr(Year, 2, 5))]
    
    
    x <- xt
    
  }
  
  if (ref.scenario != "No reference")
  {
    #x[, Model := make.names(Model)]
    #ref.scenario <- make.names(ref.scenario)
    Scenarios <- unique(x$Model)
    
    xt <-
      as.data.table(
        dcast(
          x,
          formula = Iter + Gender + Year + AgeRange + State + Type+Stand ~ Model,
          value.var = "Value"
        )
      )
    
    
    OtherScen <- Scenarios[Scenarios != ref.scenario]
    
    for (sc in OtherScen)
    {
      if (ver == "absolute")
        xt[, eval(paste0("`",sc,"`")) := eval(parse(text = paste0("`",sc, "`-", ref.scenario)))]
      if (ver == "relative")
        xt[, eval(paste0(sc)) := eval(parse(text = paste0(
          "100*(`", sc, "`-", ref.scenario, ")/", ref.scenario
        )))]
    }
    xt[, eval(paste0(ref.scenario)) := 0]
    
    xt <-
      melt(xt,
           id.vars = c(
             "Iter",
             "Gender",
             "Year",
             "AgeRange",
             "State",
             "Type",
             "Stand"
           ))
    setnames(xt, c("variable", "value"), c("Model", "Value"))
    
    x <- xt
  }
  
  setcolorder(
    x,
    c(
      "Iter",
      "Model",
      "State",
      "Year",
      "Gender",
      "AgeRange",
      "Stand",
      "Type",
      "Value"
    )
  )
  
  x.det <- x[Iter == 0]
  x.psa <- x[Iter > 0]
  
  x.psa <-
    x.psa[, list(
      mean.Value = mean(Value),
      median.Value = median(Value),
      lcl.Value = quantile(Value, 0.025),
      ucl.Value = quantile(Value, 0.975),
      max.Value = max(Value),
      min.Value = min(Value)
    ),
    by = list(Model, Year, Gender, AgeRange, State, Type, Stand)]
  
  setkeyv(x.psa,
          c(
            "Model",
            "Year",
            "Gender",
            "AgeRange",
            "State",
            "Type",
            "Stand"
          ))
  setkeyv(x.det,
          c(
            "Model",
            "Year",
            "Gender",
            "AgeRange",
            "State",
            "Type",
            "Stand"
          ))
  
  x.det[, Iter := NULL]
  
  if (length(x.psa$Model) > 0 &
      length(x.det$Model) > 0)
    xf <- x.det[J(x.psa)]
  if (length(x.psa$Model) == 0 & length(x.det$Model) > 0)
    xf <- x.det
  if (length(x.psa$Model) > 0 & length(x.det$Model) == 0)
    xf <- x.psa
  if (length(x.psa$Model) == 0 &
      length(x.det$iter) == 0)
    stop("zero rows in input data")
  
  setnames(xf, "Value", "deterministic.Value")
  
  # standor not
  if (class(standardized) == 'list'){
      xf <- xf
  }else{
    if (standardized == 0)
      xf <- xf[Stand == "non-standardized"]
    if (standardized == 1)
      xf <- xf[Stand == "standardized"]
  }

  
  if (("Median" %in% output) == F)
    xf[, median.Value := NULL]
  if (("Mean" %in% output) == F)
    xf[, mean.Value := NULL]
  if (("LCL" %in% output) == F)
    xf[, lcl.Value := NULL]
  if (("UCL" %in% output) == F)
    xf[, ucl.Value := NULL]
  if (("Deterministic" %in% output) == F)
    xf[, deterministic.Value := NULL]
  
  
  # mostattributes(xf)<-attributes(m)
  
  attributes(xf)$output.created<-date()
  attributes(xf)$ref.scenario <- ref.scenario
  attributes(xf)$ver <- ver
  
  class(xf) <-c("Prevalences","data.table","data.frame")
  
  
  return(xf)
}

Prevalences <-
  function(m ,
           models = "",
           gender = c("All", "Men", "Women"),
           ages = c(65:100),
           years = c(2006:2060),
           states = c(
             "Disease free",
             "Any disease",
             "CVD",
             "CD",
             "Dementia",
             "Any disability",
             "Total population",
             "Disability no dementia",
             "CVD disability",
             "s.1",
             "s.2",
             "s.3",
             "s.4",
             "s.5",
             "s.6",
             "s.7",
             "s.10"
           ),
           type = "cases",
           ref.scenario = "No reference",
           ref.year = 0,
           ver = "absolute",
           standard.pop = NA,
           standardized = 0,
           output = c("Median", "LCL", "UCL", "Mean", "Deterministic"),
           settings=default.settings())
  {
    #assuming iter==0 is deterministic, all other are stochastic
    
    # standard population for providing standardized prevalences
    # use model 2015 population data as standard by default
    
    if (models == "")
      models = unique(m$Model)
    
    if (is.na(standard.pop))
    {
      std <-
        m[Year == 2015 &
            Iter == 0 &
            Model == unique(Model)[1], list (Gender, Age, s.1, s.2, s.3, s.4, s.5, s.6, s.7, s.10)]
      std[, std.pop := s.1 + s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
      std[, `:=`(
        s.1 = NULL,
        s.2 = NULL,
        s.3 = NULL,
        s.4 = NULL,
        s.5 = NULL,
        s.6 = NULL,
        s.7 = NULL,
        s.10 = NULL
      )]
    }
    setkey(m, Gender, Age)
    setkey(std, Gender, Age)
    m <- m[J(std)]
    rm(std)
    
    
    
    x <- m[Gender %in% gender & Model %in% models & Year %in% years,
           list(Iter,
                Model,
                Year,
                Age,
                Gender,
                s.1,
                s.2,
                s.3,
                s.4,
                s.5,
                s.6,
                s.7,
                s.8,
                s.9,
                s.10,
                std.pop)]
    rm(m)
    gc()
    
    #x[,states:=make.names(states)]
    states <- make.names(states)
    
    if (class(ages) == "integer") {
      x <-
        x[Age >= min(ages) &
            Age <= max(ages), AgeRange := paste(min(ages), max(ages), sep = "-")]
    }
    if (class(ages) == "list")
    {
      xx <- list()
      i <- 1
      for (ar in ages)
      {
        i <- i + 1
        xx[[paste(i)]] <- copy(x[Age >= min(ar) & Age <= max(ar)])
        xx[[paste(i)]][, AgeRange := paste(min(ar), max(ar), sep = "-")]
      }
      x <- rbindlist(xx)
      rm(xx)
    }
    
    gc()
    
    
    x[, TotalPopulation := s.1 + s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
    xx <- list()
    for (state in states)
    {
      xc <- copy(x)
      xc[, State := state]
      
      if ("CVD.disab" == state) {
        xc[, Value := s.5 + s.6]
      }
      if ("CVD.and.dementia.disab" == state) {
        xc[, Value := s.5 + s.6 + s.7]
      }
      if ("Dementia.disab" == state) {
        xc[, Value := s.7 + s.6]
      }
      if ("Disease.free" == state) {
        xc[, Value := s.1]
      }
      if ("Any.disease" == state) {
        xc[, Value := s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
      }
      if ("CVD" == state) {
        xc[, Value := s.2 + s.3 + s.5 + s.6]
      }
      if ("CD" == state) {
        xc[, Value := s.3 + s.4 + s.6 + s.7]
      }
      if ("Dementia" == state) {
        xc[, Value := s.6 + s.7]
      }
      if ("Any.disability" == state) {
        xc[, Value := s.5 + s.6 + s.7 + s.10]
      }
      if ("Disability.no.dementia" == state) {
        xc[, Value := s.5 + s.10]
      }
      if ("CVD.disability" == state) {
        xc[, Value := s.6 + s.5]
      }
      if ("Total.population" == state) {
        xc[, Value := s.1 + s.2 + s.3 + s.4 + s.5 + s.6 + s.7 + s.10]
      }
      if ("s.1" == state) {
        xc[, Value := s.1]
      }
      if ("s.2" == state) {
        xc[, Value := s.2]
      }
      if ("s.3" == state) {
        xc[, Value := s.3]
      }
      if ("s.4" == state) {
        xc[, Value := s.4]
      }
      if ("s.5" == state) {
        xc[, Value := s.5]
      }
      if ("s.6" == state) {
        xc[, Value := s.6]
      }
      if ("s.7" == state) {
        xc[, Value := s.7]
      }
      if ("s.8" == state) {
        xc[, Value := s.8]
      }
      if ("s.9" == state) {
        xc[, Value := s.9]
      }
      if ("s.10" == state) {
        xc[, Value := s.10]
      }
      
      xx[[state]] <- xc
    }
    
    x <- rbindlist(xx)
    rm(xx)
    gc()
    
    
    x[, `:=`(
      s.1 = NULL,
      s.2 = NULL,
      s.3 = NULL,
      s.4 = NULL,
      s.5 = NULL,
      s.6 = NULL,
      s.7 = NULL,
      s.8 = NULL,
      s.9 = NULL,
      s.10 = NULL
    )]
    
    
    x[, prevalence := 100 * Value / TotalPopulation]
    
    x1 <- copy(x)
    
    x <- x[is.na(AgeRange) == F]
    x <-
      x[, list(prevalence = weighted.mean(prevalence, w = std.pop),
               pop = sum(TotalPopulation)),
        by = list(Iter, Model, State, Year, Gender, AgeRange)]
    x[, Stand := "standardized"]
    
    x1 <- x1[is.na(AgeRange) == F]
    x1 <-
      x1[, list(
        prevalence = weighted.mean(prevalence, w = TotalPopulation),
        pop = sum(TotalPopulation)
      ),
      by = list(Iter, Model, State, Year, Gender, AgeRange)]
    x1[, Stand := "non-standardized"]
    
    x <- rbind(x, x1)
    
    rm(x1)
    gc()
    
    x[, cases := (prevalence / 100) * pop]
    
    
    if (length(type) == 1 & type[1] == "cases")
    {
      x[, Value := cases]
      x[, Type := "cases"]
    }
    if (length(type) == 1 & type[1] == "prevalence")
    {
      x[, Value := prevalence]
      x[, Type := "prevalence"]
    }
    if ("cases" %in% type & "prevalence" %in% type)
    {
      x1 <- copy(x)
      
      x[, Value := cases]
      x[, Type := "cases"]
      x1[, Value := prevalence]
      x1[, Type := "prevalence"]
      
      x <- rbind(x, x1)
      rm(x1)
      gc()
    }
    
    x[, `:=`(prevalence = NULL,
             pop = NULL,
             cases = NULL)]
    
    
    if (ref.year > 0)
    {
      x[, Model := make.names(Model)]
      x[, Year := paste0("y", Year)]
      ref.year <- paste0("y", ref.year)
      Years = unique(x$Year)
      xt <-
        as.data.table(
          dcast(
            x,
            formula = Iter + Gender + Model + AgeRange + State + Type + Stand ~ Year,
            value.var = "Value"
          )
        )
      OtherYears <- Years[Years != ref.year]
      
      tt = 10 # TODO
      for (yrs in OtherYears)
      {
        if (ver == "absolute")
          xt[, eval(paste0(yrs)) := eval(parse(text = paste0(yrs, "-", ref.year)))]
        if (ver == "relative")
          xt[, eval(paste0(yrs)) := eval(parse(text = paste0(
            "100*(", yrs, "-", ref.year, ")/", ref.year
          )))]
        if (ver == "annrel")
          xt[, eval(paste0(yrs)) := eval(parse(text = paste0(
            "((", yrs, "/", ref.year, ")**(1/", tt, "))-1"
          )))]
      }
      xt <-
        melt(xt,
             id.vars = c(
               "Iter",
               "Gender",
               "Model",
               "AgeRange",
               "State",
               "Type",
               "Stand"
             ))
      setnames(xt, c("variable", "value"), c("Year", "Value"))
      xt[Year == ref.year, Value := 0]
      xt[, Year := as.numeric(substr(Year, 2, 5))]
      
      
      x <- xt
      
    }
    
    
    
    
    if (ref.scenario != "No reference")
    {
      #x[, Model := make.names(Model)]
      #ref.scenario <- make.names(ref.scenario)
      Scenarios <- unique(x$Model)
      
      xt <-
        as.data.table(
          dcast(
            x,
            formula = Iter + Gender + Year + AgeRange + State + Type+Stand ~ Model,
            value.var = "Value"
          )
        )
      
      
      OtherScen <- Scenarios[Scenarios != ref.scenario]
      
      for (sc in OtherScen)
      {
        if (ver == "absolute")
          xt[, eval(paste0("`",sc,"`")) := eval(parse(text = paste0("`",sc, "`-", ref.scenario)))]
        if (ver == "relative")
          xt[, eval(paste0(sc)) := eval(parse(text = paste0(
            "100*(`", sc, "`-", ref.scenario, ")/", ref.scenario
          )))]
      }
      xt[, eval(paste0(ref.scenario)) := 0]
      
      xt <-
        melt(xt,
             id.vars = c(
               "Iter",
               "Gender",
               "Year",
               "AgeRange",
               "State",
               "Type",
               "Stand"
             ))
      setnames(xt, c("variable", "value"), c("Model", "Value"))
      
      x <- xt
    }
    
    setcolorder(
      x,
      c(
        "Iter",
        "Model",
        "State",
        "Year",
        "Gender",
        "AgeRange",
        "Stand",
        "Type",
        "Value"
      )
    )
    
    x.det <- x[Iter == 0]
    x.psa <- x[Iter > 0]
    
    x.psa <-
      x.psa[, list(
        mean.Value = mean(Value),
        median.Value = median(Value),
        lcl.Value = quantile(Value, 0.025),
        ucl.Value = quantile(Value, 0.975)
      ),
      by = list(Model, Year, Gender, AgeRange, State, Type, Stand)]
    
    setkeyv(x.psa,
            c(
              "Model",
              "Year",
              "Gender",
              "AgeRange",
              "State",
              "Type",
              "Stand"
            ))
    setkeyv(x.det,
            c(
              "Model",
              "Year",
              "Gender",
              "AgeRange",
              "State",
              "Type",
              "Stand"
            ))
    
    x.det[, Iter := NULL]
    
    if (length(x.psa$Model) > 0 &
        length(x.det$Model) > 0)
      xf <- x.det[J(x.psa)]
    if (length(x.psa$Model) == 0 & length(x.det$Model) > 0)
      xf <- x.det
    if (length(x.psa$Model) > 0 & length(x.det$Model) == 0)
      xf <- x.psa
    if (length(x.psa$Model) == 0 &
        length(x.det$iter) == 0)
      stop("zero rows in input data")
    
    setnames(xf, "Value", "deterministic.Value")
    
    if (standardized == 0)
      xf <- xf[Stand == "non-standardized"]
    if (standardized == 1)
      xf <- xf[Stand == "standardized"]
    
    if (("Median" %in% output) == F)
      xf[, median.Value := NULL]
    if (("Mean" %in% output) == F)
      xf[, mean.Value := NULL]
    if (("LCL" %in% output) == F)
      xf[, lcl.Value := NULL]
    if (("UCL" %in% output) == F)
      xf[, ucl.Value := NULL]
    if (("Deterministic" %in% output) == F)
      xf[, deterministic.Value := NULL]
    
    
    # mostattributes(xf)<-attributes(m)
    
    attributes(xf)$output.created<-date()
    attributes(xf)$ref.scenario <- ref.scenario
    attributes(xf)$ver <- ver
    
    class(xf) <-c("Prevalences","data.table","data.frame")
    
    
    return(xf)
  }


# Mortality ---------------------------------------------------------------

Mortality_China<-function(m, 
                          gender=c('All',"Men","Women"), 
                          ages=list(c(35:100),c(65:100),c(50:100)), 
                          standard.pop = '',
                          disease=c("CVD",'nCVD','Total',
                                    'CVD(s.1)','nCVD(s.1)','Total(s.1)',
                                    'CVD(s.2)','nCVD(s.2)','Total(s.2)',
                                    'CVD(s.3)','nCVD(s.3)','Total(s.3)',
                                    'CVD(s.4)','nCVD(s.4)','Total(s.4)',
                                    'CVD(s.5)','nCVD(s.5)','Total(s.5)',
                                    'CVD(s.6)','nCVD(s.6)','Total(s.6)',
                                    'CVD(s.7)','nCVD(s.7)','Total(s.7)',
                                    'CVD(s.10)','nCVD(s.10)','Total(s.10)',
                                    # Diseases
                                    'CVD(CVDall)','nCVD(CVDall)','Total(CVDall)',
                                    'CVD(Dementiaall)','nCVD(Dementiaall)','Total(Dementiaall)',
                                    'CVD(Disabilityall)','nCVD(Disabilityall)','Total(Disabilityall)'),
                          type=c("deaths","mortality"),
                          ref.scenario="No reference", 
                          ver="absolute")
{
  require(data.table)
  
  if (is.na(standard.pop))
  {#该步骤是设置初始的人群，将数据转变成基线（时间自己设定）男性，女性，全部的不同年龄点总人数
    inipop_0<-data.table(readRDS('D:\\Obj 3_scenario analysis\\Pop structure (China UN 2022 ref).RDS'))
    inipop_std <- inipop_0[inipop_0$Year == 2015,]
    inipop_stdm <- melt(inipop_std,id = c('Year','Age'),
                        variable.name = 'Gender',
                        value.name = 'std.pop')
    inipop_stdm$Gender  <- ifelse(inipop_stdm$Gender == 'Male','Men','Women')
    inipop_stdmd <- dcast(inipop_stdm,Age~Gender,value.var = 'std.pop')
    inipop_stdmd$std.pop <- inipop_stdmd$Women + inipop_stdmd$Men
    inipop_stdmd$Gender <- 'All'
    inipop_stdm[,Year := NULL]
    
    inipop_stdall <- rbind(inipop_stdm[,c('Age','Gender','std.pop')],
                           inipop_stdmd[,c('Age','Gender','std.pop')])
    std <- copy(data.table(inipop_stdall))
  }else {
    std <- data.table(readRDS(standard.pop))
  }
  
  grps<-list()
  for (a in ages)
  {
    agegrp<-paste0(min(a),"-",max(a))
    subm<-(m[Age %in% a & Gender %in% gender,])
    # if (disease=="CVD")  subm[,Value:=MOV1_8+MOV2_8+MOV3_8+MOV4_8+MOV5_8+MOV6_8+MOV7_8+MOV10_8]
    # if (disease=="nCVD")  subm[,Value:=MOV1_9+MOV2_9+MOV3_9+MOV4_9+MOV5_9+MOV6_9+MOV7_9+MOV10_9]
    # if (disease=="Total") subm[,Value:=MOV1_8+MOV2_8+MOV3_8+MOV4_8+MOV5_8+MOV6_8+MOV7_8+MOV10_8+
    #                              MOV1_9+MOV2_9+MOV3_9+MOV4_9+MOV5_9+MOV6_9+MOV7_9+MOV10_9]
    xx_mort <- list()
    for (state in disease)
    {
      xc <- copy(subm)
      xc[, State := state]
      
      if (state=="CVD") {
        xc[,Value:=MOV1_8+MOV2_8+MOV3_8+MOV4_8+MOV5_8+MOV6_8+MOV7_8+MOV10_8]
      } 
      if (state=="nCVD"){
        xc[,Value:=MOV1_9+MOV2_9+MOV3_9+MOV4_9+MOV5_9+MOV6_9+MOV7_9+MOV10_9]
      }  
      if (state=="Total"){
        xc[,Value:=MOV1_8+MOV2_8+MOV3_8+MOV4_8+MOV5_8+MOV6_8+MOV7_8+MOV10_8+
             MOV1_9+MOV2_9+MOV3_9+MOV4_9+MOV5_9+MOV6_9+MOV7_9+MOV10_9]
      } 
      
      
      # states s.
      for(st in c(1:7,10)){
        if (state == paste0("CVD(s.",st,")")) {
          xc[,Value:= eval(parse(text = paste0('MOV',st,'_8')))]
        } 
        if (state == paste0("nCVD(s.",st,")")){
          xc[,Value:= eval(parse(text = paste0('MOV',st,'_9')))]
        }  
        if (state == paste0("Total(s.",st,")")){
          xc[,Value:=eval(parse(text = paste0('MOV',st,'_9','+','MOV',st,'_8')))]
        } 
      }
     
      # states DIS
      for(dis in c('CVD','Dementia','Disability')){
        if (dis == 'CVD'){
          if (state == paste0("CVD(",dis,"all)")) {
            xc[,Value := MOV2_8 + MOV3_8 + MOV5_8 + MOV6_8]
          } 
          if (state == paste0("nCVD(",dis,"all)")){
            xc[,Value := MOV2_9 + MOV3_9 + MOV5_9 + MOV6_9]
          }  
          if (state == paste0("Total(",dis,"all)")){
            xc[,Value := MOV2_8 + MOV3_8 + MOV5_8 + MOV6_8 + 
                 MOV2_9 + MOV3_9 + MOV5_9 + MOV6_9]
          } 
        }else if (dis == 'Dementia'){
          if (state == paste0("CVD(",dis,"all)")) {
            xc[,Value := MOV6_8 + MOV7_8]
          } 
          if (state == paste0("nCVD(",dis,"all)")){
            xc[,Value := MOV6_9 + MOV7_9]
          }  
          if (state == paste0("Total(",dis,"all)")){
            xc[,Value := MOV6_8 + MOV7_8 + MOV6_9 + MOV7_9]
          } 
        }else if (dis == 'Disability'){
          if (state == paste0("CVD(",dis,"all)")) {
            xc[,Value := MOV5_8 + MOV6_8 + MOV7_8 + MOV10_8]
          } 
          if (state == paste0("nCVD(",dis,"all)")){
            xc[,Value := MOV5_9 + MOV6_9 + MOV7_9 + MOV10_9]
          }  
          if (state == paste0("Total(",dis,"all)")){
            xc[,Value := MOV5_8 + MOV6_8 + MOV7_8 + MOV10_8 + 
                 MOV5_9 + MOV6_9 + MOV7_9 + MOV10_9]
          } 
        }
      }
      xx_mort[[state]] <- xc
    }
    ##因为不同状态人群都将转向死亡，上述内容是将该年龄段下根据不同的死亡状态进行人数的汇总
    x <- rbindlist(xx_mort)
    rm(xx_mort)
    gc()
    subm_mort <- copy(x)
    subm_mort[, Pop:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]
    subm_mort_all <- copy(subm_mort)
    subm_mort_all$mortality <- subm_mort_all$Value/subm_mort_all$Pop #这里没有*100，单位不是%
    agelimit <- min(subm_mort_all$Age)
    setkey(subm_mort_all, Gender, Age)
    setkey(std, Gender, Age)
    subm_mort_all <- subm_mort_all[J(std)]
    subm_mort_all <- subm_mort_all[Age >= agelimit ,]
    # rm(std)
    subm_mort_stand <-
      subm_mort_all[, list(mort = weighted.mean(mortality, w = std.pop),
                           pop = sum(Pop)),
        by = list(Iter, Model, State, Year, Gender)]
    subm_mort_stand[,cases := mort * pop]
    subm_mort_stand[,Stand := 'standardized'] 
    
    subm_mort_nonstand <-
      subm_mort_all[, list(mort = weighted.mean(mortality, w = Pop),
                           pop = sum(Pop),
                           cases = sum(Value)),
                      by = list(Iter, Model, State, Year, Gender)]
    subm_mort_nonstand[,Stand := 'non-standardized']
    subm2 <- rbind(subm_mort_stand,subm_mort_nonstand)
    rm(subm_mort_stand,subm_mort_nonstand)
    gc()
    names(subm2)
    if ('deaths' %in% type & length(type) == 1 ){
      subm2x <- copy(subm2)
      subm2x[,Value := cases] 
      subm2x[,type := 'deaths']
      sub2 <- copy(subm2x)
      rm(subm2x)
    }
    if ('mortality' %in% type & length(type) == 1 ){
      subm2xx <- copy(subm2)
      subm2xx[,Value := mort] 
      subm2xx[,type := 'mortality']
      sub2 <- copy(subm2xx)
      rm(subm2xx)
    }
    if ('mortality' %in% type & 'deaths' %in% type &length(type) == 2 ){
      subm2x <- copy(subm2)
      subm2x[,Value := cases]
      subm2x[,type := 'deaths']
      
      subm2xx <- copy(subm2)
      subm2xx[,Value := mort] 
      subm2xx[,type := 'mortality']
      
      
      sub2 <- rbind(subm2x,subm2xx)
      rm(subm2x,subm2xx)
      gc()
    }
      
    subm2 <- copy(sub2)
    rm(sub2)
    gc()
    
    # subm2[,Group:=disease]
    subm2[,AgeGrp:=agegrp]
    grps[[agegrp]]<-subm2
    rm(subm)
    rm(subm2)
  }
  gc()
  
  pred<-rbindlist(grps)
  rm(grps)
  
  
  
  #differences
  if (ref.scenario!="No reference")
  {
    pred2<-dcast.data.table(pred, 
                            formula=Iter+Group+Gender+Year+AgeGrp+type+Group~Model, 
                            value.var="Value")
    scenarios<- as.character(unique(pred$Model))
    OtherScen <-scenarios[scenarios != ref.scenario]
    
    setnames(pred2,names(pred2), make.names(names((pred2))))
    scenarios<-make.names(scenarios)
    ref.scenario<-make.names(ref.scenario)
    
    for (sc in scenarios)
    {
      if (ver=="absolute") pred2[,eval(paste0(sc)):= eval(parse(text = paste0(sc,"-",ref.scenario)))]
      if (ver=="relative") pred2[,eval(paste0(sc)):= eval(parse(text = paste0("100*(",sc,"-",ref.scenario,")/",ref.scenario)))]
    }
    pred2<-melt(pred2, measure.vars=scenarios)
    setnames(pred2,c("variable","value"), c("Model","Value"))
    pred<-pred2
    rm(pred2)
  }

  pred.det<-pred[Iter==0]
  pred.psa<-pred[Iter>0]

  pred.psa<-pred.psa[!is.na(pred.psa$Value),
                     list(mean.Value=mean(Value), 
                          median.Value=quantile(Value, 0.5),
                           lcl.Value=quantile(Value, 0.025), 
                          ucl.Value=quantile(Value, 0.975),
                          max.Value = max(Value),
                          min.Value = min(Value)),
                     by=list(Model, Gender, Year,AgeGrp,type,State,Stand)]
  
  
  setkeyv(pred.psa,c("Model","Gender","Year","AgeGrp",'type','State','Stand'))
  setkeyv(pred.det,c("Model","Gender","Year","AgeGrp",'type','State','Stand'))
  
  pred.det[, Iter:=NULL]
  
  if (length(pred.psa$Model)>0 & length(pred.det$Model)>0) xf<-pred.det[J(pred.psa)]
  if (length(pred.psa$Model)==0 & length(pred.det$Model)>0) xf<-pred.det
  if (length(pred.psa$Model)>0 & length(pred.det$Model)==0) xf<-pred.psa
  if (length(pred.psa$Model)==0 & length(pred.det$Iter)==0) stop("zero rows in input data")
  
  # class(xf)<-"Mortality"
  # attr(xf,"ref.scenario")<-ref.scenario
  # attr(xf,"ver")<-ver
  # attr(xf,"disease")<-disease
  # attr(xf,"type")<-type

  # 
  # pred.psa[,type := type]
  # pred.psa[,disease:=disease]
  # pred.psa[,ver := ver]
  # pred.psa[,scenario :=ref.scenario]
  return(xf)
  
}

# Averted ---------------------------------------------------------------
CasesAverted<-function(m, models="",gender=c("Men","Women","All"),
                       states=c("Total deaths","CVD incident cases","CVD deaths","Non-CVD deaths","Dementia incident cases",
                                "Cognitive decline incident cases","Disability incident cases"),
                       ages=c(65:100),
                       cumulative=F,
                       type="absolute",
                       output=c("Median","UCL","LCL"),
                       settings=default.settings())
{
  movs<-names(m)[grep(pattern="^MOV[0-9]+_[0-9]+$", x=names(m), perl=T)]
  to<-as.numeric(sapply(strsplit(movs, split = "_"), "[", 2))
  from<-  as.numeric(substr(sapply(strsplit(movs, split = "_"), "[", 1),4,10))
  
  x<-copy(m)
  
  x[,TotalPopulation:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]#Total population: 当年的总人数
  x<-x[,c("Model","Iter","Gender","Year","Age","TotalPopulation",movs), with=F]
  
  for (i in unique(to))
  {
    pos<-(to==i & from!=i)
    
    a<-from[pos]
    b<-to[pos]
    
    x[,eval(paste0("TO",i)):=eval(parse(text=paste0("MOV",a,"_",b, collapse="+")))]
    #cat(paste0(paste0("TO",i),":=",paste0("MOV",a,"_",b, collapse="+"),"\n"))
  }
  
  tos<-sort(names(x)[grep(pattern="^TO[0-9]*", x=names(x), perl=T)])
  x<-x[,c("Model","Iter","Gender","Year","Age","TotalPopulation",tos), with=F]
  
  # x<-melt(x, id.vars=c("Model","Iter","Gender","Year","Age"))
  
  if (models[1]=="") models=unique(x$Model)
  
  scenarios=models[models!="Baseline"]
  if (("Baseline" %in% models)==F) stop("No model named 'Baseline' in input m parameter")
  
  x<-x[Gender %in% gender & Model %in% models,
       c("Iter", "Model","Year","Age","Gender","TotalPopulation",tos), with=F]
  if (class(ages)=="integer") {x[Age>=min(ages) & Age<=max(ages),AgeRange:=paste(min(ages), max(ages), sep="-")]}
  if (class(ages)=="list")
  {
    xx<-list()
    i<-1
    for (ar in ages)
    {
      i<-i+1
      xx[[paste(i)]]<-copy(x[Age>=min(ar) & Age<=max(ar)])
      xx[[paste(i)]][,AgeRange:=paste(min(ar), max(ar), sep="-")]
    }
    x<-rbindlist(xx)
    rm(xx)
  }
  
  x<-x[is.na(AgeRange)==F]
  
  x[,`Total deaths`:=TO8+TO9]
  x[,`CVD deaths`:=TO8]
  x[,`Non-CVD deaths`:=TO9]
  x[,`CVD incident cases`:=TO2+TO3+TO5+TO6]
  x[,`Disability incident cases`:=TO5+TO6+TO7+TO10] #TO10 added
  x[,`Cognitive decline incident cases`:=TO3+TO4+TO6+TO7]
  x[,`Dementia incident cases`:=TO6+TO7]
  x[,`Disease free`:=TO1] # more output
  #以下步骤通过melt把To几给编进变量中了
  x<-melt(x, id.vars=c("Iter","Model","Year","Gender","AgeRange","Age","TotalPopulation"), value.name = "Cases", variable.name = "State")
  x<-x[State %in% states]
  x<-x[,list(Cases=sum(Cases), TotalPopulation=sum(TotalPopulation)), by=list(Iter,Model, Year, Gender, AgeRange, State)]
  #总死亡率
  pop<-x[Model=="Baseline" & State=="Total deaths", list(Iter, Year, Gender, AgeRange,TotalPopulation)]
  x[,TotalPopulation:=NULL]
  
  x<-dcast.data.table(x,formula=Iter+State+Year+Gender+AgeRange~Model, value.var = "Cases")
  
  x<-merge(x, pop, by=c("Iter","Year","Gender","AgeRange"))
  
  for ( s in scenarios)
  {#用基线的人群减去两种场景对应的人群，得到不同数的减少量
    x[,eval(s):=eval(parse(text=paste0("Baseline-`",s,"`")))]
  }
  
  x<-melt(x,id.vars = c("Iter","State","Year","Gender","AgeRange","TotalPopulation"), variable.name="Scenario",value.name = "value" )
  
  if (cumulative & type =="per 100K population") stop("arguments: cumulative=TRUE and type='per 100K population' can not be used together.")
  
  if (type=="per 100K population") x[,value:=(value*100000/TotalPopulation)]
  
  x[,TotalPopulation:=NULL]
  
  if (cumulative)
  {
    setkey(x, Iter, State, Gender, AgeRange, Scenario, Year)
    x<-x[,list(value=cumsum(value), Year=Year), by=list(Iter, State, Gender, AgeRange, Scenario)]
  }
  
  x.det<-x[Iter==0]
  x.psa<-x[Iter>0]
  
  setnames(x.det,"value","deterministic.Value")
  
  x.psa<-x.psa[,list(mean.Value=mean(value), median.Value=median(value),
                     lcl.Value=quantile(value, 0.025), ucl.Value=quantile(value,0.975)),
               by=list(State, Year,Gender, AgeRange, Scenario)]
  
  setkey(x.psa, State, Year,Gender, AgeRange, Scenario)
  setkey(x.det, State, Year,Gender, AgeRange, Scenario)
  
  x.det[, Iter:=NULL]
  
  if (length(x.psa$Scenario)>0 & length(x.det$Scenario)>0) xf<-x.det[J(x.psa)]
  if (length(x.psa$Scenario)==0 & length(x.det$Scenario)>0) xf<-x.det
  if (length(x.psa$Scenario)>0 & length(x.det$Scenario)==0) xf<-x.psa
  if (length(x.psa$Scenario)==0 & length(x.det$Scenario)==0) stop("zero rows in input data")
  
  if (("Median" %in% output)==F) xf[,median.Value:=NULL]
  if (("Mean" %in% output)==F) xf[,mean.Value:=NULL]
  if (("LCL" %in% output)==F) xf[,lcl.Value:=NULL]
  if (("UCL" %in% output)==F) xf[,ucl.Value:=NULL]
  if (("Deterministic" %in% output)==F) xf[,deterministic.Value:=NULL]
  
  class(xf)<-c("Incidences","data.table","data.frame")
  
  #copy attributes from m object
  
  attributes(xf)$code.files<-attributes(m)$code.files
  attributes(xf)$inputs.files<-attributes(m)$inputs.files
  attributes(xf)$code.version.description<-attributes(m)$code.version.description
  attributes(xf)$inputs.version.description<-attributes(m)$inputs.version.description
  attributes(xf)$created<-attributes(m)$created
  attributes(xf)$code.version<-attributes(m)$code.version
  attributes(xf)$inputs.version<-attributes(m)$inputs.version
  attributes(xf)$scenario.prevalences<-attributes(m)$scenario.prevalences
  attributes(xf)$scenario.name<-attributes(m)$scenario.name
  attributes(xf)$output.created<-date()
  
  return(xf)
  
}

# Life expectancy ---------------------------------------------------------------
LE<-function(m,years=2016:2050,models="",
             gender=c("Men","Women","All"), 
             ages=c(65), 
             method=2,
             ref.scenario="No reference",ver="absolute", ref.year=0, DFLE_LE_ratio=F,
             DLE_LE_ratio=F,
             DemLE_LE_ratio=F,
             type=c("LE","LE_no_dementia","LE_no_disab", 
                    "LE_healthy", "LE_with_disab","LE_with_dementia",
                    "LE_no_cvd","LE_no_ci"),
             output=c("Median","LCL","UCL"))
{
  
  #mx<-calc2.LE(m, method=method)
  if (models=="") models=unique(m$Model)
  if (DFLE_LE_ratio)
  {
    m[,DFLE_LE_ratio:=LE_no_disab/LE]
    type<-c(type,"DFLE_LE_ratio")
  }
  
  if (DemLE_LE_ratio)
  {
    m[,DemLE_LE_ratio:=LE_with_dementia/LE]
    type<-c(type,"DemLE_LE_ratio")
  }
  
  x1<-m[Year %in% years & Gender %in% gender & Age %in% ages & Model %in% models,
        c("Iter", "Model", "Gender", "Year", "Age",type), with=F]
  
  #x_checkLE<-m[Year %in% years & Gender %in% gender & Age %in% ages & Model %in% models,
         #c("Iter", "Model", "Gender", "Year", "Age","qx","mx","lx","Lx","Tx",type), with=F]  
 
  # x<-melt(x_checkLE, measure.vars=type)
  # rm(x_checkLE)   
  
  

  
  x<-melt(x1, measure.vars=type)
  rm(x1)
  setnames(x,c("variable","value"), c("Type","Value"))
  
  
  if (ref.year>0)
  {
    
    x[,Year:=paste0("y",Year)]
    all.years<- as.character(unique(x$Year))
    x2<-dcast.data.table(x, formula=Iter+Gender+Model+Age+Type~Year, value.var="Value")
    ref.yyear<-paste0("y",ref.year)
    other.years <-all.years[all.years != ref.yyear]
    
    for (yr in other.years)
    {
      if (ver=="absolute") x2[,eval(paste0(yr)):= eval(parse(text = paste0(yr,"-",ref.yyear)))]
      if (ver=="relative") x2[,eval(paste0(yr)):= eval(parse(text = paste0("100*(",yr,"-",ref.year,")/",ref.yyear)))]
    }
    
    x2<-melt(x2, measure.vars=all.years)
    setnames(x2,c("variable","value"), c("Year","Value"))
    x2[,Year:=as.numeric(substr(Year, 2, 5))]
    x2[Year==ref.year,Value:=0]
    x<-x2
    rm(x2)
  }
  
  if (ref.scenario!="No reference")
  {
    x2<-dcast.data.table(x, formula=Iter+Gender+Year+Age+Type~Model, value.var="Value")
    scenarios<- as.character(unique(x$Model))
    OtherScen <-scenarios[scenarios != ref.scenario]
    
    #setnames(x2,names(x2), make.names(names((x2))))
    #scenarios<-make.names(scenarios)
    #ref.scenario<-make.names(ref.scenario)
    
    for (sc in c(scenarios[scenarios != ref.scenario], ref.scenario))
    {
      if (ver=="absolute") x2[,eval(paste0("",sc,"")):= eval(parse(text = paste0("`",sc,"`-`",ref.scenario,"`")))]
      if (ver=="relative") x2[,eval(paste0("",sc,"")):= eval(parse(text = paste0("100*(`",sc,"`-`",ref.scenario,"`)/`",ref.scenario,"`")))]
    }
    x2<-melt(x2, measure.vars=scenarios)
    setnames(x2,c("variable","value"), c("Model","Value"))
    x<-x2
    rm(x2)
  }
  
  x.det<-x[Iter==0]
  x.psa<-x[Iter>0]
  
  x.psa<-x.psa[,list(mean.Value=mean(Value), 
                     median.Value=median(Value),
                     lcl.Value=quantile(Value, 0.025), 
                     ucl.Value=quantile(Value, 0.975),
                     max.Value = max(Value),
                     min.Value = min(Value)),
               by=list(Model, Gender, Year, Age, Type)]
  
  
  setkeyv(x.psa,c("Model","Year","Gender","Age","Type"))
  setkeyv(x.det,c("Model","Year","Gender","Age","Type"))
  
  x.det[, Iter:=NULL]
  
  if (length(x.psa$Model)>0 & length(x.det$Model)>0) xf<-x.det[J(x.psa)]
  if (length(x.psa$Model)==0 & length(x.det$Model)>0) xf<-x.det
  if (length(x.psa$Model)>0 & length(x.det$Model)==0) xf<-x.psa
  if (length(x.psa$Model)==0 & length(x.det$Model)==0) stop("zero rows in input data")
  
  setnames(xf, "Value","deterministic.Value")
  
  if (("Median" %in% output)==F) xf[,median.Value:=NULL]
  if (("Mean" %in% output)==F) xf[,mean.Value:=NULL]
  if (("LCL" %in% output)==F) xf[,lcl.Value:=NULL]
  if (("UCL" %in% output)==F) xf[,ucl.Value:=NULL]
  if (("Deterministic" %in% output)==F) xf[,deterministic.Value:=NULL]
  
  class(xf)<-append("LE",class(xf))
  
  #  attr.names<-names(attributes(m))
  #  for (an in attr.names[(attr.names %in% c("row.names","class",".internal.selfref","names")==F)])
  #    attributes(xf)[an]<-attributes(m)[an]
  
  attributes(xf)$output.created<-date()
  attributes(xf)$psa.iters<-as.data.table(m[,list(`PSA iterations`=max(Iter)),by="Model"])
  
  attributes(xf)$output.created<-date()
  #saveRDS(xf,"D:/CAM_model_learning_ZYJ/Rdata/LE_calculation_check_CI2.5_FI0.08.RDS")
  return(xf)
}

LY.gained<-function(m, ref.model="Baseline",
                    models="",gender=c("Men","Women","All"),
                    ages=list(c(35:100),c(65:100)),
                    years=c(2006:settings$finalYear),
                    cumulative=T,
                    output=c("Median","UCL","LCL"),
                    settings=default.settings())
{
  
  # ref.model="Baseline"
  # models=""
  # gender=c("All")
  # ages=list(c(65:100))
  # years=c(2006:settings$finalYear)
  # cumulative=T
  # output=c("Median","UCL","LCL")
  # 
  # 
  
  if (models=="") models=unique(m$Model)
  x1<-list()
  for (a in ages)
  {
    x1[[a[1]]]<-m[Year %in% years & Gender %in% gender & Age %in% a & Model %in% models,
                  list(Iter,Model, Gender, Year, Age, s.1,s.2,s.3,s.4,s.5,s.6,s.7, s.10)]
    x1[[a[1]]][,AgeGrp:=paste0(min(a),"-",max(a))]
  }
  x1<-rbindlist(x1)
  
  x<-x1[,list(LY=sum(s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10),
              LY_no_dementia=sum(s.1+s.2+s.3+s.4+s.5+s.10),
              LY_dementia=sum(s.6+s.7),
              LY_no_disab=sum(s.1+s.2+s.3+s.4),
              LY_disab=sum(s.5+s.6+s.7+s.10)),
        by=list(Iter,Year,Model, Gender, AgeGrp)]
  
  
  
  x<-melt(x, id.vars = c("Iter","Year","Model","Gender","AgeGrp"), variable.name = "Type",value.name = "LY")
  
  x1<-x[Model==ref.model]
  setnames(x1, "LY","LY_baseline")
  x1[,Model:=NULL]
  x2<-x[Model!=ref.model]
  
  x<-merge(x1,x2, by=c("Iter","Year","Gender","AgeGrp","Type"))
  
  x[,LY_gained:=LY-LY_baseline]
  
  if (cumulative)
  {
    #cat("\nCumulative version")
    setkey(x, Iter, Gender, AgeGrp, Type, Model, Year)
    x<-x[,list(LY_gained=cumsum(LY_gained), LY=cumsum(LY), LY_baseline=cumsum(LY_baseline), Year=Year),
         by=list(Iter, Gender, AgeGrp, Type, Model)]
  }
  
  x<-dcast.data.table(x, formula=Iter+Gender+AgeGrp+Model+Year~Type, value.var = "LY_gained")
  x[,PropDementia:=LY_dementia/LY]
  x[,PropDisability:=LY_disab/LY]
  x<-melt(x, id.vars = c("Iter","Gender","AgeGrp","Model","Year"), variable.name = "Type", value.name = "LY_gained")
  
  x.det <- x[Iter == 0]
  x.det[, Iter := NULL]
  x.psa <- x[Iter > 0]
  
  x.psa <- x.psa[, list(
    median.Value = median(LY_gained, na.rm=T),
    lcl.Value = quantile(LY_gained, 0.025, na.rm=T),
    ucl.Value = quantile(LY_gained, 0.975, na.rm=T)
  ),
  by = list(Model, Gender, Year, AgeGrp, Type)]
  setnames(x.det, "LY_gained","deterministic.Value")
  
  
  xf<-merge(x.det, x.psa, by=c("Model","Gender","Year","AgeGrp","Type"))
  
  class(xf)<-c("LYG",class(xf))
  attributes(xf)$models=models
  attributes(xf)$gender=gender
  attributes(xf)$ages=ages
  attributes(xf)$years=years
  attributes(xf)$cumulative=cumulative
  attributes(xf)$output=output
  attributes(xf)$ref.model=ref.model
  
  
  return(xf)
  
  
  
}

# TPs ---------------------------------------------------------------------

# m <- readRDS('Different data scoures result/final_result of Baseline1203.RDS')
# special for any tps
flowchart_China<-function(m, 
                          gender=c('All',"Men","Women"),
                          ages=list(c(35:100),c(65:100)),
                          roads = c('All road'), # ,'Single road'
                          median_state=c('Health',"CVD Total",
                                     'CVD only','CVD & FI',
                                     'CVD & CI','CVD only',
                                     'Dementia only','FI only',
                                     'CI Total','Dementia Total', 
                                     'CVD Total','FI Total'),
                          ref.scenario="No reference",
                          ver="absolute")
{
  require(data.table)
  resdata <- data.table(
    statenum = c(c(1:7,10),'2,4,6,7','6,7','2,3,5,6','5,6,7,8'),
    median_state = c('Health','CVD only','CVD & CIND',
                     'CVD & CI','CVD & FI','CVD & Dementia',
                     'Dementia only','FI only',
                     'CI Total','Dementia Total', 
                     'CVD Total','FI Total')
  )
  
  
  grps<-list()
  
  for (a in ages)
  {
    agegrp<-paste0(min(a),"-",max(a))
    subm<-(m[Age %in% a & Gender %in% gender,])
    
    to <- c(1:10)
    namesall <- names(subm)
    
    if ('All road' %in% roads & length(roads) == 1){
      submx_all <- list()
      for (sta in median_state){
        statenum <- as.numeric(unlist(strsplit(resdata[resdata$median_state == sta, 
                                                       statenum],split = ',')))
        input_names <- paste0('MOV',statenum,'_',rep(to,e = length(statenum)))
        input_names <- input_names[input_names %in% namesall] 
        output_names <- paste0('MOV',to,'_',rep(statenum,e = 10))
        output_names <- output_names[output_names %in% namesall]
        submx1 <- copy(subm)
        submx1[,`:=`(Value = eval(parse(text = paste0(input_names ,collapse = ' + '))))]
        submx1[,type := 'Out']
        submx2 <- copy(subm)
        submx2[,`:=`(Value = eval(parse(text = paste0(output_names ,collapse = ' + '))))]
        submx2[,type := 'In']
        submx <- rbind(submx1,submx2)
        submx[,Pop:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]
        submx[,roads := roads]
        submx[,roads_name := sta]
        submx[,State := sta]
        submx_all[[sta]] <- submx
        # single 
      }
      submtotal <- rbindlist(submx_all)
      rm(submx_all)
    }
   
    if ('Single road' %in% roads & length(roads) == 1){
      submx_single <- list()
      for (sta in median_state){
        statenum <- as.numeric(unlist(strsplit(resdata[resdata$median_state == sta, 
                                                       statenum],split = ',')))
        input_names <- paste0('MOV',statenum,'_',rep(to,e = length(statenum)))
        input_names <- input_names[input_names %in% namesall] 
        output_names <- paste0('MOV',to,'_',rep(statenum,e = 10))
        output_names <- output_names[output_names %in% namesall]
        submx_inp <- list()
        for (ipn in input_names){
          submxx1 <- copy(subm)
          submxx1[,`:=`(Value = ipn)]
          submxx1[,type := 'Out']
          submxx1[,roads := roads]
          submxx1[,roads_name := ipn]
          submx_inp[[ipn]] <- submxx1
          rm(submxx1)
        }
        
        submx_oup <- list()
        for (oup in output_names){
          submxx2 <- copy(subm)
          submxx2[,`:=`(Value = oup)]
          submx2[,type := 'In']
          submxx2[,roads := roads]
          submxx2[,roads_name := oup]
          submx_oup[[oup]] <- submxx2
          rm(submxx2)
        }
        submx_inp <- rbindlist(submx_inp)
        submx_oup <- rbindlist(submx_oup)
        submx_inpoup <- rbind(submx_inp,submx_oup)
        submx_inpoup[,Pop:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]
        submx_inpoup[,State := sta]
        submx_single[[sta]] <- submx_inpoup
        # single 
      }
      submtotal <- rbindlist(submx_single)
      rm(submx_single)
    }
    
    # if ('Single road' %in% roads & 'All road' %in% roads & length(roads) == 2){
    #   submx_all <- list()
    #   submx_single <- list()
    #   for (sta in median_state){
    #     statenum <- as.numeric(unlist(strsplit(resdata[resdata$median_state == sta, 
    #                                                    statenum],split = ',')))
    #     input_names <- paste0('MOV',statenum,'_',rep(to,e = length(statenum)))
    #     input_names <- input_names[input_names %in% namesall] 
    #     output_names <- paste0('MOV',to,'_',rep(statenum,e = 10))
    #     output_names <- output_names[output_names %in% namesall] 
    #     
    #     
    #     submx1 <- copy(subm)
    #     submx1[,`:=`(Value = eval(parse(text = paste0(input_names ,collapse = ' + '))))]
    #     submx1[,type := 'Out']
    #     submx2 <- copy(subm)
    #     submx2[,`:=`(Value = eval(parse(text = paste0(output_names ,collapse = ' + '))))]
    #     submx2[,type := 'In']
    #     submx <- rbind(submx1,submx2)
    #     rm(submx1,submx2)
    #     submx[,Pop:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]
    #     submx[,roads := "All road"]
    #     submx[,roads_name := sta]
    #     submx[,State := sta]
    #     submx_all[[sta]] <- submx
    # 
    #     # single
    #     submx_inp <- list()
    #     for (ipn in input_names){
    #       submxx1 <- copy(subm)
    #       submxx1[,`:=`(Value = ipn)]
    #       submxx1[,type := 'Out']
    #       submxx1[,roads := 'Single road']
    #       submxx1[,roads_name := ipn]
    #       submx_inp[[ipn]] <- submxx1
    #       rm(submxx1)
    #     }
    #     
    #     submx_oup <- list()
    #     for (oup in output_names){
    #       submxx2 <- copy(subm)
    #       submxx2[,`:=`(Value = oup)]
    #       submxx2[,type := 'In']
    #       submxx2[,roads := 'Single road']
    #       submxx2[,roads_name := oup]
    #       submx_oup[[oup]] <- submxx2
    #       rm(submxx2)
    #     }
    #     submx_inp <- rbindlist(submx_inp)
    #     submx_oup <- rbindlist(submx_oup)
    #     submx_inpoup <- rbind(submx_inp,submx_oup)
    #     submx_inpoup[,Pop:=s.1+s.2+s.3+s.4+s.5+s.6+s.7+s.10]
    #     submx_inpoup[,State := sta]
    #     submx_single[[sta]] <- submx_inpoup
    #     # single 
    #   }
    #   submx_all <- rbindlist(submx_all)
    #   submx_single <- rbindlist(submx_single)
    #   submtotal <- rbind(submx_all,submx_single)
    # }
    # rm(submx_all,submx_single)
    gc()

    # subm2[,Group:=disease]
    submtotal[,AgeGrp:=agegrp]
    grps[[agegrp]]<-submtotal
    rm(submtotal)
  }

  pred_orgin<-rbindlist(grps)
  rm(grps)
  pred <- pred_orgin[!is.na(pred_orgin$Value),list(Value=sum(Value)),
                   by=list(Iter,Model,Gender,Year,AgeGrp,type,roads,roads_name,State)]


  pred.det<-pred[Iter==0]
  pred.psa<-pred[Iter>0]

  pred.psa<-pred.psa[!is.na(pred.psa$Value),list(mean.Value=mean(Value), 
                                                 median.Value=quantile(Value, 0.5),
                                                 lcl.Value=quantile(Value, 0.025), 
                                                 ucl.Value=quantile(Value, 0.975),
                                                 max.Value = max(Value),
                                                 min.Value = min(Value)),
                     by=list(Model, Gender, Year,AgeGrp,type,roads,roads_name,State)]


  setkeyv(pred.psa,c("Model","Gender","Year","AgeGrp",'type','roads',"roads_name","State"))
  setkeyv(pred.det,c("Model","Gender","Year","AgeGrp",'type','roads',"roads_name","State"))

  pred.det[, Iter:=NULL]

  if (length(pred.psa$Model)>0 & length(pred.det$Model)>0) xf<-pred.det[J(pred.psa)]
  if (length(pred.psa$Model)==0 & length(pred.det$Model)>0) xf<-pred.det
  if (length(pred.psa$Model)>0 & length(pred.det$Model)==0) xf<-pred.psa
  if (length(pred.psa$Model)==0 & length(pred.det$Iter)==0) stop("zero rows in input data")

  return(xf)

}
# 
# 
# 
# Plot ----
plot_LY_change<-function(x, ylab="Life years", Type=c("LY","LY_no_dementia","LY_no_disab"), alternative=F,
                  show.values=c("Deterministic","Stochastic mean","Stochastic median","Confidence limits"))
{
  x<-as.data.table(x)
  x<-x[variable %in% Type]
  
  p<-ggplot(data=x, aes(x=AgeGrp, y=value, fill=Model))
  if ("Deterministic" %in% show.values) p<-p+geom_bar(stat = "identity", position="dodge")
  if ("Confidence limits" %in% show.values) p<-p+geom_errorbar(aes(ymin=lcl, ymax=ucl),
                                                               colour="black", width=.1,  position = position_dodge(width=0.9))
  if ("Stochastic mean" %in% show.values) p<-p+geom_point(aes(y=mea),position = position_dodge(width=0.9))
  if ("Stochastic median" %in% show.values) p<-p+geom_point(aes(y=med),position = position_dodge(width=0.9))
  p<-p+facet_wrap(Gender~variable)+
    xlab("Age group")+ylab("Life years")+
    
    expand_limits(y=0)
  
  return(p)
}
