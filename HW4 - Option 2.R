rm(list=ls(all=T)) # this just removes everything from memory


usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#initializeKeras()
initializeKeras<-function(first.time=F){
  
  if(Sys.info()["sysname"]=="Windows") {
    usePackage("reticulate")
  }
  
  usePackage("keras")
  if(first.time){
    library(keras);keras::install_keras()
  }
  
  py_discover_config()
  py_available(initialize = FALSE)
  import("numpy")
  import("keras")
  
  stopifnot(py_module_available('keras'))
  
} #initializeKeras


getRawFred<-function(symbols,...){
  # get Raw (not transformed) data from quandl
  usePackage('Quandl')
  #SET KEY
  #Please do not use my file, use your file or replace readChar() with your api key
  Quandl::Quandl.api_key(
    readChar('C:/Users/navee/Dropbox (CSU Fullerton)/ISDS 570/SHARED/HW4/quandl.api.txt',file.info('C:/Users/navee/Dropbox (CSU Fullerton)/ISDS 570/SHARED/HW4/quandl.api.txt')$size)
  )
  
  raw.fred=Quandl(paste('FRED/',symbols,sep=''), ...)
  names(raw.fred)<-symbols
  return(raw.fred)  
}

#Question number 1
#time.series.xts=getRawFred(symbols=c('GDPC1','UNRATE','CPIAUCSL','EMRATIO','INDPRO'),start_date='1979-09-01',collapse='quarterly',type='xts')


oneStepGDPGrowthForecast<-function(time.series.xts,seed=1){
  initializeKeras() # Lexical error is a known April 2019 bug - workaround: in R: update tensorflow package; install devtools package; uninstall keras package; run: devtools::install_github("rstudio/reticulate",upgrade='never'); run: devtools::install_github("rstudio/keras",upgrade='never'); run: initializeKeras(first.time=T)
  
  #Note: you do not need to difference all predictors
  differenced.xts<-diff(log(time.series.xts))
  
  #Do not lag y
  y<-differenced.xts[,1]
  
  #Lag x if appropriate (note that you can use lagged y as x - just change 2 to 1)
  x<-lag(differenced.xts[,2:ncol(differenced.xts)])#use k=n to add lags 
  names(x)<-paste(names(x),'.lag1',sep='')
  
  #combine
  transformed.xts<-merge(y,x)
  #extract x.new
  x.new<-tail(transformed.xts,1); x.new<-x.new[,2:ncol(x.new)]
  #remove na's
  transformed.xts<-na.omit(transformed.xts)
  
  x.matrix<-as.matrix(transformed.xts[,2:ncol(transformed.xts)])
  y.vector<-as.vector(transformed.xts[,1])
  
  x.train<-x.matrix
  y.train<-y.vector
  
  # scale (normalize) data
  
  #NOTE: for non-normal data consider other type od scaling such as (y.train-min(y.train))/(max(y.train)-min(y.train)), remember to unscale with a propoer equation
  #plot(density(y.train))
  #shapiro.test(y.train) #small p-values indicate extreme non-normality
  #qqnorm(y.train);qqline(y.train)
  
  x.train.scaled<-scale(x.train)
  y.train.scaled<-scale(y.train)
  
  # scale (normalize) new x
  x.new.scaled <- scale(x.new, center = attr(x.train.scaled, "scaled:center") , scale = attr(x.train.scaled, "scaled:scale"))  
  
  #This is required (you may skip Quiet=T)
  #use_session_with_seed(seed,disable_gpu=T,disable_parallel_cpu = T,quiet=T)
  tensorflow::tf$random$set_seed(1)
  
  # Model - this is where you need to setup your model
  hidden.units.1=5
  hidden.units.2=3
  
  model = keras_model_sequential()
  model %>%
    layer_dense(input_shape = ncol(x), units = hidden.units.1, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(units = hidden.units.2, activation = "relu") %>%
    layer_dropout(0.2) %>%
    layer_dense(units = 1)
  
  #regression-specific compilation
  model %>%
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error","MSE")
    )
  
 # summary(model)
  
  
  #This is where you need to setup your fitting parameters
  verbose=1
  validation.split=0.3
  epochs=100
  batch.size=5
  patience=10
  
  callbacks=NULL
  if(!is.na(patience)) callbacks = list(keras::callback_early_stopping(patience=patience,mode="auto"))
  
  
  fit = model %>%
    keras::fit(
      x = x.train.scaled,
      y = y.train.scaled,
      shuffle = T,
      verbose=verbose,
      validation_split = validation.split,
      epochs = epochs,
      callbacks=callbacks,
      batch_size = batch.size
    )
  #plot(fit)
  
  predictions <- model %>% predict(x.new.scaled)
  #unscale (denormalize)
  #NOTE: rewrite if you use other types of scaling
  predictions <- predictions * attr(y.train.scaled, "scaled:scale") + attr(y.train.scaled, "scaled:center")
  
  #recycle x.new (so we do not have to wory about the index)
  fcst.xts<-x.new
  fcst.xts$fcst<-predictions
  fcst.xts<-fcst.xts[,'fcst']
  
  return(fcst.xts)
} # of oneStepGDPGrowthForecast()

#Question number 2
#gdp.fcst<-oneStepGDPGrowthForecast(time.series.xts=getRawFred(symbols=c('GDPC1','UNRATE','CPIAUCSL','EMRATIO','INDPRO'),start_date='1979-09-01',collapse='quarterly',type='xts'))

#Quetsion 3
#Run forecast in a loop, plot and report MASE

#parameters
backtest.start='2007-01-01'
gdp.forecast.backtest<-NULL

#load
symbols=c('GDPC1','UNRATE','CPIAUCSL','EMRATIO','INDPRO')
start.date='1979-09-01'
time.series.xts=getRawFred(symbols=symbols,start_date=start.date,collapse='quarterly',type='xts')

#enumerator
backtest.quarters<-index(time.series.xts)
backtest.quarters<-backtest.quarters[which(backtest.quarters>=backtest.start)]


#backtest loop: subset and run (inefficient but safe)
for(q in backtest.quarters){
  #q<-backtest.quarters[1]
  cat('Backtesting',q,'...\n')
  #subset
  time.series.xts.subset<-time.series.xts[which(index(time.series.xts)<=q),]
  #forecast
  one.step.fcst<-oneStepGDPGrowthForecast(time.series.xts=time.series.xts.subset)
  #append
  gdp.forecast.backtest<-rbind(gdp.forecast.backtest,one.step.fcst)
} # of for each quarter


 #attach actuals
gdp.forecast.backtest<-cbind(diff(log(time.series.xts[,1]))[which(index(time.series.xts)>=backtest.start),],gdp.forecast.backtest)
#col names
names(gdp.forecast.backtest)<-c('GDP.growth','GDP.growth.fcst')

#Question 4
usePackage('PerformanceAnalytics')
chart.TimeSeries(gdp.forecast.backtest,legend.loc='bottomright') 
tail(gdp.forecast.backtest)

#MASE
getMASE<-function(y.xts, yhat.xts){
  #Mean Absolute Scaled Error
  comp<-na.omit(cbind(y.xts,yhat.xts))
  comp$AE<-abs(comp[,1]-comp[,2])
  sum.abs.diff.y<-sum(abs(diff(y.xts)),na.rm=T)
  TT<-nrow(comp)
  MASE=sum(comp$AE)/( (TT/(TT-1)) * sum.abs.diff.y)
  return(MASE)
}

MASE=getMASE(gdp.forecast.backtest[,1],gdp.forecast.backtest[,2])
cat('MASE:',MASE,'\n')
