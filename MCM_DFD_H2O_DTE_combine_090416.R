#CREATED: GTP 8-18-16
#UPDATED: GTP 9-4-16
#combine blext, DFD, DWSD, & DTE(!) datasets to run predictive models in order to predict MCM occupancy
#the new twist here is that I'm only looking at blext data that's been recorded since
#January 1st, 2016

setwd("/Users/geoffperrin/Desktop/DLBA/R_folder/working_directory/")
## set the seed to make your partition reproductible
set.seed(123)

###INSTALL PACKAGES
#gmodels allows crosstables to be created
install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("caret")
install.packages("pscl")
install.packages("ROCR")
install.packages("reshape")
install.packages("reshape2")
install.packages("munsell")
install.packages("data.table")
install.packages("randomForest")
install.packages("rpart")
install.packages("pROC")
install.packages("plotROC")
library("gmodels")
library("lubridate")
library("plyr")
library("caret")
library("pscl")
library("ROCR")
library("reshape")
library("reshape2")
library("data.table")
library("randomForest")
library("rpart")
library("pROC")
library("plotROC")

###DATA INPUT###
  #read in blext data
    #q: what was v2 of this file?
    #a: v2 was the file that made sure that parcel ID was coded as text - VERY IMPORTANT
    blext_raw <- read.csv("Recent_Blexts_8_18_16_v2.csv")
    
    #subset to residential data
      blext <- blext_raw[which(blext_raw$Property.Class=="Residential-Structure"),]
      blext <- blext[which(blext$MCM.Structure=="Yes"),]
      
    #  '''
    #    NOTE: code <<maybe>> or <<partial>> occupied houses as occupied... Ive googled a lot of the <<maybes>> and theyre nearly all
    #    structually sound --- i.e., someone could occupy it, but someone might not be sleeping there every night - GP line of thinking
    #    about 3k rows are <<maybe>> or <<partial>>
    #    blext_unknown<-subset(blext, (!(blext$MCM.Occupancy=="Occupied") & !(blext$MCM.Occupancy=="Unoccupied")))
    #  '''
      
      blext[(!(blext$MCM.Occupancy=="Occupied") & !(blext$MCM.Occupancy=="Unoccupied")), "MCM.Occupancy"] <- "Occupied"

    #add occupied dummy var
      blext$MCM.Occupancy.Dummy <- 1
      blext[(blext$MCM.Occupancy=="Unoccupied"), "MCM.Occupancy.Dummy"]<-0  
      
    #only keep vars we need
      blext_vars <- c("Account.Name", "Parcel.ID", "MCM.Condition", "MCM.Structure", "MCM.Occupancy.Dummy")      
      blext <- blext[blext_vars]
  
      
  #read in DFD/USPS data
    DFD_USPS_raw <- read.csv("Fire_USPS_Data_8_18_16.csv")
        
    #subset to residential data
      DFD_USPS <- DFD_USPS_raw[which(DFD_USPS_raw$Property.Class=="Residential-Structure"),]
        
    #code fire dummy var
      DFD_USPS$Fire.Dummy <- ifelse((DFD_USPS$DFD.Fire..Occurrence.Date)=="",0,1)

    #code USPS occupancy dummy
      DFD_USPS$USPS.Occupancy.Dummy <- ifelse((DFD_USPS$USPS..Vacancy.Indicated)=="No",1,0)
          
    #only keep vars we need
      DFD_USPS_vars <- c("Account.Name", "Parcel.ID", "Fire.Dummy", "USPS.Occupancy.Dummy")
      DFD_USPS <- DFD_USPS[DFD_USPS_vars]
      
  #read in DWSD water usage data
    dwsd_raw<-read.csv("DWSD_12MoUsage_20160815.csv")
    dwsd_vars <- c("TAXTYPECODE", "ACCOUNTSTATUSCODE", "Ref_ID",
                    "CURRENTPREMISESTREETNUMBER", "CURRENTPREMISESTREETNAME", 
                    "DECEMBER", "NOVEMBER", "OCTOBER", "SEPTEMBER", "AUGUST", "JULY", "JUNE", "MAY",
                    "APRIL", "MARCH", "FEBRUARY", "JANUARY")
    dwsd <- dwsd_raw[dwsd_vars]
    names(dwsd)[names(dwsd) == "Ref_ID"] <- "Parcel.ID"  
    dwsd$dwsd.account.on.dummy <- ifelse((dwsd$ACCOUNTSTATUSCODE)>0,0,1)
    
    dwsd <- subset(dwsd, (dwsd$Parcel.ID!="-1"))
    dwsd[6:18] <- lapply(dwsd[6:18], function(x) as.numeric(as.character(x)))
    dwsd[6:18][is.na(dwsd[6:18])] <- 0

    dwsd_3 <- data.table(dwsd, key="Parcel.ID")
    dwsd_3 <- dwsd_3[, list(TAXTYPECODE, ACCOUNTSTATUSCODE, Parcel.ID, CURRENTPREMISESTREETNUMBER, CURRENTPREMISESTREETNAME, dwsd.account.on.dummy,
                            DECEMBER=sum(DECEMBER), NOVEMBER=sum(NOVEMBER), OCTOBER=sum(OCTOBER), SEPTEMBER=sum(SEPTEMBER), AUGUST=sum(AUGUST), 
                            JULY=sum(JULY), JUNE=sum(JUNE), MAY=sum(MAY),
                            APRIL=sum(APRIL), MARCH=sum(MARCH), FEBRUARY=sum(FEBRUARY), JANUARY=sum(JANUARY)), by=Parcel.ID]
    dwsd_3 <- dwsd_3[!duplicated(dwsd_3$Parcel.ID), ]   
    dwsd_3[,1] <- NULL
    attach(dwsd_3)
    dwsd_3$year_total <- DECEMBER+NOVEMBER+OCTOBER+SEPTEMBER+AUGUST+JULY+JUNE+MAY+APRIL+MARCH+FEBRUARY+JANUARY
    detach(dwsd_3)    
    
    #drop missing or blank parcel.ID rows
      dwsd_final <- dwsd_3[which(!(trimws(dwsd_3$Parcel.ID)==""|trimws(dwsd_3$Parcel.ID)=="-")),]    
      
      
  #read in QVF (voting records) data
    qvf_raw<-read.csv("QVF_Detroit20160816.csv")
    qvf<-qvf_raw
    colnames(qvf)[5] <- "Parcel.ID"
    #change reg date to a date object
      qvf$date_reg<-mdy(qvf$date_reg)
      
    #only keep vars we need
      qvf_vars=c("Parcel.ID", "date_reg")
      qvf_2<-qvf[qvf_vars] 
    #need to make this list unique, with only one Parcel.ID entry and the most recent date_reg
      qvf_2_sort<-qvf_2[order(qvf_2$Parcel.ID, qvf_2$date_reg),]
      most_recent_qvf<-qvf_2_sort[!duplicated(qvf_2_sort$Parcel.ID, fromLast = TRUE),]   
      

  
  #read in DTE data
      dte_raw <- read.csv("DTE_ActiveAgreementsGasElectric_20160824.csv")
      dte_vars <- c("Ref_ID", "DS_SER_PRODUCT_TYPE")
      dte <- dte_raw[dte_vars]
      #remove duplicates
      dte_nodupe <- dte[!duplicated(dte),]
      
      #reshape from long to wide
        dte_reshape <- dcast(dte_nodupe, Ref_ID ~ DS_SER_PRODUCT_TYPE)
        dte_reshape$Var.2 <- NULL
        dte_reshape <- dte_reshape[which(dte_reshape$Ref_ID!=""),]
        #code gas and electric dummy vars
          dte_reshape$Gas.Dummy <- ifelse(is.na(dte_reshape$GAS),0,1)
          dte_reshape$Electric.Dummy <- ifelse(is.na(dte_reshape$ELC),0,1)
        
        #rename and keep the columns we want
          colnames(dte_reshape)[1] <- "Parcel.ID"
          dte_vars <- c("Parcel.ID", "Gas.Dummy", "Electric.Dummy")
          dte_final <- dte_reshape[dte_vars]
      
###END DATA INPUT###
      
      
###merge data sets together
      
  #merge blext and DFD_USPS together
      blext_DFD_USPS <- merge(x=blext, y=DFD_USPS, by="Parcel.ID", all.x=TRUE)
      
      #check to see what empty rows were
        apply(blext_DFD_USPS, 2, function(x) length(which(!is.na(x))))
        
  #merge dwsd data onto blext_DFD_USPS data
        blext_DFD_USPS_dwsd <- merge(x=blext_DFD_USPS, y=dwsd_final, by="Parcel.ID", all.x=TRUE)
        
      #check to see what non empty row count was
        apply(blext_DFD_USPS_dwsd, 2, function(x) length(which(!is.na(x))))        
        
      #if parcel isnt in dwsd database, then code as a 0
        blext_DFD_USPS_dwsd$dwsd.account.on.dummy <- ifelse(is.na(blext_DFD_USPS_dwsd$dwsd.account.on.dummy),0,blext_DFD_USPS_dwsd$dwsd.account.on.dummy)
        blext_DFD_USPS_dwsd[14:26][is.na(blext_DFD_USPS_dwsd[14:26])] <- 0  
        
        
  #merge on QVF (voting records) data
    blext_DFD_USPS_dwsd_qvf <- merge(x=blext_DFD_USPS_dwsd, y=most_recent_qvf, by="Parcel.ID", all.x=TRUE)
    #add voting records dummy
      blext_DFD_USPS_dwsd_qvf$voting.records.dummy <- ifelse(!is.na(blext_DFD_USPS_dwsd_qvf$date_reg),1,0)
      
  #merge on DTE data
      blext_DFD_USPS_dwsd_qvf_dte <- merge(x=blext_DFD_USPS_dwsd_qvf, y=dte_final, by="Parcel.ID", all.x=TRUE)
      #fill in gas and electric dummy vars
        blext_DFD_USPS_dwsd_qvf_dte$Gas.Dummy <- ifelse(is.na(blext_DFD_USPS_dwsd_qvf_dte$Gas.Dummy),0,blext_DFD_USPS_dwsd_qvf_dte$Gas.Dummy)
        blext_DFD_USPS_dwsd_qvf_dte$Electric.Dummy <- ifelse(is.na(blext_DFD_USPS_dwsd_qvf_dte$Electric.Dummy),0,blext_DFD_USPS_dwsd_qvf_dte$Electric.Dummy)      
          
      #only keep variables we want
        my_vars<-c("Parcel.ID", "Account.Name.x", "MCM.Condition", "MCM.Occupancy.Dummy",
                    "Fire.Dummy", "USPS.Occupancy.Dummy", "voting.records.dummy", "Gas.Dummy", "Electric.Dummy", "dwsd.account.on.dummy", "DECEMBER", "NOVEMBER", "OCTOBER", "SEPTEMBER", "AUGUST", "JULY", "JUNE", "MAY",
                    "APRIL", "MARCH", "FEBRUARY", "JANUARY", "year_total")
        
        regression_input <- blext_DFD_USPS_dwsd_qvf_dte[my_vars]
        
        #for NAs in fire dummy, code them as zero
          regression_input$Fire.Dummy <- ifelse(is.na(regression_input$Fire.Dummy),0,regression_input$Fire.Dummy)
          
        #for NAs in USPS occupancy, code them as zero
          regression_input$USPS.Occupancy.Dummy <- ifelse(is.na(regression_input$USPS.Occupancy.Dummy),0,regression_input$USPS.Occupancy.Dummy)
          

        #clear everything we dont need from memory
          #objects_to_keep <- c("regression_input", "DFD_USPS", "dwsd_final", "most_recent_qvf", "dte_final")
          #objects_to_remove <- ls()[-which(ls() %in% objects_to_keep)]
          #rm(list=objects_to_remove)
        
###FINISHED MERGING DATA TOGETHER### 
        
        
        
###RUN PREDICTIVE MODELS ON FINAL MCM GTJ QVF DWSD DATA###
    ###run logit model based on MCM attributes
        
    #split data up into train and test datasets
      Train <- createDataPartition(regression_input$MCM.Occupancy.Dummy, p=0.75, list=FALSE)
      occupyTrain <- regression_input[Train,]
      occupyTest <- regression_input[-Train,]
        
        #model w no MCM as dependent
        model_logit<-glm(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                           year_total, data=occupyTrain, family=binomial(link='logit'))               
        
        #fitting the results to the test data
        occupyTest$pr.logit <- predict(model_logit, newdata=occupyTest, type='response')
        fitted.results <- predict(model_logit, newdata=occupyTest, type='response')
        
        
        
        #determine MSE of test data
        MSE.logit <- sum((occupyTest$pr.logit - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)   
        
        
        #'''
        #MSE.logit
        #[1] 0.0616963
        #'''
        
        #model w no MCM as dependent & with months as dependent
        model_logit_months<-glm(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                           DECEMBER+NOVEMBER+OCTOBER+SEPTEMBER+AUGUST+JULY+JUNE+MAY+APRIL+MARCH+FEBRUARY+JANUARY, 
                         data=occupyTrain, family=binomial(link='logit'))               
        
        #fitting the results to the test data
        occupyTest$pr.logit.months <- predict(model_logit_months, newdata=occupyTest, type='response')
        fitted.results.months <- predict(model_logit_months, newdata=occupyTest, type='response')
        
        
        
        #determine MSE of test data
        MSE.logit.months <- sum((occupyTest$pr.logit.months - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)        
        
        #'''
        #MSE.logit.months
        #[1] 0.06151882
        #'''        
        
  #run tree classification model
      model_tree <- rpart(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                                   year_total, data=occupyTrain)
      
      printcp(model_tree) # display the results 
      plotcp(model_tree) # visualize cross-validation results 
      summary(model_tree) # detailed summary of splits
      
      # plot tree 
      plot(model_tree, uniform=TRUE, 
           main="Regression Tree for MCM.Occupancy ")
      text(model_tree, use.n=TRUE, all=TRUE, cex=.8)
      
      # create attractive postscript plot of tree 
      post(model_tree, file = "tree.ps", 
           title = "Classification Tree for MCM.Occupancy (Year Total)")
      
      #fitting the results to the test data
      occupyTest$pr.tree <- predict(model_tree, newdata=occupyTest)
      fitted.results.tree <- predict(model_tree, newdata=occupyTest)
      
      
      
      #determine MSE of test data
      MSE.tree <- sum((occupyTest$pr.tree - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)   
      
      #'''
      #MSE.tree
      #[1] 0.0643091
      #'''
      #'      
        
  #run tree classification model with months as input
      model_tree_months <- rpart(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                                                  DECEMBER+NOVEMBER+OCTOBER+SEPTEMBER+AUGUST+JULY+JUNE+MAY+APRIL+MARCH+FEBRUARY+JANUARY, data=occupyTrain)
      
      
      printcp(model_tree_months) # display the results 
      plotcp(model_tree_months) # visualize cross-validation results 
      summary(model_tree_months) # detailed summary of splits
      
      # plot tree 
      plot(model_tree_months, uniform=TRUE, 
           main="Regression Tree for MCM.Occupancy ")
      text(model_tree_months, use.n=TRUE, all=TRUE, cex=.8)
      
      # create attractive postscript plot of tree 
      post(model_tree_months, file = "tree_months.ps", 
           title = "Classification Tree for MCM.Occupancy (Months)")
      
      #fitting the results to the test data
      occupyTest$pr.tree.months <- predict(model_tree_months, newdata=occupyTest)
      fitted.results.tree.months <- predict(model_tree_months, newdata=occupyTest)
      
      
      
      #determine MSE of test data
      MSE.tree.months <- sum((occupyTest$pr.tree.months - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)   
      
      #'''
      #MSE.tree.months
      #[1] 0.06195036
      #'''
      #'
      #'

  #run random forest model
      model_randomForest <- randomForest(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                                           year_total, data=occupyTrain)
      
      plot(model_randomForest, log="y")
      varImpPlot(model_randomForest)
      
      
      #fitting the results to the test data
      occupyTest$pr.rForest <- predict(model_randomForest, newdata=occupyTest, type='response')
      fitted.results.rForest <- predict(model_randomForest, newdata=occupyTest, type='response')
      
      
      
      #determine MSE of test data
      MSE.rForest <- sum((occupyTest$pr.rForest - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)   
      
      #'''
      #MSE.rForest
      #[1] 0.05884159
      #'''
      #'
  #run random forest model with DWSD monthly usage as input
      model_randomForest_months <- randomForest(MCM.Occupancy.Dummy~Fire.Dummy+USPS.Occupancy.Dummy+voting.records.dummy+Gas.Dummy+Electric.Dummy+dwsd.account.on.dummy+
                                                  DECEMBER+NOVEMBER+OCTOBER+SEPTEMBER+AUGUST+JULY+JUNE+MAY+APRIL+MARCH+FEBRUARY+JANUARY, data=occupyTrain)
      
      
      plot(model_randomForest_months, log="y")
      varImpPlot(model_randomForest_months)
      
      #fitting the results to the test data
      occupyTest$pr.rForest.months <- predict(model_randomForest_months, newdata=occupyTest, type='response')
      fitted.results.rForest.months <- predict(model_randomForest_months, newdata=occupyTest, type='response')
      
      
      
      #determine MSE of test data
      MSE.rForest.months <- sum((occupyTest$pr.rForest.months - occupyTest$MCM.Occupancy.Dummy)^2)/nrow(occupyTest)   
      
      #'''
      #MSE.rForest.months
      #[1] 0.05819327
      #'''
      #'

      
      
      #finding "optimal" cut point of the model
      pred <- prediction(occupyTest$pr.rForest.months, occupyTest$MCM.Occupancy.Dummy)
      perf <- performance(pred, "tpr", "fpr")
      plot(perf)
      abline(a=0, b=1)
      
      opt.cut = function(perf, pred){
        cut.ind = mapply(FUN=function(x, y, p){
          d = (x - 0)^2 + (y-1)^2
          ind = which(d == min(d))
          c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
            cutoff = p[[ind]])
        }, perf@x.values, perf@y.values, pred@cutoffs)
      }
      print(opt.cut(perf, pred))
      occupancy_threshold <- opt.cut(perf,pred)[3]
      
      #measuring true positive / false positive rates
        occupyTest$rForest.Occupancy.6 <- ifelse(occupyTest$pr.rForest.months>0.6,1,0)
        tpr.6 <- sum(occupyTest$MCM.Occupancy.Dummy==1 & occupyTest$rForest.Occupancy.6==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==1))
        fpr.6 <- sum(occupyTest$MCM.Occupancy.Dummy==0 & occupyTest$rForest.Occupancy.6==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==0))
        occupyTest$rForest.Occupancy.max <- ifelse(occupyTest$pr.rForest.months>occupancy_threshold,1,0)
        tpr.max <- sum(occupyTest$MCM.Occupancy.Dummy==1 & occupyTest$rForest.Occupancy.max==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==1))
        fpr.max <- sum(occupyTest$MCM.Occupancy.Dummy==0 & occupyTest$rForest.Occupancy.max==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==0))        
        occupyTest$rForest.Occupancy.9 <- ifelse(occupyTest$pr.rForest.months>0.9,1,0)
        tpr.9 <- sum(occupyTest$MCM.Occupancy.Dummy==1 & occupyTest$rForest.Occupancy.9==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==1))
        fpr.9 <- sum(occupyTest$MCM.Occupancy.Dummy==0 & occupyTest$rForest.Occupancy.9==1)/
          (sum(occupyTest$MCM.Occupancy.Dummy==0))        
        