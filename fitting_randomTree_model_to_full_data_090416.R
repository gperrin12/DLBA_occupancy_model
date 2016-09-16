#CREATED: GTP 8-23-16
#UPDATED: GTP 9-4-16
#take the randomForest model created in "MCM_DFD_H2O_combine_082316 and 
#apply that model to all parcels in the city

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
install.packages("reshape2")
install.packages("munsell")
install.packages("data.table")
install.packages("randomForest")
install.packages("rpart")
install.packages("XLConnect")
library("gmodels")
library("lubridate")
library("plyr")
library("caret")
library("pscl")
library("ROCR")
library("reshape2")
library("data.table")
library("randomForest")
library("rpart")
library("XLConnect")

###DATA INPUT###

  # Increase before package loading
  options(java.parameters = "-Xmx4000m")

  #read in list of all detroit parcels
    parcel_raw <- readWorksheet(loadWorkbook("DetroitParcels_SFAccounts_20160823v2.xlsx"),sheet=1)
    #parcel_raw <- read.xlsx("DetroitParcels_SFAccounts_20160823v2.xlsx", 1)
    #subset to residential non-lot data
      parcel<-parcel_raw[which(parcel_raw$Zoning.Type=="Residential"),]
      parcel<-parcel[which(parcel$Property.Class=="Structure"),]
      
      
      
###END DATA INPUT###
      
      
###merge data sets together
      
  #merge parcel list and DFD_USPS together
      parcel_DFD_USPS <- merge(x=parcel, y=DFD_USPS, by="Parcel.ID", all.x=TRUE)
      
      #check to see what empty rows were
        apply(parcel_DFD_USPS, 2, function(x) length(which(!is.na(x))))
        
  #merge dwsd data onto parcel_DFD_USPS data
        parcel_DFD_USPS_dwsd <- merge(x=parcel_DFD_USPS, y=dwsd_final, by="Parcel.ID", all.x=TRUE)
        
      #check to see what non empty row count was
        apply(parcel_DFD_USPS_dwsd, 2, function(x) length(which(!is.na(x))))        
        
      #if parcel isnt in dwsd database, then code as a 0
        parcel_DFD_USPS_dwsd$dwsd.account.on.dummy <- ifelse(is.na(parcel_DFD_USPS_dwsd$dwsd.account.on.dummy),0,parcel_DFD_USPS_dwsd$dwsd.account.on.dummy)

        parcel_DFD_USPS_dwsd[15:27][is.na(parcel_DFD_USPS_dwsd[15:27])] <- 0  
        
        
  #merge on QVF (voting records) data
    parcel_DFD_USPS_dwsd_qvf <- merge(x=parcel_DFD_USPS_dwsd, y=most_recent_qvf, by="Parcel.ID", all.x=TRUE)
    #add voting records dummy
    parcel_DFD_USPS_dwsd_qvf$voting.records.dummy <- ifelse(!is.na(parcel_DFD_USPS_dwsd_qvf$date_reg),1,0)
    
    #merge on DTE data
    parcel_DFD_USPS_dwsd_qvf_dte <- merge(x=parcel_DFD_USPS_dwsd_qvf, y=dte_final, by="Parcel.ID", all.x=TRUE)
    #fill in gas and electric dummy vars
      parcel_DFD_USPS_dwsd_qvf_dte$Gas.Dummy <- ifelse(is.na(parcel_DFD_USPS_dwsd_qvf_dte$Gas.Dummy),0,parcel_DFD_USPS_dwsd_qvf_dte$Gas.Dummy)
      parcel_DFD_USPS_dwsd_qvf_dte$Electric.Dummy <- ifelse(is.na(parcel_DFD_USPS_dwsd_qvf_dte$Electric.Dummy),0,parcel_DFD_USPS_dwsd_qvf_dte$Electric.Dummy)      
          
      #only keep variables we want
        my_vars2<-c("Parcel.ID", "Account.ID", "Account.Name.x", "Property.Ownership", "Zoning.Type", 
                    "Fire.Dummy", "USPS.Occupancy.Dummy", "voting.records.dummy", "Gas.Dummy", "Electric.Dummy",
                    "dwsd.account.on.dummy", "DECEMBER", "NOVEMBER", "OCTOBER", "SEPTEMBER", "AUGUST", "JULY", "JUNE", "MAY",
                    "APRIL", "MARCH", "FEBRUARY", "JANUARY", "year_total")
        
        prediction_input <- parcel_DFD_USPS_dwsd_qvf_dte[my_vars2]
        
        #for NAs in fire dummy, code them as zero
          prediction_input$Fire.Dummy <- ifelse(is.na(prediction_input$Fire.Dummy),0,prediction_input$Fire.Dummy)
          
        #for NAs in USPS occupancy, code them as zero
          prediction_input$USPS.Occupancy.Dummy <- ifelse(is.na(prediction_input$USPS.Occupancy.Dummy),0,prediction_input$USPS.Occupancy.Dummy)
          
###end data merge
          

###add prediction to the full dataset
        final_detroit <- prediction_input
        final_detroit$pr.rForest.months <- predict(model_randomForest_months, newdata=prediction_input, type='response')
        
        #threshold determined by finding "optimal" cut point of the model in the combine program
          final_detroit$occupy_prediction <- ifelse(final_detroit$pr.rForest.months>occupancy_threshold, "Occupied", "Not Occupied")
        
        #checking to see if this property that kevin shelton passes the test: it does!
          final_detroit_check <- final_detroit[grep('743 Virginia', final_detroit$Account.Name.x),]
          
        #historgram
          hist(final_detroit$pr.rForest.months, breaks=100)
          
          
        #export data to csv
          file_name <- paste("final_occupancy_output_",Sys.Date(),".csv", sep="")
          write.csv(final_detroit, file = file_name) 
