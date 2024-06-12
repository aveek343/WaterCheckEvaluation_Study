# Run this script (1_2_Second_and_Later_run_DataHandler.R) instead of 1_DataHandler.R once the database is created for the second and any later runs.
# 01/27/2023
# Author: Camilo B.

rm(list = ls()) # remove everything

library(remotes)
library(HSClientR)
library(tidyverse)
library(DBI)
options(timeout=100)

# Load urls.csv 
# This file contains the urls of all the files we need.
# It was created using an R client to interact with HydroShare API
# The HydroShare client is available on https://github.com/program--/HSClientR
# The file is included here to make sure this resource runs independently.

# There is no need to run this section of the code
# Get urls for the files needed
# HS Resource: http://www.hydroshare.org/resource/fe0377e960b741c4a52dc6ea49db7d80
HydroShare_ResourceID <- "fe0377e960b741c4a52dc6ea49db7d80"
AllFiles <- hs_files(HydroShare_ResourceID)$results # All files included in the HS resource
AllFiles %>%
  filter(str_detect(url, 'Database_CSVFiles')) %>% # select the files needed to create the database
  write.csv('urls_USU_WaterConservation.csv') # create the CSV file with all urls

# Start running here

# Database creation
dir.create(file.path('Database')) # create folder
con <- dbConnect(RSQLite::SQLite(), "./Database/ProjectDatabase_RawData.db") # create database

# Create database tables
# Sites
dbSendQuery(conn = con,
            "CREATE TABLE IF NOT EXISTS Sites
            (SiteID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            City VARCHAR(50) NOT NULL,
            State VARCHAR(50) NOT NULL,
            ZipCode INTEGER NOT NULL)")

# Flume property data
dbSendQuery(conn = con,
            "CREATE TABLE IF NOT EXISTS FlumePropertyData
            (SiteID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            BuildingType VARCHAR(50),
            NumberOfBeds DOUBLE,
            NumberOfBaths DOUBLE,
            PrimaryIrrigationMethod VARCHAR,
            Pool VARCHAR(50),
            LotSize DOUBLE,
            HomeSize DOUBLE,
            EstimatedValue DOUBLE,
            YearBuilt INTEGER)")

# WaterCheckData Table
dbSendQuery(conn = con,
            "CREATE TABLE IF NOT EXISTS WaterCheckData
            (WaterCheckUSUID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            WaterCheckDate DATETIME NOT NULL,
            SummerOccupants INTEGER,
            WinterOccupants INTEGER,
            OwnsRents VARCHAR(50),
            TimeLivedatResidence VARCAHR,
            WaterCheckHowHeard VARCAHR,
            WaterCheckWhyDone VARCAHR,
            ParcelAreaft2 INTEGER,
            TurfAreaft2 INTEGER,
            OtherIrrAreaft2 INTEGER,
            PNIAreaft2 INTEGER,
            HardscapeAreaft2 INTEGER,
            TotalIrrAreaft2 INTEGER,
            PropertyParcelID INTEGER,
            PropertyZip INTEGER,
            CatchCupTestDU_Percent VARCHAR,
            CatchCupTestPrecipRate_inhr VARCHAR,
            SiteID INTEGER,
            IrrigationActionItems_NChecks INTEGER,
            IrrigationActionItemsBrokenHead INTEGER,
            IrrigationActionItemsBrokenNozzle INTEGER,
            IrrigationActionItemsBrokenValve INTEGER,
            IrrigationActionItemsBrokenPipe INTEGER,
            IrrigationActionItemsClog INTEGER,
            IrrigationActionItemsCoverageIssues INTEGER,
            IrrigationActionItemsLowHeadDrainage INTEGER,
            IrrigationActionItemsMismatchedHeadTypes  INTEGER,
            IrrigationActionItemsMisalignedHeads INTEGER,
            IrrigationActionItemsBlockedHeads INTEGER,
            IrrigationActionItemsWrongSprayPattern INTEGER,
            IrrigationActionItemsOverspray INTEGER,
            IrrigationActionItemsSunkenHeads INTEGER,
            IrrigationActionItemsTiltedHeads INTEGER,
            LandscapeActionItems_NChecks INTEGER,
            LandscapeActionItemsDrySpots INTEGER,
            LandscapeActionItemsMulchNeeded INTEGER,
            LandscapeActionItemsMismatchedPlantTypes INTEGER,
            LandscapeActionItemsPonding INTEGER,
            LandscapeActionItemsCompaction INTEGER,
            LandscapeActionItemsThatch INTEGER,
            FOREIGN KEY (SiteID) REFERENCES Sites (SiteID) ON DELETE NO ACTION ON UPDATE NO ACTION)")

# Water use table
dbSendQuery(conn = con,
            "CREATE TABLE IF NOT EXISTS WaterUse
            (WaterUseID INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
            Datetime VARCHAR(250) NOT NULL,
            VolumeGal DOUBLE,
            SiteID INTEGER NOT NULL,
            FOREIGN KEY (SiteID) REFERENCES Sites (SiteID) ON DELETE NO ACTION ON UPDATE NO ACTION)")
# # _______________________________
# End of database table creation

# List tables
dbListTables(con)

# Load files urls - from HS
files <- read_csv('urls_USU_WaterConservation.csv') # this file has all the information about the data files we need

# Load training data
files %>%
  filter(str_detect(url, 'TrainingData.csv')) %>%
  pull(url) %>% # select the url
  read_csv() %>%
  mutate(label = factor(label, levels = c('irrigation', 'other'))) -> td # training data

# Weather data
files %>%
  filter(str_detect(url, 'daily_WeatherData_GVFarm.csv')) %>%
  pull(url) %>% # select the url
  read_csv() %>%
  filter(station == 'GV_Farm') -> daily_wd # training data











