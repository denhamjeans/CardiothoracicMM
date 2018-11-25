##Purpose: PowerPoint M&M Automation Script for PoW Cardiothoracics Dept
##Author: Rory Denham
##Date Created: 24/11/2018

#Load Libraries ------------------------
library(officer)
library(magrittr)
library(tidyr)
library(ggplot2)


#Presentation setup ---------------------------------------------------

# Text for Slides ----------------------

# Intro summary slide
intro<-"Earlier this year we talked about how limited housing supply was helping to drive accelerating house prices across the country. In such an environment you would expect to see housing vacancies decline. Indeed, if you look at the rate of rental or homeowner vacancies you see a substantial reduction. But if we look a little closer at the housing inventory data something curious emerges."

# Set captions -------------------------

#Date caption for title
title.date<-paste(month1, " and ", month2, year)

cap1<-"During the Great Recession homeowner vacancy rates spiked, and gradually came back down. Rental vacancy rates did not spike nearly as much, but also came down in recent years as housing markets have gotten pretty tight."
cap2<-"The top two panels show the vacant for rent vacant for sale units that make up the rental and homeowner vacancy rates. The bottom right panel shows year-round vacant units which have been rented or sold but the new renters or owners have not moved in yet. That has a pretty clear seasonal pattern, matching the rhythm of the U.S. housing market, but remains constant at a little under one percent. The bottom left panel shows the share of housing units that are vacant and held off the market."
cap3<-"The year-round vacant other category has increased almost a full percentage point since 2005. Just to be clear, that's a lot of housing units. A one percentage point increase corresponds to over one million housing units. The largest component, taking up about a quarter are those units vacant due to personal/family reasons. This includes situations where the owner is in assisted living and not occupying the unit."
cap4<-"The U.S. Census Bureau began tracking a breakdown of the other category since 2012. This chart shows the breakdown of the percent distribution for the second quarter of 2017."
cap5<-"The share vacant due to foreclosure has declined quite a lot since 2012 when the U.S. housing market was still early in recovery. Those foreclosed housing units have largely moved through the system."
myftr<-"RoryDenham R to PowerPoint"


# Build the Deck -----------------------

read_pptx("Powerpoint_Templates/blank.pptx") %>%
  # Title Slide
  # add AIHW presentation ideas for title to make the slides look clean
  add_slide(layout="Title Slide", master="Office Theme") %>%
  ph_with_text(type = "ctrTitle", str = "POWH Cardiac Surgery Data Review & Morbidity and Mortality") %>% 
  ph_with_text(type = "subTitle", str = title.date) %>% 
  ph_with_text(type = "ftr", str = myftr ) %>%
  # Summary Slide:
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with_text(type = "title", index=1,str = "Summary") %>%
  ph_with_text(type="body",str = intro ) %>% 
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "1" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  # Slide with Chart 1:
  add_slide(layout = "Content with Caption", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap1) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart1.png") %>%
  ph_with_text(type = "title", index=1,str = "Homeowner and rental vacancy rates have declined") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "2" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  # Slide with Chart 2:
  add_slide(layout = "Content with Caption", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap2) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart2.png") %>%
  ph_with_text(type = "title", index=1,str = "More homes held off market") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "3" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  # Slide with Chart 3:
  add_slide(layout = "Content with Caption", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap3) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart3.png") %>%
  ph_with_text(type = "title", index=1,str = "Growth comes from other category") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "4" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  # Slide with Chart 4:
  add_slide(layout = "Content with Caption", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap4) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart4.png") %>%
  ph_with_text(type = "title", index=1,str = "Breakdown of the other category") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "5" ) %>%
  ph_with_text(type = "dt", str = format(Sys.Date(),"%B %d,%Y")) %>%
  # Slide with Chart 5:
  add_slide(layout = "Content with Caption", master = "Office Theme") %>%
  ph_with_text(type = "body", index=2,str = cap5) %>% 
  ph_with_img(type = "body", index = 1, src = "img/chart5.png") %>%
  ph_with_text(type = "title", index=1,str = "Distribution of other category over time") %>%
  ph_with_text(type = "ftr", str = myftr ) %>%
  ph_with_text(type = "sldNum", str = "6" ) %>%
  ph_with_text(type = "dt", str =format(Sys.Date(),"%B %d,%Y")) %>%
  # Print to save powerpoint
  print( target = "Housing Vacancy Blog Post.pptx") %>% 
  invisible()
