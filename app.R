# install.packages("dplyr")
# install.packages("DT")
# install.packages("highcharter")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyr")
# install.packages("stringr")
library(dplyr)
library(DT)
library(highcharter)
library(shiny)
library(shinydashboard)
library(tidyr)
library(stringr)
#Manapulate data

test <- read.csv("customer_data.csv",head=TRUE) 

cus_NA <- test[test$customer_id == "#N/A",]
cus_id_NA <- test[test$customer_id != "#N/A" & test$acq_date == "#N/A",]
cus_id <- test[test$customer_id != "#N/A" & test$acq_date != "#N/A",]
cus_id$acq_date <- as.Date(cus_id$acq_date)
cus_id$sub_date <- as.Date(cus_id$sub_date)

#solve ID
cus_id$profit <- cus_id$revenue - cus_id$cost
cus_id$count <- 1
id1 <- data.frame(summarize(group_by(cus_id), country = "Total",prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id2 <- data.frame(summarize(group_by(cus_id, country),prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id3 <- data.frame(summarize(group_by(cus_id, country,prd_type), mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id4 <- data.frame(summarize(group_by(cus_id, country,prd_type, mrk_channel), revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id5 <- data.frame(summarize(group_by(cus_id, mrk_channel), country = "Total",prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id6 <- data.frame(summarize(group_by(cus_id, mrk_channel, country),prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id7 <- data.frame(summarize(group_by(cus_id, mrk_channel,prd_type) , country= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id8 <- data.frame(summarize(group_by(cus_id,prd_type), country = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id <- rbind(id1, id2, id3, id4, id5, id6, id7, id8)

#solve ID, NA
cus_id_NA$profit <- cus_id_NA$revenue - cus_id_NA$cost
cus_id_NA$count <- 1
id_NA1 <- data.frame(summarize(group_by(cus_id_NA), country = "Total",prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA2 <- data.frame(summarize(group_by(cus_id_NA, country),prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA3 <- data.frame(summarize(group_by(cus_id_NA, country,prd_type), mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA4 <- data.frame(summarize(group_by(cus_id_NA, country,prd_type, mrk_channel), revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA5 <- data.frame(summarize(group_by(cus_id_NA, mrk_channel), country = "Total",prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA6 <- data.frame(summarize(group_by(cus_id_NA, mrk_channel, country),prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA7 <- data.frame(summarize(group_by(cus_id_NA, mrk_channel,prd_type) , country= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA8 <- data.frame(summarize(group_by(cus_id_NA,prd_type), country = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
id_NA <- rbind(id_NA1, id_NA2, id_NA3, id_NA4, id_NA5, id_NA6, id_NA7, id_NA8)

#solve NA:
cus_NA$profit <- cus_NA$revenue - cus_NA$cost
cus_NA$count <- 1
na1 <- data.frame(summarize(group_by(cus_NA), country = "Total",prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na2 <- data.frame(summarize(group_by(cus_NA, country),prd_type = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na3 <- data.frame(summarize(group_by(cus_NA, country,prd_type), mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na4 <- data.frame(summarize(group_by(cus_NA, country,prd_type, mrk_channel), revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na5 <- data.frame(summarize(group_by(cus_NA, mrk_channel), country = "Total",prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na6 <- data.frame(summarize(group_by(cus_NA, mrk_channel, country),prd_type = "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na7 <- data.frame(summarize(group_by(cus_NA, mrk_channel,prd_type) , country= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na8 <- data.frame(summarize(group_by(cus_NA,prd_type), country = "Total", mrk_channel= "Total", revenue = sum(revenue,na.rm=TRUE), cost = sum(cost,na.rm=TRUE), profit = sum(profit,na.rm=TRUE), count = sum(count,na.rm=TRUE)))
na <- rbind(na1, na2, na3, na4, na5, na6, na7, na8)

#cohort
cus_id$datediff <- as.numeric(as.Date(cus_id$sub_date)- as.Date(cus_id$acq_date))
cus_id$month <-  round(case_when(cus_id$datediff > 28 & cus_id$datediff < 59 ~ 1, cus_id$datediff > 59 & cus_id$datediff < 89 ~ 2, TRUE ~ cus_id$datediff/30),0)
cus_id$acq_month <- format(as.Date(cus_id$acq_date),"%Y-%m")
co1 <- data.frame(summarise(group_by(cus_id, acq_month, month), country = "Total",prd_type = "Total", mrk_channel= "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co2 <- data.frame(summarise(group_by(cus_id, acq_month, month, country),prd_type = "Total", mrk_channel= "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co3 <- data.frame(summarise(group_by(cus_id, acq_month, month, country,prd_type), mrk_channel= "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co4 <- data.frame(summarise(group_by(cus_id, acq_month, month,prd_type , mrk_channel), country = "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co5 <- data.frame(summarise(group_by(cus_id, acq_month, month,prd_type), country = "Total", mrk_channel= "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co6 <- data.frame(summarise(group_by(cus_id, acq_month, month, mrk_channel), country = "Total",prd_type = "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co7 <- data.frame(summarise(group_by(cus_id, acq_month, month, mrk_channel, country),prd_type = "Total",count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co8 <- data.frame(summarise(group_by(cus_id, acq_month, month, country, prd_type,mrk_channel),count= sum(count,na.rm=TRUE),revenue=sum(revenue,na.rm=TRUE), cost=sum(cost,na.rm=TRUE), profit =sum(profit,na.rm=TRUE)))
co <- rbind(co1, co2, co3, co4, co5, co6, co7, co8)


#app.R UI
ui <- dashboardPage(
  dashboardHeader(title = "Customer Analysis", titleWidth = 180),
  #side bar with list of selections 
  dashboardSidebar(width = 180,
                   uiOutput("typ"),
                   sidebarMenu(id ="tab",
                               hr(),
                               menuItem("About Data", tabName = "about"),
                               menuItem("Customer Group 1", tabName = "cus_id"),
                               menuItem("Cohort Analysis", tabName = "cohort"),
                               menuItem("Customer Group 2", tabName = "cus_id_na"),
                               menuItem("Customer Group 3", tabName = "cus_na"),
                               menuItem("Conclusion & Request", tabName = "more")
                   )
                   ),
  
  dashboardBody(
    tags$head(tags$style(
      HTML('.content {
           font-size: 0.9em;
           }
           #.skin-blue .main-header .logo {
           #                     background-color: blue;
           #}
           #.skin-blue .main-header .navbar {
           #                     background-color: blue;
           #                     }
           ')
      )),
    tabItems(
      tabItem("about",
              fluidPage(
                HTML('<p>
                     <mark><label>ABOUT DATA </label></mark><br>
                     <p><label>I would like to take some notes before we go further with visualization and analysis</label></p>
                     <ul><li>The file of dataset has been cleaned and named "test_kim_ung.csv"</li></ul>
                     <ul><li> Customer ID: To understand more about customer behavior, customers have been separated into other groups: </li></ul>
                     <ul><ul><li> There exists 406 unknown customer ID (row numbers) and no information about acquisition & subscription date --> I set this group with name: Customer Group 3</li></ul></ul>
                     <ul><ul><li> 73 customers have ID, but no information about acquisition & subscription date --> they are in Customer Group 2</li></ul></ul>
                     <ul><ul><li> 6900 customers have ID, and acquisition & subscription date information belong to Customer Group 1</li></ul></ul>
                     <ul><ul><li> Cohort Analysis only set up for Customer Group 1 with full information</li></ul></ul>
                     <ul><li>Below are the information of some columns:</li></ul>
                     <ul><ul><li> customer_id: Customer ID</li></ul></ul>
                     <ul><ul><li> prd_type: created based on Product Type, including: product_1a (price 79.00), product_1b (89.00), product_3 (99.00), product_4 (39.00)</li></ul></ul>
                     <ul><ul><li> mrk_channel: Marketing Channel</li></ul></ul>
                     <ul><ul><li> acq_date: Acquisition Date</li></ul></ul>
                     <ul><ul><li> sub_date: Subscription Date</li></ul></ul>
                     <ul><ul><li> profit: Revenue - Cost</li></ul></ul>
                     <ul><ul><li> count: number of users based on acquisition date, subscription date, product type, marketing channel</li></ul></ul>
                     <ul><ul><li> month: [0,14]: for cohort analysis. For example,  (Month) 0 means current month that customer registered, (Month) 1 means 1 month later acquisition date,.</li></ul></ul>
                     </p>
                     '))
                ),
      tabItem("cus_id",
    fluidRow(
      box(width = 6,
          uiOutput("mrk_chan1"),
          title = "Country - Product Type", status = "primary", solidHeader = TRUE,
          DT::dataTableOutput("copr1")
      ),
      box(width = 6,
          uiOutput("prod_typ1"),
          title = "Country - Maketing Channel", status = "primary", solidHeader = TRUE,
          DT::dataTableOutput("comc1")
      )
    ),
    fluidRow(
      box(width = 6 ,uiOutput("countr1"),
          title = "Marketing Channel- Product Type", status = "primary", solidHeader = TRUE,
          DT::dataTableOutput("mcpr1")
      ),
      box(width = 6,
          title = "Comment", status = "primary", solidHeader = TRUE,
          HTML('<p>
                     <ul><li>In general, country 1 has better performance than country 2</li></ul>
<mark><label>Number of users: </label></mark><br>
               <ul><li>Country 1 with 63,3% almost double country 2  with 36,7% in total</li></ul>
               <ul><li>Only product_3 (price 99.00) are registered in country 1, while only product_1a (price 79.00) is used in country 2. Although product_1b (price 89.00) surpasses others in the total number of users, Product_4 (price 39.00) has the biggest numbers of consumption, mostly in country 1</li></ul>
               <ul><li>Marketing channel 1, channel 3 are the most effective channels, followed by channel 5.  Channel 4 and channel 2 has the lowest number of users. Country 1 is influenced by marketing more than country 2</li></ul>
               <ul><li>Marketing has a small effect on product_1a, while product_1b is active on almost channels. Channel 1, channel 3 and channel 5 is more active than others</li></ul>
               
               <mark><label>Revenue: </label></mark><br>
               <ul><li>Country 1 occupies 55.4%, country 2 is 44.6%, not much difference as numbers of users</li></ul>
               <ul><li>Product_1b have the biggest number of revenue, with 61.5% in total, double revenue of product_4 and 6 times more than product_3. Product_1a stands at the bottom of the list with only 1.9</li></ul>
               <ul><li>Channel 1, channel 3 and channel 5 are on the top of the revenue list</li></ul>
               
               <mark><label>Cost: </label></mark><br>
               <ul><li>Country 1 and country 2 almost have the same share</li></ul>
               <ul><li>Product_1b has the highest numbers with around 62%, followed by product_4 and product_3</li></ul>
               <ul><li>Channel1, channel 3, and channel 5 spend more money than others</li></ul>
               
               <mark><label>Profit: </label></mark><br>
               <ul><li>Only country 1 make profit, but cannot surpass the loss of country 2</li></ul>
               <ul><li>Product_1b, product_4 only has the positive numbers in country 1 and product_1b is only active in country 2 and makes money</li></ul>
               <ul><li>All marketing channel make profit in country 1, but loss in country 2, except channel 1</li></ul>
               <ul><li>Although spending less money on product_1a, but all marketing channel have the positive numbers</li></ul>
               <ul><li>Product_1b has the biggest loss in spite of investing huge amount of money</li></ul>
               
               
               
               </p>
               ')
      )
    )),
    tabItem("cohort", 
              tabBox(title = "Cohort Analysis" , width = 12,
                     tabPanel(width = 12,style = "overflow-y:scroll; max-height: 600px; position:relative;", title = "Percentage"
                              ,    fluidRow(
                                box( column(width = 12, fixedRow( # Width = sum of component columns 
                                  column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("countr2")))
                                  ,column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("prod_typ2")))
                                  ,column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("mrk_chan2"))))),
                                  
                                  # uiOutput("countr2"),
                                  # uiOutput("prod_typ2"),
                                  # uiOutput("mrk_chan2"),
                                  width = 6, height = 530,
                                  title = "Performance",status = "primary", solidHeader = TRUE,
                                  highchartOutput("per", height = 390)
                                ),
                                box(width = 6,
                                    title = "Comment", status = "primary", solidHeader = TRUE,
                                    HTML('<p>
<mark><label> Percentage </label></mark><br>
                     <ul><li>Although gradually decreasing, the retention rate is quite high before month 12, calculated by the acquisition date. </li></ul>
<ul><li>Number of users: from start to month 13, the average of retention is always above 50%. The retention over user life time and product time have a positive sign until the end of period. </li></ul>
<ul><li>Revenue: Fluctuate from 76% to 60% during 11 months. The retention over user lifetime and product time are quite good -> programs and events of company are quite good to keep customers </li></ul>
<ul><li>Cost: company also pays a lot of money to keep customers, the retention rate is quite high above 70% until month 11. </li></ul>
<ul><li>Profit: not good at all and fluctuation before month 8. However, from month 9, everything becomes better with the positive sign in the retention. </li></ul>
</p>
               ')
                                )
                              )
                              ,fluidRow(
                                box(width = 12,
                                    title = "Cohort Retention", status = "primary", solidHeader = TRUE,
                                    DT::dataTableOutput("core")
                                )
                              )),
                     tabPanel(width = 12,style = "overflow-y:scroll; max-height: 600px; position:relative;", title = "Absolute Value"
                              ,    fluidRow(
                                box( column(width = 12, fixedRow( # Width = sum of component columns 
                                  column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("countr4")))
                                  ,column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("prod_typ4")))
                                  ,column(4,div(style="display: inline-block;vertical-align:top; width: 100px;",uiOutput("mrk_chan4"))))),
               
                                  width = 6, height = 530,
                                  title = "Performance",status = "primary", solidHeader = TRUE,
                                  highchartOutput("per4", height = 390)
                                ),
                                box(width = 6,
                                    title = "Comment", status = "primary", solidHeader = TRUE,
                                    HTML('<p>
<mark><label> Absolute Value</label></mark><br>
<ul><li>Number of users: As opposite to percentages, absolute value the numbers of users decrease over the period. After 2- 3 months, the numbers reduce half, 4 times after 8 months, 9 times after 13-14 months</li></ul>
                                         
                                         <ul><li>Revenue: also have the same story with the number of users, fall 2 times after 6 months and 9 times after 13- 14 months </li></ul>
                                         
                                         <ul><li>Cost: this also drop over the time, and the same speed with revenue and number of users. </li></ul>
                                         <ul><li>Profit: surprisingly, profit really has better performance over the time. Especially, after month 4, everything become positive with significant numbers. It means that the good sign to continue and promote via marketing channel and develop product types</li></ul>
                                         </p>
               ')
                                )
                              )
                              ,fluidRow(
                                box(width = 12,
                                    title = "Cohort Retention", status = "primary", solidHeader = TRUE,
                                    DT::dataTableOutput("core4")
                                )
                              ))
                     )),
    
    tabItem("cus_id_na",
            fluidRow(
              box(width = 6,
                  uiOutput("mrk_chan3"),
                  title = "Country - Product Type", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("copr3")
              ),
              box(width = 6,
                  uiOutput("prod_typ3"),
                  title = "Country - Maketing Channel", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("comc3")
              )
            ),
            fluidRow(
              box(width = 6 ,uiOutput("countr3"),
                  title = "Marketing Channel- Product Type", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("mcpr3")
              ),
              box(width = 6,
                  title = "Comment", status = "primary", solidHeader = TRUE,
                  HTML('<p>
                     <ul><li>Customer group 2 does not use product_1a. This product may be the special offer for loyal customers or customer with full of data about ID, acquisition and subscription date. </li></ul>
<ul><li>Country 1 has the higher numbers than country 2</li></ul>

<mark><label>Number of users: </label></mark><br>
<ul><li>Country 1 is 61,6%, is 1.6 times more than country 2</li></ul>
<ul><li>Product_1b is more attractive than product_4 and product_3 in total, almost 2 times</li></ul>
<ul><li>Marketing channel 5 has the highest number, surpass channel 2, channel 4 around 2 times. Channel 2 stands at the bottom of the list</li></ul>
<mark><label>Revenue - Cost: </label></mark><br>
<ul><li>Product _1b consumes most of money and also has the highest revenue in comparison by others</li></ul>
<ul><li>Channel 5 and channel 1 stand on the top of 2 lists, while channel 2 has the smallest numbers. </li></ul>
<ul><li>Country 1 spends more than country 2, and also has higher numbers in revenue</li></ul>

<mark><label>Profit: </label></mark><br>
<ul><li>Except product_3, other products make money for company, mostly from product_1b</li></ul>
<ul><li>Country 1 and country 2 quite have the same share </li></ul>
<ul><li>Channel 1, 5, and 3 have the positive numbers </li></ul>

</p>
               ')
              )
            )),
    tabItem("cus_na",
            fluidRow(
              box(width = 6,
                  uiOutput("mrk_chan"),
                  title = "Country - Product Type", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("copr")
              ),
              box(width = 6,
                  uiOutput("prod_typ"),
                  title = "Country - Maketing Channel", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("comc")
              )
            ),
            fluidRow(
              box(width = 6 ,uiOutput("countr"),
                  title = "Marketing Channel- Product Type", status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("mcpr")
              ),
              box(width = 6,
                  title = "Comment", status = "primary", solidHeader = TRUE,
                  HTML('<p>
                     <ul><li>Customer Group 3 mostly focus on product_1b, product_3 in country 1</li></ul>
<ul><li>Product_1a does not exist in this group. As the same prediction with Customer Group 2, this product is a special offer for loyal customers or customer with full data of ID, acquisition & subscription date</li></ul>
<ul><li>Channel 5 is not active with Group 3. I suppose, this channel is quit private and invest in the long-term goals and loyal customers</li></ul>

<mark><label>Number of users: </label></mark><br>
<ul><li>Channel 1 is the most attractive with unknown customers</li></ul>
<ul><li>Country 1 makes up above 75%, only less than 25% in country 2</li></ul>
<ul><li>Channel 1 attracts around 72% in the number of users, followed by channel 3</li></ul>

<mark><label>Revenue - Cost: </label></mark><br>
<ul><li>Investment focus on product_1b, product_3 and mostly on Channel 1, Channel 4. Therefore, revenue from these products and channels are on the top. </li></ul>
<ul><li>Around 74% revenue and cost belong to country 1</li></ul>

<mark><label>Profit: </label></mark><br>
<ul><li>Unfortunately, company can not make money from this customer group, but not too much (-490.06) </li></ul>
<ul><li>Channel 2, 3 make loss more than channel 1 and 4</li></ul>
</p>
               ')
              )
            )),
    tabItem("more",
            fluidPage(
              HTML('<p>
                     <mark><label> Conclusions: </label></mark><br>
<ul><li>Country 1 can make profit better than country 2 and also pay attention with marketing channels  --> we can focus on promoting more marketing campaigns, channels, events  to attract more and more potential customers in country 1</li></ul>
<ul><li>Product_4 (price 39.00)  is the most favorite choice for all customer groups, especially in country 1 --> Should offer coupons, discount with product_4 for loyal customers.</li></ul>
<ul><li>Being careful when making campaigns or investing in product_1b, because this product_1b can make loss.</li></ul>
<ul><li>Channel 1, 3, 5 are quite good to attract and keep customers with company, especially channel 1. </li></ul>
<ul><li>Should more consider about product_1a,  less spending can make good profit</li></ul>

<mark><label> Additional data points to look at for better analysis and conclusions: </label></mark><br>
<ul><li>More information about customer: history purchase, customer feedback, reason for register again product type in the same months, etc. These will help to improve customer experience and demand the need of customer as much as possible. Customer is significantly essential for every company and good customer experience is a key to keep their loyalty.</li></ul>

<ul><li>Additional details about types of cost: deals/ vouchers/ coupons cost IT cost, marketing cost, trading cost, administration cost, agency cost, etc. These will support for deep-dive analysis and actuarial results. </li></ul>

<ul><li>Further details and information about revenue: why customers consume products, but no money return company, why revenue is too small (0.01, 0.02, etc), what is the average of revenue every hour, day, week, month, quarter, year over year,. These information will explore more about revenue, forecast and report about effectiveness and productivity of each products, marketing channel or revenue from loyal or big customers. </li></ul>

<ul><li>Need more information about active customers, retained customers, numbers of bought products (calculation at acquisition date and time series). These will follow both of retention over users life time and product life time. </li></ul>

<ul><li>More dataset about customers, products, revenue, cost, etc. will have better analysis with the long time series to analyze year over year, predict future trend and prepare for seasonal campaign, events as much as possible. </li></ul>
<mark><label>Thank you so much for your attention. If you have any questions, feel free to contact me:  kiung711.de@gmail.com </label></mark><br>
</p>
                     '))
    )
    )
    ))

#app.R SERVER
server <- function(input, output,session) {
  output$typ <- renderUI({selectInput("ty"  , "Choose KPI tracking: ",choices= c("Number of users","Revenue", "Cost", "Profit"),selected = TRUE, width = "12em")})
  output$countr <- renderUI({selectInput("cou"  , "Country: ",choices= unique(na$country),selected = TRUE, width = "12em")})
  output$prod_typ <- renderUI({selectInput("prd"  , "Product Type: ",choices= unique(na$prd_type),selected = TRUE, width = "12em")})
  output$mrk_chan <- renderUI({selectInput("mrc"  , "Marketing Channel: ",choices= unique(na$mrk_channel),selected = TRUE, width = "12em")})
  
  output$countr1 <- renderUI({selectInput("cou1"  , "Country: ",choices= unique(id$country),selected = TRUE, width = "12em")})
  output$prod_typ1 <- renderUI({selectInput("prd1"  , "Product Type: ",choices= unique(id$prd_type),selected = TRUE, width = "12em")})
  output$mrk_chan1 <- renderUI({selectInput("mrc1"  , "Marketing Channel: ",choices= unique(id$mrk_channel),selected = TRUE, width = "12em")})
  
  output$countr2 <- renderUI({selectInput("cou2"  , "Country: ",choices= unique(co$country),selected = TRUE, width = "12em")})
  output$prod_typ2 <- renderUI({selectInput("prd2"  , "Product Type: ",choices= unique(dtcou()$prd_type),selected = TRUE, width = "12em")})
  output$mrk_chan2 <- renderUI({selectInput("mrc2"  , "Marketing Channel: ",choices= unique(dtpr()$mrk_channel),selected = TRUE, width = "12em")})
  
  output$countr3 <- renderUI({selectInput("cou3"  , "Country: ",choices= unique(id_NA$country),selected = TRUE, width = "12em")})
  output$prod_typ3 <- renderUI({selectInput("prd3"  , "Product Type: ",choices= unique(id_NA$prd_type),selected = TRUE, width = "12em")})
  output$mrk_chan3 <- renderUI({selectInput("mrc3"  , "Marketing Channel: ",choices= unique(id_NA$mrk_channel),selected = TRUE, width = "12em")})
 
  output$countr4 <- renderUI({selectInput("cou4"  , "Country: ",choices= unique(co$country),selected = TRUE, width = "14em")})
  output$prod_typ4 <- renderUI({selectInput("prd4"  , "Product Type: ",choices= unique(dtcou4()$prd_type),selected = TRUE, width = "14em")})
  output$mrk_chan4 <- renderUI({selectInput("mrc4"  , "Marketing Channel: ",choices= unique(dtpr4()$mrk_channel),selected = TRUE, width = "14em")})
  
  
  dtcou <- reactive({co[which(co$country == input$cou2),]})
  dtpr <- reactive({co[which(co$country == input$cou2 & co$prd_type == input$prd2),]})
  
  dtcou4 <- reactive({co[which(co$country == input$cou4),]})
  dtpr4 <- reactive({co[which(co$country == input$cou4 & co$prd_type == input$prd4),]})
  
##PAGE 2: CUSTOMER GROUP 1  
  #Country- Product Type
  output$copr1<- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id[id$mrk_channel==input$mrc1,],country,prd_type),count = sum(count)))%>% spread(prd_type, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id[id$mrk_channel==input$mrc1,],country,prd_type),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id[id$mrk_channel==input$mrc1,],country,prd_type),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id[id$mrk_channel==input$mrc1,],country,prd_type),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  }) 
  
  #Country- Marketing Channel
  output$comc1 <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id[id$prd_type==input$prd1,],country,mrk_channel),count = sum(count)))%>% spread(mrk_channel, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id[id$prd_type==input$prd1,],country,mrk_channel),revenue = sum(revenue)))%>% spread(mrk_channel, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id[id$prd_type==input$prd1,],country,mrk_channel),cost = sum(cost)))%>% spread(mrk_channel, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id[id$prd_type==input$prd1,],country,mrk_channel),profit = sum(profit)))%>% spread(mrk_channel, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
  #Product Type- Marketing Channel
  output$mcpr1 <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id[id$country ==input$cou1,],prd_type,mrk_channel),count = sum(count)))%>% spread(prd_type, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '270px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id[id$country ==input$cou1,],prd_type,mrk_channel),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '270px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id[id$country ==input$cou1,],prd_type,mrk_channel),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '270px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id[id$country ==input$cou1,],prd_type,mrk_channel),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '270px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
  
  ###PAGE3: COHORT ANALYSIS
  user_p <- reactive({
  user <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), count= sum(count,na.rm=TRUE)))%>% spread( month,count)
  user_pe <- data.frame(month = user$acq_month)
  user_pe$`0` <- 100
  for (i in 1:(ncol(user)-2) ){
    out <- round((user[[paste(i)]]/user$`0`)*100,1)
    user_pe[[paste(i)]] <- out
  }
  user_pe
  })

  rev_p <- reactive({
    rev <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), revenue= sum(revenue,na.rm=TRUE)))%>% spread( month,revenue)
    rev_pe <- data.frame(month = rev$acq_month)
    rev_pe$`0` <- 100
    for (i in 1:(ncol(rev)-2) ){
      out <- round((rev[[paste(i)]]/rev$`0`)*100,1)
      rev_pe[[paste(i)]] <- out
    }
    rev_pe
  })

  cost_p <- reactive({
    cost <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), cost= sum(cost,na.rm=TRUE)))%>% spread( month,cost)
    cost_pe <- data.frame(month = cost$acq_month)
    cost_pe$`0` <- 100
    for (i in 1:(ncol(cost)-2) ){
      out <- round((cost[[paste(i)]]/cost$`0`)*100,1)
      cost_pe[[paste(i)]] <- out
    }
    cost_pe
  })

  profit_p <- reactive({
    profit <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), profit = sum(profit,na.rm=TRUE)))%>% spread( month,profit)
    profit_pe <- data.frame(month = profit$acq_month)
    profit_pe$`0` <- 100
    for (i in 1:(ncol(profit)-2) ){
      out <- round((profit[[paste(i)]]/profit$`0`)*100,1)
      profit_pe[[paste(i)]] <- out
    }
    profit_pe
  })

  output$core <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(user_p()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(rev_p()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(cost_p()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(profit_p()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
  })
 
  user_c<- reactive({
    user <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), count= sum(count,na.rm=TRUE)))%>% spread( month,count)
    user_pc <- data.frame( month = "0", avg = 100)
    for( i in 1:(ncol( user)-2)){
      out <- data.frame(month = paste(i), avg = round(sum( user[[paste(i)]],na.rm = TRUE)*100/ sum(user$`0`,na.rm = TRUE), 1))
      user_pc <- rbind(user_pc, out)
    }
    user_pc$avg <- round(user_pc$avg,1)
    user_pc
  }) 
  
  rev_c<- reactive({
    rev <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), revenue = sum(revenue,na.rm=TRUE)))%>% spread( month,revenue)
    rev_pc <- data.frame( month = "0", avg = 100)
    for( i in 1:(ncol( rev)-2)){
      out <- data.frame(month = paste(i), avg = round(sum( user[[paste(i)]],na.rm = TRUE)*100/ sum(user$`0`,na.rm = TRUE), 1))
      rev_pc <- rbind(rev_pc, out)
    }
    rev_pc$avg <- round(rev_pc$avg,1)
    rev_pc
  }) 
  
  cost_c<- reactive({
    cost <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), cost= sum(cost,na.rm=TRUE)))%>% spread( month,cost)
    cost_pc <- data.frame( month = "0", avg = 100)
    for( i in 1:(ncol( cost)-2)){
      out <- data.frame(month = paste(i), avg = round(sum( user[[paste(i)]],na.rm = TRUE)*100/ sum(user$`0`,na.rm = TRUE), 1))
      cost_pc <- rbind(cost_pc, out)
    }
    cost_pc$avg <- round(cost_pc$avg,1)
    cost_pc
  }) 
  
  profit_c<- reactive({
    profit <- data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), profit= sum(profit,na.rm=TRUE)))%>% spread( month,profit)
    profit_pc <- data.frame( month = "0", avg = 100)
    for( i in 1:(ncol( profit)-2)){
      out <- data.frame(month = paste(i), avg = round(sum( user[[paste(i)]],na.rm = TRUE)*100/ sum(user$`0`,na.rm = TRUE), 1))
      profit_pc <- rbind(profit_pc, out)
    }
    profit_pc$avg <- round(profit_pc$avg,1)
    profit_pc
  }) 
  # Performance
  output$per <- renderHighchart({
    if(input$ty == "Number of users"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(user_c()$month)) %>%
        hc_yAxis_multiples (
          list( title = list(text = "Users %"))
          
        ) %>%
        hc_add_series(name="Users %", user_c()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.1f} %<br/>") %>%
        hc_exporting(enabled = TRUE)} 
      
    else if (input$ty == "Revenue"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(rev_c()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Revenue %"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Revenue %", rev_c()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.1f} %<br/>") %>%
        hc_exporting(enabled = TRUE)} 
    else if (input$ty == "Cost"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(cost_c()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Cost %"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Cost %", cost_c()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.1f} %<br/>") %>%
        hc_exporting(enabled = TRUE)} 
    else {
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(profit_c()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Profit %"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Profit %", profit_c()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.1f} %<br/>") %>%
        hc_exporting(enabled = TRUE)}  
  })
  # Absolute value
  user_p4 <- reactive(data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), count= sum(count,na.rm=TRUE)))%>% spread( month,count))
  rev_p4 <- reactive(data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), revenue= sum(revenue,na.rm=TRUE)))%>% spread( month,revenue))
  cost_p4 <- reactive(data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), cost= sum(cost,na.rm=TRUE)))%>% spread( month,cost))
  profit_p4 <- reactive(data.frame(summarize(group_by(co[co$country== input$cou2 & co$prd_type == input$prd2 & co$mrk_channel == input$mrc2,],month, acq_month), profit= sum(profit,na.rm=TRUE)))%>% spread( month,profit))
  
  
  output$core4 <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(user_p4()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(rev_p4()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(cost_p4()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(profit_p4()
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '450px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
  })
  
  user_c4<- reactive({
    user <- data.frame(summarize(group_by(co[co$country== input$cou4 & co$prd_type == input$prd4 & co$mrk_channel == input$mrc4,],month, acq_month), count= sum(count,na.rm=TRUE)))%>% spread( month,count)
    user_pc <- data.frame( month = "0", avg = mean(user$`0`))
    for( i in 1:(ncol( user)-2)){
      out <- data.frame(month = paste(i), avg = mean( user[[paste(i)]],na.rm = TRUE))
      user_pc <- rbind(user_pc, out)
    }
    user_pc$avg <- round(user_pc$avg,1)
    user_pc
  }) 
  
  rev_c4<- reactive({
    rev <- data.frame(summarize(group_by(co[co$country== input$cou4 & co$prd_type == input$prd4 & co$mrk_channel == input$mrc4,],month, acq_month), revenue = sum(revenue,na.rm=TRUE)))%>% spread( month,revenue)
    rev_pc <- data.frame( month = "0", avg = mean(rev$`0`))
    for( i in 1:(ncol(rev)-2)){
      out <- data.frame(month = paste(i), avg = mean( rev[[paste(i)]],na.rm = TRUE))
      rev_pc <- rbind(rev_pc, out)
    }
    rev_pc$avg <- round(rev_pc$avg,1)
    rev_pc
  }) 
  
  cost_c4<- reactive({
    cost <- data.frame(summarize(group_by(co[co$country== input$cou4 & co$prd_type == input$prd4 & co$mrk_channel == input$mrc4,],month, acq_month), cost= sum(cost,na.rm=TRUE)))%>% spread( month,cost)
    cost_pc <- data.frame( month = "0", avg = mean(cost$`0`))
    for( i in 1:(ncol( cost)-2)){
      out <- data.frame(month = paste(i), avg = mean( cost[[paste(i)]],na.rm = TRUE))
      cost_pc <- rbind(cost_pc, out)
    }
    cost_pc$avg <- round(cost_pc$avg,1)
    cost_pc
  }) 
  
  profit_c4<- reactive({
    profit <- data.frame(summarize(group_by(co[co$country== input$cou4 & co$prd_type == input$prd4 & co$mrk_channel == input$mrc4,],month, acq_month), profit= sum(profit,na.rm=TRUE)))%>% spread( month,profit)
    profit_pc <- data.frame( month = "0", avg = mean(profit$`0`))
    for( i in 1:(ncol( profit)-2)){
      out <- data.frame(month = paste(i), avg = mean( profit[[paste(i)]],na.rm = TRUE))
      profit_pc <- rbind(profit_pc, out)
    }
    profit_pc$avg <- round(profit_pc$avg,1)
    profit_pc
  }) 
  
  output$per4 <- renderHighchart({
    if(input$ty == "Number of users"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(user_c4()$month)) %>%
        hc_yAxis_multiples (
          list( title = list(text = "Users"))
          
        ) %>%
        hc_add_series(name="Users", user_c4()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.0f} <br/>") %>%
        hc_exporting(enabled = TRUE)} 
    
    else if (input$ty == "Revenue"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(rev_c4()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Revenue"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Revenue", rev_c4()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.0f} <br/>") %>%
        hc_exporting(enabled = TRUE)} 
    else if (input$ty == "Cost"){
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(cost_c4()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Cost"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Cost", cost_c4()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.0f} <br/>") %>%
        hc_exporting(enabled = TRUE)} 
    else {
      highchart() %>%
        hc_subtitle(text = paste(input$ty," - Tracking",sep= " "),align = "center") %>%
        hc_xAxis(categories = unique(profit_c4()$month)) %>%
        hc_yAxis_multiples (
          #create_yaxis(naxis = 2),
          list( title = list(text = "Profit"))
          
          
          #list(title = list(text = "Euro"))
        ) %>%
        hc_add_series(name="Profit", profit_c4()$avg, color= "orange"  , zIndex = 9, type = "spline", dataLabels = list(enabled = FALSE)) %>%
        hc_tooltip(crosshairs = TRUE, shared = TRUE,pointFormat = "<span style=\"color:{point.color}\">\u25CF</span> {series.name}: {point.y:,.0f} %<br/>") %>%
        hc_exporting(enabled = TRUE)}  
  })
  
  ##PAGE 4: CUSTOMER GROUP 2  
  #Country- Product Type
  output$copr3<- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$mrk_channel==input$mrc3,],country,prd_type),count = sum(count)))%>% spread(prd_type, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$mrk_channel==input$mrc3,],country,prd_type),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$mrk_channel==input$mrc3,],country,prd_type),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$mrk_channel==input$mrc3,],country,prd_type),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  }) 
  
  #Country- Marketing Channel
  output$comc3 <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$prd_type==input$prd3,],country,mrk_channel),count = sum(count)))%>% spread(mrk_channel, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$prd_type==input$prd3,],country,mrk_channel),revenue = sum(revenue)))%>% spread(mrk_channel, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$prd_type==input$prd3,],country,mrk_channel),cost = sum(cost)))%>% spread(mrk_channel, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$prd_type==input$prd3,],country,mrk_channel),profit = sum(profit)))%>% spread(mrk_channel, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
  #Product Type- Marketing Channel
  output$mcpr3 <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$country ==input$cou3,],prd_type,mrk_channel),count = sum(count)))%>% spread(prd_type, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '250px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$country ==input$cou3,],prd_type,mrk_channel),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '250px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$country ==input$cou3,],prd_type,mrk_channel),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '250px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(id_NA[id_NA$country ==input$cou3,],prd_type,mrk_channel),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '250px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
  
  
##PAGE: CUSTOMER GROUP 3 
  #Country- Product Type
  output$copr<- DT::renderDataTable({
    if(input$ty == "Number of users"){
    DT::datatable(data.frame(summarize(group_by(na[na$mrk_channel==input$mrc,],country,prd_type),count = sum(count)))%>% spread(prd_type, count)
                  #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                  ,extensions = c("Buttons","FixedColumns")
                  ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                #, columnDefs = list(list(targets = 0, visible = FALSE))
                                ,fixedHeader = TRUE)
                  ,rownames = FALSE
                  ,escape = FALSE
                  ,filter = c("top")
    )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(na[na$mrk_channel==input$mrc,],country,prd_type),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(na[na$mrk_channel==input$mrc,],country,prd_type),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(na[na$mrk_channel==input$mrc,],country,prd_type),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  }) 
  
  #Country- Marketing Channel
  output$comc <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(na[na$prd_type==input$prd,],country,mrk_channel),count = sum(count)))%>% spread(mrk_channel, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(na[na$prd_type==input$prd,],country,mrk_channel),revenue = sum(revenue)))%>% spread(mrk_channel, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(na[na$prd_type==input$prd,],country,mrk_channel),cost = sum(cost)))%>% spread(mrk_channel, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(na[na$prd_type==input$prd,],country,mrk_channel),profit = sum(profit)))%>% spread(mrk_channel, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '125px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
  #Product Type- Marketing Channel
  output$mcpr <- DT::renderDataTable({
    if(input$ty == "Number of users"){
      DT::datatable(data.frame(summarize(group_by(na[na$country ==input$cou,],prd_type,mrk_channel),count = sum(count)))%>% spread(prd_type, count)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '230px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Revenue"){
      DT::datatable(data.frame(summarize(group_by(na[na$country ==input$cou,],prd_type,mrk_channel),revenue = sum(revenue)))%>% spread(prd_type, revenue)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '230px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}
    else if (input$ty == "Cost"){
      DT::datatable(data.frame(summarize(group_by(na[na$country ==input$cou,],prd_type,mrk_channel),cost = sum(cost)))%>% spread(prd_type, cost)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '230px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )} 
    else {
      DT::datatable(data.frame(summarize(group_by(na[na$country ==input$cou,],prd_type,mrk_channel),profit = sum(profit)))%>% spread(prd_type, profit)
                    #,caption = HTML(paste(input$ty," - Tracking",sep= " "))
                    ,extensions = c("Buttons","FixedColumns")
                    ,option=list( dom = "Bfrtip", buttons = list('copy', 'csv', 'print'),scrollX=TRUE, fixedColumns =TRUE, paging = FALSE,scrollY= '230px'
                                  #, columnDefs = list(list(targets = 0, visible = FALSE))
                                  ,fixedHeader = TRUE)
                    ,rownames = FALSE
                    ,escape = FALSE
                    ,filter = c("top")
      )}  
  })
}

shinyApp(ui, server)

