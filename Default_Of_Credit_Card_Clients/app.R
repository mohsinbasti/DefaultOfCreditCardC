library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(pageWithSidebar(
    headerPanel("Default of Credit Card Clients"),

    # Sidebar with a slider input for number of bins 
        sidebarPanel(
            conditionalPanel(condition = "input.tabselected==1",
                             selectInput("option_1","Select From Following Options: ",
                                         choices = c("none","head" = 1,"tail" = 2,"class" = 3,"dim" = 4,"summary" = 5,"names" = 6,"str" = 7)),),
            conditionalPanel(condition = "input.tabselected==2",
                             selectInput("option_2","Select Type Of Plot: ",
                                         choices = c("none","Correlation Plot" = 1,"Histograms" = 2,"Box Plots" = 3,"Bar Plots" = 4, "Dot Plot" = 5 ,"Violin Plot" = 6)),),
            conditionalPanel(condition = "input.tabselected==3",
                             selectInput("option_3","Please choose Model",
                                         choices = c("none","Decision Tree" = 1,"Naive Bayes" = 2, "Neural Network" = 3, "Random Forest" = 4,"SVM" = 5)),),
            conditionalPanel(condition = "input.tabselected==4"),
            conditionalPanel(condition = "input.tabselected==5",
                             sliderInput("LIMIT_BAL", "Enter LIMIT_BAL:",
                                         min = 10000, max = 1000000, value = 0),
                             selectInput("SEX","Please select Gender: ",
                                         choices = c("Male", "Female", "Other")),
                             selectInput("EDUCATION","Please select Education: ",
                                         choices = c("0", "1", "2", "3","4","5","6")),
                             selectInput("MARRIAGE","Please select Marital Status: ",
                                         choices = c("0", "1", "2")),
                             sliderInput("AGE", "Enter Age:",
                                         min = 21, max = 79, value = 0),
                             sliderInput("PAY_1", "Enter Pay_1:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("PAY_2", "Enter Pay_2:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("PAY_3", "Enter Pay_3:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("PAY_4", "Enter Pay_4:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("PAY_5", "Enter Pay_5:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("PAY_6", "Enter Pay_6:",
                                         min = -2, max = 8, value = 0),
                             sliderInput("BILL_AMT1", "Enter BILL_AMT1:",
                                         min = -165580, max = 964511, value = 0),
                             sliderInput("BILL_AMT2", "Enter BILL_AMT2:",
                                         min = -69777, max = 983931, value = 0),
                             sliderInput("BILL_AMT3", "Enter BILL_AMT3:",
                                         min = -157264, max = 1664089, value = 0),
                             sliderInput("BILL_AMT4", "Enter BILL_AMT4:",
                                         min = -170000, max = 891586, value = 0),
                             sliderInput("BILL_AMT5", "Enter BILL_AMT5:",
                                         min = -81334, max = 927171, value = 0),
                             sliderInput("BILL_AMT6", "Enter BILL_AMT6:",
                                         min = -339603, max = 961664, value = 0),
                             sliderInput("PAY_AMT1", "Enter Pay_Amt1:",
                                         min = 0, max = 873552, value = 0),
                             sliderInput("PAY_AMT2", "Enter Pay_Amt2:",
                                         min = 0, max = 1684259, value = 0),
                             sliderInput("PAY_AMT3", "Enter Pay_Amt3:",
                                         min = 0, max = 896040, value = 0),
                             sliderInput("PAY_AMT4", "Enter Pay_Amt4:",
                                         min = 0, max = 621000, value = 0),
                             sliderInput("PAY_AMT5", "Enter Pay_Amt5:",
                                         min = 0, max = 426529, value = 0),
                             sliderInput("PAY_AMT6", "Enter Pay_Amt6:",
                                         min = 0, max = 528666, value = 0),
                             actionButton("action1", label = "show output")
                             )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel( 
                tabPanel("DatasetExploration", value = 1, conditionalPanel(condition = "input.option_1==1", verbatimTextOutput("hea")),
                         conditionalPanel(condition = "input.option_1==2", verbatimTextOutput("tai")),
                         conditionalPanel(condition = "input.option_1==3", verbatimTextOutput("cla")),
                         conditionalPanel(condition = "input.option_1==4", verbatimTextOutput("di")),
                         conditionalPanel(condition = "input.option_1==5", verbatimTextOutput("summ")),
                         conditionalPanel(condition = "input.option_1==6", verbatimTextOutput("nam")),
                         conditionalPanel(condition = "input.option_1==7", verbatimTextOutput("st"))
                         ),
                tabPanel("Plots", value = 2,conditionalPanel(condition = "input.option_2==1", plotOutput("corr")),
                         conditionalPanel(condition = "input.option_2==2", 
                                          selectInput("opt_graph","please select option:",
                                                      choices = c("none" = 0,"LIMIT_BAL" = 1, "PAY_0" = 2,"PAY_2" = 3, "PAY_3" = 4,
                                                                  "PAY_4" = 5, "PAY_5" = 6, "PAY_6" = 7, "BILL_AMT1" = 8, "BILL_AMT2" = 9,
                                                                  "BILL_AMT3" = 10, "BILL_AMT4" = 11, "BILL_AMT5" = 12, "BILL_AMT6" = 13,
                                                                  "PAY_AMT1" = 14, "PAY_AMT2" = 15, "PAY_AMT3" = 16, "PAY_AMT4" = 17,
                                                                  "PAY_AMT5" = 18, "PAY_AMT6" = 19, "default_payment_next_month" = 20))),
                         conditionalPanel(condition = "input.opt_graph==1", plotOutput("limitbal")),
                         conditionalPanel(condition = "input.opt_graph==2", plotOutput("pay0")),
                         conditionalPanel(condition = "input.opt_graph==3", plotOutput("pay2")),
                         conditionalPanel(condition = "input.opt_graph==4", plotOutput("pay3")),
                         conditionalPanel(condition = "input.opt_graph==5", plotOutput("pay4")),
                         conditionalPanel(condition = "input.opt_graph==6", plotOutput("pay5")),
                         conditionalPanel(condition = "input.opt_graph==7", plotOutput("pay6")),
                         conditionalPanel(condition = "input.opt_graph==8", plotOutput("billamt1")),
                         conditionalPanel(condition = "input.opt_graph==9", plotOutput("billamt2")),
                         conditionalPanel(condition = "input.opt_graph==10", plotOutput("billamt3")),
                         conditionalPanel(condition = "input.opt_graph==11", plotOutput("billamt4")),
                         conditionalPanel(condition = "input.opt_graph==12", plotOutput("billamt5")),
                         conditionalPanel(condition = "input.opt_graph==13", plotOutput("billamt6")),
                         conditionalPanel(condition = "input.opt_graph==14", plotOutput("payamt1")),
                         conditionalPanel(condition = "input.opt_graph==15", plotOutput("payamt2")),
                         conditionalPanel(condition = "input.opt_graph==16", plotOutput("payamt3")),
                         conditionalPanel(condition = "input.opt_graph==17", plotOutput("payamt4")),
                         conditionalPanel(condition = "input.opt_graph==18", plotOutput("payamt5")),
                         conditionalPanel(condition = "input.opt_graph==19", plotOutput("payamt6")),
                         conditionalPanel(condition = "input.opt_graph==20", plotOutput("defaultpayment")),
                         conditionalPanel(condition = "input.option_2==3", 
                                          selectInput("opt1_graph","please select option:",
                                                      choices = c("none" = 0,"LIMIT_BAL_VS_default_payment" = 1,
                                                                  "MARRIAGE_VS_default_payment" = 2, "AGE_VS_default_payment" = 3,
                                                                  "BILL_AMT1_VS_default_payment" = 4, "BILL_AMT2_VS_default_payment" = 5,
                                                                  "BILL_AMT3_VS_default_payment" = 6, "BILL_AMT4_VS_default_payment" = 7,
                                                                  "BILL_AMT5_VS_default_payment" = 8, "BILL_AMT6_VS_default_payment" = 9,
                                                                  "education_VS_default_payment" = 10))),
                         conditionalPanel(condition = "input.opt1_graph==1", plotOutput("limitbalvsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==2", plotOutput("marrvsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==3", plotOutput("agevsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==4", plotOutput("billp1vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==5", plotOutput("billp2vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==6", plotOutput("billp3vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==7", plotOutput("billp4vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==8", plotOutput("billp5vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==9", plotOutput("billp6vsdpayment")),
                         conditionalPanel(condition = "input.opt1_graph==10", plotOutput("eduvsdpayment")),
                         conditionalPanel(condition = "input.option_2==4",
                                          selectInput("opt2_graph","please select option:",
                                                      choices = c("none" = 0,"Default_VS_nonDefault" = 1, "Gender Wise" = 2, "Merital Status Wise" = 3))),
                         conditionalPanel(condition = "input.opt2_graph==1", plotOutput("defvsnondef")),
                         conditionalPanel(condition = "input.opt2_graph==2", plotOutput("gendwise")),
                         conditionalPanel(condition = "input.opt2_graph==3", plotOutput("meritalwise")),
                         conditionalPanel(condition = "input.option_2==5",
                                          selectInput("opt3_graph","please select option:",
                                                      choices = c("none" = 0,"AGE_VS_LIMITBAL" = 1, "EDUVATION_VS_LIMITBAL" = 2,
                                                                  "MARRIAGE_VS_LIMITBAL" = 3, "SEX_VS_LIMITBAL" = 4)),
                                          conditionalPanel(condition = "input.opt3_graph==1", plotOutput("agelimbal")),
                                          conditionalPanel(condition = "input.opt3_graph==2", plotOutput("edulimbal")),
                                          conditionalPanel(condition = "input.opt3_graph==3", plotOutput("marrlimbal")),
                                          conditionalPanel(condition = "input.opt3_graph==4", plotOutput("sexlimbal"))),
                         conditionalPanel(condition = "input.option_2==6",
                                          selectInput("opt4_graph","please select option:",
                                                      choices = c("none" = 0,"AGE_VS_Default/NonDefault" = 1, "EDUCATION_VS_Default/NonDefault" = 2,
                                                                  "MARRIAGE_VS_Default/NonDefault" = 3)),
                                          conditionalPanel(condition = "input.opt4_graph==1", plotOutput("agedefnondef")),
                                          conditionalPanel(condition = "input.opt4_graph==2", plotOutput("edudefnondef")),
                                          conditionalPanel(condition = "input.opt4_graph==3", plotOutput("marrdefnondef"))),
                         ),
                tabPanel("Models", value = 3, conditionalPanel(condition = "input.option_3==1", verbatimTextOutput("decision")),
                         conditionalPanel(condition = "input.option_3==2", verbatimTextOutput("naive")),
                         conditionalPanel(condition = "input.option_3==3", verbatimTextOutput("neural")),
                         conditionalPanel(condition = "input.option_3==4", verbatimTextOutput("random")),
                         conditionalPanel(condition = "input.option_3==5", verbatimTextOutput("svm"))
                         ),
                tabPanel("Models Accuracy Comparison", value = 4, plotOutput("plot")),
                tabPanel("Prediction", value = 5, conditionalPanel(condition = "input.action1", verbatimTextOutput("outputdata"),verbatimTextOutput("output"))),
                id = "tabselected"
                )
            )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        plotAccuracy
    })
    output$hea <- renderPrint({
        head(myData)
    })
    output$tai <- renderPrint({
        tail(myData)
    })
    output$cla <- renderPrint({
        class(myData)
    })
    output$di <- renderPrint({
        dim(myData)
    })
    output$summ <- renderPrint({
        summary(myData)
    })
    output$nam <- renderPrint({
        names(myData)
    })
    output$st <- renderPrint({
        str(myData)
    })
    output$corr <- renderPlot({
        corrplot(coorelation, method = "circle")
    })
    output$limitbal <- renderPlot({
        hist(myData$LIMIT_BAL,
             main="Limit of Balance Amount",
             xlab="Balance Limit",
             col="darkmagenta",
             freq=FALSE
        )
    })
    output$pay0 <- renderPlot({
        hist(myData$PAY_0,
             main="Amount of Pay 0",
             xlab="Pay_0",
             col="pink",
             freq=FALSE
        )
    })
    output$pay2 <- renderPlot({
        hist(myData$PAY_2,
             main="LAmount of Pay 2",
             xlab="Pay_2",
             col="red",
             freq=FALSE
        )
    })
    output$pay3 <- renderPlot({
        hist(myData$PAY_3,
             main="Amount of Pay 3",
             xlab="Pay_3",
             col="yellow",
             freq=FALSE
        )
    })
    output$pay4 <- renderPlot({
        hist(myData$PAY_4,
             main="Amount of Pay 4",
             xlab="Pay_4",
             col="violet",
             freq=FALSE
        )
    })
    output$pay5 <- renderPlot({
        hist(myData$PAY_5,
             main="Amount of Pay 5",
             xlab="Pay_5",
             col="green",
             freq=FALSE
        )
    })
    output$pay6 <- renderPlot({
        hist(myData$PAY_6,
             main="Amount of Pay 6",
             xlab="Pay_6",
             col="blue",
             freq=FALSE
        )
    })
    output$billamt1 <- renderPlot({
        hist(myData$BILL_AMT1,
             main="Amount of Bill 1",
             xlab="BILL_AMT1",
             col="cyan",
             freq=FALSE
        )
    })
    output$billamt2 <- renderPlot({
        hist(myData$BILL_AMT2,
             main="Amount of Bill 2",
             xlab="BILL_AMT2",
             col="darkmagenta",
             freq=FALSE
        )
    })
    output$billamt3 <- renderPlot({
        hist(myData$BILL_AMT3,
             main="Amount of Bill 3",
             xlab="BILL_AMT3",
             col="pink",
             freq=FALSE
        )
    })
    output$billamt4 <- renderPlot({
        hist(myData$BILL_AMT4,
             main="Amount of Bill 4",
             xlab="BILL_AMT4",
             col="red",
             freq=FALSE
        )
    })
    output$billamt5 <- renderPlot({
        hist(myData$BILL_AMT5,
             main="Amount of Bill 5",
             xlab="BILL_AMT5",
             col="yellow",
             freq=FALSE
        )
    })
    output$billamt6 <- renderPlot({
        hist(myData$BILL_AMT6,
             main="Amount of Bill 6",
             xlab="BILL_AMT6",
             col="violet",
             freq=FALSE
        )
    })
    output$payamt1 <- renderPlot({
        hist(myData$PAY_AMT1,
             main="Amount of Pay 1",
             xlab="PAY_AMT1",
             col="blue",
             freq=FALSE
        )
    })
    output$payamt2 <- renderPlot({
        hist(myData$PAY_AMT2,
             main="Amount of Pay 2",
             xlab="PAY_AMT2",
             col="cyan",
             freq=FALSE
        )
    })
    output$payamt3 <- renderPlot({
        hist(myData$PAY_AMT3,
             main="Amount of Pay 3",
             xlab="PAY_AMT3",
             col="brown",
             freq=FALSE
        )
    })
    output$payamt4 <- renderPlot({
        hist(myData$PAY_AMT4,
             main="Amount of Pay 4",
             xlab="PAY_AMT4",
             col="orange",
             freq=FALSE
        )
    })
    output$payamt5 <- renderPlot({
        hist(myData$PAY_AMT5,
             main="Amount of Pay 5",
             xlab="PAY_AMT5",
             col="black",
             freq=FALSE
        )
    })
    output$payamt6 <- renderPlot({
        hist(myData$PAY_AMT6,
             main="Amount of Pay 6",
             xlab="PAY_AMT6",
             col="darkmagenta",
             freq=FALSE
        )
    })
    output$defaultpayment <- renderPlot({
        hist(myData$default_payment_next_month,
             main="Default of Payment Next Month",
             xlab="default_payment_next_month",
             col="green",
             freq=FALSE
        )
    })
    output$limitbalvsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=LIMIT_BAL)) + 
            geom_boxplot(color="black", fill="green", alpha=0.9)
    })
    output$marrvsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=MARRIAGE)) + 
            geom_boxplot(color="black", fill="orange", alpha=0.9)
    })
    output$agevsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=AGE)) + 
            geom_boxplot(color="black", fill="pink", alpha=0.9)
    })
    output$billp1vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT1)) + 
            geom_boxplot(color="black", fill="orange", alpha=0.9)
    })
    output$billp2vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT2)) + 
            geom_boxplot(color="black", fill="purple", alpha=0.9)
    })
    output$billp3vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT3)) + 
            geom_boxplot(color="black", fill="pink", alpha=0.9)
    })
    output$billp4vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT4)) + 
            geom_boxplot(color="black", fill="red", alpha=0.9)
    })
    output$billp5vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT5)) + 
            geom_boxplot(color="black", fill="yellow", alpha=0.9)
    })
    output$billp6vsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT6)) + 
            geom_boxplot(color="black", fill="brown", alpha=0.9)
    })
    output$eduvsdpayment <- renderPlot({
        ggplot(myData, aes(x=default_payment_next_month, y=EDUCATION)) + 
            geom_boxplot(color="black", fill="purple", alpha=0.9)
    })
    output$violin <- renderPlot({
        ggplot(data = myData, aes(x = default_payment_next_month,y=AGE)) +
            geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Age")+xlab("Non-defaulters and Defaulters")
    })
    output$dot <- renderPlot({
        ggplot(myData, aes(x=LIMIT_BAL, y=AGE, color=LIMIT_BAL)) + 
            geom_point(size=3)
    })
    output$defvsnondef <- renderPlot({
        ggplot(data = myData, mapping = aes(x = default_payment_next_month, fill = default_payment_next_month)) +
            geom_bar() +
            xlab("IsDefault") +
            ggtitle(" Defaulters VS non-Defaulters") +
            stat_count(aes(label = ..count..), geom = "label")
    })
    output$gendwise <- renderPlot({
        ggplot(data = myData, mapping = aes(x = SEX, fill = default_payment_next_month)) +
            geom_bar() +
            xlab("SEX") +
            ggtitle("Gender Wise Classification") +
            stat_count(aes(label = ..count..), geom = "label")
    })
    output$meritalwise <- renderPlot({
        ggplot(data = myData, mapping = aes(x = MARRIAGE, fill = default_payment_next_month)) +
            geom_bar() +
            xlab("Marriage") +
            ggtitle("Merital Status Wise Classification") +
            stat_count(aes(label = ..count..), geom = "label")
    })
    output$agelimbal <- renderPlot({
        ggplot(myData, aes(x=LIMIT_BAL, y=AGE, color=LIMIT_BAL)) + 
            geom_point(size=3)
    })
    output$edulimbal <- renderPlot({
        ggplot(myData, aes(x=LIMIT_BAL, y=EDUCATION, color=LIMIT_BAL)) + 
            geom_point(size=3)
    })
    output$marrlimbal <- renderPlot({
        ggplot(myData, aes(x=LIMIT_BAL, y=MARRIAGE, color=LIMIT_BAL)) + 
            geom_point(size=3)
    })
    output$sexlimbal <- renderPlot({
        ggplot(myData, aes(x=LIMIT_BAL, y=SEX, color=LIMIT_BAL)) + 
            geom_point(size=3)
    })
    output$agedefnondef <- renderPlot({
        ggplot(data = myData, aes(x = default_payment_next_month,y=AGE)) +
            geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Age")+xlab("Non-defaulters and Defaulters")
    })
    output$edudefnondef <- renderPlot({
        ggplot(data = myData, aes(x = default_payment_next_month,y=EDUCATION)) +
            geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Educatiom")+xlab("Non-defaulters and Defaulters")
    })
    output$marrdefnondef <- renderPlot({
        ggplot(data = myData, aes(x = default_payment_next_month,y=MARRIAGE)) +
            geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Marriage")+xlab("Non-defaulters and Defaulters")
    })
    output$decision <- renderPrint({
        cm1
    })
    output$naive <- renderPrint({
        cm2
    })
    output$neural <- renderPrint({
        cm4
    })
    output$random <- renderPrint({
        cm3
    })
    output$svm <- renderPrint({
        cm
    })
    output$outputdata <- renderPrint({
        newPdata <- data.frame(
            ID = 0,
            LIMIT_BAL = input$LIMIT_BAL,
            SEX = input$SEX,
            EDUCATION = input$EDUCATION,
            MARRIAGE = input$MARRIAGE,
            AGE = input$AGE,
            PAY_0  = input$PAY_1,
            PAY_2 = input$PAY_2,
            PAY_3 = input$PAY_3,
            PAY_4 = input$PAY_4,
            PAY_5 = input$PAY_5,
            PAY_6 = input$PAY_6,
            BILL_AMT1 = input$BILL_AMT1,
            BILL_AMT2 = input$BILL_AMT2,
            BILL_AMT3 = input$BILL_AMT3,
            BILL_AMT4 = input$BILL_AMT4,
            BILL_AMT5 = input$BILL_AMT5,
            BILL_AMT6 = input$BILL_AMT6,
            PAY_AMT1 = input$PAY_AMT1,
            PAY_AMT2 = input$PAY_AMT2,
            PAY_AMT3 = input$PAY_AMT3,
            PAY_AMT4 = input$PAY_AMT4,
            PAY_AMT5 = input$PAY_AMT5,
            PAY_AMT6 = input$PAY_AMT6
        )
    })
    output$output <- renderPrint({
        predictions = predict(SVMclassifier, newdata = newPdata)
        as.integer(predictions[[1]])
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)