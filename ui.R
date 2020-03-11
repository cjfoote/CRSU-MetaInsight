###### Continuous_Bayesian ######

#install.packages(c("dplyr","metafor", "netmeta","shiny", "shinyAce","rmarkdown", "knitr", "shinydashboard", "gemtc"
#  , "shinyalert", "ggplot2", "plotly"))

library(dplyr)
library(metafor)
library(netmeta)
library(shiny)
library(shinyAce)
library(rmarkdown)
library(knitr)
library(shinydashboard)
library(gemtc)
library(shinyalert)
library(ggplot2)
library(plotly)



source("PlotFunctionsRKO.R", local = TRUE) # Plot functions
#source("PlotFunctionsContRKO.R", local = TRUE) # Plot fun
load("blank.rds") # Objects to store data for plot functions

shinyUI(navbarPage(id="meta",
  "MetaInsight", 
  header = singleton(tags$head(includeScript("google_analytics2.js"))),
  tabPanel(id="home", "Home", 
           h2("MetaInsight (continuous - including Bayesian estimates) V2.1 **", tags$sup("Beta", style="color:#6CC0ED"), align= "left"),
           h4(tags$a(href="https://crsu.shinyapps.io/metainsight_binary2/", "For binary outcomes please click here.", target="_blank")),
           fluidRow(
                                     column(3, br(), br(),
                                    img(src='network2.jpg', width=500, height=400, align = "center")
                                       ),
                                        column(2),
                                       column(5, 
                                     br(),
                                     p(tags$strong("** New features updated on 15 Nov 2019 (V2.1) ** :")),
                                     actionLink("history_click", "Click here to view a full update history of MetaInsight - continuous data"),
                                     p(tags$ul(tags$li("Network connectivity information is now displayed on the '1b. Network Plot' tab"))),
                                     p(tags$ul(tags$li("A plot of residual deviance from NMA model and UME inconsistency model is now displayed on the '3f.deviance report' tab to assist users to determine the model fit")))
                                     )),
                                     br(),
                                     p("Rhiannon K Owen,  Naomi Bradbury, Yiqiao Xin, Nicola Cooper, and Alex Sutton", align= "left"),
                                    p("For feedback/questions about this app please contact Professor Alex Sutton", tags$a(href="mailto:ajs22@leicester.ac.uk", "ajs22@leicester.ac.uk", align= "left")),
                                    br(),
                                    p("App powered by Rshiny.All frequentist statistical calculations are performed using R package netmeta (Gerta Rücker, Guido Schwarzer, Ulrike Krahn and Jochem König
                                      2017).", tags$a(href="http://CRAN.R-project.org/package=netmeta", "netmeta: Network Meta-Analysis using Frequentist Methods. R package version 0.9-8.",target="_blank"), 
                                      "All Bayesian statistical calculations are performed using R package gemtc (Gert van Valkenhoef, Joel Kuiper 2016)", 
                                      tags$a(href="https://cran.r-project.org/web/packages/gemtc/gemtc.pdf", "gemtc: Network Meta-Analysis Using Bayesian Methods R package version 0.8-2.
                                      ",target="_blank")),
                                    p("For users wishing to analyse large treatment networks or fit complex network meta-analysis models, please seek advice from technical experts."),
                                    br(),    
                                    p("THE SOFTWARE IS PROVIDED AS IS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT 
                                       NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
                                       IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
                                       WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE 
                                       OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."),
                                     wellPanel(
                                       fluidRow( 
                                         column(3, img(src='CRSUlogo.jpg', width=250, height=125)),
                                         column(9, tags$div(class="header", checked=NA,
                                                            
                                                            tags$p("Please click ", tags$a(href="http://www.nihrcrsu.org", "here ", target="_blank"), "for more information about the Complex Reviews Support Unit (CRSU)")
                                                            
                                         )
                                         )
                                       )
                                     )
),


#########################
### Tab 2 - Load data ###
#########################

# Within the load data tab let users select a file to upload, the upload happens in a sidebarPanel on
# the left and the mainPanel will show the data once file uploaded. Code to show data is in the server 
# section below

tabPanel("Load Data",
         
         sidebarLayout(
           sidebarPanel(
             h4(tags$strong("Step 1 - Please select a data file (.csv) to upload")),
             br(),
             p(tags$strong("Note: Excel files should be saved in 'csv (Comma delimited) (*.csv)' format. Default maximum file size is 5MB.")),
             fileInput(inputId="data", label="", buttonLabel="Select", placeholder="No file selected"),
             br(),
             tags$hr(),
             #checkboxInput(inputId = "header", label = "First row as column headings", value = TRUE),
             #checkboxInput(inputId = "stringAsFacSteptors", label = "stringAsfactors", FALSE),
             #radioButtons(inputId = "sep", label="File Delimiter", choices=c(Comma=",", Semicolon=";", Tab="\t", Space= " "), selected=","),
             h4(tags$strong("Step 2 - Please copy and paste the treatment labels")),
             br(),
             p(tags$strong("Note: The first row must be 'Number' tabspace 'Label' as shown in the pre-loaded format, case sensitive.")),
             p(tags$strong("      Treatment names may only contain letters, digits, and underscore (_).")),
             br(), 
            aceEditor("listCont2", value="Number\tLabel
1\tPlacebo
2\tOrlistat
3\tSibutramine
4\tMetformin
5\tOrli_Sibut
6\tRimonbant", mode="r", theme="eclipse")
             ),
           mainPanel(
             tabsetPanel(id="instructions",
               tabPanel("Long format upload", 
                        h2(tags$strong("Instructions for uploading long format data")),
                        br(),
                        p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for long format data, where each row contains one treatment arm. Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                                      Instructions are as below.")),
                        h4(tags$strong("Step 1:")),
                        p(),
                        p("The long format data file should contain six columns. Headings of columns are case sensitive."), 
                        p(tags$ul(tags$li("The", tags$strong("first"), "column should be labelled", tags$strong("StudyID"), "and contain the study identifier, starting from 1, then 2, 3, 4... etc."))),
                        p(tags$ul(tags$li("The", tags$strong("second"), "column should be labelled", tags$strong("Study"), "and contain the name (e.g., author,year) of the study. The study name must be unique for each study."))),
                        p(tags$ul(tags$li("The", tags$strong("third"), "column should be labelled", tags$strong("T"), "and contain the numerical treatment code used in each arm of the study.", 
                                          tags$strong("If applicable, your reference treatment (e.g. Placebo/Control)"), tags$strong(tags$u("needs to be labelled as 1."))))),
                        p(tags$ul(tags$li("The", tags$strong("fourth"), "column should be labelled", tags$strong("N"), "and contain the number of participants in each arm of the study."))),
                        p(tags$ul(tags$li("The", tags$strong("fifth"), "column should be labelled", tags$strong("Mean"), "and contain the mean value of the outcome in each arm of the study."))),
                        p(tags$ul(tags$li("The", tags$strong("sixth"), "column should be labelled", tags$strong("SD"), "and contain the standard deviation of the outcome in each arm of the study."))),
                        p("An example of this structure can be seen in the", tags$strong("'View Data'"), "tab."),
                        p("The csv file that is used to produce the example dataset can be downloaded from here:"),
                        # Button
                        downloadButton("downloadData", "Download the example dataset in long format"),
                        br(),
                        br(),
                        h4(tags$strong("Step 2:")),
                        p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
                        p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
                        p("The default 'treatment labels' text file can be downloaded from here:"),
                        # Button
                        downloadButton("downloadlabel", "Download the example 'treatment labels' text file"),
                        br(),
                        p(),
                        p("This default dataset is from Gray, LJ. et al. A systematic review and mixed treatment 
                          comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498."),
                        br(),
                        p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data' tab will automatically update once a file is successfully loaded."))
                        ),
               
               tabPanel("Wide format upload",
                        h2(tags$strong("Instructions for uploading wide format data")),
                        br(),
                        p(tags$strong("MetaInsight allows data in either long format, or wide format. This tab provides instructions for wide format data, where each row contains all the treatment arms from one study. Please follow Steps 1 and 2 to upload the data file and enter the treatment labels. 
                                      Instructions are as below.")),
                        h4(tags$strong("Step 1:")),
                        p("Your data needs to have exactly the same variable names as in the example data which can be downloaded from here:"),
                        # Button
                        downloadButton("downloadDataWide", "Download the example dataset in wide format"),
                        br(),
                        p("Headings of columns are case sensitive."),
                        p(tags$ul(tags$li(tags$strong("StudyID"), "contains study identifier, starting from 1, then 2, 3, 4... etc."))),
                        p(tags$ul(tags$li(tags$strong("Study"), "contains name (e.g., author,year) of the study. The study name must be unique for each study."))),
                        p(tags$ul(tags$li(tags$strong("T.1, T.2, ..., up to T.6"), "contains treatment given for study arm 1, 2, ..., up to 6, respectively given as a numerical code"))),
                        p(tags$ul(tags$li(tags$strong("N.1, N.2, ..., up to N.6"), "contains number of participants in study arm 1, 2, ..., up to 6, respectively"))),
                        p(tags$ul(tags$li(tags$strong("Mean.1, Mean.2, ..., up to Mean.6"), "contains the mean value of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
                        p(tags$ul(tags$li(tags$strong("SD.1, SD.2, ..., up to SD.6"), "contains standard deviation of the outcome in study arm 1, 2, ..., up to 6, respectively"))),
                        p(tags$strong("Note: If applicable, your reference treatment (e.g. Placebo/Control)", 
                                      tags$u("needs to be labelled as treatment 1"))),
                        p(tags$strong("      The maximum number of arms for each trial allowed in the MetaInsight app is 6.")),
                        br(),
                        h4(tags$strong("Step 2:")),
                        p("Enter the labels to match with the numerical treatment codes in the data file. Labels should be short to allow for clear display on figures."),
                        p("Data can be copy and pasted from Excel or another tab separated file such as '.txt'"),
                        p("The default 'treatment labels' text file can be downloaded from here:"),
                        # Button
                        downloadButton("downloadlabel2", "Download the example 'treatment labels' text file"),
                        br(),
                        p(),
                        p("This default dataset is from Gray, LJ. et al. A systematic review and mixed treatment 
                          comparison of pharmacological interventions for the treatment of obesity. Obesity reviews 13.6 (2012): 483-498."),
                        br(),
                        p(tags$strong("Note: The default dataset, pre-loaded on the 'View Data' tab, and its pre-loaded treatment labels 
                                      will be used for analysis if no file is selected or no treatment labels are pasted. The 'View Data'
                                      tab will automatically update once a file is successfully loaded."))
                        ),
               
               tabPanel("View Data", 
                        p("Please double check if the total number of treatments matches the total number of treatment labels, 
                          i.e. make sure each treatment code in the data has a corresponding treatment label, 
                          and there is no additional treatment label which does not exist in the data."),
                        uiOutput("tb"))#,
               #tabPanel("Default Data", tableOutput("defaultTable"))
               )
           )
         )),

tabPanel("Data analysis",
         sidebarLayout(
           # Sidebar with a checkbox group input to select studies to exclude
           sidebarPanel(
             radioButtons("outcomeCont", "Outcome for continuous data:", c("Mean Difference (MD)" = "MD","Standardised Mean Difference (SMD)" = "SMD")),
             radioButtons('rankoptsCont', 'For treatment rankings, smaller outcome values are:', c("Desirable" = "good", "Undesirable" = "bad")),  
             radioButtons("modelCont", "Model:", c("Random effect (RE)" = "RE", "Fixed effect (FE)" = "FE")),
             h3("Select studies to exclude:"),
             uiOutput("ChoicesCont2"), h5("NB: If a whole treatment is removed from the analysis the NMA will return an error message. To overcome this, please remove the treatment from the data."), width = 3
           ),
           
           mainPanel(
             tags$style(HTML("
                  .tabbable > .nav > li > a                  {background-color: white;  color:#2196c4}
                  .tabbable > .nav > li > a[data-value='1. Data summary'] {background-color: #2196c4;  color:white; font-size: 18px}
                  .tabbable > .nav > li > a[data-value='1a. Study Results'] {background-color: white;}
                  .tabbable > .nav > li > a[data-value='1b. Network Plot'] {background-color: white;}
                  .tabbable > .nav > li > a[data-value='2. Frequentist network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
                  .tabbable > .nav > li > a[data-value='2a. Forest Plot'] {background-color: white}
                  .tabbable > .nav > li > a[data-value='2b. Comparison of all treatment pairs'] {background-color: white;}
                  .tabbable > .nav > li > a[data-value='2c. Inconsistency'] {background-color: white;}
                  .tabbable > .nav > li > a[data-value='3. Bayesian network meta-analysis'] {background-color: #2196c4;   color:white; font-size: 18px}
                  .tabbable > .nav > li[class=active]    > a {font-weight:900;font-style: italic;text-decoration: underline }
                  ")),
             tabsetPanel(
               tabPanel("1. Data summary", tabsetPanel(
                        tabPanel("1a. Study Results", plotOutput("forestPlotCont", height = "1000px", width = "800px"), radioButtons('format_freq0', 'Document format', c('PDF', 'SVG'), inline = TRUE),  downloadButton('downloadStudyCont')),
                        tabPanel("1b. Network Plot",
                        column(6, plotOutput("netGraphStaticCont"), radioButtons('format_freq1', 'Document format', c('PDF', 'PNG'), inline = TRUE),
                               
                               downloadButton('downloadNetworkCont'), 
                               br(),
                               br(),
                               br(),
                               verbatimTextOutput("netconnect")),
                        column(6, plotOutput("netGraphUpdatingCont"), radioButtons('format_freq2', 'Document format', c('PDF', 'PNG'), inline = TRUE),
                               downloadButton('downloadNetworkUpdateCont'), 
                               br(),
                               br(),
                               br(),
                               verbatimTextOutput("netconnect_sub"))
                        ) 
               )),
               tabPanel("2. Frequentist network meta-analysis", tabsetPanel(
                        tabPanel("2a. Forest Plot",
                            column(6, plotOutput("ComparisonCont2", height = "400px", width = "400px"), textOutput("textcompcont"), textOutput("refcont4"), radioButtons('format_freq3', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadCompCont2')
                              ),
                            column(6, plotOutput("SFPUpdatingContComp", height = "400px", width = "400px"), textOutput("textcont5"), textOutput("refcont3"), radioButtons('format_freq4', 'Document format', c('PDF', 'PNG'), inline = TRUE), downloadButton('downloadCompCont'))
                             ),
                        tabPanel("2b. Comparison of all treatment pairs",
                            helpText("Treatments are ranked from best to worst along the leading diagonal. Above the leading diagonal are estimates from pairwise meta-analyses, below the leading diagonal are estimates from network meta-analyses"),
                            helpText("Relative treatment effects in ranked order for all studies"), tableOutput("rankChartStaticCont"), downloadButton('downloadRankCont', "Download"),
                            helpText("Relative treatment effects in ranked order with studies excluded"), tableOutput("rankChartUpdatingCont"), downloadButton('downloadRankUpdateCont')),
                        tabPanel("2c. Inconsistency", 
                            helpText("Assessment of inconsistency for all studies"), 
                            tableOutput("Incon1"), downloadButton('downloadInconCont', "Download"),
                            helpText("Assessment of inconsistency with studies excluded"), 
                            tableOutput("Incon2"), downloadButton('downloadInconCont2', "Download")
                        )
             )
           ),
           tabPanel("3. Bayesian network meta-analysis", tabsetPanel(
             tabPanel("3a. Forest plot",
                      helpText("Baysesian result using the gemtc package.Please note the outcome for continuous data has to be mean difference for the Bayesian analysis. 
                               Standardised mean difference cannot be analysed.", tags$strong("Please note each simulation may take 20 seconds.", style="color:#FF0000")),
                      fixedRow(
                        column(6, align = "center",
                               p(tags$strong("Results for all studies")),
                               p("Please click the button below to run Bayesian analysis for all studies, and after each time when you change the radiobutton selections."),
                               actionButton("baye_do", "Click here to run the main analysis for all studies")
                        ),
                        column(6, align = "center",
                               p(tags$strong("Results with studies excluded")),
                               p("Please click the button below to run each time after you finish the selection of studies, or change the radiobutton selections."),
                               actionButton("sub_do", "Click here to run the sensitivity analysis")
                        )),
                      fixedRow(
                        column(6, align = "center",
                               plotOutput("gemtc"),
                               p("Model fit:"),
                               tableOutput("dic"),
                               textOutput("textcont_gemtc"),
                               br(),
                               br(),
                               radioButtons('format2', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                               downloadButton('downloadBaye_plot')
                        ),
                        column(6, align = "center",
                               plotOutput("gemtc_sub"),
                               p("Model fit:"),
                               tableOutput("dic_sub"),
                               textOutput("textcont_gemtc_sub"),
                               br(),
                               br(),
                               radioButtons('format4', 'Document format', c('PDF', 'PNG'), inline = TRUE), 
                               downloadButton('downloadBaye_plot_sub')
                        )
                      )
                      ),
             tabPanel("3b. Comparison of all treatment pairs",
                      helpText("Please note: if you change the selections on the sidebar, 
                                         you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
                      p(tags$strong("In contrast to the 'comparison of all treatment pairs' tab in the frequentist NMA results, 
                        this table only contains the estimates from the network meta analysis, 
                        i.e. does not contain estimates from pairwise meta-analysis which only contains direct evidence. 
                                    If you would like to obtain the pairwise meta-analysis results, please run 3d. Nodesplit model")),
                      br(),
                      p(tags$strong("Treatment effects for all studies: comparison of all treatment pairs.")),
                      tableOutput("baye_comparison"),
                      downloadButton('downloadbaye_comparison'),
                      br(),
                      br(),
                      p(tags$strong("Treatment effects with studies excluded: comparison of all treatment pairs.")),
                      tableOutput("baye_comparison_sub"),
                      downloadButton('downloadbaye_comparison_sub')
),
             tabPanel("3c. Ranking table", 
                      helpText("Please note: if you change the selections on the sidebar, 
                                         you will need to re-run the primary and/or sensitivity analysis from the 'Forest Plot' page."),
                      fixedRow(
                        column(6, align = "center",
                               p(tags$strong("Ranking table for all studies - Probability for each treatment to be the best")),
                               #tableOutput("prob"),
                               div(tableOutput("prob"), style = "font-size:100%"),
                               downloadButton('downloadBaye_rank')
                        ),
                        column(6, align = "center",
                               plotOutput("gemtc_rank")
                        )),
                      fixedRow(
                        column(6, align = "center",
                               p(tags$strong("Ranking table with studies excluded - Probability for each treatment to be the best")),
                               div(tableOutput("prob_sub"), style = "font-size:100%"),
                               downloadButton('downloadBaye_rank_sub')
                        ),
                        column(6, align = "center",
                               plotOutput("gemtc_rank_sub")
                               
                        ))
             ),
            tabPanel("3d. Nodesplit model", 
                     helpText("Please note: This may take a few minutes depending on the number of treatment options. 
                         In some circumstances, e.g. number of treatments is larger than 6, running notesplit model may cause the app to disconnect.
                              If this problem persists, please contact the developers"),
                     fluidRow(
                       column(6,
                              p(tags$strong("Inconsistency test with notesplitting model for all studies")),
                              actionButton("node", "Click here to run the nodesplitting analysis for all studies"),
                              tableOutput("node_table"),
                              downloadButton('downloadnode')
                       ),
                       column(6,
                              p(tags$strong("Inconsistency test with notesplitting model with studies excluded")),
                              actionButton("node_sub", "Click here to run the nodesplitting analysis with studies excluded"),
                              tableOutput("node_table_sub"),
                              downloadButton('downloadnode_sub')
                       ))
            ),
            tabPanel("3e. Bayesian result details",
                    fluidRow(
                       column(6,
                          p(tags$strong("Results details for all studies")),
                          verbatimTextOutput("gemtc_results"),
                          p(tags$strong("Gelman convergence assessment plot for all studies")),
                          plotOutput("gemtc_gelman")
                          ),
                      column(6,
                          p(tags$strong("Results details with studies excluded")),
                          verbatimTextOutput("gemtc_results_sub"),
                          p(tags$strong("Gelman convergence assessment plot with studies excluded")),
                          plotOutput("gemtc_gelman_sub"))
         )),        
              tabPanel("3f. Deviance report",
                             p(tags$strong("Deviance report for all studies and the sensitivity analysis")),
                       fluidRow(
                         column(6,
                                p(tags$strong("residual deviance from NMA model and UME inconsistency model for all studies")),
                                plotlyOutput("dev_scat")),
                         column(6,
                                p(tags$strong("residual deviance from NMA model and UME inconsistency model with studies excluded")),
                                plotlyOutput("dev_scat_sub")
                         )),
                         p("This plot represents each data points' contribution to the residual deviance for the 
                        NMA with consistency (horizontal axis) and the unrelated mean effect (ume) inconsistency models 
                        (vertical axis) along with the line of equality. The points on the equality line means there is no
                        improvement in model fit when using the inconsistency model, suggesting that there is no evidence of inconsistency. 
                        Points above the equality line means they have a smaller residual deviance for the consistency model indicating a 
                        better fit in the NMA consistency model and points below the equality line
                        means they have a better fit in the ume inconsistency model. Please note that the unrelated mean effects model 
                        may not handle multi-arm trials correctly. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
                          decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
                         br(),
                         br(),
                         br(),
                       
                       fluidRow(
                               column(6,
                                      p(tags$strong("Per-arm residual deviance for all studies")),
                                      plotlyOutput("dev1")),
                               column(6,
                                      p(tags$strong("Per-arm residual deviance for sensitivity analysis")),
                                      plotlyOutput("dev1_sub")
                               ),
                               
                               br(),
                               
                          p("This stem plot represents the posterior residual deviance per study arm. The total number of stems equals 
                                 to the total number of data points in the network meta analysis. Each stem is corresponding to each arm in 
                                 each study in the deviance results below ($dev.ab) (through which you can identify which stem corresponds 
                                 to which study arm). The smaller residual deviance (the shorter stem), dev.ab, the better model fit for each 
                                 data point. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
                                 decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd.)"),
                               br(),
                               br(),
                               br(),
                               column(6,
                                      p(tags$strong("Leverage plot for all studies")),
                                      plotlyOutput("dev2")
                               ),
                               column(6,
                                      p(tags$strong("Leverage plot for sensitivity analysis")),
                                      plotlyOutput("dev2_sub"))
                               ),
                             br(),
                             p("This leverage plot shows the average leverage of the data point across the arms for each study 
                             (sum($lev.ab) for each study/number of arms for each study) versus the square root of the average residual 
                               deviance across the arms for each study (sum($dev.ab) for each study / number of arms for each study). 
                               The leverage for each data point, is calculated as the posterior mean of the residual 
                               deviance, minus the deviance at the posterior mean of the fitted values. The leverage plot may be used to 
                               identify influential and/or poorly fitting studies and can be used to check how each study is affecting 
                               the overall model fit and DIC. Curves of the form x2 + y = c, c = 1, 2, 3, ., where x represents square root 
                               of residual deviance, and y represents the leverage, are marked on the plot. Points lying on such parabolas 
                               each contribute an amount c to the DIC (Spiegelhalter et al., 2002). Points that lie outside the line with 
                               c = 3 can generally be identified as contributing to the model's poor fit. Points with a high leverage are 
                               influential, which means that they have a strong influence on the model parameters that generate their fitted 
                               values. (Further reading: Dias S, Ades AE, Welton NJ, Jansen JP, Sutton AJ. Network meta-anlaysis for 
                               decision-making. Chapter 3 Model fit, model comparison and outlier detection. @2018 John Wiley & Sons Ltd. 
                               Spiegelhalter et al. (2002) Bayesian measures of model complexity and fit. J. R. Statist. Soc.B 64, Part4, 
                               pp.583-639)"),
                              br(),
                              br()
                             ),

         tabPanel("3g. Model details", tabsetPanel(
                  tabPanel("3g-1. Model codes",
                          p(tags$strong("Model codes for analysis of all studies")),
                          downloadButton('download_code'),
                          verbatimTextOutput("code")
                          ),
                  tabPanel("3g-2. Initial values",
                          p(tags$strong("Initial values")),
                          downloadButton('download_inits_1', "Download initial values for chain 1"),
                          downloadButton('download_inits_2', "Download initial values for chain 2"),
                          downloadButton('download_inits_3', "Download initial values for chain 3"),
                          downloadButton('download_inits_4', "Download initial values for chain 4"),
                          verbatimTextOutput("inits")
                          ),
                  tabPanel("3g-3. Download simulations",
                           p(tags$strong("Download simulated data")),
                           downloadButton('download_data1', "Download data from chain 1"),
                           br(),
                           downloadButton('download_data2', "Download data from chain 2"),
                           br(),
                           downloadButton('download_data3', "Download data from chain 3"),
                           br(),
                           downloadButton('download_data4', "Download data from chain 4")
                  ),
                  tabPanel("3g-4. Deviance details",
                  column(6,
                         p(tags$strong("Deviance data for all studies")),
                         verbatimTextOutput("dev"),
                         verbatimTextOutput("dev_ume")
                         ),
                  column(6,
                         p(tags$strong("Deviance data for sensitivity analysis")),
                         verbatimTextOutput("dev_sub")),

                  
                  )

))



         )))))),

tabPanel(id="history", "Full update history",
         fluidRow(column(8,
                         p(tags$strong("** New features updated on 15 Nov 2019 (v2.1) ** :")),
                         p(tags$ul(tags$li("Network connectivity information is now shown on the '1b. Network Plot' tab"))),
                         p(tags$ul(tags$li("A plot of residual deviance from NMA model and UME inconsistency model is now displayed on the '3f.deviance report' tab to assist users to determine the model fit"))),
                        
                         p(tags$strong("** New features updated on 19 August 2019 (v2) ** :")),
                         p("Long and wide format data uploading:"),
                         p(tags$ul(tags$li("This version now allows uploading data in 'long' format (1 study arm per row), in addition to the 'wide' 
                                           format (1 whole study per row). The app will automatically detect the data format and prepare the data for 
                                           analysis accordingly. Instructions for uploading data are on the 'Load data' tab with subtabs for formatting 
                                           'long' and 'wide' data, respectively."))),
                         p(tags$ul(tags$li("Example datasets with associated treatment label files can now be downloaded which users can adapt to create
                                           files of their data in the correct format for the app."))),
                         p(tags$ul(tags$li("Preparing the 'wide' format data is now more flexible: when there are less than 6 treatment arms, the users 
                                           just need to input the headings up to their maximum number of arms, i.e., you do not need to add the empty 
                                           columns with headings in the data anymore."))),
                         p(tags$ul(tags$li(tags$strong("For existing users (breaking change)"),": Please note that the ", tags$strong("required heading names have been changed"),
                                           "slightly compared to the last version so you will need to change the names of the headings in your existing data files accordingly. 
                                           Please see instructions on the 'Load data' page."))),
                         p("2. Bayesian analysis:"),
                         p(tags$ul(tags$li("This app now allows Bayesian network meta-analysis, 
                                           in addition to the existing frequentist network meta-analysis."))),
                         p(tags$ul(tags$li("The Bayesian network meta-analysis uses the R gemtc package, 
                                           which can generate forest plots of the results, generate comparisons of 
                                           relative treatment effects for all possible treatment pairs from the NMA, provide a ranking 
                                           table and a median rank chart based on the probability for each treatment to be the best, 
                                           perform nodesplit models to compare direct estimates and indirect estimates,
                                           Check the model convergence using the Gelman-Rubin convergence assessment 
                                           plot, check model fit by producing the deviance reports, and provide model details including model code, 
                                           and initial values and simulations ready for download.
                                           ")))
                         ))),
tabPanel(id="privacy", "Privacy notice",
         #img(src='gdpr.jpg', width=1000, height=1500, align = "center")
         tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                     src="gdpr.pdf")
)
)
)
