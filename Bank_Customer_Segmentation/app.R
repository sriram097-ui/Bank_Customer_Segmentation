required <- c("shiny","shinydashboard","DT","plotly","dplyr","readr","ggplot2","scales")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(missing)) stop("Install packages with: install.packages(c(", paste(sprintf('"%s"',missing),collapse=", "), "))")
library(shiny); library(shinydashboard); library(DT); library(plotly)
library(dplyr); library(readr); library(ggplot2); library(scales)

DATA_FILE <- "data/bank_customers.csv"
demo_data <- function(n=1200, seed=42) {
  set.seed(seed)
  tibble(CustomerID=sprintf("C%05d",1:n),
    Age=pmin(pmax(round(rnorm(n,42,12)),18),85),
    AnnualIncome=round(exp(rnorm(n,log(62000),.48))),
    Tenure=pmin(pmax(round(rnorm(n,7,4)),0),30),
    Balance=round(pmax(0,rlnorm(n,log(18000),1)),2),
    NumOfProducts=pmin(pmax(rpois(n,2.1)+1,1),6),
    CreditScore=pmin(pmax(round(rnorm(n,690,55)),300),850),
    IsActiveMember=sample(c("Active","Inactive"),n,TRUE,c(.72,.28)),
    HasCreditCard=sample(c("Yes","No"),n,TRUE,c(.68,.32)),
    Region=sample(c("North","South","East","West","Central"),n,TRUE),
    ChurnRisk=sample(c("Low","Medium","High"),n,TRUE,c(.55,.30,.15)))
}
if(file.exists(DATA_FILE)) {
  data <- tryCatch(read_csv(DATA_FILE,show_col_types=FALSE),error=function(e) demo_data())
  if(!nrow(data)) data <- demo_data()
} else data <- demo_data()
names(data) <- make.names(names(data),unique=TRUE)
map_names <- list(CustomerID=c("CustomerID","CustomerId","customer_id","id"),
 Age=c("Age","age"), AnnualIncome=c("AnnualIncome","EstimatedSalary","Income","annual_income"),
 Tenure=c("Tenure","YearsAtBank","YearsWithBank"), Balance=c("Balance","AccountBalance"),
 NumOfProducts=c("NumOfProducts","Products","NumProducts"), CreditScore=c("CreditScore","credit_score","Score"),
 IsActiveMember=c("IsActiveMember","ActiveMember","IsActive","Active"), HasCreditCard=c("HasCreditCard","CreditCard","HasCard"),
 Region=c("Region","Geography","Country"), ChurnRisk=c("ChurnRisk","Churn","Exited","Risk"))
for(target in names(map_names)) {
  if(!target %in% names(data)) {
    hit <- intersect(make.names(map_names[[target]]),names(data))
    if(length(hit)) names(data)[match(hit[1],names(data))] <- target
  }
}
if(!"CustomerID"%in%names(data)) data$CustomerID <- sprintf("C%05d",seq_len(nrow(data)))
for(nm in c("Age","AnnualIncome","Tenure","Balance","NumOfProducts","CreditScore"))
  if(!nm%in%names(data)) data[[nm]] <- NA_real_ else data[[nm]] <- suppressWarnings(as.numeric(data[[nm]]))
for(nm in c("IsActiveMember","HasCreditCard","Region","ChurnRisk"))
  if(!nm%in%names(data)) data[[nm]] <- "Unknown" else data[[nm]] <- as.character(data[[nm]])

ui <- dashboardPage(skin="blue",
 dashboardHeader(title="Bank Customer Intelligence",titleWidth=290),
 dashboardSidebar(width=290,sidebarMenu(
  menuItem("Executive Overview",tabName="overview",icon=icon("tachometer-alt")),
  menuItem("Customer Segments",tabName="segments",icon=icon("users")),
  menuItem("Portfolio Explorer",tabName="portfolio",icon=icon("chart-pie")),
  menuItem("Risk & Engagement",tabName="risk",icon=icon("shield-alt")),
  menuItem("Customer Data",tabName="data",icon=icon("table"))),
  tags$hr(),h4("Filters",style="padding-left:15px;color:#d9e7f2;"),
  selectInput("region","Region",choices=c("All regions",sort(unique(data$Region)))),
  sliderInput("age","Customer age",min=18,max=85,value=c(18,85)),
  selectInput("activity","Membership status",choices=c("All",sort(unique(data$IsActiveMember)))),
  actionButton("reset","Reset filters",icon=icon("undo"),class="btn-primary")),
 dashboardBody(tags$style(HTML("
 .content-wrapper,.right-side{background:#f3f6fb}.main-header .logo{font-weight:700}
 .small-box,.box{border-radius:12px;box-shadow:0 5px 18px rgba(28,48,74,.08)}
 .content{padding:20px}.hero{padding:20px 24px;background:linear-gradient(120deg,#102a43,#176b87);color:white;border-radius:14px;margin-bottom:18px}
 .hero h2{margin-top:0;font-weight:700}.skin-blue .main-sidebar{background:#102a43}
 .skin-blue .main-header .logo,.skin-blue .main-header .navbar{background:#0b1f33}
 .skin-blue .sidebar-menu>li.active>a{border-left-color:#39c6c8}")),
 tabItems(
  tabItem(tabName="overview",
   fluidRow(box(width=12,solidHeader=FALSE,div(class="hero",h2("Bank Customer Segmentation"),p("A decision-ready view of customer value, engagement, portfolio mix and retention opportunities.")))),
   fluidRow(valueBoxOutput("n",3),valueBoxOutput("bal",3),valueBoxOutput("income",3),valueBoxOutput("active",3)),
   fluidRow(box(title="Customer profile by age",width=6,status="primary",plotlyOutput("ageplot",height=300)),
            box(title="Balance distribution",width=6,status="primary",plotlyOutput("balplot",height=300))),
   fluidRow(box(title="Customer mix by region",width=6,status="primary",plotlyOutput("regionplot",height=300)),
            box(title="Products held",width=6,status="primary",plotlyOutput("productsplot",height=300)))),
  tabItem(tabName="segments",
   fluidRow(box(title="Segmentation controls",width=12,status="primary",
    p("K-means uses standardized numeric attributes. Review segment profiles before assigning business personas."),
    sliderInput("k","Number of segments (K)",2,6,4),actionButton("recluster","Recalculate",icon=icon("project-diagram")))),
   fluidRow(box(title="Segment size",width=7,status="primary",plotlyOutput("segmentplot",height=340)),
            box(title="Segment profile",width=5,status="primary",DTOutput("segmenttable"))),
   fluidRow(box(title="Customer PCA projection",width=12,status="primary",plotlyOutput("pcaplot",height=400)))),
  tabItem(tabName="portfolio",
   fluidRow(box(title="Income vs. account balance",width=7,status="primary",plotlyOutput("scatter",height=360)),
            box(title="Credit score distribution",width=5,status="primary",plotlyOutput("credit",height=360))),
   fluidRow(box(title="Tenure and balance",width=6,status="primary",plotlyOutput("tenure",height=320)),
            box(title="Products by membership status",width=6,status="primary",plotlyOutput("activeproducts",height=320)))),
  tabItem(tabName="risk",
   fluidRow(valueBoxOutput("highrisk",3),valueBoxOutput("multi",3),valueBoxOutput("lowbal",3),valueBoxOutput("tenurekpi",3)),
   fluidRow(box(title="Risk indicator mix",width=6,status="primary",plotlyOutput("riskplot",height=320)),
            box(title="Engagement by region",width=6,status="primary",plotlyOutput("engagement",height=320))),
   fluidRow(box(title="Actionable recommendations",width=12,status="primary",uiOutput("recommendations")))),
  tabItem(tabName="data",fluidRow(box(title="Customer records",width=12,status="primary",
   p("Search, sort and export filtered records using the table controls."),DTOutput("table"))))
 )))
server <- function(input,output,session) {
 filtered <- reactive({
  d <- data
  if(!is.null(input$region)&&input$region!="All regions") d <- d[d$Region==input$region,,drop=FALSE]
  if(!is.null(input$age)) d <- d[is.na(d$Age)|(d$Age>=input$age[1]&d$Age<=input$age[2]),,drop=FALSE]
  if(!is.null(input$activity)&&input$activity!="All") d <- d[d$IsActiveMember==input$activity,,drop=FALSE]
  d
 })
 observeEvent(input$reset,{updateSelectInput(session,"region",selected="All regions");updateSliderInput(session,"age",value=c(18,85));updateSelectInput(session,"activity",selected="All")})
 clustered <- reactive({
  d<-filtered(); vars<-intersect(c("Age","AnnualIncome","Tenure","Balance","NumOfProducts","CreditScore"),names(d))
  x<-d[,vars,drop=FALSE]; x<-x[,vapply(x,function(z)sum(!is.na(z))>2&&length(unique(na.omit(z)))>1,logical(1)),drop=FALSE]
  if(nrow(d)<input$k+1||ncol(x)<2){d$Segment<-"Segment 1";return(list(data=d,pca=data.frame()))}
  for(nm in names(x)) x[[nm]][is.na(x[[nm]])]<-median(x[[nm]],na.rm=TRUE)
  x<-x[,vapply(x,function(z)sd(z)>0,logical(1)),drop=FALSE]
  if(ncol(x)<2){d$Segment<-"Segment 1";return(list(data=d,pca=data.frame()))}
  xs<-scale(x);set.seed(202);km<-kmeans(xs,centers=min(input$k,nrow(d)-1),nstart=20)
  d$Segment<-factor(paste("Segment",km$cluster))
  pc<-as.data.frame(prcomp(xs)$x[,1:2,drop=FALSE]);names(pc)<-c("PC1","PC2");pc$Segment<-d$Segment;pc$CustomerID<-d$CustomerID
  list(data=d,pca=pc)
 })
 output$n<-renderValueBox(valueBox(comma(nrow(filtered())),"Customers in view",icon=icon("users"),color="aqua"))
 output$bal<-renderValueBox({d<-filtered();valueBox(if(all(is.na(d$Balance)))"N/A" else dollar(sum(d$Balance,na.rm=TRUE)),"Total account balance",icon=icon("wallet"),color="green")})
 output$income<-renderValueBox({d<-filtered();valueBox(if(all(is.na(d$AnnualIncome)))"N/A" else dollar(mean(d$AnnualIncome,na.rm=TRUE)),"Average annual income",icon=icon("chart-line"),color="purple")})
 output$active<-renderValueBox({d<-filtered();valueBox(percent(mean(tolower(d$IsActiveMember)%in%c("active","1","yes","true"),na.rm=TRUE)),"Active members",icon=icon("user-check"),color="teal")})
 output$highrisk<-renderValueBox({d<-filtered();valueBox(comma(sum(tolower(d$ChurnRisk)%in%c("high","1","yes","true"),na.rm=TRUE)),"High-risk customers",icon=icon("exclamation-triangle"),color="red")})
 output$multi<-renderValueBox({d<-filtered();valueBox(comma(sum(d$NumOfProducts>=3,na.rm=TRUE)),"Customers with 3+ products",icon=icon("layer-group"),color="purple")})
 output$lowbal<-renderValueBox({d<-filtered();q<-if(all(is.na(d$Balance)))0 else quantile(d$Balance,.25,na.rm=TRUE);valueBox(comma(sum(d$Balance<=q,na.rm=TRUE)),"Lower-quartile balances",icon=icon("coins"),color="yellow")})
 output$tenurekpi<-renderValueBox({d<-filtered();valueBox(if(all(is.na(d$Tenure)))"N/A" else paste0(round(mean(d$Tenure,na.rm=TRUE),1)," yrs"),"Average tenure",icon=icon("calendar-alt"),color="aqua")})
 output$ageplot<-renderPlotly({d<-filtered();ggplotly(ggplot(d,aes(Age))+geom_histogram(binwidth=5,fill="#176B87",color="white")+labs(x="Age",y="Customers")+theme_minimal())})
 output$balplot<-renderPlotly({d<-filtered()%>%filter(!is.na(Balance),Balance>0);ggplotly(ggplot(d,aes(Balance))+geom_histogram(bins=35,fill="#39A9A8",color="white")+scale_x_continuous(labels=label_number(scale_cut=cut_short_scale()))+labs(x="Account balance",y="Customers")+theme_minimal())})
 output$regionplot<-renderPlotly({d<-filtered()%>%count(Region,sort=TRUE);ggplotly(ggplot(d,aes(reorder(Region,n),n,fill=Region))+geom_col(show.legend=FALSE)+coord_flip()+labs(x=NULL,y="Customers")+theme_minimal())})
 output$productsplot<-renderPlotly({d<-filtered()%>%count(NumOfProducts);ggplotly(ggplot(d,aes(factor(NumOfProducts),n))+geom_col(fill="#176B87")+labs(x="Number of products",y="Customers")+theme_minimal())})
 output$segmentplot<-renderPlotly({d<-clustered()$data;s<-d%>%count(Segment);ggplotly(ggplot(s,aes(Segment,n,fill=Segment))+geom_col(show.legend=FALSE)+labs(x=NULL,y="Customers")+theme_minimal())})
 output$segmenttable<-renderDT({d<-clustered()$data;out<-d%>%group_by(Segment)%>%summarise(Customers=n(),AvgAge=mean(Age,na.rm=TRUE),AvgIncome=mean(AnnualIncome,na.rm=TRUE),AvgBalance=mean(Balance,na.rm=TRUE),AvgTenure=mean(Tenure,na.rm=TRUE),.groups="drop");datatable(out,rownames=FALSE,options=list(dom="tip",pageLength=6))%>%formatCurrency(c("AvgIncome","AvgBalance"),currency="$",digits=0)%>%formatRound(c("AvgAge","AvgTenure"),1)})
 output$pcaplot<-renderPlotly({p<-clustered()$pca;if(!nrow(p))return(plot_ly()%>%layout(title="Not enough variation for PCA"));plot_ly(p,x=~PC1,y=~PC2,color=~Segment,type="scatter",mode="markers",text=~CustomerID,hovertemplate="Customer %{text}<extra></extra>")})
 output$scatter<-renderPlotly({d<-filtered();ggplotly(ggplot(d,aes(AnnualIncome,Balance,color=Region))+geom_point(alpha=.65)+scale_x_continuous(labels=label_number(scale_cut=cut_short_scale()))+scale_y_continuous(labels=label_number(scale_cut=cut_short_scale()))+labs(x="Annual income",y="Balance")+theme_minimal())})
 output$credit<-renderPlotly({d<-filtered();ggplotly(ggplot(d,aes(CreditScore))+geom_histogram(bins=30,fill="#176B87",color="white")+labs(x="Credit score",y="Customers")+theme_minimal())})
 output$tenure<-renderPlotly({d<-filtered();ggplotly(ggplot(d,aes(Tenure,Balance))+geom_point(alpha=.55,color="#176B87")+geom_smooth(method="loess",se=FALSE,color="#E58E26")+labs(x="Years with bank",y="Balance")+theme_minimal())})
 output$activeproducts<-renderPlotly({d<-filtered()%>%count(IsActiveMember,NumOfProducts);ggplotly(ggplot(d,aes(factor(NumOfProducts),n,fill=IsActiveMember))+geom_col(position="dodge")+labs(x="Products",y="Customers",fill="Member status")+theme_minimal())})
 output$riskplot<-renderPlotly({d<-filtered()%>%count(ChurnRisk,sort=TRUE);ggplotly(ggplot(d,aes(reorder(ChurnRisk,n),n,fill=ChurnRisk))+geom_col(show.legend=FALSE)+coord_flip()+labs(x="Risk category",y="Customers")+theme_minimal())})
 output$engagement<-renderPlotly({d<-filtered()%>%group_by(Region)%>%summarise(rate=mean(tolower(IsActiveMember)%in%c("active","1","yes","true"),na.rm=TRUE),.groups="drop");ggplotly(ggplot(d,aes(reorder(Region,rate),rate))+geom_col(fill="#39A9A8")+coord_flip()+scale_y_continuous(labels=percent)+labs(x=NULL,y="Active-member rate")+theme_minimal())})
 output$recommendations<-renderUI({d<-filtered();ar<-mean(tolower(d$IsActiveMember)%in%c("active","1","yes","true"),na.rm=TRUE);hr<-sum(tolower(d$ChurnRisk)%in%c("high","1","yes","true"),na.rm=TRUE);tagList(
  div(class="callout callout-info",h4("1. Value-based engagement"),p("Compare segment-level balances and income to prioritize appropriate relationship services.")),
  div(class="callout callout-warning",h4("2. Re-engage inactive members"),p(paste0("Observed active-member rate: ",percent(ar,accuracy=.1),". Test tailored digital education and product onboarding."))),
  div(class="callout callout-danger",h4("3. Validate retention risk"),p(paste(hr,"customers are flagged high risk in the current view. Validate the risk labels before any customer outreach."))),
  div(class="callout callout-success",h4("4. Personalize offers responsibly"),p("Use segment profiles to test relevant savings, lending or service propositions; measure outcomes and avoid discriminatory targeting.")) )})
 output$table<-renderDT({datatable(filtered(),rownames=FALSE,filter="top",extensions="Buttons",options=list(dom="Bfrtip",buttons=c("copy","csv","excel"),pageLength=15,scrollX=TRUE))})
}
shinyApp(ui,server)
