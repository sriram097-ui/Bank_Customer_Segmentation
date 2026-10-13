# Bank Customer Segmentation — Professional R Dashboard

A multi-page Shiny app for VS Code with interactive filters, executive KPIs, K-means segmentation, PCA projection, portfolio analysis, engagement/risk views, recommendations and an exportable data table.

## Run it
1. Install [R](https://cran.r-project.org/) and the R extension in VS Code.
2. Open this extracted folder in VS Code.
3. In the VS Code terminal, run:

```r
install.packages(c("shiny","shinydashboard","DT","plotly","dplyr","readr","ggplot2","scales"))
shiny::runApp()
```

## Add your own dataset
Save your CSV as `data/bank_customers.csv`. A synthetic demo dataset is included at `data/bank_customers.csv`, so the dashboard runs immediately. The data is fabricated for demonstration and is not real customer information. If you remove the CSV, the app also falls back to generated demo data.

Recognized common columns: `CustomerID`/`CustomerId`/`id`, `Age`, `AnnualIncome`/`EstimatedSalary`/`Income`, `Tenure`/`YearsAtBank`, `Balance`, `NumOfProducts`/`Products`, `CreditScore`, `IsActiveMember`/`ActiveMember`, `HasCreditCard`, `Region`/`Geography`/`Country`, `ChurnRisk`/`Churn`/`Exited`.

Some datasets use different names or values; adjust the mapping section near the top of `app.R` if needed. Demo results are not real bank data.

## Segmentation
K-means runs on standardized numeric fields. PCA is for visualization only. Segment labels are not automatically business personas; inspect profiles and validate stability before acting on results.

## Data safety
Use anonymized data. Do not share account numbers, card details, passwords or directly identifying customer information. Add authentication and access controls before deploying this app.
