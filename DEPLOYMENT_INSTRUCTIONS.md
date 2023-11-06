# Deployment Instructions

This page provides detailed instructions for deploying the Shiny application to shinyapps.io and setting up a custom domain through GoDaddy.

## Deploying to shinyapps.io

Follow these steps to deploy your application to shinyapps.io:

### Set Up shinyapps.io Account

Install the `rsconnect` package, if you haven't already, and load it in R:

```r
install.packages("rsconnect")
library(rsconnect)
```

Configure your shinyapps.io account with the tokens you can obtain from your shinyapps.io dashboard:

```r
# Replace <ACCOUNT_NAME>, <ACCOUNT_TOKEN>, and <ACCOUNT_SECRET> with your specific details from shinyapps.io
rsconnect::setAccountInfo(name='<ACCOUNT_NAME>',
                          token='<ACCOUNT_TOKEN>',
                          secret='<ACCOUNT_SECRET>')
```

Deploy your Shiny application using the `deployApp` function. Be sure to change the directory path to where your Shiny app is located:

```r
# Replace <PATH_TO_YOUR_SHINY_APP> with the path to your Shiny application directory
rsconnect::deployApp('<PATH_TO_YOUR_SHINY_APP>')
```

### Retrieve Your Token from shinyapps.io

To get your token:

1. Log in to your shinyapps.io account.
2. Navigate to the "Tokens" section.
3. Use the provided tokens to replace `<ACCOUNT_TOKEN>` and `<ACCOUNT_SECRET>` in the code above.

## Hosting on GoDaddy

To host your Shiny application with a custom domain purchased from GoDaddy, follow these steps:

1. Log into your GoDaddy account.
2. Navigate to your domain settings.
3. Click on the domain you wish to configure.
4. Access the DNS tab/page from the top navigation bar.
5. Find the 'Forwarding' section and set up the domain forwarding with these settings:
   - Forward to: `https://<YOUR_APP_NAME>.shinyapps.io/<APP_DIRECTORY>` <!-- Replace with your shinyapps.io application URL -->
   - Forward Type: Choose "Forward with masking"
   - Title: Add a title, such as "Ortho Marker Genes"
   - Description: Provide a brief description, like "OMG Browser for Cross Species Single Cell Annotation"
   - Keywords: Add relevant keywords for your application
6. Save your changes to apply the new settings.

After setting up forwarding, your custom domain will point visitors directly to your shinyapps.io application URL.

Please ensure to replace placeholders like `<YOUR_APP_NAME>` and `<APP_DIRECTORY>` with your actual shinyapps.io app name and directory path.

