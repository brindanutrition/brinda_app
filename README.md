# BRINDA APP

This is the working repository for the BRINDA Rshiny app.

## Installation

### RStudio
1. To run this app download the repo with either:

```
git clone https://github.com/BioNomad/BRINDA_app.git
```

or go to `Code` > `Download Zip`

2. Go to RStudio and go to `File` > `Open File`. Now select the `app.R` file in the folder you just downloaded

3. Once the `app.R` file is loaded in RStudio click `Run App` to run 

### VScode

3. (Same step 1 and 2 as above) Run the following bash command in the terminal from the project root directory
  ```bash
  R -e "shiny::runApp('./app.R')"
  ```

## Deployment

1. Create a www.shinyapps.io account if there's not one available.
2. Run the following command in the terminal to install `rsconnect` package.
  ```bash
  install.packages('rsconnect')
  ```
3. If CRAN mirrors setting is prompted to choose, select `USA (OR) [https]` if download is initiated from US west.
4. Run the following command in the terminal to set up the authentication. 
   (The token and secret key could be different if you use a different account)
  ```bash
  rsconnect::setAccountInfo(name='brinda-app',
    token='7600FCD588C7B1297B15EF27BAD5C023',
    secret='<secret key>')
  ```
5. Run the following command in the terminal to deploy the app.
  ```bash
  library(rsconnect)
    rsconnect::deployApp()
  ````

## Video Walkthrough

![](data/video.mp4)

<video width="320" height="240" controls>
  <source src="data/video.mp4" type="video/mp4">
</video>
