# QuickGraph!
 QuickGraph is a shiny app designed to make visualizing, analyzing, and graphing large CSV files easier for new R users. 
 To get started, please run the following code in R: 
   **install.packages("shiny") 
   library(shiny) 
   runGitHub(repo = "QuickGraph", username = "Skillmerism", ref = "main") **
 Notes: 
   Application should open immediately upon running the code, no working directory or other packages needed. 
   Only CSV files are accepted. 
   When selecting columns for analysis, please be aware that only certain column types (numeric for mean, character for listing unique values, etc) will be accepted. Other       types may produce an error. To resolve the error, please select a different column. 
