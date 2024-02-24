
# easyEWAS APP

<!-- badges: start -->
  <!-- badges: end -->
  
  The goal of easyEWAS APP is to conduct Epigenome-wide association study and visualize results for researchers less experienced in EWAS or parallel computing.

## Installation

You can run the APP like so:
  
  ``` r
pacman::p_load(
  "shiny",
  "shinythemes",
  "tidyverse",
  "lmerTest",
  "parallel",
  "foreach",
  "doParallel"
)
runGitHub("ewasAPP", "ytwangZero") 
```
