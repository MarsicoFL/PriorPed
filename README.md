# PriorPed
This is a shiny app for prioritising family members for genotyping in missing person cases. The methods are published in https://linkinghub.elsevier.com/retrieve/pii/S1872497320301484

## Installation
You can easily locally run PriorPed opening Rstudio an executing the following commands:
```{r, eval = FALSE}
install.packages("shiny")
library(shiny)
shiny::runGitHub(repo = "MarsicoFL/PriorPed")
```
Also, it they are not installed previously, the following packages are required
```{r, eval = FALSE}

install.packages("shinyjs")

install.packages("pedtools")

install.packages("glue")

install.packages("patchwork")
```
