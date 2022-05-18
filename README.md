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

## Use
A quick guide for performing simulations is presented below:
(1) select the reference allele frequency database (examples are from leapdna.org).
(2) Choose the individuals (could be groups of individuals) to simulate.
(3) Choose the missing person (it should no be one of the selected in step 2).
(4) Select number of simulations:
      - nProfiles: number of genotypes simulated for individuals selected in 2.
      - lrSims: number of MP/POI genotypes simulated for LR expected values.
Note: the number of simulations could be computationally expensive, so it is recommeded selecting a low number (nProfiles = 5, and lrSims = 100) for complex pedigrees. Another option is performing different simulations for different groups (those selected in 2). 
(5) Click on Simulate!


![Screenshot]("Data/Captura de pantalla de 2022-05-18 20-03-15.png")
