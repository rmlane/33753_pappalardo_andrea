This zip file contains the R code and data for the paper
"Modeling time-varying exposure using inverse probability of treatment weights"
by Nathalie Grafféo, Aurélien Latouche, Ronald B Geskus, and Sylvie Chevret.

For questions, comments or remarks about the code please contact N. Grafféo (nathalie.graffeo@univ-amu.fr). 

The code and data are given for examples in sections 3.2, 3.3, 3.4 and 4 as R files.

The code has been written using R version 3.3.3 (Platform: x86_64-w64-mingw32/x64, 64-bit)
Running under: Windows >= 8 x64 (build 9200)
The packages used were survival_2.40-1 and ipw_1.0-11.

To reproduce the results presented in the manuscript in sections 3.2 and 3.3, 
just run the main analysis file Analyses_dataExample.R: 
R CMD BATCH Analyses_dataExample.R & to run the script. 
All figures and summaries will be stored in the results subfolder.

To reproduce the results presented in the manuscript in section 3.4, 
just run the main analysis file Analyses_Simulations.R: 
R CMD BATCH Analyses_Simulations.R & to run the script.
**Please note that to speed up the code, you can use parallel process.
Note also that, to save time, we put here the number of patients equal to 200 
(whereas it is equal to 500 in the paper). We provided the corresponding
results, SimulationsResults.csv, in the results subfolder.**

To reproduce the results presented in the manuscript in section 4, 
just run the main analysis file Analyses_miniCESAME.R: 
R CMD BATCH Analyses_miniCESAME.R & to run the script.
The table will be stored in the results subfolder.
**Please, note that the code needs time to run (3h47) 
on Intel(R) Xeon(R) CPU X5650@2.67GHz 2.66GHz (2 processeurs). 
Thus, we provided inside the program a subset (mytoy) 
with results Table_toy.txt stored in the results subfolder.** 
