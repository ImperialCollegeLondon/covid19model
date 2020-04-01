# covid19model
Code for modelling estimated deaths and cases for COVID19. 

This repository has code for replication purposes. The bleeding edge code and advancements are done in a private repository. Ask report authors for any collaborations. 

# How to run the code

There are two ways to run our code:-
* Open the rstudio project covid19model.Rproj file in rstudio and run/source base.r file
* To run from commandline please enter the cloned directory and type 'Rscript base.r base' in terminal
* The results are stored in two folders results and figures.
* Results has the stored stan fits and data used for plotting
* Figures have the images with daily cases, daily death and Rt for all countries.

## Please note to not make you wait for long we have by default run sampling for short period. To be comparable with report please uncomment the line 206 and comment out line 207. This will run sampling for 4000 iterations with 2000 warmups and 4 chains.