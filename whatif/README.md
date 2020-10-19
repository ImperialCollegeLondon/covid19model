# whatif
Code to be used for running whatif scenario of switiching Rt profiles and news
onset-to-death distribution.

This code requires you to install **epidemia from 'exponential_new'**. Default branch uses logit link while we need 'exponential' link to make things easy to switch. The command to be used is:
```
devtools::install_github('ImperialCollegeLondon/epidemia', ref='exponential_new')
```

To run the code from terminal do following
```
cd covid19model/whatif
# run what fit and scenario switching
Rscript fit-rt-switch-profiles.r
# run onset to deathj fitting
Rscript onset-death-analysis-bin.r
```
To run the code from rstudio
```
# source 
source whatif/fit-rt-switch-profiles.r
# source usa script, make sure you make full true in options
source whatif/onset-death-analysis-bin.r
```
## Notice
 
:warning: This code is released with no support. We try our best to look at issues and pull request but can't help people with setup most of the time.

:warning: This model is in active development and so parameter name and behaviors, and output file formats will change without notice.

:warning: As with any mathematical model, it is easy to misconfigure inputs and therefore get meaningless outputs. The development team only endorses outputs it has itself generated.