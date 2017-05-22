# GDD-Mollusc

Here are the associated scripts for analysis and plotting in Broell, F, JSP, McCain, and CT Taggart 2017, Thermal time explains size-at-age variation in molluscs, Marine Ecology Progress Series.

The scripts run as numbered, and do the following:

1) Apply exclusion criteria to certain temperature treatments, as described in the paper and supplemental materials.
2) Uses regression and ANCOVA to analyze the use of the growing degree day (aka thermal time), and populates a table of these results. The main function (*table.pop*) is also used for the next script to plot the results.
3) Figures 2, 3 and 4.
4) Residual checker for examining model residuals.

Since writing this code, I've realized that the functions are not as "defensible" as they could be. Therefore, if you are adapting this for your own use, I recommend using "spot-checks", as I've done along the way as well.
