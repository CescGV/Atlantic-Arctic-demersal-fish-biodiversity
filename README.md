# Atlantic Arctic demersal fish biodiversity

(This paper was produced using data that is now available on demand only, at: http://metadata.nmdc.no/metadata-api/landingpage/f77112db062b5924d079a54b311260fb).

These are the different scripts used for the analysis of the Norwegian bottom trawling database of the IMR, published in PNAS January 2023: https://doi.org/10.1073/pnas.2120869120 

The final R scripts used in this work are updated on the 1st February 2023. 

There are 6 scripts:


0. Data gathering and cleaning --> Here I collect all the raw data, check for taxon names correctness and mistakes in the data. Output --> Full dataset
0.1 Data cleaning --> Before the anaysis, I cleaned the data to exclude broken gear, non-fish species, etc. 

1. Alpha diversity GAM -->  The first script to look at is the "Data exploration" --> In it, I investigate the characteristics of the database 
in terms of potential biases of the data and calculation of richness at each site. I check the temporal distribution of longitude and latitude, as well as the number of trawls per year and the effort at each trawl, per year. There is a negative trend of swept_effort per trawl, with time, which needs to be accounted. I fit a GAM model to study how has the species richness changed with time, considering that the effort is different at each trawl, with a negative trend with time. 

2. BRT Alpha diversity --> Once understood that the species richness at each site (alpha diversity) has increased with time, I would like investigate
the environmental drivers of this change, which afterwards allow for a spatial modelling of the alpha diversity. This is here done using BRT models.

3. Beta diversity --> Traditional approach to beta diversity

4. Gamma diversity --> SAC curves for gamma diversity considering the effort (Sites). It is relevant to keep in mind that the effort at each site is not the same
(remember that the swept area at each trawl declines with time) but the shape of the SAC (already over the main curvature of a log-shaped curve) suggests that the area has been properly sampled annually. 
