# Atlantic Arctic demersal fish biodiversity

# These are the different scripts used for the analysis of the Norwegian bottom trawling database of the IMR. There are 4 scripts:


1. The first script to look at is the "Data exploration" --> In it, I try to investigate the characteristics of the database 
in terms of potential biases of the data and calculation of richness at each site. I check the temporal distribution of longitude and latitude, as well as the number of trawls per year and the effort at each trawl, per year. There is a negative trend of swept_effort per trawl, with time, which needs to be accounted. I fit a GAM model to study how has the species richness changed with time, considering that the effort is different at each trawl, with a negative trend with time. 

2. Alpha diversity --> Once understood that the species richness at each site (alpha diversity) has declined with time, I would like investigate
the environmental drivers of this change, which afterwards allow for a spatial modelling of the alpha diversity. This is here done using BRT models.

3. Beta diversity --> Traditional approach to beta diversity

4. Gamma diversity --> SAC curves for gamma diversity considering the effort (Sites). It is relevant to keep in mind that the effort at each site is not the same
(remember that the swept area at each trawl declines with time) but the shape of the SAC suggests that the area has been properly every year. 
