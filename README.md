# plot_localmoran.R

Two functions are contained in plot_localmoran.R

plot_local_moran, which is modified from https://aledemogr.com/2017/10/16/global-and-local-measures-of-spatial-autocorrelation/

Produces local Moran's I plots showing HH, HL, LH, LL clusters.

## sp.remove.na.rows()

The second is a helper function for removing rows in a spatailpolygonsdataframe or spatialpointsdataframe where NAs occur. You specify the column name to check for NAs.

This function was modified from Jeffery Evans Answer: https://gis.stackexchange.com/questions/89512/r-dealing-with-missing-data-in-spatialpolygondataframes-for-moran-test

