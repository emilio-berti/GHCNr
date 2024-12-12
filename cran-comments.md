# GHCNr v0.8.0

## Check
R CMD check --as-cran highlights (NOTE) the URL in DESCRIPTION: <https://www.ncei.noaa.gov/access/services/data/v1>.
The URL is correct, but the API returns a 400 error if the whole API call is not specified (i.e. including query parameters).

## Acronyms

  - GHCN = Global Historical Climatology Network (<https://www.ncei.noaa.gov/products/land-based-station>)
  - GHCNd = Global Historical Climatology Network daily (<https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>)
