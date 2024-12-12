# GHCNr v0.7.0

## Comments about checks
R CMD check --as-cran highlights (NOTE) the URL in DESCRIPTION: <https://www.ncei.noaa.gov/access/services/data/v1>.
The URL is correct, but the API returns a 400 error if the whole API call is not specified (i.e. including query parameters).

## Acronyms

  - GHCN = Global Historical Climatology Network (<https://www.ncei.noaa.gov/products/land-based-station>)
  - GHCNd = Global Historical Climatology Network daily (<https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>)

## Pre-submission R CMD check --as-cran results

R CMD check --as-cran GHCNr_0.7.0.tar.gz >> GHCNr/cran-comments.md
  
* using log directory ‘/home/squirry/GHCNr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-conda-linux-gnu (64-bit)
* R was compiled by
    x86_64-conda-linux-gnu-cc (Anaconda gcc) 11.2.0
    GNU Fortran (Anaconda gcc) 11.2.0
* running under: Ubuntu 22.04.5 LTS
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘GHCNr/DESCRIPTION’ ... OK
* this is package ‘GHCNr’ version ‘0.7.0’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [5s/16s] NOTE
Maintainer: ‘Emilio Berti <emilio.berti@idiv.de>’

New submission

Found the following (possibly) invalid URLs:
  URL: https://www.ncei.noaa.gov/access/services/data/v1
    From: DESCRIPTION
    Status: 400
    Message: Bad Request
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘GHCNr’ can be installed ... [14s/12s] OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... NOTE
unable to verify current time
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking startup messages can be suppressed ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [19s/16s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
 OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... [11s/19s] OK
* checking PDF version of manual ... OK
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 3 NOTEs
* using log directory ‘/home/squirry/GHCNr.Rcheck’
* using R version 4.3.1 (2023-06-16)
* using platform: x86_64-conda-linux-gnu (64-bit)
* R was compiled by
    x86_64-conda-linux-gnu-cc (Anaconda gcc) 11.2.0
    GNU Fortran (Anaconda gcc) 11.2.0
* running under: Ubuntu 22.04.5 LTS
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘GHCNr/DESCRIPTION’ ... OK
* this is package ‘GHCNr’ version ‘0.7.0’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [6s/17s] NOTE
Maintainer: ‘Emilio Berti <emilio.berti@idiv.de>’

New submission

Found the following (possibly) invalid URLs:
  URL: https://www.ncei.noaa.gov/access/services/data/v1
    From: DESCRIPTION
    Status: 400
    Message: Bad Request
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘GHCNr’ can be installed ... [13s/12s] OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... OK
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking startup messages can be suppressed ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... [19s/16s] OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking contents of ‘data’ directory ... OK
* checking data for non-ASCII characters ... OK
* checking LazyData ... OK
* checking data for ASCII and uncompressed saves ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
 OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... [10s/19s] OK
* checking PDF version of manual ... OK
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 2 NOTEs
