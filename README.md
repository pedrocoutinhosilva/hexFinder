# hexFinder <img src="man/figures/logo.svg" align="right" alt="" width="130" />
<!-- badges: start -->
[![R-CMD-check](https://github.com/pedrocoutinhosilva/hexFinder/workflows/R-CMD-check/badge.svg)](https://CRAN.R-project.org/package=hexFinder)
[![Codecov test coverage](https://codecov.io/gh/pedrocoutinhosilva/hexFinder/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pedrocoutinhosilva/hexFinder?branch=main)
[![cranlogs](https://www.r-pkg.org/badges/version/hexFinder)](https://CRAN.R-project.org/package=hexFinder)
[![cranlogs](https://cranlogs.r-pkg.org/badges/hexFinder)](https://CRAN.R-project.org/package=hexFinder)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/hexFinder)](https://CRAN.R-project.org/package=hexFinder)
<!-- badges: end -->

Scavenge the web for possible hex logos for CRAN packages.

---

## installation
###### 1 - Install the package:

```R
# Install released version from CRAN
install.packages('hexFinder')
# Or the most recent development version from github:
devtools::install_github('pedrocoutinhosilva/hexFinder')
```

###### 2 - Include it in your project:
```R
library(hexFinder)
```

---

## Usage

Call the `find_hex()` function with the name of the package you would like to find a hex logo for, and optionally, a place where the logo will be stored:
```R
> find_hex("ggplot2", "output")
Downloaded from GitHub repo hex for ggplot2
[1] "output/ggplot2.png"
```

When a package is not on CRAN, or a logo for the package cannot be found, a simple logo will be generated:
```R
> find_hex("notapackage", "output")
Not a cran package
No logo found, generated hex for notapackage
[1] "output/notapackage.svg"
```

As an alternative, you can also use the `scavenge()` function. It is functionaly the same as `find_hex()`, but with extra racoons:
```R
> scavenge("rlang", "output")
Summoning racoons to help with the search...
         /\ /\
       -'<o_o>'-   _
         () ()\  ,'_\
         ( . ) )/._./
         (_)-(_).--'
Downloaded from GitHub repo hex for rlang
[1] "output/rlang.png"
```

## Dealing with github API limits

The Github API used by this package does have some limits. The first time in a session that you make a search you might see the following message:
```R
> find_hex("devtools", "output")
No github personal access token provided.
Limited search rates for github will apply.
Set up github_pat enviromental variable if you plan to query multiple repos in a short time
Downloaded from GitHub repo hex for devtools
[1] "output/devtools.svg"
```
This limit is tipically on a problem (Its around 50 requests per hour), but if you are planning to do a large amount of searches, you can set up a `github_pat` enviromental variable using a .Renv file or calling `Sys.setenv()`:
```R
Sys.setenv(github_pat = "your_personal_access_token")
```

Mre information on how to get a github personal access token, at https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token.

## Documentation

Online documentation is available at: https://www.anatomyofcode.com/hexFinder