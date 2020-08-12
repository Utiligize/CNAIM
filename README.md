![example workflow name](https://github.com/Utiligize/CNAIM/workflows/R-CMD-check/badge.svg)

# CNAIM ([cnaim.io](https://www.cnaim.io/))
Common Network Asset Indices Methodology (CNAIM) package for the R language.


Contains a series of algorithms which determine the probability of failure,
consequences of failure and monetary risk associated with electricity
distribution companies' assets such as transformers and cables. Results are
visualized in an easy-to-understand risk matrix.

![risk matrix](man/figures/risk_matrix.png?raw=true "Risk matrix visualization")

Open source implementation by [Utiligize](https://www.utiligize.com/) inspired by the framework specified by the Distribution Network Operators and the regulator (Ofgem) in Great Britain. The specification document is available [here.](https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf)

## Installation
To install the latest development version from GitHub, run the following from an R console:
```r
if (!require('devtools'))
  install.packages('devtools')
devtools::install_github('Utiligize/CNAIM')
```

A CRAN submission has been made.

## Getting Started
We recommend you check out the examples and documentation at [cnaim.io](https://www.cnaim.io/).

## Contributing
Please report any issues on GitHub or send an email to contact@utiligize.com

## License
This package is made available under the MIT license
See the [LICENSE](LICENSE) file for more details.
