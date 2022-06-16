# CNAIM ([cnaim.io])

![R-CMD-check status](https://github.com/Utiligize/CNAIM/workflows/R-CMD-check/badge.svg)
![License](https://img.shields.io/badge/license-MIT%20License-blue.svg)

Common Network Asset Indices Methodology (CNAIM) package for the R language.

Contains a series of algorithms which determine the probability of failure,
consequences of failure and monetary risk associated with electricity
distribution companies' assets such as transformers and cables. Results are
visualized in an easy-to-understand risk matrix.

![risk matrix](man/figures/risk_matrix.png?raw=true "Risk matrix visualization")

Open source implementation by [Utiligize] inspired by the framework specified
by the Distribution Network Operators and the regulator (Ofgem) in Great
Britain. The specification document is available
[here](https://www.ofgem.gov.uk/system/files/docs/2017/05/dno_common_network_asset_indices_methodology_v1.1.pdf).

## Installation

To install the latest development version from GitHub, run the following from
an R console:

```r
if (!require('devtools'))
  install.packages('devtools')
devtools::install_github('Utiligize/CNAIM')
```

A CRAN submission has been made.

## Getting Started

We recommend you check out the examples and documentation at [cnaim.io].

## Contributing

Please report any issues on GitHub or send an email to <contact@utiligize.com>.

## License

This package is made available under the MIT license:

```text
Copyright (c) 2020 Utiligize ApS

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

// SPDX-License-Identifier: MIT
```

[cnaim.io]: https://www.cnaim.io/
[Utiligize]: https://www.utiligize.com/
