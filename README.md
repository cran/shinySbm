<!-- Compiling paragraph: start -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`shinySbm` is a R package containing a shiny application. This
application provides a user-friendly interface for network analysis
based on the `sbm` package made by Chiquet J, Donnet S and Barbillon P
(2023) [CRAN](https://CRAN.R-project.org/package=sbm). The `sbm` package
regroups into a unique framework tools for estimating and manipulating
variants of the stochastic block model. `shinySbm` allows you to easily
apply and explore the outputs of a Stochastic Block Model without
programming. It is useful if you want to analyze your network data
(adjacency matrix or list of edges) without knowing the `R` language or
to learn the basics of the `sbm` package.

Stochastic block models (SBMs) are probabilistic models in statistical
analysis of graphs or networks, that can be used to discover or
understand the (hidden/latent) structure of a network, as well as for
clustering purposes.

Stochastic Block Models are applied on network to simplify the
information they gather, and help visualize the main
behaviours/categories/relationships present in your network. It’s a
latent model which identify significant blocks (groups) of nodes with
similar connectivity patterns. This could help you to know if your
network: hides closed sub-communities, is hierarchical, or has another
specific structure.

With `shinySbm` you should also be able to:

-   Easily run a Stochastic Block Model (set your model, infer
    associated parameters and choose the number of blocks)
-   Get some nice outputs as matrix and network plots organized by
    blocks
-   Get a summary of the modelling
-   Extract lists of nodes associated with their blocks

## How to use the application

### On Shiny Migale

I you want to use shinySBM without having to code a single line, the app
is available on [Migale](https://shiny.migale.inrae.fr/app/ShinySBM).

### With `R`

#### Installation

You can install the development version of shinySbm like so:

    install.packages("shinySbm")

The shinySbm package should be installed.

#### Running the application

From a new `R` session run

    shinySbm::shinySbmApp()

### With `docker`

#### Installation

If you are familiar to `docker`, you can also download the docker image
by running the command:

    docker pull registry.forgemia.inra.fr/theodore.vanrenterghem/shinysbm:latest

#### Running the application

Once installed you can run the command to launch the app:

    docker run -p 3838:3838 registry.forgemia.inra.fr/theodore.vanrenterghem/shinysbm:latest

And then from your browser find the address `http://localhost:3838/`

## Contact

Any questions, problems or comments regarding this application ?  
Contact us: <shiny.sbm.dev@gmail.com>

## References

Chiquet J, Donnet S, Barbillon P (2023). sbm: Stochastic Blockmodels. R
package version 0.4.5,  
<https://CRAN.R-project.org/package=sbm>.
