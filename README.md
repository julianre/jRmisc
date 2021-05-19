# jRmisc

## Overview

I wrote this package to store my modified `ggplot2` theme (`theme_jr`). While I was at it, I added a function to format percentages (`percentjr`, adds thin space between the number and the percent sign). It also includes a customized version of `Session_Info`, which displays the version of Pandoc, pandoc-citeproc and [pandoc-crossref](https://github.com/lierdakil/pandoc-crossref), if it is used within an RMarkdown document.

In addition, there are functions for inserting a table caption and images with caption in (R)-Markdown. Both are intended for use with pandoc-crossref. These functions generate code that works with the Lua filters `short-caption` and `table-short-caption`.

## Installation

``` r
# The easiest way to get jRmisc is to install it from GitHub:
# install.packages("remotes")
remotes::install_github("julianre/jRmisc")
```
