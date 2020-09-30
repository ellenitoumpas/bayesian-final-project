# bayesian-final-project
Bayesian Analysis Final Project

## Getting started

## Install developer requirements

1. [Install R](https://cran.r-project.org/mirrors.html)

Alternatively, you can install using [Docker](https://hub.docker.com/_/r-base) or [Ansible](https://github.com/Oefenweb/ansible-r) on Ubuntu.
<!-- TODO: add Ansible playbooks and Docker installs for different systems -->

Homebrew is great, but I currently avoid using it to install R, because of a number of bugs.

2. Edit the `setup.R` file to include any packages you need

3. In terminal, run:

```zsh
$ Rscript ./setup.R
```

## Lint

```zsh
$ devtools::install_github("REditorSupport/languageserver")
```

## Exporting as PDF (aka Knitting)

Dependencies:

```zsh
$ brew install pandoc pandoc-citeproc mactex # or basictex
```

To knit aka export Rmarkdown files to latex. In terminal run:

```zsh
$ Rscript -e 'rmarkdown::render("MATH2269_final_project.Rmd")'
```
