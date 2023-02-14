FROM debian:stable

ENV DEBIAN_FRONTEND=noninteractive
ENV R_CRAN_WEB: "https://cran.rstudio.com"

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y \
    gcc wget locales git rsync gfortran xvfb autoconf pkg-config       \
    texinfo texlive-latex-extra texlive-fonts-recommended tk8.6-dev    \
    libcurl4-openssl-dev libblas-dev libbz2-dev libicu-dev libjpeg-dev \
    liblapack-dev liblzma-dev libncurses5-dev libpcre2-dev libpng-dev  \
    libreadline-dev libxt-dev
RUN localedef -i en_US -f UTF-8 en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN apt-get install -y \
    pandoc libssl-dev libcairo2-dev \
    texlive-fonts-extra qpdf file


ADD r-svn /r-svn

WORKDIR /r-svn
RUN git config --global --add safe.directory $PWD || true
RUN sed -i.bak 's|$(GIT) svn info|./.github/scripts/svn-info.sh|' Makefile.in
RUN ./.github/scripts/wget-recommended.sh
RUN ./.github/scripts/svn-info.sh

RUN CC=gcc ./configure --enable-R-shlib --with-blas --with-lapack --disable-java
RUN make -j2
RUN xvfb-run make check-all
RUN tail -n100 tests/*.fail || true

RUN make install
RUN Rscript -e 'install.packages(                        \
    c("remotes", "rcmdcheck"),                           \
    repos = "https://cran.rstudio.com/", Ncpus = 2L)'



ADD S7 /S7
WORKDIR /S7
RUN Rscript -e 'remotes::install_local(dependencies = TRUE, Ncpus = 2L)'
RUN Rscript -e 'rcmdcheck::rcmdcheck(      \
    args = c("--no-manual", "--as-cran"),  \
    error_on = "warning")'

## to run locally, start in an empty directory and call `git clone`
## to produce this directory structure
# .
# ├── r-svn/
# └── S7/

# git clone --depth 100 https://github.com/t-kalinowski/r-svn --branch S5  # fetch depth must be > 1
# git clone --depth 1   https://github.com/t-kalinowski/S7    --branch base-R-support

## Then build the docker image.
# docker build -t r-svn-s5 -f S5/.github/r-svn-s5.Dockerfile .
# docker run -it r-svn-s5 bash
