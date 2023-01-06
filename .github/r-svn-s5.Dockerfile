FROM debian:stable

RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get install -y \
    gcc wget locales git rsync gfortran xvfb autoconf pkg-config texinfo texlive-latex-extra texlive-fonts-recommended tk8.6-dev \
    libcurl4-openssl-dev libblas-dev libbz2-dev libicu-dev libjpeg-dev liblapack-dev liblzma-dev libncurses5-dev libpcre2-dev libpng-dev libreadline-dev libxt-dev
RUN localedef -i en_US -f UTF-8 en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN apt-get install -y pandoc libssl-dev libcairo2-dev texlive-fonts-extra qpdf


ADD r-svn /r-svn
ADD S5 /S5


WORKDIR /r-svn

RUN git config --global --add safe.directory $PWD || true
RUN sed -i.bak 's|$(GIT) svn info|./.github/workflows/svn-info.sh|' Makefile.in
RUN ./.github/workflows/wget-recommended.sh
RUN ./.github/workflows/svn-info.sh

RUN CC=gcc ./configure --enable-R-shlib --with-blas --with-lapack --disable-java

RUN make -j2

RUN xvfb-run make check-all
RUN tail -n100 tests/*.fail || true

RUN make install

RUN Rscript -e 'install.packages(                        \
    c("remotes", "rcmdcheck"),                           \
    repos = "https://cran.rstudio.com/", Ncpus = 2L)'



WORKDIR /S5

RUN Rscript -e 'remotes::install_local(dependencies = TRUE, Ncpus = 2L)'

RUN Rscript -e 'rcmdcheck::rcmdcheck(      \
    args = c("--no-manual", "--as-cran"),  \
    error_on = "warning")'

## to run locally:
# expected directory
# .
# ├── Dockerfile
# ├── r-svn/
# └── S5/

# git clone --depth 1 https://github.com/t-kalinowski/r-svn --branch S5
# git clone --depth 1 https://github.com/t-kalinowski/S5    --branch base-R-support

# docker build -t r-svn-s5 .
# docker run -it r-svn bash
