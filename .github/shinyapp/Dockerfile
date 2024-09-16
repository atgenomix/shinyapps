FROM openanalytics/r-ver:4.4.1

LABEL maintainer="Yuting LIN <yuting.lin@atgenomix.com>"

ENV SPARK_HOME /home/spark-current

RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/latest
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    openjdk-8-jdk \
    wget \
    curl \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install --no-install-recommends -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# install R packages for shiny functionality
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'sparklyr', 'pysparklyr', 'dplyr', 'glue', 'DBI', 'pheatmap', 'bslib', 'ggplot2'))"

# install
RUN R -q -e "options(warn=2); install.packages(c('Rmpfr'))"

# install R code R CMD build rnaseq
COPY rnaseq_0.0.1.tar.gz /build/rnaseq_0.0.1.tar.gz
RUN R CMD INSTALL /build/rnaseq_0.0.1.tar.gz

EXPOSE 3838

# create user
#RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
#USER shiny
