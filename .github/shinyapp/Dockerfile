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
    git \
    python3-dev \
    python3-pip \
    virtualenv \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install --no-install-recommends -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# install R packages for shiny functionality
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'sparklyr', 'pysparklyr', 'dplyr', 'glue', 'DBI', 'pheatmap', 'bslib', 'ggplot2', 'Rmpfr'))"

# prepare virtualenv
RUN /usr/bin/python3.10 -m virtualenv -p '/usr/bin/python3.10' /root/.virtualenvs/r-sparklyr-pyspark-3.5 \
    && /root/.virtualenvs/r-sparklyr-pyspark-3.5/bin/python -m pip install --upgrade --no-user 'pyspark==3.5.1' 'pandas!=2.1.0' PyArrow grpcio google-api-python-client grpcio_status 'rpy2==3.5.12'

# install R code R CMD build rnaseq
ADD /rnaseq /build/rnaseq
RUN cd /build \
    && R CMD build rnaseq \
    && R CMD INSTALL /build/rnaseq_0.0.1.tar.gz

EXPOSE 3838

# create user
#RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
#USER shiny
