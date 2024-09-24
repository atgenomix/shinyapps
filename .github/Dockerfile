ARG base_image=ghcr.io/atgenomix/runtime/base-r:1.6_22.04
FROM ${base_image}

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

# install R packages for shiny functionality
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'sparklyr', 'pysparklyr', 'dplyr', 'glue', 'DBI', 'pheatmap', 'bslib', 'ggplot2', 'Rmpfr'))"

# prepare virtualenv
RUN /usr/bin/python3.10 -m virtualenv -p '/usr/bin/python3.10' /root/.virtualenvs/r-sparklyr-pyspark-3.5 \
    && rm -rf /root/.virtualenvs/r-sparklyr-pyspark-3.5/lib \
    && mv /root/.virtualenvs/r-sparklyr-pyspark-3.5/local/* /root/.virtualenvs/r-sparklyr-pyspark-3.5/ \
    && /root/.virtualenvs/r-sparklyr-pyspark-3.5/bin/python -m pip install --upgrade --no-user 'pyspark==3.5.2' 'pandas!=2.1.0' PyArrow grpcio google-api-python-client grpcio_status 'rpy2==3.5.1'

# install R code R CMD build rnaseq
ADD rnaseq /build/rnaseq
RUN cd /build \
    && R CMD build /build/rnaseq \
    && R CMD INSTALL /build/rnaseq_0.0.1.tar.gz

# copy shiny proxy application yml file to /home
COPY atgenomix/shinyapp/application.yml /home/application.yml

EXPOSE 3838

# create user
#RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
#USER shiny
