FROM openanalytics/r-ver:4.4.1

LABEL maintainer="Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

ARG SPARK_VERSION=3.3.0
ARG PIPER_VERSION=6.4.0
ARG package_token=
ENV SPARK_HOME /home/spark-current

RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/latest
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    maven \
    openjdk-8-jdk \
    wget \
    curl \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install --no-install-recommends -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# download spark
RUN cd /home \
    && wget https://archive.apache.org/dist/spark/spark-${SPARK_VERSION}/spark-${SPARK_VERSION}-bin-hadoop3.tgz \
    && tar -xvzf /home/spark-${SPARK_VERSION}-bin-hadoop3.tgz --directory=/home \
    && rm /home/spark-${SPARK_VERSION}-bin-hadoop3.tgz \
    && rm /home/spark-${SPARK_VERSION}-bin-hadoop3/jars/guava-14*.jar \
    && ln -s "/home/spark-${SPARK_VERSION}-bin-hadoop3" $SPARK_HOME \
    && mkdir $SPARK_HOME/work

# install Hive and SeqsLab related jars
COPY pom.xml /home/pom.xml
RUN cd /home \
    && sed -i "s/{{SPARK_VERSION}}/${SPARK_VERSION}/g" /home/pom.xml \
    && mvn dependency:copy-dependencies -DoutputDirectory="${SPARK_HOME}/jars/"

RUN wget --user=download --password=atgxdownload -O ${SPARK_HOME}/jars/atgenomix-plugin.jar \
        https://nexus-atgx.azurewebsites.net/repository/maven-releases/com/atgenomix/seqslab/atgenomix-plugin/${PIPER_VERSION}/atgenomix-plugin-${PIPER_VERSION}.jar \
    && curl -o ${SPARK_HOME}/jars/seqslab-authenticator-1.0.0.jar -L \
        https://${package_token}@maven.pkg.github.com/AnomeGAP/PiedPiper/com/atgenomix/seqslab/seqslab-authenticator/1.0.0/seqslab-authenticator-1.0.0.jar \
    && wget -O ${SPARK_HOME}/jars/play-functional_2.12-2.9.1.jar https://repo1.maven.org/maven2/com/typesafe/play/play-functional_2.12/2.9.1/play-functional_2.12-2.9.1.jar \
    && wget -O ${SPARK_HOME}/jars/play-json_2.12-2.9.1.jar https://repo1.maven.org/maven2/com/typesafe/play/play-json_2.12/2.9.1/play-json_2.12-2.9.1.jar \
    && wget -O ${SPARK_HOME}/jars/mysql-connector-java-8.0.30.jar https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.30/mysql-connector-java-8.0.30.jar

# install R packages for shiny functionality
RUN R -q -e "options(warn=2); install.packages(c('shiny', 'sparklyr', 'pysparklyr', 'dplyr', 'glue', 'DBI', 'pheatmap', 'bslib', 'ggplot2'))"

# install
RUN R -q -e "options(warn=2); install.packages(c('Rmpfr'))"

# install R code
COPY rnaseq_0.0.1.tar.gz /build/rnaseq_0.0.1.tar.gz
RUN R CMD INSTALL /build/rnaseq_0.0.1.tar.gz

EXPOSE 3838

# create user
#RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
#USER shiny

CMD ["R", "-q", "-e", "rnaseq::runShiny()"]
