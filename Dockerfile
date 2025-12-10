# syntax=docker/dockerfile:1.7
ARG MODE=prod
ARG BASE_IMAGE
FROM ${BASE_IMAGE:-rocker/r-ver:4.2.3}

# Install system libraries
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libx11-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    make \
    zlib1g-dev \
    libsecret-1-dev \
    pandoc \
    cmake \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    libicu-dev \
    libudunits2-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled --ncpus -1 httpuv

WORKDIR /root/DashboardCWPdataset

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_version('jsonlite', version = '1.9.1', upgrade = 'never', repos = 'https://cran.r-project.org')"

RUN mkdir -p data 

RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data

ENV RENV_PATHS_ROOT=/root/.cache/R/renv
ENV MODE=${MODE}

# Si en mode dev, changer pour le user rstudio
RUN if [ "$MODE" = "dev" ]; then \
      mkdir -p /home/rstudio/.cache/R/renv && \
      mkdir -p /home/rstudio/DashboardCWPdataset/renv/library && \
      chown -R rstudio:rstudio /home/rstudio && \
      printf '%s\n' \
        "RENV_CONFIG_CACHE_ENABLED=FALSE" \
        "RENV_PATHS_ROOT=/home/rstudio/DashboardCWPdataset/renv" \
        "RENV_PATHS_CACHE=/home/rstudio/DashboardCWPdataset/renv/library" \
        > /home/rstudio/.Renviron ; \
    fi


# Ces variables sont utilisées par R/renv à runtime
ENV RENV_PATHS_CACHE=${RENV_PATHS_ROOT}

ARG RENV_LOCK_HASH
RUN if [ -z "${RENV_LOCK_HASH}" ]; then \
      export RENV_LOCK_HASH=$(sha256sum renv.lock | cut -d' ' -f1); \
    fi && \
    echo "RENV_LOCK_HASH=${RENV_LOCK_HASH}" > /tmp/renv_lock_hash.txt

RUN mkdir -p ${RENV_PATHS_ROOT}
COPY renv.lock ./
COPY renv/activate.R renv/
COPY renv/settings.json renv/

#using remotes incase cache keep ancient renv version

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org')"
RUN Rscript -e "remotes::install_version('renv', version = jsonlite::fromJSON('renv.lock')\$Packages[['renv']]\$Version, repos = 'https://cran.r-project.org')"

# COPY renv/library/ renv/library/
#removed for now as no need to do it in local

# Restore renv packages
RUN R -e "renv::activate()" 
# Used to setup the environment (with the path cache) carreful keep in multiple lines
RUN R -e "renv::restore()" 
RUN R -e "renv::repair()" 
RUN R -e "renv::isolate()" 
RUN R -e "renv::status()" 

ENV RENV_CONFIG_CACHE_ENABLED=FALSE
ENV RENV_PATHS_ROOT=/root/DashboardCWPdataset/renv
ENV RENV_PATHS_CACHE=/root/DashboardCWPdataset/renv/library
RUN mkdir -p /root/DashboardCWPdataset/renv/library

ARG TEST_SCRIPT_REF=main
ARG TEST_SCRIPT_URL="https://raw.githubusercontent.com/firms-gta/tunaatlas_pie_map_shiny/${TEST_SCRIPT_REF}/testing_loading_of_all_packages.R"

RUN R -e "source(url('$TEST_SCRIPT_URL'), local=TRUE, encoding='UTF-8')"

COPY ./* ./

RUN Rscript global.R

ARG BRANCH
ENV BUILD_BRANCH=${BRANCH}

RUN echo "✅ MODE is: $MODE" && echo "✅ BRANCH is: $BRANCH" && echo "✅ BUILD_BRANCH is: $BUILD_BRANCH"
RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data
RUN R -e "if (Sys.getenv('BUILD_BRANCH') == 'dev') { library(dplyr); library(here); full <- qs::qread(here::here('data/default_dataset.qs')); full <- full %>% dplyr::filter(fishing_fleet == 'EUFRA - France (EU)'); qs::qsave(full, here::here('data/default_dataset.qs')); file.remove(here::here('data/data.qs')); source(here::here('create_or_load_default_dataset.R')) }"
RUN echo "✅ MODE is: $MODE" && echo "✅ BRANCH is: $BRANCH" && echo "✅ BUILD_BRANCH is: $BUILD_BRANCH"

RUN mkdir -p /etc/DashboardCWPdataset/

RUN if [ "$MODE" = "dev" ]; then R -e "renv::isolate()"; fi

EXPOSE 3838
EXPOSE 8787

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

ENTRYPOINT ["/entrypoint.sh"]

