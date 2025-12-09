FROM ghcr.io/bastienird/gta_nc_datapaper:latest AS builder
COPY inputs/data/GTA/NCD_MAPPED.qs /root/DashboardCWPdataset/data/NCD_MAPPED.qs
COPY inputs/data/FSJ/FS_MAPPED.qs /root/DashboardCWPdataset/data/FS_MAPPED.qs

WORKDIR /root/DahsboardCWPdataset
RUN mkdir -p /tmp/flattened_lib \
 && cp -LR renv/library/R-4.2/x86_64-pc-linux-gnu/* /tmp/flattened_lib/


# Install system libraries
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    libv8-dev \
    libsodium-dev \
    libsecret-1-dev \
    git \
    libnetcdf-dev \
    curl \
    udunits-bin \
    gdal-bin \
    libjq-dev \
    cmake \
    protobuf-compiler \
    libprotobuf-dev \
    wget \
    librdf0 \
    librdf0-dev \
    libtbb-dev \
    libzmq3-dev \
    libpoppler-cpp-dev \
    redland-utils \
    dos2unix && \
    apt-get clean

RUN install2.r --error --skipinstalled --ncpus -1 httpuv

WORKDIR /root/DashboardCWPdataset

RUN Rscript -e "install.packages('remotes', repos='https://cloud.r-project.org'); \
                remotes::install_version('qs', version = '0.26.3', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('jsonlite', version = '1.9.1', upgrade = 'never', repos = 'https://cran.r-project.org'); \
                remotes::install_version('readr', version = '2.1.5', upgrade = 'never', repos = 'https://cran.r-project.org')"

ARG DOI_CSV_HASH
RUN echo "DOI_CSV_HASH=${DOI_CSV_HASH}" > /tmp/doi_csv_hash.txt

RUN mkdir -p data 
COPY DOI.csv ./DOI.csv
COPY data/ ./data/

RUN dos2unix DOI.csv && sed -i -e '$a\' DOI.csv

RUN bash -c "tail -n +2 DOI.csv | tr -d '\r' | \
    while IFS=',' read -r DOI FILE; do \
        RECORD_ID=\$(echo \"\$DOI\" | awk -F/ '{print \$NF}' | sed 's/zenodo\\.//'); \
        EXT=\${FILE##*.}; BASE=\${FILE%.*}; \
        ORIGINAL=\"./data/\$FILE\"; \
        if [ \"\$EXT\" = \"qs\" ]; then \
            TARGET=\"./data/\${BASE}_\${RECORD_ID}.qs\"; \
        elif [ \"\$EXT\" = \"zip\" ]; then \
            TARGET=\"./data/\${BASE}_\${RECORD_ID}.zip\"; \
        else \
            TARGET_CSV=\"./data/\${BASE}_\${RECORD_ID}.\${EXT}\"; \
            TARGET=\"\${TARGET_CSV%.*}.qs\"; \
        fi; \
        URL=\"https://zenodo.org/record/\$RECORD_ID/files/\$FILE?download=1\"; \
        if [ -f \"\$TARGET\" ]; then continue; fi; \
        if [ -f \"\$ORIGINAL\" ]; then \
            if [ \"\$EXT\" = \"qs\" ]; then cp \"\$ORIGINAL\" \"\$TARGET\"; \
            elif [ \"\$EXT\" = \"zip\" ]; then cp \"\$ORIGINAL\" \"\$TARGET\"; \
            else cp \"\$ORIGINAL\" \"\$TARGET_CSV\"; fi; \
        else \
            if [ \"\$EXT\" = \"qs\" ] || [ \"\$EXT\" = \"zip\" ]; then \
                wget -nv -O \"\$TARGET\" \"\$URL\" || { echo \"\$FILE\" >> DOI_failed.csv; continue; } \
            else \
                wget -nv -O \"\$TARGET_CSV\" \"\$URL\" || { echo \"\$FILE\" >> DOI_failed.csv; continue; } \
            fi; \
        fi; \
        if [ \"\$EXT\" = \"zip\" ]; then \
            echo \"📦 Extracting zip file: \$TARGET\" && \
            unzip -q -o \"\$TARGET\" -d \"./data/\" && \
            echo \"✅ Zip file extracted successfully\"; \
        elif [ \"\$EXT\" != \"qs\" ]; then \
            Rscript -e \"qs::qsave(readr::read_csv('\$TARGET_CSV'), '\$TARGET')\" && rm -f \"\$TARGET_CSV\"; \
        fi; \
    done"

RUN echo \"✅ Listing files in ./data after conversion:\" && ls -lh ./data

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

RUN echo "✅ Listing files in ./data after conversion:" && ls -lh ./data


# Run the data update script Downloading the data (cached if DOI.csv did not change).
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

