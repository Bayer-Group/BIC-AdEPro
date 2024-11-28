# BEGIN DOCKERFILE
# Define build arguments for the Rocker version, repository, and runtime settings
# R Version
ARG ROCKER_VERSION=4.3.2
# Use the configurable Rocker version
FROM rocker/shiny:${ROCKER_VERSION}
# Install required system dependencies
RUN apt-get update && apt upgrade -y && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y -q \
    libssl-dev libxml2-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    curl iputils-ping \
    && apt-get clean \
    && rm -rf /tmp/* /var/tmp/* /srv/shiny-server/* \
    && rm -rf /var/lib/apt/lists/*
# Install R package devtools
RUN R -e "install.packages('devtools')"
# Install the specified package from the given GitHub repository
# Modify this to match your repo
RUN R -e "devtools::install_github('Bayer-Group/BIC-AdEPro', ref = 'api')"
# Set entrypoint and pass runtime arguments to the CMD
ENTRYPOINT ["R", "-e"]
# Modifiy this line to match your startup command
CMD ["library('adepro'); launch_adepro(host = '0.0.0.0', port = 3838)"]
# END DOCKERFILE
