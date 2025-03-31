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
COPY . /app
WORKDIR /app
RUN R -e "install.packages('devtools')"
# Install the specified package from the given GitHub repository
# Modify this to match your repo
RUN R -e "devtools::install_local('/app')"
# Set entrypoint and pass runtime arguments to the CMD
RUN mkdir -p /srv/shiny-server/app
RUN echo 'library(adepro); adepro::run_app()' > /srv/shiny-server/app/app.R

# 6. Setze Berechtigungen
RUN chown -R shiny:shiny /srv/shiny-server

# 7. Exponiere Port & starte Shiny Server
EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
