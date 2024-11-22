FROM dockerregistryspa.azurecr.io/dockerregistryspa/spa-rshiny-base:4.3.2-v1.0.7
COPY --chown=shiny:shiny ./ /srv/shiny-server

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y -q curl iputils-ping \
      && apt-get clean \
      && rm -rf /tmp/* /var/tmp/* \
      && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('devtools')"
COPY /requirements.txt /requirements.txt

RUN /install-packages.sh
