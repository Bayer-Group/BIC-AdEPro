FROM rocker/shiny:4.3.2


RUN apt-get update && apt upgrade -y && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y -q libssl-dev libxml2-dev libharfbuzz-dev \
      libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev curl iputils-ping \  
      && apt-get clean \
      && rm -rf /tmp/* /var/tmp/* /srv/shiny-server/* \
      && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('Bayer-Group/BIC-AdEPro')"

ENTRYPOINT ["R", "-e"]
CMD ["library('adepro'); launch_adepro(host = '0.0.0.0',port=3838)"]
