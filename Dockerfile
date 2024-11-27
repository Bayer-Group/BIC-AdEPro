
FROM dockerregistryspa.azurecr.io/dockerregistryspa/spa-rshiny-base:4.3.2-v1.0.7

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y -q curl iputils-ping \
      && apt-get clean \
      && rm -rf /tmp/* /var/tmp/* \
      && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('Bayer-Group/BIC-AdEPro')"

ENTRYPOINT ["R", "-e"]
CMD ["library('adepro'); launch_adepro(host = '0.0.0.0',port=3838)"]
