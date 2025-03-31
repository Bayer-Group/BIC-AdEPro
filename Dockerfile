FROM dockerregistryspa.azurecr.io/dockerregistryspa/spa-rshiny-base:4.3.0-1.0.0
COPY . /app
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_local('/app')"
# Set entrypoint and pass runtime arguments to the CMD
RUN echo 'library(adepro); adepro::launch_adepro()' > /srv/shiny-server/app.R
