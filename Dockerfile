FROM rocker/shiny:4.0.0

RUN apt-get update -qq && apt-get -y --no-install-recommends install \ 
    libxml2-dev \
    libcairo2-dev \
    libpq-dev \ 
    libssh2-1-dev \
    libcurl4-openssl-dev \
    libssl-dev
# update system libraries
RUN apt-get update && \ 
    apt-get upgrade -y && \  
    apt-get clean

# copy the app to the image
COPY Rivals_of_Aether_Toolset.Rproj /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/
COPY renv.lock srv/shiny-server/

COPY renv srv/shiny-server/renv/
COPY R /srv/shiny-server/R/
COPY input /srv/shiny-server/input/
COPY www /srv/shiny-server/www/
COPY cpp /srv/shiny-server/cpp/
COPY js /srv/shiny-server/js/

RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# remove install files                       
RUN rm -rf /var/lib/apt/lists/*

# set non-root                       
RUN useradd shiny_user
USER shiny_user

# run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]