FROM rocker/shiny:4.1.1

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
COPY Rivals_of_Aether_Toolset.Rproj ./Rivals_of_Aether_Toolset.Rproj
COPY global.R ./global.R
COPY app.R ./app.R
COPY server.R ./server.R
COPY ui.R ./ui.R
COPY renv.lock ./renv.lock

COPY renv /srv/shiny-server/renv/
COPY src ./src/
COPY input ./input/
COPY www ./www/
COPY cpp ./cpp/
COPY js ./js/

RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# remove install files                       
RUN rm -rf /var/lib/apt/lists/*

# make all app files readable, gives rwe permisssion (solves issue when dev in Windows, but building in Ubuntu)
#RUN chmod -R 755 ./Rivals_of_Aether_Toolset.Rproj/
#RUN chmod -R 755 ./global.R
#RUN chmod -R 755 ./app.R
#RUN chmod -R 755 ./server.R
#RUN chmod -R 755 ./ui.R 
#RUN chmod -R 755 ./src/
#RUN chmod -R 755 ./www/
#RUN chmod -R 755 ./cpp/
#RUN chmod -R 755 ./js/

# expose port (for local deployment only)
# EXPOSE 3838 

# set non-root                       
RUN useradd shiny_user
USER shiny_user

# run app
CMD ["R", "-e", "shiny::runApp('./', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT')))"]