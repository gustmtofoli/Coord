FROM rocker/shiny-verse:latest

RUN sudo apt-get -y update
RUN sudo apt-get -y install libgdal-dev
RUN sudo apt-get -y install libproj-dev
RUN sudo apt-get -y install libprotobuf-dev
RUN sudo apt-get -y install libjq-dev
RUN sudo apt-get -y install libv8-dev
RUN sudo apt-get -y install libudunits2-dev
RUN sudo apt-get -y install protobuf-compiler
RUN sudo apt-get -y install libcurl4-openssl-dev
RUN sudo apt-get -y install libssl-dev

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboardPlus', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('DT', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('spocc', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sp', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"

ADD samples/ /srv/shiny-server/
RUN mkdir www
ADD www/ /srv/shiny-server/www/
COPY Coord.Rproj /srv/shiny-server/
COPY SpeciesDataFromDataBasesOutputs.R /srv/shiny-server/
COPY SpeciesDataFromDataBasesService.R /srv/shiny-server/
COPY app.R /srv/shiny-server/

EXPOSE 3838

RUN sudo chown -R shiny:shiny /srv/shiny-server
