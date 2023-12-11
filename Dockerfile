FROM rocker/r-ver:4.1.2
RUN apt-get update
RUN apt-get update
RUN apt-get install -y --fix-missing sudo git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev librsvg2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN apt-get update
RUN apt-get install -y cron nano wget libgdal-dev \
libjq-dev \
libproj-dev \
libprotobuf-dev \
protobuf-compiler \
make \
libgeos-dev \
libudunits2-dev \
gdal-bin \
libproj-dev \
libv8-dev \
netcdf-bin && rm -rf /var/lib/apt/lists/* && which cron && \
rm -rf /etc/cron.*/*

RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("pkgload",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.6")'

# App packages
RUN Rscript -e 'install.packages("pacman")'
RUN Rscript -e 'install.packages("bslib")'
RUN Rscript -e 'install.packages("googledrive")'
RUN Rscript -e 'install.packages("googlesheets4")'
RUN Rscript -e 'install.packages("gargle")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("tidyr")'
RUN Rscript -e 'install.packages("magrittr")'
RUN Rscript -e 'install.packages("lubridate")'
RUN Rscript -e 'install.packages("ggmosaic")'
RUN Rscript -e 'install.packages("leaflet")'
RUN Rscript -e 'install.packages("leaflet.extras")'
RUN Rscript -e 'install.packages("sf")'
RUN Rscript -e 'install.packages("plotly")'
RUN Rscript -e 'install.packages("readr")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("janitor")'
RUN Rscript -e 'install.packages("htmlwidgets")'
RUN Rscript -e 'install.packages("shinyjs")'
RUN Rscript -e 'install.packages("data.table")'
RUN Rscript -e 'install.packages("shinycustomloader")'
RUN Rscript -e 'install.packages("geojsonio")'

RUN Rscript -e 'install.packages("tidyverse")'
RUN Rscript -e 'install.packages("here")'
RUN Rscript -e 'install.packages("scales")'
RUN Rscript -e 'install.packages("ggrepel")'
RUN Rscript -e 'install.packages("BiocManager")'

COPY run-app.R /root/run-app.R

RUN mkdir /root/app
COPY app /root/app

RUN mkdir /root/prep-app
COPY prep-app /root/prep-app

##RUN mkdir $HOME/build_zone
# copy the app to the image
##ADD ./ /root/build_zone/app
# copy bash script
##COPY ./google-sheet.sh /root/build_zone/
##COPY ./.secrets/ /root/build_zone/app/
# Give execution rights on the bash script
##RUN chmod 0744 /root/build_zone/google-sheet.sh
# copy cronjob to cron directory
##COPY ./crontab /etc/cron.d/crontab
# Give execution rights on the cron job
##RUN chmod 0644 /etc/cron.d/crontab
# Apply cron job
##RUN crontab /etc/cron.d/crontab
# Create the log file to be able to run tail
##RUN touch /var/log/cron.log
#
##COPY Rprofile.site /usr/lib/R/etc/

RUN mkdir /root/data

RUN chmod -R 755 /root/run-app.R
RUN chmod -R 755 /root/app
RUN chmod -R 755 /root/prep-app
RUN chmod -R 755 /root/data

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
##RUN chmod -R 755 $HOME/build_zone
##RUN chmod -R 755 $HOME/build_zone/app
##RUN chmod -R 755 $HOME/build_zone/app/data

# create new directory for fonts
##RUN mkdir -p /home/.fonts
# copy fonts to new directory
##COPY /app/www/fonts/Akshar/Akshar-VariableFont.ttf /home/.fonts
##COPY /app/www/fonts/Inter/Inter-VariableFont.ttf /home/.fonts
# install fonts
##RUN fc-cache -f -v

EXPOSE 3838

#COPY ./entrypoint.sh /entrypoint.sh
#RUN chmod 0744 /entrypoint.sh

#Run the shiny app and the cron
##CMD ["/entrypoint.sh"]

CMD ["R", "-e", "shiny::runApp('/root/run-app.R', host = '0.0.0.0', port = 3838)"]
#CMD ["tail", "-f", "/dev/null"]
