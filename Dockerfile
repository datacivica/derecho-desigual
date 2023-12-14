FROM rocker/r-ver:4.3.2
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

RUN apt-get install -y --fix-missing libfreetype6-dev libpng-dev libtiff-dev libjpeg-dev libcurl4-openssl-dev libxml2-dev libssl-dev libfontconfig1-dev

RUN echo "options(repos ='https://cran.rstudio.com/', unzip = 'internal', download.file.method = 'libcurl', Ncpus = parallel::detectCores())" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e 'install.packages("pacman", dependencies = TRUE)'
RUN Rscript -e 'install.packages("shiny", dependencies = TRUE)'
RUN Rscript -e 'install.packages("tidyverse", dependencies = TRUE)'
RUN Rscript -e 'install.packages("ggplot2", dependencies = TRUE)'
RUN Rscript -e 'install.packages("spatstat", dependencies = TRUE)'
RUN Rscript -e 'install.packages("mxmaps", dependencies = TRUE)'
RUN Rscript -e 'install.packages("here", dependencies = TRUE)'
RUN Rscript -e 'install.packages("data.table", dependencies = TRUE)'
RUN Rscript -e 'install.packages("scales", dependencies = TRUE)'
RUN Rscript -e 'install.packages("ggrepel", dependencies = TRUE)'
RUN Rscript -e 'install.packages("BiocManager", dependencies = TRUE)'
RUN Rscript -e 'install.packages("bslib", dependencies = TRUE)'

COPY run-app.R /run-app.R

RUN mkdir /app
COPY app /app

RUN mkdir /prep-app
COPY prep-app /prep-app

RUN mkdir /data

RUN chmod -R 755 /run-app.R
RUN chmod -R 755 /app
RUN chmod -R 755 /prep-app
RUN chmod -R 755 /data
# RUN mkdir /root/data
# 
# RUN chmod -R 755 run-app.R
# RUN chmod -R 755 app
# RUN chmod -R 755 prep-app
# RUN chmod -R 755 data

# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
##RUN chmod -R 755 $HOME/build_zone
##RUN chmod -R 755 $HOME/build_zone/app
##RUN chmod -R 755 $HOME/build_zone/app/data

# Fonts
RUN apt-get update; apt-get install -y fontconfig

# Barlow Condensed
# Bold
RUN mkdir -p /usr/share/fonts/truetype/Barlow_Condensed
COPY /app/www/fonts/Barlow_Condensed/BarlowCondensed-Bold.ttf /usr/share/fonts/truetype/Barlow_Condensed
# Regular
RUN mkdir -p /usr/share/fonts/truetype/Barlow_Condensed
COPY /app/www/fonts/Barlow_Condensed/BarlowCondensed-Regular.ttf /usr/share/fonts/truetype/Barlow_Condensed

# Barlow
RUN mkdir -p /usr/share/fonts/truetype/Barlow
COPY /app/www/fonts/Barlow/Barlow-Regular.ttf /usr/share/fonts/truetype/Barlow

# Roboto Slab
RUN mkdir -p /usr/share/fonts/truetype/Roboto_Slab
COPY /app/www/fonts/Roboto_Slab/RobotoSlab-VariableFont_wght.ttf /usr/share/fonts/truetype/Roboto_Slab

# Install fonts
RUN fc-cache -f -v


EXPOSE 3838

CMD ["Rscript", "/run-app.R"]

