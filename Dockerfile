FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev

RUN R -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'tidyverse', 'lubridate', 'plotly', 'reshape2', 'shinyWidgets'), repos='https://cran.rstudio.com/')"

COPY ./app /srv/shiny-server/

RUN chmod -R 755 /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]