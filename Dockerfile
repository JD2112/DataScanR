# Start from the official Shiny image
FROM jd21/datascanr:test
LABEL authors="Jyotirmoy Das (jyotirmoy.das@liu.se)" \
    description="Exploratory Data Analysis from Clinical Data"

# Copy your Shiny app files into the container
RUN rm -rf /srv/shiny-server/
COPY ./DataScanR/ /srv/shiny-server/
COPY helpers/ /helpers/

# install shiny and Cairo
RUN Rscript /helpers/install.R shiny DT ggplot2 bslib data.table dlookr tidyr shinycssloaders dplyr patchwork ggpubr hrbrthemes ggdist corrplot pals NbClust

RUN Rscript /helpers/install.R plotly bsicons

# set shiny app directories and permissions
COPY ./DataScanR/shiny-server.conf /etc/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the default Shiny port
EXPOSE 3838

# Run the Shiny server
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=3838)"]
