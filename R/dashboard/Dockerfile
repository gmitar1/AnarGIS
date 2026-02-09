# Use R 4.5.2 with Shiny (matches your version)
FROM rocker/shiny-verse:4.5.2

# SQLite dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# R packages
RUN R -e "install.packages(c('RSQLite', 'DBI', 'ggplot2'), repos='https://cloud.r-project.org/')"

COPY app.R /srv/shiny-server/
COPY podaci.sqlite /srv/shiny-server/
COPY stilovi.css /srv/shiny-server/

# Fix permissions for Shiny server
RUN chown -R shiny:shiny /srv/shiny-server/

# Expose Shiny default port
EXPOSE 3838

# Run the application
CMD ["/usr/bin/shiny-server"]
