FROM plotly/heroku-docker-r:3.6.2_heroku18

# on build, copy application files
COPY . /analytics/

# for installing additional dependencies etc.
RUN if [ -f '/analytics/onbuild' ]; then bash /app/onbuild; fi; 

# look for /app/apt-packages and if it exists, install the packages contained
RUN if [ -f '/analytics/apt-packages' ]; then apt-get update -q && cat apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;              

# look for /app/init.R and if it exists, execute it
RUN if [ -f '/analytics/init.R' ]; then /usr/bin/R --no-init-file --no-save --quiet --slave -f /analytics/init.R; fi; 

# here app.R needs to match the name of the file which contains your app              
CMD cd /analytics && /usr/bin/R --no-save -f /analytics/analytics.R

#dev-z963u7g0.eu.auth0.com