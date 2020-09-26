#Base image
FROM rocker/r-ver:3.4.4

# Run command on the native OS at image build time / create directory
RUN mkdir /home/app \
&&  mkdir /home/app/src/

# Copy files to image
COPY src/source-me.R /home/app/src/source-me.R
COPY src/prim_forests_loss_area.csv /home/app/src/prim_forests_loss_area.csv

# Execute Rscript
CMD cd /home/app/src/ \
  && R -e "source('source-me.R')"
