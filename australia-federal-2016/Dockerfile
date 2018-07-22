FROM rocker/tidyverse

# Dockerfile to demonstrate knitting Rmd notebook as an example of using Docker for reproducible research

# to build: docker build -t australia-federal-2016 .
# to run (replace mount source with wherever on local host you'd like output to go): docker run --rm --mount "type=bind,target=/analysis-output,source=/tmp" australia-federal-2016

VOLUME /analysis-output

RUN R -e 'install.packages(c("ggthemes", "scales"))'
RUN mkdir /analysis-source
COPY Notebook.Rmd /analysis-source/

CMD ["R", "-e", "rmarkdown::render('/analysis-source/Notebook.Rmd', output_file='/analysis-output/Notebook.html')"]
