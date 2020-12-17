# based on Debian OS
FROM rocker/verse:latest

RUN R --quiet -e "install.packages(c('scorecard', 'h2o', 'xgboost'), repos = 'https://mirrors.tuna.tsinghua.edu.cn/CRAN/')" && \
  rm -rf /tmp/*

# docker build -t shichenxie/scorecard .
# docker run --rm -p 8787:8787 -e PASSWORD=pwd shichenxie/scorecard
# CTRL+C # quit running docker images

# docker login --username=shichenxie --email=shichen_xie@qq.com
# docker push shichenxie/scorecard
# ref # http://ropenscilabs.github.io/r-docker-tutorial/
