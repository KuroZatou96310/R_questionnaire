FROM --platform=linux/amd64 ubuntu:26.04

ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# 基本ツール
RUN apt update && \
    apt install -y --no-install-recommends \
    openssh-server \
    wget \
    curl \
    gnupg \
    ca-certificates \
    software-properties-common \
    dirmngr \
    gdebi-core \
    git

# CRAN key 登録
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \
    | gpg --dearmor -o /etc/apt/keyrings/cran.gpg

RUN echo "deb [signed-by=/etc/apt/keyrings/cran.gpg] https://cloud.r-project.org/bin/linux/ubuntu resolute-cran40/" \
    > /etc/apt/sources.list.d/cran-r.list
# R install
RUN apt update && \
    apt install -y --no-install-recommends \
    r-base \
    r-base-dev

# Shiny 用ライブラリ
RUN apt install -y --no-install-recommends \
    libfreetype-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev\
    libtiff5-dev \
    libuv1-dev

# 日本語用フォント
RUN apt-get install -y fonts-noto-cjk

#Rの使うライブラリあったらここ入れといて
RUN Rscript -e "install.packages(c('qrencoder','shiny','rmarkdown','jsonlite','uuid','digest', 'DBI', 'RSQLite','bslib','showtext','ggplot2'), repos='https://cloud.r-project.org')"

# Shiny Server install
RUN curl -fL --retry 5 --retry-all-errors -o shiny-server-1.5.23.1030-amd64.deb \
    https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb && \
    gdebi -n shiny-server-1.5.23.1030-amd64.deb

# app 配置
#COPY shinyApp/ /srv/shiny-server/
# 所有権をshinyに統一



## データ保存用ディレクトリ作成
RUN rm -rf /srv/shiny-server/*

RUN git clone --depth 1 \
    https://github.com/KuroZatou96310/R_questionnaire.git \
    /srv/shiny-server

# データベース用ディレクトリ作成と権限付与
RUN mkdir /srv/shiny-server/data
RUN chown shiny:shiny /srv/shiny-server/data

RUN cp \
    /srv/shiny-server/shiny-server.conf \
    /etc/shiny-server/shiny-server.conf

# port
EXPOSE 3838

# 起動
CMD ["/usr/bin/shiny-server"]




#docker build -t shiny-server .\
#docker run -d -p 3838:3838 --name aa22222 shiny-server
#Ctrl + Shift + P -> Dev Containers: Attach to Running Container... -> aaaaaaaみたいな感じ
