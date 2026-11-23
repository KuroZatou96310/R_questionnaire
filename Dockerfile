FROM --platform=linux/amd64 rocker/shiny:latest

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

# アプリで使うフォント、Git、追加パッケージのシステム依存
RUN apt-get update && apt-get install -y --no-install-recommends \
        fonts-noto-cjk \
        git \
        libfontconfig1-dev \
        libfribidi-dev \
        libharfbuzz-dev \
        libtiff-dev \
        libuv1-dev \
        openssh-server \
    && rm -rf /var/lib/apt/lists/*

# rocker/shiny に含まれるパッケージは再インストールせず、不足分だけ追加
RUN install2.r --error --skipinstalled -n -1 \
        qrcode DBI RSQLite showtext ggplot2 wordcloud uuid

# アプリ配置
RUN rm -rf /srv/shiny-server/*

RUN git clone --depth 1 \
    https://github.com/KuroZatou96310/R_questionnaire.git \
    /srv/shiny-server

# データベース用ディレクトリ作成と権限付与
RUN mkdir -p /srv/shiny-server/data \
    && chown shiny:shiny /srv/shiny-server/data

RUN cp /srv/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

#docker build -t shiny-server .\
#docker run -d -p 3838:3838 --name questionnaire shiny-server
#Ctrl + Shift + P -> Dev Containers: Attach to Running Container... -> aaaaaaaみたいな感じ
