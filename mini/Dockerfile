FROM --platform=linux/amd64 ubuntu:26.04 

ENV DEBIAN_FRONTEND=noninteractive 

# 基本ツール 

RUN apt update && \ 

    apt install -y --no-install-recommends \ 

        curl \  

        gnupg \ 

        ca-certificates \ 

        gdebi-core  

 

# CRAN key 登録 

RUN mkdir -p /etc/apt/keyrings && \ 

    curl -fsSL https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc \ 

    | gpg --dearmor -o /etc/apt/keyrings/cran.gpg 

 

RUN echo "deb [signed-by=/etc/apt/keyrings/cran.gpg] \ 

    https://cloud.r-project.org/bin/linux/ubuntu resolute-cran40/" \ 

    > /etc/apt/sources.list.d/cran-r.list 

 

# R install 

RUN apt update && \ 

    apt install -y --no-install-recommends \ 

    r-base \ 

    r-base-dev 

 

# ライブラリ 

RUN apt install -y --no-install-recommends \ 

    libfreetype-dev \ 

    libfontconfig1-dev \ 

    libcairo2-dev \ 

    libharfbuzz-dev \ 

    libfribidi-dev 

 

#R 使用パッケージ 

RUN Rscript -e \ 

    "install.packages(c('shiny','rmarkdown'), repos='https://cloud.r-project.org')" 

# Shiny Server install 

RUN curl -fL --retry 5 --retry-all-errors -o shiny-server-1.5.23.1030-amd64.deb \ 

    https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb && \ 

    gdebi -n shiny-server-1.5.23.1030-amd64.deb 

 

RUN rm -rf /var/lib/apt/lists/* 

EXPOSE 3838 

CMD ["/usr/bin/shiny-server"] 