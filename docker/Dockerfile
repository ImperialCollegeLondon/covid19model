FROM jrnold/rstan:latest

RUN add-apt-repository universe && apt-get update && apt-get install -y \
    ttf-ubuntu-font-family \
    fonts-open-sans \
    fonts-arkpandora

RUN ttf-ubuntu-font-family

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr svglite \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

WORKDIR /var/model
CMD Rscript base.r base