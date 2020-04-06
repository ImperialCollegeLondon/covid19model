FROM jrnold/rstan:latest

RUN apt-get update && apt-get install -y \
    fonts-open-sans \
    fonts-arkpandora \
	fonts-adf-verana

RUN ttf-ubuntu-font-family

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr svglite \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

WORKDIR /var/model
CMD Rscript base.r base