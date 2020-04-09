FROM jrnold/rstan:latest

RUN echo "deb http://http.debian.net/debian stretch-backports main contrib non-free" >> /etc/apt/sources.list.d/stretch-backports.list
RUN apt-get update && apt-get install -y \
    fonts-open-sans \
    fonts-arkpandora \
	fonts-adf-verana

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr svglite jsonlite \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

WORKDIR /var/model
CMD Rscript base.r base