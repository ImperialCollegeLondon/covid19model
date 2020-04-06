FROM jrnold/rstan:latest

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr svglite \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

WORKDIR /var/model
CMD Rscript base.r base