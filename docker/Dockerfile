FROM jrnold/rstan:latest

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY docker/run_model_script.sh /root/
WORKDIR /var/model
ENTRYPOINT ["sh", "/root/run_model_script.sh"]

