FROM jrnold/rstan:latest

RUN install2.r --error --deps TRUE \
	gdata EnvStats ggpubr svglite \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY docker/run_model_script.sh /root/
COPY docker/download_data.sh /root/
WORKDIR /var/model
ENTRYPOINT [ "sh" ]
CMD ["/root/run_model_script.sh"]

