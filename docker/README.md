# Running the code with Docker

[Docker][] provides an alternative way to run the model without the need to
setup or install any dependencies.

A Dockerfile is provided in the `docker` directory. To build an image containing
all of the dependencies of the model run the following command from the root
directory of the project:

```
docker build -f docker/Dockerfile -t covid19model:latest .
```

If your user is not in the docker group then you will need to run the above
command prefixed with `sudo`. The same applies to the below.

This creates an image that can then be used to execute the model. From the root
directory of the project run the command:

```
docker run --rm -it -v $(pwd):/var/model covid19model:latest $(id -u) $(id -g)
```

In the above the `-v $(pwd)/:/var/model` option makes the current directory
accessible when the container is run. This means the code which is executed is
that in the current directory and includes any changes made. The use of `$(id
-u)` and `$(id -g)` ensure that all model output files will be owned by the
current user instead of root.

[Docker]: https://www.docker.com/
