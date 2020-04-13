# Running the code with Docker

[Docker][] provides an alternative way to run the model without the need to
manually setup or install any dependencies. The Docker file contains the latest version of the model and can be used to run it independently of the repo. 

### Running on Linux or MacOS
```
docker run --rm -v $(pwd)/results:/var/model/results -v $(pwd)/figures:/var/model/figures harrisonzhu5080/covid19model:latest
```

### Running on Windows using PowerShell
```
docker run --rm -v ${pwd}/results:/var/model/results -v ${pwd}/figures:/var/model/figures harrisonzhu5080/covid19model:latest
```

### Settings
Two environment variables are supported for setting the model run parameters:
* DEBUG=TRUE, performs a very fast debug run
* FULL=TRUE, performs a full run of the model

Those options can also be passed directly to base.r on the command line setting either `--full` or `--debug`. 

When neither option is set a longer debug run is performed. For proper estimates always use `FULL=TRUE` or the `--full` flag. 

The usual option of setting the user for Docker `--user $(id -u):$(id -g)` is also supported.

## Running custom changes using Docker

If you want to run your own changes in the Docker image above the whole work directory can be mounted as volume replacing, the contents in the Docker image itself. 

### Linux or MacOS
```
docker run --rm -v $(pwd):/var/model harrisonzhu5080/covid19model:latest
```

Other files in the directory can also be run, using:

```
docker run --rm -v $(pwd):/var/model harrisonzhu5080/covid19model:latest Rscript <filename>
```

### Windows using PowerShell
```
docker run --rm -v ${pwd}:/var/model harrisonzhu5080/covid19model:latest
```

Other files in the directory can also be run, using:

```
docker run --rm -v ${pwd}:/var/model harrisonzhu5080/covid19model:latest Rscript <filename>
```

[Docker]: https://www.docker.com/


## Building the Docker image

A Dockerfile is provided in the `docker` directory. To build an image containing
all of the dependencies of the model run the following command from the root
directory of the project:

```
docker build -f docker/Dockerfile -t covid19model:latest .
```
