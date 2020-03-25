# Table of Contents

- [Create image of OpenFrame](#step-1-create-image-of-openframe)
	* [install docker](#11-install-docker)
	* [get centos container](#12-get-centos-container)
	* [install OpenFrame](#12-full-example)
	* [create OpenFrame image](#12-full-example)
        * [use OpenFrame image](#12-full-example)
- [Azure](#step-2-dbdgen)
	* [add AKS service](#21-usage)
	* [connect to the Node](#22-full-example)
	* []

## Step 1. Create image of OpenFrame

### 1.1 Install docker

First, you need to get the OpenFrame image to use the AKS service. To create the image, you need to install Docker and create your own account. Your account will be needed when you push/pull the images in your repository in Dockerhub.

```sudo apt-get remove docker docker-engine docker.io```

```sudo apt install docker.io```

```sudo systemctl start docker```

```sudo systemctl enable docker```

```sudo docker –version```

	
### 1.2 Get centos container

**Run an empty Centos container to install OpenFrame.** 

Search the official Centos image and pull it on your VM.

```sudo docker search centos```

```sudo docker pull centos```

Use the image to run a container. **Set the hostname with -h option when you run it.** 

OpenFrame will need the hostname to get the licenses or set the envionment.

```sudo docker run -h [hostname] -i -t centos ```

Other docker commands :

| COMMAND      | DESCRIPTION                     |
|--------------|---------------------------------|
| docker ps    | check running containers        |
| docker ps -a | check all containers            |
| docker exec  | execute a running container     |
| docker stop  | stop the container              |

**docker run and docker exec are different!!**

Example :

sudo docker ps -a | grep centos

|CONTAINER ID|IMAGE |COMMAND    |CREATED      |STATUS      |PORTS|NAMES       |   
|------------|------|-----------|-------------|------------|-----|------------|  
|fc58fa646357|centos|"/bin/bash"|2 minutes ago|Up 2 minutes|     |keen_poitras|

sudo docker exec -i -t fc58fa646357 /bin/bash

sudo docker stop fc58fa646357

