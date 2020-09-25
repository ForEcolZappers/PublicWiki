---
presentation:
  title: 'Brief introduction to Docker'
  author: 'Alex Guizar'
  theme: 'simple.css'
  transition: 'fade'
  backgroundTransition: 'fade'
---

---

![](https://www.nebulaworks.com/blog/wp-content/uploads/2016/08/01-docker-container.jpg)

**Brief introduction to Docker**

---

**Docker**

_(not a virtual machine)_


---

- Consistent environments: tools, dependencies are installed when you deploy a container
- Persistent networking and data I/O
- Scalable architecture (...)

---

![](https://www.docker.com/sites/default/files/d8/2018-11/docker-containerized-and-vm-transparent-bg.png)


---

**Core concepts when using Docker**

- IMAGE
- CONTAINER
- DOCKERFILE

---

![](https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse1.mm.bing.net%2Fth%3Fid%3DOIP.NJ7bf_myr1RWlXPq_Vm6IwHaFj%26pid%3DApi&f=1)

---

**DOCKERFILE**

- It's a recipe for building your virtual environment
- Contains a list of actions describing how to build your images and processes to run when building these

---

![](https://i.imgur.com/rVMTm1C.png)

---

**IMAGE**

- Form the basis of containers
- Stackable layers
- Uses the same kernels as the Unix systems to build the containers on top

---

**A `CONTAINER` is a running instance of an `IMAGE`**

The image serves a template to create the target environment, OS/software and libraries

---

![](https://workwiththebest.intraway.com/wp-content/uploads/sites/4/2016/10/docker-filesystems-multilayer.png)


---

**Using Docker in a nutshell**

1. Write a `DOCKERFILE`

2. `build` an `IMAGE` from a `DOCKERFILE`

3. `run` a `CONTAINER` from an `IMAGE`

---

- Install DOCKER: https://www.docker.com/products/docker-desktop

---

### 1. Sourcing from another IMAGE

<!-- With the proliferation of Docker images you'd probably want to start by simpling running an IMAGE into a CONTAINER
-->

---

![](https://i.imgur.com/QNVHa4p.png)

---

- **Example: `rocker-geospatial`**
https://www.rocker-project.org/


```shell
docker run -p 8787:8787 -e USERID=$UID -e ROOT=TRUE -e PASSWORD=YOURPASS --name rocker -v YOURDIRECTORY:/home/rstudio/ rocker/geospatial:latest
```

---

- Open `localhost:8787` in your browser. Use the following details to log in to the Rstudio session.
- `user`: rstudio
- `password`: yourpassword


---

### 2. Toy APP developed entirely in Docker

---

`Dockerfile`

```shell
#Base image
FROM rocker/r-ver:3.4.4

# Run command on the native OS at image build time / create directory
RUN mkdir /home/app \
&&  mkdir /home/app/src/

# Copy files to image
COPY src/source-me.R /home/app/src/source-me.R
COPY src/prim_forests_loss_area.csv /home/app/src/prim_forests_loss_area.csv

# Execute Rscript
CMD cd /home/app/src/ \
  && R -e "source('source-me.R')"
```

---

To run this exercise:

```shell
docker build -t defor-img .
docker run --rm -v PATH-TO-YOUR-FOLDER:/home/app defor-img
```

---

### 3. Jupyter environment

https://github.com/guizar/jupyter-gdal-env

---

Thank you!
