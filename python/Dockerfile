FROM continuumio/miniconda:4.7.12
RUN conda install python=3.7
RUN conda install py4j --yes
RUN conda install jupyter jupyterlab --yes
EXPOSE 8888 8888
RUN mkdir /base
RUN mkdir /pylibs
RUN mkdir /pylibs/interface
RUN touch /pylibs/interface/__init__.py
COPY ./interface.py /pylibs/interface
COPY ./Example.ipynb /base
WORKDIR /base
ENV PYTHONPATH "${PYTHONPATH}:/pylibs"
CMD jupyter lab --ip=0.0.0.0 --port 8888 --allow-root