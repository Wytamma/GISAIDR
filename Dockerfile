FROM mambaorg/micromamba:1.5.8-alpine3.18

RUN micromamba install -y -n base \
	conda-forge:r-base=4.4.1 \
	conda-forge::r-devtools=2.4.5 \
	conda-forge::r-optparse=1.7.5

RUN eval "$(micromamba shell hook --shell `basename $SHELL`)"
COPY . /GISAIDR
RUN micromamba run -n base Rscript -e "devtools::install('/GISAIDR')"

RUN ln -s /GISAIDR/bin/GISAIDR /opt/conda/bin/GISAIDR
