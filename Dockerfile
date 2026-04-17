FROM python:3.11-bookworm

ENV DEBIAN_FRONTEND=noninteractive \
    PYTHONUNBUFFERED=1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        r-base \
        r-base-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        gfortran \
        make \
        g++ \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

RUN Rscript -e "install.packages(c('shiny', 'ggplot2', 'dplyr', 'tidyr', 'lmtest', 'car'), repos='https://cloud.r-project.org')"

COPY . /workspace

EXPOSE 3838

CMD ["sleep", "infinity"]
