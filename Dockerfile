FROM python:3.11-bookworm

ENV DEBIAN_FRONTEND=noninteractive \
    PYTHONUNBUFFERED=1

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates \
        r-base \
        r-cran-shiny \
        r-cran-ggplot2 \
        r-cran-dplyr \
        r-cran-tidyr \
        r-cran-lmtest \
        r-cran-car \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /workspace

COPY requirements.txt /tmp/requirements.txt
RUN pip install --no-cache-dir -r /tmp/requirements.txt

RUN Rscript -e "stopifnot(requireNamespace('shiny', quietly = TRUE)); stopifnot(requireNamespace('ggplot2', quietly = TRUE)); stopifnot(requireNamespace('dplyr', quietly = TRUE)); stopifnot(requireNamespace('tidyr', quietly = TRUE)); stopifnot(requireNamespace('lmtest', quietly = TRUE)); stopifnot(requireNamespace('car', quietly = TRUE))"

COPY . /workspace

EXPOSE 3838

CMD ["sleep", "infinity"]
