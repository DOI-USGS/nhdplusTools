FROM rocker/geospatial

ENV _R_CHECK_CRAN_INCOMING_="false"
ENV _R_CHECK_FORCE_SUGGESTS_="true"
ENV _R_CHECK_DONTTEST_EXAMPLES_="false"
ENV APT_PKGS="libudunits2-dev libgdal-dev libgeos-dev libproj-dev libcurl4-openssl-dev libssh2-1-dev libssl-dev libxml2-dev zlib1g-dev git p7zip-full"

USER root

RUN wget --no-check-certificate -O /usr/local/share/ca-certificates/DOIRootCA2.crt https://raw.githubusercontent.com/dblodgett-usgs/hydrogeoenv/master/linux/DOIRootCA2.cer \
  && chmod 644 /usr/local/share/ca-certificates/DOIRootCA2.crt && update-ca-certificates

RUN apt-get update \
  && apt-get install -y --no-install-recommends ${APT_PKGS} \
  && apt-get install -y --no-install-recommends qpdf pandoc pandoc-citeproc \
  && export PATH="/usr/local/lib/R/site-library/littler/examples/:${PATH}" \
  && echo "options(Ncpus = $(nproc --all))" >> /usr/local/lib/R/etc/Rprofile.site \
  && install2.r devtools

COPY DESCRIPTION /check/

RUN cd /check \
  && Rscript -e 'devtools::install_dev_deps()'

RUN tlmgr update --self && \
  tlmgr install ec
