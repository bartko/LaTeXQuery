#!/bin/bash


######################################
# Copyright (C) 2014 Bartosz Marciniak
# marciniak.b@gmail.com
######################################


basexpath=/basex772

${basexpath}/bin/basex -b input-document="$1" \
      -b output-document="$2" \
      -b settings-document="settings.xml" \
      "latexquery.xq"





