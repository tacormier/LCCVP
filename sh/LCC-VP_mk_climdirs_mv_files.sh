#!/bin/bash

if [ -z "$1" ]; then
    echo "usage: LCC-VP_mk_climdirs_mv_files.sh  <indir>"
    exit
fi

indir=${1}
qrcpdirs=

#abandoned this for python
