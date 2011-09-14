#!/bin/bash
shopt -s extglob
# Absolute path to this script
SCRIPT=`readlink -f $0`
SCRIPTPATH=`dirname $SCRIPT`
cd $SCRIPTPATH && rm -f *.sgf.@(png|toplay)
