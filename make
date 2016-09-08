#!/bin/bash

APP_NAME=Faple
SRC_DIR=./$APP_NAME
SRC_FILES="./$APP_NAME/Parser.fs \
           ./$APP_NAME/Eval.fs \
           ./$APP_NAME/Builtins.fs \
           ./$APP_NAME/Faple.fs"
BUILD_DIR=build
FAILED=0
LIBS="\
      -r packages/FParsec/lib/net40-client/FParsecCS.dll\
      -r packages/FParsec/lib/net40-client/FParsec.dll\
      --staticlink:FParsec\
      --staticlink:FParsecCS"

function build {
    echo ">>> Building.."
    fsc --nologo $SRC_FILES $LIBS -o $BUILD_DIR/$APP_NAME.exe
    FAILED="$?"
}

function run {
    echo ">>> Running.."
    ./$BUILD_DIR/$APP_NAME.exe
}


build
if [ $FAILED -ne 0 ]
then
    echo ">>> Build failed"
    exit
fi

echo ">>> Build succeeded"

if [ "$1" == "run" ]
then
    run
fi


