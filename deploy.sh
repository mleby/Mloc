#!/bin/bash

./build.sh

cp -v client/Mloc ~/bin/
cp -v updatedb/updatedb ~/bin/
cp -v locate/locate ~/bin/

# TODO zajistit deploy indexeru
