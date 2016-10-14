#!/bin/bash
cd updatedb
lazbuild --build-all -q updatedb.lpr
cd ..

cd client
lazbuild --build-all -q Mloc.lpr
cd ..

cd tests
lazbuild --build-all -q mloctests.lpr
./mloctests
cd ..

