#!/bin/bash

echo
echo ------------------------------------------------------------------
cd updatedb
lazbuild --build-all --build-mode=Release updatedb.lpr
cd ..

echo
echo ------------------------------------------------------------------
cd client
lazbuild --build-all --build-mode=Release Mloc.lpr
cd ..

echo
echo ------------------------------------------------------------------
cd tests
lazbuild --build-all -q mloctests.lpr
./mloctests
cd ..

