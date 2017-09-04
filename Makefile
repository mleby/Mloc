build: relupdatedb rellocate relclient

relupdatedb:
	cd updatedb && lazbuild --build-all --build-mode=Release updatedb.lpr

relclient:
	cd client && lazbuild --build-all --build-mode=Release Mloc.lpr

rellocate:
	cd locate && lazbuild --build-all --build-mode=Release locate.lpr

debugAll:
	cd locate && lazbuild --build-all --build-mode=Debug locate.lpr
	cd client && lazbuild --build-all --build-mode=Debug Mloc.lpr
	cd updatedb && lazbuild --build-all --build-mode=Debug updatedb.lpr

runtest: build
	cd tests && lazbuild --build-all -q mloctests.lpr && ./mloctests --format=plain -a -p

README.md: README.mds
	mdpreproc < README.mds > README.md

install: runtest # README.md
	cp -v client/Mloc ~/bin/
	cp -v updatedb/updatedb ~/bin/
	cp -v locate/locate ~/bin/
