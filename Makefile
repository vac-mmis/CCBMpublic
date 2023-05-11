all:
	(cd RCGen; make)

install:
	ln -s `pwd`/rc /usr/local/bin/rc

backup:
	(cd RCGen; make clean)
	cd ..; tar cvfz RC3-`date +%Y%m%d-%H%M`.tgz RC3)
