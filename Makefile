install-r-ubuntu-16.04:
	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
	sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'
	sudo apt-get update
	sudo apt-get install r-base r-base-dev
	sudo apt-get install libgdal-dev libproj-dev

install-dependencies-ubuntu-16.04:
	sudo apt-get install libprotobuf-dev
	sudo add-apt-repository -y ppa:opencpu/jq
	sudo apt-get update
	sudo apt-get install libjq-dev
	sudo apt-get install libv8-dev
	sudo apt-get install libudunits2-dev
	sudo apt-get update

install-dependencies-ubuntu-18.04:
	sudo apt-get install libgdal-dev libproj-dev
	sudo apt-get install libprotobuf-dev
	sudo apt-get install libjq-dev
	sudo apt-get install libv8-dev
	sudo apt-get install libudunits2-dev
	sudo apt-get install protobuf-compiler
	sudo apt-get install libcurl4-openssl-dev
	sudo apt-get install libgdal-dev
	sudo apt-get install libssl-dev

install-r-ubuntu-18.04:
	sudo apt-get install r-base r-base-dev
	sudo apt-get install libgdal-dev libproj-dev

