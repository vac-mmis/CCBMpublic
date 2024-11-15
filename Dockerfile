FROM haskell:9.2-slim

RUN apt update
RUN apt install nano
RUN apt install wget
RUN apt install unzip

RUN groupadd ccbm
RUN useradd -m -g ccbm -s /bin/bash ccbm

RUN mkdir -p /home/ccbm

WORKDIR /home/ccbm


# Install the latest Version of CCBM
RUN wget https://github.com/vac-mmis/CCBMpublic/archive/refs/heads/main.zip -O ccbm.zip
RUN unzip ccbm.zip
RUN rm ccbm.zip


# Install Boost
RUN wget 'https://boostorg.jfrog.io/artifactory/main/release/1.64.0/source/boost_1_64_0.tar.bz2' -O boost.tar.bz2
RUN tar xjf boost.tar.bz2
RUN mkdir CCBMpublic-main/src/lib
RUN mv boost_1_64_0 CCBMpublic-main/src/lib/boost/
RUN rm boost.tar.bz2


# Install the FANN-library
RUN wget https://github.com/libfann/fann/archive/refs/heads/master.zip -O fann.zip
RUN unzip fann.zip
RUN mv fann-master CCBMpublic-main/src/lib/fann
RUN rm fann.zip


WORKDIR /home/ccbm/CCBMpublic-main

RUN cabal update
RUN make -i
RUN ln -s /home/ccbm/CCBMpublic-main/RCGen/dist-newstyle/build/aarch64-linux/ghc-9.2.8/RCGen-0.1.0.0/x/rcg/build/rcg/rcg RCGen/rcg

# make sure that rc can be found
RUN echo "PATH=\"\$PATH:/home/ccbm/CCBMpublic-main\"" >> /home/ccbm/.bashrc

RUN echo "PS1='CCBM[\w]\$ '" >> /home/ccbm/.bashrc

RUN chown -R ccbm:ccbm /home/ccbm
USER ccbm

CMD bash
