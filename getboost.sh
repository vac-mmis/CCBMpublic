BOOST_V=1_64_0

wget 'https://boostorg.jfrog.io/artifactory/main/release/1.64.0/source/boost_1_64_0.tar.bz2' -O boost.tar.bz2
tar xjf boost.tar.bz2
rm boost.tar.bz2
mv boost_1_64_0/ src/lib/boost/
