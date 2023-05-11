// short test program for Gaussian duration model
#include <iostream>
#include <math.h>

using namespace std;

double theT;
double theTime() {
    return theT;
}
double theDeltaT() {
    return 0.1;
}

double normalcdf(double time, double mean, double sigma) {
    if (0 == sigma) {
        cerr << "Invalid sigma 0 in normalcdf.\n";
        return false;
    }
    return 0.5 * (1.0 + erf(1/sqrt(2.0) * (time - mean)/sigma));
}

bool normalModel(double startTime, double mean, double sigma) {
    double x,a,b;

    a = (theTime() - theDeltaT()) - startTime;
    b = theTime() - startTime;

    double cdfa = normalcdf(a, mean, sigma);
    double cdfb = normalcdf(b, mean, sigma);
    if (1.0 == cdfa) {
        return true;
    }
    x = drand48();
    double r =((cdfb - cdfa) / (1.0 - cdfa));

    return x<=r;
}

const int n = 1000000;
int nleft;

main() {
    double mean = 10;
    double sigma = 2.5;

    nleft = n;
    for (theT=0; theT<20; theT+=theDeltaT()) {
        int ndied = 0;
        for (int i=0; i<nleft; i++) {
            if (normalModel(0,mean,sigma)) {
                ndied++;
            }
        }
        nleft -= ndied;
        cout << theT << ' ' << (ndied*1.0)/(n*theDeltaT()) << '\n';
    }
}
