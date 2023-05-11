typedef struct {
  double x, y;
} Observation;

void setXYcoordinates(double x, double y);

extern Observation theState;

//////////////////////////////////////////////////////////////////////
// More or less fancy estimation stuff ...

#define HAS_ESTIMATOR

#define CELLS_NX 5
#define CELLS_NY 5


template <class Particle>
class Estimate : public ::Estimator<Particle> {
    double time;
    Weight cells[CELLS_NX][CELLS_NY];
    double estX, estY;
public:
    std::ostream& printHeader(std::ostream &os) const {
        // TODO
        return os;
    }

    void start(double time) {
        this->time = time;
        estX = estY = 0;

        for(int i=0;i<CELLS_NX; i++)
            for(int j=0;j<CELLS_NY;j++)
                cells[i][j] = 0;
    }

    void pre_collect(const Particle &p) {
        (void) p;
    }

    void collect(const Particle &p) {
        int xbin = floor(theState.x / 1.4);
        int ybin = floor(theState.y / 1.4);
        Weight w = p.getTotalWeight();
        double pw = p_value(prob(w));
        cells[xbin][ybin] += w;
        estX += theState.x * pw;
        estY += theState.y * pw;
    }

    void finish() {}

    std::ostream& print(std::ostream &os) const {
        os << time << ' ' << estX << ' ' << estY;
        
        for(int i=0;i<CELLS_NX; i++)
            for(int j=0;j<CELLS_NY;j++)
                os << ' ' << cells[i][j];
        os << std::endl;
        return os;
    }
};
