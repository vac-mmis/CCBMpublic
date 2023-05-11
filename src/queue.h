#pragma once

#include <limits>

template <typename T>
inline double getKey(T d);

template <typename T>
inline unsigned long getIndex(T d);

template <typename T>
inline void setIndex(T d, unsigned long i);

template <typename T, class C>
class Queue {
    T *heapData;
    unsigned long heapSize;

    unsigned long hparent(unsigned long i) {
        return i > 0 ? (i-1) / 2 : 0;
    }
    unsigned long hleft(unsigned long i) {
        return 2*i+1;
    }
    unsigned long hright(unsigned long i) {
        return 2*i+2;
    }

    double hkey(unsigned long i) {
        return getKey(heapData[i]);
    }
    void hAssign(unsigned long index, T p) {
        heapData[index] = p;
        setIndex(p,index);
    }

    void hSwap(unsigned long i, unsigned long j) {
        T tmp;

        tmp = heapData[i];
        hAssign(i,heapData[j]);
        hAssign(j,tmp);
    }

    void hmake(unsigned long size) {
        // deleted in destructor
        heapData = new T[size];
        heapSize = size;
    }

    void heapify(unsigned long i) {
        unsigned long smallest;
        // cerr << "heapify " << i << ' ' << hleft(i) << ' ' << hright(i) << ' ' << hkey(i) << '\n';

        if (hleft(i) < heapSize && hkey(hleft(i))<hkey(i))
            smallest = hleft(i);
        else
            smallest = i;
        if (hright(i) < heapSize && hkey(hright(i))<hkey(smallest))
            smallest = hright(i);
        if (smallest!=i) {
            hSwap(i,smallest);
            heapify(smallest);
        }
    }

    void buildheap() {
        if (heapSize<2) return; // nothing to do ...
        // loop should actually read for (...; i >= 0; ...)
        // as i is unsigned, i >= 0 always holds, so we test if i < 0-1 = 0xFFFF...
        for (unsigned long i = heapSize/2-1; i<std::numeric_limits<unsigned long>::max(); i--)
            heapify(i);
    }

public:
    Queue(): heapData(NULL) {}

    ~Queue() {
        if (heapData)
            delete[] heapData;
    }

    void make(C &container) {
        unsigned long i = 0;

        hmake(container.size());
        // cout << "Router size: " << container.size() << '\n';
        for (auto &e: container) {
            hAssign(i, &e);
            // cout << "Router " << i << ": " << getKey(t) << '\n';
            i++;
        }

        buildheap();
    }

    bool empty() {
        return heapSize == 0;
    }

    T get() {
        T val;

        if (heapSize == 0) throw std::logic_error("Router::get: Queue Empty");

        val = heapData[0];
        heapSize--;
        hAssign(0, heapData[heapSize]);

        heapify(0);
        return val;
    }

    void decrease(unsigned long index) {
        while (index>0 && hkey(index) < hkey(hparent(index))) {
            hSwap(index,hparent(index));
            index=hparent(index);
        }
    }
};


template<typename Vertex>
int numChildren(Vertex v);
template<typename Vertex>
Vertex getChild(Vertex v, int i);
template<typename Vertex>
double getWeight(Vertex v, int i);
template<typename Vertex>
double getDistance(Vertex v);
template<typename Vertex>
void setDistance(Vertex v, double d);
template<typename Vertex>
void setNext(Vertex v, Vertex u);
template<typename Vertex>
void showInfo(const char *what, Vertex v);

template<typename Vertex, typename Container>
void dijkstra(Container &container) {
    Queue<Vertex, Container> q;
    Vertex u, v;
    int i;
    double duv;
    int plans = 1;
    q.make(container);

    while (!q.empty()) {
        u = q.get();
        if (getDistance(u) < std::numeric_limits<double>::infinity()) {
            // if the goal is reachable from u, it is also rechable from child nodes of u
            plans += numChildren(u) - 1;
        }
        for (i=0; i<numChildren(u); i++) {
            v = getChild(u,i);
            // cerr << v->name << '\n';
            duv = getDistance(u)+getWeight(u,i);
            if (getDistance(v) > duv) {
                setDistance(v,duv);
                setNext(v,u);
                q.decrease(getIndex(v));
            }
        }
    }
    std::clog << "Number of valid plans is " << plans << '\n';
}
