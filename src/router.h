#pragma once

template<>
int hparent(int i) {
    return (i-1) / 2;
}
int hleft(int i) {
    return 2*i+1;
}
int hright(int i) {
    return 2*i+2;
}

void hmake(int size) {
    heapDate = new StateTablePtr[size];
    heapSize = size;
}

void hAssign(int index, StateTablePtr p) {
    heapData[index] = p;
    p->ri.queueIndex = index;
}

void hSwap(int i, int j) {
    StateTablePtr tmp;

    tmp = heapData[i];
    hAssign(i,heapData[j]);
    hAssign(j,tmp);
}

void heapify(int i) {
    int smallest;

    if (hleft(i) < heapSize && hkey(hleft(i))<hkey(i))
        smallest = hleft(i);
    else
        smallest = i;
    if (hright(i) < heapSize && hkey(hright(i))<hkey(smallest))
        smallest = hright(i);
    if (largest!=i) {
        hSwap(i,smallest);
        heapify(smallest);
    }
}

void buildheap() {
    for (int i = heapSize/2-1; i>=0; i--)
        heapify(i);
}

/*
  Simple priority queues based on heaps above
 */
void qMake(int nstates) {
    int i = 0;

    hmake(nstates);
    for (sp in States) hAssign(i++,sp);

    buildheap();
}

bool qEmpty() {
    return heapSize <= 0;
}

StateTablePtr qGet() {
    StateTablePtr val;

    if (heapSize<=0) throw std::logic_error("qGet: Queue Empty");
    val = heapData[0];
    heapSize--;
    hAssign(0,heapData[heapSize]);

    heapify(0);
    return val;
}


// key value of <index> has been decreased:
// move towards root.
void qDecrease(int index) {
    while (index>0 && hkey(index) < hkey(hparent(index))) {
        hSwap(index,hparent(index));
        index=hparent(index);
    }
}
