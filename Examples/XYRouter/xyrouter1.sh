#!/bin/sh

DOMAINF=Models/xyrouterd.pddl
PROBLEMF=Models/xyrouterp.pddl
OBSERVEF=Models/xyrouterd-observations
DATAF=Data/r218-ur-xy.dat

TOOLPFX=`basename $PROBLEMF .pddl`

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <outdir>"
    exit 1
fi

OUT="$1"
mkdir "$OUT"

../../rc -d $DOMAINF -p $PROBLEMF -o $OBSERVEF -w "$OUT" -f "$OUT/$TOOLPFX" -MMFHA || exit 1

MARGINAL="$OUT/${TOOLPFX}-marginal"
FILTER="$OUT/${TOOLPFX}-filter"
HMM="$OUT/${TOOLPFX}-filter"
ANALYZER="$OUT/${TOOLPFX}-analyzer"

STATES="$OUT"/xyrouter.states

./${ANALYZER} -o "$STATES"
#./${FILTER} <${DATAF} -s xyrouter-uninformed.smooth -t1 >xyrouter-uninformed.trace
./${FILTER} -l "$STATES" <${DATAF} -s "$OUT"/xyrouter-informed-filter.smooth -t1 >"$OUT"/xyrouter-informed-filter.trace
./${MARGINAL} -l "$STATES" <${DATAF} >"$OUT"/xyrouter-informed-marginal.trace
./${HMM} -l "$STATES" <${DATAF} -s "$OUT"/xyrouter-informed-hmm.smooth -t1 >"$OUT"/xyrouter-informed-hmm.trace

R --slave <<EOF
for (filter in c("filter", "marginal", "hmm")) {
    d <- read.table(paste0("$OUT/xyrouter-informed-", filter, ".trace"))
    estimates <- d[,2:3]
    cells     <- as.matrix(d[,-c(1,2,3)])

    l <- read.table("${DATAF}")

    c <- c(0.7,2.1,3.5,4.9,6.3)

    baz <- matrix(nrow=1,ncol=1,data=1)

    doit <- function() {
            for(i in 1:nrow(cells)) {
                    m <- matrix(nrow=5,ncol=5,data=as.vector(cells[i,]),byrow=TRUE)
                    image(baz,col="lightgray",ylim=c(0,7),xlim=c(0,7),main=i)
                    image(c,c,log(m),col=heat.colors(256),add=TRUE)
                    lines(l[1:i,],type="b",col="red")
                    lines(estimates[1:i,],type="b",col="blue")
                    #Sys.sleep(0.1)
                    #readline("Hit return to continue")
            }
    }
    pdf(file=paste0("$OUT/xyrouter-", filter, ".pdf"))
    doit()
    dev.off()
}
EOF
