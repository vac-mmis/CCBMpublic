#!/bin/sh

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <output directory> [RC options ...]"
    exit 1
fi

dir="$1"
shift

# Find true location of script
scriptpath=$(readlink $0)
if [ -z $scriptpath ]; then scriptpath=$0; fi
MYHOME=$(cd $(dirname $scriptpath); pwd)

RC=${MYHOME}/../../rc

mkdir -p "$dir/build"

# ${RC} -d xyrouterd.pddl -p xyrouterp55.pddl -O -v2 -MV -C"-DUSE_ACTION_OBSERVATION"
# ${RC} -d xyrouterd.pddl -p xyrouterp51.pddl -O -v2 -MMAF -C"-DUSE_ACTION_OBSERVATION"

${RC} -d xyrouterd.pddl -p xyrouterp51_55.pddl -w "$dir/build/" -f "$dir/" -O -v2 -MMFSTOVA -C"-DUSE_ACTION_OBSERVATION -DUSE_GOAL_ESTIMATION" "$@"

#
#
# ${MYHOME}/xyrouter55/xyrouterp55-analyzer -o xy55.states
# ${MYHOME}/xyrouter51/xyrouterp51-analyzer -o xy51.states
# #
# ${MYHOME}/xyrouter51_55/xyrouterp51_55-analyzer -g g51 -o xy51_55-g51.states
# ${MYHOME}/xyrouter51_55/xyrouterp51_55-analyzer -g g55 -o xy51_55-g55.states
#
# ${MYHOME}/xyrouter51_55/xyrouterp51_55-simulator -i i21 -g g55 -l xy51_55-g55.states > xy55.plan
# ${MYHOME}/xyrouter51_55/xyrouterp51_55-marginal -l g55:xy51_55-g55.states -l g51:xy51_55-g51.states < xy55.plan > goal.est.mf.txt
# #${MYHOME}/xyrouter51_55/xyrouterp51_55-filter -l g55:xy51_55-g55.states -l g51:xy51_55-g51.states < xy55.plan > goal.est.pf.txt
#
# #
#  ${MYHOME}/xyrouter55/xyrouterp55-marginal -l xy55.states < xy55.plan 2>&1 >/dev/null | grep LL  | sed -e 's/^.*LL=//' |sed -e 's/\([^,]*\).*$/\1/' > xy55.mf.log
#  ${MYHOME}/xyrouter51/xyrouterp51-marginal -l xy51.states < xy55.plan 2>&1 >/dev/null | grep LL  | sed -e 's/^.*LL=//' |sed -e 's/\([^,]*\).*$/\1/' > xy51.mf.log
# # ${MYHOME}/xyrouter55/xyrouterp55-filter -l xy55.states < xy55.plan # 2>&1 >/dev/null | grep LL  | sed -e 's/^.*LL=//' |sed -e 's/\([^,]*\).*$/\1/' > xy55.pf.log
# # ${MYHOME}/xyrouter51/xyrouterp51-filter -l xy51.states < xy55.plan 2>&1 >/dev/null | grep LL  | sed -e 's/^.*LL=//' |sed -e 's/\([^,]*\).*$/\1/' > xy51.pf.log
# #
# #
# cat >tmp.R <<EOF
# require(ggplot2)
# require(reshape2)
# r1 <- read.table("xy55.mf.log")
# r2 <- read.table("xy51.mf.log")
# ll2 <- read.table("goal.est.mf.txt")
# ll <- data.frame(ll1=r1,ll2=r2)
# ll <- apply(ll,1,function(row){
#   row - max(row)
# })
# ll <- t(apply(ll,2, function(col){exp(col)/sum(exp(col))}))
# ll <- data.frame(time=1:nrow(ll),ll)
# ll\$source <- "ll"
# ll2\$source <- "est"
# names(ll) <- c("Time", "G1", "G2", "Source")
# names(ll2) <- c("Time", "G1", "G2", "Source")
# ll <-rbind(ll,ll2)
# llm<-melt(ll, id.vars=c("Time","Source"))
# p<- ggplot(llm, aes(Time, value)) + geom_line(aes(color=variable)) + labs(title="Goalrec") + facet_wrap(~Source)
# ggsave("result.png",p)
#
# EOF
#
#
# R --slave -f tmp.R
# open result.png
