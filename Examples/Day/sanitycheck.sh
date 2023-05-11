#!/bin/bash

#This file compares the output of the marginal filter for the obsservation file obs_3.dat with
#a prespecified file that contains the correct output. If this shell returns a success the sanity #check is successful.

if cmp --silent -- data/sanity.dat out/p-marginal.dat; then
  echo "SUCCESS: File contents are identical"
else
  echo "ERROR: File contents differ"
fi
