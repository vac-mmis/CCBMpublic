declare -a arr=("ABC" "Ann" "CoreModel" "Day" "Duration" "Fluents" "GoalRec" "HMM" "Morning" "non-repeating" "R218" "Shoes" "ShoesTimed" "TestActionCount" "Validator" "XYRouter")

echo "Running examples:"

for directory in "${arr[@]}"

do

cd $directory
./run_test.sh new_out
cd ..

done

echo "Testing correctness of installation:"

for directory in "${arr[@]}"

do

cd $directory/old_out
files=$(find . -type f ! \( -name '.DS_Store' -or -name '*.err' -or -name '*.o' -or -name '*.rcconf' \))
cd ..

echo $directory

for file in $files
do
  if cmp --silent -- old_out/$file new_out/$file; then
    echo "SUCCESS: $file file contents are identical"
  else
    echo "ERROR: $file file contents differ"
  fi
done

cd ..

done
