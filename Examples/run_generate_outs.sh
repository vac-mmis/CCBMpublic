declare -a arr=("ABC" "Ann" "CoreModel" "Day" "Duration" "Fluents" "GoalRec" "HMM" "Morning" "non-repeating" "R218" "Shoes" "ShoesTimed" "TestActionCount" "Validator" "XYRouter")

echo "Running examples:"

for directory in "${arr[@]}"

do

cd $directory
./run_test.sh old_out
cd ..

done

echo "Outputs generated"
