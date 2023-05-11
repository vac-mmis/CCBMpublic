#include <iostream>
#include <unistd.h>
#include <string>
#include <fstream>
#include <vector>
#include <stack>
#include <unordered_map>
#include <cmath>

#include "fann_wrapper.h"

#include <boost/functional/hash.hpp>
#include <boost/optional.hpp>

#include "util/parameter.hpp"

#include "global.h"
#include "model.h"

using namespace std;

void getData(int num, char* files[], FANN::training_data &data, FANN::training_data &scaleData, bool ignoreDuplicates) {
    typedef std::unordered_map<vector<int>, vector<int>, boost::hash<std::vector<int>>> tstates_map; // state bits -> goaldistance(s)
    tstates_map states_map;
    int numElements = 0;

    // read data file by file now
    for (int arg=0; arg<num; arg++) {
//        std::string fn = *it;
        std::string fn = files[arg];
        std::cerr << "Reading file " << fn << '\n';

        ifstream file;
        stack<vector<int> > states;
        int maxindex=0;
        int index=0;

        file.open(fn.c_str(), std::ifstream::in);
        // ignore first line (header)
        file.ignore(100000,'\n');

        while (file.good()) {
            int opid, step, distance;
            vector<int> bits;

            file >> opid >> step >> distance;
            index++;

            if (maxindex < index) {
                maxindex=index;
            }

            for (int i=0; i<ELEMENTS_UNIVERSE; i++) {
                int a;
                file >> a;
                bits.push_back(a);
            }

            states.push(bits);
            numElements++;
            // ignore rest of line
            file.ignore(100000,'\n');
        }

        maxindex++;

        while (!states.empty()) {
            vector<int> bits;
            bits = states.top();
            states.pop();

            tstates_map::iterator it = states_map.find(bits);
            if (it == states_map.end()) { // not found

                vector<int> gds;
                gds.push_back(maxindex);
                states_map[bits] = gds;
            } else { // found
                if (ignoreDuplicates && maxindex < it->second[0]) { // we found a new minimum
                    it->second.clear();
                    it->second.push_back(maxindex);
                } else if (!ignoreDuplicates) {
                    it->second.push_back(maxindex);
                }
            }
            maxindex--;
            // put to hash map  bits -> value
        }
    }

    fann_type input[numElements+2][ELEMENTS_UNIVERSE];
    fann_type output[numElements+2][1];
    int row=0;
    // iterate hash map and create training data
    for (tstates_map::iterator it = states_map.begin(); it != states_map.end(); ++it) {
        vector<int> bits = it->first;
        for (vector<int>::iterator vit=it->second.begin(); vit!=it->second.end(); ++vit) {

            for (unsigned int i=0; i<bits.size(); i++) {
                input[row][i] = (fann_type)bits[i];
            }
            output[row++][0]=(fann_type)*vit;
        }
    }

    // for scaling reasons,
    for (unsigned int i=0; i< ELEMENTS_UNIVERSE; i++) {
        // add empty state
        input[row][i] = 0;
        output[row][0] = 0;
        // add full state
        input[row+1][i] = 1;
        output[row+1][0] = 0;
    }

    data.set_train_data(row, ELEMENTS_UNIVERSE, (fann_type*)input, 1, (fann_type*)output);
    scaleData.set_train_data(row+2, ELEMENTS_UNIVERSE, (fann_type*)input, 1, (fann_type*)output);
}

int main(int argc, char* argv[]) {
    std::ios::sync_with_stdio(false);

    int num_layers = 3; //recommendation from Satzger.Kramer:2013
    int num_output = 1; // goal distance
    int num_input = ELEMENTS_UNIVERSE; // number of atoms
    int num_hidden = num_input/2; // recommendation from Satzger.Kramer:2013
    int epochs = 1000;
    bool ignoreduplicates = true;
    const float desired_error = (const float) 0.0001;

    std::string o_nnfile;
    std::string o_nnlfile;
    std::string o_testfile;
    std::string o_trainfile;
    size_t parsed_params;

    FANN::neural_net network;

    options::option opts;

    try {
        opts.add_help("Trains a neural network on data files.\n usage: [OPTIONS] <statefile> {other statefile}");

        opts.add("epochs", "number of training epochs")
            .parameter("num", epochs);

        opts.add("f", "save trained net to <filename>")
            .parameter("filename", o_nnfile);

        opts.add("load-ann", "load trained neural network from file <filename>")
            .parameter("filename", o_nnlfile);

        opts.add("save-training", "save generated training data to <filename>")
            .parameter("filename", o_trainfile);

        opts.add("test", "uses <filename> to run test after training")
            .parameter("filename", o_testfile);

        opts.add("d", "", "don't remove duplicate states with different output",
        [&](std::string, std::vector<std::string>, size_t) -> size_t {
            ignoreduplicates = false;
            return 0;
        });

        options::add_help_param(opts);

        parsed_params = opts.parse(argc, argv, options::ERROR_IGNORE);

        if (argc - parsed_params < 1) {
            std::cerr << "No statesfiles specified!\n";
            std::cerr << "Use --help to show all available options\n";
            return 1;
        }

    } catch (std::exception &e) {
        std::cerr << e.what() << '\n';
        std::cerr << "Use --help to show all available options\n";
        return 1;
    }


    if (!network.create_standard(num_layers, num_input, num_hidden, num_output)) {
        std::cerr << "Cannot create neural network" << '\n';
        return -1;
    }

    if (!o_nnlfile.empty()) {
        if (!network.create_from_file(o_nnlfile)) {
            std::cerr << "Unable to load neural network from file " << o_nnlfile << '\n';
            exit(-1);
        }
    } else {
        network.set_activation_function_output(FANN::LINEAR);
        network.set_activation_function_hidden(FANN::SIGMOID_SYMMETRIC);
        network.set_train_stop_function(FANN::STOPFUNC_MSE);
        network.set_training_algorithm(FANN::TRAIN_RPROP);

        FANN::training_data data, scaleData;

        getData(argc-parsed_params, &argv[parsed_params],data,scaleData,ignoreduplicates);
        network.set_scaling_params(scaleData, 0, 1, 0, 1);

        network.scale_train(data);

        if (!o_trainfile.empty()) { // save generated training data, just for debugging purposes
            std::cerr << "Saving Training data to file " << o_trainfile << '\n';
            data.save_train(o_trainfile);
        }

        std::cerr << "Start training...\n";
        network.train_on_data(data, epochs, 0, desired_error);
        std::cerr << "... finished training.\n";
    }


    if (!o_nnfile.empty()) {
        if (!network.save(o_nnfile)) {
            std::cerr << "Unable to save neural network to file " << o_nnfile << '\n';
        }
    }

    if (!o_testfile.empty()) { // test with this data
        fann_type *calc_out;
        FANN::training_data test_data, scaleData;
        char* arr[] = {(char*) o_testfile.c_str()};
        double mse = 0;
        double round_mse = 0;
        double floor_mse = 0;
        double ceil_mse = 0;

        getData(1, arr,test_data, scaleData, ignoreduplicates);

        // report result
        for (unsigned int i=0; i<test_data.length_train_data(); i++) {
            network.scale_input(test_data.get_input()[i]);
            calc_out = network.run(test_data.get_input()[i]);
            network.descale_output(calc_out);
            mse += (calc_out[0] - test_data.get_output()[i][0]) * (calc_out[0] - test_data.get_output()[i][0]);
            round_mse += (round(calc_out[0]) - test_data.get_output()[i][0]) * (round(calc_out[0]) - test_data.get_output()[i][0]);
            floor_mse += (floor(calc_out[0]) - test_data.get_output()[i][0]) * (floor(calc_out[0]) - test_data.get_output()[i][0]);
            ceil_mse += (ceil(calc_out[0]) - test_data.get_output()[i][0]) * (ceil(calc_out[0]) - test_data.get_output()[i][0]);
            std::cout << calc_out[0] << " vs. " << test_data.get_output()[i][0] << '\n';
        }

        mse /= test_data.length_train_data();
        floor_mse /= test_data.length_train_data();
        round_mse /= test_data.length_train_data();
        ceil_mse /= test_data.length_train_data();
        double rmse = sqrt(mse);
        double round_rmse = sqrt(round_mse);
        double floor_rmse = sqrt(floor_mse);
        double ceil_rmse = sqrt(ceil_mse);

        std::cout << "Real:  MSE: " << mse << ", RMSE: " << rmse << '\n';
        std::cout << "Round: MSE: " << round_mse << ", RMSE: " << round_rmse << '\n';
        std::cout << "Floor: MSE: " << floor_mse << ", RMSE: " << floor_rmse << '\n';
        std::cout << "Ceil:  MSE: " << ceil_mse << ", RMSE: " << ceil_rmse << '\n';
    }
}
