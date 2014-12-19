/*
 * Copyright 2014 The RySim Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include <boost/filesystem.hpp>
#include <boost/scoped_ptr.hpp>
#include <gflags/gflags.h>
#include <glog/logging.h>
#include <string>

#include "SimulationController.h"

static bool ValidateFileName(const char* flagname, const std::string& filename) {
    if (!boost::filesystem::exists(filename)) {
        LOG(ERROR) << filename << ", supplied for " << flagname << " does not exist!";
        return false;
    }

    if (!boost::filesystem::is_regular_file(filename)) {
        LOG(ERROR) << filename << ", supplied for " << flagname << " is not a regular file!";
        return false;
    }
    return true;
}

DEFINE_string(experiment_file, "",
        "JSON file that contains experiment specification. This must be "
        "specified and the file must be readable.");
DEFINE_int64(random_seed, 1,
        "Integer that is used to seed the random number generation for "
        "the simulation run. Defaults to 1.");

int main(int argc, char **argv) {
    // Initialize Google's logging library.
    google::InitGoogleLogging(argv[0]);

    // Initialize Google's flag library.
    google::RegisterFlagValidator(&FLAGS_experiment_file, &ValidateFileName);
    google::ParseCommandLineFlags(&argc, &argv, true);

    LOG(INFO) << "Initializing SEIR DES Application";
    SimulationController controller;
    if (!controller.init(FLAGS_experiment_file, FLAGS_random_seed)) {
        LOG(ERROR) << "Initialization in SEIR DES failed!";
        LOG(INFO) << "Finished SEIR DES Application";
        LOG(INFO) << "SEIR DES Application FAILED";
        return 1;
    }

    LOG(INFO) << "Running SEIR DES Application";
    if (!controller.run()) {
        LOG(ERROR) << "Run in SEIR DES failed!";
        LOG(INFO) << "Finished SEIR DES Application";
        LOG(INFO) << "SEIR DES Application FAILED";
        return 2;
    }

    LOG(INFO) << "Finished SEIR DES Application";
    LOG(INFO) << "SEIR DES Application SUCCEEDED";
    controller.printResults();
    return 0;
}
