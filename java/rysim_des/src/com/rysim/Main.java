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
package com.rysim;

import joptsimple.OptionParser;
import joptsimple.OptionSet;

import java.io.IOException;
import java.io.PrintWriter;

public class Main {
    private final static String TAG = "Main";
    private static String mExperimentFile;
    private static long mRandomSeed;
    private static final SimulationController mSimulationController = new SimulationController();

    public static void main(String[] args) throws IOException {
        parseCommandLine(args);

        SimpleLogger.info(TAG, "Initializing SEIR DES Application");

        if (!mSimulationController.init(mExperimentFile, mRandomSeed)) {
            SimpleLogger.error(TAG, "Initialization in SEIR DES failed!");
            SimpleLogger.info(TAG, "Finished SEIR DES Application");
            SimpleLogger.info(TAG, "SEIR DES Application FAILED");
            assert(false);
        }

        SimpleLogger.info(TAG, "Running SEIR DES Application");
        if (!mSimulationController.run()) {
            SimpleLogger.error(TAG, "Run in SEIR DES failed!");
            SimpleLogger.info(TAG, "Finished SEIR DES Application");
            SimpleLogger.info(TAG, "SEIR DES Application FAILED");
            assert(false);
        }

        SimpleLogger.info(TAG, "Finished SEIR DES Application");
        SimpleLogger.info(TAG, "SEIR DES Application SUCCEEDED");
        mSimulationController.printResults();
    }

    private static void parseCommandLine(String[] args) throws IOException {
        OptionParser parser = new OptionParser();
        parser.accepts(
                "experiment_file",
                "JSON file that contains experiment specification. This must be specified and the file must be "
                        + "readable.")
                .withRequiredArg()
                .ofType(String.class);
        parser.accepts(
                "random_seed",
                "long that is used to seed the random number generation for the simulation run. Defaults to 1.")
                .withOptionalArg()
                .ofType(long.class);

        OptionSet optionSet = parser.parse(args);

        if (optionSet.valueOf("experiment_file") == null) {
            parser.printHelpOn(new PrintWriter(System.out));
            SimpleLogger.fatal(TAG, "No value provided for experiment_file!");
        }
        mExperimentFile = (String)optionSet.valueOf("experiment_file");

        if (optionSet.valueOf("random_seed") == null) {
            SimpleLogger.info(TAG, "Setting random_seed to default of 1");
            mRandomSeed = 1;
        } else {
            mRandomSeed = (long)optionSet.valueOf("random_seed");
            if (mRandomSeed < 0) {
                parser.printHelpOn(new PrintWriter(System.out));
                SimpleLogger.fatal(TAG, "Negative value provided for random_seed!");
            }
        }
    }
}
