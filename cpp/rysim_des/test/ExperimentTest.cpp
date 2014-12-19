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

#include "Experiment.h"

#include <glog/logging.h>
#include <gtest/gtest.h>

#if !defined(__null)
#define __null (0)
#endif

using ::testing::Test;

class ExperimentTest : public Test {
    public:
        ExperimentTest() { }
        ~ExperimentTest() { }
};

TEST_F(ExperimentTest, LoopSEIRExperiment) {
    Experiment experiment;
    ASSERT_TRUE(experiment.parse("test/data/loop_seir_experiment.json"));

    EXPECT_EQ("LoopSEIRExperiment", experiment.getExperimentName());
    EXPECT_EQ("loop_seir_model.json", experiment.getModelFilename());
    EXPECT_EQ(1000, experiment.getEventLimit());
}

TEST_F(ExperimentTest, SimpleSEIRExperiment) {
    Experiment experiment;
    ASSERT_TRUE(experiment.parse("test/data/simple_seir_experiment.json"));

    EXPECT_EQ("SimpleSEIRExperiment", experiment.getExperimentName());
    EXPECT_EQ("simple_seir_model.json", experiment.getModelFilename());
    EXPECT_EQ(1000, experiment.getEventLimit());
}

TEST_F(ExperimentTest, TrivialSEIRExperiment) {
    Experiment experiment;
    ASSERT_TRUE(experiment.parse("test/data/trivial_seir_experiment.json"));

    EXPECT_EQ("TrivialSEIRExperiment", experiment.getExperimentName());
    EXPECT_EQ("trivial_seir_model.json", experiment.getModelFilename());
    EXPECT_EQ(0, experiment.getEventLimit());
}
