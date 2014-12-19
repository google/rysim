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

#include "SimulationController.h"

#include <boost/filesystem.hpp>
#include <glog/logging.h>
#include <gtest/gtest.h>

#include "EventQueue.h"
#include "NumberGenerator.h"

#if !defined(__null)
#define __null (0)
#endif

using ::testing::Test;

class SimulationControllerTest : public Test {
    public:
        SimulationControllerTest() { }
        ~SimulationControllerTest() { }

        virtual void SetUp() {
        	EventQueue::get()->clear();
        	NumberGenerator::get()->clear();
            origDir_ = boost::filesystem::current_path();
            boost::filesystem::path workDir = origDir_;
            workDir /= "test";
            workDir /= "data";
            boost::filesystem::current_path(workDir);
        }

        virtual void TearDown() {
            boost::filesystem::current_path(origDir_);
        }

    private:
        boost::filesystem::path origDir_;
};

TEST_F(SimulationControllerTest, LoopSEIR) {
    SimulationController controller;
    controller.init("loop_seir_experiment.json", 1);
    controller.run();

    EXPECT_EQ(static_cast<uint64_t>(4), controller.getAgentCount());
    EXPECT_EQ(static_cast<uint64_t>(1000), controller.getEventLimit());
    EXPECT_EQ(static_cast<uint64_t>(1002), controller.getEventCount());
    EXPECT_EQ(static_cast<uint64_t>(835), controller.getTimestamp());
}


 TEST_F(SimulationControllerTest, SimpleSEIR) {
    SimulationController controller;
    controller.init("simple_seir_experiment.json", 1);
    controller.run();

    EXPECT_EQ(static_cast<uint64_t>(3), controller.getAgentCount());
    EXPECT_EQ(static_cast<uint64_t>(1000), controller.getEventLimit());
    EXPECT_EQ(static_cast<uint64_t>(7), controller.getEventCount());
    EXPECT_EQ(static_cast<uint64_t>(25), controller.getTimestamp());
}

TEST_F(SimulationControllerTest, TrivialSEIR) {
    SimulationController controller;
    controller.init("trivial_seir_experiment.json", 1);
    controller.run();

    EXPECT_EQ(static_cast<uint64_t>(1), controller.getAgentCount());
    EXPECT_EQ(static_cast<uint64_t>(0), controller.getEventLimit());
    EXPECT_EQ(static_cast<uint64_t>(0), controller.getEventCount());
    EXPECT_EQ(static_cast<uint64_t>(0), controller.getTimestamp());
}

