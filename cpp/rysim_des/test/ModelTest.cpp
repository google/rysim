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

#include "Model.h"

#include <glog/logging.h>
#include <gtest/gtest.h>

#include "NumberGenerator.h"

#if !defined(__null)
#define __null (0)
#endif

using ::testing::Test;

class ModelTest : public Test {
    public:
        ModelTest() { }
        ~ModelTest() { }

        virtual void SetUp() {
        	NumberGenerator::get()->clear();
        	NumberGenerator::get()->init(1);
        }
};

TEST_F(ModelTest, LoopSEIRModel) {
    Model model;
    ASSERT_TRUE(model.parse("test/data/loop_seir_model.json"));

    EXPECT_EQ(4, model.getModelSize());
}

TEST_F(ModelTest, SimpleSEIRModel) {
    Model model;
    ASSERT_TRUE(model.parse("test/data/simple_seir_model.json"));

    EXPECT_EQ(3, model.getModelSize());
}

TEST_F(ModelTest, TrivialSEIRModel) {
    Model model;
    ASSERT_TRUE(model.parse("test/data/trivial_seir_model.json"));

    EXPECT_EQ(1, model.getModelSize());
}
