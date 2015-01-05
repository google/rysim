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
//#include "SimulationController.h"

#include <boost/filesystem.hpp>
#include <boost/lexical_cast.hpp>
#include <glog/logging.h>
#include <iostream>
//#include <json/json.h>

//#include "Experiment.h"
//#include "NumberGenerator.h"
#include "../include/SimulationController.h"
#include "../include/Experiment.h"
#include "../../../third_party/cpp/jsoncpp/include/json/json.h"

SimulationController::SimulationController() {
    timestamp_ = 0;
    eventCount_ = 0;
    eventLimit_ = 0;
}

SimulationController::~SimulationController() {

}

bool SimulationController::init(const std::string& filename, const long int& seed) {
    NumberGenerator::get()->init(seed);

    Experiment experiment;
    if (!experiment.parse(filename)) {
        LOG(ERROR) << "Unable to parse supplied experiment file = " << filename;
        return false;
    }
    experimentName_ = experiment.getExperimentName();
    experimentType_ = experiment.getExperimentType();
    eventLimit_ = experiment.getEventLimit();
    experimentType_ = experiment.getExperimentType();

    std::string modelFilename = experiment.getModelFilename();
    boost::filesystem::path experimentPath(filename);
    boost::filesystem::path modelPath(modelFilename);

    std::string trueFilename = experimentPath.parent_path().string();
    if (trueFilename == "") {
    	trueFilename += "./";
    } else {
    	trueFilename += "/";
    }
    trueFilename += modelPath.string();

    if (!model_.parse(trueFilename)) {
        LOG(ERROR) << "Unable to parse supplied model file = " << modelFilename << " @ " << trueFilename;
        return false;
    }

    return true;
}

bool SimulationController::run() {
	timestamp_ = 0;
    while (eventCount_ < eventLimit_ && !EventQueue::get()->isEmpty()) {
        Event event = EventQueue::get()->pop();
        model_.processEvent(event);
        eventCount_++;
        timestamp_ = event.getTimestamp();
        while (EventQueue::get()->isNextEventAt(timestamp_)) {
            event = EventQueue::get()->pop();
            model_.processEvent(event);
            eventCount_++;
        }
        model_.advanceState(timestamp_);
    }
    return true;
}

void SimulationController::printResults() {
	Json::Value results;
	results["experiment"] = experimentName_;
	results["model"] = model_.getModelName();
	results["kernel"] = "C++DES";
	results["type"] = experimentType_;
	results["event_count"] = static_cast<Json::UInt64>(eventCount_);
	results["final_time"] = static_cast<Json::UInt64>(timestamp_);
	results["agents"] = static_cast<Json::UInt64>(model_.getModelSize());
	results["connections"] = static_cast<Json::UInt64>(model_.getTotalConnections());

    std::cout << "JSONBEGIN";
    std::cout << results;
    std::cout << "JSONEND" << std::endl;
}

uint64_t SimulationController::getEventCount() const {
    return eventCount_;
}

uint64_t SimulationController::getEventLimit() const {
    return eventLimit_;
}

uint64_t SimulationController::getAgentCount() const {
    return model_.getModelSize();
}

int64_t SimulationController::getTimestamp() const {
    return timestamp_;
}
