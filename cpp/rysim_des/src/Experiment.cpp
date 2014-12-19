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

#include <boost/lexical_cast.hpp>
#include <fstream>
#include <glog/logging.h>

#include "ParserUtils.h"

Experiment::Experiment() {
    modelFilename_ = "";
    experimentName_ = "";
    eventLimit_ = 0;
    type_= "";
}

Experiment::~Experiment() {
}

std::string Experiment::getModelFilename() const {
    return modelFilename_;
}

std::string Experiment::getExperimentName() const {
    return experimentName_;
}

uint64_t Experiment::getEventLimit() const {
    return eventLimit_;
}

std::string Experiment::getExperimentType() const {
	return type_;
}


bool Experiment::parse(const std::string& filename) {
    Json::Value root;
    Json::Reader reader;
    std::ifstream input(filename.c_str());

    if (!reader.parse(input, root)) {
    	LOG(ERROR) << "Failed to parse experiment JSON file, " << filename;
    	return false;
    }

    if (!ParserUtils::parseAsString(root, "model_filename", &modelFilename_)) return false;
    if (!ParserUtils::parseAsString(root, "experiment_name", &experimentName_)) return false;
    if (!ParserUtils::parseAsUInt64(root, "event_limit", &eventLimit_)) return false;
    if (!ParserUtils::parseAsString(root, "type", &type_)) return false;

    return true;
}
