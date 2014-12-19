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

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>
#include <fstream>
#include <glog/logging.h>

#include "LoggingUtils.h"
#include "ParserUtils.h"

Model::Model() {
}

Model::~Model() {
}

bool Model::parse(const std::string& filename) {
    Json::Value root;
    Json::Reader reader;
    std::ifstream input(filename.c_str());

    if (!reader.parse(input, root)) {
    	LOG(ERROR) << "Failed to parse model JSON file, " << filename;
    	return false;
    }

    if (!ParserUtils::parseAsString(root, "model_name", &modelName_)) return false;
    if (!ParserUtils::parseAsUInt64(root, "total_connections", &totalConnections_)) return false;
    if (!parseAsDistributions(root, "distributions")) return false;
    if (!parseAsAgents(root, "agents", &agents_)) return false;
    if (!validateConnections(agents_)) return false;


    return true;
}

uint64_t Model::getModelSize() const {
    return agents_.size();
}

uint64_t Model::getTotalConnections() const {
	return totalConnections_;
}

std::string Model::getModelName() const {
	return modelName_;
}

void Model::processEvent(const Event& event) {
    std::string target = event.getTarget();
    if (!isRegistered(agents_, target)) {
        LOG(FATAL) << "Attempted to process event [" << LoggingUtils::toString(event)
                   << "] for non-registered Agent, " << target;
    }
    dirtyAgents_.insert(target);
    agents_[target].processEvent(event);
}

void Model::advanceState(int64_t timestamp) {
    for (std::set<std::string>::iterator iter = dirtyAgents_.begin();
            iter != dirtyAgents_.end();
            ++iter) {
        agents_[*iter].advanceState(timestamp);
    }
    dirtyAgents_.clear();
}

bool Model::isRegistered(const Agents& agents, const std::string& label) {
    return agents.find(label) != agents.end();
}

bool Model::validateConnections(const Agents& agents) {
    for(Agents::const_iterator agentIter = agents.begin();
            agentIter != agents.end(); ++agentIter) {
        std::vector<std::string> connections = agentIter->second.getConnections();
        for(int connectionsIdx = 0; connectionsIdx < connections.size(); connectionsIdx++) {
            if (!isRegistered(agents, connections[connectionsIdx])) {
                LOG(ERROR) << "Agent connection list contained unregistered agent, "
                        << connections[connectionsIdx];
                return false;
            }
        }
    }
    return true;
}

 bool Model::parseAsDistributions(const Json::Value& root, const std::string& entryName) {
	 Json::Value array;
	 if (!ParserUtils::parseAsArray(root, entryName, &array)) return false;

	 for (Json::Value::iterator iter = array.begin(); iter != array.end(); iter++) {
		 if (!parseAsDistribution(*iter)) return false;
	 }
	 return true;
 }

bool Model::parseAsDistribution(const Json::Value& root) {
	std::string label;
	NumberGenerator::DistributionType type;
	float scale;
	std::vector<float> params;

	if (!ParserUtils::parseAsString(root, "label", &label)) return false;
	if (!parseAsDistributionType(root, "type", &type)) return false;
	if (!ParserUtils::parseAsFloat(root, "scale", &scale) || scale < 0.0f) {
		scale = 1.0f;
	}
	if (!ParserUtils::parseAsVectorOfFloats(root, "params", &params)) return false;

	NumberGenerator::Signature signature(type, scale, params);
     if (!NumberGenerator::get()->registerDistribution(label, signature)) {
         LOG(ERROR) << "Unable to register distributions, " << label;
         return false;
     }
     return true;
}

bool Model::parseAsDistributionType(
        		const Json::Value& root, const std::string& entryName,
        		NumberGenerator::DistributionType* value) {
     std::string type;
     if (!ParserUtils::parseAsString(root, "type", &type)) {
    	 LOG(ERROR) << "Unable to find type in distribution!";
    	 return false;
     }

     boost::algorithm::to_lower(type);

     if("gaussiantail" == type) {
    	 *value = NumberGenerator::DISTRIBUTION_TYPE_GAUSSIANTAIL;
     } else if ("exponential" == type) {
    	 *value = NumberGenerator::DISTRIBUTION_TYPE_EXPONENTIAL;
     } else if ("flat" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_FLAT;
     } else if ("lognormal" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_LOGNORMAL;
     } else if ("poisson" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_POISSON;
     } else if ("bernoulli" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_BERNOULLI;
     } else if ("binomial" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_BINOMIAL;
     } else if ("negativebinomial" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_NEGATIVEBINOMIAL;
     } else if ("geometric" == type) {
         *value = NumberGenerator::DISTRIBUTION_TYPE_GEOMETRIC;
     } else {
        LOG(ERROR) << "Parsed unknown distribution type, " << type;
        return false;
     }
     return true;
}

bool Model::parseAsAgents(
		const Json::Value& root, const std::string& entryName,
		Agents* value) {
	 Json::Value array;
	 if (!ParserUtils::parseAsArray(root, entryName, &array)) return false;

	 for (Json::Value::iterator iter = array.begin(); iter != array.end(); iter++) {
		 if (!parseAsAgent(*iter, value)) return false;
	 }
     return true;
}

bool Model::parseAsAgent(const Json::Value& root, Agents* agents) {
	std::string label;
	if (!ParserUtils::parseAsString(root, "label", & label)) return false;
	if (isRegistered(*agents, label)) {
		LOG(ERROR) << "Parsing agent with label already parsed, " << label;
		return false;
	}

	State state;
	std::vector<std::string> connections;
	std::string s2e, e2i, i2r, r2s;
	if (!parseAsAgentState(root, "state", &state)) return false;
	if (!ParserUtils::parseAsVectorOfStrings(root, "connections", &connections)) return false;
	if (!parseAsAgentTransition(root, "s2e", &s2e)) return false;
	if (!parseAsAgentTransition(root, "e2i", &e2i)) return false;
	if (!parseAsAgentTransition(root, "i2r", &i2r)) return false;
	if (!parseAsAgentTransition(root, "r2s", &r2s)) {
		r2s = "undefined";
	}

	Agent agent(label, state, connections, s2e, e2i, i2r, r2s);
    (*agents)[label] = agent;
	return true;
}

bool Model::parseAsAgentState(
		const Json::Value& root, const std::string& entryName, State* value) {
    std::string state;
    if (!ParserUtils::parseAsString(root, "state", &state)) {
   	 LOG(ERROR) << "Unable to find state in distribution!";
   	 return false;
    }


    boost::algorithm::to_lower(state);

    if ("susceptible" == state) {
    	*value = STATE_SUSCEPTIBLE;
    } else if ("exposed" == state) {
    	*value = STATE_EXPOSED;
    } else if ("infectious" == state) {
    	*value = STATE_INFECTIOUS;
    } else if ("recovered" == state) {
    	*value = STATE_RECOVERED;
    } else {
       LOG(ERROR) << "Parsed unknown agent state, " << state;
       return false;
    }
    return true;
}

bool Model::parseAsAgentTransition(
		const Json::Value& root, const std::string& entryName, std::string* value) {
	if (!ParserUtils::parseAsString(root, entryName, value)) return false;
	if (!NumberGenerator::get()->isRegistered(*value)) {
		LOG(ERROR) << "Parsed unregistered value for " << entryName;
		return false;
	}
	return true;
}
