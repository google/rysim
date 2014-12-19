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
#ifndef MODEL_H_
#define MODEL_H_

#include <json/json.h>
#include <map>
#include <set>
#include <stdint.h>
#include <string>
#include <vector>

#include "Agent.h"
#include "EventQueue.h"
#include "NumberGenerator.h"
#include "State.h"

class Model {
    public:
        Model();
        virtual ~Model();

        bool parse(const std::string& filename);
        uint64_t getModelSize() const;
        uint64_t getTotalConnections() const;
        std::string getModelName() const;

        void processEvent(const Event& event);
        void advanceState(const int64_t timestamp);

    private:
        typedef std::map<std::string, Agent> Agents;

        Agents agents_;
        std::set<std::string> dirtyAgents_;
        std::string modelName_;
        uint64_t totalConnections_;

        void seedState();

        static bool isRegistered(const Agents& agents, const std::string& label);
        static bool validateConnections(const Agents& agents);

        static bool parseAsDistributions(const Json::Value& root, const std::string& entryName);
        static bool parseAsDistribution(const Json::Value& root);
        static bool parseAsDistributionType(
        		const Json::Value& root, const std::string& entryName,
        		NumberGenerator::DistributionType* value);

        static bool parseAsAgents(
        		const Json::Value& root, const std::string& entryName, Agents* value);
        static bool parseAsAgent(const Json::Value& root, Agents* agents);
        static bool parseAsAgentState(
        		const Json::Value& root, const std::string& entryName, State* value);
        static bool parseAsAgentTransition(
        		const Json::Value& root, const std::string& entryName, std::string* value);
};

#endif /* MODEL_H_ */
