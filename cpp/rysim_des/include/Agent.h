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
#ifndef AGENT_H_
#define AGENT_H_

#include <string>
#include <vector>

#include "EventQueue.h"
#include "State.h"

class Agent {
    public:
		Agent();
        Agent(
        		const std::string& label, State currentState,
        		const std::vector<std::string>& connections, const std::string& s2e,
        		const std::string& e2i, const std::string& i2r, const std::string& r2s);
        virtual ~Agent();

        const std::string& getLabel() const;
        State getCurrentState() const;
        State getNextState() const;
        std::vector<std::string> getConnections() const;
        const std::string& getS2E() const;
        const std::string& getE2I() const;
        const std::string& getI2R() const;
        const std::string& getR2S() const;

        void processEvent(const Event& event);
        void advanceState(const int64_t timestamp);

    private:
        std::string label_;
        State currentState_;
        State nextState_;
        std::vector<std::string> connections_;
        std::string s2e_;
        std::string e2i_;
        std::string i2r_;
        std::string r2s_;

        bool shouldTransition(State newState);
        void transitionState(int64_t timestamp);
        void sendEvent(int64_t timestamp, const std::string& label, State state);
        void infectNeighbours(int64_t timestamp, int64_t delta);
        uint64_t callDistribution(std::string label) const;
};

#endif /* AGENT_H_ */
