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
#include "Agent.h"

#include <glog/logging.h>

#include "LoggingUtils.h"
#include "NumberGenerator.h"

Agent::Agent() {
	nextState_ = STATE_UNKNOWN;
}

Agent::Agent(
        		const std::string& label, State currentState,
        		const std::vector<std::string>& connections, const std::string& s2e,
        		const std::string& e2i, const std::string& i2r, const std::string& r2s) {
	label_ = label;
	currentState_ = currentState;
	nextState_ = STATE_UNKNOWN;
    connections_ = connections;
    s2e_ = s2e;
    e2i_ = e2i;
    i2r_ = i2r;
    r2s_ = r2s;
    transitionState(0);
}

Agent::~Agent() {
}

const std::string& Agent::getLabel() const {
    return label_;
}

State Agent::getCurrentState() const {
    return currentState_;
}

State Agent::getNextState() const {
    return nextState_;
}

std::vector<std::string> Agent::getConnections() const {
    return connections_;
}

const std::string& Agent::getS2E() const {
    return s2e_;
}

const std::string& Agent:: getE2I() const {
    return e2i_;
}

const std::string& Agent::getI2R() const {
    return i2r_;
}

const std::string& Agent::getR2S() const {
    return r2s_;
}

void Agent::processEvent(const Event& event) {
    State newState = event.getBody();
    if (shouldTransition(newState))
        nextState_ = newState;
}

void Agent::advanceState(const int64_t timestamp) {
    if (nextState_ != STATE_UNKNOWN) {
        currentState_ = nextState_;
        transitionState(timestamp);
    }
    nextState_ = STATE_UNKNOWN;
}

bool Agent::shouldTransition(State newState) {
    bool ret_val = false;
    switch (currentState_) {
        case STATE_SUSCEPTIBLE:
            if (newState == STATE_EXPOSED) {
                ret_val = true;
            } else {
                LOG(FATAL) << "Unexpected new state, " << LoggingUtils::toString(newState) << ", while in SUSCEPTIBLE!";
            }
            break;
        case STATE_EXPOSED:
            if (newState == STATE_INFECTIOUS) {
                ret_val = true;
            } else if (newState == STATE_EXPOSED) {
                ret_val = false;
            } else {
                LOG(FATAL) << "Unexpected new state, " << LoggingUtils::toString(newState) << ", while in EXPOSED!";
            }
            break;
        case STATE_INFECTIOUS:
            if (newState == STATE_RECOVERED) {
                ret_val = true;
            } else if (newState == STATE_EXPOSED) {
                ret_val = false;
            } else {
                LOG(FATAL) << "Unexpected new state, " << LoggingUtils::toString(newState) << ", while in INFECTIOUS!";
            }
            break;
        case STATE_RECOVERED:
            if (newState == STATE_SUSCEPTIBLE
                    && r2s_ != "undefined") {
                ret_val = true;
            } else if (newState == STATE_EXPOSED) {
                ret_val = false;
            } else {
                LOG(FATAL) << "Unexpected new state, " << LoggingUtils::toString(newState) << ", while in RECOVERED!";
            }
            break;
        default:
            LOG(FATAL) << "Unknown current state!";
            break;
    }
    return ret_val;
}

void Agent::transitionState(int64_t timestamp) {
    int64_t delta;
    switch (currentState_) {
        case STATE_SUSCEPTIBLE:
            break;
        case STATE_EXPOSED:
            delta = callDistribution(e2i_);
            sendEvent(timestamp + delta, label_, STATE_INFECTIOUS);
            break;
        case STATE_INFECTIOUS:
            delta = callDistribution(i2r_);
            sendEvent(timestamp + delta, label_, STATE_RECOVERED);
            infectNeighbours(timestamp, delta);
            break;
        case STATE_RECOVERED:
            if (r2s_ != "undefined") {
                delta = callDistribution(r2s_);
                sendEvent(timestamp + delta, label_, STATE_SUSCEPTIBLE);
            }
            break;
        default:
            LOG(FATAL) << label_ << ": Unknown current state!";
            break;
    }
}

void Agent::sendEvent(int64_t timestamp, const std::string& label, State state) {
    Event event;
    event.setTimestamp(timestamp);
    event.setTarget(label);
    event.setBody(state);
    EventQueue::get()->push(event);
}

void Agent::infectNeighbours(int64_t timestamp, int64_t delta) {
    for (int idx = 0; idx < connections_.size(); idx++) {
        int64_t infection_delta = callDistribution(s2e_);
        if (infection_delta <= delta) {
        	sendEvent(timestamp + infection_delta, connections_[idx], STATE_EXPOSED);
        }
    }
}

uint64_t Agent::callDistribution(std::string label) const {
	return 1 + NumberGenerator::get()->callDistribution(label);
}
