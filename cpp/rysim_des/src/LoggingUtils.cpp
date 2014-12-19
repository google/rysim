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
#include "LoggingUtils.h"

#include <glog/logging.h>
#include <sstream>

std::string LoggingUtils::toString(const State& state) {
	switch (state) {
	case STATE_SUSCEPTIBLE:
		return "SUSCEPTIBLE";
		break;
	case STATE_EXPOSED:
		return "EXPOSED";
		break;
	case STATE_INFECTIOUS:
		return "INFECTIOUS";
		break;
	case STATE_RECOVERED:
		return "RECOVERED";
		break;
	case STATE_UNKNOWN:
		return "UNKNOWN";
		break;
	case NUM_STATE:
		return "NUM_STATE";
		break;
	default:
		LOG(FATAL) << "Attempted to converted malformed State to string";
	}
	// Should not be reached
	return "";
}

std::string LoggingUtils::toString(const Event& event) {
	std::stringstream stream;
	stream << "timestamp = " << event.getTimestamp() << ", target = " << event.getTarget()
		   << ", body = " << toString(event.getBody());
	return stream.str();
}

std::string LoggingUtils::toString(const Agent& agent) {
	std::vector<std::string> connections = agent.getConnections();
	std::string connectionsString;
	for (std::vector<std::string>::iterator iter = connections.begin();
			iter != connections.end();
			iter++)
		connectionsString += *iter + " ";

	std::stringstream stream;
	stream << "label = " << agent.getLabel() << ", currentState = " << toString(agent.getCurrentState())
			<< ", nextState = " << toString(agent.getNextState()) << ", connections = [ "
			<< connectionsString << "]";
	return stream.str();
}
