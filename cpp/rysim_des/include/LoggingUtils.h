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
#ifndef LOGGINGUTILS_H_
#define LOGGINGUTILS_H_

#include <string>

#include "Agent.h"
#include "Event.h"
#include "State.h"

class LoggingUtils {
public:
	static std::string toString(const State& state);
	static std::string toString(const Event& event);
	static std::string toString(const Agent& agent);

private:
	LoggingUtils() {}
	virtual ~LoggingUtils() {}
};

#endif /* LOGGINGUTILS_H_ */
