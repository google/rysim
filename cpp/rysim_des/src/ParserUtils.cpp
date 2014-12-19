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
#include "ParserUtils.h"

#include <glog/logging.h>

bool ParserUtils::parseAsString(
		const Json::Value& root, const std::string& entryName, std::string* value) {
	if (!root.isMember(entryName) || !root[entryName].isString()) {
		LOG(WARNING) << "Could not find/parse " << entryName << " in JSON file!";
        return false;
	}
	*value = root[entryName].asString();
    return true;
}

bool ParserUtils::parseAsUInt64(
		const Json::Value& root, const std::string& entryName, uint64_t* value) {
	if (!root.isMember(entryName) || !root[entryName].isIntegral()) {
		LOG(WARNING) << "Could not find/parse " << entryName << " in JSON file!";
        return false;
	}
	*value = root[entryName].asUInt64();
    return true;
}

bool ParserUtils::parseAsFloat(
		const Json::Value& root, const std::string& entryName, float* value) {
	if (!root.isMember(entryName) || !root[entryName].isNumeric()) {
		LOG(WARNING) << "Could not find/parse " << entryName << " in JSON file!";
        return false;
	}
	*value = root[entryName].asFloat();
    return true;
}

bool ParserUtils::parseAsArray(
		const Json::Value& root, const std::string& entryName, Json::Value* value) {
	if (!root.isMember(entryName) || !root[entryName].isArray()) {
		LOG(WARNING) << "Could not find/parse " << entryName << " in JSON file!";
        return false;
	}
	*value = root[entryName];
	return true;
}

bool ParserUtils::parseAsVectorOfFloats(
		const Json::Value& root, const std::string& entryName, std::vector<float>* value) {
	Json::Value array;
	if (!parseAsArray(root, entryName, &array)) {
		LOG(WARNING) << "Unable to find/parse " << entryName << " in JSON file!";
		return false;
	}

	for (Json::Value::iterator iter = array.begin(); iter != array.end(); iter++) {
		if (!(*iter).isNumeric()) {
			LOG(WARNING) << "Unable to parse " << entryName << " in JSON file!\n"
					     << "raw input = " << (*iter).asString();
			return false;
		}
		value->push_back((*iter).asFloat());
	}
	return true;
}

bool ParserUtils::parseAsVectorOfStrings(
		const Json::Value& root, const std::string& entryName,
		std::vector<std::string>* value) {
	Json::Value array;
	if (!parseAsArray(root, entryName, &array)) {
		LOG(WARNING) << "Unable to find/parse " << entryName << " in JSON file!";
		return false;
	}

	for (Json::Value::iterator iter = array.begin(); iter != array.end(); iter++) {
		if (!(*iter).isString()) {
			LOG(WARNING) << "Unable to parse " << entryName << " in JSON file!\n"
					   	 << "raw input = " << (*iter).asString();
			return false;
		}
		value->push_back((*iter).asString());
	}
	return true;
}
