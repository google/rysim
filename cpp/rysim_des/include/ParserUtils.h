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
#ifndef PARSERUTILS_H_
#define PARSERUTILS_H_

#include <json/json.h>
#include <stdint.h>
#include <string>

class ParserUtils {
	public:
    	static bool parseAsString(
    			const Json::Value& root, const std::string& entryName, std::string* value);
    	static bool parseAsUInt64(
    			const Json::Value& root, const std::string& entryName, uint64_t* value);
    	static bool parseAsFloat(
    			const Json::Value& root, const std::string& entryName, float* value);
    	static bool parseAsArray(
    			const Json::Value& root, const std::string& entryName, Json::Value* value);
    	static bool parseAsVectorOfFloats(
    			const Json::Value& root, const std::string& entryName, std::vector<float>* value);
    	static bool parseAsVectorOfStrings(
    			const Json::Value& root, const std::string& entryName,
    			std::vector<std::string>* value);
	private:
    	ParserUtils() {}
    	virtual ~ParserUtils() {}
};

#endif /* PARSERUTILS_H_ */
