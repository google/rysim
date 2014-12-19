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
#ifndef EXPERIMENT_H_
#define EXPERIMENT_H_

#include <json/json.h>
#include <stdint.h>
#include <string>

class Experiment {
    public:
        Experiment();
        virtual ~Experiment();

        std::string getModelFilename() const;
        std::string getExperimentName() const;
        uint64_t getEventLimit() const;
        std::string getExperimentType() const;

        bool parse(const std::string& filename);

    private:
        std::string modelFilename_;
        std::string experimentName_;
        uint64_t eventLimit_;
        std::string type_;
};

#endif /* EXPERIMENT_H_ */
