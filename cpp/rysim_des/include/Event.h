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
#ifndef EVENT_H_
#define EVENT_H_

#include <stdint.h>
#include <string>

#include "State.h"

class Event {
    public:
        static Event invalidEvent();

        Event();
        virtual ~Event();

        int64_t getTimestamp() const;
        std::string getTarget() const;
        State getBody() const;

        void setTimestamp(const int64_t timestamp);
        void setTarget(const std::string& target);
        void setBody(const State body);

        bool operator<(const Event& other) const;
        bool isAtSameTime(const Event& other) const;

        private:
        int64_t timestamp_;
        std::string target_;
        State body_;

        Event(int64_t timestamp);
};
#endif /* STATE_H_ */
