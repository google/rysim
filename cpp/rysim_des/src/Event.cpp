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
#include "Event.h"

#include <glog/logging.h>
Event Event::invalidEvent() {
    static Event event(-1);
    return event;
}

Event::Event() {
    timestamp_ = 0;
}

Event::~Event() {
}

int64_t Event::getTimestamp() const {
    return timestamp_;
}

std::string Event::getTarget() const {
    return target_;
}

State Event::getBody() const {
    return body_;
}

void Event::setTimestamp(const int64_t timestamp) {
    timestamp_ = timestamp;
}

void Event::setTarget(const std::string& target) {
    target_ = target;
}

void Event::setBody(const State body) {
    body_ = body;
}

bool Event::operator <(const Event& other) const {
 return timestamp_ > other.timestamp_;
}

bool Event::isAtSameTime(const Event& other) const {
    return timestamp_ == other.timestamp_;
}

Event::Event(int64_t timestamp) {
    timestamp_ = timestamp;
}
