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
#include "EventQueue.h"

#include <glog/logging.h>

#include "LoggingUtils.h"

EventQueue* EventQueue::get() {
    static EventQueue queue;
    return &queue;
}

EventQueue::~EventQueue() {
}

Event EventQueue::pop() {
    if (queue_.size() == 0) {
    	LOG(FATAL) << "Attempted to pop from empty queue!";
    }

    Event event = queue_.top();
    queue_.pop();
    return event;
}

void EventQueue::push(const Event& event) {
    queue_.push(event);
}


bool EventQueue::isEmpty() {
    return queue_.size() == 0;
}


bool EventQueue::isNextEventAt(int64_t timestamp) {
	if (queue_.empty())
		return false;
    Event event;
    event.setTimestamp(timestamp);
    return queue_.top().isAtSameTime(event);
}

void EventQueue::clear() {
	while (!queue_.empty())
		queue_.pop();
}

EventQueue::EventQueue() {
}
