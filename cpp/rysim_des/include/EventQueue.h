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
#ifndef EVENTQUEUE_H_
#define EVENTQUEUE_H_

#include <queue>
#include <stdint.h>

#include "Event.h"

class EventQueue {
    public:
        static EventQueue* get();
        virtual ~EventQueue();

        Event pop();
        void push(const Event& event);
        bool isEmpty();
        bool isNextEventAt(int64_t timestamp);

        void clear();
    private:
        std::priority_queue<Event> queue_;

        EventQueue();

        // Don't implement since this is a Singleton.
        EventQueue(EventQueue const&);
        void operator=(EventQueue const&);
};

#endif /* EVENTQUEUE_H_ */
