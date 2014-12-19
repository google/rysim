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
package com.rysim;

import java.util.Comparator;
import java.util.PriorityQueue;

public class EventQueue {
    private static final String TAG = "EventQueue";
    private static final EventQueue mInstance = new EventQueue();
    private static final PriorityQueue<Event> mQueue = new PriorityQueue<>(1, new EventQueueSort());

    public static class EventQueueSort implements Comparator<Event> {
        @Override
        public int compare(Event event, Event event2) {
            long diff = event.getTimestamp() - event2.getTimestamp();
            if (diff < 0) {
                return -1;
            } else if (diff > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    }

    private EventQueue() {

    }

    public static EventQueue getInstance() {
        return mInstance;
    }

    public Event pop() {
        if (mQueue.size() == 0) {
            SimpleLogger.fatal(TAG, "Attempted to pop from an empty queue!");
        }
        return mQueue.poll();
    }

    public synchronized void push(Event event) {
        mQueue.add(event);
    }

    public boolean isEmpty() {
        return mQueue.size() == 0;
    }

    public boolean isNextEventAt(long timestamp) {
        if (mQueue.size() == 0)
            return false;
        Event event = mQueue.peek();

        return event.getTimestamp() == timestamp;
    }

    public void clear() {
        mQueue.clear();
    }
}