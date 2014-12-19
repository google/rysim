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

import akka.actor.UntypedActor;

import java.util.Comparator;
import java.util.PriorityQueue;

public class EventQueue extends UntypedActor {
    private static final String TAG = "EventQueue";
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

    public static class PopMessage {
        public static PopMessage create() {
            return new PopMessage();
        }
    }

    public static class PushMessage {
        public Event event;
        public static PushMessage create(Event event) {
            PushMessage message = new PushMessage();
            message.event = event;
            return message;
        }
    }

    public static class IsEmptyMessage {
        public static IsEmptyMessage create() {
            return new IsEmptyMessage();
        }
    }

    public static class IsNextEventAtMessage {
        public long timestamp;

        public static IsNextEventAtMessage create(long timestamp) {
            IsNextEventAtMessage message = new IsNextEventAtMessage();
            message.timestamp = timestamp;
            return message;
        }
    }

    private EventQueue() {
        // TODO: Figure out why this weird hack is need to make tests pass...
        mQueue.clear();
    }

    @Override
    public void onReceive(Object message) throws Exception {
        if (message instanceof PopMessage) {
            handlePopMessage();
        } else if (message instanceof PushMessage) {
            handlePushMessage((PushMessage) message);
        } else if (message instanceof IsEmptyMessage) {
            handleIsEmptyMessage();
        } else if (message instanceof IsNextEventAtMessage) {
            handleIsNextEventAtMessage((IsNextEventAtMessage) message);
        } else {
            unhandled(message);
        }
    }

    private void handlePopMessage() {
        if (mQueue.size() == 0)
            SimpleLogger.fatal(TAG, "Attempted to pop from an empty queue!");
        Event result = mQueue.poll();
        getSender().tell(result, getSelf());
    }

    private void handlePushMessage(PushMessage message) {
        mQueue.add(message.event);
    }

    private void handleIsEmptyMessage() {
        getSender().tell(mQueue.size() == 0, getSelf());
    }

    private void handleIsNextEventAtMessage(IsNextEventAtMessage message) {
        if (mQueue.size() == 0) {
            getSender().tell(false, getSelf());
            return;
        }

        Event event = mQueue.peek();
        getSender().tell(event.getTimestamp() == message.timestamp, getSelf());
    }
}