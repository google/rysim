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

public class Event {
    private long mTimestamp;
    private String mTarget;
    private Agent.State mState;

    public Event() {
    }

    public long getTimestamp() {
        return mTimestamp;
    }

    public String getTarget() {
        return mTarget;
    }

    public Agent.State getState() {
        return mState;
    }

    public void setTimestamp(long timestamp) {
        mTimestamp = timestamp;
    }

    public void setTarget(String target) {
        mTarget = target;
    }

    public void setState(Agent.State state) {
        mState = state;
    }

    public boolean isAtSameTime(Event other) {
        return mTimestamp == other.mTimestamp;
    }
}