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

import org.json.simple.JSONObject;

public class SimulationController {
    private static final String TAG = "SimulationController";

    private String mExperimentName;
    private String mExperimentType;
    private long mEventLimit;
    private final Model mModel = new Model();
    private long mTimestamp = 0;
    private long mEventCount = 0;

    public SimulationController() {

    }

    public long getAgentCount() {
        return mModel.getModelSize();
    }

    public long getEventLimit() {
        return mEventLimit;
    }

    public long getEventCount() {
        return mEventCount;
    }

    public long getTimestamp() {
        return mTimestamp;
    }

    public boolean init(String filename, long seed) {
        NumberGenerator.getInstance().init(seed);

        final Experiment experiment = new Experiment();
        if (!experiment.parse(filename)) {
            SimpleLogger.error(TAG, "Unable to parse supplied experiment file = " + filename);
            return false;
        }
        mExperimentName = experiment.getExperimentName();
        mExperimentType = experiment.getExperimentType();
        mEventLimit = experiment.getEventLimit();

        if (!mModel.parse(experiment.getModelFilename())) {
            SimpleLogger.error(TAG, "Unable to parse supplied experiment file = " + experiment.getModelFilename());
            return false;
        }

        return true;
    }

    public boolean run() {
        while (mEventCount < mEventLimit && !EventQueue.getInstance().isEmpty()) {
            Event event = EventQueue.getInstance().pop();
            mModel.processEvent(event);
            mEventCount++;
            mTimestamp = event.getTimestamp();
            while (EventQueue.getInstance().isNextEventAt(mTimestamp)) {
                event = EventQueue.getInstance().pop();
                mModel.processEvent(event);
                mEventCount++;
            }
            mModel.advanceState(mTimestamp);
        }

        return true;
    }

    @SuppressWarnings("unchecked")
    public void printResults() {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("experiment", mExperimentName);
        jsonObject.put("kernel", "JavaDES");
        jsonObject.put("type", mExperimentType);
        jsonObject.put("model", mModel.getModelName());
        jsonObject.put("event_count", mEventCount);
        jsonObject.put("final_time", mTimestamp);
        jsonObject.put("agents", mModel.getModelSize());
        jsonObject.put("connections", mModel.getTotalConnections());

        System.out.println("JSONBEGIN");
        System.out.println(jsonObject);
        System.out.println("JSONEND");
    }
 }
