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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SimulationController {
    private static final String TAG = "SimulationController";

    private String mExperimentName;
    private String mExperimentType;
    private long mEventLimit;
    private final Model mModel = new Model();
    private long mTimestamp = 0;
    private long mEventCount = 0;

    private final ExecutorService mThreadPool = Executors.newCachedThreadPool();
    private final Lock mLock = new ReentrantLock();
    private final Condition mAgentsOutstanding = mLock.newCondition();
    private long mOutstandingAgents = 0;

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
        HashMap<String, List<Event>> timestampHash = new HashMap<>();
        while (mEventCount < mEventLimit && !EventQueue.getInstance().isEmpty()) {
            Event event = EventQueue.getInstance().pop();
            addEventToHash(timestampHash, event);
            mTimestamp = event.getTimestamp();
            while (EventQueue.getInstance().isNextEventAt(mTimestamp)) {
                event = EventQueue.getInstance().pop();
                addEventToHash(timestampHash, event);
            }

            for (Map.Entry<String, List<Event>> entry : timestampHash.entrySet())
                mThreadPool.execute(createProcessTimestampRunnable(entry.getKey(), mTimestamp, entry.getValue()));

            mLock.lock();
            try {
                while (mOutstandingAgents > 0)
                    mAgentsOutstanding.await();
            } catch (Exception e) {
                SimpleLogger.fatal(TAG, "While awaiting for outstanding agents received exception, " + e);
            } finally {
                mLock.unlock();
                timestampHash.clear();
            }
        }
        mThreadPool.shutdown();
        return true;
    }

    private void addEventToHash(HashMap<String, List<Event>> timestampHash, Event event) {
        mEventCount++;
        String target = event.getTarget();

        List<Event> eventList;
        if (timestampHash.containsKey(target)) {
           eventList = timestampHash.get(target);
        } else {
            eventList = new LinkedList<>();
        }

        eventList.add(event);
        timestampHash.put(target, eventList);
    }

    private Runnable createProcessTimestampRunnable(final String target, final long timestamp, final List<Event> events) {
        mLock.lock();
        try {
            mOutstandingAgents++;
        } finally {
            mLock.unlock();
        }

        return new Runnable() {
            @Override
            public void run() {
                synchronized (this) {
                    mModel.processTimestamp(target, timestamp, events);
                    mLock.lock();
                    try {
                        mOutstandingAgents--;
                        if (mOutstandingAgents == 0)
                            mAgentsOutstanding.signal();
                    } finally {
                        mLock.unlock();
                    }
                }
            }
        };
    }

    @SuppressWarnings("unchecked")
    public void printResults() {
        JSONObject jsonObject = new JSONObject();
        jsonObject.put("experiment", mExperimentName);
        jsonObject.put("kernel", "JavaDESThread");
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
