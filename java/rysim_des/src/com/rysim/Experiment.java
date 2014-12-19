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
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Experiment {
    private static final String TAG = "Experiment";
    private String mExperimentName;
    private String mExperimentType;
    private String mModelFilename;
    private long mEventLimit;

    public Experiment() { }

    public String getExperimentName() {
        return mExperimentName;
    }

    public String getExperimentType() {
        return  mExperimentType;
    }

    public String getModelFilename() {
        return mModelFilename;
    }

    public long getEventLimit() {
        return mEventLimit;
    }

    public boolean parse(String filename) {
        final FileReader fileReader;
        try {
            fileReader = new FileReader(filename);
        } catch (FileNotFoundException e) {
            SimpleLogger.error(TAG, "FileReader threw exception, " + e);
            return false;
        }

        final JSONParser jsonParser = new JSONParser();
        final JSONObject jsonObject;
        try {
            jsonObject = (JSONObject)jsonParser.parse(fileReader);
        } catch (IOException e) {
            SimpleLogger.error(TAG, "JSONParser threw exception, " + e);
            return false;
        } catch (ParseException e) {
            SimpleLogger.error(TAG, "JSONParser threw exception, " + e);
            return false;
        }

        mExperimentName = (String) jsonObject.get("experiment_name");
        if (mExperimentName == null) {
            SimpleLogger.error(TAG, "Unable to find/parse experiment_name!");
            return false;
        }

        mExperimentType = (String) jsonObject.get("type");
        if (mExperimentType == null) {
            SimpleLogger.error(TAG, "Unable to find/parse type!");
            return false;
        }

        mModelFilename = (String) jsonObject.get("model_filename");
        if (mModelFilename == null) {
            SimpleLogger.error(TAG, "Unable to find/parse model_filename!");
            return false;
        }

        // The experiment and mode files are intended to be in the same relative directory, but we can't cwd into that
        // in Java, so need to extract the parent directory of the experiment and attach it to the model for testing
        // and such.
        Path experimentPath = Paths.get(filename);
        Path modelPath = experimentPath.getParent();
        if (modelPath != null)
            mModelFilename = modelPath.resolve(mModelFilename).toString();

        Long eventLimit = (Long) jsonObject.get("event_limit");
        if (eventLimit == null) {
            SimpleLogger.error(TAG, "Unable to find/parse event_limit!");
            return false;
        }

        mEventLimit = eventLimit;

        return true;
    }
}