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

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

class Model {
    private static final String TAG = "Model";

    private String mModelName;
    HashMap<String, Agent> mAgents = new HashMap<>();
    private Long mTotalConnections;

    public Model() { }

    public long getModelSize() {
        return mAgents.size();
    }

    public long getTotalConnections() { return mTotalConnections; }

    public String getModelName() { return mModelName; }

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

        mModelName = (String) jsonObject.get("model_name");
        if (mModelName == null) {
            SimpleLogger.error(TAG, "Unable to find/parse model_name!");
            return false;
        }

        mTotalConnections = (Long) jsonObject.get("total_connections");
        if (mTotalConnections == null) {
            SimpleLogger.error(TAG, "Unable to find/parse total_connections!");
            return false;
        }

        JSONArray distributions = (JSONArray) jsonObject.get("distributions");
        if (distributions == null) {
            SimpleLogger.error(TAG, "Unable to find/parse distribution!");
            return false;
        }
        if (!parseDistributions(distributions)) return false;

        JSONArray agents = (JSONArray) jsonObject.get("agents");
        if (agents == null) {
            SimpleLogger.error(TAG, "Unable to find/parse agents!");
            return false;
        }
        return parseAgents(agents) && validateConnections();
    }


    public void processTimestamp(String target, long timestamp, List<Event> events) {
        if (mAgents.get(target) == null)
            SimpleLogger.fatal(
                    TAG,
                    "Attempted to process timestamp, " + timestamp + ", for non-registered Agent, " + target);
        mAgents.get(target).processTimestamp(timestamp, events);
    }

    @SuppressWarnings("unchecked")
    private boolean parseDistributions(JSONArray distributions) {
        for (Object object : distributions) {
            JSONObject distribution = (JSONObject) object;

            String label = (String) distribution.get("label");
            if (label == null) {
                SimpleLogger.error(TAG, "Unable to find/parse label in distribution!");
                return false;
            }
            String typeString = (String) distribution.get("type");
            if (typeString == null) {
                SimpleLogger.error(TAG, "Unable to find/parse type in distribution!");
                return false;
            }
            NumberGenerator.DistributionType distributionType = convertToDistributionType(typeString);
            if (distributionType == null) {
                SimpleLogger.error(TAG, "Unable to convert type in distribution!");
                return false;
            }
            Number scaleNumber = (Number) distribution.get("scale");
            Float scale;
            if (scaleNumber == null) {
                SimpleLogger.info(TAG, "Scale not present, inserting 1.0");
                scale = new Float(1.0f);
            } else {
                scale = scaleNumber.floatValue();
                if (scale <= 0.0) {
                    SimpleLogger.warning(TAG, "Scale non-positive, inserting 1.0");
                    scale = 1.0f;
                }
            }
            JSONArray paramsList = (JSONArray) distribution.get("params");
            if (paramsList == null) {
                SimpleLogger.error(TAG, "Unable to convert params in distribution!");
                return false;
            }

            Float[] params = new Float[paramsList.size()];
            int idx = 0;
            for (Object param : paramsList) {
                params[idx] = ((Number) param).floatValue();
                idx++;
            }

            NumberGenerator.Signature signature = new NumberGenerator.Signature(
                    distributionType,
                    scale,
                    params);

            if (!NumberGenerator.getInstance().registerDistribution(label, signature)) {
                SimpleLogger.error(TAG, "Unable to register distribution " + label + "!");
                return false;
            }
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private boolean parseAgents(JSONArray agents) {
        for(Object object: agents) {
            JSONObject jsonAgent = (JSONObject) object;

            String label = (String) jsonAgent.get("label");
            if (label == null) {
                SimpleLogger.error(TAG, "Unable to find/parse label in agent!");
                return false;
            }
            if (mAgents.get(label) != null) {
                SimpleLogger.error(TAG, "Parsed agent with duplicate label, " + label + "!");
                return false;
            }
            String stateString = (String) jsonAgent.get("state");
            if (stateString == null) {
                SimpleLogger.error(TAG, "Unable to find/parse state in agent!");
                return false;
            }
            Agent.State state = convertToAgentState(stateString);
            if (state == null) {
                SimpleLogger.error(TAG, "Unable to convert state in agent!");
                return false;
            }
            List<String> connections = (List<String>) jsonAgent.get("connections");
            if (connections == null) {
                SimpleLogger.error(TAG, "Unable to convert connections in agent!");
                return false;
            }
            String s2e = (String) jsonAgent.get("s2e");
            if (s2e == null) {
                SimpleLogger.error(TAG, "Unable to find/parse s2e in agent!");
                return false;
            }
            String e2i = (String) jsonAgent.get("e2i");
            if (e2i == null) {
                SimpleLogger.error(TAG, "Unable to find/parse e2i in agent!");
                return false;
            }
            String i2r = (String) jsonAgent.get("i2r");
            if (i2r == null) {
                SimpleLogger.error(TAG, "Unable to find/parse i2r in agent!");
                return false;
            }
            String r2s = (String) jsonAgent.get("r2s");

            Agent agent = new Agent(label, state, connections, s2e, e2i, i2r, r2s);
            mAgents.put(label, agent);
        }
        return true;
    }

    private boolean validateConnections() {
        Set<String> labels = mAgents.keySet();
        for (String label : labels) {
            Agent agent = mAgents.get(label);
            for (String connection : agent.getConnections()) {
                if (!labels.contains(connection)) {
                    SimpleLogger.error(TAG, "Found unregistered connection, " + connection + "!");
                    return false;
                }
            }
        }
        return true;
    }

    private NumberGenerator.DistributionType convertToDistributionType(String typeString) {
        switch (typeString.toLowerCase()) {
            case "gaussiantail":
                return NumberGenerator.DistributionType.GAUSSIAN_TAIL;
            case "exponential":
                return NumberGenerator.DistributionType.EXPONENTIAL;
            case "flat":
                return NumberGenerator.DistributionType.FLAT;
            case "lognormal":
                return NumberGenerator.DistributionType.LOGNORMAL;
            case "poisson":
                return NumberGenerator.DistributionType.POISSON;
            case "bernoulli":
                return NumberGenerator.DistributionType.BERNOULLI;
            case "binomial":
                return NumberGenerator.DistributionType.BINOMIAL;
            case "negativebinomial":
                return NumberGenerator.DistributionType.NEGATIVE_BINOMIAL;
            case "geometric":
                return NumberGenerator.DistributionType.GEOMETRIC;
            default:
                SimpleLogger.error(TAG, "Parsed unknown distribution type, " + typeString);
                return null;
        }
    }

    private Agent.State convertToAgentState(String stateString) {
        switch (stateString.toLowerCase()) {
            case "susceptible":
                return Agent.State.SUSCEPTIBLE;
            case "exposed":
                return Agent.State.EXPOSED;
            case "infectious":
                return Agent.State.INFECTIOUS;
            case "recovered":
                return Agent.State.RECOVERED;
            default:
                SimpleLogger.error(TAG, "Parsed unknown agent state, " + stateString);
                return null;
        }
    }
}
