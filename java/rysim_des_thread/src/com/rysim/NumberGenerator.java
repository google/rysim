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

import java.util.HashMap;
import java.util.Random;

public class NumberGenerator {
    private static final String TAG = "NumberGenerator";
    private boolean mIsInitialized = false;
    private Random mRandom;
    private final HashMap<String, Signature> mDistributions = new HashMap<String, Signature>();
    enum DistributionType {
        GAUSSIAN_TAIL,
        EXPONENTIAL,
        FLAT,
        LOGNORMAL,
        POISSON,
        BERNOULLI,
        BINOMIAL,
        NEGATIVE_BINOMIAL,
        GEOMETRIC
    }

    public static class Signature {
        private DistributionType mDistributionType;
        private float mScale;
        private Float[] mParams;

        public Signature(DistributionType distributionType, float scale, Float[] params) {
            mDistributionType = distributionType;
            mScale = scale;
            mParams = params;
        }

        public DistributionType getType() {
            return mDistributionType;
        }

        public float getScale() {
            return mScale;
        }

        public Float[] getParams() {
            return mParams;
        }
    }

    private static final NumberGenerator mInstance = new NumberGenerator();

    private NumberGenerator() {

    }

    public static NumberGenerator getInstance() {
        return mInstance;
    }

    public void init(long seed) {
        if (mIsInitialized)
            SimpleLogger.fatal(TAG, "Attempted to initialize NumberGenerator when already initialized!");

        mIsInitialized = true;
        mRandom = new Random(seed);
    }

    @SuppressWarnings("unchecked")
    public boolean registerDistribution(String label, Signature signature) {
        if (!mIsInitialized)
            SimpleLogger.fatal(TAG, "Attempted to register distribution when NumberGenerator is not initialized!");

        if (mDistributions.get(label) != null)
            SimpleLogger.fatal(TAG, "Attempted to register distribution called " + label + " multiple times!");

        mDistributions.put(label, signature);
        return true;
    }

    public long callDistribution(String label) {
        if (!mIsInitialized)
            SimpleLogger.fatal(TAG, "Attempted to call distribution when NumberGenerator is not initialized!");

        Signature signature = mDistributions.get(label);
        if (signature == null)
            SimpleLogger.fatal(TAG, "Attempted to call distribution called " + label + " when not registered!");
        float retVal = signature.getScale() * generateNumber(signature.getType(), signature.getParams());

        if (retVal < 0.0f)
            SimpleLogger.fatal(TAG, "Generated negative value in callDistribution!");

        return Math.round(retVal);
    }

    public void clear() {
        mDistributions.clear();
        mIsInitialized = false;
    }

    private float generateNumber(DistributionType type, Float[] params) {
        switch (type) {
            case GAUSSIAN_TAIL:
                return generateGaussianTail(params[0], params[1]);
            case EXPONENTIAL:
                return generateExponential(params[0]);
            case FLAT:
                return generateFlat(params[0], params[1]);
            case LOGNORMAL:
                return generateLognormal(params[0], params[1]);
            case POISSON:
                return generatePoisson(params[0]);
            case BERNOULLI:
                return generateBernoulli(params[0]);
            case BINOMIAL:
                return generateBinomial(params[0], params[1]);
            case NEGATIVE_BINOMIAL:
                return generateNegativeBinomial(params[0], params[1]);
            case GEOMETRIC:
                return generateGeometric(params[0]);
        }
        SimpleLogger.fatal(TAG, "generateNumber failed due to unknown distribution type!");
        return -1.0f;
    }

    private float generateUniform() {
        return mRandom.nextFloat();
    }

    private float generateGaussianTail(float a, float sigma) {
        float ret_val = -1.0f;
        while (ret_val < 0.0f) {
            float u1 = generateUniform();
            float u2 = generateUniform();
            float test_val = (float) Math.abs(sigma * Math.cos(2 * Math.PI * u1) * Math.sqrt(-2 * Math.log(u2)));
            if (test_val > a) {
                ret_val = test_val;
                continue;
            }
            test_val = (float)Math.abs(sigma * Math.sin(2 * Math.PI * u1) * Math.sqrt(-2 * Math.log(u2)));
            if (test_val > a)
                ret_val = test_val;
        }
        return ret_val;
    }

    private float generateExponential(float lambda) {
        float u = generateUniform();
        return (float)(-lambda * Math.log(u));
    }

    private float generateFlat(float a, float b) {
        float u = generateUniform();
        return a + (b - a) * u;
    }

    private float generateLognormal(float mu, float sigma) {
        float u = generateUniform();
        return (float)Math.exp(mu + sigma * u);
    }

    private float generatePoisson(float lambda) {
        float products = generateUniform();
        float target = (float)Math.exp(-lambda);
        int n = 1;
        while (products >= target) {
            products *= generateUniform();
            n++;
        }
        return n;
    }

    private float generateBernoulli(float p) {
        float u = generateUniform();
        if (p > u)
            return 1;
        else
            return 0;
    }

    private float generateBinomial(float p, float n) {
        int count = 0;
        for (int i = 0; i < n; i++) {
            float u = generateUniform();
            if (u < p)
                count++;
        }
        return (float) count;
    }

    private float generateNegativeBinomial(float p, float n) {
        int count = 0;
        int i = 0;
        while (i < n) {
            float u = generateUniform();
            if (u > p)
                i++;
            else
                count++;
        }
        return (float) count;
    }

    private float generateGeometric(float p) {
        float u = generateUniform();
        return (float) Math.ceil(Math.log(u) / Math.log(1 - p));
    }
}