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
#include "NumberGenerator.h"

#include <cmath>
#include <cstdlib>
#include <glog/logging.h>

NumberGenerator::Signature::Signature() {
}

NumberGenerator::Signature::Signature(
        DistributionType type, float scale, std::vector<float> params) {
    type_ = type;
    scale_ = scale;
    params_ = params;
}

NumberGenerator::Signature::~Signature() {
}

NumberGenerator::DistributionType NumberGenerator::Signature::getType() const {
    return type_;
}

float NumberGenerator::Signature::getScale() const {
    return scale_;
}

const std::vector<float>& NumberGenerator::Signature::getParams() const {
    return params_;
}

NumberGenerator* NumberGenerator::get() {
    static NumberGenerator generator;
    return &generator;
}

NumberGenerator::~NumberGenerator() {
}

void NumberGenerator::init(const long int& seed) {
    if (!isInitialized_) {
        isInitialized_ = true;
        srand(seed);
    } else {
        LOG(FATAL) << "Attempted to initialize NumberGenerator when already initialized!";
    }
}

NumberGenerator::NumberGenerator() {
    isInitialized_ = false;
}

bool NumberGenerator::registerDistribution(const std::string& label,
        const NumberGenerator::Signature& signature) {
    if (!isInitialized_) {
        LOG(FATAL) << "Attempted to registerDistribution when not initialized!";
    }

    if (isRegistered(label)) {
        LOG(ERROR) << "Attempted to register multiple distributions with the same label, "
                << label;
        return false;
    }
    distributions_[label] = signature;
    return true;
}

uint64_t NumberGenerator::callDistribution(const std::string& label) {
    if (!isRegistered(label)) {
        LOG(FATAL) << "Attempted to callDistribution with non-registered distribution, " << label;
    }

    Signature signature = distributions_[label];
    float ret_val = signature.getScale() * generateNumber(signature.getType(),
            signature.getParams());

    if (ret_val < 0.0) {
        LOG(FATAL) << "callDistribution failed due to number generator returning a "
                   << "negative value, " << ret_val;
    }

    return ret_val;
}

bool NumberGenerator::isRegistered(const std::string& label) {
    return distributions_.find(label) != distributions_.end();
}

void NumberGenerator::clear() {
	isInitialized_ = false;
	distributions_.clear();
}

float NumberGenerator::generateNumber(NumberGenerator::DistributionType type,
        const std::vector<float>& params) {
    switch (type) {
        case DISTRIBUTION_TYPE_GAUSSIANTAIL:
            return generateGaussianTail(params[0], params[1]);
            break;
        case DISTRIBUTION_TYPE_EXPONENTIAL:
            return generateExponential(params[0]);
            break;
        case DISTRIBUTION_TYPE_FLAT:
            return generateFlat(params[0], params[1]);
            break;
        case DISTRIBUTION_TYPE_LOGNORMAL:
            return generateLognormal(params[0], params[1]);
            break;
        case DISTRIBUTION_TYPE_POISSON:
            return generatePoisson(params[0]);
            break;
        case DISTRIBUTION_TYPE_BERNOULLI:
            return generateBernoulli(params[0]);
            break;
        case DISTRIBUTION_TYPE_BINOMIAL:
            return generateBinomial(params[0], params[1]);
            break;
        case DISTRIBUTION_TYPE_NEGATIVEBINOMIAL:
            return generateNegativeBinomial(params[0], params[1]);
            break;
        case DISTRIBUTION_TYPE_GEOMETRIC:
            return generateGeometric(params[0]);
            break;
        default:
            break;
    }
    LOG(FATAL) << "generateNumber failed due to unknown distribution type!";
    return -1.0f;
}

float NumberGenerator::generateUniform() const {
    return rand() / double(RAND_MAX);
}

float NumberGenerator::generateGaussianTail(float a, float sigma) const {
    float ret_val = -1.0f;
    while (ret_val < 0.0f) {
        float u1 = generateUniform();
        float u2 = generateUniform();
        float test_val = fabs(sigma * cos(2 * M_PI * u1) * sqrt(-2 * log(u2)));
        if (test_val > a) {
            ret_val = test_val;
            continue;
        }
        test_val = fabs(sigma * sin(2 * M_PI * u1) * sqrt(-2 * log(u2)));
        if (test_val > a)
            ret_val = test_val;
    }
    return ret_val;
}

float NumberGenerator::generateExponential(float lambda) const {
    float u = generateUniform();
    return -lambda * log(u);
}

float NumberGenerator::generateFlat(float a, float b) const {
    float u = generateUniform();
    return a + (b - a) * u;
}

float NumberGenerator::generateLognormal(float mu, float sigma) const {
    float u = generateUniform();
    return exp(mu + sigma * u);
}

float NumberGenerator::generatePoisson(float lambda) const {
    float products = generateUniform();
    float target = exp(-lambda);
    int n = 1;
    while (products >= target) {
        products *= generateUniform();
        n++;
    }
    return n;
}

float NumberGenerator::generateBernoulli(float p) const {
    float u = generateUniform();
    if (p > u)
        return 1;
    else
        return 0;
}

float NumberGenerator::generateBinomial(float p, float n) const {
    int count = 0;
    for (int i = 0; i < n; i++) {
        float u = generateUniform();
        if (u < p)
            count++;
    }
    return (float)count;
}

float NumberGenerator::generateNegativeBinomial(float p, float n) const {
    int count = 0;
    int i = 0;
    while (i < n) {
        float u = generateUniform();
        if (u > p)
            i++;
        else
            count++;
    }
    return (float)count;
}

float NumberGenerator::generateGeometric(float p) const {
    float u = generateUniform();
    return (float)ceil(log(u)/log(1-p));
}

