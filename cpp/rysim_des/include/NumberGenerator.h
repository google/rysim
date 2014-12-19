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
#ifndef NUMBERGENERATOR_H_
#define NUMBERGENERATOR_H_

#include <map>
#include <stdint.h>
#include <string>
#include <vector>

class NumberGenerator {
    public:
        enum DistributionType {
            DISTRIBUTION_TYPE_GAUSSIANTAIL = 0,
            DISTRIBUTION_TYPE_EXPONENTIAL,
            DISTRIBUTION_TYPE_FLAT,
            DISTRIBUTION_TYPE_LOGNORMAL,
            DISTRIBUTION_TYPE_POISSON,
            DISTRIBUTION_TYPE_BERNOULLI,
            DISTRIBUTION_TYPE_BINOMIAL,
            DISTRIBUTION_TYPE_NEGATIVEBINOMIAL,
            DISTRIBUTION_TYPE_GEOMETRIC,
            NUM_DISTRIBUTION_TYPE,
        };

        class Signature {
            public:
                Signature();
                Signature(DistributionType type, float scale, std::vector<float> params);
                virtual ~Signature();

                DistributionType getType() const;
                float getScale() const;
                const std::vector<float>& getParams() const;

            private:
                DistributionType type_;
                float scale_;
                std::vector<float> params_;
        };

        static NumberGenerator* get();
        virtual ~NumberGenerator();

        void init(const long int& seed);
        bool registerDistribution(const std::string& label, const Signature& signature);
        uint64_t callDistribution(const std::string& label);
        bool isRegistered(const std::string& label);
        void clear();

    private:
        bool isInitialized_;
        std::map<std::string, Signature> distributions_;

        NumberGenerator();

        // Don't implement since this is a Singleton.
        NumberGenerator(NumberGenerator const&);
        void operator=(NumberGenerator const&);

        float generateNumber(DistributionType type, const std::vector<float>& params);
        float generateUniform() const;
        float generateGaussianTail(float a, float sigma) const;
        float generateExponential(float lambda) const;
        float generateFlat(float a, float b) const;
        float generateLognormal(float mu, float sigma) const;
        float generatePoisson(float lambda) const;
        float generateBernoulli(float p) const;
        float generateBinomial(float p, float n) const;
        float generateNegativeBinomial(float p, float n) const;
        float generateGeometric(float p) const;

};

#endif /* NUMBERGENERATOR_H_ */
