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

import akka.actor.ActorRef;
import akka.actor.Props;
import akka.actor.UntypedActor;


public class MasterActor extends UntypedActor {
    private static final String TAG = "MasterActor";
    private ActorRef mSimulationController;

    public static class RunMessage {
        public String filename;
        public long seed;
        public static RunMessage create(String filename, long seed) {
            RunMessage message = new RunMessage();
            message.filename = filename;
            message.seed = seed;
            return message;
        }
    }

    @Override
    public void onReceive(Object message) throws Exception {
        if (message instanceof RunMessage) {
            handleRunMessage((RunMessage) message);
        } else {
            unhandled(message);
        }
    }

    private void handleRunMessage(RunMessage message) {
        SimpleLogger.info(TAG, "Initializing SEIR DES Actor Application");
        mSimulationController = getContext().actorOf(Props.create(SimulationController.class), "SimulationController");

        Boolean result = null;
        result = MessagingUtils.askSynchronous(
                mSimulationController, SimulationController.InitMessage.create(message.filename, message.seed),
                result);
        if (result == null || !result) {
            SimpleLogger.error(TAG, "Initialization in SEIR DES Actor failed!");
            SimpleLogger.info(TAG, "Finished SEIR DES Actor Application");
            SimpleLogger.info(TAG, "SEIR DES Actor Application FAILED");
            endExperiment();
        }
        SimpleLogger.info(TAG, "Running SEIR DES Application");

        result = MessagingUtils.askSynchronous(
                mSimulationController, SimulationController.RunMessage.create(), result);
        if (result == null || !result) {
            SimpleLogger.error(TAG, "Run in SEIR DES Actor failed!");
            SimpleLogger.info(TAG, "Finished SEIR DES Actor Application");
            SimpleLogger.info(TAG, "SEIR DES Actor Application FAILED");
            endExperiment();
        }

        MessagingUtils.askSynchronous(mSimulationController, SimulationController.PrintMessage.create(), result);
        SimpleLogger.info(TAG, "Finished SEIR DES Actor Application");
        SimpleLogger.info(TAG, "SEIR DES Actor Application SUCCEEDED");
        endExperiment();
    }

    private void endExperiment() {
        getContext().system().shutdown();
    }
}
