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
import akka.actor.UntypedActor;

import java.util.List;

public class Agent extends UntypedActor {
    private static final String TAG = "Agent";

    public enum State {
        SUSCEPTIBLE,
        EXPOSED,
        INFECTIOUS,
        RECOVERED,
        UNKNOWN
    }

    private String mLabel;
    private State mCurrentState;
    private State mNextState;
    private List<String> mConnections;
    private String mS2E;
    private String mE2I;
    private String mI2R;
    private String mR2S;
    private ActorRef mEventQueue;

    public static class InitMessage {
        public String label;
        public State currentState;
        public List<String> connections;
        public String s2e;
        public String e2i;
        public String i2r;
        public String r2s;
        public ActorRef eventQueue;
        public ActorRef simulationController;
        public static InitMessage create(
                String label, State currentState, List<String> connections, String s2e, String e2i, String i2r,
                String r2s, ActorRef eventQueue, ActorRef simulationController) {
            InitMessage message = new InitMessage();
            message.label = label;
            message.currentState = currentState;
            message.connections = connections;
            message.s2e = s2e;
            message.e2i = e2i;
            message.i2r = i2r;
            message.r2s = r2s;
            message.eventQueue = eventQueue;
            message.simulationController = simulationController;
            return message;
        }
    }

    public static class ProcessEventMessage {
        public Event event;
        public static ProcessEventMessage create(Event event) {
            ProcessEventMessage message = new ProcessEventMessage();
            message.event = event;
            return message;
        }
    }

    public static class AdvanceStateMessage {
        public long timestamp;
        public static AdvanceStateMessage create(long timestamp) {
            AdvanceStateMessage message = new AdvanceStateMessage();
            message.timestamp = timestamp;
            return message;
        }
    }

    public Agent() { }

    @Override
    public void onReceive(Object message) throws Exception {
        if (message instanceof InitMessage) {
            handleInitMessage((InitMessage) message);
        } else if (message instanceof ProcessEventMessage) {
            handleProcessEventMessage((ProcessEventMessage) message);
        } else if (message instanceof AdvanceStateMessage) {
            handleAdvanceStateMessage((AdvanceStateMessage) message);
        } else {
            unhandled(message);
        }
    }

    public String getLabel() {
        return mLabel;
    }

    public State getCurrentState() {
        return mCurrentState;
    }

    public State getNextState() {
        return mNextState;
    }

    public List<String> getConnections() {
        return mConnections;
    }

    public String getS2E() {
        return mS2E;
    }

    public String getE2I() {
        return mE2I;
    }

    public String getI2R() {
        return mI2R;
    }

    public String getR2S() {
        return mR2S;
    }

    private void handleInitMessage(InitMessage message) {
        mLabel = message.label;
        mCurrentState = message.currentState;
        mNextState = State.UNKNOWN;
        mConnections = message.connections;
        mS2E = message.s2e;
        mE2I = message.e2i;
        mI2R = message.i2r;
        mR2S = message.r2s;
        mEventQueue = message.eventQueue;

        transitionState(0);
        message.simulationController.tell(SimulationController.InitializedMessage.create(), getSelf());
    }

    private void handleProcessEventMessage(ProcessEventMessage message) {
        State newState = message.event.getState();
        if (shouldTransition(newState))
            mNextState = newState;
    }

    private void handleAdvanceStateMessage(AdvanceStateMessage message) {
        if (mNextState != State.UNKNOWN) {
            mCurrentState = mNextState;
            transitionState(message.timestamp);
        }
        mNextState = State.UNKNOWN;
        getSender().tell(SimulationController.AdvancedMessage.create(), getSelf());
    }

    private boolean shouldTransition(State newState) {
        boolean retValue = false;
        switch (mCurrentState) {
            case SUSCEPTIBLE:
                if (newState == State.EXPOSED) {
                    retValue = true;
                } else {
                    SimpleLogger.fatal(TAG, "Unexpected new state, " + newState.toString() + ", while in SUSCEPTIBLE!");
                }
                break;
            case EXPOSED:
                if (newState == State.INFECTIOUS) {
                    retValue = true;
                } else if (newState == State.EXPOSED) {
                    retValue = false;
                } else {
                    SimpleLogger.fatal(TAG, "Unexpected new state, " + newState.toString() + ", while in EXPOSED!");
                }
                break;
            case INFECTIOUS:
                if (newState == State.RECOVERED) {
                    retValue = true;
                } else if (newState == State.EXPOSED) {
                    retValue = false;
                } else {
                    SimpleLogger.fatal(TAG, "Unexpected new state, " + newState.toString() + ", while in INFECTIOUS!");
                }
                break;
            case RECOVERED:
                if (newState == State.SUSCEPTIBLE && mR2S != null) {
                    retValue = true;
                } else if (newState == State.EXPOSED) {
                    retValue = false;
                } else {
                    SimpleLogger.fatal(TAG, "Unexpected new state, " + newState.toString() + ", while in RECOVERED!");
                }
                break;
            default:
                SimpleLogger.fatal(TAG, "Unknown current state!");
                break;
        }
        return retValue;
    }

    private void transitionState(long timestamp) {
        long delta;
        switch (mCurrentState) {
            case SUSCEPTIBLE:
                break;
            case EXPOSED:
                delta = callDistribution(mE2I);
                sendEvent(timestamp + delta, mLabel, State.INFECTIOUS);
                break;
            case INFECTIOUS:
                delta = callDistribution(mI2R);
                sendEvent(timestamp + delta, mLabel, State.RECOVERED);
                infectNeighbours(timestamp, delta);
                break;
            case RECOVERED:
                if (mR2S != null) {
                    delta = callDistribution(mR2S);
                    sendEvent(timestamp + delta, mLabel, State.SUSCEPTIBLE);
                }
                break;
            default:
                SimpleLogger.fatal(TAG, mLabel + ": Unknown current state!");
                break;
        }
    }

    private void sendEvent(long timestamp, String label, State state) {
        Event event = new Event();
        event.setTimestamp(timestamp);
        event.setTarget(label);
        event.setState(state);
        mEventQueue.tell(EventQueue.PushMessage.create(event), getSelf());
    }

    private void infectNeighbours(long timestamp, long delta) {
        for (String connection : mConnections) {
            long infectionDelta = callDistribution(mS2E);

            if (infectionDelta <= delta) {
                sendEvent(timestamp + infectionDelta, connection, State.EXPOSED);
            }
        }
    }

    private long callDistribution(String label) {
        return 1 + NumberGenerator.getInstance().callDistribution(label);
    }
}