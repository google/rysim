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

import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.testkit.TestActorRef;
import org.junit.*;

public class SimulationControllerTest {
    private ActorSystem mActorSystem;

    @BeforeClass
    public static void setUpClass() { }

    @Before
    public void setUpTest() throws Exception {
        mActorSystem = ActorSystem.create();
        NumberGenerator.getInstance().clear();
    }

    @After
    public void tearDownTest() throws Exception {
        mActorSystem.shutdown();
        mActorSystem = null;
    }

    @AfterClass
    public static void tearDownClass() { }

    @Test
    public void testRunLoopSEIR() {
        final TestActorRef<SimulationController> actorRef = TestActorRef.create(
                mActorSystem, Props.create(SimulationController.class));

        Boolean result = null;
        result = MessagingUtils.askSynchronous(
            actorRef, SimulationController.InitMessage.create("test/data/loop_seir_experiment.json", 1), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        result = MessagingUtils.askSynchronous(actorRef, SimulationController.RunMessage.create(), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        final SimulationController simulationController = actorRef.underlyingActor();
        Assert.assertEquals(4, simulationController.getAgentCount());
        Assert.assertEquals(1000, simulationController.getEventLimit());
        Assert.assertEquals(1002, simulationController.getEventCount());
        Assert.assertEquals(835, simulationController.getTimestamp());
    }

    @Test
    public void testRunSimpleSEIR() {
        final TestActorRef<SimulationController> actorRef = TestActorRef.create(
                mActorSystem, Props.create(SimulationController.class));

        Boolean result = null;
        result = MessagingUtils.askSynchronous(
                actorRef, SimulationController.InitMessage.create("test/data/simple_seir_experiment.json", 1), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        result = MessagingUtils.askSynchronous(actorRef, SimulationController.RunMessage.create(), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        final SimulationController simulationController = actorRef.underlyingActor();
        Assert.assertEquals(3, simulationController.getAgentCount());
        Assert.assertEquals(1000, simulationController.getEventLimit());
        Assert.assertEquals(7, simulationController.getEventCount());
        Assert.assertEquals(25, simulationController.getTimestamp());
    }

    @Test
    public void testRunTrivialSEIR() {
        final TestActorRef<SimulationController> actorRef = TestActorRef.create(
                mActorSystem, Props.create(SimulationController.class));

        Boolean result = null;
        result = MessagingUtils.askSynchronous(
                actorRef, SimulationController.InitMessage.create("test/data/trivial_seir_experiment.json", 1), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        result = MessagingUtils.askSynchronous(actorRef, SimulationController.RunMessage.create(), result);

        Assert.assertNotNull(result);
        Assert.assertTrue(result);

        final SimulationController simulationController = actorRef.underlyingActor();
        Assert.assertEquals(1, simulationController.getAgentCount());
        Assert.assertEquals(0, simulationController.getEventLimit());
        Assert.assertEquals(0, simulationController.getEventCount());
        Assert.assertEquals(0, simulationController.getTimestamp());
    }
}