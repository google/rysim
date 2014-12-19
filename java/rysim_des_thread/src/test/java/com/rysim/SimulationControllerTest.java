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


import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class SimulationControllerTest {
    private SimulationController mSimulationController;

    @Before
    public void setUp() throws Exception {
        mSimulationController = new SimulationController();
        NumberGenerator.getInstance().clear();
        EventQueue.getInstance().clear();
    }

    @After
    public void tearDown() throws Exception {
        mSimulationController = null;
    }

    @Test
    public void testRunLoopSEIR() throws Exception {
        Assert.assertTrue(mSimulationController.init("test/data/loop_seir_experiment.json", 1));
        Assert.assertTrue(mSimulationController.run());

        Assert.assertEquals(4, mSimulationController.getAgentCount());
        Assert.assertEquals(1000, mSimulationController.getEventLimit());
        Assert.assertEquals(1002, mSimulationController.getEventCount());
        Assert.assertEquals(835, mSimulationController.getTimestamp());
    }

    @Test
    public void testRunSimpleSEIR() throws Exception {
        Assert.assertTrue(mSimulationController.init("test/data/simple_seir_experiment.json", 1));
        Assert.assertTrue(mSimulationController.run());

        Assert.assertEquals(3, mSimulationController.getAgentCount());
        Assert.assertEquals(1000, mSimulationController.getEventLimit());
        Assert.assertEquals(7, mSimulationController.getEventCount());
        Assert.assertEquals(25, mSimulationController.getTimestamp());
    }

    @Test
    public void testRunTrivialSEIR() throws Exception {
        Assert.assertTrue(mSimulationController.init("test/data/trivial_seir_experiment.json", 1));
        Assert.assertTrue(mSimulationController.run());

        Assert.assertEquals(1, mSimulationController.getAgentCount());
        Assert.assertEquals(0, mSimulationController.getEventLimit());
        Assert.assertEquals(0, mSimulationController.getEventCount());
        Assert.assertEquals(0, mSimulationController.getTimestamp());
    }
}