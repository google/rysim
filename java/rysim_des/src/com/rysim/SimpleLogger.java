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

import java.lang.String;

public class SimpleLogger {
    public enum Level {
        DEBUG,
        INFO,
        WARNING,
        ERROR,
        FATAL
    };

    public static void log(Level level, String tag, String msg) {
        System.out.println(tag + ":" + level.toString() + ":" + msg);
        System.out.flush();
        assert (level != Level.FATAL);
    }

    public static void debug(String tag, String msg) {
        log(Level.DEBUG, tag, msg);
    }

    public static void info(String tag, String msg) {
        log(Level.INFO, tag, msg);
    }

    public static void warning(String tag, String msg) {
        log(Level.WARNING, tag, msg);
    }

    public static void error(String tag, String msg) {
        log(Level.ERROR, tag, msg);
    }

    public static void fatal(String tag, String msg) {
        log(Level.FATAL, tag, msg);
    }
}