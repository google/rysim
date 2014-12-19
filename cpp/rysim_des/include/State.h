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
#ifndef STATE_H_
#define STATE_H_

enum State {
    STATE_SUSCEPTIBLE = 0,
    STATE_EXPOSED = 1,
    STATE_INFECTIOUS = 2,
    STATE_RECOVERED = 3,
    STATE_UNKNOWN = 5,
    NUM_STATE = 6
};

#endif /* STATE_H_ */
