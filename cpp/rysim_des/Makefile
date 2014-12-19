# Copyright 2014 The RySim Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

DEBUG ?= 1
USE_CCACHE ?= 0

ifeq ($(USE_CCACHE), 0)
CXX = clang++
else
CXX = ccache clang++ -Qunused-arguments -fcolor-diagnostics
endif

JSONCPP_DIR = ../../third_party/cpp/jsoncpp

BASE_CXXFLAGS = -Iinclude -I$(JSONCPP_DIR)/include -Wall -Werror
ifeq ($(DEBUG), 1)
BASE_CXXFLAGS += -g -O0 -DDEBUG
else
BASE_CXXFLAGS += -O2 -DNDEBUG
endif

BASE_LDFLAGS = -lglog -lgflags
ifeq ($(DEBUG), 1)
BASE_LDFLAGS +=
else
BASE_LDFLAGS +=
endif

GTEST_DIR = ../../third_party/cpp/gtest-1.7.0/
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)

RYSIM_CXXFLAGS = $(BASE_CXXFLAGS)
RYSIM_LDFLAGS = $(BASE_LDFLAGS) -lboost_filesystem -lboost_system
RYSIM_OBJS = src/Agent.o						\
	     src/Event.o						\
	     src/EventQueue.o						\
	     src/Experiment.o						\
	     src/LoggingUtils.o						\
	     src/Model.o						\
	     src/NumberGenerator.o					\
	     src/ParserUtils.o						\
	     src/SimulationController.o					\
	     $(JSONCPP_DIR)/jsoncpp.o

RYSIM_TESTS_CXXFLAGS = $(RYSIM_CXXFLAGS) -I$(GTEST_DIR)/include
RYSIM_TESTS_LDFLAGS = $(RYSIM_LDFLAGS)
RYSIM_TESTS_OBJS = $(RYSIM_OBJS)					\
		       test/ExperimentTest.o				\
		       test/ModelTest.o					\
		       test/SimulationControllerTest.o

all: bin tests

bin: rysim

rysim: CXXFLAGS = $(RYSIM_CXXFLAGS)
rysim: LDFLAGS = $(RYSIM_LDFLAGS)
rysim: $(RYSIM_OBJS) src/RysimMain.o
	$(CXX) $(CXXFLAGS) $(LDFLAGS) $(RYSIM_OBJS) src/RysimMain.o -o rysim

run_tests: tests
	./rysim_tests

tests: gtest rysim_tests

rysim_tests: CPPFLAGS += -isystem $(GTEST_DIR)/include
rysim_tests: CXXFLAGS = $(RYSIM_TESTS_CXXFLAGS) -pthread
rysim_tests: LDFLAGS = $(RYSIM_TESTS_LDFLAGS) -lpthread
rysim_tests: $(RYSIM_TESTS_OBJS)
	$(CXX) gtest_main.a $(CXXFLAGS) $(LDFLAGS) $(RYSIM_TESTS_OBJS) -o rysim_tests

gtest: CPPFLAGS += -isystem $(GTEST_DIR)/include
gtest: CXXFLAGS = $(RYSIM_TESTS_CXXFLAGS)
gtest: LDFLAGS = $(RYSIM_TESTS_LDFLAGS)
gtest:  gtest_main.a

GTEST_HEADERS = $(GTEST_DIR)/include/gtest/*.h \
		$(GTEST_DIR)/include/gtest/internal/*.h
GTEST_SRCS_ = $(GTEST_DIR)/src/*.cc $(GTEST_DIR)/src/*.h $(GTEST_HEADERS)
gtest-all.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
	    $(GTEST_DIR)/src/gtest-all.cc

gtest_main.o : $(GTEST_SRCS_)
	$(CXX) $(CPPFLAGS) -I$(GTEST_DIR) $(CXXFLAGS) -c \
	    $(GTEST_DIR)/src/gtest_main.cc

gtest.a : gtest-all.o
	$(AR) $(ARFLAGS) $@ $^

gtest_main.a : gtest-all.o gtest_main.o
	$(AR) $(ARFLAGS) $@ $^

%.o : %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

%.o : %.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	find . -name "*~" -delete
	find . -name "*.o" -delete
	-rm rysim
	-rm rysim_tests
	-rm gtest_main.a
