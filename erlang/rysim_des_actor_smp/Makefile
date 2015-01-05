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

REBAR:=rebar
APPSRC:=src/rysim_des_actor_smp.app.src
VERSION:=$(shell grep vsn $(APPSRC) | awk -F\" '{print $$2}')

.PHONY: all erl test clean doc

all: erl dist

deps:
	$(REBAR) get-deps

dist: erl
	$(REBAR) escriptize

erl: deps
	$(REBAR) compile

unit_test: erl
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

integration_test: erl
	$(REBAR) skip_deps=true ct

test: unit_test integration_test

clean:
	$(REBAR) clean
	-rm -rvf ebin doc .eunit logs rysim_des_actor_smp.tar
	find test -name "*.beam" -delete
	find . -name "*~" -delete

depclean:
	$(REBAR) delete-deps
	-rm -rvf deps

distclean: clean depclean
	-rm -rvf logs

doc: erl
	$(REBAR) doc

tarball: dist
	tar c rysim_des_actor_smp ebin/ deps/ > rysim_des_actor_smp.tar
