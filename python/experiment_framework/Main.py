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
import gflags
import json
import math
import networkx
import os
import random
import re
import sqlite3
import subprocess
import sys

# Global values
distributions_list = list()
experiment_file = ''
output_console_log = ''
output_results_db = None
agent_states = ['susceptible', 'exposed', 'infectious', 'recovered']

# gflag defn's and registration
FLAGS = gflags.FLAGS
# Input control values
gflags.DEFINE_string('distributions_file', '',
                     'JSON file containing a list of distribution specifications to be used. These should be hand '
                     'checked to produce sane values')
gflags.DEFINE_string('generators_file', '',
                     'JSON file containing a list of generator specifications to be used.')
gflags.DEFINE_string('kernel_file', '',
                     'JSON file that contains a list of kernel paths to be tested. '
                     'File is expected to contain one entry with a member "kernels" '
                     'that contains the list. This is required.')

# Generation control values
gflags.DEFINE_integer('events_start', 5000,
                      'Value to start the events count at, defaults to 2500',
                      1)
gflags.DEFINE_integer('events_end', 50000,
                      'Value to end the events count at, defaults to 50000',
                      1)
gflags.DEFINE_integer('seed', 1,
                      'Seed value for random number generator, defaults to 1')
gflags.DEFINE_boolean('dry_run', False,
                      'Print out what would have been written out to the '
                      'database, but doesn\'t actually write anything out.')

# Execution control values
gflags.DEFINE_integer('max_time', 60000,
                      'Max time to allow the application to run for in mS, '
                      'defaults to 1 minute.')
gflags.DEFINE_integer('max_mem', 0,
                      'Max memory to allow the application to use, in kB, '
                      'defaults to 0, which indicates no limit.')
gflags.DEFINE_integer('hertz', 120,
                      'Rate at which the running application status is sampled. Defaults to 120Hz.')
gflags.DEFINE_integer('iterations', 50,
                      'Number iterations to run of each test. Defaults to 50.')
gflags.DEFINE_integer('worker', 0,
                      'Which worker number is this instance. 0, '
                      'indicates it is the only worker/there is no need to '
                      'fan out the results.')
gflags.DEFINE_integer('num_workers', 1,
                      'Total number of workers. This is used to partition the '
                      'work set and fan out the results. Not setting this '
                      'correctly will not kill your job, but will give you a '
                      'mis-balanced workload.')
gflags.DEFINE_string('machine', 'None',
                     'Machine that experiments are being running on. This defaults '
                     'to "None", which means the analyzer will ignore these results.')


# Flag Validators
def validate_distributions_file(distributions_file):
    if distributions_file == '':
        print "distributions_file not specified"
        return False
    if not os.path.isfile(distributions_file):
        print "distributions_file, '{}', does not exist".format(distributions_file)
        print os.getcwd()
        return False
    if not os.access(distributions_file, os.R_OK):
        print "distributions_file, '{}', is not readable".format(distributions_file)
        return False
    return True


def validate_generators_file(generators_file):
    if generators_file == '':
        print "generators_file not specified"
        return False
    if not os.path.isfile(generators_file):
        print "generators_file, '{}', does not exist".format(generators_file)
        print os.getcwd()
        return False
    if not os.access(generators_file, os.R_OK):
        print "generators_file, '{}', is not readable".format(generators_file)
        return False
    return True


def validate_kernel_file(kernel_file):
    if kernel_file == '':
        print "kernel_file not specified"
        return False
    if not os.path.isfile(kernel_file):
        print "kernel_file, '{}', does not exist".format(kernel_file)
        print os.getcwd()
        return False
    if not os.access(kernel_file, os.R_OK):
        print "kernel_file, '{}', is not readable".format(kernel_file)
        return False
    return True


def validate_kernel(kernel):
    if kernel == '':
        print "kernel not specified"
        return False
    if not os.path.isfile(kernel):
        print "kernel, '{}', does not exist".format(kernel)
        print os.getcwd()
        return False
    if not os.access(kernel, os.X_OK):
        print "kernel_file, '{}', is not executable".format(kernel)
        return False
    return True


gflags.RegisterValidator('distributions_file', validate_distributions_file)
gflags.RegisterValidator('generators_file', validate_generators_file)
gflags.RegisterValidator('kernel_file', validate_kernel_file)
gflags.MarkFlagAsRequired('distributions_file')
gflags.MarkFlagAsRequired('generators_file')
gflags.MarkFlagAsRequired('kernel_file')


def generate_doubling_values(start, end):
    values = list()
    values.append(int(start))
    x = 2 * start
    while x < end:
        values.append(int(x))
        x *= 2
    if start != end:
        values.append(int(end))
    return values


def generate_connection_graph(graph_type, params, count):
    lower_type = graph_type.lower()
    if lower_type == 'complete':
        assert (len(params) == 0)
        return networkx.complete_graph(count)
    elif lower_type == 'complete-bipartite':
        assert (len(params) == 2)
        assert (int(params[0]) > 0.0)
        assert (int(params[1]) > 0.0)
        n1 = int(round(count * float(params[0]) / float(params[1])))
        n2 = int(round(count * float(params[1]) / float(params[0])))
        n1 = 1 if n1 < 1 else n1
        n2 = 1 if n2 < 1 else n2
        return networkx.complete_bipartite_graph(n1, n2)
    elif lower_type == 'circular-ladder':
        assert (len(params) == 0)
        return networkx.circular_ladder_graph(count)
    elif lower_type == 'cycle':
        assert (len(params) == 0)
        return networkx.cycle_graph(count)
    elif lower_type == 'periodic-2grid':
        assert (len(params) == 2)
        assert (int(params[0]) > 0.0)
        assert (int(params[1]) > 0.0)
        width = int(round(math.sqrt(count * float(params[0]) / float(params[1]))))
        height = int(round(math.sqrt(count * float(params[1]) / float(params[0]))))
        width = 1 if width < 1 else width
        height = 1 if height < 1 else height
        return networkx.grid_2d_graph(width, height, True)
    elif lower_type == 'nonperiodic-2grid':
        assert (len(params) == 2)
        assert (int(params[0]) > 0.0)
        assert (int(params[1]) > 0.0)
        width = int(round(math.sqrt(count * float(params[0]) / float(params[1]))))
        height = int(round(math.sqrt(count * float(params[1]) / float(params[0]))))
        width = 1 if width < 1 else width
        height = 1 if height < 1 else height
        return networkx.grid_2d_graph(width, height, False)
    elif lower_type == 'hypercube':
        assert (len(params) == 0)
        return networkx.hypercube_graph(int(round(math.log(count, 2))))
    elif lower_type == 'star':
        assert (len(params) == 0)
        return networkx.star_graph(count - 1)
    elif lower_type == 'wheel':
        assert (len(params) == 0)
        return networkx.wheel_graph(count)
    elif lower_type == 'erdos-reyni':
        assert (len(params) == 1)
        return networkx.erdos_renyi_graph(count, float(params[0]))
    elif lower_type == 'watts-strogatz':
        assert (len(params) == 2)
        if int(params[0]) >= count / 2:
            k = int(count / 2 - 1)
        else:
            k = int(params[0])
        return networkx.connected_watts_strogatz_graph(count, k, int(params[1]))
    else:
        print "Unknown graph type {}".format(lower_type)
        assert False


def create_result_table():
    global output_results_db
    if output_results_db is None:
        pass
    cmd_str = "CREATE TABLE IF NOT EXISTS raw_results (machine text, kernel text, type text, model text, " \
              "iteration long, event_count long, final_time long, cpu long, maxmem long, agents long, " \
              "connections long)"
    output_results_db.execute(cmd_str)


def write_result_entry(result):
    global output_results_db
    if output_results_db is None:
        pass
    cmd_str = "INSERT INTO raw_results " \
              "(machine, kernel, type, model, iteration, event_count, final_time, cpu, maxmem, agents, " \
              "connections) " \
              "VALUES ('{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}')"\
        .format(result.json["machine"], result.json["kernel"], result.json["type"], result.json["model"],
                result.json["iteration"], result.json["event_count"], result.json["final_time"], result.json["cpu"],
                result.json["maxmem"], result.json["agents"], result.json["connections"])
    output_results_db.execute(cmd_str)
    output_results_db.commit()


class ExperimentResults(object):
    """Parses and stores the raw results from an experiment run"""

    def __init__(self, console_log, iteration):
        self.json = dict()
        self.json["machine"] = FLAGS.machine
        self.json["iteration"] = iteration
        self.json["cpu"] = 0
        self.json["maxmem"] = 0
        self.console_log = console_log
        self.parse_console_log()
        print self.json

    # noinspection PyBroadException
    def parse_console_log(self):
        """Parse the raw results and store"""
        parsing_json = False
        json_string = ""
        for line in self.console_log.splitlines():
            match = re.search(r'JSONBEGIN.*', line)
            if not parsing_json and match:
                parsing_json = True
                continue
            match = re.search(r'JSONEND.*', line)
            if parsing_json and match:
                parsing_json = False

                json_string = json_string.replace('\\"', '"')
                json_string = json_string.replace('"{', '{')
                json_string = json_string.replace('}"', '}')

                try:
                    results_json = json.loads(json_string)
                    print results_json
                    for key, value in results_json.items():
                        self.json[key] = value
                except:
                    print "Failed to parse as JSON, {}".format(json_string)
                    sys.exit(1)
                continue
            match = re.search(r'CPU: (.*)', line)
            if match:
                self.json["cpu"] = int(match.group(1))
                continue
            match = re.search(r'MAXMEM: (.*)', line)
            if match:
                self.json["maxmem"] = int(match.group(1))
                continue
            if parsing_json:
                json_string += line
                continue


class ExperimentRunner(object):
    """Takes an experiment and kernel and performs an experimental run."""

    def __init__(self, experiment, kernel):
        self.experiment = experiment
        self.kernel = kernel
        self.results = None
        self.succeeded = False
        self.console_log = ""
        self.results_json = dict()

    def run(self, iteration):
        """Run requested experiment"""
        try:
            print "Trying '{} --experiment_file {}'".format(self.kernel,
                                                            self.experiment)
            if FLAGS.max_mem != 0:
                self.console_log = subprocess.check_output(['../../third_party/perl/timeout.pl',
                                                            '-t', str(FLAGS.max_time),
                                                            '-m', str(FLAGS.max_mem),
                                                            '-x', str(FLAGS.hertz),
                                                            self.kernel,
                                                            '--experiment_file',
                                                            self.experiment,
                                                            '--random_seed',
                                                            str(iteration)])
            else:
                self.console_log = subprocess.check_output(['../../third_party/perl/timeout.pl',
                                                            '-t', str(FLAGS.max_time),
                                                            '-x', str(FLAGS.hertz),
                                                            self.kernel,
                                                            '--experiment_file',
                                                            self.experiment,
                                                            '--random_seed',
                                                            str(iteration)])
            self.succeeded = True
        except subprocess.CalledProcessError as e:
            self.console_log = e.output
            self.succeeded = False

        self.results = ExperimentResults(self.console_log, iteration)
        return self.succeeded

    def print_results(self, iteration):
        """Print result of last run"""
        print "{} {} {}\n".format(os.path.basename(self.kernel),
                                  self.experiment,
                                  str(iteration))
        if self.succeeded:
            print "SUCCESS"
            print json.dumps(self.results.results_json, indent=4, separators=(',', ': '))
        else:
            print "FAILURE"
            print json.dumps(self.results.results_json, indent=4, separators=(',', ': '))
        print "{}\n".format(self.console_log)

    def write_results(self, iteration):
        """Write out the results of the last run to the console log, and results json list"""
        with open(output_console_log, "a") as f:
            f.write("{} {} {}\n".format(os.path.basename(self.kernel),
                                        self.experiment,
                                        str(iteration)))
            f.write("{}\n".format(self.console_log))
            if self.succeeded:
                write_result_entry(self.results)
            else:
                print "Skipping failed in json file"
                print self.console_log


def perform_experiment(experiment, kernel):
    """Execute experiment, print results and write them out """
    runner = ExperimentRunner(experiment,
                              kernel)
    for i in range(1, FLAGS.iterations + 1):
        if FLAGS.worker == 0 or (i % FLAGS.num_workers) + 1 == FLAGS.worker:
            print "Running {} iteration #{}".format(experiment, i)
            if not runner.run(i):
                print "Failed {} on iteration #{}".format(experiment, i)
                return False
            if FLAGS.dry_run:
                runner.print_results(i)
            else:
                runner.write_results(i)
    return True


def generate_model_file(generator, agent_count):
    global distributions_list
    num_distributions = int(round(random.uniform(1, len(distributions_list))))
    distributions = random.sample(distributions_list, num_distributions)

    model_doc = dict()
    model_doc["distributions"] = distributions
    model_doc["model_name"] = generator["base_name"] + "_" + str(agent_count)
    model_doc["agents"] = list()
    graph = generate_connection_graph(generator["generator_type"],
                                      generator["generator_params"],
                                      agent_count)
    total_connections = 0
    global agent_states
    for node in graph.nodes_iter():
        agent = dict()
        agent["label"] = "Agent_" + str(node).replace('(', '').replace(')', '').replace(',', '').replace(' ', '_')
        agent["state"] = random.sample(agent_states, 1)[0]
        agent["s2e"] = random.sample(distributions, 1)[0]["label"]
        agent["e2i"] = random.sample(distributions, 1)[0]["label"]
        agent["i2r"] = random.sample(distributions, 1)[0]["label"]
        if not ("is_looping" in generator) or generator["is_looping"] is False:
            agent["r2s"] = random.sample(distributions, 1)[0]["label"]
        agent["connections"] = list()
        for connection in graph.neighbors_iter(node):
            agent["connections"].append("Agent_" + str(connection)
                                        .replace('(', '').replace(')', '').replace(',', '').replace(' ', '_'))
            total_connections += 1
        model_doc["agents"].append(agent)
    model_doc["total_connections"] = total_connections
    model_filename = os.path.join(".", generator["base_name"] + "_model_" + str(agent_count) + ".json")
    with open(model_filename, 'w') as output_file:
        json.dump(model_doc, output_file, indent=4, separators=(',', ': '))
    print "Emitted model {} for agent_count {}".format(model_filename, str(agent_count))
    return model_filename


def generate_experiment_file(generator, event_limit, agent_count):
    experiment = dict()
    experiment["experiment_name"] = generator["base_name"]
    experiment["event_limit"] = event_limit
    experiment["type"] = generator["generator_type"]
    experiment["model_filename"] = generator["base_name"] + "_model_" + str(agent_count) + ".json"
    experiment_filename = os.path.join(".", generator["base_name"] + "_experiment_" + str(agent_count) + "_"
                                       + str(event_limit) + ".json")
    with open(experiment_filename, 'w') as output_file:
        json.dump(experiment, output_file, indent=4, separators=(',', ': '))
    print "Emitted experiment {} for agent_count {} & event_limit {}".format(experiment_filename, str(agent_count),
                                                                             str(event_limit))
    return experiment_filename


def main(argv):
    try:
        FLAGS(argv)  # parse flags
    except gflags.FlagsError, e:
        print '%s\nUsage: %s ARGS\n%s' % (e, sys.argv[0], FLAGS)
        sys.exit(1)

    if not FLAGS.dry_run:
        print "Clobbering existing output files"
        global output_console_log
        if FLAGS.worker == 0:
            output_console_log = os.path.join('.', 'run_console_{}.log'.format(FLAGS.machine))
        else:
            output_console_log = os.path.join('.', 'run_console_{}_{}.log'.format(FLAGS.machine, str(FLAGS.worker)))

        open(output_console_log, 'w').close()

        global output_results_db
        if FLAGS.worker == 0:
            db_path = os.path.join('.', 'run_results_{}.db'.format(FLAGS.machine))
        else:
            db_path = os.path.join('.', 'run_results_{}_{}.db'.format(FLAGS.machine, str(FLAGS.worker)))
        try:
            os.unlink(db_path)
        except OSError, e:
            print "Unable able to unlink {} due to {}".format(db_path, e)
            pass
        output_results_db = sqlite3.connect(db_path)
        create_result_table()

    with open(FLAGS.kernel_file, "r") as f:
        kernel_json = json.load(f)

    kernels = list()
    for kernel in kernel_json["kernels"]:
        if validate_kernel(kernel):
            kernels.append(kernel)

    with open(FLAGS.distributions_file, 'r') as f:
        distributions_json = json.load(f)
    global distributions_list
    distributions_list = distributions_json["distributions"]

    with open(FLAGS.generators_file, 'r') as f:
        generators_json = json.load(f)

    for generator in generators_json["generators"]:
        for agent_count in generate_doubling_values(generator["agents_start"], generator["agents_end"]):
            model_filename = generate_model_file(generator, agent_count)
            experiment_filenames = list()
            for event_limit in generate_doubling_values(FLAGS.events_start, FLAGS.events_end):
                experiment_filenames.append(generate_experiment_file(generator, event_limit, agent_count))
            for kernel in kernels:
                for experiment_filename in experiment_filenames:
                    if not perform_experiment(experiment_filename, kernel):
                        print "Failed run for {} on {}, skipping remaining for model".format(kernel,
                                                                                             experiment_filename)
                        break
            for experiment_filename in experiment_filenames:
                try:
                    os.unlink(experiment_filename)
                    print "Unlinked experiment {}".format(experiment_filename)
                except OSError, e:
                    print "Unable able to unlink {} due to {}".format(experiment_filename, e)
            try:
                os.unlink(model_filename)
                print "Unlinked model {}".format(model_filename)
            except OSError, e:
                print "Unable able to unlink {} due to {}".format(model_filename, e)

    if output_results_db is not None:
        output_results_db.commit()
        output_results_db.close()

if __name__ == '__main__':
    main(sys.argv)
