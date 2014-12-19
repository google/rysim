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
from abc import ABCMeta, abstractmethod
from array import *
import collections
import gflags
import numpy
import os
import pprint
import re
import scipy.integrate
import scipy.interpolate
import sqlite3
import sys
from matplotlib import pylab
import pandas as pd
import statsmodels.formula.api as sm
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as pyplot

# Global state
experiment_db = None
event_count_buckets = [5000, 10000, 20000, 40000, 50000]
bucketing_factor = 0.001
kernel_results_table = None
kernel_machine_results_table = None
kernel_machine_type_results_table = None
fit_comparison_table = dict()

# gflag defn's and registration
FLAGS = gflags.FLAGS
gflags.DEFINE_string('root_dir', '.',
                     'Root directory to start searching and where to store the database. Defaults to the current '
                     'directory')
gflags.DEFINE_string('output_db', 'experiment.db',
                     'Name of the database file that should be created. If the file already exists it will be '
                     'overwritten. Defaults to "experiment.db"')
gflags.DEFINE_bool('read_inputs', False,
                   'Controls if the application should re-read the inputs. If so the output DB will be clobbered '
                   'entirely. If not only the analysis tables will be removed')


class DBWrapper(object):
    def __init__(self, db_filename):
        self.db = sqlite3.connect(db_filename, check_same_thread=False)

    def commit(self):
        self.db.commit()

    def execute_safe(self, cmd):
        self.execute(cmd)
        self.commit()

    def execute(self, cmd):
        self.db.execute(cmd)

    def select(self, cmd):
        return self.db.execute(cmd)

    def cleanup(self):
        self.db.commit()
        self.db.close()
        self.db = None


class ResultsTable(object):
    filtered_table_entry = collections.namedtuple('FilteredTableEntry', ['event_count', 'event_count_std',
                                                                         'agents', 'agents_std',
                                                                         'connections', 'connections_std',
                                                                         'cpu', 'cpu_std',
                                                                         'maxmem', 'maxmem_std'])
    filtered_entry = collections.namedtuple('FilteredEntry', ['mean', 'std'])

    def __init__(self):
        self.raw_table = dict()
        self.filtered_table = dict()

    def get_keys(self):
        return self.filtered_table.keys()

    def add_entry(self, key, bucket, model, event_count, agents, connections, cpu, maxmem):
        if key not in self.raw_table.keys():
            self.raw_table[key] = dict()
        if model not in self.raw_table[key].keys():
            self.raw_table[key][model] = dict()
        if agents not in self.raw_table[key][model].keys():
            self.raw_table[key][model][agents] = dict()
        if connections not in self.raw_table[key][model][agents].keys():
            self.raw_table[key][model][agents][connections] = dict()
        if bucket not in self.raw_table[key][model][agents][connections].keys():
            self.raw_table[key][model][agents][connections][bucket] = dict()
            self.raw_table[key][model][agents][connections][bucket]["cpu"] = list()
            self.raw_table[key][model][agents][connections][bucket]["maxmem"] = list()
            self.raw_table[key][model][agents][connections][bucket]["event_count"] = list()

        self.raw_table[key][model][agents][connections][bucket]["cpu"].append(cpu)
        self.raw_table[key][model][agents][connections][bucket]["maxmem"].append(maxmem)
        self.raw_table[key][model][agents][connections][bucket]["event_count"].append(event_count)

    def create_filtered_table(self):
        self.filtered_table = dict()
        for key in self.raw_table.keys():
            self.filtered_table[key] = list()
            for model in self.raw_table[key].keys():
                for agents in self.raw_table[key][model].keys():
                    for connections in self.raw_table[key][model][agents].keys():
                        for bucket in self.raw_table[key][model][agents][connections].keys():
                            if len(self.raw_table[key][model][agents][connections][bucket]["event_count"]) is 0:
                                continue
                            event_count = ResultsTable.filter_bucket_entry(
                                self.raw_table[key][model][agents][connections][bucket]["event_count"])
                            cpu = ResultsTable.filter_bucket_entry(
                                self.raw_table[key][model][agents][connections][bucket]["cpu"])
                            maxmem = ResultsTable.filter_bucket_entry(
                                self.raw_table[key][model][agents][connections][bucket]["maxmem"])
                            self.filtered_table[key].append(ResultsTable.filtered_table_entry(
                                event_count=event_count.mean, event_count_std=event_count.std,
                                agents=agents, agents_std=0,
                                connections=connections, connections_std=0,
                                cpu=cpu.mean, cpu_std=cpu.std,
                                maxmem=maxmem.mean, maxmem_std=maxmem.std))

    @staticmethod
    def filter_bucket_entry(entry):
        return ResultsTable.filtered_entry(mean=numpy.mean(entry), std=numpy.std(entry))

    def get_entries_for_key(self, key):
        return self.filtered_table[key]

    def get_event_count_lists_for_key(self, key):
        key_data = self.get_entries_for_key(key)
        return ResultsTable.filtered_entry(mean=[row[0] for row in key_data], std=[row[1] for row in key_data])

    def get_agents_lists_for_key(self, key):
        key_data = self.get_entries_for_key(key)
        return ResultsTable.filtered_entry(mean=[row[2] for row in key_data], std=[row[3] for row in key_data])

    def get_connections_lists_for_key(self, key):
        key_data = self.get_entries_for_key(key)
        return ResultsTable.filtered_entry(mean=[row[4] for row in key_data], std=[row[5] for row in key_data])

    def get_cpu_lists_for_key(self, key):
        key_data = self.get_entries_for_key(key)
        return ResultsTable.filtered_entry(mean=[row[6] for row in key_data], std=[row[7] for row in key_data])

    def get_maxmem_lists_for_key(self, key):
        key_data = self.get_entries_for_key(key)
        return ResultsTable.filtered_entry(mean=[row[8] for row in key_data], std=[row[9] for row in key_data])


class ScoreTable(object):
    def __init__(self, kernels, tag):
        global fit_comparison_table
        fit_comparison_table[tag] = 0.0
        self.tag = tag
        self.r2_values = list()
        self.kernels = kernels
        self.table = dict()
        self.total_count = 0
        self.total_score_idx = len(kernels)
        for kernel in kernels:
            self.table[kernel] = array('I', [0] * (1 + len(kernels)))

    def get_table(self):
        return self.table

    def get_total_count(self):
        return self.total_count

    def add_1d_fit_score(self, fits):
        self.total_count += 1
        f_list = list()
        for kernel in fits.keys():
            slope = fits[kernel][0][0]
            intercept = fits[kernel][1]
            self.r2_values.append(float(fits[kernel][2]))
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])

            f_list.append((scipy.integrate.quad(lambda x: slope * x + intercept,
                                                x_min, x_max)[0],
                           kernel[0]))
        f_list.sort()
        for i in range(0, len(f_list)):
            self.table[f_list[i][1]][i] += 1
            self.table[f_list[i][1]][self.total_score_idx] += len(f_list) - i
        global fit_comparison_table
        fit_comparison_table[self.tag] = numpy.mean(self.r2_values)

    def add_2d_fit_score(self, fits):
        self.total_count += 1
        f_list = list()
        for kernel in fits.keys():
            slope_x = fits[kernel][0][0]
            slope_y = fits[kernel][0][1]
            intercept = fits[kernel][1]
            self.r2_values.append(float(fits[kernel][2]))
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])
            y_min = float(fits[kernel][3][1])
            y_max = float(fits[kernel][4][1])

            f_list.append((scipy.integrate.dblquad(lambda x, y: slope_x * x + slope_y * y + intercept,
                                                   x_min, x_max,
                                                   lambda x: y_min, lambda x: y_max)[0],
                           kernel[0]))
        f_list.sort()
        for i in range(0, len(f_list)):
            self.table[f_list[i][1]][i] += 1
            self.table[f_list[i][1]][self.total_score_idx] += len(f_list) - i
        global fit_comparison_table
        fit_comparison_table[self.tag] = numpy.mean(self.r2_values)

    def add_3d_fit_score(self, fits):
        self.total_count += 1
        f_list = list()
        for kernel in fits.keys():
            slope_x = fits[kernel][0][0]
            slope_y = fits[kernel][0][1]
            slope_z = fits[kernel][0][2]
            intercept = fits[kernel][1]
            self.r2_values.append(float(fits[kernel][2]))
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])
            y_min = float(fits[kernel][3][1])
            y_max = float(fits[kernel][4][1])
            z_min = float(fits[kernel][3][2])
            z_max = float(fits[kernel][4][2])

            f_list.append((scipy.integrate.tplquad(lambda x, y, z: slope_x * x + slope_y * y + slope_z * z + intercept,
                                                   x_min, x_max,
                                                   lambda x: y_min, lambda x: y_max,
                                                   lambda x, y: z_min, lambda x, y: z_max)[0],
                           kernel[0]))
        f_list.sort()
        for i in range(0, len(f_list)):
            self.table[f_list[i][1]][i] += 1
            self.table[f_list[i][1]][self.total_score_idx] += len(f_list) - i
        global fit_comparison_table
        fit_comparison_table[self.tag] = numpy.mean(self.r2_values)


class MachineComparisonTable(object):
    machine_core_counts = {'m3.large': 2,
                           'm3.2xlarge': 8,
                           'm3.medium': 1,
                           'm3.xlarge': 4}

    def __init__(self, kernels):
        self.kernels = kernels
        self.table = dict()

    def add_1d_fit_score(self, fits, machine):
        machine_entry = self.get_machine_entry(machine)
        for kernel in fits.keys():
            slope = fits[kernel][0][0]
            intercept = fits[kernel][1]
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])

            machine_entry[kernel[0]].append(scipy.integrate.quad(lambda x: slope * x + intercept, x_min, x_max)[0])

    def add_2d_fit_score(self, fits, machine):
        machine_entry = self.get_machine_entry(machine)
        for kernel in fits.keys():
            slope_x = fits[kernel][0][0]
            slope_y = fits[kernel][0][1]
            intercept = fits[kernel][1]
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])
            y_min = float(fits[kernel][3][1])
            y_max = float(fits[kernel][4][1])

            machine_entry[kernel[0]].append(scipy.integrate.dblquad(lambda x, y: slope_x * x + slope_y * y + intercept,
                                                                    x_min, x_max,
                                                                    lambda x: y_min, lambda x: y_max)[0])

    def add_3d_fit_score(self, fits, machine):
        machine_entry = self.get_machine_entry(machine)
        for kernel in fits.keys():
            slope_x = fits[kernel][0][0]
            slope_y = fits[kernel][0][1]
            slope_z = fits[kernel][0][2]
            intercept = fits[kernel][1]
            x_min = float(fits[kernel][3][0])
            x_max = float(fits[kernel][4][0])
            y_min = float(fits[kernel][3][1])
            y_max = float(fits[kernel][4][1])
            z_min = float(fits[kernel][3][2])
            z_max = float(fits[kernel][4][2])

            machine_entry[kernel[0]].append(scipy.integrate.tplquad(
                lambda x, y, z: slope_x * x + slope_y * y + slope_z * z + intercept,
                x_min, x_max,
                lambda x: y_min, lambda x: y_max,
                lambda x, y: z_min, lambda x, y: z_max)[0])

    def get_machine_entry(self, machine):
        if machine in self.table:
            return self.table[machine]
        else:
            self.table[machine] = dict()
            for kernel in self.kernels:
                self.table[machine][kernel] = list()
            return self.table[machine]

    def generate_artifacts(self, key_label_caption, key_label_filename, independent_caption, independent_filename,
                           dependent_caption, dependent_filename):
        per_kernel_data = dict()
        for kernel in self.kernels:
            per_kernel_data[kernel] = dict()

        for machine in self.table.keys():
            for kernel in self.kernels:
                per_kernel_data[kernel][MachineComparisonTable.machine_core_counts[machine]] = \
                    numpy.mean(self.table[machine][kernel])
        per_kernel_splines = dict()
        for kernel in self.kernels:
            data_list = list()
            per_kernel_splines[kernel] = dict()
            for cores, value in per_kernel_data[kernel].iteritems():
                data_list.append((cores, value))
            data_list.sort()
            x_list = list()
            y_list = list()
            for entry in data_list:
                x_list.append(entry[0])
                y_list.append(entry[1])
            x_data = numpy.array(x_list)
            y_data = numpy.array(y_list)
            x_new = numpy.linspace(x_data.min(), x_data.max(), 300)
            y_new = scipy.interpolate.spline(x_data, y_data, x_new)
            per_kernel_splines[kernel]['x_data'] = x_data
            per_kernel_splines[kernel]['y_data'] = y_data
            per_kernel_splines[kernel]['x_new'] = x_new
            per_kernel_splines[kernel]['y_new'] = y_new
            GenericArtifacts.set_figure_params()
            filename_base = "machine_comparison_{}_vs_{}_{}_{}".format(independent_filename, dependent_filename,
                                                                       str(kernel).lower(), key_label_filename)
            plot_filename = os.path.join(FLAGS.root_dir, "{}_plot.eps".format(filename_base))

            print "\tGenerating {}".format(plot_filename)
            pylab.figure(1)
            pylab.clf()

            pylab.plot(x_data, y_data, linestyle='-', color='k')
            pylab.scatter(x_data, y_data, marker='s', color='k', label=kernel)

            pylab.autoscale()
            pylab.xlabel("Number of Cores")
            pylab.ylabel(dependent_caption)
            pylab.legend()
            pylab.savefig(plot_filename, bbox_inches='tight', orientation='landscape')

            caption = "Plot of Machine Comparison of {} for {} vs {}".format(kernel, independent_caption,
                                                                             dependent_caption)
            tex_filename = os.path.join(FLAGS.root_dir, "{}_plot.tex".format(filename_base))
            print "\tGenerating {}".format(tex_filename)
            tex_figure_path = os.path.join("figures", filename_base)
            output_latex = r"""\begin{figure}
\centering
"""
            output_latex += "\\includegraphics[angle=180,origin=c]{%s}\n" % tex_figure_path
            output_latex += "\\caption{%s}\n" % caption
            output_latex += "\\label{fig:%s}\n" % filename_base
            output_latex += r"""\end{figure}"""
            with open(tex_filename, 'w') as f:
                f.write(output_latex)

            tex_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
            print "\tGenerating {}".format(tex_filename)
            output_latex = r"""\begin{table}[h]
"""

            output_latex += r"""\begin{tabular}{|c|c|}
\hline
"""
            output_latex += r"""Cores & Score \\
\hline
"""
            for entry in data_list:
                cores = entry[0]
                score = entry[1]
                output_latex += "%d & %.4e \\\\ \n" % (cores, score)
            output_latex += r"""\hline
\end{tabular}
"""

            output_latex += "\\caption{Machine Comparison of %s for %s vs %s in %s}\n" % (kernel, independent_caption,
                                                                                          dependent_caption,
                                                                                          key_label_caption)
            output_latex += "\\label{tab:%s}\n" % filename_base
            output_latex += r"""\end{table}"""

            with open(tex_filename, 'w') as f:
                f.write(output_latex)

        filename_base = "machine_comparison_{}_vs_{}_{}".format(independent_filename, dependent_filename,
                                                                key_label_filename)
        plot_filename = os.path.join(FLAGS.root_dir, "{}_plot.eps".format(filename_base))

        print "\tGenerating {}".format(plot_filename)
        pylab.figure(1)
        pylab.clf()

        markers = ['v', '^', 's', 'D', 'x', '*', 'h']
        markers_count = 0
        for kernel in self.kernels:
            x_new = per_kernel_splines[kernel]['x_new']
            y_new = per_kernel_splines[kernel]['y_new']
            x_data = per_kernel_splines[kernel]['x_data']
            y_data = per_kernel_splines[kernel]['y_data']

            pylab.plot(x_data, y_data, linestyle='-', color='k')
            pylab.scatter(x_data, y_data, marker=markers[markers_count], color='k', label=kernel)
            markers_count += 1

        pylab.autoscale()
        pylab.xlabel("Number of Cores")
        pylab.ylabel(dependent_caption)
        pylab.legend()
        pylab.savefig(plot_filename, bbox_inches='tight', orientation='landscape')

        caption = "Multiline Plot of Machine Comparison for {} vs {}".format(independent_caption, dependent_caption)
        tex_filename = os.path.join(FLAGS.root_dir, "{}_plot.tex".format(filename_base))
        print "\tGenerating {}".format(tex_filename)
        tex_figure_path = os.path.join("figures", filename_base)
        output_latex = r"""\begin{figure}
\centering
"""
        output_latex += "\\includegraphics[angle=180,origin=c]{%s}\n" % tex_figure_path
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{fig:%s}\n" % filename_base
        output_latex += r"""\end{figure}"""
        with open(tex_filename, 'w') as f:
            f.write(output_latex)

class GenericArtifacts:
    __metaclass__ = ABCMeta
    linear_regression = collections.namedtuple('LinearRegression', ['slope', 'intercept', 'r_squared', 'min', 'max'])

    def __init__(self, results_table, key_label_tuple):
        self.results_table = results_table
        self.key_label_tuple = key_label_tuple
        self.keys = self.results_table.get_keys()
        self.sub_key_label_tuple = None if len(self.key_label_tuple) is 1 else self.key_label_tuple[1:]
        self.sub_keys = None if not self.sub_key_label_tuple else set()
        self.kernels = set()

        self.cpu_ranges = dict()
        self.maxmem_ranges = dict()
        self.event_count_ranges = dict()
        self.agents_ranges = dict()
        self.connections_ranges = dict()

        self.event_count_vs_cpu_fits = dict()
        self.event_count_vs_maxmem_fits = dict()
        self.agents_vs_cpu_fits = dict()
        self.agents_vs_maxmem_fits = dict()
        self.connections_vs_cpu_fits = dict()
        self.connections_vs_maxmem_fits = dict()

        self.event_count_and_agents_vs_cpu_fits = dict()
        self.event_count_and_agents_vs_maxmem_fits = dict()
        self.event_count_and_connections_vs_cpu_fits = dict()
        self.event_count_and_connections_vs_maxmem_fits = dict()
        self.agents_and_connections_vs_cpu_fits = dict()
        self.agents_and_connections_vs_maxmem_fits = dict()

        self.event_count_and_agents_and_connections_vs_cpu_fits = dict()
        self.event_count_and_agents_and_connections_vs_maxmem_fits = dict()

        for key in self.keys:
            self.calculate_fits_for_key(key)
            self.kernels.add(key[0])
            if self.sub_keys is not None:
                self.sub_keys.add(key[1:])

    def calculate_fits_for_key(self, key):
        self.cpu_ranges[key] = self.results_table.get_cpu_lists_for_key(key).mean
        self.maxmem_ranges[key] = self.results_table.get_maxmem_lists_for_key(key).mean
        self.event_count_ranges[key] = self.results_table.get_event_count_lists_for_key(key).mean
        self.agents_ranges[key] = self.results_table.get_event_count_lists_for_key(key).mean
        self.connections_ranges[key] = self.results_table.get_event_count_lists_for_key(key).mean

        self.event_count_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.event_count_ranges[key], self.cpu_ranges[key])
        self.event_count_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.event_count_ranges[key], self.maxmem_ranges[key])
        self.agents_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.agents_ranges[key], self.cpu_ranges[key])
        self.agents_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.agents_ranges[key], self.maxmem_ranges[key])
        self.connections_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.connections_ranges[key], self.cpu_ranges[key])
        self.connections_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_1d(
            self.connections_ranges[key], self.maxmem_ranges[key])

        self.event_count_and_agents_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.event_count_ranges[key], self.agents_ranges[key], self.cpu_ranges[key])
        self.event_count_and_agents_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.event_count_ranges[key], self.agents_ranges[key], self.maxmem_ranges[key])
        self.event_count_and_connections_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.event_count_ranges[key], self.connections_ranges[key], self.cpu_ranges[key])
        self.event_count_and_connections_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.event_count_ranges[key], self.connections_ranges[key], self.maxmem_ranges[key])
        self.agents_and_connections_vs_cpu_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.agents_ranges[key], self.connections_ranges[key], self.cpu_ranges[key])
        self.agents_and_connections_vs_maxmem_fits[key] = GenericArtifacts.calculate_linear_regression_2d(
            self.agents_ranges[key], self.connections_ranges[key], self.maxmem_ranges[key])

        self.event_count_and_agents_and_connections_vs_cpu_fits[key] = \
            GenericArtifacts.calculate_linear_regression_3d(
                self.event_count_ranges[key], self.agents_ranges[key], self.connections_ranges[key],
                self.cpu_ranges[key])
        self.event_count_and_agents_and_connections_vs_maxmem_fits[key] = \
            GenericArtifacts.calculate_linear_regression_3d(
                self.event_count_ranges[key], self.agents_ranges[key], self.connections_ranges[key],
                self.maxmem_ranges[key])

    def filter_dict_for_sub_key(self, raw_dict, sub_key):
        return_dict = dict()
        for kernel in iter(self.kernels):
            return_dict[(kernel,)] = raw_dict[(kernel,) + sub_key]
        return return_dict

    @staticmethod
    def key_tuple_to_caption_string(key_tuple, capitialize=False):
        return_string = ""
        for entry in key_tuple:
            if not capitialize:
                return_string += "{} and ".format(entry)
            else:
                return_string += "{} and ".format(str(entry).capitalize())
        return return_string[:-5]

    @staticmethod
    def key_tuple_to_filename_string(key_tuple, lowercase=False):
        return_string = ""
        for entry in key_tuple:
            if not lowercase:
                return_string += "{}_".format(entry)
            else:
                return_string += "{}_".format(str(entry).lower())
        return return_string[:-1]

    @abstractmethod
    def generate_multiline_plots(self):
        pass

    @abstractmethod
    def generate_fit_tables(self):
        pass

    @abstractmethod
    def generate_score_tables(self):
        pass

    @abstractmethod
    def generate_machine_comparison_tables(self):
        pass

    @staticmethod
    def set_figure_params():
        fig_width = 9.5  # width in inches, this will be the height on the page due to rotation
        fig_height = 7  # height in inches, this will be the width on the page due to rotation
        fig_size = [fig_width, fig_height]
        fig_params = {'backend': 'ps',
                      'axes.labelsize': 8,
                      'text.fontsize': 8,
                      'legend.fontsize': 8,
                      'xtick.labelsize': 6,
                      'ytick.labelsize': 6,
                      'text.usetex': True,
                      'figure.figsize': fig_size}
        pylab.rcParams.update(fig_params)

    @staticmethod
    def calculate_linear_regression_1d(x_list, f_list):
        results = sm.ols(formula="F ~ X", data=({'F': f_list, 'X': x_list})).fit()
        slope = list()
        slope.append(results.params['X'])
        min_value = list()
        min_value.append(min(x_list))
        max_value = list()
        max_value.append(max(x_list))
        intercept = results.params['Intercept']
        r_squared = results.rsquared
        return GenericArtifacts.linear_regression(slope=slope, intercept=intercept, r_squared=r_squared,
                                                  min=min_value, max=max_value)

    @staticmethod
    def calculate_linear_regression_2d(x_list, y_list, f_list):
        results = sm.ols(formula="F ~ X + Y", data=({'F': f_list, 'X': x_list, 'Y': y_list})).fit()
        slope = list()
        slope.append(results.params['X'])
        slope.append(results.params['Y'])
        min_value = list()
        min_value.append(min(x_list))
        min_value.append(min(y_list))
        max_value = list()
        max_value.append(max(x_list))
        max_value.append(max(y_list))
        intercept = results.params['Intercept']
        r_squared = results.rsquared
        return GenericArtifacts.linear_regression(slope=slope, intercept=intercept, r_squared=r_squared,
                                                  min=min_value, max=max_value)

    @staticmethod
    def calculate_linear_regression_3d(x_list, y_list, z_list, f_list):
        results = sm.ols(formula="F ~ X + Y + Z", data=({'F': f_list, 'X': x_list, 'Y': y_list, 'Z': z_list})).fit()
        slope = list()
        slope.append(results.params['X'])
        slope.append(results.params['Y'])
        slope.append(results.params['Z'])
        min_value = list()
        min_value.append(min(x_list))
        min_value.append(min(y_list))
        min_value.append(min(z_list))
        max_value = list()
        max_value.append(max(x_list))
        max_value.append(max(y_list))
        max_value.append(max(z_list))
        intercept = results.params['Intercept']
        r_squared = results.rsquared

        return GenericArtifacts.linear_regression(slope=slope, intercept=intercept, r_squared=r_squared,
                                                  min=min_value, max=max_value)

    @staticmethod
    def generate_1d_multiline_plot(fits, x_ranges, x_label, f_label, caption, filename_base):
        markers = ['v', '^', 's', 'D', 'x', '*', 'h']
        GenericArtifacts.set_figure_params()
        filename_base = filename_base.replace('.', '_')
        plot_filename = os.path.join(FLAGS.root_dir, "{}.eps".format(filename_base))

        print "\tGenerating {}".format(plot_filename)
        pylab.figure(1)
        pylab.clf()

        marker_count = 0
        for kernel in fits.keys():
            x_list = [0]
            x_max = max(x_ranges[kernel])
            x_list.append(x_max / 2)
            x_list.append(x_max)
            f_fit = lambda x: x * fits[kernel][0][0] + fits[kernel][1]
            y_list = [f_fit(entry) for entry in x_list]

            pylab.plot(x_list, y_list, marker=markers[marker_count], linestyle='-', color='k', label=kernel[0])
            marker_count += 1

        pylab.autoscale()
        pylab.xlabel(x_label)
        pylab.ylabel(f_label)
        pylab.legend(bbox_to_anchor=(0., 1.02, 1., 0.102), loc=0, ncol=3, mode="expand", borderaxespad=0.)
        pylab.savefig(plot_filename, bbox_inches='tight', orientation='landscape')

        tex_filename = os.path.join(FLAGS.root_dir, "{}.tex".format(filename_base))
        print "\tGenerating {}".format(tex_filename)
        tex_figure_path = os.path.join("figures", filename_base)
        output_latex = r"""\begin{figure}
\centering
"""
        output_latex += "\\includegraphics[angle=180,origin=c]{%s}\n" % tex_figure_path
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{fig:%s}\n" % filename_base
        output_latex += r"""\end{figure}"""
        with open(tex_filename, 'w') as f:
            f.write(output_latex)

    @staticmethod
    def generate_1d_fit_table(key_labels, fits, caption, filename_base):
        filename_base = filename_base.replace('.', '_')
        table_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
        print "\tGenerating {}".format(table_filename)

        output_latex = r"""\begin{table}[h]
"""
        output_latex += "\\begin{tabular}{|"
        for _ in key_labels:
            output_latex += "l|"
        output_latex += "|c|c|c|}\n"
        output_latex += "\\hline\n"
        for label in key_labels:
            output_latex += "{} & ".format(label)
        output_latex += "Slope & Intercept & $R^2$ \\\\\n\\hline\n"

        for key in fits.keys():
            for entry in key:
                output_latex += "%s & " % entry
            output_latex += " %.4g & %.4g & %.4g \\\\\n" % (fits[key][0][0], fits[key][1], fits[key][2])
        output_latex += r"""\hline
\end{tabular}
"""
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{tab:%s}\n" % filename_base
        output_latex += r"""\end{table}"""

        with open(table_filename, 'w') as f:
            f.write(output_latex)

    @staticmethod
    def generate_2d_fit_table(key_labels, fits, x_label, y_label, caption, filename_base):
        filename_base = filename_base.replace('.', '_')
        table_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
        print "\tGenerating {}".format(table_filename)

        output_latex = r"""\begin{table}[h]
"""
        output_latex += "\\begin{tabular}{|"
        for _ in key_labels:
            output_latex += "l|"
        output_latex += "|c|c|c|c|}\n"
        output_latex += "\\hline\n"
        for label in key_labels:
            output_latex += "{} & ".format(label)
        output_latex += "{} Slope & {} Slope & Intercept & $R^2$ \\\\\n\\hline\n".format(x_label, y_label)

        for key in fits.keys():
            for entry in key:
                output_latex += "%s & " % entry
            output_latex += "%.4g & %.4g & %.4g & %.4g \\\\\n" % (fits[key][0][0], fits[key][0][1], fits[key][1],
                                                                  fits[key][2])
        output_latex += r"""\hline
\end{tabular}
"""
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{tab:%s}\n" % filename_base
        output_latex += r"""\end{table}"""

        with open(table_filename, 'w') as f:
            f.write(output_latex)

    @staticmethod
    def generate_3d_fit_table(key_labels, fits, x_label, y_label, z_label, caption, filename_base):
        filename_base = filename_base.replace('.', '_')
        table_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
        print "\tGenerating {}".format(table_filename)

        output_latex = r"""\begin{table}[h]
"""
        output_latex += "\\begin{tabular}{|"
        for _ in key_labels:
            output_latex += "l|"
        output_latex += "|c|c|c|c|c|}\n"
        output_latex += "\\hline\n"
        for label in key_labels:
            output_latex += "{} & ".format(label)
        output_latex += "{} Slope & {} Slope & {} Slope & Intercept & $R^2$ \\\\\n\\hline\n".format(x_label, y_label,
                                                                                                    z_label)

        for key in fits.keys():
            for entry in key:
                output_latex += "%s & " % entry
            output_latex += "%.4g & %.4g & %.4g & %.4g & %.4g \\\\\n" % (fits[key][0][0], fits[key][0][1],
                                                                         fits[key][0][2], fits[key][1], fits[key][2])
        output_latex += r"""\hline
\end{tabular}
"""
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{tab:%s}\n" % filename_base
        output_latex += r"""\end{table}"""

        with open(table_filename, 'w') as f:
            f.write(output_latex)

    @staticmethod
    def generate_score_table(score_table, caption, filename_base):
        ordinal_ranks = ["1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"]
        filename_base = filename_base.replace('.', '_')
        table_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
        print "\tGenerating {}".format(table_filename)

        output_latex = r"""\begin{table}[h]
"""
        output_latex += "\\begin{tabular}{|l|"
        for _ in score_table.get_table().keys():
            output_latex += "|c"
        output_latex += "||c"
        output_latex += "|}\\hline\n"
        output_latex += "Kernel "
        for i in range(0, len(score_table.get_table().keys())):
            output_latex += "& %s " % ordinal_ranks[i]
        output_latex += "& Total Score "
        output_latex += "\\\\\n"

        output_latex += r"""\hline
"""
        total_count = score_table.get_total_count()
        assert(total_count > 0)

        for kernel in score_table.get_table().keys():
            output_latex += "%s " % kernel
            for entry in score_table.get_table()[kernel]:
                if entry > 0:
                    output_latex += "& %d " % entry
                else:
                    output_latex += "& \\textemdash "
            output_latex += "\\\\\n"

        output_latex += r"""\hline
\end{tabular}
"""
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{tab:%s}\n" % filename_base
        output_latex += r"""\end{table}"""

        with open(table_filename, 'w') as f:
            f.write(output_latex)

    @staticmethod
    def generate_score_percentage_table(score_table, caption, filename_base):
        ordinal_ranks = ["1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th"]
        filename_base = filename_base.replace('.', '_')
        table_filename = os.path.join(FLAGS.root_dir, "{}_table.tex".format(filename_base))
        print "\tGenerating {}".format(table_filename)

        output_latex = r"""\begin{table}[h]
"""
        output_latex += "\\begin{tabular}{|l|"
        for _ in score_table.get_table().keys():
            output_latex += "|c"
        output_latex += "|}\\hline\n"
        output_latex += "Kernel "
        for i in range(0, len(score_table.get_table().keys())):
            output_latex += "& %s " % ordinal_ranks[i]
        output_latex += "\\\\\n"

        output_latex += r"""\hline
"""

        total_count = float(score_table.get_total_count())
        assert(total_count > 0.0)
        for kernel in score_table.get_table().keys():
            output_latex += "%s " % kernel
            for i in range(0, len(score_table.get_table().keys())):
                entry = score_table.get_table()[kernel][i]
                if entry > 0:
                    output_latex += "& %5.4f " % (float(entry) / total_count)
                else:
                    output_latex += "& \\textemdash "
            output_latex += "\\\\\n"

        output_latex += r"""\hline
\end{tabular}
"""
        output_latex += "\\caption{%s}\n" % caption
        output_latex += "\\label{tab:%s}\n" % filename_base
        output_latex += r"""\end{table}"""

        with open(table_filename, 'w') as f:
            f.write(output_latex)


class KernelArtifacts(GenericArtifacts):
    def __init__(self, results_table):
        super(KernelArtifacts, self).__init__(results_table, ("Kernel",))

    def generate_multiline_plots(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        GenericArtifacts.generate_1d_multiline_plot(self.event_count_vs_cpu_fits, self.event_count_ranges,
                                                    "Event Count", "CPU Time (mS)",
                                                    "Trend lines for Event Count vs CPU Time for per {} fits".format(
                                                        key_label_caption),
                                                    "event_count_vs_cpu_per_{}_multiline_plot".format(
                                                        key_label_filename))
        GenericArtifacts.generate_1d_multiline_plot(self.event_count_vs_maxmem_fits, self.event_count_ranges,
                                                    "Event Count", "Max Memory (kB)",
                                                    "Trend lines for Event Count vs Max Memory for per {} fits".format(
                                                        key_label_caption),
                                                    "event_count_vs_maxmem_per_{}_multiline_plot".format(
                                                        key_label_filename))
        GenericArtifacts.generate_1d_multiline_plot(self.agents_vs_cpu_fits, self.agents_ranges,
                                                    "Agents", "CPU Time (mS)",
                                                    "Trend lines for Agents vs CPU Time for per {} fits".format(
                                                        key_label_caption),
                                                    "agents_vs_cpu_per_{}_multiline_plot".format(key_label_filename))
        GenericArtifacts.generate_1d_multiline_plot(self.agents_vs_maxmem_fits, self.agents_ranges,
                                                    "Agents", "Max Memory (kB)",
                                                    "Trend lines for Agents vs Max Memory for per {} fits".format(
                                                        key_label_caption),
                                                    "agents_vs_maxmem_per_{}_multiline_plot".format(key_label_filename))
        GenericArtifacts.generate_1d_multiline_plot(self.connections_vs_cpu_fits, self.connections_ranges,
                                                    "Connections", "CPU Time (mS)",
                                                    "Trend lines for Connections vs CPU Time for per {} fits".format(
                                                        key_label_caption),
                                                    "connections_vs_cpu_per_{}_multiline_plot".format(
                                                        key_label_filename))
        GenericArtifacts.generate_1d_multiline_plot(self.connections_vs_maxmem_fits, self.connections_ranges,
                                                    "Connections", "Max Memory (kB)",
                                                    "Trend lines for Connections vs Max Memory for per {} fits".format(
                                                        key_label_caption),
                                                    "connections_vs_maxmem_per_{}_multiline_plot".format(
                                                        key_label_filename))

    def generate_fit_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.event_count_vs_cpu_fits,
                                               "Event Count vs CPU Time (mS) for per {} fits".format(key_label_caption),
                                               "event_count_vs_cpu_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.event_count_vs_maxmem_fits,
                                               "Event Count vs Max Memory (kB) for per {} fits".format(
                                                   key_label_caption),
                                               "event_count_vs_maxmem_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.agents_vs_cpu_fits,
                                               "Agents vs CPU Time (mS) for per {} fits".format(key_label_caption),
                                               "agents_vs_cpu_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.agents_vs_maxmem_fits,
                                               "Agents vs Max Memory (kB) for per {} fits".format(key_label_caption),
                                               "agents_vs_maxmem_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.connections_vs_cpu_fits,
                                               "Connections vs CPU Time (mS) for per {} fits".format(key_label_caption),
                                               "connections_vs_cpu_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_1d_fit_table(self.key_label_tuple,
                                               self.connections_vs_maxmem_fits,
                                               "Connections vs Max Memory (kB) for per {} fits".format(
                                                   key_label_caption),
                                               "connections_vs_maxmem_per_{}_fit".format(key_label_filename))

        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.event_count_and_agents_vs_cpu_fits, "Event Count", "Agents",
                                               "Event Count and Agents vs CPU Time (mS) for per {} fits".format(
                                                   key_label_caption),
                                               "event_count_and_agents_vs_cpu_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.event_count_and_agents_vs_maxmem_fits, "Event Count", "Agents",
                                               "Event Count and Agents vs Max Memory (kB) for per {} fits".format(
                                                   key_label_caption),
                                               "event_count_and_agents_vs_maxmem_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.event_count_and_connections_vs_cpu_fits,
                                               "Event Count", "Connections",
                                               "Event Count and Connections vs CPU Time (mS) for per {} fits".format(
                                                   key_label_caption),
                                               "event_count_and_connections_vs_cpu_per_{}_fit".format(
                                                   key_label_filename))
        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.event_count_and_connections_vs_maxmem_fits,
                                               "Event Count", "Connections",
                                               "Event Count and Connections vs Max Memory (kB) for per {} fits".format(
                                                   key_label_caption),
                                               "event_count_and_connections_vs_maxmem_per_{}_fit".format(
                                                   key_label_filename))
        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.agents_and_connections_vs_cpu_fits, "Agents", "Connections",
                                               "Agents and Connections vs CPU Time (mS) for per {} fits".format(
                                                   key_label_caption),
                                               "agents_and_connections_vs_cpu_per_{}_fit".format(key_label_filename))
        GenericArtifacts.generate_2d_fit_table(self.key_label_tuple,
                                               self.agents_and_connections_vs_maxmem_fits, "Agents", "Connections",
                                               "Agents and Connections vs Max Memory (kB) for per {} fits".format(
                                                   key_label_caption),
                                               "agents_and_connections_vs_maxmem_per_{}_fit".format(key_label_filename))

        GenericArtifacts.generate_3d_fit_table(self.key_label_tuple,
                                               self.event_count_and_agents_and_connections_vs_cpu_fits,
                                               "Event Count", "Agents", "Connections",
                                               "Event Count and Agents and Connections vs CPU Time (mS) for per {} "
                                               "fits".format(key_label_caption),
                                               "event_count_and_agents_and_connections_vs_cpu_per_{}_fit".format(
                                                   key_label_filename))
        GenericArtifacts.generate_3d_fit_table(self.key_label_tuple,
                                               self.event_count_and_agents_and_connections_vs_maxmem_fits,
                                               "Event Count", "Agents", "Connections",
                                               "Event Count and Agents and Connections vs Max Memory (kB) for per "
                                               "{} fits".format(key_label_caption),
                                               "event_count_and_agents_and_connections_vs_maxmem_per_{}_fit".format(
                                                   key_label_filename))

    def generate_score_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        score_tables = dict()

        selection = (("Kernel", ), ("Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.agents_vs_cpu_fits)

        selection = (("Kernel", ), ("Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.connections_vs_cpu_fits)

        selection = (("Kernel", ), ("Events", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.event_count_vs_cpu_fits)

        selection = (("Kernel", ), ("Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.agents_and_connections_vs_cpu_fits)

        selection = (("Kernel", ), ("Events", "Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.event_count_and_agents_vs_cpu_fits)

        selection = (("Kernel", ), ("Events", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.event_count_and_connections_vs_cpu_fits)

        selection = (("Kernel", ), ("Events", "Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_3d_fit_score(self.event_count_and_agents_and_connections_vs_cpu_fits)

        selection = (("Kernel", ), ("Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.agents_vs_maxmem_fits)

        selection = (("Kernel", ), ("Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.connections_vs_maxmem_fits)

        selection = (("Kernel", ), ("Events", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_1d_fit_score(self.event_count_vs_maxmem_fits)

        selection = (("Kernel", ), ("Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.agents_and_connections_vs_maxmem_fits)

        selection = (("Kernel", ), ("Events", "Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.event_count_and_agents_vs_maxmem_fits)

        selection = (("Kernel", ), ("Events", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_2d_fit_score(self.event_count_and_connections_vs_maxmem_fits)

        selection = (("Kernel", ), ("Events", "Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        score_tables[selection].add_3d_fit_score(self.event_count_and_agents_and_connections_vs_maxmem_fits)

        for selection, table in score_tables.iteritems():
            independent_vars = selection[1]
            independent_caption = ""
            independent_filename = ""
            for var in independent_vars:
                independent_caption += "{} and ".format(var)
                independent_filename += "{}_".format(str(var).lower())
            independent_caption = independent_caption[:-5]
            independent_filename = independent_filename[:-1]

            dependent_caption = selection[2]
            if dependent_caption == "CPU":
                dependent_filename = "cpu"
            else:
                dependent_filename = "maxmem"

            GenericArtifacts.generate_score_table(table,
                                                  "Scores based on {} vs {} for {} fits".format(independent_caption,
                                                                                                dependent_caption,
                                                                                                key_label_caption),
                                                  "{}_vs_{}_per_{}_fits_scores".format(independent_filename,
                                                                                       dependent_filename,
                                                                                       key_label_filename))
            GenericArtifacts.generate_score_percentage_table(table,
                                                             "Score percentages based on {} vs {} for {} fits".format(
                                                                 independent_caption,
                                                                 dependent_caption,
                                                                 key_label_caption),
                                                             "{}_vs_{}_per_{}_fits_score_percentage".format(
                                                                 independent_filename,
                                                                 dependent_filename,
                                                                 key_label_filename))

    def generate_machine_comparison_tables(self):
        pass


class KernelMachineArtifacts(GenericArtifacts):
    def __init__(self, results_table):
        super(KernelMachineArtifacts, self).__init__(results_table, ("Kernel", "Machine"))

    def generate_multiline_plots(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        for sub_key in iter(self.sub_keys):
            sub_key_caption = GenericArtifacts.key_tuple_to_caption_string(sub_key)
            sub_key_filename = GenericArtifacts.key_tuple_to_filename_string(sub_key)

            event_count_ranges = self.filter_dict_for_sub_key(self.event_count_ranges, sub_key)
            agents_ranges = self.filter_dict_for_sub_key(self.agents_ranges, sub_key)
            connections_ranges = self.filter_dict_for_sub_key(self.connections_ranges, sub_key)

            event_count_vs_cpu_fits = self.filter_dict_for_sub_key(self.event_count_vs_cpu_fits, sub_key)
            event_count_vs_maxmem_fits = self.filter_dict_for_sub_key(self.event_count_vs_maxmem_fits, sub_key)
            agents_vs_cpu_fits = self.filter_dict_for_sub_key(self.agents_vs_cpu_fits, sub_key)
            agents_vs_maxmem_fits = self.filter_dict_for_sub_key(self.agents_vs_maxmem_fits, sub_key)
            connections_vs_cpu_fits = self.filter_dict_for_sub_key(self.connections_vs_cpu_fits, sub_key)
            connections_vs_maxmem_fits = self.filter_dict_for_sub_key(self.connections_vs_maxmem_fits, sub_key)

            GenericArtifacts.generate_1d_multiline_plot(event_count_vs_cpu_fits, event_count_ranges,
                                                        "Event Count", "CPU Time (mS)",
                                                        "Trend lines for Event Count vs CPU Time for per {} fits for {}"
                                                        .format(key_label_caption, sub_key_caption),
                                                        "event_count_vs_cpu_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_multiline_plot(event_count_vs_maxmem_fits, event_count_ranges,
                                                        "Event Count", "Max Memory (kB)",
                                                        "Trend lines for Event Count vs Max Memory for per {} fits "
                                                        "for {}".format(key_label_caption, sub_key_caption),
                                                        "event_count_vs_maxmem_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_multiline_plot(agents_vs_cpu_fits, agents_ranges,
                                                        "Agents", "CPU Time (mS)",
                                                        "Trend lines for Agents vs CPU Time for per {} fits for "
                                                        "{}".format(key_label_caption, sub_key_caption),
                                                        "agents_vs_cpu_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_multiline_plot(agents_vs_maxmem_fits, agents_ranges,
                                                        "Agents", "Max Memory (kB)",
                                                        "Trend lines for Agents vs Max Memory for per {} fits for "
                                                        "{}".format(key_label_caption, sub_key_caption),
                                                        "agents_vs_maxmem_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_multiline_plot(connections_vs_cpu_fits, connections_ranges,
                                                        "Connections", "CPU Time (mS)",
                                                        "Trend lines for Connections vs CPU Time for per {} fits for "
                                                        "{}".format(key_label_caption, sub_key_caption),
                                                        "connections_vs_cpu_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_multiline_plot(connections_vs_maxmem_fits, connections_ranges,
                                                        "Connections", "Max Memory (kB)",
                                                        "Trend lines for Connections vs Max Memory for per {} fits for "
                                                        "{}".format(key_label_caption, sub_key_caption),
                                                        "connections_vs_maxmem_per_{}_multiline_plot_for_{}".format(
                                                            key_label_filename, sub_key_filename))

    def generate_fit_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        for sub_key in iter(self.sub_keys):
            sub_key_caption = GenericArtifacts.key_tuple_to_caption_string(sub_key)
            sub_key_filename = GenericArtifacts.key_tuple_to_filename_string(sub_key)

            event_count_vs_cpu_fits = self.filter_dict_for_sub_key(self.event_count_vs_cpu_fits, sub_key)
            event_count_vs_maxmem_fits = self.filter_dict_for_sub_key(self.event_count_vs_maxmem_fits, sub_key)
            agents_vs_cpu_fits = self.filter_dict_for_sub_key(self.agents_vs_cpu_fits, sub_key)
            agents_vs_maxmem_fits = self.filter_dict_for_sub_key(self.agents_vs_maxmem_fits, sub_key)
            connections_vs_cpu_fits = self.filter_dict_for_sub_key(self.connections_vs_cpu_fits, sub_key)
            connections_vs_maxmem_fits = self.filter_dict_for_sub_key(self.connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_cpu_fits, sub_key)
            event_count_and_agents_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_maxmem_fits, sub_key)
            event_count_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_cpu_fits, sub_key)
            event_count_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_maxmem_fits, sub_key)
            agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_cpu_fits, sub_key)
            agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_cpu_fits, sub_key)
            event_count_and_agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_maxmem_fits, sub_key)

            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   event_count_vs_cpu_fits,
                                                   "Event Count vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_vs_cpu_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   event_count_vs_maxmem_fits,
                                                   "Event Count vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "event_count_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                    sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   agents_vs_cpu_fits,
                                                   "Agents vs CPU Time (mS) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "agents_vs_cpu_per_{}_fit_for_{}".format(key_label_filename,
                                                                                            sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   agents_vs_maxmem_fits,
                                                   "Agents vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "agents_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                               sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   connections_vs_cpu_fits,
                                                   "Connections vs CPU Time (mS) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "connections_vs_cpu_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                 sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   connections_vs_maxmem_fits,
                                                   "Connections vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "connections_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                    sub_key_filename))

            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_agents_vs_cpu_fits, "Event Count", "Agents",
                                                   "Event Count and Agents vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_agents_vs_maxmem_fits, "Event Count", "Agents",
                                                   "Event Count and Agents vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_connections_vs_cpu_fits,
                                                   "Event Count", "Connections",
                                                   "Event Count and Connections vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_connections_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_connections_vs_maxmem_fits,
                                                   "Event Count", "Connections",
                                                   "Event Count and Connections vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_connections_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   agents_and_connections_vs_cpu_fits, "Agents", "Connections",
                                                   "Agents and Connections vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "agents_and_connections_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   agents_and_connections_vs_maxmem_fits, "Agents", "Connections",
                                                   "Agents and Connections vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "agents_and_connections_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))

            GenericArtifacts.generate_3d_fit_table(("Kernel",),
                                                   event_count_and_agents_and_connections_vs_cpu_fits,
                                                   "Event Count", "Agents", "Connections",
                                                   "Event Count and Agents and Connections vs CPU Time (mS) for per {} "
                                                   "fits".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_and_connections_vs_cpu_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))
            GenericArtifacts.generate_3d_fit_table(("Kernel",),
                                                   event_count_and_agents_and_connections_vs_maxmem_fits,
                                                   "Event Count", "Agents", "Connections",
                                                   "Event Count and Agents and Connections vs Max Memory (kB) for per "
                                                   "{} fits for {}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_and_connections_vs_maxmem_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))

    def generate_score_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        score_tables = dict()
        selection = (("Kernel", "Machine", ), ("Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", ), ("Events", "Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)

        for sub_key in iter(self.sub_keys):
            event_count_vs_cpu_fits = self.filter_dict_for_sub_key(self.event_count_vs_cpu_fits, sub_key)
            event_count_vs_maxmem_fits = self.filter_dict_for_sub_key(self.event_count_vs_maxmem_fits, sub_key)
            agents_vs_cpu_fits = self.filter_dict_for_sub_key(self.agents_vs_cpu_fits, sub_key)
            agents_vs_maxmem_fits = self.filter_dict_for_sub_key(self.agents_vs_maxmem_fits, sub_key)
            connections_vs_cpu_fits = self.filter_dict_for_sub_key(self.connections_vs_cpu_fits, sub_key)
            connections_vs_maxmem_fits = self.filter_dict_for_sub_key(self.connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_cpu_fits, sub_key)
            event_count_and_agents_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_maxmem_fits, sub_key)
            event_count_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_cpu_fits, sub_key)
            event_count_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_maxmem_fits, sub_key)
            agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_cpu_fits, sub_key)
            agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_cpu_fits, sub_key)
            event_count_and_agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_maxmem_fits, sub_key)

            score_tables[(("Kernel", "Machine", ), ("Agents", ), "CPU")].add_1d_fit_score(agents_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Agents", ), "Max Memory")].add_1d_fit_score(agents_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Connections", ), "CPU")].add_1d_fit_score(connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Connections", ), "Max Memory")].add_1d_fit_score(
                connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", ), "CPU")].add_1d_fit_score(event_count_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", ), "Max Memory")].add_1d_fit_score(
                event_count_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Agents", "Connections",), "CPU")].add_2d_fit_score(
                agents_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Agents", "Connections",), "Max Memory")].add_2d_fit_score(
                agents_and_connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Agents",), "CPU")].add_2d_fit_score(
                event_count_and_agents_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Agents",), "Max Memory")].add_2d_fit_score(
                event_count_and_agents_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Connections",), "CPU")].add_2d_fit_score(
                event_count_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Connections",), "Max Memory")].add_2d_fit_score(
                event_count_and_connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Agents", "Connections",), "CPU")].add_2d_fit_score(
                event_count_and_agents_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", ), ("Events", "Agents", "Connections",), "Max Memory")].\
                add_3d_fit_score(event_count_and_agents_and_connections_vs_maxmem_fits)

        for selection, table in score_tables.iteritems():
            independent_vars = selection[1]
            independent_caption = ""
            independent_filename = ""
            for var in independent_vars:
                independent_caption += "{} and ".format(var)
                independent_filename += "{}_".format(str(var).lower())
            independent_caption = independent_caption[:-5]
            independent_filename = independent_filename[:-1]

            dependent_caption = selection[2]
            if dependent_caption == "CPU":
                dependent_filename = "cpu"
            else:
                dependent_filename = "maxmem"

            GenericArtifacts.generate_score_table(table,
                                                  "Scores based on {} vs {} for {} fits".format(independent_caption,
                                                                                                dependent_caption,
                                                                                                key_label_caption),
                                                  "{}_vs_{}_per_{}_fits_scores".format(independent_filename,
                                                                                       dependent_filename,
                                                                                       key_label_filename))
            GenericArtifacts.generate_score_percentage_table(table,
                                                             "Score percentages based on {} vs {} for {} fits".format(
                                                                 independent_caption,
                                                                 dependent_caption,
                                                                 key_label_caption),
                                                             "{}_vs_{}_per_{}_fits_score_percentage".format(
                                                                 independent_filename,
                                                                 dependent_filename,
                                                                 key_label_filename))

    def generate_machine_comparison_tables(self):
        pass


class KernelMachineTypeArtifacts(GenericArtifacts):
    def __init__(self, results_table):
        super(KernelMachineTypeArtifacts, self).__init__(results_table, ("Kernel", "Machine", "Type"))

    def generate_multiline_plots(self):
        pass

    def generate_fit_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        for sub_key in iter(self.sub_keys):
            sub_key_caption = GenericArtifacts.key_tuple_to_caption_string(sub_key)
            sub_key_filename = GenericArtifacts.key_tuple_to_filename_string(sub_key)

            event_count_vs_cpu_fits = self.filter_dict_for_sub_key(self.event_count_vs_cpu_fits, sub_key)
            event_count_vs_maxmem_fits = self.filter_dict_for_sub_key(self.event_count_vs_maxmem_fits, sub_key)
            agents_vs_cpu_fits = self.filter_dict_for_sub_key(self.agents_vs_cpu_fits, sub_key)
            agents_vs_maxmem_fits = self.filter_dict_for_sub_key(self.agents_vs_maxmem_fits, sub_key)
            connections_vs_cpu_fits = self.filter_dict_for_sub_key(self.connections_vs_cpu_fits, sub_key)
            connections_vs_maxmem_fits = self.filter_dict_for_sub_key(self.connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_cpu_fits, sub_key)
            event_count_and_agents_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_maxmem_fits, sub_key)
            event_count_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_cpu_fits, sub_key)
            event_count_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_maxmem_fits, sub_key)
            agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_cpu_fits, sub_key)
            agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_cpu_fits, sub_key)
            event_count_and_agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_maxmem_fits, sub_key)

            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   event_count_vs_cpu_fits,
                                                   "Event Count vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_vs_cpu_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   event_count_vs_maxmem_fits,
                                                   "Event Count vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "event_count_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                    sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   agents_vs_cpu_fits,
                                                   "Agents vs CPU Time (mS) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "agents_vs_cpu_per_{}_fit_for_{}".format(key_label_filename,
                                                                                            sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   agents_vs_maxmem_fits,
                                                   "Agents vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "agents_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                               sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   connections_vs_cpu_fits,
                                                   "Connections vs CPU Time (mS) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "connections_vs_cpu_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                 sub_key_filename))
            GenericArtifacts.generate_1d_fit_table(("Kernel",),
                                                   connections_vs_maxmem_fits,
                                                   "Connections vs Max Memory (kB) for per {} fits for {}".format(
                                                       key_label_caption, sub_key_caption),
                                                   "connections_vs_maxmem_per_{}_fit_for_{}".format(key_label_filename,
                                                                                                    sub_key_filename))

            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_agents_vs_cpu_fits, "Event Count", "Agents",
                                                   "Event Count and Agents vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_agents_vs_maxmem_fits, "Event Count", "Agents",
                                                   "Event Count and Agents vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_connections_vs_cpu_fits,
                                                   "Event Count", "Connections",
                                                   "Event Count and Connections vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_connections_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   event_count_and_connections_vs_maxmem_fits,
                                                   "Event Count", "Connections",
                                                   "Event Count and Connections vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_connections_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   agents_and_connections_vs_cpu_fits, "Agents", "Connections",
                                                   "Agents and Connections vs CPU Time (mS) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "agents_and_connections_vs_cpu_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))
            GenericArtifacts.generate_2d_fit_table(("Kernel",),
                                                   agents_and_connections_vs_maxmem_fits, "Agents", "Connections",
                                                   "Agents and Connections vs Max Memory (kB) for per {} fits for "
                                                   "{}".format(key_label_caption, sub_key_caption),
                                                   "agents_and_connections_vs_maxmem_per_{}_fit_for_{}".format(
                                                       key_label_filename, sub_key_filename))

            GenericArtifacts.generate_3d_fit_table(("Kernel",),
                                                   event_count_and_agents_and_connections_vs_cpu_fits,
                                                   "Event Count", "Agents", "Connections",
                                                   "Event Count and Agents and Connections vs CPU Time (mS) for per {} "
                                                   "fits".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_and_connections_vs_cpu_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))
            GenericArtifacts.generate_3d_fit_table(("Kernel",),
                                                   event_count_and_agents_and_connections_vs_maxmem_fits,
                                                   "Event Count", "Agents", "Connections",
                                                   "Event Count and Agents and Connections vs Max Memory (kB) for per "
                                                   "{} fits for {}".format(key_label_caption, sub_key_caption),
                                                   "event_count_and_agents_and_connections_vs_maxmem_per_{}_fit_for_"
                                                   "{}".format(key_label_filename, sub_key_filename))

    def generate_score_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)

        score_tables = dict()
        selection = (("Kernel", "Machine", "Type", ), ("Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Agents", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Agents", "Connections", ), "CPU")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Agents", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)
        selection = (("Kernel", "Machine", "Type", ), ("Events", "Agents", "Connections", ), "Max Memory")
        score_tables[selection] = ScoreTable(self.kernels, selection)

        for sub_key in iter(self.sub_keys):
            event_count_vs_cpu_fits = self.filter_dict_for_sub_key(self.event_count_vs_cpu_fits, sub_key)
            event_count_vs_maxmem_fits = self.filter_dict_for_sub_key(self.event_count_vs_maxmem_fits, sub_key)
            agents_vs_cpu_fits = self.filter_dict_for_sub_key(self.agents_vs_cpu_fits, sub_key)
            agents_vs_maxmem_fits = self.filter_dict_for_sub_key(self.agents_vs_maxmem_fits, sub_key)
            connections_vs_cpu_fits = self.filter_dict_for_sub_key(self.connections_vs_cpu_fits, sub_key)
            connections_vs_maxmem_fits = self.filter_dict_for_sub_key(self.connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_cpu_fits, sub_key)
            event_count_and_agents_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_vs_maxmem_fits, sub_key)
            event_count_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_cpu_fits, sub_key)
            event_count_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_maxmem_fits, sub_key)
            agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_cpu_fits, sub_key)
            agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.agents_and_connections_vs_maxmem_fits, sub_key)

            event_count_and_agents_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_cpu_fits, sub_key)
            event_count_and_agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_maxmem_fits, sub_key)

            score_tables[(("Kernel", "Machine", "Type", ), ("Agents", ), "CPU")].add_1d_fit_score(agents_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Agents", ), "Max Memory")].add_1d_fit_score(
                agents_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Connections", ), "CPU")].add_1d_fit_score(
                connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Connections", ), "Max Memory")].add_1d_fit_score(
                connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", ), "CPU")].add_1d_fit_score(
                event_count_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", ), "Max Memory")].add_1d_fit_score(
                event_count_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Agents", "Connections",), "CPU")].add_2d_fit_score(
                agents_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Agents", "Connections",), "Max Memory")].add_2d_fit_score(
                agents_and_connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Agents",), "CPU")].add_2d_fit_score(
                event_count_and_agents_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Agents",), "Max Memory")].add_2d_fit_score(
                event_count_and_agents_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Connections",), "CPU")].add_2d_fit_score(
                event_count_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Connections",), "Max Memory")].add_2d_fit_score(
                event_count_and_connections_vs_maxmem_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Agents", "Connections",), "CPU")].\
                add_2d_fit_score(event_count_and_agents_and_connections_vs_cpu_fits)
            score_tables[(("Kernel", "Machine", "Type", ), ("Events", "Agents", "Connections",), "Max Memory")].\
                add_3d_fit_score(event_count_and_agents_and_connections_vs_maxmem_fits)

        for selection, table in score_tables.iteritems():
            independent_vars = selection[1]
            independent_caption = ""
            independent_filename = ""
            for var in independent_vars:
                independent_caption += "{} and ".format(var)
                independent_filename += "{}_".format(str(var).lower())
            independent_caption = independent_caption[:-5]
            independent_filename = independent_filename[:-1]

            dependent_caption = selection[2]
            if dependent_caption == "CPU":
                dependent_filename = "cpu"
            else:
                dependent_filename = "maxmem"

            GenericArtifacts.generate_score_table(table,
                                                  "Scores based on {} vs {} for {} fits".format(independent_caption,
                                                                                                dependent_caption,
                                                                                                key_label_caption),
                                                  "{}_vs_{}_per_{}_fits_scores".format(independent_filename,
                                                                                       dependent_filename,
                                                                                       key_label_filename))
            GenericArtifacts.generate_score_percentage_table(table,
                                                             "Score percentages based on {} vs {} for {} fits".format(
                                                                 independent_caption,
                                                                 dependent_caption,
                                                                 key_label_caption),
                                                             "{}_vs_{}_per_{}_fits_score_percentage".format(
                                                                 independent_filename,
                                                                 dependent_filename,
                                                                 key_label_filename))

    def generate_machine_comparison_tables(self):
        key_label_caption = GenericArtifacts.key_tuple_to_caption_string(self.key_label_tuple)
        key_label_filename = GenericArtifacts.key_tuple_to_filename_string(self.key_label_tuple, lowercase=True)
        machine_comparison_tables = dict()
        machine_comparison_tables[(("Events", "Connections", ), "CPU")] = MachineComparisonTable(self.kernels)

        machine_comparison_tables[(("Events", "Agents", "Connections", ), "Max Memory")] = MachineComparisonTable(
            self.kernels)

        for sub_key in self.sub_keys:
            machine = sub_key[0]
            event_count_and_connections_vs_cpu_fits = self.filter_dict_for_sub_key(
                self.event_count_and_connections_vs_cpu_fits, sub_key)
            event_count_and_agents_and_connections_vs_maxmem_fits = self.filter_dict_for_sub_key(
                self.event_count_and_agents_and_connections_vs_maxmem_fits, sub_key)

            machine_comparison_tables[(("Events", "Connections",), "CPU")].add_2d_fit_score(
                event_count_and_connections_vs_cpu_fits, machine)
            machine_comparison_tables[(("Events", "Agents", "Connections",), "Max Memory")].add_3d_fit_score(
                event_count_and_agents_and_connections_vs_maxmem_fits, machine)

        selection = (("Events", "Connections",), "CPU")
        independent_caption = "Events \& Connections"
        independent_filename = "events_connections"
        dependent_caption = "CPU"
        dependent_filename = "cpu"
        print "Printing results for {}".format(selection)
        machine_comparison_tables[selection].generate_artifacts(key_label_caption, key_label_filename,
                                                                independent_caption, independent_filename,
                                                                dependent_caption, dependent_filename)

        selection = (("Events", "Agents", "Connections",), "Max Memory")
        independent_caption = "Events \& Agents \& Connections"
        independent_filename = "events_agents_connections"
        dependent_caption = "Maximum Memory"
        dependent_filename = "maxmem"
        print "Printing results for {}".format(selection)
        machine_comparison_tables[selection].generate_artifacts(key_label_caption, key_label_filename,
                                                                independent_caption, independent_filename,
                                                                dependent_caption, dependent_filename)


def read_raw_inputs():
    print "Reading in raw results"
    create_str = "CREATE TABLE IF NOT EXISTS raw_results (machine text, kernel text, type text, model text, " \
                 "iteration long, event_count long, final_time long, cpu long, maxmem long, agents long, " \
                 "connections long, bucket long)"
    experiment_db.execute(create_str)
    for input_file in os.listdir(FLAGS.root_dir):
        if re.search(r'run_result.*\.db', input_file):
            result_file = os.path.join(FLAGS.root_dir, input_file)
            print 'Reading results from {}'.format(result_file)
            input_db = DBWrapper(result_file)
            read_results(input_db)
            input_db.cleanup()


def get_correct_type(row):
    row = list(row)
    model = row[3]
    if re.match("CompleteBi.*", model):
        row[2] = "complete-bipartite"
    elif re.match("SmallModel.*", model):
        row[2] = "Watts-Strogatz"
    elif re.match("Cycle.*", model):
        row[2] = "cycle"
    elif re.match("Hyper.*", model):
        row[2] = "hypercube"
    elif re.match("Star.*", model):
        row[2] = "star"
    elif re.match("Complete.*", model):
        row[2] = "complete"
    elif re.match("Erdos.*", model):
        row[2] = "erdos-reyni"
    elif re.match("Wheel.*", model):
        row[2] = "wheel"
    elif re.match("Circular.*", model):
        row[2] = "circular-ladder"
    elif re.match("Periodic.*", model):
        row[2] = "periodic-2grid"
    elif re.match("NonPeriodic.*", model):
        row[2] = "nonperiodic-2grid"
    else:
        print "Unknown model {}".format(model)
        assert False
    return row


def get_bucket_event_count(event_count):
    global event_count_buckets
    global bucketing_factor
    for bucket in event_count_buckets:
        if (1.0 + bucketing_factor) * bucket >= event_count >= (1.0 - bucketing_factor) * bucket:
            return bucket
    return None


def read_results(input_db):
    global experiment_db
    cmd_str = "SELECT machine, kernel, type, model, iteration, event_count, final_time, cpu, maxmem, agents, " \
              "connections FROM 'raw_results'"
    for row in input_db.select(cmd_str):
        if row[2] == "None":
            row = get_correct_type(row)
        bucket = get_bucket_event_count(row[5])
        if bucket is None:
            continue

        cmd_str = "INSERT INTO raw_results " \
                  "(machine, kernel, type, model, iteration, event_count, final_time, cpu, maxmem, agents, " \
                  "connections, bucket) " \
                  "VALUES ('{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}')" \
            .format(row[0], row[1], row[2], row[3], row[4], row[5], row[6], row[7], row[8],
                    row[9], row[10], bucket)
        experiment_db.execute(cmd_str)
    experiment_db.commit()


def generate_per_kernel_results_table():
    global experiment_db
    global kernel_results_table
    global event_count_buckets

    kernel_results_table = ResultsTable()

    select_cmd = "SELECT kernel, bucket, model, event_count, agents, connections, cpu, maxmem FROM raw_results"
    for row in experiment_db.select(select_cmd):
        kernel_results_table.add_entry((row[0],), row[1], row[2], row[3], row[4], row[5], row[6], row[7])

    kernel_results_table.create_filtered_table()


def generate_per_kernel_results_artifacts():
    print "Generating per kernel artifacts"
    global kernel_results_table
    kernel_artifacts = KernelArtifacts(kernel_results_table)
    kernel_artifacts.generate_multiline_plots()
    kernel_artifacts.generate_fit_tables()
    kernel_artifacts.generate_score_tables()
    kernel_artifacts.generate_machine_comparison_tables()
    print "Finished per kernel artifacts"


def generate_per_kernel_and_machine_results_table():
    global experiment_db
    global kernel_machine_results_table
    global event_count_buckets

    kernel_machine_results_table = ResultsTable()

    select_cmd = "SELECT kernel, machine, bucket, model, event_count, agents, connections, cpu, maxmem FROM raw_results"
    for row in experiment_db.select(select_cmd):
        kernel_machine_results_table.add_entry((row[0], row[1]), row[2], row[3], row[4], row[5], row[6], row[7], row[8])

    kernel_machine_results_table.create_filtered_table()


def generate_per_kernel_and_machine_results_artifacts():
    print "Generating per kernel and machine artifacts"
    global kernel_machine_results_table
    kernel_and_machine_artifacts = KernelMachineArtifacts(kernel_machine_results_table)
    kernel_and_machine_artifacts.generate_multiline_plots()
    kernel_and_machine_artifacts.generate_fit_tables()
    kernel_and_machine_artifacts.generate_score_tables()
    kernel_and_machine_artifacts.generate_machine_comparison_tables()
    print "Finished per kernel and machine artifacts"


def generate_per_kernel_and_machine_and_type_results_table():
    global experiment_db
    global kernel_machine_type_results_table
    global event_count_buckets

    kernel_machine_type_results_table = ResultsTable()

    select_cmd = "SELECT kernel, machine, type, bucket, model, event_count, agents, connections, cpu, maxmem FROM " \
                 "raw_results"
    for row in experiment_db.select(select_cmd):
        kernel_machine_type_results_table.add_entry((row[0], row[1], row[2]), row[3], row[4], row[5], row[6], row[7],
                                                    row[8], row[9])

    kernel_machine_type_results_table.create_filtered_table()


def generate_per_kernel_and_machine_and_type_results_artifacts():
    print "Generating per kernel and machine and type artifacts"
    global kernel_machine_type_results_table
    kernel_and_machine_and_type_artifacts = KernelMachineTypeArtifacts(kernel_machine_type_results_table)
    kernel_and_machine_and_type_artifacts.generate_fit_tables()
    kernel_and_machine_and_type_artifacts.generate_score_tables()
    kernel_and_machine_and_type_artifacts.generate_machine_comparison_tables()
    print "Finished per kernel and machine and type artifacts"


def generate_fit_comparison_artifacts():
    global fit_comparison_table
    cpu_fit_list = list()
    memory_fit_list = list()
    for key, value in fit_comparison_table.iteritems():
        if key[2] == "CPU":
            cpu_fit_list.append((value, key,))
        else:
            memory_fit_list.append((value, key,))

    cpu_fit_list.sort()
    cpu_fit_list.reverse()

    cpu_comparison_filename = os.path.join(FLAGS.root_dir, "cpu_fit_comparison_table.tex")
    print "\tGenerating {}".format(cpu_comparison_filename)

    output_latex = r"""\begin{table}[h]
"""

    output_latex += r"""\begin{tabular}{|l|l|c|}
\hline
"""
    output_latex += r"""Selection Keys & Independent Variables & $R^2$ \\
\hline
"""
    for cpu_fit in cpu_fit_list:
        score = cpu_fit[0]
        entry = cpu_fit[1]
        for key in entry[0]:
            output_latex += "{} \& ".format(key)
        output_latex = output_latex[:-4]
        output_latex += " & "
        for var in entry[1]:
            output_latex += "{} \& ".format(var)
        output_latex = output_latex[:-4]
        output_latex += " & %5.4f" % float(score)
        output_latex += r""" \\
"""
    output_latex += r"""\hline
\end{tabular}
"""
    output_latex += "\\caption{Comparisons for CPU fits}\n"
    output_latex += "\\label{tab:cpu_fit_comparison}\n"
    output_latex += r"""\end{table}"""

    with open(cpu_comparison_filename, 'w') as f:
        f.write(output_latex)

    memory_fit_list.sort()
    memory_fit_list.reverse()

    memory_comparison_filename = os.path.join(FLAGS.root_dir, "memory_fit_comparison_table.tex")
    print "\tGenerating {}".format(memory_comparison_filename)

    output_latex = r"""\begin{table}[h]
"""

    output_latex += r"""\begin{tabular}{|l|l|c|}
\hline
"""
    output_latex += r"""Selection Keys & Independent Variables & $R^2$ \\
\hline
"""
    for memory_fit in memory_fit_list:
        score = memory_fit[0]
        entry = memory_fit[1]
        for key in entry[0]:
            output_latex += "{} \& ".format(key)
        output_latex = output_latex[:-4]
        output_latex += " & "
        for var in entry[1]:
            output_latex += "{} \& ".format(var)
        output_latex = output_latex[:-4]
        output_latex += " & %5.4f" % float(score)
        output_latex += r""" \\
"""
    output_latex += r"""\hline
\end{tabular}
"""
    output_latex += "\\caption{Comparisons for Memory fits}\n"
    output_latex += "\\label{tab:memory_fit_comparison}\n"
    output_latex += r"""\end{table}"""

    with open(memory_comparison_filename, 'w') as f:
        f.write(output_latex)


def process_raw_results():
    generate_per_kernel_results_table()
    generate_per_kernel_results_artifacts()
    generate_per_kernel_and_machine_results_table()
    generate_per_kernel_and_machine_results_artifacts()
    generate_per_kernel_and_machine_and_type_results_table()
    generate_per_kernel_and_machine_and_type_results_artifacts()
    generate_fit_comparison_artifacts()


def main(argv):
    global experiment_db

    try:
        FLAGS(argv)  # parse flags
    except gflags.FlagsError, e:
        print '%s\nUsage: %s ARGS\n%s' % (e, sys.argv[0], FLAGS)
        sys.exit(1)

    full_path = os.path.join(FLAGS.root_dir, FLAGS.output_db)
    if FLAGS.read_inputs:
        print "Unlinking {}".format(full_path)
        try:
            os.unlink(full_path)
        except OSError, e:
            print "Unable able to unlink {} due to {}".format(full_path, e)
    else:
        print "Reusing {}".format(full_path)

    experiment_db = DBWrapper(full_path)

    if FLAGS.read_inputs:
        read_raw_inputs()

    process_raw_results()

    experiment_db.cleanup()


if __name__ == '__main__':
    main(sys.argv)
