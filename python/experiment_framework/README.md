Script for generating experiment files and running them in the work
environment.
## Directories
<dl>
<dt>kernels/</dt>
<dd>Set of kernel binary files to be used in experiments.</dd>
</dl>

## Execution Commands
`experiment_framework`

Experiment runner script. Accepts the following flags:

- *--distributions_file*
JSON file containing a list of distribution specifications to be used. These
should be hand checked to produce sane values.

- *--generators_file*
JSON file containing a list of generator specifications to be used.

- *--kernel_file*
JSON file that contains a list of kernel paths to be tested. File is expected to
contain one entry with a member "kernels" that contains the list. This is
required.

- *--events_start*
Value to start the events count at, defaults to 2500.

- *--events_end*
Value to end the events count at, defaults to 50000.

- *--seed*
Seed value for random number generator, defaults to 1.

- *--dry_run*
Print out what would have been written out to the database, but doesn't actually
write anything out.

- *--max_time*
Max time to allow the application to run for in mS, defaults to 1 minute.

- *--max_mem*
Max memory to allow the application to use, in kB, defaults to 0, which
indicates no limit.

- *--hertz*
Rate at which the running application status is sampled. Defaults to 120Hz.

- *--iterations*
Number iterations to run of each test. Defaults to 50.

- *--worker*
Which worker number is this instance. 0, indicates it is the only worker/there
is no need to fan out the results.

- *--num_workers*
Total number of workers. This is used to partition the work set and fan out the
results. Not setting this correctly will not kill your job, but will give you a
mis-balanced workload.

- *--machine*
Machine that experiments are being running on. This defaults to "None", which
means the analyzer will ignore these results.
