Combines multiple results and performs a analysis on them. Produces a
variety of artifacts to be put in LaTeX documents.

## Execution Commands
`results_analyzer`

Experiment analyzer script. Accepts the following flags:

- *--root_dir*
Root directory to start searching and where to store the database. Defaults to
the current directory.

- *--output_db*
Name of the database file that should be created. If the file already exists it
will be overwritten. Defaults to "experiment.db".

- *--read_inputs*
Controls if the application should re-read the inputs. If so the output DB will
be clobbered entirely. If not only the analysis tables will be removed.

