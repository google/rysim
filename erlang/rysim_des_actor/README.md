Actor based Erlang/OTP implementation of SEIR simulation kernel with SMP support
turned OFF.
## Directories
<dl>
<dt>deps/</dt>
<dd>Dependencies pulled in by rebar.</dd>
<dt>ebin/</dt>
<dd>Generated Erlang bytecode and other artifacts.</dd>
<dt>include/</dt>
<dd>Project header files with record definitions.</dd>
<dt>src/</dt>
<dd>Source files for project.</dd>
<dt>test/</dt>
<dd>Source and other files for integration tests.</dd>
</dl>

## Build Commands
`make`

- Runs the `all` target.

`make all`

- Builds all of the non-test code and packages up the result in to an escript.

`make deps`

- Pulls in the project dependencies. If **deps/** directory exists, does
  nothing.

`make dist`

- Packages up the project artifacts up into an escript.

`make erl`

- Builds all of the project's artifacts include dependencies.

`make unit_test`

- Run unit tests defined in source files.

`make integration_test`

- Run common tests in **test/** directory.

`make test`

- Run both the unit and integration.

`make clean`

- Clean up all the build artifacts, doesn't remove deps.

`make depclean`

- Remove the dependencies.

`make distclean`

- Scrub everything from the project directory.

`make tarball`

- Create a tarball with the escript and .ebin files that can be moved around and
  run.

## Execution Commands
`rysim_des_actor`

Actor based simulation kernel. Accepts the following flags:

- *--experiment_file*
JSON file that contains experiment specification. This must be specified
and the file must be readable.

- *--random_seed*
Integer that is used to seed the random number generation for the
simulation run. Defaults to 1.
