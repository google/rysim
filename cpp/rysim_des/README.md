Sequential C++ implementation of SEIR simulation kernel.
## Directories
<dl>
<dt>include/</dt>
<dd>Header files for main target.</dd>
<dt>src/</dt>
<dd>Source files for main target.</dd>
<dt>test/</dt>
<dd>Header and source files for tests.</dd>
</dl>
## Build Commands
`make`

- Runs the `all` target.

`make all`

- Builds both the kernel and test binaries.

`make bin` & `make rysim`

- Builds the kernel binary.

`make tests`

- Builds the tests binary.

`make run_tests`

- Run test binary, will build if needed.

### Build Options
`DEBUG=<value> <command>`

- `value` = 0, build without debugging symbols and optimize code.
- `value` = 1, build with debugging symbols.
- `value` defaults to 1.

`USE_CCACHE=<value> <command>`

- `value = 0`, call clang++ directly.
- `value = 1`, cache calls to clang++ using ccache.
- `value` defaults to 0.

## Execution Commands
`rysim`

Sequential simulation kernel. Accepts the following flags:

- *--experiment_file*
JSON file that contains experiment specification. This must be specified
and the file must be readable.

- *--random_seed*
Integer that is used to seed the random number generation for the
simulation run. Defaults to 1.