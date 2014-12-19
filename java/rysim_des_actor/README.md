Actor based Java implementation of SEIR simulation kernel.
## Directories
<dl>
<dt>build/</dt>
<dd>Location for the artifacts from the build system.</dd>
<dt>gradle/</dt>
<dd>Gradle related files.</dd>
<dt>src/</dt>
<dd>Project source files.</dd>
<dt>test/</dt>
<dd>Source and other files for tests.</dd>
</dl>

## Build Commands
`./gradlew tasks`

- Pulls in correct version of Gradle and displays all available tasks.

`./gradle assemble`

- Assembles the artifacts of this project.

`./gradlew build`

- Assembles and tests this project.

`./gradle clean`
 - Deletes the **build/** directory.

`./gradlew distTar`

- Bundles the project as a JVM application with libs and OS specific scripts.

## Execution Commands
`run.sh`

Actor based simulation kernel. Accepts the following flags:

- *--experiment_file*
JSON file that contains experiment specification. This must be specified
and the file must be readable.

- *--random_seed*
Integer that is used to seed the random number generation for the
simulation run. Defaults to 1.