RySim is a set of implementations of a simple SEIR simulation system. They were
used as part of Ryan Harrison's MSc research and are provided here for
reference. Most of this code has not been extensively reviewed and is not ready
for production usage.

All of the current simulation implementations use some variant of a sequential
DES algorithm and do not implement more complex distributed algorithms. There
are scripts and tools provided for reproducing results found in the thesis.

Please contact Ryan Harrison (rharrison@google.com, zoddicus@gmail.com) with any
questions/comments.

If you wish to contribute code please read CONTRIBUTING.md.

This source code is covered by the Apache 2.0 license as described in LICENSE
unless otherwise specified.

This is not an official Google product.

## Directories
<dl>
<dt>cpp/</dt>
<dd>Contains sequential C++ implementation.</dd>
<dt>erlang/</dt>
<dd>Contains sequential and Actor based Erlang implementations.</dd>
<dt>java/</dt>
<dd>Contains sequential, thread based, and Actor based Java
implementations.</dd>
<dt>python/</dt>
<dd>Contains utility scripts for running the experiments.</dd>
<dt>third_party/</dt>
<dd>Various bits of code used from other sources by this project.</dd>
</dl>
