This code has been successfully built and run on Debian 7, Ubuntu 14.04 & Mint
17.1. The versions of Erlang from Erlang Solutions are often newer then what is
available by default from a distribution. This code has been run using the HIPE
version of the Erlang VM.

## Required Tools and Libraries
- [Erlang](https://www.erlang-solutions.com/downloads/download-erlang-otp)
- [Rebar](https://github.com/rebar/rebar)

## Directories
<dl>
<dt>rysim_des/</dt>
<dd>Sequential Erlang implementation of SEIR simulation kernel.</dd>
<dt>rysim_des_actor/</dt>
<dd>Actor based Erlang/OTP implementation of SEIR simulation kernel with SMP support turned OFF.</dd>
<dt>rysim_des_actor_smp/</dt>
<dd>Actor based Erlang/OTP implementation of SEIR simulation kernel with SMP support turned ON.</dd>
</dl>