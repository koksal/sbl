  [sbt]: https://github.com/sbt/sbt
  [Graphviz]: http://www.graphviz.org/

This repository contains code for "Synthesis of Biological Models from Mutation
Experiments" by Ali Sinan Koksal, Yewen Pu, Saurabh Srivastava, Rastislav
Bodik, Jasmin Fisher, and Nir Piterman in POPL 2013.

This code is an experimental prototype and is fairly ugly in many places.  The
code is released for openness, and not for good design or good coding
practices.

Scala, ScalaZ3 and Z3 versions used by the tool have been updated to recent
versions to make installation and running easier.

# Dependencies

The tool runs on Linux (64-bit) and Mac OS X.

You should have Java 7 and [sbt] 0.13.2 installed to run the tool. To visualize
executions, you also need [Graphviz].

# Running

Sample scripts for invoking the synthesizer are given in the folder `scripts`.

To run the synthesizer on the "VPC1" Vulval Precursor Cell template described
in Section 7 of the paper, run:

    ./scripts/synthesize-vpc1

Templates and specifications for all models described in the paper are in the
folder `programs` and `specifications`, respectively.
