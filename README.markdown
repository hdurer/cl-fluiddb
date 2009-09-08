

cl-fluiddb
==========

cl-fluiddb is a work-in-progress project of accessing the [fluiddb](http://fluidinfo.com/fluiddb) from Common Lisp.
This is by no means done, expect breaking changes in the future.

I do all my work in SBCL, so this is the only implementation currently supported.


Requirements
------------

You will need

 - [Drakma](http://weitz.de/drakma/) -- I am using version 1.0.
 - [cl-json](http://common-lisp.net/project/cl-json/) -- I am using 0.3.2
 - flexi-streams (is a dependency of drakma anyway
 - bordeaux-threads
 - All dependencies of those libs above.

There is also a beginning of some tests written in Lift, so you will need the lift library.