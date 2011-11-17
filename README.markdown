

cl-fluiddb / cl-fluidinfo
=========================

cl-fluidinfo (cl-fluiddb is the old, origina name) is a work-in-progress project of accessing the [fluidinfo](http://fluidinfo.com/fluiddb) from Common Lisp.
This is by no means done, expect breaking changes in the future.

I do all my work in SBCL, so this is the only implementation currently supported.


Requirements
------------

You will need

 - [Drakma](http://weitz.de/drakma/)
 - [cl-json](http://common-lisp.net/project/cl-json/)
 - flexi-streams (is a dependency of drakma anyway)
 - bordeaux-threads
 - All dependencies of those libs above.

You should use [Quicklisp](http://www.quicklisp.org/) to get these dependencies.  No, really.  If you aren't using it yet, do yourself a favour and install it before proceeding.

There is also a beginning of some tests written in Lift, so you will need the lift library.