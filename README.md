cl-traveller
============

These are the Common Lisp Traveller5 utilities, a package to manage the mechanical aspects of running a Traveller5 universe.

Currently the basic Model is still in development. World generation works, full System generation is almost done, and Sophont creation is in progress.

Once the Model works, Controller methods/functions and multiple Views will be implemented, starting with a simple text view to be used straight from the Lisp REPL; next up either a Gtk GUI app or a webapp using Weblocks.

If you have a Common Lisp implementation, feel free to try out the work as it progresses.

If you haven't, multiple good implementations are available for free. The code is tested on:

Multiplatform:

Allegro Common Lisp : http://www.franz.com/downloads.lhtml
ECL: http://ecls.sourceforge.net/
ABCL: http://abcl.org/

Linux: 

Check your distro package repository for:
 - sbcl (Steel Bank Common Lisp)
 - ecl
 - ccl (Clozure Common Lisp)
