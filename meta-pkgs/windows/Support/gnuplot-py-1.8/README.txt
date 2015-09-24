Gnuplot.py -- A pipe-based interface to the gnuplot plotting program.

The Gnuplot.py home page is

    http://gnuplot-py.sourceforge.net

There you can get the latest version, view the documentation, or
report bugs.  There is also a mailing list for Gnuplot.py users.  You
can subscribe to the mailing list or view the archive of old articles
at

    http://lists.sourceforge.net/mailman/listinfo/gnuplot-py-users


Documentation
-------------

The quickest way to learn how to use Gnuplot.py is to install it and
run the simple demonstration by typing `python demo.py', then look at
the demo.py file to see the commands that created the demo.  One of
the examples is probably similar to what you want to do.

Don't forget to read the Gnuplot.html, README.txt, and FAQ.txt files
in the Gnuplot.py distribution.

HTML documentation for the Python classes is included in the doc/
directory of the distribution and is also available online (follow
links from the home page).  This documentation is extracted
automatically from the package's docstrings using happydoc and should
be helpful though it is known to have some formatting problems.
Alternatively, you can look at the docstrings yourself by opening the
python files in an editor.

Finally, there is a new mailing list for Gnuplot.py users.  For more
information about subscribing to the list or viewing the archive of
old articles, please go to

     http://lists.sourceforge.net/lists/listinfo/gnuplot-py-users

To get good use out of Gnuplot.py, you will want to know something
about gnuplot, for which a good source is the gnuplot help (run
gnuplot then type `help', or read it online at

    http://www.gnuplot.info/gnuplot.html

).

For a relatively thorough test of Gnuplot.py, type `python test.py'
which goes systematically through most Gnuplot.py features.


Installation
------------

Quick instructions:

1. Download gnuplot-py-1.8.tar.gz or gnuplot-py-1.8.zip.

2. Extract the archive to a temporary directory.

3. Install by changing to the directory and typing "python setup.py
   install".

More information:

Obviously, you must have the gnuplot program if Gnuplot.py is to be of
any use to you.  Gnuplot can be obtained via
<http://www.gnuplot.info>.  You also need a copy of the numpy package, which
is available from the Scipy group at <http://www.scipy.org/Download>.

Gnuplot.py uses Python distutils
<http://www.python.org/doc/current/inst/inst.html> and can be
installed by untarring the package, changing into the top-level
directory, and typing "python setup.py install".  The Gnuplot.py
package is pure Python--no compilation is necessary.

Gnuplot.py is structured as a python package.  That means that it
installs itself as a subdirectory called `Gnuplot' under a directory
of your python path (usually site-packages).  If you don't want to use
distutils you can just move the main Gnuplot.py directory there and
rename it to "Gnuplot".

There are some configuration options that can be set near the top of
the platform-dependent files gp-unix.py (Unix), gp_mac.py (Macintosh),
gp_macosx.py (Mac OS X), gp_win32.py (Windows), and gp_java.py
(Jython/Java).  (Obviously, you should change the file corresponding
to your platform.)  See the extensive comments in gp_unix.py for a
description of the meaning of each configuration variable.  Sensible
values are already chosen, so it is quite possible that you don't have
to change anything.

Import the main part of the package into your python programs using
`import Gnuplot'.  Some other features can be found in the modules
Gnuplot.funcutils and Gnuplot.PlotItems.


Installation via RPM (for Linux/Unix)
-------------------------------------

I decided that it doesn't make sense to package up RPM versions of
Gnuplot.py, since the place where the files need to be installed
depends on what version of Python you are using.  But if you want the
benefits of RPM management, it's easy for you to create your own RPM
from the source distribution then install the RPM:

1. Change into the source directory.

2. Create the RPM:

   $ python ./setup.py bdist --format=rpm

3. Install it (as root):

   # rpm -ivh dist/gnuplot-py-1.7-1.noarch.rpm


Installation on Windows
-----------------------

I don't run Windows, but thanks to the help of users there is now a
way to use Gnuplot.py on that platform.  Any feedback or additional
suggestions having to do with Windows would be especially appreciated.

Because the main MS-Windows gnuplot executable (wgnuplot.exe) doesn't
accept commands on standard input, Gnuplot.py cannot communicate with
it directly.  However, there is a simple little program called
`pgnuplot.exe' that accepts commands on stdin and passes them to
wgnuplot.  So to run Gnuplot.py on Windows, you need to make sure that
pgnuplot.exe is installed.  It comes with gnuplot since at least
version 3.7.1.  Alternatively you can get pgnuplot.exe alone by
downloading `testing/windows-stdin.zip' from one of the gnuplot
archives (e.g.,
<ftp://ftp.gnuplot.info/pub/gnuplot/testing/windows-stdin.zip>).

Continue installing Gnuplot.py by following the instructions in the
previous section.


Installation on the Macintosh
-----------------------------

Thanks to more user help, Gnuplot.py should work on the Macintosh too.
(Here I am referring to Mac OS versions prior to OS X; OS X is unix so
no special considerations apply there.)

Since pipes don't exist on the Mac, communication with gnuplot is via
a python module called gnuplot_Suites.py (included) which uses
AppleEvents.  Note that you will have to convert the python files to
Mac text files (different end-of-line character).  Currently it is not
possible to print directly to a printer; however, it should be
possible to print to a postscript file and print that file manually.
Also, inline data does not seem to be supported.  Let me know if you
find other problems or have patches to fix Mac limitations.


Assistance
----------

If you are having trouble installing or using Gnuplot.py, please check
the following sources for help:

1. Read the documentation!  For simple questions, start with the
   Gnuplot.html, README.txt, and FAQ.txt files in the distribution.
   For more detailed information, check the online class documentation
   at

       http://gnuplot-py.sourceforge.net/doc/

2. Check the mailing list archives.  Chances are that somebody has
   already asked a similar questions and you are one quick search away
   from the answer.  Information about the mailing list is available
   at

       http://lists.sourceforge.net/mailman/listinfo/gnuplot-py-users

3. Ask your question on the mailing list.  I am trying to move most
   email traffic about Gnuplot.py to the mailing list, partly because
   there are many questions (for example about Windows or Macintosh
   platforms) that I am completely unable to answer.  The mailing list
   also provides an archive of old articles which should build up into
   a valuable resource, and a place to exchange ideas about future
   changes.


Feedback
--------

I would love to have feedback from people letting me know whether they
find Gnuplot.py useful.  And certainly let me know about any problems,
suggestions, or enhancements.  For most purposes, please send your
emails to the Gnuplot.py users mailing list:

    gnuplot-py-users@lists.sourceforge.net

Information about the mailing list can be obtained at

    http://lists.sourceforge.net/mailman/listinfo/gnuplot-py-users


Compatibility
-------------

Gnuplot.py has been tested with version 3.7 of gnuplot, and I believe
it should work with version 3.5 (though some features, like enhanced
postscript mode and binary splot mode, will not work).  Let me know if
you have trouble.

Gnuplot.py was developed under Linux and Digital Unix; it should work
without much problem on other versions of Unix.  If you need to modify
it for your system tell me what was necessary and I'll include your
changes in a future release.

Gnuplot.py should also work under Windows and Macintosh (see above).
Feedback for these platforms is especially appreciated since I can't
test them myself.


License
-------

See the file LICENSE.txt for license info.  In brief, Gnuplot is LGPL.


Credits
-------

See CREDITS.txt for a list of people who have contributed code and/or
ideas to Gnuplot.py.  Thanks especially to Konrad Hinsen
<hinsen@ibs.ibs.fr>, who wrote the first, procedural interface version
of Gnuplot.py.


--
Michael Haggerty
<mhagger@alum.mit.edu>
(But please use the mailing list for Gnuplot.py-related issues.)
