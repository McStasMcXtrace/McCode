From  owner-ptk@WPI.EDU  Tue Aug  1 01:05:05 1995
Return-Path: <owner-ptk@WPI.EDU>
Date: Mon, 31 Jul 1995 20:04:29 -0400
From: bohm@cs.Buffalo.EDU (Eric J. Bohm)
Message-Id: <199508010004.UAA20132@gagarin.cs.Buffalo.EDU>
To: ptk@WPI.EDU
Subject: TkPerl install on SCO ODT 3.0 (my hacks)
Sender: owner-ptk@WPI.EDU
Precedence: bulk
P-From: "Eric J. Bohm" <bohm@cs.Buffalo.EDU>

To compile Tk-b6 on SCO ODT 3.0 using perl-5.001m:

I Had to comment out this line in the myConfig file used by MakeMaker.

#$define .= " -DHAVE_SYS_SELECT_H" if ($Config{'i_sysselct'});

sys/select.h in SCO has a time definition that conflicts with sys/time.h

Also needed to change all the GNUmakefile files to change
-include $(wildcard *.d)
to
include $(wildcard *.d)

Our GNU make 3.67 blows up on those lines.  Since its over two years old, I
guess we're a little overdue for an upgrade there.  Downloading and
upgrading make soon, I'll post an update if that noticably improves things.

With those revisions in place I can build a statically linked tkperl (SCO
doesn't seem inclined to build a dynamic perl no matter how hard I hack at
it).  Runs all the demos quite nicely.  Now I'm off to learn tkperl
programming from the UserGuide.

I can make a diff patch if anyone's that keen on the results.


							thanks

								EJB
