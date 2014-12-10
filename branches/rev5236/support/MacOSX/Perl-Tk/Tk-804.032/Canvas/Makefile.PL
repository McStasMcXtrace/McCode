

use Tk::MMutil;
Tk::MMutil::TkExtMakefile(
      ($^O eq 'MSWin32' ? () : ('LIBS'        => ["-lm"])),
      'dynamic_ptk' => 1
      );


