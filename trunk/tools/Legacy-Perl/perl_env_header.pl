# Default configuration (for all high level perl scripts)
# Included from perl_env_header.pl

{
    if($ENV{"MCSTAS"}) {
        $MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } else {
        if ($Config{'osname'} eq 'MSWin32') {
            $MCSTAS::sys_dir = "c:\\mcstas\\lib";
        } else {
            $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
        }
    }

    if($ENV{"MCSTAS_TOOLS"}) {
        $MCSTAS::perl_dir = "$ENV{'MCSTAS_TOOLS'}/perl";
    } else {
        $MCSTAS::perl_dir = "$MCSTAS::sys_dir/perl";
    }

    $MCSTAS::perl_modules = "$MCSTAS::perl_dir/modules";
}
