#!/usr/local/bin/perl -w
######################################################################
# parproc.pl -- Sample script, runs 10 jobs, 5 at a time.
#
# From the book Perl Power! (Addison-Wesley) by Michael Schilli 1999
######################################################################

use Proc::Simple;

$| = 1;                                 # debuffer output
$max_parallel_jobs = 5;                 # jobs processed in parallel
@running = ();                          # array of running jobs

foreach $job (1..9) {                   # create pseudo jobs
    push(@todo, "sleep 3"); 
}                                       

######################################################################
                                        # while there are jobs to do
while($#todo >= 0 || $#running >= 0) {  # or started ones are running
    @running = grep { $_->poll() } @running;  # remove finished jobs

    if($#running + 1 < $max_parallel_jobs &&  # space free in running?
       defined($job = pop(@todo))) {          # ... and job available

        print "Starting job '$job' ... ";
        $proc = Proc::Simple->new();    # new process
        $proc->start($job) || die "Cannot start job $job";
        push(@running, $proc);          # include in running list
    
        print "STARTED. (Remaining: ", $#todo+1, 
              " Running: ", $#running + 1, ")\n";
        next;                           # proceed without delay
    }
    sleep(1);                           # pause ... and proceed
}
