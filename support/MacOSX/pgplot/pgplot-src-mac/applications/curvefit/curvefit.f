*Date:     Fri, 4 May 90 09:15 EST
*From:     <GILL@QUCDNAST.BITNET>
*Subject:  Latest version of CurveFit for PGPlot packages - May 4, 1990
c-------------------------------------------------------------------------------
c
c                                   CurveFit
c
c         A general curve fitting routine using PGPLOT graphics for use on
c      VAX/uVAX systems, possibly any system capable of running PGPLOT
c      interactively.
c
c         CurveFit will plot up to 30 data sets (a maximum of 2000 points for
c      each data set, a maximum of 20000 points total for one graph), saved
c      as (xp,yp) in ascending {xp} order.  Different data sets in one file
c      must be separated by one non-numeric line.  CurveFit can also modify the
c      data set, by flipping the x and y axes and/or taking logarithms of
c      either or both of them.  Labels and title can be added to the plots.
c      In addition, CurveFit allows one to fit various types of curves to the
c      data sets.  These are:
c
c           (a)  straight line (connect the dots)
c           (b)  cubic spline
c           (c)  smoothing (Bezier) polynomial
c           (d)  low-pass Fourier transform filter
c           (e)  a best-fit polynomial of up to 10 terms
c
c         CurveFit is menu driven and allows one to display all intermediary
c      results.  The x,y plotting ranges are user determined, as is the
c      fitting range (over the x coordinate).  Each type of fit can only be
c      used once with each data set, though different fits may be used with
c      the same data set (however, one could load the data set in again and do
c      the same fit over a different range).  Different data sets can have
c      different symbols and line styles/widths associated with them, so they
c      can be differentiated.  The manner in which the line styles/widths is
c      determined can be changed if the user so desires, as can the symbols
c      used to plot the data points.  Plots can be made on any device that is
c      supported by PGPLOT.
c
c         The programme is fairly well documented.  A subsantial amount of
c      virtual memory is required (of the order of 7 Mb), but this can be
c      decreased by lowering the number of data points that may be plotted.
c      The following are pseudo-global variables that must be changed in each
c      subroutine in order for the programme to work.
c
c           MAXPOINTS    the maximum number of points allowed per graph
c           MAXPOINTSET  the maximum number of data points per data set
c           MAXDATASET   the maximum number of data sets per graph
c           NCOEFF       the maximum number of coefficients for polynomial
c                            fitting
c           S2           gives the extra number of points calculated by the
c                            curve fitting routines to make the curves look
c                            smooth, given as 4*MAXPOINTS or 8*MAXPOINTS
c
c         The Bezier polynomial fitting is taken from Borland's TURBO Graphix
c      Toolbox (translated into FORTRAN), while the cubic spline routines
c      are partially taken from there, and partially my own algorithm.  The
c      polynomial fitting and the fast Fourier transforms are taken from
c      the book Numerical Recipes by W.H. Press et al.  Except as noted below,
c      the routines all seem to do what they are supposed to.  Note that the
c      polynomial curve fitting gives the coefficients and their standard
c      deviation as well.  Take these values with a grain of salt!
c
c         Currently, everything seems to work fairly well, except for some
c      numerical difficulties with the fast Fourier transform that I am
c      unable to fathom.  The problem only becomes noticeable when the FFT
c      smoothing takes place over more than about 100 data points, when
c      anomalous step functions appear at intervals of powers of 2.  The
c      step functions get larger as more data points are being smoothed over.
c      I would appreciate a solution if anyone comes up with one.
c
c         One other minor difficulty discovered occurs when one is taking
c      logs of more than one data set.  It is possible that the endpoints
c      used for the plotting will not cover the full range of all of the
c      data sets plotted.  A fix would require several more variables, plus
c      thinking, of which I do not have time at the present.
c
c         Distribute and use this programme as you see fit.  If anyone comes
c      up with major changes or improvements, please send me a copy of the
c      source code.  If anyone wishes a current copy, send me a note, and
c      the current source code will be sent out to them.  I can be reached
c      at
c
c            BITNET:    gill@qucdnast
c            INTERNET:  gill@bill.phy.queensu.ca
c
c
c  October 27, 1987 -     Fix of subroutine Spline1.  It was able to crash
c                     with a particular spacing of {xp}.  It was also found
c                     to be incorrect.
c                         Fix to all calculation subroutines, so that the
c                     fit will be made over only the points INSIDE the
c                     given fit range, inclusively, i.e. no extrapolation.
c  October 28, 1987 -     Fix of Subroutine Spline2.  It gave discontinuous
c                     spikes at the endpoints.
c                         Add double precision calculations to Spline1 and
c                     Spline2 to allow accurate subtractions/additions.
c  November 6, 1987 -     Fix of calculation procedures that give the range
c                     of the plot.  These are now given to 2 digits accuracy.
c                         Add the option to plot or not plot the individual
c                     data points.
c  November 13, 1987 -    Fix the x,y plotting limits when new data sets are
c                     loaded.
c                         Each {xp,yp} pair must be on the same line.  Other
c                     information is now allowed on each line after {xp,yp}.
c  March 8, 1988 -        Add READDATA subroutine, that allows one to:
c                     a) skip lines at the beginning before data input,
c                     b) have up to 25 columns of data that can be accessed,
c                     c) read multiple data sets from the same file.
c  April 21, 1988 -       Fix of number of points associated with the data
c                     set number.
c                         The default video and hardcopy devices are now
c                     /VT and /IM, respectively.
c  May 30, 1988 -         Add the option of user inputed line styles/widths.
c  June 2, 1988 -         Correct the problem of bad standard deviations on the
c                     polynomial coefficients.
c                         Add REAL*8 and /G_FLOATING to all of the polynomial
c                     calculations.
c  June 13, 1988 -        Add some idiot-proofing (read `Error checking') of
c                     the curve-fitting routines.
c  July 27, 1988 -        Correct the assignments of the fitting range
c                     variables.
c  August 29, 1988 -      Add error-trapping of non-character data input.
c                         Add scaling (in x and/or y) of any data set.
c                         Add ability to toggle all line drawing and fits for
c                     any data set at any time.
c                         Add user chosen line style/line width combinations.
c                         Add user chosen data point plot symbols.  The size of
c                     these is regulated by the total number of points (more
c                     points gives smaller symbols).
c  Christmas, 1988 -      Rearrange the storage method for the data.  Now
c                     there is a maximum of 20000 points, or 30 data sets of
c                     2000 points apiece.  This should allow for more
c                     flexibility for the user.
c  June 21, 1989   -      Change the entrance into the routines to allow
c                     multiple changes to data sets without going to the
c                     main menu screen in between each change.
c                         Fix of the log routines.  Check that your machine
c                     will assign 0 to 10^-100.
c  June 22, 1989   -      Fix of number of points plotted for various fits.
c                     Now each fit may have a different number of data points.
c  July 4, 1989    -      Fix of spline calculation routines.  Splines of more
c                     than one dataset were not saved correctly or plotted.
c  July 6, 1989    -      The default line width, line style will now always
c                     be 1 and 1.
c  July 11, 1989   -      Updated the help file subroutine.
c  July 14, 1989   -      Correct the plot symbol choosing routine.
c                         More idiot-proofing.
c                         Add an UpCase function (works with ASCII only).
c  August 29, 1989 -      Change the order of commands so that the main menu
c                     will not automatically be drawn between commands.  To
c                     redraw menu, type in `R' or `M'.
c  August 31, 1989 -      Fix READDATA, so that any data read in will be from
c                     one line only.  Before, multiple lines were permitted.
c                     Also, an error message is given if the asked for column
c                     does not exist.  The column can then be chosen again.
c  November 23, 1989 -    Update dataset number entry, so that multiple sets
c                     may be included on the same line, including the 0 dataset
c                     (to quit asking).  User is informed of entry errors and
c                     reprompted.
c                         Programme will announce when a particular data file
c                     has been used up and closed.
c                         If one overshoots a data line, typing 'B' will go
c                     back one line.  Typing 'Bn' will go back n lines.
c                     CurveFit is never case-sensitive.  One can also go back
c                     n lines from the first lineskip prompt.
c                         More error checking added - it is much harder to get
c                     it to crash now.
c  January 5, 1990   -    The subroutine SCALEDATASET has been added.  It
c                     includes the expansion/contraction of before, as well as
c                     the ability to slide data sets around the graph, and to
c                     subtract or add one data set from/to another.
c  March 14, 1990    -    Using BACKSPACE to go backwards through a file is
c                     very bad programming, as BACKSPACE = REWIND + (n-1) READ,
c                     so keep track of line number in file, and do the
c                     REWIND/READ combo in software.
c  April 6, 1990     -    Add confirmation when leaving CurveFit.
c  May 4, 1990       -    Correct backspacing through a data file.
c
c      Last revision - May 4, 1990 - Arnold Gill
c      Last revision to Tim Pearson (tjp@deimos.caltech.edu) - May 4, 1990
c
c-------------------------------------------------------------------------------
      PROGRAM CurveFit
c
      INTEGER    maxpoints,s2,ncoeff,maxdataset,dataset,ierror,i,j,ma,
     *     totalpoints,cdataset,ChooseDataSet,ii,maxpointset,offset,m,n
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,ncoeff=10,
     *     maxdataset=30,maxpointset=2000)
c
c -----   The PARAMETER statement exists in many SUBROUTINEs, as needed.
c      Any changes to MAXPOINTS, MAXPOINTSET, NCOEFF, MAXDATASET must be
c      done in all of them.
c
      REAL       xdata(maxpointset),ydata(maxpointset),x(maxpoints),
     *     y(maxpoints),zero,xbez(s2),ybez(s2),xspl(s2),yspl(s2),
     *     coeff(ncoeff,2),xfr(maxpoints),yfr(maxpoints),r1,r2,r3,r4,
     *     xpoly(s2),ypoly(s2),x1,x2,y1,y2,f1,f2,one,
     *     scale,xminmax(2,maxdataset),yminmax(2,maxdataset)
      INTEGER    power(ncoeff),xstart(maxdataset,2),styles(maxdataset,3)
     *     ,maxply(maxdataset),maxfr(maxdataset),maxbez(maxdataset),
     *     maxspl(maxdataset)
      CHARACTER  xlabel*80,ylabel*80,title*80,blank*40,video*3,
     *     hardcopy*3,ans*1,datain*80,outstring*80,UpCase*1
      LOGICAL    labels,xlog(maxdataset),ylog(maxdataset),f,t,flip,
     *     fits(5,maxdataset),points,fileopen,linestyle,quit,ok
c
      DATA  blank/'                                        '/
      DATA  f/.FALSE./,t/.TRUE./,one/1.0/,zero/0.0/
c
      COMMON  /PlyDat/xpoly,ypoly,coeff,power,maxply,ma
      COMMON  /SplDat/xspl,yspl,maxspl,/BezDat/xbez,ybez,maxbez
      COMMON  /FrDat/xfr,yfr,maxfr
      COMMON  /XYDat/x,y,xdata,ydata,xminmax,yminmax
c
c -----   Opening title
c
c      WRITE (*,'('1')')
c      WRITE (*,98) 'Display the general description  [n] ? '
c      READ (*,97) ans
c      ans=UpCase(ans)
c      IF (ans.EQ.'Y') CALL Description
c
c -----   Choose the display device to use.  Unless you have the latest
c      version of PGPLOT, comment out the next program line.
c
      WRITE (*,100)
100   FORMAT ('1'///10X,'Choose your video display terminal'///10X,
     *     '(Default device is /VT)'//)
      CALL PGLDEV
      WRITE (*,*)
      video='   '
      READ (*,97) video
      IF (video(1:1).NE.'/') video='/vt'
      y1=1E35
      y2=-y1
      x1=y1
      x2=-y1
      dataset=0
      xlabel=blank//blank
      ylabel=blank//blank
      title=blank//blank
      labels=f
      fileopen=f
      linestyle=f
      totalpoints=0
      ierror=0
c
c -----   Data input
c
      CALL ReadData (dataset,xstart,x1,y1,x2,y2,fileopen,totalpoints)
      f1=x1
      f2=x2
      points=f
      DO 112 i=1,maxdataset
        styles(i,1)=1
        styles(i,2)=1
        styles(i,3)=-1
        DO 111 j=1,5
          fits(j,i)=f
111     CONTINUE
        xlog(i)=f
        ylog(i)=f
112   CONTINUE
      fits(1,dataset)=t
1     CONTINUE
c
c -----   Main menu for data and plot manipulation
c
      CALL Menu (ans,fits,dataset,xlog,ylog,flip,labels,points,
     *     linestyle,x1,x2,y1,y2,f1,f2)
      GOTO 1112
1111  CONTINUE
      WRITE (*,*)
      WRITE (*,98) 'Input command  [`M'' for Menu, <RETURN> to plot'//
     *     ' graph]: '
      READ (*,97) ans
      ans=UpCase(ans)
      IF (ans.EQ.'M') GOTO 1
1112  CONTINUE
      IF (((ans.LT.'1').OR.(ans.GT.'9')).AND.
     *     (ans.NE.'H').AND.(ans.NE.'X').AND.(ans.NE.'Q').AND.
     *     (ans.NE.'E').AND.(ans.NE.'L').AND.(ans.NE.'F').AND.
     *     (ans.NE.'N').AND.(ans.NE.'T').AND.(ans.NE.'A').AND.
     *     (ans.NE.'D').AND.(ans.NE.'S').AND.(ans.NE.'V')) THEN
c
c -----   Plot data with current values for parameters
c
        CALL Plot (xstart,x1,x2,y1,y2,f1,f2,xlabel,ylabel,title,labels,
     *     points,video,fits,dataset,f,totalpoints,styles,linestyle)
        CALL Menu (ans,fits,dataset,xlog,ylog,flip,labels,points,
     *       linestyle,x1,x2,y1,y2,f1,f2)
        GOTO 1112
      ELSE IF (ans.EQ.'1') THEN
c
c -----   Toggle the straight line data connecting from PGPLOT from
c     chosen data set
c
2       CONTINUE
        WRITE (*,'((A),(A),I2)') '0(Dis)Connect the points of which ',
     *       'dataset(s) : 1 - ',dataset
        WRITE (*,*) '   0 to quit, <ENTER> = current dataset'
        datain=blank//blank
        READ (*,97) datain
        CALL ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
        IF (.NOT.quit) GOTO 2
      ELSE IF (ans.EQ.'2') THEN
c
c -----   Toggle and calculate the cubic spline fitting routine for
c     the chosen data set
c
3       CONTINUE
        WRITE (*,'((A),(A),I2)') '0Fit Cubic splines to which ',
     *       'dataset(s) : 1 - ',dataset
        WRITE (*,*) '   0 to quit, <ENTER> = current dataset'
        datain=blank//blank
        READ (*,97) datain
        CALL ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
        IF (.NOT.quit) GOTO 3
      ELSE IF (ans.EQ.'3') THEN
c
c -----   Toggle and calculate the Bezier polynomial data smoothing
c     for the chosen data set
c
4       CONTINUE
        WRITE (*,'((A),(A),I2)') '0Fit a Bezier polynomial to which',
     *       ' dataset(s) : 1 - ',dataset
        WRITE (*,*) '   0 to quit, <ENTER> = current dataset'
        datain=blank//blank
        READ (*,97) datain
        CALL ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
        IF (.NOT.quit) GOTO 4
      ELSE IF (ans.EQ.'4') THEN
c
c -----   Toggle and calculate the Fourier transform data smoothing
c     for the chosen data set
c
5       CONTINUE
        WRITE (*,'((A),(A),I2)') '0Smooth using an FFT of which ',
     *       'dataset(s) : 1 - ',dataset
        WRITE (*,*) '   0 to quit, <ENTER> = current dataset'
        datain=blank//blank
        READ (*,97) datain
        CALL ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
        IF (.NOT.quit) GOTO 5
      ELSE IF (ans.EQ.'5') THEN
c
c -----   Toggle and calculate the best fit polynomial of data for the
c     chosen data set
c
6       CONTINUE
        WRITE (*,'((A),(A),I2)') '0Fit a polynomial to which ',
     *       'dataset(s) : 1 - ',dataset
        WRITE (*,*) '   0 to quit, <ENTER> = current dataset, multiple'
     *       //' entries allowed'
        datain=blank//blank
        READ (*,97) datain
        CALL ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
        IF (.NOT.quit) GOTO 6
      ELSE IF (ans.EQ.'6') THEN
c
c -----   Toggle and calculate the base 10 logs of the x,y data as requested
c     for the chosen data set
c
65      CONTINUE
        j=2
        outstring=blank//blank
        DO 7 i=1,dataset
          IF (xlog(i).OR.ylog(i)) THEN
            WRITE (outstring(j:j+1),'(I2)') i
            j=j+3
          ENDIF
7      CONTINUE
        IF (j.GT.2) THEN
          WRITE (*,97) '0Logs are already calculated for data sets # :'
          WRITE (*,97) outstring
        ENDIF
8       CONTINUE
        WRITE (*,'((A),I2)') '0Take logs of which dataset : 1 - ',
     *       dataset
        WRITE (*,*) '     0 to quit, <ENTER> = current dataset'
        READ (*,97) datain
        IF (datain(1:1).EQ.'0') GOTO 1006
        cdataset=ChooseDataSet(datain,dataset,ok)
        IF (.NOT.ok) GOTO 8
        IF ((cdataset.GT.dataset).OR.(cdataset.LT.1)) THEN
          WRITE (*,99) 7,7,'Error in input.  Reenter data set number'
          GOTO 8
        ENDIF
        offset=xstart(cdataset,1)-1
        WRITE (*,*)
        WRITE (*,98) 'Take logs of x data  [n]: '
        READ (*,97) ans
        IF (UpCase(ans).EQ.'Y') THEN
          IF (.NOT.xlog(cdataset)) THEN
            xlog(cdataset)=t
            x1=1.0E35
            x2=-1.0E35
            DO 9 i=1,xstart(cdataset,2)
              IF (x(offset+i).GT.zero) THEN
                x(offset+i)=LOG10(x(offset+i))
                x1=MIN(x(offset+i),x1)
                x2=MAX(x(offset+i),x2)
              ELSE
                WRITE (*,99) 7,7,'Error - log of a non-positive number'
                x(offset+i)=-100.0
              ENDIF
9           CONTINUE
            DO 10 i=2,5
              fits(i,cdataset)=f
10          CONTINUE
          ENDIF
        ELSE IF (xlog(cdataset)) THEN
          xlog(cdataset)=f
          x1=1.0E35
          x2=-1.0E35
          DO 11 i=1,xstart(cdataset,2)
            x(offset+i)=1.0D1**DBLE(x(offset+i))
            x1=MIN(x(offset+i),x1)
            x2=MAX(x(offset+i),x2)
11        CONTINUE
          DO 12 i=2,5
            fits(i,cdataset)=f
12        CONTINUE
        ENDIF
        WRITE (*,98) 'Take logs of y data  [n]: '
        READ (*,97) ans
        IF (UpCase(ans).EQ.'Y') THEN
          IF (.NOT.ylog(cdataset)) THEN
            ylog(cdataset)=t
            y1=1.0E35
            y2=-1.0E35
            DO 13 i=1,xstart(cdataset,2)
              IF (y(offset+i).GT.zero) THEN
                y(offset+i)=LOG10(y(offset+i))
                y1=MIN(y(offset+i),y1)
                y2=MAX(y(offset+i),y2)
              ELSE
                WRITE (*,99) 7,7,'Error - log of a non-positive number'
                y(offset+i)=-100.0
              ENDIF
13          CONTINUE
            DO 14 i=2,5
              fits(i,cdataset)=f
14          CONTINUE
          ENDIF
        ELSE IF (ylog(cdataset)) THEN
          ylog(cdataset)=f
          y1=1.0E35
          y2=-1.0E35
          DO 15 i=1,xstart(cdataset,2)
            y(offset+i)=1.0D1**DBLE(y(offset+i))
            y1=MIN(y(offset+i),y1)
            y2=MAX(y(offset+i),y2)
15        CONTINUE
          DO 16 i=2,5
            fits(i,cdataset)=f
16        CONTINUE
        ENDIF
        CALL SetLimits (x1,x2)
        CALL SetLimits (y1,y2)
        f1=x1
        f2=x2
        GOTO 65
1006    CONTINUE
      ELSE IF (ans.EQ.'7') THEN
c
c -----   Set the range of the x coordinate to plot
c
        ii=1
17      CONTINUE
        WRITE (*,96) '0Current plotting range of x-coordinate is [ ',
     *       x1,' to ',x2,' ]'
        WRITE (*,98) 'Enter new range: '
        READ (*,*,ERR=1000,IOSTAT=ierror) x1,x2
        IF (x1.EQ.x2) THEN
          x2=x2+one
        ELSE IF (x1.GT.x2) THEN
          CALL Swap (x1,x2)
        ENDIF
        f1=MIN(f1,x1)
        f2=MAX(f2,x2)
      ELSE IF (ans.EQ.'8') THEN
c
c -----   Set the range of the y coordinate to plot
c
        ii=2
18      CONTINUE
        WRITE (*,96) '0Current plotting range of y-coordinate is [ ',
     *       y1,' to ',y2,' ]'
        WRITE (*,98) 'Enter new range: '
        READ (*,*,ERR=1000,IOSTAT=ierror) y1,y2
        IF (y1.EQ.y2) THEN
          y2=y2+one
        ELSE IF (y1.GT.y2) THEN
          CALL Swap (y1,y2)
        ENDIF
      ELSE IF (ans.EQ.'9') THEN
c
c -----   Set the range of the x coordinate over which to use the
c      different fitting routines
c
        ii=3
19      CONTINUE
        WRITE (*,96) '0Current fitting range of x-coordinate is [ ',
     *       f1,' to ',f2,' ]'
        WRITE (*,98) 'Enter new range: '
        READ (*,*,ERR=1000,IOSTAT=ierror) f1,f2
        IF (f1.EQ.f2) THEN
          f2=f2+one
        ELSE IF (f1.GT.f2) THEN
          CALL Swap (f1,f2)
        ENDIF
      ELSE IF (ans.EQ.'V') THEN
c
c -----   Choose the display device to use.  Unless you have the latest
c      version of PGPLOT, comment out the next program line.
c
        WRITE (*,1900)
1900    FORMAT ('0',10X,'Choose your video display terminal'//10X,
     *       '(Default device is /VT)'//)
        CALL PGLDEV
        WRITE (*,*)
        video='   '
        READ (*,97) video
        IF (video(1:1).NE.'/') video='/vt'
      ELSE IF (ans.EQ.'N') THEN
c
c -----   Get a new data file to analyse and start a new plot
c
        WRITE (*,98) 'This will erase the current plot.  Proceed'//
     *     '  [n]  ? '
        READ (*,97) ans
        IF (UpCase(ans).EQ.'Y') THEN
          y1=1E35
          y2=-y1
          x1=y1
          x2=-y1
          dataset=0
          xlabel=blank//blank
          ylabel=blank//blank
          title=blank//blank
          labels=f
          DO 205 i=1,maxdataset
            styles(i,1)=1
            styles(i,2)=1
            styles(i,3)=-1
            DO 20 j=1,5
              fits(j,i)=f
20          CONTINUE
            xlog(i)=f
            ylog(i)=f
205       CONTINUE
          fits(1,1)=t
          points=f
          linestyle=f
          CALL ReadData (dataset,xstart,x1,y1,x2,y2,fileopen,
     *         totalpoints)
          f1=x1
          f2=x2
        ENDIF
      ELSE IF (ans.EQ.'A') THEN
c
c -----   Add a new data file to current plot.  MAXDATASET data sets are
c      permitted.
c
        IF (dataset.EQ.maxdataset) THEN
          WRITE (*,99) 7,7,'No more room for additional data sets'
        ELSE
          Call ReadData (dataset,xstart,x1,y1,x2,y2,fileopen,
     *         totalpoints)
          xlog(dataset)=f
          ylog(dataset)=f
          fits(1,dataset)=t
          DO 21 i=2,5
            fits(i,dataset)=f
21        CONTINUE
          f1=x1
          f2=x2
        ENDIF
      ELSE IF (ans.EQ.'F') THEN
c
c -----   Exchange the x and y axes, with the various fits being set to
c      false (i.e. they need to be redone)
c
22      CONTINUE
        WRITE (*,'((A),(A),I2)') '0Flip the X-Y axes of which ',
     *       'dataset : 1 - ',dataset
        WRITE (*,*) '     0 to quit,  <ENTER> = current dataset'
        READ (*,97) datain
        IF (datain(1:1).EQ.'0') GOTO 1007
        cdataset=ChooseDataSet(datain,dataset,ok)
        IF (.NOT.ok) GOTO 22
        IF ((cdataset.GT.dataset).OR.(cdataset.LT.1)) THEN
          WRITE (*,99) 7,7,'Error in input.  Reenter data set number'
          GOTO 22
        ENDIF
        DO 23 i=1,xstart(cdataset,2)
          CALL Swap (x(offset+i),y(offset+i))
23      CONTINUE
        fits(1,cdataset)=t
        DO 24 i=2,5
          fits(i,cdataset)=f
24      CONTINUE
        flip=.NOT.flip
        CALL Swap (x1,y1)
        CALL Swap (x2,y2)
        f1=x1
        f2=x2
        GOTO 22
1007    CONTINUE
      ELSE IF (ans.EQ.'H') THEN
c
c -----   Choose the hardcopy device, and send it
c
        WRITE (*,101)
101     FORMAT ('1'///10X,'Choose your hardcopy device:'///10X,
     *     '(Default device is /IM)'//)
        CALL PGLDEV
        WRITE (*,*)
        hardcopy='   '
        READ (*,97) hardcopy
        IF (hardcopy(1:1).NE.'/') hardcopy='/im'
        CALL Plot (xstart,x1,x2,y1,y2,f1,f2,xlabel,ylabel,title,labels,
     *     points,hardcopy,fits,dataset,t,totalpoints,styles,
     *     linestyle)
      ELSE IF (ans.EQ.'S') THEN
        CALL ScaleDataSet (x1,x2,y1,y2,fits,xstart,dataset)
      ELSE IF (ans.EQ.'T') THEN
c
c -----   Place and toggle labels and title on the plot
c
        IF (labels) THEN
          WRITE (*,*)
          WRITE (*,98) 'Remove the title/labels  [n] ? '
          READ (*,97) ans
          IF (UpCase(ans).NE.'Y') THEN
            WRITE (*,*)
            WRITE (*,98) 'Change the title/labels  [y] ? '
            READ (*,97) ans
            IF (UpCase(ans).NE.'N') THEN
              WRITE (*,*)
              WRITE (*,98) 'Change the x-axis label  [n] ? '
              READ (*,97) ans
              IF (UpCase(ans).EQ.'Y') THEN
                WRITE (*,*) xlabel
                WRITE (*,*) 'Enter the new x-axis label :'
                READ (*,97) xlabel
              ENDIF
              WRITE (*,*)
              WRITE (*,98) 'Change the y-axis label  [n] ? '
              READ (*,97) ans
              IF (UpCase(ans).EQ.'Y') THEN
                WRITE (*,*) ylabel
                WRITE (*,*) 'Enter the new y-axis label :'
                READ (*,97) ylabel
              ENDIF
              WRITE (*,*)
              WRITE (*,98) 'Change the title  [n] ? '
              READ (*,97) ans
              IF (UpCase(ans).EQ.'Y') THEN
                WRITE (*,*) title
                WRITE (*,*) 'Enter the new title of the plot :'
                READ (*,97) title
              ENDIF
            ENDIF
          ELSE
            labels=f
          ENDIF
        ELSE
          labels=t
          WRITE (*,*) 'Enter the x-axis label :'
          READ (*,97) xlabel
          WRITE (*,*) 'Enter the y-axis label :'
          READ (*,97) ylabel
          WRITE (*,*) 'Enter the title of the plot :'
          READ (*,97) title
        ENDIF
      ELSE IF (ans.EQ.'D') THEN
c
c -----   Toggle the plotting of the data points
c
        points=.NOT.points
        IF (points) THEN
          WRITE (*,3200) dataset
3200      FORMAT ('0There is/are currently ',I2,
     *         ' different set(s) of data plotted.'//,4X,
     *         'Choose the symbol for graphing of data points from the',
     *         ' table below'//,19X,
     *         '(-1)  do not plot this dataset'//,12X,
     *         '(2)  plus sign',13X,'(12) open star'/,12X
     *         '(3)  asterisk',14X,'(13) filled triangle'/,12X,
     *         '(4)  open circle',11X,'(14) open cross'/,12X,
     *         '(5)  times sign',12X,'(15) star of david'/,12X,
     *         '(6)  open square',11X,'(16) filled square'/,12X,
     *         '(7)  open triangle',9X,'(17) filled circle'/,12X,
     *         '(10) hyperbolic square',5X,'(18) filled star'/,12X,
     *         '(11) open diamond'//)
          WRITE (*,*)
          ii=4
          DO 36 i=1,dataset
34          CONTINUE
            WRITE (*,'((A),I2,(A),$)') ' Choice for data set #',i,': '
            READ (*,*,ERR=1000,IOSTAT=ierror) m
            IF (.NOT.(((m.GE.2).AND.(m.LE.7)).OR.
     *          ((m.GE.10).AND.(m.LE.18)).OR.(m.EQ.-1))) GOTO 1000
            styles(i,3)=m
36        CONTINUE
        ELSE
          DO 37 i=1,maxdataset
            styles(i,3)=-1
37        CONTINUE
        ENDIF
      ELSE IF (ans.EQ.'L') THEN
c
c -----   Toggle the choice of linestyle to and from USER / SYSTEM, and
c     set the user's choice of linestyle.
c
        linestyle=.NOT.linestyle
        IF (linestyle) THEN
          WRITE (*,3700) dataset
3700      FORMAT ('0There is/are currently ',I2,
     *         ' different set(s) of data plotted.'//,4X,
     *         'Choose the line style and the line width from'/,2X,
     *         'the table below and enter as  ls,lw  (ex 1,9)'//,5X,
     *         'Line style:     1)  solid line'/,21X,
     *         '2)  long dashes'/,21X,'3)  dash-dot-dash'/,21X,
     *         '4)  dotted'/,21X,'5)  dash-dot-dot-dot'//,5X,
     *         'Line width:     1 through 21'//)
          ii=5
          DO 40 i=1,dataset
38          CONTINUE
            WRITE (*,'((A),I2,(A),$)') ' Choice for data set #',i,': '
            READ (*,*,ERR=1000,IOSTAT=ierror) m,n
            IF ((m.LT.1).OR.(m.GT.5).OR.(n.LT.1).OR.(n.GT.21)) GOTO 1000
            styles(i,1)=m
            styles(i,2)=n
40        CONTINUE
        ELSE
          DO 41 i=1,dataset
            styles(i,1)=1
            styles(i,2)=1
41        CONTINUE
        ENDIF
      ELSE
        WRITE (*,*)
        WRITE (*,'(2A1,(A))') 7,7,'Confirm exit from CurveFit - enter'//
     *            ' a capital `C'': '
        READ (*,97) ans
c
c -----   Quit the programme CurveFit - requires an upper-case `C'
c
        IF (ans.EQ.'C') STOP
      ENDIF
      GOTO 1111
c
c -----   Error checking/error flag resetting routine.
c
1000  CONTINUE
      WRITE (*,99) 7,7,'***  Error in input - redo!  ***'
      IF (ierror.GT.0) CALL ERRTST (ierror,m)
      GOTO (17,18,19,34,38),ii
96    FORMAT ((A),E10.3,A4,E10.3,A2)
97    FORMAT ((A))
98    FORMAT (1X,(A),$)
99    FORMAT ('0',2(A1),(A))
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE ToggleDataSet (fits,xstart,f1,f2,ans,datain,dataset,
     *     maxdataset,quit)
c
c -----   Takes an input string of dataset numbers, separates them and
c     operates on the valid values.  Errors are detected and the user is
c     notified.  This allows more than one data set number to be included
c     on each input line, including the 0 data set which quits entry.  All
c     numbers after 0 on the entry line are ignored.
c
      REAL       f1,f2
      INTEGER    dataset,maxdataset,xstart(maxdataset,2),ind,last,ians,
     *     cdataset,ierror,j
      LOGICAL    quit,fits(5,maxdataset),ok
      CHARACTER  ans*1,datain*80,ch*1
c
      ians=ICHAR(ans)-ICHAR('0')
      last=80
      ind=1
c
c -----   Determine the length of the input string.  If zero, default is used.
c
      DO 1 last=80,1,-1
        ch=datain(last:last)
        IF ((ch.GE.'0').AND.(ch.LE.'9')) GOTO 2
1     CONTINUE
      last=0
      GOTO 3
2     CONTINUE
      last=last+1
3     CONTINUE
      IF (last.EQ.0) THEN
        cdataset=dataset
        GOTO 5
      ENDIF
4     CONTINUE
c
c -----   Read the dataset number from the given passed substring
c
      READ (datain(ind:last),*,IOSTAT=ierror) cdataset
      IF (ierror.NE.0) THEN
c
c -----   Bad, non-integer, input
c
        IF (ierror.GT.0) CALL ERRTST (ierror,j)
        WRITE (*,400) 7,7
400     FORMAT (1X,2A1,'***  Error in input.  ***')
      ELSEIF ((cdataset.GT.dataset).OR.(cdataset.LT.0)) THEN
c
c -----   Invalid integer input
c
        WRITE (*,401) 7,7,cdataset
401     FORMAT (1X,2A1,'***  Error in input. ',I4,' is not valid.  ***')
        GOTO 6
      ENDIF
5     CONTINUE
c
c -----   Toggle the dataset logical and do the appropriate action
c
      IF (cdataset.EQ.0) GOTO 8
      fits(ians,cdataset)=.NOT.fits(ians,cdataset)
      IF (fits(ians,cdataset)) THEN
        IF (ians.EQ.1) THEN
          ok=.TRUE.
        ELSEIF (ians.EQ.2) THEN
          CALL SplineCalc (xstart(cdataset,1)-1,xstart(cdataset,2),
     *         f1,f2,cdataset,ok)
        ELSEIF (ians.EQ.3) THEN
          CALL BezierCalc (xstart(cdataset,1)-1,xstart(cdataset,2),f1,
     *         f2,cdataset,ok)
        ELSEIF (ians.EQ.4) THEN
          CALL FourierCalc (xstart(cdataset,1)-1,xstart(cdataset,2),f1,
     *         f2,cdataset,ok)
        ELSEIF (ians.EQ.5) THEN
          CALL PolynomialCalc (xstart(cdataset,1)-1,xstart(cdataset,2),
     *         f1,f2,cdataset,ok)
        ENDIF
        fits(ians,cdataset)=ok
      ENDIF
c
c -----   Inform user of result
c
      IF (fits(ians,cdataset)) THEN
        WRITE (*,500) cdataset,' is now ON'
      ELSE
        WRITE (*,500) cdataset,' is now OFF'
      ENDIF
500   FORMAT (' Dataset #',I2,(A))
6     CONTINUE
c
c -----   Find first numeric character in input string from current position
c
      ch=datain(ind:ind)
      IF ((ch.GE.'0').AND.(ch.LE.'9')) GOTO 7
      ind=ind+1
      IF (ind.GE.last) GOTO 8
      GOTO 6
7     CONTINUE
c
c -----   When first numeric character is found, look for a non-numeric
c     character (a delimiter - i.e. NOT one of 0..9,+,-)
c
      ind=ind+1
      IF (ind.GE.last) GOTO 8
      ch=datain(ind:ind)
      IF (((ch.GE.'0').AND.(ch.LE.'9')).OR.(ch.EQ.'-').OR.(ch.EQ.'+'))
     *     GOTO 7
      ind=ind+1
      IF (ind.GE.last) GOTO 8
      GOTO 4
8     CONTINUE
      quit=cdataset.EQ.0
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      FUNCTION ChooseDataSet (datain,dataset,ok)
c
      INTEGER    ChooseDataSet,dataset,ierror,idummy
      CHARACTER  datain*2
      LOGICAL    ok
c
      READ (datain,*,IOSTAT=ierror) idummy
      ok=ierror.LE.0
      IF (.NOT.ok) THEN
        WRITE (*,'(1X,2A1,(A))') 7,7,'***  Error in input ***'
        RETURN
      ENDIF
      IF (idummy.EQ.0) THEN
        ChooseDataSet=dataset
      ELSE
        ChooseDataSet=idummy
      ENDIF
      RETURN
      END
c
c ----------------------------------------------------------------------
c
      FUNCTION UpCase (ch)
c
c -----   This function works on ASCII machines.  EBCDIC is a ?
c
      CHARACTER*1  UpCase,ch
c
      IF ((ch.GE.'a').AND.(ch.LE.'z')) THEN
        Upcase=CHAR(ICHAR(ch)-ICHAR('a')+ICHAR('A'))
      ELSE
        UpCase=ch
      ENDIF
      RETURN
      END
c
c ------------------------------------------------------------------------------
c
      FUNCTION StrLen (string)
c
c -----   Returns the real length of the string, not the amount of memory set
c     aside for it.
c
      INTEGER    StrLen,i,ilength
      CHARACTER  string*(*)
c
      ilength=LEN(string)
      StrLen=0
      DO i=ilength,1,-1
        IF (string(i:i).NE.' ') THEN
          StrLen=i
          RETURN
        ENDIF
      ENDDO
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE Menu (ans,fits,dataset,xlog,ylog,flip,labels,points,
     *     linestyle,x1,x2,y1,y2,f1,f2)
c
c -----   The main menu screen with all of the current plot settings for
c      the current data
c
      INTEGER       maxpoints,spoly,ncoeff,maxdataset,dataset,ma,i,j
      PARAMETER     (maxpoints=20000,ncoeff=10,maxdataset=30,
     *     spoly=8*maxpoints)
      REAL          xp(spoly),yp(spoly),coeff(ncoeff,2),x1,x2,y1,y2,
     *     f1,f2
      INTEGER       power(ncoeff),maxply(maxdataset)
      CHARACTER*1   ans,UpCase
      CHARACTER*34  plotted(5),dummy
      LOGICAL       fits(5,maxdataset),xlog,ylog,flip,labels,points,
     *     linestyle
c
      COMMON  /PlyDat/xp,yp,coeff,power,maxply,ma
c
      WRITE (*,100)
100   FORMAT ('0',24X,'CurveFit - Main Menu'/)
      DO 2 i=1,5
        dummy='[                                ]'
        DO 1 j=1,dataset
          dummy(j+2:j+2)='-'
          IF (fits(i,j)) dummy(j+2:j+2)='+'
1       CONTINUE
        plotted(i)=dummy
2     CONTINUE
      WRITE (*,93) '(1)  Connect points with straight line ',plotted(1)
      WRITE (*,93) '(2)  Connect points with cubic spline  ',plotted(2)
      WRITE (*,93) '(3)  Smooth data with Bezier polynomial',plotted(3)
      WRITE (*,93) '(4)  Smooth data with Fourier transform',plotted(4)
      WRITE (*,93) '(5)  Best fit polynomial               ',plotted(5)
      IF (xlog.AND.ylog) THEN
        WRITE (*,97) '(6)  Logarithms of x or y data','[ XY ]'
      ELSE IF (xlog) THEN
        WRITE (*,97) '(6)  Logarithms of x or y data','[ X ]'
      ELSE IF (ylog) THEN
        WRITE (*,97) '(6)  Logarithms of x or y data','[ Y ]'
      ELSE
        WRITE (*,97) '(6)  Logarithms of x or y data','[ NONE ]'
      ENDIF
      WRITE (*,94) '(7)  Plotting range of x coordinate           [',
     *     x1,' to ',x2,' ]'
      WRITE (*,94) '(8)  Plotting range of y coordinate           [',
     *     y1,' to ',y2,' ]'
      WRITE (*,94) '(9)  Fitting range of x coordinate            [',
     *     f1,' to ',f2,' ]'
      IF (flip) THEN
        WRITE (*,95) '(F)  Flip the x and y axes','[ ON ]'
      ELSE
        WRITE (*,95) '(F)  Flip the x and y axes','[ OFF ]'
      ENDIF
      IF (points) THEN
        WRITE (*,98) '(D)  Data points are plotted','[ ON ]'
      ELSE
        WRITE (*,98) '(D)  Data points are plotted','[ OFF ]'
      ENDIF
      IF (labels) THEN
        WRITE (*,99) '(T)  Labels and title placed on the plot','[ ON ]'
      ELSE
        WRITE (*,99) '(T)  Labels and title placed on the plot',
     *       '[ OFF ]'
      ENDIF
      WRITE (*,92) '(A)  Add a data file to existing plot','[ ',dataset,
     *     ' ]'
      IF (linestyle) THEN
        WRITE (*,96) '(L)  User/System defined plotted line styles',
     *     '[ USER ]'
      ELSE
        WRITE (*,96) '(L)  User/System defined plotted line styles',
     *     '[ SYSTEM ]'
      ENDIF
      WRITE (*,101)
101   FORMAT (1X,'(S)  Scale/Slide/Subtract a particular dataset'/1X,
     *     '(H)  Send plot to disk for hardcopy'/1X,
     *     '(V)  Select a new video display device'/1X,
     *     '(N)  New data file and plot'/1X,'(E,Q,X)  Exit CurveFit',
     *     //10X,'Choose one of the above     [Plot to screen]:  ',$)
      READ (*,'(A)') ans
      ans=UpCase(ans)
92    FORMAT (1X,(A),17X,(A),I<INT(LOG10(FLOAT(dataset)))+1>,(A))
93    FORMAT (1X,(A),3X,(A))
94    FORMAT (1X,(A),E10.3,A4,E10.3,A2)
95    FORMAT (1X,(A),28X,(A))
96    FORMAT (1X,(A),10X,(A))
97    FORMAT (1X,(A),24X,(A))
98    FORMAT (1X,(A),26X,(A))
99    FORMAT (1X,(A),14X,(A))
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE Plot (xstart,x1,x2,y1,y2,f1,f2,xlabel,ylabel,title,
     *     labels,points,device,fits,dataset,hard,totalpoints,styles,
     *     linestyle)
c
c -----   The general plotting subroutine
c
      INTEGER    maxpoints,s2,ncoeff,maxdataset,dataset,i,j,ma,
     *     totalpoints,symbol,offset
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,ncoeff=10,
     *     maxdataset=30)
      REAL       x(maxpoints),y(maxpoints),coeff(ncoeff,2),
     *     xbez(s2),ybez(s2),xspl(s2),x1,x2,y1,y2,
     *     yspl(s2),xfr(maxpoints),xplot(s2),yplot(s2),f1,f2,
     *     yfr(maxpoints),xpoly(s2),ypoly(s2)
      INTEGER    power(ncoeff),plotsym(maxdataset),xstart(maxdataset,2),
     *     styles(maxdataset,3),maxply(maxdataset),maxbez(maxdataset),
     *     maxfr(maxdataset),maxspl(maxdataset)
      CHARACTER  xlabel*(*),ylabel*(*),title*(*),device*3,ans*1,
     *     plotname*25,UpCase*1,blank*40
      LOGICAL    fits(5,maxdataset),labels,hard,points,linestyle
c
      DATA  plotsym/3,11,6,7,12,4,16,17,2,5,13,14,15,18,10,3,11,6,7,12,
     *     4,16,17,2,5,13,14,15,18,10/
      DATA  blank/'+                                       '/
c
c -----   These are the PGPlot plot symbols, appearing in the following
c      (default) order (twice to cover the 30 possible data sets):
c           asterisk, diamond, square, triangle, star, circle,
c           filled square, filled circle, plus, times, filled triangle,
c           open cross, star of David, filled star, rounded square
c
      COMMON  /XYDat/x,y,/SplDat/xspl,yspl,maxspl
      COMMON  /BezDat/xbez,ybez,maxbez,/FrDat/xfr,yfr,maxfr
      COMMON  /PlyDat/xpoly,ypoly,coeff,power,maxply,ma
c
      IF (hard) THEN
        WRITE (*,'((A),$)') '0Enter plot file name : '
        READ (*,'((A))') plotname
        CALL PGBegin (0,plotname//device,1,1)
      ELSE
        CALL PGBegin (0,device,1,1)
      ENDIF
      CALL PGSCH (1.0)
      CALL PGSCF (2)
      IF (hard) CALL PGSLW (2)
      CALL PGEnv (x1,x2,y1,y2,0,1)
      DO 8 i=1,dataset
        offset=xstart(i,1)-1
        symbol=styles(i,3)
        IF (symbol.EQ.-1) GOTO 2
        IF (symbol.EQ.0) symbol=plotsym(i)
        DO 1 j=1,xstart(i,2)
          xplot(j)=x(offset+j)
          yplot(j)=y(offset+j)
1       CONTINUE
        CALL PGSCH (MAX(0.5,1.0-0.1*(totalpoints/200)))
        IF (hard) THEN
          CALL PGSLW (2)
          IF (points) CALL PGPoint (xstart(i,2),xplot,yplot,symbol)
          IF (i.EQ.1) THEN
            CALL PGSLW (1)
            CALL PGSCH (1.0)
c            CALL PGIden
          ENDIF
        ELSE
          CALL PGSLW (1)
          CALL PGSCH (MAX(0.5,1.0-0.1*(totalpoints/200)))
          IF (points) CALL PGPoint (xstart(i,2),xplot,yplot,symbol)
        ENDIF
2       CONTINUE
        CALL PGSCH (1.0)
        CALL PGSLS (styles(i,1))
        CALL PGSLW (styles(i,2))
        IF (fits(1,i)) THEN
          DO 3 j=1,xstart(i,2)
            xplot(j)=x(offset+j)
            yplot(j)=y(offset+j)
3         CONTINUE
          CALL PGLine (xstart(i,2),xplot,yplot)
        ENDIF
        IF (fits(2,i)) THEN
          DO 4 j=1,maxspl(i)
            xplot(j)=xspl(8*offset+j)
            yplot(j)=yspl(8*offset+j)
4         CONTINUE
          CALL PGLine (maxspl(i),xplot,yplot)
        ENDIF
        IF (fits(3,i)) THEN
          DO 5 j=1,maxbez(i)
            xplot(j)=xbez(8*offset+j)
            yplot(j)=ybez(8*offset+j)
5         CONTINUE
          CALL PGLine (maxbez(i),xplot,yplot)
        ENDIF
        IF (fits(4,i)) THEN
          DO 6 j=1,maxfr(i)
            xplot(j)=xfr(offset+j)
            yplot(j)=yfr(offset+j)
6         CONTINUE
          CALL PGLine (maxfr(i),xplot,yplot)
        ENDIF
        IF (fits(5,i)) THEN
          DO 7 j=1,maxply(i)
            xplot(j)=xpoly(8*offset+j)
            yplot(j)=ypoly(8*offset+j)
7         CONTINUE
          CALL PGLine (maxply(i),xplot,yplot)
        ENDIF
8     CONTINUE
      IF (.NOT.hard) THEN
        WRITE (*,800) 7,'Press T for titles, <ENTER> to quit: '
800     FORMAT (1X,A1,(A),$)
        READ (*,'((A))') ans
        IF (UpCase(ans).NE.'T') GOTO 9
      ENDIF
      WRITE (*,'((A))') blank
      CALL PGSCH (1.0)
      CALL PGSCF (2)
      IF (hard) CALL PGSLW (2)
      IF (labels) CALL PGLabel (xlabel,ylabel,title)
      IF (.NOT.hard) THEN
        WRITE (*,800) 7,'Finished plotting - press <ENTER> to '//
     *       'continue: '
        READ (*,'((A))') ans
c
c -----   The following is done to clear the screen after the plot is
c      finished, i.e. when the main menu appears again, on some strange
c      terminals, like the LANPAR Vision II (VT240 clone).
c
9       CONTINUE
        CALL PGAsk (.FALSE.)
        CALL PGAdvance
      ENDIF
      CALL PGEnd
c
c -----   For Tektronics emulators, recall plot routines to return to text mode
c
      IF (device.EQ.'/te') THEN
        ans=CHAR(27)
        WRITE (*,'(A6)') ans//'[?38l'
      ENDIF
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
c -----   The above options statement allows the use of this routine with
c      more than 140 data points, due to N! problems just before label 3.
c
      SUBROUTINE BezierCalc (offset,numpt,f1,f2,dataset,ok)
c
c -----   The routine for calculating the Bezier polynomial over the
c      x-coordinate range from F1 to F2.  The routine is translated exactly
c      from Borland's TURBO Graphix Toolbox.  The polynomial will space an
c      additional 7 points {xp,yp} between every two data points {x,y},
c      i.e. 8 times as dense.
c
      INTEGER    maxpoints,s2,maxdataset,dataset,numpt,i,j,maxcpt,
     *     intpt,n,maxpointset,offset,maxnum
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,maxdataset=30,
     *     maxpointset=2000)
      INTEGER    maxbez(maxdataset)
      REAL       x(maxpoints),y(maxpoints),xp(s2),yp(s2),t,quot,f1,f2,
     *     one,deltat
      REAL*8     c(0:maxpointset),sumx,sumy,done,prod
      LOGICAL    ok
c
      DATA  one,done/1.0,1.0D0/
c
      COMMON  /XYDat/x,y,/BezDat/xp,yp,maxbez
c
      ok=.FALSE.
      i=0
1     CONTINUE
        i=i+1
      IF (x(offset+i).LT.f1) GOTO 1
      j=i
2     CONTINUE
      IF (j+1.LE.numpt.AND.x(offset+j+1).LE.f2) THEN
        j=j+1
        GOTO 2
      ENDIF
      maxcpt=j-i
      maxnum=(j-i+1)*8
      maxbez(dataset)=maxnum
      IF (maxcpt.LT.2) THEN
        WRITE (*,'(1X,A1,(A))') 7,'Error in Bezier Calculation routine!'
        RETURN
      ENDIF
      f1=x(offset+i)
      f2=x(offset+j)
      IF (f1.GT.f2) CALL Swap (f1,f2)
      deltat=one/(maxnum-1)
      c(0)=done
      c(maxcpt)=done
      DO 3 n=0,maxcpt-2
        c(n+1)=c(n)*(maxcpt-n)/(n+1)
3     CONTINUE
      DO 8 intpt=1,maxnum
        t=(intpt-1)*deltat
        IF (t.LE.0.5) THEN
          quot=one-t
          prod=quot
          DO 4 n=1,maxcpt-1
            prod=prod*quot
4         CONTINUE
          quot=t/quot
          sumx=x(offset+j)
          sumy=y(offset+j)
          DO 5 n=maxcpt,1,-1
            sumx=c(n-1)*x(offset+i+n-1)+quot*sumx
            sumy=c(n-1)*y(offset+i+n-1)+quot*sumy
5         CONTINUE
        ELSE
          quot=t
          prod=quot
          DO 6 n=1,maxcpt-1
            prod=prod*quot
6         CONTINUE
          quot=(one-t)/quot
          sumx=x(offset+i)
          sumy=y(offset+i)
          DO 7 n=1,maxcpt
            sumx=c(n)*x(offset+i+n)+quot*sumx
            sumy=c(n)*y(offset+i+n)+quot*sumy
7         CONTINUE
        ENDIF
        xp(8*offset+intpt)=SNGL(sumx*prod)
        yp(8*offset+intpt)=SNGL(sumy*prod)
8     CONTINUE
      ok=.TRUE.
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE PolynomialCalc (offset,numpt,f1,f2,dataset,ok)
c
c -----   Calculate the best fit polynomial over the x-coordinate range
c      F1 to F2.  The routine used is called Singular Value Decomposition
c      (SVDFIT), taken from the book Numerical Recipes by W.H. Press et al.
c      It is slower than normal solving of the matrices involved, but will
c      never give an infinite coefficient.  Coefficients that tend toward
c      infinity while cancelling another are set to zero with this method.
c      Other negligible coefficients are also set to zero (determined by
c      the size of TOL*WMAX).  The best fit is done with a polynomial of
c      user specified order, though the number of non-zero coefficients must
c      be less than 10.  This routine also returns the error in the
c      coefficients and data points to plot.  It is assumed that no
c      uncertainties exist in the input data.
c
      INTEGER    maxpoints,s2,ncoeff,maxdataset,dataset,ma,i,j,k,
     *     numpt,maxpointset,offset,maxnum,ierror
      CHARACTER  ans*1
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,maxdataset=30,
     *     ncoeff=10,maxpointset=2000)
      REAL       x(maxpoints),y(maxpoints),coeff(ncoeff,2),
     *     xpoly(s2),ypoly(s2),tol,zero,f1,f2,const,xp,FPoly
      REAL*8     v(ncoeff,ncoeff),u(maxpointset,ncoeff),w(ncoeff),
     *     wmax,thresh
      INTEGER    power(ncoeff),maxply(maxdataset)
      LOGICAL    ok
c
      COMMON  /XYDat/x,y,/PlyDat/xpoly,ypoly,coeff,power,maxply,ma
c
      DATA  tol,zero/1.0E-10,0.0/
c
      ok=.FALSE.
      i=0
1     CONTINUE
        i=i+1
      IF (x(offset+i).LT.f1) GOTO 1
      j=i
2     CONTINUE
      IF (j+1.LE.numpt.AND.x(offset+j+1).LE.f2) THEN
        j=j+1
        GOTO 2
      ENDIF
      maxnum=j-i+1
      IF (maxnum.LT.2) THEN
        WRITE (*,'(1X,A1,(A),(A))') 7,'Error in Polynomial Calculation',
     *       ' routine!'
        RETURN
      ENDIF
      f1=x(offset+i)
      f2=x(offset+j)
      IF (f1.GT.f2) CALL Swap (f1,f2)
c
c -----   Enter the user chosen non-zero coefficients.  The data will
c      be fit only to these powers of the polynomial.
c
      WRITE (*,100)
100   FORMAT (' Enter the power of any coefficient that is to be ',
     *     'included in the fit.'/'          (^Z to finish)'//2X,
     *     'ex.  Best-fit to a cubic, enter'/5X,'0 1 2 3^Z'//)
      ma=0
      DO 3 j=1,ncoeff
        coeff(j,1)=zero
        coeff(j,2)=zero
        power(j)=1000
3     CONTINUE
      WRITE (*,'(1X,(A),$)') 'Power(s) : '
      READ (*,*,ERR=1000,END=4,IOSTAT=ierror) (power(k),k=1,ncoeff)
4     CONTINUE
      CALL PowerSort (power,k,ma,ncoeff)
      DO 6 j=1,maxnum
        xp=x(offset+i+j-1)
        DO 5 k=1,ma
          IF (xp.EQ.zero) THEN
            u(j,k)=zero
          ELSE
            u(j,k)=xp**power(k)
          ENDIF
5       CONTINUE
        ypoly(8*offset+j)=y(offset+i+j-1)
6     CONTINUE
      CALL SVDCMP (u,maxnum,ma,ncoeff,w,v)
      wmax=zero
      DO 7 j=1,ma
        wmax=MAX(wmax,w(j))
7     CONTINUE
      thresh=tol*wmax
      DO 8 j=1,ma
        IF (w(j).LT.thresh) w(j)=zero
8     CONTINUE
      CALL SVBKSB (u,w,v,maxnum,ma,ncoeff,ypoly,dataset,coeff,8*offset)
      CALL SVDVar (v,ma,ncoeff,w,coeff)
      maxply(dataset)=10*maxnum
      const=(f2-f1)/(maxply(dataset)-1)
      DO 9 j=1,maxply(dataset)
        xpoly(8*offset+j)=const*(j-1)+f1
        ypoly(8*offset+j)=FPoly(xpoly(8*offset+j),coeff,power,ma)
9     CONTINUE
      ok=.TRUE.
      WRITE (*,101)
101   FORMAT ('0The calculated coefficients are : '//)
      DO 10 i=1,ma,2
        IF (i+1.LE.ma) THEN
          WRITE (*,102) power(i),coeff(i,1),coeff(i,2),
     *         power(i+1),coeff(i+1,1),abs(coeff(i+1,2))
102       FORMAT (2(7X,'x^',I3,4X,E12.5,' +- ',E9.3))
        ELSE
          WRITE (*,103) power(i),coeff(i,1),abs(coeff(i,2))
103       FORMAT (7X,'x^',I3,4X,E12.5,' +- ',E9.3)
        ENDIF
10    CONTINUE
      WRITE (*,104)
104   FORMAT ('0Press <return> to continue ',$)
11    CONTINUE
      READ (*,'((A))') ans
      RETURN
c
c -----   Error checking/error flag resetting routine.
c
1000  CONTINUE
      IF (ierror.GT.0) CALL ERRTST (ierror,j)
      WRITE (*,105) 7,7,'***  Error in input - redo!  ***'
105   FORMAT ('0',2A1,(A))
      GOTO 3
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
c -----   This was added to help reduce problems with large powers and x
c      very small or very large.
c
      FUNCTION FPoly (x,coeff,power,ma)
c
c -----   This calculates the polynomial at a point x.  If x=0, then the
c      polynomial is set to the constant, coeff(0), instead of to a
c      possible infinity.  This is needed by the routine PolynomialCalc.
c
      INTEGER    ncoeff,m,ma,i
      REAL*8     sum,zero,xp
      PARAMETER  (ncoeff=10)
      REAL       coeff(ncoeff,2),FPoly,x
      INTEGER    power(ncoeff)
c
      DATA  zero/0.0D0/
c
      xp=DBLE(x)
      IF (xp.NE.zero) THEN
        sum=zero
        DO 1 i=1,ma
          sum=sum+DBLE(coeff(i,1))*(xp**power(i))
1       CONTINUE
c
c -----   The following idiot-proofing is made to avoid over- and
c      underflows.  Of course, it could still happen in the previous
c      step if x is too large or too small.
c
        IF (sum.EQ.zero) THEN
          FPoly=0.0
        ELSE IF (DLOG10(DABS(sum)).GT.38) THEN
          FPoly=1.0E30
        ELSE IF (DLOG10(DABS(sum)).LT.-38) THEN
          FPoly=0.0
        ELSE
          FPoly=SNGL(sum)
        ENDIF
      ELSE
        m=0
2       CONTINUE
          m=m+1
          IF ((power(m).NE.0).AND.(m.LE.ma)) GOTO 2
        IF (m.GT.ma) THEN
          FPoly=0.0
        ELSE
          FPoly=coeff(m,1)
        ENDIF
      ENDIF
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE PowerSort (power,k,ma,ncoeff)
c
c -----   This routine is a simple insertion sort.  It sorts the vector
c      POWER into ascending order, counting the number of distinct powers
c      while eliminating any doubled powers.
c
      INTEGER  ncoeff,ma,i,j,k,power(ncoeff),ip
c
      DO 3 i=2,ncoeff
        ip=power(i)
        DO 1 j=i-1,1,-1
          IF (power(j).LE.ip) GOTO 2
          power(j+1)=power(j)
1       CONTINUE
        j=0
2       CONTINUE
        power(j+1)=ip
3     CONTINUE
      DO 6 i=1,ncoeff-1
        IF (power(i+1).EQ.1000) GOTO 7
4       CONTINUE
          IF (power(i).EQ.power(i+1)) THEN
            DO 5 j=i+1,ncoeff-1
              power(j)=power(j+1)
5           CONTINUE
            GOTO 4
          ENDIF
6     CONTINUE
7     CONTINUE
      ma=i
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE SVDCMP (a,m,n,np,w,v)
c
c -----   This subroutine is taken exactly from the book Numerical Recipes
c      by W.H. Press et al., and is used by the routine PolynomialCalc to
c      do the singular value decomposition (SVD) of the input data matrix.
c      The results of the SVD can have certain values zeroed to eliminate
c      them from the resultant coefficients.
c
      INTEGER    i,j,k,l,m,n,its,nm,maxpointset,np
      PARAMETER  (maxpointset=2000)
      REAL*8     a(maxpointset,np),w(np),v(np,np),rv1(maxpointset),
     *     x,y,z,h,c,s,f,g,scale,anorm,zero,one
c
      DATA  zero,one/0.0D0,1.0D0/
c
      g=zero
      scale=zero
      anorm=zero
      DO 25 i=1,n
        l=i+1
        rv1(i)=scale*g
        g=zero
        s=zero
        scale=zero
        IF (i.LE.m) THEN
          DO 11 k=i,m
            scale=scale+ABS(a(k,i))
11        CONTINUE
          IF (scale.NE.zero) THEN
            DO 12 k=i,m
              a(k,i)=a(k,i)/scale
              s=s+a(k,i)*a(k,i)
12          CONTINUE
            f=a(i,i)
            g=-SIGN(SQRT(s),f)
            h=f*g-s
            a(i,i)=f-g
            IF (i.NE.n) THEN
              DO 15 j=l,n
                s=zero
                DO 13 k=i,m
                  s=s+a(k,i)*a(k,j)
13              CONTINUE
                f=s/h
                DO 14 k=i,m
                  a(k,j)=a(k,j)+f*a(k,i)
14              CONTINUE
15            CONTINUE
            ENDIF
            DO 16 k=i,m
              a(k,i)=scale*a(k,i)
16          CONTINUE
          ENDIF
        ENDIF
        w(i)=scale*g
        g=zero
        s=zero
        scale=zero
        IF ((i.LE.m).AND.(i.NE.n)) THEN
          DO 17 k=l,n
            scale=scale+ABS(a(i,k))
17        CONTINUE
          IF (scale.NE.zero) THEN
            DO 18 k=l,n
              a(i,k)=a(i,k)/scale
              s=s+a(i,k)*a(i,k)
18          CONTINUE
            f=a(i,l)
            g=-SIGN(SQRT(s),f)
            h=f*g-s
            a(i,l)=f-g
            DO 19 k=l,n
              rv1(k)=a(i,k)/h
19          CONTINUE
            IF (i.NE.m) THEN
              DO 23 j=l,m
                s=zero
                DO 21 k=l,n
                  s=s+a(j,k)*a(i,k)
21              CONTINUE
                DO 22 k=l,n
                  a(j,k)=a(j,k)+s*rv1(k)
22              CONTINUE
23            CONTINUE
            ENDIF
            DO 24 k=l,n
              a(i,k)=scale*a(i,k)
24          CONTINUE
          ENDIF
        ENDIF
        anorm=MAX(anorm,(ABS(w(i))+ABS(rv1(i))))
25    CONTINUE
      DO 32 i=n,1,-1
        IF (i.LT.n) THEN
          IF (g.NE.zero) THEN
            DO 26 j=l,n
              v(j,i)=(a(i,j)/a(i,l))/g
26          CONTINUE
            DO 29 j=l,n
              s=zero
              DO 27 k=l,n
                s=s+a(i,k)*v(k,j)
27            CONTINUE
              DO 28 k=l,n
                v(k,j)=v(k,j)+s*v(k,i)
28            CONTINUE
29          CONTINUE
          ENDIF
          DO 31 j=l,n
            v(i,j)=zero
            v(j,i)=zero
31        CONTINUE
        ENDIF
        v(i,i)=one
        g=rv1(i)
        l=i
32    CONTINUE
      DO 39 i=n,1,-1
        l=i+1
        g=w(i)
        IF (i.LT.n) THEN
          DO 33 j=l,n
            a(i,j)=zero
33        CONTINUE
        ENDIF
        IF (g.NE.zero) THEN
          g=one/g
          IF (i.NE.n) THEN
            DO 36 j=l,n
              s=zero
              DO 34 k=l,m
                s=s+a(k,i)*a(k,j)
34            CONTINUE
              f=s*g/a(i,i)
              DO 35 k=i,m
                a(k,j)=a(k,j)+f*a(k,i)
35            CONTINUE
36          CONTINUE
          ENDIF
          DO 37 j=i,m
            a(j,i)=a(j,i)*g
37        CONTINUE
        ELSE
          DO 38 j=i,m
            a(j,i)=zero
38        CONTINUE
        ENDIF
        a(i,i)=a(i,i)+one
39    CONTINUE
      DO 49 k=n,1,-1
        DO 48 its=1,30
          DO 41 l=k,1,-1
            nm=l-1
            IF ((ABS(rv1(l))+anorm).EQ.anorm) GOTO 2
            IF ((ABS(w(nm))+anorm).EQ.anorm) GOTO 1
41        CONTINUE
1         c=zero
          s=one
          DO 43 i=l,k
            f=s*rv1(i)
            IF ((ABS(f)+anorm).NE.anorm) THEN
              g=w(i)
              h=SQRT(f*f+g*g)
              w(i)=h
              h=one/h
              c=g*h
              s=-f*h
              DO 42 j=1,m
                y=a(j,nm)
                z=a(j,i)
                a(j,nm)=y*c+z*s
                a(j,i)=-y*s+z*c
42            CONTINUE
            ENDIF
43        CONTINUE
2         z=w(k)
          IF (l.EQ.k) THEN
            IF (z.LT.zero) THEN
              w(k)=-z
              DO 44 j=1,n
                v(j,k)=-v(j,k)
44            CONTINUE
            ENDIF
            GOTO 3
          ENDIF
          IF (its.EQ.30) PAUSE 'No convergence in 30 iterations'
          x=w(l)
          nm=k-1
          y=w(nm)
          g=rv1(nm)
          h=rv1(k)
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y)
          g=SQRT(f*f+one)
          f=((x-z)*(x+z)+h*(y/(f+SIGN(g,f))-h))/x
          c=one
          s=one
          DO 47 j=l,nm
            i=j+1
            g=rv1(i)
            y=w(i)
            h=s*g
            g=c*g
            z=SQRT(f*f+h*h)
            rv1(j)=z
            c=f/z
            s=h/z
            f=x*c+g*s
            g=-x*s+g*c
            h=y*s
            y=y*c
            DO 45 nm=1,n
              x=v(nm,j)
              z=v(nm,i)
              v(nm,j)=x*c+z*s
              v(nm,i)=-x*s+z*c
45          CONTINUE
            z=SQRT(f*f+h*h)
            w(j)=z
            IF (z.NE.zero) THEN
              z=one/z
              c=f*z
              s=h*z
            ENDIF
            f=c*g+s*y
            x=-s*g+c*y
            DO 46 nm=1,m
              y=a(nm,j)
              z=a(nm,i)
              a(nm,j)=y*c+z*s
              a(nm,i)=-y*s+z*c
46          CONTINUE
47        CONTINUE
          rv1(l)=zero
          rv1(k)=f
          w(k)=x
48      CONTINUE
3       CONTINUE
49    CONTINUE
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE SVBKSB (u,w,v,m,n,np,b,dataset,coeff,offset)
c
c -----   This subroutine is taken from the book Numerical Recipes by
c      W.H. Press et al., and is used by the routine PolynomialCalc to
c      calculate the final solution of the least-squares fit coefficients,
c      after negligible coefficients have been zeroed.
c
      INTEGER    i,j,n,m,np,jj,dataset,maxpointset,s2,maxdataset,
     *     maxpoints,offset
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,maxdataset=30,
     *     maxpointset=2000)
      REAL*8     u(maxpointset,np),w(np),v(np,np),tmp(maxpointset),zero,
     *     s
      REAL       b(s2),coeff(np,2)
c
      DATA  zero/0.0D0/
c
      DO 2 j=1,n
        s=zero
        IF (w(j).NE.zero) THEN
          DO 1 i=1,m
            s=s+u(i,j)*b(offset+i)
1         CONTINUE
          s=s/w(j)
        ENDIF
        tmp(j)=s
2     CONTINUE
      DO 4 j=1,n
        s=zero
        DO 3 jj=1,n
          s=s+v(j,jj)*tmp(jj)
3       CONTINUE
        coeff(j,1)=s
4     CONTINUE
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE SVDVar (v,m,np,w,stdev)
c
c -----   A subroutine taken from the book Numerical Recipes by W.H. Press
c      et al., required by the routine PolynomialCalc to obtain standard
c      deviations of the least-squares fit coefficients.
c
      INTEGER    i,m,np,k,ncoeff
      PARAMETER  (ncoeff=10)
      REAL*8     v(np,np),w(np),wti(ncoeff),zero,sum
      REAL       stdev(np,2)
c
      DATA  zero/0.0D0/
c
      DO 1 i=1,m
        wti(i)=zero
        IF (w(i).NE.zero) wti(i)=1.0/w(i)/w(i)
1     CONTINUE
      DO 3 i=1,m
        sum=zero
        DO 2 k=1,m
          sum=sum+v(i,k)*v(i,k)*wti(k)
2       CONTINUE
        stdev(i,2)=SQRT(sum)
3     CONTINUE
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE FourierCalc (offset,numpt,f1,f2,dataset,ok)
c
c -----   Calculate the Fourier transform smoothed data from x-coordinate
c      F1 to F2.  The method is paraphrased from the book Numerical Recipes
c      by W.H. Press et al.  The extra size for YFOUR is due to zero padding
c      out to the next largest power of 2.  For larger values for the number
c      of smoothing points (PTS), the resultant curve will become less
c      featureless as more high frequency 'noise' is removed.
c
      INTEGER    dataset,i,j,k,m,nmin,maxpoints,maxdataset,offset,
     *     numpt,maxnum,ierror,maxpointset
      PARAMETER  (maxpoints=20000,maxpointset=2000,maxdataset=30)
      INTEGER    maxfr(maxdataset)
      REAL       x(maxpoints),y(maxpoints),xfr(maxpoints),
     *     yfr(maxpoints),zero,one,pts,f1,f2,rn1,const,window,yn,y1,
     *     yfour(4*maxpointset)
      LOGICAL    ok
c
      COMMON  /XYDat/x,y,/FrDat/xfr,yfr,maxfr
c
      DATA    zero,one/0.0,1.0/
c
      ok=.FALSE.
      i=0
1     CONTINUE
        i=i+1
      IF (x(offset+i).LT.f1) GOTO 1
      j=i
2     CONTINUE
      IF (j+1.LE.numpt.AND.x(offset+j+1).LE.f2) THEN
        j=j+1
        GOTO 2
      ENDIF
      maxnum=j-i+1
      maxfr(dataset)=maxnum
      IF (maxnum.LT.2) THEN
        WRITE (*,'(1X,A1,(A),(A))') 7,'Error in Fourier Calculation',
     *       ' routine!'
        RETURN
      ENDIF
      f1=x(offset+i)
      f2=x(offset+j)
      IF (f1.GT.f2) CALL Swap (f1,f2)
      DO 3 k=0,maxnum-1
        yfour(2*k+1)=y(offset+i+k)
        yfour(2*k+2)=zero
        xfr(offset+k+1)=x(offset+i+k)
3     CONTINUE
      WRITE (*,'(''0'',(A),I4,(A),I3,(A),$)') 'Number of smoothing '//
     *     'points to use [ n <',maxnum,' - ',maxnum/10,' is good ]: '
      READ (*,*,ERR=1000,IOSTAT=ierror) pts
      IF (pts.GE.maxnum) THEN
        WRITE (*,'(1X,A1,(A),(A),I3)') 7,'Error in input - number of ',
     *       'points must be less than ',maxnum
        GOTO 3
      ENDIF
      nmin=maxnum+2.0*pts
      m=2
4     IF (m.LE.nmin) THEN
        m=2*m
        GOTO 4
      ENDIF
      const=pts/m*pts/m
      y1=yfour(1)
      yn=yfour(2*maxnum-1)
      rn1=one/(maxnum-one)
      DO 5 j=1,maxnum
        yfour(2*j-1)=yfour(2*j-1)-rn1*(y1*(maxnum-j)+yn*(j-1))
5     CONTINUE
      DO 6 j=2*maxnum+1,2*m
        yfour(j)=zero
6     CONTINUE
      CALL Four1 (yfour,2*m,1)
c
c -----   The FFT is multiplied by a low band pass filter of characteristic
c      size PTS.  PTS > 2 will eliminate some high frequencies completely.
c
      DO 7 j=1,m/2-1
        window=AMAX1(zero,one-const*j*j)
        k=2*j+1
        yfour(k)=yfour(k)*window
        yfour(k+1)=yfour(k+1)*window
        k=2*(m-j)+1
        yfour(k)=yfour(k)*window
        yfour(k+1)=yfour(k+1)*window
7     CONTINUE
      yfour(m+1)=zero
      yfour(m+2)=zero
      CALL Four1 (yfour,2*m,-1)
      DO 8 j=1,maxnum
        yfr(offset+j)=yfour(2*j-1)/m+rn1*(y1*(maxnum-j)+yn*(j-1))
8     CONTINUE
      ok=.TRUE.
      RETURN
c
c -----   Error checking/error flag resetting routine.
c
1000  CONTINUE
      IF (ierror.GT.0) CALL ERRTST (ierror,j)
      WRITE (*,100) 7,7,'***  Error in input - redo!  ***'
100   FORMAT ('0',2A1,(A))
      GOTO 3
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE Four1 (data,nn,isign)
c
c -----   A FFT subroutine needed for the subroutine FourierCalc.  This
c      routine is taken exactly from the book Numerical Recipes by
c      W.H. Press et al.
c
      REAL*8     wr,wi,wpr,wpi,wtemp,theta,twopi,two
      INTEGER    nn,isign,j,n,i,istep,mmax,m
      REAL       data(nn),tempi,tempr
c
      DATA  two,twopi/2.0D0,6.28318530717958647692D0/
c
      n=nn
      j=1
      DO 2 i=1,n,2
        IF (j.GT.i) THEN
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        ENDIF
        m=n/2
1       IF ((m.ge.2).AND.(j.GT.m)) THEN
          j=j-m
          m=m/2
          GOTO 1
        ENDIF
        j=j+m
2     CONTINUE
      mmax=2
3     IF (n.GT.mmax) THEN
        istep=2*mmax
        theta=twopi*isign*mmax
        wpr=-two*DSIN(theta/two)**2
        wpi=DSIN(theta)
        wr=1.0D0
        wi=0.0D0
        DO 5 m=1,mmax,2
          DO 4 i=m,n,istep
            j=i+mmax
            tempr=SNGL(wr)*data(j)-SNGL(wi)*data(j+1)
            tempi=SNGL(wr)*data(j+1)+SNGL(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
4         CONTINUE
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
5       CONTINUE
        mmax=istep
      GOTO 3
      ENDIF
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE SplineCalc (offset,numpt,f1,f2,dataset,ok)
c
c -----   Calculates the cubic spline fit from x-coordinates F1 to F2.
c      The routine algorithm is derived from Borland's TURBO Graphix
c      Toolbox along with the equations given in Numerical Recipes by
c      W.H. Press et al.
c
      INTEGER    dataset,n,j,i,k,maxpoints,s2,maxdataset,offset,numpt,
     *     maxnum,maxpointset
      PARAMETER  (maxpoints=20000,s2=8*maxpoints,maxdataset=30,
     *     maxpointset=2000)
      INTEGER    maxspl(maxdataset)
      REAL       x(maxpoints),y(maxpoints),xp(maxpointset),Spline2,
     *     yp(maxpointset),y2prime(maxpoints),xspl(s2),yspl(s2),f1,f2,
     *     deltax
      LOGICAL    ok
c
      COMMON  /XYDat/x,y,/SplDat/xspl,yspl,maxspl
c
      ok=.TRUE.
      i=0
1     CONTINUE
        i=i+1
      IF (x(offset+i).LT.f1) GOTO 1
      j=i
2     CONTINUE
      IF (j+1.LE.numpt.AND.x(offset+j+1).LE.f2) THEN
        j=j+1
        GOTO 2
      ENDIF
      n=j-i+1
      IF (n.LT.2) THEN
        WRITE (*,'(1X,A1,(A))') 7,'Error in Spline Calculation routine!'
        ok=.FALSE.
        RETURN
      ENDIF
      f1=x(offset+i)
      f2=x(offset+j)
      IF (f1.GT.f2) CALL Swap (f1,f2)
      maxnum=8*n
      maxspl(dataset)=maxnum
      DO 3 k=1,n
        xp(k)=x(offset+i+k-1)
        yp(k)=y(offset+i+k-1)
3     CONTINUE
      CALL Spline1 (xp,yp,y2prime,n)
      deltax=(f2-f1)/(maxnum-1)
      DO 4 k=1,maxnum-1
        xspl(8*offset+k)=f1+(k-1)*deltax
4     CONTINUE
      xspl(8*offset+maxnum)=f2
      DO 5 k=1,maxnum
        yspl(8*offset+k)=Spline2(xspl(8*offset+k),xp,yp,y2prime,n)
5     CONTINUE
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      SUBROUTINE Spline1 (x,y,y2prime,n)
c
c -----   A subroutine needed for the cubic spline calculations.  It is
c      an algorithm derived from the cubic spline equations given in
c      Numerical Recipes by W.H. Press et al.  The boundary condition
c      of zero first derivative at the endpoints is assumed here.
c
      INTEGER    n,i,maxpointset
      PARAMETER  (maxpointset=2000)
      REAL       x(n),y(n),y2prime(n)
      REAL*8     a(2:maxpointset),b(2:maxpointset-1),dummy(maxpointset)
c
      a(2)=DBLE(x(2))-x(1)
      a(3)=DBLE(x(3))-x(2)
      b(2)=2.0D0*(DBLE(x(3))-x(1))+a(2)/2.0D0
      dummy(2)=6.0D0*(DBLE(y(3))-y(2))/a(3)-3.0D0*(DBLE(y(2))-y(1))/a(2)
      DO 1 i=3,n-1
        a(i+1)=DBLE(x(i+1))-x(i)
        b(i)=2.0D0*(DBLE(x(i+1))-x(i-1))-a(i)*a(i)/b(i-1)
        dummy(i)=6.0D0*((DBLE(y(i+1))-y(i))/a(i+1)-
     *     (DBLE(y(i))-y(i-1))/a(i))-a(i)*dummy(i-1)/b(i-1)
1     CONTINUE
      dummy(n)=(dummy(n-1)/b(n-1)-6.0D0*(DBLE(y(n))-y(n-1))/a(n)/a(n))/
     *         (2.0D0+a(n)/b(n-1))
      DO 2 i=n-1,2,-1
        dummy(i)=(dummy(i)-a(i+1)*dummy(i+1))/b(i)
2     CONTINUE
      dummy(1)=dummy(2)*0.5D0-3.0D0*(DBLE(y(2))-y(1))/a(2)/a(2)
      DO 3 i=1,n
        y2prime(i)=SNGL(dummy(i))
3     CONTINUE
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      OPTIONS /G_FLOATING
c
      FUNCTION Spline2 (x,xp,yp,y2prime,n)
c
c -----   A subroutine needed for the cubic spline calculations.  It is
c      an algorithm derived from the eqautions given in Numerical Recipes
c      by W.H. Press et al.
c
      INTEGER  n,j
      REAL     xp(n),yp(n),y2prime(n),x,Spline2
      REAL*8   dx,a,b,c,d
c
      IF (x.LE.xp(1)) THEN
        Spline2=yp(1)
      ELSE IF (x.GE.xp(n)) THEN
        Spline2=yp(n)
      ELSE
        j=1
1       CONTINUE
        IF ((x.GT.xp(j)).AND.(x.LE.xp(j+1))) GOTO 2
          j=j+1
          GOTO 1
2       CONTINUE
        dx=DBLE(xp(j+1))-xp(j)
        a=(DBLE(xp(j+1))-x)/dx
        b=1.0D0-a
        c=(a*a*a-a)*dx*dx/6.0D0
        d=(b*b*b-b)*dx*dx/6.0D0
        Spline2=a*yp(j)+b*yp(j+1)+c*y2prime(j)+d*y2prime(j+1)
      ENDIF
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE ReadData (dataset,xstart,x1,y1,x2,y2,fileopen,
     *     totalpoints)
c
c -----   This subroutine reads in all data files, sets the plotting limits,
c     and counts the number of points read in.  ReadData asks if more data is
c     to be read from the current open data file.  If not, it will prompt for
c     a filename until a correct one is entered.  ReadData then prompts for the
c     number of lines to skip from the current position in the file,
c     where N >= 0 simply skips that number of lines and reads the data,
c           N = -1 reads a line, prints it, and confirms the skip,
c           N < -1 skips ABS(N) lines and prompts again for lines to skip,
c           'Bn' backs up n line(s) (not case-sensitive!).
c     Finally, the data is read in, after prompting for the columns for the
c     (x,y) input.  Up to 25 columns are accessible from a single data file.
c     A data set ends when a non-numeric line is run into, or the file ends.
c
      INTEGER    maxpoints,maxdataset,dataset,ierror,i,j,xcol,ycol,
     *     lineskip,totalpoints,maxcolumn,ii,offset,maxcol,maxpointset,
     *     linesread,StrLen
      PARAMETER  (maxpoints=20000,maxdataset=30,maxpointset=2000,
     *     maxcolumn=25)
      REAL       xdata(maxpointset),ydata(maxpointset),x(maxpoints),
     *     y(maxpoints),x1,x2,y1,y2,one,zero,data(maxcolumn),
     *     xminmax(2,maxdataset),yminmax(2,maxdataset)
      INTEGER    xstart(maxdataset,2)
      CHARACTER  fname*40,blank*40,ans*1,linein*500,UpCase*1,longans*6
      LOGICAL    fileopen
c
      DATA    blank/'                                        '/
      DATA    zero,one/0.0,1.0/
      SAVE    fname,linesread
      COMMON  /XYDat/x,y,xdata,ydata,xminmax,yminmax
c
1     CONTINUE
      IF (fileopen) THEN
        WRITE (*,*)
        WRITE (*,98) 'More data from the same file [y] ? '
        READ (*,97) ans
        IF (UpCase(ans).EQ.'N') THEN
          CLOSE (10)
          fileopen=.FALSE.
          linesread=0
        ENDIF
      ENDIF
      IF (.NOT.fileopen) THEN
        WRITE (*,*)
        WRITE (*,98) 'Input the full name of the data file '//
     *       '(^Z to quit): '
2       CONTINUE
        fname=blank
        READ (*,97,END=11) fname
        OPEN (UNIT=10,FILE=fname,STATUS='old',IOSTAT=ierror)
        IF (ierror.NE.0) THEN
          WRITE (*,'(A1,A20,(A))') 7,' Error opening file ',fname
          WRITE (*,*)
          WRITE (*,98) 'Input the correct data file name: '
          GOTO 2
        ENDIF
        fileopen=.TRUE.
        linesread=0
      ENDIF
      ii=1
3     CONTINUE
      WRITE (*,300)
300   FORMAT ('0Skip n lines before data is read'/,'  (n=-1, show ',
     *     'each line and confirm skip  --  n<-1, skip ABS(n) lines ',
     *     'and prompt)'/,'  (Bn, go backwards n lines and prompt --',
     *     ' otherwise skip n lines and continue on)')
      WRITE (*,98) 'Skip how many lines ? '
      READ (*,97) longans
      ans=UpCase(longans(1:1))
      IF (ans.EQ.'B') THEN
        READ (longans(2:6),*,ERR=1000,END=33,IOSTAT=ierror) lineskip
        IF (lineskip.LE.1) lineskip=1
33      CONTINUE
        REWIND (10,ERR=3)
        linesread=MAX(linesread-lineskip-1,0)
        DO 34 i=1,linesread
          READ (10,*)
34      CONTINUE
        GOTO 3
      ELSE
        IF (ans.EQ.' ') GOTO 3
        READ (longans,*,ERR=1000,IOSTAT=ierror) lineskip
      ENDIF
      IF (lineskip.EQ.-1) THEN
4       CONTINUE
        READ (10,97,END=55) linein
        linesread=linesread+1
        WRITE (*,*) linein(1:StrLen(linein))
        WRITE (*,98) 'Skip line (Bn = n lines backwards)  [y] ? '
        READ (*,97) longans
        ans=UpCase(longans)
        IF (ans.EQ.'B') THEN
          READ (longans(2:6),*,ERR=465,END=465,IOSTAT=ierror) lineskip
          IF (lineskip.LE.1) lineskip=1
465       CONTINUE
          IF (ierror.GT.0) CALL ERRTST (ierror,j)
          linesread=MAX(linesread-lineskip-1,0)
          REWIND (10)
          DO 47 i=1,linesread
            READ (10,*)
47        CONTINUE
48        CONTINUE
          GOTO 4
        ENDIF
        IF (ans.NE.'N') GOTO 4
        BACKSPACE 10
        linesread=linesread-1
      ELSE
        DO 5 i=1,ABS(lineskip)
          READ (10,*,END=55)
5       CONTINUE
        linesread=linesread+ABS(lineskip)
        IF (lineskip.LT.0) GOTO 3
        GOTO 6
55      CONTINUE
        i=0
        GOTO 8
      ENDIF
6     CONTINUE
      ii=2
      WRITE (*,*)
      WRITE (*,98) 'Choose the columns for the x,y data (1 - 25): '
      READ (*,*,ERR=1000,IOSTAT=ierror) xcol,ycol
      maxcol=MAX(xcol,ycol)
      IF ((maxcol.GT.maxcolumn).OR.(xcol.EQ.ycol)) GOTO 6
      ii=3
      DO 7 i=1,MIN(maxpoints-totalpoints,maxpointset)
        linesread=linesread+1
        READ (10,97,ERR=1000,END=8,IOSTAT=ierror) linein
        READ (linein,*,ERR=1000,END=65,IOSTAT=ierror)
     *       (data(j),j=1,maxcol)
        xdata(i)=data(xcol)
        ydata(i)=data(ycol)
        GOTO 7
65      CONTINUE
        WRITE (*,96) 7,7,'***  ERROR  -  Incorrect number of '//
     *       'columns  -  Re-enter requested columns  ***'
        IF (i.EQ.1) GOTO 66
        WRITE (*,*) 'Input line number: ',linesread
        WRITE (*,*) linein
        WRITE (*,98) 'Ignore error - continue on  [n] ?: '
        READ (*,97) ans
        IF (UpCase(ans).EQ.'Y') GOTO 7
66      CONTINUE
        BACKSPACE (10)
        linesread=linesread-1
        GOTO 6
7     CONTINUE
8     CONTINUE
      CLOSE (10)
      fileopen=.FALSE.
      linesread=0
9     CONTINUE
c
c -----   General initialization routines
c
      IF (i-1.LE.0) THEN
        WRITE (*,96) 7,7,'*** ERROR  -  No points read from data '//
     *       'file  ***'
        GOTO 1
      ENDIF
      dataset=dataset+1
      IF (dataset.EQ.1) THEN
        xstart(dataset,1)=1
      ELSE
        xstart(dataset,1)=xstart(dataset-1,1)+xstart(dataset-1,2)
      ENDIF
      xstart(dataset,2)=i-1
      totalpoints=totalpoints+i-1
      WRITE (*,*) i-1,' data points read'
      xminmax(1,dataset)=1E35
      xminmax(2,dataset)=-1E35
      yminmax(1,dataset)=1E35
      yminmax(2,dataset)=-1E35
      offset=xstart(dataset,1)-1
      DO 10 i=1,xstart(dataset,2)
        x(i+offset)=xdata(i)
        xminmax(1,dataset)=MIN(xminmax(1,dataset),xdata(i))
        xminmax(2,dataset)=MAX(xminmax(2,dataset),xdata(i))
        x1=MIN(x1,xdata(i))
        x2=MAX(x2,xdata(i))
        y(i+offset)=ydata(i)
        yminmax(1,dataset)=MIN(yminmax(1,dataset),ydata(i))
        yminmax(2,dataset)=MAX(yminmax(2,dataset),ydata(i))
        y1=MIN(y1,ydata(i))
        y2=MAX(y2,ydata(i))
10    CONTINUE
      CALL SetLimits (x1,x2)
      CALL SetLimits (y1,y2)
11    CONTINUE
      IF (.NOT.fileopen) WRITE (*,96) 7,7,'***  No more data in '
     *     //fname(1:StrLen(fname))//' - file is closed  ***'
      IF (dataset.NE.0) RETURN
      WRITE (*,98) 'No datasets have been entered.  Try [a]gain or'//
     *     ' [s]top  [A]: '
      READ (*,97) ans
      IF (UpCase(ans).NE.'S') GOTO 1
      STOP
96    FORMAT ('0',2A1,(A))
97    FORMAT ((A))
98    FORMAT (1X,(A),$)
c
c -----   Error checking/error flag resetting routine.
c
1000  CONTINUE
      IF (ierror.GT.0) CALL ERRTST (ierror,j)
      IF (ii.EQ.3) THEN
        GOTO 9
      ELSE
        WRITE (*,97) 7,7,'***  Error in input - redo!  ***'
        GOTO (3,6),ii
      ENDIF
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE SetLimits (lo,hi)
c
c -----   This subroutine sets the X-Y plotting limits anytime that new
c     data is input or logs/antilogs are calculated from the data.  It
c     is meant to be accurate to 2 significant figures.
c
      REAL       oldlo,oldhi,lo,hi,flo,fhi,zero,one,loglo,loghi,
     *     mantlo,manthi
      INTEGER    ordlo,ordhi
      LOGICAL    neglo,neghi
c
      DATA  zero/0.0/,one/1.0/
c
      oldlo=lo
      oldhi=hi
      neglo=(oldlo.LT.zero)
      neghi=(oldhi.LT.zero)
      loglo=zero
      loghi=zero
      IF (oldlo.NE.zero) THEN
        loglo=LOG10(ABS(oldlo))
        ordlo=INT(loglo)
        mantlo=loglo-ordlo+one
        IF (mantlo.LT.one) THEN
          mantlo=mantlo+one
          ordlo=ordlo-one
        ENDIF
        flo=10.0**mantlo
        IF (ABS(flo-NINT(flo)).GT.0.1) THEN
          flo=INT(flo)
          IF (neglo) flo=flo+one
        ENDIF
        lo=SIGN(flo*10.0**(ordlo-1),lo)
      ENDIF
      IF (oldhi.NE.zero) THEN
        loghi=LOG10(ABS(oldhi))
        ordhi=INT(loghi)
        manthi=loghi-ordhi+one
        IF (manthi.LT.one) THEN
          manthi=manthi+one
          ordhi=ordhi-one
        ENDIF
        fhi=10.0**manthi
        IF (ABS(fhi-NINT(fhi)).GT.0.1) THEN
          fhi=INT(fhi)
          IF (.NOT.neghi) fhi=fhi+one
        ENDIF
        hi=SIGN(fhi*10.0**(ordhi-1),hi)
      ENDIF
      IF ((loglo-loghi.GE.2.0).AND.(oldlo.NE.zero)) THEN
        hi=zero
      ELSE IF ((loghi-loglo.GE.2.0).AND.(oldhi.NE.zero)) THEN
        lo=zero
      ENDIF
      IF (ABS(lo-hi).LT.1.E-4*lo) THEN
        lo=lo*.999
        hi=hi*1.001
      ENDIF
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE Swap (f1,f2)
c
c -----     Swaps the values F1 and F2.
c
      REAL  f1,f2,temp
c
      temp=f1
      f1=f2
      f2=temp
      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE ScaleDataSet (x1,x2,y1,y2,fits,xstart,dataset)
c
c -----   Slide/Expand/Difference the chosen data sets.  This only acts on
c     the raw data, NOT on the various possible fits.  The fits are turned
c     off for the data set affected.
c
      INTEGER    maxpoints,maxpointset,maxdataset,ii,i,ChooseDataSet,
     *     offset,cdataset,dataset,ierror,m,ds1,ds2,start1,start2,
     *     finish1,finish2,less,more,point
      PARAMETER  (maxpoints=20000,maxdataset=30,maxpointset=2000)
      REAL       x(maxpoints),y(maxpoints),xdata(maxpointset),
     *     ydata(maxpointset),xminmax(2,maxdataset),x1,x2,y1,y2,
     *     yminmax(2,maxdataset),r1,r2,r3,r4,slide,scale,diff,value
      INTEGER    xstart(maxdataset,2)
      CHARACTER  dset*2,UpCase*1,ans*2
      LOGICAL    ok,fits(5,maxdataset),f,add
c
      DATA  f/.FALSE./
c
      COMMON  /XYDat/x,y,xdata,ydata,xminmax,yminmax
c
1     CONTINUE
      WRITE (*,100)
100   FORMAT ('0Choose one of the following actions:'//10X,'[S]lide ',
     *     'a data set'/10X,'[E]xpand/shrink a data set'/10X,
     *     '[A]dd/subtract one data set from another'/10X,
     *     '[Q]uit and return to main menu'//' Choice  [q]: ',$)
      READ (*,97) ans
      IF (UpCase(ans).EQ.'S') GOTO 10
      IF (UpCase(ans).EQ.'E') GOTO 20
      IF (UpCase(ans).EQ.'A') GOTO 30
      RETURN
c
c -----   Slide the data set up/down and/or left/right
c
10    CONTINUE
      WRITE (*,'((A),I2)') '0Slide which dataset : 1 - ',dataset
      WRITE (*,98) '     0 to quit,  <ENTER> = current dataset: '
      READ (*,97,END=1) dset
      IF (dset(1:1).EQ.'0') GOTO 1
      cdataset=ChooseDataSet(dset,dataset,ok)
      IF (.NOT.ok) GOTO 10
      IF ((cdataset.GT.dataset).OR.(cdataset.LT.1)) THEN
        WRITE (*,99) 7,7,'Error in input.  Reenter data set number'
        GOTO 10
      ENDIF
      offset=xstart(cdataset,1)-1
      WRITE (*,*)
      WRITE (*,98) 'Do you wish to slide the data set horzontally  '//
     *     '[n]  ? '
      READ (*,97,END=1) ans
      IF (UpCase(ans).EQ.'Y') THEN
        WRITE (*,10000) xminmax(1,cdataset),xminmax(2,cdataset),x1,x2
        ii=1
12      CONTINUE
        WRITE (*,98) 'Enter the amount to slide (+ve = right, -ve'//
     *       ' = left): '
        READ (*,*,ERR=1000,IOSTAT=ierror,END=1) slide
        DO 13 i=1,xstart(cdataset,2)
          x(offset+i)=x(offset+i)+slide
13      CONTINUE
        xminmax(1,cdataset)=xminmax(1,cdataset)+slide
        xminmax(2,cdataset)=xminmax(2,cdataset)+slide
        DO 14 i=2,5
          fits(i,cdataset)=f
14      CONTINUE
      ENDIF
      WRITE (*,*)
      WRITE (*,98) 'Do you wish to slide the data set vertically  '//
     *     '[n]  ? '
      READ (*,97,END=1) ans
      IF (UpCase(ans).EQ.'Y') THEN
        WRITE (*,10001) yminmax(1,cdataset),yminmax(2,cdataset),y1,y2
        ii=2
15      CONTINUE
        WRITE (*,98) 'Enter the amount to slide (+ve = up, -ve'//
     *       ' = down): '
        READ (*,*,ERR=1000,IOSTAT=ierror,END=1) slide
        DO 16 i=1,xstart(cdataset,2)
          y(offset+i)=y(offset+i)+slide
16      CONTINUE
        yminmax(1,cdataset)=yminmax(1,cdataset)+slide
        yminmax(2,cdataset)=yminmax(2,cdataset)+slide
        DO 17 i=2,5
          fits(i,cdataset)=f
17      CONTINUE
      ENDIF
      GOTO 10
c
c -----   Expand/contract the data set in the x/y directions
c
20    CONTINUE
      WRITE (*,'((A),I2)') '0Scale which dataset : 1 - ',dataset
      WRITE (*,98) '     0 to quit,  <ENTER> = current dataset: '
      READ (*,97,END=1) dset
      IF (dset(1:1).EQ.'0') GOTO 1
      cdataset=ChooseDataSet(dset,dataset,ok)
      IF (.NOT.ok) GOTO 20
      IF ((cdataset.GT.dataset).OR.(cdataset.LT.1)) THEN
        WRITE (*,99) 7,7,'Error in input.  Reenter data set number'
        GOTO 20
      ENDIF
      offset=xstart(cdataset,1)-1
      WRITE (*,*)
      WRITE (*,98) 'Do you wish to scale the X range  [n]  ? '
      READ (*,97,END=1) ans
      IF (UpCase(ans).EQ.'Y') THEN
        WRITE (*,10000) xminmax(1,cdataset),xminmax(2,cdataset),x1,x2
        WRITE (*,10002)
        ii=3
21      CONTINUE
        WRITE (*,98) 'Conversion: '
        READ (*,*,ERR=1000,IOSTAT=ierror,END=1) r1,r2,r3,r4
        IF ((r1.EQ.r2).OR.(r3.EQ.r4)) GOTO 1000
        scale=(r4-r3)/(r2-r1)
        DO 22 i=1,xstart(cdataset,2)
          x(offset+i)=x(offset+i)*scale
22      CONTINUE
        xminmax(1,cdataset)=xminmax(1,cdataset)*scale
        xminmax(2,cdataset)=xminmax(2,cdataset)*scale
        DO 23 i=2,5
          fits(i,cdataset)=f
23      CONTINUE
      ENDIF
      WRITE (*,*)
      WRITE (*,98) 'Do you wish to scale the Y range  [n]  ? '
      READ (*,97,END=1) ans
      IF (UpCase(ans).EQ.'Y') THEN
        WRITE (*,10001) yminmax(1,cdataset),yminmax(2,cdataset),y1,y2
        WRITE (*,10002)
        ii=4
24      CONTINUE
        WRITE (*,98) 'Conversion: '
        READ (*,*,ERR=1000,IOSTAT=ierror,END=1) r1,r2,r3,r4
        IF ((r1.EQ.r2).OR.(r3.EQ.r4)) GOTO 1000
        scale=(r4-r3)/(r2-r1)
        DO 25 i=1,xstart(cdataset,2)
          y(offset+i)=y(offset+i)*scale
25      CONTINUE
        yminmax(1,cdataset)=yminmax(1,cdataset)*scale
        yminmax(2,cdataset)=yminmax(2,cdataset)*scale
        DO 26 i=2,5
          fits(i,cdataset)=f
26      CONTINUE
      ENDIF
      GOTO 20
c
c -----   Subtract/add one data set from/to another
c
30    CONTINUE
      WRITE (*,*)
      WRITE (*,98) '[A]dd, [S]ubtract, or [Q]uit  [q]: '
      READ (*,97,END=1) ans
      IF (ans.EQ.'qq') RETURN
      ans=UpCase(ans)
      IF (ans.EQ.'Q') GOTO 1
      add=(ans.EQ.'A')
      ii=5
31    CONTINUE
      WRITE (*,3100) dataset
3100  FORMAT (' Enter the dataset numbers (first +- second = newfirst)',
     *     '  [ 1 - ',I<INT(LOG10(FLOAT(dataset)))+1>,']: ',$)
      READ (*,*,ERR=1000,IOSTAT=ierror,END=1) ds1,ds2
      IF ((ds1.GT.dataset).OR.(ds2.GT.dataset).OR.(ds1.LT.1).OR.
     *     (ds2.LT.1).OR.(ds1.EQ.ds2)) GOTO 1000
      start1=xstart(ds1,1)
      start2=xstart(ds2,1)
      finish1=start1-1+xstart(ds1,2)
      finish2=start2-1+xstart(ds2,2)
32    CONTINUE
      IF (x(start1).LT.x(start2)) THEN
        start1=start1+1
        IF (start1.GT.finish1) GOTO 37
        GOTO 32
      ENDIF
33    CONTINUE
      IF (x(finish1).GT.x(finish2)) THEN
        finish1=finish1-1
        IF (start1.GT.finish1) GOTO 37
        GOTO 33
      ENDIF
c
c -----   Now, data set 1 is bracketed inside data set 2.  A linear
c     interpolation is used between two points from data set 2 to
c     correspond to the point in question from data set 1.
c
      more=start2+1
      DO 35 point=start1,finish1
34      CONTINUE
        IF (x(more).LT.x(point)) THEN
          more=more+1
          IF (more.GT.finish2) GOTO 38
          GOTO 34
        ENDIF
        less=more-1
        diff=x(more)-x(less)
        value=y(more)
        IF (diff.GT.0.0) value=value-(value-y(less))*(x(more)-x(point))/
     *       diff
        IF (add) value=-value
        y(point)=y(point)-value
35    CONTINUE
      DO 36 i=2,5
        fits(i,ds1)=f
36    CONTINUE
      GOTO 30
37    CONTINUE
      WRITE (*,'(''0'',2A1,(A),I3,A2,I3,(A))') 7,7,
     *     '***  Error.  Datasets',ds1,',',ds2,' do not overlap.  ***'
      GOTO 30
c
c -----   Should never actually get here
c
38    CONTINUE
      WRITE (*,99) 7,7,'***  Program error in adding/subtracting.  ***'
      GOTO 30
c
c -----   Error checking/error flag resetting routine
c
1000  CONTINUE
      WRITE (*,99) 7,7,'***  Error in input - redo!  ***'
      IF (ierror.GT.0) CALL ERRTST (ierror,m)
      GOTO (12,15,21,24,31),ii
97    FORMAT ((A))
98    FORMAT (1X,(A),$)
99    FORMAT ('0',2(A1),(A))
10000 FORMAT ('0Range of x-coordinate for this dataset is [ ',E10.3,
     *     ' to ',E10.3,' ]'/' Current plotting range of x-coordinate',
     *     ' is [ ',E10.3,' to ',E10.3,' ]'/)
10001 FORMAT ('0Range of y-coordinate for this dataset is [ ',E10.3,
     *     ' to ',E10.3,' ]'/' Current plotting range of y-coordinate',
     *     ' is [ ',E10.3,' to ',E10.3,' ]'/)
10002 FORMAT ('0Enter the value to be scaled by specifying a range ',
     *     'conversion:'//3X,'ex.  0 1 0 35.7'/8X,'converts the ',
     *     'old range from 0 to 1 to a new range from 0 to 35.7'/)
      END
c
c-------------------------------------------------------------------------------
c
c      SUBROUTINE Description
cc
cc -----   A short description of the programme CurveFit
cc
c      CHARACTER  ans*1
cc
c      WRITE (*,100)
c100   FORMAT ('0',32X,'CurveFit'//7X,'A general curve fitting routin',
c     *    'e with graphics for systems with PGPlot.'//5X,
c     *    'CurveFit will plot up to 30 data sets of up to 2000 points',
c     *    ' each,'/1X,'(to a maximum of 20000 points), saved as (xp,',
c     *    'yp) in ascending {xp}'/1X,'order.  It can also modify the ',
c     *    'data set, by flipping the x and/or y'/1X,
c     *    'axes, taking logarithms of any x and/or y data, and/or ',
c     *    'scaling any'/1X,'data set in the x and/or y directions.'//5X,
c     *    'In addition, CurveFit allows one to fit ',
c     *    'various types of curves to'/1X,'the data sets.  These ',
c     *    'include:'/7X,'(a)  straight line (connect the dots)'/7X,
c     *    '(b)  cubic spline'/7X,'(c)  smoothing (Bezier) polynomial',
c     *    /7X,'(d)  low-pass Fourier transform filter'/7X,'(e)  a ',
c     *    'best-fit polynomial of up to 10 terms'//5X,'CurveFit is ',
c     *    'menu driven and allows one to display any intermediary'/1X,
c     *    'results.  The x,y plotting ranges are user determined, as',
c     *    ' is the fitting'/1X,'range (over the x coordinate).  Plots',
c     *    ' can be made on any device that is'/1X,'supported by PG',
c     *    'Plot.'////1X,'Press <return> to continue',$)
c      READ (*,'((A))') ans
c      WRITE (*,'(''1'')')
c      RETURN
c      END
