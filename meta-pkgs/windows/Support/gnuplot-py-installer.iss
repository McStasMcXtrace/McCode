[Setup]
AppId={{ED5E8A40-8013-4209-979C-6D8452E45C55}
AppName=gnuplot-py-installer
AppVersion=1.0
AppVerName=1.0
AppPublisher=McStasMcXtrace
AppPublisherURL=mcstas.org
DefaultDirName=gnuplot-py
OutputBaseFilename=gnuplot-py-installer
OutputDir=.
DisableReadyPage=yes
DisableDirPage=yes
DisableFinishedPage=yes

[Files]
Source: "gnuplot-py-1.8\*"; DestDir: "{app}"; Flags: recursesubdirs;

[Run]
Filename: "python.exe"; Parameters: "setup.py build"; WorkingDir: "{app}"; Flags: shellexec runascurrentuser; Description: "build"
Filename: "python.exe"; Parameters: "setup.py install"; WorkingDir: "{app}"; Flags: shellexec runascurrentuser; Description: "install"
