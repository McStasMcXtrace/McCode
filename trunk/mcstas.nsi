; mcstas.nsi
;
; This script is based on example1.nsi, but it remember the directory, 
; has uninstall support and (optionally) installs start menu shortcuts.
;
; It will install example2.nsi into a directory that the user selects,

;--------------------------------

!ifndef VERSION
  !define VERSION "MCSTAS_VERSION"
!endif

; The name of the installer
Name "McStas"

; The file to write
OutFile "McStas-${VERSION}.exe"

; The default installation directory
InstallDir C:\McStas

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\McStas" "Install_Dir"

;--------------------------------

; Pages

Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install

Section "Perl 5.6 (optional, recommend)"
   File ActivePerl-5.6.1.635-MSWin32-x86.msi
   ExecWait "msiexec /i ActivePerl-5.6.1.635-MSWin32-x86.msi"
SectionEnd

Section "Dev-cpp (optional, recommend)"
   File devcpp4980.exe
   ExecWait "devcpp4980.exe"
SectionEnd

Section "Scilab (optional, recommend)"
   File scilab-4.0.exe
   ExecWait "scilab-4.0.exe"
SectionEnd

Section "Cortona VRML viewer (optional, recommend)"
   File cortvrml.exe
   ExecWait "cortvrml.exe"
SectionEnd

Section "McStas ${VERSION} (required)"
  File mcstas-${VERSION}-i686-Intel-Win32.zip
  ZipDLL::extractall "mcstas-${VERSION}-i686-Intel-Win32.zip" "."
  SectionIn RO
  
  ; Set output path to the installation directory.
  ;SetOutPath $INSTDIR

  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("NSIS", "AUTO").r0'
  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("DEVBIN", "c:\Dev-cpp\bin").r0'
  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("PERLBIN", "c:\perl\bin").r0'
  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("SCIBIN", "c:\progra~1\scilab-4.0\bin").r0'
  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("MCSTAS_SITE", "$INSTDIR").r0'
  System::Call 'Kernel32::SetEnvironmentVariableA(t, t) i("MCVERSION", "${VERSION}").r0'
      

  ; Execute batch installer
  ExecWait "mcstas-${VERSION}\install.bat"
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\McStas "Install_Dir" "$INSTDIR"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\McStas" "DisplayName" "McStas"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\McStas" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\McStas" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\McStas" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

  CreateDirectory "$SMPROGRAMS\McStas"
  CreateShortCut "$SMPROGRAMS\McStas\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\McStas\McStas (MakeNSISW).lnk" "$INSTDIR\mcstas-1.10beta.nsi" "" "$INSTDIR\mcstas-1.10beta.nsi" 0
  
SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\McStas"
  DeleteRegKey HKLM SOFTWARE\McStas

  ; Remove files and uninstaller
  Delete $INSTDIR\mcstas-1.10beta.nsi
  Delete $INSTDIR\uninstall.exe

  ; Remove shortcuts, if any
  Delete "$SMPROGRAMS\McStas\*.*"

  ; Remove directories used
  RMDir "$SMPROGRAMS\McStas"
  RMDir "$INSTDIR"

SectionEnd
