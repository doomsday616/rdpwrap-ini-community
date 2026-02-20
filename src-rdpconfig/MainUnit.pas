{
  Copyright 2017 Stas'M Corp.
  Copyright 2024-2026 RDP Wrapper Community Edition contributors

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  Ported from Delphi VCL to Lazarus LCL for community CI builds.
}

unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ExtCtrls, Registry;

type
  FILE_VERSION = record
    Version: record case Boolean of
      True: (dw: DWORD);
      False: (w: record
        Minor, Major: Word;
      end;)
    end;
    Release, Build: Word;
    bDebug, bPrerelease, bPrivate, bSpecial: Boolean;
  end;

  WTS_SESSION_INFOW = record
    SessionId: DWORD;
    Name: packed array [0..33] of WideChar;
    State: DWORD;
  end;
  WTS_SESSION = array[0..0] of WTS_SESSION_INFOW;
  PWTS_SESSION_INFOW = ^WTS_SESSION;

  { TMainForm }
  TMainForm = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    bApply: TButton;
    bLicense: TButton;
    cbSingleSessionPerUser: TCheckBox;
    cbAllowTSConnections: TCheckBox;
    cbHideUsers: TCheckBox;
    cbCustomPrg: TCheckBox;
    gbDiag: TGroupBox;
    gbGeneral: TGroupBox;
    lListener: TLabel;
    lRDPPort: TLabel;
    lService: TLabel;
    lsListener: TLabel;
    lsService: TLabel;
    lsSuppVer: TLabel;
    lsTSVer: TLabel;
    lsWrapper: TLabel;
    lsWrapVer: TLabel;
    lTSVer: TLabel;
    lWrapper: TLabel;
    lWrapVer: TLabel;
    rgNLA: TRadioGroup;
    rgShadow: TRadioGroup;
    seRDPPort: TSpinEdit;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure cbAllowTSConnectionsClick(Sender: TObject);
    procedure seRDPPortChange(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure bLicenseClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ExecWait(Cmdline: String): Boolean;
    procedure ReadSettings;
    procedure WriteSettings;
  end;

const
  winstadll = 'winsta.dll';

  { Service manager / service access constants }
  SC_MANAGER_CONNECT       = $0001;
  SERVICE_QUERY_STATUS     = $0004;

  { Service state constants }
  SERVICE_STOPPED          = $00000001;
  SERVICE_START_PENDING    = $00000002;
  SERVICE_STOP_PENDING     = $00000003;
  SERVICE_RUNNING          = $00000004;
  SERVICE_CONTINUE_PENDING = $00000005;
  SERVICE_PAUSE_PENDING    = $00000006;
  SERVICE_PAUSED           = $00000007;

type
  SC_HANDLE = THandle;

var
  MainForm: TMainForm;
  Ready: Boolean = False;
  Arch: Byte;
  OldWow64RedirectionValue: LongBool;
  OldPort: Word;
  INI: String;

{ WinStation API }
function WinStationEnumerateW(hServer: THandle;
  var ppSessionInfo: PWTS_SESSION_INFOW; var pCount: DWORD): BOOL; stdcall;
  external winstadll name 'WinStationEnumerateW';
function WinStationFreeMemory(P: Pointer): BOOL; stdcall;
  external winstadll name 'WinStationFreeMemory';

{ Service Control Manager API }
function OpenSCManagerW(lpMachineName, lpDatabaseName: PWideChar;
  dwDesiredAccess: DWORD): SC_HANDLE; stdcall; external 'advapi32.dll';
function OpenServiceW(hSCManager: SC_HANDLE; lpServiceName: PWideChar;
  dwDesiredAccess: DWORD): SC_HANDLE; stdcall; external 'advapi32.dll';
function QueryServiceStatusEx(hService: SC_HANDLE; InfoLevel: DWORD;
  lpBuffer: Pointer; cbBufSize: DWORD; pcbBytesNeeded: PDWORD): BOOL; stdcall;
  external 'advapi32.dll' name 'QueryServiceStatusEx';
function CloseServiceHandle(hSCObject: SC_HANDLE): BOOL; stdcall;
  external 'advapi32.dll';

implementation

{$R *.lfm}

uses
  LicenseUnit;

function ExpandPath(Path: String): String;
var
  Buf: array[0..511] of WideChar;
begin
  Result := '';
  FillChar(Buf, SizeOf(Buf), 0);
  if Arch = 64 then
    Path := StringReplace(Path, '%ProgramFiles%', '%ProgramW6432%', [rfReplaceAll, rfIgnoreCase]);
  if ExpandEnvironmentStringsW(PWideChar(UnicodeString(Path)), @Buf[0], 512) > 0 then
    Result := String(WideString(Buf));
end;

function DisableWowRedirection: Boolean;
type
  TFunc = function(var Wow64FsEnableRedirection: LongBool): LongBool; stdcall;
var
  hMod: THandle;
  Wow64DisableWow64FsRedirection: TFunc;
begin
  Result := False;
  hMod := GetModuleHandle('kernel32');
  if hMod <> 0 then
    Pointer(Wow64DisableWow64FsRedirection) := GetProcAddress(hMod, 'Wow64DisableWow64FsRedirection')
  else
    Exit;
  if Wow64DisableWow64FsRedirection <> nil then
    Result := Wow64DisableWow64FsRedirection(OldWow64RedirectionValue);
end;

function RevertWowRedirection: Boolean;
type
  TFunc = function(var Wow64RevertWow64FsRedirection: LongBool): LongBool; stdcall;
var
  hMod: THandle;
  Wow64RevertWow64FsRedirection: TFunc;
begin
  Result := False;
  hMod := GetModuleHandle('kernel32');
  if hMod <> 0 then
    Pointer(Wow64RevertWow64FsRedirection) := GetProcAddress(hMod, 'Wow64RevertWow64FsRedirection')
  else
    Exit;
  if Wow64RevertWow64FsRedirection <> nil then
    Result := Wow64RevertWow64FsRedirection(OldWow64RedirectionValue);
end;

function GetFileVersion(const FileName: String; var FileVersion: FILE_VERSION): Boolean;
type
  { VS_FIXEDFILEINFO — define locally for portability }
  TVSFixedFileInfoLocal = packed record
    dwSignature: DWORD;
    dwStrucVersion: DWORD;
    dwFileVersionMS: DWORD;
    dwFileVersionLS: DWORD;
    dwProductVersionMS: DWORD;
    dwProductVersionLS: DWORD;
    dwFileFlagsMask: DWORD;
    dwFileFlags: DWORD;
    dwFileOS: DWORD;
    dwFileType: DWORD;
    dwFileSubtype: DWORD;
    dwFileDateMS: DWORD;
    dwFileDateLS: DWORD;
  end;
  VS_VERSIONINFO = record
    wLength, wValueLength, wType: Word;
    szKey: array[1..16] of WideChar;
    Padding1: Word;
    Value: TVSFixedFileInfoLocal;
    Padding2, Children: Word;
  end;
  PVS_VERSIONINFO = ^VS_VERSIONINFO;
const
  VFF_DEBUG = 1;
  VFF_PRERELEASE = 2;
  VFF_PRIVATE = 8;
  VFF_SPECIAL = 32;
  LOAD_LIBRARY_AS_DATAFILE_LOCAL = $00000002;
var
  hFile: HMODULE;
  hResourceInfo: HRSRC;
  VersionInfo: PVS_VERSIONINFO;
begin
  Result := False;

  hFile := LoadLibraryExW(PWideChar(UnicodeString(FileName)), 0, LOAD_LIBRARY_AS_DATAFILE_LOCAL);
  if hFile = 0 then
    Exit;

  hResourceInfo := FindResourceW(hFile, PWideChar(PtrUInt(1)), PWideChar(PtrUInt($10)));
  if hResourceInfo = 0 then begin
    FreeLibrary(hFile);
    Exit;
  end;

  VersionInfo := Pointer(LoadResource(hFile, hResourceInfo));
  if VersionInfo = nil then begin
    FreeLibrary(hFile);
    Exit;
  end;

  FileVersion.Version.dw := VersionInfo^.Value.dwFileVersionMS;
  FileVersion.Release := Word(VersionInfo^.Value.dwFileVersionLS shr 16);
  FileVersion.Build := Word(VersionInfo^.Value.dwFileVersionLS);
  FileVersion.bDebug := (VersionInfo^.Value.dwFileFlags and VFF_DEBUG) = VFF_DEBUG;
  FileVersion.bPrerelease := (VersionInfo^.Value.dwFileFlags and VFF_PRERELEASE) = VFF_PRERELEASE;
  FileVersion.bPrivate := (VersionInfo^.Value.dwFileFlags and VFF_PRIVATE) = VFF_PRIVATE;
  FileVersion.bSpecial := (VersionInfo^.Value.dwFileFlags and VFF_SPECIAL) = VFF_SPECIAL;

  FreeLibrary(hFile);
  Result := True;
end;

function IsWrapperInstalled(var WrapperPath: String): ShortInt;
var
  TermServiceHost,
  TermServicePath: String;
  Reg: TRegistry;
begin
  Result := -1;
  WrapperPath := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if not Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\TermService') then
      Exit;
    TermServiceHost := Reg.ReadString('ImagePath');
    Reg.CloseKey;
    if Pos('svchost.exe', LowerCase(TermServiceHost)) = 0 then begin
      Result := 2;
      Exit;
    end;
    if not Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\TermService\Parameters') then
      Exit;
    TermServicePath := Reg.ReadString('ServiceDll');
    Reg.CloseKey;
    if (Pos('termsrv.dll', LowerCase(TermServicePath)) = 0)
    and (Pos('rdpwrap.dll', LowerCase(TermServicePath)) = 0) then begin
      Result := 2;
      Exit;
    end;

    if Pos('rdpwrap.dll', LowerCase(TermServicePath)) > 0 then begin
      WrapperPath := TermServicePath;
      Result := 1;
    end else
      Result := 0;
  finally
    Reg.Free;
  end;
end;

function GetTermSrvState: ShortInt;
type
  SERVICE_STATUS_PROCESS = record
    dwServiceType,
    dwCurrentState,
    dwControlsAccepted,
    dwWin32ExitCode,
    dwServiceSpecificExitCode,
    dwCheckPoint,
    dwWaitHint,
    dwProcessId,
    dwServiceFlags: DWORD;
  end;
  PSERVICE_STATUS_PROCESS = ^SERVICE_STATUS_PROCESS;
const
  SvcName = 'TermService';
  SC_STATUS_PROCESS_INFO = 0;
var
  hSC: SC_HANDLE;
  hSvc: THandle;
  lpServiceStatusProcess: PSERVICE_STATUS_PROCESS;
  Buf: Pointer;
  cbBufSize, pcbBytesNeeded: Cardinal;
begin
  Result := -1;
  hSC := OpenSCManagerW(nil, nil, SC_MANAGER_CONNECT);
  if hSC = 0 then
    Exit;

  hSvc := OpenServiceW(hSC, PWideChar(UnicodeString(SvcName)), SERVICE_QUERY_STATUS);
  if hSvc = 0 then begin
    CloseServiceHandle(hSC);
    Exit;
  end;

  pcbBytesNeeded := 0;
  QueryServiceStatusEx(hSvc, SC_STATUS_PROCESS_INFO, nil, 0, @pcbBytesNeeded);

  cbBufSize := pcbBytesNeeded;
  GetMem(Buf, cbBufSize);

  if not QueryServiceStatusEx(hSvc, SC_STATUS_PROCESS_INFO, Buf, cbBufSize, @pcbBytesNeeded) then begin
    FreeMem(Buf, cbBufSize);
    CloseServiceHandle(hSvc);
    CloseServiceHandle(hSC);
    Exit;
  end else begin
    lpServiceStatusProcess := Buf;
    Result := ShortInt(lpServiceStatusProcess^.dwCurrentState);
  end;
  FreeMem(Buf, cbBufSize);
  CloseServiceHandle(hSvc);
  CloseServiceHandle(hSC);
end;

function IsListenerWorking: Boolean;
var
  pCount: DWORD;
  SessionInfo: PWTS_SESSION_INFOW;
  I: Integer;
begin
  Result := False;
  if not WinStationEnumerateW(0, SessionInfo, pCount) then
    Exit;
  for I := 0 to pCount - 1 do
    if SessionInfo^[I].Name = 'RDP-Tcp' then begin
      Result := True;
      Break;
    end;
  WinStationFreeMemory(SessionInfo);
end;

function ExtractLicenseText: String;
var
  LicFile: String;
  L: TStringList;
begin
  Result := '';
  { Community edition: read LICENSE file from application directory }
  LicFile := ExtractFilePath(ParamStr(0)) + 'LICENSE';
  if not FileExists(LicFile) then
    LicFile := ExtractFilePath(ParamStr(0)) + 'LICENSE.txt';
  if not FileExists(LicFile) then begin
    Result := 'License file not found.';
    Exit;
  end;
  L := TStringList.Create;
  try
    L.LoadFromFile(LicFile);
    Result := L.Text;
  finally
    L.Free;
  end;
end;

function TMainForm.ExecWait(Cmdline: String): Boolean;
var
  si: STARTUPINFOW;
  pi: PROCESS_INFORMATION;
  WideCmdLine: UnicodeString;
begin
  Result := False;
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;
  WideCmdLine := UnicodeString(Cmdline);
  UniqueString(WideCmdLine);
  if not CreateProcessW(nil, PWideChar(WideCmdLine), nil, nil, True, 0, nil, nil, @si, @pi) then begin
    MessageBoxW(Handle,
      PWideChar(UnicodeString('CreateProcess error (code: ' + IntToStr(GetLastError) + ').')),
      'Error', MB_ICONERROR or MB_OK);
    Exit;
  end;
  CloseHandle(pi.hThread);
  WaitForSingleObject(pi.hProcess, INFINITE);
  CloseHandle(pi.hProcess);
  Result := True;
end;

procedure TMainForm.ReadSettings;
var
  Reg: TRegistry;
  SecurityLayer, UserAuthentication: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Control\Terminal Server');
    try
      cbAllowTSConnections.Checked := not Reg.ReadBool('fDenyTSConnections');
    except
    end;
    try
      cbSingleSessionPerUser.Checked := Reg.ReadBool('fSingleSessionPerUser');
    except
    end;
    try
      cbCustomPrg.Checked := Reg.ReadBool('HonorLegacySettings');
    except
    end;
    Reg.CloseKey;

    Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp');
    seRDPPort.Value := 3389;
    try
      seRDPPort.Value := Reg.ReadInteger('PortNumber');
    except
    end;
    OldPort := seRDPPort.Value;
    SecurityLayer := 0;
    UserAuthentication := 0;
    try
      SecurityLayer := Reg.ReadInteger('SecurityLayer');
      UserAuthentication := Reg.ReadInteger('UserAuthentication');
    except
    end;
    if (SecurityLayer = 0) and (UserAuthentication = 0) then
      rgNLA.ItemIndex := 0;
    if (SecurityLayer = 1) and (UserAuthentication = 0) then
      rgNLA.ItemIndex := 1;
    if (SecurityLayer = 2) and (UserAuthentication = 1) then
      rgNLA.ItemIndex := 2;
    try
      rgShadow.ItemIndex := Reg.ReadInteger('Shadow');
    except
    end;
    Reg.CloseKey;
    Reg.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System');
    try
      cbHideUsers.Checked := Reg.ReadBool('dontdisplaylastusername');
    except
    end;
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

procedure TMainForm.WriteSettings;
var
  Reg: TRegistry;
  SecurityLayer, UserAuthentication: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server', True);
    try
      Reg.WriteBool('fDenyTSConnections', not cbAllowTSConnections.Checked);
    except
    end;
    try
      Reg.WriteBool('fSingleSessionPerUser', cbSingleSessionPerUser.Checked);
    except
    end;
    try
      Reg.WriteBool('HonorLegacySettings', cbCustomPrg.Checked);
    except
    end;
    Reg.CloseKey;

    Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp', True);
    try
      Reg.WriteInteger('PortNumber', seRDPPort.Value);
    except
    end;
    if OldPort <> seRDPPort.Value then begin
      OldPort := seRDPPort.Value;
      ExecWait('netsh advfirewall firewall set rule name="Remote Desktop" new localport=' + IntToStr(OldPort));
    end;
    case rgNLA.ItemIndex of
      0: begin
        SecurityLayer := 0;
        UserAuthentication := 0;
      end;
      1: begin
        SecurityLayer := 1;
        UserAuthentication := 0;
      end;
      2: begin
        SecurityLayer := 2;
        UserAuthentication := 1;
      end;
      else begin
        SecurityLayer := -1;
        UserAuthentication := -1;
      end;
    end;
    if SecurityLayer >= 0 then begin
      try
        Reg.WriteInteger('SecurityLayer', SecurityLayer);
        Reg.WriteInteger('UserAuthentication', UserAuthentication);
      except
      end;
    end;
    if rgShadow.ItemIndex >= 0 then begin
      try
        Reg.WriteInteger('Shadow', rgShadow.ItemIndex);
      except
      end;
    end;
    Reg.CloseKey;
    Reg.OpenKey('\SOFTWARE\Policies\Microsoft\Windows NT\Terminal Services', True);
    if rgShadow.ItemIndex >= 0 then begin
      try
        Reg.WriteInteger('Shadow', rgShadow.ItemIndex);
      except
      end;
    end;
    Reg.CloseKey;
    Reg.OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System', True);
    try
      Reg.WriteBool('dontdisplaylastusername', cbHideUsers.Checked);
    except
    end;
    Reg.CloseKey;
  finally
    Reg.Free;
  end;
end;

function CheckSupport(FV: FILE_VERSION): Byte;
var
  VerTxt: String;
begin
  Result := 0;
  if (FV.Version.w.Major = 6) and (FV.Version.w.Minor = 0) then
    Result := 1;
  if (FV.Version.w.Major = 6) and (FV.Version.w.Minor = 1) then
    Result := 1;
  VerTxt := Format('%d.%d.%d.%d',
    [FV.Version.w.Major, FV.Version.w.Minor, FV.Release, FV.Build]);
  if Pos('[' + VerTxt + ']', INI) > 0 then
    Result := 2;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
  WrapperPath, INIPath: String;
  FV: FILE_VERSION;
  L: TStringList;
  CheckSupp: Boolean;
begin
  CheckSupp := False;
  case IsWrapperInstalled(WrapperPath) of
    -1: begin
      lsWrapper.Caption := 'Unknown';
      lsWrapper.Font.Color := clGrayText;
    end;
    0: begin
      lsWrapper.Caption := 'Not installed';
      lsWrapper.Font.Color := clGrayText;
    end;
    1: begin
      lsWrapper.Caption := 'Installed';
      lsWrapper.Font.Color := clGreen;
      CheckSupp := True;
      INIPath := ExtractFilePath(ExpandPath(WrapperPath)) + 'rdpwrap.ini';
      if not FileExists(INIPath) then
        CheckSupp := False;
    end;
    2: begin
      lsWrapper.Caption := '3rd-party';
      lsWrapper.Font.Color := clRed;
    end;
  end;
  case GetTermSrvState of
    -1, 0: begin
      lsService.Caption := 'Unknown';
      lsService.Font.Color := clGrayText;
    end;
    SERVICE_STOPPED: begin
      lsService.Caption := 'Stopped';
      lsService.Font.Color := clRed;
    end;
    SERVICE_START_PENDING: begin
      lsService.Caption := 'Starting...';
      lsService.Font.Color := clGrayText;
    end;
    SERVICE_STOP_PENDING: begin
      lsService.Caption := 'Stopping...';
      lsService.Font.Color := clGrayText;
    end;
    SERVICE_RUNNING: begin
      lsService.Caption := 'Running';
      lsService.Font.Color := clGreen;
    end;
    SERVICE_CONTINUE_PENDING: begin
      lsService.Caption := 'Resuming...';
      lsService.Font.Color := clGrayText;
    end;
    SERVICE_PAUSE_PENDING: begin
      lsService.Caption := 'Suspending...';
      lsService.Font.Color := clGrayText;
    end;
    SERVICE_PAUSED: begin
      lsService.Caption := 'Suspended';
      lsService.Font.Color := clWindowText;
    end;
  end;
  if IsListenerWorking then begin
    lsListener.Caption := 'Listening';
    lsListener.Font.Color := clGreen;
  end else begin
    lsListener.Caption := 'Not listening';
    lsListener.Font.Color := clRed;
  end;
  if WrapperPath = '' then begin
    lsWrapVer.Caption := 'N/A';
    lsWrapVer.Font.Color := clGrayText;
  end else
    if not GetFileVersion(ExpandPath(WrapperPath), FV) then begin
      lsWrapVer.Caption := 'N/A';
      lsWrapVer.Font.Color := clGrayText;
    end else begin
      lsWrapVer.Caption :=
        IntToStr(FV.Version.w.Major)+'.'+
        IntToStr(FV.Version.w.Minor)+'.'+
        IntToStr(FV.Release)+'.'+
        IntToStr(FV.Build);
      lsWrapVer.Font.Color := clWindowText;
    end;
  if not GetFileVersion('termsrv.dll', FV) then begin
    lsTSVer.Caption := 'N/A';
    lsTSVer.Font.Color := clGrayText;
  end else begin
    lsTSVer.Caption :=
      IntToStr(FV.Version.w.Major)+'.'+
      IntToStr(FV.Version.w.Minor)+'.'+
      IntToStr(FV.Release)+'.'+
      IntToStr(FV.Build);
    lsTSVer.Font.Color := clWindowText;
    lsSuppVer.Visible := CheckSupp;
    if CheckSupp then begin
      if INI = '' then begin
        L := TStringList.Create;
        try
          L.LoadFromFile(INIPath);
          INI := L.Text;
        except
        end;
        L.Free;
      end;
      case CheckSupport(FV) of
        0: begin
          lsSuppVer.Caption := '[not supported]';
          lsSuppVer.Font.Color := clRed;
        end;
        1: begin
          lsSuppVer.Caption := '[supported partially]';
          lsSuppVer.Font.Color := clOlive;
        end;
        2: begin
          lsSuppVer.Caption := '[fully supported]';
          lsSuppVer.Font.Color := clGreen;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.bLicenseClick(Sender: TObject);
begin
  LicenseForm.mText.Text := ExtractLicenseText;
  if LicenseForm.ShowModal <> mrOk then
    Halt(0);
end;

procedure TMainForm.cbAllowTSConnectionsClick(Sender: TObject);
begin
  if Ready then
    bApply.Enabled := True;
end;

procedure TMainForm.seRDPPortChange(Sender: TObject);
begin
  if Ready then
    bApply.Enabled := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
type
  TGetNativeSystemInfo = procedure(var lpSystemInfo: TSystemInfo); stdcall;
var
  SI: TSystemInfo;
  hKernel: THandle;
  _GetNativeSystemInfo: TGetNativeSystemInfo;
begin
  FillChar(SI, SizeOf(SI), 0);
  hKernel := GetModuleHandle('kernel32');
  if hKernel <> 0 then begin
    Pointer(_GetNativeSystemInfo) := GetProcAddress(hKernel, 'GetNativeSystemInfo');
    if _GetNativeSystemInfo <> nil then
      _GetNativeSystemInfo(SI)
    else
      GetSystemInfo(@SI);
  end else
    GetSystemInfo(@SI);
  case SI.wProcessorArchitecture of
    0: Arch := 32;
    6: Arch := 64; // Itanium-based x64
    9: Arch := 64; // Intel/AMD x64
    else Arch := 0;
  end;
  if Arch = 64 then
    DisableWowRedirection;
  ReadSettings;
  Ready := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Arch = 64 then
    RevertWowRedirection;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if bApply.Enabled then
    CanClose := MessageBoxW(Handle, 'Settings are not saved. Do you want to exit?',
      'Warning', MB_ICONWARNING or MB_YESNO) = IDYES;
end;

procedure TMainForm.bOKClick(Sender: TObject);
begin
  if bApply.Enabled then begin
    WriteSettings;
    bApply.Enabled := False;
  end;
  Close;
end;

procedure TMainForm.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.bApplyClick(Sender: TObject);
begin
  WriteSettings;
  bApply.Enabled := False;
end;

end.
