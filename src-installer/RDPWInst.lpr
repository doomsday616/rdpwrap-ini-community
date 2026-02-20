{
  RDP Wrapper Library - Installer
  Copyright 2018 Stas'M Corp.
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

  Ported from Delphi to Free Pascal for community CI builds.
}

program RDPWInst;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows, Registry, WinSvc;

{ ---------- External API declarations ---------- }

function EnumServicesStatusExW(
  hSCManager: SC_HANDLE;
  InfoLevel, dwServiceType, dwServiceState: DWORD;
  lpServices: PByte;
  cbBufSize: DWORD;
  var pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  pszGroupName: PWideChar): BOOL; stdcall;
  external advapi32 name 'EnumServicesStatusExW';

function ConvertStringSidToSidW(
  StringSid: PWideChar;
  var Sid: PSID): BOOL; stdcall;
  external advapi32 name 'ConvertStringSidToSidW';

function SetEntriesInAclW(
  cCountOfExplicitEntries: ULONG;
  pListOfExplicitEntries: Pointer;
  OldAcl: PACL;
  var NewAcl: PACL): DWORD; stdcall;
  external advapi32 name 'SetEntriesInAclW';

function SetNamedSecurityInfoW(
  pObjectName: PWideChar;
  ObjectType: DWORD;
  SecurityInfo: DWORD;
  psidOwner: PSID;
  psidGroup: PSID;
  pDacl: PACL;
  pSacl: PACL): DWORD; stdcall;
  external advapi32 name 'SetNamedSecurityInfoW';

{ WinInet declarations }
const
  INTERNET_OPEN_TYPE_PRECONFIG = 0;
  INTERNET_FLAG_RELOAD = $80000000;

function InternetOpenW(lpszAgent: PWideChar; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PWideChar; dwFlags: DWORD): THandle; stdcall;
  external 'wininet.dll' name 'InternetOpenW';

function InternetOpenUrlW(hInternet: THandle; lpszUrl: PWideChar;
  lpszHeaders: PWideChar; dwHeadersLength, dwFlags, dwContext: DWORD): THandle; stdcall;
  external 'wininet.dll' name 'InternetOpenUrlW';

function InternetReadFile(hFile: THandle; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall;
  external 'wininet.dll' name 'InternetReadFile';

function InternetCloseHandle(hInternet: THandle): BOOL; stdcall;
  external 'wininet.dll' name 'InternetCloseHandle';

{ ---------- Type declarations ---------- }

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

  SERVICE_STATUS_PROCESS = packed record
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

  ENUM_SERVICE_STATUS_PROCESS = packed record
    lpServiceName,
    lpDisplayName: PWideChar;
    ServiceStatusProcess: SERVICE_STATUS_PROCESS;
  end;
  PENUM_SERVICE_STATUS_PROCESS = ^ENUM_SERVICE_STATUS_PROCESS;

  { EXPLICIT_ACCESS structure for ACL manipulation }
  EXPLICIT_ACCESS_W = packed record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;
    grfInheritance: DWORD;
    Trustee: record
      pMultipleTrustee: Pointer;
      MultipleTrusteeOperation: DWORD;
      TrusteeForm: DWORD;
      TrusteeType: DWORD;
      ptstrName: Pointer;
    end;
  end;

const
  SC_ENUM_PROCESS_INFO = 0;
  TermService = 'TermService';

  { ACL constants }
  GENERIC_ALL = $10000000;
  GRANT_ACCESS = 1;
  NO_MULTIPLE_TRUSTEE = 0;
  SUB_CONTAINERS_AND_OBJECTS_INHERIT = 3;
  TRUSTEE_IS_SID = 0;
  TRUSTEE_IS_WELL_KNOWN_GROUP = 5;
  SE_FILE_OBJECT = 1;
  DACL_SECURITY_INFORMATION = 4;

var
  Installed: Boolean;
  Online: Boolean;
  WrapPath: String;
  Arch: Byte;
  OldWow64RedirectionValue: LongBool;
  TermServicePath: String;
  FV: FILE_VERSION;
  TermServicePID: DWORD;
  ShareSvc: array of String;
  sShareSvc: String;

{ ---------- Architecture detection ---------- }

function SupportedArchitecture: Boolean;
var
  SI: TSystemInfo;
begin
  GetNativeSystemInfo(@SI);
  case SI.wProcessorArchitecture of
    0: begin
      Arch := 32;
      Result := True;
    end;
    6: Result := False; // Itanium
    9: begin
      Arch := 64;
      Result := True;
    end;
    else Result := False;
  end;
end;

{ ---------- WoW64 filesystem redirection ---------- }

type
  TWow64RedirectFunc = function(var Value: LongBool): LongBool; stdcall;

function DisableWowRedirection: Boolean;
var
  hModule: THandle;
  Func: TWow64RedirectFunc;
begin
  Result := False;
  hModule := GetModuleHandle(kernel32);
  if hModule = 0 then Exit;
  Pointer(Func) := GetProcAddress(hModule, 'Wow64DisableWow64FsRedirection');
  if Func <> nil then
    Result := Func(OldWow64RedirectionValue);
end;

function RevertWowRedirection: Boolean;
var
  hModule: THandle;
  Func: TWow64RedirectFunc;
begin
  Result := False;
  hModule := GetModuleHandle(kernel32);
  if hModule = 0 then Exit;
  Pointer(Func) := GetProcAddress(hModule, 'Wow64RevertWow64FsRedirection');
  if Func <> nil then
    Result := Func(OldWow64RedirectionValue);
end;

{ ---------- Registry helpers ---------- }

procedure CheckInstall;
var
  Code: DWORD;
  TermServiceHost: String;
  Reg: TRegistry;
begin
  if Arch = 64 then
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if not Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\TermService') then
  begin
    Reg.Free;
    Code := GetLastError;
    Writeln('[-] OpenKeyReadOnly error (code ', Code, ').');
    Halt(Code);
  end;
  TermServiceHost := Reg.ReadString('ImagePath');
  Reg.CloseKey;
  if (Pos('svchost.exe', LowerCase(TermServiceHost)) = 0)
  and (Pos('svchost -k', LowerCase(TermServiceHost)) = 0) then
  begin
    Reg.Free;
    Writeln('[-] TermService is hosted in a custom application (BeTwin, etc.) - unsupported.');
    Writeln('[*] ImagePath: "', TermServiceHost, '".');
    Halt(ERROR_NOT_SUPPORTED);
  end;
  if not Reg.OpenKeyReadOnly('\SYSTEM\CurrentControlSet\Services\TermService\Parameters') then
  begin
    Reg.Free;
    Code := GetLastError;
    Writeln('[-] OpenKeyReadOnly error (code ', Code, ').');
    Halt(Code);
  end;
  TermServicePath := Reg.ReadString('ServiceDll');
  Reg.CloseKey;
  if (Pos('termsrv.dll', LowerCase(TermServicePath)) = 0)
  and (Pos('rdpwrap.dll', LowerCase(TermServicePath)) = 0) then
  begin
    Reg.Free;
    Writeln('[-] Another third-party TermService library is installed.');
    Writeln('[*] ServiceDll: "', TermServicePath, '".');
    Halt(ERROR_NOT_SUPPORTED);
  end;
  Reg.Free;
  Installed := Pos('rdpwrap.dll', LowerCase(TermServicePath)) > 0;
end;

{ ---------- Service management ---------- }

function SvcGetStart(SvcName: String): Integer;
var
  hSC: SC_HANDLE;
  hSvc: THandle;
  Code: DWORD;
  Buf: Pointer;
  cbBufSize, pcbBytesNeeded: Cardinal;
  lpServiceConfig: PQueryServiceConfig;
begin
  Result := -1;
  Writeln('[*] Checking ', SvcName, '...');
  hSC := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_CONNECT);
  if hSC = 0 then
  begin
    Code := GetLastError;
    Writeln('[-] OpenSCManager error (code ', Code, ').');
    Exit;
  end;

  hSvc := OpenService(hSC, PWideChar(WideString(SvcName)), SERVICE_QUERY_CONFIG);
  if hSvc = 0 then
  begin
    CloseServiceHandle(hSC);
    Code := GetLastError;
    Writeln('[-] OpenService error (code ', Code, ').');
    Exit;
  end;

  QueryServiceConfig(hSvc, nil, 0, @pcbBytesNeeded);
  cbBufSize := pcbBytesNeeded;
  GetMem(Buf, cbBufSize);

  if not QueryServiceConfig(hSvc, Buf, cbBufSize, @pcbBytesNeeded) then
  begin
    FreeMem(Buf, cbBufSize);
    CloseServiceHandle(hSvc);
    CloseServiceHandle(hSC);
    Code := GetLastError;
    Writeln('[-] QueryServiceConfig error (code ', Code, ').');
    Exit;
  end else begin
    lpServiceConfig := Buf;
    Result := Integer(lpServiceConfig^.dwStartType);
  end;
  FreeMem(Buf, cbBufSize);
  CloseServiceHandle(hSvc);
  CloseServiceHandle(hSC);
end;

procedure SvcConfigStart(SvcName: String; dwStartType: Cardinal);
var
  hSC: SC_HANDLE;
  hSvc: THandle;
  Code: DWORD;
begin
  Writeln('[*] Configuring ', SvcName, '...');
  hSC := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_CONNECT);
  if hSC = 0 then
  begin
    Code := GetLastError;
    Writeln('[-] OpenSCManager error (code ', Code, ').');
    Exit;
  end;

  hSvc := OpenService(hSC, PWideChar(WideString(SvcName)), SERVICE_CHANGE_CONFIG);
  if hSvc = 0 then
  begin
    CloseServiceHandle(hSC);
    Code := GetLastError;
    Writeln('[-] OpenService error (code ', Code, ').');
    Exit;
  end;

  if not ChangeServiceConfig(hSvc, SERVICE_NO_CHANGE, dwStartType,
    SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
  begin
    CloseServiceHandle(hSvc);
    CloseServiceHandle(hSC);
    Code := GetLastError;
    Writeln('[-] ChangeServiceConfig error (code ', Code, ').');
    Exit;
  end;
  CloseServiceHandle(hSvc);
  CloseServiceHandle(hSC);
end;

procedure SvcStart(SvcName: String);
var
  hSC: SC_HANDLE;
  hSvc: THandle;
  Code: DWORD;
  pch: PWideChar;
begin
  Writeln('[*] Starting ', SvcName, '...');
  hSC := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE, SC_MANAGER_CONNECT);
  if hSC = 0 then
  begin
    Code := GetLastError;
    Writeln('[-] OpenSCManager error (code ', Code, ').');
    Exit;
  end;

  hSvc := OpenService(hSC, PWideChar(WideString(SvcName)), SERVICE_START);
  if hSvc = 0 then
  begin
    CloseServiceHandle(hSC);
    Code := GetLastError;
    Writeln('[-] OpenService error (code ', Code, ').');
    Exit;
  end;

  pch := nil;
  if not StartService(hSvc, 0, pch) then
  begin
    Code := GetLastError;
    if Code = 1056 then
    begin
      Sleep(2000);
      if not StartService(hSvc, 0, pch) then
      begin
        CloseServiceHandle(hSvc);
        CloseServiceHandle(hSC);
        Writeln('[-] StartService error (code ', Code, ').');
        Exit;
      end;
    end else begin
      CloseServiceHandle(hSvc);
      CloseServiceHandle(hSC);
      Writeln('[-] StartService error (code ', Code, ').');
      Exit;
    end;
  end;
  CloseServiceHandle(hSvc);
  CloseServiceHandle(hSC);
end;

{ ---------- TermService process enumeration ---------- }

procedure CheckTermsrvProcess;
var
  hSC: SC_HANDLE;
  dwNeedBytes, dwReturnBytes, dwResumeHandle, Code: DWORD;
  Svc: array of ENUM_SERVICE_STATUS_PROCESS;
  I: Integer;
  Found, Started: Boolean;
  TermServiceName: String;
begin
  Started := False;
  repeat
    hSC := OpenSCManager(nil, SERVICES_ACTIVE_DATABASE,
      SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
    if hSC = 0 then
    begin
      Code := GetLastError;
      Writeln('[-] OpenSCManager error (code ', Code, ').');
      Halt(Code);
    end;

    dwResumeHandle := 0;
    SetLength(Svc, 1489);
    FillChar(Svc[0], SizeOf(ENUM_SERVICE_STATUS_PROCESS) * Length(Svc), 0);

    if not EnumServicesStatusExW(hSC, SC_ENUM_PROCESS_INFO, SERVICE_WIN32,
      SERVICE_STATE_ALL, @Svc[0],
      SizeOf(ENUM_SERVICE_STATUS_PROCESS) * Length(Svc),
      dwNeedBytes, dwReturnBytes, dwResumeHandle, nil) then
    begin
      Code := GetLastError;
      if Code <> ERROR_MORE_DATA then
      begin
        CloseServiceHandle(hSC);
        Writeln('[-] EnumServicesStatusEx error (code ', Code, ').');
        Halt(Code);
      end else begin
        SetLength(Svc, 5957);
        FillChar(Svc[0], SizeOf(ENUM_SERVICE_STATUS_PROCESS) * Length(Svc), 0);
        if not EnumServicesStatusExW(hSC, SC_ENUM_PROCESS_INFO, SERVICE_WIN32,
          SERVICE_STATE_ALL, @Svc[0],
          SizeOf(ENUM_SERVICE_STATUS_PROCESS) * Length(Svc),
          dwNeedBytes, dwReturnBytes, dwResumeHandle, nil) then
        begin
          CloseServiceHandle(hSC);
          Code := GetLastError;
          Writeln('[-] EnumServicesStatusEx error (code ', Code, ').');
          Halt(Code);
        end;
      end;
    end;
    CloseServiceHandle(hSC);

    Found := False;
    for I := 0 to Length(Svc) - 1 do
    begin
      if Svc[I].lpServiceName = nil then
        Break;
      if LowerCase(String(WideString(Svc[I].lpServiceName))) = LowerCase(TermService) then
      begin
        Found := True;
        TermServiceName := String(WideString(Svc[I].lpServiceName));
        TermServicePID := Svc[I].ServiceStatusProcess.dwProcessId;
        Break;
      end;
    end;

    if not Found then
    begin
      Writeln('[-] TermService not found.');
      Halt(ERROR_SERVICE_DOES_NOT_EXIST);
    end;

    if TermServicePID = 0 then
    begin
      if Started then
      begin
        Writeln('[-] Failed to set up TermService. Unknown error.');
        Halt(ERROR_SERVICE_NOT_ACTIVE);
      end;
      SvcConfigStart(TermService, SERVICE_AUTO_START);
      SvcStart(TermService);
      Started := True;
    end else begin
      Writeln('[+] TermService found (pid ', TermServicePID, ').');
      Break;
    end;
  until False;

  SetLength(ShareSvc, 0);
  for I := 0 to Length(Svc) - 1 do
  begin
    if Svc[I].lpServiceName = nil then
      Break;
    if Svc[I].ServiceStatusProcess.dwProcessId = TermServicePID then
      if String(WideString(Svc[I].lpServiceName)) <> TermServiceName then
      begin
        SetLength(ShareSvc, Length(ShareSvc) + 1);
        ShareSvc[Length(ShareSvc) - 1] := String(WideString(Svc[I].lpServiceName));
      end;
  end;

  sShareSvc := '';
  for I := 0 to Length(ShareSvc) - 1 do
    if sShareSvc = '' then
      sShareSvc := ShareSvc[I]
    else
      sShareSvc := sShareSvc + ', ' + ShareSvc[I];

  if sShareSvc <> '' then
    Writeln('[*] Shared services found: ', sShareSvc)
  else
    Writeln('[*] No shared services found.');
end;

{ ---------- Privilege and process management ---------- }

function AddPrivilege(SePriv: String): Boolean;
var
  hToken: THandle;
  SeNameValue: Int64;
  tkp: TOKEN_PRIVILEGES;
  ReturnLength: Cardinal;
  ErrorCode: Cardinal;
begin
  Result := False;
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES
    or TOKEN_QUERY, @hToken) then
  begin
    ErrorCode := GetLastError;
    Writeln('[-] OpenProcessToken error (code ', ErrorCode, ').');
    Exit;
  end;
  if not LookupPrivilegeValue(nil, PWideChar(WideString(SePriv)), SeNameValue) then
  begin
    ErrorCode := GetLastError;
    Writeln('[-] LookupPrivilegeValue error (code ', ErrorCode, ').');
    Exit;
  end;
  tkp.PrivilegeCount := 1;
  tkp.Privileges[0].Luid := SeNameValue;
  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
  if not AdjustTokenPrivileges(hToken, False, @tkp, SizeOf(tkp), nil, @ReturnLength) then
  begin
    ErrorCode := GetLastError;
    Writeln('[-] AdjustTokenPrivileges error (code ', ErrorCode, ').');
    Exit;
  end;
  Result := True;
end;

procedure KillProcess(PID: DWORD);
var
  hProc: THandle;
  Code: DWORD;
begin
  hProc := OpenProcess(PROCESS_TERMINATE, False, PID);
  if hProc = 0 then
  begin
    Code := GetLastError;
    Writeln('[-] OpenProcess error (code ', Code, ').');
    Halt(Code);
  end;
  if not TerminateProcess(hProc, 0) then
  begin
    CloseHandle(hProc);
    Code := GetLastError;
    Writeln('[-] TerminateProcess error (code ', Code, ').');
    Halt(Code);
  end;
  CloseHandle(hProc);
end;

{ ---------- Process execution ---------- }

function ExecWait(Cmdline: String): Boolean;
var
  si: STARTUPINFOW;
  pi: PROCESS_INFORMATION;
  WCmdline: WideString;
begin
  Result := False;
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  WCmdline := WideString(Cmdline);
  if not CreateProcessW(nil, PWideChar(WCmdline), nil, nil, True, 0, nil, nil, @si, @pi) then
  begin
    Writeln('[-] CreateProcess error (code: ', GetLastError, ').');
    Exit;
  end;
  CloseHandle(pi.hThread);
  WaitForSingleObject(pi.hProcess, INFINITE);
  CloseHandle(pi.hProcess);
  Result := True;
end;

{ ---------- Path helpers ---------- }

function ExpandPath(Path: String): String;
var
  Buf: array[0..511] of WideChar;
begin
  Result := '';
  FillChar(Buf, SizeOf(Buf), 0);
  if Arch = 64 then
    Path := StringReplace(Path, '%ProgramFiles%', '%ProgramW6432%', [rfReplaceAll, rfIgnoreCase]);
  if ExpandEnvironmentStringsW(PWideChar(WideString(Path)), Buf, 512) > 0 then
    Result := String(WideString(Buf));
end;

{ ---------- ServiceDll registry management ---------- }

procedure SetWrapperDll;
var
  Reg: TRegistry;
  Code: DWORD;
begin
  if Arch = 64 then
    Reg := TRegistry.Create(KEY_WRITE or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if not Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\TermService\Parameters', True) then
  begin
    Code := GetLastError;
    Writeln('[-] OpenKey error (code ', Code, ').');
    Halt(Code);
  end;
  try
    Reg.WriteExpandString('ServiceDll', WrapPath);
    if (Arch = 64) and (FV.Version.w.Major = 6) and (FV.Version.w.Minor = 0) then
      ExecWait('"' + ExpandPath('%SystemRoot%') + '\system32\reg.exe" add HKLM\SYSTEM\CurrentControlSet\Services\TermService\Parameters /v ServiceDll /t REG_EXPAND_SZ /d "' + WrapPath + '" /f');
  except
    Writeln('[-] WriteExpandString error.');
    Halt(ERROR_ACCESS_DENIED);
  end;
  Reg.CloseKey;
  Reg.Free;
end;

procedure ResetServiceDll;
var
  Reg: TRegistry;
  Code: DWORD;
begin
  if Arch = 64 then
    Reg := TRegistry.Create(KEY_WRITE or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if not Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\TermService\Parameters', True) then
  begin
    Code := GetLastError;
    Writeln('[-] OpenKey error (code ', Code, ').');
    Halt(Code);
  end;
  try
    Reg.WriteExpandString('ServiceDll', '%SystemRoot%\System32\termsrv.dll');
  except
    Writeln('[-] WriteExpandString error.');
    Halt(ERROR_ACCESS_DENIED);
  end;
  Reg.CloseKey;
  Reg.Free;
end;

{ ---------- File operations ---------- }

{ Community edition: copy files from the same directory instead of embedded resources }
function CopyFileFromDir(const SrcDir, FileName, DestPath: String): Boolean;
var
  SrcPath: String;
begin
  Result := False;
  SrcPath := IncludeTrailingPathDelimiter(SrcDir) + FileName;
  if not FileExists(SrcPath) then
  begin
    Writeln('[-] File not found: ', SrcPath);
    Exit;
  end;
  Result := CopyFileW(PWideChar(WideString(SrcPath)),
    PWideChar(WideString(DestPath)), False);
  if Result then
    Writeln('[+] Copied ', FileName, ' -> ', DestPath)
  else
    Writeln('[-] Failed to copy ', SrcPath, ' -> ', DestPath, ' (code ', GetLastError, ').');
end;

procedure GrantSidFullAccess(Path, SID: String);
var
  p_SID: PSID;
  pDACL: PACL;
  EA: EXPLICIT_ACCESS_W;
  Code, Res: DWORD;
begin
  p_SID := nil;
  if not ConvertStringSidToSidW(PWideChar(WideString(SID)), p_SID) then
  begin
    Code := GetLastError;
    Writeln('[-] ConvertStringSidToSid error (code ', Code, ').');
    Exit;
  end;
  FillChar(EA, SizeOf(EA), 0);
  EA.grfAccessPermissions := GENERIC_ALL;
  EA.grfAccessMode := GRANT_ACCESS;
  EA.grfInheritance := SUB_CONTAINERS_AND_OBJECTS_INHERIT;
  EA.Trustee.pMultipleTrustee := nil;
  EA.Trustee.MultipleTrusteeOperation := NO_MULTIPLE_TRUSTEE;
  EA.Trustee.TrusteeForm := TRUSTEE_IS_SID;
  EA.Trustee.TrusteeType := TRUSTEE_IS_WELL_KNOWN_GROUP;
  EA.Trustee.ptstrName := p_SID;

  pDACL := nil;
  Res := SetEntriesInAclW(1, @EA, nil, pDACL);
  if Res = ERROR_SUCCESS then
  begin
    if SetNamedSecurityInfoW(PWideChar(WideString(Path)), SE_FILE_OBJECT,
      DACL_SECURITY_INFORMATION, nil, nil, pDACL, nil) <> ERROR_SUCCESS then
    begin
      Code := GetLastError;
      Writeln('[-] SetNamedSecurityInfo error (code ', Code, ').');
    end;
    LocalFree(HLOCAL(pDACL));
  end else begin
    Writeln('[-] SetEntriesInAcl error (code ', Res, ').');
  end;
end;

function GitINIFile(var Content: String): Boolean;
const
  URL = 'https://raw.githubusercontent.com/doomsday616/rdpwrap-ini-community/main/res/rdpwrap.ini';
var
  NetHandle, UrlHandle: THandle;
  Buf: array[0..1023] of Byte;
  BytesRead: DWORD;
  Str: AnsiString;
begin
  Result := False;
  Content := '';
  NetHandle := InternetOpenW(PWideChar(WideString('RDP Wrapper Community Update')),
    INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if NetHandle = 0 then Exit;
  UrlHandle := InternetOpenUrlW(NetHandle, PWideChar(WideString(URL)), nil, 0,
    INTERNET_FLAG_RELOAD, 0);
  if UrlHandle = 0 then
  begin
    InternetCloseHandle(NetHandle);
    Exit;
  end;
  repeat
    InternetReadFile(UrlHandle, @Buf[0], SizeOf(Buf), BytesRead);
    SetString(Str, PAnsiChar(@Buf[0]), BytesRead);
    Content := Content + String(Str);
  until BytesRead = 0;
  InternetCloseHandle(UrlHandle);
  InternetCloseHandle(NetHandle);
  Result := True;
end;

procedure ExtractFiles;
var
  DllName, S: String;
  AppDir, DestDir: String;
  OnlineINI: TStringList;
begin
  DestDir := ExtractFilePath(ExpandPath(WrapPath));
  if not DirectoryExists(DestDir) then
  begin
    if ForceDirectories(DestDir) then
    begin
      Writeln('[+] Folder created: ', DestDir);
      GrantSidFullAccess(DestDir, 'S-1-5-18');  // Local System
      GrantSidFullAccess(DestDir, 'S-1-5-6');   // Service group
    end else begin
      Writeln('[-] ForceDirectories error.');
      Writeln('[*] Path: ', DestDir);
      Halt(0);
    end;
  end;

  AppDir := ExtractFilePath(ParamStr(0));

  { Handle INI file }
  if Online then
  begin
    Writeln('[*] Downloading latest INI file...');
    OnlineINI := TStringList.Create;
    try
      if GitINIFile(S) then
      begin
        OnlineINI.Text := S;
        S := DestDir + 'rdpwrap.ini';
        OnlineINI.SaveToFile(S);
        Writeln('[+] Latest INI file -> ', S);
      end else begin
        Writeln('[-] Failed to get online INI file, using local copy.');
        Online := False;
      end;
    finally
      OnlineINI.Free;
    end;
  end;

  if not Online then
  begin
    { Try local rdpwrap.ini first }
    S := AppDir + 'rdpwrap.ini';
    if FileExists(S) then
    begin
      CopyFileFromDir(AppDir, 'rdpwrap.ini', DestDir + 'rdpwrap.ini');
    end else begin
      Writeln('[-] rdpwrap.ini not found in ', AppDir);
      Writeln('[*] You may need to download it manually.');
    end;
  end;

  { Copy the appropriate DLL }
  case Arch of
    32: DllName := 'rdpwrap_x86.dll';
    64: DllName := 'rdpwrap_x64.dll';
  end;

  { Try architecture-specific DLL first, fallback to generic rdpwrap.dll }
  if FileExists(AppDir + DllName) then
    CopyFileFromDir(AppDir, DllName, ExpandPath(WrapPath))
  else if FileExists(AppDir + 'rdpwrap.dll') then
    CopyFileFromDir(AppDir, 'rdpwrap.dll', ExpandPath(WrapPath))
  else
    Writeln('[-] DLL not found. Tried: ', DllName, ', rdpwrap.dll');
end;

procedure DeleteFiles;
var
  Code: DWORD;
  FullPath, Path: String;
begin
  FullPath := ExpandPath(TermServicePath);
  Path := ExtractFilePath(FullPath);

  if not DeleteFileW(PWideChar(WideString(Path + 'rdpwrap.ini'))) then
  begin
    Code := GetLastError;
    Writeln('[-] DeleteFile error (code ', Code, ').');
    Exit;
  end;
  Writeln('[+] Removed file: ', Path + 'rdpwrap.ini');

  if not DeleteFileW(PWideChar(WideString(FullPath))) then
  begin
    Code := GetLastError;
    Writeln('[-] DeleteFile error (code ', Code, ').');
    Exit;
  end;
  Writeln('[+] Removed file: ', FullPath);

  if not RemoveDirectoryW(PWideChar(WideString(ExtractFilePath(ExpandPath(TermServicePath))))) then
  begin
    Code := GetLastError;
    Writeln('[-] RemoveDirectory error (code ', Code, ').');
    Exit;
  end;
  Writeln('[+] Removed folder: ', ExtractFilePath(ExpandPath(TermServicePath)));
end;

{ ---------- File version ---------- }

function GetFileVersion(const FileName: String; var FileVersion: FILE_VERSION): Boolean;
type
  VS_VERSIONINFO = packed record
    wLength, wValueLength, wType: Word;
    szKey: array[1..16] of WideChar;
    Padding1: Word;
    Value: VS_FIXEDFILEINFO;
    Padding2, Children: Word;
  end;
  PVS_VERSIONINFO = ^VS_VERSIONINFO;
const
  VFF_DEBUG = 1;
  VFF_PRERELEASE = 2;
  VFF_PRIVATE = 8;
  VFF_SPECIAL = 32;
var
  hFile: HMODULE;
  hResourceInfo: HRSRC;
  VersionInfo: PVS_VERSIONINFO;
begin
  Result := False;
  hFile := LoadLibraryExW(PWideChar(WideString(FileName)), 0, LOAD_LIBRARY_AS_DATAFILE);
  if hFile = 0 then Exit;

  hResourceInfo := FindResourceW(hFile, PWideChar(MAKEINTRESOURCE(1)), PWideChar(MAKEINTRESOURCE($10)));
  if hResourceInfo = 0 then begin FreeLibrary(hFile); Exit; end;

  VersionInfo := Pointer(LoadResource(hFile, hResourceInfo));
  if VersionInfo = nil then begin FreeLibrary(hFile); Exit; end;

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

{ ---------- Version checking ---------- }

function ReadINIFile(const FilePath: String): String;
var
  SL: TStringList;
begin
  Result := '';
  SL := TStringList.Create;
  try
    if FileExists(FilePath) then
      SL.LoadFromFile(FilePath);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure CheckTermsrvVersion;
var
  SuppLvl: Byte;
  VerTxt, INIContent, INIPath: String;
  AppDir: String;
begin
  GetFileVersion(ExpandPath(TermServicePath), FV);
  VerTxt := Format('%d.%d.%d.%d',
    [FV.Version.w.Major, FV.Version.w.Minor, FV.Release, FV.Build]);
  Writeln('[*] Terminal Services version: ', VerTxt);

  if (FV.Version.w.Major = 5) and (FV.Version.w.Minor = 1) then
  begin
    Writeln('[!] Windows XP is not supported.');
    Exit;
  end;
  if (FV.Version.w.Major = 5) and (FV.Version.w.Minor = 2) then
  begin
    Writeln('[!] Windows Server 2003 is not supported.');
    Exit;
  end;

  SuppLvl := 0;
  if (FV.Version.w.Major = 6) and (FV.Version.w.Minor = 0) then
  begin
    SuppLvl := 1;
    if (Arch = 32) and (FV.Release = 6000) and (FV.Build = 16386) then
    begin
      Writeln('[!] This version of Terminal Services may crash on logon attempt.');
      Writeln('It''s recommended to upgrade to Service Pack 1 or higher.');
    end;
  end;
  if (FV.Version.w.Major = 6) and (FV.Version.w.Minor = 1) then
    SuppLvl := 1;

  { Check INI for full support: try local file first, then beside the EXE }
  AppDir := ExtractFilePath(ParamStr(0));
  INIPath := AppDir + 'rdpwrap.ini';
  if FileExists(INIPath) then
    INIContent := ReadINIFile(INIPath)
  else
    INIContent := '';

  if Pos('[' + VerTxt + ']', INIContent) > 0 then
    SuppLvl := 2;

  case SuppLvl of
    0: begin
      Writeln('[-] This version of Terminal Services is not supported.');
      Writeln('Try running "RDPWInst -w" to download latest INI file.');
    end;
    1: begin
      Writeln('[!] This version of Terminal Services is supported partially.');
      Writeln('It means you may have some limitations such as only 2 concurrent sessions.');
    end;
    2: Writeln('[+] This version of Terminal Services is fully supported.');
  end;
end;

procedure CheckTermsrvDependencies;
const
  CertPropSvc = 'CertPropSvc';
  SessionEnv = 'SessionEnv';
begin
  if SvcGetStart(CertPropSvc) = SERVICE_DISABLED then
    SvcConfigStart(CertPropSvc, SERVICE_DEMAND_START);
  if SvcGetStart(SessionEnv) = SERVICE_DISABLED then
    SvcConfigStart(SessionEnv, SERVICE_DEMAND_START);
end;

{ ---------- Terminal Server registry configuration ---------- }

procedure TSConfigRegistry(Enable: Boolean);
var
  Reg: TRegistry;
  Code: DWORD;
begin
  if Arch = 64 then
    Reg := TRegistry.Create(KEY_WRITE or KEY_WOW64_64KEY)
  else
    Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;

  if not Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server', True) then
  begin
    Code := GetLastError;
    Writeln('[-] OpenKey error (code ', Code, ').');
    Halt(Code);
  end;
  try
    Reg.WriteBool('fDenyTSConnections', not Enable);
  except
    Writeln('[-] WriteBool error.');
    Halt(ERROR_ACCESS_DENIED);
  end;
  Reg.CloseKey;

  if Enable then
  begin
    if not Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\Licensing Core', True) then
    begin
      Code := GetLastError;
      Writeln('[-] OpenKey error (code ', Code, ').');
      Halt(Code);
    end;
    try
      Reg.WriteBool('EnableConcurrentSessions', True);
    except
      Writeln('[-] WriteBool error.');
      Halt(ERROR_ACCESS_DENIED);
    end;
    Reg.CloseKey;

    if not Reg.OpenKey('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon', True) then
    begin
      Code := GetLastError;
      Writeln('[-] OpenKey error (code ', Code, ').');
      Halt(Code);
    end;
    try
      Reg.WriteBool('AllowMultipleTSSessions', True);
    except
      Writeln('[-] WriteBool error.');
      Halt(ERROR_ACCESS_DENIED);
    end;
    Reg.CloseKey;

    if not Reg.KeyExists('\SYSTEM\CurrentControlSet\Control\Terminal Server\AddIns') then
    begin
      Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\AddIns', True);
      Reg.CloseKey;
      Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\AddIns\Clip Redirector', True);
      try
        Reg.WriteString('Name', 'RDPClip');
        Reg.WriteInteger('Type', 3);
      except
        Writeln('[-] WriteInteger error.');
        Halt(ERROR_ACCESS_DENIED);
      end;
      Reg.CloseKey;
      Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\AddIns\DND Redirector', True);
      try
        Reg.WriteString('Name', 'RDPDND');
        Reg.WriteInteger('Type', 3);
      except
        Writeln('[-] WriteInteger error.');
        Halt(ERROR_ACCESS_DENIED);
      end;
      Reg.CloseKey;
      Reg.OpenKey('\SYSTEM\CurrentControlSet\Control\Terminal Server\AddIns\Dynamic VC', True);
      try
        Reg.WriteInteger('Type', -1);
      except
        Writeln('[-] WriteInteger error.');
        Halt(ERROR_ACCESS_DENIED);
      end;
      Reg.CloseKey;
    end;
  end;
  Reg.Free;
end;

procedure TSConfigFirewall(Enable: Boolean);
begin
  if Enable then
  begin
    ExecWait('netsh advfirewall firewall add rule name="Remote Desktop" dir=in protocol=tcp localport=3389 profile=any action=allow');
    ExecWait('netsh advfirewall firewall add rule name="Remote Desktop" dir=in protocol=udp localport=3389 profile=any action=allow');
  end else
    ExecWait('netsh advfirewall firewall delete rule name="Remote Desktop"');
end;

{ ---------- INI update ---------- }

function CheckINIDate(Filename, Content: String; var Date: Integer): Boolean;
var
  Str: TStringList;
  I: Integer;
begin
  Result := False;
  Str := TStringList.Create;
  try
    if Filename <> '' then
    begin
      try
        Str.LoadFromFile(Filename);
      except
        Writeln('[-] Failed to read INI file.');
        Exit;
      end;
    end else
      Str.Text := Content;

    I := 0;
    while I < Str.Count do
    begin
      if Pos('Updated=', Str[I]) = 1 then
        Break;
      Inc(I);
    end;
    if I >= Str.Count then
    begin
      Writeln('[-] Failed to check INI date.');
      Exit;
    end;

    Content := StringReplace(Str[I], 'Updated=', '', []);
    Content := StringReplace(Content, '-', '', [rfReplaceAll]);
  finally
    Str.Free;
  end;

  try
    Date := StrToInt(Content);
  except
    Writeln('[-] Wrong INI date format.');
    Exit;
  end;
  Result := True;
end;

procedure CheckUpdate;
var
  INIPath, S: String;
  Str: TStringList;
  I, OldDate, NewDate: Integer;
begin
  INIPath := ExtractFilePath(ExpandPath(TermServicePath)) + 'rdpwrap.ini';
  if not CheckINIDate(INIPath, '', OldDate) then
    Halt(ERROR_ACCESS_DENIED);
  Writeln('[*] Current update date: ',
    Format('%d.%.2d.%.2d', [OldDate div 10000, OldDate div 100 mod 100, OldDate mod 100]));

  if not GitINIFile(S) then
  begin
    Writeln('[-] Failed to download latest INI from GitHub.');
    Halt(ERROR_ACCESS_DENIED);
  end;
  if not CheckINIDate('', S, NewDate) then
    Halt(ERROR_ACCESS_DENIED);
  Writeln('[*] Latest update date:  ',
    Format('%d.%.2d.%.2d', [NewDate div 10000, NewDate div 100 mod 100, NewDate mod 100]));

  if NewDate = OldDate then
    Writeln('[*] Everything is up to date.')
  else if NewDate > OldDate then
  begin
    Writeln('[+] New update is available, updating...');
    CheckTermsrvProcess;

    Writeln('[*] Terminating service...');
    AddPrivilege('SeDebugPrivilege');
    KillProcess(TermServicePID);
    Sleep(1000);

    if Length(ShareSvc) > 0 then
      for I := 0 to Length(ShareSvc) - 1 do
        SvcStart(ShareSvc[I]);
    Sleep(500);

    Str := TStringList.Create;
    try
      Str.Text := S;
      try
        Str.SaveToFile(INIPath);
      except
        Writeln('[-] Failed to write INI file.');
        Halt(ERROR_ACCESS_DENIED);
      end;
    finally
      Str.Free;
    end;

    SvcStart(TermService);
    Writeln('[+] Update completed.');
  end else
    Writeln('[*] Your INI file is newer than public file. Are you a developer? :)');
end;

{ ---------- Main ---------- }

var
  I: Integer;
begin
  Writeln('RDP Wrapper Library v1.6.2');
  Writeln('Community Edition Installer v2.7');
  Writeln('Copyright (C) Stas''M Corp. 2018');
  Writeln('Copyright (C) Community Edition 2024-2026');
  Writeln('');

  if (ParamCount < 1)
  or (
    (ParamStr(1) <> '-l')
    and (ParamStr(1) <> '-i')
    and (ParamStr(1) <> '-w')
    and (ParamStr(1) <> '-u')
    and (ParamStr(1) <> '-r')
  ) then
  begin
    Writeln('USAGE:');
    Writeln('RDPWInst.exe [-l|-i[-s][-o]|-w|-u[-k]|-r]');
    Writeln('');
    Writeln('-l          display the license agreement');
    Writeln('-i          install wrapper to Program Files folder (default)');
    Writeln('-i -s       install wrapper to System32 folder');
    Writeln('-i -o       online install mode (loads latest INI file)');
    Writeln('-w          get latest update for INI file');
    Writeln('-u          uninstall wrapper');
    Writeln('-u -k       uninstall wrapper and keep settings');
    Writeln('-r          force restart Terminal Services');
    Exit;
  end;

  if ParamStr(1) = '-l' then
  begin
    { Read license from file in same directory }
    if FileExists(ExtractFilePath(ParamStr(0)) + 'LICENSE') then
    begin
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(ParamStr(0)) + 'LICENSE');
        Writeln(Text);
      finally
        Free;
      end;
    end else
      Writeln('License file not found.');
    Exit;
  end;

  if Win32MajorVersion < 6 then
  begin
    Writeln('[-] Unsupported Windows version:');
    Writeln('  only >= 6.0 (Vista, Server 2008 and newer) are supported.');
    Exit;
  end;

  if not SupportedArchitecture then
  begin
    Writeln('[-] Unsupported processor architecture.');
    Exit;
  end;

  CheckInstall;

  if ParamStr(1) = '-i' then
  begin
    if Installed then
    begin
      Writeln('[*] RDP Wrapper Library is already installed.');
      Halt(ERROR_INVALID_FUNCTION);
    end;
    Writeln('[*] Notice to user:');
    Writeln('  - By using all or any portion of this software, you are agreeing');
    Writeln('  to be bound by all the terms and conditions of the license agreement.');
    Writeln('  - To read the license agreement, run the installer with -l parameter.');
    Writeln('  - If you do not agree to any terms of the license agreement,');
    Writeln('  do not use the software.');

    Writeln('[*] Installing...');
    if ParamStr(2) = '-s' then
      WrapPath := '%SystemRoot%\system32\rdpwrap.dll'
    else
      WrapPath := '%ProgramFiles%\RDP Wrapper\rdpwrap.dll';

    if Arch = 64 then
      DisableWowRedirection;

    CheckTermsrvVersion;
    CheckTermsrvProcess;

    Writeln('[*] Extracting files...');
    Online := (ParamStr(2) = '-o') or (ParamStr(3) = '-o');
    ExtractFiles;

    Writeln('[*] Configuring service library...');
    SetWrapperDll;

    Writeln('[*] Checking dependencies...');
    CheckTermsrvDependencies;

    Writeln('[*] Terminating service...');
    AddPrivilege('SeDebugPrivilege');
    KillProcess(TermServicePID);
    Sleep(1000);

    if Length(ShareSvc) > 0 then
      for I := 0 to Length(ShareSvc) - 1 do
        SvcStart(ShareSvc[I]);
    Sleep(500);
    SvcStart(TermService);
    Sleep(500);

    Writeln('[*] Configuring registry...');
    TSConfigRegistry(True);
    Writeln('[*] Configuring firewall...');
    TSConfigFirewall(True);

    Writeln('[+] Successfully installed.');

    if Arch = 64 then
      RevertWowRedirection;
  end;

  if ParamStr(1) = '-u' then
  begin
    if not Installed then
    begin
      Writeln('[*] RDP Wrapper Library is not installed.');
      Halt(ERROR_INVALID_FUNCTION);
    end;
    Writeln('[*] Uninstalling...');

    if Arch = 64 then
      DisableWowRedirection;

    CheckTermsrvProcess;

    Writeln('[*] Resetting service library...');
    ResetServiceDll;

    Writeln('[*] Terminating service...');
    AddPrivilege('SeDebugPrivilege');
    KillProcess(TermServicePID);
    Sleep(1000);

    Writeln('[*] Removing files...');
    DeleteFiles;

    if Length(ShareSvc) > 0 then
      for I := 0 to Length(ShareSvc) - 1 do
        SvcStart(ShareSvc[I]);
    Sleep(500);
    SvcStart(TermService);
    Sleep(500);

    if ParamStr(2) <> '-k' then
    begin
      Writeln('[*] Configuring registry...');
      TSConfigRegistry(False);
      Writeln('[*] Configuring firewall...');
      TSConfigFirewall(False);
    end;

    if Arch = 64 then
      RevertWowRedirection;

    Writeln('[+] Successfully uninstalled.');
  end;

  if ParamStr(1) = '-w' then
  begin
    if not Installed then
    begin
      Writeln('[*] RDP Wrapper Library is not installed.');
      Halt(ERROR_INVALID_FUNCTION);
    end;
    Writeln('[*] Checking for updates...');
    CheckUpdate;
  end;

  if ParamStr(1) = '-r' then
  begin
    Writeln('[*] Restarting...');
    CheckTermsrvProcess;

    Writeln('[*] Terminating service...');
    AddPrivilege('SeDebugPrivilege');
    KillProcess(TermServicePID);
    Sleep(1000);

    if Length(ShareSvc) > 0 then
      for I := 0 to Length(ShareSvc) - 1 do
        SvcStart(ShareSvc[I]);
    Sleep(500);
    SvcStart(TermService);

    Writeln('[+] Done.');
  end;
end.
