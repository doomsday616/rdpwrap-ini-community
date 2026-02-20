{
  RDP Wrapper Library - Configuration Tool
  Copyright 2014 Stas'M Corp.
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

program RDPConf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainUnit,
  LicenseUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.Title := 'Remote Desktop Protocol Configuration';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TLicenseForm, LicenseForm);
  Application.Run;
end.
