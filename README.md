# RDP Wrapper Library — Community Edition

English | [简体中文](README.zh-CN.md)

[![Release](https://img.shields.io/github/release/doomsday616/rdpwrap-ini-community.svg)](https://github.com/doomsday616/rdpwrap-ini-community/releases)
![License](https://img.shields.io/github/license/doomsday616/rdpwrap-ini-community.svg)
![Downloads](https://img.shields.io/github/downloads/doomsday616/rdpwrap-ini-community/total.svg)
![Environment](https://img.shields.io/badge/Windows-Vista%20|%207%20|%208%20|%2010%20|%2011-brightgreen.svg)
[![Telegram](https://img.shields.io/badge/chat-Telegram-blue.svg)](https://t.me/rdpwrap)

> **Community-maintained fork of [stascorp/rdpwrap](https://github.com/stascorp/rdpwrap).**
> This fork actively accepts pull requests and keeps `rdpwrap.ini` up to date for the latest Windows builds — including **Windows 11 25H2** and **26H1 Insider**.

---

## About

RDP Wrapper Library enables Remote Desktop Host support and concurrent RDP sessions on reduced-functionality Windows editions (Home, Starter, etc.) for home usage.

It works as a layer between the Service Control Manager and Terminal Services — the original `termsrv.dll` file remains **untouched**. This method is resilient against Windows Update.

*Original project by Stas'M (binarymaster), inspired by [Windows Product Policy Editor](http://forums.mydigitallife.info/threads/39411-Windows-Product-Policy-Editor) — big thanks to kost.*

## Why this fork?

The upstream repository [`stascorp/rdpwrap`](https://github.com/stascorp/rdpwrap) has not had a release since **December 2017** (v1.6.2). The `master` branch does not receive regular pushes, and support is community-driven on a per-`termsrv.dll`-build basis via issues.

This fork exists to:

- **Accept and merge pull requests** for INI updates and bug fixes
- **Maintain `rdpwrap.ini`** with support for the latest Windows builds
- **Provide ready-to-use release packages** with compiled binaries + updated INI
- **Build `rdpwrap.dll` from source** via GitHub Actions CI (MSVC, x86 + x64)

### Maintainers

| Role | Who |
|------|-----|
| Fork owner | [@doomsday616](https://github.com/doomsday616) |
| INI reviewer / reverse engineering | [@sjackson0109](https://github.com/sjackson0109) |

Want to contribute? See [CONTRIBUTING.md](CONTRIBUTING.md).

---

## Download & Install

### Quick start

1. Go to [**Releases**](https://github.com/doomsday616/rdpwrap-ini-community/releases) and download the latest `.zip`
2. Extract all files to a folder
3. Right-click **`install.bat`** → **Run as Administrator**

### Update INI file only

Right-click **`update.bat`** → **Run as Administrator**

### Uninstall

Right-click **`uninstall.bat`** → **Run as Administrator**

### Files in release package

| File | Description |
|------|-------------|
| `RDPWInst.exe` | Installer / uninstaller |
| `RDPCheck.exe` | Local RDP connection tester |
| `RDPConf.exe` | RDP Wrapper configuration UI |
| `rdpwrap.ini` | Build-specific configuration (community-maintained) |
| `install.bat` | Quick install script |
| `uninstall.bat` | Quick uninstall script |
| `update.bat` | Quick INI update script |

> **Note:** `RDPWInst.exe` and `RDPConf.exe` are built from source using Free Pascal / Lazarus (open-source Delphi alternatives). `RDPCheck.exe` is the original upstream v1.6.2 binary. `rdpwrap.dll` is built from the C++ source. The community-maintained `rdpwrap.ini` is the key file that adds support for new Windows builds.

---

## Key features

- RDP host on any Windows edition from Vista onward
- Console and remote sessions simultaneously
- Same user logged in locally and remotely (see RDPConf)
- Up to [15 concurrent sessions](https://github.com/stascorp/rdpwrap/issues/192) (depends on hardware/OS)
- Session shadowing via [Task Manager](http://cdn.freshdesk.com/data/helpdesk/attachments/production/1009641577/original/remote_control.png?1413476051) (Win 7) or [Remote Desktop Connection](http://woshub.com/rds-shadow-how-to-connect-to-a-user-session-in-windows-server-2012-r2/) (Win 8+)
- Full [multi-monitor support](https://github.com/stascorp/rdpwrap/issues/163)

---

## Attention

It is **strongly recommended** to have the original `termsrv.dll` file when using RDP Wrapper. If you have previously modified it with other patchers, the service may become unstable.

---

## FAQ

**Where do I download the binaries?**
From the [Releases](https://github.com/doomsday616/rdpwrap-ini-community/releases) page of this repository.

**Is it legal?**
There is no definitive answer — see [this discussion](https://github.com/stascorp/rdpwrap/issues/26).

**The installer tries to access the Internet — is this normal?**
Yes, it works in online mode by default. Remove the `-o` flag in `install.bat` to disable it.

**What is online install mode?**
Introduced in v1.6.1. On first install with `-o`, it downloads the [latest INI file](https://github.com/doomsday616/rdpwrap-ini-community/blob/main/res/rdpwrap.ini) from GitHub. See [#132](https://github.com/stascorp/rdpwrap/issues/132).

**What is the INI file?**
Introduced in v1.5. It stores wrapping settings and per-build patch data. When a new `termsrv.dll` build is released, support is added by updating the INI — no binary changes needed.

**Config Tool shows version 1.5 but I installed a newer version?**
Since v1.5, `rdpwrap.dll` itself has not changed — all configuration is in the INI file.

**Config Tool shows `[not supported]` — what do I do?**
1. Make sure you're online and run `update.bat`
2. If that doesn't help, check our [issues](https://github.com/doomsday616/rdpwrap-ini-community/issues) for your `termsrv.dll` build
3. If not listed, [open an issue](https://github.com/doomsday616/rdpwrap-ini-community/issues/new) with your build version

**Why can't RDPCheck change resolution?**
`RDPCheck` is only for quick testing. Use `mstsc.exe` (Microsoft Remote Desktop Client) for full settings. Connect to `127.0.0.1` or `127.0.0.2` for loopback.

---

## Known issues

- On Windows 8+ **tablet PCs**, inactive sessions may be logged out by the system — [#37](https://github.com/stascorp/rdpwrap/issues/37)
- On Windows 10, you can accidentally lock yourself out — [#50](https://github.com/stascorp/rdpwrap/issues/50)
- Does not work with **RemoteFX** enabled hosts — [#127](https://github.com/stascorp/rdpwrap/issues/127)
- `termsrv.dll` crash on logon — Windows Vista Starter RTM x86 (`6.0.6000.16386`)
- If Terminal Services hangs at startup, add `rdpwrap.dll` to your **antivirus exclusions**, or isolate with:
  ```
  sc config TermService type= own
  ```
- Some antivirus software (AVG, Norton) may remove RDP Wrapper — make sure you downloaded from [official releases](https://github.com/doomsday616/rdpwrap-ini-community/releases), then add to exclusions — [#191](https://github.com/stascorp/rdpwrap/issues/191)
- On Windows 10 Home after Creators Update, `rfxvmt.dll` may be missing causing `[not listening]` — v1.6.2 installer can auto-restore it, or see [#194](https://github.com/stascorp/rdpwrap/issues/194)

---

## Change log

### Community updates (this fork)

#### 2026-02-20 — v1.6.2-community.2
- Rebuilt repository from upstream with clean history
- Merged INI updates from [@sjackson0109](https://github.com/sjackson0109) (upstream PR [#4062](https://github.com/stascorp/rdpwrap/pull/4062)):
  - Added Windows 11 25H2 (`10.0.26100.x`) support
  - Added Windows 11 26H1 Insider (`10.0.28000.x`) support
  - Added latest Windows 10 builds (`10.0.19041.6456`, `10.0.19045.6466`)
  - Added patch code `mov_eax_1_nop_2`
- Added [reverse engineering guide](docs/HOW-TO-ADD-NEW-WINDOWS-BUILDS.md) for adding new Windows builds
- Upgraded build system to MSVC v143 (Visual Studio 2022) for GitHub Actions CI
- Ported `RDPWInst.exe` to Free Pascal — fully open-source build from source
- Ported `RDPConf.exe` to Lazarus/LCL — fully open-source build from source
- Added GitHub Actions workflow for automated builds (x86 + x64 DLL, FPC installer, Lazarus config tool)
- Added release packaging with upstream EXEs + community INI + freshly built DLL
- Added bilingual README (English + Chinese)
- Added governance: `CONTRIBUTING.md`, `.github/CODEOWNERS`

### Upstream history (stascorp/rdpwrap)

#### 2017-12-27 — v1.6.2
- Installer updated
- Include updated INI file for latest Windows builds
- Added check for supported Windows versions ([#155](https://github.com/stascorp/rdpwrap/issues/155))
- Added feature to take INI file from current directory ([#300](https://github.com/stascorp/rdpwrap/issues/300))
- Added feature to restore `rfxvmt.dll` (missing in Windows 10 Home — [#194](https://github.com/stascorp/rdpwrap/issues/194))
- RDP Config updated — added custom start programs ([#13](https://github.com/stascorp/rdpwrap/issues/13#issuecomment-77651843))
- MSI installation package added ([#14](https://github.com/stascorp/rdpwrap/issues/14))

#### 2016-08-01 — v1.6.1
- Include updated INI file for latest Windows builds
- Installer updated — added online install mode, keep settings on uninstall
- RDP Config updated — fixed firewall rule on port change, added hide-users-on-logon

#### 2015-08-12 — v1.6
- Added support for Windows 10
- INI file size reduced (comments moved to KB file)
- Installer updated — added workaround for error 1056, added update support
- RDP Checker updated — changed connect IP to `127.0.0.2`
- RDP Config updated — added all shadowing modes, writes to group policy

#### 2014-12-11 — v1.5
- Added INI config support — extend version support without rebuilding binaries
- Added support for Windows 8.1 + KB3000850, Windows 10 TP Update 2
- Added diagnostics feature to RDP Config

#### 2014-11-14 — v1.4
- Added support for Windows 10 Technical Preview Update 1
- Added support for Windows Vista SP2 / Windows 7 SP1 with KB3003743
- Added new RDP Configuration Program

#### 2014-10-21
- Installer: added System32 install, fixed NLA setting preservation
- RDP Checker: SecurityLayer/UserAuthentication values managed properly

#### 2014-10-20 — v1.3
- Added support for Windows 10 Technical Preview
- Added support for Windows 7 + KB2984972, Windows 8 + KB2973501
- Extended Vista (SP0/SP1/SP2) and Windows 7 (SP0/SP1) support
- Installer updated to v2.2, fixed Vista x64 path expansion

#### 2014-07-26 — v1.2
- Added support for Windows 8 DP/CP/RP/RTM, Windows 8.1 Preview/RTM
- Installer updated to v2.1

#### 2013-12-09
- C++ port by Fusix — x64 support added
- New command line installer v2.0 + local RDP checker

#### 2013-10-22 — v1.1
- Stable release — improved wrapper for internal SL Policy function
- Added Windows 8 Single Language support

#### 2013-10-19 — v1.0
- First beta — basic SL Policy wrapper

---

## Building from source

### C++ DLL (`rdpwrap.dll`)

The x86/x64 DLL is built with **Microsoft Visual Studio 2022** (or any version with MSVC v143 toolset).

```bash
# Using MSBuild (Visual Studio Developer Command Prompt)
msbuild src-x86-x64-Fusix/RDPWrap.sln /p:Configuration=Release /p:Platform=Win32
msbuild src-x86-x64-Fusix/RDPWrap.sln /p:Configuration=Release /p:Platform=x64
```

### Installer (`RDPWInst.exe`)

Built with [Free Pascal](https://www.freepascal.org/) (console application, no GUI dependencies).

```bash
fpc -Moobjfpc -Sh -O3 -XX -CX -WG -Twin32 -FEbin src-installer/RDPWInst.lpr
```

### Configuration tool (`RDPConf.exe`)

Built with [Lazarus](https://www.lazarus-ide.org/) (LCL GUI application).

```bash
lazbuild --build-mode=Release src-rdpconfig/RDPConf.lpi
```

### RDP Checker (`RDPCheck.exe`)

Uses the upstream v1.6.2 prebuilt binary. The source (`src-rdpcheck/`) requires Embarcadero Delphi with the Microsoft RDP ActiveX control (`TMsRdpClient2`), which cannot be easily ported to open-source toolchains.

### GitHub Actions CI

All builds are automated via [GitHub Actions](.github/workflows/build.yml). Every push to `main` and every tagged release triggers a full build cycle.

---

## Links

- **This repository:** https://github.com/doomsday616/rdpwrap-ini-community
- **Upstream repository:** https://github.com/stascorp/rdpwrap
- **Telegram chat:** https://t.me/rdpwrap
- **Discussion:** [Andrew Block .net](http://web.archive.org/web/20150810054558/http://andrewblock.net/enable-remote-desktop-on-windows-8-core/) · [MDL Forum](http://forums.mydigitallife.info/threads/55935-RDP-Wrapper-Library-(works-with-Windows-8.1-Basic))
- **Tutorial:** [How to find offsets for new termsrv.dll versions](http://www.youtube.com/watch?v=FiD86tmRBtk)

---

## Supported `termsrv.dll` versions

<details>
<summary>Click to expand full list (150+ builds)</summary>

#### Windows Vista / Server 2008 (6.0.x)
- `6.0.6000.16386` — Vista RTM
- `6.0.6001.18000` — Vista SP1
- `6.0.6002.18005` — Vista SP2
- `6.0.6002.19214` — Vista SP2 + KB3003743 (GDR)
- `6.0.6002.23521` — Vista SP2 + KB3003743 (LDR)

#### Windows 7 / Server 2008 R2 (6.1.x)
- `6.1.7600.16385` — Win 7 RTM
- `6.1.7600.20890` — Win 7 + KB2479710
- `6.1.7600.21316` — Win 7 + KB2750090
- `6.1.7601.17514` — Win 7 SP1
- `6.1.7601.21650` — Win 7 SP1 + KB2479710
- `6.1.7601.21866` — Win 7 SP1 + KB2647409
- `6.1.7601.22104` — Win 7 SP1 + KB2750090
- `6.1.7601.18540` — Win 7 SP1 + KB2984972 (GDR)
- `6.1.7601.22750` — Win 7 SP1 + KB2984972 (LDR)
- `6.1.7601.18637` — Win 7 SP1 + KB3003743 (GDR)
- `6.1.7601.22843` — Win 7 SP1 + KB3003743 (LDR)
- `6.1.7601.23403` — Win 7 SP1 + KB3125574
- `6.1.7601.24234` — Win 7 SP1 + KB4462923

#### Windows 8 / Server 2012 (6.2.x)
- `6.2.8102.0` — Win 8 Developer Preview
- `6.2.8250.0` — Win 8 Consumer Preview
- `6.2.8400.0` — Win 8 Release Preview
- `6.2.9200.16384` — Win 8 / Server 2012
- `6.2.9200.17048` — Win 8 + KB2973501 (GDR)
- `6.2.9200.21166` — Win 8 + KB2973501 (LDR)

#### Windows 8.1 / Server 2012 R2 (6.3.x)
- `6.3.9431.0` — Win 8.1 Preview
- `6.3.9600.16384` — Win 8.1 / Server 2012 R2
- `6.3.9600.17095` — Win 8.1 + KB2959626
- `6.3.9600.17415` — Win 8.1 + KB3000850
- `6.3.9600.18692` — Win 8.1 + KB4022720
- `6.3.9600.18708` — Win 8.1 + KB4025335
- `6.3.9600.18928` — Win 8.1 + KB4088876
- `6.3.9600.19093` — Win 8.1 + KB4343891

#### Windows 10 (10.0.x) — selected builds
- `10.0.10240.16384` — Win 10 RTM
- `10.0.10586.0` — Win 10 TH2
- `10.0.14393.0` — Win 10 RS1 (Anniversary Update)
- `10.0.15063.0` — Win 10 RS2 (Creators Update)
- `10.0.17134.1` — Win 10 RS4 (April 2018 Update)
- `10.0.17763.1` — Win 10 RS5 (October 2018 Update)
- `10.0.19041.6456` — Win 10 20H1 *(community-added)*
- `10.0.19045.6466` — Win 10 22H2 *(community-added)*
- ...and 100+ more intermediate builds (see `res/rdpwrap.ini` for the full list)

#### Windows 11 (10.0.26xxx+) — community-added
- `10.0.26100.x` — Windows 11 25H2
- `10.0.28000.x` — Windows 11 26H1 Insider

</details>

<details>
<summary>Confirmed working editions</summary>

- Windows Vista: Starter (SP1+), Home Basic, Home Premium, Business, Enterprise, Ultimate
- Windows Server 2008
- Windows 7: Starter, Home Basic, Home Premium, Professional, Enterprise, Ultimate
- Windows Server 2008 R2
- Windows 8: all editions including Single Language
- Windows Server 2012
- Windows 8.1: all editions including Connected (with Bing)
- Windows Server 2012 R2
- Windows 10: Home, Home SL, Pro, Enterprise
- Windows Server 2016 Technical Preview
- Windows 11: Home, Pro *(community-tested)*

</details>

---

## License

[Apache License 2.0](LICENSE)

This is an independent community fork. It is **not** an official release channel for `stascorp/rdpwrap`.
