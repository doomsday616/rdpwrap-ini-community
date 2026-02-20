# RDP Wrapper Library — 社区版

[English](README.md) | 简体中文

[![Release](https://img.shields.io/github/release/doomsday616/rdpwrap-ini-community.svg)](https://github.com/doomsday616/rdpwrap-ini-community/releases)
![License](https://img.shields.io/github/license/doomsday616/rdpwrap-ini-community.svg)
![Downloads](https://img.shields.io/github/downloads/doomsday616/rdpwrap-ini-community/total.svg)
![Environment](https://img.shields.io/badge/Windows-Vista%20|%207%20|%208%20|%2010%20|%2011-brightgreen.svg)
[![Telegram](https://img.shields.io/badge/chat-Telegram-blue.svg)](https://t.me/rdpwrap)

> **[stascorp/rdpwrap](https://github.com/stascorp/rdpwrap) 的社区维护分支。**
> 本分支积极接受 PR，持续更新 `rdpwrap.ini` 以支持最新 Windows 版本——包括 **Windows 11 25H2** 和 **26H1 Insider**。

---

## 简介

RDP Wrapper Library 可以在功能精简版 Windows（家庭版、入门版等）上启用远程桌面主机功能，并支持多用户并发 RDP 会话，适用于个人使用。

它作为服务控制管理器和终端服务之间的中间层工作——原始的 `termsrv.dll` 文件**不会被修改**。这种方式不受 Windows 更新影响。

*原始项目由 Stas'M (binarymaster) 开发，灵感来自 [Windows Product Policy Editor](http://forums.mydigitallife.info/threads/39411-Windows-Product-Policy-Editor)，感谢 kost。*

## 为什么要有这个分支？

上游仓库 [`stascorp/rdpwrap`](https://github.com/stascorp/rdpwrap) 自 **2017 年 12 月**（v1.6.2）以来没有新的发布。`master` 分支不再定期更新，新版本支持完全依赖社区在 issue 中逐个 `termsrv.dll` 版本提供。

本分支的目标：

- **接受并合并 PR** 以更新 INI 文件和修复问题
- **维护 `rdpwrap.ini`** 以支持最新 Windows 版本
- **提供可直接使用的发布包**，包含编译好的二进制文件 + 更新的 INI
- **从源码构建全部组件**——通过 GitHub Actions CI 实现（MSVC + Free Pascal + Lazarus）

### 维护者

| 角色 | 负责人 |
|------|--------|
| 分支所有者 | [@doomsday616](https://github.com/doomsday616) |
| INI 审核 / 逆向工程 | [@sjackson0109](https://github.com/sjackson0109) |

想参与贡献？请查看 [CONTRIBUTING.md](CONTRIBUTING.md)。

---

## 下载与安装

### 快速开始

1. 前往 [**Releases**](https://github.com/doomsday616/rdpwrap-ini-community/releases) 下载最新的 `.zip` 文件
2. 解压所有文件到一个文件夹
3. 右键点击 **`install.bat`** → **以管理员身份运行**

### 仅更新 INI 文件

右键点击 **`update.bat`** → **以管理员身份运行**

### 卸载

右键点击 **`uninstall.bat`** → **以管理员身份运行**

### 发布包文件说明

| 文件 | 说明 |
|------|------|
| `RDPWInst.exe` | 安装/卸载工具 |
| `RDPCheck.exe` | 本地 RDP 连接测试工具 |
| `RDPConf.exe` | RDP Wrapper 配置界面 |
| `rdpwrap.ini` | 针对特定版本的配置文件（社区维护） |
| `install.bat` | 快速安装脚本 |
| `uninstall.bat` | 快速卸载脚本 |
| `update.bat` | 快速更新 INI 脚本 |

> **说明：** `RDPWInst.exe` 和 `RDPConf.exe` 使用 Free Pascal / Lazarus（开源 Delphi 替代品）从源码编译。`RDPCheck.exe` 为上游 v1.6.2 原始二进制文件。`rdpwrap.dll` 从 C++ 源码编译。社区维护的 `rdpwrap.ini` 是添加新 Windows 版本支持的关键文件。

---

## 主要特性

- 在 Vista 及以后的所有 Windows 版本上启用 RDP 主机
- 控制台会话和远程会话可同时使用
- 同一用户可同时在本地和远程登录（参见 RDPConf）
- 支持最多 [15 个并发会话](https://github.com/stascorp/rdpwrap/issues/192)（取决于硬件和系统）
- 通过[任务管理器](http://cdn.freshdesk.com/data/helpdesk/attachments/production/1009641577/original/remote_control.png?1413476051)（Win 7）或[远程桌面连接](http://woshub.com/rds-shadow-how-to-connect-to-a-user-session-in-windows-server-2012-r2/)（Win 8+）进行会话投影
- 完整的[多显示器支持](https://github.com/stascorp/rdpwrap/issues/163)

---

## 注意事项

**强烈建议**使用 RDP Wrapper 时保持原始的 `termsrv.dll` 文件。如果你之前使用其他补丁工具修改过该文件，服务可能会变得不稳定。

---

## 常见问题

**从哪里下载二进制文件？**
从本仓库的 [Releases](https://github.com/doomsday616/rdpwrap-ini-community/releases) 页面下载。

**这合法吗？**
没有确定答案——参见[此讨论](https://github.com/stascorp/rdpwrap/issues/26)。

**安装程序会访问互联网，这正常吗？**
是的，默认以在线模式工作。如需禁用，删除 `install.bat` 中的 `-o` 参数。

**什么是在线安装模式？**
v1.6.1 引入的功能。首次安装时使用 `-o` 参数，安装程序会从 GitHub 下载[最新的 INI 文件](https://github.com/doomsday616/rdpwrap-ini-community/blob/main/res/rdpwrap.ini)。参见 [#132](https://github.com/stascorp/rdpwrap/issues/132)。

**INI 文件是什么？**
v1.5 引入的功能。它存储包装设置和针对特定版本的补丁数据。当新的 `termsrv.dll` 版本发布时，只需更新 INI 文件即可添加支持——无需更改二进制文件。

**配置工具显示版本 1.5，但我安装了更新的版本？**
自 v1.5 起，`rdpwrap.dll` 本身没有变化——所有配置都在 INI 文件中。

**配置工具显示 `[not supported]` 怎么办？**
1. 确保联网，然后运行 `update.bat`
2. 如果无效，检查我们的 [issues](https://github.com/doomsday616/rdpwrap-ini-community/issues) 中是否有你的 `termsrv.dll` 版本
3. 如果没有，请[提交一个 issue](https://github.com/doomsday616/rdpwrap-ini-community/issues/new) 并附上你的版本号

**为什么 RDPCheck 无法更改分辨率？**
`RDPCheck` 仅用于快速测试。请使用 `mstsc.exe`（微软远程桌面客户端）进行完整设置。连接 `127.0.0.1` 或 `127.0.0.2` 进行回环测试。

---

## 已知问题

- Windows 8+ **平板电脑**上，非活动会话可能会被系统注销 — [#37](https://github.com/stascorp/rdpwrap/issues/37)
- Windows 10 上可能会意外锁定自己 — [#50](https://github.com/stascorp/rdpwrap/issues/50)
- 不兼容启用了 **RemoteFX** 的主机 — [#127](https://github.com/stascorp/rdpwrap/issues/127)
- `termsrv.dll` 在登录时崩溃 — Windows Vista Starter RTM x86 (`6.0.6000.16386`)
- 如果终端服务启动时挂起，请将 `rdpwrap.dll` 添加到**杀毒软件排除列表**，或使用以下命令隔离：
  ```
  sc config TermService type= own
  ```
- 部分杀毒软件（AVG、Norton）可能删除 RDP Wrapper — 确认从[官方发布页](https://github.com/doomsday616/rdpwrap-ini-community/releases)下载后，添加到排除列表 — [#191](https://github.com/stascorp/rdpwrap/issues/191)
- Windows 10 家庭版创意者更新后，`rfxvmt.dll` 可能缺失导致 `[not listening]` — v1.6.2 安装程序可自动恢复，或参见 [#194](https://github.com/stascorp/rdpwrap/issues/194)

---

## 更新日志

### 社区更新（本分支）

#### 2026-02-20 — v1.6.2-community.2
- 从上游重建仓库，保持干净的历史记录
- 合并 [@sjackson0109](https://github.com/sjackson0109) 的 INI 更新（上游 PR [#4062](https://github.com/stascorp/rdpwrap/pull/4062)）：
  - 添加 Windows 11 25H2 (`10.0.26100.x`) 支持
  - 添加 Windows 11 26H1 Insider (`10.0.28000.x`) 支持
  - 添加最新 Windows 10 版本（`10.0.19041.6456`、`10.0.19045.6466`）
  - 添加补丁代码 `mov_eax_1_nop_2`
- 添加[逆向工程指南](docs/HOW-TO-ADD-NEW-WINDOWS-BUILDS.md)
- 升级构建系统至 MSVC v143（Visual Studio 2022）
- 将 `RDPWInst.exe` 移植到 Free Pascal — 完全开源，可从源码编译
- 将 `RDPConf.exe` 移植到 Lazarus/LCL — 完全开源，可从源码编译
- 添加 GitHub Actions 工作流实现自动化构建（x86 + x64 DLL、FPC 安装器、Lazarus 配置工具）
- 添加发布包：社区构建的 DLL + FPC/Lazarus EXE + 上游 RDPCheck + 社区 INI + 批处理脚本
- 添加双语 README（英文 + 中文）
- 添加治理文件：`CONTRIBUTING.md`、`.github/CODEOWNERS`

### 上游历史（stascorp/rdpwrap）

#### 2017-12-27 — v1.6.2
- 安装程序更新
- 包含适用于最新 Windows 版本的 INI 文件
- 添加对支持的 Windows 版本的检查（[#155](https://github.com/stascorp/rdpwrap/issues/155)）
- 添加从当前目录获取 INI 文件的功能（[#300](https://github.com/stascorp/rdpwrap/issues/300)）
- 添加恢复 `rfxvmt.dll` 的功能（Windows 10 家庭版缺失此文件 — [#194](https://github.com/stascorp/rdpwrap/issues/194)）
- RDP Config 更新 — 添加自定义启动程序（[#13](https://github.com/stascorp/rdpwrap/issues/13#issuecomment-77651843)）
- 添加 MSI 安装包（[#14](https://github.com/stascorp/rdpwrap/issues/14)）

#### 2016-08-01 — v1.6.1
- 包含适用于最新 Windows 版本的 INI 文件
- 安装程序更新 — 添加在线安装模式、卸载时保留设置
- RDP Config 更新 — 修复端口更改时的防火墙规则、添加隐藏登录用户功能

#### 2015-08-12 — v1.6
- 添加 Windows 10 支持
- 减小 INI 文件体积（注释移至 KB 文件）
- 安装程序更新 — 添加错误 1056 解决方法、添加更新支持
- RDP Checker 更新 — 连接 IP 改为 `127.0.0.2`
- RDP Config 更新 — 添加所有投影模式、写入组策略

#### 2014-12-11 — v1.5
- 添加 INI 配置支持 — 无需重新编译即可扩展版本支持
- 添加 Windows 8.1 + KB3000850、Windows 10 TP Update 2 支持
- 为 RDP Config 添加诊断功能

#### 2014-11-14 — v1.4
- 添加 Windows 10 Technical Preview Update 1 支持
- 添加 Windows Vista SP2 / Windows 7 SP1 + KB3003743 支持
- 添加新的 RDP 配置程序

#### 2014-10-21
- 安装程序：添加 System32 安装方式、修复 NLA 设置保留
- RDP Checker：正确管理 SecurityLayer/UserAuthentication 值

#### 2014-10-20 — v1.3
- 添加 Windows 10 Technical Preview 支持
- 添加 Windows 7 + KB2984972、Windows 8 + KB2973501 支持
- 扩展 Vista（SP0/SP1/SP2）和 Windows 7（SP0/SP1）支持
- 安装程序更新至 v2.2，修复 Vista x64 路径扩展

#### 2014-07-26 — v1.2
- 添加 Windows 8 DP/CP/RP/RTM、Windows 8.1 Preview/RTM 支持
- 安装程序更新至 v2.1

#### 2013-12-09
- Fusix 的 C++ 移植 — 添加 x64 支持
- 新命令行安装程序 v2.0 + 本地 RDP 测试工具

#### 2013-10-22 — v1.1
- 稳定版 — 改进内部 SL Policy 函数包装
- 添加 Windows 8 单语言版支持

#### 2013-10-19 — v1.0
- 首个测试版 — 基本 SL Policy 包装

---

## 从源码构建

### C++ DLL (`rdpwrap.dll`)

使用 **Microsoft Visual Studio 2022**（或任何带有 MSVC v143 工具集的版本）构建 x86/x64 DLL。

```bash
# 使用 MSBuild（Visual Studio 开发者命令提示符）
msbuild src-x86-x64-Fusix/RDPWrap.sln /p:Configuration=Release /p:Platform=Win32
msbuild src-x86-x64-Fusix/RDPWrap.sln /p:Configuration=Release /p:Platform=x64
```

### 安装程序 (`RDPWInst.exe`)

使用 [Free Pascal](https://www.freepascal.org/) 编译（控制台应用，无 GUI 依赖）。

```bash
fpc -Moobjfpc -Sh -O3 -XX -CX -WG -Twin32 -FEbin src-installer/RDPWInst.lpr
```

### 配置工具 (`RDPConf.exe`)

使用 [Lazarus](https://www.lazarus-ide.org/) 编译（LCL GUI 应用）。

```bash
lazbuild --build-mode=Release src-rdpconfig/RDPConf.lpi
```

### RDP 检查工具 (`RDPCheck.exe`)

使用上游 v1.6.2 预编译二进制文件。源码（`src-rdpcheck/`）需要 Embarcadero Delphi 和 Microsoft RDP ActiveX 控件（`TMsRdpClient2`），无法轻松移植到开源工具链。

### GitHub Actions CI

所有构建通过 [GitHub Actions](.github/workflows/build.yml) 自动化。每次推送到 `main` 和每个标签发布都会触发完整构建流程。

---

## 相关链接

- **本仓库：** https://github.com/doomsday616/rdpwrap-ini-community
- **上游仓库：** https://github.com/stascorp/rdpwrap
- **Telegram 群组：** https://t.me/rdpwrap
- **讨论：** [Andrew Block .net](http://web.archive.org/web/20150810054558/http://andrewblock.net/enable-remote-desktop-on-windows-8-core/) · [MDL 论坛](http://forums.mydigitallife.info/threads/55935-RDP-Wrapper-Library-(works-with-Windows-8.1-Basic))
- **教程：** [如何为新版 termsrv.dll 查找偏移量](http://www.youtube.com/watch?v=FiD86tmRBtk)

---

## 许可证

[Apache License 2.0](LICENSE)

这是一个独立的社区分支，**不是** `stascorp/rdpwrap` 的官方发布渠道。
