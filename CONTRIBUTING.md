# Contributing to RDP Wrapper — Community Edition

Thank you for your interest in contributing! This community fork actively accepts pull requests.

## How to contribute

### Updating `rdpwrap.ini` for new Windows builds

This is the most common and most valuable contribution. When a new Windows update ships a new `termsrv.dll` build, support needs to be added to the INI file.

1. Check the [reverse engineering guide](docs/HOW-TO-ADD-NEW-WINDOWS-BUILDS.md) for instructions
2. Test your changes on the target Windows build
3. Submit a PR modifying `res/rdpwrap.ini`

### Bug reports

1. Check [existing issues](https://github.com/doomsday616/rdpwrap-ini-community/issues) first
2. Include your Windows version, `termsrv.dll` build number, and RDP Wrapper version
3. Include relevant logs or screenshots

### Code contributions

- **C++ DLL changes** — modify files in `src-x86-x64-Fusix/`
- **Installer changes** — modify `src-installer/RDPWInst.lpr` (Free Pascal)
- **Config tool changes** — modify files in `src-rdpconfig/` (Lazarus/LCL)

## Code review

- INI file changes are reviewed by [@sjackson0109](https://github.com/sjackson0109) and [@doomsday616](https://github.com/doomsday616)
- All other changes are reviewed by [@doomsday616](https://github.com/doomsday616)

## Development setup

See the "Building from source" section in [README.md](README.md) for build instructions.

## Code style

- C++ code: follow existing style in `src-x86-x64-Fusix/`
- Pascal code: `{$mode objfpc}{$H+}`, standard FPC/Lazarus conventions
- Commit messages: English, imperative mood (e.g. "Add support for build 10.0.26100.3456")
- INI entries: follow existing format, include Windows version comment

## License

By contributing, you agree that your contributions will be licensed under the [Apache License 2.0](LICENSE).
