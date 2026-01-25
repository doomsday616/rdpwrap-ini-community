# How to Add Support for New Windows Builds

This guide explains the technical process for reverse engineering new Windows builds to extract the necessary RDP Wrapper configuration parameters.

## Overview

When Microsoft releases new Windows updates, the `termsrv.dll` file changes, and RDP Wrapper needs updated offset configurations to function properly. This document outlines the manual reverse engineering process required to find these offsets.

## Prerequisites

### Required Tools

**Disassemblers (Choose one):**
- **Ghidra** (Free, recommended) - NSA's reverse engineering tool
- **IDA Pro** (Commercial) - Industry standard
- **x64dbg** (Free) - Good for dynamic analysis
- **Radare2** (Free) - Command-line focused

**Supporting Tools:**
- **HxD** or similar hex editor
- **PE Explorer** - For PE structure analysis
- **Process Monitor** - Runtime file/registry monitoring
- **API Monitor** - Function call tracing
- **RDPCheck.exe** - For testing configurations

### Required Knowledge

- Assembly language (x86/x64)
- PE file format basics
- Windows API understanding
- Basic cryptography concepts

## Step 1: Obtain the Target File

### Extract termsrv.dll

```powershell
# Navigate to System32 directory
cd C:\Windows\System32

# Copy termsrv.dll to analysis directory
copy termsrv.dll C:\Analysis\termsrv.dll

# Get file version information
Get-ItemProperty C:\Analysis\termsrv.dll | Select-Object VersionInfo
```

### Determine Version Number

```powershell
# PowerShell method
(Get-Item C:\Analysis\termsrv.dll).VersionInfo.ProductVersion

# Alternative: WMIC method
wmic datafile where name="C:\\Windows\\System32\\termsrv.dll" get Version
```

The version format will be: `10.0.XXXXX.YYYY` (e.g., `10.0.26100.7623`)

## Step 2: Initial Analysis

### Load in Disassembler

1. Open termsrv.dll in your chosen disassembler
2. Let it complete initial analysis (auto-analysis)
3. Examine the import table for key functions
4. Identify the main code sections

### Key Function Identification

Search for these critical functions that RDP Wrapper needs to patch:

1. `CSessionArbitrationHelper::IsSingleSessionPerUserEnabled`
2. `CDefPolicy::Query`
3. `CEnforcementCore::GetInstanceOfTSLicense`
4. `CSLQuery::Initialize`

## Step 3: Finding Function Offsets

### Method 1: String Reference Analysis

```
1. Search for relevant strings:
   - "Terminal Services"
   - "Session"
   - "License"
   - "Policy"
   - Error messages related to licensing

2. Follow cross-references from strings to functions
3. Analyze the functions that reference these strings
```

### Method 2: Import Table Analysis

```
1. Examine imported functions:
   - GetTokenInformation
   - WinStationQueryInformationW
   - RegQueryValueExW
   - License-related APIs

2. Find functions that call these imports
3. Trace backwards to find policy validation logic
```

### Method 3: Pattern Matching

Look for specific assembly patterns that indicate the functions we need to patch:

#### Single User Patch Pattern
```asm
; Look for patterns like:
BB 01 00 00 00    ; mov ebx, 1 (single session enabled)
; Or:
B8 01 00 00 00    ; mov eax, 1
```

#### DefPolicy Patch Pattern
```asm
; Look for license policy validation:
B8 01 00 00 00       ; mov eax, 1 (policy result)
89 81 38 06 00 00    ; mov [rcx+638h], eax (store result)
; Or similar patterns with different registers
```

## Step 4: Extracting Configuration Parameters

### Single User Offset

1. Find `CSessionArbitrationHelper::IsSingleSessionPerUserEnabled`
2. Look for the instruction that returns 1 (single session restriction)
3. Note the file offset of this instruction
4. The patch will change this to return 0 (allow multiple sessions)

### DefPolicy Offset

1. Find `CDefPolicy::Query`
2. Look for license validation logic
3. Find where it sets the result to indicate "licensed"
4. Note the offset for the instruction to patch

### LocalOnly Offset

1. Find `CEnforcementCore::GetInstanceOfTSLicense`
2. Look for local connection restrictions
3. Find the jump/conditional that enforces local-only policy
4. Note the offset to patch this restriction

### SLInit Parameters

1. Find `CSLQuery::Initialize`
2. Analyze the data structure it initializes
3. Find the memory offsets for these fields:
   - `bInitialized`
   - `bServerSku`
   - `lMaxUserSessions`
   - `bAppServerAllowed`
   - `bRemoteConnAllowed`
   - `bMultimonAllowed`
   - `ulMaxDebugSessions`
   - `bFUSEnabled`

## Step 5: Creating the Configuration

### Basic INI Structure

```ini
[10.0.XXXXX.YYYY]
; Single user session patch
SingleUserPatch.x64=1
SingleUserOffset.x64=OFFSET_HEX
SingleUserCode.x64=PATCH_CODE

; License policy patch
DefPolicyPatch.x64=1
DefPolicyOffset.x64=OFFSET_HEX
DefPolicyCode.x64=PATCH_CODE

; Local-only restriction patch
LocalOnlyPatch.x64=1
LocalOnlyOffset.x64=OFFSET_HEX
LocalOnlyCode.x64=PATCH_CODE

; Software licensing hook
SLInitHook.x64=1
SLInitOffset.x64=OFFSET_HEX
SLInitFunc.x64=New_CSLQuery_Initialize

[10.0.XXXXX.YYYY-SLInit]
bServerSku.x64=OFFSET_HEX
bRemoteConnAllowed.x64=OFFSET_HEX
bFUSEnabled.x64=OFFSET_HEX
bAppServerAllowed.x64=OFFSET_HEX
bMultimonAllowed.x64=OFFSET_HEX
lMaxUserSessions.x64=OFFSET_HEX
ulMaxDebugSessions.x64=OFFSET_HEX
bInitialized.x64=OFFSET_HEX
```

### Common Patch Codes

```ini
; Available patch codes (defined in [PatchCodes] section):
Zero=00                              ; Set to zero
nop=90                              ; No operation
jmpshort=EB                         ; Short jump
mov_eax_1_nop_2=B8010000009090      ; mov eax,1 + 2 NOPs
CDefPolicy_Query_eax_rcx_jmp=B80001000089813806000090EB  ; Policy bypass
```

## Step 6: Testing and Validation

### Initial Testing

1. Create a test INI file with your calculated offsets
2. Back up the original rdpwrap.ini
3. Replace with your test configuration
4. Restart Terminal Services
5. Run RDPCheck.exe to verify status

### Dynamic Analysis

1. Use x64dbg to attach to the running termsrv.exe process
2. Set breakpoints at your calculated offsets
3. Verify that your patches are being applied correctly
4. Monitor for any crashes or unexpected behavior

### Validation Steps

```powershell
# Stop Terminal Services
net stop TermService

# Apply new configuration
copy test_rdpwrap.ini C:\Program Files\RDP Wrapper\rdpwrap.ini

# Start Terminal Services
net start TermService

# Test with RDPCheck
RDPCheck.exe

# Test actual RDP connection
mstsc /v:localhost
```

## Step 7: Documentation and Sharing

### Document Your Findings

Create a detailed report including:
- Windows build version and SHA256 of termsrv.dll
- Methodology used
- Specific offsets found
- Testing results
- Any challenges encountered

### Share with Community

1. Post your configuration in a GitHub issue
2. Include the termsrv.dll file (zipped) for verification
3. Provide testing evidence (screenshots from RDPCheck)
4. Document any system-specific requirements

## Common Challenges

### Address Space Layout Randomization (ASLR)

Modern Windows uses ASLR, but the relative offsets within the DLL remain constant. Always work with file offsets, not memory addresses.

### Compiler Optimizations

Microsoft's compiler optimizations can:
- Inline functions
- Reorder code
- Change calling conventions
- Merge similar functions

### Code Signing

Windows verifies code signatures, so:
- Patches must be applied at runtime, not to the file
- Use the RDP Wrapper's hooking mechanism
- Never modify the original termsrv.dll

### Function Variations

The same logical function might be implemented differently across builds:
- Different assembly patterns
- Different register usage
- Inlined vs separate functions

## Advanced Techniques

### Comparative Analysis

When analyzing a new build:
1. Compare with a known working build
2. Look for similar patterns and structures
3. Use diff tools on disassembled code

### Automated Pattern Detection

Some community members have created scripts to:
- Search for common assembly patterns
- Compare function signatures
- Suggest likely offset candidates

### Binary Diffing

Tools like BinDiff can help identify:
- Changed functions between builds
- Similar code blocks
- Function renaming/reorganization

## Community Resources

### Trusted Contributors

Community members known for accurate analysis:
- **@Fabliv** - Consistently provides verified configurations
- **@sebaxakerhtc** - Regular contributor with detailed analysis
- **@maxpiva** - Historical configurations and tools

### Useful Repositories

- Main project: `stascorp/rdpwrap`
- Community tools: Various forks with analysis scripts
- Configuration databases: Community-maintained INI collections

## Contributing Your Analysis

### GitHub Issue Format

When posting a new configuration:

```markdown
## Windows Build: 10.0.XXXXX.YYYY

**System Information:**
- Edition: Windows 11 Pro/Home/Enterprise
- Architecture: x64
- Installation: Clean/Update from X.X.X

**Analysis Results:**
[Paste your INI configuration here]

**Verification:**
- ✅ RDPCheck shows "Installed" and "Listening"
- ✅ Multiple simultaneous connections tested
- ✅ No crashes or stability issues

**Files:**
[Attach termsrv.dll.zip]
```

### Testing by Others

Before a configuration is accepted:
1. Multiple community members should test
2. Verify on different system configurations
3. Confirm no regressions on existing functionality
4. Test edge cases (different user accounts, domain environments)

## Conclusion

Adding support for new Windows builds requires:
- Technical reverse engineering skills
- Patience for trial-and-error testing
- Community collaboration for verification
- Detailed documentation for maintainability

While this process cannot be easily automated due to Microsoft's security measures and varying compilation patterns, the community has developed efficient workflows that typically produce working configurations within days of new Windows releases.

The key to success is methodical analysis, thorough testing, and collaboration with the experienced community members who have mastered this process.