# lazhelp
Lazarus help viewer improvement

chm package:

- range, overflow and memory leaks checks and fixes
- constant and variable parameters
- source readability: formatting, naming, commenting
- improve performance of operations with strings and memory
- improve usability for users

LHelp:

- get default encoding from Locale ID and from HTML 'meta' tag.
- fixed RPC calls, when Lazarus open context help and LHelp already open
- open multiple CHM in single tab by default
- prevented memory leaks on exceptions
- Index tab behavior as in MS Help

chmmaker:

- TOC and Index files without path, in same directory with project
- added default codepage selection
- added error log tab, with messages from CHM writer
- added binary TOC and Index options
