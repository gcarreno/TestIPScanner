# Test IP Scanner

[![Build Status](https://github.com/gcarreno/TestIPScanner/actions/workflows/main.lazarus.yml/badge.svg?branch=main)](https://github.com/gcarreno/TestIPScanner/actions)
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/gcarreno/TestIPScanner/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/gcarreno/TestIPScanner/releases/latest)
[![Supports macOS](https://img.shields.io/badge/support-macOS-black?logo=macOS)](https://github.com/gcarreno/TestIPScanner/releases/latest)
[![License](https://img.shields.io/github/license/gcarreno/TestIPScanner)](https://github.com/gcarreno/TestIPScanner/blob/master/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/gcarreno/TestIPScanner?label=latest%20release)](https://github.com/gcarreno/TestIPScanner/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/gcarreno/TestIPScanner/total)](https://github.com/gcarreno/TestIPScanner/releases)

An attempt to do a graphical application to scan a range of IPs and some other tools.

This entry on the [Test 🌟](https://github.com/gcarreno/TestStar) list is inspired by this repository: [IPAvailabilityScanner](https://github.com/vrwallace/IPAvailabilityScanner).

It implements:
1. Retrieving the machine's Public IP address from one of these providers:
    - http://ifconfig.me
    - http://ifconfig.co
    - http://ipecho.net/plain
    - http://checkip.amazonaws.com
2. Performing an IP scan on a range of IPs ( Not Implemented yet ).
3. Performing a Ping ( Not Implemented yet ).
4. Performing a Trace Route ( Not Implemented yet ).

This application is also using the Property Storage on an `INI` file. It stores the size of the Window and some values like the last selected tab and the last inputs on several edits.

## Important Details

### Linux

In order to perform a Ping the packet needs to be in raw mode. This implies it needs super user permissions.

If you want Ping and TraceRoute to work you must run the program with `sudo`:

```console
$ sudo bin/TestIPScanner
```

### Windows

This has been tested under Windows 11 and it is working on both non and "Run as User".

Many thanks to [ikel](https://github.com/ikelaiah) for doing the tests.

### macOS

I'm unable to test under macOS so I'll need some help here.

## Known bugs

- The IP Edit component needs to be focused in order to refresh internal fields right after starting up the application.
    For some reason, when the `TINIPropStorage` fills in the `TIPEdit.Text` property, the component does not refresh the internal fields.
