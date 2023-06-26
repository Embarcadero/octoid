# Octoid

<p align="center"><img src="logo/OctoidMainLogoheader.png" alt=" " height="240" width="351" /></p>


## Intro 

Octoid is a tool for translating Objective-C headers into Delphi code. It is intended as a replacement for SDKTransform which ships with Delphi.
(Octoid is an acronym for Objective-C TranslatOr Into Delphi)


<p align="center" style="margin: 50px 0 0 0;"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 

## License

Please refer to the [license](https://github.com/Embarcadero/octoid/blob/main/LICENSE) regarding conditions of use for the code of Octoid.


## Technical details

If you are interested in the details about how the Octoid code works, please refer to the [technical details readme](https://github.com/Embarcadero/octoid/blob/main/TechDetails.md).

## Dependencies

Octoid depends on Erik Van Bilsen's libraries:

[Chet](https://github.com/neslib/Chet)
[Neslib.Clang](https://github.com/neslib/Neslib.Clang)

Which Erik has granted permission to use in Octoid, and are presently included in the Octoid source

Neslib.Clang is dependent on 64-bit CLang binaries that ship with [LLVM](https://llvm.org/)

In order to run Octoid, you will need to download and install LLVM. At time of writing, Octoid works with version 10.0.0 (there was no installer for Windows 64-bit for version 10.0.1).

**Please ensure that you select the "Add LLVM to the system path for all users" option when installing**

[This is a direct link to the version 10.0.0 installer](https://github.com/llvm/llvm-project/releases/download/llvmorg-10.0.0/LLVM-10.0.0-win64.exe)

[This is a link to the releases page](https://releases.llvm.org/) which lists all releases

<p align="center"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 

## Desktop App

The desktop app is a GUI (VCL) application for Windows. It allows you to select an SDK, select a framework and transform it into a Delphi unit.

The options page allows you customize how a banner (if required) is inserted into the unit.

<p align="center" style="margin: 25px 0 25px 0;"><img src="images/OctoidDesktop.png" alt=" " height="586" width="800" /></p>

<p align="center"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 

## Commandline App

The commandline app is provided for automated transforms, or if you just prefer the command line. The usage is as follows:

Octoid [Options] [Extras]

Options:

--help or --?  - show this help

--sdkroot <sdkroot> - root of the target SDK

--clanginclude <clanginclude> - path to the Clang include files. May be omitted if a supported version of Clang is installed

--platform <platform> - the target platform (macOS or iOS)

--unit <framework> - (optional) transform only the framework with the specified name (default is all frameworks in the SDK)

--out <out> - (optional) directory where the output .pas files should be placed (default is current directory)

--typemap <typemap> - (optional) typemap containing equals separated values (can override existing mappings)

Extras: additional options to be passed to libClang

<p align="center"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 

## Transforming 3rd party frameworks 

It is possible to transform 3rd party frameworks, by copying the `.framework` folder into the `System\Library\Frameworks` folder of the nominated platform SDK.
It should be noted however, if the framework depends on any other 3rd party frameworks (including from the same vendor), they will need to be copied also, in order for a successful transform. Once a 3rd party framework has been copied, when the desktop app is restarted, or when a switch is made between SDKs, it will include the 3rd party framework in the frameworks list

<p align="center"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 

## Dealing with fatal errors during translation

If a conversion fails to produce a translation using the desktop app, the output window will show and be at the bottom of the output so as to highlight the errors.

Commonly, this will be due to missing header files, e.g. you may see an output like:

```
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\usr\include\libkern/_OSByteOrder.h:76:10: error: 'libkern/i386/_OSByteOrder.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\usr\include\stdio.h:407:10: error: 'secure/_stdio.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\usr\include\strings.h:97:10: error: 'secure/_strings.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\usr\include\string.h:194:10: error: 'secure/_string.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\usr\include\libkern/OSByteOrder.h:53:10: error: 'libkern/i386/OSByteOrder.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/Foundation.framework/Headers/NSObjCRuntime.h:9:10: error: 'objc/NSObjCRuntime.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/Foundation.framework/Headers/NSObject.h:6:9: error: 'objc/NSObject.h' file not found
```

As per the error messages, e.g. `error: 'libkern/i386/_OSByteOrder.h' file not found`, it is expecting a file to be present that has not been imported. For this particular error, one or more files are missing from `\usr\include\libkern\i386`.

To resolve this particular kind of issue, you will need to add the required folders/files to the SDK configuration. In Delphi: 

1. Use Tools|Options, Deployment > SDK Manager to bring up the SDK Manager
2. Select the SDK that is being targeted by Octoid
3. Select a path in the Include paths section
4. Click the Add button in the top right
5. Enter the path on remote machine - in this instance `$(SDKROOT)/usr/include/libkern`
6. Enter a file mask of: `*`
7. Ensure that the `Include path` radio button is selected
8. In this particular case, to make sure that subfolders are also imported, the `Include subdirectories` checkbox is checked
9. Click OK
10. Repeat steps 4-7 (and 8 if necessary) for any other folders that are missing, as per the errors
11. Click `Update Local File Cache`
12. Click Save

(For macOS, you will likely need to also add: `/usr/include/secure`, `/usr/include/objc`, `/usr/include/c++`, and `/usr/include/libDER`)

Attempt to translate the desired framework again. You may note a new set of errors, so repeat the above process until the framework translates successfully.

Other errors may include missing frameworks, e.g.:

```
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVDisplayLink.h:26:10: error: 'OpenGL/OpenGL.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVOpenGLBuffer.h:20:10: error: 'OpenGL/OpenGL.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVOpenGLBuffer.h:21:10: error: 'OpenGL/gltypes.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVOpenGLTexture.h:22:10: error: 'OpenGL/OpenGL.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVOpenGLTexture.h:23:10: error: 'OpenGL/gltypes.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/CoreVideo.framework/Headers/CVOpenGLTextureCache.h:16:10: error: 'OpenGL/OpenGL.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/AppKit.framework/Headers/NSOpenGL.h:10:9: error: 'OpenGL/CGLTypes.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/AppKit.framework/Headers/NSOpenGL.h:11:9: error: 'OpenGL/gltypes.h' file not found
\\Mac\Home\Documents\Embarcadero\Studio\SDKs\MacOSX11.1.sdk\System\Library\Frameworks/QuartzCore.framework/Headers/CAOpenGLLayer.h:8:9: error: 'OpenGL/OpenGL.h' file not found
```

The process for resolving missing frameworks is similar to that of above, except applies to the Frameworks section in the SDK configuration. The example above indicates that the `OpenGL` framework is missing and will need to be imported

<p align="center"><img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
<img src="logo/OctoidLogo40x40.png" alt=" " height="40" width="40" />
</p> 


