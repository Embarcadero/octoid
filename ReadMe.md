# Octoid

## Intro

Octoid is a tool for translating Objective-C headers into Delphi code. It is intended as a replacement for SDKTransform which ships with Delphi.

(Octoid is an acronym for Objective-C TranslatOr Into Delphi)

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

## Desktop App

The desktop app is a GUI (VCL) application for Windows. It allows you to select an SDK, select a framework and transform it into a Delphi unit.

The options page allows you customize how a banner (if required) is inserted into the unit.

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

--out <typemap> - (optional) typemap containing equals separated values (can override existing mappings)

Extras: additional options to be passed to libClang

## Transforming 3rd party frameworks

It is possible to transform 3rd party frameworks, by copying the `.framework` folder into the `System\Library\Frameworks` folder of the nominated platform SDK.
It should be noted however, if the framework depends on any other 3rd party frameworks (including from the same vendor), they will need to be copied also, in order for a successful transform
Once a 3rd party framework has been copied, when the desktop app is restarted, or when a switch is made between SDKs, it will include the 3rd party framework in the frameworks list

## Translation Process

### TObjCHeaderTranslator.Translate

This method is central to the translation process. By way of `CreateCombinedHeaderFile` it searches for .h files in the frameworks Headers folder, and combines them into a temporary .h file which is passed to the `ParseTranslationUnit` method. The result is a translation of all the declarations in the framework and dependent libraries. The `Cursor` property of `TranslationUnit` is the root cursor for accessing all the declarations

### TCustomTranslator.VisitTypes

The `VisitTypes` method, which is passed to `VisitChildren` method of the root `Cursor` in `TCustomTranslator.AnalyzeTypes`, traverses all declaration cursors, and adds the cursor to the respective list, depending on the type of declaration, i.e.:

* `FConsts` for constants and enumerated constants
* `FTypes` for structs, unions, and typedefs (includes interface types in Objective-C)
* `FDeclaredTypes` for types that may be 'forward' declared

`HandleTypeDeclaration` is overridden in `TObjCHeaderTranslator` and adds the cursor to these lists:

* `FClasses` for types to be declared as interfaces/classes
* `FCategories` for types to be 'aggregated' to classes
* `FExportedContsts` for types to be declared as functions that retrieve the value from the framework

### TObjCHeaderTranslator.HandleTypeDeclaration

Adds declarations to the respective lists, and for Objective-C interface and category declarations, does 'discovery' of block method types that need to be added to the type declarations. 

### TObjCHeaderTranslator.DiscoverBlockMethods

Iterates the methods of the declared type, and if a parameter that is a block method type is discovered, adds the method to the `FBlockMethods` list, including any parameters declared in the block method type.

### TCustomTranslator.WriteDelphiSource

Writes the unit source, i.e.

* Copyright header (if any)
* Unit name based on the platform and framework
* Interface uses clause based on the dependent frameworks. **NOTE: This needs attention as it can result in frameworks being added that are not dependencies**
* Constants
* Types - in the order of: forward declarations, types, classes
* Functions
* Implementation uses clause
* Exported const functions
* Module load handling

### TCustomTranslator.WriteTypes

Writes the forward declarations (e.g. Objective-C interfaces), types (e.g. structs translated to records), classes (Objective-C) and exported const functions

### TObjCHeaderTranslator.WriteClasses

Handles writing of block methods (see also `TObjCHeaderTranslator.DiscoverBlockMethods`), classes and exported const function prototypes.

### TObjCHeaderTranslator.WriteInterfaceType

Writes the 'Interface Class', 'Interface Instance' and Objective-C import class declarations.

#### TObjCHeaderTranslator.WriteInterfaceClassType and TObjCHeaderTranslator.WriteInterfaceInstanceType

Writes the declaration header, and enumerates the methods and adds them to `FMethods`. When adding the method, a check is made for existing methods so as to resolve method overloading. The overloads are then resolved, the methods are sorted in alpha order and written out. At the end of `TObjCHeaderTranslator.WriteInterfaceInstanceType` the Objective-C import class declaration is written.

## Conversion

### Types

Conversion of simple types is achieved via the `TypeMap` dictionary. Most types are set up in the overridden `TObjCHeaderTranslator.DoSetupTypeMap` method, however it might be possible to push some back to `TCustomTranslator.SetupTypeMap`.

Actual conversion is done in the `TObjCHeaderTranslator.GetDelphiTypeName` method.

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

As per the error messages, e.g. `error: 'libkern/i386/_OSByteOrder.h' file not found`, it is expecting a file to be present that has not been imported. For this particular error, one or more files are missing from `\usr\include\libkern\i386`

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

























