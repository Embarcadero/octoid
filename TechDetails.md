# Technical details

These are some of the technical details about how Octoid works. It is planned to expand these details over time.

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
