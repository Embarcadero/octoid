unit Octoid.Consts;

interface

const
  cSDKUserIncludeFolder = 'usr\include\';
  cSDKFrameworksFolder = 'System\Library\Frameworks\';
  cSDKSubFrameworksFolder = 'System\Library\SubFrameworks\';
  cSDKPrivateFrameworksFolder = 'System\Library\PrivateFrameworks\';
  cSDKUserLibClangIncludePath = 'usr\lib\clang\include';

  cAPIDeprecatedStartsWith = 'API_DEPRECATED';
  cAPINSAvailableStartsWith = 'NS_AVAILABLE';

  cTargetPlatformMacOS = 'MACOS';
  cTargetPlatformIOS = 'IOS';

  cSwitchHelpWord = '--help';
  cSwitchHelpSymbol = '--?';
  cSwitchSdkRoot = '--sdkroot';
  cSwitchClangInclude = '--clanginclude';
  cSwitchFramework = '--unit';
  cSwitchTargetPlatform = '--platform';
  cSwitchOutputPath = '--out';
  cSwitchTypeMapFile = '--typemap';
  cSwitchTypeUnitMapFile = '--typeunitmap';
  cSwitchDump = '--dump';
  cSwitchErrors = '--errors';

  cLLVMRegistryPath = 'Software\LLVM\LLVM';
  cLLVMClangFolder = 'lib\clang';
  cLLVMClangIncludeFolder = 'include';

  cErrorLimitDefault = 50;

implementation

end.
