{.$DEFINE SCALEMM_DEBUG}  // slower but better debugging (no inline functions etc)

{$IFDEF SCALEMM_DEBUG}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$ASSERTIONS     ON}
  {$DEBUGINFO      ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS    ON}
  {$define SCALEMM_MAGICTEST}
  {$define SCALEMM_FILLFREEMEM}
  {.$define SCALEMM_OUTPUTSTRING}
  {$define PURE_PASCAL}
{$ELSE}      // default "release" mode, much faster!
  {$OPTIMIZATION   ON}         // 235% faster!
  {$STACKFRAMES    OFF}        // 12% faster
  {$ASSERTIONS     OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS    OFF}
  {$if CompilerVersion >= 17}
    {.$define HASINLINE}        // Delphi 2005 or newer
  {$ifend}
  {.$define INLINEGOWN}
  //{$D-}  //disable this to get line debug info in Intel vTune
  //{$L-}  //disable this to get line debug info in Intel vTune
{$ENDIF}

// other posible defines:
{.$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
{$DEFINE Align8Bytes}     // 8 byte aligned header, needed for OmniThreadLibrary etc
{.$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead
{.$DEFINE USEMEDIUM}      // handling of 2048..16384 bytes blocks

{$IFDEF DEBUG}
  {.$DEFINE SCALEMM_FILLFREEMEM}
{$ENDIF}
