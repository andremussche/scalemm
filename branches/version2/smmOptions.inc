{.$DEFINE SCALEMM_DEBUG}  // slower but better debugging (no inline functions etc)
{.$DEFINE IntelVTune}      //debug info in release build

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
  {$if CompilerVersion >= 17}  // Delphi 2005 or newer
    {$define HASINLINE}
  {$ifend}
  {$define INLINEGOWN}
//  {$D-}
//  {$L-}
{$ENDIF}

//use this to get line debug info in Intel vTune
{$IFDEF IntelVTune}
  {$undef HASINLINE}
  {$undef INLINEGOWN}
  {$D+}
  {$L+}
{$ENDIF}

// other posible defines:
{.$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
{$DEFINE Align8Bytes}     // 8 byte aligned header, needed for OmniThreadLibrary etc
{.$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead

{$IFDEF CPUX64}
  {$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
  {$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead
{$ENDIF}

{$IFDEF DEBUG}
  {.$DEFINE SCALEMM_FILLFREEMEM}
{$ENDIF}

{$if CompilerVersion >= 17}
  {$define USEMEMMANAGEREX}
{$ifend}

