{.$DEFINE SCALEMM_DEBUG}  // slower but better debugging (no inline functions etc)

{$IFDEF SCALEMM_DEBUG}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$ASSERTIONS     ON}
  {$DEBUGINFO      ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS    ON}
  {$define SCALEMM_MAGICTEST}
  {.$define SCALEMM_FILLFREEMEM}
  {.$define SCALEMM_OUTPUTSTRING}
{$ELSE}      // default "release" mode, much faster!
  {$OPTIMIZATION   ON}         // 235% faster!
  {$STACKFRAMES    OFF}        // 12% faster
  {$ASSERTIONS     OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS    OFF}
  {$if CompilerVersion >= 17}
    {$define HASINLINE}        // Delphi 2005 or newer
  {$ifend}
  {$define INLINEGOWN}
  {$D-}
  {$L-}
{$ENDIF}

// other posible defines:
{.$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
{.$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead
{.$DEFINE USEMEDIUM}      // handling of 2048..16384 bytes blocks

{$IFDEF DEBUG}
  {.$DEFINE FILLFREEMEM}
{$ENDIF}
