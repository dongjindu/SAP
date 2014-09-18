*---------------------------------------------------------------------*
*       FORM RESET_STATL                                              *
*---------------------------------------------------------------------*
FORM RESET_STATL.
  CLEAR STATL.
  STATL-SMAXCNT    = 500.
  STATL-SENDDAT    = SY-DATUM.
  STATL-SENDTIME   = SY-UZEIT.
  IF SY-UZEIT > '001500'.
    STATL-STIME = SY-UZEIT.
    SUBTRACT 900 FROM STATL-STIME.
    STATL-STIME+2(4) = '0000'.
  ENDIF.
  STATL-SMANDT     = '*  '.
  STATL-SBENU      = '*'.
  STATL-STCOD      = '*'.
  STATL-SPROGRAM   = '*'.
  STATL-STASK      = '*'.
  STATL-SPATH      = '*'.
  STATL-SSCREEN    = '*'.
  STATL-SSYSTEMID  = '*'.
  STATL-SWPID      = '*'.
  STATL-SDAT       = SY-DATUM.
ENDFORM.
