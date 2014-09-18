*----------------------------------------------------------------------*
*   INCLUDE ZXQQMU05                                                   *
*----------------------------------------------------------------------*

IF I_VRGNG = 'PMM4' AND I_VIQMEL-QMART = 'Q2'.
*IF I_AKTYP = 'PMM4'.
  AUTHORITY-CHECK OBJECT 'ZQM_REV' ID 'ZQM_REV' FIELD 'X'.
  IF SY-SUBRC <> 0.
 MESSAGE E000(ZMQM) WITH 'No Authorization to set COMPLETION indicator'.
  ENDIF.
ENDIF.
