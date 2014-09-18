*----------------------------------------------------------------------*
*   INCLUDE ZXTOBU03                                                   *
*----------------------------------------------------------------------*

IF I_REFOBJ_REC-EQTYP = 'E' OR I_REFOBJ_REC-EQTYP = 'V'.
ELSE.
  CLEAR: C_ITOBCHANGEABLE_REC-ANLNR.
  E_CHANGED = 'X'.
*  MESSAGE S899(MM) WITH 'Check Asset Number with Finance'.
 CALL FUNCTION 'POPUP_TO_INFORM'
   EXPORTING
     TITEL         = 'Information'
     TXT1          = '   Pleae Check Asset Number with Finance'
     TXT2          = ' '
*    TXT3          = ' '
*    TXT4          = ' '
           .

ENDIF.
