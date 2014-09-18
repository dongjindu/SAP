*----------------------------------------------------------------------*
*   INCLUDE ZLSVIMF01                                                  *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  SET_ID_LIST  INPUT
*&---------------------------------------------------------------------*
MODULE SET_ID_LIST INPUT.
  CALL FUNCTION 'G_RW_SET_SELECT'
       EXPORTING
            FIELD_NAME                 = 'RACCT'
            SET                        = 'CF-*'
            TABLE                      = 'GLFUNCT'
            TYPELIST                   = 'BSMD'
            SHOW_FIELD_NAME            = 'X'
            NO_MAINTENANCE             = 'X'
       IMPORTING
            SET_NAME                   = ZVFI_CASHFLOW-ZSETI
       EXCEPTIONS
            NO_SETS                    = 1
            NO_SET_PICKED              = 2
            OTHERS                     = 3.
ENDMODULE.                 " SET_ID_LIST  INPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_LOG  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_LOG OUTPUT.
  CASE STATUS-ACTION.
    WHEN 'U'.
      ZVFI_CASHFLOW-AEDAT = SY-DATUM.
      ZVFI_CASHFLOW-AEZET = SY-UZEIT.
      ZVFI_CASHFLOW-AENAM = SY-UNAME.
      ZTFI_CASHFLOW-AEDAT = SY-DATUM.
      ZTFI_CASHFLOW-AEZET = SY-UZEIT.
      ZTFI_CASHFLOW-AENAM = SY-UNAME.
    WHEN 'A'.
      ZVFI_CASHFLOW-ERDAT = SY-DATUM.
      ZVFI_CASHFLOW-ERZET = SY-UZEIT.
      ZVFI_CASHFLOW-ERNAM = SY-UNAME.
      ZTFI_CASHFLOW-ERDAT = SY-DATUM.
      ZTFI_CASHFLOW-ERZET = SY-UZEIT.
      ZTFI_CASHFLOW-ERNAM = SY-UNAME.
  ENDCASE.
ENDMODULE.                 " TABLE_LOG  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_LOG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_LOG INPUT.
  CASE STATUS-ACTION.
    WHEN 'U'.
      ZVFI_CASHFLOW-AEDAT = SY-DATUM.
      ZVFI_CASHFLOW-AEZET = SY-UZEIT.
      ZVFI_CASHFLOW-AENAM = SY-UNAME.
      ZTFI_CASHFLOW-AEDAT = ZVFI_CASHFLOW-AEDAT.
      ZTFI_CASHFLOW-AEZET = ZVFI_CASHFLOW-AEZET.
      ZTFI_CASHFLOW-AENAM = ZVFI_CASHFLOW-AENAM.
    WHEN 'A'.
      ZVFI_CASHFLOW-ERDAT = SY-DATUM.
      ZVFI_CASHFLOW-ERZET = SY-UZEIT.
      ZVFI_CASHFLOW-ERNAM = SY-UNAME.
      ZTFI_CASHFLOW-ERDAT = ZVFI_CASHFLOW-ERDAT.
      ZTFI_CASHFLOW-ERZET = ZVFI_CASHFLOW-ERZET.
      ZTFI_CASHFLOW-ERNAM = ZVFI_CASHFLOW-ERNAM.
  ENDCASE.

ENDMODULE.                 " TABLE_LOG  INPUT
