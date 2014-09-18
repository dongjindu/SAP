*----------------------------------------------------------------------*
*   INCLUDE Z_MOON_ALV_TOP                                             *
*----------------------------------------------------------------------*
TYPE-POOLS : SLIS.
*----------------------------------------------------------------------*
*  TABLES                                                              *
*----------------------------------------------------------------------*
DATA : GT_EVENTS              TYPE SLIS_T_EVENT,
       GT_LISTHEADER          TYPE SLIS_T_LISTHEADER,
       GT_FIELDCAT            TYPE SLIS_T_FIELDCAT_ALV,
       GT_SP_GROUP            TYPE SLIS_T_SP_GROUP_ALV,
       GT_SORT                TYPE SLIS_T_SORTINFO_ALV,
       GT_EXCLUDING           TYPE SLIS_T_EXTAB.
*----------------------------------------------------------------------*
*  STRUCTURE                                                           *
*----------------------------------------------------------------------*
DATA : GS_LAYOUT              TYPE SLIS_LAYOUT_ALV,
       GS_GRIDSET             TYPE LVC_S_GLAY,
       GS_EXIT_CAUSED_BY_USER TYPE SLIS_EXIT_BY_USER,
       GS_VARIANT             TYPE DISVARIANT,
       GS_FIELDCAT            TYPE SLIS_FIELDCAT_ALV,
       GS_PRINT               TYPE SLIS_PRINT_ALV,
       GS_SORT                TYPE SLIS_SORTINFO_ALV,
       GS_SGROUP              TYPE SLIS_SP_GROUP_ALV.

DATA L_EXCLUDING LIKE LINE OF GT_EXCLUDING.

*----------------------------------------------------------------------*
*  DATA                                                                *
*----------------------------------------------------------------------*
DATA : G_PROGRAM              LIKE SY-REPID,
       G_EXIT_CAUSED_BY_CALLER,
       G_EXIT,
       G_SAVE                 VALUE 'A',
       G_VARIANT              TYPE DISVARIANT.

DATA: GV_DEFAULT(1)  TYPE C,
      GV_REPID    LIKE SY-REPID.

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM ALV_VARIANT_F4 CHANGING P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT,
        LV_NOF4 TYPE C.

  CLEAR LV_NOF4.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_VARI'.
      IF SCREEN-INPUT = 0.
        LV_NOF4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR RS_VARIANT.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.

  IF SY-SUBRC = 0 AND LV_NOF4 = SPACE.
    P_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
