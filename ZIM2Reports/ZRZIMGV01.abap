*&---------------------------------------------------------------------*
*& INCLUDE ZRZIMGV01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Configuration 관리 PVR Module Inculde      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.27                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  F4_ZTERM_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE F4_ZTERM_SCRCOM INPUT.

  GET CURSOR FIELD DY_FIELD LINE DY_LINE.
  REFRESH F4HLP.
  CLEAR F4HLP.
  F4HLP-FIELDNAME = 'ZTIMIMG01-ZTERM'.
  F4HLP-STEPL = DY_LINE.
  APPEND F4HLP.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = 'SAPMZIMG'
            DYNUMB     = '0100'
       TABLES
            DYNPFIELDS = F4HLP
       EXCEPTIONS
            OTHERS     = 4.

  IF SY-SUBRC = 0.
    ZTERM = F4HLP-FIELDVALUE.
  ELSE.
    CLEAR ZTERM.
  ENDIF.

*------- Eingabem?lichkeiten anzeigen ---------------------------------
  IF STATUS-ACTION CA 'AC'.
    XSHOW = SPACE.
  ELSE.
    XSHOW = 'X'.
  ENDIF.
  CALL FUNCTION 'FI_F4_ZTERM'
       EXPORTING
            I_KOART = SPACE
            I_XSHOW = XSHOW
            I_ZTERM = ZTERM
            I_ZTYPE = 'R'
       IMPORTING
            E_ZTERM = ZTERM.

*------- ausgew?lte Zahlungsbedingung ?ernehmen ----------------------
  IF NOT ZTERM IS INITIAL.
    ZTIMIMG01-ZTERM = ZTERM.
  ENDIF.

ENDMODULE.                 " F4_ZTERM_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  HESP_ZTERM_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR0100 INPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSIMIMG01-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSIMIMG01-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = EKKO-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH ZSIMIMG01-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    ZSIMIMG01-ZTERM = T052-ZTERM.
  ENDIF.
ENDMODULE.
