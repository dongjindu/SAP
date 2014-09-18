REPORT ZFBELSUC  MESSAGE-ID ZZ.

* Data declaration
TYPE-POOLS: SLIS.

TABLES:
  BKPF,
  BSEG.

DATA:
  HF_SELE    TYPE P,   " Z?ler ob Selektion nach Belegposition/-kopf
  HF_PFELD   TYPE P,                   " Z?ler Tabellenzeilen
  HF_COUNT   TYPE P,
  HF_REPID   LIKE  SYST-REPID,
  HF_STATUS TYPE SLIS_FORMNAME VALUE 'STANDARD_02',
  HF_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.

DATA: BEGIN OF I_TAB OCCURS 500,       " Ergebnisliste f? den ALV
  DMBTR   LIKE    BSEG-DMBTR,
  BELNR   LIKE    BSEG-BELNR,
  GJAHR   LIKE    BSEG-GJAHR,
  BUKRS   LIKE    BSEG-BUKRS,
  XBLNR   LIKE    BKPF-XBLNR,
  BKTXT   LIKE    BKPF-BKTXT,
  HKONT   LIKE    BSEG-HKONT,
  LIFNR   LIKE    BSEG-LIFNR,
  KUNNR   LIKE    BSEG-KUNNR,
  ZUONR   LIKE    BSEG-ZUONR,
  SGTXT   LIKE    BSEG-SGTXT,
  END   OF I_TAB.

DATA I_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV    " Feldkatalog
       WITH HEADER LINE.

DATA BEGIN OF HF_DISVARIANT.           " ALV Variante
        INCLUDE STRUCTURE DISVARIANT.
DATA END   OF HF_DISVARIANT.

SELECTION-SCREEN BEGIN OF BLOCK BKPF WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
  S_BUKRS  FOR  BSEG-BUKRS,
  S_BELNR  FOR  BSEG-BELNR,
  S_GJAHR  FOR  BSEG-GJAHR,
  S_XBLNR  FOR  BKPF-XBLNR,
  S_BKTXT  FOR  BKPF-BKTXT.
SELECTION-SCREEN END   OF BLOCK BKPF.

SELECTION-SCREEN BEGIN OF BLOCK BSEG
  WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
  S_DMBTR  FOR  BSEG-DMBTR,
  S_HKONT  FOR  BSEG-HKONT,
  S_LIFNR  FOR  BSEG-LIFNR,
  S_KUNNR  FOR  BSEG-KUNNR,
  S_ZUONR  FOR  BSEG-ZUONR,
  S_SGTXT  FOR  BSEG-SGTXT,
  S_KOSTL  FOR  BSEG-KOSTL,
  S_AUFNR  FOR  BSEG-AUFNR.
SELECTION-SCREEN END   OF BLOCK BSEG.

PARAMETERS:
  P_MAXCT(3)   TYPE N     DEFAULT '999'.

AT SELECTION-SCREEN.


START-OF-SELECTION.

  HF_REPID = SY-REPID.

* Feldkatalog f? ALV-Ausgabe aufbauen
  PERFORM AUFBAUEN_FIELDCAT USING 'BELNR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'BUKRS' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'XBLNR' 'BKPF'.
  PERFORM AUFBAUEN_FIELDCAT USING 'BKTXT' 'BKPF'.
  PERFORM AUFBAUEN_FIELDCAT USING 'DMBTR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'ZUONR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'SGTXT' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'GJAHR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'HKONT' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'LIFNR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'KUNNR' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'KOSTL' 'BSEG'.
  PERFORM AUFBAUEN_FIELDCAT USING 'AUFNR' 'BSEG'.

* Daten selektieren
* Pr?en ob Selektion nach Kopffeldern
  CLEAR HF_SELE.
  DESCRIBE TABLE S_BKTXT LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_XBLNR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  IF HF_SELE GT 0.                     " Ja, selektieren
    SELECT * FROM BKPF
      WHERE  BELNR IN S_BELNR
      AND    GJAHR IN S_GJAHR
      AND    BUKRS IN S_BUKRS
      AND    XBLNR IN S_XBLNR
      AND    BKTXT IN S_BKTXT.

      MOVE-CORRESPONDING BKPF TO I_TAB.

      SELECT * FROM BSEG
          WHERE  BELNR EQ BKPF-BELNR
          AND    GJAHR EQ BKPF-GJAHR
          AND    BUKRS EQ BKPF-BUKRS.
        MOVE-CORRESPONDING BSEG TO I_TAB.
        APPEND I_TAB.

        ADD 1 TO HF_COUNT.
        IF HF_COUNT GE P_MAXCT.
       MESSAGE I999 WITH 'Maximale Selektion' P_MAXCT 'Zeilen erreicht'.
          EXIT.
        ENDIF.

      ENDSELECT.
    ENDSELECT.
  ENDIF.


  CLEAR HF_SELE.
  DESCRIBE TABLE S_DMBTR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_BELNR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_GJAHR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_HKONT LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_LIFNR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_KUNNR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_ZUONR LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  DESCRIBE TABLE S_SGTXT LINES HF_PFELD.
  ADD HF_PFELD TO HF_SELE.
  IF HF_SELE GT 0.                     " Ja, selektieren
    SELECT * FROM BSEG
      WHERE  DMBTR IN S_DMBTR
      AND    BELNR IN S_BELNR
      AND    GJAHR IN S_GJAHR
      AND    BUKRS IN S_BUKRS
      AND    HKONT IN S_HKONT
      AND    LIFNR IN S_LIFNR
      AND    KUNNR IN S_KUNNR
      AND    ZUONR IN S_ZUONR
      AND    SGTXT IN S_SGTXT.

      MOVE-CORRESPONDING BSEG TO I_TAB.
      APPEND I_TAB.
*   write:  /
*     bseg-belnr,
*     bseg-gjahr,
*     bseg-dmbtr,
*     bseg-sgtxt.
      ADD 1 TO HF_COUNT.
      IF HF_COUNT GE P_MAXCT.
       MESSAGE I999 WITH 'Maximale Selektion' P_MAXCT 'Zeilen erreicht'.
        EXIT.
      ENDIF.
    ENDSELECT.
  ENDIF.

* call function 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*         I_PROGRAM_NAME         =
*         I_INTERNAL_TABNAME     =
*         I_STRUCTURE_NAME       =
*         I_CLIENT_NEVER_DISPLAY = 'X'
*         I_INCLNAME             =
*      changing
*           ct_fieldcat            = i_fieldcat
*      exceptions
*           inconsistent_interface = 1
*           program_error          = 2
*           others                 = 3.

  HF_DISVARIANT-REPORT = 'ZFBELSUC'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
*         I_INTERFACE_CHECK        = ' '
          I_CALLBACK_PROGRAM       = HF_REPID
*         i_callback_pf_status_set = hf_status
          I_CALLBACK_USER_COMMAND  = HF_USER_COMMAND
            I_STRUCTURE_NAME         = 'ZFBELSUC'
*         IS_LAYOUT                =
          IT_FIELDCAT              =  I_FIELDCAT[]
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =
*         IT_SORT                  =
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
            I_SAVE                   = 'A'
            IS_VARIANT               = HF_DISVARIANT
*         IT_EVENTS                =
*         IT_EVENT_EXIT            =
*         IS_PRINT                 =
*         I_SCREEN_START_COLUMN    = 0
*         I_SCREEN_START_LINE      = 0
*         I_SCREEN_END_COLUMN      = 0
*         I_SCREEN_END_LINE        = 0
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER  =
*         ES_EXIT_CAUSED_BY_USER   =
       TABLES
            T_OUTTAB                 = I_TAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
*&---------------------------------------------------------------------*
*&      Form  AUFBAUEN_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0075   text                                                *

*      -->P_0076   text                                                *

*----------------------------------------------------------------------*
FORM AUFBAUEN_FIELDCAT USING P_FIELDNAME
                             P_REF_TABNAME.

  I_FIELDCAT-FIELDNAME    = P_FIELDNAME.
  I_FIELDCAT-REF_TABNAME  = P_REF_TABNAME.
  APPEND I_FIELDCAT.

ENDFORM.                               " AUFBAUEN_FIELDCAT

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                  RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN  'WAHL'.                      "menubutton
      CASE RS_SELFIELD-SEL_TAB_FIELD.  " welches Feld
        WHEN 'I_TAB-LIFNR'.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'LIF' FIELD  I_TAB-LIFNR.
          CALL TRANSACTION 'FK03' AND SKIP FIRST SCREEN.
        WHEN 'I_TAB-KUNNR'.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'KUN' FIELD  I_TAB-KUNNR.
          CALL TRANSACTION 'FD03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'BLN' FIELD  I_TAB-BELNR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          CLEAR R_UCOMM.
      ENDCASE.
    WHEN '&IC1'.                       "dobbleclick
      CASE RS_SELFIELD-SEL_TAB_FIELD.  " welches Feld
        WHEN 'I_TAB-LIFNR'.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'LIF' FIELD  I_TAB-LIFNR.
          CALL TRANSACTION 'FK03' AND SKIP FIRST SCREEN.
        WHEN 'I_TAB-KUNNR'.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'KUN' FIELD  I_TAB-KUNNR.
          CALL TRANSACTION 'FD03' AND SKIP FIRST SCREEN.
        WHEN OTHERS.
          READ TABLE I_TAB INDEX RS_SELFIELD-TABINDEX. "cursorposit.
          SET PARAMETER ID 'BUK' FIELD  I_TAB-BUKRS.
          SET PARAMETER ID 'BLN' FIELD  I_TAB-BELNR.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          CLEAR R_UCOMM.
      ENDCASE.
  ENDCASE.
ENDFORM.
