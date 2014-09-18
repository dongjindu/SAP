*&---------------------------------------------------------------------*
*& Report  ZRIMLDCST                                                   *
*&---------------------------------------------------------------------*
*&  Program : LDC Rate Management Status.
*&     Name : SH, Na INFOLINK Ltd.                                     *
*&     Date : 2004.02.13                                               *
*&---------------------------------------------------------------------*
*&    Desc. :                                                          *
*&---------------------------------------------------------------------*
REPORT  ZRIMLDCST    MESSAGE-ID ZIM
                      LINE-SIZE 155
                      NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE: <ICON>.
TABLES : ZTBKPF, ZTBSEG, ZTBDIV, ZTBHIS,
         T163B, T163C, T685T, LFA1, MARA,
         T005T, T134T,
         EKBZ, EKBE, KONV, EKKO, EKPO,
         ZTIMIMG01, ZTIMIMG00, ZTIMIMG08,
         ZTREQHD, ZTREQIT, RV61A.

TYPE-POOLS : SLIS.

*----------------------------------------------------------------------*
* Internal TAble Declaration                                           *
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_REQ OCCURS 0,
       EBELN           LIKE   ZTREQIT-EBELN,
       EBELP           LIKE   ZTREQIT-EBELP.
DATA : END   OF IT_REQ.

DATA : BEGIN OF IT_TAB OCCURS 0,
         LAND1   LIKE LFA1-LAND1,         " Country Type..
         LANDX   LIKE T005T-LANDX,        " Country Desc..
         LIFNR   LIKE LFA1-LIFNR,         " Vendor Code..
         NAME1   LIKE LFA1-NAME1,         " Vendor Name..
         MATNR   LIKE MARA-MATNR,         " Material number..
         MTART   LIKE MARA-MTART,         " MIP/LP/KD..
         MTBEZ   LIKE T134T-MTBEZ,        " Desc. of material type..
         TRANS   LIKE ZTREQHD-ZFTRANS,    " Transportation type..
         PB00    LIKE BSEG-DMBTR,         " [Tot] Gross-Price Amount..
         FRA1    LIKE BSEG-DMBTR,         " [Tot] Ocean Freight Amount..
** changed by Furong on 08/30/2005
         FRA1R   TYPE ZTMM_6026_01-NETPRUOM, " [Tot] Ocean Freight Rate.
         ZOTI    LIKE BSEG-DMBTR,            " [Tot] B/L Expense Amount.
         ZOTIR   LIKE ZTMM_6026_01-NETPRUOM, " [Tot] B/L Expense Rate..
         ZOTH    LIKE BSEG-DMBTR,            " [Tot] L/C Expense Amount.
         ZOTHR   LIKE ZTMM_6026_01-NETPRUOM, " [Tot] L/C Expense Rate..
         PB00_O  LIKE BSEG-DMBTR,            " [Ocn] Gross-Price Amount.
         FRA1_O  LIKE BSEG-DMBTR,            " [Ocn] Ocean Freight amt.
         FRA1R_O LIKE ZTMM_6026_01-NETPRUOM, " [Ocn] Ocean Freight
         ZOTI_O  LIKE BSEG-DMBTR,            " [Ocn] B/L Expense Amount.
         ZOTIR_O LIKE ZTMM_6026_01-NETPRUOM, " [Ocn] B/L Expense
         ZOTH_O  LIKE BSEG-DMBTR,            " [Ocn] L/C Expense Amount.
         ZOTHR_O LIKE ZTMM_6026_01-NETPRUOM, " [Ocn] L/C Expense
         PB00_A  LIKE BSEG-DMBTR,            " [Air] Gross-Price Amount.
         FRA1_A  LIKE BSEG-DMBTR,            " [Air] Ocean Freight Amt.
         FRA1R_A LIKE ZTMM_6026_01-NETPRUOM, " [Air] Ocean Freight Rate.
         ZOTI_A  LIKE BSEG-DMBTR,            " [Air] B/L Expense Amount.
         ZOTIR_A LIKE ZTMM_6026_01-NETPRUOM,  " [Air] B/L Expense Rate..
         ZOTH_A  LIKE BSEG-DMBTR,             " [Air] L/C Expense Amt.
         ZOTHR_A LIKE ZTMM_6026_01-NETPRUOM.  " [Air] L/C Expense Rate.

*         FRA1R   TYPE ZTPMTHD-ZFUSITR,    " [Tot] Ocean Freight Rate..
*         ZOTI    LIKE BSEG-DMBTR,         " [Tot] B/L Expense Amount..
*         ZOTIR   LIKE ZTPMTHD-ZFUSITR,    " [Tot] B/L Expense Rate..
*         ZOTH    LIKE BSEG-DMBTR,         " [Tot] L/C Expense Amount..
*         ZOTHR   LIKE ZTPMTHD-ZFUSITR,    " [Tot] L/C Expense Rate..
*         PB00_O  LIKE BSEG-DMBTR,         " [Ocn] Gross-Price Amount..
*         FRA1_O  LIKE BSEG-DMBTR,         " [Ocn] Ocean Freight Amount.
.
*         FRA1R_O LIKE ZTPMTHD-ZFUSITR,    " [Ocn] Ocean Freight
*         ZOTI_O  LIKE BSEG-DMBTR,         " [Ocn] B/L Expense Amount..
*         ZOTIR_O LIKE ZTPMTHD-ZFUSITR,    " [Ocn] B/L Expense
*         ZOTH_O  LIKE BSEG-DMBTR,         " [Ocn] L/C Expense Amount..
*         ZOTHR_O LIKE ZTPMTHD-ZFUSITR,    " [Ocn] L/C Expense
*         PB00_A  LIKE BSEG-DMBTR,         " [Air] Gross-Price Amount..
*         FRA1_A  LIKE BSEG-DMBTR,         " [Air] Ocean Freight Amount.
.
*         FRA1R_A LIKE ZTPMTHD-ZFUSITR,    " [Air] Ocean Freight Rate..
*         ZOTI_A  LIKE BSEG-DMBTR,         " [Air] B/L Expense Amount..
*         ZOTIR_A LIKE ZTPMTHD-ZFUSITR,    " [Air] B/L Expense Rate..
*         ZOTH_A  LIKE BSEG-DMBTR,         " [Air] L/C Expense Amount..
*         ZOTHR_A LIKE ZTPMTHD-ZFUSITR.    " [Air] L/C Expense Rate..

** end of change
DATA : END OF   IT_TAB.

DATA : BEGIN OF IT_KO OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA :   EBELN    LIKE EKKO-EBELN,
         EBELP    LIKE EKPO-EBELP,
         SHKZG    LIKE EKBE-SHKZG,
         SHKZG1   LIKE EKBZ-SHKZG,
         BEWTP    LIKE EKBE-BEWTP,
         GJAHR    LIKE EKBE-GJAHR,
         BELNR    LIKE EKBE-BELNR,
         ZFTRANS  LIKE ZTREQHD-ZFTRANS,
       END OF IT_KO.

DATA : BEGIN OF IT_LAND OCCURS 0.
         INCLUDE STRUCTURE T005T.
DATA : END OF IT_LAND.

DATA : BEGIN OF IT_LFA OCCURS 0.
         INCLUDE STRUCTURE LFA1.
DATA : END OF IT_LFA.

DATA : BEGIN OF IT_T134T OCCURS 0.
         INCLUDE STRUCTURE T134T.
DATA : END OF IT_T134T.

DATA : BEGIN OF IT_DIV OCCURS 0,
         EBELN     LIKE   ZTBDIV-EBELN,      " PO Header.
         EBELP     LIKE   ZTBDIV-EBELP,      " PO Item.
         ZFRVSX    LIKE   ZTBKPF-ZFRVSX,     " Reverse Posting Yes/No..
         BUKRS     LIKE   ZTBDIV-BUKRS,      " Company Code..
         ZFACDO    LIKE   ZTBKPF-ZFACDO,     " Charge document No..
         ZFFIYR    LIKE   ZTBKPF-ZFFIYR,     " Fiscal Year..
         ZFDCSTX   LIKE   ZTBDIV-ZFDCSTX,    " Delivery Cost Yes/No..
         ZFCSTGRP  LIKE   ZTBDIV-ZFCSTGRP,   " Cost Group..
         ZFCD      LIKE   ZTBDIV-ZFCD,       " Cost Code..
         WRBTR     LIKE   ZTBDIV-WRBTR,      " Amount..
         DMBTR     LIKE   ZTBDIV-DMBTR,      " Local Amount.
         HWAER     LIKE   ZTBDIV-HWAER,      " Currency.
         MENGE     LIKE   ZTBDIV-MENGE,      " Quantity.
         MEINS     LIKE   ZTBDIV-MEINS.      " Unit.
DATA : END   OF IT_DIV.

DATA : BEGIN OF IT_EKBZ OCCURS 0,
         EBELN     LIKE   EKBZ-EBELN,        " PO Header
         EBELP     LIKE   EKBZ-EBELP,        " PO Item
         VGABE     LIKE   EKBZ-VGABE,        " Transaction Type.
         GJAHR     LIKE   EKBZ-GJAHR,        " Fiscal Year.
         BELNR     LIKE   EKBZ-BELNR,        " Document No..
         BEWTP     LIKE   EKBZ-BEWTP,        " PO History Type.
         DMBTR     LIKE   EKBZ-DMBTR,        " Amount in local currency.
         HSWAE     LIKE   EKBZ-HSWAE,        " Local currency key.
         MENGE     LIKE   EKBZ-MENGE,        " Quantity.
         MEINS     LIKE   EKPO-MEINS,        " Unit.
         SHKZG     LIKE   EKBZ-SHKZG,        " Debit/credit indicator.
         KSCHL     LIKE   EKBZ-KSCHL.        " Condition type.
DATA : END   OF IT_EKBZ.

DATA : BEGIN OF IT_EKBE  OCCURS 0,
         EBELN     LIKE   EKBE-EBELN,        " PO Header
         EBELP     LIKE   EKBE-EBELP,        " PO Item
         VGABE     LIKE   EKBE-VGABE,        " Transaction Type.
         GJAHR     LIKE   EKBE-GJAHR,        " Fiscal Year.
         BELNR     LIKE   EKBE-BELNR,        " Document No..
         BEWTP     LIKE   EKBE-BEWTP,        " PO History Type.
         DMBTR     LIKE   EKBE-DMBTR,        " Amount in local currency.
         HSWAE     LIKE   EKBE-HSWAE,        " Currency.
         MENGE     LIKE   EKBE-MENGE,        " Quantity.
         MEINS     LIKE   EKPO-MEINS,        " Unit.
         SHKZG     LIKE   EKBE-SHKZG.        " Debit/credit indicator.
DATA : END   OF IT_EKBE.

*----------------------------------------------------------------------*
*   Declaration Variables..
*----------------------------------------------------------------------*
DATA : W_ERR_CHK      TYPE  C,
       W_LINE         TYPE  I,
       W_LINES        TYPE  I,
       W_LAND1        LIKE  LFA1-LAND1,
       W_PB00         LIKE  EKBZ-DMBTR,
       W_PB00_O       LIKE  EKBZ-DMBTR,
       W_PB00_A       LIKE  EKBZ-DMBTR,
       W_FRA1         LIKE  EKBZ-DMBTR,
       W_FRA1_O       LIKE  EKBZ-DMBTR,
       W_FRA1_A       LIKE  EKBZ-DMBTR,
       W_ZOTH         LIKE  EKBZ-DMBTR,
       W_ZOTH_O       LIKE  EKBZ-DMBTR,
       W_ZOTH_A       LIKE  EKBZ-DMBTR,
       W_ZOTI         LIKE  EKBZ-DMBTR,
       W_ZOTI_O       LIKE  EKBZ-DMBTR,
       W_ZOTI_A       LIKE  EKBZ-DMBTR,
       W_LIFNR        LIKE  LFA1-LIFNR,
       W_MTART        LIKE  MARA-MTART.
DATA : W_SORT(10)     TYPE  C.
DATA : P_BUKRS        LIKE  ZTIMIMG00-ZFBUKRS.

*>> Declaration of variable for ALV Display.
DATA: G_REPID LIKE SY-REPID.
DATA: G_LAYOUT          TYPE SLIS_LAYOUT_ALV.
DATA: G_STATUS          TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.
DATA: GT_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV.
DATA: GT_SORT           TYPE SLIS_T_SORTINFO_ALV.
DATA: LS_FIELDCAT       TYPE SLIS_FIELDCAT_ALV.
DATA: LS_SORT           TYPE SLIS_SORTINFO_ALV.
DATA: POS               TYPE I.
DATA: G_USER_COMMAND    TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.
DATA: W_LIST_INDEX      LIKE SY-TABIX.
*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS  FOR ZTREQHD-BUKRS NO-EXTENSION NO INTERVALS,
                S_LAND1  FOR LFA1-LAND1,
                S_MTART  FOR MARA-MTART,
                S_LIFNR  FOR EKKO-LIFNR,
                S_BEDAT  FOR EKKO-BEDAT,
                S_BUDAT  FOR EKBZ-BUDAT,
                S_ZTERM  FOR EKKO-ZTERM NO-DISPLAY,
                S_BSART  FOR EKKO-BSART NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN : BEGIN OF LINE,
                   COMMENT 01(15) TEXT-R01.
SELECTION-SCREEN : COMMENT 24(8) TEXT-R03.
PARAMETERS : P_C RADIOBUTTON GROUP RDG DEFAULT 'X'.
SELECTION-SCREEN : COMMENT 40(8) TEXT-R02.
PARAMETERS : P_V RADIOBUTTON GROUP RDG.
SELECTION-SCREEN : COMMENT 56(8) TEXT-R04.
PARAMETERS : P_M RADIOBUTTON GROUP RDG.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_ALV AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.
*>> Initial Value Setting.
INITIALIZATION.                          " Initial Values Setting.
  PERFORM  P1000_SET_BUKRS.
  SET  TITLEBAR  'ZRIMY6'.               " GUI TITLE  SETTING

*-----------------------------------------------------------------------
* Start of Selection..
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> Import System Configuration Check.
  PERFORM P1000_CONFIG_CHECK   USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.   EXIT. ENDIF.

*>> Data Selection.
  PERFORM P2000_READ_DATA      USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* End of Selection..
*-----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM P3000_DATA_WRITE.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P1000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
*>> Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.

*>> Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  SELECT *
    FROM ZTIMIMG01
   WHERE BSTYP = 'F'.

*>> Purchasing document type appending..
    MOVE ZTIMIMG01-BSART TO S_BSART-LOW.
    MOVE 'I'             TO S_BSART-SIGN.
    MOVE 'EQ'            TO S_BSART-OPTION.
    APPEND S_BSART.

*>> Payment term appending..
    MOVE ZTIMIMG01-ZTERM TO S_ZTERM-LOW.
    MOVE 'I'             TO S_ZTERM-SIGN.
    MOVE 'EQ'            TO S_ZTERM-OPTION.
    APPEND S_ZTERM.
  ENDSELECT.

ENDFORM.                    " P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P2000_READ_DATA USING    W_ERR_CHK.
  CLEAR W_LAND1.
*>> Select P/O appropriate for Selection Condition.
  SELECT A~EBELN B~EBELP A~LIFNR C~MATNR C~MTART
    INTO (IT_KO-EBELN, IT_KO-EBELP, IT_KO-LIFNR,
          IT_KO-MATNR, IT_KO-MTART)
    FROM EKKO AS A INNER JOIN EKPO AS B
         ON A~EBELN EQ B~EBELN
         INNER JOIN MARA AS C
            ON B~MATNR EQ C~MATNR
            WHERE A~LIFNR IN S_LIFNR
              AND A~BEDAT IN S_BEDAT
              AND A~BSART IN S_BSART
              AND A~ZTERM IN S_ZTERM
              AND A~BSTYP EQ 'F'
              AND A~EBELN NE SPACE
              AND C~MTART IN S_MTART
              AND C~MATNR NE SPACE.

    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
               FROM LFA1
              WHERE LIFNR =  IT_KO-LIFNR
                AND LAND1 IN S_LAND1.
      IF SY-SUBRC EQ 0.
        MOVE LFA1-LAND1 TO IT_KO-LAND1.
*>> Select Good Price.
        PERFORM P2000_MAKE_PB00.
        IF NOT IT_KO-PB00 IS INITIAL.
*>> Select Freight & Other Charge..
          PERFORM P2000_MAKE_FRA1.
          APPEND IT_KO.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.

  IF P_C EQ 'X'.                 "> In case of country base selection.
    SORT IT_KO BY LAND1.
    READ TABLE IT_KO INDEX 1.
    MOVE IT_KO-LAND1 TO W_LAND1.
    LOOP AT IT_KO.
      ON CHANGE OF IT_KO-LAND1.
        MOVE IT_KO-LAND1 TO S_LAND1-LOW.
        MOVE 'I'         TO S_LAND1-SIGN.
        MOVE 'EQ'        TO S_LAND1-OPTION.
        APPEND S_LAND1.
      ENDON.
    ENDLOOP.
    SELECT * INTO TABLE IT_LAND
             FROM T005T
            WHERE LAND1 IN S_LAND1
              AND SPRAS =  SY-LANGU.
  ELSEIF P_V EQ 'X'.             "> In case of vendor base selection.
    SORT IT_KO BY LIFNR.
    READ TABLE IT_KO INDEX 1.
    MOVE IT_KO-LIFNR TO W_LIFNR.
    LOOP AT IT_KO.
      ON CHANGE OF IT_KO-LIFNR.
        MOVE IT_KO-LIFNR TO S_LIFNR-LOW.
        MOVE 'I'         TO S_LIFNR-SIGN.
        MOVE 'EQ'        TO S_LIFNR-OPTION.
        APPEND S_LIFNR.
      ENDON.
    ENDLOOP.
    SELECT * INTO TABLE IT_LFA
             FROM LFA1
            WHERE LIFNR IN S_LIFNR.
  ELSEIF P_M EQ 'X'.
    SORT IT_KO BY MTART.
    READ TABLE IT_KO INDEX 1.
    MOVE IT_KO-MTART TO W_MTART.
    LOOP AT IT_KO.
      ON CHANGE OF IT_KO-MATNR.
        MOVE IT_KO-MTART TO S_MTART-LOW.
        MOVE 'I'         TO S_MTART-SIGN.
        MOVE 'EQ'        TO S_MTART-OPTION.
        APPEND S_MTART.
      ENDON.
    ENDLOOP.
    SELECT * INTO TABLE IT_T134T
             FROM T134T
            WHERE MTART IN S_MTART
              AND SPRAS =  SY-LANGU.
  ENDIF.

  DESCRIBE TABLE IT_KO LINES W_LINES.
  LOOP AT IT_KO.
    IF P_C EQ 'X'.
      IF IT_KO-LAND1 EQ W_LAND1.
      ELSE.
        MOVE W_LAND1 TO IT_TAB-LAND1.
        READ TABLE IT_LAND WITH KEY LAND1 = IT_TAB-LAND1.
        MOVE IT_LAND-LANDX TO IT_TAB-LANDX.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.
      MOVE IT_KO-LAND1 TO W_LAND1.
      PERFORM P3000_MAKE_PB00.
      PERFORM P3000_MAKE_FRA1.
      PERFORM P3000_MAKE_ZOTI.
      PERFORM P3000_MAKE_ZOTH.
      IF SY-TABIX EQ W_LINES.
        MOVE W_LAND1 TO IT_TAB-LAND1.
        READ TABLE IT_LAND WITH KEY LAND1 = IT_TAB-LAND1.
        MOVE IT_LAND-LANDX TO IT_TAB-LANDX.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.

    ELSEIF P_V EQ 'X'.
      IF IT_KO-LIFNR EQ W_LIFNR.
      ELSE.
        MOVE W_LIFNR TO IT_TAB-LIFNR.
        READ TABLE IT_LFA WITH KEY LIFNR = IT_TAB-LIFNR.
        MOVE IT_LFA-NAME1 TO IT_TAB-NAME1.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.
      MOVE IT_KO-LIFNR TO W_LIFNR.
      PERFORM P3000_MAKE_PB00.
      PERFORM P3000_MAKE_FRA1.
      PERFORM P3000_MAKE_ZOTI.
      PERFORM P3000_MAKE_ZOTH.
      IF SY-TABIX EQ W_LINES.
        MOVE W_LIFNR TO IT_TAB-LIFNR.
        READ TABLE IT_LFA WITH KEY LIFNR = IT_TAB-LIFNR.
        MOVE IT_LFA-NAME1 TO IT_TAB-NAME1.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.
    ELSEIF P_M EQ 'X'.
      IF IT_KO-MTART EQ W_MTART.
      ELSE.
        MOVE W_MTART TO IT_TAB-MTART.
        READ TABLE IT_T134T WITH KEY MTART = IT_TAB-MTART.
        MOVE IT_T134T-MTBEZ TO IT_TAB-MTBEZ.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.
      MOVE IT_KO-MTART TO W_MTART.
      PERFORM P3000_MAKE_PB00.
      PERFORM P3000_MAKE_FRA1.
      PERFORM P3000_MAKE_ZOTI.
      PERFORM P3000_MAKE_ZOTH.
      IF SY-TABIX EQ W_LINES.
        MOVE W_MTART TO IT_TAB-MTART.
        READ TABLE IT_T134T WITH KEY MTART = IT_TAB-MTART.
        MOVE IT_T134T-MTBEZ TO IT_TAB-MTBEZ.
        APPEND IT_TAB. CLEAR IT_TAB.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_TAB.
    IF NOT IT_TAB-PB00 IS INITIAL.
      IT_TAB-FRA1R   = ( IT_TAB-FRA1   / IT_TAB-PB00 )   * 100.
    ELSE. IT_TAB-FRA1R = 0. ENDIF.
    IF NOT IT_TAB-PB00_O IS INITIAL.
      IT_TAB-FRA1R_O = ( IT_TAB-FRA1_O / IT_TAB-PB00_O ) * 100.
    ELSE. IT_TAB-FRA1R_O = 0. ENDIF.
    IF NOT IT_TAB-PB00_A IS INITIAL.
      IT_TAB-FRA1R_A = ( IT_TAB-FRA1_A / IT_TAB-PB00_A ) * 100.
    ELSE. IT_TAB-FRA1_A = 0. ENDIF.
    IF NOT IT_TAB-PB00 IS INITIAL.
      IT_TAB-ZOTIR   = ( IT_TAB-ZOTI   / IT_TAB-PB00 )   * 100.
    ELSE. IT_TAB-ZOTIR = 0. ENDIF.
    IF NOT IT_TAB-PB00_O IS INITIAL.
      IT_TAB-ZOTIR_O = ( IT_TAB-ZOTI_O / IT_TAB-PB00_O ) * 100.
    ELSE. IT_TAB-ZOTIR_O = 0. ENDIF.
    IF NOT IT_TAB-PB00_A IS INITIAL.
      IT_TAB-ZOTIR_A = ( IT_TAB-ZOTI_A / IT_TAB-PB00_A ) * 100.
    ELSE. IT_TAB-ZOTIR_A = 0. ENDIF.
    IF NOT IT_TAB-PB00 IS INITIAL.
      IT_TAB-ZOTHR   = ( IT_TAB-ZOTH   / IT_TAB-PB00 )   * 100.
    ELSE. IT_TAB-ZOTHR = 0. ENDIF.
    IF NOT IT_TAB-PB00_O IS INITIAL.
      IT_TAB-ZOTHR_O = ( IT_TAB-ZOTH_O / IT_TAB-PB00_O ) * 100.
    ELSE. IT_TAB-ZOTHR_O = 0. ENDIF.
    IF NOT IT_TAB-PB00_A IS INITIAL.
      IT_TAB-ZOTHR_A = ( IT_TAB-ZOTH_A / IT_TAB-PB00_A ) * 100.
    ELSE. IT_TAB-ZOTHR_A = 0. ENDIF.
    MODIFY IT_TAB INDEX SY-TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P2000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET TITLEBAR  'ZRIMY6'.

  IF P_ALV EQ 'X'.
    PERFORM P3000_ALV_WRITE.
  ELSE.
    PERFORM P3000_REPORT_WRITE.
  ENDIF.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company Code Setting..
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
FORM P3000_APPEND_FIELDCAT.

  CLEAR: GT_FIELDCAT, GT_SORT, POS.
  IF P_C EQ 'X'.
    POS = POS + 1.
    LS_FIELDCAT-COL_POS        = POS.
    LS_FIELDCAT-FIELDNAME      = 'LAND1'.
    LS_FIELDCAT-SELTEXT_M      = 'Country'.
    LS_FIELDCAT-OUTPUTLEN      = 3.
    LS_FIELDCAT-EMPHASIZE      = 'C200'.
    LS_SORT-FIELDNAME          = 'LAND1'.
    LS_FIELDCAT-KEY            = 'X'.
    APPEND LS_SORT     TO GT_SORT.
    APPEND LS_FIELDCAT TO GT_FIELDCAT.
  ELSEIF P_V EQ 'X'.
    POS = POS + 1.
    LS_FIELDCAT-COL_POS        = POS.
    LS_FIELDCAT-FIELDNAME      = 'LIFNR'.
    LS_FIELDCAT-SELTEXT_M      = 'Vendor'.
    LS_FIELDCAT-OUTPUTLEN      = 10.
    LS_FIELDCAT-EMPHASIZE      = 'C200'.
    LS_SORT-FIELDNAME          = 'LIFNR'.
    LS_FIELDCAT-KEY            = 'X'.
    APPEND LS_SORT     TO GT_SORT.
    APPEND LS_FIELDCAT TO GT_FIELDCAT.
  ELSEIF P_M EQ 'X'.
    POS = POS + 1.
    LS_FIELDCAT-COL_POS        = POS.
    LS_FIELDCAT-FIELDNAME      = 'MTART'.
    LS_FIELDCAT-SELTEXT_M      = 'Mat'.
    LS_FIELDCAT-OUTPUTLEN      = 3.
    LS_FIELDCAT-EMPHASIZE      = 'C200'.
    LS_SORT-FIELDNAME          = 'MTART'.
    LS_FIELDCAT-KEY            = 'X'.
    APPEND LS_SORT     TO GT_SORT.
    APPEND LS_FIELDCAT TO GT_FIELDCAT.
  ENDIF.

******************************* Good Price *****************************
*>> Good Price.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'PB00'.
  LS_FIELDCAT-SELTEXT_M      = 'Good Price'.
  LS_FIELDCAT-OUTPUTLEN      = 15.
  LS_FIELDCAT-EMPHASIZE      = 'C100'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Good Price Ocean.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'PB00_O'.
  LS_FIELDCAT-SELTEXT_M      = 'Ocean'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C100'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Good Price Air.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'PB00_A'.
  LS_FIELDCAT-SELTEXT_M      = 'Air'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C100'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

****************************** Freight *********************************
*>> Freight Total.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1'.
  LS_FIELDCAT-SELTEXT_M      = 'Freight'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Freight Total Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1R'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Freight)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Freight Ocean.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1_O'.
  LS_FIELDCAT-SELTEXT_M      = 'Ocean'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Freight Ocean Rate.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1R_O'.
  LS_FIELDCAT-SELTEXT_M      = ' Rate(Ocean)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Freight Air.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1_A'.
  LS_FIELDCAT-SELTEXT_M      = 'Air'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Freight Air Rate.
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'FRA1R_A'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Air)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C400'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

************************* Other Expense[B/L] ***************************
*>> Other Expense[B/L]..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTI'.
  LS_FIELDCAT-SELTEXT_M      = 'B/L Other'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[B/L] Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTIR'.
  LS_FIELDCAT-SELTEXT_M      = 'B/L Other Rate'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[B/L] Ocean..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTI_O'.
  LS_FIELDCAT-SELTEXT_M      = 'B/L Other(Ocean)'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[B/L] Ocean Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTIR_O'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Ocean)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[B/L] Air..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTI_A'.
  LS_FIELDCAT-SELTEXT_M      = 'B/L Other(Air)'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[B/L] Air Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTIR_A'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Air)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C500'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

************************* Other Expense[L/C] ***************************
*>> Other Expense[L/C]..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTH'.
  LS_FIELDCAT-SELTEXT_M      = 'L/C Other'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[L/C] Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTHR'.
  LS_FIELDCAT-SELTEXT_M      = 'L/C Other Rate'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[L/C] Ocean..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTH_O'.
  LS_FIELDCAT-SELTEXT_M      = 'L/C Other(Ocean)'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[L/C] Ocean Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTHR_O'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Ocean)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[L/C] Air..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTH_A'.
  LS_FIELDCAT-SELTEXT_M      = 'L/C Other(Air)'.
  LS_FIELDCAT-OUTPUTLEN      = 11.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

*>> Other Expense[L/C] Air Rate..
  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZOTHR_A'.
  LS_FIELDCAT-SELTEXT_M      = 'Rate(Air)'.
  LS_FIELDCAT-OUTPUTLEN      = 5.
  LS_FIELDCAT-EMPHASIZE      = 'C300'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_PB00
*&---------------------------------------------------------------------*
FORM P2000_MAKE_PB00.
  CLEAR: W_PB00, W_PB00_O, W_PB00_A.

  SELECT *
    FROM EKBE
   WHERE EBELN =  IT_KO-EBELN
     AND EBELP =  IT_KO-EBELP
     AND BEWTP =  'Q'
     AND VGABE =  '2'
     AND BUDAT IN S_BUDAT.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
               FROM ZTREQHD
              WHERE EBELN = IT_KO-EBELN.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING EKBE TO IT_KO.
        IF ZTREQHD-ZFTRANS EQ 'O'.
          IF EKBE-SHKZG EQ 'S'.
            W_PB00_O = W_PB00_O + EKBE-DMBTR.
          ELSE.
            W_PB00_O = W_PB00_O - EKBE-DMBTR.
          ENDIF.
        ELSE.
          IF EKBE-SHKZG EQ 'S'.
            W_PB00_A = W_PB00_A + EKBE-DMBTR.
          ELSE.
            W_PB00_A = W_PB00_A - EKBE-DMBTR.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.

  MOVE W_PB00_A TO IT_KO-PB00_A.
  MOVE W_PB00_O TO IT_KO-PB00_O.
  IT_KO-PB00 = IT_KO-PB00_O + IT_KO-PB00_A.

ENDFORM.                    " P2000_MAKE_PB00
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_FRA1
*&---------------------------------------------------------------------*
FORM P2000_MAKE_FRA1.
  CLEAR : W_FRA1, W_FRA1_O, W_FRA1_A.
  CLEAR : W_ZOTI, W_ZOTI_O, W_ZOTI_A.
  CLEAR : W_ZOTH, W_ZOTH_O, W_ZOTH_A.

  SELECT *
    FROM ZTBDIV
   WHERE EBELN     = IT_KO-EBELN
     AND EBELP     = IT_KO-EBELP.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE *
        FROM ZTBKPF
       WHERE BUKRS =  ZTBDIV-BUKRS
         AND BELNR =  ZTBDIV-BELNR
         AND GJAHR =  ZTBDIV-GJAHR
         AND BUDAT IN S_BUDAT.
      IF ZTBKPF-ZFPOSYN EQ 'Y'.
        IF ZTBDIV-COND_TYPE EQ 'FRA1'.
          IF ZTBDIV-ZFCD EQ 'OBC'.
            W_FRA1_O = W_FRA1_O + ZTBDIV-DMBTR.
          ELSEIF ZTBDIV-ZFCD EQ 'ABC'.
            W_FRA1_A = W_FRA1_A + ZTBDIV-DMBTR.
          ENDIF.
        ELSEIF ZTBDIV-COND_TYPE EQ 'ZOTI'.
          IF ZTREQHD-ZFTRANS EQ 'O'.
            W_ZOTI_O = W_ZOTI_O + ZTBDIV-DMBTR.
          ELSE.
            W_ZOTI_A = W_ZOTI_A + ZTBDIV-DMBTR.
          ENDIF.
        ELSEIF ZTBDIV-COND_TYPE EQ 'ZOTH'.
          IF ZTREQHD-ZFTRANS EQ 'O'.
            W_ZOTH_O = W_ZOTH_O + ZTBDIV-DMBTR.
          ELSE.
            W_ZOTH_A = W_ZOTH_A + ZTBDIV-DMBTR.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDSELECT.

  MOVE W_FRA1_A TO IT_KO-FRA1_A.
  MOVE W_FRA1_O TO IT_KO-FRA1_O.
  IT_KO-FRA1 = W_FRA1_A + W_FRA1_O.

  MOVE W_ZOTI_A TO IT_KO-ZOTI_A.
  MOVE W_ZOTI_O TO IT_KO-ZOTI_O.
  IT_KO-ZOTI = W_ZOTI_A + W_ZOTI_O.

  MOVE W_ZOTH_A TO IT_KO-ZOTH_A.
  MOVE W_ZOTH_O TO IT_KO-ZOTH_O.
  IT_KO-ZOTH = W_ZOTH_A + W_ZOTH_O.

ENDFORM.                    " P2000_MAKE_FRA1
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_PB00
*&---------------------------------------------------------------------*
FORM P3000_MAKE_PB00.

  IT_TAB-PB00   = IT_TAB-PB00   + IT_KO-PB00.
  IT_TAB-PB00_O = IT_TAB-PB00_O + IT_KO-PB00_O.
  IT_TAB-PB00_A = IT_TAB-PB00_A + IT_KO-PB00_A.

ENDFORM.                    " P3000_MAKE_PB00
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_FRA1
*&---------------------------------------------------------------------*
FORM P3000_MAKE_FRA1.

  IT_TAB-FRA1   = IT_TAB-FRA1   + IT_KO-FRA1.
  IT_TAB-FRA1_O = IT_TAB-FRA1_O + IT_KO-FRA1_O.
  IT_TAB-FRA1_A = IT_TAB-FRA1_A + IT_KO-FRA1_A.

ENDFORM.                    " P3000_MAKE_FRA1
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_ZOTI
*&---------------------------------------------------------------------*
FORM P3000_MAKE_ZOTI.

  IT_TAB-ZOTI   = IT_TAB-ZOTI   + IT_KO-ZOTI.
  IT_TAB-ZOTI_O = IT_TAB-ZOTI_O + IT_KO-ZOTI_O.
  IT_TAB-ZOTI_A = IT_TAB-ZOTI_A + IT_KO-ZOTI_A.

ENDFORM.                    " P3000_MAKE_ZOTI
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_ZOTH
*&---------------------------------------------------------------------*
FORM P3000_MAKE_ZOTH.

  IT_TAB-ZOTH   = IT_TAB-ZOTH   + IT_KO-ZOTH.
  IT_TAB-ZOTH_O = IT_TAB-ZOTH_O + IT_KO-ZOTH_O.
  IT_TAB-ZOTH_A = IT_TAB-ZOTH_A + IT_KO-ZOTH_A.

ENDFORM.                    " P3000_MAKE_ZOTH
*&---------------------------------------------------------------------*
*&      Form  P3000_ALV_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ALV_WRITE.

  PERFORM P3000_APPEND_FIELDCAT.      " ALV Report TiTle.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = G_REPID
            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
            IS_LAYOUT               = G_LAYOUT
            I_GRID_TITLE            = 'LDC Rate Status List'
            I_SAVE                  = 'A'
            IT_SORT                 = GT_SORT[]
            IT_FIELDCAT             = GT_FIELDCAT[]
       TABLES
            T_OUTTAB                = IT_TAB
       EXCEPTIONS
            PROGRAM_ERROR           = 1
            OTHERS                  = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E977 WITH 'Error occured on displaying grid.'.
  ENDIF.

ENDFORM.                    " P3000_ALV_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_REPORT_WRITE
*&---------------------------------------------------------------------*
FORM P3000_REPORT_WRITE.

  PERFORM P3000_TITLE_WRITE.
  LOOP AT IT_TAB.
    PERFORM P3000_LINE_WRITE.
    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P3000_REPORT_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE:/55  '[ LDC Rate Status List ]' CENTERED
             COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/122 'Date : ', SY-DATUM.  ", 93 'Page : ', W_PAGE.

  IF P_C EQ 'X'.
    MOVE 'Country'   TO W_SORT.
  ELSEIF P_V EQ 'X'.
    MOVE 'Vendor'    TO W_SORT.
  ELSEIF P_M EQ 'X'.
    MOVE 'Material'  TO W_SORT.
  ENDIF.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE NO-GAP,
      (25)W_SORT             NO-GAP CENTERED, SY-VLINE NO-GAP,
      (24)'Good Price'       NO-GAP CENTERED, SY-VLINE NO-GAP,
      (23)'Freight'          NO-GAP CENTERED, SY-VLINE NO-GAP,
      (11)'Rate'             NO-GAP CENTERED, SY-VLINE NO-GAP,
      (23)'B/L Other Charge' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (08)'Rate'             NO-GAP CENTERED, SY-VLINE NO-GAP,
      (23)'L/C Other Charge' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (08)'Rate'             NO-GAP CENTERED, SY-VLINE NO-GAP.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE NO-GAP.
  IF P_C EQ 'X'.
    WRITE: (10)IT_TAB-LAND1 NO-GAP, (15)IT_TAB-LANDX NO-GAP.
  ELSEIF P_V EQ 'X'.
    WRITE: (10)IT_TAB-LIFNR NO-GAP, (15)IT_TAB-NAME1 NO-GAP.
  ELSEIF P_M EQ 'X'.
    WRITE: (10)IT_TAB-MTART NO-GAP, (15)IT_TAB-MTBEZ NO-GAP.
  ENDIF.
  WRITE: SY-VLINE NO-GAP,
      (24)IT_TAB-PB00   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-FRA1   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (11)IT_TAB-FRA1R  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTI   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTIR  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTH   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTHR  NO-GAP,  SY-VLINE NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE:/ SY-VLINE NO-GAP, (25)'(Ocean)' NO-GAP, SY-VLINE NO-GAP,
      (24)IT_TAB-PB00_O   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-FRA1_O   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (11)IT_TAB-FRA1R_O  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTI_O   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTIR_O  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTH_O   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTHR_O  NO-GAP,  SY-VLINE NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE:/ SY-VLINE NO-GAP, (25)'(Air)' NO-GAP, SY-VLINE NO-GAP,
      (24)IT_TAB-PB00_A   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-FRA1_A   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (11)IT_TAB-FRA1R_A  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTI_A   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTIR_A  NO-GAP,  SY-VLINE NO-GAP,
      (23)IT_TAB-ZOTH_A   CURRENCY 'USD' NO-GAP,  SY-VLINE NO-GAP,
      (08)IT_TAB-ZOTHR_A  NO-GAP,  SY-VLINE NO-GAP.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

ENDFORM.                    " P3000_LAST_WRITE
