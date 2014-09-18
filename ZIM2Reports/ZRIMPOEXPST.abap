*&---------------------------------------------------------------------*
*& Report  ZRIMLDCST                                                   *
*&---------------------------------------------------------------------*
*&  Program : Import Expense List by P/O
*&     Name : SH, Na INFOLINK Ltd.                                     *
*&     Date : 2004.02.26                                               *
*&---------------------------------------------------------------------*
*&    Desc. :                                                          *
*&---------------------------------------------------------------------*
REPORT  ZRIMLDCST  MESSAGE-ID  ZIM
                   LINE-SIZE   148
                   NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE: <ICON>.
TABLES : ZTBKPF, ZTBSEG, ZTBDIV, ZTBHIS,
         T163B, T163C, T685T, LFA1, MARA,
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
         EBELN    LIKE EKKO-EBELN,
         EBELP    LIKE EKPO-EBELP,
         HSWAE    LIKE EKBE-HSWAE,        " Currency..
         PB00     LIKE BSEG-DMBTR,        " [Tot] Gross-Price Amount..
         FRA1     LIKE BSEG-DMBTR,        " [Tot] Freight Amount..
         ZOA1     LIKE BSEG-DMBTR,        " [Tot] Duty Amount..
         ZFCSTGRP LIKE ZTBDIV-ZFCSTGRP,   " Cost Group..
         ZFCD     LIKE ZTBDIV-ZFCD,       " Title Code..
         SGTXT    LIKE ZTBDIV-SGTXT,      " Item Text..
         FRA1R    TYPE ZTPMTHD-ZFUSITR,   " [Tot] Freight Rate..
         ZOTI     LIKE BSEG-DMBTR,        " [Tot] B/L Expense Amount..
         ZOTIR    LIKE ZTPMTHD-ZFUSITR,   " [Tot] B/L Expense Rate..
         ZOTH     LIKE BSEG-DMBTR,        " [Tot] L/C Expense Amount..
         ZOTHR    LIKE ZTPMTHD-ZFUSITR,   " [Tot] L/C Expense Rate..
         PB00_O   LIKE BSEG-DMBTR,        " [Ocn] Gross-Price Amount..
         FRA1_O   LIKE BSEG-DMBTR,        " [Ocn] Ocean Freight Amount..
         FRA1R_O  LIKE ZTPMTHD-ZFUSITR,   " [Ocn] Ocean Freight
         ZOTI_O   LIKE BSEG-DMBTR,        " [Ocn] B/L Expense Amount..
         ZOTIR_O  LIKE ZTPMTHD-ZFUSITR,   " [Ocn] B/L Expense
         ZOTH_O   LIKE BSEG-DMBTR,        " [Ocn] L/C Expense Amount..
         ZOTHR_O  LIKE ZTPMTHD-ZFUSITR,   " [Ocn] L/C Expense
         PB00_A   LIKE BSEG-DMBTR,        " [Air] Gross-Price Amount..
         FRA1_A   LIKE BSEG-DMBTR,        " [Air] Ocean Freight Amount..
         FRA1R_A  LIKE ZTPMTHD-ZFUSITR,   " [Air] Ocean Freight Rate..
         ZOTI_A   LIKE BSEG-DMBTR,        " [Air] B/L Expense Amount..
         ZOTIR_A  LIKE ZTPMTHD-ZFUSITR,   " [Air] B/L Expense Rate..
         ZOTH_A   LIKE BSEG-DMBTR,        " [Air] L/C Expense Amount..
         ZOTHR_A  LIKE ZTPMTHD-ZFUSITR.   " [Air] L/C Expense Rate..
DATA : END OF    IT_TAB.

DATA : BEGIN OF IT_TAB1 OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA : END OF IT_TAB1.
DATA : BEGIN OF IT_KO OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA :   SHKZG    LIKE EKBE-SHKZG,
         SHKZG1   LIKE EKBZ-SHKZG,
         BEWTP    LIKE EKBE-BEWTP,
         GJAHR    LIKE EKBE-GJAHR,
         BELNR    LIKE EKBE-BELNR,
         ZFTRANS  LIKE ZTREQHD-ZFTRANS,
       END OF IT_KO.

DATA : BEGIN OF IT_DIV OCCURS 0,
         EBELN     LIKE   ZTBDIV-EBELN,      " PO Header.
         EBELP     LIKE   ZTBDIV-EBELP,      " PO Item.
         ZFRVSX    LIKE   ZTBKPF-ZFRVSX,     " Reverse Posting Yes/No..
         BUKRS     LIKE   ZTBDIV-BUKRS,      " Company Code..
         ZFACDO    LIKE   ZTBKPF-ZFACDO,     " Charge document No..
         ZFFIYR    LIKE   ZTBKPF-ZFFIYR,     " Fiscal Year..
         SGTXT     LIKE   ZTBDIV-SGTXT,      " Item Text..
         ZFDCSTX   LIKE   ZTBDIV-ZFDCSTX,    " Delivery Cost Yes/No..
         ZFCSTGRP  LIKE   ZTBDIV-ZFCSTGRP,   " Cost Group..
         COND_TYPE LIKE   ZTBDIV-COND_TYPE,  " Condition type..
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
       W_Count        type  i,
       w_count1       type  i,
       W_TABIX        LIKE  SY-TABIX,
       W_EBELN        LIKE  EKKO-EBELN,
       W_LAND1        LIKE  LFA1-LAND1,
       W_PB00         LIKE  EKBZ-DMBTR,
       W_PB00_O       LIKE  EKBZ-DMBTR,
       W_PB00_A       LIKE  EKBZ-DMBTR,
       W_FRA1         LIKE  EKBZ-DMBTR,
       W_ZOA1         LIKE  EKBZ-DMBTR,
       W_FRA1_O       LIKE  EKBZ-DMBTR,
       W_FRA1_A       LIKE  EKBZ-DMBTR,
       W_ZOTH         LIKE  EKBZ-DMBTR,
       W_ZOTH_O       LIKE  EKBZ-DMBTR,
       W_ZOTH_A       LIKE  EKBZ-DMBTR,
       W_ZOTI         LIKE  EKBZ-DMBTR,
       W_ZOTI_O       LIKE  EKBZ-DMBTR,
       W_ZOTI_A       LIKE  EKBZ-DMBTR,
       W_ZFCD         LIKE  ZTBDIV-ZFCD,
       W_LIFNR        LIKE  LFA1-LIFNR,
       W_PROFL        LIKE  MARA-PROFL.
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
                S_PROFL  FOR MARA-PROFL,
                S_EBELN  FOR EKKO-EBELN,
                S_LIFNR  FOR EKKO-LIFNR,
                S_BEDAT  FOR EKKO-BEDAT,
                S_BUDAT  FOR EKBZ-BUDAT,
                S_ZTERM  FOR EKKO-ZTERM NO-DISPLAY,
                S_BSART  FOR EKKO-BSART NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.

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
  ENDCASE.
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
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_KO
    FROM EKKO AS A INNER JOIN EKPO AS B
         ON A~EBELN EQ B~EBELN
            WHERE A~LIFNR IN S_LIFNR
              AND A~BEDAT IN S_BEDAT
              AND A~BSART IN S_BSART
              AND A~ZTERM IN S_ZTERM
              AND A~EBELN IN S_EBELN
              AND A~BSTYP EQ 'F'
              AND A~EBELN NE SPACE.

  IF SY-SUBRC EQ 0.
    LOOP AT IT_KO.
      MOVE IT_KO-EBELN TO S_EBELN-LOW.
      MOVE 'I'         TO S_EBELN-SIGN.
      MOVE 'EQ'        TO S_EBELN-OPTION.
      APPEND S_EBELN.
    ENDLOOP.
  ENDIF.

*>> Select Good Price Data..
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_EKBE
    FROM EKBE
   WHERE EBELN IN S_EBELN
     AND BEWTP =  'Q'
     AND VGABE =  '2'
     AND BUDAT IN S_BUDAT.

*>> Select Freight and Other Charges.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_DIV
    FROM ZTBKPF AS H INNER JOIN ZTBDIV AS I
      ON H~BUKRS EQ I~BUKRS
     AND H~BELNR EQ I~BELNR
     AND H~GJAHR EQ I~GJAHR
     WHERE H~ZFPOSYN EQ 'Y'
       AND I~EBELN   IN S_EBELN
       AND I~NEWBS   EQ '40'.

  SORT IT_KO BY EBELN EBELP.

  CLEAR W_LINES.
  LOOP AT IT_KO.
    ADD 1 TO W_LINES.
    CLEAR: IT_EKBE, IT_DIV.
    READ TABLE IT_EKBE WITH KEY EBELN = IT_KO-EBELN
                                EBELP = IT_KO-EBELP.
    MOVE IT_KO-EBELN TO IT_TAB1-EBELN.
    MOVE IT_KO-EBELP TO IT_TAB1-EBELP.
    IF SY-SUBRC EQ 0.
      MOVE IT_EKBE-DMBTR TO IT_TAB1-PB00.
      MOVE IT_EKBE-HSWAE TO IT_TAB1-HSWAE.
      READ TABLE IT_DIV WITH KEY EBELN     = IT_KO-EBELN
                                 EBELP     = IT_KO-EBELP
                                 COND_TYPE = 'FRA1'.
      IF SY-SUBRC EQ 0.
        IF IT_DIV-DMBTR NE IT_TAB1-FRA1.
          MOVE IT_DIV-DMBTR TO IT_TAB1-FRA1.
        ENDIF.
      ENDIF.
      READ TABLE IT_DIV WITH KEY EBELN     = IT_KO-EBELN
                                 EBELP     = IT_KO-EBELP
                                 COND_TYPE = 'ZOA1'.
      IF SY-SUBRC EQ 0.
        IF IT_DIV-DMBTR NE IT_TAB1-ZOA1.
          MOVE IT_DIV-DMBTR TO IT_TAB1-ZOA1.
        ENDIF.
      ENDIF.
      LOOP AT IT_DIV WHERE EBELN     =  IT_KO-EBELN
                       AND EBELP     =  IT_KO-EBELP
                       AND ZFDCSTX   NE 'X'
                       AND COND_TYPE NE SPACE.
        MOVE IT_DIV-DMBTR    TO IT_TAB1-ZOTI.
        MOVE IT_DIV-ZFCSTGRP TO IT_TAB1-ZFCSTGRP.
        MOVE IT_DIV-ZFCD     TO IT_TAB1-ZFCD.
        MOVE IT_DIV-SGTXT    TO IT_TAB1-SGTXT.
        ADD 1 TO W_LINE.
        APPEND IT_TAB1.
        CLEAR: IT_TAB1-PB00, IT_TAB1-FRA1, IT_TAB1-ZOA1.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT IT_TAB1 BY EBELN ZFCD.
  CLEAR W_EBELN.
  LOOP AT IT_TAB1.
    MOVE IT_TAB1-EBELN TO IT_TAB-EBELN.
    IF SY-TABIX EQ 1.
      PERFORM P2000_MAKE_DCST.
    ELSE.
      IF IT_TAB1-EBELN EQ W_EBELN.
        PERFORM P2000_MAKE_DCST.
      ELSE.
        APPEND IT_TAB.
        CLEAR IT_TAB.
        PERFORM P2000_MAKE_DCST.
      ENDIF.
    ENDIF.
    MOVE IT_TAB1-EBELN TO W_EBELN.
    AT LAST.
      APPEND IT_TAB.
      CLEAR IT_TAB.
    ENDAT.
  ENDLOOP.

  CLEAR: w_tabix, w_count, w_count1, w_ebeln, w_line, W_LINES.
  w_count1 = 1.
  sort it_tab1 by ebeln zfcstgrp zfcd.
  loop at it_tab1 where not zoti is initial.
    add 1 to: w_line, w_count.
    w_lines = w_line + 1.
    add it_tab1-zoti      to w_zoti.
    move it_tab1-zfcd     to it_tab-zfcd.
    move it_tab1-sgtxt    to it_tab-sgtxt.
    move it_tab1-ebeln    to it_tab-ebeln.
    move it_tab1-zfcstgrp to it_tab-zfcstgrp.
    read table it_tab1 index w_lines.
    if sy-subrc eq 0.
      if it_tab1-zfcd ne it_tab-zfcd.
        move w_zoti           to it_tab-zoti.
        if w_count gt 1.
          if it_tab-ebeln eq w_ebeln.
            if it_tab-zfcd ne w_zfcd.
              append it_tab.
            else.
              modify it_tab index w_count1
                transporting ebeln zoti zfcd sgtxt zfcstgrp.
              add 1 to w_count1.
            endif.
          else.
            modify it_tab index w_count1
              transporting ebeln zoti zfcd sgtxt zfcstgrp.
            add 1 to w_count1.
          endif.
        else.
          modify it_tab index 1
            transporting ebeln zoti zfcd sgtxt zfcstgrp.
          add 1 to w_count1.
        endif.
        clear w_zoti.
      else.
        move w_zoti           to it_tab-zoti.
*        if w_count gt 1.
*          modify it_tab index w_count1
*            transporting ebeln zoti zfcd sgtxt zfcstgrp.
*          add 1 to w_count1.
*        else.
*        endif.
      endif.
    else. " last data of internal table..
      append it_tab.
    endif.
    move it_tab-ebeln to w_ebeln.
    move it_tab-zfcd  to w_zfcd.
  endloop.

*  CLEAR: w_tabix, w_line, W_LINES.
*  SORT IT_TAB1 BY EBELN ZFCSTGRP ZFCD.
*  loop at it_tab1 WHERE ZFDCSTX   NE 'X'.
*    add 1 to w_line.
*    w_lines = w_line + 1.
*    move-corresponding it_tab1 to it_tab.
*    move it_tab1-zfcd to w_zfcd.
*    read table it_tab1 index w_lines.
*    if sy-subrc eq 0.
*      if it_tab1-zfcd eq w_zfcd.
*        it_tab-zoti =  it_tab1-zoti + it_tab-zoti.
*        if w_tabix is initial.
*          w_tabix = sy-tabix - 1.
*        endif.
*        modify it_tab index w_tabix.
*      else.
*        append it_tab.
*        w_tabix = w_tabix + 1.
*      endif.
*    else.
*      modify it_tab index w_tabix.
*    endif.
*  endloop.

  SORT IT_TAB BY EBELN.
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
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'EBELN'.
  LS_FIELDCAT-SELTEXT_M      = 'P/O No.'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'LAND1'.
  LS_FIELDCAT-KEY            = 'X'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

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
  SORT IT_TAB BY EBELN ZFCSTGRP ZFCD.
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
  WRITE:/55  '[ Import Expense List by P/O ]' CENTERED
             COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/122 'Date : ', SY-DATUM.  ", 93 'Page : ', W_PAGE.

  MOVE 'P/O No.'   TO W_SORT.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE NO-GAP,
      (10)W_SORT             NO-GAP CENTERED, SY-VLINE NO-GAP,
      (25)'Good Price'       NO-GAP CENTERED, SY-VLINE NO-GAP,
      (24)'Freight'          NO-GAP CENTERED, SY-VLINE NO-GAP,
      (24)'Duty'             NO-GAP CENTERED, SY-VLINE NO-GAP,
      (59)'Other Charge' NO-GAP CENTERED, SY-VLINE NO-GAP.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ON CHANGE OF IT_TAB-EBELN.
    IF SY-TABIX NE 1.
      WRITE:/ SY-ULINE.
    ENDIF.
    MOVE IT_TAB-EBELN TO W_EBELN.
    MOVE IT_TAB-PB00  TO W_PB00.
    MOVE IT_TAB-FRA1  TO W_FRA1.
    MOVE IT_TAB-ZOA1  TO W_ZOA1.
  ENDON.
  WRITE: SY-VLINE NO-GAP,
      (10)W_EBELN  NO-GAP,  SY-VLINE NO-GAP.
  IF NOT W_PB00 IS INITIAL.
    WRITE: (25)W_PB00 CURRENCY IT_TAB-HSWAE  NO-GAP,  SY-VLINE NO-GAP,
           (24)W_FRA1 CURRENCY IT_TAB-HSWAE  NO-GAP,  SY-VLINE NO-GAP,
           (24)W_ZOA1 CURRENCY IT_TAB-HSWAE  NO-GAP,  SY-VLINE NO-GAP.
  ELSE.
    WRITE: (25)'' NO-GAP,  SY-VLINE NO-GAP,
           (24)'' NO-GAP,  SY-VLINE NO-GAP,
           (24)'' NO-GAP,  SY-VLINE NO-GAP.
  endif.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: (03)IT_TAB-ZFCD,
      (30)IT_TAB-SGTXT  NO-GAP,  SY-VLINE NO-GAP,
      (24)IT_TAB-ZOTI    CURRENCY IT_TAB-HSWAE NO-GAP,  SY-VLINE NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
*  WRITE:/ SY-ULINE.
  CLEAR: W_EBELN, W_PB00, W_FRA1, W_ZOA1.
  FORMAT RESET.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.
  write: / sy-uline.
ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_DCST
*&---------------------------------------------------------------------*
FORM P2000_MAKE_DCST.
  IF NOT IT_TAB1-PB00 IS INITIAL.
    ADD IT_TAB1-PB00 TO IT_TAB-PB00.
  ENDIF.
  IF NOT IT_TAB1-FRA1 IS INITIAL.
    ADD IT_TAB1-FRA1 TO IT_TAB-FRA1.
  ENDIF.
  IF NOT IT_TAB1-ZOA1 IS INITIAL.
    ADD IT_TAB1-ZOA1 TO IT_TAB-ZOA1.
  ENDIF.
ENDFORM.                    " P2000_MAKE_DCST
