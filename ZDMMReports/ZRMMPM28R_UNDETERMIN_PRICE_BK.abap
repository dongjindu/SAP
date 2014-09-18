************************************************************************
* Program Name : ZRMMPM28R_UNDETERMIN_PRICE_02
* Created by   : Min-su Park
* Created on   : 2003.11.19.
* Pattern      :
* Description  : Undeterminated Price by Person
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZRMMPM28R_UNDETERMIN_PRICE_02                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRMMPM29R_UNDETERMIN_PRICE_02 MESSAGE-ID ZMMM .

TABLES : MARC, EINE, MARA.

*ALV Definition.
TYPE-POOLS: SLIS.
DATA:   WA_EVENTS      TYPE SLIS_T_EVENT                              ,
        W_REPID LIKE SY-REPID                                         ,
        WA_SORT     TYPE SLIS_T_SORTINFO_ALV                          ,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE         ,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE' ,
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER                    ,
        W_PRINT_P TYPE SLIS_PRINT_ALV                                 .

*Internal Table
DATA : BEGIN OF IT_PRICE OCCURS 0,
         WERKS(05)               , "Plant
         EKGRP LIKE MARC-EKGRP   , "Purchase Group
         EKNAM LIKE T024-EKNAM   , "Person
         MATNR LIKE MARA-MATNR   , "Material
         STPRS LIKE MBEW-STPRS   , "STD Price
*        KBETR_Z LIKE KONP-KBETR , "Zero Price
         ST   TYPE I             , "STD Price Count
         ZO   TYPE I             , "ZERO Price Count
         S1   TYPE I             , "Less than 1M
         S2   TYPE I             , "Less than 2M
         S3   TYPE I             , "More than 2M
         SUM  TYPE I             , "SUM
         ST1(10)          , "STD Price Count
         ZO1(10)           , "ZERO Price Count
         S11(10)           , "Less than 1M
         S21(10)           , "Less than 2M
         S31(10)           , "More than 2M
         SUM1(10)          , "SUM
         ERSDA LIKE MARA-ERSDA   , "Material Creation Date
       END OF IT_PRICE.

*Select Options
SELECT-OPTIONS :
        S_WERKS FOR MARC-WERKS,  "Plant
        S_EKORG FOR EINE-EKORG,  "Purchase Organization
        S_EKGRP FOR MARC-EKGRP.  "Purchase Group

INITIALIZATION.
  S_EKORG-SIGN = 'I'. S_EKORG-OPTION = 'EQ'. S_EKORG-LOW = 'PU01'.
  APPEND S_EKORG.

AT SELECTION-SCREEN.
  PERFORM CHK_INTPUT.

START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM ALV_FIELD_BUILD.
  PERFORM ALV_FUNCTION.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA : IT_TMP   LIKE IT_PRICE OCCURS 0 WITH HEADER LINE.
  DATA : IT_ITEM  LIKE IT_PRICE OCCURS 0 WITH HEADER LINE.
  DATA : IT_ITEM1 LIKE IT_PRICE .
  DATA : IT_TOTAL LIKE IT_PRICE OCCURS 0 WITH HEADER LINE.
  DATA : IT_A017 LIKE A017 OCCURS 0 WITH HEADER LINE.
  DATA : W_NETPR LIKE EINE-NETPR,
         W_EKGRP LIKE EINE-EKGRP,
         W_MONATE  LIKE KOMP-ANZ_MONATE.
  DATA : ERDAT LIKE KONH-ERDAT,
         KZUST LIKE KONH-KZUST,
         W_IN  LIKE MSEG-ERFMG,
         W_OUT LIKE MSEG-ERFMG,
         W_OK                 ,
         W_CHK                .

  SELECT * FROM ZVMM_NEW_SPRICE
           INTO CORRESPONDING FIELDS OF TABLE IT_TMP
          WHERE WERKS IN S_WERKS
*           AND EKORG IN S_EKORG
            AND MTART = 'ROH'
            AND ( EKGRP IN S_EKGRP )
                  "" OR EKGRP = SPACE )
            AND LVORM  <> 'X'
            AND LVORM1 <> 'X'
            AND LVORM2 <> 'X'.
*            AND MATNR = '1234570000'.

  LOOP AT IT_TMP.
    CLEAR : W_NETPR, W_CHK, W_OK, W_EKGRP.
    SELECT SINGLE NETPR EKGRP
      FROM ZVMM_INFORECORD
      INTO (W_NETPR, W_EKGRP)
     WHERE MATNR = IT_TMP-MATNR.
*     WERKS = IT_TMP-WERKS AND

    IF SY-SUBRC <> 0 AND IT_TMP-EKGRP+0(1) = 'A'. "[1]
      W_CHK = 'X'.
      IF IT_TMP-STPRS = 0.
        IT_ITEM1-ST = IT_ITEM1-ST + 1.
        IT_ITEM1-ZO = IT_ITEM1-ZO + 1.
      ENDIF.
      CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
          EXPORTING
            I_DATUM_BIS             = SY-DATUM
            I_DATUM_VON             = IT_TMP-ERSDA
*   I_KZ_INCL_BIS           = ' '
*   I_KZ_VOLLE_MONATE       = 'X'
          IMPORTING
             E_MONATE               = W_MONATE .
      IF W_MONATE >= 0 AND W_MONATE <= 1.
        IT_ITEM1-S1 = IT_ITEM1-S1 + 1.
      ELSEIF W_MONATE > 1 AND W_MONATE <= 2.
        IT_ITEM1-S2 = IT_ITEM1-S2 + 1.
      ELSEIF W_MONATE > 2.
        IT_ITEM1-S3 = IT_ITEM1-S3 + 1.
      ENDIF.
    ELSE.                                         "[1]
      IF W_EKGRP+0(1) = 'A'.                      "[2]

*Person
        SELECT SINGLE EKNAM
                 INTO IT_TMP-EKNAM
                 FROM T024
                WHERE EKGRP = W_EKGRP.
*Increase NO STANDARD PRICE(STD Price chk)
        IF IT_TMP-STPRS = 0. IT_TMP-ST = IT_TMP-ST + 1. ENDIF.
*Check Zero price
        SELECT SINGLE NETPR
                 FROM ZVMM_INFORECORD
                 INTO W_NETPR
                WHERE WERKS = IT_TMP-WERKS
                  AND MATNR = IT_TMP-MATNR.
        IF W_NETPR = 0     . IT_TMP-ZO = IT_TMP-ZO + 1. ENDIF.
*Check Month.
        SELECT * FROM A017
                 INTO CORRESPONDING FIELDS OF TABLE IT_A017
                WHERE KSCHL = 'PB00'
*             AND LIFNR = P_LIFNR
                  AND MATNR = IT_TMP-MATNR
                  AND EKORG IN S_EKORG
                  AND WERKS = IT_TMP-WERKS
                  AND DATAB <= SY-DATUM.
        IF SY-SUBRC <> 0.
          SELECT * FROM A018
                   INTO CORRESPONDING FIELDS OF TABLE IT_A017
                  WHERE KSCHL = 'PB00'
*             AND LIFNR = P_LIFNR
                    AND MATNR = IT_TMP-MATNR
                    AND EKORG IN S_EKORG
*                  AND WERKS = IT_TMP-WERKS
                    AND DATAB <= SY-DATUM.
        ENDIF.
        SORT IT_A017 BY DATAB DESCENDING.
        LOOP AT IT_A017.
          SELECT SINGLE ERDAT KZUST
                   INTO (ERDAT, KZUST)
                   FROM KONH
                  WHERE KNUMH = IT_A017-KNUMH.
          IF SY-SUBRC = 0 AND KZUST+0(1) = 'X'.
            W_OK = 'X'.
            EXIT.
          ELSE.
            CLEAR IT_A017.
          ENDIF.
        ENDLOOP.
        IF W_OK = 'X'.                          "[3]
          CALL FUNCTION 'MONTHS_BETWEEN_TWO_DATES_NEW'
            EXPORTING
              I_DATUM_BIS             = SY-DATUM
              I_DATUM_VON             = IT_TMP-ERSDA
*   I_KZ_INCL_BIS           = ' '
*   I_KZ_VOLLE_MONATE       = 'X'
            IMPORTING
               E_MONATE               = W_MONATE .
          IF W_MONATE >= 0 AND W_MONATE <= 1.
            IT_TMP-S1 = IT_TMP-S1 + 1.
          ELSEIF W_MONATE > 1 AND W_MONATE <= 2.
            IT_TMP-S2 = IT_TMP-S2 + 1.
          ELSEIF W_MONATE > 2.
            IT_TMP-S3 = IT_TMP-S3 + 1.
          ENDIF.
        ENDIF.                                  "[3]
      ENDIF.                                    "[2]
    ENDIF.                                      "[1]
    IT_TMP-EKGRP = W_EKGRP.
    IF IT_TMP-EKGRP = SPACE.
     DELETE IT_TMP.
    ELSE.
     MODIFY IT_TMP.
    ENDIF.
  ENDLOOP.
  SORT IT_TMP BY WERKS EKGRP.
  LOOP AT IT_TMP.
     CLEAR IT_ITEM.
     MOVE-CORRESPONDING IT_TMP TO IT_ITEM.
* SUM of Purchase Group
    AT END OF EKGRP.
      SUM.
      IT_ITEM-ST1  = IT_TMP-ST.
      IT_ITEM-ZO1  = IT_TMP-ZO.
      IT_ITEM-S11  = IT_TMP-S1.
      IT_ITEM-S21  = IT_TMP-S2.
      IT_ITEM-S31  = IT_TMP-S3.
      IT_ITEM-SUM1 = IT_TMP-ZO + IT_TMP-S1 +
                     IT_TMP-S2 + IT_TMP-S3.
      APPEND IT_ITEM.
    ENDAT.
* SUM Total
    AT LAST.
      SUM.
      IT_TOTAL-WERKS = 'Total'.
      IT_TOTAL-EKGRP = SPACE  .
      IT_TOTAL-EKNAM = SPACE  .
      IT_TOTAL-ST1  = IT_TMP-ST + IT_ITEM1-ST.
      IT_TOTAL-ZO1  = IT_TMP-ZO + IT_ITEM1-ZO.
      IT_TOTAL-S11  = IT_TMP-S1 + IT_ITEM1-S1.
      IT_TOTAL-S21  = IT_TMP-S2 + IT_ITEM1-S2.
      IT_TOTAL-S31  = IT_TMP-S3 + IT_ITEM1-S3.
      IT_TOTAL-SUM1 = IT_TMP-ZO + IT_ITEM1-ZO +
                      IT_TMP-S1 + IT_ITEM1-S1 +
                      IT_TMP-S2 + IT_ITEM1-S2 +
                      IT_TMP-S3 + IT_ITEM1-S3.
      APPEND IT_TOTAL.
    ENDAT.
  ENDLOOP.
  SORT IT_ITEM BY WERKS EKGRP.
  IT_PRICE[] = IT_TOTAL[].
  CLEAR IT_PRICE.
  IT_ITEM1-WERKS = SPACE  .
  IT_ITEM1-EKGRP = SPACE  .
  IT_ITEM1-EKNAM = SPACE  .
  IT_ITEM1-ST1  = IT_ITEM1-ST.
  IT_ITEM1-ZO1  = IT_ITEM1-ZO.
  IT_ITEM1-S11  = IT_ITEM1-S1.
  IT_ITEM1-S21  = IT_ITEM1-S2.
  IT_ITEM1-S31  = IT_ITEM1-S3.
  IT_ITEM1-SUM1 = IT_ITEM1-ZO + IT_ITEM1-S1 +
                  IT_ITEM1-S2 + IT_ITEM1-S3.
  MOVE-CORRESPONDING IT_ITEM1 TO IT_PRICE.
  APPEND IT_PRICE.
  LOOP AT IT_ITEM.
    MOVE-CORRESPONDING IT_ITEM TO IT_PRICE.
    APPEND IT_PRICE.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CHK_INTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_INTPUT.
  READ TABLE S_EKGRP INDEX 1.
  CHECK SY-SUBRC = 0.
  IF S_EKGRP-LOW+0(1) <> 'A'.
    MESSAGE E024.
  ENDIF.
  IF S_EKGRP-HIGH <> SPACE AND S_EKGRP-HIGH+0(1) <> 'A'.
    MESSAGE E024.
  ENDIF.
ENDFORM.                    " CHK_INTPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELD_BUILD.
  W_REPID = SY-REPID.
  CLEAR : IT_FIELDCAT[], WA_EVENTS[], WA_LIST_TOP_OF_PAGE[].
  PERFORM FIELDCAT_INIT  USING IT_FIELDCAT[].
  PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
  PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I.
*Plant
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'WERKS'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Plant'.
  LS_FIELDCAT-SELTEXT_M     = 'Plant'.
  LS_FIELDCAT-SELTEXT_S     = 'Plant'.
  LS_FIELDCAT-OUTPUTLEN     = '4'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Purchasing Group
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'EKGRP'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Purchasing Group'.
  LS_FIELDCAT-SELTEXT_M     = 'Purchasing Group'.
  LS_FIELDCAT-SELTEXT_S     = 'Purchasing Group'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Person
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'EKNAM'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Person'.
  LS_FIELDCAT-SELTEXT_M     = 'Person'.
  LS_FIELDCAT-SELTEXT_S     = 'Person'.
  LS_FIELDCAT-OUTPUTLEN     = '18'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*STD Price
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ST1'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'STD Price'.
  LS_FIELDCAT-SELTEXT_M     = 'STD Price'.
  LS_FIELDCAT-SELTEXT_S     = 'STD Price'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Zero Price
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ZO1'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Zero Price'.
  LS_FIELDCAT-SELTEXT_M     = 'Zero Price'.
  LS_FIELDCAT-SELTEXT_S     = 'Zero Price'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Less than 1M
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'S11'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Less than 1M'.
  LS_FIELDCAT-SELTEXT_M     = 'Less than 1M'.
  LS_FIELDCAT-SELTEXT_S     = 'Less than 1M'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Less than 2M
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'S21'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Less than 2M'.
  LS_FIELDCAT-SELTEXT_M     = 'Less than 2M'.
  LS_FIELDCAT-SELTEXT_S     = 'Less than 2M'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*More than 2M
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'S31'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'More than 2M'.
  LS_FIELDCAT-SELTEXT_M     = 'More than 2M'.
  LS_FIELDCAT-SELTEXT_S     = 'More than 2M'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*SUM
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'SUM1'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'SUM'.
  LS_FIELDCAT-SELTEXT_M     = 'SUM'.
  LS_FIELDCAT-SELTEXT_S     = 'SUM'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = LT_EVENTS.
  READ TABLE LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: INFO_TXT(50).
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-100.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Plant Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(4)  = S_WERKS-LOW .
  INFO_TXT+10(2) = 'To'      .
  INFO_TXT+13(4) = S_WERKS-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Plant:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Purchase Organization Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(4)  = S_EKORG-LOW .
  INFO_TXT+10(2) = 'To'      .
  INFO_TXT+13(4) = S_EKORG-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Pur. Organization:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Purchase group Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'      .
  INFO_TXT+5(4)  = S_EKGRP-LOW .
  INFO_TXT+10(2) = 'To'        .
  INFO_TXT+13(4) = S_EKGRP-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Purchase Group:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.


ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FUNCTION.
  W_REPID = SY-REPID  .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
*     I_STRUCTURE_NAME             =
*     IT_SORT                      = WA_SORT[]
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = IT_FIELDCAT[]
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_PRICE
            .
ENDFORM.                    " ALV_FUNCTION
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
