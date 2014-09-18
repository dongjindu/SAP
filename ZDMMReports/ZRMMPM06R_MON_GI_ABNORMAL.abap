************************************************************************
* Program Name : ZRMMPM06R_MON_GI_ABNORMAL
* Created by   : Min-su Park
* Created on   : 2003.08.18.
* Pattern      :
* Description  : Monthly GI(abnormal) List
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.18.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

REPORT  ZRMMPM06R_MON_GI_ABNORMAL MESSAGE-ID ZMMM    .
TYPE-POOLS: SLIS.
*ALV Definition.
DATA:   WA_EVENTS   TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID                           ,
        WA_SORT     TYPE SLIS_T_SORTINFO_ALV,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*Data Definition.
TABLES : MSEG, MKPF, MARC, RM07M, MARA.
DATA   : IT_ZSMM_RMMPM06 TYPE ZSMM_RMMPM06 OCCURS 0 WITH HEADER LINE.
DATA   : SPMON TYPE SPMON.
*SUM Data
DATA : W_DMBTR LIKE MSEG-DMBTR,
       W_MENGE LIKE MSEG-MENGE.
SELECT-OPTIONS :
      S_WERKS  FOR MSEG-WERKS OBLIGATORY    , "Plant
      S_LGORT  FOR MSEG-LGORT OBLIGATORY    , "Storage Location
      S_MTART  FOR MARA-MTART               , "Material Type
      S_RAUBE  FOR MARA-RAUBE               , "Shop
      S_DISPO  FOR MARC-DISPO OBLIGATORY    , "Manager
      S_SPMON1 FOR SPMON         OBLIGATORY , "Period(Month)
      S_BWART  FOR MSEG-BWART    NO-DISPLAY ,
      S_BUDAT  FOR MKPF-BUDAT    NO-DISPLAY .
PARAMETERS P_BWART LIKE RM07M-BWARTWA .

INITIALIZATION.
  S_MTART-LOW = 'ROH'.
  APPEND S_MTART.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_BWART.
  PERFORM POSSBLE_BWART.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BWART-HIGH.
*  PERFORM POSSBLE_BWART.
*
AT SELECTION-SCREEN.
  PERFORM CHECK_BWART.
  PERFORM SELECTION_ADJUSTMENT.
  PERFORM MAKE_BASIC_DATA.
  PERFORM ALV_FIELD_BUILD.

START-OF-SELECTION.
  SORT IT_ZSMM_RMMPM06 BY DMBTR DESCENDING.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
*     I_STRUCTURE_NAME             = 'ZSMM_RMMPM06'
*     IT_SORT                      = WA_SORT[]
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = IT_FIELDCAT[]
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_ZSMM_RMMPM06
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT
               USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.
*Plant
  clear ls_fieldcat.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'WERKS'.
  LS_FIELDCAT-REF_FIELDNAME = 'WERKS'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Plant'.
  LS_FIELDCAT-SELTEXT_M     = 'Plant'.
  LS_FIELDCAT-SELTEXT_S     = 'Plant'.
  LS_FIELDCAT-OUTPUTLEN     = '4'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Storage Location
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LGORT'.
  LS_FIELDCAT-REF_FIELDNAME = 'LGORT'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Storage Location'.
  LS_FIELDCAT-SELTEXT_M     = 'Storage Location'.
  LS_FIELDCAT-SELTEXT_S     = 'Storage Location'.
  LS_FIELDCAT-OUTPUTLEN     = '4'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Shop
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'RAUBE'.
  LS_FIELDCAT-REF_FIELDNAME = 'RAUBE'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Shop'.
  LS_FIELDCAT-SELTEXT_M     = 'Shop'.
  LS_FIELDCAT-SELTEXT_S     = 'Shop'.
  LS_FIELDCAT-OUTPUTLEN     = '2'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*POC
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'DISPO'.
  LS_FIELDCAT-REF_FIELDNAME = 'DISPO'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'POC'.
  LS_FIELDCAT-SELTEXT_M     = 'POC'.
  LS_FIELDCAT-SELTEXT_S     = 'POC'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Mvt
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'BWART'.
  LS_FIELDCAT-REF_FIELDNAME = 'BWART'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Mvt'.
  LS_FIELDCAT-SELTEXT_M     = 'Mvt'.
  LS_FIELDCAT-SELTEXT_S     = 'Mvt'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*RC
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'GRUND'.
  LS_FIELDCAT-REF_FIELDNAME = 'GRUND'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'RC'.
  LS_FIELDCAT-SELTEXT_M     = 'RC'.
  LS_FIELDCAT-SELTEXT_S     = 'RC'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Mat. No.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MATNR'.
  LS_FIELDCAT-REF_FIELDNAME = 'MATNR'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Mat. No.'.
  LS_FIELDCAT-SELTEXT_M     = 'Mat. No.'.
  LS_FIELDCAT-SELTEXT_S     = 'Mat. No.'.
* LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Mat. Discription
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MAKTX'.
  LS_FIELDCAT-REF_FIELDNAME = 'MAKTX'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Mat. Description'.
  LS_FIELDCAT-SELTEXT_M     = 'Mat. Description'.
  LS_FIELDCAT-SELTEXT_S     = 'Mat. Description'.
  LS_FIELDCAT-OUTPUTLEN     = '25'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*GI QTY
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MENGE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-DO_SUM        = ''.
  LS_FIELDCAT-QFIELDNAME    = 'MEINS'.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'GI Qty'.
  LS_FIELDCAT-SELTEXT_M     = 'GI Qty'.
  LS_FIELDCAT-SELTEXT_S     = 'GI Qty'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Unit of Measure
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MEINS'.
  LS_FIELDCAT-REF_FIELDNAME = 'MEINS'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Unit'.
  LS_FIELDCAT-SELTEXT_M     = 'Unit'.
  LS_FIELDCAT-SELTEXT_S     = 'Unit'.
  LS_FIELDCAT-OUTPUTLEN     = '5'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Unit Price
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'NETPR'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-DO_SUM        = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
  LS_FIELDCAT-SELTEXT_L     = 'Unit Price'.
  LS_FIELDCAT-SELTEXT_M     = 'Unit Price'.
  LS_FIELDCAT-SELTEXT_S     = 'Unit Price'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*GI Amount
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'DMBTR'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-DO_SUM        = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = 'WAERS'.
  LS_FIELDCAT-SELTEXT_L     = 'GI Amount'.
  LS_FIELDCAT-SELTEXT_M     = 'GI Amount'.
  LS_FIELDCAT-SELTEXT_S     = 'GI Amount'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Curr
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'WAERS'.
  LS_FIELDCAT-REF_FIELDNAME = 'WAERS'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Curr'.
  LS_FIELDCAT-SELTEXT_M     = 'Curr'.
  LS_FIELDCAT-SELTEXT_S     = 'Curr'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.


*NO DISPLAY FIELD
  clear ls_fieldcat.
  LS_FIELDCAT-FIELDNAME     = 'SPMON'.
  LS_FIELDCAT-NO_OUT        = 'X'    .
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
  clear ls_fieldcat.
  LS_FIELDCAT-FIELDNAME     = 'BUDAT'.
  LS_FIELDCAT-NO_OUT        = 'X'    .
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.



ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING E06_LT_SORT TYPE SLIS_T_SORTINFO_ALV.
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.

  LS_SORT-FIELDNAME = 'WERKS'.
  LS_SORT-SPOS      = 1.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'X'.
  APPEND LS_SORT TO E06_LT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = 'LGORT'.
  LS_SORT-SPOS      = 2.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'X'.
  APPEND LS_SORT TO E06_LT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = 'BWART'.
  LS_SORT-SPOS      = 3.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'X'.
  APPEND LS_SORT TO E06_LT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = 'GRUND'.
  LS_SORT-SPOS      = 4.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'X'.
  APPEND LS_SORT TO E06_LT_SORT.

  CLEAR LS_SORT.
  LS_SORT-FIELDNAME = 'MATNR'.
  LS_SORT-SPOS      = 5.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'X'.
  APPEND LS_SORT TO E06_LT_SORT.

*  CLEAR LS_SORT.
*  LS_SORT-FIELDNAME = 'SPMON'.
*  LS_SORT-SPOS      = 5.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = 'X'.
*  LS_SORT-GROUP     = 'A'.
*  APPEND LS_SORT TO E06_LT_SORT.
ENDFORM.                    " SORT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM E03_EVENTTAB_BUILD                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E03_LT_EVENTS                                                 *
*---------------------------------------------------------------------*
FORM E03_EVENTTAB_BUILD USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM E04_COMMENT_BUILD                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E04_LT_TOP_OF_PAGE                                            *
*---------------------------------------------------------------------*
FORM E04_COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
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

*Storage Location Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From' .
  INFO_TXT+5(4)  = S_LGORT-LOW.
  INFO_TXT+10(2) = 'To' .
  INFO_TXT+13(4) = S_LGORT-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP    = 'S'.
  LS_LINE-KEY    = 'Storage Location:'.
  LS_LINE-INFO   = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Manager
  CLEAR INFO_TXT.
  INFO_TXT+0(4)    = 'From'.
  INFO_TXT+5(3)    = S_DISPO-LOW.
  INFO_TXT+10(2)   = 'To'.
  INFO_TXT+13(3)   = S_DISPO-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP      = 'S'.
  LS_LINE-KEY      = 'Manager:'.
  LS_LINE-INFO     = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Mvmt Type
*  CLEAR INFO_TXT.
*  INFO_TXT+0(4)    = 'From'.
*  INFO_TXT+5(3)    = S_BWART-LOW.
*  INFO_TXT+10(2)   = 'To'.
*  INFO_TXT+13(3)   = S_BWART-HIGH.
*  CLEAR LS_LINE.
*  LS_LINE-TYP      = 'S'.
*  LS_LINE-KEY      = 'Mvmt Type:'.
*  LS_LINE-INFO     = INFO_TXT.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.
*
**Reason Code
*  CLEAR INFO_TXT.
*  INFO_TXT+0(4)    = 'From'.
*  INFO_TXT+5(4)    = S_GRUND-LOW.
*  INFO_TXT+10(2)   = 'To'.
*  INFO_TXT+13(4)   = S_GRUND-HIGH.
*  CLEAR LS_LINE.
*  LS_LINE-TYP      = 'S'.
*  LS_LINE-KEY      = 'Reason Code:'.
*  LS_LINE-INFO     = INFO_TXT.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

**Material No.
*  CLEAR INFO_TXT.
*  INFO_TXT+0(4)   = 'From'.
*  INFO_TXT+5(18)  = S_MATNR-LOW.
*  INFO_TXT+24(2)  = 'To'.
*  INFO_TXT+27(18) = S_MATNR-HIGH.
*  CLEAR LS_LINE.
*  LS_LINE-TYP     = 'S'.
*  LS_LINE-KEY     = 'Material No:'.
*  LS_LINE-INFO    = INFO_TXT.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Period
  CLEAR INFO_TXT.
  INFO_TXT+0(4)    = 'From'.
  INFO_TXT+5(6)    = S_SPMON1-LOW.
  INFO_TXT+12(2)   = 'To'.
  INFO_TXT+15(4)   = S_SPMON1-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP      = 'S'.
  LS_LINE-KEY      = 'Reason Code:'.
  LS_LINE-INFO     = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECTION_ADJUSTMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_ADJUSTMENT.
  CLEAR : IT_ZSMM_RMMPM06[], S_BUDAT[].
*Adjust Monthly criterion to Daily criterion
  IF S_SPMON1-LOW <> SPACE.
    CONCATENATE : S_SPMON1-LOW '01' INTO S_BUDAT-LOW.
    S_BUDAT-SIGN   = S_SPMON1-SIGN.
    S_BUDAT-OPTION = S_SPMON1-OPTION.
    CASE S_SPMON1-OPTION.
      WHEN 'BT' OR 'NB'.
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
             EXPORTING
                  DAY_IN            = S_BUDAT-LOW
             IMPORTING
                  LAST_DAY_OF_MONTH = S_BUDAT-HIGH.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ELSE.
          APPEND S_BUDAT.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFORM.                    " SELECTION_ADJUSTMENT
*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_BASIC_DATA.
* DATA : TMP_ZSMM_RMMPM06 LIKE IT_ZSMM_RMMPM06
*                              OCCURS 0 WITH HEADER LINE.
**Get Data
*  SELECT * FROM ZVMM_GIABNORMAL
*           INTO CORRESPONDING FIELDS OF TABLE IT_ZSMM_RMMPM06
*          WHERE WERKS IN S_WERKS
*            AND LGORT IN S_LGORT
*            AND MTART IN S_MTART
*            AND RAUBE IN S_RAUBE
*            AND DISPO IN S_DISPO
*            AND BWART IN S_BWART
*            AND BUDAT IN S_BUDAT.
**Fill Monthly Field(SPMON) for Sorting and Sum Monthly, etc.
*  LOOP AT IT_ZSMM_RMMPM06.
*    IT_ZSMM_RMMPM06-SPMON = IT_ZSMM_RMMPM06-BUDAT+0(06).
*    MODIFY IT_ZSMM_RMMPM06.
*  ENDLOOP.
*
**Calculate Unit Price not from Database but from GI Amount / GI Qty
*  TMP_ZSMM_RMMPM06[] = IT_ZSMM_RMMPM06[].
*  SORT TMP_ZSMM_RMMPM06 BY WERKS LGORT BWART GRUND MATNR SPMON.
*  LOOP AT TMP_ZSMM_RMMPM06.
*    AT END OF SPMON.
*       SUM.
*       IT_ZSMM_RMMPM06-NETPR
*           = TMP_ZSMM_RMMPM06-DMBTR / TMP_ZSMM_RMMPM06-MENGE.
*       MODIFY IT_ZSMM_RMMPM06 TRANSPORTING NETPR
*                              WHERE WERKS = TMP_ZSMM_RMMPM06-WERKS
*                                AND LGORT = TMP_ZSMM_RMMPM06-LGORT
*                                AND BWART = TMP_ZSMM_RMMPM06-BWART
*                                AND GRUND = TMP_ZSMM_RMMPM06-GRUND
*                                AND MATNR = TMP_ZSMM_RMMPM06-MATNR
*                                AND SPMON = TMP_ZSMM_RMMPM06-SPMON.
*    ENDAT.
*    CLEAR IT_ZSMM_RMMPM06.
*  ENDLOOP.
  IF S_LGORT IS INITIAL AND S_RAUBE IS INITIAL AND S_DISPO IS INITIAL.
    PERFORM MAKE_SUM_BY_MAT_PLT.
  ELSE.
    PERFORM MAKE_IT_ZSMM_RMMPM06.
  ENDIF.

ENDFORM.                    " MAKE_BASIC_DATA
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
  CLEAR : IT_FIELDCAT[], WA_EVENTS[],
          WA_LIST_TOP_OF_PAGE[], WA_SORT[].
  PERFORM FIELDCAT_INIT USING IT_FIELDCAT[].
  PERFORM E03_EVENTTAB_BUILD USING WA_EVENTS[].
  PERFORM E04_COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
  PERFORM SORT_BUILD   USING WA_SORT[].
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  POSSBLE_BWART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POSSBLE_BWART.
  DATA: IT_RETURN LIKE DDSHRETVAL OCCURS 10 WITH HEADER LINE.
  DATA: IT_DFIES LIKE DFIES OCCURS 10 WITH HEADER LINE.
  DATA: BEGIN OF IT_BWART OCCURS 0,
            BWART LIKE T156T-BWART,
            SOBKZ LIKE T156T-SOBKZ,
            BTEXT LIKE T156T-BTEXT,
            INDEX(04)             ,
        END OF IT_BWART.

  SELECT * FROM T156T
           INTO CORRESPONDING FIELDS OF TABLE IT_BWART
          WHERE BWART IN ('551', '905')
            AND KZVBR = SPACE
            AND SPRAS = 'E'.

  LOOP AT IT_BWART.
    CONCATENATE IT_BWART-BWART IT_BWART-SOBKZ INTO IT_BWART-INDEX.
    MODIFY IT_BWART.
  ENDLOOP.

  CLEAR: IT_DFIES, IT_DFIES[].
  IT_DFIES-FIELDNAME = 'BWART'.
  IT_DFIES-POSITION  = 1.
  IT_DFIES-OFFSET    = 0.
  IT_DFIES-INTLEN    = 3.
  IT_DFIES-OUTPUTLEN = 3.
  IT_DFIES-SCRTEXT_S = 'MvT'.
  APPEND IT_DFIES.
  CLEAR IT_DFIES.

  CLEAR: IT_DFIES.
  IT_DFIES-FIELDNAME = 'SOBKZ'.
  IT_DFIES-POSITION  = 2.
  IT_DFIES-OFFSET    = 3.
  IT_DFIES-INTLEN    = 1.
  IT_DFIES-OUTPUTLEN = 1.
  IT_DFIES-SCRTEXT_S = 'S'.
  APPEND IT_DFIES.
  CLEAR IT_DFIES.

  CLEAR: IT_DFIES.
  IT_DFIES-FIELDNAME = 'BTEXT'.
  IT_DFIES-POSITION  = 3.
  IT_DFIES-OFFSET    = 4.
  IT_DFIES-INTLEN    = 20.
  IT_DFIES-OUTPUTLEN = 20.
  IT_DFIES-SCRTEXT_S = 'Text'.
  APPEND IT_DFIES.
  CLEAR IT_DFIES.

  CLEAR: IT_DFIES.
  IT_DFIES-FIELDNAME = 'INDEX'.
  IT_DFIES-POSITION  = 4.
  IT_DFIES-OFFSET    = 24.
  IT_DFIES-INTLEN    = 4.
  IT_DFIES-OUTPUTLEN = 4.
  IT_DFIES-SCRTEXT_S = 'INDEX'.
  APPEND IT_DFIES.
  CLEAR IT_DFIES.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      RETFIELD               = 'BWART'
*   PVALKEY                = ' '
      DYNPPROG               = 'ZRMMPM06R_MON_GI_ABNORMAL'
      DYNPNR                 = SY-DYNNR
      DYNPROFIELD            = 'BWART'
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
      VALUE_ORG              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
    TABLES
      VALUE_TAB              = IT_BWART
      FIELD_TAB              = IT_DFIES
*   RETURN_TAB             =
*   DYNPFLD_MAPPING        =
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " POSSBLE_BWART_LOW
*&---------------------------------------------------------------------*
*&      Form  CHECK_BWART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BWART.
  CLEAR S_BWART.
  REFRESH S_BWART.
  IF P_BWART = '551' OR P_BWART = '905'.
    CASE P_BWART.
      WHEN '551'.
        S_BWART-SIGN = 'I'. S_BWART-OPTION = 'EQ'. S_BWART-LOW = '551'.
        APPEND S_BWART.
        S_BWART-SIGN = 'I'. S_BWART-OPTION = 'EQ'. S_BWART-LOW = '552'.
        APPEND S_BWART.
      WHEN '905'.
        S_BWART-SIGN = 'I'. S_BWART-OPTION = 'EQ'. S_BWART-LOW = '905'.
        APPEND S_BWART.
        S_BWART-SIGN = 'I'. S_BWART-OPTION = 'EQ'. S_BWART-LOW = '906'.
        APPEND S_BWART.
    ENDCASE.
  ELSE.
    MESSAGE E023.
  ENDIF.
ENDFORM.                    " CHECK_BWART
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_ZSMM_RMMPM06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_IT_ZSMM_RMMPM06.
  DATA : TMP_ZSMM_RMMPM06 LIKE IT_ZSMM_RMMPM06
                               OCCURS 0 WITH HEADER LINE.
  DATA : TMP_ZSMM_RMMPM07 LIKE ZSMM_RMMPM07
                             OCCURS 0 WITH HEADER LINE.

  CLEAR IT_ZSMM_RMMPM06.
  REFRESH IT_ZSMM_RMMPM06.

*Get Data

  CASE P_BWART.
    WHEN '551'.
      SELECT * FROM ZVMM_GIABNORMAL
               INTO CORRESPONDING FIELDS OF TABLE TMP_ZSMM_RMMPM07
              WHERE WERKS IN S_WERKS
                AND LGORT IN S_LGORT
                AND MTART IN S_MTART
                AND RAUBE IN S_RAUBE
                AND DISPO IN S_DISPO
                AND BWART IN S_BWART
                AND BUDAT IN S_BUDAT.
      SORT TMP_ZSMM_RMMPM06 BY WERKS LGORT RAUBE MATNR.
*Get data for display
      LOOP AT TMP_ZSMM_RMMPM07.
        MOVE-CORRESPONDING TMP_ZSMM_RMMPM07
                        TO  IT_ZSMM_RMMPM06.
        CASE P_BWART.
          WHEN '551' OR '905'.
            W_DMBTR =
              W_DMBTR + TMP_ZSMM_RMMPM07-DMBTR.
            W_MENGE =
              W_MENGE + TMP_ZSMM_RMMPM07-MENGE.
          WHEN '552' OR '906'.
            W_DMBTR =
              W_DMBTR - TMP_ZSMM_RMMPM07-DMBTR.
            W_MENGE =
              W_MENGE - TMP_ZSMM_RMMPM07-MENGE.
        ENDCASE.

        AT END OF MATNR.
          SUM.
          IT_ZSMM_RMMPM06-NETPR = W_DMBTR / W_MENGE.
          IT_ZSMM_RMMPM06-DMBTR = W_DMBTR.
          IT_ZSMM_RMMPM06-MENGE = W_MENGE.
          APPEND IT_ZSMM_RMMPM06.
          CLEAR : W_DMBTR, W_MENGE.
        ENDAT.
        CLEAR IT_ZSMM_RMMPM06.
      ENDLOOP.
    WHEN '905'.
      SELECT * FROM ZVMM_GIABNORMAL
               INTO CORRESPONDING FIELDS OF TABLE TMP_ZSMM_RMMPM06
              WHERE WERKS IN S_WERKS
                AND LGORT IN S_LGORT
                AND MTART IN S_MTART
                AND RAUBE IN S_RAUBE
                AND DISPO IN S_DISPO
                AND BWART IN S_BWART
                AND BUDAT IN S_BUDAT.
      SORT TMP_ZSMM_RMMPM06 BY WERKS LGORT RAUBE DISPO MATNR.
*Get data for display
      LOOP AT TMP_ZSMM_RMMPM06.
        MOVE-CORRESPONDING TMP_ZSMM_RMMPM06
                        TO  IT_ZSMM_RMMPM06.
        CASE P_BWART.
          WHEN '551' OR '905'.
            W_DMBTR =
              W_DMBTR + TMP_ZSMM_RMMPM06-DMBTR.
            W_MENGE =
              W_MENGE + TMP_ZSMM_RMMPM06-MENGE.
          WHEN '552' OR '906'.
            W_DMBTR =
              W_DMBTR - TMP_ZSMM_RMMPM06-DMBTR.
            W_MENGE =
              W_MENGE - TMP_ZSMM_RMMPM06-MENGE.
        ENDCASE.

        AT END OF MATNR.
          SUM.
          IT_ZSMM_RMMPM06-NETPR = W_DMBTR / W_MENGE.
          IT_ZSMM_RMMPM06-DMBTR = W_DMBTR.
          IT_ZSMM_RMMPM06-MENGE = W_MENGE.
          APPEND IT_ZSMM_RMMPM06.
          CLEAR : W_DMBTR, W_MENGE.
        ENDAT.
        CLEAR IT_ZSMM_RMMPM06.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " MAKE_IT_ZSMM_RMMPM06
*&---------------------------------------------------------------------*
*&      Form  MAKE_SUM_BY_MAT_PLT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SUM_BY_MAT_PLT.
  DATA : TMP_ZSMM_RMMPM08 LIKE ZSMM_RMMPM08
                               OCCURS 0 WITH HEADER LINE.

  CLEAR IT_ZSMM_RMMPM06.
  REFRESH IT_ZSMM_RMMPM06.

*Get Data
  SELECT * FROM ZVMM_GIABNORMAL
           INTO CORRESPONDING FIELDS OF TABLE TMP_ZSMM_RMMPM08
          WHERE WERKS IN S_WERKS
            AND LGORT IN S_LGORT
            AND MTART IN S_MTART
            AND RAUBE IN S_RAUBE
            AND DISPO IN S_DISPO
            AND BWART IN S_BWART
            AND BUDAT IN S_BUDAT.
  SORT TMP_ZSMM_RMMPM08 BY WERKS MATNR.

*Get data for display
  LOOP AT TMP_ZSMM_RMMPM08.
    MOVE-CORRESPONDING TMP_ZSMM_RMMPM08
                    TO  IT_ZSMM_RMMPM06.
    CASE P_BWART.
      WHEN '551' OR '905'.
        W_DMBTR =
          W_DMBTR + TMP_ZSMM_RMMPM08-DMBTR.
        W_MENGE =
          W_MENGE + TMP_ZSMM_RMMPM08-MENGE.
      WHEN '552' OR '906'.
        W_DMBTR =
          W_DMBTR - TMP_ZSMM_RMMPM08-DMBTR.
        W_MENGE =
          W_MENGE - TMP_ZSMM_RMMPM08-MENGE.
    ENDCASE.

    AT END OF MATNR.
      SUM.
      IT_ZSMM_RMMPM06-NETPR = W_DMBTR / W_MENGE.
      IT_ZSMM_RMMPM06-DMBTR = W_DMBTR.
      IT_ZSMM_RMMPM06-MENGE = W_MENGE.
      APPEND IT_ZSMM_RMMPM06.
      CLEAR : W_DMBTR, W_MENGE.
    ENDAT.
    CLEAR IT_ZSMM_RMMPM06.
  ENDLOOP.
ENDFORM.                    " MAKE_SUM_BY_MAT_PLT
