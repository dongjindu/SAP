************************************************************************
* Program Name      : ZRPP_ENG_STOCK
* Creation Date     : 09/11/2007
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZRPP_ENG_STOCK_PLAN NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES: ZTPP_ENG_STOCK.
TYPE-POOLS: SLIS, VRM.
DATA: BEGIN OF IT_BWART OCCURS 30,

      BWART LIKE ZTPP_ENG_STOCK-BWART,
      DESC(20),
      END OF IT_BWART.

DATA: BEGIN OF IT_OUTPUT OCCURS 0,
*      WERKS LIKE MARC-WERKS,
*      MATNR LIKE MARA-MATNR,
      DESC(20),
      BWART LIKE ZTPP_ENG_STOCK-BWART,
      TOTAL(13),
      QTY01(13),
      QTY02(13),
      QTY03(13),
      QTY04(13),
      QTY05(13),
      QTY06(13),
      QTY07(13),
      QTY08(13),
      QTY09(13),
      QTY10(13),
      QTY11(13),
      QTY12(13),
      QTY13(13),
      QTY14(13),
      QTY15(13),
      QTY16(13),
      QTY17(13),
      QTY18(13),
      QTY19(13),
      QTY20(13),
      QTY21(13),
      QTY22(13),
      QTY23(13),
      QTY24(13),
      QTY25(13),
      QTY26(13),
      QTY27(13),
      QTY28(13),
      QTY29(13),
      QTY30(13),
      QTY31(13),
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_OUTPUT.

DATA: BEGIN OF IT_OUTPUT_MTH OCCURS 0,
*      WERKS LIKE MARC-WERKS,
*      MATNR LIKE MARA-MATNR,
      DESC(20),
      BWART LIKE ZTPP_ENG_STOCK-BWART,
      TOTAL(13),
      QTY01(13),
      QTY02(13),
      QTY03(13),
      QTY04(13),
      QTY05(13),
      QTY06(13),
      QTY07(13),
      QTY08(13),
      QTY09(13),
      QTY10(13),
      QTY11(13),
      QTY12(13),
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_OUTPUT_MTH.

DATA: BEGIN OF IT_TAB OCCURS 0,
*      WERKS LIKE MARC-WERKS,
*      MATNR LIKE MARA-MATNR,
      DESC(20),
      BWART LIKE ZTPP_ENG_STOCK-BWART,
      TOTAL TYPE i,
      QTY01 TYPE I,
      QTY02 TYPE I,
      QTY03 TYPE I,
      QTY04 TYPE I,
      QTY05 TYPE I,
      QTY06 TYPE I,
      QTY07 TYPE I,
      QTY08 TYPE I,
      QTY09 TYPE I,
      QTY10 TYPE I,
      QTY11 TYPE I,
      QTY12 TYPE I,
      QTY13 TYPE I,
      QTY14 TYPE I,
      QTY15 TYPE I,
      QTY16 TYPE I,
      QTY17 TYPE I,
      QTY18 TYPE I,
      QTY19 TYPE I,
      QTY20 TYPE I,
      QTY21 TYPE I,
      QTY22 TYPE I,
      QTY23 TYPE I,
      QTY24 TYPE I,
      QTY25 TYPE I,
      QTY26 TYPE I,
      QTY27 TYPE I,
      QTY28 TYPE I,
      QTY29 TYPE I,
      QTY30 TYPE I,
      QTY31 TYPE I,
      IF(4) TYPE C,
      CELLTAB TYPE LVC_T_STYL,
     END OF IT_TAB.


*DATA: BEGIN OF IT_TAB OCCURS 0,
**      WERKS LIKE MARC-WERKS,
**      MATNR LIKE MARA-MATNR,
*      DESC(20),
*      BWART LIKE ZTPP_ENG_STOCK-BWART,
*      TOTAL LIKE MSEG-MENGE,
*      QTY01 LIKE MSEG-MENGE,
*      QTY02 LIKE MSEG-MENGE,
*      QTY03 LIKE MSEG-MENGE,
*      QTY04 LIKE MSEG-MENGE,
*      QTY05 LIKE MSEG-MENGE,
*      QTY06 LIKE MSEG-MENGE,
*      QTY07 LIKE MSEG-MENGE,
*      QTY08 LIKE MSEG-MENGE,
*      QTY09 LIKE MSEG-MENGE,
*      QTY10 LIKE MSEG-MENGE,
*      QTY11 LIKE MSEG-MENGE,
*      QTY12 LIKE MSEG-MENGE,
*      QTY13 LIKE MSEG-MENGE,
*      QTY14 LIKE MSEG-MENGE,
*      QTY15 LIKE MSEG-MENGE,
*      QTY16 LIKE MSEG-MENGE,
*      QTY17 LIKE MSEG-MENGE,
*      QTY18 LIKE MSEG-MENGE,
*      QTY19 LIKE MSEG-MENGE,
*      QTY20 LIKE MSEG-MENGE,
*      QTY21 LIKE MSEG-MENGE,
*      QTY22 LIKE MSEG-MENGE,
*      QTY23 LIKE MSEG-MENGE,
*      QTY24 LIKE MSEG-MENGE,
*      QTY25 LIKE MSEG-MENGE,
*      QTY26 LIKE MSEG-MENGE,
*      QTY27 LIKE MSEG-MENGE,
*      QTY28 LIKE MSEG-MENGE,
*      QTY29 LIKE MSEG-MENGE,
*      QTY30 LIKE MSEG-MENGE,
*      QTY31 LIKE MSEG-MENGE,
*      IF(4) TYPE C,
*      CELLTAB TYPE LVC_T_STYL,
*     END OF IT_TAB.
*
*DATA: IT_output LIKE TABLE OF IT_tab WITH HEADER LINE.

DATA: BEGIN OF IT_EXCEL_100 OCCURS 0,
      COL01(20), " description
      COL02(5), " movement type
      COL03(7), " total
      COL04(7), " 1
      COL05(7), " 2
      COL06(7), "
      COL07(7), "
      COL08(7), "
      COL09(7), "
      COL10(7), "
      COL11(7),
      COL12(7), "
      COL13(7),
      COL14(7),
      COL15(7),
      COL16(7),
      COL17(7),
      COL18(7),
      COL19(7),
      COL20(7),
      COL21(7),
      COL22(7),
      COL23(7),
      COL24(7),
      COL25(7),
      COL26(7),
      COL27(7),
      COL28(7),
      COL29(7),
      COL30(7),
      COL31(7),
      COL32(7),
      COL33(7),
      COL34(7),
      END OF IT_EXCEL_100.

DATA: L_LINE TYPE I,
        L_CN(2) TYPE N,
        L_TEXT(30).


FIELD-SYMBOLS : <FS01>, <FS02>, <FS-QTY>.

DATA: BEGIN OF IT_DAY OCCURS 31,
        SEQ(2)  TYPE N,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_DAY.

DATA: BEGIN OF IT_MONTH OCCURS 12,
        SEQ(2) TYPE N,
        MONTH(3),
      END   OF IT_MONTH.

DATA : Z_MAX_DATE LIKE SY-DATUM,
       Z_BEG_DATE LIKE SY-DATUM,
       W_YYMM(6),
       w_yyyy(4),
       w-yyyy like mkpf-MJAHR..

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I,
      W_NO_DATA(1),
      W-MATNR TYPE MARA-MATNR,
      W_FLAG(1).

DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_TOT  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_FNAME_TOT    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_EXCLUDE      TYPE UI_FUNCTIONS,
       IT_EXCLUDE_TOT  TYPE UI_FUNCTIONS.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       WA_TOT_LAYOUT TYPE LVC_S_LAYO,
       W_FIELDNAME  LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
      WA_TOT_SAVE  TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_TOT_VARIANT TYPE DISVARIANT.     "for parameter IS_VARIANT

data: w_werks like t001w-werks.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WA_CUSTOM_CONTROL_TOT TYPE    SCRFNAME VALUE 'ALV_CONTAINER_TOT',
      ALV_GRID_TOT          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER_TOT    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

**--- Constants
CONSTANTS : C_WERKS LIKE MARC-WERKS VALUE 'E001'.

DATA :IT_LVC  LIKE LVC_S_ROW.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: LT_TAB LIKE TABLE OF ZTPP_ENG_STOCK WITH HEADER LINE,
        WA_GRAND_P LIKE IT_TAB,
        WA_GRAND_N LIKE IT_TAB,
        WA_TEMP LIKE IT_TAB.

  DATA: W_TOTAL TYPE I,
        L_CN(2) TYPE N,
        W_BWART LIKE LT_TAB-BWART,
        W_DESC LIKE IT_BWART-DESC.

  REFRESH: IT_TAB, IT_OUTPUT.
  CLEAR:  IT_TAB, IT_OUTPUT.
  SELECT * INTO TABLE LT_TAB
    FROM ZTPP_ENG_STOCK
** FOR E002
*    WHERE MATNR = W-MATNR
** 02/22/12
*    WHERE WERKS = W_WERKS
*      AND MATNR = W-MATNR
    WHERE MATNR = W-MATNR
** 02/22/12
** END
      AND BUDAT BETWEEN Z_BEG_DATE AND Z_MAX_DATE.
  SORT LT_TAB BY BWART BUDAT.

  IF SY-SUBRC = 0.

    READ TABLE IT_BWART INDEX 1.
    W_DESC = IT_BWART-DESC.

    LOOP AT IT_BWART.
      IF IT_BWART-DESC  = W_DESC.
        IF SY-TABIX > 1.
          CLEAR: IT_TAB-DESC.
        ELSE.
          IT_TAB-DESC = IT_BWART-DESC.
        ENDIF.
      ELSE.
        IF L_LINE = 0.
          L_LINE = 1.
        ELSE.
          L_LINE = 0.
        ENDIF.
        LOOP AT IT_TAB.
          MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
*          IF L_LINE = 1.
*            IT_OUTPUT-IF = 'C210'.
*          ENDIF.
          APPEND IT_OUTPUT.
        ENDLOOP.
        CLEAR: IT_OUTPUT.

        LOOP AT IT_TAB.
          WA_TEMP-TOTAL = WA_TEMP-TOTAL + IT_TAB-TOTAL.
        ENDLOOP.
        L_CN = '01'.
        WHILE L_CN <= '31'.
          CONCATENATE 'IT_TAB-QTY' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS-QTY>.
          CONCATENATE 'WA_TEMP-QTY' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS01>.
          LOOP AT IT_TAB.
            <FS01> = <FS01> + <FS-QTY>.
          ENDLOOP.
          L_CN =  L_CN + 1.
        ENDWHILE.
        MOVE-CORRESPONDING WA_TEMP TO IT_OUTPUT.
        IT_OUTPUT-DESC = 'Total'.
        IT_OUTPUT-IF = 'C310'.
        APPEND IT_OUTPUT.
        REFRESH IT_TAB.
        CLEAR: IT_TAB, IT_OUTPUT, WA_TEMP.
        W_DESC = IT_BWART-DESC.
        IT_TAB-DESC = IT_BWART-DESC.
      ENDIF.
      IT_TAB-BWART = IT_BWART-BWART.

      LOOP AT IT_DAY.
        CONCATENATE 'IT_TAB-QTY' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          LOOP AT LT_TAB WHERE BWART = IT_BWART-BWART
                           AND BUDAT = IT_DAY-DATUM.
            IF LT_TAB-SHKZG = 'S'.
              <FS-QTY> = <FS-QTY> + LT_TAB-MENGE.
            ELSE.
              <FS-QTY> = <FS-QTY> - LT_TAB-MENGE.
            ENDIF.
          ENDLOOP.
          W_TOTAL = W_TOTAL + <FS-QTY>.
        ENDIF.
      ENDLOOP.
      IT_TAB-TOTAL = W_TOTAL.
      APPEND IT_TAB.
      CLEAR: IT_TAB, W_TOTAL.
    ENDLOOP.

    IF L_LINE = 0.
      L_LINE = 1.
    ELSE.
      L_LINE = 0.
    ENDIF.
    LOOP AT IT_TAB.
      MOVE-CORRESPONDING IT_TAB TO IT_OUTPUT.
*      IF L_LINE = 1.
*        IT_OUTPUT-IF = 'C210'.
*      ENDIF.
      APPEND IT_OUTPUT.
    ENDLOOP.
    CLEAR: IT_OUTPUT.

    LOOP AT IT_TAB.
      WA_TEMP-TOTAL = WA_TEMP-TOTAL + IT_TAB-TOTAL.
    ENDLOOP.
    L_CN = '01'.
    WHILE L_CN <= '31'.
      CONCATENATE 'IT_TAB-QTY' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS-QTY>.
      CONCATENATE 'WA_TEMP-QTY' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS01>.
      LOOP AT IT_TAB.
        <FS01> = <FS01> + <FS-QTY>.
      ENDLOOP.
      L_CN =  L_CN + 1.
    ENDWHILE.
    MOVE-CORRESPONDING WA_TEMP TO IT_OUTPUT.
    IT_OUTPUT-DESC = 'Total'.
    IT_OUTPUT-IF = 'C310'.
    APPEND IT_OUTPUT.
    REFRESH IT_TAB.
    CLEAR: IT_TAB, IT_OUTPUT, WA_TEMP.
    W_DESC = IT_BWART-DESC.
    IT_TAB-DESC = IT_BWART-DESC.

    LOOP AT IT_OUTPUT.
      IF IT_OUTPUT-DESC = 'Total'.
        IF IT_OUTPUT-TOTAL > 0.
          WA_GRAND_P-TOTAL = WA_GRAND_P-TOTAL + IT_OUTPUT-TOTAL.
        ELSE.
          WA_GRAND_N-TOTAL = WA_GRAND_N-TOTAL + IT_OUTPUT-TOTAL.
        ENDIF.

        L_CN = '01'.
        WHILE L_CN <= '31'.
          CONCATENATE 'IT_OUTPUT-QTY' L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS-QTY>.
          IF  <FS-QTY> > 0.
            CONCATENATE 'WA_GRAND_P-QTY' L_CN INTO L_TEXT.
          ELSE.
            CONCATENATE 'WA_GRAND_N-QTY' L_CN INTO L_TEXT.
          ENDIF.
          ASSIGN (L_TEXT) TO <FS01>.
          <FS01> = <FS01> + <FS-QTY>.
          L_CN =  L_CN + 1.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

    WA_GRAND_P-DESC = 'Stock +'.
    MOVE-CORRESPONDING WA_GRAND_P TO IT_OUTPUT.
    IT_OUTPUT-IF = 'C310'.
    APPEND IT_OUTPUT.
    WA_GRAND_N-DESC = 'Stock -'.
    MOVE-CORRESPONDING WA_GRAND_N TO IT_OUTPUT.
    IT_OUTPUT-IF = 'C310'.
    APPEND IT_OUTPUT.
    CLEAR: IT_OUTPUT.

    WA_TEMP-TOTAL = WA_GRAND_P-TOTAL + WA_GRAND_N-TOTAL.
    WA_TEMP-DESC = 'Month Stock'.

    L_CN = '01'.
    WHILE L_CN <= '31'.
      CONCATENATE 'WA_TEMP-QTY' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS-QTY>.
      CONCATENATE 'WA_GRAND_P-QTY' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS01>.
      CONCATENATE 'WA_GRAND_N-QTY' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS02>.
      <FS-QTY> = <FS01> + <FS02>.
      L_CN =  L_CN + 1.
    ENDWHILE.
    MOVE-CORRESPONDING WA_TEMP TO IT_OUTPUT.
    IT_OUTPUT-IF = 'C310'.
    APPEND IT_OUTPUT.
  ELSE.
    W_NO_DATA = 'X'.
    MESSAGE I000(ZZ) WITH  'No data found'.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_DAYS.
  DATA: L_COUNT TYPE I.
  DATA: L_DATE LIKE SY-DATUM,
        L_DATE_CHAR(8).

  CLEAR: IT_DAY, IT_DAY[].
  IF W_YYMM IS INITIAL.
    W_YYMM = SY-DATUM+0(6).
  ENDIF.
  CONCATENATE W_YYMM+0(4) W_YYMM+4(2) '01' INTO L_DATE_CHAR.
  Z_BEG_DATE = L_DATE_CHAR.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN                  = Z_BEG_DATE
   IMPORTING
     LAST_DAY_OF_MONTH       =  Z_MAX_DATE
*   EXCEPTIONS
*     DAY_IN_NO_DATE          = 1
*     OTHERS                  = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  L_COUNT = '01'.
  L_DATE = Z_BEG_DATE.

  WHILE L_DATE <= Z_MAX_DATE.
*   PERFORM READ_WORKING_DATE USING '+' L_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    APPEND IT_DAY.  CLEAR: IT_DAY.
  ENDWHILE.
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING P_L_KALID.
  SELECT SINGLE KALID INTO P_L_KALID
  FROM ZVPP_CAPACITY
 WHERE ARBPL = 'T'   .
ENDFORM.                    " read_shop_calid
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

INCLUDE ZRPP_ENG_STOCK_PBO.
INCLUDE ZRPP_ENG_STOCK_PAI.


*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LINE  text
*      -->P_1971   text
*----------------------------------------------------------------------*
*FORM BUILD_COLOR_ALL USING p_line p_fname.
*  if p_line  = 1.
*    wa_color-color-col = 6.
*    wa_color-color-int = 1.
*    wa_color-fname = p_fname.
*    append wa_color to it_color.
*    clear wa_color.
*  endif.
*endform.                    " BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*&      Form  SET_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_MONTHS.
  DATA: L_CN(2) TYPE N,
        L_DATE_CHAR(8).
  refresh it_month.
  L_CN = '01'.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Jan'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Feb'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Mar'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Apr'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'May'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Jun'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Jul'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Aug'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Sep'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Oct'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Nov'.
  APPEND IT_MONTH.
  L_CN =  L_CN + 1.
  IT_MONTH-SEQ = L_CN.
  IT_MONTH-MONTH = 'Dec'.
  APPEND IT_MONTH.

  IF W_YYYY IS INITIAL.
    W_YYYY = SY-DATUM+0(4).
  ENDIF.
*  CONCATENATE W_YYYY+0(4) '0101' INTO L_DATE_CHAR.
*  Z_BEG_DATE = L_DATE_CHAR.
*  CONCATENATE W_YYYY+0(4) '1231' INTO L_DATE_CHAR.
*  Z_MAX_DATE = L_DATE_CHAR.

*  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*    EXPORTING
*      DAY_IN                  = Z_BEG_DATE
*   IMPORTING
*     LAST_DAY_OF_MONTH       =  Z_MAX_DATE
**   EXCEPTIONS
**     DAY_IN_NO_DATE          = 1
**     OTHERS                  = 2
*            .
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

 ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA.
  PERFORM SET_DAYS.
  PERFORM GET_DATA.
*  IF W_NO_DATA = 'X'.
*    CLEAR: W_NO_DATA.
*    EXIT.
*  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWN_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWN_EXCEL_100.
  DATA: L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR IT_EXCEL_100.
  REFRESH IT_EXCEL_100.
  MOVE 'Description' TO IT_EXCEL_100-COL01.
  MOVE 'Mrv' TO IT_EXCEL_100-COL02.
  MOVE 'Total' TO IT_EXCEL_100-COL03.
  L_CN = '04'.
  LOOP AT IT_DAY.
    CONCATENATE 'IT_EXCEL_100-COL' L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS01>.
    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.
    MOVE L_DATUM+0(5) TO <FS01>.
    L_CN = L_CN + 1.
  ENDLOOP.
  APPEND IT_EXCEL_100.
  LOOP AT IT_OUTPUT.
    CLEAR IT_EXCEL_100.
    MOVE IT_OUTPUT-DESC TO IT_EXCEL_100-COL01.
    MOVE IT_OUTPUT-BWART TO IT_EXCEL_100-COL02.
    MOVE IT_OUTPUT-TOTAL TO IT_EXCEL_100-COL03.
    LOOP AT IT_DAY.
      L_CN = IT_DAY-SEQ + 3.
      CONCATENATE 'IT_EXCEL_100-COL' L_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS01>.
      CONCATENATE 'IT_OUTPUT-QTY' IT_DAY-SEQ INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FS02>.
      <FS01> = <FS02>.
    ENDLOOP.
    APPEND IT_EXCEL_100.
  ENDLOOP.
  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
     FILENAME                      = 'ENG_DAILY.XLS'
     FILETYPE                      = 'DAT'
     ITEM                          = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILETYPE_NO_SHOW              = ' '
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      DATA_TAB                      = IT_EXCEL_100
*   EXCEPTIONS
*     INVALID_FILESIZE              = 1
*     INVALID_TABLE_WIDTH           = 2
*     INVALID_TYPE                  = 3
*     NO_BATCH                      = 4
*     UNKNOWN_ERROR                 = 5
*     GUI_REFUSE_FILETRANSFER       = 6
*     CUSTOMER_ERROR                = 7
*     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DOWN_EXCEL_100
