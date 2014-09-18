*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'INQ'.
*      PERFORM SET_MONTHS.
      PERFORM SELECT_DATA_200.
    WHEN 'EXIT'.
*      CLEAR: W_NEW, W_REFRESH.
      LEAVE PROGRAM.
    WHEN 'BACK'.
*      CLEAR: W_NEW, W_REFRESH.
      LEAVE TO SCREEN 0.
    WHEN 'EXCL'.
*      PERFORM DOWN_EXCEL_100.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA_200.
  PERFORM GET_DATA_200.
ENDFORM.                    " SELECT_DATA_200
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_200.

  DATA: LT_TAB LIKE TABLE OF ZTPP_ENG_STOCK WITH HEADER LINE,
        WA_GRAND_P LIKE IT_TAB,
        WA_GRAND_N LIKE IT_TAB,
        WA_TEMP LIKE IT_TAB,
        L_DATE_CHAR(8),
        L_FIRST_DATE LIKE SY-DATUM,
        L_LAST_DATE LIKE SY-DATUM.

  DATA: W_TOTAL TYPE I,
        L_CN(2) TYPE N,
        W_BWART LIKE LT_TAB-BWART,
        W_DESC LIKE IT_BWART-DESC.

  REFRESH: IT_TAB, IT_OUTPUT,IT_OUTPUT_MTH.
  CLEAR:  IT_TAB, IT_OUTPUT, IT_OUTPUT_MTH.

  IF W_YYYY IS INITIAL.
    W_YYYY = SY-DATUM+0(4).
  ENDIF.
  CONCATENATE W_YYYY+0(4) '0101' INTO L_DATE_CHAR.
  Z_BEG_DATE = L_DATE_CHAR.
  CONCATENATE W_YYYY+0(4) '1231' INTO L_DATE_CHAR.
  Z_MAX_DATE = L_DATE_CHAR.

  SELECT * INTO TABLE LT_TAB
    FROM ZTPP_ENG_STOCK
** FOR E002
*    WHERE MATNR = W-MATNR
** 02/22/12
*    WHERE WERKS = W_WERKS
*      AND MATNR = W-MATNR
   WHERE MATNR = W-MATNR
** end 02/22/12
** EN
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
        WHILE L_CN <= '12'.
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

      LOOP AT IT_MONTH.
        CONCATENATE 'IT_TAB-QTY' IT_MONTH-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        CONCATENATE W_YYYY IT_MONTH-SEQ '01' INTO L_DATE_CHAR.
        L_FIRST_DATE = L_DATE_CHAR.
        CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
           EXPORTING
             DAY_IN                  = L_FIRST_DATE
          IMPORTING
            LAST_DAY_OF_MONTH       =  L_LAST_DATE
*   EXCEPTIONS
*     DAY_IN_NO_DATE          = 1
*     OTHERS                  = 2
                   .
        IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        LOOP AT LT_TAB WHERE BWART = IT_BWART-BWART
                         AND BUDAT BETWEEN L_FIRST_DATE
                                    AND L_LAST_DATE.
          IF LT_TAB-SHKZG = 'S'.
            <FS-QTY> = <FS-QTY> + LT_TAB-MENGE.
          ELSE.
            <FS-QTY> = <FS-QTY> - LT_TAB-MENGE.
          ENDIF.
        ENDLOOP.
        W_TOTAL = W_TOTAL + <FS-QTY>.

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
    WHILE L_CN <= '12'.
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
        WHILE L_CN <= '12'.
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
    WHILE L_CN <= '12'.
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
  LOOP AT IT_OUTPUT.
    MOVE-CORRESPONDING IT_OUTPUT TO IT_OUTPUT_MTH.
    APPEND IT_OUTPUT_MTH.
    CLEAR: IT_OUTPUT, IT_OUTPUT_MTH.
  ENDLOOP.
ENDFORM.                    " GET_DATA_200
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
   CASE OK_CODE.
    WHEN 'INQ'.
      PERFORM SELECT_DATA.
    WHEN 'EXIT'.
*      CLEAR: W_NEW, W_REFRESH.
      LEAVE PROGRAM.
    WHEN 'BACK'.
*      CLEAR: W_NEW, W_REFRESH.
      LEAVE TO SCREEN 0.
    WHEN 'EXCL'.
      PERFORM DOWN_EXCEL_100.
    ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
