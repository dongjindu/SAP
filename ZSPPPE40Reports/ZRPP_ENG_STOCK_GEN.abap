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

REPORT zrpp_eng_stock NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
TABLES: ztpp_eng_stock, mkpf, t001w.
TYPE-POOLS: slis, vrm.

DATA: it_tab LIKE TABLE OF ztpp_eng_stock WITH HEADER LINE.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1),
      w_flag(1).
DATA: w_prgrp LIKE pgmi-prgrp.

DATA: xname    TYPE vrm_id,
      xlist    TYPE vrm_values,
      xvalue   LIKE LINE OF xlist.

DATA:  l_kalid LIKE kako-kalid.
DATA:  BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
        END OF it_matnr.

*CONSTANTS : C_WERKS LIKE MARC-WERKS VALUE 'E001'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_prgrp LIKE pgmi-prgrp OBLIGATORY..
SELECT-OPTIONS: s_cpudt FOR mkpf-cpudt OBLIGATORY.
*SELECT-OPTIONS: s_budat FOR mkpf-budat NO-DISPLAY. "OBLIGATORY.
*PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
*SELECT-OPTIONS: s_werks FOR t001w-werks OBLIGATORY.
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN: ULINE.
PARAMETERS: p_arch AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

*at selection-screen on value-request for p_prdt.
AT SELECTION-SCREEN OUTPUT.
  PERFORM make_dropdown_list_box.
  PERFORM set_init_data.

START-OF-SELECTION.

*  CASE p_prdt.
*    WHEN '1'.
*      w_prgrp = 'MIP-ENG'.
*    WHEN '2'.
*      w_prgrp = 'KD-ENG'.
*    WHEN '3'.
*      w_prgrp = 'MIP-3C'.
*    WHEN '4'.
*      w_prgrp = 'ALL-ENG'.
*    WHEN '5'.
*      w_prgrp = 'GROUP-1'.
*    WHEN '6'.
*      w_prgrp = 'GROUP-2'.
*    WHEN '7'.
*      w_prgrp = 'GROUP-3'.
*  ENDCASE.

  PERFORM read_data.

  IF it_tab[] IS INITIAL.
    MESSAGE i001 WITH text-004.
    EXIT.
  ENDIF.
  PERFORM update_table.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM read_data.

*  DATA: lt_pgmi LIKE TABLE OF pgmi WITH HEADER LINE.
  DATA: BEGIN OF lt_tab OCCURS 0,
        mblnr LIKE mseg-mblnr,
        mjahr LIKE mkpf-mjahr,
** Changed by Furong on 07/29/08
        zeile LIKE mseg-zeile,
** End of change
        werks LIKE ztpp_eng_stock-werks,
        matnr LIKE ztpp_eng_stock-matnr,
        bwart LIKE ztpp_eng_stock-bwart,
        shkzg LIKE ztpp_eng_stock-shkzg,
        cpudt LIKE ztpp_eng_stock-cpudt,
        budat LIKE ztpp_eng_stock-budat,
        crdate LIKE ztpp_eng_stock-crdate,
        menge LIKE ztpp_eng_stock-menge,
        END OF lt_tab.
  DATA: lt_tab_archive LIKE TABLE OF lt_tab WITH HEADER LINE.

  DATA: l_begin_date LIKE sy-datum,
        l_end_date LIKE sy-datum,
        l_next_month LIKE sy-datum.

  CLEAR : it_tab, it_tab[].

** On 09/26/13
*  SELECT * INTO TABLE lt_pgmi
*   FROM pgmi
*   WHERE prgrp = w_prgrp
*** FOR E002
**     AND WERKS = C_WERKS.
*     AND werks IN s_werks.
*** END E002
*
*  LOOP AT lt_pgmi.
*    lt_matnr-matnr = lt_pgmi-nrmit.
*    APPEND lt_matnr.
*  ENDLOOP.

  REFRESH it_matnr.

  PERFORM get_engine_material USING p_prgrp
                                    p_werks.

*  REFRESH it_pgmi.
** End on 09/26/13

  IF it_matnr IS INITIAL.
    w_no_data = 'X'.
    MESSAGE i000(zz) WITH  'No data found'.
    EXIT.
  ELSE.


*    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
*      EXPORTING
*        iv_date             = s_cpudt-low
*      IMPORTING
*        ev_month_begin_date = l_begin_date
**       EV_MONTH_END_DATE   =  L_END_DATE
*      .
*
*    CALL FUNCTION 'OIL_GET_PREV_MONTH'
*      EXPORTING
*        i_date = l_begin_date
*      IMPORTING
*        e_date = l_begin_date.
*
*    s_budat-sign = s_cpudt-sign.
*    s_budat-option = s_cpudt-option.
*    s_budat-low = l_begin_date.
*
*    IF NOT s_cpudt-high IS INITIAL.
*
*      CALL FUNCTION 'OIL_GET_NEXT_MONTH'
*        EXPORTING
*          i_date = s_cpudt-high
*        IMPORTING
*          e_date = l_next_month.
*
*      CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
*        EXPORTING
*          iv_date             = l_next_month
*        IMPORTING
**         EV_MONTH_BEGIN_DATE = L_BEGIN_DATE
*          ev_month_end_date   = l_end_date.
*      s_budat-high = l_end_date.
*    else.
*      s_budat-high = '29991231'.
*    ENDIF.
*    APPEND s_budat.
*** End addation
*
*    SELECT a~mblnr a~mjahr zeile werks matnr bwart shkzg      "Addition
*                 cpudt budat menge
*            INTO CORRESPONDING FIELDS OF TABLE lt_tab
*            FROM mseg AS b INNER JOIN mkpf AS a
*              ON a~mblnr = b~mblnr
*             AND a~mjahr = b~mjahr
*           FOR ALL ENTRIES IN it_matnr
*           WHERE b~zbudat IN s_budat
*             AND b~matnr = it_matnr-matnr
*             AND a~cpudt IN s_cpudt.

    SELECT a~mblnr a~mjahr zeile werks matnr bwart shkzg      "Addition
              cpudt budat menge
         INTO CORRESPONDING FIELDS OF TABLE lt_tab
         FROM mkpf AS a
        INNER JOIN mseg AS b
           ON a~mblnr = b~mblnr
          AND a~mjahr = b~mjahr
        WHERE cpudt IN s_cpudt
   %_HINTS ORACLE 'ORDERED USE_NL (T_00 T_01) INDEX (T_00 "MKPF~Z02")'.

    SORT it_matnr BY matnr.

    LOOP AT lt_tab.
      READ TABLE it_matnr WITH KEY matnr = lt_tab-matnr
                                   BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE lt_tab.
      ENDIF.
    ENDLOOP.

*-   Commented by Tuning project 05.11.2012
*    SELECT A~MBLNR A~MJAHR ZEILE WERKS MATNR BWART SHKZG
*           CPUDT BUDAT MENGE
*       INTO CORRESPONDING FIELDS OF TABLE LT_TAB
*       FROM MKPF AS A
*       INNER JOIN MSEG AS B
*       ON A~MBLNR = B~MBLNR
*       FOR ALL ENTRIES IN LT_MATNR
*       WHERE MATNR = LT_MATNR-MATNR
*         AND CPUDT IN S_CPUDT.
** Changed by Furong on 11/25/09
    IF p_arch = 'X'.
      SELECT mblnr mjahr zeile werks matnr bwart shkzg cpudt budat menge
              INTO CORRESPONDING FIELDS OF TABLE lt_tab_archive
              FROM mari
              FOR ALL ENTRIES IN it_matnr
              WHERE matnr = it_matnr-matnr
                AND cpudt IN s_cpudt.
      LOOP AT lt_tab_archive.
        READ TABLE lt_tab WITH KEY mblnr = lt_tab_archive-mblnr
                                 mjahr = lt_tab_archive-mjahr
                                 zeile = lt_tab_archive-zeile.
        IF sy-subrc <> 0.
          CLEAR lt_tab.
          MOVE-CORRESPONDING lt_tab_archive TO lt_tab.
          APPEND lt_tab.
        ENDIF.
      ENDLOOP.
    ENDIF.
*    IF SY-SUBRC = 0.
    IF NOT lt_tab[] IS INITIAL.
** End of change

      REFRESH it_tab.
      CLEAR: it_tab.

      LOOP AT lt_tab.
        MOVE-CORRESPONDING lt_tab TO it_tab.
*        IT_TAB = LT_TAB.
        it_tab-werks = lt_tab-werks.
        it_tab-matnr = lt_tab-matnr.
        it_tab-bwart = lt_tab-bwart.
        it_tab-shkzg = lt_tab-shkzg.
        it_tab-cpudt = lt_tab-cpudt.
        it_tab-budat = lt_tab-budat.
        it_tab-menge = lt_tab-menge.
        it_tab-crdate = sy-datum.
        COLLECT it_tab.
        CLEAR: it_tab, lt_tab.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    "READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_dropdown_list_box.
  CLEAR : xlist[] , xvalue.

  xvalue-text = 'MIP-ENG'.
  xvalue-key  = '1'.
  APPEND xvalue TO xlist .
*
  xvalue-text = 'KD-ENG'.
  xvalue-key  = '2'.
  APPEND xvalue TO xlist .

  xvalue-text = 'MIP-3C'.
  xvalue-key  = '3'.
  APPEND xvalue TO xlist .

  xvalue-text = 'ALL-ENG'.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .

  xvalue-text = 'GROUP-1'.
  xvalue-key  = '5'.
  APPEND xvalue TO xlist .

  xvalue-text = 'GROUP-2'.
  xvalue-key  = '6'.
  APPEND xvalue TO xlist .

  xvalue-text = 'GROUP-3'.
  xvalue-key  = '7'.
  APPEND xvalue TO xlist .


  PERFORM list_box_function USING 'P_PRDT'.
*  READ TABLE XLIST INTO XVALUE  INDEX 1.
*  P_PRDT = XVALUE-KEY.

ENDFORM.                    " MAKE_DROPDOWN_LIST_BOX

*---------------------------------------------------------------------*
*       FORM LIST_BOX_FUNCTION                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_LIST_NAME                                                   *
*---------------------------------------------------------------------*
FORM list_box_function USING   p_list_name .
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = p_list_name  " list box
      values          = xlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
ENDFORM.                    " list_box_function
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  MODIFY ztpp_eng_stock FROM TABLE it_tab.
  IF sy-subrc = 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  set_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_data.

  s_cpudt-option = 'BT'.
  s_cpudt-sign = 'I'.
  s_cpudt-low = sy-datum - 1.
  s_cpudt-high = sy-datum.
  APPEND s_cpudt.
ENDFORM.                    " set_init_data
*&---------------------------------------------------------------------*
*&      Form  GET_ENGINE_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_PRGRP  text
*      -->P_S_WERKS  text
*----------------------------------------------------------------------*
FORM get_engine_material  USING  p_prgrp
                                 p_werks.
  DATA: lt_pgmi LIKE TABLE OF pgmi WITH HEADER LINE.
  DATA: l_matnr LIKE marc-matnr.

  SELECT * INTO TABLE lt_pgmi
  FROM pgmi
  WHERE prgrp = p_prgrp
    AND werks = p_werks.

  LOOP AT lt_pgmi.
    it_matnr-matnr = lt_pgmi-nrmit.
    SELECT SINGLE matnr INTO l_matnr
      FROM mara
      WHERE matnr = it_matnr-matnr
        AND mtart = 'PROD'.
    IF sy-subrc <> 0.
      COLLECT it_matnr.
    ELSE.
      PERFORM get_engine_material USING lt_pgmi-nrmit
*                          lt_pgmi-werks
                           lt_pgmi-wemit. "02.26.2014 Victor
    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_ENGINE_MATERIAL
