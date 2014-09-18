*&---------------------------------------------------------------------*
*& Report  ZRIMBLLIST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : B/L Item List with Container data                     *
*&      작성자 : Shin-Ho, Na                                           *
*&      작성일 : 2001.11.8                                             *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*& Tuning: Andy Choi
*&---------------------------------------------------------------------*
REPORT  zrimbllist   MESSAGE-ID zim
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ztbl,
         ztblit,
         ztcivhd,
         ztiv,
         lips,
         likp,
         eikp,
         ekko,
         mkpf,
         mseg.
*>> Declaration Type-Pool for ALV Display.
TYPE-POOLS : slis.
*-----------------------------------------------------------------------
* B/L 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF it_tab OCCURS 0,
        ebeln        LIKE ztblit-ebeln,           " P/O No.
        ebelp        LIKE ztblit-ebelp,           " P/O Item No.
        zfblno       LIKE ztbl-zfblno,            " B/L Doc. No.
        zfshno       LIKE ztbl-zfshno,            " Voyage No.
        zfpoyn       LIKE ztbl-zfpoyn,            " Monetary Y/N.
        zfetd        LIKE ztbl-zfetd,             " ETD
        zfcarnm      LIKE ztbl-zfcarnm,           " Vessel Name.
        zfsprt       LIKE ztbl-zfsprt,            " Loading Port.
        ekgrp        LIKE ztbl-ekgrp,             " Purchasing Group.
        w_ekgrp(20)  TYPE c,                      " Purchas.Grp. Desc.
        lifnr        LIKE ztbl-lifnr,             " Vendor.
        w_lifnr(30)  TYPE c,                      " Vendor Name.
        zfhblno      LIKE ztbl-zfhblno,           " House B/L No.
        zfrgdsr      LIKE ztbl-zfrgdsr,           " Material Name.
        zfeta        LIKE ztbl-zfeta,             " ETA
        zfvia        LIKE ztbl-zfvia,             " VIA
        zfaprt       LIKE ztbl-zfaprt,            " 도착항
        zfford       LIKE ztbl-zfford,            " Shipping Company.
        w_zfford(30) TYPE c,                      " Shipping co. Name.
        zfbeni       LIKE ztbl-zfbeni,            " Beneficiary.
        w_zfbeni(30) TYPE c,                      " Beneficiary Name.
        zfcivrn      LIKE ztcivhd-zfcivrn,
        zfcivno      LIKE ztcivhd-zfcivno,
        zfcust       LIKE ztiv-zfcust,            " Clearance Status.
*        ZFGRST(03)   LIKE C,                      " G/R Status.
        w_zfcust(20) TYPE c,
        borgr_grp    LIKE likp-borgr_grp,         " Seal No.
        matnr        LIKE ztblit-matnr,           " Material.
        txz01        LIKE ztblit-txz01,           " Material Desc.
        zfreqno      LIKE ztreqhd-zfreqno,        " Import Request No.
        zfreqty      LIKE ztreqhd-zfreqty,        " Import Request Type.
        zfopnno      LIKE ztreqhd-zfopnno,        " L/C approval No.
        traid        LIKE likp-traid,             " Container Number.
        vbeln        LIKE likp-vbeln,             " Delivery No.
        kdmat        LIKE lips-kdmat,             " Case No.
        lfimg        LIKE lips-lfimg,             " Actual qty del..
        vrkme        LIKE lips-vrkme.             " Sales unit.
DATA : END OF it_tab.
DATA : BEGIN OF it_tab1 OCCURS 0.
        INCLUDE STRUCTURE it_tab.
DATA : END   OF it_tab1.

*>> Declaration of variable for ALV Display.
DATA: g_repid LIKE sy-repid.
DATA: g_layout          TYPE slis_layout_alv.
DATA: g_status          TYPE slis_formname VALUE 'P2000_ALV_PF_STATUS'.
DATA: gt_fieldcat       TYPE slis_t_fieldcat_alv.
DATA: gt_sort           TYPE slis_t_sortinfo_alv.
DATA: ls_fieldcat       TYPE slis_fieldcat_alv.
DATA: ls_sort           TYPE slis_sortinfo_alv.
DATA: pos               TYPE i.
DATA: g_save(1)         TYPE c.
DATA: g_variant         LIKE disvariant.
DATA: g_user_command    TYPE slis_formname VALUE 'P2000_ALV_COMMAND'.
DATA: w_top_of_page     TYPE slis_t_listheader.
DATA: w_eventcat        TYPE slis_t_event WITH HEADER LINE.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   zrimpreltop.    " 구매 Released  Report Data Define용 Include
INCLUDE   zrimsortcom.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   zrimutil01.     " Utility function 모음

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs   FOR ztbl-bukrs NO INTERVALS
                                         NO-EXTENSION,
                s_ebeln   FOR ztblit-ebeln,
                s_hblno   FOR ztbl-zfhblno,
                s_blno    FOR ztbl-zfblno,
                s_civno   FOR ztcivhd-zfcivno,
                s_eta     FOR ztbl-zfeta         NO-EXTENSION,
                s_shty    FOR ztbl-zfshty,
                s_ford    FOR ztbl-zfford,
                s_werks   FOR ztbl-zfwerks,
                s_matnr   FOR ztblit-matnr,
                s_vbeln   FOR likp-vbeln,
                s_traid   FOR likp-traid,
                s_ekgrp   FOR ztbl-ekgrp,
                s_cust    FOR ztiv-zfcust NO-DISPLAY.
PARAMETERS :    p_poyn    LIKE ztbl-zfpoyn.
PARAMETERS :    p_via     LIKE ztbl-zfvia.

SELECTION-SCREEN END OF BLOCK b1.

*>> Check G/R Status.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS : p_yes AS CHECKBOX DEFAULT 'X'.
PARAMETERS : p_no  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_y AS CHECKBOX.
PARAMETERS : p_n AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

* PARAMETER 초기값 Setting..
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   p1000_set_bukrs.
  PERFORM   p2000_set_parameter USING w_eventcat[].

* Title Text Write..
TOP-OF-PAGE.
  PERFORM   p3000_title_write.                  " 헤더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 파라메타 설정.
  PERFORM   p2000_set_selete_option   USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT.
  PERFORM   p1000_get_it_tab          USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT..
  PERFORM   p1000_read_text           USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.

* Container Data Select.
  PERFORM   p1000_get_container       USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.

* Report Write..
  PERFORM   p3000_data_write          USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택?
      w_field_nm = 'ZFBLNO'.
      ASSIGN w_field_nm   TO <sort_field>.

      PERFORM handle_sort TABLES  it_tab
                          USING   sy-ucomm.
    WHEN 'DISP'.          " L/C 조?
      PERFORM p2000_multi_selection.

      IF w_selected_lines EQ 1.
        READ TABLE it_selected INDEX 1.
        PERFORM p2000_show_lc USING it_selected-zfreqno.
      ELSEIF w_selected_lines GT 1.
        MESSAGE e965.
      ENDIF.

    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM p3000_to_pc_download.
    WHEN 'REFR'.
* 구매의뢰 테이블 SELECT
      PERFORM   p1000_get_it_tab          USING   w_err_chk.

      IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
      PERFORM   p1000_read_text           USING   w_err_chk.

      IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.
      PERFORM reset_list.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM p2000_set_parameter USING w_eventcat TYPE slis_t_event.
  SET  TITLEBAR 'ZIM24'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM p3000_title_write.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[  B/L Receipt Detail ]' CENTERED
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /100 'Date : ', sy-datum.  ", 101 'Page : ', W_PAGE.

  WRITE : / sy-uline.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / sy-vline NO-GAP,
            (20) 'P/O No'            CENTERED, sy-vline NO-GAP,
            (20) 'Money/Non-Money'   CENTERED, sy-vline NO-GAP,
            (10) 'E.T.D'             CENTERED, sy-vline NO-GAP,
            (15) 'Vessel name'       CENTERED, sy-vline NO-GAP,
            (15) 'Port of loading'   CENTERED, sy-vline NO-GAP,
            (15) 'Purchase group'    CENTERED, sy-vline NO-GAP,
            (20) 'Vendor'            CENTERED, sy-vline NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / sy-vline NO-GAP,
            (20) 'B/L No'            CENTERED, sy-vline NO-GAP,
            (20) 'Main Item'         CENTERED, sy-vline NO-GAP,
            (10) 'E.T.A'             CENTERED, sy-vline NO-GAP,
            (15) 'VIA'               CENTERED, sy-vline NO-GAP,
            (15) 'Port of arrival'   CENTERED, sy-vline NO-GAP,
            (15) 'Forwarder'         CENTERED, sy-vline NO-GAP,
            (20) 'Beneficiary'       CENTERED, sy-vline NO-GAP.
  WRITE : / sy-uline.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM p2000_set_selete_option   USING    w_err_chk.
*
  w_err_chk = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ztimimg00.
* Not Found
  IF sy-subrc NE 0.
    w_err_chk = 'Y'.   MESSAGE s961.   EXIT.
  ENDIF.

  IF p_via  IS INITIAL.  p_via  = '%'.   ENDIF.
  IF p_poyn IS INITIAL.  p_poyn = '%'.   ENDIF.

  IF NOT p_y IS INITIAL.
    MOVE: 'I'  TO s_cust-sign,
         'EQ' TO s_cust-option,
         'Y'  TO s_cust-low.
    APPEND s_cust.
  ENDIF.
  IF NOT p_n IS INITIAL.
    MOVE: 'I'  TO s_cust-sign,
         'EQ' TO s_cust-option,
         'N'  TO s_cust-low.
    APPEND s_cust.
    MOVE: 'I'  TO s_cust-sign,
         'EQ' TO s_cust-option,
         '1'  TO s_cust-low.
    APPEND s_cust.
    MOVE: 'I'  TO s_cust-sign,
         'EQ' TO s_cust-option,
         '2'  TO s_cust-low.
    APPEND s_cust.
    MOVE: 'I'  TO s_cust-sign,
         'EQ' TO s_cust-option,
         '3'  TO s_cust-low.
    APPEND s_cust.

  ENDIF.
ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM p1000_read_text USING    w_err_chk.

  LOOP AT it_tab1.
    w_tabix = sy-tabix.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : lfa1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              xlifnr         = it_tab1-zfford
         IMPORTING
              xlfa1          = lfa1
         EXCEPTIONS
              key_incomplete = 01
              not_authorized = 02
              not_found      = 03.

    MOVE: lfa1-name1   TO   it_tab1-w_zfford.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : lfa1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              xlifnr         = it_tab1-lifnr
         IMPORTING
              xlfa1          = lfa1
         EXCEPTIONS
              key_incomplete = 01
              not_authorized = 02
              not_found      = 03.

    MOVE: lfa1-name1   TO   it_tab1-w_lifnr.

*-----------------------------------------------------------------------
* T024 SELECT( 구매그룹)
*-----------------------------------------------------------------------
    SELECT SINGLE eknam INTO it_tab1-w_ekgrp
      FROM t024
     WHERE ekgrp = it_tab1-ekgrp.
    CASE it_tab1-zfcust.
      WHEN '1'.
        MOVE 'Declaration Creation' TO it_tab1-w_zfcust.
      WHEN '2'.
        MOVE 'object to declare'    TO it_tab1-w_zfcust.
      WHEN '3'.
        MOVE 'in declaring'         TO it_tab1-w_zfcust.
      WHEN 'Y'.
        MOVE 'Completed Clearance'  TO it_tab1-w_zfcust.
      WHEN 'N'.
        MOVE 'Not object to clear'  TO it_tab1-w_zfcust.
    ENDCASE.
    MODIFY it_tab1 INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM p3000_data_write USING      w_err_chk.

  DATA: titlebar TYPE lvc_title.
  IF p_yes EQ 'X'.
    titlebar = 'Container List by Material [G/R Completed]'.
  ELSEIF p_no EQ 'X'.
    titlebar = 'Container List by Material [G/R Incompleted]'.
  ENDIF.
  SET PF-STATUS 'ZIM24'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM24'.           " GUI TITLE SETTING..

  PERFORM p3000_append_fieldcat.      " ALV Report TiTle.

  g_repid = sy-repid.
  DATA: slis_formname(30)  TYPE c.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = g_repid
            is_layout                = g_layout
            it_fieldcat              = gt_fieldcat[]
            it_sort                  = gt_sort[]
            i_callback_pf_status_set = g_status
            i_callback_user_command  = g_user_command
            i_grid_title             =  titlebar
            i_save                   = g_save
            is_variant               = g_variant
*            I_SCREEN_START_COLUMN = 1000
*            I_SCREEN_START_LINE = 30
       TABLES
            t_outtab           = it_tab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE e977 WITH 'An Error occured during Grid Dispaly .'.
  ENDIF.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM reset_list.

  MOVE 0 TO sy-lsind.

  w_page = 1.
  w_line = 1.
  w_count = 0.
  PERFORM   p3000_title_write.                  " 해더 출력...
* 레포트 Write
  PERFORM   p3000_data_write          USING   w_err_chk.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM p2000_multi_selection.

  DATA: index   TYPE p,
        zfreqno LIKE ztreqst-zfreqno,
        zfamdno LIKE ztreqst-zfamdno,
        zfrlst1 LIKE ztreqst-zfrlst1,
        zfrlst2 LIKE ztreqst-zfrlst2.

  REFRESH it_selected.
  CLEAR w_selected_lines.

  MOVE : w_list_index    TO index,
         it_tab-zfblno   TO zfreqno.

  DO.
    CLEAR markfield.
    READ LINE sy-index FIELD VALUE markfield.
    IF sy-subrc NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( markfield EQ 'x' ) OR ( markfield EQ 'X' ).
      MOVE : it_tab-zfblno  TO it_selected-zfreqno.
      APPEND it_selected.
      ADD 1 TO w_selected_lines.
    ENDIF.
  ENDDO.

  IF w_selected_lines EQ 0.
    IF index GT 0.
      MOVE : zfreqno TO it_selected-zfreqno.

      APPEND it_selected.
      ADD 1 TO w_selected_lines.
    ELSE.
      MESSAGE s962.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM p2000_page_check.

  IF w_line >= 53.
    WRITE : / sy-uline.
    w_page = w_page + 1.    w_line = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM p3000_last_write.

  IF w_count GT 0.
    FORMAT RESET.
    WRITE : /102 'Total', w_count, 'case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p3000_line_write.
  DATA: w_po(20),
        w_dom_text(20).

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  CONCATENATE it_tab-ebeln '-' it_tab-zfshno INTO w_po.

  "DOMAIN - 유환여부.
  PERFORM  get_dd07t USING 'ZDPOYN' it_tab-zfpoyn
                     CHANGING   w_dom_text.

  WRITE : / sy-vline NO-GAP,
            (18)it_tab-traid      NO-GAP,  sy-vline NO-GAP,
            (20)w_po              NO-ZERO, sy-vline NO-GAP,
                it_tab-zfblno     NO-GAP,  sy-vline NO-GAP,
                it_tab-zfhblno    NO-GAP,  sy-vline NO-GAP,
            (15)it_tab-zfcarnm           , sy-vline NO-GAP,
            (20)w_dom_text       CENTERED, sy-vline NO-GAP,
            (10)it_tab-zfetd     CENTERED, sy-vline NO-GAP,
            (15)it_tab-zfsprt            , sy-vline NO-GAP,
            (15)it_tab-w_ekgrp           , sy-vline NO-GAP,
            (20)it_tab-w_lifnr           , sy-vline NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

* hide
  MOVE sy-tabix  TO w_list_index.
  HIDE: w_list_index, it_tab.
  MODIFY it_tab INDEX sy-tabix.
  FORMAT RESET.
  WRITE : / sy-vline NO-GAP,
            (20) it_tab-zfhblno             , sy-vline NO-GAP,
            (20) it_tab-zfrgdsr             , sy-vline NO-GAP,
            (10) it_tab-zfeta       CENTERED, sy-vline NO-GAP,
            (15) it_tab-zfvia               , sy-vline NO-GAP,
            (15) it_tab-zfaprt              , sy-vline NO-GAP,
            (15) it_tab-w_zfford            , sy-vline NO-GAP,
            (20) it_tab-w_zfbeni            , sy-vline NO-GAP.

* hide...
  MOVE sy-tabix  TO w_list_index.
  HIDE: w_list_index, it_tab.
  MODIFY it_tab INDEX sy-tabix.
  w_count = w_count + 1.

  WRITE : / sy-uline.
ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM p2000_show_lc USING    p_zfreqno.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD p_zfreqno.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

* JSY 주석처리 2003.04.01
* READ ZTIMIMG00.
*  SELECT SINGLE * FROM ZTIMIMG00.
*  IF ZTIMIMG00-BLSTYN EQ 'X'.
  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
*  ELSE.
*     CALL TRANSACTION 'ZIM22' AND SKIP  FIRST SCREEN.
*  ENDIF.

* 구매의뢰 테이블 SELECT
  PERFORM   p1000_get_it_tab          USING   w_err_chk.
  IF w_err_chk EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   p1000_read_text           USING   w_err_chk.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  PERFORM reset_list.
ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM p1000_get_it_tab USING    w_err_chk.

  w_err_chk = 'N'.                " Error Bit Setting
  REFRESH : it_tab1.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_tab1
    FROM  ( ztbl AS h INNER JOIN ztblit AS i
              ON h~zfblno = i~zfblno )
            INNER JOIN ztcivit AS d
              ON i~zfblno = d~zfblno
             AND i~zfblit = d~zfblit
   WHERE h~bukrs   IN   s_bukrs
     AND h~zfeta   IN   s_eta
     AND h~zfwerks IN   s_werks
     AND i~ebeln   IN   s_ebeln
     AND i~matnr   IN   s_matnr
     AND h~zfhblno IN   s_hblno
     AND h~ekgrp   IN   s_ekgrp
     AND h~zfvia   LIKE p_via
     AND h~zfford  IN   s_ford
     AND h~zfpoyn  LIKE p_poyn
     AND h~zfshty  IN   s_shty.

  IF sy-subrc NE 0.               " Not Found?
    w_err_chk = 'Y'.  MESSAGE s966.    EXIT.
  ENDIF.
  LOOP AT it_tab1.
    SELECT SINGLE *
             FROM ztcivhd
            WHERE zfcivno IN s_civno
              AND zfcivrn EQ it_tab1-zfcivrn.
    IF sy-subrc NE 0.
      DELETE it_tab1 INDEX sy-tabix.
    ELSE.
      MOVE ztcivhd-zfcivno TO it_tab1-zfcivno.
      SELECT SINGLE *
               FROM ztiv
              WHERE zfblno = it_tab1-zfblno.
      IF sy-subrc EQ 0.
        IF NOT p_y IS INITIAL.
          MOVE ztiv-zfcust TO it_tab1-zfcust.
          MODIFY it_tab1 INDEX sy-tabix.
        ELSE.
          DELETE it_tab1 INDEX sy-tabix.
        ENDIF.
      ELSE.
        IF NOT p_n IS INITIAL.
          MOVE 'N' TO it_tab1-zfcust.
          MODIFY it_tab1 INDEX sy-tabix.
        ELSE.
          DELETE it_tab1 INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_CONTAINER
*&---------------------------------------------------------------------*
FORM p1000_get_container USING    w_err_chk.
*  LOOP AT IT_TAB1.
*    SELECT *
*      FROM LIKP
*     WHERE BOLNR =  IT_TAB1-ZFCIVNO
*       AND TRAID IN S_TRAID.
*      IF SY-SUBRC EQ 0.
*        SELECT *
*          FROM LIPS
*         WHERE VBELN = LIKP-VBELN
*           AND VGBEL = IT_TAB1-EBELN
*           AND VGPOS = IT_TAB1-EBELP.
*          IF SY-SUBRC EQ 0.
*            MOVE-CORRESPONDING IT_TAB1 TO IT_TAB.
*            MOVE LIKP-BORGR_GRP        TO IT_TAB-BORGR_GRP.
*            MOVE LIKP-VBELN            TO IT_TAB-VBELN.
*            MOVE LIKP-TRAID            TO IT_TAB-TRAID.
*            MOVE LIPS-LFIMG            TO IT_TAB-LFIMG.
*            MOVE LIPS-VRKME            TO IT_TAB-VRKME.
*            MOVE LIPS-KDMAT            TO IT_TAB-KDMAT.
*            APPEND IT_TAB.
*          ENDIF.
*        ENDSELECT.
*      ENDIF.
*    ENDSELECT.
*  ENDLOOP.
*  LOOP AT it_tab.
*    SELECT SINGLE *
*             FROM mseg
*            WHERE ebeln = it_tab-ebeln
*              AND ebelp = it_tab-ebelp.
*    IF sy-subrc EQ 0.
*      IF p_no EQ 'X' AND p_yes IS INITIAL.
*        DELETE it_tab INDEX sy-tabix.
*      ENDIF.
*    ELSE.
*      IF p_yes EQ 'X' AND p_no IS INITIAL.
*        DELETE it_tab INDEX sy-tabix.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

  DATA: BEGIN OF w_lips,
         borgr_grp    LIKE likp-borgr_grp,
         vbeln        LIKE likp-vbeln    ,
         traid        LIKE likp-traid    ,
         lfimg        LIKE lips-lfimg    ,
         vrkme        LIKE lips-vrkme    ,
         kdmat        LIKE lips-kdmat    ,
        END OF w_lips.

  LOOP AT it_tab1.
    SELECT
      likp~borgr_grp	likp~vbeln
      likp~traid    	lips~lfimg
      lips~vrkme    	lips~kdmat
    INTO corresponding fields of w_lips
    FROM lips
        INNER JOIN  likp
           ON lips~vbeln = likp~vbeln
    WHERE bolnr =  it_tab1-zfcivno
       AND traid IN s_traid
       AND vgbel = it_tab1-ebeln
       AND vgpos = it_tab1-ebelp.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_tab1 TO it_tab.
        MOVE w_lips-borgr_grp        TO it_tab-borgr_grp.
        MOVE w_lips-vbeln            TO it_tab-vbeln.
        MOVE w_lips-traid            TO it_tab-traid.
        MOVE w_lips-lfimg            TO it_tab-lfimg.
        MOVE w_lips-vrkme            TO it_tab-vrkme.
        MOVE w_lips-kdmat            TO it_tab-kdmat.
        APPEND it_tab.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

  data: l_idx like sy-tabix,
        l_sum like ekbe-menge.
  LOOP AT it_tab.
    l_idx = sy-tabix.
    SELECT sum( menge ) into l_sum
             FROM ekbe
            WHERE ebeln = it_tab-ebeln
              AND ebelp = it_tab-ebelp
              AND BEWTP = 'E'.
    IF l_sum > 0.
      IF p_no EQ 'X' AND p_yes IS INITIAL.
        DELETE it_tab INDEX l_idx.
      ENDIF.
    ELSE.
      IF p_yes EQ 'X' AND p_no IS INITIAL.
        DELETE it_tab INDEX l_idx.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_tab LINES w_line.
  IF w_line EQ 0.
    w_err_chk = 'Y'.  MESSAGE s966.    EXIT.
  ENDIF.
ENDFORM.                    " P1000_GET_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM get_dd07t USING    p_domname
                               p_field
                      CHANGING p_w_name.
  CLEAR : dd07t, p_w_name.

  IF p_field IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM dd07t WHERE domname     EQ p_domname
                      AND   ddlanguage  EQ sy-langu
                      AND   as4local    EQ 'A'
                      AND   domvalue_l  EQ p_field
                      ORDER BY as4vers DESCENDING.
    EXIT.
  ENDSELECT.

  p_w_name   = dd07t-ddtext.
  TRANSLATE p_w_name TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM p1000_set_bukrs.

  CLEAR : ztimimg00, p_bukrs.
  SELECT SINGLE * FROM ztimimg00.
  IF NOT ztimimg00-zfbufix IS INITIAL.
    MOVE  ztimimg00-zfbukrs   TO  p_bukrs.
  ENDIF.

*>> 회사코드 SET.
  MOVE: 'I'          TO s_bukrs-sign,
        'EQ'         TO s_bukrs-option,
        p_bukrs      TO s_bukrs-low.
  APPEND s_bukrs.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
FORM p2000_alv_pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'ZIM24A'.
ENDFORM.                    " P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM p2000_alv_command USING r_ucomm      LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield.
  DATA: mm03_start_sicht(15) TYPE c  VALUE 'BDEKLPQSVXZA'.
  CASE r_ucomm.
      READ TABLE it_tab INDEX rs_selfield-tabindex.
*>> Display Material Master.
    WHEN 'DSMA'.
      PERFORM p2000_dsma USING it_tab-ebeln it_tab-ebelp.
*>> Display Purchase Order.
    WHEN 'DSPO'.
      PERFORM p2000_dspo USING it_tab-ebeln.
*>> Display Bill of Lading.
    WHEN 'DSBL'.
      PERFORM p2000_dsbl USING it_tab-zfblno.
*>> Display Inbound Delivery.
    WHEN 'DSIB'.
      PERFORM p2000_dsib USING it_tab-vbeln.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
FORM p3000_append_fieldcat.

  CLEAR : w_line.
  CLEAR: gt_fieldcat, gt_sort, pos.

  CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'MATNR'.
  ls_fieldcat-seltext_m      = 'Material No.'.
  ls_fieldcat-outputlen      = 15.
  ls_fieldcat-emphasize      = 'C200'.
  ls_sort-fieldname          = 'MATNR'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'TXZ01'.
  ls_fieldcat-seltext_m      = 'Description'.
  ls_fieldcat-outputlen      = 20.
  ls_fieldcat-emphasize      = 'C200'.
  ls_sort-fieldname          = 'TXZ01'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFHBLNO'.
  ls_fieldcat-seltext_m      = 'House B/L No.'.
  ls_fieldcat-outputlen      = 20.
  ls_fieldcat-emphasize      = 'C200'.
  ls_sort-fieldname          = 'ZFHBLNO'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFETD'.
  ls_fieldcat-seltext_m      = 'E.T.D.'.
  ls_fieldcat-outputlen      = 8.
  ls_fieldcat-emphasize      = 'C200'.
  ls_fieldcat-input          = '1'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFETA'.
  ls_fieldcat-seltext_m      = 'E.T.A.'.
  ls_fieldcat-outputlen      = 8.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFCARNM'.
  ls_fieldcat-seltext_m      = 'Vessel Name'.
  ls_fieldcat-outputlen      = 20.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'W_EKGRP'.
  ls_fieldcat-seltext_m      = 'Purch. Grp'.
  ls_fieldcat-outputlen      = 10.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'EBELN'.
  ls_fieldcat-seltext_m      = 'P/O No.'.
  ls_fieldcat-outputlen      = 10.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'EBELP'.
  ls_fieldcat-seltext_m      = 'Item'.
  ls_fieldcat-outputlen      = 5.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'VBELN'.
  ls_fieldcat-seltext_m      = 'Delivery'.
  ls_fieldcat-outputlen      = 10.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'TRAID'.
  ls_fieldcat-seltext_m      = 'Container No.'.
  ls_fieldcat-outputlen      = 18.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'BORGR_GRP'.
  ls_fieldcat-seltext_m      = 'Seal No.'.
  ls_fieldcat-outputlen      = 15.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'KDMAT'.
  ls_fieldcat-seltext_m      = 'Case No.'.
  ls_fieldcat-outputlen      = 18.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'LFIMG'.
  ls_fieldcat-seltext_m      = 'Quantity'.
  ls_fieldcat-outputlen      = 18.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'VRKME'.
  ls_fieldcat-seltext_m      = 'Unit'.
  ls_fieldcat-outputlen      = 3.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'W_ZFCUST'.
  ls_fieldcat-seltext_m      = 'Clearance Status'.
  ls_fieldcat-outputlen      = 15.
  ls_fieldcat-emphasize      = 'C200'.
  ls_sort-fieldname          = 'W_ZFCUST'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFCIVNO'.
  ls_fieldcat-seltext_m      = 'CI#'.
  ls_fieldcat-outputlen      = 15.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.

    CLEAR: ls_fieldcat, ls_sort.
  pos = pos + 1.
  ls_fieldcat-col_pos        = pos.
  ls_fieldcat-fieldname      = 'ZFCIVRN'.
  ls_fieldcat-seltext_m      = 'Inv#'.
  ls_fieldcat-outputlen      = 15.
  ls_fieldcat-emphasize      = 'C200'.
  APPEND ls_sort     TO gt_sort.
  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  p2000_dsma
*&---------------------------------------------------------------------*
FORM p2000_dsma USING    ebeln ebelp.
  DATA: mm03_start_sicht(15) TYPE c  VALUE 'BDEKLPQSVXZA'.
  SELECT SINGLE *
           FROM ekpo
          WHERE ebeln = ebeln
            AND ebelp = ebelp.
  SET PARAMETER ID 'MAT' FIELD ekpo-matnr.
  SET PARAMETER ID 'BUK' FIELD ekpo-bukrs.
  SET PARAMETER ID 'WRK' FIELD ekpo-werks.
  SET PARAMETER ID 'LAG' FIELD ''.
  SET PARAMETER ID 'MXX' FIELD mm03_start_sicht.
  CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " p2000_dsma
*&---------------------------------------------------------------------*
*&      Form  p2000_DSPO
*&---------------------------------------------------------------------*
FORM p2000_dspo USING    ebeln.
  SELECT SINGLE * FROM ekko
         WHERE    ebeln EQ ebeln.
  IF sy-subrc EQ 0.

    IF ekko-bstyp EQ 'K'.
      SET PARAMETER ID 'CTR' FIELD ebeln.
      CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
    ELSEIF ekko-bstyp EQ 'L'.
      SET PARAMETER ID 'SAG' FIELD ebeln.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BSP' FIELD ''.
      EXPORT 'BSP' TO MEMORY ID 'BSP'.
      SET PARAMETER ID 'BES' FIELD ebeln.
      EXPORT 'BES'  TO MEMORY ID 'BES'.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " p2000_DSPO
*&---------------------------------------------------------------------*
*&      Form  P2000_DSBL
*&---------------------------------------------------------------------*
FORM p2000_dsbl USING    zfblno.

  SET PARAMETER ID 'ZPBLNO'  FIELD zfblno.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DSBL
*&---------------------------------------------------------------------*
*&      Form  P2000_DSIB
*&---------------------------------------------------------------------*
FORM p2000_dsib USING    vbeln.

  SET PARAMETER ID 'VLM' FIELD vbeln.
  CALL TRANSACTION 'VL33N' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DSIB
