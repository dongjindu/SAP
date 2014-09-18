************************************************************************
* Program Name      : ZAHRU011
* Author            : IG.Moon
* Creation Date     : 5/18/2009
* Specifications By : Euna Lee
* Description       : [HR] Quarterly Report of Benefit Plan
* Modifications Log
* Date        Developer   Request ID    Description
*=======================================================================
* 03/31/2011  VALERIAN    UD1K951270    Correct logic to calculate
*                                       dependent's age.
* 03/31/2011  VALERIAN    UD1K951272    If DOB of dependent is blank
*                                       assume the age less than 26
************************************************************************
REPORT zahru011 MESSAGE-ID zmco.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: dyname         TYPE progname,
      dynumb         TYPE sychar04,
      exc_exctab     TYPE slis_t_extab,
      popup_fieldcat TYPE slis_t_fieldcat_alv,
      f              TYPE slis_fieldcat_alv,
      selfield       TYPE slis_selfield,
      exitfield,
      color_active(3)  VALUE 'C50',
      tabix LIKE sy-tabix.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: kcde.

* Tables
TABLES : pa0002, pa0106.
DATA $ix TYPE i.

* For creating the help list on selection screen
DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.
*Group/Div	GRSI	Contract #	Member	DOB	SSN	Sex	Relation

TYPE-POOLS: truxs.

DATA: it_raw TYPE truxs_t_text_data.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA : BEGIN OF i_pa0167 OCCURS 0,
        pernr LIKE pa0167-pernr,
        begda LIKE pa0167-begda,
        endda LIKE pa0167-endda,
        depcv LIKE pa0167-depcv,
        dty01 LIKE pa0167-dty01,
        dty02 LIKE pa0167-dty02,
        dty03 LIKE pa0167-dty03,
        dty04 LIKE pa0167-dty04,
        dty05 LIKE pa0167-dty05,
        dty06 LIKE pa0167-dty06,
        dty07 LIKE pa0167-dty07,
        dty08 LIKE pa0167-dty08,
        dty09 LIKE pa0167-dty09,
        dty10 LIKE pa0167-dty10,
        dty11 LIKE pa0167-dty11,
        dty12 LIKE pa0167-dty12,
        dty13 LIKE pa0167-dty13,
        dty14 LIKE pa0167-dty14,
        dty15 LIKE pa0167-dty15,
        dty16 LIKE pa0167-dty16,
        dty17 LIKE pa0167-dty17,
        dty18 LIKE pa0167-dty18,
        dt119 LIKE pa0167-dty19,
        dty20 LIKE pa0167-dty20,

        did01 LIKE pa0167-did01,
        did02 LIKE pa0167-did02,
        did03 LIKE pa0167-did03,
        did04 LIKE pa0167-did04,
        did05 LIKE pa0167-did05,
        did06 LIKE pa0167-did06,
        did07 LIKE pa0167-did07,
        did08 LIKE pa0167-did08,
        did09 LIKE pa0167-did09,
        did10 LIKE pa0167-did10,
        did11 LIKE pa0167-did11,
        did12 LIKE pa0167-did12,
        did13 LIKE pa0167-did13,
        did14 LIKE pa0167-did14,
        did15 LIKE pa0167-did15,
        did16 LIKE pa0167-did16,
        did17 LIKE pa0167-did17,
        did18 LIKE pa0167-did18,
        did19 LIKE pa0167-did19,
        did20 LIKE pa0167-did20,

        vorna LIKE pa0002-vorna,
        midnm LIKE pa0002-midnm,
        nachn LIKE pa0002-nachn,
        gbdat LIKE pa0002-gbdat,
        perid LIKE pa0002-perid,
        gesch LIKE pa0002-gesch,
        anzkd LIKE pa0002-anzkd,
        stras LIKE pa0006-stras,
        locat LIKE pa0006-locat,
        ort01 LIKE pa0006-ort01,
        state LIKE pa0006-state,
        pstlz LIKE pa0006-pstlz,
        telnr LIKE pa0006-telnr,
        trdat LIKE pa0167-endda,
        pltyp LIKE pa0167-pltyp,
        pardt LIKE pa0167-pardt,
       END OF i_pa0167.

DATA : BEGIN OF i_pa0021 OCCURS 0,
        pernr LIKE pa0021-pernr,
        subty LIKE pa0021-subty,
        objps LIKE pa0021-objps,
        begda LIKE pa0021-begda,
        endda LIKE pa0021-endda,
        fgbdt LIKE pa0021-fgbdt,
        aedtm LIKE pa0021-aedtm,
        fasex LIKE pa0021-fasex,
        favor LIKE pa0021-favor,
        fanam LIKE pa0021-fanam,
        perid LIKE pa0106-perid,
        erbnr LIKE pa0021-erbnr,
        fnmzu LIKE pa0021-fnmzu,
        finit LIKE pa0021-finit,
     END OF i_pa0021.

DATA $i_pa0021 LIKE i_pa0021 OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_row_tab,
        pernr LIKE pa0000-pernr,
        seqno(2) TYPE n,
        subty LIKE pa0021-subty,
        vorna LIKE pa0002-vorna,
        perid LIKE pa0002-perid,
        gbdat LIKE pa0002-gbdat,
        objps LIKE pa0021-objps,
        midnm LIKE pa0002-midnm,
        nachn LIKE pa0002-nachn,
        gesch LIKE pa0002-gesch,
        begda LIKE pa0167-begda,
        endda LIKE pa0167-endda,
        dtyxx LIKE pa0167-dty01,
        depcv LIKE pa0167-depcv,
        anzkd LIKE pa0002-anzkd,
        trdat LIKE pa0167-endda,
        begda_d LIKE pa0021-begda,
        endda_d LIKE pa0021-endda,
        fnmzu LIKE pa0021-fnmzu,
        fgbdt LIKE pa0021-fgbdt,
        pardt LIKE pa0167-pardt,
        stras LIKE pa0006-stras,
        locat LIKE pa0006-locat,
        ort01 LIKE pa0006-ort01,
        state LIKE pa0006-state,
        pstlz LIKE pa0006-pstlz,
        gendr(10),
        relcode(10),
        sort_k(1),
        flag,
      END OF ty_row_tab.

DATA : BEGIN OF i_addr OCCURS 0,
        pernr LIKE pa0167-pernr,
        stras LIKE pa0006-stras,
        locat LIKE pa0006-locat,
        ort01 LIKE pa0006-ort01,
        state LIKE pa0006-state,
        pstlz LIKE pa0006-pstlz,
        endda LIKE pa0006-endda,
       END OF i_addr.

DATA it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA : BEGIN OF i_memory OCCURS 0,
        pernr LIKE pa0000-pernr,
        seqno(2) TYPE n,
        subty LIKE pa0021-subty,
        vorna LIKE pa0002-vorna,
        perid LIKE pa0002-perid,
        gbdat LIKE pa0002-gbdat,
        objps LIKE pa0021-objps,
        midnm LIKE pa0002-midnm,
        nachn LIKE pa0002-nachn,
        begda LIKE pa0167-begda,
        endda LIKE pa0167-endda,
        dtyxx LIKE pa0167-dty01,
        trdat LIKE pa0167-endda,
        gendr(10),
        relcode(10),
       END OF i_memory.

DATA  gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA : BEGIN OF term_emp OCCURS 0,
        pernr LIKE pa0167-pernr,
       END OF term_emp.

DATA : icon_red_scr TYPE icon_d,
       icon_green_scr TYPE icon_d,
       icon_yellow_scr TYPE icon_d,
       icon_doc_scr  TYPE icon_d.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DATA: r_date TYPE datum,
      r_time TYPE uzeit,
      r_user TYPE uname.

************************************************************************
DATA  : info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.

DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK pp WITH FRAME TITLE text-p01.
SELECT-OPTIONS s_pernr FOR pa0002-pernr.
PARAMETERS : p_pltyp LIKE pa0167-pltyp OBLIGATORY.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.

PARAMETERS p_ee AS CHECKBOX.
SELECTION-SCREEN COMMENT 5(13) text-0e5 FOR FIELD p_ee.
PARAMETERS p_ee1 AS CHECKBOX.
SELECTION-SCREEN COMMENT 25(14) text-0e6 FOR FIELD p_ee1.
PARAMETERS p_eef AS CHECKBOX.
SELECTION-SCREEN COMMENT 43(17) text-0e7 FOR FIELD p_eef.
PARAMETERS p_wav AS CHECKBOX.
SELECTION-SCREEN COMMENT 63(6) text-0e8 FOR FIELD p_wav.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK pp.

SELECTION-SCREEN BEGIN OF BLOCK bd WITH FRAME TITLE text-d01.
PARAMETERS: p_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK bd.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-s13.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.
PARAMETERS p_debug AS CHECKBOX.
PARAMETERS p_call NO-DISPLAY.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_pltyp.
  PERFORM popup_pltyp USING p_pltyp.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_ee IS INITIAL AND p_ee1 IS INITIAL
     AND p_eef IS INITIAL AND p_wav IS INITIAL .
    MESSAGE i000 WITH 'Please select dependent coverage.'.
    EXIT.
  ENDIF.

  PERFORM initialize.
  PERFORM get_table.
  PERFORM move_out.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  CHECK g_error EQ false.
  SORT gt_out BY pernr sort_k.

  IF p_call EQ true.
    __cls i_memory.
    LOOP AT gt_out.
      MOVE-CORRESPONDING gt_out TO i_memory.
      APPEND i_memory.
      EXPORT i_memory TO MEMORY ID 'ESS_BENEFIT_INFO'.
    ENDLOOP.
  ELSE.
    PERFORM set_output.
  ENDIF.

*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.


ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<MONTH>  text
*----------------------------------------------------------------------*
FORM check_num CHANGING n_value.
  DATA num(12) VALUE ' 0123456789'.

  IF n_value CN num. n_value = 'delete'. ENDIF.

ENDFORM.                    " CHECK_NUM
*---------------------------------------------------------------------*
*       FORM eliminate_qt                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  N_VALUE                                                       *
*---------------------------------------------------------------------*
FORM eliminate_qt CHANGING n_value.
  REPLACE : '"' WITH '' INTO n_value,
            '"' WITH '' INTO n_value.
  CONDENSE n_value NO-GAPS.

ENDFORM.                    " CHECK_NUM


*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  IF p_call EQ false.
    __process 'Preparing output...' '95'.
  ENDIF.

  __cls gt_out.

  SORT i_addr BY pernr.
  SORT it_status BY pernr.

  LOOP AT it_row_tab.

    READ TABLE it_status WITH KEY pernr = it_row_tab-pernr
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING it_row_tab TO gt_out.
    READ TABLE i_addr WITH KEY pernr = it_row_tab-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-stras = i_addr-stras.
      gt_out-locat = i_addr-locat.
      gt_out-ort01 = i_addr-ort01.
      gt_out-state = i_addr-state.
      gt_out-pstlz = i_addr-pstlz.
    ENDIF.

    IF it_row_tab-gesch EQ '2'.
      gt_out-gendr = 'F'.
    ELSEIF it_row_tab-gesch EQ '1'.
      gt_out-gendr = 'M'.
    ELSE.
      gt_out-gendr = ''.
    ENDIF.

    CASE it_row_tab-dtyxx.
      WHEN 'H'.
        gt_out-relcode = 'Subscriber'. "'H'
        gt_out-sort_k = '1'.
      WHEN '1'.
        gt_out-relcode = 'Spouse'."'S'.
        gt_out-sort_k = '2'.
      WHEN '2' OR '6' OR '9'.                               " OR '13'.
        gt_out-relcode = 'Child'."'C'.
        gt_out-sort_k = '4'.
      WHEN '13'.
        gt_out-relcode = 'Domestic Partner'.
        gt_out-sort_k = '3'.
      WHEN OTHERS.
        gt_out-relcode = 'Others'."'O'.
        gt_out-sort_k = '5'.
    ENDCASE.

*    TRANSLATE gt_out-relcode TO UPPER CASE.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT

*---------------------------------------------------------------------*
*       FORM set_output                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_output.

  PERFORM show_progress     USING 'Preparing screen...' ''.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA $exists(1).
  DATA l_answer(1).

  PERFORM pop_up USING
      'This will create Credit/Debit memos massively.'
      'Do you really want to proceed?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.
ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR : g_error.

ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_FKDAT  text
*      <--P_STR_DATE  text
*----------------------------------------------------------------------*
FORM convert_date  USING    f_date  LIKE sy-datum
                   CHANGING f_dtout TYPE char10.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = f_date
       IMPORTING
            date_external            = f_dtout
       EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  make_msg_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*----------------------------------------------------------------------*
FORM make_msg_string USING    p_msg.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = p_msg.

ENDFORM.                    " MAKE_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon.

*  LOOP AT gt_out.
*    $ix = sy-tabix.
*
*    CASE gt_out-err_bdc.
*      WHEN 'X'.
*        gt_out-icon = icon_led_red.
*      WHEN 'N'.
*        gt_out-icon = icon_led_green.
*      WHEN OTHERS.
*        CLEAR gt_out-icon.
*    ENDCASE.
*
*    CASE gt_out-err_prc.
*      WHEN 'X'.
*        gt_out-icon2 = icon_led_yellow.
*      WHEN OTHERS.
*        CLEAR gt_out-icon2.
*    ENDCASE.
*
*    CASE gt_out-err_body.
*      WHEN 'X'.
*        gt_out-icon2 = icon_led_red.
*      WHEN OTHERS.
*    ENDCASE.
*
*    MODIFY gt_out INDEX $ix TRANSPORTING icon2 icon.
*  ENDLOOP.

ENDFORM.                    " apply_icon

*---------------------------------------------------------------------*
*       FORM value_help                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_J                                                           *
*---------------------------------------------------------------------*
FORM value_help CHANGING p_j.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display       = ' '
       IMPORTING
            index         = p_j
       TABLES
            fields        = help_field
            select_values = help_vtab
            valuetab      = help_value.
ENDFORM.                               " VALUE_HELP

*---------------------------------------------------------------------*
*       FORM double_click                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E_ROW                                                         *
*  -->  E_COLUMN                                                      *
*  -->  ES_ROW_NO                                                     *
*---------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
*  DATA l_index TYPE i.
*
*  l_index = e_row-index.
*
*  READ TABLE gt_out INDEX l_index.
*  IF sy-subrc = 0.
*    IF e_column = 'REF_DOC'.
*      CHECK gt_out-ref_doc NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-ref_doc .
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*
*    ENDIF.
*    IF e_column = 'BILL_DOC'.
*      CHECK gt_out-bill_doc NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-bill_doc.
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF e_column = 'VBELN'.
*      CHECK gt_out-vbeln NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-vbeln.
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*    ENDIF.
*
*  ENDIF.
*
*  CALL METHOD cl_gui_control=>set_focus EXPORTING control = g_grid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  get_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table.

  __cls it_status.
  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE pernr IN s_pernr
    AND begda <= p_date
    AND endda >= p_date
    AND stat2 EQ '0'.

  __cls i_pa0167.

  DATA: cond(72) TYPE c,
        atab LIKE TABLE OF cond.


  IF p_ee EQ true.
    CONCATENATE 'A~DEPCV = ''' 'EE' '''' INTO cond.
    APPEND cond TO atab.
  ENDIF.

  IF p_ee1 EQ true.
    CONCATENATE 'OR A~DEPCV = ''' 'EE+1' '''' INTO cond.
    APPEND cond TO atab.
  ENDIF.

  IF p_eef EQ true.
    CONCATENATE 'OR A~DEPCV = ''' 'EE+F' '''' INTO cond.
    APPEND cond TO atab.
  ENDIF.

  IF p_wav EQ true.
    CONCATENATE 'OR A~DEPCV = ''' 'WAIV' '''' INTO cond.
    APPEND cond TO atab.
  ENDIF.

  READ TABLE atab INTO cond INDEX 1.
  IF sy-subrc EQ 0 AND cond(2) EQ 'OR'.
    SHIFT cond LEFT BY 2 PLACES.
    MODIFY atab INDEX 1 FROM cond.
  ENDIF.

  __u_break.

  SELECT a~pernr a~begda a~endda

         a~dty01 a~dty02 a~dty03
         a~dty04 a~dty05 a~dty06
         a~dty07 a~dty08 a~dty09
         a~dty10 a~dty11 a~dty12
         a~dty13 a~dty14 a~dty15
         a~dty16 a~dty17 a~dty18
         a~dty19 a~dty20

         a~did01 a~did02 a~did03
         a~did04 a~did05 a~did06
         a~did07 a~did08 a~did09
         a~did10 a~did11 a~did12
         a~did13 a~did14 a~did15
         a~did16 a~did17 a~did18
         a~did19 a~did20

         a~depcv
         b~vorna b~midnm b~nachn
         b~gbdat b~perid b~gesch
         b~anzkd
         a~pltyp a~pardt a~depcv

    INTO CORRESPONDING FIELDS OF TABLE i_pa0167
          FROM pa0167 AS a
         INNER JOIN pa0002 AS b
            ON b~pernr EQ a~pernr
           AND b~endda EQ '99991231'
         WHERE a~pernr IN s_pernr
           AND a~begda <= p_date
           AND a~endda >= p_date
           AND a~pltyp =  p_pltyp
           AND (atab).

  SORT i_pa0167 BY  pernr.

  IF NOT i_pa0167[] IS INITIAL.
    __cls i_addr.

    SELECT pernr stras locat ort01 state pstlz endda
      INTO CORRESPONDING FIELDS OF TABLE i_addr
           FROM pa0006
          FOR ALL ENTRIES IN i_pa0167
           WHERE pernr EQ i_pa0167-pernr
             AND subty = '1'
             AND endda = '99991231'.

  ENDIF.


  __cls i_pa0021.
  SELECT a~pernr a~subty a~objps
         a~begda a~endda a~fgbdt
         a~aedtm a~fasex
         a~favor a~fanam a~erbnr a~fnmzu a~finit
         b~perid
  INTO CORRESPONDING FIELDS OF TABLE i_pa0021
    FROM pa0021 AS a
    INNER JOIN pa0106 AS b
       ON b~pernr EQ a~pernr
      AND b~subty EQ a~subty
      AND b~objps EQ a~objps
      AND b~sprps EQ a~sprps
      AND b~endda EQ '99991231'
      AND b~begda EQ a~begda
      AND b~seqnr EQ a~seqnr
      WHERE a~pernr IN s_pernr
        AND a~begda <= sy-datum
        AND a~endda >= sy-datum
        AND a~subty NE '7'.

  SORT i_pa0021 BY pernr subty objps begda.

  __cls $i_pa0021.

  SELECT a~pernr a~subty a~objps
         a~begda a~endda a~fgbdt
         a~aedtm a~fasex
         a~favor a~fanam a~erbnr a~fnmzu a~finit
         b~perid
  INTO CORRESPONDING FIELDS OF TABLE $i_pa0021
    FROM pa0021 AS a
    INNER JOIN pa0106 AS b
       ON b~pernr EQ a~pernr
      AND b~subty EQ a~subty
      AND b~objps EQ a~objps
      AND b~sprps EQ a~sprps
      AND b~endda EQ '99991231'
      AND b~seqnr EQ a~seqnr
      WHERE a~pernr IN s_pernr
        AND a~subty NE '7'.

  SORT $i_pa0021 BY pernr subty objps begda .
  DELETE ADJACENT DUPLICATES FROM $i_pa0021 COMPARING pernr subty objps.
  SORT $i_pa0021 BY pernr subty objps.

*


  __cls it_row_tab.
  PERFORM get_itab_new TABLES i_pa0021
                             $i_pa0021
                              i_pa0167
                              it_row_tab.

  LOOP AT it_row_tab.
    TRANSLATE it_row_tab-vorna TO UPPER CASE.
    TRANSLATE it_row_tab-midnm TO UPPER CASE.
    TRANSLATE it_row_tab-nachn TO UPPER CASE.
    MODIFY it_row_tab INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " get_table
*---------------------------------------------------------------------*
*       FORM get_itab_new                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_I_PA0021                                                    *
*  -->  P_I_PA0167                                                    *
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM get_itab_new TABLES p_i_pa0021 STRUCTURE i_pa0021
                         p_$i_pa0021 STRUCTURE i_pa0021
                         p_i_pa0167 STRUCTURE i_pa0167
                         p_itab STRUCTURE it_row_tab.

  DATA : dtyxx TYPE ben_deptyp,
         didxx TYPE ben_depid,
         seqno(2) TYPE n,
         $fr TYPE i.

  DATA tmp_itab LIKE it_row_tab OCCURS 10 WITH HEADER LINE.
* DATA age LIKE p_date.                                    "UD1K951270
  DATA age TYPE p0347-scryy.                                "UD1K951270
  LOOP AT p_i_pa0167 .

    __cls tmp_itab.

    MOVE-CORRESPONDING p_i_pa0167 TO tmp_itab.

    CLEAR seqno.
    tmp_itab-dtyxx = 'H'.
    tmp_itab-flag = true.
    APPEND tmp_itab.
    CLEAR tmp_itab.
    READ TABLE p_i_pa0021 WITH KEY pernr = p_i_pa0167-pernr
                                   BINARY SEARCH.
    $fr = sy-tabix.
    IF sy-subrc NE 0.
      APPEND LINES OF tmp_itab TO p_itab.
      CONTINUE.
    ENDIF.

    LOOP AT  p_i_pa0021 FROM $fr.
      IF p_i_pa0021-pernr NE p_i_pa0167-pernr.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING p_i_pa0167 TO tmp_itab.

      READ TABLE p_$i_pa0021 WITH KEY pernr = p_i_pa0021-pernr
                                      subty = p_i_pa0021-subty
                                      objps = p_i_pa0021-objps
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        tmp_itab-pardt = p_$i_pa0021-begda.
      ENDIF.

      tmp_itab-pernr = p_i_pa0021-pernr.
      tmp_itab-seqno = p_i_pa0021-erbnr(2).
      tmp_itab-subty = p_i_pa0021-subty.
      tmp_itab-objps = p_i_pa0021-objps.
      tmp_itab-begda = p_i_pa0021-begda.
      tmp_itab-endda = p_i_pa0021-endda.
      tmp_itab-vorna = p_i_pa0021-favor.
      tmp_itab-midnm = space.
      tmp_itab-nachn = p_i_pa0021-fanam.
      tmp_itab-gbdat = p_i_pa0021-fgbdt.
      tmp_itab-dtyxx = p_i_pa0021-subty.
      tmp_itab-gesch = p_i_pa0021-fasex.
      tmp_itab-begda_d = p_i_pa0021-begda.
      tmp_itab-endda_d = p_i_pa0021-endda.
      tmp_itab-perid = p_i_pa0021-perid.
      tmp_itab-fnmzu = p_i_pa0021-fnmzu.
      tmp_itab-fgbdt = p_i_pa0021-fgbdt.
      tmp_itab-midnm = p_i_pa0021-finit.
      CLEAR tmp_itab-anzkd.
      APPEND tmp_itab.
    ENDLOOP.

    SORT tmp_itab BY begda subty objps.

    SORT tmp_itab BY subty objps.

    DO 20 TIMES VARYING dtyxx FROM p_i_pa0167-dty01
                              NEXT p_i_pa0167-dty02
                VARYING didxx FROM p_i_pa0167-did01
                              NEXT p_i_pa0167-did02.
      IF dtyxx IS INITIAL.
        EXIT.
      ELSE.

        READ TABLE p_i_pa0021 WITH KEY pernr = p_i_pa0167-pernr
                                       subty = dtyxx
                                       objps = didxx
                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE tmp_itab WITH KEY subty = dtyxx
                                       objps = didxx
                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            tmp_itab-flag = true.
            tmp_itab-begda = p_i_pa0167-begda.
            tmp_itab-endda = p_i_pa0167-endda.
           MODIFY tmp_itab INDEX sy-tabix TRANSPORTING flag begda endda.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    DELETE tmp_itab WHERE flag NE true.
    LOOP AT tmp_itab.
      $ix = sy-tabix.
      IF tmp_itab-subty EQ '2' OR tmp_itab-subty EQ '6' OR
         tmp_itab-subty EQ '9'. " OR tmp_itab-subty EQ '13' .
*        age = p_date - tmp_itab-fgbdt.                    "UD1K951270
*        IF AGE >= '00200000'.
* BEGIN OF UD1K951270
*       IF age >= '00260000'. " 6/11/2010 by ig.moon

        CLEAR age.                                          "UD1K951272

        IF NOT tmp_itab-fgbdt IS INITIAL.                   "UD1K951272
          CALL FUNCTION 'HR_HK_DIFF_BT_2_DATES'
               EXPORTING
                    date1         = p_date
                    date2         = tmp_itab-fgbdt
                    output_format = '03'
               IMPORTING
                    years         = age.
        ENDIF.                                              "UD1K951272

        IF age >= 26.
* END OF UD1K951270
          SELECT SINGLE * FROM pa0106
           WHERE pernr EQ tmp_itab-pernr
             AND subty EQ tmp_itab-subty
             AND objps EQ tmp_itab-objps
             AND sprps EQ space
             AND endda EQ '99991231'
*             AND ( BEN03 EQ 'X' OR DISAB EQ 'X' ).
             AND ( disab EQ 'X' ). " 6/11/2010 by ig.moon

          IF sy-subrc NE 0.
            DELETE tmp_itab INDEX $ix.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

    APPEND LINES OF tmp_itab TO p_itab.
  ENDLOOP.

ENDFORM.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  get_full_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$ITAB_NACHN  text
*      -->P_$ITAB_VORNA  text
*      <--P_GT_OUT_MEMBER  text
*----------------------------------------------------------------------*
FORM get_full_name USING    p_lastname
                            p_firstname
                   CHANGING p_member.

  CONCATENATE p_lastname ',^' p_firstname INTO p_member.
  TRANSLATE p_member TO UPPER CASE.
  REPLACE '^' WITH ' ' INTO p_member.
  CONDENSE p_member.

ENDFORM.                    " get_full_name
*&---------------------------------------------------------------------*
*&      Form  check_ssn_pa002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EXCEL_SSN  text
*----------------------------------------------------------------------*
FORM check_ssn_pa002 USING  p_ssn.
  SELECT SINGLE pernr INTO gt_out-pernr
  FROM pa0002
  WHERE perid EQ p_ssn.

*  if P_SSN+0(2) eq '99'.
*    gt_out-err = 'D'.
*  endif.

ENDFORM.                    " check_ssn_pa002
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'PERNR'       'TM ID'                8  'CHAR' '' '',
    'X'  'PERID'       'SSN'                 20  'CHAR' '' '',
    'X'  'VORNA'       'First Name'          30  'CHAR' '' '',
    'X'  'MIDNM'       'Init.'                5  'CHAR' '' '',
    'X'  'NACHN'       'Last Name'           30  'CHAR' '' '',
    ' '  'GBDAT'       'DOB'                  8  'DATS' '' '',
    ' '  'GENDR'       'Gender'               1  'CHAR' '' '',
    ' '  'STRAS'       'Addr1'               30  'CHAR' '' '',
    ' '  'LOCAT'       'Addr2'               30  'CHAR' '' '',
    ' '  'ORT01'       'City'                30  'CHAR' '' '',
    ' '  'STATE'       'State'               20  'CHAR' '' '',
    ' '  'PSTLZ'       'Zip'                 20  'CHAR' '' '',
    ' '  'PARDT'       'Eff.DT'               8  'DATS' '' '',
    ' '  'DEPCV'       'Dep.Cov.'             4  'CHAR' '' '',
    ' '  'RELCODE'     'Relation'            10  'CHAR' '' ''.
*    ' '  'SORT_K'      'S'                    1  'CHAR' '' ''.

  PERFORM change_fieldcat USING ft_fieldcat[] .


ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

*Subscriber > Spouse > Child > Others

  sort_tab :
             'PERNR' ' ' 'X' 'X' 'X'.
*             'PERID' ' ' 'X' 'X' 'X'.
*             'SORT_K' ' ' 'X' 'X' 'X'.
*             'VORNA' ' ' 'X' 'X' 'X',
*             'MIDNM' ' ' 'X' 'X' 'X',
*             'NACHN' ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*---------------------------------------------------------------------*
*       FORM pf_status_set                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'." excluding ft_extab.
ENDFORM.                    "PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

*  CASE fp_ucomm.                                          "UD1K951270
*  ENDCASE.                                                "UD1K951270

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA : $g_total(20),
         $g_avail(20),
         $g_unavail(20),
         $g_active(20),
         $g_inactive(20),
         $g_etc(20).

  REFRESH gt_listheader.

  l_text = '[HR] Quarterly Report of Benefit Plan'.
  PERFORM set_header_line USING :
          'P' 'H' '' l_text '',
          'D' 'S' 'Key Date:' p_date ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  popup_pltyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PLTYP  text
*----------------------------------------------------------------------*
FORM popup_pltyp USING p_fieldname.

  DATA: BEGIN OF lt_pltyp OCCURS 0,
           pltyp LIKE t5uc1-pltyp,
           ltext LIKE t5uc1-ltext,
         END OF lt_pltyp.

  DATA: BEGIN OF fields_tab OCCURS 1,
            pltyp LIKE t5uc1-pltyp,
            ltext LIKE t5uc1-ltext,
            color(3),
         END OF fields_tab.

  CLEAR: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  REFRESH: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  APPEND dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname             = dyname
            dynumb             = dynumb
            translate_to_upper = 'X'
       TABLES
            dynpfields         = dynpfields
       EXCEPTIONS
            OTHERS             = 9.

  __cls lt_pltyp.

  SELECT a~pltyp b~ltext INTO TABLE lt_pltyp
  FROM t5ub1 AS a INNER JOIN t5uc1 AS b
  ON b~pltyp EQ a~pltyp
  AND b~barea EQ a~barea
  AND b~langu EQ sy-langu
  WHERE a~barea EQ '10'
    AND a~bpcat EQ 'A'.

  SORT lt_pltyp BY pltyp.

  f-reptext_ddic  = 'Type'.
  f-fieldname = 'PLTYP'.
  f-outputlen = 4.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic = 'Text'.
  f-fieldname = 'LTEXT'.
  DESCRIBE FIELD fields_tab-ltext LENGTH f-outputlen.
  APPEND f TO popup_fieldcat.

  tabix = sy-tabix.

  LOOP AT lt_pltyp.
    fields_tab-pltyp = lt_pltyp-pltyp.
    fields_tab-ltext = lt_pltyp-ltext.
    APPEND fields_tab.
    CLEAR fields_tab.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            i_linemark_fieldname    = 'COLOR'
            i_tabname               = 'FIELDS_TAB'
            it_fieldcat             = popup_fieldcat
            i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
            i_callback_program      = dyname
            it_excluding            = exc_exctab
       IMPORTING
            es_selfield             = selfield
            e_exit                  = exitfield
       TABLES
            t_outtab                = fields_tab.

  READ TABLE fields_tab INDEX tabix.
  MODIFY fields_tab INDEX tabix.

  IF exitfield IS INITIAL.
    READ TABLE fields_tab INDEX selfield-tabindex.
    p_pltyp = fields_tab-pltyp.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-pltyp.
    APPEND dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              dyname     = dyname
              dynumb     = dynumb
         TABLES
              dynpfields = dynpfields.

  ENDIF.


ENDFORM.                    " popup_pltyp
*&---------------------------------------------------------------------*
*&      Form  change_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.

    IF gs_fieldcat-fieldname EQ 'RELCODE'.
      gs_fieldcat-lowercase = 'X'.
      MODIFY pt_fieldcat FROM gs_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
