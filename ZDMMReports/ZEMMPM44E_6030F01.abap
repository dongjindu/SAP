*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM44E_6030F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*/ Select
  DATA: lt_ztmm_6030_01 LIKE it_ztmm_6030_01.
  IF rb1 = 'X'.     "KD
*1. By IV Date
    PERFORM select_by_ivdate CHANGING lt_ztmm_6030_01.
  ELSEIF rb2 = 'X'. "LP
*2. By GR Date
    PERFORM select_by_grdate CHANGING lt_ztmm_6030_01.
  ENDIF.

*/
  LOOP AT lt_ztmm_6030_01 ASSIGNING <fs_ztmm_6030_01>.
*Old Price
    <fs_ztmm_6030_01>-dmbtrbymenge =
     <fs_ztmm_6030_01>-dmbtr / <fs_ztmm_6030_01>-menge.
*Retroactive Price
    <fs_ztmm_6030_01>-retroactive = p_retro.

*Difference Price
    <fs_ztmm_6030_01>-diffprice =
     <fs_ztmm_6030_01>-retroactive - <fs_ztmm_6030_01>-dmbtrbymenge.

*Difference Amount
    <fs_ztmm_6030_01>-diffamount =
     <fs_ztmm_6030_01>-diffprice * <fs_ztmm_6030_01>-menge.
  ENDLOOP.

*/
  it_ztmm_6030_01 = lt_ztmm_6030_01.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ZTMM_6030_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  dsp_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dsp_data.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " dsp_data
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_it_fcat  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES p_it_fcat STRUCTURE it_fcat.
* Build the fieldcat according to DDIC structure ZSMM_6002_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZSMM_6030_01'
       CHANGING
            ct_fieldcat      = p_it_fcat[].

* Make Column header
  LOOP AT p_it_fcat.
    IF p_it_fcat-fieldname = 'BUDAT'.
      p_it_fcat-coltext = 'GR Date'.
    ELSEIF p_it_fcat-fieldname = 'MENGE'.
      p_it_fcat-coltext = 'IV Qty'.
      p_it_fcat-do_sum  = 'X'.
    ELSEIF p_it_fcat-fieldname = 'DMBTR'.
      p_it_fcat-no_out = 'X'.
    ELSEIF p_it_fcat-fieldname = 'DMBTRBYMENGE'.
      p_it_fcat-coltext = 'Old Price'.
      p_it_fcat-do_sum  = 'X'.
    ELSEIF p_it_fcat-fieldname = 'RETROACTIVE'.
      p_it_fcat-coltext = 'Retoactive Price'.
      p_it_fcat-do_sum  = 'X'.
    ELSEIF p_it_fcat-fieldname = 'DIFFPRICE'.
      p_it_fcat-coltext = 'Difference Price'.
      p_it_fcat-do_sum  = 'X'.
    ELSEIF p_it_fcat-fieldname = 'DIFFAMOUNT'.
      p_it_fcat-coltext = 'Difference Amount'.
      p_it_fcat-do_sum  = 'X'.
    ELSEIF p_it_fcat-fieldname = 'EKKO_WAERS'.
      p_it_fcat-coltext = 'PO Currency'.
    ELSEIF p_it_fcat-fieldname = 'MARK'.
      p_it_fcat-no_out = 'X'.
*    ELSEIF p_it_fcat-fieldname = 'MATNR'.
*      p_it_fcat-no_out = 'X'.
*      p_it_fcat-outputlen = 18.
*    ELSEIF p_it_fcat-fieldname = 'DESC_ZCH_DESC'.
*      p_it_fcat-checkbox = space.
    ENDIF.
    MODIFY p_it_fcat.
  ENDLOOP.
ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  make_it_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_sort.
* Make sort fields
  CLEAR: wa_sort, it_sort.
  wa_sort-spos      = '01'.
  wa_sort-fieldname = 'EBELN'.
  wa_sort-up        = 'X'.
*wa_sort-DOWN
*wa_sort-GROUP
*  wa_sort-subtot = 'X'.
*wa_sort-COMP
*wa_sort-EXPA
*wa_sort-SELTEXT
*wa_sort-OBLIGATORY
*wa_sort-LEVEL
*wa_sort-NO_OUT
  APPEND wa_sort TO it_sort.
ENDFORM.                    " make_it_sort
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW  text
*----------------------------------------------------------------------*
FORM get_selected_rows
             USING ext_row LIKE it_row.
  CLEAR: ext_row.
  DATA: lv_pgm(40). "Program Name
*/ get selected rows
  CALL METHOD crv_gui_alv_grid->get_selected_rows
           IMPORTING et_index_rows = ext_row.
* Transfer the methods to the frontend using command FLUSH .
  CALL METHOD cl_gui_cfw=>flush.
  IF sy-subrc NE 0.
* add your handling, for example
    lv_pgm = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = lv_pgm
              txt2  = sy-subrc
              txt1  = 'Error in Flush'(500).
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  get_itable_selected
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTMM_6030_01  text
*      <--P_LT_ZTMM_6030_01  text
*----------------------------------------------------------------------*
FORM get_itable_selected
             USING    imt_row    LIKE it_row
                      imt_itable TYPE STANDARD TABLE
             CHANGING ext_itable TYPE STANDARD TABLE.
  CLEAR: ext_itable.
*/
  DATA: drv_iline  TYPE REF TO data.
  DATA: drv_eline  TYPE REF TO data.

  FIELD-SYMBOLS: <lf_s_row> LIKE LINE OF imt_row.
  FIELD-SYMBOLS: <lf_iline> TYPE ANY.
  FIELD-SYMBOLS: <lf_eline> TYPE ANY.


*/ Get line type dynamically and attach the field-symbol
  CREATE DATA drv_iline LIKE LINE OF imt_itable.
  ASSIGN drv_iline->* TO <lf_iline>.

  CREATE DATA drv_eline LIKE LINE OF ext_itable.
  ASSIGN drv_eline->* TO <lf_eline>.

*/
  LOOP AT imt_row ASSIGNING <lf_s_row>.
    READ TABLE imt_itable ASSIGNING <lf_iline>
                         INDEX     <lf_s_row>-index.
    APPEND <lf_iline> TO ext_itable.
  ENDLOOP.
ENDFORM.                    " get_itable_selected
*&---------------------------------------------------------------------*
*&      Form  dsp_detailed_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dsp_detailed_view
              USING im_structure_name
                    imt_itable TYPE STANDARD TABLE.

  DATA: lv_repid(40). lv_repid = sy-repid.
*  DATA: wa_events TYPE slis_t_event.
  FIELD-SYMBOLS: <lf_fcat> LIKE it_fcat.

  LOOP AT it_fcat ASSIGNING <lf_fcat>.
    CLEAR: wa_fcat_list.
    MOVE-CORRESPONDING <lf_fcat> TO wa_fcat_list.
* set fields
    PERFORM modify_wa_fcat_list.
    APPEND wa_fcat_list TO it_fcat_list.
  ENDLOOP.
* set event
  CLEAR: it_slis_t_event.
  PERFORM eventtab_build USING it_slis_t_event.
* set list heading
  CLEAR: it_slis_t_listheader.
  PERFORM comment_build  USING it_slis_t_listheader.

* Set Sort
  PERFORM set_sort USING it_fm_sort.

*/
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = lv_repid
     i_callback_pf_status_set       = 'PS_TB_LIST'
*     i_callback_user_command        = 'USER_COMMAND'
*     i_structure_name               = im_structure_name
"You can ALV w/o Structure. At this time it_fieldcat[] have to be used.
*   IS_LAYOUT                      =
     it_fieldcat                    = it_fcat_list
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
   it_sort                        = it_fm_sort
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
   i_save                         = 'A'
*   IS_VARIANT                     =
     it_events                      = it_slis_t_event
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = imt_itable
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " dsp_detailed_view
*&---------------------------------------------------------------------*
*&      Form  ps_tb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ps_tb.
* Instanciate PF-STATUS & TITLEBAR.
  DATA: lv_numbering       TYPE i.
  DATA: lv_numbering_c(10) TYPE c.
  DESCRIBE TABLE it_ztmm_6030_01 LINES lv_numbering.
  lv_numbering_c = lv_numbering.

  CONCATENATE 'EMMPM44 Revaluation'
              ': Select Entries'
              lv_numbering_c
    INTO w_title
    SEPARATED BY space.

  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'                "PF-STATUS
              im_it_func = it_func             "Excluding func
              im_tb      = 'TB'                "TITLEBAR
              im_title   = w_title.            "TITLE
  CLEAR it_func.

* Dynamic Function Code Text
*  dynftext  = 'Detailed Selected View'.
ENDFORM.                    " ps_tb
*&---------------------------------------------------------------------*
*&      Form  set_wa_layo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_wa_layo.
*Set a titlebar for the grid control
  DATA: lv_numbering       TYPE i.
  DATA: lv_numbering_c(10) TYPE c.
  DESCRIBE TABLE it_ztmm_6030_01 LINES lv_numbering.
  lv_numbering_c = lv_numbering.
  CONCATENATE 'Revaluation List'
              ': Select Entries'
              lv_numbering_c
    INTO wa_layo-grid_title
    SEPARATED BY space.
*Set Row Selection Mode
  wa_layo-sel_mode   = 'A'.     "Activate Row Selection
ENDFORM.                    " set_wa_layo
*&---------------------------------------------------------------------*
*&      Form  get_desc_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desc_matnr USING    value(p_langu)
                         value(p_matnr) LIKE mara-matnr
                CHANGING value(p_maktx) LIKE makt-maktx.
  CLEAR: p_maktx.
  SELECT SINGLE maktx INTO p_maktx
    FROM  makt
    WHERE spras = p_langu AND
          matnr = p_matnr.
ENDFORM.                    "desc_matnr
*&---------------------------------------------------------------------*
*&      Form  desc_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LIFNR  text
*      <--P_IO_desc_lifnr  text
*----------------------------------------------------------------------*
FORM desc_lifnr USING    value(im_lifnr)
                CHANGING value(ex_desc_lifnr).
  CLEAR: ex_desc_lifnr.
  SELECT SINGLE name1
    INTO ex_desc_lifnr
    FROM lfa1
    WHERE lifnr = im_lifnr.
ENDFORM.                    " desc_lifnr
*&---------------------------------------------------------------------*
*&      Form  detailed_selected_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detailed_selected_view.
*/ Get Selected rows
  PERFORM get_selected_rows
                   USING it_row.
*/ Get  lt_ztmm_6030_01_selected
  DATA: lt_ztmm_6030_01 LIKE it_ztmm_6030_01.
  PERFORM get_itable_selected
                   USING    it_row
                            it_ztmm_6030_01
                   CHANGING lt_ztmm_6030_01.
* Set column header
  CLEAR: it_fcat[].
  PERFORM mask_columns TABLES it_fcat.
* Detailed Selected View Display
  PERFORM dsp_detailed_view
               USING 'ZSMM_6030_01'
                     lt_ztmm_6030_01.
ENDFORM.                    " detailed_selected_view
*&---------------------------------------------------------------------*
*&      Form  modify_wa_fcat_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_wa_fcat_list  text
*----------------------------------------------------------------------*
FORM modify_wa_fcat_list.
  IF wa_fcat_list-fieldname = 'EBELN'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s =  'Purch.doc.'.
  ELSEIF wa_fcat_list-fieldname = 'EBELP'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Item'.
  ELSEIF wa_fcat_list-fieldname = 'BUDAT'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'GR Date'.
  ELSEIF wa_fcat_list-fieldname = 'MBLNR'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Mat. doc.'.
  ELSEIF wa_fcat_list-fieldname = 'MEINS'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'OUn'.
  ELSEIF wa_fcat_list-fieldname = 'MENGE'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'IV Qty'.
  ELSEIF wa_fcat_list-fieldname = 'T001_WAERS'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Curr.'.
*  ELSEIF wa_fcat_list-fieldname = 'DMBTR'.
*    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
*    wa_fcat_list-seltext_s = '  '.
  ELSEIF wa_fcat_list-fieldname = 'DMBTRBYMENGE'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Old Price'.
  ELSEIF wa_fcat_list-fieldname = 'RETROACTIVE'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Retro. Price'.
  ELSEIF wa_fcat_list-fieldname = 'DIFFPRICE'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Diff. Price'.
  ELSEIF wa_fcat_list-fieldname = 'DIFFAMOUNT'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'Diff. Amt.'.
  ELSEIF wa_fcat_list-fieldname = 'EKKO_WAERS'.
    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
    wa_fcat_list-seltext_s = 'PO Curr.'.
*  ELSEIF wa_fcat_list-fieldname = 'MARK'.
*    wa_fcat_list-seltext_l = wa_fcat_list-seltext_m =
*    wa_fcat_list-seltext_s = 'GR Date'.
  ENDIF.

ENDFORM.                    " modify_wa_fcat_list
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SLIS_T_EVENT  text
*----------------------------------------------------------------------*
FORM eventtab_build
                   USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SLIS_T_LISTHEADER  text
*----------------------------------------------------------------------*
FORM comment_build
               USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader.
  DATA: lw_info  TYPE slis_entry,
        lw_name1 LIKE t001w-name1,
        lw_maktx LIKE makt-maktx,
        lw_ekotx LIKE t024e-ekotx,
        lw_eknam LIKE t024-eknam.
* title
  CLEAR ls_line.
  ls_line-typ  = 'H'. " H = Header, S = Selection, A = Action
  ls_line-info = 'Detailed Selected View'.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT_BUILD

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = it_slis_t_listheader.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ps_tb_LIST                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ps_tb_list USING rt_extab TYPE slis_t_extab.
  CLEAR: rt_extab[], rt_extab.

* Instanciate PF-STATUS & TITLEBAR.
  w_title = 'Detailed Selected View'.
  SET TITLEBAR 'TB' WITH w_title.
  SET PF-STATUS 'STANDARD' OF PROGRAM 'SAPLSALV'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  select_by_grdate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_by_grdate
             CHANGING ext_itable TYPE STANDARD TABLE.
  CLEAR: ext_itable.

  SELECT ekpo~ebeln
         ekpo~ebelp
         mkpf~budat "Posting date in the document (GR Doc)
         mkpf~mblnr "Number of material document  (GR Date)
         ekpo~meins
         ekbe~menge "IV Qty
         t001~waers AS t001_waers "Company Code Currency Key
         ekbe~dmbtr               "Amount in local currency
         ekko~waers AS ekko_waers "PO Currency Key
    INTO CORRESPONDING FIELDS OF TABLE ext_itable
    FROM ekpo   "Purchasing Document Item
      INNER JOIN t001  "Company Code
      ON t001~bukrs = ekpo~bukrs  "Company code
      INNER JOIN ekko  "Purchasing Document Header
      ON ekko~ebeln = ekpo~ebeln  "PO No.
      INNER JOIN ekbe  "History per Purchasing Document
      ON ekbe~ebeln = ekpo~ebeln AND
         ekbe~ebelp = ekpo~ebelp AND
         ekbe~bewtp = 'Q'   "PO history category ('Q'=I/V)
      INNER JOIN mkpf   "Header: Material Document
      ON
*Specific for GR Date
         mkpf~mblnr = ekbe~lfbnr AND
         "Document number of a reference document

         mkpf~mjahr = ekbe~gjahr     "Material doc. year
    WHERE ekpo~matnr =  p_matnr AND
          ekko~lifnr =  p_lifnr AND
          mkpf~budat IN s_budat.    "GR Date
ENDFORM.                    " select_by_grdate
*&---------------------------------------------------------------------*
*&      Form  select_by_ivdate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_ZTMM_6030_01  text
*----------------------------------------------------------------------*
FORM   select_by_ivdate
             CHANGING ext_itable TYPE STANDARD TABLE.
  CLEAR: ext_itable.
  SELECT ekpo~ebeln
         ekpo~ebelp
         bkpf~belnr AS mblnr "Accounting document number
         bkpf~budat "Posting date in the document
*         mkpf~budat "Posting date in the document (GR Doc)
*         mkpf~mblnr "Number of material document  (GR Date)
         ekpo~meins
         ekbe~menge "IV Qty
         t001~waers AS t001_waers "Company Code Currency Key
         ekbe~dmbtr               "Amount in local currency
         ekko~waers AS ekko_waers "PO Currency Key
    INTO CORRESPONDING FIELDS OF TABLE ext_itable
    FROM ekpo   "Purchasing Document Item
      INNER JOIN t001  "Company Code
      ON t001~bukrs = ekpo~bukrs  "Company code
      INNER JOIN ekko  "Purchasing Document Header
      ON ekko~ebeln = ekpo~ebeln  "PO No.
      INNER JOIN ekbe  "History per Purchasing Document
      ON ekbe~ebeln = ekpo~ebeln AND
         ekbe~ebelp = ekpo~ebelp AND
         ekbe~bewtp = 'Q'        AND "PO history category ('Q'=I/V)
*/Begin of Added by Hakchin(20040408)
         ekbe~lfbnr = space " Reference Doc
*/End of Added by Hakchin(20040408)
      INNER JOIN bkpf   "Accounting Document Header
      ON
*Specific for IV Date
         bkpf~belnr = ekbe~belnr AND "Accounting document number
         bkpf~gjahr = ekbe~gjahr     "Fiscal year
    WHERE ekpo~matnr =  p_matnr AND
          ekko~lifnr =  p_lifnr AND
          bkpf~budat IN s_ivdat.    "IV Date
ENDFORM.                    " select_by_ivdate
*&---------------------------------------------------------------------*
*&      Form  set_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FM_SORT  text
*----------------------------------------------------------------------*
FORM set_sort USING    ext_fm_sort LIKE it_fm_sort.
  CLEAR: ext_fm_sort.
  DATA: ls_fm_sort LIKE LINE OF ext_fm_sort.

* LIST SORT SEQENCE
  ls_fm_sort-spos           = 1.
  ls_fm_sort-fieldname      = 'EBELN'.
  ls_fm_sort-tabname        = 'IMT_ITABLE'.
  ls_fm_sort-up             = 'X'.
  APPEND ls_fm_sort TO ext_fm_sort.

  ls_fm_sort-spos           = 2.
  ls_fm_sort-fieldname      = 'EBELP'.
  ls_fm_sort-tabname        = 'IMT_ITABLE'.
  ls_fm_sort-up             = 'X'.
  APPEND ls_fm_sort TO ext_fm_sort.

ENDFORM.                    " set_sort
