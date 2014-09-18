************************************************************************
* Program Name      : ZRPP210R_WIRE_HOUR
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 1.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : ALC Mixture Plan Per Code(ALC).
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zrpp210r_wire_hour      NO STANDARD PAGE HEADING
                                MESSAGE-ID zmpp.

TABLES: ztpp_wire_hour ,  "ALC Code Summary - Short Term Plan Table
        ztpp_status,  "Status ID Mapping Between Legarcy and SAP
        ztpp_common_vals,zvpp_capacity.


INCLUDE <icon>.
INCLUDE <list>.

*****// ALV //**********************************************************
TYPE-POOLS: slis.
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_fc       TYPE slis_t_fieldcat_alv,
      g_fieldcat_s LIKE LINE OF gt_fieldcat,
      gs_layout   TYPE slis_layout_alv,
      gs_print    TYPE slis_print_alv,
      gt_sort     TYPE slis_t_sortinfo_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_header   TYPE slis_t_listheader,
*      gt_header1  TYPE slis_t_listheader,
      gt_colinfo_table TYPE slis_t_specialcol_alv. "line color.

* hierarchy(simple)
DATA : g_tabname_header       TYPE slis_tabname,       "header part
       g_tabname_item         TYPE slis_tabname,       "detail list
       gs_keyinfo             TYPE slis_keyinfo_alv.   "relation key

* return
DATA : g_exit_caused_by_caller  TYPE c,
       gs_exit_caused_by_user   TYPE slis_exit_by_user.

DATA: col_pos TYPE i,
      cnt     TYPE i,
      g_save  TYPE c,
      g_repid LIKE sy-repid,
      g_variant LIKE disvariant.
DATA: gt_extab TYPE slis_t_extab WITH HEADER LINE.
************************************************************************

****************************************************
* Definition of Internal Tables
****************************************************
DATA: it_alc_short TYPE TABLE OF  ztpp_wire_hour WITH HEADER LINE.

TYPES: BEGIN OF st_data,
*        Reporting Point, Part, Column, Model, Code .
*        rp type ZTPP_WIRE_HOUR-rp,
         no(6),
         model TYPE ztpp_wire_hour-model,
         alc_vals1 TYPE ztpp_wire_hour-alc_vals1,
         alc_vals2 TYPE ztpp_wire_hour-alc_vals2,
         alc_vals3 TYPE ztpp_wire_hour-alc_vals3,
         alc_vals4 TYPE ztpp_wire_hour-alc_vals4,
         alc_vals5 TYPE ztpp_wire_hour-alc_vals5,

         d_1 TYPE ztpp_wire_hour-d_1,
         seq TYPE ztpp_wire_hour-seq,
         bodyin TYPE ztpp_wire_hour-bodyin,
         wbs TYPE ztpp_wire_hour-wbs,
         paint TYPE ztpp_wire_hour-paint,
         prj TYPE ztpp_wire_hour-prj,
         pbs TYPE ztpp_wire_hour-pbs,
         t01 TYPE ztpp_wire_hour-mitu,
         h02 TYPE ztpp_wire_hour-h02,
         h04 TYPE ztpp_wire_hour-h04,
         h06 TYPE ztpp_wire_hour-h06,
         h08 TYPE ztpp_wire_hour-h08,
         h10 TYPE ztpp_wire_hour-h10,
         h12 TYPE ztpp_wire_hour-h12,
         h14 TYPE ztpp_wire_hour-h14,
         h16 TYPE ztpp_wire_hour-h16,
         h18 TYPE ztpp_wire_hour-h18,
         h20 TYPE ztpp_wire_hour-h20,
         h22 TYPE ztpp_wire_hour-h22,
         h24 TYPE ztpp_wire_hour-h24,
         t02 TYPE ztpp_wire_hour-mitu,
         h26 TYPE ztpp_wire_hour-h26,
         h28 TYPE ztpp_wire_hour-h28,
         h30 TYPE ztpp_wire_hour-h30,
         h32 TYPE ztpp_wire_hour-h32,
         h34 TYPE ztpp_wire_hour-h34,
         h36 TYPE ztpp_wire_hour-h36,
         h38 TYPE ztpp_wire_hour-h38,
         h40 TYPE ztpp_wire_hour-h40,
         h42 TYPE ztpp_wire_hour-h42,
         h44 TYPE ztpp_wire_hour-h44,
         h46 TYPE ztpp_wire_hour-h46,
         h48 TYPE ztpp_wire_hour-h48,
         t03 TYPE ztpp_wire_hour-mitu,
         h50 TYPE ztpp_wire_hour-h50,
         h52 TYPE ztpp_wire_hour-h52,
         h54 TYPE ztpp_wire_hour-h54,
         h56 TYPE ztpp_wire_hour-h56,
         h58 TYPE ztpp_wire_hour-h58,
         h60 TYPE ztpp_wire_hour-h60,
         h62 TYPE ztpp_wire_hour-h62,
         h64 TYPE ztpp_wire_hour-h64,
         h66 TYPE ztpp_wire_hour-h66,
         h68 TYPE ztpp_wire_hour-h68,
         h70 TYPE ztpp_wire_hour-h70,
         h72 TYPE ztpp_wire_hour-h72,
         rem TYPE ztpp_wire_hour-mitu,
         stot TYPE ztpp_wire_hour-stot,
         mitu TYPE ztpp_wire_hour-mitu,
         fore TYPE ztpp_wire_hour-fore,
         gtot TYPE ztpp_wire_hour-stot,
       END OF st_data.

DATA: it_data_t TYPE TABLE OF st_data WITH HEADER LINE.
DATA: BEGIN OF it_data OCCURS 0,
*        Reporting Point, Model, Column, Code .
*        rp(03),   "Reporting Point
         no(06),
         model(04),  "Model
         alc_vals1(05),  "ALC Column
         alc_vals2(05),  "ALC Column
         alc_vals3(05),  "ALC Column
         alc_vals4(05),  "ALC Column
         alc_vals5(05),  "ALC Code

         d_1(06) TYPE p,  "Prev. Prod. Report
         seq(05) TYPE p,  "Sequence
         bodyin(05) TYPE p,  "Body IN
         wbs(05) TYPE p,  "WBS
         paint(05) TYPE p,  "Paint IN
         prj(05) TYPE p,  "Paint Reject
         pbs(05) TYPE p,  "PBS Out
         t01(06) TYPE p,  " First Day.. Total
         h02(05) TYPE p,                                    "2 Hrs.
         h04(05) TYPE p,
         h06(05) TYPE p,
         h08(05) TYPE p,
         h10(05) TYPE p,
         h12(05) TYPE p,
         h14(05) TYPE p,
         h16(05) TYPE p,
         h18(05) TYPE p,
         h20(05) TYPE p,
         h22(05) TYPE p,
         h24(05) TYPE p,
         t02(06) TYPE p,  " Second Day.. Total
         h26(05) TYPE p,
         h28(05) TYPE p,
         h30(05) TYPE p,
         h32(05) TYPE p,
         h34(05) TYPE p,
         h36(05) TYPE p,
         h38(05) TYPE p,
         h40(05) TYPE p,                                    "2 Hrs.
         h42(05) TYPE p,                                    "2 Hrs.
         h44(05) TYPE p,
         h46(05) TYPE p,
         h48(05) TYPE p,
         t03(06) TYPE p,  " Third Day.. Total
         h50(05) TYPE p,
         h52(05) TYPE p,
         h54(05) TYPE p,
         h56(05) TYPE p,
         h58(05) TYPE p,
         h60(05) TYPE p,
         h62(05) TYPE p,
         h64(05) TYPE p,
         h66(05) TYPE p,
         h68(05) TYPE p,
         h70(05) TYPE p,              "2  Hrs.
         h72(05) TYPE p,
         rem(06) TYPE p,  " Remain Quantity
         stot(06) TYPE p,  " Sum 21 Days
         mitu(05) TYPE p,
         fore(6) TYPE p,  " Forecast Quantity
         gtot(7) TYPE p,  " Grand Total
         chkbox(01) ,
      END OF it_data.
*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_SUM01' .


****************************************************
* Selection-Screen
****************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_rp    TYPE ztpp_wire_hour-rp MATCHCODE
                                             OBJECT zsh_rp,
            p_model TYPE ztpp_veh_model-model MATCHCODE
                                              OBJECT zsh_veh_model ,
            p_code1 TYPE ztpp_wire_hour-alc_vals1,
            p_code2 TYPE ztpp_wire_hour-alc_vals2,
            p_code3 TYPE ztpp_wire_hour-alc_vals3,
            p_code4 TYPE ztpp_wire_hour-alc_vals4,
            p_code5 TYPE ztpp_wire_hour-alc_vals5.
SELECTION-SCREEN  END OF BLOCK blk1.

***************************************************
INITIALIZATION.
***************************************************
  g_repid = sy-repid.

*************************************************
AT SELECTION-SCREEN ON p_rp.
*************************************************
*  select single  * from  ztpp_status
*    where rp_point = p_rp.
*    if sy-subrc ne 0.
*      message e000 with 'Not registered Reporting Point !'.
*      exit.
*    endif.

*************************************************
START-OF-SELECTION.
*************************************************
  PERFORM initial_data.
  PERFORM make_data_for_display.

*************************************************
END-OF-SELECTION.
*************************************************
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'IT_DATA'.
  PERFORM  build_layout      USING  'X'   space   space.
  PERFORM  build_comment     USING  gt_header[].
* ALV FUNCTION CALL
  PERFORM  start_grid_viewer TABLES  it_data.

*************************************************
TOP-OF-PAGE.
*************************************************
  PERFORM top_of_page.


*&---------------------------------------------------------------------*
*&      Form  initial_data
*&---------------------------------------------------------------------*
*       Searching Raw Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_data.
  RANGES: lr_rp FOR ztpp_wire_hour-rp,           "Reporting Point
          lr_model FOR ztpp_wire_hour-model,     "Model
          lr_code1 FOR ztpp_wire_hour-alc_vals1,  "Column
          lr_code2 FOR ztpp_wire_hour-alc_vals2,  "Column
          lr_code3 FOR ztpp_wire_hour-alc_vals3,  "Column
          lr_code4 FOR ztpp_wire_hour-alc_vals4,  "Column
          lr_code5 FOR ztpp_wire_hour-alc_vals5.  "Code

  IF p_rp <> space .
    lr_rp-sign = 'I'.
    lr_rp-option = 'EQ'.
    lr_rp-low = p_rp.
    APPEND lr_rp.
  ENDIF.
  IF p_model <> space.
    lr_model-sign = 'I'.
    lr_model-option = 'EQ'.
    lr_model-low = p_model.
    APPEND lr_model.
  ENDIF.
  IF p_code1 <> space.
    lr_code1-sign = 'I'.
    lr_code1-option = 'EQ'.
    lr_code1-low = p_code1.
    APPEND lr_code1.
  ENDIF.
  IF p_code2 <> space.
    lr_code2-sign = 'I'.
    lr_code2-option = 'EQ'.
    lr_code2-low = p_code2.
    APPEND lr_code2.
  ENDIF.
  IF p_code3 <> space.
    lr_code3-sign = 'I'.
    lr_code3-option = 'EQ'.
    lr_code3-low = p_code3.
    APPEND lr_code3.
  ENDIF.
  IF p_code4 <> space.
    lr_code4-sign = 'I'.
    lr_code4-option = 'EQ'.
    lr_code4-low = p_code4.
    APPEND lr_code4.
  ENDIF.
  IF p_code5 <> space.
    lr_code5-sign = 'I'.
    lr_code5-option = 'EQ'.
    lr_code5-low = p_code5.
    APPEND lr_code5.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_alc_short
    FROM ztpp_wire_hour
    WHERE rp        IN lr_rp    AND
          model     IN lr_model AND
          alc_vals1 IN lr_code1 AND
          alc_vals2 IN lr_code2 AND
          alc_vals3 IN lr_code3 AND
          alc_vals4 IN lr_code4 AND
          alc_vals5 IN lr_code5   .
ENDFORM.                    " initial_data

*&---------------------------------------------------------------------*
*&      Form  make_data_for_display
*&---------------------------------------------------------------------*
*       Modification of Data For Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data_for_display.
  DATA: l_ser            TYPE i,
        l_no(3)          TYPE n.

* Reporting Point, Column, Model, Code .
  SORT it_alc_short BY rp         model     alc_vals1 alc_vals2
                        alc_vals3 alc_vals4 alc_vals5 .
  LOOP AT it_alc_short.
    CLEAR it_data_t.
    MOVE-CORRESPONDING it_alc_short TO it_data_t.
    COLLECT it_data_t.
  ENDLOOP.

* Loop again for the Numbering and Sum for the total.
  SORT it_data_t BY model     alc_vals1 alc_vals2
                    alc_vals3 alc_vals4 alc_vals5 .

  LOOP AT it_data_t.
    l_ser          = l_ser + 1 .
    it_data_t-no   = l_ser     .
** FURONG ON 07/17/12 FOR 3 SHIFT
    it_data_t-t01  = it_data_t-h02 + it_data_t-h04 + it_data_t-h06 +
   it_data_t-h08  + it_data_t-h10 + it_data_t-h12 + it_data_t-h14 +
                    it_data_t-h16 + it_data_t-h18 + it_data_t-h20 +
                    it_data_t-h22 + it_data_t-h24.
    it_data_t-t02  = it_data_t-h26 + it_data_t-h28 + it_data_t-h30 +
    it_data_t-h32  + it_data_t-h34 + it_data_t-h36 + it_data_t-h38 +
    it_data_t-h40  + it_data_t-h42 + it_data_t-h44 + it_data_t-h46 +
    it_data_t-h48.
    it_data_t-t03  = It_data_t-h50 + it_data_t-h52 + it_data_t-h54 +
    it_data_t-h56  + it_data_t-h58 + it_data_t-h60 + it_data_t-h62 +
    it_data_t-h64  + it_data_t-h66 + it_data_t-h68 + it_data_t-h70 +
    it_data_t-h72.

*    it_data_t-t01  = it_data_t-h02 + it_data_t-h04 + it_data_t-h06 +
*    it_data_t-h08  + it_data_t-h10 + it_data_t-h12 + it_data_t-h14 +
*                     it_data_t-h16 + it_data_t-h18 + it_data_t-h20 .
*    it_data_t-t02  = it_data_t-h22 + it_data_t-h24 + it_data_t-h26 +
*    it_data_t-h28  + it_data_t-h30 + it_data_t-h32 + it_data_t-h34 +
*                     it_data_t-h36 + it_data_t-h38 + it_data_t-h40 .
*    it_data_t-t03  = it_data_t-h42 + it_data_t-h44 + it_data_t-h46 +
*    it_data_t-h48  + it_data_t-h50 + it_data_t-h52 + it_data_t-h54 +
*                     it_data_t-h56 + it_data_t-h58 + it_data_t-h60 .
** END ON 07/17/12
    it_data_t-gtot = it_data_t-stot + it_data_t-mitu.
    MODIFY it_data_t         .
    MOVE-CORRESPONDING it_data_t TO it_data.
    it_data-gtot = it_data-stot + it_data-mitu.             "UD1K912950
    APPEND it_data.
  ENDLOOP.
ENDFORM.                    " make_data_for_display

*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events.
  CONSTANTS : c_pss TYPE slis_formname VALUE 'PF_STATUS_SET',
              c_uc  TYPE slis_formname VALUE 'USER_COMMAND',
              c_top TYPE slis_formname VALUE 'TOP_OF_PAGE'.
  REFRESH gt_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = gt_events.

  PERFORM modify_gt_events
          TABLES  gt_events
          USING :
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.

ENDFORM.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_C_TOP  text
*----------------------------------------------------------------------*
FORM modify_gt_events TABLES p_events_t LIKE gt_events
                      USING  p_form p_value.

  DATA: ls_event TYPE slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_form
                          INTO ls_event.
  IF sy-subrc EQ 0.
    MOVE     p_value     TO   ls_event-form.
    MODIFY   p_events_t  FROM ls_event INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0441   text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_intab.
  DATA : wa_date LIKE sy-datum,
         wa_kalid  LIKE kako-kalid,
         d_day(15),
         d_date(5),
         wa_pdate LIKE sy-datum.

  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.
  FIELD-SYMBOLS : <wa> TYPE ANY.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_internal_tabname = p_intab
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = gt_fc.

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*          Start of Setting The Field's Attributes
                                  'S' 'NO'  ' ',
*          Main Setting of The Field's Attributes
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
*          End of Setting The Field's Attributes
                                  'E' 'SELTEXT_L'   'No.',

                                  'S' 'MODEL'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '04',
                                  'E' 'SELTEXT_L'   'Model',

                                  'S' 'ALC_VALS1'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'ALC Value1',

                                  'S' 'ALC_VALS2'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'ALC Value2',

                                  'S' 'ALC_VALS3'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'ALC Value3',

                                  'S' 'ALC_VALS4'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'ALC Value4',

                                  'S' 'ALC_VALS5'   ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'ALC Value5',

                                  'S' 'D_1'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'D - 1'   ,

                                  'S' 'SEQ'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Sequence',

                                  'S' 'BODYIN'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Body IN',

                                  'S' 'WBS'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'WBS',

                                  'S' 'PAINT'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Paint IN',

                                  'S' 'PRJ'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'P/REj'     ,

                                  'S' 'PBS'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'P B S'  .
  PERFORM read_start_day    USING wa_date   .
  wa_pdate = wa_date.
  PERFORM read_shop_calid   USING wa_kalid  .
  ASSIGN wa_date TO <wa>.
  WRITE wa_date TO <wa> MM/DD/YYYY .
  MOVE <wa>(5) TO d_date.
  CONCATENATE d_date 'Total' INTO d_day."D1
  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                  'S' 'T01'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   d_day,

                                  'S' 'H02'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(0)H',

                                  'S' 'H04'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(1)H',

                                  'S' 'H06'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(2)H',

                                  'S' 'H08'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(3)H',

                                  'S' 'H10'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(4)H',

                                  'S' 'H12'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(5)H',

                                  'S' 'H14'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(6)H',

                                  'S' 'H16'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(7)H',

                                  'S' 'H18'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(8)H',

                                  'S' 'H20'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(9)H',

** Furong on 07/17/12

                                  'S' 'H22'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(A)H',

                                  'S' 'H24'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(B)H'.

  wa_date = wa_pdate + 1.
  PERFORM call_workday  USING wa_date .
  wa_pdate = wa_date.
  ASSIGN wa_date TO <wa>.
  WRITE wa_date TO <wa> MM/DD/YYYY .
  MOVE <wa>(5) TO d_date.
  CONCATENATE d_date 'Total' INTO d_day."D+1

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :

                                  'S' 'T02'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'    d_day,

                                  'S' 'H26'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(C)H',

                                  'S' 'H28'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(D)H',

                                  'S' 'H30'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(E)H',

                                  'S' 'H32'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(F)H',

                                  'S' 'H34'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(G)H',

                                  'S' 'H36'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(H)H',

                                  'S' 'H38'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(I)H',

                                  'S' 'H40'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(J)H',

                                  'S' 'H42'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(K)H',

                                  'S' 'H44'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(L)H',

                                  'S' 'H46'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(M)H',

                                  'S' 'H48'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(N)H'.

  wa_date = wa_pdate + 1.
  PERFORM call_workday  USING wa_date .
  ASSIGN wa_date TO <wa>.
  WRITE wa_date TO <wa> MM/DD/YYYY .
  MOVE <wa>(5) TO d_date.
  CONCATENATE d_date 'Total' INTO d_day."D2

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                  'S' 'T03'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   d_day,

                                  'S' 'H50'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(O)H',

                                  'S' 'H52'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(P)H',

                                  'S' 'H54'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(Q)H',

                                  'S' 'H56'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(R)H',

                                  'S' 'H58'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(S)H',

                                  'S' 'H60'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(T)H',


                                  'S' 'H62'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(U)H',

                                  'S' 'H64'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(V)H',

                                  'S' 'H66'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(W)H',

                                  'S' 'H68'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(X)H',

                                 'S' 'H70'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(Y)H',


                                  'S' 'H72'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   '2(Z)H',


*  wa_date = wa_pdate + 1.
*  PERFORM call_workday  USING wa_date .
*  ASSIGN wa_date TO <wa>.
*  WRITE wa_date TO <wa> MM/DD/YYYY .
*  MOVE <wa>(5) TO d_date.
*  CONCATENATE d_date 'Total' INTO d_day."D+1
*
*  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*
*                                  'S' 'T02'    ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'    d_day,
*
*                                  'S' 'H22'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H24'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H26'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H28'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H30'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H32'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H34'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H36'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H38'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H40'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs'.
*  wa_date = wa_pdate + 2.
*  PERFORM call_workday  USING wa_date .
*  ASSIGN wa_date TO <wa>.
*  WRITE wa_date TO <wa> MM/DD/YYYY .
*  MOVE <wa>(5) TO d_date.
*  CONCATENATE d_date 'Total' INTO d_day."D2
*
*  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*                                  'S' 'T03'    ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   d_day,
*
*                                  'S' 'H42'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H44'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H46'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H48'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H50'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H52'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H54'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H56'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H58'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',
*
*                                  'S' 'H60'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   '2 Hrs',



** End on 07/17/12

                                  'S' 'REM'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'REM'     ,

                                  'S' 'STOT'   ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'S-TOT' ,

                                  'S' 'MITU'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'MITU',

                                  'S' 'FORE'   ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'FORE'    ,

                                  'S' 'GTOT'   ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '07',
                                  'E' 'SELTEXT_L'   'G-TOT'.
* DELETE FIELDS WITHOUT DATA                         "UD1K912950
  DELETE gt_fieldcat WHERE fieldname = 'REM' OR
                           fieldname = 'FORE'.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Category For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_1259   text
*      -->P_1260   text
*      -->P_1261   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat LIKE gt_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.

  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR : g_fieldcat_s.
    READ TABLE gt_fc INTO g_fieldcat_s
                     WITH KEY fieldname  = p_field.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'G_FIELDCAT_S-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO cnt.
    g_fieldcat_s-col_pos = cnt.
    APPEND g_fieldcat_s TO p_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0445   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout USING p_cb p_color p_sum.

  CLEAR gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
*  gs_layout-default_item      = 'X'.
* check box
  IF p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  ENDIF.
* line color
  IF p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  ENDIF.
* sum
  IF p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  ENDIF.
*
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
*        L_MANAGER(50),
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_time(8),
*        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

 DATA: l_serial LIKE ztpp_input_plan-serial,
        l_rp06_serial(10),
        l_model LIKE ztpp_input_plan-modl,
        l_body LIKE ztpp_input_plan-body_ser,
        l_rp06 LIKE ztpp_input_plan-rp06,
        l_rp06_time LIKE sy-uzeit,
        l_rp06_date LIKE sy-datum,
        l_rp06_time_char(8),
        l_bodyno(9).

* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  APPEND ls_line TO p_gt_header.

** Furong on 04/11/13

 SELECT MAX( serial ) INTO l_serial
   FROM ztpp_input_plan
    WHERE status = '06'.
   select single modl body_ser rp06
     INTO (l_model, l_body, l_rp06)
    FROM ztpp_input_plan
    WHERE serial = l_serial.

  CONCATENATE l_model l_body INTO l_bodyno.

  select single atwrt into l_rp06_serial
    from ausp as a
    inner join cabn as b
    on a~atinn = b~atinn
    where objek = l_bodyno
      and klart = '002'
      and atnam = 'P_RP06_SERIAL'.

   write: l_rp06+8(6) to l_rp06_time.
   write: l_rp06+0(8) to l_rp06_date.

   WRITE l_rp06_date TO l_date .

  ls_line-typ = 'S'.
  CONCATENATE 'AS OF :'  l_date(10)
              INTO ls_line-info SEPARATED BY ' '.
  WRITE: l_rp06_time TO l_rp06_time_char.
  CONCATENATE ls_line-info l_rp06_time_char(8) INTO ls_line-info
              SEPARATED BY ' '.

  CONCATENATE ls_line-info l_bodyno 'SEQ'  l_rp06_serial
              INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
** end on 04/11/13

* User
  ls_line-typ  = 'A'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  APPEND ls_line TO p_gt_header.

* today
  ls_line-typ  = 'A'.
  ls_line-key  = 'Today : '.
  WRITE  sy-datum TO ls_line-info.
  WRITE  sy-uzeit TO l_time      .
  CONCATENATE  ls_line-info  l_time  INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

ENDFORM.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM start_grid_viewer TABLES p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
*     i_bypassing_buffer       = 'X'
      i_callback_program       = g_repid
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      i_callback_top_of_page   = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'SET_STATUS'
*     I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*     IT_SORT                  = GT_SORT[]
      i_save                   = 'A'
*      is_variant               = g_variant
      it_events                = gt_events[]
      is_print                 = gs_print
      it_list_commentary       = gt_header
    IMPORTING
      e_exit_caused_by_caller  = g_exit_caused_by_caller
      es_exit_caused_by_user   = gs_exit_caused_by_user
    TABLES
      t_outtab                 = p_intab.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " start_grid_viewer

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_header.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  read_start_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATE  text
*----------------------------------------------------------------------*
form read_start_day  USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.

endform.                    " read_start_day
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
form read_shop_calid USING   pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'.

endform.                    " read_shop_calid
*&---------------------------------------------------------------------*
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATE  text
*----------------------------------------------------------------------*
form call_workday  USING    pa_date.
  DATA: l_ident             LIKE t001w-fabkl.

  SELECT SINGLE fabkl  INTO l_ident
    FROM t001w
   WHERE werks = 'P001'   .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = '+'
      date                         = pa_date
      factory_calendar_id          = l_ident
    IMPORTING
      date                         = pa_date
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.

endform.                    " call_workday
