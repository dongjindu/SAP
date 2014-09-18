************************************************************************
* Program Name      : ZRPP202R_SEQUENCED_LONG_PLAN
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 2.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : Long Term's Sequenced Plan Per Code(ALC).
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zrpp202r_sequenced_long_plan  NO STANDARD PAGE HEADING
                                      MESSAGE-ID zmpp.

TABLES: ztpp_common_vals, "[PP] COMMON Information Table
        ztpp_seq_sum02 ,  "ALC Code Summary - Long Term Plan Table
        ztpp_status.

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

* FIELD SYMBOLS
FIELD-SYMBOLS: <fs> TYPE ANY.

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

****************************************************
* Definition of Internal Tables
****************************************************
DATA: it_alc_long TYPE TABLE OF  ztpp_seq_sum02 WITH HEADER LINE.

TYPES: BEGIN OF st_data,
*        Reporting Point, Part, Column, Model, Code .
*        rp type ZTPP_SEQ_SUM01-rp,
         no(6),
         code(3),
         part(01),  " U or C
         alc_code TYPE ztpp_seq_sum02-alc_code,
         model TYPE ztpp_seq_sum02-model,
         alc_vals TYPE ztpp_seq_sum02-alc_vals,

         d_1 TYPE ztpp_seq_sum01-d_1,
         seq TYPE ztpp_seq_sum02-seq,
         bodyin TYPE ztpp_seq_sum02-bodyin,
         wbs TYPE ztpp_seq_sum02-wbs,
         paint TYPE ztpp_seq_sum02-paint,
         prj TYPE ztpp_seq_sum02-prj,
         pbs TYPE ztpp_seq_sum02-pbs,

         d01 TYPE ztpp_seq_sum02-d01,
         d02 TYPE ztpp_seq_sum02-d02,
         d03 TYPE ztpp_seq_sum02-d03,
         d04 TYPE ztpp_seq_sum02-d04,
         d05 TYPE ztpp_seq_sum02-d05,
         d06 TYPE ztpp_seq_sum02-d06,
         d07 TYPE ztpp_seq_sum02-d07,
         d08 TYPE ztpp_seq_sum02-d08,
         d09 TYPE ztpp_seq_sum02-d09,
         d10 TYPE ztpp_seq_sum02-d10,
         d11 TYPE ztpp_seq_sum02-d11,
         d12 TYPE ztpp_seq_sum02-d12,
         d13 TYPE ztpp_seq_sum02-d13,
         d14 TYPE ztpp_seq_sum02-d14,
         d15 TYPE ztpp_seq_sum02-d15,
         d16 TYPE ztpp_seq_sum02-d16,
         d17 TYPE ztpp_seq_sum02-d17,
         d18 TYPE ztpp_seq_sum02-d18,
         d19 TYPE ztpp_seq_sum02-d19,
         d20 TYPE ztpp_seq_sum02-d20,
         d21 TYPE ztpp_seq_sum02-d21,
         mitu TYPE ztpp_seq_sum02-mitu,
         stot TYPE ztpp_seq_sum02-stot,
         fore TYPE ztpp_seq_sum02-fore,
         gtot TYPE ztpp_seq_sum02-stot,
       END OF st_data.

DATA: it_data_t TYPE TABLE OF st_data WITH HEADER LINE.
DATA: BEGIN OF it_data OCCURS 0,
*        Reporting Point, Model, Column, Code .
*        rp(03),   "Reporting Point
         no(06),
         code(3),
         model(04),  "Model
         alc_code(04),  "ALC Column
         alc_vals(05),  "ALC Code

         d_1(06) TYPE p,  "Prev. Prod. Report
         seq(05) TYPE p,  "Sequence
         bodyin(05) TYPE p,  "Body IN
         wbs(05) TYPE p,  "WBS
         paint(05) TYPE p,  "Paint IN
         prj(05) TYPE p,  "Paint Reject
         pbs(05) TYPE p,  "PBS Out

         d01(06) TYPE p,                                    "1 Hrs.
         d02(06) TYPE p,
         d03(06) TYPE p,
         d04(06) TYPE p,
         d05(06) TYPE p,
         d06(06) TYPE p,
         d07(06) TYPE p,
         d08(06) TYPE p,
         d09(06) TYPE p,
         d10(06) TYPE p,
         d11(06) TYPE p,
         d12(06) TYPE p,
         d13(06) TYPE p,
         d14(06) TYPE p,
         d15(06) TYPE p,
         d16(06) TYPE p,
         d17(06) TYPE p,
         d18(06) TYPE p,
         d19(06) TYPE p,
         d20(06) TYPE p,                                    "1 Hrs.
         d21(06) TYPE p,                                    "10 Hrs.
         rem(06) TYPE p,  " Remain Quantity
         stot(06) TYPE p,  " Sum 21 Days
         mitu(05) TYPE p,
         fore(6) TYPE p,  " Forecast Quantity
         gtot(7) TYPE p,  " Grand Total
         chkbox(01) ,
      END OF it_data.
DATA : z_max_date LIKE sy-datum.
****************************************************
* Work-Area Variables Definition
****************************************************
DATA: alv_grid               TYPE REF TO cl_gui_alv_grid,
      gs_custom_container    TYPE REF TO cl_gui_custom_container,
      wa_date                TYPE d,
      wa_kalid               LIKE kako-kalid                ,
      wa_container           TYPE scrfname VALUE 'CONTAINER',
      wa_datum               TYPE d.
*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_SUM02' .


****************************************************
* Selection-Screen
****************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_rp    TYPE ztpp_seq_sum02-rp MATCHCODE
                                             OBJECT zsh_rp,
            p_model TYPE zmodel            MATCHCODE
                                             OBJECT zsh_veh_model ,
            p_col   TYPE ztpp_seq_sum02-alc_code,
            p_code  TYPE ztpp_seq_sum02-alc_vals .
SELECTION-SCREEN  END OF BLOCK blk1.

***************************************************
INITIALIZATION.
***************************************************
  g_repid = sy-repid.
  wa_date = sy-datum.

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
  RANGES: lr_rp FOR ztpp_seq_sum02-rp,         "Reporting Point
          lr_model FOR ztpp_seq_sum02-model,   "Model
          lr_col FOR ztpp_seq_sum02-alc_code,  "Column
          lr_code FOR ztpp_seq_sum02-alc_vals. "Code

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
  IF p_col <> space.
    lr_col-sign = 'I'.
    lr_col-option = 'EQ'.
    lr_col-low = p_col.
    APPEND lr_col.
  ENDIF.
  IF p_code <> space.
    lr_code-sign = 'I'.
    lr_code-option = 'EQ'.
    lr_code-low = p_code.
    APPEND lr_code.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_alc_long
    FROM ztpp_seq_sum02
    WHERE rp       IN lr_rp    AND
          model    IN lr_model AND
          alc_code IN lr_col   AND
          alc_vals IN lr_code    .
************************************************
*  perform make_temp_data-for-ztpp_alc_sh.
************************************************

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
*---start wskim 03/22/2005
  DATA : z_date_count TYPE i,
         d_field(11),
         d_num(2) TYPE n.
  FIELD-SYMBOLS : <wa_d> TYPE ANY.

*---end

* Reporting Point, Column, Model, Code .
  SORT it_alc_long BY rp alc_code model alc_vals .
  LOOP AT it_alc_long.
    CLEAR it_data_t.
    MOVE-CORRESPONDING it_alc_long TO it_data_t.
*   Part(Unique or Color)
    MOVE it_alc_long-alc_code+00(01) TO it_data_t-part.
*   Code(Code Number conversion)
    it_data_t-code = l_no = it_alc_long-alc_code+1(3) .
*   ALC Code: Number Part Conversion.
    CONCATENATE it_data_t-part l_no INTO it_data_t-alc_code .
    COLLECT it_data_t.
  ENDLOOP.

* Loop again for the Numbering and Sum for the total.
  SORT it_data_t BY model    ASCENDING
                    part     DESCENDING
                    code     ASCENDING .

  LOOP AT it_data_t.
    l_ser          = l_ser + 1 .
    it_data_t-no   = l_ser     .
    MODIFY it_data_t         .
    MOVE-CORRESPONDING it_data_t TO it_data.
    it_data-gtot = it_data-mitu + it_data-stot.
    APPEND it_data.
  ENDLOOP.
*---start wskim 03/22/2005
  PERFORM get_date_input CHANGING z_date_count.
  LOOP AT it_data.
    d_num =  z_date_count  + 1.
    DO .
      IF d_num > 21.
        EXIT.
      ENDIF.
      CONCATENATE 'IT_DATA-D' d_num INTO d_field.
      ASSIGN (d_field) TO <wa_d>.
      it_data-rem =  it_data-rem + <wa_d>.
      d_num = d_num + 1.
    ENDDO.
    MODIFY it_data FROM it_data.
  ENDLOOP.
*---End

ENDFORM.                    " make_data_for_display

*&---------------------------------------------------------------------*
*&      Form  READ_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATUM  text
*----------------------------------------------------------------------*
FORM read_start_day USING    pa_datum.
  CLEAR: ztpp_common_vals.
  SELECT SINGLE *
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
ENDFORM.                    " READ_START_DAY

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

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
  DATA: l_d01(10), l_d02(10), l_d03(10), l_d04(10), l_d05(10),
        l_d06(10), l_d07(10), l_d08(10), l_d09(10), l_d10(10),
        l_d11(10), l_d12(10), l_d13(10), l_d14(10), l_d15(10),
        l_d16(10), l_d17(10), l_d18(10), l_d19(10), l_d20(10),
        l_d21(10), l_name(30), l_no(2) TYPE n,
         w_field(3),
         w_num(2) TYPE n,
         l_date(10).


  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  PERFORM read_start_day    USING wa_date   .
  PERFORM read_shop_calid   USING wa_kalid  .

*  DO 21 TIMES.
*    l_no = l_no + 1 .
*    PERFORM call_workday  USING wa_date .
*    CONCATENATE 'L_D' l_no INTO l_name  .
*    ASSIGN (l_name)        TO   <fs>    .
**   <fs>  =  wa_date      .
*    WRITE wa_date TO <fs> MM/DD/YYYY    .
*    wa_date = wa_date + 1 .
*  ENDDO.

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

                                  'S' 'ALC_CODE'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '04',
                                  'E' 'SELTEXT_L'   'ALC Column',

                                  'S' 'ALC_VALS'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '04',
                                  'E' 'SELTEXT_L'   'BroadCast Code',

                                  'S' 'D_1'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
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

                                  'S' 'PAINT'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Paint IN',

                                  'S' 'PBS'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'P B S'  ,

                                  'S' 'PRJ'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'P/REj'     ,

                                  'S' 'WBS'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'WBS'.
*---start#1 wskim 03/22/2005
  CLEAR w_num.
  w_num = 1.
  DO.
    CLEAR w_field.
    IF  wa_date > z_max_date.
      EXIT.
    ENDIF.
    CONCATENATE 'D' w_num INTO w_field.

    WRITE wa_date TO l_date  DD/MM/YYYY .
    wa_date = wa_date + 1.
    PERFORM read_working_date USING '+'  wa_kalid  wa_date .

    PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                    'S'  w_field     ' ',
                                    ' ' 'JUST'        'L',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '05',
                                    'E' 'SELTEXT_L'   l_date.
    w_num = w_num + 1.
  ENDDO.
*
*                                  'S' 'D02'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d02(5),
*
*                                  'S' 'D03'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d03(5),
*
*                                  'S' 'D04'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d04(5),
*
*                                  'S' 'D05'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d05(5),
*
*                                  'S' 'D06'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d06(5),
*
*                                  'S' 'D07'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d07(5),
*
*                                  'S' 'D08'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d08(5),
*
*                                  'S' 'D09'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d09(5),
*
*                                  'S' 'D10'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d10(5),
*
*                                  'S' 'D11'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d11(5),
*
*                                  'S' 'D12'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d12(5),
*
*                                  'S' 'D13'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d13(5),
*
*                                  'S' 'D14'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d14(5),
*
*                                  'S' 'D15'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d15(5),
*
*                                  'S' 'D16'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d16(5),
*
*                                  'S' 'D17'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d17(5),
*
*                                  'S' 'D18'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d18(5),
*
*                                  'S' 'D19'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d19(5),
*
*                                  'S' 'D20'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d20(5),
*
*                                  'S' 'D21'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   l_d21(5),

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
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
* DELETE FIELDS                                      "UD1K912950
  DELETE gt_fieldcat WHERE fieldname = 'FORE' ."OR
*                                 fieldname = 'REM'.

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
  gs_layout-default_item      = 'X'.
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
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
*            i_bypassing_buffer       = 'X'
            i_callback_program       = g_repid
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            i_callback_pf_status_set = 'SET_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
            is_variant               = g_variant
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
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATE  text
*----------------------------------------------------------------------*
FORM call_workday USING    pa_date.
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
ENDFORM.                    " call_workday
*&---------------------------------------------------------------------*
*&      Form  get_date_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_Z_DATE_COUNT  text
*----------------------------------------------------------------------*
FORM get_date_input CHANGING p_date_count.

  CLEAR : z_max_date.
  SELECT MAX( sqdt ) INTO z_max_date
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*   FROM ztpp_pmt07jb_a
** End
    WHERE gubb = 'A'.

  PERFORM read_start_day    USING wa_datum  .
  PERFORM read_shop_calid   USING wa_kalid  .
*  wa_datum = wa_datum + 1.
*  p_date_count = 1.
  DO.
    IF  wa_datum > z_max_date.
      EXIT.
    ENDIF.
    PERFORM read_working_date USING '+'  wa_kalid  wa_datum .
    p_date_count = p_date_count + 1.
    wa_datum = wa_datum + 1.

  ENDDO.

ENDFORM.                    " get_date_input
*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3141   text
*      -->P_WA_KALID  text
*      -->P_WA_DATUM  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.

ENDFORM.                    " read_working_date
