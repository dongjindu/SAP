************************************************************************
* Program Name      : ZRPP203R_SEQUENCED_WEEK_PLAN
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.10.
* Specifications By : MH Moon
* Pattern           : 2.1
* Development Request No : UD1K907187
* Addl Documentation:
* Description       : Weekly Term's Sequenced Plan
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 04/05/2005 chris        UD1K915361   add mrp remain qty
************************************************************************
REPORT  zrpp203r_sequenced_week_plan  NO STANDARD PAGE HEADING
                                      MESSAGE-ID zmpp.

TABLES: ztpp_common_vals, "[PP] COMMON Information Table
        ztpp_seq_sum03 ,  "ALC Code Summary - Long Term Plan Table
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
FIELD-SYMBOLS: <fs> TYPE any.

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
DATA: g_mrp_last_week  TYPE i.
****************************************************
* Definition of Internal Tables
****************************************************
DATA: it_alc_long TYPE TABLE OF  ztpp_seq_sum03 WITH HEADER LINE.

TYPES: BEGIN OF st_data,
*        Reporting Point, Part, Column, Model, Code .
*        rp type ZTPP_SEQ_SUM01-rp,
         no(6),
         code(3),
         part(01),  " U or C
         alc_code TYPE ztpp_seq_sum03-alc_code,
         model TYPE ztpp_seq_sum03-model,
         alc_vals TYPE ztpp_seq_sum03-alc_vals,

         w_1 TYPE ztpp_seq_sum03-w_1,
         seq TYPE ztpp_seq_sum03-seq,
         bodyin TYPE ztpp_seq_sum03-bodyin,
*        wbs TYPE ztpp_seq_sum03-wbs,
         paint TYPE ztpp_seq_sum03-paint,
*        prj TYPE ztpp_seq_sum03-prj,
         pbs TYPE ztpp_seq_sum03-pbs,

         w01 TYPE ztpp_seq_sum03-w01,
         w02 TYPE ztpp_seq_sum03-w02,
         w03 TYPE ztpp_seq_sum03-w03,
         w04 TYPE ztpp_seq_sum03-w04,
         w05 TYPE ztpp_seq_sum03-w05,
         w06 TYPE ztpp_seq_sum03-w06,
         w07 TYPE ztpp_seq_sum03-w07,
         w08 TYPE ztpp_seq_sum03-w08,
         w09 TYPE ztpp_seq_sum03-w09,
         w10 TYPE ztpp_seq_sum03-w10,
         w11 TYPE ztpp_seq_sum03-w11,
         w12 TYPE ztpp_seq_sum03-w12,
         w13 TYPE ztpp_seq_sum03-w13,
         w14 TYPE ztpp_seq_sum03-w14,
         w15 TYPE ztpp_seq_sum03-w15,
         w16 TYPE ztpp_seq_sum03-w16,
         w17 TYPE ztpp_seq_sum03-w17,
         w18 TYPE ztpp_seq_sum03-w18,
         w19 TYPE ztpp_seq_sum03-w19,
         w20 TYPE ztpp_seq_sum03-w20,
         w21 TYPE ztpp_seq_sum03-w21,
         w99 TYPE ztpp_seq_sum03-w99,
         mitu TYPE ztpp_seq_sum03-mitu,
*        STOT type ZTPP_SEQ_SUM03-STOT,
*        FORE type ZTPP_SEQ_SUM02-FORE,
*        GTOT type ZTPP_SEQ_SUM02-STOT,
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

         w_1(06) TYPE p,  "Prev. Prod. Report
         seq(05) TYPE p,  "Sequence
         bodyin(05) TYPE p,  "Body IN
*        wbs(05) TYPE p,  "WBS
         paint(05) TYPE p,  "Paint IN
*        prj(05) TYPE p,  "Paint Reject
         pbs(05) TYPE p,  "PBS Out

         w01(06) TYPE p,                                    "1 Hrs.
         w02(06) TYPE p,
         w03(06) TYPE p,
         w04(06) TYPE p,
         w05(06) TYPE p,
         w06(06) TYPE p,
         w07(06) TYPE p,
         w08(06) TYPE p,
         w09(06) TYPE p,
         w10(06) TYPE p,
         w11(06) TYPE p,
         w12(06) TYPE p,
         w13(06) TYPE p,
         w14(06) TYPE p,
         w15(06) TYPE p,
         w16(06) TYPE p,
         w17(06) TYPE p,
         w18(06) TYPE p,
         w19(06) TYPE p,
         w20(06) TYPE p,                                    "1 Hrs.
         w21(06) TYPE p,                                    "10 Hrs.
         w99(06) TYPE p,  " Remain Quantity
*        STOT(06) TYPE P,  " Sum 21 Days
         mitu(05) TYPE p,
*        fore(6) TYPE P,  " Forecast Quantity
*        GTOT(7) TYPE P,  " Grand Total
         chkbox(01) ,
      END OF it_data.

DATA: BEGIN OF it_inputplan_date OCCURS 0,
       rsnum LIKE ztpp_input_plan-rsnum,
       END OF it_inputplan_date.
****************************************************
* Work-Area Variables Definition
****************************************************
DATA: alv_grid               TYPE REF TO cl_gui_alv_grid,
      gs_custom_container    TYPE REF TO cl_gui_custom_container,
      wa_date                TYPE d,
      ca_date                TYPE d,
      wa_kalid               LIKE kako-kalid                ,
      wa_container           TYPE scrfname VALUE 'CONTAINER'.

*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
DATA: c_jobs(40)              VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              VALUE 'SEQ_SUM03' .

DATA : it_7jb_date LIKE ztpp_pmt07jb_a OCCURS 0 WITH HEADER LINE,
       it_7jb_a LIKE it_7jb_date OCCURS 0 WITH HEADER LINE.

****************************************************
* Selection-Screen
****************************************************
SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_rp    TYPE ztpp_seq_sum03-rp MATCHCODE
                                             OBJECT zsh_rp,
            p_model TYPE zmodel            MATCHCODE
                                             OBJECT zsh_veh_model ,
            p_col   TYPE ztpp_seq_sum03-alc_code,
            p_code  TYPE ztpp_seq_sum03-alc_vals .
SELECTION-SCREEN  END OF BLOCK blk1.

SELECTION-SCREEN  BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_date FOR sy-datum.
SELECTION-SCREEN SKIP.
*PARAMETERS: P_ND RADIOBUTTON GROUP GRP,
*            P_SD RADIOBUTTON GROUP GRP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) text-m01 FOR FIELD p_w01.
PARAMETERS: p_w01 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 20(3) text-m02 FOR FIELD p_w02.
PARAMETERS: p_w02 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 40(3) text-m03 FOR FIELD p_w03.
PARAMETERS: p_w03 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 60(3) text-m04 FOR FIELD p_w04.
PARAMETERS: p_w04 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) text-m05 FOR FIELD p_w05.
PARAMETERS: p_w05 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 20(3) text-m06 FOR FIELD p_w06.
PARAMETERS: p_w06 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 40(3) text-m07 FOR FIELD p_w07.
PARAMETERS: p_w07 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 60(3) text-m08 FOR FIELD p_w08.
PARAMETERS: p_w08 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) text-m09 FOR FIELD p_w09.
PARAMETERS: p_w09 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 20(3) text-m10 FOR FIELD p_w10.
PARAMETERS: p_w10 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 40(3) text-m11 FOR FIELD p_w11.
PARAMETERS: p_w11 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 60(3) text-m12 FOR FIELD p_w12.
PARAMETERS: p_w12 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(3) text-m13 FOR FIELD p_w13.
PARAMETERS: p_w13 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 20(3) text-m14 FOR FIELD p_w14.
PARAMETERS: p_w14 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 40(3) text-m15 FOR FIELD p_w15.
PARAMETERS: p_w15 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN COMMENT 60(3) text-m16 FOR FIELD p_w16.
PARAMETERS: p_w16 LIKE sy-datum." OBLIGATORY.
SELECTION-SCREEN END   OF LINE.


SELECTION-SCREEN  END OF BLOCK blk2.

***************************************************
INITIALIZATION.
***************************************************
  g_repid = sy-repid.
  wa_date = sy-datum.
  PERFORM get_init_value.
*************************************************
AT SELECTION-SCREEN OUTPUT.
*************************************************
*  PERFORM CHECK_SCREEN.
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
  RANGES: lr_rp FOR ztpp_seq_sum03-rp,         "Reporting Point
          lr_model FOR ztpp_seq_sum03-model,   "Model
          lr_col FOR ztpp_seq_sum03-alc_code,  "Column
          lr_code FOR ztpp_seq_sum03-alc_vals. "Code

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
    FROM ztpp_seq_sum03
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
    APPEND it_data.
  ENDLOOP.
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
  DATA : w_field(3),
         w_num(2) TYPE n,
         l_date(10).
  DATA: l_day LIKE scal-indicator,
  l_first_date LIKE sy-datum,
  l_second_date LIKE sy-datum,
  l_third_date LIKE sy-datum,
  l_first(1),
  l_second(1).


  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

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

                                  'S' 'W_1'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'W - 1'   ,

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
                                  'E' 'SELTEXT_L'   'P B S'  .
*
*                                  'S' 'PRJ'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   'P/REj'     ,
*
*                                  'S' 'WBS'    ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'WBS',
*---start#1 wskim 03/22/2005
*  PERFORM READ_START_DAY    USING WA_DATE   .
*  PERFORM READ_SHOP_CALID   USING WA_KALID  .
*  CLEAR W_NUM.
*  W_NUM = 1.
*  CA_DATE = WA_DATE.
*  DO 21 TIMES.
*    CLEAR W_FIELD.
*    CONCATENATE 'W' W_NUM INTO W_FIELD.
*    WRITE CA_DATE TO L_DATE  DD/MM/YYYY .
*    PERFORM SETTING_FIELDCAT TABLES GT_FIELDCAT USING :
*
*                                    'S' W_FIELD     ' ',
*                                    ' ' 'JUST'        'L',
*                                    ' ' 'KEY'         ' ',
*                                    ' ' 'DDICTXT'     'L',
*                                    ' ' 'OUTPUTLEN'   '06',
*                                    'E' 'SELTEXT_L'   L_DATE(05) .
*    W_NUM = W_NUM + 1.
*    PERFORM GET_WEEK USING W_NUM CA_DATE
*                     CHANGING CA_DATE.
*
*  ENDDO.

** Changed by Furong on 06/17/09

  IF NOT sy-datum IN s_date.
    PERFORM read_start_day    USING wa_date   .
    PERFORM read_shop_calid   USING wa_kalid  .
    CLEAR w_num.
    w_num = 1.

    PERFORM get_date_input_plan.
    READ TABLE it_inputplan_date INDEX 1.

    l_first_date = sy-datum.
    PERFORM get_week USING w_num l_first_date
                       CHANGING l_first_date.
    IF it_inputplan_date-rsnum >= l_first_date.
      WRITE sy-datum TO l_date  DD/MM/YYYY .
      l_first_date = sy-datum.
    ELSE.
      WRITE it_inputplan_date-rsnum TO l_date  DD/MM/YYYY .
      l_first_date = it_inputplan_date-rsnum.
    ENDIF.
    CONCATENATE 'W' w_num INTO w_field.

    PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                    'S' w_field       ' ',
                                    ' ' 'JUST'        'L',
                                    ' ' 'KEY'         ' ',
                                    ' ' 'DDICTXT'     'L',
                                    ' ' 'OUTPUTLEN'   '06',
                                    'E' 'SELTEXT_L'   l_date(5).

*  L_FIRST_DATE = IT_INPUTPLAN_DATE-RSNUM.
    PERFORM get_week USING w_num l_first_date
                       CHANGING l_first_date.
    l_second_date = l_first_date + 7.
    l_third_date = l_second_date + 7.

    w_num = w_num + 1.
    CLEAR: l_first,l_second.

    LOOP AT it_inputplan_date.
      IF w_num > 3.
        EXIT.
      ENDIF.
      IF it_inputplan_date-rsnum >= l_first_date AND
         it_inputplan_date-rsnum < l_second_date AND
         l_first IS INITIAL.
        CLEAR w_field.
        WRITE it_inputplan_date-rsnum TO l_date  DD/MM/YYYY .
        CONCATENATE 'W' w_num INTO w_field.

        PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                        'S' w_field       ' ',
                                        ' ' 'JUST'        'L',
                                        ' ' 'KEY'         ' ',
                                        ' ' 'DDICTXT'     'L',
                                        ' ' 'OUTPUTLEN'   '06',
                                        'E' 'SELTEXT_L'   l_date(5).
        w_num = w_num + 1.
        l_first = 'X'.
      ENDIF.
      IF it_inputplan_date-rsnum >= l_second_date AND
          it_inputplan_date-rsnum < l_third_date AND
          l_second IS INITIAL.
        CLEAR w_field.
        WRITE it_inputplan_date-rsnum TO l_date  DD/MM/YYYY .
        CONCATENATE 'W' w_num INTO w_field.

        PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                        'S' w_field       ' ',
                                        ' ' 'JUST'        'L',
                                        ' ' 'KEY'         ' ',
                                        ' ' 'DDICTXT'     'L',
                                        ' ' 'OUTPUTLEN'   '06',
                                        'E' 'SELTEXT_L'   l_date(5).
        w_num = w_num + 1.
        l_second = 'X'.
      ENDIF.

      IF it_inputplan_date-rsnum >= l_third_date.
        CLEAR w_field.
        WRITE it_inputplan_date-rsnum TO l_date  DD/MM/YYYY .
        CONCATENATE 'W' w_num INTO w_field.

        PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                        'S' w_field       ' ',
                                        ' ' 'JUST'        'L',
                                        ' ' 'KEY'         ' ',
                                        ' ' 'DDICTXT'     'L',
                                        ' ' 'OUTPUTLEN'   '06',
                                        'E' 'SELTEXT_L'   l_date(5).
        w_num = w_num + 1.
        EXIT.
      ENDIF.

    ENDLOOP.

*  PERFORM READ_START_DAY    USING WA_DATE   .
*  PERFORM READ_SHOP_CALID   USING WA_KALID  .
*  CLEAR W_NUM.
*  W_NUM = 1.
*  CA_DATE = WA_DATE.
*
*  DO 3 TIMES.
*    CLEAR W_FIELD.
*    CONCATENATE 'W' W_NUM INTO W_FIELD.
*    WRITE CA_DATE TO L_DATE  DD/MM/YYYY .
*    PERFORM SETTING_FIELDCAT TABLES GT_FIELDCAT USING :
*
*                                    'S' W_FIELD     ' ',
*                                    ' ' 'JUST'        'L',
*                                    ' ' 'KEY'         ' ',
*                                    ' ' 'DDICTXT'     'L',
*                                    ' ' 'OUTPUTLEN'   '06',
*                                    'E' 'SELTEXT_L'   L_DATE(05) .
*    W_NUM = W_NUM + 1.
*    PERFORM GET_WEEK USING W_NUM CA_DATE
*                     CHANGING CA_DATE.
*
*  ENDDO.
*
*** Changed by Furong on 06/16/08 "UD1K943889
*  DO 2 TIMES.
*    SELECT * INTO TABLE IT_7JB_A
*     FROM ZTPP_PMT07JB_A
*      WHERE GUBB EQ 'A'
*        AND SQDT >= CA_DATE.
*
*    IF SY-SUBRC = 0.
*      CLEAR W_FIELD.
*      CONCATENATE 'W' W_NUM INTO W_FIELD.
*      WRITE CA_DATE TO L_DATE  DD/MM/YYYY .
*      PERFORM SETTING_FIELDCAT TABLES GT_FIELDCAT USING :
*
*                                      'S' W_FIELD     ' ',
*                                      ' ' 'JUST'        'L',
*                                      ' ' 'KEY'         ' ',
*                                      ' ' 'DDICTXT'     'L',
*                                      ' ' 'OUTPUTLEN'   '06',
*                                      'E' 'SELTEXT_L'   L_DATE(05) .
*      W_NUM = W_NUM + 1.
*      PERFORM GET_WEEK USING W_NUM CA_DATE
*                       CHANGING CA_DATE.
*
*    ENDIF.
*
*    REFRESH: IT_7JB_A.
*  ENDDO.

** End of change on 06/17/09

*  REFRESH IT_7JB_DATE.
*
*  SELECT * INTO TABLE IT_7JB_A
*  FROM ZTPP_PMT07JB_A
*   WHERE GUBB EQ 'A'.
*
*  IT_7JB_DATE[] = IT_7JB_A[].
*  SORT IT_7JB_DATE BY SQDT.
*  DELETE ADJACENT DUPLICATES FROM IT_7JB_DATE COMPARING SQDT.
*
*  LOOP AT IT_7JB_DATE.
*    CALL FUNCTION 'DATE_COMPUTE_DAY'
*         EXPORTING
*              DATE = IT_7JB_DATE-SQDT
*         IMPORTING
*              DAY  = L_DAY.
*
*    IF L_DAY = '1'.
*      CLEAR W_FIELD.
*      WRITE IT_7JB_DATE-SQDT TO L_DATE  DD/MM/YYYY .
*      CONCATENATE 'W' W_NUM INTO W_FIELD.
*      PERFORM SETTING_FIELDCAT TABLES GT_FIELDCAT USING :
*                                      'S' W_FIELD       ' ',
*                                      ' ' 'JUST'        'L',
*                                      ' ' 'KEY'         ' ',
*                                      ' ' 'DDICTXT'     'L',
*                                      ' ' 'OUTPUTLEN'   '06',
*                                      'E' 'SELTEXT_L'   L_DATE(5).
*      W_NUM = W_NUM + 1.
*    ENDIF.
*  ENDLOOP.
*  REFRESH: IT_7JB_A, IT_7JB_DATE.

** end of Change

    PERFORM get_date_7jb_a.
*  CLEAR w_num.
    LOOP AT it_7jb_date.
      CLEAR w_field.
      WRITE it_7jb_date-sqdt TO l_date  DD/MM/YYYY .
      CONCATENATE 'W' w_num INTO w_field.
      PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                      'S' w_field       ' ',
                                      ' ' 'JUST'        'L',
                                      ' ' 'KEY'         ' ',
                                      ' ' 'DDICTXT'     'L',
                                      ' ' 'OUTPUTLEN'   '06',
                                      'E' 'SELTEXT_L'   l_date(5).
      w_num = w_num + 1.
    ENDLOOP.

  ELSE.
    FIELD-SYMBOLS : <fs01>, <fs02>.
    DATA: l_text(30).
    w_num = 1.
    DO 16 TIMES.
      CONCATENATE 'P_W' w_num INTO l_text.
      ASSIGN (l_text) TO <fs01>.

      CONCATENATE 'W' w_num INTO w_field.

      WRITE <fs01> TO l_date  DD/MM/YYYY .
      PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                       'S' w_field       ' ',
                                       ' ' 'JUST'        'L',
                                       ' ' 'KEY'         ' ',
                                       ' ' 'DDICTXT'     'L',
                                       ' ' 'OUTPUTLEN'   '06',
                                       'E' 'SELTEXT_L'   l_date(5).
      w_num = w_num + 1.
      CLEAR w_field.
    ENDDO.

  ENDIF.

*  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
*
*                                  'S' 'W02'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-02' ,
*
*                                  'S' 'W03'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-03' ,
*
*                                  'S' 'W04'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-04' ,
*
*                                  'S' 'W05'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-05' ,
*
*                                  'S' 'W06'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-06' ,
*
*                                  'S' 'W07'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-07' ,
*
*                                  'S' 'W08'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-08' ,
*
*                                  'S' 'W09'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-09' ,
*
*                                  'S' 'W10'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-10' ,
*
*                                  'S' 'W11'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-11' ,
*
*                                  'S' 'W12'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-12' ,
*
*                                  'S' 'W13'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-13' ,
*
*                                  'S' 'W14'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-14' ,
*
*                                  'S' 'W15'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-15' ,
*
*                                  'S' 'W16'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-16' ,
*
*                                  'S' 'W17'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-17' ,
*
*                                  'S' 'W18'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-18' ,
*
*                                  'S' 'W19'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-19' ,
*
*                                  'S' 'W20'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-20' ,
*
*                                  'S' 'W21'     ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'W-21' ,
*
*                                  'S' 'REM'    ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '05',
*                                  'E' 'SELTEXT_L'   'REM'     ,
*
*                                  'S' 'STOT'   ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'S-TOT' ,
* requested by MY. Hur changed by chris LI
  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                   'S' 'W99'     ' ',
                                   ' ' 'JUST'        'L',
                                   ' ' 'KEY'         ' ',
                                   ' ' 'DDICTXT'     'L',
                                   ' ' 'OUTPUTLEN'   '06',
                                   'E' 'SELTEXT_L'   'REM'.

* end of change on 04/04/2005

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                   'S' 'MITU'    ' ',
                                   ' ' 'JUST'        'L',
                                   ' ' 'KEY'         ' ',
                                   ' ' 'DDICTXT'     'L',
                                   ' ' 'OUTPUTLEN'   '05',
                                   'E' 'SELTEXT_L'   'MITU'.
*
*                                  'S' 'FORE'   ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '06',
*                                  'E' 'SELTEXT_L'   'FORE'    ,
*
*                                  'S' 'GTOT'   ' ',
*                                  ' ' 'JUST'        'L',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'DDICTXT'     'L',
*                                  ' ' 'OUTPUTLEN'   '07',
*                                  'E' 'SELTEXT_L'   'G-TOT'.
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
  SELECT SINGLE modl body_ser rp06
    INTO (l_model, l_body, l_rp06)
   FROM ztpp_input_plan
   WHERE serial = l_serial.

  CONCATENATE l_model l_body INTO l_bodyno.

  SELECT SINGLE atwrt INTO l_rp06_serial
    FROM ausp AS a
    INNER JOIN cabn AS b
    ON a~atinn = b~atinn
    WHERE objek = l_bodyno
      AND klart = '002'
      AND atnam = 'P_RP06_SERIAL'.

  WRITE: l_rp06+8(6) TO l_rp06_time.
  WRITE: l_rp06+0(8) TO l_rp06_date.

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
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = gt_header.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  get_week
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_NUM  text
*      <--P_WA_DATE  text
*----------------------------------------------------------------------*
FORM get_week USING    p_w_num p_wa_date
              CHANGING p_ca_date.
  DATA: l_date  TYPE d,
        week LIKE scal-week,
        l_week LIKE scal-week,
        l_ca_date TYPE d.


  l_date =   p_wa_date.

  l_date = l_date + 7.

* READ THE WORKING DATE "
* REQUESTED BY MY HUR CHANGED BY CHRIS
* NO NEED TO USE FACTORY CALENDAR
*  PERFORM read_working_date USING '+'  wa_kalid  L_DATE .

  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = l_date
    IMPORTING
      week         = week
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = week
    IMPORTING
      date         = p_ca_date
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.

ENDFORM.                    " get_week
*&---------------------------------------------------------------------*
*&      Form  get_date_7jb_a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_7jb_a.
  REFRESH it_7jb_date.

  SELECT * INTO TABLE it_7jb_a
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*  FROM ztpp_pmt07jb_a
** End
   WHERE gubb EQ 'B'.

  it_7jb_date[] = it_7jb_a[].
  SORT it_7jb_date BY sqdt.
  DELETE ADJACENT DUPLICATES FROM it_7jb_date COMPARING sqdt.

ENDFORM.                    " get_date_7jb_a
*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_MONDAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CA_DATE  text
*----------------------------------------------------------------------*
FORM get_working_monday USING    p_ca_date.

ENDFORM.                    " GET_WORKING_MONDAY
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
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
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_DATE_INPUT_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_input_plan.
  REFRESH it_inputplan_date.

  SELECT rsnum INTO TABLE it_inputplan_date
  FROM ztpp_input_plan
  WHERE rsnum <> '00000000'
** On 05/21/13
    and MITU <> 'Y'.
** End o 05/21/13
  SORT it_inputplan_date BY rsnum.
  DELETE ADJACENT DUPLICATES FROM it_inputplan_date COMPARING rsnum.

ENDFORM.                    " GET_DATE_INPUT_PLAN
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
*  LOOP AT SCREEN.
*    IF P_SD = ' '.
*      IF SCREEN-NAME = 'P_W01' OR SCREEN-NAME = 'P_W02' OR
*         SCREEN-NAME = 'P_W03' OR SCREEN-NAME = 'P_W04' OR
*         SCREEN-NAME = 'P_W05' OR SCREEN-NAME = 'P_W06' OR
*         SCREEN-NAME = 'P_W07' OR SCREEN-NAME = 'P_W08' OR
*         SCREEN-NAME = 'P_W09' OR SCREEN-NAME = 'P_W10' OR
*         SCREEN-NAME = 'P_W11' OR SCREEN-NAME = 'P_W12' OR
*         SCREEN-NAME = 'P_W13' OR SCREEN-NAME = 'P_W14' OR
*         SCREEN-NAME = 'P_W15' OR SCREEN-NAME = 'P_W16'.
*        SCREEN-INPUT = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF SCREEN-NAME = 'P_W01' OR SCREEN-NAME = 'P_W02' OR
*             SCREEN-NAME = 'P_W03' OR SCREEN-NAME = 'P_W04' OR
*             SCREEN-NAME = 'P_W05' OR SCREEN-NAME = 'P_W06' OR
*             SCREEN-NAME = 'P_W07' OR SCREEN-NAME = 'P_W08' OR
*             SCREEN-NAME = 'P_W09' OR SCREEN-NAME = 'P_W10' OR
*             SCREEN-NAME = 'P_W11' OR SCREEN-NAME = 'P_W12' OR
*             SCREEN-NAME = 'P_W13' OR SCREEN-NAME = 'P_W14' OR
*             SCREEN-NAME = 'P_W15' OR SCREEN-NAME = 'P_W16'.
*        SCREEN-INPUT = 1.
*        MODIFY SCREEN.
*      ENDIF.
*
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " check_screen
*&---------------------------------------------------------------------*
*&      Form  get_init_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_init_value.
  DATA: l_report LIKE rsvar-report,
        l_variant LIKE rsvar-variant,
        lt_rsparams LIKE TABLE OF rsparams WITH HEADER LINE.
  DATA: l_date_c(8).
  l_report = 'ZRPP203R_SEQUENCED_WEEK_PLAN'.
  l_variant = 'DEFAULT'.

  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report                      = l_report
      variant                     = l_variant
*     MOVE_OR_WRITE               = 'W'
*     NO_IMPORT                   = ' '
*     EXECUTE_DIRECT              = ' '
*   IMPORTING
*     SP                          =
    TABLES
*     L_PARAMS                    =
*     L_PARAMS_NONV               =
*     L_SELOP                     =
*     L_SELOP_NONV                =
      valutab                     =  lt_rsparams
*     OBJECTS                     =
*     FREE_SELECTIONS_DESC        =
*     FREE_SELECTIONS_VALUE       =
   EXCEPTIONS
     variant_non_existent        = 1
     variant_obsolete            = 2
     report_not_existent         = 3
     OTHERS                      = 4
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.
  LOOP AT lt_rsparams.
    CASE lt_rsparams-selname.
      WHEN 'S_DATE'.
        s_date-sign = lt_rsparams-sign.
        s_date-option = lt_rsparams-option.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                    lt_rsparams-low+3(2) INTO l_date_c.
        s_date-low = l_date_c.
        CONCATENATE lt_rsparams-high+6(4) lt_rsparams-high+0(2)
                    lt_rsparams-high+3(2) INTO l_date_c.

        s_date-high = l_date_c.
        APPEND s_date.
      WHEN 'P_W01'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                       lt_rsparams-low+3(2) INTO l_date_c.
        p_w01 = l_date_c.
      WHEN 'P_W02'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                   lt_rsparams-low+3(2) INTO l_date_c.
        p_w02 = l_date_c.
      WHEN 'P_W03'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                   lt_rsparams-low+3(2) INTO l_date_c.
        p_w03 = l_date_c.
      WHEN 'P_W04'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                   lt_rsparams-low+3(2) INTO l_date_c.
        p_w04 = l_date_c.
      WHEN 'P_W05'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w05 = l_date_c.
      WHEN 'P_W06'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w06 = l_date_c.
      WHEN 'P_W07'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w07 = l_date_c.
      WHEN 'P_W08'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w08 = l_date_c.
      WHEN 'P_W09'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w09 = l_date_c.
      WHEN 'P_W10'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w10 = l_date_c.
      WHEN 'P_W11'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w11 = l_date_c.
      WHEN 'P_W12'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w12 = l_date_c.
      WHEN 'P_W13'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w13 = l_date_c.
      WHEN 'P_W14'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w14 = l_date_c.
      WHEN 'P_W15'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w15 = l_date_c.
      WHEN 'P_W16'.
        CONCATENATE lt_rsparams-low+6(4) lt_rsparams-low+0(2)
                         lt_rsparams-low+3(2) INTO l_date_c.
        p_w16 = l_date_c.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_init_value
