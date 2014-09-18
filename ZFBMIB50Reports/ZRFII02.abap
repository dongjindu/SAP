*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 06/10/2003
*& Specification By       : hs.jeong
*& Pattern                : Report 1-2
*& Development Request No : UD1K902603
*& Addl documentation     :
*& Description  : PI Budget/Actual Report
*&
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zrfii02 MESSAGE-ID  zmfi.
TYPE-POOLS: slis, vrm.
INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.

CONSTANTS:
  c_f2code               LIKE sy-ucomm                    VALUE '&ETA'.


DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.


DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c.
*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.


*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
TABLES: aufk, impr, fmfctr, ripasw, codia.
DATA: it_out  TYPE TABLE OF impr WITH HEADER LINE.
DATA: it_imzo TYPE TABLE OF imzo WITH HEADER LINE.
DATA: it_impr TYPE TABLE OF impr WITH HEADER LINE.

*DATA : it_budget LIKE zfi_io_budget OCCURS 0 WITH HEADER LINE.
*DATA : it_actual LIKE zfi_io_actual OCCURS 0 WITH HEADER LINE.
*
DATA : it_budget LIKE zfi_pi_budget OCCURS 0 WITH HEADER LINE.
DATA : it_actual LIKE zfi_pi_actual_act OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF gt_out OCCURS 0,
        posid        LIKE  impr-posid,
        kostl        LIKE  impr-kostl,
        key(2),
        act(20),
        gjahr        LIKE  impr-gjahr,
        prnam        LIKE  impr-prnam,
        tot          LIKE  cosp-wtg001,
        before       LIKE  cosp-wtg001,
        last         LIKE  cosp-wtg001,
        year         LIKE  cosp-wtg001,
        year1        LIKE  cosp-wtg001,
        year2        LIKE  cosp-wtg001,
        year3        LIKE  cosp-wtg001,
        year4        LIKE  cosp-wtg001,
        year5        LIKE  cosp-wtg001,
        after        LIKE  cosp-wtg001,
*        POSNR        LIKE  IMPR-POSNR,
        posnr        LIKE  impr-posnr,
        aufnr         LIKE  aufk-aufnr,
        chkbox       TYPE c,
        light        TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF gt_out.
*---Temp
DATA: BEGIN OF gt_temp OCCURS 0,
        posid        LIKE  impr-posid,
        kostl        LIKE  impr-kostl,
        act(10),
        tot          LIKE  cosp-wtg001,
        before       LIKE  cosp-wtg001,
        last         LIKE  cosp-wtg001,
        year         LIKE  cosp-wtg001,
        year1        LIKE  cosp-wtg001,
        year2        LIKE  cosp-wtg001,
        year3        LIKE  cosp-wtg001,
        year4        LIKE  cosp-wtg001,
        year5        LIKE  cosp-wtg001,
        after        LIKE  cosp-wtg001,
        posnr        LIKE  impr-posnr,
        aufnr         LIKE  aufk-aufnr,
        chkbox       TYPE c,
        light        TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
**
        ktext         LIKE  aufk-ktext,
        akstl         LIKE  aufk-akstl,
**
      END OF gt_temp.
*----for combox
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.
*---WORK AREA
DATA : wa_t_cnt TYPE i,
       wa_before  LIKE  imzo-gjahr,
       wa_last    LIKE  imzo-gjahr,
       wa_year    LIKE  imzo-gjahr,
       wa_year1   LIKE  imzo-gjahr,
       wa_year2   LIKE  imzo-gjahr,
       wa_year3   LIKE  imzo-gjahr,
       wa_year4   LIKE  imzo-gjahr,
       wa_year5   LIKE  imzo-gjahr,
       wa_year6   LIKE  imzo-gjahr,
       wa_after   LIKE  imzo-gjahr,
       wa_before_txt(10),
       wa_after_txt(10).

DATA : wa_before_amt   LIKE  cosp-wtg001,
       wa_after_amt    LIKE  cosp-wtg001.
*---Currenty
DATA :  wa_n_tot          LIKE  cosp-wtg001,
        wa_n_before       LIKE  cosp-wtg001,
        wa_n_last         LIKE  cosp-wtg001,
        wa_n_year         LIKE  cosp-wtg001,
        wa_n_year1        LIKE  cosp-wtg001,
        wa_n_year2        LIKE  cosp-wtg001,
        wa_n_year3        LIKE  cosp-wtg001,
        wa_n_year4        LIKE  cosp-wtg001,
        wa_n_year5        LIKE  cosp-wtg001,
        wa_n_after        LIKE  cosp-wtg001.
*---Actual
DATA :  wa_a_tot          LIKE  cosp-wtg001,
        wa_a_before       LIKE  cosp-wtg001,
        wa_a_last         LIKE  cosp-wtg001,
        wa_a_year         LIKE  cosp-wtg001,
        wa_a_year1        LIKE  cosp-wtg001,
        wa_a_year2        LIKE  cosp-wtg001,
        wa_a_year3        LIKE  cosp-wtg001,
        wa_a_year4        LIKE  cosp-wtg001,
        wa_a_year5        LIKE  cosp-wtg001,
        wa_a_after        LIKE  cosp-wtg001.
*---Commitment
DATA :  wa_c_tot          LIKE  cosp-wtg001,
        wa_c_before       LIKE  cosp-wtg001,
        wa_c_last         LIKE  cosp-wtg001,
        wa_c_year         LIKE  cosp-wtg001,
        wa_c_year1        LIKE  cosp-wtg001,
        wa_c_year2        LIKE  cosp-wtg001,
        wa_c_year3        LIKE  cosp-wtg001,
        wa_c_year4        LIKE  cosp-wtg001,
        wa_c_year5        LIKE  cosp-wtg001,
        wa_c_after        LIKE  cosp-wtg001.
*---Downpayment
DATA :  wa_d_tot          LIKE  cosp-wtg001,
        wa_d_before       LIKE  cosp-wtg001,
        wa_d_last         LIKE  cosp-wtg001,
        wa_d_year         LIKE  cosp-wtg001,
        wa_d_year1        LIKE  cosp-wtg001,
        wa_d_year2        LIKE  cosp-wtg001,
        wa_d_year3        LIKE  cosp-wtg001,
        wa_d_year4        LIKE  cosp-wtg001,
        wa_d_year5        LIKE  cosp-wtg001,
        wa_d_after        LIKE  cosp-wtg001.
DATA :  wa_gjahr        LIKE  impr-gjahr,
        wa_prnam        LIKE  impr-prnam.
*----2004/03/24
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/08, by WSKIM
*---Start
RANGES r_posid FOR impr-posid.
*---End
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE c010.
SELECT-OPTIONS: s_prnam FOR impr-prnam MEMORY ID imt OBLIGATORY
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
                            DEFAULT 'HMMA0001'.
*---End
PARAMETERS:  p_ayear LIKE impr-gjahr   MEMORY ID gjr OBLIGATORY
                                      DEFAULT sy-datum+0(4),

             p_fiscal  LIKE impr-gjahr.

PARAMETERS: p_auth(1) TYPE c DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK s1.

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE c020.
** overall
**SELECTION-SCREEN      BEGIN OF BLOCK jhr.
**SELECTION-SCREEN        BEGIN OF LINE.
**PARAMETER:            p_oall LIKE raip1-geswt DEFAULT 'X'
**                      RADIOBUTTON GROUP wrt.
**SELECTION-SCREEN      COMMENT (25) p03
**                      FOR FIELD p_oall.
**SELECTION-SCREEN        END   OF LINE.
** year
**SELECTION-SCREEN        BEGIN OF LINE.
**PARAMETER:            p_year LIKE raip1-jhrwt
**                      RADIOBUTTON GROUP wrt.
**SELECTION-SCREEN      COMMENT (25) p04
**                      FOR FIELD p_year.
**SELECTION-SCREEN      POSITION 33.
**PARAMETER:            p_gjahr   LIKE bpja-gjahr.
**SELECTION-SCREEN        END  OF LINE.
**SELECTION-SCREEN      END   OF BLOCK jhr.

SELECTION-SCREEN END OF BLOCK s2.
*--------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE c030.
PARAMETERS : p_r1       RADIOBUTTON GROUP g1,
             p_r2       RADIOBUTTON GROUP g1.

PARAMETERS : p_posid LIKE impr-posid.
SELECT-OPTIONS:
  s_kostl   FOR   impr-kostl.
SELECTION-SCREEN END OF BLOCK s3.
*------------------------------------------------*
PARAMETERS: p_act(2) TYPE c AS LISTBOX VISIBLE LENGTH 25
               DEFAULT '01'. " OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE c040.
PARAMETERS : p_chk1 AS CHECKBOX ,"DEFAULT 'X',
             p_chk2 AS CHECKBOX ."DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK s4.

SELECT-OPTIONS: so_ippos FOR ripasw-ippos.

SELECTION-SCREEN BEGIN OF BLOCK s5 WITH FRAME TITLE text-030.
PARAMETERS : p_low  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK s5.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS :
  p_layout LIKE disvariant-variant.   "LAYOUT
SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.
*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f4_variant CHANGING p_layout.
*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
INITIALIZATION.
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
*  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.
*--title set
  c010 = 'Run Parameter'.
  c020 = 'Select option'.
  c030 = 'Select option'.
*  p03  = 'Overall values'.
*  p04  = 'Annual values (year)'.
  wa_repid = sy-repid.

* for combo box
  REFRESH : it_val.
  CLEAR   : it_val.
  w_line-key  = '01'.
  w_line-text = 'All'.
  APPEND w_line TO it_val.
  w_line-key  = '02'.
  w_line-text = 'Plan'.
  APPEND w_line TO it_val.
  w_line-key  = '03'.
  w_line-text = 'Ori Bud'.
  APPEND w_line TO it_val.
  w_line-key  = '04'.
  w_line-text = 'Supplement'.
  APPEND w_line TO it_val.
  w_line-key  = '05'.
  w_line-text = 'Return'.
  APPEND w_line TO it_val.
  w_line-key  = '06'.
  w_line-text = 'Current Budget'.
  APPEND w_line TO it_val.
  w_line-key  = '07'.
  w_line-text = 'Actual'.
  APPEND w_line TO it_val.
  w_line-key  = '08'.
  w_line-text = 'Comm'.
  APPEND w_line TO it_val.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
*  w_line-key  = '09'.
*  w_line-text = 'Downpayment'.
*  APPEND w_line TO it_val.
*  w_line-key  = '10'.
*  w_line-text = 'Residual'.
  w_line-key  = '09'.
  w_line-text = 'Available'.
  APPEND w_line TO it_val.
  w_line-key  = '10'.
  w_line-text = 'Downpayment'.
*---End
  APPEND w_line TO it_val.
*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
END-OF-SELECTION.
  IF p_r2 = 'X'.
    EXPORT s_prnam  p_ayear p_act  p_posid s_kostl it_val
           p_chk1 p_chk2   p_fiscal p_low TO MEMORY.
    SUBMIT zrfii02s   AND RETURN.
    EXIT.
  ENDIF.

  PERFORM set_year.
* ==> 5. build field category
  PERFORM build_field_category
  USING :
   'POSID'  'Position ID'   '16' 'X' 'C'  ' '  ' '  '  ' '  ' 'CHAR',
   'KOSTL'  'CostCener'     '10' 'X' 'L'  ' '  ' '  '  ' '  ' 'CHAR',
   'ACT'    'Activity'      '16' 'X' 'L'  ' '  ' '  '  ' '  ' 'CHAR',
   'TOT'    'Total'         '15' 'X' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'BEFORE' wa_before_txt   '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR' ,
   'LAST'   wa_last         '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR'   wa_year         '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR1'  wa_year1        '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR2'  wa_year2        '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR3'  wa_year3        '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR4'  wa_year4        '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'YEAR5'  wa_year5        '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR',
   'AFTER'  wa_after_txt    '15' ' ' 'R'  ' '  ' '  '  ' '  ' 'CURR'.
* ==> 6. build sorts info
  REFRESH gt_sorts.
  PERFORM build_sort_table
    USING :
       '1'    'POSID'   'X'   'X'   '*'.
* ==> 1. select data from db
  PERFORM select_data.
  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No found data '.
    EXIT.
  ENDIF.
* ==> 2. set variant default
  PERFORM set_variant CHANGING wa_var.
* ==> 3. set layout for alv style
  PERFORM set_layout CHANGING gs_layout.
* ==> 4. set events for alv
  PERFORM set_events CHANGING gt_events.
*===> 5. set event for top-of-page grid.
  PERFORM set_build_event.
*===>
  PERFORM comment_build USING  w_top_of_page[].

* ==> 7. call function display alv.

  CALL FUNCTION wa_alv_function_name
    EXPORTING
         i_callback_program      = wa_repid
         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout               = gs_layout
         it_fieldcat             = gt_fieldcat[]
         it_special_groups       = gt_sp_group[]
         it_sort                 = gt_sorts[]
*         IT_FILTER               =
         i_default               = wa_default
         i_save                  = wa_var_save
         is_variant              = wa_var
*         it_events               = gt_events[]
         it_events               =  w_eventcat[]
         is_print                = gs_prnt
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         t_outtab                = gt_out.
***********************************************************************
*

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
  DATA : wa_posid(20),
         wa_pos TYPE i.
**Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/09, by WSKIM
*---Start
*  CONCATENATE p_posid '%'  INTO wa_posid.
  PERFORM range_posid USING p_posid.
*---End
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_out  FROM impr
        WHERE gjahr = p_ayear
*---Start
*          AND posid LIKE wa_posid
          AND  posid IN r_posid
*---End
          AND prnam IN s_prnam
          AND kostl IN s_kostl.
*---make gt_out by activity
  LOOP AT it_out.
*---2003/12/16 jh smodify
    IF p_low = 'X'.   "low level pi
      CHECK it_out-posid+7(4) <> 0000.
    ENDIF.
    PERFORM get_activity
        USING  it_out-kostl it_out-posid it_out-gjahr it_out-prnam.

  ENDLOOP.

  SORT gt_out BY posid key kostl ASCENDING.
  LOOP AT gt_out.
    MOVE gt_out-gjahr TO wa_gjahr.
    MOVE gt_out-prnam TO wa_prnam.
    AT NEW posid.
      PERFORM cal_pi_budget USING gt_out-posid
                                  wa_gjahr
                                  wa_prnam.
      PERFORM cal_pi_actual USING gt_out-posid
                                  wa_gjahr
                                  wa_prnam.

      CLEAR : wa_gjahr, wa_prnam.
    ENDAT.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
    IF p_fiscal <> space.
      MOVE p_fiscal TO gt_out-gjahr.
    ENDIF.
*---End
    IF gt_out-key =< 06.
      PERFORM get_pi_budget USING gt_out-posid gt_out-key
                                  gt_out-gjahr gt_out-prnam.
    ELSEIF gt_out-key > 06.
      PERFORM get_pi_actual USING gt_out-posid gt_out-key
                                  gt_out-gjahr gt_out-prnam.
    ENDIF.
*---CAL Available.

*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*  IF gt_out-key = '10'.
    IF gt_out-key = '09'.
*      IF p_chk2 = 'X' AND p_chk1 = ' '.
*        PERFORM get_pi_budget2 USING  gt_out-posid gt_out-key
*                                      gt_out-gjahr gt_out-prnam.
*---End

      gt_out-tot    =  wa_n_tot    - wa_a_tot    - wa_c_tot.
      gt_out-before =  wa_n_before - wa_a_before - wa_c_before.
      gt_out-last   =  wa_n_last   - wa_a_last   - wa_c_last.
      gt_out-year   =  wa_n_year   - wa_a_year   - wa_c_year.
      gt_out-year1  =  wa_n_year1  - wa_a_year1  - wa_c_year1.
      gt_out-year2  =  wa_n_year2  - wa_a_year2  - wa_c_year2.
      gt_out-year3  =  wa_n_year3  - wa_a_year3  - wa_c_year3.
      gt_out-year4  =  wa_n_year4  - wa_a_year4  - wa_c_year4.
      gt_out-year5  =  wa_n_year5  - wa_a_year5  - wa_c_year5.
      gt_out-after  =  wa_n_after  - wa_a_after  - wa_c_after.
*---Start
      IF p_fiscal <> space.
        gt_out-tot    = gt_out-before + gt_out-last + gt_out-year
                      + gt_out-year1  + gt_out-year2 + gt_out-year3
                      + gt_out-year4  + gt_out-year5 + gt_out-after  .

      ENDIF.
*---End
      MODIFY gt_out.

*---Start
*begin insert
*      PERFORM clear_dummy.

    ELSEIF  p_fiscal NE space AND ( gt_out-key = '02' OR
        gt_out-key = '03' OR gt_out-key = '04' OR gt_out-key = '05'
            OR gt_out-key = '06' ).
      gt_out-tot =  gt_out-last + gt_out-year + gt_out-year1 +
                    gt_out-year2 + gt_out-year3 + gt_out-year4 +
                    gt_out-year5 + gt_out-before + gt_out-after.
      MODIFY gt_out.

*---End
*---Start
*      ENDIF.
*      PERFORM clear_dummy.
    ENDIF.
    AT END OF  posid.
      PERFORM clear_dummy.
    ENDAT.
*---End
  ENDLOOP.
*------2003/04/16
  IF p_chk1 =  ' ' AND p_chk2 = ' '.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    IF p_act = '10'.
    IF p_act = '09'.
*---End
      LOOP AT gt_out.
*---Start
*        IF gt_out-key = '10'.
        IF gt_out-key = '09'.
*---End
          CONTINUE.
        ELSE.
          DELETE gt_out.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  SORT gt_out ASCENDING BY posid kostl key.
ENDFORM.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM alv_event_pf_status_set USING rt_extab TYPE slis_t_extab.
                                                            "#EC *
  IF wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.


ENDFORM.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM alv_event_user_command USING r_ucomm     LIKE sy-ucomm
                                      rs_selfield TYPE slis_selfield.
                                                            "#EC *


  CASE r_ucomm.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE gt_out INDEX rs_selfield-tabindex.
      CASE rs_selfield-fieldname.
*        WHEN 'AUFNR'.
*          SET PARAMETER ID 'ANR' FIELD gt_out-aufnr.
*          CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
        WHEN 'ACT'.
          IF gt_out-key = '02'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'IM36' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '03'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'IM33' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '04'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'IM31' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key = '05'.
            SET PARAMETER ID 'IMT' FIELD gt_out-prnam.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'IM39' AND SKIP FIRST SCREEN.
          ELSEIF gt_out-key =  '07' OR gt_out-key = '08' OR
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*                 gt_out-key = '09'.
          gt_out-key = '09' or  gt_out-key = '10' or  gt_out-key = '06'.
*---End
            SET PARAMETER ID 'IMT' FIELD s_prnam-low.
            SET PARAMETER ID 'IMP' FIELD gt_out-posid.
            SET PARAMETER ID 'GJR' FIELD p_ayear.
            CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'POSID'.
          SET PARAMETER ID 'IMT' FIELD s_prnam-low.
          SET PARAMETER ID 'IMP' FIELD gt_out-posid.
          SET PARAMETER ID 'GJR' FIELD p_ayear.
          SET PARAMETER ID 'IPPOS' FIELD so_ippos-low.
          CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
      ENDCASE.
*   ---------------------------------- switching view type grid or list
    WHEN 'LIST' OR 'GRID'.
      PERFORM switch_list_or_grid USING r_ucomm.
  ENDCASE.

  CHECK r_ucomm EQ 'LIST' OR
        r_ucomm EQ 'GRID'.

  rs_selfield-exit = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
FORM set_variant CHANGING cs_vari TYPE disvariant.

  CHECK p_layout NE space.

  cs_vari-report      = sy-repid.
  cs_vari-handle      = space.
  cs_vari-log_group   = space.
  cs_vari-username    = space.
  cs_vari-variant     = p_layout.
  cs_vari-text        = space.
  cs_vari-dependvars  = space.

ENDFORM.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM set_events CHANGING ct_events TYPE slis_t_event.

  FIELD-SYMBOLS: <ls_event> TYPE slis_alv_event.

  DATA: l_event TYPE lvc_fname.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type     = 0
       IMPORTING
            et_events       = ct_events
       EXCEPTIONS
            list_type_wrong = 1
            OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    DELETE ct_events WHERE name NE 'END_OF_PAGE'
                       AND name NE 'TOP_OF_PAGE'
                       AND name NE 'TOP_OF_LIST'
                       AND name NE 'END_OF_LIST'.
    LOOP AT ct_events ASSIGNING <ls_event>.
      CONCATENATE 'ALV_EVENT_'
                  <ls_event>-name
                  INTO <ls_event>-form.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM set_layout CHANGING cs_layo TYPE slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space.
  "?????
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = ' '.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = ' '.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM alv_event_top_of_page.                                 "#EC CALLED
*  WRITE : /(10) 'nvestment Program' , p_prnam.
*          /(10) 'BBBBBBB',  BKPF-BUKRS INVERSE COLOR 1 INPUT ON,
*           (20) 'CCCCCCC',  BKPF-BELNR INPUT ON.
ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM alv_event_top_of_list.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM alv_event_end_of_page.
*  NEW-LINE.
*  ULINE.
*  DATA: l_page(10).
*  WRITE : sy-pagno TO l_page.
*  WRITE: /(120) l_page CENTERED.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM alv_event_end_of_list.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM display_header.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
              it_list_commentary = w_top_of_page.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM switch_list_or_grid USING r_ucomm.

  DATA: ls_vari      TYPE disvariant,
       ls_slis_layo TYPE slis_layout_alv,
       lt_slis_fcat TYPE slis_t_fieldcat_alv,
       lt_slis_sort TYPE slis_t_sortinfo_alv,
       lt_slis_filt TYPE slis_t_filter_alv,
       ls_slis_prnt TYPE slis_print_alv.


  IF r_ucomm = 'LIST' AND
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF r_ucomm = 'GRID' AND
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE wa_alv_function_name.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION wa_alv_get_info_name
       IMPORTING
            es_layout     = ls_slis_layo
            et_fieldcat   = lt_slis_fcat
            et_sort       = lt_slis_sort
            et_filter     = lt_slis_filt
            es_variant    = ls_vari
       EXCEPTIONS
            no_infos      = 1
            program_error = 2
            OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
              it_events                = gt_events[]
         TABLES
              t_outtab                 = gt_out
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.
  ENDIF.
  IF r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION wa_alv_function_name
         EXPORTING
              i_callback_program       = wa_repid
              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
              is_layout                = ls_slis_layo
              it_fieldcat              = lt_slis_fcat
              it_sort                  = lt_slis_sort
              it_filter                = lt_slis_filt
              i_default                = ' '  "gs_test-vari_default
              i_save                   = wa_var_save
              is_variant               = ls_vari
              is_print                 = ls_slis_prnt
*                it_events               = gt_events[]
         TABLES
              t_outtab                 = gt_out
         EXCEPTIONS
              program_error            = 1
              OTHERS                   = 2.

  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM build_field_category USING
                                  p_fieldname       " field name
                                  p_title           " field title
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_type            " Type
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-datatype   = p_type.
  IF p_fieldname = 'TOT'.
    ls_fieldcat-emphasize = 'C700'.
  ENDIF.
  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM f4_variant CHANGING c_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant,
        l_exit     TYPE char1.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = ls_variant
            i_save              = 'A'
*           it_default_fieldcat =
       IMPORTING
            e_exit              = l_exit
            es_variant          = ls_variant
       EXCEPTIONS
            not_found = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF l_exit EQ space.
      c_variant = ls_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM build_sort_table USING  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  DATA: ls_sort TYPE slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  APPEND ls_sort TO gt_sorts.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM set_line_color USING    p_color.
  DATA: ls_fieldcat   TYPE slis_fieldcat_alv,
        lt_color      TYPE slis_t_specialcol_alv,
        ls_color      TYPE slis_specialcol_alv.

  REFRESH lt_color.
  CLEAR   lt_color.
  LOOP AT gt_fieldcat INTO ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    APPEND ls_color TO lt_color.
    gt_out-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
FORM build_field_category1 USING
                                  p_fieldname       " field name
                                  p_title           " field title
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = p_fieldname.
  ls_fieldcat-seltext_l = p_title.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  ls_fieldcat-just      = p_just.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  APPEND ls_fieldcat TO gt_fieldcat.
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  GET_ACTIVITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*      -->P_GT_OUT_AKSTL  text
*----------------------------------------------------------------------*
FORM get_activity USING    u_kostl
                           u_posid
                           u_gjahr
                           u_prnam.
*-----Budget / Actual
  IF p_chk1 = 'X' AND p_chk2 = ' '.      " budget
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '02'       TO   gt_out-key.
    MOVE 'Plan'     TO   gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '03'         TO gt_out-key.
    MOVE 'Original Budget'   TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '04'           TO gt_out-key.
    MOVE 'Supplement'  TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '05'         TO gt_out-key.
    MOVE 'Return'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '06'         TO gt_out-key.
    MOVE 'Current Budget'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
  ELSEIF p_chk2 = 'X' AND p_chk1 = ' '.    "actual
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '07'         TO gt_out-key.
    MOVE 'Actual'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '08'         TO gt_out-key.
    MOVE 'Commitment'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '09'         TO gt_out-key.
    MOVE '10'         TO gt_out-key.
*---End
    MOVE 'Downpayment'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '10'         TO gt_out-key.
*    MOVE 'Residual'      TO gt_out-act.
    MOVE '09'         TO gt_out-key.
    MOVE 'Available'   TO gt_out-act.
*---End
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
  ELSEIF p_chk1 = 'X' AND p_chk2 = 'X'.    " bugget + budget
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '02'       TO   gt_out-key.
    MOVE 'Plan'     TO   gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '03'         TO gt_out-key.
    MOVE 'Original Budget'   TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '04'           TO gt_out-key.
    MOVE 'Supplement'  TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '05'         TO gt_out-key.
    MOVE 'Return'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '06'         TO gt_out-key.
    MOVE 'Current Budget'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '07'         TO gt_out-key.
    MOVE 'Actual'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
    MOVE '08'         TO gt_out-key.
    MOVE 'Commitment'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '09'         TO gt_out-key.
    MOVE '10'         TO gt_out-key.
*---End
    MOVE 'Downpayment'      TO gt_out-act.
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.
    CLEAR  gt_out.
    MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '10'         TO gt_out-key.
*    MOVE 'Residual'      TO gt_out-act.
    MOVE '09'         TO gt_out-key.
    MOVE 'Available'   TO gt_out-act.
*---End
    MOVE u_posid    TO   gt_out-posid.
    MOVE u_gjahr    TO   gt_out-gjahr.
    MOVE u_prnam TO gt_out-prnam.
    APPEND gt_out.
    CLEAR  gt_out.

  ELSE.
    READ TABLE it_val WITH KEY key = p_act INTO w_line.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    IF w_line-key = '01'  OR w_line-key = '10'.
    IF w_line-key = '01'  OR w_line-key = '09'.
*---End
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '02'         TO gt_out-key.
      MOVE 'Plan'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '03'         TO gt_out-key.
      MOVE 'Original Budget'  TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '04'           TO gt_out-key.
      MOVE 'Supplement'  TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '05'         TO gt_out-key.
      MOVE 'Return'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '06'         TO gt_out-key.
      MOVE 'Current Budget'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '07'         TO gt_out-key.
      MOVE 'Actual'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE '08'         TO gt_out-key.
      MOVE 'Commitment'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '09'         TO gt_out-key.
      MOVE '10'         TO gt_out-key.
*---End
      MOVE 'Downpayment'      TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
      CLEAR  gt_out.
      MOVE u_kostl    TO   gt_out-kostl.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*    MOVE '10'         TO gt_out-key.
*    MOVE 'Residual'      TO gt_out-act.
      MOVE '09'         TO gt_out-key.
      MOVE 'Available'   TO gt_out-act.
*---End
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
    ELSE.
      MOVE u_kostl    TO   gt_out-kostl.
      MOVE w_line-key         TO gt_out-key.
      MOVE w_line-text        TO gt_out-act.
      MOVE u_posid    TO   gt_out-posid.
      MOVE u_gjahr    TO   gt_out-gjahr.
      MOVE u_prnam TO gt_out-prnam.
      APPEND gt_out.
      CLEAR  gt_out.
    ENDIF.

  ENDIF.
ENDFORM.                    " GET_ACTIVITY
*&---------------------------------------------------------------------*
*&      Form  set_year
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_year.
  CLEAR : wa_before, wa_last, wa_year, wa_year1, wa_year2,
          wa_year3.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/11/30, by WSKIM
*---Start
  DATA : wa_llast LIKE wa_last,
         wa_year6 LIKE wa_last.
*---End
  wa_last  = p_ayear - 1.
*---Start
  wa_llast = wa_last - 1.
*---End
  wa_year  = p_ayear.
  wa_year1 = p_ayear + 1.
  wa_year2 = p_ayear + 2.
  wa_year3 = p_ayear + 3.
  wa_year4 = p_ayear + 4.
  wa_year5 = p_ayear + 5.
*---Start
  wa_year6 = wa_year5 + 1.
*  CONCATENATE '~' wa_last  INTO wa_before_txt.
*  CONCATENATE wa_year5 '~' INTO wa_after_txt.
  CONCATENATE '~' wa_llast  INTO wa_before_txt.
  CONCATENATE wa_year6 '~' INTO wa_after_txt.
*---End
ENDFORM.                    " set_year
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING  lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
          l_manager(50),
          l_date(50),
          l_list(50),
          l_dsnam LIKE t024d-dsnam,
          l_h_dsnam LIKE t024d-dsnam,
          l_ldate(10),
          l_hdate(10).
*-------------- HEADER
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.     "HEADER TITLE (H001)
  APPEND ls_line TO lt_top_of_page.

  ls_line-typ  = 'S'.
  ls_line-key  = 'Investment program : '.
  ls_line-info = s_prnam-low.
  APPEND ls_line TO lt_top_of_page.
*--
  ls_line-typ  = 'S'.
  ls_line-key  = 'Approval Year : '.
  ls_line-info = p_ayear.
  APPEND ls_line TO lt_top_of_page.
*--
  ls_line-typ  = 'S'.
  ls_line-key  = 'Position ID : '.
  CONCATENATE   p_posid  ' ~'  p_posid INTO l_list.
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'DISPLAY_HEADER'.
  APPEND w_eventcat.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DUMMY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_dummy.
  CLEAR : wa_n_tot, wa_n_before, wa_n_last, wa_n_year, wa_n_year1,
          wa_n_year2, wa_n_year3, wa_n_year4, wa_n_year4,
          wa_n_year5, wa_n_after.
  CLEAR : wa_a_tot, wa_a_before, wa_a_last, wa_a_year, wa_a_year1,
          wa_a_year2, wa_a_year3, wa_a_year4, wa_a_year4,
          wa_a_year5, wa_a_after.
  CLEAR : wa_c_tot, wa_c_before, wa_c_last, wa_c_year, wa_c_year1,
          wa_c_year2, wa_c_year3, wa_c_year4, wa_c_year4,
          wa_c_year5, wa_c_after.
  CLEAR : wa_d_tot, wa_d_before, wa_d_last, wa_d_year, wa_d_year1,
          wa_d_year2, wa_d_year3, wa_d_year4, wa_d_year4,
          wa_d_year5, wa_d_after.
ENDFORM.                    " CLEAR_DUMMY
*&---------------------------------------------------------------------*
*&      Form  get_pi_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_KEY  text
*      -->P_GT_OUT_GJAHR  text
*      -->P_GT_OUT_PRNAM  text
*----------------------------------------------------------------------*
FORM get_pi_budget USING    u_posid
                            u_key
                            u_gjahr
                            u_prnam.
  LOOP AT it_budget.
    IF p_fiscal <> ' '.
      CHECK it_budget-gjahr = p_fiscal ."or it_budget-gjahr = '1111'.
    ENDIF.
    CASE u_key.
      WHEN '02'.       "Plan
        CASE it_budget-gjahr.
          WHEN '1111'.
            ADD  it_budget-plan    TO   gt_out-tot.
          WHEN wa_last.
            ADD  it_budget-plan    TO   gt_out-last.
          WHEN wa_year.
            ADD  it_budget-plan    TO   gt_out-year.
          WHEN wa_year1.
            ADD  it_budget-plan    TO   gt_out-year1.
          WHEN wa_year2.
            ADD  it_budget-plan    TO   gt_out-year2.
          WHEN wa_year3.
            ADD  it_budget-plan    TO   gt_out-year3.
          WHEN wa_year4.
            ADD  it_budget-plan    TO   gt_out-year4.
          WHEN wa_year5.
            ADD  it_budget-plan    TO   gt_out-year5.
          WHEN OTHERS.
            IF it_budget-gjahr < wa_last.
              ADD it_budget-plan  TO   gt_out-before.
            ELSEIF it_budget-gjahr > wa_year5.
              ADD it_budget-plan  TO   gt_out-after.
            ENDIF.
        ENDCASE.
      WHEN '03'.      "Org
        CASE it_budget-gjahr.
          WHEN '1111'.
            ADD  it_budget-org     TO   gt_out-tot.
          WHEN wa_before.
            ADD  it_budget-org     TO   gt_out-before.
          WHEN wa_last.
            ADD  it_budget-org     TO   gt_out-last.
          WHEN wa_year.
            ADD  it_budget-org     TO   gt_out-year.
          WHEN wa_year1.
            ADD  it_budget-org     TO   gt_out-year1.
          WHEN wa_year2.
            ADD  it_budget-org     TO   gt_out-year2.
          WHEN wa_year3.
            ADD  it_budget-org     TO   gt_out-year3.
          WHEN wa_year4.
            ADD  it_budget-org     TO   gt_out-year4.
          WHEN wa_year5.
            ADD  it_budget-org     TO   gt_out-year5.
          WHEN OTHERS.
            IF it_budget-gjahr < wa_last.
              ADD it_budget-org  TO   gt_out-before.
            ELSEIF it_budget-gjahr > wa_year5.
              ADD it_budget-org   TO   gt_out-after.
            ENDIF.
        ENDCASE.
      WHEN '04'.      "Supplement
        CASE it_budget-gjahr.
          WHEN '1111'.
            ADD  it_budget-supp    TO   gt_out-tot.
          WHEN wa_before.
            ADD  it_budget-supp    TO   gt_out-before.
          WHEN wa_last.
            ADD  it_budget-supp    TO   gt_out-last.
          WHEN wa_year.
            ADD  it_budget-supp    TO   gt_out-year.
          WHEN wa_year1.
            ADD  it_budget-supp    TO   gt_out-year1.
          WHEN wa_year2.
            ADD  it_budget-supp    TO   gt_out-year2.
          WHEN wa_year3.
            ADD  it_budget-supp    TO   gt_out-year3.
          WHEN wa_year4.
            ADD  it_budget-supp    TO   gt_out-year4.
          WHEN wa_year5.
            ADD  it_budget-supp    TO   gt_out-year5.
          WHEN OTHERS.
            IF it_budget-gjahr < wa_last.
              ADD it_budget-supp TO   gt_out-before.
            ELSEIF it_budget-gjahr > wa_year5.
              ADD it_budget-supp  TO   gt_out-after.
            ENDIF.
        ENDCASE.
      WHEN '05'.      "Ret
        CASE it_budget-gjahr.
          WHEN '1111'.
            ADD  it_budget-ret     TO   gt_out-tot.
          WHEN wa_before.
            ADD  it_budget-ret     TO   gt_out-before.
          WHEN wa_last.
            ADD  it_budget-ret     TO   gt_out-last.
          WHEN wa_year.
            ADD  it_budget-ret     TO   gt_out-year.
          WHEN wa_year1.
            ADD  it_budget-ret     TO   gt_out-year1.
          WHEN wa_year2.
            ADD  it_budget-ret     TO   gt_out-year2.
          WHEN wa_year3.
            ADD  it_budget-ret     TO   gt_out-year3.
          WHEN wa_year4.
            ADD  it_budget-ret     TO   gt_out-year4.
          WHEN wa_year5.
            ADD  it_budget-ret     TO   gt_out-year5.
          WHEN OTHERS.
            IF it_budget-gjahr < wa_last.
              ADD it_budget-ret  TO   gt_out-before.
            ELSEIF it_budget-gjahr > wa_year5.
              ADD it_budget-ret   TO   gt_out-after.
            ENDIF.
        ENDCASE.
      WHEN '06'.      "Curre Bud
        CASE it_budget-gjahr.
          WHEN '1111'.
            gt_out-tot  =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_tot = gt_out-tot.
          WHEN wa_before.
            gt_out-before  =
                   it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_before  = gt_out-before.
          WHEN wa_last.
            gt_out-last =
                   it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_last = gt_out-last.
          WHEN wa_year.
            gt_out-year =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year = gt_out-year.
          WHEN wa_year1.
            gt_out-year1 =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year1 = gt_out-year1.
          WHEN wa_year2.
            gt_out-year2 =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year2 = gt_out-year2.
          WHEN wa_year3.
            gt_out-year3 =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year3 = gt_out-year3.
          WHEN wa_year4.
            gt_out-year4 =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year4 = gt_out-year4.
          WHEN wa_year5.
            gt_out-year5 =
                 it_budget-org + it_budget-supp + it_budget-ret.
            wa_n_year5 = gt_out-year5.
          WHEN OTHERS.
            IF it_budget-gjahr < wa_last.
              gt_out-before = gt_out-before + it_budget-org
                            + it_budget-supp + it_budget-ret.
              wa_n_before = gt_out-before.
            ELSEIF it_budget-gjahr > wa_year5.
              gt_out-after  = gt_out-after  + it_budget-org
                            + it_budget-supp + it_budget-ret.
              wa_n_after = gt_out-after.
            ENDIF.
        ENDCASE.
    ENDCASE.
    MODIFY gt_out.
  ENDLOOP.
ENDFORM.                    " get_pi_budget
*&---------------------------------------------------------------------*
*&      Form  get_pi_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_KEY  text
*----------------------------------------------------------------------*
FORM get_pi_actual USING    u_posid
                            u_key
                            u_gjahr
                            u_prnam.
  LOOP AT it_actual WHERE ippos <>  0.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
    IF p_fiscal <> ' '.
      CHECK it_actual-gjahr = p_fiscal  .
    ENDIF.
*---End
    CASE u_key.
      WHEN '07'.       "Actual
        CASE it_actual-wrttp.
          WHEN '04' OR '11'.
            ADD it_actual-tot     TO   gt_out-tot.
            ADD it_actual-tot     TO   wa_a_tot.
            CASE it_actual-gjahr.
              WHEN wa_last.
                ADD  it_actual-tot     TO   gt_out-last.
                ADD  it_actual-tot     TO   wa_a_last.
              WHEN wa_year.
                ADD  it_actual-tot     TO   gt_out-year.
                ADD  it_actual-tot     TO   wa_a_year.
              WHEN wa_year1.
                ADD  it_actual-tot     TO   gt_out-year1.
                ADD  it_actual-tot     TO   wa_a_year1.
              WHEN wa_year2.
                ADD  it_actual-tot     TO   gt_out-year2.
                ADD  it_actual-tot     TO   wa_a_year2.
              WHEN wa_year3.
                ADD  it_actual-tot     TO   gt_out-year3.
                ADD  it_actual-tot     TO   wa_a_year3.
              WHEN wa_year4.
                ADD  it_actual-tot     TO   gt_out-year4.
                ADD  it_actual-tot     TO   wa_a_year4.
              WHEN wa_year5.
                ADD  it_actual-tot     TO   gt_out-year5.
                ADD  it_actual-tot     TO   wa_a_year5.
              WHEN OTHERS.
                IF it_actual-gjahr < wa_last.
                  ADD it_actual-tot    TO   gt_out-before.
                  ADD it_actual-tot    TO   wa_a_before.
                ELSEIF it_actual-gjahr > wa_year5.
                  ADD it_actual-tot    TO   gt_out-after.
                  ADD it_actual-tot    TO   wa_a_after.
                ENDIF.
            ENDCASE.
        ENDCASE.
      WHEN '08'.       "Commitment
        CASE it_actual-wrttp.
          WHEN '22' OR '21'.
            ADD it_actual-tot     TO   gt_out-tot.
            ADD it_actual-tot     TO   wa_c_tot.
            CASE it_actual-gjahr.
              WHEN wa_before.
                ADD  it_actual-tot     TO   gt_out-before.
                ADD  it_actual-tot     TO   wa_c_before.
              WHEN wa_last.
                ADD  it_actual-tot     TO   gt_out-last.
                ADD  it_actual-tot     TO   wa_c_last.
              WHEN wa_year.
                ADD  it_actual-tot     TO   gt_out-year.
                ADD  it_actual-tot     TO   wa_c_year.
              WHEN wa_year1.
                ADD  it_actual-tot     TO   gt_out-year1.
                ADD  it_actual-tot     TO   wa_c_year1.
              WHEN wa_year2.
                ADD  it_actual-tot     TO   gt_out-year2.
                ADD  it_actual-tot     TO   wa_c_year2.
              WHEN wa_year3.
                ADD  it_actual-tot     TO   gt_out-year3.
                ADD  it_actual-tot     TO   wa_c_year3.
              WHEN wa_year4.
                ADD  it_actual-tot     TO   gt_out-year4.
                ADD  it_actual-tot     TO   wa_c_year4.
              WHEN wa_year5.
                ADD  it_actual-tot     TO   gt_out-year5.
                ADD  it_actual-tot     TO   wa_c_year5.
              WHEN OTHERS.
                IF it_actual-gjahr < wa_last.
                  ADD it_actual-tot    TO   gt_out-before.
                  ADD it_actual-tot    TO   wa_c_before.
                ELSEIF it_actual-gjahr > wa_year5.
                  ADD it_actual-tot    TO   gt_out-after.
                  ADD it_actual-tot    TO   wa_c_after.
                ENDIF.
            ENDCASE.
        ENDCASE.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*      WHEN '09'.       "Downpayment
      WHEN '10'.       "Downpayment
*---End
        CASE it_actual-wrttp.
          WHEN '12'.
            ADD it_actual-tot     TO   gt_out-tot.
            ADD it_actual-tot     TO   wa_d_tot.
            CASE it_actual-gjahr.
              WHEN wa_before.
                ADD  it_actual-tot     TO   gt_out-before.
                ADD  it_actual-tot     TO   wa_d_before.
              WHEN wa_last.
                ADD  it_actual-tot     TO   gt_out-last.
                ADD  it_actual-tot     TO   wa_d_last.
              WHEN wa_year.
                ADD  it_actual-tot     TO   gt_out-year.
                ADD  it_actual-tot     TO   wa_d_year.
              WHEN wa_year1.
                ADD  it_actual-tot     TO   gt_out-year1.
                ADD  it_actual-tot     TO   wa_d_year1.
              WHEN wa_year2.
                ADD  it_actual-tot     TO   gt_out-year2.
                ADD  it_actual-tot     TO   wa_d_year2.
              WHEN wa_year3.
                ADD  it_actual-tot     TO   gt_out-year3.
                ADD  it_actual-tot     TO   wa_d_year3.
              WHEN wa_year4.
                ADD  it_actual-tot     TO   gt_out-year4.
                ADD  it_actual-tot     TO   wa_d_year4.
              WHEN wa_year5.
                ADD  it_actual-tot     TO   gt_out-year5.
                ADD  it_actual-tot     TO   wa_d_year5.
              WHEN OTHERS.
                IF it_actual-gjahr < wa_last.
                  ADD it_actual-tot    TO   gt_out-before.
                  ADD it_actual-tot    TO   wa_d_before.
                ELSEIF it_actual-gjahr > wa_year5.
                  ADD it_actual-tot    TO   gt_out-after.
                  ADD it_actual-tot    TO   wa_d_after.
                ENDIF.
            ENDCASE.
        ENDCASE.
    ENDCASE.
    MODIFY gt_out.
  ENDLOOP.
ENDFORM.                    " get_pi_actual
*&---------------------------------------------------------------------*
*&      Form  get_io_budget2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*      -->P_GT_OUT_KEY  text
*----------------------------------------------------------------------*
FORM get_pi_budget2 USING    u_posid
                             u_key
                             u_gjahr
                             u_prnam.
  REFRESH : it_budget.
  CLEAR   : it_budget.
  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_gjahr
       TABLES
            out   = it_budget.

  LOOP AT it_budget.
    CASE it_budget-gjahr.
      WHEN '1111'.
        gt_out-tot  =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_tot = gt_out-tot.
      WHEN wa_year.
        gt_out-year =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year = gt_out-year.
      WHEN wa_year1.
        gt_out-year1 =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year1 = gt_out-year1.
      WHEN wa_year2.
        gt_out-year2 =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year2 = gt_out-year2.
      WHEN wa_year3.
        gt_out-year3 =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year3 = gt_out-year3.
      WHEN wa_year4.
        gt_out-year4 =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year4 = gt_out-year4.
      WHEN wa_year5.
        gt_out-year5 =
             it_budget-org + it_budget-supp + it_budget-ret.
        wa_n_year5 = gt_out-year5.
      WHEN OTHERS.
        IF it_budget-gjahr < wa_last.
          gt_out-before = gt_out-before + it_budget-org
                        + it_budget-supp + it_budget-ret.
          wa_n_before = gt_out-before.
        ELSEIF it_budget-gjahr > wa_year5.
          gt_out-after  = gt_out-after  + it_budget-org
                        + it_budget-supp + it_budget-ret.
          wa_n_after = gt_out-after.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " get_io_budget2
*&---------------------------------------------------------------------*
*&      Form  cal_pi_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_GJAHR  text
*      -->P_GT_OUT_PRNAM  text
*----------------------------------------------------------------------*
FORM cal_pi_budget USING    u_posid
                            u_gjahr
                            u_prnam.

  REFRESH : it_budget.
  CLEAR   : it_budget.
  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_gjahr
       TABLES
            out   = it_budget.
ENDFORM.                    " cal_pi_budget
*&---------------------------------------------------------------------*
*&      Form  cal_pi_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_GJAHR  text
*      -->P_GT_OUT_PRNAM  text
*----------------------------------------------------------------------*
FORM cal_pi_actual USING    u_posid
                            u_gjahr
                            u_prnam.

  REFRESH : it_actual.
  CLEAR   : it_actual.

  CALL FUNCTION 'Z_FFI_GET_PI_ACTUAL_ACT'
    EXPORTING
      posid         = u_posid
      gjahr         = u_gjahr
      prnam         = u_prnam
      ippos         = ' '
*                IMPORTING
*                  AMT           =
    TABLES
      out           = it_actual
*                EXCEPTIONS
*                  NO_DATA       = 1
*                  OTHERS        = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " cal_pi_actual
*&---------------------------------------------------------------------*
*&      Form  range_posid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_POSID  text
*----------------------------------------------------------------------*
FORM range_posid USING    pp_posid.
  DATA : p_parnr LIKE impr-parnr.
  DATA : it_temp LIKE impr OCCURS 0 WITH HEADER LINE.
  REFRESH :it_temp,r_posid.
  CLEAR p_parnr.
  IF pp_posid+7(4) = 0000.
    SELECT SINGLE posnr INTO p_parnr FROM impr
      WHERE gjahr = p_ayear
       AND  posid EQ pp_posid.
    IF sy-subrc = 0.
      SELECT * INTO TABLE it_temp FROM impr
       WHERE parnr EQ p_parnr.
      LOOP AT it_temp.
        MOVE : it_temp-posid TO r_posid-low,
               'I'           TO r_posid-sign,
               'EQ'          TO r_posid-option.
        APPEND r_posid.
      ENDLOOP.
    ENDIF.
  ELSE.
    MOVE : pp_posid TO r_posid-low,
         'I'           TO r_posid-sign,
         'EQ'          TO r_posid-option.
    APPEND r_posid.

  ENDIF.

ENDFORM.                    " range_posid
