************************************************************************
* Program Name      : ZRCO_LABOR_COST
* Author            : Chris Li, Andy Choi
* Creation Date     : 03/15/2005
* Specifications By : Andy Choi
* Pattern           :
* Development Request No : UD1K914960
* Addl Documentation:
* Description       : This report generate the lobor cost analysis
*                     result for each cost center based on the actual
*                     cost posting and reglect the support and being
*                     support between cost centers.
*
* Modification Logs
* Date       Developer    RequestNo    Description

*Note 1012176 - Indexes for tables PPOIX and PPOPX
* RPCIPQ00
************************************************************************
* Date        Developer  Request          Description
* 04/02/2007  Manju      UD1K940237       Switch signs of Amount & Hour
*                                         for previous period
* 01/25/2012  Valerian   UD1K953792       Correct Korean Coordinator
*                                         Monthly Working Hour
* 07/24/2012  Valerian   UD1K955235       Create new version of program
*                                         ZACOR21
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer  CTS No.    Description
* 06/20/2013    T00303   UD1K958557  U1: Apply Archiving
************************************************************************
REPORT zrco_labor_cost  MESSAGE-ID zmco.


*include zrco_labortop.
*----------------------------------------------------------------------*
*   INCLUDE ZRCO_LABORTOP                                              *
*----------------------------------------------------------------------*
TABLES:   coss, cosp,coep, csks,
          ztco_labor_cost2,
          zsco_labor, pa0002.

RANGES: r_cc    FOR  csks-kostl.
RANGES: r_ce    FOR  coss-kstar.
RANGES: p_perbl FOR sy-datum.
DATA:   gv_prev_period LIKE s016-spmon.

DATA:   t_cehier   LIKE sethier   OCCURS 0 WITH HEADER LINE.
DATA:   t_cevalues LIKE setvalues OCCURS 0 WITH HEADER LINE.
DATA:   t_cegroup  LIKE ztco_cegroup OCCURS 0 WITH HEADER LINE.


*// == 2011.09.22 change by yn.kim for ECC6. standard changed. ==//*
*// == ORG P/G : RPCPCC0D -- itab => list_table.
DATA: BEGIN OF list_tab OCCURS 0.
        INCLUDE STRUCTURE pna_ccdata.
DATA:   seqno LIKE ppoix-seqno.
DATA:   run_id TYPE p_evnum.
DATA:   fld_name(10) TYPE c.
DATA:   bukrs TYPE bukrs.
DATA:   io TYPE c.
DATA: END OF list_tab.

***DATA:   BEGIN OF list_tab  OCCURS 0.
***        INCLUDE STRUCTURE pna_ccdata.
***DATA:     seqno LIKE ppoix-seqno.
***DATA:     run_id TYPE p_evnum.
***DATA:     fld_name(10) TYPE c.
***DATA:   END OF list_tab .
*// ======================== Changed end ========================//*


DATA: list_table LIKE list_tab OCCURS 0 WITH HEADER LINE.

DATA:   rspar LIKE rsparams OCCURS 10 WITH HEADER LINE.
RANGES: r_wt FOR t512w-lgart.
DATA:   ok_code LIKE sy-ucomm,
        save_ok LIKE sy-ucomm.
TYPES:  BEGIN OF s_ee,
         pernr     LIKE pa0001-pernr,
         persg     LIKE pa0001-persg,
         ptext     LIKE t501t-ptext,
         persk     LIKE pa0001-persk,
         schkz     LIKE pa0007-schkz,
         begda     LIKE pa0001-begda,
        END OF s_ee.
TYPES:  BEGIN OF s_out,
         grp1      LIKE zsco_labor-grp1,
         grp2      LIKE zsco_labor-grp1,
         grp3      LIKE zsco_labor-grp1,
         kostl     LIKE csks-kostl,
         frocc     LIKE csks-kostl,
         tocc      LIKE csks-kostl,

*         persg     like pa0001-persg,
*         gtext(10) type c,
*         persk     like pa0001-persk,
*         ktext(10) type c,
         empct     TYPE zempct,            "empl.type

         schkz     LIKE pa0007-schkz,
         kztxt(10) TYPE c,

         zcunt     LIKE pna_ccdata-emp_count,
         regul     LIKE pna_ccdata-betrg,
         overt     LIKE pna_ccdata-betrg,
         bonus     LIKE pna_ccdata-betrg,
         zleav     LIKE pna_ccdata-betrg,
         othco     LIKE pna_ccdata-betrg,
         totco     LIKE pna_ccdata-betrg,
         pensn     LIKE pna_ccdata-betrg,
         health    LIKE pna_ccdata-betrg,
         workc     LIKE pna_ccdata-betrg,
         insur     LIKE pna_ccdata-betrg,
         tax       LIKE pna_ccdata-betrg,
         othbe     LIKE pna_ccdata-betrg,
         totbe     LIKE pna_ccdata-betrg,
         tcost     LIKE pna_ccdata-betrg,
         tregu     LIKE pna_ccdata-anzhl,
         tover     LIKE pna_ccdata-anzhl,
         tothr     LIKE pna_ccdata-anzhl,
         thour     LIKE pna_ccdata-anzhl,
         tempo     LIKE pna_ccdata-betrg,                   "UD1K955235
        END OF s_out.
TYPES:  BEGIN OF s_dis.
        INCLUDE TYPE s_out.
TYPES:    clrtb     TYPE lvc_t_scol.
TYPES:  END OF s_dis.
DATA:   it_ee   TYPE STANDARD TABLE OF s_ee.
DATA:   it_out  TYPE STANDARD TABLE OF s_out.
DATA:   it_out_s  TYPE STANDARD TABLE OF s_out.
DATA:   wa_out  TYPE s_out.
DATA:   it_dis  TYPE STANDARD TABLE OF s_dis.
DATA:   it_out1 TYPE STANDARD TABLE OF s_out.
DATA:   BEGIN OF it_covp OCCURS 0,
*           gjahr   like covp-gjahr,
*           perio   like covp-perio,
           kostl   LIKE csks-kostl,
           objnr   LIKE covp-objnr,
           parob1  LIKE covp-parob1,
*           beknz   like covp-beknz,
*           sgtxt   like covp-sgtxt,
           kstar   LIKE covp-kstar,
           meinb   LIKE covp-meinb,
           wkgbtr  LIKE covp-wkgbtr,
           mbgbtr  LIKE covp-mbgbtr,
           pkost   LIKE csks-kostl,
        END OF it_covp.
DATA: BEGIN OF it_mha_sup  OCCURS 0,
       kostl   LIKE ztco_mha-kostl,
       srkostl LIKE ztco_mha-srkostl,
*       LGART   like ztco_mha-lgart,
       anzhl   LIKE ztco_mha-anzhl,
     END OF it_mha_sup.

DATA:   BEGIN OF it_catsco OCCURS 0,
          counter    LIKE catsco-counter,
          stokz      LIKE catsco-stokz,
          workdate   LIKE catsco-workdate,
          catshours  LIKE catsco-catshours,
          skostl     LIKE catsco-skostl,
          lstar      LIKE catsco-lstar,
          rkostl     LIKE catsco-rkostl,
        END OF it_catsco.
DATA:   BEGIN OF it_cc_grp OCCURS 0,
          grp1      LIKE zsco_labor-grp1,
          grp2      LIKE zsco_labor-grp1,
          grp3      LIKE zsco_labor-grp1.
        INCLUDE STRUCTURE setvalues.
DATA:   END OF it_cc_grp.
DATA:   BEGIN OF it_schkz OCCURS 0,
         schkz       LIKE pa0007-schkz,
         ptext(10),
        END OF it_schkz.
DATA:   BEGIN OF it_persk OCCURS 0,
          persk      LIKE pa0001-persk,
          ptext(10),
        END OF it_persk.
DATA:   BEGIN OF it_persg OCCURS 0,
          persg      LIKE pa0001-persg,
          ptext(10),
        END OF it_persg.
DATA:   l_lines TYPE i.

DATA: BEGIN OF st_mis_acc,
        hkont TYPE bsis-hkont,
        shkzg TYPE bsis-shkzg,
        dmbtr TYPE maxbt,
        kostl TYPE bsis-kostl,
        belnr TYPE bsis-belnr,
        buzei TYPE bsis-buzei,
        gjahr TYPE bsis-gjahr,
      END OF st_mis_acc.

DATA    c_mark     TYPE c VALUE 'X'.
* ALV

DATA:   wc_control        TYPE        scrfname VALUE 'CC_ALV',
        wc_alv            TYPE REF TO cl_gui_alv_grid,
        wc_container      TYPE REF TO cl_gui_custom_container.

* CLASS DECLARATION
CLASS   lcl_event_receiver DEFINITION DEFERRED. "ALV EVENT HANDLE

DATA :  event_receiver TYPE REF TO lcl_event_receiver.

* INTERNAL TABLES FOR ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat ,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort.

* VARIABLES FOR ALV GRID
DATA : ws_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: w_container(100),
      w_control(100),
      w_alv(100),
      w_itab(100),
      w_structure LIKE dd02l-tabname.

CONSTANTS: c_structure(100) VALUE 'ZSCO_AALA_REPORT_'.

RANGES: r_lgart1 FOR t512w-lgart,
        r_lgart2 FOR t512w-lgart,
        r_lgart3 FOR t512w-lgart.

CONSTANTS: user(5) TYPE          c VALUE 'USR',
           manager(5) TYPE       c VALUE 'MAN',
           administrator(5) TYPE c VALUE 'ADM'.
* Manager's mode or Administrator mode
DATA: e_mode(3) TYPE c.

*- U1 Start
DATA: g_blart TYPE blart.
DATA: gr_budat TYPE RANGE OF budat.
*RANGES: GR_BUDAT FOR BKPF-BUDAT.
*- U1 End

* BEGIN OF UD1K953792
DATA: frdate TYPE sy-datum,
      todate TYPE sy-datum.
* END OF UD1K953792
****************************************************************
* LOCAL CLASSES: EVEN HANDLING
****************************************************************
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES:IMPLEMENTATION
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*end include
TABLES: t512w.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS     p_kokrs       LIKE tka01-kokrs OBLIGATORY DEFAULT 'H201'.
PARAMETERS     p_gjahr       LIKE coss-gjahr  OBLIGATORY MEMORY ID gjr.
SELECT-OPTIONS p_perio       FOR  coss-perbl  OBLIGATORY NO-EXTENSION  .
*arameters     p_versn       like coss-versn  default '0' obligatory   .
PARAMETERS :   p_read  RADIOBUTTON GROUP rd,
               p_calc  RADIOBUTTON GROUP rd,
               p_save  RADIOBUTTON GROUP rd.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-005 FOR FIELD p_ksgru.
PARAMETERS     p_ksgru       LIKE rksb1-ksgru.
PARAMETERS:    p_ok(2)   TYPE c.
SELECTION-SCREEN END OF LINE.
*temperary add employee
SELECT-OPTIONS p_kostl       FOR  csks-kostl .
SELECT-OPTIONS p_pernr       FOR  pa0002-pernr .
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
PARAMETERS: p_cycle(6)    TYPE c  DEFAULT 'HCR101' NO-DISPLAY.
PARAMETERS  p_atype       LIKE catsd-awart
                               DEFAULT 'PE20' NO-DISPLAY.
** changed by Furong on 10/26/2006
PARAMETERS: p_cyc    AS CHECKBOX DEFAULT 'X',
            p_ex_acc AS CHECKBOX DEFAULT 'X'. "Exclude Accruals
** end of change
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
PARAMETERS: p_acc_hr AS CHECKBOX DEFAULT ' ', "Accrual from HR
            p_acc_co AS CHECKBOX DEFAULT 'X'. "Accrual from CO

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

SELECTION-SCREEN END OF BLOCK b3.

*
AT SELECTION-SCREEN OUTPUT.
  IF p_ok = '22' AND e_mode = 'ADM'.
    LOOP AT SCREEN.
      IF screen-name  CS 'P_PERNR'.
        screen-input = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ELSEIF screen-name  CS 'P_OK'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name  CS 'P_PERNR'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF e_mode <> 'ADM' AND screen-name  CS 'P_OK'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*AT SELECTION-SCREEN ON P_OK.
*  IF P_OK = '22'.
*    LOOP AT SCREEN.
*      IF screen-name  CS 'P_PERNR'.
*        screen-input = 1.
*        screen-invisible = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

AT SELECTION-SCREEN .
  PERFORM check_selection.                                  "UD1K955235
  PERFORM make_perrange.
  PERFORM read_cc.
  PERFORM read_ce.
  PERFORM read_cc_group.
  PERFORM set_wagetype.
  PERFORM make_text.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  DATA: l_cousertype(3) TYPE c.
  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
  e_mode = l_cousertype.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*  check e_mode = manager or e_mode = administrator.
  IF p_ex_acc = 'X'.
    CLEAR: p_acc_hr, p_acc_co.
  ENDIF.

  CASE c_mark .

    WHEN p_read.
      PERFORM read_old_data.
    WHEN p_calc.
      PERFORM do_calculation.
    WHEN p_save.
      PERFORM delete_old_data.
      PERFORM do_calculation.
      PERFORM save_result.

  ENDCASE.

END-OF-SELECTION.
  CALL SCREEN '100'.


*&---------------------------------------------------------------------*
*&      Form  make_perrange
*&---------------------------------------------------------------------*
*      CONVERT THE PERIOD INTO DATE RANGE
*----------------------------------------------------------------------*
FORM make_perrange.
  DATA: l_date LIKE sy-datum.
  DATA: i_perio TYPE i.
  DATA: l_month(02).

* calculation allowed for single period.
  IF p_read = ' '.
    p_perio-high = p_perio-low.
  ENDIF.

  LOOP AT p_perio.
    p_perbl-sign   = 'I'.
    p_perbl-option = 'BT'.
    l_month = p_perio-low+1(2).
    CONCATENATE p_gjahr l_month '01' INTO l_date.
    p_perbl-low = l_date.

    IF NOT p_perio-high IS INITIAL.
      l_month = p_perio-high+1(2).
      CONCATENATE p_gjahr l_month '01' INTO l_date.
    ENDIF.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = l_date
      IMPORTING
        last_day_of_month = p_perbl-high.

    APPEND p_perbl.

* BEGIN OF UD1K953792
    frdate = l_date.
    todate = p_perbl-high.
* END OF UD1K953792

    l_date = p_perbl-low - 1.
    gv_prev_period = l_date(6).

    CLEAR p_perbl.
  ENDLOOP.

ENDFORM.                    " make_perrange
*&---------------------------------------------------------------------*
*&      Form  READ_CC
*&---------------------------------------------------------------------*
*     Read the cost center if the input is cost center group
*----------------------------------------------------------------------*
FORM read_cc.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.

* CHECK THE COST CENTER VALUE INPUT
  IF NOT p_kostl IS INITIAL.
    r_cc[] = p_kostl[].
    EXIT.
  ENDIF.

  CHECK NOT p_ksgru IS INITIAL.

  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      setclass      = '0101'
      shortname     = p_ksgru
      kokrs         = 'H201'
      ktopl         = 'HNA1'
    TABLES
      matching_sets = t_setlist.
  IF t_setlist[] IS INITIAL.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ELSE.
    READ TABLE t_setlist INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      setid                     = t_setlist-setname
    TABLES
      set_hierarchy             = t_sethier
      set_values                = t_setvalues
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CC RANGE.
  r_cc-sign = 'I'.
  r_cc-option = 'BT'.
  LOOP AT t_setvalues.
    r_cc-low  = t_setvalues-from.
    r_cc-high = t_setvalues-to.
    APPEND r_cc.
  ENDLOOP.
  CLEAR r_cc.
ENDFORM.                    " READ_CC
*&---------------------------------------------------------------------*
*&      Form  READ_CC_GROUP
*&---------------------------------------------------------------------*
*       read the cost center group hierarchy of HMMA1
*----------------------------------------------------------------------*
FORM read_cc_group.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.
  DATA: l_group LIKE rksb1-ksgru.
  DATA: l_beg TYPE i, l_end TYPE i.

  l_group = 'HMMA1'.
  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      setclass      = '0101'
      shortname     = l_group
      kokrs         = 'H201'
      ktopl         = 'HNA1'
    TABLES
      matching_sets = t_setlist.
  IF t_setlist[] IS INITIAL.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ELSE.
    READ TABLE t_setlist INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      setid                     = t_setlist-setname
    TABLES
      set_hierarchy             = t_sethier
      set_values                = t_setvalues
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO GROUP TABLE
  l_beg = 1.
  LOOP AT t_sethier.
    IF t_sethier-level = '1'.
      CLEAR: it_cc_grp.
      it_cc_grp-grp1 = t_sethier-shortname.
    ENDIF.

    IF t_sethier-level = '2'.
      it_cc_grp-grp2 = t_sethier-shortname.
    ENDIF.

    IF t_sethier-level = '3'.
      it_cc_grp-grp3 = t_sethier-shortname.
    ENDIF.

    IF t_sethier-vcount NE 0.
      l_end = l_beg + t_sethier-vcount - 1.
      LOOP AT t_setvalues FROM l_beg TO l_end.
        l_beg = l_beg + 1.
        MOVE-CORRESPONDING t_setvalues TO it_cc_grp.
        APPEND it_cc_grp.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " READ_CC_GROUP

*&---------------------------------------------------------------------*
*&      Form  READ_CE
*&---------------------------------------------------------------------*
*   read the cost element group for report
*----------------------------------------------------------------------*
FORM read_ce.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.
  DATA: l_indh LIKE sy-tabix,
        l_indv LIKE sy-tabix.

  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      setclass      = '0102'
      shortname     = 'H201_HR'
      kokrs         = 'H201'
      ktopl         = 'HNA1'
    TABLES
      matching_sets = t_setlist.
  IF t_setlist[] IS INITIAL.
    MESSAGE e002(sy) WITH 'Cost element group does not exist'.
    EXIT.
  ELSE.
    READ TABLE t_setlist INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      setid                     = t_setlist-setname
    TABLES
      set_hierarchy             = t_sethier
      set_values                = t_setvalues
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE e002(sy) WITH 'Cost element group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CE RANGE.
  r_ce-sign = 'I'.
  r_ce-option = 'BT'.
  LOOP AT t_setvalues.
    r_ce-low  = t_setvalues-from.
    r_ce-high = t_setvalues-to.
    APPEND r_ce.
  ENDLOOP.
  CLEAR r_ce.
* SET THE NODE NAME
  l_indh = l_indv = 1.

  LOOP AT t_sethier.
    IF t_sethier-vcount NE 0.
      l_indh = l_indv + t_sethier-vcount - 1.
      LOOP AT t_setvalues FROM l_indv TO l_indh.
        l_indv = l_indv + 1.
        t_setvalues-lfieldname = t_sethier-setid+8(20).
        MODIFY t_setvalues.
      ENDLOOP.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  t_cehier[] = t_sethier[].
  t_cevalues[] = t_setvalues[].

* BEGIN OF UD1K955235
  SELECT * INTO TABLE t_cegroup
    FROM ztco_cegroup.

  SORT t_cegroup BY hkont.
* END OF UD1K955235
ENDFORM.                    " READ_CE
*&---------------------------------------------------------------------*
*&      Form  GET_COST
*&---------------------------------------------------------------------*
*   read the labor cost data by standard program
*----------------------------------------------------------------------*
FORM get_cost.

  PERFORM make_selection.

  SUBMIT zrco_rpcpcc00_01
     WITH SELECTION-TABLE rspar
     AND RETURN.
*  IMPORT THE RESULT

*// 2011.09.22 by yn.kim ==> structure [list_table] is changed for ECC6
  IMPORT list_table[] FROM MEMORY ID 'LIST'.
  FREE MEMORY.
  DESCRIBE TABLE list_table LINES l_lines.

  TABLES: zcolab02, pa0001, t528t, t513s, pa0008.
  DATA: i_zcolab02 LIKE zcolab02 OCCURS 0 WITH HEADER LINE,
        i_pa0001   LIKE pa0001   OCCURS 0 WITH HEADER LINE,
        i_pa0008   LIKE pa0008   OCCURS 0 WITH HEADER LINE,
        i_t528t    LIKE t528t    OCCURS 0 WITH HEADER LINE,
        i_t513s    LIKE t513s    OCCURS 0 WITH HEADER LINE.
  DATA: l_dats(8) TYPE c.
  SELECT * INTO TABLE i_pa0001 FROM pa0001
     WHERE bukrs =  p_kokrs
       AND pernr IN p_pernr
       ORDER BY pernr  endda.
*  SELECT * INTO TABLE i_t528t FROM t528t
*     WHERE sprsl = sy-langu AND otype = 'S' AND endda = '99991231'.
*  SELECT * INTO TABLE i_t513s FROM t513s
*     WHERE sprsl = sy-langu AND endda = '99991231'.
  SELECT * INTO TABLE i_pa0008 FROM pa0008
    WHERE pernr IN p_pernr
    ORDER BY pernr  endda.

  LOOP AT list_table.

*   ANZHL       time
*   BETRG       amt
*   BETRG_D     amt debit
*   BETRG_C     MTD amt credit
*   MTD_ANZHL   MTD time
*   MTD_BETRG   MTD amt
*   MTD_BETRG_D YTD amt debit
*   MTD_BETRG_C YTD amt credit
*   YTD_ANZHL   YTD time
*   YTD_BETRG   YTD headcount
*   YTD_BETRG_D YTD headcount
*   YTD_BETRG_C YTD headcount
*   EMP_COUNT   Headcount

    MOVE-CORRESPONDING list_table  TO list_tab.
    CLEAR list_tab-budat.
    COLLECT list_tab.

* save to ztable for further analysis
*    MOVE-CORRESPONDING list_table  TO i_zcolab02.
*
*    LOOP AT i_pa0001 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-persg = i_pa0001-persg.
*    i_zcolab02-persk = i_pa0001-persk.
*
*    LOOP AT i_pa0008 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-trfar = i_pa0008-trfar.
*    i_zcolab02-trfgb = i_pa0008-trfgb.
*    i_zcolab02-trfgr = i_pa0008-trfgr. "grp
*    i_zcolab02-trfst = i_pa0008-trfst. "lev
*
*
*    i_zcolab02-kokrs = p_kokrs.
*    i_zcolab02-gjahr = p_gjahr.
*    l_dats = list_table-budat.
*    i_zcolab02-monat = l_dats+4(2).
*    COLLECT i_zcolab02.  CLEAR i_zcolab02.
  ENDLOOP.

*  delete from zcolab02 where kokrs = p_kokrs
*                         and gjahr = p_gjahr
*                         and monat = p_perio-low.
*  commit work.
*  insert zcolab02 from table i_zcolab02.

ENDFORM.                    " GET_COST
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_selection.
*COMPANY CODE
  rspar-selname = 'P_BUKRS'. rspar-kind = 'P'.
  rspar-low  = 'H201'.
  APPEND rspar.
  CLEAR: rspar.
*PERIOD
  LOOP AT p_perbl.
    rspar-selname = 'P_BUDAT'.
    rspar-kind = 'S'.
    rspar-sign = p_perbl-sign.
    rspar-option = p_perbl-option.
    rspar-low  = p_perbl-low.
    rspar-high = p_perbl-high.
    APPEND rspar.
    CLEAR: rspar.
  ENDLOOP.
*COST CENTER
  LOOP AT r_cc.
    rspar-selname = 'P_KOSTL'.
    rspar-kind    = 'S'.
    rspar-sign    = r_cc-sign.
    rspar-option  = r_cc-option.
    rspar-low      = r_cc-low.
    rspar-high   = r_cc-high.
    APPEND rspar.
    CLEAR: rspar.
  ENDLOOP.
*GENEAL LEDGER(COST ELEMENT)
  LOOP AT r_ce.
    rspar-selname = 'P_HKONT'.
    rspar-kind    = 'S'.
    rspar-sign    = r_ce-sign.
    rspar-option  = r_ce-option.
    rspar-low     = r_ce-low.
    rspar-high    = r_ce-high.
    APPEND rspar.
    CLEAR: rspar.
  ENDLOOP.
*WAGE TYPE RADIO BUTTON
  rspar-selname = 'WTYPE2'.
  rspar-kind = 'P'.
  rspar-low  = 'X'.
  APPEND rspar.
  CLEAR: rspar.
*WAGE TYPE
*  LOOP AT R_WT.
*    RSPAR-SELNAME = 'r_lgart'.
*    RSPAR-KIND    = 'P'.
*    RSPAR-LOW     = R_WT-LOW.
*    APPEND RSPAR.
*    CLEAR: RSPAR.
*  ENDLOOP.

*EXCLUDING ACCRUE VALUE
  rspar-selname = 'P_EX_ACC'.
  rspar-kind = 'P'.
*  rspar-low  = space.
* accrual from CO... no need to get data from HR
  IF p_acc_hr = ' '.
    rspar-low  = 'X'.
  ELSE.
    rspar-low  =  p_ex_acc.
  ENDIF.

  APPEND rspar.
  CLEAR: rspar.

* add employee temporary
  rspar-selname = 'P_PERNR'.
  rspar-kind    = 'S'.
  rspar-sign    = p_pernr-sign.
  rspar-option  = p_pernr-option.
  rspar-low     = p_pernr-low.
  rspar-high    = p_pernr-high.
  APPEND rspar.
  CLEAR: rspar.



ENDFORM.                    " MAKE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SET_WAGETYPE
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
FORM set_wagetype.
  REFRESH r_wt. CLEAR r_wt.
  CLEAR : r_lgart1, r_lgart1[],r_lgart2, r_lgart2[],
          r_lgart3, r_lgart3[].

  r_lgart1-option = 'EQ'. r_lgart1-sign   = 'I'.
  r_lgart2-option = 'EQ'. r_lgart2-sign   = 'I'.
  r_lgart3-option = 'EQ'. r_lgart3-sign   = 'I'.
  r_wt-option     = 'EQ'. r_wt-sign       = 'I'.

  TABLES: ztco_mh_time.
  SELECT * FROM ztco_mh_time.

    CASE ztco_mh_time-zgart.
* Regular
      WHEN '1'.
        r_lgart1-low = ztco_mh_time-lgart. APPEND r_lgart1.
        r_wt-low = ztco_mh_time-lgart. APPEND r_wt.

* Over Time
      WHEN '2'.
        r_lgart2-low = ztco_mh_time-lgart. APPEND r_lgart2.
        r_wt-low = ztco_mh_time-lgart. APPEND r_wt.

* paid leave, holiday
      WHEN '5' OR '9'.
        r_lgart3-low = ztco_mh_time-lgart. APPEND r_lgart3.
        r_wt-low = ztco_mh_time-lgart. APPEND r_wt.
    ENDCASE.

  ENDSELECT.

  CLEAR: r_lgart1, r_lgart2, r_lgart3.
ENDFORM.                    " SET_WAGETYPE
*&---------------------------------------------------------------------*
*&      Form  GET_EE_GROUP
*&---------------------------------------------------------------------*
*     read the emplyess information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ee_group.
** On 04/16/14
  DATA: pt0007 LIKE p0007 OCCURS 0 WITH HEADER LINE.
*    DATA: pt0007 LIKE pa0007 OCCURS 0 WITH HEADER LINE.
** End on 04/16/14
  DATA: subrc  LIKE sy-subrc.
  DATA: wa_ee  TYPE s_ee.
  DATA: wa_eel TYPE s_ee.
  DATA: lt_ee  TYPE STANDARD TABLE OF s_ee.
  DATA: lt_t501t LIKE t501t OCCURS 0 WITH HEADER LINE.
  READ TABLE p_perbl INDEX 1.
* READ THE EMPOLYEE GROUP AND SUBGROUP
  SELECT pernr persg persk
   INTO CORRESPONDING FIELDS OF TABLE it_ee
   FROM pa0001
   FOR ALL ENTRIES IN list_table
   WHERE pernr = list_table-pernr AND
         begda LE p_perbl-high     AND
         endda GE p_perbl-high     AND
         sprps NE 'X'.
  SORT it_ee BY pernr.
  DELETE ADJACENT DUPLICATES FROM it_ee
     COMPARING pernr.

* Check if terminated EE EXIST
  LOOP AT it_ee INTO wa_ee.
    IF wa_ee-persg = '5'.
*   READ THE PREVIOUS RECORD EE GROUP.
      SELECT pernr persg persk begda
        INTO CORRESPONDING FIELDS OF TABLE lt_ee
        FROM pa0001
        WHERE pernr = wa_ee-pernr.
      SORT lt_ee BY begda DESCENDING.
      LOOP AT lt_ee INTO wa_eel.
        IF wa_eel-persg NE '5'.
          wa_ee-persg = wa_eel-persg.
          MODIFY it_ee FROM wa_ee.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF wa_ee-persg NE '1' AND
         wa_ee-persg NE '9'.
        wa_ee-persg = '1'.  "IF OTHER GROUP, CONSIDER AS GROUP '1'.
        MODIFY it_ee FROM wa_ee.
      ENDIF.
    ENDIF.
  ENDLOOP.

* READ THE GROUP TEXT
  SELECT * INTO TABLE lt_t501t
    FROM t501t
    FOR ALL ENTRIES IN it_ee
    WHERE persg = it_ee-persg AND
          sprsl = 'EN'      .
  LOOP AT it_ee INTO wa_ee.
    CLEAR: lt_t501t.
    READ TABLE lt_t501t WITH KEY persg = wa_ee-persg.
    IF sy-subrc EQ 0.
      wa_ee-ptext = lt_t501t-ptext.
      MODIFY it_ee FROM wa_ee.
    ENDIF.
  ENDLOOP.

* READ EE WORK SCHEDULE RULE
  LOOP AT it_ee INTO wa_ee.
    CLEAR: pt0007, pt0007[].

** On 04/16/14
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr     = wa_ee-pernr
        infty     = '0007'
        begda     = p_perbl-low
        endda     = p_perbl-high
      IMPORTING
        subrc     = subrc
      TABLES
        infty_tab = pt0007.

*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
*        PERNR     = WA_EE-PERNR
*        INFTY     = '0007'
*        BEGDA     = P_PERBL-HIGH
*        ENDDA     = P_PERBL-HIGH
*      IMPORTING
*        SUBRC     = SUBRC
*      TABLES
*        INFTY_TAB = PT0007.
** End on 04/16/14

    IF subrc NE 0.
      MESSAGE e000 WITH 'WORK SCHEDULE ERROR FOR' wa_ee-pernr.
    ELSE.
** On 04/16/14
      SORT pt0007 BY endda DESCENDING.
** End on 04/16/14
      READ TABLE pt0007 INDEX 1.
      wa_ee-schkz = pt0007-schkz.
      MODIFY it_ee FROM wa_ee.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_EE_GROUP
" GET_SUPPORT_COST
*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMARY
*&---------------------------------------------------------------------*
*       summarize the cost data by cost element EE group
*----------------------------------------------------------------------*
FORM data_summary.
  PERFORM ce_summary.
* perform include_co_accrual.                               "UD1K955235
  PERFORM include_missing_accrual.                          "UD1K955235
  PERFORM group_summary.
  PERFORM get_text.
ENDFORM.                    " DATA_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  CE_SUMMARY
*&---------------------------------------------------------------------*
*       SUMMARIZE THE DATA FOR EACH EMPLOYEE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ce_summary.
  DATA: l_tabix LIKE sy-tabix.
  DATA: wa_list LIKE list_table.

  SORT list_tab BY pernr kostl hkont.
  CLEAR: wa_out.

  LOOP AT list_tab.
    l_tabix = sy-tabix + 1.

*   SUMMARIZE THE COST AND WORKING HOURS
    PERFORM sal_time_sum USING wa_out
                               list_tab-hkont
                               list_tab-betrg.

*   TIME
    IF list_tab-lgart IN r_lgart1.
      wa_out-tregu  = wa_out-tregu + list_tab-anzhl.
    ELSEIF list_tab-lgart IN r_lgart2.
      wa_out-tover  = wa_out-tover + list_tab-anzhl.
    ELSEIF list_tab-lgart IN r_lgart3.
      wa_out-tothr  = wa_out-tothr + list_tab-anzhl.
    ENDIF.

*   CHECK IF THE NEXT RECORD IS FOR THE SAME EMPLOYEE
    CLEAR: wa_list.
    READ TABLE list_tab INTO wa_list INDEX l_tabix.
    IF wa_list-pernr <> list_tab-pernr
    OR wa_list-kostl <> list_tab-kostl.

      PERFORM get_other_fields USING wa_out list_tab.

* BEGIN OF UD1K953792
      DATA: l_molga     TYPE molga,
            l_seqnr     TYPE pc261-seqnr,
            it_rgdir    TYPE pc261 OCCURS 0 WITH HEADER LINE,
            payroll_res TYPE payus_result,
            inter       TYPE payus_result-inter,
            wa_rt       TYPE pc207.

      IF wa_out-empct = 'K' AND wa_out-tregu = 0.
        CALL FUNCTION 'CU_READ_RGDIR'
          EXPORTING
            persnr          = list_tab-pernr
          IMPORTING
            molga           = l_molga
          TABLES
            in_rgdir        = it_rgdir
          EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

        CALL FUNCTION 'CD_READ_LAST'
          EXPORTING
            begin_date      = frdate
            end_date        = todate
          IMPORTING
            out_seqnr       = l_seqnr
          TABLES
            rgdir           = it_rgdir
          EXCEPTIONS
            no_record_found = 1
            OTHERS          = 2.

        IF sy-subrc = 0.
          CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
              clusterid                    = 'RU'
              employeenumber               = list_tab-pernr
              sequencenumber               = l_seqnr
              read_only_international      = ' '
            CHANGING
              payroll_result               = payroll_res
            EXCEPTIONS
              illegal_isocode_or_clusterid = 1
              error_generating_import      = 2
              import_mismatch_error        = 3
              subpool_dir_full             = 4
              no_read_authority            = 5
              no_record_found              = 6
              versions_do_not_match        = 7
              error_reading_archive        = 8
              error_reading_relid          = 9
              OTHERS                       = 10.

          inter = payroll_res-inter.
          READ TABLE inter-rt INTO wa_rt WITH KEY lgart = '/5UH'.
          IF sy-subrc = 0.
            wa_out-tregu = wa_rt-anzhl.
          ENDIF.
        ENDIF.
      ENDIF.
* END OF UD1K953792

*     "SUM THE SUBTOTAL
      wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                   + wa_out-bonus + wa_out-zleav
                   + wa_out-othco + wa_out-tempo.           "UD1K955235
      wa_out-totbe = wa_out-pensn + wa_out-health
                   + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                   + wa_out-tax   + wa_out-othbe.
      wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                   + wa_out-tothr.
      wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT
*     "SAVE THE SUMMARY RESULT FOR EACH EMPLOYEE

      APPEND wa_out TO it_out. CLEAR: wa_out.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " CE_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  SAL_TIME_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
FORM sal_time_sum USING    pa_out  STRUCTURE wa_out
                           f_hkont LIKE pna_ccdata-hkont
                           f_betrg LIKE pna_ccdata-betrg.
*                          pa_list structure pna_ccdata.
  DATA: l_node(30).

*GET THE COST ELEMENT GROUP NODE NAME
  PERFORM get_cenodename USING f_hkont l_node.

*SUMMARIZE THE COST AND BENEFIT
  CASE l_node.
    WHEN 'H201_HR_11'.  "REGULAR PAY
      pa_out-regul  = pa_out-regul + f_betrg.

    WHEN 'H201_HR_12'.  "OVERTIME PAY
      pa_out-overt  = pa_out-overt + f_betrg.

    WHEN 'H201_HR_13'.  "BONUS PAY
      pa_out-bonus  = pa_out-bonus + f_betrg.

    WHEN 'H201_HR_14'.  "LEAVE PAY
      pa_out-zleav  = pa_out-zleav + f_betrg.

    WHEN 'H201_HR_15'.  "OTHER PAY
      pa_out-othco  = pa_out-othco + f_betrg.

    WHEN 'H201_HR_21'.  "PENSION & 401K
      pa_out-pensn  = pa_out-pensn + f_betrg.

    WHEN 'H201_HR_22'.  "INSURANCE
      pa_out-health = pa_out-health + f_betrg.

    WHEN 'H201_HR_23'.  "INSURANCE
      pa_out-workc  = pa_out-workc + f_betrg.

    WHEN 'H201_HR_24'.  "INSURANCE
      pa_out-insur  = pa_out-insur + f_betrg.

    WHEN 'H201_HR_25'.  "TAX
      pa_out-tax  = pa_out-tax + f_betrg.

    WHEN 'H201_HR_26'.  "OTHERS
      pa_out-othbe  = pa_out-othbe + f_betrg.

    WHEN 'H201_HR_18'.  "TEMPORARY LABOR
      pa_out-tempo  = pa_out-tempo + f_betrg.

    WHEN OTHERS.

      pa_out-othbe  = pa_out-othbe + f_betrg.
  ENDCASE.

*SUMMARIZE THE WORKING HOURS
*  case l_node.
*    when 'H201_HR_11'.  "REGULAR WORKING HOUR
*      pa_out-tregu  = pa_out-tregu + pa_list-anzhl.
*
*    when 'H201_HR_12'.  "OVERTIME WORKING HOUR
*      pa_out-tover  = pa_out-tover + pa_list-anzhl.
*
*    when others.        "OTHER WORKING HOUR
*      pa_out-tothr  = pa_out-tothr + pa_list-anzhl.
*  endcase.

ENDFORM.                    " SAL_TIME_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_CENODENAME
*&---------------------------------------------------------------------*
*       FIND THE COST ELEMENT GROUP NODE NAME
*----------------------------------------------------------------------*
*      -->P_PA_LIST_HKONT  text
*----------------------------------------------------------------------*
FORM get_cenodename USING  p_hkont LIKE pna_ccdata-hkont
                           p_node .

* BEGIN UD1K955235
*  loop at t_cevalues.
*    if p_hkont ge t_cevalues-from and
*       p_hkont le t_cevalues-to.
*      p_node = t_cevalues-lfieldname.
*      exit.
*    endif.
*  endloop.

  CLEAR t_cegroup.
  READ TABLE t_cegroup WITH KEY hkont = p_hkont
                                BINARY SEARCH.
  p_node = t_cegroup-cegroup.
* END UD1K955235

ENDFORM.                    " GET_CENODENAME
*&---------------------------------------------------------------------*
*&      Form  GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
FORM get_other_fields USING  pa_out  STRUCTURE wa_out
                             pa_list STRUCTURE pna_ccdata.
  DATA: wa_ee  TYPE s_ee.

  pa_out-kostl  = pa_list-kostl.
*Headcount...
  pa_out-zcunt  = 1.

*GET THE EE GROUP/SUBGROUP/WORKING SCHEDULE RULE
  CLEAR: wa_ee.
  READ TABLE it_ee INTO wa_ee WITH KEY pernr = pa_list-pernr
                   BINARY SEARCH.
  IF sy-subrc EQ 0.
    PERFORM get_emp_categ(zacop01) USING wa_ee-persg wa_ee-persk
                                   CHANGING pa_out-empct.
    pa_out-schkz  = wa_ee-schkz.

  ELSE.
    pa_out-empct = 'X'.

*    pa_out-persg  = wa_ee-persg.
*    pa_out-gtext  = wa_ee-ptext.
*    pa_out-persk  = wa_ee-persk.

    pa_out-schkz  = wa_ee-schkz.
  ENDIF.
ENDFORM.                    " GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GROUP_SUMMARY
*&---------------------------------------------------------------------*
*       DATA SUMMARY BY
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM group_summary.

  CLEAR: it_out1[], wa_out, it_out_s.
  it_out_s[] = it_out[].
  SORT it_out BY kostl empct schkz.  "persg persk schkz.
  LOOP AT it_out INTO wa_out.
    COLLECT wa_out INTO it_out1.
  ENDLOOP.
  CLEAR it_out.
  it_out = it_out1.
  CLEAR it_out1.

ENDFORM.                    " GROUP_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*      GET THE SUPPORT AND BEING SUPPORT COST BT COST CENTERS
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM get_support_info.
  RANGES: r_ce  FOR covp-kstar.
  DATA: l_cc    LIKE csks-kostl.
  DATA: BEGIN OF lt_cc OCCURS 0,
          kostl    LIKE csks-kostl,
          objnr    LIKE csks-objnr,
        END OF lt_cc.

* GET THE COST ELEMENT FOR REGULAR SALARY/WAGE
  LOOP AT t_cevalues WHERE lfieldname CA 'H201_HR'.
    r_ce-sign   = 'I'.
    r_ce-option = 'BT'.
    r_ce-low    = t_cevalues-from.
    r_ce-high   = t_cevalues-to.
    APPEND r_ce.
  ENDLOOP.

*** UD1K940482 ( start )
*** New Logic for gathering the row data by IG.MOON 5/5/2007

  PERFORM gather_row_data.

*** UD1K940482 ( end )

*** Commented by IG.MOON 5/5/2007
*** UD1K940482 ( start )
***
****READ THE SUPPPORT RECORDS
***  select a~kostl  b~objnr  b~parob1
***         b~kstar  b~meinb
***         sum( b~wkgbtr ) sum( b~mbgbtr )
***    into table it_covp
***    from  csks as a inner join covp as b
***      on  a~objnr = b~objnr   and
***          a~kokrs = b~kokrs
***    where b~refbt  = 'K'       and
***          b~refbn  = 'SUPPORT' and
***          b~kokrs  = p_kokrs   and
***          b~vrgng  = 'KAMV'    and
***          b~perio  in p_perio  and
***          b~gjahr  eq p_gjahr  and
***          b~kstar  in r_ce     and
***        ( b~stflg  ne 'X'       or
***          b~stokz  ne 'X' )    and
***          a~kostl in r_cc
***     group by a~kostl b~objnr b~parob1 b~kstar b~meinb.
*** UD1K940482 ( end )

* DELETE THE RECORD THAT CYCLE IS NOT P_CYCLE
*  if 1 = 2.
*    loop at it_covp.
*      if it_covp-sgtxt cs p_cycle.
*      else.
*        delete it_covp.
*      endif.
*    endloop.
*  endif.

* GET THE PARTNER COST CENTER
  SELECT kostl objnr INTO TABLE lt_cc
   FROM csks
   FOR ALL ENTRIES IN it_covp
   WHERE objnr = it_covp-parob1.

* by IG. MOON 5/4/2007
*** UD1K940482
  SORT lt_cc BY objnr .

  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_covp.
    l_idx = sy-tabix.
    READ TABLE lt_cc WITH KEY objnr = it_covp-parob1 BINARY SEARCH .
    IF sy-subrc EQ 0.
      it_covp-pkost = lt_cc-kostl.
      MODIFY it_covp INDEX l_idx TRANSPORTING pkost.
    ENDIF.
  ENDLOOP.

*NO split regular / OT ; expense .. no split... Andy
  SELECT kostl srkostl SUM( anzhl )
       INTO TABLE it_mha_sup
       FROM ztco_mha
       WHERE kokrs = p_kokrs
         AND gjahr = p_gjahr
         AND perid IN p_perio
         AND kostl IN r_cc
         AND lgart = '3'
       GROUP BY kostl srkostl.

ENDFORM.                    " GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM support_summary.
  EXIT.

  DATA:  l_exist .
  DATA:  wa_out1 TYPE s_out.
  DATA:  lt_out TYPE STANDARD TABLE OF s_out.
  CLEAR: wa_out,
         it_out1.
* APPEND THE SUPPORT COST
*  loop at it_covp.
*    wa_out-kostl = it_covp-kostl.
*    if it_covp-beknz = 'S'.
*      wa_out-frocc = it_covp-pkost.
*    elseif it_covp-beknz = 'H'.
*      wa_out-tocc  = it_covp-pkost.
*    endif.
*    wa_out-regul = - it_covp-wkgbtr.
*    append wa_out to it_out1.
*    clear wa_out.
*  endloop.

* APPEND THE SUPPORT TIME
  LOOP AT it_catsco.
    wa_out-kostl = it_catsco-skostl.
    wa_out-tocc  = it_catsco-rkostl.
    wa_out-tregu = it_catsco-catshours.
    APPEND wa_out TO it_out1.
    CLEAR wa_out.
    wa_out-kostl = it_catsco-rkostl.
    wa_out-frocc  = it_catsco-skostl.
    wa_out-tregu = it_catsco-catshours.
    APPEND wa_out TO it_out1.
    CLEAR wa_out.
  ENDLOOP.
* DELETE THE ENTRIES THAT NOT IN THE ENTERED CC
  LOOP AT it_out1 INTO wa_out.
    CLEAR: l_exist.
    LOOP AT r_cc.
      IF wa_out-kostl GE r_cc-low AND
         wa_out-kostl LE r_cc-high.
        l_exist = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF l_exist NE 'X'.
      DELETE TABLE it_out1 FROM wa_out.
    ENDIF.
  ENDLOOP.

* Sumarize for the same sender or receiver
  SORT it_out1 BY kostl frocc tocc.
  LOOP AT it_out1 INTO wa_out.
    COLLECT wa_out INTO lt_out.
  ENDLOOP.
* ATTACH THE SUPPORT RESULT TO THE OUTPT TABLE
  APPEND LINES OF lt_out TO it_out.

ENDFORM.                    " SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  FINAL_SUMMARY
*&---------------------------------------------------------------------*
FORM final_summary.
  DATA: wa_out1 TYPE s_out.
  DATA: wa_dis  TYPE s_dis.
  CLEAR: it_out1, wa_out.
* for layout test. making some data in internal table
*   perform make_test_data.

* here need to be deleted end.

  SORT it_out BY  kostl ASCENDING
                  frocc ASCENDING
                  tocc  ASCENDING
                  empct.
*                  gtext descending
*                  ktext descending
*                  kztxt ascending.

  LOOP AT it_out INTO wa_out.
*    wa_out-persg = space.
*    wa_out-persk = space.
*    wa_out-schkz = space.
    COLLECT wa_out INTO it_out1.
  ENDLOOP.

  CLEAR: it_out.
* ATTACH THE COST CENTER GROUP
  LOOP AT it_out1 INTO wa_out.
    PERFORM find_cc_group USING wa_out.
    MODIFY it_out1 FROM wa_out.
  ENDLOOP.
*
  CLEAR it_out.

* MOVE RESULT TO DISPLAY TABLE
  LOOP AT it_out1 INTO wa_out.
    MOVE-CORRESPONDING wa_out TO wa_dis.
    APPEND wa_dis TO it_dis.
  ENDLOOP.

ENDFORM.                    " FINSAL SUMMARY

*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*     read the support hours
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_support_hour.

  EXIT.
  DATA: lt_cats  LIKE it_catsco OCCURS 0 WITH HEADER LINE.
  SELECT counter stokz workdate catshours
         skostl  lstar rkostl
   INTO  TABLE lt_cats
   FROM  catsco
   WHERE workdate IN p_perbl AND
         kokrs = 'H201'      AND
       ( skostl IN r_cc  OR
         rkostl IN r_cc  )   AND
         lstar = 'MAN_HR'.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  it_catsco[] = lt_cats[] .
*  DELETE THE REVERSED DOC
  LOOP AT lt_cats.
    READ TABLE it_catsco WITH KEY counter = lt_cats-counter
                                  stokz   = 'X'.
    IF sy-subrc EQ 0.
      DELETE lt_cats.
    ENDIF.
  ENDLOOP.
  CLEAR: it_catsco[], it_catsco.
  it_catsco[] = lt_cats[].
ENDFORM.                    " GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*&      Form  MAKE_TEXT
*&---------------------------------------------------------------------*
*    Make the display text in report for employee group,subgroup and
*    working schedule rule name
*----------------------------------------------------------------------*
FORM make_text.
  PERFORM make_persg_text USING: '1' 'Active',
                                 '9' 'Inpatriate'.
  PERFORM make_persk_text USING: 'U0' 'Hourly',
                                 'U2' 'Salary',
                                 'U3' 'Salary',
                                 'U8' 'Contract'.

  TABLES: ztco_mh_ws.
  DATA: wstxt(3) TYPE c,
        wskey(5) TYPE c.
  SELECT * FROM ztco_mh_ws.
    wskey+1(4) = ztco_mh_ws-schkz.
    CONCATENATE 'WS' ztco_mh_ws-anzsh INTO wstxt.
    PERFORM make_schkz_text USING: wskey  wstxt.
  ENDSELECT.
*  perform make_schkz_text using: ' 1000' 'Standard',
*                                 ' 1001' 'Shift 1',
*                                 ' 1002' 'Shift 2',
*                                 ' 1003' 'Shift 3',
*                                 ' 2001' 'Shift 1',
*                                 ' 2002' 'Shift 2',
*                                 ' 2003' 'Shift 3',
*                                 ' 3001' 'Shift 1',
*                                 ' 3002' 'Shift 2',
*                                 ' 3003' 'Shift 3'.

ENDFORM.                    " MAKE_TEXT
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSK_TEXT
*&---------------------------------------------------------------------*
FORM make_persk_text USING p_1 p_2.
  it_persk-persk = p_1.
  it_persk-ptext = p_2.
  APPEND it_persk.
ENDFORM.                    "make_persk_text
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSG_TEXT
*&---------------------------------------------------------------------*
FORM make_persg_text USING p_1 p_2.
  it_persg-persg = p_1.
  it_persg-ptext = p_2.
  APPEND it_persg.
ENDFORM.                    "make_persg_text

*&---------------------------------------------------------------------*
*&      Form  MAKE_SCHKZ_TEXT
*&---------------------------------------------------------------------*
FORM make_schkz_text USING p_1 p_2.
  it_schkz-schkz = p_1.
  it_schkz-ptext = p_2.
  APPEND it_schkz.
ENDFORM.                    "make_schkz_text

*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
FORM get_text .
  DATA: l_idx LIKE sy-tabix.

  LOOP AT it_out INTO wa_out.
    CHECK wa_out-schkz <> space.

    l_idx = sy-tabix.

*ANDY - comment
** GET THE EE GROUP TEXT
** for terminated person, but we have cost for the period
** chang the person group to 'ACTIVE'
*    if wa_out-persg = '5'.
*      wa_out-persg = '1'.
*    endif.
*    clear: it_persg.
*    read table it_persg with key persg = wa_out-persg.
*    if sy-subrc eq 0.
*      wa_out-gtext = it_persg-ptext.
*    else.
*      message i000 with text-010.
*    endif.
** GET THE EE SUBGROUP TEXT
*    clear: it_persk.
*    read table it_persk with key persk = wa_out-persk.
*    if sy-subrc eq 0.
*      wa_out-ktext = it_persk-ptext.
*    else.
*      message e000 with text-010.
*    endif.

* GET THE EE WS RULE TEXT

    CLEAR: it_schkz.
    READ TABLE it_schkz WITH KEY schkz = wa_out-schkz.
    IF sy-subrc EQ 0.
      wa_out-kztxt = it_schkz-ptext.
    ELSE.
      WRITE:/ '***Error in time wage type configuration: ',
              wa_out-schkz.
    ENDIF.
    MODIFY it_out INDEX l_idx FROM wa_out TRANSPORTING kztxt.
  ENDLOOP.


ENDFORM.                    "get_text
*&---------------------------------------------------------------------*
*&      Form  FIND_CC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*----------------------------------------------------------------------*
FORM find_cc_group USING    pa_out STRUCTURE wa_out.
  LOOP AT it_cc_grp.
    IF pa_out-kostl GE it_cc_grp-from AND
       pa_out-kostl LE it_cc_grp-to.
      pa_out-grp1 = it_cc_grp-grp1.
      pa_out-grp2 = it_cc_grp-grp2.
      pa_out-grp3 = it_cc_grp-grp3.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FIND_CC_GROUP




*&****ALV FORMS*********************************************************

*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click USING    p_e_column_fieldname
                        p_es_row_no_row_id.

ENDFORM.                    " DBL_CLICK
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_alv OUTPUT.

  IF wc_container IS INITIAL.
    PERFORM create_container .
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
    PERFORM set_sort_total .
    PERFORM start_alv_display.
    PERFORM sssign_event.
  ELSE.
    PERFORM set_alv_layout .
    PERFORM build_field_catalog .
    PERFORM set_sort_total .
    PERFORM refresh_display.
  ENDIF.

ENDMODULE.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
  CREATE OBJECT wc_container
    EXPORTING
      container_name              = 'CC_ALV'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT wc_alv
    EXPORTING
      i_parent      = wc_container
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_alv_layout.

  CLEAR : ws_layout, w_variant.
*  WS_LAYOUT-ZEBRA           = 'X'.
  ws_layout-edit            = ' '.
  ws_layout-sel_mode        = 'A'.
  ws_layout-language        = sy-langu.
  ws_layout-cwidth_opt      = 'X'.
  ws_layout-no_merging      = 'X'.
  ws_layout-no_keyfix       = 'X'.
  ws_layout-ctab_fname      = 'CLRTB'.
  w_variant-report            = sy-repid.
  w_variant-username          = sy-uname.
* BUILD THE CELL COLOR
  PERFORM build_cell_color.

ENDFORM.                    " SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_color.
  DATA: ct TYPE lvc_t_scol.
  DATA: w_ct  LIKE LINE OF ct.
  DATA: wa_dis TYPE s_dis.

  LOOP AT it_dis INTO wa_dis.
    w_ct-fname = 'TCOST'.
    w_ct-color-col = '5'.
    w_ct-color-int = '1'.
    APPEND w_ct TO ct.
    w_ct-fname = 'TOTCO'.
    w_ct-color-col = '5'.
    APPEND w_ct TO ct.
    w_ct-fname = 'TOTBE'.
    w_ct-color-col = '5'.
    APPEND w_ct TO ct.
    w_ct-fname = 'THOUR'.
    w_ct-color-col = '5'.
    APPEND w_ct TO ct.

    wa_dis-clrtb = ct.
    MODIFY it_dis FROM wa_dis.
  ENDLOOP.

ENDFORM.                    " BUILD_CELL_COLOR

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog.
  DATA: lw_itab TYPE slis_tabname.
  DATA: wa_fc   LIKE LINE OF it_fieldcat.
  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  lw_itab = 'ZSCO_LABOR2'.                                  "UD1K955235

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE '
    EXPORTING
      i_structure_name   = lw_itab
      i_bypassing_buffer = 'X'
    CHANGING
      ct_fieldcat        = it_fieldcat.
* SET THE FIELD ATTRIBUTE
  LOOP AT it_fieldcat INTO wa_fc.
    IF wa_fc-fieldname = 'SCHKZ' OR
       wa_fc-fieldname = 'PERSK' OR
       wa_fc-fieldname = 'PERSG'.
      DELETE it_fieldcat INDEX sy-tabix.
      CONTINUE.
    ENDIF.
    IF wa_fc-fieldname = 'KOSTL' OR
       wa_fc-fieldname = 'FROCC' OR
       wa_fc-fieldname = 'EMPCT' OR
*       wa_fc-fieldname = 'GTEXT' or
*       wa_fc-fieldname = 'PERSK' or
       wa_fc-fieldname = 'SCHKZ' OR
*       wa_fc-fieldname = 'PERSG' or
*       wa_fc-fieldname = 'KTEXT' or
       wa_fc-fieldname = 'GRP1'  OR
       wa_fc-fieldname = 'GRP2'  OR
       wa_fc-fieldname = 'GRP3'  OR
       wa_fc-fieldname = 'KZTXT' OR
       wa_fc-fieldname = 'TOCC'.
      wa_fc-key  = 'X'.
      MODIFY it_fieldcat FROM wa_fc.
    ELSE.
      wa_fc-do_sum  = 'X'.
      MODIFY it_fieldcat FROM wa_fc.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SORT  text
*----------------------------------------------------------------------*
FORM set_sort_total .

  PERFORM fill_sort_filed USING:  '1' 'KOSTL' 'X' ' ' 'X',
                                  '2' 'FROCC' 'X' ' ' ' ',
                                  '3' 'TOCC'  'X' ' ' ' ',
                                  '4' 'EMPCT' 'X' ' ' ' ',
                                  '6' 'KZTXT' 'X' ' ' ' '.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM start_alv_display.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  w_structure = 'ZSCO_LABOR'.
  CALL METHOD wc_alv->set_table_for_first_display
    EXPORTING
      is_layout       = ws_layout
      i_save          = w_save
      is_variant      = w_variant
      i_default       = space
    CHANGING
      it_fieldcatalog = it_fieldcat
      it_sort         = it_sort
      it_outtab       = it_dis.

ENDFORM.                    " START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SSSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event.

ENDFORM.                    " SSSIGN_EVENT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_SORT_FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_sort_filed USING p_spos
                           p_field
                           p_up
                           p_down
                           p_total.
  DATA: wa_sort LIKE LINE OF it_sort.

  wa_sort-spos = p_spos.
  wa_sort-fieldname  = p_field.
  wa_sort-up         = p_up.
  wa_sort-down       = p_down.
  wa_sort-subtot     = p_total.
  APPEND wa_sort TO it_sort.
ENDFORM.                    " FILL_SORT_FILED
*&---------------------------------------------------------------------*
*&      Form  make_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_test_data.
  PERFORM enter_test_data USING : '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 1' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 2' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 3' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Hourly' 'Shift 1' '1' '200'.


ENDFORM.                    " make_test_data
*&---------------------------------------------------------------------*
*&      Form  enter_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2644   text
*      -->P_2645   text
*      -->P_2646   text
*      -->P_2647   text
*      -->P_2648   text
*      -->P_2649   text
*      -->P_2650   text
*      -->P_2651   text
*----------------------------------------------------------------------*
FORM enter_test_data USING    p_1 p_2 p_3 p_4 p_5 p_6 p_7 p_8.
  wa_out-kostl = p_1.
  wa_out-frocc = p_2.
  wa_out-tocc = p_3.
*  wa_out-gtext = p_4.
*  wa_out-ktext = p_5.
  wa_out-kztxt = p_6.
  wa_out-zcunt = p_7.
  wa_out-regul = p_8.
  APPEND wa_out TO it_out.
ENDFORM.                    " enter_test_data
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_display.
  CALL METHOD wc_alv->refresh_table_display.


ENDFORM.                    " REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM MAKE_OUT_FORMAT.
*  DATA: WA_OUT1 TYPE S_OUT.
*  CLEAR: WA_OUT.
*  LOOP AT IT_OUT INTO WA_OUT1.
*    PERFORM INITAL_DIS_FORMAT USING:
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 3'.
*
*  ENDLOOP.
** SORT
*  SORT IT_DIS BY KOSTL ASCENDING
*                 FROCC ASCENDING
*                 TOCC  ASCENDING
*                 GTEXT DESCENDING
*                 KTEXT DESCENDING
*                 KZTXT ASCENDING.
* DELETE ADJACENT DUPLICATES FROM IT_DIS
*    COMPARING KOSTL FROCC TOCC GTEXT KTEXT KZTXT.
*
*ENDFORM.                    " MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*&      Form  INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_KOSTL  text
*      -->P_2756   text
*      -->P_2757   text
*      -->P_2758   text
*----------------------------------------------------------------------*
*FORM INITAL_DIS_FORMAT USING  P_1 P_2 P_3 P_4 .
*
*   WA_OUT-KOSTL = P_1.
*   WA_OUT-GTEXT = P_2.
*   WA_OUT-KTEXT = P_3.
*   WA_OUT-KZTXT = P_4.
*   APPEND WA_OUT TO IT_OUT2.
*ENDFORM.                    " INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*&      Form  do_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_calculation.
* cost center is one : valid on end of month.
  PERFORM get_cost.
  IF l_lines NE 0.
    PERFORM get_ee_group.
    PERFORM data_summary.

*ANDY... FIX IT LATER
    IF p_cyc = 'X'.
      PERFORM get_support_info.

      IF 1 = 2.
        PERFORM get_support_hour.
        PERFORM support_summary.
      ENDIF.

      PERFORM append_support_info.

    ENDIF.

    PERFORM final_summary.
  ENDIF.

ENDFORM.                    " do_calculation
*&---------------------------------------------------------------------*
*&      Form  read_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_old_data.
  DATA: w_dis LIKE LINE OF it_dis.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_out
    FROM ztco_labor_cost2
    WHERE kokrs   = p_kokrs
     AND  gjahr   = p_gjahr
     AND  periol IN p_perio
"     between p_perio-low and p_perio-high
*    and  versn   = p_versn
     AND  kostl   IN r_cc
     AND  accrl  =  p_ex_acc.

  CHECK sy-subrc EQ 0.

  LOOP AT it_out INTO wa_out.
    MOVE-CORRESPONDING wa_out TO w_dis.
    APPEND w_dis TO it_dis.
  ENDLOOP.
ENDFORM.                    " read_old_data
*&---------------------------------------------------------------------*
*&      Form  delete_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_old_data.
  DELETE FROM ztco_labor_cost2 WHERE kokrs  = p_kokrs
                                AND gjahr  = p_gjahr
                                AND periol IN p_perio
                                AND accrl  =  p_ex_acc.
*                               and perioh = p_perio-high.
ENDFORM.                    " delete_old_data
*&---------------------------------------------------------------------*
*&      Form  save_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_result.
  DATA: lt_out LIKE ztco_labor_cost2 OCCURS 0 WITH HEADER LINE.
  DATA: w_dis  LIKE LINE OF it_dis.
  DATA: w_out  LIKE LINE OF it_out.
  CLEAR: w_out.
  LOOP AT it_dis INTO w_dis.
* loop at it_out_s into w_out.
    MOVE-CORRESPONDING w_dis TO lt_out.
    lt_out-kokrs  = p_kokrs.
    lt_out-gjahr  = p_gjahr.
    lt_out-periol = p_perio-low.
** changed by Furong on 10/26/2006
    lt_out-accrl  = p_ex_acc.
** end of change
*    lt_out-perioh = p_perio-high.
*   lt_out-versn  = p_versn.
    APPEND lt_out.
  ENDLOOP.
  CHECK NOT lt_out IS INITIAL.

* modify ZTCO_LABOR_COST2 from table lt_out.
  INSERT ztco_labor_cost2 FROM TABLE lt_out
         ACCEPTING DUPLICATE KEYS.

  COMMIT WORK AND WAIT.
ENDFORM.                    " save_result
*&---------------------------------------------------------------------*
*&      Form  append_support_info
*&---------------------------------------------------------------------*
FORM append_support_info.
  LOOP AT it_covp.
    CLEAR: wa_out.

*   SUMMARIZE THE COST AND WORKING HOURS
    PERFORM sal_time_sum USING wa_out
                               it_covp-kstar
                               it_covp-wkgbtr.

    wa_out-kostl = it_covp-kostl.

    READ TABLE it_mha_sup WITH KEY kostl   = it_covp-kostl
                                   srkostl = it_covp-pkost.
    IF sy-subrc = 0.
      wa_out-frocc  = it_covp-kostl.
      wa_out-tocc   = it_covp-pkost.
    ELSE.
*      read table it_mha_sup with key kostl   = it_covp-pkost
*                                     srkostl = it_covp-kostl.
      wa_out-tocc   = it_covp-kostl.
      wa_out-frocc  = it_covp-pkost.
    ENDIF.


*     "SUM THE SUBTOTAL
    wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                 + wa_out-bonus + wa_out-zleav
                 + wa_out-othco + wa_out-tempo.             "UD1K955235
    wa_out-totbe = wa_out-pensn + wa_out-health
                 + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                 + wa_out-tax   + wa_out-othbe.
    wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                 + wa_out-tothr.
    wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT

    COLLECT wa_out INTO it_out.

  ENDLOOP.

* hours---
  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_out INTO wa_out.
    CHECK wa_out-frocc <> space.
    l_idx = sy-tabix.
    READ TABLE it_mha_sup WITH KEY kostl   = wa_out-frocc
                                   srkostl = wa_out-tocc.
    wa_out-thour = it_mha_sup-anzhl.
    MODIFY it_out INDEX l_idx FROM wa_out TRANSPORTING thour.
  ENDLOOP.

ENDFORM.                    " append_support_info
*&---------------------------------------------------------------------*
*&      Form  include_co_accrual
*&---------------------------------------------------------------------*
FORM include_co_accrual.
  SORT it_out BY kostl empct schkz.

* CO accrual
  DATA: lt_co_acc LIKE ztco_payacc OCCURS 0 WITH HEADER LINE.

  IF p_acc_co = 'X' AND p_ex_acc = ' '.


*should be...yyyymm = S016-SPMON

    SELECT * INTO TABLE lt_co_acc
      FROM ztco_payacc
      WHERE kokrs = p_kokrs
        AND gjahr = gv_prev_period(4)
        AND perid = gv_prev_period+4(2)
        AND kostl IN r_cc.

    SELECT * APPENDING TABLE lt_co_acc
      FROM ztco_payacc
      WHERE kokrs = p_kokrs
        AND gjahr = p_gjahr
        AND perid IN p_perio
        AND kostl IN r_cc.

* BEGIN OF UD1K955235
* Remove OT15, OT20 from FI Accrual
    DELETE lt_co_acc WHERE lgart = 'OT15'
                        OR lgart = 'OT20'.
* END OF UD1K955235

    LOOP AT lt_co_acc.

*      read table it_out with key kostl = lt_co_acc-kostl
*                                 empct = lt_co_acc-empct
* Begin of changes  - UD1K940237
* Change the sign of Amount & HR for previous Period
      IF  gv_prev_period(4) EQ   lt_co_acc-gjahr AND
          gv_prev_period+4(2) EQ lt_co_acc-perid.
        lt_co_acc-acc_amt = lt_co_acc-acc_amt * -1.
        lt_co_acc-acc_hr  = lt_co_acc-acc_hr * -1.
      ENDIF.

* End of changes - UD1K940237

      PERFORM sal_time_sum USING wa_out
                                 lt_co_acc-hkont
                                 lt_co_acc-acc_amt.
*   TIME
      IF lt_co_acc-lgart IN r_lgart1.
        wa_out-tregu  = wa_out-tregu + lt_co_acc-acc_hr.
      ELSEIF lt_co_acc-lgart IN r_lgart2.
        wa_out-tover  = wa_out-tover + lt_co_acc-acc_hr.
      ELSEIF lt_co_acc-lgart IN r_lgart3.
        wa_out-tothr  = wa_out-tothr + lt_co_acc-acc_hr.
      ENDIF.

*     "SUM THE SUBTOTAL
      wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                   + wa_out-bonus + wa_out-zleav
                   + wa_out-othco + wa_out-tempo.           "UD1K955235
      wa_out-totbe = wa_out-pensn + wa_out-health
                   + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                   + wa_out-tax   + wa_out-othbe.
      wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                   + wa_out-tothr.
      wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT


      wa_out-kostl  = lt_co_acc-kostl.
      wa_out-empct  = lt_co_acc-empct.

      COLLECT wa_out INTO it_out. CLEAR: wa_out.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " include_co_accrual

*** UD1K940478 ( START )
*** by IG.MOON 5/5/2007
FORM gather_row_data.

  CLEAR it_covp.
  REFRESH it_covp.

  RANGES lr_objnr FOR  csks-objnr.

  DATA: BEGIN OF i_objnr OCCURS 0,
          objnr LIKE csks-objnr,
          kostl LIKE csks-kostl,
        END OF i_objnr.

  SELECT DISTINCT objnr kostl INTO TABLE i_objnr
    FROM  csks
    WHERE kostl IN r_cc .

  LOOP AT i_objnr .
    lr_objnr-sign   = 'I'.
    lr_objnr-low    = i_objnr-objnr.
    lr_objnr-option = 'EQ'.
    APPEND lr_objnr.
  ENDLOOP.

  SELECT objnr  parob1
         kstar  meinb
         SUM( wkgbtr ) AS wkgbtr SUM( mbgbtr ) AS mbgbtr
    INTO CORRESPONDING FIELDS OF TABLE it_covp
    FROM  covp
    WHERE objnr IN lr_objnr  AND
          kstar  IN r_ce     AND
          gjahr  EQ p_gjahr  AND
          perio  IN p_perio  AND
          refbt  = 'K'       AND
          refbn  = 'SUPPORT' AND
          kokrs  = p_kokrs   AND
          vrgng  = 'KAMV'    AND
        ( stflg  NE 'X' AND stokz  NE 'X' )
     GROUP BY objnr parob1 kstar meinb.

  SORT i_objnr BY objnr.
  LOOP AT it_covp .
    READ TABLE i_objnr WITH KEY objnr = it_covp-objnr
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_covp-kostl = i_objnr-kostl .
      MODIFY it_covp .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GATHER_ROW_DATA
*** UD1K940478 ( END )
*&---------------------------------------------------------------------*
*&      Form  INCLUDE_MISSING_ACCRUAL
*&---------------------------------------------------------------------*
*       Include missing accrual from Manual Posting
*----------------------------------------------------------------------*
FORM include_missing_accrual.
  DATA: l_fp_date TYPE sy-datum,
        l_lp_date TYPE sy-datum.

*  DATA: BEGIN OF LT_MIS_ACC OCCURS 0,
*          HKONT TYPE BSIS-HKONT,
*          SHKZG TYPE BSIS-SHKZG,
*          DMBTR TYPE MAXBT,
*          KOSTL TYPE BSIS-KOSTL,
*          BELNR TYPE BSIS-BELNR,
*          BUZEI TYPE BSIS-BUZEI,
*          GJAHR TYPE BSIS-GJAHR,
*        END OF LT_MIS_ACC.

  DATA: lt_mis_acc LIKE st_mis_acc OCCURS 0 WITH HEADER LINE.

  IF p_acc_co = 'X' AND p_ex_acc = ' '.
    SORT it_out BY kostl empct schkz.

* Missing accrual (post manually)

* Get posting date
    CONCATENATE p_gjahr p_perio-low+1(2) '01' INTO l_fp_date.

    CALL FUNCTION 'DATE_GET_MONTH_LASTDAY'
      EXPORTING
        i_date = l_fp_date
      IMPORTING
        e_date = l_lp_date.

    SELECT hkont shkzg dmbtr kostl belnr buzei gjahr
      INTO CORRESPONDING FIELDS OF TABLE lt_mis_acc
      FROM bsis
     FOR ALL ENTRIES IN t_cegroup
     WHERE bukrs = p_kokrs
       AND hkont = t_cegroup-hkont
       AND gjahr = p_gjahr
       AND budat BETWEEN l_fp_date AND l_lp_date
       AND blart <> 'PY'
       AND kostl IN r_cc.

*- U1 START
    IF p_arch EQ 'X'.
      CLEAR: g_blart.
      g_blart = 'PY'.
      PERFORM archive_read_bsis TABLES lt_mis_acc
                                USING  l_fp_date
                                       l_lp_date.
    ENDIF.
*- U1 End

    LOOP AT lt_mis_acc.
      IF lt_mis_acc-shkzg = 'H'.
        lt_mis_acc-dmbtr = lt_mis_acc-dmbtr * -1.
      ENDIF.

      PERFORM sal_time_sum USING wa_out
                                 lt_mis_acc-hkont
                                 lt_mis_acc-dmbtr.

*     "SUM THE SUBTOTAL
      wa_out-totco = wa_out-regul + wa_out-overt  "LABOR COST SUBTOTAL
                   + wa_out-bonus + wa_out-zleav
                   + wa_out-othco + wa_out-tempo.           "UD1K955235
      wa_out-totbe = wa_out-pensn + wa_out-health
                   + wa_out-workc + wa_out-insur  "BENEFIT SUBTOTAL
                   + wa_out-tax   + wa_out-othbe.
      wa_out-thour = wa_out-tregu + wa_out-tover  "WORK HOUR SUBTOTAL
                   + wa_out-tothr.
      wa_out-tcost = wa_out-totco + wa_out-totbe. "TOTAL OF COST/BENEFIT


      wa_out-kostl  = lt_mis_acc-kostl.

      COLLECT wa_out INTO it_out. CLEAR: wa_out.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " include_missing_accrual

* BEGIN OF UD1K955235
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTION
*&---------------------------------------------------------------------*
*       Check check parameters/selections before proceeding
*----------------------------------------------------------------------*
FORM check_selection.
  IF NOT p_save IS INITIAL AND
   ( NOT p_ksgru IS INITIAL OR
     NOT p_kostl[] IS INITIAL ).
    MESSAGE e002(sy) WITH 'Clear Cost Center/CC Group before saving'.
  ENDIF.
ENDFORM.                    " CHECK_SELECTION
* END OF UD1K955235
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
FORM archive_read_bsis TABLES  pt_mis_acc STRUCTURE st_mis_acc
                       USING   p_fp_date TYPE sy-datum
                               p_lp_date TYPE sy-datum.
  DATA: lt_bsis  LIKE bsis OCCURS 0 WITH HEADER LINE,
        lt_ybkpf TYPE bkpf OCCURS 10,
        lt_ybseg TYPE bseg OCCURS 10,
        lt_selections TYPE rsds_range OCCURS 10.
  DATA: l_errors      TYPE boole.

  l_errors = 'X'.
  PERFORM set_range_budat USING   p_fp_date
                                  p_lp_date.

  PERFORM set_sel_condition_for_bsis CHANGING lt_selections.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      i_selections       = lt_selections
    TABLES
      e_bkpf             = lt_ybkpf
      e_bseg             = lt_ybseg
      e_bsis             = lt_bsis
*     E_BSAS             = LT_BSAS
*     E_BSAD             = LT_BSAD
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      no_infostruc_found = 1
      selections_error   = 2
      OTHERS             = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_bsis.
      READ TABLE t_cegroup WITH KEY hkont = lt_bsis-hkont.
      IF sy-subrc EQ 0.
        pt_mis_acc-hkont = lt_bsis-hkont.
        pt_mis_acc-shkzg = lt_bsis-shkzg.
        pt_mis_acc-dmbtr = lt_bsis-dmbtr.
        pt_mis_acc-kostl = lt_bsis-kostl.
        pt_mis_acc-belnr = lt_bsis-belnr.
        pt_mis_acc-buzei = lt_bsis-buzei.
        pt_mis_acc-gjahr = lt_bsis-gjahr.
        APPEND pt_mis_acc.
      ENDIF.
      CLEAR: pt_mis_acc,
             lt_bsis.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSIS
*&---------------------------------------------------------------------*
FORM set_sel_condition_for_bsis
                          CHANGING  pt_selections TYPE rsds_trange.

  PERFORM set_select_field_option USING    'BKPF'
                                           'BUKRS'
                                           'I'
                                           'EQ'
                                           'P_KOKRS'
                                           'P'
                                  CHANGING pt_selections.

  PERFORM set_select_field_option USING   'BKPF'
                                          'GJAHR'
                                          'I'
                                          'EQ'
                                          'P_GJAHR'
                                          'P'
                                 CHANGING pt_selections.

  PERFORM set_select_field_option USING    'BKPF'
                                           'BUDAT'
                                           ' '
                                           ' '
                                           'GR_BUDAT'
                                           'S'
                                  CHANGING pt_selections.



  PERFORM set_select_field_option USING   'BKPF'
                                          'BLART'
                                          'I'
                                          'NE'
                                          'G_BLART'
                                          'P'
                                 CHANGING pt_selections.


  PERFORM set_select_field_option USING    'BSEG'
                                           'KOSTL'
                                           ' '
                                           ' '
                                           'R_CC'
                                           'S'
                                  CHANGING pt_selections.

ENDFORM.                    " SET_SEL_CONDITION_FOR_BSIS
*&---------------------------------------------------------------------*
*&      Form  SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
FORM set_select_field_option
                         USING    p_tablename
                                  p_fieldname
                                  p_sign
                                  p_option
                                  p_selfield
                                  p_param
                         CHANGING pt_selections TYPE rsds_trange.

  DATA:  ls_selopt    LIKE rsdsselopt,
         lt_selopt    TYPE rsds_selopt_t,
         ls_frange    TYPE rsds_frange,
         lt_frange    TYPE rsds_frange_t,
         ls_trange    TYPE rsds_range,
         lt_trange    TYPE rsds_trange.

  DATA: l_name TYPE string.

  FIELD-SYMBOLS: <fs_itab>  TYPE ANY TABLE,
                 <fs_wa>    TYPE any,
                 <fs_value> TYPE any.

  ls_trange-tablename = p_tablename.

  CASE p_param.
    WHEN 'P'.
      l_name = p_selfield.
      ASSIGN (l_name) TO <fs_value>.

      IF NOT <fs_value> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        ls_selopt-sign = p_sign.
        ls_selopt-option = p_option.
        ls_selopt-low = <fs_value>.
        APPEND ls_selopt TO lt_selopt.

        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.

    WHEN 'S'.
      CONCATENATE p_selfield
                  '[]'
             INTO l_name.
      ASSIGN (l_name) TO <fs_itab>.

      IF NOT <fs_itab> IS INITIAL.
        REFRESH: lt_selopt.
        ls_frange-fieldname = p_fieldname.
        LOOP AT <fs_itab> ASSIGNING <fs_wa>.
          MOVE-CORRESPONDING <fs_wa> TO ls_selopt.
*          LS_SELOPT = <FS_WA>.
          APPEND ls_selopt TO lt_selopt.
        ENDLOOP.
        ls_frange-selopt_t = lt_selopt[].
        APPEND ls_frange TO lt_frange.
      ENDIF.
  ENDCASE.

  IF NOT lt_frange[] IS INITIAL .
    ls_trange-frange_t = lt_frange[].
    APPEND ls_trange TO lt_trange.
  ENDIF.

  APPEND LINES OF lt_trange TO pt_selections.


ENDFORM.                    " SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE_BUDAT
*&---------------------------------------------------------------------*
FORM set_range_budat  USING   p_fp_date TYPE sy-datum
                              p_lp_date TYPE sy-datum.
  DATA: lr_budat TYPE RANGE OF budat WITH HEADER LINE.

  lr_budat-sign = 'I'.
  lr_budat-option = 'BT'.
  lr_budat-low = p_fp_date.
  lr_budat-high = p_lp_date.
  APPEND lr_budat.
  gr_budat[] = lr_budat[].
  CLEAR: lr_budat.
ENDFORM.                    " SET_RANGE_BUDAT
