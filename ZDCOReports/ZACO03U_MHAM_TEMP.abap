************************************************************************
* Program Name      : ZACO03U_MHAM
* Author            : Hyung Jin Youn
* Creation Date     : 06/10/2003
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: UD1K902602
* Add documentation :
* Description       : It is required to gather data from HR Time Sheet
*                     to calculate variance of man_hour by work center
*                     (Cost Center)
*                     the timesheet data should be collected
*                     by fy+period+CCtr
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
* 08/2005  ANDY
* 05/11/11 Valerian  UD1K952620    Change data source from CATSDB
*                                  and Time Evaluation to info
*                                  type tables.
* 08/31/11 Valerian  UD1K953035    This program is used for temporary
*                                  only. Data will be collected instead
*                                  of refreshed Once Golive program will
*                                  be removed from production system.
*
*                                  Add Start Date in the selection
*                                  screen.
************************************************************************
REPORT zaco03u_mham MESSAGE-ID zmco.


*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
INCLUDE zlzgco_global_formto1.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
TYPE-POOLS: slis.

** Tables
TABLES : ztco_mhhrtrans, catsdb, ztco_mhaatype, tka01, cosl, bwom_ifact,
         zsco_cosl_key01, zsco_cosl_lst01.


** Internal table
*DATA : IT_ZTCO_MHHRTRANS LIKE Hashed TABLE OF ZTCO_MHHRTRANS
*                         WITH UNIQUE KEY GJAHR PERID KOSTL LSTAR
*                         WITH HEADER LINE .
DATA : it_ztco_mhhrtrans  LIKE STANDARD TABLE OF ztco_mhhrtrans
                          WITH HEADER LINE .
DATA : it_post            LIKE STANDARD TABLE OF ztco_mhhrtrans
                          WITH HEADER LINE .
* For TimeSheet
DATA : it_tmp_catsdb     LIKE STANDARD TABLE OF ztco_mhhrtrans
                         WITH HEADER LINE .
* BEGIN OF UD1K953035
DATA: it_mhhrtrans_tmp LIKE STANDARD TABLE OF ztco_mhhrtrans
                       WITH HEADER LINE.
* END OF UD1K953035

* Emp.Data
DATA : BEGIN OF i_emp OCCURS 0,
         pernr LIKE pa0001-pernr,
         hire  LIKE sy-datum,
         fire  LIKE sy-datum,
       END OF i_emp.

DATA : it_date           LIKE STANDARD TABLE OF casdayattr
                         WITH HEADER LINE.
DATA : g_workdays   TYPE i,
       g_totdays(2) TYPE n.


DATA : g_weekday TYPE wotnr,
       g_freeday TYPE cind,
       g_holiday TYPE cind.

DATA : it_t552a          LIKE STANDARD TABLE OF t552a
                         WITH HEADER LINE.

*data : begin of it_pern occurs 0,
*         pernr   type pernr_d,
*         kostl   type kostl,
*         emp_fte type pranz,
*       end of it_pern.

DATA: i_timetype LIKE ztco_mh_time OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_sch OCCURS 0,
        schkz     TYPE schkn,
        zeity     TYPE dzeity,
        mosid     TYPE mosid,
        date      LIKE sy-datum,
        freeday	  TYPE cind,  "non working 'X'
        shift(1)  TYPE c,
       END OF it_sch.

* For B/F Quan.
DATA : it_tmp_cosl       LIKE STANDARD TABLE OF ztco_mhhrtrans
                         WITH HEADER LINE .
* For A/A Type
DATA : it_ztco_mhaatype  LIKE STANDARD TABLE OF ztco_mhaatype
                         WITH HEADER LINE .
* For DATA retrieval
DATA : BEGIN OF it_catsdb OCCURS 5000,
          workdate  LIKE catsdb-workdate,
          skostl    LIKE catsdb-skostl  , "Sender CCtr
          lstar     LIKE catsdb-lstar   ,
          rkostl    LIKE catsdb-rkostl  , "Rec. CCtr
          awart     LIKE catsdb-awart   ,
          unit      LIKE catsdb-unit    ,
          status    LIKE catsdb-status  ,
          catshours LIKE catsdb-catshours.
DATA : END OF   it_catsdb.
DATA : BEGIN OF it_cosl OCCURS 500.
        INCLUDE STRUCTURE zsco_cosl_key01.
        INCLUDE STRUCTURE zsco_cosl_lst01.
DATA : END OF   it_cosl.

** Range
RANGES : r_workdate FOR catsdb-workdate,
         r_perid    FOR ztco_mhhrtrans-perid.
RANGES:  r_wt FOR t512w-lgart.

** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
* For DD data
DATA : gv_percount       LIKE cosp-perbl. "Period Counter
DATA : gv_ci_tabname     TYPE ddobjname .
DATA : it_et_fieldlist   LIKE TABLE OF rfvicp_ddic_tabl_fieldname
                         WITH HEADER LINE.
DATA : BEGIN OF wa_obj ,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF wa_obj.
DATA : it_obj_cctr_at   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

RANGES : pnpkostl FOR pernr-kostl.

** For ALV
DATA : gv_repid LIKE sy-repid.
DATA : gv_status       TYPE slis_formname VALUE 'PF_STATUS'.
DATA : gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA : gv_col_pos TYPE i.
DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat,
       it_eventcat          TYPE slis_t_event,
       wa_eventcat          LIKE LINE OF it_eventcat.
DATA : it_events	          TYPE slis_t_event,
       it_event_exit	    TYPE slis_t_event_exit.
* Globale Daten
INCLUDE rptbal01.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  gv_repid = sy-repid.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
  PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY   ID cac OBLIGATORY,
               p_gjahr LIKE anlp-gjahr   MEMORY   ID gjr OBLIGATORY.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) text-002.
*   From Period.
  PARAMETERS: p_frper LIKE rku01g-perbi OBLIGATORY.

  SELECTION-SCREEN COMMENT 52(05) text-003.
*   To Period.
  PARAMETERS: p_toper LIKE rku01g-perbi NO-DISPLAY. " OBLIGATORY.
  SELECTION-SCREEN END OF LINE.
  SELECT-OPTIONS: s_begda FOR p0001-begda NO-EXTENSION.     "UD1K953035
  PARAMETERS: p_frhr  TYPE CHECK DEFAULT ' ' NO-DISPLAY.    "UD1K952620
* PARAMETERS: p_frhr  AS CHECKBOX DEFAULT ' '.              "UD1K952620
  SELECTION-SCREEN END OF BLOCK bl1.

  SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-001.
  PARAMETERS : p_lstar LIKE csla-lstar            DEFAULT 'MAN_HR'
                                                         OBLIGATORY,
               p_ncoal LIKE grpdynp-name_coall    DEFAULT 'DIRECT'
                                                         OBLIGATORY.
*  select-options : s_lgart for t512w-lgart.
  SELECT-OPTIONS : s_lgart1 FOR t512w-lgart NO INTERVALS,
                   s_lgart2 FOR t512w-lgart NO INTERVALS.
*                   s_lgart3 FOR t512w-lgart NO INTERVALS.
*                  s_lgart4 for t512w-lgart no intervals.
  PARAMETERS: p_mofid LIKE t552a-mofid DEFAULT 'U1' NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK bl2.

* For HR Time Sheet
  SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-006.
  SELECT-OPTIONS : s_status FOR catsdb-status     DEFAULT '30'
                                                         NO-DISPLAY.
  "OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK bl3.
* For B/F Quantity
  SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-005.
  PARAMETERS : p_versn LIKE cosl-versn            DEFAULT '000'
                                                  OBLIGATORY,
               p_wrttp LIKE cosl-wrttp            DEFAULT '4'
                                                  OBLIGATORY.
  SELECT-OPTIONS : s_vrgng FOR cosl-vrgng        .
  SELECTION-SCREEN END OF BLOCK bl4.

  SELECT-OPTIONS : s_pernr FOR p0001-pernr NO-DISPLAY.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check period range
  PERFORM check_period_range.
* Searching for CCtr group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ncoal.
  PERFORM read_cctr_group.

AT SELECTION-SCREEN OUTPUT.
  AUTHORITY-CHECK OBJECT 'S_DEVELOP'
     ID 'DEVCLASS' DUMMY
     ID 'OBJTYPE'  FIELD 'DEBUG'
     ID 'OBJNAME'  DUMMY
     ID 'P_GROUP'  DUMMY
     ID 'ACTVT'    FIELD '03'.
  IF  sy-subrc <> 0.
    LOOP AT SCREEN.
      IF screen-name  CS 'S_PERNR'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM cal_per_count.
* Controlling Area Information
  PERFORM read_tka01.
* Read CCtrs
  PERFORM read_cctr.
* Preparation to select data

* Set default Wage Type
  PERFORM set_def_wage_type.

  PERFORM pre_to_select.
* Enqueue ZTCO_MHHRTRANS
  PERFORM enqueue_ztco_mhhrtrans.
* Get Data for appending later
  PERFORM get_data.                                         "UD1K953035
* Delete DATA in Table ZTCO_MHHRTRANS.
  PERFORM delete_data.

**// Mod. By Hyung Jin Youn 2004.07.16.
** Change of Source data for Timesheet
* Select DATA from CATSDB
* Read TimeSheet DATA
* PERFORM READ_FR_CATSDB.
  IF p_frhr = 'X'.
    PERFORM read_fr_catsdb2.
  ELSE.
    PERFORM read_co_mh.
  ENDIF.
**// End of Mod.

**// Mod. By Hyung Jin Youn 2004.08.23
* Change the logics about reading HR timesheet data
  PERFORM read_fr_catsdb3.
**// End of Mod.

* Read B/F Quantity
  PERFORM read_bf_quan.
* Aggregate data from B/F and TimeSheet
  PERFORM add_up_data.

* Preparation of ALV
  PERFORM pre_report_adj.
* Do not Commit Work or Dequeue explicitly
* LUW will do

* Collect data from temporary table
  PERFORM collect_data.                                     "UD1K953035
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM call_alv_list.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_range.
*NO-DISPLAY / Only One period
  p_toper = p_frper.
*  IF P_FRPER > P_TOPER.
*    MESSAGE E031.
*  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_group.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      class                    = '0101'
*     CRUSER                   = '*'
      field_name               = space
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      searchfld_required       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      set_name                 = p_ncoal
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      no_set_picked            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tka01.
  CLEAR tka01.
  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.
ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  PRE_TO_SELECT
*&---------------------------------------------------------------------*
*       Preparation to select data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_to_select.
* Read A/A type
  CLEAR : it_ztco_mhaatype, it_ztco_mhaatype.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_mhaatype
           FROM ztco_mhaatype
          WHERE selind = 'X'.

  IF it_ztco_mhaatype[] IS INITIAL .
    MESSAGE e042.
  ENDIF.

* Convert Periods to date Range
  CLEAR : r_workdate, r_workdate[].
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_frper
       IMPORTING
            e_date  = r_workdate-low.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_toper
       IMPORTING
            e_date  = r_workdate-high.
  r_workdate-sign   = 'I'.
  r_workdate-option = 'BT'.
  APPEND r_workdate.

* BEGIN OF UD1K953035
  READ TABLE s_begda INDEX 1.
  IF sy-subrc = 0.
    IF s_begda-high IS INITIAL.
      s_begda-high = s_begda-low.
    ENDIF.

    IF s_begda-low >= r_workdate-low AND
       s_begda-high <= r_workdate-high.
      REFRESH r_workdate.
      r_workdate-low = s_begda-low.
      r_workdate-high = s_begda-high.
      r_workdate-sign   = 'I'.
      r_workdate-option = 'BT'.
      APPEND r_workdate.
    ELSE.
      MESSAGE e000 WITH 'Start Date should be in period:' p_frper.
    ENDIF.
  ENDIF.
* END OF UD1K953035

  CALL FUNCTION 'DAY_ATTRIBUTES_GET'
       EXPORTING
            factory_calendar = 'HM'
            holiday_calendar = 'U1'
            date_from        = r_workdate-low
            date_to          = r_workdate-high
       TABLES
            day_attributes   = it_date.

* RPUWSH00   Revaluation of the Planned Working Time Infotype (0007)
  SELECT * INTO TABLE it_t552a FROM t552a
      WHERE mofid = p_mofid
        AND kjahr = p_gjahr
        AND monat = p_toper.
  DATA: lstr(6) TYPE c.
  CONCATENATE p_gjahr p_frper+1(2) INTO lstr.
  FIELD-SYMBOLS : <fs> TYPE ANY.
  DATA : num(2) TYPE n.
  DATA : f_field(14).

  LOOP AT it_t552a.
    it_sch-schkz   = it_t552a-schkz  .
    it_sch-zeity   = it_t552a-zeity  .
    it_sch-mosid   = it_t552a-mosid  .

    num = 1.
    DO 30 TIMES.
      CONCATENATE 'IT_T552A-TPR' num INTO f_field.
      ASSIGN (f_field) TO <fs>.
      IF <fs> EQ space.  EXIT. ENDIF.

      CONCATENATE lstr num INTO it_sch-date.
      CASE <fs>+3(1).
        WHEN '8'.   it_sch-freeday = 'X'.
        WHEN '3'.   it_sch-shift   = '3'.
        WHEN '2'.   it_sch-shift   = '2'.
        WHEN OTHERS.it_sch-shift   = '1'.
      ENDCASE.
      APPEND it_sch.
      CLEAR: it_sch-freeday, it_sch-shift, it_sch-date.
      num = num + 1.
    ENDDO.

  ENDLOOP.

  DATA: l_mon LIKE t009b-bumon.
  l_mon = p_frper+1(2).
  CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
       EXPORTING
            par_month = l_mon
            par_year  = p_gjahr
       IMPORTING
            par_days  = g_totdays.

  CLEAR g_workdays.
  LOOP AT it_date WHERE freeday = space.
    g_workdays = g_workdays + 1.
  ENDLOOP.

ENDFORM.                    " PRE_TO_SELECT

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_catsdb.

** Caution !!!
* CATSDB has MASS-Volume data.
* There is no appropriate Index with key - 'STATUS , AWART, Workdate'
* It seems any of fields in the key combination dose not deserve to be
* used as an index field
* Recommend to run this program in background mode if possible

* BUT, This program is made to run on On-Line mode to check report
* (request By functional Member - 2003.10.07)

* Set AWART
  RANGES : r_l_awart FOR catsdb-awart.
  CLEAR  : r_l_awart, r_l_awart[].
  LOOP AT it_ztco_mhaatype.
    r_l_awart-low  = it_ztco_mhaatype-awfrom.
    r_l_awart-high = it_ztco_mhaatype-awto.
    IF    r_l_awart-low  NE space
      AND r_l_awart-high NE space .
      r_l_awart-sign = 'I'.
      r_l_awart-option = 'BT'.
    ELSE.
      r_l_awart-sign = 'I'.
      r_l_awart-option = 'EQ'.
    ENDIF.
    APPEND r_l_awart.
    CLEAR  r_l_awart.
  ENDLOOP.

* Index - ZD1
*          WORKDATE  LIKE CATSDB-WORKDATE,
*          SKOSTL    LIKE CATSDB-SKOSTL  , "Sender CCtr
*          LSTAR     LIKE CATSDB-LSTAR   ,
*          RKOSTL    LIKE CATSDB-RKOSTL  , "Rec. CCtr
*          AWART     LIKE CATSDB-AWART   ,
*          UNIT      LIKE CATSDB-UNIT    ,
*          STATUS    LIKE CATSDB-STATUS  ,
*          CATSHOURS LIKE CATSDB-CATSHOURS.

  CLEAR : it_catsdb, it_catsdb[].
  SELECT workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         INTO CORRESPONDING FIELDS OF TABLE it_catsdb
         FROM catsdb
        WHERE awart    IN r_l_awart
          AND status   IN s_status
          AND workdate IN r_workdate.

  IF  it_catsdb[] IS INITIAL.
    MESSAGE e026.
  ENDIF.

  CLEAR it_catsdb.

* Re-org.
  CLEAR : it_tmp_catsdb, it_tmp_catsdb[].
  LOOP AT it_catsdb.
    it_tmp_catsdb-gjahr = p_gjahr.
* AT => Set as MAN_HR
* Always MAN_HR is used as AT no matter what the AT in timesheet is
* (request by Functional Member)
    it_tmp_catsdb-lstar = p_lstar.
* Unit
    it_tmp_catsdb-unit  = it_catsdb-unit.
* CCtr
* Sender, Receiver = O, X -> Sender
    IF    it_catsdb-skostl NE space
      AND it_catsdb-rkostl EQ space.
      it_tmp_catsdb-kostl = it_catsdb-skostl.
    ELSEIF
* Sender, Receiver = O, O -> Receiver /  Support Case
          it_catsdb-skostl NE space
      AND it_catsdb-rkostl NE space.
      it_tmp_catsdb-kostl = it_catsdb-rkostl.
    ELSE.
* Sender, Receiver = X, O -> Sender / No Kostl
* Sender, Receiver = X, X -> Sender / No Kostl
      it_tmp_catsdb-kostl = it_catsdb-skostl.
    ENDIF.
* Check Cost Center Range (User input -> CCtr Group)
* Ex.> Direct
    CLEAR it_costcenterlist.
    READ TABLE it_costcenterlist
         WITH KEY costcenter = it_tmp_catsdb-kostl.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
* Period
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
      EXPORTING
        i_date               = it_catsdb-workdate
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
      IMPORTING
        e_buper              = it_tmp_catsdb-perid
*       E_GJAHR              =
      EXCEPTIONS
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* Actual Quantity
    it_tmp_catsdb-actqty = it_catsdb-catshours.
* Collect data
    COLLECT it_tmp_catsdb.
    CLEAR it_tmp_catsdb.
    CLEAR it_catsdb.
  ENDLOOP.

  CLEAR it_tmp_catsdb.

ENDFORM.                    " READ_FR_CATSDB

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_MHHRTRANS
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_mhhrtrans.

  DATA : lv_perid LIKE ztco_mhhrtrans-perid.

  lv_perid = p_frper.

  DO 16 TIMES .
    IF lv_perid =< p_toper.
      CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_MHHRTR'
        EXPORTING
          mode_ztco_mhhrtrans       = 'E'
          mandt                     = sy-mandt
          gjahr                     = p_gjahr
          perid                     = lv_perid
*         KOSTL                     =
*         LSTAR                     =
*         X_GJAHR                   = ' '
*         X_PERID                   = ' '
*         X_KOSTL                   = ' '
*         X_LSTAR                   = ' '
*         _SCOPE                    = '2'
*         _WAIT                     = ' '
*         _COLLECT                  = ' '
        EXCEPTIONS
          foreign_lock              = 1
          system_failure            = 2
          OTHERS                    = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
* Period Counting
    lv_perid = lv_perid  + 1.

  ENDDO.
ENDFORM.                    " ENQUEUE_ZTCO_MHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
* local Data definition
  DATA : lv_answer,
         lv_cnt TYPE sy-dbcnt.
  DATA : lv_title(80).
* Period Range
  CLEAR : r_perid, r_perid[].
* Building Period Range
  r_perid-low    = p_frper.
  r_perid-high   = p_toper.
  r_perid-sign   = 'I'.
  r_perid-option = 'BT'.
  APPEND r_perid.
  CLEAR  r_perid.

* message
  SELECT COUNT( * ) INTO lv_cnt FROM ztco_mhhrtrans
           WHERE gjahr = p_gjahr
             AND perid IN r_perid.
  IF lv_cnt > 0.
    CLEAR : lv_answer,  lv_title.
   CONCATENATE 'All data will be lost ' p_gjahr '/' p_frper '/' p_toper
                                                          INTO lv_title.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
*     DEFAULTOPTION        = 'Y'
        textline1            = lv_title
        textline2            = 'In Table - ZTCO_MHHRTRANS'
        titel                = 'Delete DATA in Table'
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
      IMPORTING
        answer               = lv_answer.

    IF  lv_answer <> 'J'.
      MESSAGE e043.
    ENDIF.
  ENDIF.

* whenever running this program
* All data - Period relative - should be deleted and replaced
* with new records
  DELETE FROM ztco_mhhrtrans
         WHERE gjahr = p_gjahr
           AND perid IN r_perid.

* No Check Subrc


ENDFORM.                    " DELETE_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr.
* Making an internal table for CCtr to select data
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_frper+1(2) '01' INTO lv_datum.

  CLEAR : it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            controllingarea = p_kokrs
            date_from       = lv_datum
            costcentergroup = p_ncoal
       TABLES
            costcenterlist  = it_costcenterlist
            return          = it_return.
* Message
  PERFORM dis_bapi_message.


* Cost center
  CLEAR : pnpkostl, pnpkostl[].
  LOOP AT it_costcenterlist.
    pnpkostl-low    = it_costcenterlist-costcenter.
    pnpkostl-sign   = 'I'.
    pnpkostl-option = 'EQ'.
    APPEND pnpkostl.
    CLEAR  pnpkostl.
  ENDLOOP.

ENDFORM.                    " READ_CCTR

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_bapi_message.
  IF NOT it_return[] IS INITIAL.
    LOOP AT   it_return.
      MESSAGE ID     it_return-id
              TYPE   it_return-type
              NUMBER it_return-number
              WITH   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  READ_BF_QUAN
*&---------------------------------------------------------------------*
*       Read BackFlush Quantity
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bf_quan.
* Using COSL
* It is NOT sure that ALL data in COSL were summurized ONLY from B/F.
* An inspection is On-going now.  (2003.10.07)
* Some part of bellow query can be changed
* in case that the conclusion is not made or changed

* IN HMMA Business Flow , Only B/F quantity will be summed up at COSL
* (From Consultant) (2003.10.08)

* Quantity Fields -> Activity qty    ( LST001~ LST016)

* Read Dynamic Fields Name
  PERFORM read_field_name_from_dd_cosl.

* Set Object KEY
  PERFORM set_obj_key.

* Read AT Quantity DATA from COSS
  PERFORM read_at_quan_fr_cosl.

* Re-org
  PERFORM re_org_cosl_data.

ENDFORM.                    " READ_BF_QUAN

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_field_name_from_dd_cosl.

  CLEAR : it_et_fieldlist, it_et_fieldlist[].

* read DD infor. COSS Key Part
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSL_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSL_LST01'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSL

*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*       Read DD information
*----------------------------------------------------------------------*
*      -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
FORM read_dd_info TABLES   it_l_et_fieldlist STRUCTURE it_et_fieldlist
                  USING    p_ci_tabname      LIKE gv_ci_tabname.
* Local DATA definition
  DATA : it_l_fdlist LIKE STANDARD TABLE OF it_et_fieldlist
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : it_l_fdlist,     it_l_fdlist[].
  CLEAR gv_ci_tabname.
  gv_ci_tabname = p_ci_tabname.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            ic_tabname   = gv_ci_tabname
       TABLES
            et_fieldlist = it_l_fdlist
       EXCEPTIONS
            not_found    = 1
            OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  APPEND LINES OF  it_l_fdlist       TO it_l_et_fieldlist.

ENDFORM.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_AT_QUAN_FR_COSL
*&---------------------------------------------------------------------*
*       Read AT Quantity Total DATA from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_at_quan_fr_cosl.
  CLEAR : it_cosl, it_cosl[].
  CLEAR cosl.
  SELECT (it_et_fieldlist)
         INTO CORRESPONDING FIELDS OF TABLE it_cosl
         FROM cosl
          FOR ALL ENTRIES IN it_obj_cctr_at
        WHERE lednr = '00'
          AND objnr = it_obj_cctr_at-objnr
          AND gjahr = p_gjahr
          AND wrttp = p_wrttp
          AND versn = p_versn
          AND vrgng IN s_vrgng.
  CLEAR : it_cosl.
ENDFORM.                    " READ_AT_QUAN_FR_COSL

*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Set Object Key
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_obj_key.
  CLEAR : it_obj_cctr_at, it_obj_cctr_at[].
  LOOP AT it_costcenterlist.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
              lstar = p_lstar
         IMPORTING
              objnr = it_obj_cctr_at-objnr.
    it_obj_cctr_at-kostl = it_costcenterlist-costcenter.
    it_obj_cctr_at-lstar = p_lstar.
    APPEND it_obj_cctr_at. CLEAR it_obj_cctr_at.
    CLEAR it_costcenterlist.
  ENDLOOP.
  CLEAR : it_obj_cctr_at.
ENDFORM.                    " SET_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_COSL_DATA
*&---------------------------------------------------------------------*
*       reorganization - period split
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_org_cosl_data.

* Local Data definition
  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_lst_nam(30).
  DATA : lv_cnt  LIKE  cosp-perbl.

  CLEAR   it_cosl.
  CLEAR : it_tmp_cosl, it_tmp_cosl[].

  LOOP AT it_cosl.
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
    lv_cnt = p_frper .
* Key Part
    it_tmp_cosl-gjahr = p_gjahr.
    CLEAR it_obj_cctr_at.
    READ TABLE    it_obj_cctr_at
         WITH KEY objnr = it_cosl-objnr.
    it_tmp_cosl-kostl = it_obj_cctr_at-kostl.
    it_tmp_cosl-lstar = it_obj_cctr_at-lstar.
* Unit
    it_tmp_cosl-unit  = it_cosl-meinh.
*
    DO gv_percount TIMES.
* Period
      CLEAR it_tmp_cosl-perid.
      it_tmp_cosl-perid = lv_cnt.
* Value Transferring
      CLEAR lv_lst_nam.
      CONCATENATE 'IT_COSL-'  'LST'  lv_cnt
             INTO lv_lst_nam.
      ASSIGN (lv_lst_nam) TO <fs1>.
      CLEAR it_tmp_cosl-curqty.
      it_tmp_cosl-curqty = <fs1>.
* Collect
      COLLECT it_tmp_cosl.
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
    CLEAR it_tmp_cosl.
    CLEAR it_cosl.
  ENDLOOP.
  CLEAR it_tmp_cosl.
ENDFORM.                    " RE_ORG_COSL_DATA

*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_per_count.
* Cal. the Counter
  gv_percount = p_toper - p_frper + 1.

ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  ADD_UP_DATA
*&---------------------------------------------------------------------*
*       Aggregate data from B/F and TimeSheet
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_up_data.
* TimeSheet  (Summed)
  CLEAR it_tmp_catsdb.  SORT it_tmp_catsdb BY gjahr perid kostl lstar.
* B/F - COSL (Summed)
  CLEAR it_tmp_cosl.    SORT it_tmp_cosl   BY gjahr perid kostl lstar.
* Cntr -> IT_COSTCENTERLIST
  CLEAR it_costcenterlist. SORT it_costcenterlist BY costcenter.

* Unit Conversion
  LOOP AT it_tmp_catsdb.
    IF it_tmp_catsdb-unit <> 'STD'.
      PERFORM unit_conv USING it_tmp_catsdb-unit
                              it_tmp_catsdb-actqty.
      MODIFY it_tmp_catsdb.
    ENDIF.
    CLEAR  it_tmp_catsdb.
  ENDLOOP.

  LOOP AT it_tmp_cosl.
    IF it_tmp_cosl-unit <> 'STD'.
      PERFORM unit_conv USING it_tmp_cosl-unit
                              it_tmp_cosl-actqty.
      MODIFY it_tmp_cosl.
    ENDIF.
    CLEAR  it_tmp_cosl.
  ENDLOOP.

* Agrregating
  CLEAR : it_ztco_mhhrtrans, it_ztco_mhhrtrans[].

  APPEND LINES OF it_tmp_catsdb TO it_ztco_mhhrtrans.
  CLEAR it_ztco_mhhrtrans.
  APPEND LINES OF it_tmp_cosl   TO it_ztco_mhhrtrans.
  CLEAR it_ztco_mhhrtrans.

* Collecting
  CLEAR : it_post, it_post[].
  LOOP AT it_ztco_mhhrtrans.
    MOVE-CORRESPONDING it_ztco_mhhrtrans TO it_post.
* Variance
    it_post-vaeqty = it_post-actqty - it_post-curqty.
    COLLECT it_post.
    CLEAR   it_post.
    CLEAR it_ztco_mhhrtrans.
  ENDLOOP.
  CLEAR   it_post.

ENDFORM.                    " ADD_UP_DATA

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT  UNIT
*      -->P_QTY   Quantity
*----------------------------------------------------------------------*
FORM unit_conv USING    p_unit
                        p_qty.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
   EXPORTING
     input                      = p_qty
*    NO_TYPE_CHECK              = 'X'
*    ROUND_SIGN                 = ' '
     unit_in                    = p_unit
     unit_out                   = 'STD'
   IMPORTING
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     output                     = p_qty
   EXCEPTIONS
     conversion_not_found       = 1
     division_by_zero           = 2
     input_invalid              = 3
     output_invalid             = 4
     overflow                   = 5
     type_invalid               = 6
     units_missing              = 7
     unit_in_not_found          = 8
     unit_out_not_found         = 9
     OTHERS                     = 10.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_unit = 'STD'.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
      i_callback_program             = gv_repid
      i_callback_pf_status_set       = gv_status
      i_callback_user_command        = gv_user_command
*     I_STRUCTURE_NAME               = 'ZTCO_MHHRTRANS'
*     IS_LAYOUT                      =
      it_fieldcat                    = it_fieldcat[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
      it_sort                        = it_sort[]
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
      i_save                         = 'A'
*     IS_VARIANT                     =
      it_events                      = it_events
      it_event_exit                  = it_event_exit
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = it_post
    EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*---------------------------------------------------------------------*
*       FORM PF_STATUS                                                *
*---------------------------------------------------------------------*
*       PF_STATUS                                                     *
*---------------------------------------------------------------------*
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'BALVLIST' EXCLUDING extab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.
  CASE ucomm.
* Important part !
* For POST - DB Update
    WHEN 'UPDA' .
      PERFORM update.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.
  WRITE : / 'Controlling Area                         : '
            , p_kokrs.
  WRITE : / 'Fiscal Year/Period/Version/Activity Type : '
            , p_gjahr, '/', p_frper, '~', p_toper, '/', p_versn
            , '/', p_lstar.
  WRITE : / 'Value Type/CO business transaction       : '
            , p_wrttp, '/',  s_vrgng-low, '~',  s_vrgng-high.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.

* Building Field Cat.
  PERFORM fieldcat_init .

* Sort IT_POST.
  SORT it_post  BY gjahr perid kostl lstar.
  CLEAR it_post.

  it_sort-fieldname = 'PERID'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.

ENDFORM.                    " PRE_REPORT_ADJ

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.
  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].
* Key
  PERFORM build_fieldcat  USING
    'IT_POST'    'PERID'  'X'            space    space
    space        '3'      'Period'       space    space    space.

  PERFORM build_fieldcat  USING
    'IT_POST'    'KOSTL'  'X'            space    space
    space        '10'     'Cost Center'  space    space    space.

  PERFORM build_fieldcat  USING
    'IT_POST'    'LSTAR'  'X'            space    space
    space        '6'      'AT'           space    space    space.
* Value
* Quantity
  PERFORM build_fieldcat USING
    'IT_POST'    'ACTQTY'  space         'X'      space
    space        '18'  'Actual M/H'      'QUAN'   'UNIT'  'IT_POST'.

  PERFORM build_fieldcat USING
    'IT_POST'    'CURQTY'  space         'X'      space
    space        '18'  'Current M/H'     'QUAN'   'UNIT'  'IT_POST'.

  PERFORM build_fieldcat USING
    'IT_POST'    'VAEQTY'  space         'X'      space
    space        '18'  'Variance M/H'   'QUAN'   'UNIT'  'IT_POST'.
* Unit
  PERFORM build_fieldcat USING
    'IT_POST'    'UNIT'   space          space    space
    space        '4'      'UNIT'         'UNIT'   space    space.

ENDFORM.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  Build_FieldCAT
*&---------------------------------------------------------------------*
*       Field_CAT
*----------------------------------------------------------------------*
*      -->P_0065   text
*      -->P_0066   text
*      -->P_0067   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0071   text
*      -->P_0072   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110).

  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " Build_FieldCAT

*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       Update.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update.

  CLEAR ztco_mhhrtrans.
  LOOP AT it_post.
* LOG
    it_post-erdat = sy-datum.
    it_post-erzet = sy-uzeit.
    it_post-ernam = sy-uname.
    CLEAR ztco_mhhrtrans.
    MOVE-CORRESPONDING  it_post TO ztco_mhhrtrans.
    INSERT ztco_mhhrtrans .
    IF sy-subrc <> 0.
      MESSAGE e044.
    ENDIF.
    CLEAR it_post.
  ENDLOOP.

* Success
  MESSAGE s009 WITH 'Data Creation'.
ENDFORM.                    " UPDATE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB2
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_catsdb2.

* Ingnore A/A type
* HR source has no indicator to check if supportive hour or
* non-supportive hour

  DATA : pnpbegda LIKE qppnp-begda,
         pnpendda LIKE qppnp-endda,
         pnptimr6 LIKE qppnp-timr6.
  DATA : sw_zl    LIKE rptxxxxx-kr_feld3.
  RANGES : lgart FOR t512w-lgart.

  DATA : it_l_rsparams LIKE STANDARD TABLE OF rsparams
                       WITH HEADER LINE .
*
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report           = 'ZACO03U_HRMH'
*   IMPORTING
*     SP                    =
    TABLES
      selection_table       = it_l_rsparams
    EXCEPTIONS
      not_found             = 1
      no_report             = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Put data
  pnptimr6 = 'X'.
  sw_zl    = 'X'.

  CLEAR : lgart, lgart[].
  lgart[] = r_wt[].


  CLEAR r_workdate.
  READ TABLE r_workdate INDEX 1.
  pnpbegda = r_workdate-low.
  pnpendda = r_workdate-high.

  SUBMIT zaco03u_hrmh
*    VIA SELECTION-SCREEN
    AND RETURN
    WITH SELECTION-TABLE it_l_rsparams
    WITH pnppernr  IN s_pernr
    WITH pnptimr6 = pnptimr6
    WITH pnpbegda = pnpbegda
    WITH pnpendda = pnpendda
    WITH sw_zl = sw_zl           "only time wage
    WITH lgart IN lgart
    WITH pnpkostl IN pnpkostl.


*<data_tab>
  IMPORT time_data_zes   = time_data_zes
         time_data_saldo = time_data_saldo
         time_data_zl    = time_data_zl
         data_tab        = data_tab
         FROM MEMORY ID 'HRM'.


** Re-org.
  DATA : lv_tabcol(30).
  DATA : lv_kostl(5) VALUE 'KOSTL', lv_anzhl(5) VALUE 'ANZHL',
         lv_pernr(5) VALUE 'PERNR', "emp#
         lv_datum(5) VALUE 'DATUM', "date
         lv_persk(5) VALUE 'PERSK', "group
         lv_persg(5) VALUE 'PERSG', "subgroup
         lv_schkz(5) VALUE 'SCHKZ', "shift
         lv_stell(5) VALUE 'STELL', "job
         lv_lgart(5) VALUE 'LGART', "wagetype
         lv_zeity(5) VALUE 'ZEITY', "subgroup for workschedule
         lv_mosid(5) VALUE 'MOSID'. "Psub group for workschedule
  DATA : l_datum LIKE sy-datum,
         l_lgart TYPE lgart.

  FIELD-SYMBOLS : <fstab> TYPE table,
                  <fswa>  TYPE ANY,
                  <fsfn>  TYPE ANY,
                  <fsval> TYPE ANY.

  CONCATENATE data_tab '[' ']' INTO lv_tabcol.
  ASSIGN (lv_tabcol) TO <fstab>.


  ASSIGN LOCAL COPY OF INITIAL LINE OF <fstab> TO <fswa>.
* If no data found in HR tables
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-101.
  ENDIF .

  CLEAR : it_tmp_catsdb, it_tmp_catsdb[].
  it_tmp_catsdb-gjahr = p_gjahr.
  it_tmp_catsdb-perid = p_frper.
  it_tmp_catsdb-lstar = p_lstar.
  it_tmp_catsdb-unit = 'STD'.


  LOOP AT <fstab> ASSIGNING <fswa>.
* CCTR -KOSTL
    ASSIGN lv_kostl TO <fsfn>.
    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
    it_tmp_catsdb-kostl = <fsval> .
* MAN_HR -ANZHL
    ASSIGN lv_anzhl TO <fsfn>.
    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
    it_tmp_catsdb-actqty = <fsval>.
* WageType
    ASSIGN lv_lgart TO <fsfn>.
    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
    l_lgart = <fsval> .


* CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'

* Collect data
    IF l_lgart IN s_lgart1 OR l_lgart IN s_lgart2.
      COLLECT it_tmp_catsdb.
    ENDIF.

    CLEAR <fswa>.
  ENDLOOP.


** Remark
** An error was found in HR program .
*  -> Rescan CCtr.
  LOOP AT it_tmp_catsdb.
    CLEAR it_costcenterlist.
    READ TABLE it_costcenterlist
      WITH KEY costcenter = it_tmp_catsdb-kostl.
    IF sy-subrc <> 0.
      DELETE it_tmp_catsdb.
    ENDIF.
    CLEAR it_tmp_catsdb.
  ENDLOOP.

ENDFORM.                    " READ_FR_CATSDB2

*&---------------------------------------------------------------------*
*&      Form  SET_DEF_WAGE_TYPE
*&---------------------------------------------------------------------*
*       Set Default Wage Type
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_def_wage_type.

*regular, OT
  SELECT * INTO TABLE i_timetype
     FROM ztco_mh_time
     WHERE zgart BETWEEN '1' AND '2'.

  CLEAR : s_lgart1, s_lgart1[],s_lgart2, s_lgart2[].


  s_lgart1-option = 'EQ'. s_lgart1-sign   = 'I'.
  s_lgart2-option = 'EQ'. s_lgart2-sign   = 'I'.

  LOOP AT i_timetype.
    IF i_timetype = '1'.
      s_lgart1-low  = i_timetype-lgart.  APPEND r_wt.
    ELSE.
      s_lgart2-low  = i_timetype-lgart.  APPEND r_wt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_DEF_WAGE_TYPE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB3
*&---------------------------------------------------------------------*
*       Cal. Supportive and Not-Supportive Working Hour
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_catsdb3.
  DATA:  t_pa2002 LIKE pa2002 OCCURS 0 WITH HEADER LINE,
         t_pa0001 LIKE pa0001 OCCURS 0 WITH HEADER LINE,

         BEGIN OF t_asshr OCCURS 0,
           pernr TYPE asshr-pernr,
           infty TYPE asshr-infty,
           subty TYPE asshr-subty,
           objps TYPE asshr-objps,
           sprps TYPE asshr-sprps,
           endda TYPE asshr-endda,
           begda TYPE asshr-begda,
           seqnr TYPE asshr-seqnr,
           kostl TYPE assob-kostl,
         END OF t_asshr.

*FIXME
* need to collect ZCOLAB03 information... later


* TimeSheet  (Summed)
  CLEAR it_tmp_catsdb.  SORT it_tmp_catsdb BY gjahr perid kostl lstar.

* Local Data definition
  DATA : it_l_catsdb3 LIKE STANDARD TABLE OF it_catsdb
                      WITH HEADER LINE .

* Select data (approved status: 30)
* BEGIN OF UD1K952620
  SELECT * INTO TABLE t_pa0001
    FROM pa0001
   WHERE pernr IN s_pernr
     AND endda = '99991231'.

  SELECT * INTO TABLE t_pa2002
    FROM pa2002
   WHERE pernr IN s_pernr
     AND begda IN r_workdate.

  IF NOT t_pa2002[] IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE t_asshr
      FROM asshr AS a JOIN assob AS b
        ON a~pdsnr = b~pdsnr
      FOR ALL ENTRIES IN t_pa2002
     WHERE a~pernr = t_pa2002-pernr
       AND a~infty = '2002'
       AND a~subty = t_pa2002-subty
       AND a~objps = t_pa2002-objps
       AND a~sprps = t_pa2002-sprps
       AND a~endda = t_pa2002-endda
       AND a~begda = t_pa2002-begda
       AND a~seqnr = t_pa2002-seqnr.
  ENDIF.

  SORT: t_pa0001 BY pernr,
        t_pa2002 BY pernr begda,
        t_asshr.

  LOOP AT t_pa2002.
    CLEAR: t_pa0001, t_asshr.
* get sender cost center
    READ TABLE t_pa0001 WITH KEY pernr = t_pa2002-pernr
                                 BINARY SEARCH.

* get receiver cost center
    READ TABLE t_asshr WITH KEY pernr = t_pa2002-pernr
                                infty = '2002'
                                subty = t_pa2002-subty
                                objps = t_pa2002-objps
                                sprps = t_pa2002-sprps
                                endda = t_pa2002-endda
                                begda = t_pa2002-begda
                                seqnr = t_pa2002-seqnr
                                BINARY SEARCH.

    it_l_catsdb3-workdate  = t_pa2002-begda.
    it_l_catsdb3-skostl    = t_pa0001-kostl.
    it_l_catsdb3-lstar     = space.
    it_l_catsdb3-rkostl    = t_asshr-kostl.
    it_l_catsdb3-awart     = t_pa2002-awart.
    it_l_catsdb3-unit      = 'H'.
    it_l_catsdb3-status    = '30'.
    it_l_catsdb3-catshours = t_pa2002-stdaz.

    APPEND it_l_catsdb3. CLEAR it_l_catsdb3.
  ENDLOOP.

  FREE: t_pa2002, t_pa0001, t_asshr.

*  CLEAR : it_l_catsdb3, it_l_catsdb3[].
*  SELECT workdate  skostl  lstar   rkostl
*         awart     unit    status  catshours
*         INTO CORRESPONDING FIELDS OF TABLE it_l_catsdb3
*         FROM catsdb
*        WHERE
**             AWART    IN R_L_AWART
*              status   IN s_status
*          AND workdate IN r_workdate.

* END OF UD1K952620
  IF  it_l_catsdb3[] IS INITIAL.
*    MESSAGE E026.
  ENDIF.

* All information for sender cctr and receiver cctr must be
* in each record
  DELETE it_l_catsdb3
   WHERE skostl    EQ space
      OR rkostl    EQ space
      OR catshours EQ space.


  DATA : BEGIN OF it_l_calwsig OCCURS 0,
            kostl     LIKE catsdb-skostl  , "CCtr
            catshours LIKE catsdb-catshours.
  DATA : END OF   it_l_calwsig.

  CLEAR : it_l_calwsig, it_l_calwsig[].
  LOOP AT it_l_catsdb3.
* Sender
    it_l_calwsig-kostl     = it_l_catsdb3-skostl.
    it_l_calwsig-catshours = it_l_catsdb3-catshours * ( -1 ).
    COLLECT it_l_calwsig.
    CLEAR   it_l_calwsig.
* Receiver
    it_l_calwsig-kostl     = it_l_catsdb3-rkostl.
    it_l_calwsig-catshours = it_l_catsdb3-catshours .
    COLLECT it_l_calwsig.
    CLEAR   it_l_calwsig.
    CLEAR it_l_catsdb3.
  ENDLOOP.

* Cal.
  DATA : it_l_catadd LIKE STANDARD TABLE OF it_tmp_catsdb
                     WITH HEADER LINE .
  LOOP AT it_l_calwsig.
    LOOP AT it_tmp_catsdb WHERE  kostl  = it_l_calwsig-kostl.
      it_tmp_catsdb-actqty = it_tmp_catsdb-actqty
                           + it_l_calwsig-catshours.
      MODIFY it_tmp_catsdb.
      CLEAR  it_tmp_catsdb.
    ENDLOOP.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING it_l_calwsig TO it_l_catadd.
      it_l_catadd-gjahr = p_gjahr.
      it_l_catadd-perid = p_frper.
      it_l_catadd-lstar = p_lstar.
      it_l_catadd-unit = 'STD'.
      it_l_catadd-actqty = it_l_calwsig-catshours.
      COLLECT it_l_catadd.
    ENDIF.
    CLEAR it_l_catadd.
    CLEAR it_l_calwsig.
  ENDLOOP.

  LOOP AT it_l_catadd.
    CLEAR  it_tmp_catsdb.
    MOVE-CORRESPONDING it_l_catadd TO it_tmp_catsdb.
    COLLECT it_tmp_catsdb.
    CLEAR   it_tmp_catsdb.
  ENDLOOP.

** Sender '-'
*  SORT IT_L_CATSDB3 BY SKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT IT_L_CATSDB3 WHERE  SKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           - IT_L_CATSDB3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.
*
** Receiver '-'
*  SORT IT_L_CATSDB3 BY RKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT IT_L_CATSDB3 WHERE  RKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           + IT_L_CATSDB3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.

  CLEAR  it_tmp_catsdb.

ENDFORM.                    " READ_FR_CATSDB3


*&---------------------------------------------------------------------*
*&      Form  get_weekday
*&---------------------------------------------------------------------*
*FORM get_weekday USING    f_datum.
**                 CHANGING f_weekday
**                          f_freeday
**                          f_holiday.
*
*  READ TABLE it_date WITH KEY date = f_datum.
*  f_weekday = it_date-weekday.
*  f_freeday = it_date-freeday.
*  f_holiday = it_date-holiday.
*
*ENDFORM.                    " get_weekday
*&---------------------------------------------------------------------*
*&      Form  read_co_mh
*&---------------------------------------------------------------------*
FORM read_co_mh.
  TABLES: ztco_mhat.
  DATA: i_mhat LIKE ztco_mhat OCCURS 0 WITH HEADER LINE.

* regular + OT
  SELECT * INTO TABLE i_mhat
    FROM ztco_mhat
    WHERE gjahr = p_gjahr
      AND perid = p_frper
      AND kostl IN pnpkostl
      AND lgart BETWEEN '1' AND '2'.  "regular, ot,

  CLEAR : it_tmp_catsdb, it_tmp_catsdb[].
  it_tmp_catsdb-gjahr = p_gjahr.
  it_tmp_catsdb-perid = p_frper.
  it_tmp_catsdb-lstar = p_lstar.
  it_tmp_catsdb-unit = 'STD'.


  LOOP AT i_mhat.
    it_tmp_catsdb-kostl  = i_mhat-kostl.
    it_tmp_catsdb-actqty = i_mhat-anzhl.
    COLLECT it_tmp_catsdb.
  ENDLOOP.

ENDFORM.                    " read_co_mh
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get data for appending later
*----------------------------------------------------------------------*
FORM get_data.
* Period Range
  CLEAR : r_perid, r_perid[].
* Building Period Range
  r_perid-low    = p_frper.
  r_perid-high   = p_toper.
  r_perid-sign   = 'I'.
  r_perid-option = 'BT'.
  APPEND r_perid.
  CLEAR  r_perid.

  SELECT * FROM ztco_mhhrtrans INTO TABLE it_mhhrtrans_tmp
           WHERE gjahr = p_gjahr
             AND perid IN r_perid.

ENDFORM.                    " get_data
* BEGIN OF UD1K953035
*&---------------------------------------------------------------------*
*&      Form  collect_data
*&---------------------------------------------------------------------*
*       Collect all data together
*----------------------------------------------------------------------*
FORM collect_data.
  LOOP AT it_mhhrtrans_tmp.
    CLEAR: it_mhhrtrans_tmp-mandt,
           it_mhhrtrans_tmp-erdat,
           it_mhhrtrans_tmp-erzet,
           it_mhhrtrans_tmp-ernam,
           it_mhhrtrans_tmp-aedat,
           it_mhhrtrans_tmp-aezet,
           it_mhhrtrans_tmp-aenam.

    COLLECT it_mhhrtrans_tmp INTO it_post.
  ENDLOOP.
ENDFORM.                    " collect_data
* END OF UD1K953035
