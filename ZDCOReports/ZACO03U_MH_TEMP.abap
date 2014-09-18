************************************************************************
* Program Name      : ZACO03U_MH
* Author            : Andy Choi
* Specifications By : Andy Choi
*
* Maintain ztco_mhaatype table : select time wage type ranges
*  	PROD	1001	UES1	X	05/27/2004	14:28:19	9071483

* Date      Developer      Request       Description
* 06/06/06  Manju          UD1K920571    Program changes to capture
*                                        Delta time changes.
* 08/31/10  Valerian       UD1K949773    Refer to table ZTCO_MH_DWS
*                                        to get Shift Information
* 05/11/11  Valerian       UD1K952620    Change data source from CATSDB
*                                        and Time Evaluation to info
*                                        type tables.
* 08/11/11  Valerian       UD1K952755    This program is used for
*                                        temporary only. Data will be
*                                        collected instead of refreshed
*                                        Once Golive program will be
*                                        removed from production system
* 08/29/11  Valerian       UD1K952870    Add Start Date in the selection
*                                        screen
* 10/06/11  Valerian       UD1K953104    Correct Employee Count Calc.
*                                        Logic
* 10/06/11  Valerian       UD1K953108    Correct Employee Count Calc.
*                                        Logic (prev.change incorrect)
************************************************************************
REPORT zaco03u_mh MESSAGE-ID zmco.


* RPTBAL00 - report
* RPTIME00 - eval
* CAT2     - recording
* CADO     - display time
*
* BAPI_CATIMESHEETMGR_INSERT
* Inserting records that generate attendances/absences and wage types
* for Human Resources (HR)
* Inserting records that trigger activity allocation for Controlling
* (CO)
* Inserting records that generate confirmations for Project System (PS)
* Inserting records that generate confirmations for Plant Maintenance
* (PM) or Customer Service (CS)
* Inserting records with information on external services for Materials
* Management (MM)
*  call function 'PM_CATS_CONFIRMATION_CREATE' in background task
*       exporting
*            cluster_no    = p_cluster_no
*            profile       = p_profile
*       tables
*            catsrecords   = p_cats_confirmation
*            extensionin   = p_cats_extensionin
*            longtext      = p_cats_longtext.


*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
*NCLUDE zlzgco_global_formto1.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
TYPE-POOLS: slis.

** Tables
TABLES : catsdb, ztco_mhaatype,
         ztco_mh_time,
         ztco_jc,  "job code mapping
         ztco_mh_ws,
         tka01, pa0007,t552a,
         ztco_mhat,
         ztco_mhad,
         pa0001,
         ztco_mha.

DATA: BEGIN OF i_pa7 OCCURS 0,
        pernr LIKE pa0007-pernr,
        begda LIKE pa0007-begda,
        endda LIKE pa0007-endda,
        arbst LIKE pa0007-arbst,  "Daily Working Hours
      END OF i_pa7.

** Internal table
DATA : BEGIN OF py_times OCCURS 0,
         werks LIKE t001p-werks, "PA
         btrtl LIKE t001p-btrtl, "sub-area
         molga LIKE t001p-molga, "country
         mofid LIKE t001p-mofid, "Public Holiday Calendar
         persg LIKE t503-persg,
         persk LIKE t503-persk,
         zeity LIKE t503-zeity,
         datum LIKE sy-datum.
*        MOSID like t001p-mosid, "Personnel Subarea Grouping for WS
        INCLUDE STRUCTURE ztco_mha.
DATA :   lgart2   LIKE ztco_mhad-lgart2.
*       dtype(1) TYPE c.
DATA : END OF py_times.

DATA : it_time           LIKE STANDARD TABLE OF ztco_mha
                         WITH HEADER LINE .
DATA : it_time_dt        LIKE it_time OCCURS 0 WITH HEADER LINE.

DATA : it_timet          LIKE STANDARD TABLE OF ztco_mhat
                         WITH HEADER LINE .
DATA : it_timet_dt        LIKE it_timet OCCURS 0 WITH HEADER LINE.

DATA : it_timed          LIKE STANDARD TABLE OF ztco_mhad
                         WITH HEADER LINE .
DATA : it_timed_dt        LIKE it_timed OCCURS 0 WITH HEADER LINE.
DATA : it_timed_b         LIKE it_timed OCCURS 0 WITH HEADER LINE.

* BEGIN OF UD1K952755
DATA : it_time_tmp       LIKE SORTED TABLE OF ztco_mha
                         WITH UNIQUE KEY MANDT
                                         KOKRS
                                         GJAHR
                                         PERID
                                         PERNR
                                         KOSTL
                                         LGART
                                         DTYPE
                                         SCHKZ
                                         MOSID
                                         JOBCD
                                         SRKOSTL
                                         CHNGE
                                         ANZSH
                                         EMPCT
                                         ERDAT
                                         ERZET
                                         ERNAM
                         WITH HEADER LINE.

DATA : it_timet_tmp      LIKE STANDARD TABLE OF ztco_mhat
                         WITH HEADER LINE .
DATA : it_timed_tmp      LIKE STANDARD TABLE OF ztco_mhad
                         WITH HEADER LINE .
* END OF UD1K952755

* Begin of changes -
DATA : it_time_db           LIKE STANDARD TABLE OF ztco_mha
                         WITH HEADER LINE.
DATA : it_timet_db          LIKE STANDARD TABLE OF ztco_mhat
                         WITH HEADER LINE .
DATA : it_timed_db          LIKE STANDARD TABLE OF ztco_mhad
                         WITH HEADER LINE .
* End of changes -

DATA: i_jobcd    LIKE ztco_jc      OCCURS 0 WITH HEADER LINE,
      i_timetype LIKE ztco_mh_time OCCURS 0 WITH HEADER LINE,
      i_wksch    LIKE ztco_mh_ws   OCCURS 0 WITH HEADER LINE,
      i_dwksch LIKE ztco_mh_dws  OCCURS 0 WITH HEADER LINE. "UD1K949773

* Emp.Data
DATA : BEGIN OF i_emp OCCURS 0,
         pernr LIKE pa0001-pernr,
         hire  LIKE sy-datum,
         fire  LIKE sy-datum,
         kostl LIKE pa0001-kostl,
         werks LIKE pa0001-werks,  "PSA      -> PSG for WS (V_001P_N)
         btrtl LIKE pa0001-btrtl,  "PSubArea -> PSG for WS (V_001P_N)
         mosid LIKE t001p-mosid,   "PSG for WS
       END OF i_emp.

DATA : gw_time  LIKE it_time,
       gw_timet LIKE it_timet,
       gw_timed LIKE it_timed.

DATA : it_date           LIKE STANDARD TABLE OF casdayattr
                         WITH HEADER LINE.
DATA : g_workdays   TYPE i,
       g_totdays(2) TYPE n.


DATA : g_weekday TYPE wotnr,
       g_freeday TYPE cind,
       g_holiday TYPE cind.

DATA : BEGIN OF it_t552a  OCCURS 0,
         zeity   LIKE t552a-zeity,
         mofid   LIKE t552a-mofid,
         mosid   LIKE t552a-mosid,
         schkz   LIKE t552a-schkz,

         solst   LIKE t552a-solst,
         tpr01   LIKE t552a-tpr01,
         tpr02   LIKE t552a-tpr02,
         tpr03   LIKE t552a-tpr03,
         tpr04   LIKE t552a-tpr04,
         tpr05   LIKE t552a-tpr05,
         tpr06   LIKE t552a-tpr06,
         tpr07   LIKE t552a-tpr07,
         tpr08   LIKE t552a-tpr08,
         tpr09   LIKE t552a-tpr09,
         tpr10   LIKE t552a-tpr10,
         tpr11   LIKE t552a-tpr11,
         tpr12   LIKE t552a-tpr12,
         tpr13   LIKE t552a-tpr13,
         tpr14   LIKE t552a-tpr14,
         tpr15   LIKE t552a-tpr15,
         tpr16   LIKE t552a-tpr16,
         tpr17   LIKE t552a-tpr17,
         tpr18   LIKE t552a-tpr18,
         tpr19   LIKE t552a-tpr19,
         tpr20   LIKE t552a-tpr20,
         tpr21   LIKE t552a-tpr21,
         tpr22   LIKE t552a-tpr22,
         tpr23   LIKE t552a-tpr23,
         tpr24   LIKE t552a-tpr24,
         tpr25   LIKE t552a-tpr25,
         tpr26   LIKE t552a-tpr26,
         tpr27   LIKE t552a-tpr27,
         tpr28   LIKE t552a-tpr28,
         tpr29   LIKE t552a-tpr29,
         tpr30   LIKE t552a-tpr30,
         tpr31   LIKE t552a-tpr31,
      END OF it_t552a.

DATA : BEGIN OF it_sch OCCURS 0,
        schkz     TYPE schkn,
        zeity     TYPE dzeity,
        mosid     TYPE mosid,

        date      LIKE sy-datum,
        freeday   TYPE cind,  "non working 'X'
        shift(1)  TYPE c,
       END OF it_sch.

DATA: BEGIN OF i_wdays OCCURS 0,
         mosid LIKE t552a-mosid,
         schkz LIKE t552a-schkz,
         zeity LIKE t552a-zeity,
         solst LIKE t552a-solst,
         ttday TYPE i,   "Total days
         works TYPE i,   "Working days
         odays TYPE i,   "ot work days

         wkday TYPE i,   "week days
         saday TYPE i,   "SAT days
         suday TYPE i,   "SUN days
         hoday TYPE i,   "HOL days

      END OF i_wdays.

RANGES : pnpkostl FOR pernr-kostl.

*data : begin of it_pern occurs 0,
*         pernr   type pernr_d,
*         kostl   type kostl,
*         emp_fte type pranz,
*       end of it_pern.

* For B/F Quan.
*DATA : it_tmp_cosl       LIKE STANDARD TABLE OF ztco_mhhrtrans
*                         WITH HEADER LINE .
* For A/A Type
DATA : it_ztco_mhaatype  LIKE STANDARD TABLE OF ztco_mhaatype
                         WITH HEADER LINE .

* For DATA retrieval
DATA : BEGIN OF it_catsdb OCCURS 0,
          pernr     LIKE catsdb-pernr,
          workdate  LIKE catsdb-workdate,
          skostl    LIKE catsdb-skostl  , "Sender CCtr
          lstar     LIKE catsdb-lstar   ,
          rkostl    LIKE catsdb-rkostl  , "Rec. CCtr
          awart     LIKE catsdb-awart   ,
          unit      LIKE catsdb-unit    ,
          status    LIKE catsdb-status  ,
          catshours LIKE catsdb-catshours.
DATA : END OF   it_catsdb.


DATA : it_catsdb_item  LIKE STANDARD TABLE OF it_catsdb
                            WITH HEADER LINE .
DATA : it_catsdb_sum   LIKE STANDARD TABLE OF it_catsdb
                            WITH HEADER LINE .

DATA : it_catsdb_item3 LIKE STANDARD TABLE OF it_catsdb
                    WITH HEADER LINE .

DATA : BEGIN OF it_cosl OCCURS 500.
        INCLUDE STRUCTURE zsco_cosl_key01.
        INCLUDE STRUCTURE zsco_cosl_lst01.
DATA : END OF   it_cosl.

DATA : BEGIN OF i_t001p OCCURS 0,
         werks LIKE t001p-werks, "PA
         btrtl LIKE t001p-btrtl, "sub-area
         molga LIKE t001p-molga, "country
         mofid LIKE t001p-mofid, "Public Holiday Calendar
         mosid LIKE t001p-mosid, "Personnel Subarea Grouping for WS
       END OF i_t001p.
DATA : BEGIN OF i_t503 OCCURS 0,
         persg LIKE t503-persg, "Employee Group
         persk LIKE t503-persk, "Employee Subgroup
         zeity LIKE t503-molga, "time type
       END OF i_t503.

* For Getting Inactive MH
DATA : BEGIN OF it_inact OCCURS 0,
         pernr  LIKE pa0000-pernr,
         date   TYPE datum,
         anzhl  LIKE ztco_mha-anzhl,
         dayhc  LIKE ztco_mha-dayhc,
         massn  LIKE pa0000-massn,
         massg  LIKE pa0000-massg,

         kostl  LIKE pa0001-kostl,
         stell  LIKE pa0001-stell,
         persg  LIKE pa0001-persg,
         persk  LIKE pa0001-persk,
       END OF it_inact.

DATA : BEGIN OF it_pa0000 OCCURS 0,
         pernr  LIKE pa0000-pernr,
         endda  LIKE pa0000-endda,
         begda  LIKE pa0000-begda,
         massn  LIKE pa0000-massn,
         massg  LIKE pa0000-massg,
       END OF it_pa0000.

DATA : BEGIN OF it_pa0001 OCCURS 0,
         pernr  LIKE pa0001-pernr,
         begda  LIKE pa0001-begda,
         endda  LIKE pa0001-endda,
         kostl  LIKE pa0001-kostl,
         stell  LIKE pa0001-stell,
         persg  LIKE pa0001-persg,
         persk  LIKE pa0001-persk,
       END OF it_pa0001.


DATA : BEGIN OF it_jc OCCURS 0,
         jobcd  LIKE ztco_jc-jobcd,
         stell  LIKE ztco_jc-stell,
       END OF it_jc.

DATA : g_total_anzhl LIKE ztco_mha-anzhl.

DATA: gv_msg(100).

** Range
RANGES : r_perid    FOR ztco_mhhrtrans-perid.
RANGES:  r_wt       FOR t512w-lgart.

** Data date/time
DATA: g_prv_date LIKE sy-datum,
      g_prv_time LIKE sy-uzeit.
DATA: g_datefr   LIKE sy-datum,
      g_dateto   LIKE sy-datum.

** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.


* Globale Daten
INCLUDE rptbal01.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: g_repid     LIKE sy-repid,
      gt_fieldcat TYPE slis_t_fieldcat_alv,
*      gs_layout   type slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
*---- ALV


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
SELECT-OPTIONS: s_begda FOR pa0001-begda NO-EXTENSION.      "UD1K952870
PARAMETER: p_run   AS CHECKBOX,
           p_cbo   AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bl1.

PARAMETER: p_delta AS CHECKBOX.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-001.
PARAMETERS : p_lstar LIKE csla-lstar  DEFAULT 'MAN_HR' NO-DISPLAY,
             p_ncoal LIKE grpdynp-name_coall    DEFAULT 'HMMA1'.
SELECT-OPTIONS : s_kostl FOR p0001-kostl.
PARAMETERS: p_mofid LIKE t552a-mofid DEFAULT 'U1' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK bl2.

* For HR Time Sheet
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-006.
SELECT-OPTIONS : s_status FOR catsdb-status     DEFAULT '30'  "Approved.
                                                       NO-DISPLAY.
"OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl3.

SELECT-OPTIONS : s_pernr    FOR p0001-pernr,
                 s_workdt   FOR catsdb-workdate NO INTERVALS.
*PARAMETERS: p_prdat LIKE sy-datum,
*            p_prtim LIKE sy-uzeit.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  g_repid = sy-repid.

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
** Calculating Period Count
*  perform cal_per_count.
* Controlling Area Information
  PERFORM read_tka01.
* Read CCtrs
  PERFORM read_cctr.
* Preparation to select data
  PERFORM read_basic_data.
  PERFORM pre_to_select.

  IF p_cbo = 'X'.
    SELECT * INTO TABLE it_time FROM ztco_mha
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND kostl IN pnpkostl
         AND pernr IN s_pernr.
  ELSE.
    IF p_run   = 'X'.
      PERFORM confirm_deletion.
    ENDIF.

*non payroll-------------------
* Attendence/Absence Type
*    perform read_catsdb_aatypes.                           "UD1K952620
* Supporting/Supported.
*    perform read_catsdb_supporting.                        "UD1K952620

*Payroll-----------------------
    PERFORM read_fr_time_eval.

    PERFORM read_hire_fire.
    PERFORM read_pa0000.
    PERFORM collect_mh.
    PERFORM calc_wdays.

*---collect inactive time
    PERFORM collect_inactive_time.

    PERFORM calc_emp_cnt.
    PERFORM calc_avg_fte.

*---collect supporting time
*---issue: net MH ?????
*    perform collect_supporting_time.                       "UD1K952620
*---collect absence time
*    perform collect_absence_time.                          "UD1K952620


  ENDIF.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_delta = 'X'.
    PERFORM capture_delta_changes.
    IF p_run = space.
      it_time[] = it_time_dt[].
    ENDIF.
  ENDIF.

  IF p_cbo = space AND p_run   = 'X'.
* Do not Commit Work or Dequeue explicitly
* LUW will do
    IF p_delta = 'X'.
      PERFORM delta_update.
    ELSE.
      PERFORM db_get.                                       "UD1K952755
      PERFORM delete_table.
      COMMIT WORK.
      PERFORM db_insert.
    ENDIF.

  ELSE.

* Preparation of ALV
    PERFORM pre_report_adj.
* Call ALV LIST
    PERFORM call_alv_list.
  ENDIF.
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
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_t001p
     FROM t001p INNER JOIN t500p
       ON t001p~werks = t500p~persa
     WHERE t500p~bukrs = p_kokrs.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_t503
     FROM t503.

* Get No working daily work scheduling
  TABLES: t550a.
  RANGES: r_tprog FOR t550a-tprog.
  r_tprog-option = 'EQ'.
  r_tprog-sign   = 'I'.
  SELECT * FROM t550a WHERE sollz = 0.
    r_tprog-low = t550a-tprog.
    APPEND r_tprog.
  ENDSELECT.

* Read A/A type
*  CLEAR : it_ztco_mhaatype, it_ztco_mhaatype.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_mhaatype
*           FROM ztco_mhaatype
*          WHERE selind = 'X'.
*
*  IF it_ztco_mhaatype[] IS INITIAL .
*    MESSAGE e042.
*  ENDIF.

* Convert Periods to date Range
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_frper
       IMPORTING
            e_date  = g_datefr.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_toper
       IMPORTING
            e_date  = g_dateto.

* BEGIN OF UD1K952870
  READ TABLE s_begda INDEX 1.
  IF sy-subrc = 0.
    IF s_begda-high IS INITIAL.
      s_begda-high = s_begda-low.
    ENDIF.

    IF s_begda-low >= g_datefr AND s_begda-high <= g_dateto.
      g_datefr = s_begda-low.
      g_dateto = s_begda-high.
    ELSE.
      MESSAGE e000 WITH 'Start Date should be in period:' p_frper.
    ENDIF.
  ENDIF.
* END OF UD1K952870

  DESCRIBE TABLE s_workdt LINES sy-tabix.
  IF sy-tabix = 0.
    s_workdt-low = g_datefr.
    s_workdt-high = g_dateto.
  ELSE.
    IF s_workdt-low < g_datefr.
      s_workdt-low = g_datefr.
    ENDIF.

    IF s_workdt-high IS INITIAL OR s_workdt-high > g_dateto.
      s_workdt-high = g_dateto.
    ENDIF.
  ENDIF.

  s_workdt-sign   = 'I'.
  s_workdt-option = 'BT'.
  APPEND s_workdt.

* Period Range
  CLEAR : r_perid, r_perid[].
* Building Period Range
  r_perid-low    = p_frper.
  r_perid-high   = p_toper.
  r_perid-sign   = 'I'.
  r_perid-option = 'BT'.
  APPEND r_perid.  CLEAR  r_perid.


*FIXME - critical error!!!!
*holiday calendar - exit in T001P
  CALL FUNCTION 'DAY_ATTRIBUTES_GET'
       EXPORTING
            factory_calendar = 'HM'
            holiday_calendar = p_mofid
            date_from        = s_workdt-low
            date_to          = s_workdt-high
       TABLES
            day_attributes   = it_date.

* RPUWSH00   Revaluation of the Planned Working Time Infotype (0007)

* T-code: PT03
* Monthly Work Schedule
  SELECT * FROM t552a
      FOR ALL ENTRIES IN i_t001p
      WHERE mofid = i_t001p-mofid
        AND mosid = i_t001p-mosid
        AND kjahr = p_gjahr
        AND monat = p_toper.
*        and schkz = '2005'.
    MOVE-CORRESPONDING t552a TO it_t552a. APPEND it_t552a.
  ENDSELECT.

  DATA: lstr(6) TYPE c.
  CONCATENATE p_gjahr p_frper+1(2) INTO lstr.
  FIELD-SYMBOLS : <fs> TYPE ANY.
  DATA : num(2) TYPE n.
  DATA : f_field(14).
  DATA: l_idx LIKE sy-tabix.

  LOOP AT it_t552a.
    it_sch-schkz   = it_t552a-schkz  .
    it_sch-zeity   = it_t552a-zeity  .
    it_sch-mosid   = it_t552a-mosid  .

    num = 1.
    DO 31 TIMES.
      CONCATENATE lstr num INTO it_sch-date.

      CONCATENATE 'IT_T552A-TPR' num INTO f_field.
      ASSIGN (f_field) TO <fs>.
      IF <fs> EQ space.
        EXIT.
      ENDIF.
      IF <fs> IN r_tprog.  "'1008'.    "Day Off
        it_sch-freeday = 'X'.
      ELSE.
        it_sch-freeday = ' '.
      ENDIF.
      APPEND it_sch.

* calc work days.
* 3 group + 2 shift; 10 hours; SAT/SUN
* 2 group + 2 shift
      CLEAR i_wdays.
      MOVE-CORRESPONDING it_sch  TO i_wdays.
      CLEAR it_date.                                        "UD1K952620
      READ TABLE it_date WITH KEY date = it_sch-date.

      i_wdays-ttday = 1.

      IF it_date-holiday = 'X'.
        IF it_sch-freeday = ' '.
          i_wdays-hoday = 1.
          i_wdays-works = 1.
        ENDIF.
      ELSE.
        IF it_date-weekday < 6.     "Mo~Fr
          IF it_sch-freeday = ' '.
            i_wdays-wkday = 1.
            i_wdays-works = 1.
          ENDIF.
        ELSEIF it_date-weekday = 6. "SAT
          IF it_sch-freeday = ' '.
            i_wdays-saday = 1.
            i_wdays-works = 1.
          ENDIF.
        ELSE.                       "SUN
          IF it_sch-freeday = ' '.
            i_wdays-suday = 1.
            i_wdays-works = 1.
          ENDIF.
        ENDIF.
      ENDIF.
*         wdays TYPE i,   "week work days
*         odays TYPE i,   "ot work days

*         wkday TYPE i,   "week days
*         saday TYPE i,   "SAT days
*         suday TYPE i,   "SUN days
*         hoday TYPE i,   "HOL days

      COLLECT i_wdays. CLEAR i_wdays.

      CLEAR: it_sch-freeday, it_sch-shift, it_sch-date.
      num = num + 1.
    ENDDO.

    READ TABLE i_wdays WITH KEY schkz   = it_t552a-schkz
                                zeity   = it_t552a-zeity
                                mosid   = it_t552a-mosid.
    l_idx = sy-tabix.
    i_wdays-solst = it_t552a-solst.
    MODIFY i_wdays INDEX l_idx.

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


* WLB3_GET_NUMBER_OF_WORKDAYS

ENDFORM.                    " PRE_TO_SELECT

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
*&      Form  confirm_deletion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirm_deletion.
* local Data definition
  DATA : lv_answer,
         lv_cnt TYPE sy-dbcnt.
  DATA : lv_title(80).

* Begin of changes -  UD1K920571
*  SELECT COUNT( * ) INTO lv_cnt FROM ztco_mha
*           WHERE gjahr = p_gjahr
*             AND perid IN r_perid.
*  IF lv_cnt = 0.
*    SELECT COUNT( * ) INTO lv_cnt FROM ztco_mhat
*             WHERE gjahr = p_gjahr
*               AND perid IN r_perid.
*  ENDIF.
* End of changes  UD1K920571.

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
    ELSE.
*      perform delete_table.
    ENDIF.
  ENDIF.



ENDFORM.                    " confirm_deletion

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
  pnpkostl-sign   = 'I'.
  pnpkostl-option = 'EQ'.

  LOOP AT it_costcenterlist.
    CHECK it_costcenterlist-costcenter IN s_kostl.
    pnpkostl-low    = it_costcenterlist-costcenter.
    APPEND pnpkostl.
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
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.
  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_time
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

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
*      PERFORM update.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.
*  write : / 'Controlling Area                         : '
*            , p_kokrs.
*  write : / 'Fiscal Year/Period/Version/Activity Type : '
*            , p_gjahr, '/', p_frper, '~', p_toper, '/', p_versn
*            , '/', p_lstar.
*  write : / 'Value Type/CO business transaction       : '
*            , p_wrttp, '/',  s_vrgng-low, '~',  s_vrgng-high.

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
  CLEAR : gt_fieldcat, gt_fieldcat[].
  PERFORM field_setting TABLES gt_fieldcat USING :
   'KOSTL'    'CC'           '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*  'ZEITY'    'ZEITY'        '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'JOBCD'    'JC'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'PERNR'    'PERNR'        '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'DTYPE'    'DT'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'LGART'    'WT'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZSH'    'Shift'        '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'SCHKZ'    'WS'           '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZHL'    'ANZHL'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'NETMH'    'NETMH'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_CNT'  'EMP_CNT'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_AVG'  'EMP_AVG'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'EMP_FTE'  'EMP_FTE'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*  'EMP_DAY'  'EMP_DAY'      '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'DAYHC'    'DAYHC'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
   'WDAYS'    'WDAYS'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
   'SOLST'    'SOLST'        '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' '.

ENDFORM.                    " PRE_REPORT_ADJ

*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.

*  add 1 to gv_col_pos.

  ls_fieldcat-tabname    = 'IT_TIME'.

  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.
*  ls_fieldcat-col_pos    = gv_col_pos.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  db_insert
*&---------------------------------------------------------------------*
FORM db_insert.
  DATA: l_it_time LIKE it_time.
* BEGIN OF UD1K952755

  LOOP AT it_time.
* BEGIN OF UD1K953108
    IF it_time-emp_cnt = 1.
      READ TABLE it_time_tmp WITH KEY pernr   = it_time-pernr
                                      emp_cnt = it_time-emp_cnt.
      IF sy-subrc = 0.
        CLEAR it_time-emp_cnt.
      ENDIF.
    ENDIF.
* END OF UD1K953108

    COLLECT it_time into it_time_tmp.
  ENDLOOP.

  it_time[] = it_time_tmp[].

  FREE it_time_tmp.

* BEGIN OF UD1K953108
* BEGIN OF UD1K953104 - Correct Duplicated Employee Count
*  LOOP AT it_time where emp_cnt > 1.
*    it_time-emp_cnt = 1.
*    MODIFY it_time INDEX SY-TABIX TRANSPORTING emp_cnt.
*  ENDLOOP.
* END OF UD1K953104
* END OF UD1K953108

  LOOP AT it_timet_tmp.
    CLEAR: it_timet_tmp-mandt.
    COLLECT it_timet_tmp into it_timet.
  ENDLOOP.

  FREE it_timet_tmp.

  LOOP AT it_timed_tmp.
    CLEAR: it_timed_tmp-mandt.
    COLLECT it_timed_tmp into it_timed.
  ENDLOOP.

  FREE it_timed_tmp.
* END OF UD1K952755

  l_it_time-erdat = sy-datum.
  l_it_time-erzet = sy-uzeit.
  l_it_time-ernam = sy-uname.

  MODIFY it_time FROM l_it_time TRANSPORTING erdat erzet ernam
         WHERE kokrs EQ p_kokrs AND
               gjahr EQ p_gjahr.

* BEGIN OF UD1K952620
*  INSERT ztco_mha02  FROM TABLE it_time.                   "UD1K952620
*  INSERT ztco_mhat02 FROM TABLE it_timet.                  "UD1K952620
*  INSERT ztco_mhad02 FROM TABLE it_timed.                  "UD1K952620

  INSERT ztco_mha  FROM TABLE it_time.
  IF sy-subrc = 0.
    INSERT ztco_mhat FROM TABLE it_timet.

    IF sy-subrc = 0.
      INSERT ztco_mhad FROM TABLE it_timed.
    ENDIF.
  ENDIF.
* END OF UD1K952620

*    MODIFY ztco_mha  FROM TABLE it_time.
*    IF sy-subrc = 0.
*      MODIFY ztco_mhat FROM TABLE it_timet.
*    ENDIF.
*    IF sy-subrc = 0.
*      MODIFY ztco_mhad FROM TABLE it_timed.
*    ENDIF.

  IF sy-subrc <> 0.
    MESSAGE e044.
  ELSE.
    MESSAGE s009 WITH 'Data Inserted'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  delta_update
*&---------------------------------------------------------------------*
FORM delta_update.
  DATA: l_it_time LIKE it_time.
  l_it_time-erdat = sy-datum.
  l_it_time-erzet = sy-uzeit.
  l_it_time-ernam = sy-uname.

  MODIFY it_time_dt
         FROM l_it_time TRANSPORTING erdat erzet ernam
         WHERE kokrs EQ p_kokrs AND
               gjahr EQ p_gjahr.

  MODIFY ztco_mha  FROM TABLE it_time_dt.
  IF sy-subrc = 0.
    MODIFY ztco_mhat FROM TABLE it_timet_dt.
  ENDIF.
  IF sy-subrc = 0.
    MODIFY ztco_mhad FROM TABLE it_timed_dt.
  ENDIF.

  IF sy-subrc <> 0.
    MESSAGE e044.
  ELSE.
    MESSAGE s009 WITH 'Delta Updated'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_fr_time_eval
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_time_eval.

* Ingnore A/A type
* HR source has no indicator to check if supportive hour or
* non-supportive hour

  DATA : pnpbegda LIKE qppnp-begda,
         pnpendda LIKE qppnp-endda,
         pnptimr6 LIKE qppnp-timr6.
  DATA : sw_zl    LIKE rptxxxxx-kr_feld3.

  DATA : it_l_rsparams LIKE STANDARD TABLE OF rsparams
                       WITH HEADER LINE .
*
*  call function 'RS_REFRESH_FROM_SELECTOPTIONS'
*    exporting
*      curr_report           = 'ZACO03U_HRMH'
**   IMPORTING
**     SP                    =
*    tables
*      selection_table       = it_l_rsparams
*    exceptions
*      not_found             = 1
*      no_report             = 2
*      others                = 3.
*
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  endif.
*
** Put data
*  pnptimr6 = 'X'.
*  sw_zl    = 'X'.
*
*  clear s_workdt.
*  read table s_workdt index 1.
*  pnpbegda = s_workdt-low.
*  pnpendda = s_workdt-high.
*
*  free memory id 'HRM'.
*  submit zaco03u_hrmh
**    VIA SELECTION-SCREEN
*    and return
*    with selection-table it_l_rsparams
*    with pnppernr  in s_pernr
*    with pnptimr6 = pnptimr6
*    with pnpbegda = pnpbegda
*    with pnpendda = pnpendda
*    with sw_zl    = sw_zl           "only time wage
*    with lgart    in r_wt
*    with pnpkostl in pnpkostl.
*
*
**<data_tab>
*  import time_data_zes   = time_data_zes
*         time_data_saldo = time_data_saldo
*         time_data_zl    = time_data_zl
*         data_tab        = data_tab
*         from memory id 'HRM'.

  DATA: t_pa2001 LIKE pa2001 OCCURS 0 WITH HEADER LINE,
        t_pa2002 LIKE pa2002 OCCURS 0 WITH HEADER LINE,
        t_pa0001 LIKE pa0001 OCCURS 0 WITH HEADER LINE,
        t_pa0007 LIKE pa0007 OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_asshr OCCURS 0,
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

  SELECT * INTO TABLE t_pa0001
    FROM pa0001
   WHERE pernr IN s_pernr
     AND endda = '99991231'.

  SELECT * INTO TABLE t_pa0007
    FROM pa0007
   WHERE pernr IN s_pernr
     AND endda = '99991231'.


  SELECT * INTO TABLE t_pa2001
    FROM pa2001
   WHERE pernr IN s_pernr
     AND begda BETWEEN g_datefr AND g_dateto.

  SELECT * INTO TABLE t_pa2002
    FROM pa2002
   WHERE pernr IN s_pernr
     AND begda BETWEEN g_datefr AND g_dateto.

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

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_pa2002
    FROM pa2001
   WHERE pernr IN s_pernr
     AND begda BETWEEN g_datefr AND g_dateto.

  SORT: t_pa0001 BY pernr,
        t_pa0007 BY pernr,
        t_pa2001 BY pernr begda,
        t_pa2002 BY pernr begda,
        t_asshr.


** Re-org.
  DATA : lv_tabcol(30).
  DATA : lv_kostl(5) VALUE 'KOSTL', lv_anzhl(5) VALUE 'ANZHL',
         lv_pernr(5) VALUE 'PERNR', "emp#
         lv_datum(5) VALUE 'DATUM', "date
         lv_schkz(5) VALUE 'SCHKZ', "shift
         lv_stell(5) VALUE 'STELL', "job
         lv_lgart(5) VALUE 'LGART', "wagetype
         lv_zeity(5) VALUE 'ZEITY', "subgroup for workschedule
         lv_mosid(5) VALUE 'MOSID', "Psub group for workschedule
         lv_werks(5) VALUE 'WERKS',
         lv_persg(5) VALUE 'PERSG',
         lv_persk(5) VALUE 'PERSK',
         lv_btrtl(5) VALUE 'BTRTL'.

  DATA : l_lgart    TYPE lgart.

  FIELD-SYMBOLS : <fstab> TYPE table,
                  <fswa>  TYPE ANY,
                  <fsfn>  TYPE ANY,
                  <fsval> TYPE ANY.

  CONCATENATE data_tab '[' ']' INTO lv_tabcol.
  ASSIGN (lv_tabcol) TO <fstab>.


*  ASSIGN LOCAL COPY OF INITIAL LINE OF <fstab> TO <fswa>.
* If no data found in HR tables
*  IF sy-subrc <> 0.
*    MESSAGE s000 WITH text-101.
*    EXIT.
*  ENDIF .

  DATA: l_stell LIKE pa0001-stell.

* BEGIN OF UD1K952620
*  LOOP AT <fstab> ASSIGNING <fswa>.
  LOOP AT t_pa2002.
    CLEAR: t_pa0001, t_pa0007.

    READ TABLE t_pa0001 WITH KEY pernr = t_pa2002-pernr
                                 BINARY SEARCH.

    READ TABLE t_pa0007 WITH KEY pernr = t_pa2002-pernr
                                 BINARY SEARCH.

* CCTR -KOSTL
*    ASSIGN lv_kostl TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-kostl = <fsval> .

    py_times-kostl = t_pa0001-kostl.



* MAN_HR -ANZHL
*    ASSIGN lv_anzhl TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-anzhl = <fsval>.

    py_times-anzhl = t_pa2002-stdaz.


* WageType
*1	Regular Working (weekday,weekend)
*2	OT
*5	Paid Leave
*9	Holiday
*A	Absence Time
*U	Unpaid Leave
*    ASSIGN lv_lgart TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    l_lgart = <fsval> .
*    READ TABLE i_timetype WITH KEY lgart = l_lgart.
*    py_times-lgart  = i_timetype-zgart.
*    py_times-lgart2 = l_lgart.

    py_times-lgart2 = t_pa2002-awart.

*HEADER
*    ASSIGN lv_werks TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-werks = <fsval> .

    py_times-werks = t_pa0001-werks.

*    ASSIGN lv_persg TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-persg = <fsval> .

    py_times-persg = t_pa0001-persg.

*    ASSIGN lv_persk TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-persk = <fsval> .

    py_times-persk = t_pa0001-persk.

*    ASSIGN lv_btrtl TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-btrtl = <fsval> .

    py_times-btrtl = t_pa0001-btrtl.


* work schedule rule
*  1000 - standard
*  1001 - admin 1st  1002 - admin 2nd
*  2001 - PP 1st,    2002 - PP 2nd
*  3001 - PM 1st,    3002 - PM 2nd  3003 - PM 3rd
* Refer T508A table...
*    ASSIGN lv_schkz TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-schkz = <fsval> .

    py_times-schkz = t_pa0007-schkz.

    READ TABLE i_t001p WITH KEY werks = py_times-werks
                                btrtl = py_times-btrtl.
    IF sy-subrc = 0.
      py_times-mofid = i_t001p-mofid.
      py_times-mosid = i_t001p-mosid.
    ENDIF.

* DATE
*    ASSIGN lv_datum TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-datum = <fsval> .

    py_times-datum = t_pa2002-begda.

* EMP#
*    ASSIGN lv_pernr TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    py_times-pernr = <fsval> .

    py_times-pernr = t_pa2002-pernr.

*'A' US-Salary, 'B' US-Wage, 'K' KR-Salary
*    IF py_times-persg = '9'.
*      py_times-empct = 'K'.
*    ELSEIF py_times-zeity = '1'.
*      py_times-empct = 'B'.
*    ELSE.
*      py_times-empct = 'A'.
*    ENDIF.
    PERFORM get_emp_categ(zacop01) USING py_times-persg py_times-persk
                                   CHANGING py_times-empct.

* time type: 1 - hourly, 2 - salary
    READ TABLE i_t503 WITH KEY persg = py_times-persg
                              persk = py_times-persk.
    IF sy-subrc = 0.
      py_times-zeity = i_t503-zeity.
    ENDIF.

* job
*    ASSIGN lv_stell TO <fsfn>.
*    ASSIGN COMPONENT <fsfn> OF STRUCTURE <fswa> TO <fsval>.
*    l_stell = <fsval>.

    l_stell = t_pa0001-stell.

    READ TABLE i_jobcd WITH KEY stell = l_stell.
    IF sy-subrc = 0.
      py_times-jobcd = i_jobcd-jobcd.
    ELSE.
      py_times-jobcd = 'XX'.
    ENDIF.

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

    IF sy-subrc = 0.
      py_times-srkostl = t_asshr-kostl.
    ENDIF.

    COLLECT py_times.

*    CLEAR <fswa>.

*Employee
    i_emp-pernr = py_times-pernr. APPEND i_emp.

    CLEAR py_times.
  ENDLOOP.
* END OF UD1K952620

  DELETE ADJACENT DUPLICATES FROM i_emp.
ENDFORM.                    " read_fr_time_eval
*&---------------------------------------------------------------------*
*&      Form  read_catsdb_aatypes
*&---------------------------------------------------------------------*
FORM read_catsdb_aatypes.

  RANGES : r_awart FOR catsdb-awart.
  r_awart-sign   = 'I'.
  r_awart-option = 'EQ'.
  SELECT * FROM ztco_mh_time
     WHERE ( zgart = 'B' AND flag  = 'X' )
        OR   zgart = 'A'.
    r_awart-low = ztco_mh_time-lgart. APPEND r_awart.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE e026.
  ENDIF.

* Select data (approved status: 30)
  SELECT pernr
         workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item
         FROM catsdb
        WHERE awart    IN r_awart
          AND status   IN s_status
          AND workdate IN s_workdt
          AND pernr    IN s_pernr.

  LOOP AT it_catsdb_item.
    it_catsdb_sum-pernr     = it_catsdb_item-pernr.
    it_catsdb_sum-workdate  = it_catsdb_item-workdate.
    it_catsdb_sum-catshours = it_catsdb_item-catshours.
    COLLECT it_catsdb_sum.
  ENDLOOP.

  SORT it_catsdb_sum BY pernr workdate.

*----deleted logic----
*date, time
*  g_cur_date = sy-datum.
*  g_cur_time = sy-uzeit.
*  SELECT SINGLE MAX( erdat ) MAX( erzet )
*           INTO (g_prv_date, g_prv_time)
*           FROM ztco_mha
*           WHERE gjahr = p_gjahr
*             AND perid IN r_perid.
*
*
*
** check index of table : Z02
*  CLEAR : it_catsdb_item, it_catsdb_item[].
*
*  IF NOT g_prv_date IS INITIAL.
*    SELECT workdate  skostl  lstar   rkostl
*           awart     unit    status  catshours
*           INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item
*           FROM catsdb
*          WHERE status   IN s_status
*            AND workdate IN s_workdt
*            AND ersda >= g_prv_date
*            AND erstm >  g_prv_time.
**..old data don't have date/time stamp...
*  ELSE.
*    SELECT workdate  skostl  lstar   rkostl
*           awart     unit    status  catshours
*           INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item
*           FROM catsdb
*          WHERE status   IN s_status
*            AND workdate IN s_workdt
*            AND ersda >= p_prdat
*            AND erstm >  p_prtim.
*  ENDIF.
ENDFORM.                    " read_catsdb_aatypes
*&---------------------------------------------------------------------*
*&      Form  read_catsdb_supporting
*&---------------------------------------------------------------------*
*       Cal. Supportive and Not-Supportive Working Hour
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_catsdb_supporting.
*FIXME
* need to collect ztco_mha information... later

* TimeSheet  (Summed)
*  clear it_tmp_catsdb.
*  sort  it_tmp_catsdb by gjahr perid kostl lstar.

  RANGES : r_awart FOR catsdb-awart.
  r_awart-sign   = 'I'.
  r_awart-option = 'EQ'.
  SELECT * FROM ztco_mh_time
     WHERE zgart = '3'
       AND flag  = 'X'.
    r_awart-low = ztco_mh_time-lgart. APPEND r_awart.
  ENDSELECT.

  IF sy-subrc <> 0.
    MESSAGE e026.
  ENDIF.

* Local Data definition

* Select data (approved status: 30)
  CLEAR : it_catsdb_item3, it_catsdb_item3[].
  SELECT pernr     workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         INTO CORRESPONDING FIELDS OF TABLE it_catsdb_item3
         FROM catsdb
        WHERE awart    IN r_awart
          AND status   IN s_status
          AND workdate IN s_workdt
          AND pernr    IN s_pernr
          AND catshours <> 0.

  IF  it_catsdb_item3[] IS INITIAL.
*    MESSAGE E026.
  ENDIF.


*FIXIT LATER - ANDY - start...
* Cal.
*  data : it_l_catadd like standard table of it_tmp_catsdb
*                     with header line .
*  loop at it_l_calwsig.
*    loop at it_tmp_catsdb where  kostl  = it_l_calwsig-kostl.
*      it_tmp_catsdb-actqty = it_tmp_catsdb-actqty
*                           + it_l_calwsig-catshours.
*      modify it_tmp_catsdb.
*      clear  it_tmp_catsdb.
*    endloop.
*    if sy-subrc <> 0.
*      move-corresponding it_l_calwsig to it_l_catadd.
*      it_l_catadd-gjahr = p_gjahr.
*      it_l_catadd-perid = p_frper.
*      it_l_catadd-lstar = p_lstar.
*      it_l_catadd-unit = 'STD'.
*      it_l_catadd-actqty = it_l_calwsig-catshours.
*      collect it_l_catadd.
*    endif.
*    clear it_l_catadd.
*    clear it_l_calwsig.
*  endloop.
*
*  loop at it_l_catadd.
*    clear  it_tmp_catsdb.
*    move-corresponding it_l_catadd to it_tmp_catsdb.
*    collect it_tmp_catsdb.
*    clear   it_tmp_catsdb.
*  endloop.
*  clear  it_tmp_catsdb.
*FIXIT LATER - ANDY - end.....



*deleted...logic
** Sender '-'
*  SORT it_catsdb_item3 BY SKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT it_catsdb_item3 WHERE  SKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           - it_catsdb_item3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.
*
** Receiver '-'
*  SORT it_catsdb_item3 BY RKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT it_catsdb_item3 WHERE  RKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           + it_catsdb_item3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.

ENDFORM.                    " read_catsdb_supporting

*
*&---------------------------------------------------------------------*
*&      Form  read_basic_data
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
FORM read_basic_data.
* Jobcode
  SELECT * INTO TABLE i_jobcd
     FROM ztco_jc.


  SELECT * INTO TABLE i_wksch
     FROM ztco_mh_ws
     WHERE kokrs = p_kokrs.

  SELECT * INTO TABLE i_timetype
     FROM  ztco_mh_time.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Maintain Time Types'.
  ENDIF.
*  DELETE i_timetype WHERE flag = 'X'.                      "UD1K952620

  REFRESH r_wt. CLEAR r_wt.
  r_wt-sign   = 'I'. r_wt-option = 'EQ'.

*exclude inactive.
  LOOP AT i_timetype WHERE zgart <> 'V'.
    r_wt-low  = i_timetype-lgart.  APPEND r_wt.
  ENDLOOP.

* BEGIN OF UD1K949773
  SELECT * INTO TABLE i_dwksch
    FROM ztco_mh_dws.
* END OF UD1K949773
ENDFORM.                    " read_basic_data
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
*&      Form  calc_avg_fte
*&---------------------------------------------------------------------*
FORM calc_avg_fte.
  DATA: l_tabix LIKE sy-tabix.
  DATA: l_arbst LIKE pa0007-arbst,
        l_wdays LIKE it_time-wdays.
*.. regular working, leave, holiday
  DATA: l_solst LIKE t552a-solst,
        l_sols2 LIKE t552a-solst.


* calculate FTE with regular working
  SORT it_time BY pernr
                  lgart.
*                 datum
*                  kostl.

* by ig.moon {
  SORT i_wdays BY mosid schkz.
* }

* REGULAR + OT + Supporting/Supported
  LOOP AT it_time WHERE lgart <= '3'.

    CLEAR i_wdays.                                          "UD1K949773
    READ TABLE i_wdays WITH KEY mosid = it_time-mosid
                                schkz = it_time-schkz
* by ig.moon {
                                BINARY SEARCH.
* }

*                               zeity = it_time-zeity.  "FIXME!!!
    l_solst = i_wdays-solst.
    l_wdays = i_wdays-works.

    IF l_solst = 0.
      it_time-emp_fte = 0.
    ELSE.
      it_time-emp_fte = it_time-anzhl / l_solst.
    ENDIF.

*consider only regulary working
    DATA: l_maxdh LIKE it_time-dayhc.
    IF it_time-wdays > 0.           "dtype = '1'.
      IF l_wdays <> 0.                                      "UD1K949773
        IF it_time-dayhc > it_time-wdays.
          it_time-emp_avg = it_time-wdays / l_wdays.
        ELSE.
          it_time-emp_avg = it_time-dayhc / l_wdays.
        ENDIF.
      ENDIF.                                                "UD1K949773
    ENDIF.

    it_time-solst   = l_solst.

*only update Headcnt
    CLEAR: it_time-emp_cnt,
           it_time-dayhc,  "it_time-emp_day,
           it_time-anzhl,   it_time-netmh,
           it_time-wdays.
    COLLECT it_time.
  ENDLOOP.

ENDFORM.                    " calc_avg_fte
*&---------------------------------------------------------------------*
*&      Form  read_hire_fire
*&---------------------------------------------------------------------*
FORM read_hire_fire.
  DATA: l_hire LIKE sy-datum,
        l_fire LIKE sy-datum.
  DATA: BEGIN OF phifi OCCURS 5.
          INCLUDE STRUCTURE phifi.
  DATA: END OF phifi.
  DATA: error_table LIKE STANDARD TABLE OF rpbenerr.

  LOOP AT i_emp.
    CALL FUNCTION 'HR_CLM_GET_ENTRY_LEAVE_DATE'
         EXPORTING
              pernr       = i_emp-pernr
              begda       = s_workdt-low
              endda       = s_workdt-high
         IMPORTING
              hire_date   = i_emp-hire
              fire_date   = i_emp-fire
         TABLES
              error_table = error_table
              phifi       = phifi.

*HR IT error
    IF i_emp-fire IS INITIAL.
      CLEAR phifi.                                          "UD1K952620
      READ TABLE phifi WITH KEY stat2 = '0'.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                date      = phifi-begda
                days      = 1
                months    = '00'
                signum    = '-'
                years     = '00'
           IMPORTING
                calc_date = i_emp-fire.

    ENDIF.

    SELECT * FROM pa0001
      WHERE pernr = i_emp-pernr
        AND endda >= s_workdt-low
      ORDER BY endda ASCENDING.
      i_emp-werks = pa0001-werks.
      i_emp-btrtl = pa0001-btrtl.
      EXIT.
    ENDSELECT.
    IF sy-subrc = 0.
*-----Personnel Subarea Grouping for Work Schedules
      CLEAR i_t001p.                                        "UD1K952620
      READ TABLE i_t001p WITH KEY werks = i_emp-werks
                                  btrtl = i_emp-btrtl.
      i_emp-mosid = i_t001p-mosid.
    ENDIF.
* Get Last CC
*  select single * kostl  into i_emp-kostl
*    from pa0001
*    where pernr = i_emp-pernr
*      and endda >= s_workdt-low.

    MODIFY i_emp.
  ENDLOOP.

* work time
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_pa7
    FROM pa0007
    FOR ALL ENTRIES IN i_emp
    WHERE pernr = i_emp-pernr
      AND endda >= s_workdt-low.

ENDFORM.                    " read_hire_fire
*&---------------------------------------------------------------------*
*&      Form  collect_mh
*&---------------------------------------------------------------------*
FORM collect_mh.

  DATA: l_dtype(1) TYPE c,
        l_datum    LIKE sy-datum.
  DATA: l_arbst LIKE pa0007-arbst.

  SORT py_times BY pernr datum DESCENDING.

* by ig.moon {
  SORT : i_emp BY pernr,
         i_wksch BY mosid schkz.
  SORT i_wdays BY mosid schkz.
* }

  LOOP AT py_times.
*Check Fire Date again.
    CLEAR i_emp.                                            "UD1K952620
    READ TABLE i_emp WITH KEY pernr = py_times-pernr
* by ig.moon. {
    BINARY SEARCH.
* }
    CHECK i_emp-fire > s_workdt-low.

    it_time-kokrs = p_kokrs.
    it_time-gjahr = p_gjahr.
    it_time-perid = p_frper.

* CCTR -KOSTL
    it_time-kostl = py_times-kostl.
* MAN_HR -ANZHL
    it_time-anzhl = py_times-anzhl.
* WageType
    it_time-lgart = py_times-lgart.

* EMP#
    it_time-pernr = py_times-pernr.

* commented by ig.moon {
*    read table i_emp with key pernr = it_time-pernr.
* }

* work schedule rule
    it_time-mosid = py_times-mosid.
    it_time-schkz = py_times-schkz.
*   it_time-zeity = py_times-zeity.  "FIXME

* subgroup / group -> category
    it_time-empct = py_times-empct.

* job
    it_time-jobcd = py_times-jobcd.

* BEGIN OF UD1K949773
    DATA: tprog TYPE tprog.

    CALL FUNCTION 'Z_CO_GET_DWS_IG'
         EXPORTING
              schkz                          = py_times-schkz
              datum                          = py_times-datum
         IMPORTING
              tprog                          = tprog
         EXCEPTIONS
              not_found_work_schedule_rules  = 1
              invalid_date                   = 2
              not_found_period_work_schedule = 3
              OTHERS                         = 4.

    IF sy-subrc <> 0.
      tprog = py_times-schkz.
    ENDIF.

    it_time-schkz = tprog.

    LOOP AT i_dwksch WHERE mosid = it_time-mosid
                       AND schkz = it_time-schkz
                       AND zsdat <= py_times-datum
                       AND zedat >= py_times-datum.

      IF NOT i_dwksch-zshif IS INITIAL.
        it_time-anzsh = i_dwksch-zshif.
      ENDIF.

      EXIT.
    ENDLOOP.

    IF sy-subrc <> 0.
      MESSAGE s000 WITH 'Check WS-Shift configuration'
                        it_time-mosid
                        it_time-schkz
                        py_times-datum.
    ENDIF.

** Shift (default=1)
*    it_time-anzsh = 1.
*    read table i_wksch with key mosid = it_time-mosid
*                                schkz = it_time-schkz
** by ig.moon. {
*                                binary search.
** }
*    if sy-subrc <> 0.
*      message e000 with 'Check WS-Shift configuration'
*                        it_time-mosid
*                        it_time-schkz.
*    endif.
*    if sy-subrc = 0.
*      it_time-anzsh = i_wksch-anzsh.
*    endif.
* END OF UD1K949773

*get work days.
    CLEAR: i_wdays.
    READ TABLE i_wdays WITH KEY mosid = it_time-mosid
                                schkz = it_time-schkz
* by ig.moon {
                                BINARY SEARCH.
* }
*                               zeity = it_time-zeity. "FIXME

*-----get STD working hour
    IF i_wdays-works = 0.
* BEGIN OF UD1K949773
*     break-point.
*     message s000 with 'Check Monthly Work Schedule (T552A)'
*                       it_time-mosid
*                       it_time-schkz.
* END OF UD1K949773
    ELSE.
      l_arbst = i_wdays-solst / i_wdays-works.
    ENDIF.

* DATE
    l_datum = py_times-datum.
    CLEAR it_date.                                          "UD1K952620
    READ TABLE it_date WITH KEY date = l_datum.
    IF it_date-holiday = 'X'.
*      it_time-holmh = it_time-anzhl.
*      it_time-holhc = 1.
      l_dtype = '2'.
    ELSE.
      IF it_date-weekday < 6.    "Mo~Fr
        IF it_date-freeday = 'X'.  "plant shut-down
*          it_time-holmh = it_time-anzhl.
*          it_time-holhc = 1.
          l_dtype = '2'.   "HMC
        ELSE.
*          it_time-daymh = it_time-anzhl.
*          it_time-dayhc = 1.
          l_dtype = '1'.
        ENDIF.
      ELSEIF it_date-weekday = 6. "SAT
*        it_time-satmh = it_time-anzhl.
*        it_time-sathc = 1.
        l_dtype = '5'.
      ELSE.                       "SUN
*        it_time-sunmh = it_time-anzhl.
*        it_time-sunhc = 1.
        l_dtype = '3'.
      ENDIF.
    ENDIF.
*---1=Wk working, 2=Wk OT, 5=SAT, 3=SUN
    it_time-dtype = l_dtype.

*-----------------------------------------------------------
*    LOOP AT i_pa7 WHERE pernr = it_time-pernr.
*      IF  py_times-datum >= i_pa7-begda
*      AND py_times-datum <= i_pa7-endda.
*        l_arbst = i_pa7-arbst.
*      ENDIF.
*    ENDLOOP.

    CLEAR i_timetype.                                       "UD1K952620
    READ TABLE i_timetype WITH KEY lgart = py_times-lgart2.
    IF sy-subrc NE 0.                                       "UD1K952620
      WRITE: /'Wage Type:', py_times-lgart2,                "UD1K952620
              'is not exist in table ZTCO_MH_TIME'.         "UD1K952620
    ENDIF.                                                  "UD1K952620
    it_time-lgart  = i_timetype-zgart.

*WHAT IT IS???
*    IF l_dtype <> '1' AND i_timetype-zgart = '1'
*    AND l_arbst = 8.
*      it_time-lgart  = '2'.  "Extra Work
*    ENDIF.

*-----------------------------------------------------------
* Max = 1, WK hour / STD hour
* Regular working only
    IF it_time-lgart = '1'.
      IF it_time-anzhl >= l_arbst.
        it_time-dayhc = 1.
      ELSE.
        it_time-dayhc = it_time-anzhl / l_arbst.
      ENDIF.

*...Clear Weekday Overtime HC
      IF l_dtype = '1'  AND it_time-lgart = '2'.
        CLEAR: it_time-dayhc.
      ENDIF.
    ENDIF.

* CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'

*-----------------------------------------------------------
* Date : valid from - ztco_mha
*FIXME!!!
*    READ TABLE it_time INTO gw_time
*      WITH KEY pernr = it_time-pernr
*               kostl = it_time-kostl.
*    IF sy-subrc = 0.
*      it_time-datum = gw_time-datum.
*    ELSE.
*      it_time-datum = l_datum.
*    ENDIF.
*-----------------------------------------------------------
* Net M/H
*supporting?????
*....Regular / OT
    IF it_time-lgart = '1'.
      it_time-netmh = it_time-anzhl.
      CLEAR it_catsdb_sum.                                  "UD1K952620
      READ TABLE it_catsdb_sum WITH KEY pernr    = it_time-pernr
                                        workdate = l_datum
                               BINARY SEARCH.
      IF sy-subrc = 0.
        it_time-netmh = it_time-netmh - it_catsdb_sum-catshours.
      ENDIF.
    ENDIF.

*Break-Time; Hard Coding...
*   IF it_time-lgart CA '12'.                               "UD1K952620
    IF it_time-lgart CA '123'.                              "UD1K952620
      IF it_time-kostl(2) = 'MX'.
        PERFORM calc_net_mh USING    it_time-anzhl
                            CHANGING it_time-netmh.
      ELSE.
        it_time-netmh = it_time-anzhl.
      ENDIF.
    ENDIF.

* get receiver cost center
    it_time-srkostl = py_times-srkostl.
    IF NOT py_times-srkostl IS INITIAL.                     "UD1K952620
      it_time-anzhl = - it_time-anzhl.                      "UD1K952620
      it_time-netmh = - it_time-netmh.                      "UD1K952620
    ENDIF.                                                  "UD1K952620

* add to t/d
    PERFORM add_to_time_sum_detail USING l_datum
                                         py_times-lgart2.
    COLLECT: it_time.

*   Receiver
    IF NOT py_times-srkostl IS INITIAL.
      it_time-kostl   = py_times-srkostl.
      it_time-srkostl = py_times-kostl.
      it_time-anzhl   = - it_time-anzhl.
      it_time-netmh   = - it_time-netmh.

      PERFORM add_to_time_sum_detail USING l_datum
                                           py_times-lgart2.
      COLLECT: it_time.

    ENDIF.

    CLEAR: it_time.
  ENDLOOP.

ENDFORM.                    " collect_mh
*&---------------------------------------------------------------------*
*&      Form  delete_table
*&---------------------------------------------------------------------*
FORM delete_table.
* BEGIN OF UD1K952620
*  DELETE FROM ztco_mha02
*       WHERE gjahr = p_gjahr
*         AND perid IN r_perid
*         AND pernr IN s_pernr.
*
*  DELETE FROM ztco_mhat02
*     WHERE gjahr = p_gjahr
*       AND perid IN r_perid.
*
*  DELETE FROM ztco_mhad02
*     WHERE gjahr = p_gjahr
*       AND perid IN r_perid
*       AND pernr IN s_pernr.

  DELETE FROM ztco_mha
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND pernr IN s_pernr.

  DELETE FROM ztco_mhat
       WHERE gjahr = p_gjahr
         AND perid IN r_perid.


  DELETE FROM ztco_mhad
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND pernr IN s_pernr.
* END OF UD1K952620

  EXIT.
  DELETE FROM ztco_mha
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND kostl IN pnpkostl.

  DELETE FROM ztco_mhat
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND kostl IN pnpkostl.

  DELETE FROM ztco_mhad
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND kostl IN pnpkostl.
ENDFORM.                    " delete_table
*&---------------------------------------------------------------------*
*&      Form  calc_wdays
*&---------------------------------------------------------------------*
FORM calc_wdays.
  DATA: l_tabix LIKE sy-tabix.

* by ig.moon {
  SORT i_wdays BY mosid schkz.
* }

*working HC (exclude overtime)
  LOOP AT it_time.
    l_tabix = sy-tabix.

    CLEAR i_wdays.                                          "UD1K949773
    READ TABLE i_wdays WITH KEY mosid = it_time-mosid
                                schkz = it_time-schkz
                                BINARY SEARCH.
*                                zeity = it_time-zeity. "FIXME

* only production member has OT days.
    CLEAR i_jobcd.                                          "UD1K952620
    READ TABLE i_jobcd WITH KEY jobcd = it_time-jobcd.
    IF i_jobcd-trfgr = space.
      CLEAR: i_wdays-hoday, i_wdays-suday, i_wdays-saday.
    ENDIF.

*Weekday + Normal Work
*Normal Work + OT
*    IF ( it_time-dtype = '1'  AND it_time-lgart = '1' )
*    OR ( it_time-dtype <> '1' AND it_time-lgart CA '12' ).
    IF it_time-lgart = '1'.
      CASE it_time-dtype.
        WHEN '1'.
          IF i_wdays-wkday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-wkday.
            it_time-wdays   = i_wdays-wkday.
          ENDIF.
        WHEN '2'.
          IF i_wdays-hoday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-hoday.
            it_time-wdays   = i_wdays-hoday.
          ENDIF.
        WHEN '3'.
          IF i_wdays-suday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-suday.
            it_time-wdays   = i_wdays-suday.
          ENDIF.
        WHEN '5'.
          IF i_wdays-saday > 0.
*           it_time-emp_day = it_time-dayhc / i_wdays-saday.
            it_time-wdays   = i_wdays-saday.
          ENDIF.
      ENDCASE.
    ENDIF.

*    it_time-wdays = i_wdays-wdays.
*    it_time-odays = i_wdays-odays.
    MODIFY it_time INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " calc_wdays
*&---------------------------------------------------------------------*
*&      Form  calc_emp_cnt
*&---------------------------------------------------------------------*
FORM calc_emp_cnt.
  DATA: l_tabix LIKE sy-tabix.
  DATA : l_stat2 LIKE pa0000-stat2.

  REFRESH it_pa0001.
  SELECT pernr endda begda kostl stell persg persk
    APPENDING CORRESPONDING FIELDS OF TABLE it_pa0001
    FROM pa0001
    FOR ALL ENTRIES IN i_emp
    WHERE pernr = i_emp-pernr
      AND endda >= g_dateto
      AND begda <= g_dateto.
  DELETE ADJACENT DUPLICATES FROM it_pa0001.
  SORT it_pa0001 BY pernr.

* Headcount - valid on end of month(working day)
  SORT it_time BY pernr lgart kostl.

  LOOP AT i_emp.
    CLEAR l_stat2.
*0	Withdrawn
*1	Inactive
*2	Retiree
*3	Active
    SELECT SINGLE stat2 INTO l_stat2 FROM pa0000
      WHERE pernr = i_emp-pernr
        AND ( begda <= g_dateto AND endda >= g_dateto  ).

    CLEAR it_pa0001.                                        "UD1K952620
    READ TABLE it_pa0001 WITH KEY pernr = i_emp-pernr BINARY SEARCH.
    IF l_stat2 = '3'.   "Active
      READ TABLE it_time WITH KEY pernr = i_emp-pernr
                                  lgart = '1'
                                  kostl = it_pa0001-kostl
                         BINARY SEARCH.
      IF sy-subrc = 0.
        l_tabix = sy-tabix.
        it_time-emp_cnt = 1.
        MODIFY it_time INDEX l_tabix.
      ENDIF.

    ELSEIF l_stat2 = '1'.   "Inactive
      READ TABLE it_time WITH KEY pernr = i_emp-pernr
                                  lgart = 'V'
                                  kostl = it_pa0001-kostl
                         BINARY SEARCH.
      IF sy-subrc = 0.
        l_tabix = sy-tabix.
        it_time-emp_cnt = 1.
        MODIFY it_time INDEX l_tabix.
      ELSE.
        CONCATENATE i_emp-pernr ' is inactive, but no time' INTO gv_msg.
        PERFORM show_warning.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " calc_emp_cnt
*&---------------------------------------------------------------------*
*&      Form  capture_delta_changes
*&---------------------------------------------------------------------*
FORM capture_delta_changes.
* Select Data
  DATA:  l_index TYPE i.
  SELECT * FROM ztco_mha
               INTO TABLE it_time_db
          WHERE kokrs EQ p_kokrs AND
                gjahr EQ p_gjahr AND
                perid IN r_perid AND
                pernr IN s_pernr AND
                kostl IN pnpkostl AND
                chnge =  space.

  SELECT * FROM ztco_mhat
               INTO TABLE it_timet_db
          WHERE kokrs EQ p_kokrs AND
                gjahr EQ p_gjahr AND
                perid IN r_perid AND
                chnge =  space.

  SELECT * FROM ztco_mhad
               INTO TABLE it_timed_db
         WHERE kokrs EQ p_kokrs AND
                gjahr EQ p_gjahr AND
                perid IN r_perid AND
                kostl IN pnpkostl AND
                pernr IN s_pernr  AND
                chnge =  space.

* Check for Delta changes - ztco_mha (only Man hours)
  LOOP AT it_time.
    l_index = sy-tabix.
    CLEAR it_time_db.
    READ TABLE it_time_db WITH KEY kokrs =    it_time-kokrs
                                   gjahr =    it_time-gjahr
                                   perid =    it_time-perid
                                   pernr =    it_time-pernr
*                                  datum =    it_time-datum
                                   kostl =    it_time-kostl
                                   lgart =    it_time-lgart
                                   dtype =    it_time-dtype
                                   anzsh =    it_time-anzsh
                                   empct =    it_time-empct
                                   jobcd =    it_time-jobcd
                                   mosid =    it_time-mosid
                                   schkz =    it_time-schkz.
*                                  zeity =    it_time-zeity.

    IF sy-subrc <> 0.
      it_time-chnge = '1'.
      APPEND it_time TO it_time_dt.
    ELSEIF it_time-anzhl <> it_time_db-anzhl.
      it_time_dt = it_time.
      it_time_dt-chnge = '1'.
      it_time_dt-anzhl   =  it_time-anzhl   - it_time_db-anzhl.
      it_time_dt-netmh   =  it_time-netmh   - it_time_db-netmh.
      it_time_dt-emp_fte =  it_time-emp_fte - it_time_db-emp_fte.
      CLEAR it_time_dt-emp_cnt.
      it_time_dt-emp_avg =  it_time-emp_avg - it_time_db-emp_avg.
*     it_time_dt-emp_day =  it_time-emp_day - it_time_db-emp_day.
      it_time_dt-dayhc   =  it_time-dayhc   - it_time_db-dayhc  .
      it_time_dt-wdays   =  it_time-wdays   - it_time_db-wdays.
      it_time_dt-solst   =  it_time-solst.
      APPEND it_time_dt.
    ENDIF.
  ENDLOOP.

* Check for Delta changes - ZTCO_MHAT (CC summary)
  LOOP AT it_timet.
    l_index = sy-tabix.
    CLEAR it_timet_db.
    READ TABLE it_timet_db WITH KEY kokrs =    it_timet-kokrs
                                    gjahr =    it_timet-gjahr
                                    perid =    it_timet-perid
                                    kostl =    it_timet-kostl
                                    lgart =    it_timet-lgart
                                    anzsh =    it_timet-anzsh
                                    empct =    it_timet-empct
                                    jobcd =    it_timet-jobcd.

    IF sy-subrc <> 0.
      it_timet-chnge = '1'.
      APPEND it_timet TO it_timet_dt.
    ELSEIF it_timet-anzhl <> it_timet_db-anzhl.
      it_timet_dt = it_timet.
      it_timet_dt-chnge = '1'.
      it_timet_dt-anzhl =  it_timet_dt-anzhl - it_timet_db-anzhl.
      it_timet_dt-netmh =  it_timet_dt-netmh - it_timet_db-netmh.
      APPEND it_timet_dt.
    ENDIF.
  ENDLOOP.

* Check for Delta changes - ZTCO_MHAD
  LOOP AT it_timed.
    l_index = sy-tabix.
    CLEAR it_timed_db.
    READ TABLE it_timed_db WITH KEY kokrs =    it_timed-kokrs
                                    gjahr =    it_timed-gjahr
                                    perid =    it_timed-perid
                                    kostl =    it_timed-kostl
                                    dtype =    it_timed-dtype
                                    lgart =    it_timed-lgart
                                    lgart2 =   it_timed-lgart2
                                    anzsh  =   it_timed-anzsh
                                    skostl =   it_timed-skostl
                                    empct  =   it_timed-empct
                                    jobcd  =   it_timed-jobcd
                                    pernr =    it_timed-pernr
*                                   mosid =    it_timed-mosid
                                    schkz =    it_timed-schkz.
*                                   zeity =    it_timed-zeity.
    IF sy-subrc <> 0.
      it_timed-chnge = '1'.
      APPEND it_timed TO it_timed_dt.
    ELSEIF it_timed-anzhl <> it_timed_db-anzhl.
      it_timed_dt = it_timed.
      it_timed-chnge = '1'.
      it_timed-anzhl =  it_timed-anzhl - it_timed_db-anzhl.
      APPEND it_timed_dt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " capture_delta_changes
*&---------------------------------------------------------------------*
*&      Form  add_to_time_sum_detail
*&---------------------------------------------------------------------*
FORM add_to_time_sum_detail  USING f_datum LIKE sy-datum
                                   f_lgart .

*////////////////////////////////////////
* marked by ig.moon
*////////////////////////////////////////


  MOVE-CORRESPONDING it_time  TO it_timet.
* + by ig.moon {
  it_timet-skostl = it_time-srkostl.
* }

  MOVE-CORRESPONDING it_time  TO it_timed.

* + by ig.moon {
  it_timed-skostl = it_time-srkostl.
* }

  it_timed-lgart2 = f_lgart.    "py_times-lgart2.

  CASE f_datum+6(2).
    WHEN '01'. it_timed-anz01 = it_timed-anzhl.
    WHEN '02'. it_timed-anz02 = it_timed-anzhl.
    WHEN '03'. it_timed-anz03 = it_timed-anzhl.
    WHEN '04'. it_timed-anz04 = it_timed-anzhl.
    WHEN '05'. it_timed-anz05 = it_timed-anzhl.
    WHEN '06'. it_timed-anz06 = it_timed-anzhl.
    WHEN '07'. it_timed-anz07 = it_timed-anzhl.
    WHEN '08'. it_timed-anz08 = it_timed-anzhl.
    WHEN '09'. it_timed-anz09 = it_timed-anzhl.
    WHEN '10'. it_timed-anz10 = it_timed-anzhl.
    WHEN '11'. it_timed-anz11 = it_timed-anzhl.
    WHEN '12'. it_timed-anz12 = it_timed-anzhl.
    WHEN '13'. it_timed-anz13 = it_timed-anzhl.
    WHEN '14'. it_timed-anz14 = it_timed-anzhl.
    WHEN '15'. it_timed-anz15 = it_timed-anzhl.
    WHEN '16'. it_timed-anz16 = it_timed-anzhl.
    WHEN '17'. it_timed-anz17 = it_timed-anzhl.
    WHEN '18'. it_timed-anz18 = it_timed-anzhl.
    WHEN '19'. it_timed-anz19 = it_timed-anzhl.
    WHEN '20'. it_timed-anz20 = it_timed-anzhl.
    WHEN '21'. it_timed-anz21 = it_timed-anzhl.
    WHEN '22'. it_timed-anz22 = it_timed-anzhl.
    WHEN '23'. it_timed-anz23 = it_timed-anzhl.
    WHEN '24'. it_timed-anz24 = it_timed-anzhl.
    WHEN '25'. it_timed-anz25 = it_timed-anzhl.
    WHEN '26'. it_timed-anz26 = it_timed-anzhl.
    WHEN '27'. it_timed-anz27 = it_timed-anzhl.
    WHEN '28'. it_timed-anz28 = it_timed-anzhl.
    WHEN '29'. it_timed-anz29 = it_timed-anzhl.
    WHEN '30'. it_timed-anz30 = it_timed-anzhl.
    WHEN '31'. it_timed-anz31 = it_timed-anzhl.
  ENDCASE.

  COLLECT: it_timet, it_timed.

  IF it_time-lgart = 'B'.
    COLLECT it_timed INTO it_timed_b.
  ENDIF.

  CLEAR  : it_timet, it_timed.

ENDFORM.                    " add_to_time_sum_detail
*&---------------------------------------------------------------------*
*&      Form  collect_absence_time
*&---------------------------------------------------------------------*
FORM collect_absence_time.
* Collect to IT_TIME... tables.
* FIXME LATER... what if CC changed...
  DATA: w_time_info LIKE it_time.
  LOOP AT it_catsdb_item.
    READ TABLE it_time INTO w_time_info
          WITH KEY pernr = it_catsdb_item-pernr
                   lgart = '1'.               "from regular working
    IF sy-subrc <> 0.
*---- error... FIXME
      CONCATENATE 'ABS HR:' it_catsdb_item3-pernr
                  ' - no time recording' INTO gv_msg.
      PERFORM show_warning.
    ELSE.
      it_time-lgart = 'B'.
      it_time-anzhl = it_catsdb_item-catshours.
      CLEAR: it_time-emp_fte, it_time-emp_cnt, it_time-emp_avg,
             it_time-dayhc,   "it_time-emp_day,
             it_time-wdays,   it_time-solst.
      COLLECT it_time.

      PERFORM add_to_time_sum_detail USING it_catsdb_item-workdate
                                           it_catsdb_item-awart.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " collect_absence_time
*&---------------------------------------------------------------------*
*&      Form  collect_supporting_time
*&---------------------------------------------------------------------*
FORM collect_supporting_time.


  DATA : BEGIN OF it_l_calwsig OCCURS 0,
            kostl     LIKE catsdb-skostl  , "CCtr
            catshours LIKE catsdb-catshours.
  DATA : END OF   it_l_calwsig.

  CLEAR : it_l_calwsig, it_l_calwsig[].
  SORT py_times BY pernr datum.

  LOOP AT it_catsdb_item3.
* Sender

    READ TABLE it_time WITH KEY pernr = it_catsdb_item3-pernr
                                lgart = '1'.
    IF sy-subrc <> 0.
*FIXME...
      CONCATENATE 'SUP HR:' it_catsdb_item3-pernr
                  ' - no time recording' INTO gv_msg.
      PERFORM show_warning.
    ELSE.
      CLEAR py_times.                                       "UD1K952620
      READ TABLE py_times WITH KEY pernr = it_catsdb_item3-pernr
                                   datum = it_catsdb_item3-workdate.
      IF sy-subrc <> 0.
        CONCATENATE 'SUP HR:' it_catsdb_item3-pernr ' - no time on '
                    it_catsdb_item3-workdate INTO gv_msg.
        PERFORM show_warning.
      ENDIF.
      it_time-kostl   = py_times-kostl.
      it_time-srkostl = it_catsdb_item3-rkostl.

      it_time-lgart = '3'.
      it_time-anzhl = it_catsdb_item3-catshours.

      IF it_time-kostl(2) = 'MX'.
        PERFORM calc_net_mh USING    it_time-anzhl
                            CHANGING it_time-netmh.
      ELSE.
        it_time-netmh = it_time-anzhl.
      ENDIF.

      it_time-anzhl = - it_time-anzhl.
      it_time-netmh = - it_time-netmh.


      CLEAR: it_time-emp_fte, it_time-emp_cnt, it_time-emp_avg,
             it_time-dayhc,   "it_time-emp_day,
             it_time-wdays,   it_time-solst.
      COLLECT it_time.

      PERFORM add_to_time_sum_detail USING it_catsdb_item3-workdate
                                           it_catsdb_item3-awart.


* Receiver
      it_time-kostl = it_catsdb_item3-rkostl.
      it_time-srkostl = py_times-kostl.
      it_time-anzhl = - it_time-anzhl.
      it_time-netmh = - it_time-netmh.
      COLLECT it_time.

      PERFORM add_to_time_sum_detail USING it_catsdb_item3-workdate
                                           it_catsdb_item3-awart.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " collect_supporting_time
*&---------------------------------------------------------------------*
*&      Form  calc_net_mh
*&---------------------------------------------------------------------*
FORM calc_net_mh USING    f_anzhl
                 CHANGING f_netmh.

  IF f_anzhl <= 4.
    f_netmh = f_anzhl - 15 / 60.  "rest time
  ELSEIF f_anzhl > 4 AND f_anzhl <= 8.
    f_netmh = f_anzhl - 25 / 60.
  ELSEIF f_anzhl > 8.
    f_netmh = f_anzhl - 25 / 60
            - ( f_anzhl - 8 ) * 5 / 60.
  ENDIF.

  IF f_netmh < 0.
    f_netmh = 0.
  ENDIF.

ENDFORM.                    " calc_net_mh
*&---------------------------------------------------------------------*
*&      Form  collect_inactive_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_inactive_time.

*  Select HR Master Record: Inactive data
  PERFORM select_pa000.

  PERFORM make_it_time.

ENDFORM.                    " collect_inactive_time
*&---------------------------------------------------------------------*
*&      Form  select_pa000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_pa000.

  DATA : l_date TYPE datum,
         l_total_date TYPE datum,
         l_work(1),
         l_times TYPE i,
         l_total TYPE i.

*  Get inactive employee
  SELECT pernr endda begda massn massg stat2
    INTO CORRESPONDING FIELDS OF TABLE it_pa0000
    FROM pa0000
    WHERE ( ( begda BETWEEN g_datefr AND g_dateto ) OR
            ( endda BETWEEN g_datefr AND g_dateto ) OR
            ( begda < g_datefr AND endda > g_dateto ) )
      AND stat2 = '1'
      AND pernr IN s_pernr.

* adjust start, end period
  LOOP AT it_pa0000.
    IF it_pa0000-begda < g_datefr.
      it_pa0000-begda = g_datefr.
    ENDIF.
    IF it_pa0000-endda > g_dateto.
      it_pa0000-endda = g_dateto.
    ENDIF.
    MODIFY it_pa0000.
  ENDLOOP.

  l_total       = g_dateto+6(2).
  l_total_date  = g_datefr.
  CLEAR g_total_anzhl.
*  Calcuate how many days in the monrh are work
  DO l_total TIMES.
    CLEAR l_work.
    PERFORM get_working_date_or_not USING    l_total_date
                                    CHANGING l_work.
* if the date is working day => Add 1 day  * 8HR
    IF l_work = 'X'.
      g_total_anzhl  = g_total_anzhl +  8 .
    ENDIF.
    l_total_date = l_total_date + 1.
  ENDDO.

*  Calcuate how many days(inactive) are work
  SORT i_emp BY pernr.
  LOOP AT it_pa0000.
    READ TABLE i_emp WITH KEY pernr = it_pa0000-pernr BINARY SEARCH.
    IF sy-subrc <> 0.
      i_emp-pernr = it_pa0000-pernr. APPEND i_emp.
    ENDIF.

    l_times = it_pa0000-endda - it_pa0000-begda + 1.
    l_date  = it_pa0000-begda .
    DO l_times TIMES.
      CLEAR l_work.
      PERFORM get_working_date_or_not USING    l_date
                                      CHANGING l_work.
* if the date is working day => Add 1 day  * 8HR
      IF l_work = 'X'.
        it_inact-pernr  = it_pa0000-pernr.
        it_inact-massn  = it_pa0000-massn.
        it_inact-massg  = it_pa0000-massg.
        it_inact-date   = l_date.
        it_inact-anzhl  = 8.
        it_inact-dayhc  = 1.

        COLLECT it_inact. CLEAR it_inact.
      ENDIF.
      l_date = l_date + 1.
    ENDDO.
    CLEAR l_date.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM i_emp.


  CHECK NOT it_inact[] IS INITIAL.
  SELECT pernr endda begda kostl stell persg persk
    INTO CORRESPONDING FIELDS OF TABLE it_pa0001
    FROM pa0001
    FOR ALL ENTRIES IN it_inact
    WHERE pernr = it_inact-pernr
      AND endda >= it_inact-date
      AND begda <= it_inact-date.

  DELETE ADJACENT DUPLICATES FROM it_pa0001.

  CHECK NOT it_pa0001[] IS INITIAL.
  SELECT jobcd stell
    INTO CORRESPONDING FIELDS OF TABLE it_jc
    FROM ztco_jc
     FOR ALL ENTRIES IN it_pa0001
    WHERE stell = it_pa0001-stell .



ENDFORM.                    " select_pa000
*&---------------------------------------------------------------------*
*&      Form  get_working_Date_or_not
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      <--P_L_WORK  text
*----------------------------------------------------------------------*
FORM get_working_date_or_not USING    p_date
                             CHANGING p_work.

  DATA : l_work LIKE scal-indicator.


  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            date                 = p_date
            factory_calendar_id  = 'HM'
       IMPORTING
            workingday_indicator = l_work.
  IF l_work = ''.
    p_work = 'X'.
  ENDIF.

ENDFORM.                    " get_working_Date_or_not
*&---------------------------------------------------------------------*
*&      Form  make_it_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_time.
  DATA : l_lgart2 LIKE ztco_mhad-lgart2.
  DATA : l_emp_avg LIKE it_time-anzhl.

  SORT it_pa0001 BY pernr begda.

  CLEAR it_time.
  LOOP AT it_inact.
*   Pernr's Cost center, job code, emp_cat...
    CLEAR it_pa0001 .
    LOOP AT it_pa0001 WHERE pernr = it_inact-pernr
                        AND endda >= it_inact-date
                        AND begda <= it_inact-date.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0 .
      it_time-kokrs = p_kokrs.
      it_time-gjahr = p_gjahr.
      it_time-perid = p_frper.
      it_time-anzhl = it_inact-anzhl.
      it_time-dayhc = it_inact-dayhc.
      it_time-lgart = 'V'.
      it_time-pernr = it_inact-pernr.
      it_time-kostl = it_pa0001-kostl.
      CLEAR it_jc.
      READ TABLE it_jc WITH KEY stell = it_pa0001-stell.
      it_time-jobcd = it_jc-jobcd.
*   Employee category
      PERFORM get_emp_categ(zacop01) USING it_pa0001-persg
                                           it_pa0001-persk
                                     CHANGING it_time-empct.
      COLLECT it_time.

      CLEAR l_lgart2.
      CONCATENATE it_inact-massn it_inact-massg INTO l_lgart2.
*   Detail data(ZTCO_MHAD)
      PERFORM add_to_time_sum_detail USING it_inact-date
                                           l_lgart2.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " make_it_time
*&---------------------------------------------------------------------*
*&      Form  read_pa0000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pa0000.
*  Get inactive employee
  SELECT pernr endda begda massn massg stat2
    INTO CORRESPONDING FIELDS OF TABLE it_pa0000
    FROM pa0000
    WHERE ( ( begda BETWEEN g_datefr AND g_dateto ) OR
            ( endda BETWEEN g_datefr AND g_dateto ) )
      AND stat2 = '1'
      AND pernr IN s_pernr.

ENDFORM.                    " read_pa0000
*&---------------------------------------------------------------------*
*&      Form  show_warning
*&---------------------------------------------------------------------*
FORM show_warning.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
    EXPORTING
*      percentage          = p_%
      text                = gv_msg
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
      .
ENDFORM.                    " show_warning










* back up by ig.moon *

**    loop at it_timet.
**      select single * from ztco_mhat
**      where kokrs eq it_timet-kokrs
**        and gjahr eq it_timet-gjahr
**        and perid eq it_timet-perid
**        and dtype eq it_timet-dtype
**        and kostl eq it_timet-kostl
**        and anzsh eq it_timet-anzsh
**        and lgart eq it_timet-lgart
**        and jobcd eq it_timet-jobcd
**        and empct eq it_timet-empct
**        and chnge eq it_timet-chnge.
**      if sy-subrc eq 0.
**
*** { ????????????????????????????????????
**        add : it_timet-anzhl to ztco_mhat-anzhl,
**              it_timet-netmh to ztco_mhat-netmh,
**              it_timet-daymh to ztco_mhat-daymh,
**              it_timet-satmh to ztco_mhat-satmh,
**              it_timet-sunmh to ztco_mhat-sunmh,
**              it_timet-holmh to ztco_mhat-holmh.
**
**        update ztco_mhat.
**
***        ztco_mhat-SKOSTL
**
*** }
**
***        break-point.
**
**      else.
**
**        ztco_mhat = it_timet.
**        insert ztco_mhat.
**
**      endif.
**    endloop.

* BEGIN OF UD1K952755
*&---------------------------------------------------------------------*
*&      Form  db_get
*&---------------------------------------------------------------------*
*       Get original data
*----------------------------------------------------------------------*
FORM db_get.
  SELECT KOKRS
         GJAHR
         PERID
         PERNR
         KOSTL
         LGART
         DTYPE
         SCHKZ
         MOSID
         JOBCD
         SRKOSTL
         CHNGE
         ANZSH
         EMPCT
         ANZHL
         NETMH
         EMP_CNT
         EMP_AVG
         EMP_FTE
         DAYHC
         WDAYS
         SOLST
       FROM ztco_mha INTO CORRESPONDING FIELDS OF TABLE it_time_tmp
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND pernr IN s_pernr.

  SELECT * FROM ztco_mhat INTO TABLE it_timet_tmp
       WHERE gjahr = p_gjahr
         AND perid IN r_perid.

  SELECT * FROM ztco_mhad INTO TABLE it_timed_tmp
       WHERE gjahr = p_gjahr
         AND perid IN r_perid
         AND pernr IN s_pernr.
ENDFORM.                    " db_get
* END OF UD1K952755
