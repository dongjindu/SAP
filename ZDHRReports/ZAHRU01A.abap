**----------------------------------------------------------------------
* Program ID        : ZAHRU01A
* Title             : Program usage Analysis
* Created on        : 9/21/2009
* Created by        : IG.MOON
*----------------------------------------------------------------------
REPORT zahru01a MESSAGE-ID zmco.

TABLES : sapwlserv, usr02, trdirt,sapwlacctp, tstct, zthrappusge,
zthrusge_rfc_svr,zthrusge_rfc_dst.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

TYPES :
 BEGIN OF ty_statdir,
  summary                       TYPE swnc_t_aggtasktype,
  hitlist_dbcalls               TYPE swnc_t_agghitlist,
  hitlist_respti                TYPE swnc_t_agghitlist,
  accountstat_tasktype          TYPE swnc_t_agguserworkload,
  time_stat                     TYPE swnc_t_aggtimes,
  table_record_stat             TYPE swnc_t_aggtablerec,
  application_stat              TYPE swnc_t_aggusertcode,
  terminal_io_stat              TYPE swnc_t_aggfrontend,
  tasktype_stat                 TYPE swnc_t_aggtasktimes,
  memory_stat                   TYPE swnc_t_aggmemory,
  rfc_client_stat               TYPE swnc_t_aggrfccli,
  rfc_server_stat               TYPE swnc_t_aggrfcsrv,
  rfc_client_dest_stat          TYPE swnc_t_aggrfcclid,
  rfc_server_dest_stat          TYPE swnc_t_aggrfcsrvd,
  spool_activity_stat           TYPE swnc_t_aggspoolact,
  spool_print_stat              TYPE swnc_t_aggspool,
  dbprocedure_stat              TYPE swnc_t_aggdbprocs,
  detailtcode_stat              TYPE swnc_t_aggtcdet,
  externsys                     TYPE swnc_t_aggextsystem,
  dbcon                         TYPE swnc_t_aggdbc,
  vmc_stat                      TYPE swnc_t_aggvmc,
  web_client                    TYPE swnc_t_aggwebclnt,
  web_client_dest               TYPE swnc_t_aggwebdest,
  web_server                    TYPE swnc_t_aggwebclnt,
  web_server_dest               TYPE swnc_t_aggwebdest,
  comp_hierarchy                TYPE swnc_t_aggcomphier,
  org_unit                      TYPE swnc_t_aggorgunit,
* Application Statistic
  as_statistic                  TYPE swnc_t_aggastat,
  as_hitl_dbcalls               TYPE swnc_t_aggashitlist,
  as_hitl_respti                TYPE swnc_t_aggashitlist,

 END OF ty_statdir.

DATA  ex_data     TYPE ty_statdir OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
    exporting
      titel = &1
      txt1  = &2
      txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl WITH FRAME TITLE text-001.
SELECT-OPTIONS s_date FOR sapwlacctp-startdate OBLIGATORY DEFAULT
sy-datum NO-EXTENSION.
PARAMETERS p_month AS CHECKBOX.
PARAMETERS p_total AS CHECKBOX DEFAULT 'X'.
PARAMETERS p_host LIKE sapwlserv-name.
SELECT-OPTIONS s_bname  FOR usr02-bname.
SELECTION-SCREEN END OF BLOCK bl.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME.
PARAMETER  p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b5.

PARAMETERS p_upd AS CHECKBOX.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*

TYPES: BEGIN OF ty_row_tab,
         ldate      TYPE datum,
         month(6)   TYPE n,
         $count     TYPE i,
         name_last  TYPE ad_namelas,
         name_first TYPE ad_namefir,
         kostl      TYPE kostl,
         orgeh      TYPE orgeh,
         orgtx      TYPE orgtx,
         ztext(70).
        INCLUDE STRUCTURE sapwltcod0.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out,

         ldate      TYPE datum,
         month(6)   TYPE n,
         kostl      TYPE kostl,
         orgeh      TYPE orgeh,
         orgtx      TYPE orgtx,
         instance   TYPE sapwlinstn,
         account    TYPE swluser,
         posid      TYPE ufps_posid,
         tcode      TYPE swltcode,
         btcjob     TYPE swlentryid,
         $count     TYPE i,
         name_last  TYPE ad_namelas,
         name_first TYPE ad_namefir,

         diastepcnt TYPE swldiastep,
         respst     TYPE swlsrespti,
         respmt     TYPE swlmrespti,
         cpust      TYPE swlcpusti,
         cpumt      TYPE swlcpumti,
         dbst       TYPE swldbst,
         dbmt       TYPE swldbtime,
         dbprmt     TYPE swldbprmt1,
         waitst     TYPE swlwaitst,
         waitmt     TYPE swlwaitmt,
         dbpcount   TYPE p,
         dbptime    TYPE p,
         dbpavti    TYPE p,
         mcputi     TYPE p,
         mdbti      TYPE p,
         mdbprti    TYPE p,
         mwaitti    TYPE p,
         mfguiti    TYPE p,
         fnetst     TYPE p,
         fguist     TYPE p,
         fguicnt    TYPE p,
         fnetmt     TYPE p,
         fguimt     TYPE p,
         fguitrpmt  TYPE p,
         bytes      TYPE p,

         ztext(70).
        INCLUDE STRUCTURE sapwldbinc.
*        INCLUDE STRUCTURE sapwltcod0.
TYPES: END OF ty_out.

DATA: BEGIN OF host_list OCCURS 0,
        name TYPE stuninst,
        host TYPE stunhost,
      END OF host_list.

DATA: BEGIN OF it_orgtx OCCURS 0,
        orgeh      TYPE orgeh,
        orgtx      TYPE orgtx,
      END OF it_orgtx .

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

* by ig.moon 4/28/11 {
DATA it_rfc_svr LIKE zthrusge_rfc_svr OCCURS 0 WITH HEADER LINE.
DATA it_rfc_dst LIKE zthrusge_rfc_dst OCCURS 0 WITH HEADER LINE.
* }

DATA $it_row_tab LIKE it_row_tab OCCURS 0 WITH HEADER LINE.
DATA $mdbti TYPE p.
DATA: g_error(1),
      g_repid  LIKE sy-repid.

DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: BEGIN OF it_user OCCURS 0,
        bname      LIKE usr21-bname,
        persnumber LIKE adcp-persnumber,
        addrnumber LIKE adcp-addrnumber,
        name_last  TYPE ad_namelas,
        name_first TYPE ad_namefir,
        kostl      TYPE kostl,
        orgeh      TYPE orgeh,
      END OF it_user .

DATA: BEGIN OF it_bname OCCURS 0,
        bname      LIKE usr21-bname,
        pernr      LIKE pa0001-pernr,
      END OF it_bname .
DATA: BEGIN OF it_pernr OCCURS 0,
        pernr      LIKE pa0001-pernr,
        kostl      TYPE kostl,
        orgeh      TYPE orgeh,
      END OF it_pernr.

DATA  i_zthrappusge LIKE zthrappusge OCCURS 0 WITH HEADER LINE.

DATA $flag.
DATA $ix TYPE i.

*----------------------------------------------------------------------*
DATA:  BEGIN OF stat_moni_1,
       respti   TYPE p,
       cputi   TYPE p,
       queueti   TYPE p,

       readdircnt TYPE p,
       readdirti   TYPE p,
       readdirbuf TYPE p,

       readseqcnt TYPE p,
       readseqti   TYPE p,
       phyreadcnt TYPE p,

       chngcnt TYPE p,
       chngti TYPE p,
       phychngrec TYPE p,

       guitime TYPE p,                 "Frontend: GUI time
       guinettime TYPE p,              "          Net time
       guicnt TYPE p,                  "          Roundtrips

END OF stat_moni_1,
*
*------Kernstruktur transferierte Bytes--------------------------------
*
BEGIN OF stat_moni_2,
       dsqlcnt TYPE p,
       abapscnt TYPE p,
       abaplcnt TYPE p,
       dynpscnt TYPE p,
       dynplcnt TYPE p,
       cualcnt TYPE p,
       ntabcnt TYPE p,
       quecnt TYPE p,
       ddiccnt TYPE p,
       cpiccnt TYPE p,
END OF stat_moni_2,
*
*------Kernstruktur Tabellenstatistik----------------------------------
*
BEGIN OF stat_moni_4,
       modcnt  LIKE sta1-modcnt,       "Number of changed rows
       dircnt  LIKE sta1-dircnt,       "Number of rows read directly
       seqcnt  LIKE sta1-seqcnt,       "Number of rows read sequential
       tabtime LIKE sta1-tabtime,      "Time for accesses
END OF stat_moni_4.

TYPES: short_host LIKE sapwlserv-hostshort,
       short_inst LIKE sapwlserv-instshort,
       long_host  LIKE sapwlserv-host,
       long_inst  LIKE sapwlserv-name.

DATA:  BEGIN OF summa OCCURS 10,
        mandt LIKE sy-mandt,
        username(12) TYPE c,
        account(12) TYPE c,
        count LIKE sy-index,
        dcount LIKE sy-index,
        ucount LIKE sy-index,
        bcount LIKE sy-index,
        scount LIKE sy-index,
        respti   TYPE p,
        cputi   TYPE p,
        queueti   TYPE p,
* --- extensions for accounting exit --------------------------------- *
        value_0 TYPE p,  "new meaning: usr DB access time (milliseconds)
        value_1 TYPE p,  "new meaning: sys load & gen time(milliseconds)
        value_2 TYPE p,  "new meaning: accessed user data (bytes)
        value_3 TYPE p,  "new meaning: accessed sys  data (bytes)
* -------------------------------------------------------------------- *
        value_4 TYPE p,
        value_5 TYPE p,
       END OF summa.

DATA   summary LIKE sapwlsumry OCCURS 0 WITH HEADER LINE.
DATA   rsystem  TYPE short_inst VALUE '00000000'.

DATA: BEGIN OF spool_print_statistic.
        INCLUDE STRUCTURE sapwlspopr.
DATA: END OF spool_print_statistic.

DATA  tt_hitl_respti LIKE sapwlhitl OCCURS 160 WITH HEADER LINE.
"top scorer of response time

DATA  tt_hitl_dbcalls LIKE sapwlhitl OCCURS 160 WITH HEADER LINE.
"top scorer of DB calls

DATA  tt_summa LIKE sapwluswlx OCCURS 0 WITH HEADER LINE.

DATA  BEGIN OF tt_summa_backup OCCURS 0. "contains old SUMMA structure
        INCLUDE STRUCTURE summa.       "if selected task type is <>
DATA  END   OF tt_summa_backup.        "'*' (total)

DATA BEGIN OF tt_more_summa OCCURS 0.  "same as TT_SUMMA, used only as
        INCLUDE STRUCTURE tt_summa.    "return table for GET-SUM...
DATA END   OF tt_more_summa.           "...OF-LONG-TERMS form

*
*------SUMMU:  Summensaetze pro CPU-Id, Tag bzgl. User-, Tcodprofil----
*
DATA: BEGIN OF summu OCCURS 100.
*       ACCOUNT(12) TYPE C,
*       TCODE(8) TYPE C,
*       COUNT LIKE SY-INDEX,
*       DCOUNT LIKE SY-INDEX,
*       UCOUNT LIKE SY-INDEX,
*       BCOUNT LIKE SY-INDEX,
*       ECOUNT LIKE SY-INDEX,
*       SCOUNT LIKE SY-INDEX.
*        INCLUDE STRUCTURE STAT_MONI_1.
*        INCLUDE STRUCTURE STAT_MONI_2.
        INCLUDE STRUCTURE sapwlustc.
DATA    entry_id_type.
DATA   END OF summu.
*

*
* --- SUMMU similar structure for tasktype split --------------------- *
*
DATA  tt_summu LIKE sapwlustcx OCCURS 250 WITH HEADER LINE.

DATA  summcua LIKE sapwlfcode OCCURS 0 WITH HEADER LINE.
*
DATA  BEGIN OF xummcua OCCURS 0.
        INCLUDE STRUCTURE summcua.
DATA  END OF xummcua.
*
DATA  readcuaflag.

DATA  BEGIN OF tt_summu_backup OCCURS 0. "contains old SUMMU structure
        INCLUDE STRUCTURE summu.       "if selected task type is <>
DATA  END   OF tt_summu_backup.        "'*' (total)

DATA BEGIN OF tt_more_summu OCCURS 0.  "same as TT_SUMMU, used only as
        INCLUDE STRUCTURE tt_summu.    "return table for GET-SUM...
DATA END   OF tt_more_summu.           "...OF-LONG-TERMS form

DATA  tt_users_of_entry_id LIKE sapwluenti OCCURS 0 WITH HEADER LINE.

* RFC statistic

DATA  rfc_client_statistic        LIKE sapwlrfcc  OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_server_statistic        LIKE sapwlrfcs  OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_client_dest_statistic   LIKE sapwlrfccd OCCURS 0
                                                  WITH HEADER LINE.
DATA  rfc_server_dest_statistic   LIKE sapwlrfcsd OCCURS 0
                                                  WITH HEADER LINE.

* Spool statistic

DATA: spool_activity_statistic LIKE sapwlspoac OCCURS 0
                                                  WITH HEADER LINE.
* Memory usage statistic

DATA  memory_statistic            LIKE sapwlmem   OCCURS 0
                                                  WITH HEADER LINE.

* Collected instances

DATA  instance_table              LIKE sapwlinst  OCCURS 0
                                                  WITH HEADER LINE.

DATA tab_app  TYPE STANDARD TABLE OF swncaggusertcode.
DATA wa_app      TYPE swncaggusertcode.

* by ig.moon 6/2/11 {
DATA tab_rfcsrvr  TYPE STANDARD TABLE OF swncaggrfcsrvr.
DATA wa_rfcsrvr      TYPE swncaggrfcsrvr.
DATA tab_rfcsrvrdest  TYPE STANDARD TABLE OF swncaggrfcsrvrdest.
DATA wa_rfcsrvrdest      TYPE swncaggrfcsrvrdest.
* }


DATA BEGIN OF tt_more_users_of_entry_id OCCURS 0.
        INCLUDE STRUCTURE tt_users_of_entry_id.
DATA END   OF tt_more_users_of_entry_id.

DATA BEGIN OF tt_users_of_entry_id_agg OCCURS 0. "list of users as desc.
        INCLUDE STRUCTURE tt_users_of_entry_id."above, but for aggregation
DATA END   OF tt_users_of_entry_id_agg.

DATA: BEGIN OF tt_user_list OCCURS 0,  "list of users. Used for display
        account LIKE tt_summu-account, "of users of one tcode/entry_id
      END   OF tt_user_list.
*
*------SUMMT:  Summensaetze pro CPU-Id, Tag bzgl. Zeitprofil-----------
*
DATA:  BEGIN OF summt OCCURS 200,
       time     LIKE sapwltimes-time,
       tcode    LIKE sta1-tcode,
       entry_id LIKE sapwltimes-entry_id,
       count LIKE sy-index.
        INCLUDE STRUCTURE stat_moni_1.
DATA   END OF summt.

*
* --- SUMMT similar structure for tasktype split --------------------- *
*
DATA  tt_summt LIKE sapwltimes OCCURS 0 WITH HEADER LINE.

DATA BEGIN OF tt_more_summt OCCURS 0.  "same as TT_SUMMT, used only as
        INCLUDE STRUCTURE tt_summt.    "return table for GET-SUM...
DATA END   OF tt_more_summt.           "...OF-LONG-TERMS form
* -------------------------------------------------------------------- *

*
*
*------SUMMR:  Summensaetze pro Tag/Tasktyp/Zeitintervall---------------
*
DATA  summr LIKE sapwltskti OCCURS 0 WITH HEADER LINE.
*
*------SUMML:  Tabellensummensaetze pro Transaktion und Tabelle---------
*
DATA:  BEGIN OF summl OCCURS 0,
       tcode       LIKE sapwlustc-tcode, "Transaktion/Report
       tasktype(1) TYPE c,             "Tasktyp
       tname       LIKE pftab-tabname. "Tabellenname
        INCLUDE STRUCTURE stat_moni_4.
DATA   END OF summl.

*
* --- SUMML similar structure for tasktype extension ----------------- *
*

DATA  tt_summl LIKE sapwltabtr OCCURS 0 WITH HEADER LINE.

DATA  BEGIN OF tt_more_summl OCCURS 0.
        INCLUDE STRUCTURE tt_summl.
DATA  END   OF tt_more_summl.

DATA  BEGIN OF tt_summl_backup OCCURS 0. "contains old SUMML structure
        INCLUDE STRUCTURE summl.       "if selected task type is <>
DATA  END   OF tt_summl_backup.        "'*' (total)
*
*------SUMMY:  Byteverkehr zwischen Praesentation und Applikation-------
*              Korr. B11k008175 QQI: Struktur angelegt
DATA  summy LIKE sapwltmbyt OCCURS 0 WITH HEADER LINE.

INCLUDE <icon>.                        " icon
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  sy-title = 'Program Usage Analysis'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_host.
  PERFORM host_input_help CHANGING p_host.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  DATA : lv_date LIKE sy-datum.

*-Check logic
  lv_date = sy-datum - 30.
  IF lv_date >= s_date-low.
    MESSAGE s000 WITH 'With -30 days from current,'
                      'Program can Not be executed'.
    STOP.
  ENDIF.

  PERFORM initialize.

  CHECK g_error = false.

  PERFORM get_row_data.
  PERFORM prepare_cc_org.

  IF p_upd EQ true.
    __cls i_zthrappusge.

    LOOP AT it_row_tab.
      IF it_row_tab-tcode+0(3) = `<AD`. "Victor 03.07.2014
        DELETE it_row_tab.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING it_row_tab TO i_zthrappusge.
      i_zthrappusge-aedat = sy-datum.
      i_zthrappusge-aetim = sy-uzeit.
      i_zthrappusge-aenam = sy-uname.

      READ TABLE it_user WITH KEY bname = i_zthrappusge-account
      BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_zthrappusge-kostl    = it_user-kostl.
        i_zthrappusge-orgeh    = it_user-orgeh.
      ENDIF.
      APPEND i_zthrappusge.
    ENDLOOP.

    IF p_host IS INITIAL.
      DELETE FROM zthrappusge WHERE ldate IN s_date
                                AND account IN s_bname.

      DELETE FROM zthrusge_rfc_svr WHERE ldate IN s_date
                                AND account IN s_bname.

      DELETE FROM zthrusge_rfc_dst WHERE ldate IN s_date
                                AND account IN s_bname.

    ELSE.
      DELETE FROM zthrappusge WHERE ldate IN s_date
                                AND instance EQ p_host
                                AND account IN s_bname.

      DELETE FROM zthrusge_rfc_svr WHERE ldate IN s_date
                                AND instance EQ p_host
                                AND account IN s_bname.

      DELETE FROM zthrusge_rfc_dst WHERE ldate IN s_date
                                AND instance EQ p_host
                                AND account IN s_bname.

    ENDIF.

    COMMIT WORK.

    MODIFY zthrusge_rfc_svr FROM TABLE it_rfc_svr.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

    MODIFY zthrusge_rfc_dst FROM TABLE it_rfc_dst.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

    MODIFY zthrappusge FROM TABLE i_zthrappusge.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.

    PERFORM show_progress USING 'Saving...' ''.

  ENDIF.

  IF p_total EQ true.

    __cls $it_row_tab.

    LOOP AT it_row_tab.
      $it_row_tab = it_row_tab.
      CLEAR $it_row_tab-instance.
      CLEAR : $it_row_tab-readseqmt,
              $it_row_tab-respmt,
              $it_row_tab-dbprmt,
              $it_row_tab-cpumt,
              $it_row_tab-fnetmt,
              $it_row_tab-fguimt,
              $it_row_tab-dbmt,
              $it_row_tab-waitmt,
              $it_row_tab-readdirmt,
              $it_row_tab-chngmt,
              $it_row_tab-dbreqmti,
              $it_row_tab-dbpavti,
              $it_row_tab-mdbti.
      COLLECT $it_row_tab.
    ENDLOOP.

    PERFORM recalc.

    __cls it_row_tab.
    it_row_tab[] = $it_row_tab[].

  ENDIF.

  IF p_month EQ true.
    __cls $it_row_tab.
    LOOP AT it_row_tab.
      $it_row_tab = it_row_tab.
      CLEAR $it_row_tab-ldate.
      CLEAR : $it_row_tab-readseqmt,
              $it_row_tab-respmt,
              $it_row_tab-dbprmt,
              $it_row_tab-cpumt,
              $it_row_tab-fnetmt,
              $it_row_tab-fguimt,
              $it_row_tab-dbmt,
              $it_row_tab-waitmt,
              $it_row_tab-readdirmt,
              $it_row_tab-chngmt,
              $it_row_tab-dbreqmti,
              $it_row_tab-dbpavti,
              $it_row_tab-mdbti.
      COLLECT $it_row_tab.
    ENDLOOP.

    PERFORM recalc.

    __cls it_row_tab.
    it_row_tab[] = $it_row_tab[].

  ENDIF.

  PERFORM show_progress USING 'Fill in description' '70'.

  PERFORM move_out.

  CHECK g_error EQ false.
  CHECK sy-batch EQ false.

  PERFORM set_output.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*


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
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
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
*&      Form  SORT_BUILD
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


  sort_tab : 'LDATE'     ' ' 'X' 'X' 'X',
             'MONTH'     ' ' 'X' 'X' 'X',
             'INSTANCE'   ' ' 'X' 'X' 'X',
             'ACCOUNT'   ' ' 'X' 'X' 'X',
             'NAME_LAST'   ' ' 'X' 'X' 'X',
             'NAME_FIRST'   ' ' 'X' 'X' 'X',
             'KOSTL'    ' ' 'X' 'X' 'X',
             'ORGEH'    ' ' 'X' 'X' 'X',
             'ORGTX'    ' ' 'X' 'X' 'X',
             'TCODE'   ' ' 'X' 'X' 'X'.


ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA : $g_total(20),
         $g_avail(20),
         $g_unavail(20),
         $g_active(20),
         $g_inactive(20),
         $g_etc(20).

  REFRESH gt_listheader.

  l_text = 'Program Usage Analyze'.
  PERFORM set_header_line USING :
          'P' 'H' '' l_text ''.

*  PERFORM set_header_line USING :
*          'D' 'P' 'Date' s_date-low      '',
*          'D' 'P' 'Date' s_date-high      ''.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
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

  CASE fp_ucomm.
  ENDCASE.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
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

  __cls : it_row_tab.

ENDFORM.                    " initialize

*&---------------------------------------------------------------------*
*&      Form  tmcode_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_TMCODE_LOW  text
*----------------------------------------------------------------------*
FORM host_input_help CHANGING p_host.

  DATA j LIKE sy-index.
  CLEAR : host_list .

  SELECT  name host
  INTO TABLE host_list
  FROM sapwlserv.

  SORT host_list.

  LOOP AT host_list.
    help_value-value = host_list-name.
    APPEND help_value.
    help_value-value = host_list-host.
    APPEND help_value.
  ENDLOOP.

  PERFORM add_fields USING: 'SAPWLSERV'  'NAME' 'X',
                            'SAPWLSERV'  'HOST' ' '.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE host_list INDEX j.
    p_host = host_list-name.
  ENDIF.

  CLEAR: dynpfields.
  REFRESH: host_list, help_field, help_vtab, help_value, dynpfields.

ENDFORM.                    " tmcode_input_help
*&---------------------------------------------------------------------*
*&      Form  add_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0864   text
*      -->P_0865   text
*      -->P_0866   text
*----------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  help_field-tabname = p_tabname.
  help_field-fieldname = p_fieldname.
  help_field-selectflag = p_flag.
  APPEND help_field.
  CLEAR help_field.
ENDFORM.                    " add_fields
*&---------------------------------------------------------------------*
*&      Form  value_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_J  text
*----------------------------------------------------------------------*
FORM value_help CHANGING p_j.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      display              = ' '
      title_in_values_list = 'Administrator Code'
    IMPORTING
      index                = p_j
    TABLES
      fields               = help_field
      select_values        = help_vtab
      valuetab             = help_value.

ENDFORM.                    " value_help
*&---------------------------------------------------------------------*
*&      Form  modify_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

ENDFORM.                    " modify_screen
*&---------------------------------------------------------------------*
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  DATA isapwlserv LIKE sapwlserv OCCURS 0 WITH HEADER LINE.
  DATA $date LIKE sy-datum.
  DATA $text(50).

  IF it_orgtx[] IS INITIAL.
    SELECT orgeh orgtx INTO TABLE it_orgtx
    FROM t527x WHERE sprsl EQ sy-langu.
    SORT it_orgtx BY orgeh.
  ENDIF.

  __cls it_row_tab.

  PERFORM show_progress     USING 'Please wait...' ' 15'.

  IF p_month EQ true.
    READ TABLE s_date INDEX 1.
    s_date-sign = 'I'.
    s_date-option = 'BT'.
    s_date-low+6(2) = '01'.
    IF NOT s_date-high IS INITIAL.
    ELSE.
      s_date-high = s_date-low.
    ENDIF.

    CALL FUNCTION 'MM_ARRANG_GET_END_OF_MONTH'
      EXPORTING
        i_datum = s_date-high
      IMPORTING
        e_datum = s_date-high.
    MODIFY s_date INDEX 1.
  ENDIF.

  IF p_host NE space.
    SELECT SINGLE * FROM sapwlserv WHERE
    name EQ p_host.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'No Host Found!'.
      g_error = true.
      EXIT.
    ENDIF.

    $date = s_date-low - 1.
    DO 1000 TIMES.
      ADD 1 TO $date.
      CHECK $date IN s_date.
      CONCATENATE 'Now processing...' sapwlserv-instshort ':'
      $date INTO $text.
      PERFORM show_progress USING $text ' 0'.
*      PERFORM get_data USING sapwlserv-instshort $date.
      PERFORM get_data_new USING sapwlserv-name $date.
    ENDDO.

  ELSE.

    SELECT * INTO TABLE isapwlserv FROM sapwlserv.
    LOOP AT isapwlserv.
      sapwlserv = isapwlserv.
      CHECK NOT sapwlserv-name IS INITIAL.

      $date = s_date-low - 1.
      DO 1000 TIMES.
        ADD 1 TO $date.
        CHECK $date IN s_date.

        CONCATENATE 'Now processing...' sapwlserv-instshort ':'
          $date INTO $text.

        PERFORM show_progress USING $text ' 0'.

*        PERFORM get_data USING sapwlserv-instshort $date.
        PERFORM get_data_new USING sapwlserv-name $date.

      ENDDO.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " get_row_data
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SAPWLSERV_INSTSHORT  text
*----------------------------------------------------------------------*
FORM get_data USING p_instshort p_date.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
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
    'X'  'LDATE'       'Date'                 8  'DATS' '' '',
    'X'  'MONTH'       'Month'                6  'NUMC' '' '',
    'X'  'INSTANCE'    'Instance'            40  'CHAR' '' '',
    'X'  'ACCOUNT'     'User'                10  'CHAR' '' '',
    'X'  'NAME_LAST'   'Last Name'           30  'CHAR' '' '',
    'X'  'NAME_FIRST'  'First Name'          30  'CHAR' '' '',
    'X'  'KOSTL'       'C.C'                 10  'CHAR' '' '',
    'X'  'ORGEH'       'Org.'                10  'CHAR' '' '',
    'X'  'ORGTX'       'Org.Desc'            30  'CHAR' '' '',
    ' '  'TCODE'       'Transaction'         40  'CHAR' '' '',
    ' '  'DIASTEPCNT'  'Number of steps'     20  'DEC' '' '',
    ' '  'RESPST'      'T Response Time'     20  'DEC' '' '',
    ' '  'RESPMT'      'Ø Response~'         20  'DEC' '' '',
    ' '  'CPUST'       'S CPU'               20  'DEC' '' '',
    ' '  'CPUMT'       'Ø CPU'               20  'DEC' '' '',
    ' '  'DBST'        'S DB'                20  'DEC' '' '',
    ' '  'DBMT'        'Ø DB'                20  'DEC' '' '',
    ' '  'DBPTIME'     'S DB Proc. Time'     20  'DEC' '' '',
    ' '  'DBPRMT'      'Ø DB Procecure'      20  'DEC' '' '',
    ' '  'WAITST'      'T wait Time'         20  'DEC' '' '',
    ' '  'WAITMT'      'Ø Wait Time'         20  'DEC' '' '',
    ' '  'READSEQCNT'  '# Sequential Reads'  20  'DEC' '' '',
    ' '  'READSEQST'   'T Seq.read time'     20  'DEC' '' '',
    ' '  'READSEQMT'   'Ø Seq.read time'     20  'DEC' '' '',
    ' '  'READDIRCNT'  '# Direct reads'      20  'DEC' '' '',
    ' '  'READDIRST'   'S Direct reads'      20  'DEC' '' '',
    ' '  'READDIRMT'   'Ø dir. reads'        20  'DEC' '' '',
    ' '  'CHNGCNT'     '# of logical changes' 20 'DEC' '' '',
    ' '  'CHNGST'      'S Changes'           20 'DEC' '' '',
    ' '  'CHNGMT'      'Ø Changes'           20 'DEC' '' '',
    ' '  'DBREQUCNT'   '# logical DB calls'  20 'DEC' '' '',
    ' '  'DBREQMTI'    'Ø logical DB calls'  20 'DEC' '' '',
    ' '  'PHYREADCNT'  '# Phys. DB reads'  20 'DEC' '' '',
    ' '  'PHYCHNGREC'  '# Modif. DB records' 20 'DEC' '' '',
    ' '  'DBUFREADCN'  '# buffer reads'      20 'DEC' '' '',
    ' '  'DBPCOUNT'    '# DB proc call'     20 'DEC' '' '',
    ' '  'DBPTIME'     'S DB proc time'     20 'DEC' '' '',
    ' '  'DBPAVTI'     'Ø time/DB pr.call'  20 'DEC' '' '',
    ' '  'MCPUTI'      '% CPU time'          20 'DEC' '' '',
    ' '  'MDBTI'       '% Database time'     20 'DEC' '' '',
    ' '  'MDBPRTI'     '% DB proc. zeit'     20 'DEC' '' '',
    ' '  'MWAITTI'     '% wait time'         20 'DEC' '' '',
    ' '  'MFGUITI'     '% GUI time'         20 'DEC' '' '',
    ' '  'FNETST'      'Total FE network~'  20 'DEC' '' '',
    ' '  'FGUIST'      'S GUI~'             20 'DEC' '' '',
    ' '  'FGUICNT'     '# Round Trips'       20  'DEC' '' '',
    ' '  'FNETMT'      'Ø FE-Network~'       20  'DEC' '' '',
    ' '  'FGUIMT'      'Ø GUI~/D'            20  'DEC' '' '',
    ' '  'FGUITRPMT'   'Ø GUI~/Trip'         20  'DEC' '' '', "
    ' '  'BYTES'       'KB'                  20  'DEC' '' '',
    ' '  'ZTEXT'        'Short description'   50  'CHAR' '' ''.

  PERFORM change_fieldcat USING ft_fieldcat[] .


ENDFORM.                    " fieldcat_init

*/ just in case

*  CALL FUNCTION 'SAPWL_WORKLOAD_GET_SUMMARY'
*       EXPORTING
*            periodtype         = 'D'
*            hostid             = sapwlserv-instshort
*            startdate          = p_date
*       TABLES
*            summary            = summary
*       EXCEPTIONS
*            unknown_periodtype = 1
*            no_data_found      = 2
*            OTHERS             = 3.
*&---------------------------------------------------------------------*
*&      Form  change_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.

    gs_fieldcat-ref_fieldname = gs_fieldcat-fieldname.
    gs_fieldcat-ref_tabname = 'SAPWLTCOD0'.

    IF p_month EQ true.
      IF gs_fieldcat-fieldname EQ 'LDATE'.
        gs_fieldcat-no_out = 'X'.
      ENDIF.
      IF gs_fieldcat-fieldname EQ 'MONTH'.
        gs_fieldcat-no_out = ' '.
      ENDIF.
    ELSE.
      IF gs_fieldcat-fieldname EQ 'LDATE'.
        gs_fieldcat-no_out = ' '.
      ENDIF.
      IF gs_fieldcat-fieldname EQ 'MONTH'.
        gs_fieldcat-no_out = 'X'.
      ENDIF.
    ENDIF.

    IF p_total EQ true.
      IF gs_fieldcat-fieldname EQ 'INSTANCE'.
        gs_fieldcat-no_out = 'X'.
      ENDIF.
    ELSE.
      IF gs_fieldcat-fieldname EQ 'INSTANCE'.
        gs_fieldcat-no_out = ' '.
      ENDIF.
    ENDIF.


    MODIFY pt_fieldcat FROM gs_fieldcat.
  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  recalc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalc.
  LOOP AT $it_row_tab.
    $ix = sy-tabix.
    IF $it_row_tab-readseqcnt NE 0.
      $it_row_tab-readseqmt = $it_row_tab-readseqst
      / $it_row_tab-readseqcnt.
    ENDIF.

    IF $it_row_tab-$count NE 0.
      $it_row_tab-respmt = $it_row_tab-respst / $it_row_tab-$count.
      $it_row_tab-dbprmt = $it_row_tab-dbptime / $it_row_tab-$count.
      $it_row_tab-cpumt  = $it_row_tab-cpust                "* 1000
                               / $it_row_tab-$count.
      $it_row_tab-fnetmt = $it_row_tab-fnetst               "* 1000
                                /  $it_row_tab-$count.
      $it_row_tab-fguimt = $it_row_tab-fguist               "* 1000
                               / $it_row_tab-$count.
      $it_row_tab-dbmt   = $it_row_tab-dbst / $it_row_tab-$count.
      $it_row_tab-waitmt = $it_row_tab-waitst / $it_row_tab-$count.
    ENDIF.

    IF $it_row_tab-readdircnt NE 0.
      $it_row_tab-readdirmt = $it_row_tab-readdirst /
                                $it_row_tab-readdircnt.
    ENDIF.

    IF $it_row_tab-chngcnt NE 0.
      $it_row_tab-chngmt = $it_row_tab-chngst / $it_row_tab-chngcnt.
    ENDIF.

    IF $it_row_tab-dbrequcnt <> 0.
      $it_row_tab-dbreqmti = $it_row_tab-dbst / $it_row_tab-dbrequcnt.
    ENDIF.

    IF $it_row_tab-dbpcount <> 0.
      $it_row_tab-dbpavti = $it_row_tab-dbptime
                                    / $it_row_tab-dbpcount.
    ENDIF.

    IF $it_row_tab-respst >= $it_row_tab-waitst.
      $it_row_tab-mwaitti = $it_row_tab-waitst /
                               $it_row_tab-respst * 100.
    ENDIF.

    $mdbti = $it_row_tab-readdirst
                        + $it_row_tab-readseqst + $it_row_tab-chngst.

    IF $mdbti <= 9999.
      $it_row_tab-mdbti = $mdbti.
    ELSE.
      CLEAR $mdbti.
    ENDIF.

    MODIFY $it_row_tab INDEX $ix.
  ENDLOOP.

ENDFORM.                    " recalc
*&---------------------------------------------------------------------*
*&      Form  prepare_cc_org
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_cc_org.

  __cls it_bname.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    SELECT SINGLE * FROM trdirt WHERE name EQ it_row_tab-tcode
                                  AND sprsl EQ sy-langu.
    IF sy-subrc EQ 0.
      it_row_tab-ztext = trdirt-text.
    ELSE.
      SELECT SINGLE * FROM tstct WHERE tcode EQ it_row_tab-tcode
                                   AND sprsl EQ sy-langu.
      IF sy-subrc EQ 0.
        it_row_tab-ztext = tstct-ttext.
      ENDIF.
    ENDIF.

    it_bname-bname = it_row_tab-account.
    COLLECT it_bname.

    MODIFY it_row_tab INDEX $ix.

  ENDLOOP.

  LOOP AT it_bname.
    $ix = sy-tabix.
    IF it_bname-bname CN '0123456789 '.
    ELSE.
      WRITE '00' TO it_bname-pernr.
      WRITE it_bname-bname TO it_bname-pernr+2.
      MODIFY it_bname INDEX $ix.
    ENDIF.
  ENDLOOP.

  SORT it_bname.

  __cls : it_user.

  SELECT a~bname
         a~persnumber
         a~addrnumber
         b~name_last b~name_first
         INTO TABLE it_user
         FROM usr21 AS a
         INNER JOIN v_addr_usr AS b
         ON  b~addrnumber EQ a~addrnumber
         AND b~persnumber EQ a~persnumber
         FOR ALL ENTRIES IN it_bname
         WHERE a~bname EQ it_bname-bname .

  __cls it_pernr.

  SELECT pernr kostl orgeh
         INTO TABLE it_pernr
         FROM pa0001
         FOR ALL ENTRIES IN it_bname
         WHERE pernr EQ it_bname-pernr .

  DATA $pernr LIKE pa0001-pernr.

  SORT it_pernr BY pernr.

  LOOP AT it_user.
    $ix = sy-tabix.
    IF it_user-bname CN '0123456789 '.
    ELSE.
      WRITE '00' TO $pernr.
      WRITE it_user-bname TO $pernr+2.
      READ TABLE it_pernr WITH KEY pernr = $pernr BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_user-kostl = it_pernr-kostl.
        it_user-orgeh = it_pernr-orgeh.

        MODIFY it_user INDEX $ix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_user BY bname.

ENDFORM.                    " prepare_cc_org

*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

  LOOP AT gt_out.
    $ix = sy-tabix.
    READ TABLE it_user WITH KEY bname = gt_out-account BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-name_last  = it_user-name_last.
      gt_out-name_first = it_user-name_first.
      gt_out-kostl    = it_user-kostl.
      gt_out-orgeh    = it_user-orgeh.

      READ TABLE it_orgtx WITH KEY orgeh = gt_out-orgeh BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-orgtx = it_orgtx-orgtx.
      ENDIF.

    ENDIF.
    MODIFY gt_out INDEX $ix.
  ENDLOOP.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SAPWLSERV_INSTSHORT  text
*      -->P_$DATE  text
*----------------------------------------------------------------------*
FORM get_data_new USING p_instshort p_date.

  DATA result1 LIKE sy-subrc.
  DATA component TYPE swnchostname.

  component = p_instshort.

  __cls ex_data.
  CALL FUNCTION 'SWNC_COLLECTOR_GET_AGGREGATES'
    EXPORTING
      component        = component
      periodtype       = 'D'
      periodstrt       = p_date
      factor           = 1000
    TABLES
      tasktype         = ex_data-summary
      tasktimes        = ex_data-tasktype_stat
      times            = ex_data-time_stat
      dbprocs          = ex_data-dbprocedure_stat
      extsystem        = ex_data-externsys
      tcdet            = ex_data-detailtcode_stat
      frontend         = ex_data-terminal_io_stat
      memory           = ex_data-memory_stat
      spoolact         = ex_data-spool_activity_stat
      tablerec         = ex_data-table_record_stat
      usertcode        = ex_data-application_stat
      userworkload     = ex_data-accountstat_tasktype
      rfcclnt          = ex_data-rfc_client_stat
      rfcclntdest      = ex_data-rfc_client_dest_stat
      rfcsrvr          = ex_data-rfc_server_stat
      rfcsrvrdest      = ex_data-rfc_server_dest_stat
      spool            = ex_data-spool_print_stat
      hitlist_database = ex_data-hitlist_dbcalls
      hitlist_resptime = ex_data-hitlist_respti
      astat            = ex_data-as_statistic
      ashitl_database  = ex_data-as_hitl_dbcalls
      ashitl_resptime  = ex_data-as_hitl_respti
      dbcon            = ex_data-dbcon
      vmc              = ex_data-vmc_stat
      websd            = ex_data-web_server_dest
      webcd            = ex_data-web_client_dest
      webs             = ex_data-web_server
      webc             = ex_data-web_client
      comp_hierarchy   = ex_data-comp_hierarchy
      org_units        = ex_data-org_unit
    EXCEPTIONS
      no_data_found    = 1
      OTHERS           = 2.

  IF sy-subrc = 0.

    tab_app = ex_data-application_stat.

    tab_rfcsrvr     = ex_data-rfc_server_stat.
    tab_rfcsrvrdest = ex_data-rfc_server_dest_stat.


    LOOP AT tab_app INTO wa_app.

      CHECK wa_app-account IN s_bname.

      MOVE-CORRESPONDING wa_app TO it_row_tab.
      it_row_tab-tcode = wa_app-entry_id.
      it_row_tab-ldate = p_date.
      it_row_tab-month = p_date(6).
      it_row_tab-instance = p_instshort.

      it_row_tab-diastepcnt = wa_app-count.


      it_row_tab-respst = wa_app-respti / 1000.
      it_row_tab-dbst   = wa_app-readdirti +
                        wa_app-readseqti + wa_app-chngti.

      it_row_tab-waitst = wa_app-queueti.
      it_row_tab-readseqcnt = wa_app-readseqcnt.
      it_row_tab-readseqst = wa_app-readseqti.
      IF wa_app-readseqcnt NE 0.
        it_row_tab-readseqmt = it_row_tab-readseqst
        / wa_app-readseqcnt.
      ENDIF.
      it_row_tab-readseqst = it_row_tab-readseqst / 1000.
      IF wa_app-count NE 0.
        it_row_tab-respmt = wa_app-respti / wa_app-count.
        it_row_tab-dbprmt = it_row_tab-dbptime / wa_app-count.
        it_row_tab-cpumt  = wa_app-cputi / wa_app-count.
        it_row_tab-fnetmt  = wa_app-guinettime / wa_app-count.
        it_row_tab-fguimt  = wa_app-guitime / wa_app-count.
        it_row_tab-dbmt       =  it_row_tab-dbst    / wa_app-count.
        it_row_tab-waitmt = it_row_tab-waitst / wa_app-count.
      ENDIF.

      it_row_tab-dbst = it_row_tab-dbst / 1000.
      it_row_tab-waitst = it_row_tab-waitst / 1000.
      it_row_tab-cpust = wa_app-cputi / 1000.

      it_row_tab-dbptime    = wa_app-dbp_time.
      it_row_tab-readdircnt = wa_app-readdircnt.

      it_row_tab-readdirst = wa_app-readdirti.
      IF wa_app-readdircnt NE 0.
        it_row_tab-readdirmt = it_row_tab-readdirst / wa_app-readdircnt.
      ENDIF.

      it_row_tab-chngcnt = wa_app-chngcnt.

      IF it_row_tab-chngcnt NE 0.
        it_row_tab-chngmt    = it_row_tab-chngst / it_row_tab-chngcnt.
      ENDIF.

      it_row_tab-chngst =  wa_app-chngti.
      it_row_tab-chngst = it_row_tab-chngst / 1000.
      it_row_tab-fguicnt    = wa_app-guicnt.

      it_row_tab-dbrequcnt  = wa_app-readdircnt +
                       wa_app-readseqcnt + it_row_tab-chngcnt.


      IF it_row_tab-dbrequcnt <> 0.
        it_row_tab-dbreqmti = it_row_tab-dbst / it_row_tab-dbrequcnt.
      ENDIF.


      it_row_tab-phyreadcnt = wa_app-phyreadcnt.
      it_row_tab-phychngrec = wa_app-phychngrec.

      it_row_tab-dbufreadcn = wa_app-readdirbuf.
      it_row_tab-dbpcount =  wa_app-dbp_count.
      it_row_tab-dbptime = wa_app-dbp_time.

      IF wa_app-dbp_count <> 0.
        it_row_tab-dbpavti = wa_app-dbp_time / wa_app-dbp_count.
      ENDIF.


      it_row_tab-respst  = wa_app-respti.

      IF it_row_tab-respst NE 0.
        it_row_tab-mcputi = it_row_tab-cpust / it_row_tab-respst * 100..
      ENDIF.

      IF it_row_tab-respst >= it_row_tab-waitst.
        it_row_tab-mwaitti   = it_row_tab-waitst /
                                 it_row_tab-respst * 100.
      ENDIF.

      it_row_tab-respst     = it_row_tab-respst    / 1000.


      wa_app-readdirti = wa_app-readdirti / 1000.
      wa_app-readseqti = wa_app-readseqti / 1000.
      wa_app-chngti = wa_app-chngti / 1000.

      $mdbti = wa_app-readdirti
                          + wa_app-readseqti + wa_app-chngti.

      IF $mdbti <= 9999.
        it_row_tab-mdbti = $mdbti.
      ELSE.
        CLEAR $mdbti.
      ENDIF.

      IF it_row_tab-respst >= it_row_tab-dbst.
        it_row_tab-mdbti   = it_row_tab-dbst / it_row_tab-respst * 100.
      ENDIF.

      IF it_row_tab-respst >= it_row_tab-dbptime.
        it_row_tab-mdbprti   = it_row_tab-dbptime /
        it_row_tab-respst * 100.
      ENDIF.

      it_row_tab-fguist = wa_app-guitime.
      it_row_tab-fnetst = wa_app-guinettime.

      IF it_row_tab-respst >= it_row_tab-fguist.
        it_row_tab-mfguiti   = it_row_tab-fguist /
            it_row_tab-respst * 100.
      ENDIF.

      IF it_row_tab-fguicnt <> 0.
        it_row_tab-fguitrpmt =  it_row_tab-fguist
         /  it_row_tab-fguicnt.
      ENDIF.
      it_row_tab-fnetst = it_row_tab-fnetst / 1000.
      it_row_tab-fguist = it_row_tab-fguist / 1000.

*      it_row_tab-bytes      = ( wa_app-abaplcnt + wa_app-abapscnt +
*                         wa_app-dynplcnt + wa_app-dynpscnt +
*                         wa_app-cualcnt  +
*                         wa_app-quecnt   + wa_app-ntabcnt +
*                         wa_app-dsqlcnt + wa_app-ddiccnt ) / 1024.

      APPEND it_row_tab.
    ENDLOOP.

    LOOP AT tab_rfcsrvr INTO wa_rfcsrvr.
      MOVE-CORRESPONDING wa_rfcsrvr TO it_rfc_svr.
      it_rfc_svr-ldate = p_date.
      it_rfc_svr-instance = p_instshort.
      it_rfc_svr-aedat = sy-datum.
      it_rfc_svr-aetim = sy-uzeit.
      it_rfc_svr-aenam = sy-uname.
      APPEND it_rfc_svr.
    ENDLOOP.

    LOOP AT tab_rfcsrvrdest INTO wa_rfcsrvrdest.
      MOVE-CORRESPONDING wa_rfcsrvrdest TO it_rfc_dst.
      it_rfc_dst-ldate = p_date.
      it_rfc_dst-instance = p_instshort.
      it_rfc_dst-aedat = sy-datum.
      it_rfc_dst-aetim = sy-uzeit.
      it_rfc_dst-aenam = sy-uname.
      APPEND it_rfc_dst.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_DATA_NEW
