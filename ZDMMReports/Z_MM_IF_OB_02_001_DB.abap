*----------------------------------------------------------------------
* Program ID        : Z_MM_IF_OB_02_001_DB
* Title             : [MM] GCCS Interface - Part Demand
* Created on        : 11/05/2008
* Created by        : Furong
* Specifications By : Crossley, Ron
* Description       : GCCS Interface - Part Demand
*                     Copy from ZM_IF_YM_1006_DEMAND for performance
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
*----------------------------------------------------------------------
REPORT z_mm_if_ob_02_001_db MESSAGE-ID zmco.

TYPE-POOLS : slis,
             icon.

TABLES: mara,
        mdsb,
        t001k,mdps, ztmm_parts_21day.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_alv.
INCLUDE  TYPE ztmm_eai_demand.
TYPES : chkbox(1),
        icon TYPE icon_d,
        tabcolor  TYPE slis_t_specialcol_alv.
TYPES: END OF ty_alv.

DATA: BEGIN OF it_resb_tmp OCCURS 0,
        matnr LIKE resb-matnr  ,
        bdter LIKE resb-bdter  ,
        bdmng LIKE resb-bdmng  ,
        lifnr LIKE lfa1-lifnr  ,
      END   OF it_resb_tmp     ,

      BEGIN OF it_day OCCURS 21,
        seq     TYPE i         ,
        datum   LIKE   sy-datum,
      END   OF it_day          ,

      BEGIN OF it_week OCCURS 21,
        seq(2)  TYPE n          ,
        datum   LIKE sy-datum   ,
      END   OF it_week          .

DATA:  it_resb_tmp1 LIKE it_resb_tmp  OCCURS 0 WITH HEADER LINE,
       it_resb      LIKE it_resb_tmp  OCCURS 0 WITH HEADER LINE,
       it_resb1     LIKE it_resb_tmp  OCCURS 0 WITH HEADER LINE,
       it_21day  LIKE ztmm_eai_demand OCCURS 0 WITH HEADER LINE,
       it_21week LIKE ztmm_eai_demand OCCURS 0 WITH HEADER LINE,
       gv_kalid    LIKE kako-kalid ,
       gv_lastdate LIKE sy-datum   ,
       gv_lastweek LIKE sy-datum   ,
       gv_text(20) TYPE c          ,
       gv_return.

DATA: gt_alv        TYPE TABLE OF ty_alv     WITH HEADER LINE   ,
      gt_fieldcat   TYPE slis_t_fieldcat_alv                    .

DATA : gs_layout   TYPE slis_layout_alv  ,
       gs_variant  TYPE disvariant       ,
       gs_fieldcat TYPE slis_fieldcat_alv,
       gv_repid    LIKE sy-repid         ,
       gv_lines TYPE i                   .

DATA : BEGIN OF it_vmat OCCURS 0,
        matnr LIKE eord-matnr,
        lifnr LIKE eord-lifnr,
       END OF it_vmat.


DATA i_ztmm_parts LIKE ztmm_parts_21day OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF material_info,
        datum LIKE sy-datum,
        matnr LIKE mara-matnr,
        bwkey LIKE mbew-bwkey,
*------ characteristics
        sobsl LIKE marc-sobsl,
        mandt LIKE mara-mandt,
        bukrs LIKE t001k-bukrs,
        bklas LIKE mbew-bklas,
        waers TYPE waers,
        vvatage TYPE vvatage,
        maktx LIKE makt-maktx,
        raube LIKE mara-raube,
        matkl LIKE mara-matkl,
        mtart LIKE mara-mtart,
        mstae LIKE mara-mstae,
        mmsta LIKE marc-mmsta,
        lifnr LIKE eord-lifnr,
        name1 LIKE lfa1-name1,
        dispo LIKE marc-dispo,
        ekgrp LIKE marc-ekgrp,
        meins LIKE mara-meins,
        profl LIKE mara-profl,

        peinh LIKE mbew-peinh,
        lbkum LIKE mbew-lbkum,
        stprs LIKE mbew-stprs,
        verpr LIKE mbew-verpr,
        salk3 LIKE mbew-salk3,

       $peinh(10),
       $stprs(10),
       $verpr(10),

      END OF material_info.

DATA : it_mat  LIKE TABLE OF material_info WITH HEADER LINE.
DATA $it_mat LIKE it_mat OCCURS 0 WITH HEADER LINE.
DATA: lt_mard LIKE mard OCCURS 0 WITH HEADER LINE.
DATA: i_mdkp LIKE mdkp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_mrp_org OCCURS 0,
        werks LIKE plaf-plwrk,  "FIXME production plant : PWWRK
        matnr LIKE pbim-matnr,
        pedtr LIKE plaf-pedtr,  "finish date
        gsmng LIKE plaf-gsmng,  "receipt (+)
        bdmng LIKE plaf-bdmng,  "requirement (-)
        delkz LIKE mdtb-delkz, "MRP element

*        dsdat LIKE mdkp-dsdat,

       END OF it_mrp_org.

DATA : BEGIN OF it_mrp OCCURS 0,
        werks LIKE plaf-plwrk,  "FIXME production plant : PWWRK
        matnr LIKE pbim-matnr,
        pedtr LIKE plaf-pedtr,  "finish date
        gsmng LIKE plaf-gsmng,  "receipt (+)
        bdmng LIKE plaf-bdmng,  "requirement (-)
*        dsdat LIKE mdkp-dsdat,
       END OF it_mrp.

DATA: BEGIN OF it_collect OCCURS 0,
        matnr LIKE mara-matnr,
        bwkey LIKE mbew-bwkey,
        dispo LIKE marc-dispo,
        $peinh(15),
        $stprs(15),
        $verpr(15),
        lifnr LIKE eord-lifnr,
        ekgrp LIKE marc-ekgrp,
        bklas LIKE mbew-bklas,
        mstae LIKE mara-mstae,
        mmsta LIKE marc-mmsta,
      END OF it_collect.

DATA: BEGIN OF i_mdpsx OCCURS 100.
        INCLUDE STRUCTURE mdps.
DATA: END OF i_mdpsx.
DATA: BEGIN OF i_mdtbx OCCURS 100.
        INCLUDE STRUCTURE mdtb.
DATA: END OF i_mdtbx.

TYPES : BEGIN OF t_mara ,
        werks LIKE marc-werks,
        matnr LIKE mara-matnr,
        beskz LIKE marc-beskz,
        meins LIKE mara-meins,
        vspvb LIKE marc-vspvb,
       END OF t_mara.

DATA : it_mara TYPE t_mara OCCURS 0 WITH HEADER LINE,
       wa_mara TYPE t_mara.

DATA : it_data  LIKE ztmm_eai_demand OCCURS 0 WITH HEADER LINE.
DATA : tot_data  LIKE ztmm_eai_demand OCCURS 0 WITH HEADER LINE.
*--< Parallel Processing Victor 09.21.2011

DATA: BEGIN OF it_job OCCURS 0,
*        mtno_f     LIKE   ztbm_bom_ecm-mtno,
*        mtno_t     LIKE   ztbm_bom_ecm-mtno,
*        matnr_f    LIKE   mara-matnr,
        matnr    LIKE   mara-matnr,
        start        TYPE   i,
        end          TYPE   i,
        jobname    LIKE   btch1140-jobname,
*        execserver LIKE   btch1140-execserver,
        jobcnt     LIKE tbtcjob-jobcount,
        msg(100),
      END   OF it_job.

*- Back job
DATA: group LIKE rzllitab-classname VALUE 'PG_STK',
      wp_available TYPE i,      "Number of dialog work processes
      wp_total TYPE i,          "Total number of dialog work
      msg(80) VALUE space,      "Container for error message in
      info LIKE rfcsi, c,       "Message text
      snd_jobs TYPE i VALUE 0,  "Work packets sent for processing
      rcv_jobs TYPE i VALUE 0,  "Work packet replies received
      excp_flag(1) TYPE c,      "Number of RESOURCE_FAILUREs
      BEGIN OF tasklist OCCURS 10,  "Task administration
         taskname    LIKE btch1140-jobname,
         rfcdest     LIKE rfcsi-rfcdest,
         rfchost     LIKE rfcsi-rfchost,
      END OF tasklist.
DATA : taskname(32).
DATA : p_count TYPE i VALUE 15.  "Max process count

DATA g_error.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
CONSTANTS:
      mddisp(2) TYPE c VALUE 'MD',     "Materialdisposition
      lfplan(2) TYPE c VALUE 'LP'.     "Langfristplanung

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
PARAMETERS: p_datum LIKE sy-datum DEFAULT sy-datum OBLIGATORY,
            p_werks LIKE marc-werks DEFAULT 'P001',
            p_bukrs LIKE t001-bukrs DEFAULT 'H201' NO-DISPLAY,
            p_split NO-DISPLAY.
** S> 08/11/11 PAUL
*PARAMETERS: p_mrp AS CHECKBOX DEFAULT ' '.
*PARAMETERS: p_ltp AS CHECKBOX DEFAULT 'X'.
** E<
PARAMETERS:p_plscn LIKE rm61r-plscn  DEFAULT '900' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS : p_d21 RADIOBUTTON GROUP g2  DEFAULT 'X',
             p_w21 RADIOBUTTON GROUP g2            ,
             p_both RADIOBUTTON GROUP g2           .

PARAMETERS p_chk21 AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-002.
PARAMETER: p_opt1 RADIOBUTTON GROUP g1 DEFAULT 'X',
           p_opt2 RADIOBUTTON GROUP g1,
           p_opt3 RADIOBUTTON GROUP g1,
           p_opt4 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME.
PARAMETERS p_dest LIKE rfcdes-rfcdest
                       DEFAULT 'WMRM01'.
*PARAMETERS: p_test as checkbox DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b33 WITH FRAME TITLE text-006.
PARAMETERS    :
    p_old   DEFAULT 'X' NO-DISPLAY, " Include Old MRP/LTP Data
    p_rcpt  DEFAULT 'X' NO-DISPLAY, " Include Planned Receipt
    p_9999  DEFAULT 'X' NO-DISPLAY, " Exclude 9999 Stock
    p_zero  NO-DISPLAY,             " Include Under 0 at 1st Period
    p_nomrp DEFAULT 'X' NO-DISPLAY. " Include No-MRP/LTP materials
SELECTION-SCREEN END OF BLOCK b33.

PARAMETERS: p_single AS CHECKBOX.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title = '[MM] GCS Interface - Part Demand'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
** Changed by Furong on 01/20/11
*  IF p_d21 = 'X' OR p_both = 'X'.
*    PERFORM set_days.
*  ENDIF.
*  IF p_w21 = 'X' OR p_both = 'X'.
*    PERFORM set_week.
*  ENDIF.
  PERFORM set_days.
  PERFORM set_week.
  PERFORM delete_past_data.
** End of change
  IF p_opt1 = 'X'.      " Display current data.
    PERFORM get_data_from_ztable.
  ELSEIF p_opt2 = 'X'.  " Refresh Data
    PERFORM save_data.
  ELSEIF p_opt3 = 'X'.  " Refresh and send data
    PERFORM save_data.
    PERFORM send_data_to_eai.
  ELSEIF p_opt4 = 'X'.  " Send existing data
    PERFORM send_data_to_eai.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  alv_variant_f4
*&---------------------------------------------------------------------*
*       F4 help for ALV Variant
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
FORM alv_variant_f4 CHANGING p_vari.
  DATA: rs_variant LIKE disvariant.

  CLEAR rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0.
    p_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                    " alv_variant_f4
*&---------------------------------------------------------------------*
*&      Form  set_days
*&---------------------------------------------------------------------*
*       Subroutine to calculate number of days
*----------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

*-reading working calendar
  PERFORM read_shop_calid  USING gv_kalid.
*-first is current input date
  it_day-seq = 0.
  it_day-datum = p_datum.
  APPEND it_day.
  CLEAR l_count.
  l_date = p_datum .
  DO 21 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  gv_kalid  l_date.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
  ENDDO.
  SORT it_day BY seq datum.
  gv_lastdate = l_date .
ENDFORM.                    " set_days
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gv_kalid  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " read_shop_calid
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
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       Subroutine to get existing data from Z table
*----------------------------------------------------------------------*
FORM get_data_from_ztable.
*  DATA: w_datfm(1) TYPE c.

  REFRESH: it_21day.
  CLEAR  : it_21day.
  IF p_d21 = 'X'.
    SELECT * FROM ztmm_eai_demand
             INTO TABLE it_21day
             WHERE epart_no IN s_matnr AND
                   edmd_type = 'D'     AND
                   edate = p_datum.
  ELSEIF p_w21 = 'X'.
    SELECT * FROM ztmm_eai_demand
             INTO TABLE it_21day
             WHERE epart_no IN s_matnr AND
                   edmd_type = 'W'     AND
                   edate = p_datum.
  ELSEIF p_both = 'X'.
    SELECT * FROM ztmm_eai_demand
             INTO TABLE it_21day
             WHERE epart_no IN s_matnr AND
                   edate = p_datum.

  ENDIF.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH text-005.
    EXIT.
  ENDIF.

  gv_text = 'Preparing output...'.
  PERFORM show_progress USING gv_text.

  REFRESH gt_alv.
  CLEAR   gt_alv.

*  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
*       EXPORTING
*            bname = sy-uname
*       IMPORTING
*            datfm = w_datfm.

  LOOP AT it_21day.
    MOVE-CORRESPONDING it_21day TO gt_alv.
*    CASE w_datfm.
*      WHEN 1.
*        CONCATENATE it_21day-edate+2(2) it_21day-edate+0(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '.'.
*
*      WHEN 2.
*        CONCATENATE it_21day-edate+0(2) it_21day-edate+2(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '/'.
*
*      WHEN 3.
*        CONCATENATE it_21day-edate+0(2) it_21day-edate+2(2)
*                    it_21day-edate+4(4) INTO gt_alv-edate
*                    SEPARATED BY '-'.
*
*      WHEN 4.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '.'.
*
*      WHEN 5.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '/'.
*
*      WHEN 6.
*        CONCATENATE it_21day-edate+4(4) it_21day-edate+0(2)
*                    it_21day-edate+2(2) INTO gt_alv-edate
*                    SEPARATED BY '-'.
*
*    ENDCASE.

    CASE it_21day-tait_targ_rslt.
      WHEN space.
      WHEN 'S'.
        gt_alv-icon = icon_led_green.
      WHEN 'F'.
        gt_alv-icon = icon_led_red.
    ENDCASE.
    APPEND gt_alv.
    CLEAR  gt_alv.
  ENDLOOP.

  CLEAR gv_lines.
  DESCRIBE TABLE  gt_alv LINES gv_lines.
  IF gv_lines = 1.
    MESSAGE s000 WITH gv_lines text-012.
  ELSEIF gv_lines > 1.
    MESSAGE s000 WITH gv_lines text-011.
  ENDIF.

  PERFORM display_alv_data.

ENDFORM.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       Subroutine to delete existing data and Save new data
*----------------------------------------------------------------------*
FORM save_data.
  DATA: l_fdate_21d LIKE sy-datum.
  DATA: l_ldate_21d LIKE sy-datum.

  CLEAR gv_text.
  gv_text = 'Refreshing data...'.
  PERFORM show_progress USING gv_text.
  IF p_d21 = 'X'.
    DELETE FROM ztmm_eai_demand
      WHERE epart_no IN s_matnr AND
            edmd_type = 'D'     AND
            edate = p_datum.
  ELSEIF p_w21 = 'X'.
    DELETE FROM ztmm_eai_demand
      WHERE epart_no IN s_matnr AND
            edmd_type = 'W'     AND
            edate = p_datum.
  ELSEIF p_both = 'X'.
    DELETE FROM ztmm_eai_demand
      WHERE epart_no IN s_matnr AND
            edate = p_datum.
  ENDIF.

  CLEAR g_error.

*  IF P_D21 = 'X'.
*
**-If 21 days data is selected, get data from first day
*
*** Changed by Furong on 03/12/10
*    SELECT A~MATNR BDTER BDMNG B~LIFNR
*           INTO TABLE IT_RESB_TMP
*           FROM MDSB AS A
*           INNER JOIN EORD AS B
*           ON A~MATNR = B~MATNR
*** Changed by Furong on 01/20/11
**           WHERE bdter >= p_datum AND
**                 a~matnr IN s_matnr AND
*           WHERE A~MATNR IN S_MATNR AND
*                 BDTER BETWEEN P_DATUM AND GV_LASTDATE AND
*** End of change
*                 XLOEK = ' ' AND
*                 VDATU <= SY-DATUM AND
*                 BDATU >= SY-DATUM.
*    .
**   SELECT MATNR BDTER BDMNG
**           INTO TABLE IT_RESB_TMP
**           FROM MDSB
**           WHERE BDTER >= P_DATUM AND
**                 MATNR IN S_MATNR AND
**                 XLOEK = ' '.
*** End of change
*
*    SORT IT_RESB_TMP BY MATNR BDTER.
*    LOOP AT IT_RESB_TMP.
*      MOVE-CORRESPONDING IT_RESB_TMP TO IT_RESB.
*      COLLECT IT_RESB.
*    ENDLOOP.
*    SORT IT_RESB BY MATNR BDTER.
*    DELETE ADJACENT DUPLICATES FROM IT_RESB COMPARING MATNR BDTER.
*
*    IF IT_RESB[] IS INITIAL.
*      MESSAGE S000 WITH TEXT-005.
*      EXIT.
*    ENDIF.
*
*  ELSEIF P_W21 = 'X'.
**-If 21 days data is selected, get data from first day of first week
*    SORT IT_WEEK.
*
*** Changed by Furong on 03/14/08   "UD1K943095
**    READ TABLE it_week INDEX 1.
**    SELECT matnr bdter bdmng
**           INTO TABLE it_resb_tmp1
**           FROM mdsb
**           WHERE bdter >= it_week-datum AND
**                 matnr IN s_matnr       AND
**                 xloek = ' '.
*
*    READ TABLE IT_WEEK INDEX 4.
*    L_FDATE_21D = IT_WEEK-DATUM - 1.
*    READ TABLE IT_WEEK INDEX 22.
*    L_LDATE_21D = IT_WEEK-DATUM.
***C__ 06/02/11 PAUL insert P_werks WHERE CONDITION
*** Changed by Furong on 03/12/10
*    SELECT A~MATNR BDTER BDMNG B~LIFNR
*      INTO TABLE IT_RESB_TMP1
*      FROM MDSB AS A
*     INNER JOIN EORD AS B
*        ON A~MATNR EQ B~MATNR
*     WHERE A~MATNR IN S_MATNR
*** Changed by Furong on 01/20/11
*       AND A~WERKS EQ P_WERKS
*       AND A~BDTER BETWEEN P_DATUM AND GV_LASTDATE
**                 BDTER >= P_DATUM AND
*** end of change
*       AND A~XLOEK = ' '
*       AND B~VDATU <= SY-DATUM
*       AND B~BDATU >= SY-DATUM.
***E_<
*
***C__ 06/02/11 PAUL insert P_werks WHERE CONDITION
*    SELECT A~MATNR BDTER BDMNG B~LIFNR
* APPENDING TABLE IT_RESB_TMP1
*      FROM MDSM AS A
*     INNER JOIN EORD AS B
*        ON A~MATNR = B~MATNR
*     WHERE A~PLSCN = '900'
*       AND A~MATNR IN S_MATNR
**       AND A~PERKS EQ =+HERL
*       AND A~WERKS EQ P_wERKS
*       AND A~BDTER BETWEEN L_FDATE_21D AND L_LDATE_21D
*       AND B~VDATU <= SY-DATUM
*       AND B~BDATU >= SY-DATUM.
***E_<*
**   SELECT MATNR BDTER BDMNG
**           INTO TABLE IT_RESB_TMP1
**           FROM MDSB
**           WHERE MATNR IN S_MATNR AND
**                 BDTER BETWEEN P_DATUM AND  L_FDATE_21D AND
**                 XLOEK = ' '.
**  SELECT MATNR BDTER BDMNG
**           APPENDING TABLE IT_RESB_TMP1
**           FROM MDSM
**            WHERE MATNR IN S_MATNR   AND
**                  PLSCN = '900'  AND
**                  BDTER BETWEEN L_FDATE_21D AND L_LDATE_21D.
*
*** End of change on 03/12/10
*
*** End of change
*    SORT IT_RESB_TMP1 BY MATNR BDTER.
*    LOOP AT IT_RESB_TMP1.
*      MOVE-CORRESPONDING IT_RESB_TMP1 TO IT_RESB1.
*      COLLECT IT_RESB1.
*    ENDLOOP.
*    SORT IT_RESB1 BY MATNR BDTER.
*    DELETE ADJACENT DUPLICATES FROM IT_RESB1 COMPARING MATNR BDTER.
*
*    IF IT_RESB1[] IS INITIAL.
*      MESSAGE S000 WITH TEXT-005.
*      EXIT.
*    ENDIF.
*
*  ELSEIF P_BOTH ='X'.


*-If both 21 days and 21 weeks data is selected, for 21 days, get data
*-from first day and for 21 weeks get data from first day of first week

*    SELECT A~MATNR BDTER BDMNG B~LIFNR
*      INTO TABLE IT_RESB_TMP
*      FROM MDSB AS A
*     INNER JOIN EORD AS B
*        ON A~MATNR = B~MATNR
*     WHERE A~MATNR IN S_MATNR
*       AND A~WERKS EQ P_WERKS
*       AND A~BDTER BETWEEN P_DATUM AND GV_LASTDATE
*       AND A~XLOEK = ' '
*       AND B~VDATU <= SY-DATUM
*       AND B~BDATU >= SY-DATUM.
*

*    LOOP AT IT_RESB_TMP.
*      IF IT_RESB_TMP-BDTER <= L_FDATE_21D.
*        IT_RESB_TMP1 = IT_RESB_TMP.
*        APPEND IT_RESB_TMP1.
*      ENDIF.
*    ENDLOOP.
*
*    SELECT A~MATNR BDTER BDMNG B~LIFNR
* APPENDING TABLE IT_RESB_TMP1
*      FROM MDSM AS A
*     INNER JOIN EORD AS B
*        ON A~MATNR = B~MATNR
*     WHERE A~PLSCN EQ '900'
*       AND A~MATNR IN S_MATNR
*       AND A~WERKS EQ P_WERKS
*       AND A~BDTER BETWEEN L_FDATE_21D AND L_LDATE_21D
*       AND B~VDATU <= SY-DATUM
*       AND B~BDATU >= SY-DATUM.
*    SORT IT_WEEK.
*
** Changed by Furong on 03/14/08   "UD1K943095
*    READ TABLE it_week INDEX 1.
*    SELECT matnr bdter bdmng
*           INTO TABLE it_resb_tmp1
*           FROM mdsb
*           WHERE bdter >= it_week-datum AND
*                 matnr IN s_matnr       AND
*                 xloek = ' '.
*
*    READ TABLE IT_WEEK INDEX 4.
*    L_FDATE_21D = IT_WEEK-DATUM - 1.
*    READ TABLE IT_WEEK INDEX 22.
*    L_LDATE_21D = IT_WEEK-DATUM.
*
* summarize MRP/LTP using date range
*    LOOP AT i_mdtbx.
*      CLEAR it_mrp_org.
*      it_mrp_org-matnr = i_mdkp-matnr.
*      it_mrp_org-werks = i_mdkp-plwrk.
*
*      IF i_mdtbx-dat00 < sy-datum AND p_old = 'X'.
*        it_mrp_org-pedtr = sy-datum.
*
*      ELSEIF
*      (i_mdtbx-dat00 >= sy-datum AND i_mdtbx-dat00 < L_LDATE_21D).
*        it_mrp_org-pedtr = L_LDATE_21D.
*      ELSEIF i_mdtbx-dat00 < g_next_60.
*        it_mrp_org-pedtr = g_next_60.
*      ELSEIF i_mdtbx-dat00 < g_next_90.
*        it_mrp_org-pedtr = g_next_90.
*      ELSE.
*        it_mrp_org-pedtr = g_next_99.
*      ENDIF.
*
*      IF NOT it_mrp_org-pedtr IS INITIAL.
*        IF i_mdtbx-plumi = '-'.  "+/-
*          it_mrp_org-bdmng = - i_mdtbx-mng01.
*        ELSEIF i_mdtbx-plumi = '+'.  "+/-
*          it_mrp_org-gsmng = i_mdtbx-mng01.
*        ENDIF.
*
*        it_mrp_org-delkz = i_mdtbx-delkz.
*
********  It's not been actiavted.
********  it_mrp_org-dsdat = i_mdkp-dsdat.
*
*        COLLECT it_mrp_org.
*      ENDIF.
*
*    ENDLOOP.

* by ig.moon {
  PERFORM get_source_list USING sy-datum.
  READ TABLE it_vmat INDEX 1.
  IF sy-subrc EQ 0.
    SORT it_vmat BY matnr lifnr.
  ELSE.
    g_error = 'X'.
  ENDIF.

  IF g_error EQ space.
    PERFORM get_materials.
    PERFORM get_9999_stock.
    PERFORM get_mrp_materials.
    PERFORM get_mrp_data USING sy-datum.
  ENDIF.
* }

  SORT it_resb_tmp BY matnr bdter.
  LOOP AT it_resb_tmp.
    MOVE-CORRESPONDING it_resb_tmp TO it_resb.
    COLLECT it_resb.
  ENDLOOP.
  SORT it_resb BY matnr bdter.
  DELETE ADJACENT DUPLICATES FROM it_resb COMPARING matnr bdter.

  SORT it_resb_tmp1 BY matnr bdter.
  LOOP AT it_resb_tmp1.
    MOVE-CORRESPONDING it_resb_tmp1 TO it_resb1.
    COLLECT it_resb1.
  ENDLOOP.
  SORT it_resb1 BY matnr bdter.
  DELETE ADJACENT DUPLICATES FROM it_resb1 COMPARING matnr bdter.

  IF it_resb[] IS INITIAL AND it_resb1[] IS INITIAL.
    IF p_chk21 NE 'X'.
      MESSAGE s000 WITH text-005.
      EXIT.
    ENDIF.
  ENDIF.
*  ENDIF.    " IF p_d21 = 'X'.

  IF p_chk21 EQ 'X'.
    IF p_d21 = 'X'.
      PERFORM set_21days_data_from_ztmm_part.
    ELSEIF p_w21 = 'X'.
      PERFORM set_21weeks_data.
    ELSEIF p_both = 'X'.
      PERFORM set_21days_data_from_ztmm_part.
      PERFORM set_21weeks_data.
    ENDIF.
  ELSE.
    IF p_d21 = 'X'.
      PERFORM set_21days_data.
    ELSEIF p_w21 = 'X'.
      PERFORM set_21weeks_data.
    ELSEIF p_both = 'X'.
      PERFORM set_21days_data.
      PERFORM set_21weeks_data.
    ENDIF.
  ENDIF.

  LOOP AT it_21week.
    MOVE-CORRESPONDING it_21week TO it_21day.
    APPEND it_21day.
    CLEAR  it_21day.
  ENDLOOP.

  SORT it_21day BY epart_no edate edmd_type.
  DELETE ADJACENT DUPLICATES FROM it_21day COMPARING
  epart_no edate edmd_type.

*  CHECK P_TEST IS INITIAL.
** Added on 12/14/11
  PERFORM get_receive_data.
** End on 12/14/11
  PERFORM update_table.

ENDFORM.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  send_data_to_eai
*&---------------------------------------------------------------------*
*       Subroutine to call RFC to pass data to WebMethods
*----------------------------------------------------------------------*
FORM send_data_to_eai.

  DATA : e_return LIKE zmms0053.

  IF p_opt4 = 'X'.
    REFRESH: it_21day.
    CLEAR  : it_21day.
    IF p_d21 = 'X'.
      SELECT * FROM ztmm_eai_demand
               INTO TABLE it_21day
               WHERE epart_no IN s_matnr AND
                     edmd_type = 'D'     AND
                     edate    = p_datum.
    ELSEIF p_w21 = 'X'.
      SELECT * FROM ztmm_eai_demand
               INTO TABLE it_21day
               WHERE epart_no IN s_matnr AND
                     edmd_type = 'W'     AND
                     edate    = p_datum.
    ELSEIF p_both = 'X'.
      SELECT * FROM ztmm_eai_demand
               INTO TABLE it_21day
               WHERE epart_no IN s_matnr AND
                     edate    = p_datum.
    ENDIF.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH text-005.
      EXIT.
    ENDIF.
  ENDIF.        " IF p_opt4 = 'X'.

** S> 08/11/11 Paul
*  check p_test is initial.
** E<

  IF NOT it_21day[] IS INITIAL.
    DATA: l_msgtxt(100) TYPE c   ,
          l_size        TYPE num9.


*    CALL FUNCTION 'Z_GCS_EAI_DEMAND'
*        DESTINATION p_dest
*     TABLES
*       eai_demand          = it_21day
*     EXCEPTIONS
*       no_data_found       = 1
*       OTHERS              = 2
*              .
*S__Send Parts Plan Interface TO GCS
    CALL FUNCTION 'Z_MM_IF_OB_02_001'
      DESTINATION p_dest
      IMPORTING
        e_return              = e_return
      TABLES
        it_body               = it_21day
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3
        OTHERS                = 4.

    IF sy-subrc <> 0.
*S__ADD NEW 'EXCEPIONS'
      CASE sy-subrc.
        WHEN '1'.
          e_return-message = 'communication_failure'.
          e_return-type    = 'E'.
        WHEN '2'.
          e_return-message = 'system_failure'.
          e_return-type    = 'E'.
        WHEN '3'.
          e_return-message = 'resource_failure'.
          e_return-type    = 'E'.
        WHEN '4'.
          e_return-message = 'Others'.
          e_return-type    = 'E'.
      ENDCASE.

      LOOP AT it_21day.
        it_21day-tait_targ_rslt = e_return-type. "F->E CHANGE
        it_21day-tait_targ_d    = sy-datum.
        it_21day-tait_targ_t    = sy-uzeit.
        it_21day-tait_targ_desc = e_return-message.

        MODIFY it_21day INDEX sy-tabix
        TRANSPORTING tait_targ_rslt tait_targ_d tait_targ_t
                     tait_targ_desc.

        gv_return =  e_return-type.

        CALL METHOD zmmc_cl_if=>if_set_key(  ifkey = 'MMIF201_ECC_OB'
                                 modle = 'GCS'  " 'MM', 'SD', 'FI'
                                 centy = 'US'  	"
                                 dirct = 'O'  " 'O' : Outbound,
                                                " 'I' : INBOUND
                                 logox = ' '
                                 ttype = 'S'
                                 cparm = '7'
                               ).

        CALL METHOD zmmc_cl_if=>if_set_messg( type    = e_return-type
                                  id      = ' '    "gt_retmsg-id
                                  message = e_return-message
                                ).

        CALL METHOD zmmc_cl_if=>if_set_param( istat = gv_return
          ifp01 = it_21day-epart_no
          ifp02 = it_21day-edate
          ifp03 = it_21day-edmd_type
          ).
      ENDLOOP.

      CALL METHOD zmmc_cl_if=>if_save_data( ).

      MESSAGE s000 WITH text-008.
    ELSE.
      LOOP AT it_21day.
*S__CHANGE BY PAUL
*        IT_21DAY-TAIT_TARG_RSLT = 'S'.
        it_21day-tait_targ_rslt = e_return-type.
        it_21day-tait_targ_d    = sy-datum.
        it_21day-tait_targ_t    = sy-uzeit.
        it_21day-tait_targ_desc = e_return-message.

        MODIFY it_21day INDEX sy-tabix
        TRANSPORTING tait_targ_rslt tait_targ_d tait_targ_t
                     tait_targ_desc.

        gv_return =  e_return-type.

        CALL METHOD zmmc_cl_if=>if_set_key(  ifkey = 'MMIF201_ECC_OB'
                                 modle = 'GCS'  " 'MM', 'SD', 'FI'
                                 centy = 'US'  	"
                                 dirct = 'O'  " 'O' : Outbound,
                                                " 'I' : INBOUND
                                 logox = ' '
                                 ttype = 'S'
                                 cparm = '7'
                               ).

        CALL METHOD zmmc_cl_if=>if_set_messg( type    = e_return-type
                                      id      = ' '    "gt_retmsg-id
                                      message = e_return-message
                                    ).

        CALL METHOD zmmc_cl_if=>if_set_param( istat = gv_return
          ifp01 = it_21day-epart_no
          ifp02 = it_21day-edate
          ifp03 = it_21day-edmd_type
          ).

      ENDLOOP.

      CALL METHOD zmmc_cl_if=>if_save_data( ).

      DESCRIBE TABLE it_21day LINES gv_lines.
      MESSAGE s000 WITH gv_lines text-009.
    ENDIF.
    UPDATE ztmm_eai_demand FROM TABLE it_21day.
  ENDIF.    " IF NOT it_21day[] IS INITIAL

ENDFORM.                    " send_data_to_eai
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       Subroutine to show progress
*----------------------------------------------------------------------*
*      -->P_gv_text  text
*----------------------------------------------------------------------*
FORM show_progress USING    p_text.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE =
      text       = p_text.

ENDFORM.                    " show_progress
*&---------------------------------------------------------------------*
*&      Form  set_21days_data
*&---------------------------------------------------------------------*
*       Subroutine to distribute data day wise
*----------------------------------------------------------------------*
FORM set_21days_data.
  DATA: l_index LIKE sy-tabix.
  DATA: l_matnr LIKE mara-matnr.

  LOOP AT it_resb.
    CLEAR: it_21day.
*    IF IT_RESB-BDTER GT GV_LASTDATE.
*      CONTINUE.
*    ENDIF.
    READ TABLE it_day WITH KEY datum = it_resb-bdter
                      BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_21day WITH KEY epart_no = it_resb-matnr.

    IF sy-subrc EQ 0.
      MOVE: sy-tabix TO l_index.

      PERFORM append_quantity.

      MODIFY it_21day INDEX l_index.

    ELSE.
      it_21day-epart_no = it_resb-matnr.
      it_21day-edate = p_datum.
** changed by Furong on 03/10/10
      it_21day-vendor = it_resb-lifnr.
** end of change
      it_21day-edmd_type = 'D'.
      it_21day-tait_targ_d = sy-datum.
      it_21day-tait_targ_t = sy-uzeit.
      it_21day-tait_targ_rslt = ' '.
      it_21day-tait_targ_desc = ' '.
      it_21day-tait_event_c = 'I'.

      PERFORM append_quantity.
      APPEND it_21day.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " set_21days_data
*&---------------------------------------------------------------------*
*&      Form  append_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM append_quantity.
  DATA: l_quantity(50),
        l_day(2) TYPE n,
        l_bdmng  LIKE resb-bdmng.

  FIELD-SYMBOLS: <quantity>.

  READ TABLE it_day WITH KEY datum = it_resb-bdter BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE: it_resb-bdmng TO l_bdmng.
  ELSE.
    MOVE: 0             TO l_bdmng.
  ENDIF.

  l_day = it_day-seq.

  CONCATENATE 'IT_21DAY-ED' l_day '_DMD' INTO l_quantity.
  ASSIGN (l_quantity) TO <quantity>.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  <quantity> = <quantity> + l_bdmng.

ENDFORM.                    " append_quantity
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       Subroutine to update Z table
*----------------------------------------------------------------------*
FORM update_table.
  CLEAR gv_lines.

  DESCRIBE TABLE it_21day LINES gv_lines.
  INSERT ztmm_eai_demand FROM TABLE it_21day ACCEPTING DUPLICATE KEYS.

  IF sy-subrc = 0.
    MESSAGE s000 WITH gv_lines text-003.
    IF p_opt2 = 'X'.
      PERFORM display_alv_data.
    ENDIF.
  ELSE.
    MESSAGE s000 WITH text-004.
  ENDIF.

ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  display_alv_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_data.

  IF p_opt2 = 'X'.
    REFRESH gt_alv.
    CLEAR   gt_alv.

    LOOP AT it_21day.
      MOVE-CORRESPONDING it_21day TO gt_alv.

      CASE it_21day-tait_targ_rslt.
        WHEN space.
        WHEN 'S'.
          gt_alv-icon = icon_led_green.
        WHEN 'F'.
          gt_alv-icon = icon_led_red.
      ENDCASE.
      APPEND gt_alv.
      CLEAR  gt_alv.

    ENDLOOP.
  ENDIF.

  PERFORM set_layout.
  CLEAR gs_variant.

  gv_repid = sy-repid.
  gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

  PERFORM alv_fieldcat.

  PERFORM alv_grid_display.

ENDFORM.                    " display_alv_data
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM set_layout.
  CLEAR gs_layout.
  gs_layout-colwidth_optimize      = 'X'.
  gs_layout-box_fieldname          = 'CHKBOX'.
  gs_layout-coltab_fieldname       = 'TABCOLOR'.
ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM alv_fieldcat.
  DATA: l_cn(2)     TYPE n,
        l_colpos    TYPE i,
        l_datum(8)        ,
        l_datum1(8)       ,
        l_rqty(9)         ,
        l_text(25)  TYPE c.

  CLEAR:  gs_fieldcat,
          gt_fieldcat.
  REFRESH gt_fieldcat.

  PERFORM build_field_cat USING: '1' 'X'  'EPART_NO' 'Material'
                                                      15 'CHAR',
                                 '2' 'X'  'EDATE' 'Reqs Date'
                                                      10 'CHAR',
                                 '3' 'X'  'EDMD_TYPE'  'Demand Ident'
                                                      12 'CHAR',
** Changed by Firong on 03/12/10
                                 '4' 'X'  'VENDOR'  'Supplier'
                                                      6 'CHAR',
** end of change
                                 '26' ' '  'ICON'       'flg'
                                                       3 'ICON'.
** Changed by Firong on 03/12/10
  l_colpos = 4.
*   L_COLPOS = 3.
** end of change
  l_cn = '00'.
  SORT it_week.
  SORT it_day.
  DO 22 TIMES.
    CLEAR gs_fieldcat.
    l_colpos = l_colpos + 1.
    IF p_d21 = 'X'.
      READ TABLE it_day WITH KEY seq = l_cn.
    ELSEIF p_w21 = 'X'.
      READ TABLE it_week WITH KEY seq = l_cn.
    ELSEIF p_both = 'X'.
      READ TABLE it_day WITH KEY seq  = l_cn.
      READ TABLE it_week WITH KEY seq = l_cn.
    ENDIF.

    CONCATENATE 'ED' l_cn '_DMD' INTO l_rqty.
    IF p_d21 = 'X'.
      WRITE it_day-datum TO l_datum MM/DD/YY.
    ELSEIF p_w21 = 'X'.
      WRITE it_week-datum TO l_datum MM/DD/YY.
    ELSEIF p_both = 'X'.
      WRITE it_day-datum TO l_datum MM/DD/YY.
      WRITE it_week-datum TO l_datum1 MM/DD/YY.
      CONCATENATE l_datum '(D)/' l_datum1 '(W)' INTO l_text.
    ENDIF.

    gs_fieldcat-col_pos       = l_colpos.  " Column position
    gs_fieldcat-fieldname     = l_rqty.    " Field Name
    IF p_d21 = 'X' OR p_w21 = 'X'.
      gs_fieldcat-seltext_m     = l_datum.   " Column heading
    ELSEIF p_both = 'X'.
      gs_fieldcat-seltext_l     = l_text.    " Column heading
    ENDIF.
    gs_fieldcat-outputlen     = '10'.      " Column width
    gs_fieldcat-datatype      = 'NUMC'.    " Data type
    APPEND gs_fieldcat TO gt_fieldcat.

    CLEAR: l_rqty.
    l_cn = l_cn + 1.

  ENDDO.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_grid_display
*&---------------------------------------------------------------------*
*       Subroutine to display data in ALV Grid form
*----------------------------------------------------------------------*
FORM alv_grid_display.
  DATA: lv_save VALUE 'A'.
*  SORT gt_alv BY epart_no.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
*     i_callback_pf_status_set = ' '
*     i_callback_user_command  = ' '
      is_layout                = gs_layout
*     it_excluding             =
      it_fieldcat              = gt_fieldcat
*     it_special_groups        =
*     it_sort                  =
      i_save                   = lv_save
      is_variant               = gs_variant
*     it_events                =
    TABLES
      t_outtab                 = gt_alv
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " alv_grid_display
*&---------------------------------------------------------------------*
*&      Form  build_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1141   text
*      -->P_1142   text
*      -->P_1143   text
*      -->P_1144   text
*      -->P_15     text
*      -->P_1146   text
*----------------------------------------------------------------------*
FORM build_field_cat USING    col_pos key field_name
                              short_txt lenght datatype.
  gs_fieldcat-col_pos       = col_pos.
  gs_fieldcat-key           = key.
  gs_fieldcat-fieldname     = field_name.
  gs_fieldcat-seltext_m     = short_txt.   " Column heading
  gs_fieldcat-outputlen     = lenght.      " Column width
  gs_fieldcat-datatype      = datatype.    " Data type
*  gs_fieldcat-qfieldname    = &6.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.                    " build_field_cat
*&---------------------------------------------------------------------*
*&      Form  set_week
*&---------------------------------------------------------------------*
*       Subroutine to calculate weeks
*----------------------------------------------------------------------*
FORM set_week.
  DATA: l_date LIKE sy-datum  ,
        l_date_1 LIKE sy-datum,
        l_cn(2) TYPE n.

  CLEAR: it_week, it_week[].

  CLEAR gv_kalid.
*-reading working calendar
  PERFORM read_shop_calid  USING gv_kalid.

  l_date = p_datum .
  CALL FUNCTION 'HR_GBSSP_GET_WEEK_DATES'
    EXPORTING
      p_pdate       = l_date
    IMPORTING
      p_sunday      = l_date
*     P_SATURDAY    =
*     P_DAY_IN_WEEK =
*     P_WEEK_NO     =
    .
  l_date = l_date + 1.

  l_cn = '00'.

  DO 22 TIMES.
    it_week-seq = l_cn.
    l_date_1 = l_date.
    PERFORM read_working_date USING '+'  gv_kalid  l_date_1.
    l_date = l_date + 7.
    IF l_date_1 > l_date.
      l_date = l_date + 7.
    ELSE.
      IF l_date = l_date_1.
        l_date_1 = l_date_1 - 7.
      ENDIF.
    ENDIF.
    it_week-datum = l_date_1.
    APPEND it_week.
    CLEAR it_week.
    l_cn = l_cn + 1.
  ENDDO.

  l_date_1 = l_date.
  PERFORM read_working_date USING '+'  gv_kalid  l_date_1.
  l_date = l_date + 7.
  IF l_date_1 > l_date.
    l_date = l_date + 7.
  ELSE.
    IF l_date = l_date_1.
      l_date_1 = l_date_1 - 7.
    ENDIF.
  ENDIF.
  gv_lastweek = l_date_1.

ENDFORM.                    " set_week
*&---------------------------------------------------------------------*
*&      Form  set_21weeks_data
*&---------------------------------------------------------------------*
*       Subroutine to distribute data week wise
*----------------------------------------------------------------------*
FORM set_21weeks_data.
*  DATA: BEGIN OF IT_MATNR OCCURS 0,
*          MATNR LIKE MARA-MATNR   ,
*        END OF IT_MATNR           .
  DATA: l_week_f LIKE sy-datum    ,
        l_seq(2) TYPE n           ,
        l_idx    TYPE sy-index    ,
        l_text_21t(30) TYPE c     ,
        l_tmp LIKE ztmm_eai_demand-ed00_dmd,
        l_vendor LIKE it_resb1-lifnr.
  .
  DATA: l_week_00 LIKE sy-datum,
        l_week_01 LIKE sy-datum,
        l_week_02 LIKE sy-datum,
        l_week_03 LIKE sy-datum,
        l_week_04 LIKE sy-datum,
        l_week_05 LIKE sy-datum,
        l_week_06 LIKE sy-datum,
        l_week_07 LIKE sy-datum,
        l_week_08 LIKE sy-datum,
        l_week_09 LIKE sy-datum,
        l_week_10 LIKE sy-datum,
        l_week_11 LIKE sy-datum,
        l_week_12 LIKE sy-datum,
        l_week_13 LIKE sy-datum,
        l_week_14 LIKE sy-datum,
        l_week_15 LIKE sy-datum,
        l_week_16 LIKE sy-datum,
        l_week_17 LIKE sy-datum,
        l_week_18 LIKE sy-datum,
        l_week_19 LIKE sy-datum,
        l_week_20 LIKE sy-datum,
        l_week_21 LIKE sy-datum,
        l_week_22 LIKE sy-datum,
        l_week_23 LIKE sy-datum.

  FIELD-SYMBOLS : <fs01>, <fs-qty>.

*  LOOP AT IT_RESB1.
*    MOVE-CORRESPONDING IT_RESB1 TO IT_MATNR.
*    COLLECT IT_MATNR.
*  ENDLOOP.
*  SORT IT_MATNR.
*  SORT IT_WEEK.
*  L_SEQ = '00'.
*
*  LOOP AT IT_MATNR.
*    CLEAR IT_WEEK.
*    CLEAR L_IDX.
*    L_SEQ = '00'.
*    L_IDX = L_IDX + 1.
*    READ TABLE IT_WEEK INDEX L_IDX.
*    L_WEEK_F = IT_WEEK-DATUM.
*    L_IDX = L_IDX + 1.
*    READ TABLE IT_WEEK INDEX L_IDX.
*
*    DO 22 TIMES.
*      CLEAR L_TMP.
*      LOOP AT IT_RESB1 WHERE MATNR = IT_MATNR-MATNR AND
*                             BDTER >= L_WEEK_F      AND
*                             BDTER < IT_WEEK-DATUM.
*
*        CLEAR L_TEXT_21T.
*        CONCATENATE 'IT_21WEEK-ED' L_SEQ '_DMD' INTO L_TEXT_21T.
*        ASSIGN (L_TEXT_21T) TO <FS-QTY>.
*        L_TMP = L_TMP + IT_RESB1-BDMNG.
*        <FS-QTY> = L_TMP..
*      ENDLOOP.  " LOOP AT it_resb1 WHERE matnr = it_matnr-matnr.
*      UNASSIGN <FS-QTY>.
*      L_WEEK_F = IT_WEEK-DATUM.
*      L_IDX = L_IDX + 1.
*      IF L_IDX = 23.
*        IT_WEEK-DATUM = GV_LASTWEEK.
*        L_SEQ = L_SEQ + 1.
*      ELSEIF L_IDX > 23.
*        EXIT.
*      ELSE.
*        READ TABLE IT_WEEK INDEX L_IDX.
*        L_SEQ = L_SEQ + 1.
*      ENDIF.
*    ENDDO.
*
*    IT_21WEEK-EPART_NO = IT_RESB1-MATNR.
*    IT_21WEEK-EDATE = P_DATUM.
*    IT_21WEEK-EDMD_TYPE = 'W'.
*    IT_21WEEK-TAIT_TARG_D = SY-DATUM.
*    IT_21WEEK-TAIT_TARG_T = SY-UZEIT.
*    IT_21WEEK-TAIT_TARG_RSLT = ' '.
*    IT_21WEEK-TAIT_TARG_DESC = ' '.
*    IT_21WEEK-TAIT_EVENT_C = 'I'.
*    APPEND IT_21WEEK.
*  ENDLOOP.
*
** New
  LOOP AT it_week.
    CONCATENATE 'L_WEEK_' it_week-seq INTO l_text_21t.
    ASSIGN (l_text_21t) TO <fs01>.
    <fs01> = it_week-datum.
  ENDLOOP.
  LOOP AT it_resb1.
    AT NEW matnr.
      it_21week-epart_no = it_resb1-matnr.
    ENDAT.

    IF it_resb1-bdter >= l_week_00 AND it_resb1-bdter < l_week_01.
      l_seq = '00'.
    ELSEIF it_resb1-bdter >= l_week_01 AND it_resb1-bdter < l_week_02.
      l_seq = '01'.
    ELSEIF it_resb1-bdter >= l_week_02 AND it_resb1-bdter < l_week_03.
      l_seq = '02'.
    ELSEIF it_resb1-bdter >= l_week_03 AND it_resb1-bdter < l_week_04.
      l_seq = '03'.
    ELSEIF it_resb1-bdter >= l_week_04 AND it_resb1-bdter < l_week_05.
      l_seq = '04'.
    ELSEIF it_resb1-bdter >= l_week_05 AND it_resb1-bdter < l_week_06.
      l_seq = '05'.
    ELSEIF it_resb1-bdter >= l_week_06 AND it_resb1-bdter < l_week_07.
      l_seq = '06'.
    ELSEIF it_resb1-bdter >= l_week_07 AND it_resb1-bdter < l_week_08.
      l_seq = '07'.
    ELSEIF it_resb1-bdter >= l_week_08 AND it_resb1-bdter < l_week_09.
      l_seq = '08'.
    ELSEIF it_resb1-bdter >= l_week_09 AND it_resb1-bdter < l_week_10.
      l_seq = '09'.
    ELSEIF it_resb1-bdter >= l_week_10 AND it_resb1-bdter < l_week_11.
      l_seq = '10'.
    ELSEIF it_resb1-bdter >= l_week_11 AND it_resb1-bdter < l_week_12.
      l_seq = '11'.
    ELSEIF it_resb1-bdter >= l_week_12 AND it_resb1-bdter < l_week_13.
      l_seq = '12'.
    ELSEIF it_resb1-bdter >= l_week_13 AND it_resb1-bdter < l_week_14.
      l_seq = '13'.
    ELSEIF it_resb1-bdter >= l_week_14 AND it_resb1-bdter < l_week_15.
      l_seq = '14'.
    ELSEIF it_resb1-bdter >= l_week_15 AND it_resb1-bdter < l_week_16.
      l_seq = '15'.
    ELSEIF it_resb1-bdter >= l_week_16 AND it_resb1-bdter < l_week_17.
      l_seq = '16'.
    ELSEIF it_resb1-bdter >= l_week_17 AND it_resb1-bdter < l_week_18.
      l_seq = '17'.
    ELSEIF it_resb1-bdter >= l_week_18 AND it_resb1-bdter < l_week_19.
      l_seq = '18'.
    ELSEIF it_resb1-bdter >= l_week_19 AND it_resb1-bdter < l_week_20.
      l_seq = '19'.
    ELSEIF it_resb1-bdter >= l_week_20 AND it_resb1-bdter < l_week_21.
      l_seq = '20'.
    ELSEIF it_resb1-bdter >= l_week_21 AND it_resb1-bdter < gv_lastweek.
      l_seq = '21'.
*    ELSEIF IT_RESB1-BDTER >= L_WEEK_22 AND IT_RESB1-BDTER < L_WEEK_23.
*      L_SEQ = '22'.
*    ELSEIF IT_RESB1-BDTER >= L_WEEK_23 AND IT_RESB1-BDTER < GV_LASTWEEK
*.
*      L_SEQ = '23'.
    ELSE.
      CLEAR: l_seq.
      CONTINUE.
    ENDIF.

    CLEAR l_text_21t.
    CONCATENATE 'IT_21WEEK-ED' l_seq '_DMD' INTO l_text_21t.
    ASSIGN (l_text_21t) TO <fs-qty>.
    IF sy-subrc = 0.
      <fs-qty> = <fs-qty> + it_resb1-bdmng.
    ENDIF.
    l_vendor = it_resb1-lifnr.
    AT END OF matnr.
      it_21week-edate = p_datum.
      it_21week-edmd_type = 'W'.
      it_21week-vendor = l_vendor.
      it_21week-tait_targ_d = sy-datum.
      it_21week-tait_targ_t = sy-uzeit.
      it_21week-tait_targ_rslt = ' '.
      it_21week-tait_targ_desc = ' '.
      it_21week-tait_event_c = 'I'.
      APPEND it_21week.
      CLEAR: it_21week,it_21week, l_vendor.
    ENDAT.
  ENDLOOP.

** End

ENDFORM.                    " set_21weeks_data
*&---------------------------------------------------------------------*
*&      Form  get_source_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source_list USING p_keydt.

  CLEAR : it_vmat[], it_vmat.

  SELECT eord~matnr eord~lifnr INTO TABLE it_vmat
  FROM eord
  WHERE eord~vdatu <= p_keydt
  AND eord~bdatu >= p_keydt
  AND werks EQ p_werks
  AND matnr IN s_matnr.

ENDFORM.                    " get_source_list
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_materials.

  DATA $ix TYPE i.

  RANGES r_bwkey FOR mbew-bwkey.

  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k
    WHERE bukrs = p_bukrs
      AND bwkey EQ p_werks.

    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

  SELECT t1~mandt t1~matnr t1~raube t1~mtart
         t1~mstae t4~mmsta t4~dispo t4~ekgrp
         t2~bwkey matkl meins profl
         t4~sobsl
         bklas lbkum stprs verpr salk3 peinh maktx
         h~bukrs h~waers
        INTO CORRESPONDING FIELDS OF TABLE it_mat
         FROM mara AS t1
            INNER JOIN makt AS t3
               ON t3~matnr = t1~matnr
              AND t3~spras EQ sy-langu
            INNER JOIN marc AS t4
               ON t1~matnr = t4~matnr
            INNER JOIN mbew AS t2
               ON t2~matnr = t4~matnr
              AND t2~bwkey = t4~werks
            INNER JOIN t001k AS g
               ON g~bwkey = t2~bwkey
            INNER JOIN t001  AS h
               ON h~bukrs = g~bukrs
          WHERE t2~bwkey IN r_bwkey
            AND t4~werks EQ p_werks
            AND t4~beskz IN ('F', 'X')
            AND t1~mtart IN ('ROH', 'HALB')
            AND t1~matnr IN s_matnr
          ORDER BY t2~bwkey DESCENDING.

  IF sy-subrc NE 0.
    MESSAGE ID 'ZMMM' TYPE 'E' NUMBER '999' WITH text-001.
    EXIT.
  ENDIF.

* by ig.moon 11/10/2009 {
  IF p_split EQ space.
    __cls $it_mat.
    __cls it_collect.

    LOOP AT it_mat.
      it_collect-matnr = it_mat-matnr.
      it_collect-bwkey = it_mat-bwkey.
      it_collect-dispo = it_mat-dispo.
      it_collect-$peinh = it_mat-peinh.
      it_collect-$stprs = it_mat-stprs.
      it_collect-$verpr = it_mat-verpr.
      it_collect-lifnr = it_mat-lifnr.
      it_collect-ekgrp = it_mat-ekgrp.
      it_collect-bklas = it_mat-bklas.
      it_collect-mstae = it_mat-mstae.
      it_collect-mmsta = it_mat-mmsta.
      APPEND it_collect.
    ENDLOOP.

    SORT it_collect BY matnr bwkey DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_collect COMPARING matnr.
    SORT it_collect BY matnr.

    LOOP AT it_mat.
      $it_mat = it_mat.
      CLEAR : $it_mat-bwkey,$it_mat-sobsl,$it_mat-dispo.
      CLEAR : $it_mat-lifnr,$it_mat-ekgrp,$it_mat-bklas.
      CLEAR : $it_mat-mstae,$it_mat-mmsta.
      COLLECT $it_mat.
    ENDLOOP.
    __cls it_mat.
    it_mat[] = $it_mat[].

    LOOP AT it_mat.
      $ix = sy-tabix.

      READ TABLE it_collect WITH KEY matnr = it_mat-matnr
      BINARY SEARCH
      .
      IF sy-subrc EQ 0.
        it_mat-peinh    =  it_collect-$peinh .
        it_mat-stprs    =  it_collect-$stprs .
        it_mat-verpr    =  it_collect-$verpr .
        it_mat-lifnr    =  it_collect-lifnr .
        it_mat-ekgrp    =  it_collect-ekgrp .
        it_mat-dispo    =  it_collect-dispo.
        it_mat-bklas    =  it_collect-bklas.
        it_mat-mstae    =  it_collect-mstae.
        it_mat-mmsta    =  it_collect-mmsta.
        it_mat-bwkey    =  it_collect-bwkey.
      ENDIF.

      MODIFY it_mat INDEX $ix.
    ENDLOOP.

  ENDIF.
* }

  SORT it_mat BY matnr bwkey DESCENDING.

ENDFORM.                    " get_materials
*&---------------------------------------------------------------------*
*&      Form  get_9999_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_9999_stock.
*exclude 999 stock
  IF p_9999 = 'X'.
    SELECT * INTO TABLE lt_mard FROM mard
       FOR ALL ENTRIES IN it_mat
       WHERE matnr = it_mat-matnr
         AND werks = it_mat-bwkey
         AND lgort = '9999'.
  ENDIF.

*get 999 WM stock
*CC – Glovis CC storage types
*IP – HMMA lineside storage types
*PH – Engine expansion storage types

ENDFORM.                    " get_9999_stock
*&---------------------------------------------------------------------*
*&      Form  get_mrp_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mrp_materials.

* MRP
*  IF p_mrp = 'X'.
  IF p_w21 NE 'X' AND p_chk21 NE 'X'.
    SELECT  a~mandt
            a~dtart a~matnr a~plwrk a~plscn a~dtnum a~dsdat a~bdbkz
            a~slkz1 a~slkz2 a~slkz3 a~slkz4 a~slkz5 a~slkz6 a~slkz7
            a~slkz8 a~vrkz1 a~vrkz2 a~vrkz3 a~mtart a~meins a~disst
            a~beskz a~sobsl a~sobes a~wrk02 a~dismm a~disvf a~dispo
            a~pldis a~ekgrp a~mtwzt a~webaz a~beazt a~fixtr a~mfhkz
            a~disls a~losvf a~loskz a~peraz a~eisbe a~minbe a~hoebe
            a~bstmi a~bstma a~bstfx a~bstrf a~sum01 a~sum02 a~sum03
            a~sum04 a~sum05 a~negbs a~msgid a~msgar a~msgnr a~msgv1
            a~msgv2 a~msgv3 a~msgv4 a~disgr a~periv a~mrppp a~bdarf
            a~lfrhy a~rdprf a~berw1 a~berw2 a~kzaus a~ausdt a~nfmat
            a~ausz1 a~ausz2 a~ausz3 a~ausz4 a~ausz5 a~ausz6 a~ausz7
            a~ausz8 a~beada a~naukz a~sauft a~kzpromo a~shflg a~shzet
            a~fabkz a~mfxdt a~bskfl a~maabc a~cflag a~grrel a~rwpro
            a~shpro a~ahdis a~berw4

    INTO TABLE i_mdkp
                 FROM mdkp AS a
                 INNER JOIN marc AS b
                 ON  b~matnr EQ a~matnr
                 AND b~werks EQ a~plwrk
                 FOR ALL ENTRIES IN it_mat
                  WHERE a~dtart = mddisp
                    AND a~matnr = it_mat-matnr
                    AND a~plwrk = it_mat-bwkey
                    AND b~dismm NE 'ND'.
  ENDIF.

* LTP = Planned Order + Simulated Qty
*  IF p_ltp = 'X'.
  IF p_d21 NE 'X'.

    SELECT a~mandt
          a~dtart a~matnr a~plwrk a~plscn a~dtnum a~dsdat a~bdbkz
          a~slkz1 a~slkz2 a~slkz3 a~slkz4 a~slkz5 a~slkz6 a~slkz7
          a~slkz8 a~vrkz1 a~vrkz2 a~vrkz3 a~mtart a~meins a~disst
          a~beskz a~sobsl a~sobes a~wrk02 a~dismm a~disvf a~dispo
          a~pldis a~ekgrp a~mtwzt a~webaz a~beazt a~fixtr a~mfhkz
          a~disls a~losvf a~loskz a~peraz a~eisbe a~minbe a~hoebe
          a~bstmi a~bstma a~bstfx a~bstrf a~sum01 a~sum02 a~sum03
          a~sum04 a~sum05 a~negbs a~msgid a~msgar a~msgnr a~msgv1
          a~msgv2 a~msgv3 a~msgv4 a~disgr a~periv a~mrppp a~bdarf
          a~lfrhy a~rdprf a~berw1 a~berw2 a~kzaus a~ausdt a~nfmat
          a~ausz1 a~ausz2 a~ausz3 a~ausz4 a~ausz5 a~ausz6 a~ausz7
          a~ausz8 a~beada a~naukz a~sauft a~kzpromo a~shflg a~shzet
          a~fabkz a~mfxdt a~bskfl a~maabc a~cflag a~grrel a~rwpro
          a~shpro a~ahdis a~berw4
    APPENDING TABLE i_mdkp
                 FROM mdkp AS a
                 INNER JOIN marc AS b
                 ON  b~matnr EQ a~matnr
                 AND b~werks EQ a~plwrk
                 FOR ALL ENTRIES IN it_mat
                  WHERE a~dtart = lfplan
                    AND a~matnr = it_mat-matnr
                    AND a~plwrk = it_mat-bwkey   "IN r_bwkey
                    AND a~plscn = p_plscn
                    AND b~dismm NE 'ND'.
  ENDIF.

*  SORT i_mdkp BY matnr plwrk.
*  read table i_mdkp index 1.
*  gv_dsdat = i_mdkp-dsdat.


ENDFORM.                    " get_mrp_materials
*&---------------------------------------------------------------------*
*&      Form  get_mrp_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mrp_data USING p_keydt.

  DATA: $fdate_21d LIKE sy-datum,
        $ldate_21d LIKE sy-datum.

  DATA: l_date LIKE sy-datum.
  DATA: dtart LIKE mdkp-dtart.

  SORT it_week.

  READ TABLE it_week INDEX 1.
  $fdate_21d = it_week-datum.
  READ TABLE it_week INDEX 22.
  $ldate_21d = it_week-datum.

* { Paul change
  LOOP AT i_mdkp.

    READ TABLE it_vmat WITH KEY matnr = i_mdkp-matnr
    BINARY SEARCH.

    REFRESH i_mdtbx.
    IF i_mdkp-dtart = mddisp.

      CALL FUNCTION 'READ_MRP_LIST'
        EXPORTING
          idtnum = i_mdkp-dtnum
          icflag = i_mdkp-cflag
        TABLES
          mdtbx  = i_mdtbx. "rec/req.

      LOOP AT i_mdtbx WHERE dat00 BETWEEN p_datum AND gv_lastdate.
        CHECK i_mdtbx-plumi = '-'.
        CLEAR it_resb_tmp.

        it_resb_tmp-matnr = i_mdkp-matnr.
        it_resb_tmp-lifnr = it_vmat-lifnr.
        it_resb_tmp-bdmng = i_mdtbx-mng01.
        it_resb_tmp-bdter = i_mdtbx-dat00.

        it_resb_tmp-bdmng = it_resb_tmp-bdmng * -1.

        COLLECT it_resb_tmp.
      ENDLOOP.
    ELSE.
      CALL FUNCTION 'DISPOBELEG_LESEN'
        EXPORTING
          dtnum = i_mdkp-dtnum
          cflag = i_mdkp-cflag
          mandt = i_mdkp-mandt
        TABLES
          mdpsx = i_mdpsx. "rec/req.

      LOOP AT i_mdpsx WHERE dat00 BETWEEN $fdate_21d AND $ldate_21d.
        CHECK i_mdpsx-plumi = '-'.
        CLEAR it_resb_tmp.

        it_resb_tmp1-matnr = i_mdkp-matnr.
        it_resb_tmp1-lifnr = it_vmat-lifnr.
        it_resb_tmp1-bdmng = i_mdpsx-mng01.
        it_resb_tmp1-bdter = i_mdpsx-dat00.

        it_resb_tmp1-bdmng = it_resb_tmp1-bdmng * -1.
*      IF I_MDPSX-dat00 between P_DATUM and GV_LASTDATE.
*        MOVE-CORRESPONDING IT_RESB_TMP1 TO IT_RESB_TMP.
*        COLLECT IT_RESB_TMP.
*        CLEAR IT_RESB_TMP.
*      ENDIF.
        COLLECT it_resb_tmp1.
      ENDLOOP.
    ENDIF.
    CLEAR : it_vmat.
  ENDLOOP.


* { 08/11/11 Paul Comment
*  __cls i_mdpsx.
*
*  LOOP AT i_mdkp.
*    REFRESH i_mdtbx.
*    IF i_mdkp-dtart = mddisp.
*
*      CALL FUNCTION 'READ_MRP_LIST'
*           EXPORTING
*                idtnum = i_mdkp-dtnum
*                icflag = i_mdkp-cflag
*           TABLES
*                mdtbx  = i_mdtbx. "rec/req.
*    ELSE.
*    CALL FUNCTION 'DISPOBELEG_LESEN'
*         EXPORTING
*              dtnum = i_mdkp-dtnum
*              cflag = i_mdkp-cflag
*              mandt = i_mdkp-mandt
*         TABLES
*              mdpsx = i_mdpsx. "rec/req.
*    LOOP AT i_mdpsx.
*      MOVE-CORRESPONDING i_mdpsx TO i_mdtbx.
*      APPEND i_mdtbx.
*    ENDLOOP.

*    ENDIF.
*  endloop.
* }


*MRP  HMMA  MRP LTP
*AR	X	X	X
*BA	X	X	X
*BE	X	X	X
*KB	X	X
*LA	X	X	X
*LE	X	X	X
*SA X   X
*SB	X	X
*SM X   X
*U2	X	X
*U3 X   X
*WB	X	X	X

*i_mdtbx-DELKZ : MRP element
*AR	Dependent reservation
*BA	Purchase requisition
*BB	Subcontractor requirements of material provided
*BE	Order item schedule line
*BP	Gross requirements planning
*BR	Process order
*CH	Batch stock
*ER	End of replenishment lead time
*E1	Subcontracting purchasing
*FE	Production order
*FH	End of planning time fence
*IH	Maintenance order
*JI	JIT call
*IM	Actual goods receipt quantity
*IW	In plant (only relevant for IS Automotive)
*KB	Individual customer stock
*KD	Customer independent requirement (currently not used)
*KK	Consignment stock for customer (availability check)
*LA	Shipping notification
*LB	Storage location stock
*LC	Batch/storage location stock
*LE	SA schedule line
*LF	JIT delivery schedule
*LK	Stock with subcontractor
*LL	Forecast delivery schedule
*MB	Goods issue
*MR	Reservation
*MS	Direct production
*NE	Network order
*PA	Planned order
*PB	Project stock
*PP	Planned independent requirement
*PR	Forecast requirement
*QM	Inspection lot for quality management
*RP	Returns item
*RR	MRP requirement (only relevant for IS-Automotive)
*RU	Confirmation
*S2	Simulated requirement from availability check
*SA	Simulation order
*SB	Dependent requirement
*SF	Safety requirement
*SH	Safety stock
*SI	Simulation requirement
*SM	Sim. dependent reqmt
*SU	Total requirements
*TB	Transfer requirements WMS
*U1	Release order for a stock transfer order
*U2	Release order for a stock transfer requisition
*U3	Transfer requirement for simulation order
*U4	Release order for stock transport scheduling agreement
*UB	Unplanned requirement
*UL	Reservation in another plant
*UR	Stock transfer reservation
*VA	Request for quotation
*VB	Quotation
*VC	Order
*VE	SD scheduling agreement
*VF	SD scheduling agreement; external service agent
*VG	Contract
*VI	Delivery w/o charge
*VJ	Delivery
*VP	Planning
*VT	Returns (availability check)
*VW	External sales order
*WA	Goods issue
*WB	Plant stock
*WE	Goods receipt
*WH	End replenishment period
*DD	Effective-out date


ENDFORM.                    " get_mrp_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_PAST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_past_data.
* } 08/11/11 Paul Change
  DATA : lv_datum LIKE sy-datum.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = '7'
      months    = '00'
      signum    = '-'
      years     = '00'
    IMPORTING
      calc_date = lv_datum.

  DELETE FROM ztmm_eai_demand WHERE edate <= lv_datum.
  COMMIT WORK.
* {
ENDFORM.                    " DELETE_PAST_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_21DAYS_DATA_FROM_ZTMM_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_21days_data_from_ztmm_part.


  CLEAR : it_21day[],it_21day.
  CLEAR : i_ztmm_parts[],i_ztmm_parts.

  SELECT * INTO TABLE i_ztmm_parts
   FROM ztmm_parts_21day
   WHERE matnr IN s_matnr
      AND datum = p_datum.

  CHECK sy-subrc EQ 0.

  LOOP AT i_ztmm_parts.

    it_21day-epart_no = i_ztmm_parts-matnr.
    it_21day-edate = i_ztmm_parts-datum.
    it_21day-vendor = i_ztmm_parts-lifnr.
    it_21day-edmd_type = 'D'.
    it_21day-tait_targ_d = sy-datum.
    it_21day-tait_targ_t = sy-uzeit.
    it_21day-tait_targ_rslt = ' '.
    it_21day-tait_targ_desc = ' '.
    it_21day-tait_event_c = 'I'.

    it_21day-ed00_dmd = i_ztmm_parts-d01.
    it_21day-ed01_dmd = i_ztmm_parts-d02.
    it_21day-ed02_dmd = i_ztmm_parts-d03.
    it_21day-ed03_dmd = i_ztmm_parts-d04.
    it_21day-ed04_dmd = i_ztmm_parts-d05.
    it_21day-ed05_dmd = i_ztmm_parts-d06.
    it_21day-ed06_dmd = i_ztmm_parts-d07.
    it_21day-ed07_dmd = i_ztmm_parts-d08.
    it_21day-ed08_dmd = i_ztmm_parts-d09.
    it_21day-ed09_dmd = i_ztmm_parts-d10.
    it_21day-ed10_dmd = i_ztmm_parts-d11.
    it_21day-ed11_dmd = i_ztmm_parts-d12.
    it_21day-ed12_dmd = i_ztmm_parts-d13.
    it_21day-ed13_dmd = i_ztmm_parts-d14.
    it_21day-ed14_dmd = i_ztmm_parts-d15.
    it_21day-ed15_dmd = i_ztmm_parts-d16.
    it_21day-ed16_dmd = i_ztmm_parts-d17.
    it_21day-ed17_dmd = i_ztmm_parts-d18.
    it_21day-ed18_dmd = i_ztmm_parts-d19.
    it_21day-ed19_dmd = i_ztmm_parts-d20.
    it_21day-ed20_dmd = i_ztmm_parts-d21.

    COLLECT it_21day.

  ENDLOOP.


ENDFORM.                    " SET_21DAYS_DATA_FROM_ZTMM_PART

*----------------------------------------------------------------------*
FORM get_receive_data.

  DATA: BEGIN OF lt_temp_21day OCCURS 0,
        matnr LIKE marc-matnr,
        END OF lt_temp_21day.

  DATA : lv_lines LIKE sy-index.
  DATA : it_data  LIKE zsmm_eai_demand OCCURS 0 WITH HEADER LINE.
  DATA : l_tabix(2) TYPE n.

  LOOP AT it_21day.
    lt_temp_21day-matnr = it_21day-epart_no.
    COLLECT lt_temp_21day.
  ENDLOOP.


*     Sort it_21day[] by epart_no edate edmd_type

  SELECT werks a~matnr beskz meins vspvb
*    into corresponding fields of table it_mara
    INTO TABLE it_mara
    FROM mara AS a
    INNER JOIN marc AS b
    ON a~matnr = b~matnr
   FOR ALL ENTRIES IN lt_temp_21day
    WHERE a~matnr = lt_temp_21day-matnr
      AND werks = 'P001'.

  DESCRIBE TABLE it_mara  LINES lv_lines.

  PERFORM read_mrp_list TABLES it_mara USING lv_lines.
  PERFORM merge_data.

ENDFORM.                    "get_receive_data

*&---------------------------------------------------------------------*
*&      Form  SET_IT_JOB
*&---------------------------------------------------------------------*
FORM read_mrp_list TABLES   p_it_mara  USING p_lines.
  DATA: lv_start_flg VALUE 'X',
        lv_index(2)  TYPE  n,
        lv_lines LIKE sy-index,
        lv_count LIKE sy-index,
        lv_tabix TYPE i,
        lv_start TYPE i,
        lv_end   TYPE i,
        lv_mod   TYPE i.

  CLEAR : wp_available, wp_total, it_job[].

  DATA: lw_start_flg VALUE 'X',
        lw_index(2)  TYPE  n.

*-Check Work Process
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = group
    IMPORTING
      max_pbt_wps                    = wp_total
      free_pbt_wps                   = wp_available
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.

  IF sy-subrc <> 0.
    WRITE:/30 'Error: Paralle Server Check.'.
    EXIT.
  ENDIF.

  IF wp_available > p_count.
    wp_available = p_count.
  ENDIF.

*  lv_count = p_lines / wp_available + 1.
  lv_count = p_lines DIV wp_available.
  lv_mod   = p_lines MOD wp_available.
  IF lv_mod > 0.
    lv_count = lv_count + 1.
  ENDIF.

  lv_start = 1.
  DO wp_available TIMES.

    it_job-start  =  lv_start.
    it_job-end  =  sy-index * lv_count.
    IF wp_available = sy-index.
      IF p_lines  < it_job-end .
        it_job-end  = p_lines.
      ENDIF.
    ENDIF.

    READ TABLE p_it_mara  INTO wa_mara INDEX lv_start.
    IF sy-subrc = 0.
      it_job-matnr  = wa_mara-matnr.

      lv_start  = 1 +  it_job-end.
      it_job-jobcnt  = it_job-end  - it_job-start + 1.
      CONCATENATE 'ZMRP_LIST :' it_job-matnr INTO it_job-jobname.

      APPEND it_job. CLEAR it_job.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IF p_single EQ 'X'.
    PERFORM call_function_mrp_list_single.
  ELSE.
    PERFORM call_function_mrp_list.
  ENDIF.
ENDFORM.                    " READ_MRP_LIST.
*&---------------------------------------------------------------------*
*&      Form  CREATE_BATCH_JOB
*&---------------------------------------------------------------------*
FORM call_function_mrp_list.

  DATA: l_timeout TYPE i.
  LOOP AT it_job.
    taskname = it_job-jobname.
    DO.
      CALL FUNCTION 'Z_MM_READ_MRP_LIST_PARALLEL'
*      CALL FUNCTION 'Z_MM_READ_MRP_LIST_PARALLEL_T'
        STARTING NEW TASK it_job-jobname
        DESTINATION IN GROUP group
        PERFORMING return_info ON END OF TASK
        EXPORTING
*         IV_MAXTASK             =
*         IV_PACKSIZE            =
          i_start                = it_job-start
          i_end                  = it_job-end
        TABLES
          it_mara                = it_mara
          t_data                 = it_data
        EXCEPTIONS
          error_with_servergroup = 1
          error_with_rfc         = 2.

      CASE sy-subrc.
        WHEN 0.
          tasklist-taskname = it_job-jobname.
          APPEND tasklist.
          WRITE: /  'Started task: ', tasklist-taskname COLOR 2.
          snd_jobs = snd_jobs + 1.
          CLEAR: excp_flag.
          EXIT.
        WHEN 1 OR 2.
          excp_flag = 'X'.
        WHEN 3.
          IF excp_flag = space.
            excp_flag = 'X'.
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
          ELSE.
            WAIT UNTIL rcv_jobs >= snd_jobs UP TO '1.0' SECONDS.
          ENDIF.
          IF sy-subrc EQ 0.
            CLEAR excp_flag.
          ENDIF.
      ENDCASE.

*    CASE sy-subrc.
*      WHEN 0.
*        tasklist-taskname = it_job-jobname.
*
*        CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
*          IMPORTING
*            rfcdest = tasklist-rfcdest
*          EXCEPTIONS
*            OTHERS  = 1.
*        APPEND tasklist.
*        WRITE: /  'Started task: ', tasklist-taskname COLOR 2.
*        snd_jobs = snd_jobs + 1.
*
*      WHEN 1 OR 2.
*        WRITE msg.
*
*        CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
*          IMPORTING
*            rfcdest = tasklist-rfcdest
*          EXCEPTIONS
*            OTHERS  = 1.
*
*        DATA: lv_server LIKE pbtresourc-servername.
*
*        MOVE: tasklist-rfcdest TO lv_server.
*    ENDCASE.

    ENDDO.
  ENDLOOP.

  DO.
    IF rcv_jobs >= snd_jobs.
      EXIT.
    ELSE.
      IF l_timeout > 100.
        EXIT.
      ELSE.
        l_timeout = l_timeout + 1.
      ENDIF.
    ENDIF.
    WAIT UP TO 30 SECONDS.
  ENDDO.

  WRITE :/ 'snd_jobs : ', snd_jobs, 'rcv_jobs: ', rcv_jobs.

ENDFORM.                    " CALL_FUNCTION_MRP_LIST
*&---------------------------------------------------------------------*
*&      Form  RETURN_INFO
*&---------------------------------------------------------------------*
FORM return_info   USING taskname.
*  DATA:  info_rfcdest LIKE tasklist-rfcdest.
*
*  RECEIVE RESULTS FROM FUNCTION 'RFC_SYSTEM_INFO'
*    IMPORTING rfcsi_export = info
*    EXCEPTIONS
*      communication_failure = 1
*      system_failure        = 2.
*
*  rcv_jobs = rcv_jobs + 1.  "Receiving data
*  IF sy-subrc NE 0.
**    handle communication and system failure
*  ELSE.
*    READ TABLE tasklist WITH KEY taskname = taskname.
*    IF sy-subrc = 0.  "Register data
*      tasklist-rfchost = info-rfchost.
*      MODIFY tasklist INDEX sy-tabix.
*    ENDIF.
*  ENDIF.

  DATA: l_msgtxt(100).

  RECEIVE RESULTS FROM FUNCTION 'Z_MM_READ_MRP_LIST_PARALLEL'
  TABLES
    t_data                       = it_data
 EXCEPTIONS
   error_with_servergroup       = 1
   error_with_rfc               = 2
    communication_failure       = 3 MESSAGE l_msgtxt
    system_failure              = 4 MESSAGE l_msgtxt
    resource_failure            = 5
    OTHERS                      = 6 .

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH l_msgtxt.
  ENDIF.

  APPEND     LINES OF it_data TO tot_data[].

  rcv_jobs = rcv_jobs + 1.  "Receiving data
  IF rcv_jobs = snd_jobs.
  ENDIF.

ENDFORM.                    " RETURN_INFO
*
*&---------------------------------------------------------------------*
*&      Form  MERGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_data .
  DATA : l_tabix(2) TYPE n.
  DATA: l_index LIKE sy-tabix.
  DATA : r_field(30).
  DATA : d_field(30).

  FIELD-SYMBOLS : <fr_date>,
                  <to_date>,
                  <fr_receipt>,
                  <to_receipt>.

  SORT tot_data BY epart_no edmd_type.
  SORT it_mara BY matnr.
  LOOP AT it_21day.
    l_index = sy-tabix.
    READ TABLE tot_data WITH KEY epart_no = it_21day-epart_no
*                                pdate = it_21day-edate
                                edmd_type = it_21day-edmd_type
                                BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR: l_tabix.

      it_21day-plnt    = 'P'.
      it_21day-line    = tot_data-line.
      it_21day-pdate   = tot_data-pdate.
      it_21day-beskz   = tot_data-beskz.
      it_21day-meins   = tot_data-meins.
      it_21day-init_qty  = tot_data-init_qty.
      it_21day-ptype =  tot_data-ptype.

      WHILE l_tabix < 22.
        CONCATENATE 'TOT_DATA-DAT_D' l_tabix INTO d_field.
        ASSIGN (d_field) TO <fr_date>.
        CONCATENATE 'IT_21day-DAT_D' l_tabix INTO d_field.
        ASSIGN (d_field) TO <to_date>.
        <to_date>  = <fr_date>.

        CONCATENATE 'TOT_DATA-RET_D' l_tabix INTO r_field.
        ASSIGN (r_field) TO <fr_receipt>.
        CONCATENATE 'IT_21DAY-RET_D' l_tabix INTO r_field.
        ASSIGN (r_field) TO <to_receipt>.

        <to_receipt>  = <fr_receipt>.
        l_tabix = l_tabix + 1.
      ENDWHILE.

      MODIFY it_21day INDEX l_index.
    ELSE.
      READ TABLE it_mara WITH KEY matnr = it_21day-epart_no
                        BINARY SEARCH.
      IF sy-subrc = 0.
        CASE it_mara-vspvb+0(1).
          WHEN  'P'.
            it_21day-ptype = 'P'.
          WHEN  'B'.
            it_21day-ptype = 'B'.
          WHEN OTHERS.
            it_21day-ptype = 'T'.
        ENDCASE.

        it_21day-beskz = it_mara-beskz.
        it_21day-meins =  it_mara-meins.
      ENDIF.
      it_21day-plnt    = 'P'.
      it_21day-line    = '1'.

      MODIFY it_21day INDEX l_index.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " MERGE_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_MRP_LIST_SINGLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function_mrp_list_single .
  DATA: l_msgtxt(100).
  DATA: ls_cm61x LIKE cm61x.


  DATA: l_timeout TYPE i.
*  LOOP AT it_job.
*    CLEAR: l_msgtxt.
*    taskname = it_job-jobname.
*    CALL FUNCTION 'Z_MM_READ_MRP_LIST_PARALLEL'
*      EXPORTING
*        is_cm61x               = ls_cm61x
*        iv_svgrp               = 'X'
*        i_start                = it_job-start
*        i_end                  = it_job-end
*      TABLES
*        it_mara                = it_mara
*        t_data                 = it_data
*      EXCEPTIONS
**       communication_failure  = 3  message l_msgtxt
**       system_f=              =  4       =
**       message                =
**       l_msgtxt               =
*        error_with_servergroup = 1
*        error_with_rfc         = 2
*        resource_failure       = 5
*        OTHERS                 = 6.
*
*    IF sy-subrc NE 0.
*      MESSAGE e000(zz) WITH 'CALL FUNCTION ERROR'.
*    ENDIF.
*
*    APPEND     LINES OF it_data TO tot_data[].
*  ENDLOOP.

  DATA: l_start TYPE i,
        l_end   TYPE i.

  DO.
    l_start = l_end = sy-index.

    READ TABLE it_mara INDEX l_start.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    WRITE:/ l_start, it_mara-matnr.

    CALL FUNCTION 'Z_MM_READ_MRP_LIST_PARALLEL'
      EXPORTING
        is_cm61x               = ls_cm61x
        iv_svgrp               = 'X'
        i_start                = l_start
        i_end                  = l_end
      TABLES
        it_mara                = it_mara
        t_data                 = it_data
      EXCEPTIONS
*       communication_failure  = 3  message l_msgtxt
*       4                      =  system_f=
*       message                =
*       l_msgtxt               =
        error_with_servergroup = 1
        error_with_rfc         = 2
        resource_failure       = 5
        OTHERS                 = 6.

    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'CALL FUNCTION ERROR'.
    ENDIF.

    APPEND     LINES OF it_data TO tot_data[].

  ENDDO.


ENDFORM.                    " CALL_FUNCTION_MRP_LIST_SINGLE
