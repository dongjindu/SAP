************************************************************************
* Program Name      : ZIPP101U_PIR_MANAGEMENT
* Author            : JongOh, Kim
* Creation Date     : 2003.09.04.
* Specifications By : JongOh, Kim
* Pattern           : 5.1.4.3
* Development Request No : UD1K901950
* Addl Documentation:
* Description       : Short/Long Term PIR Management (BAPI)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/18/05   Furong                    group Sequence + MRP sum by week
*                                      seq-sum date change from running
*                                      to reschedule date *
*                                      (ztpp_input_plan)
* 06.20.2014 Victor                    add e-mail Error notice function
************************************************************************
REPORT zipp101u_pir_management NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID zmpp.

TYPE-POOLS m60vt .
TYPES BEGIN OF ty_total.
        INCLUDE STRUCTURE rm60plvp.
TYPES:  status TYPE c,
      END OF ty_total.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : t001w,              "Plants/Branches
         ztpp_wosum,         " Work Order Summary
         ztpp_pmt07jb_c,     "Short Term Plan
         ztpp_pmt07jb_d.     "Long Term Plan

TABLES : bapisitemr, "Communication fields:indep. reqmts item data table
         cm60r,      "Common work area for planned indep. req functions
         t371f,     "IB: Object Types for User (Owner/Observer)
         ibinown.   "IB: Owner of an IBase instance

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA it_pmt07jb_c  LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE.

*-----> HEADER
DATA : BEGIN OF it_head     OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr.   "REQUIREMENT PLAN No
DATA : END OF it_head.

*-----> HEADER MATNR
DATA : BEGIN OF it_headmatnr OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,  "MATERIAL No
         pver    LIKE  ztpp_pmt07jb_c-pver.  "PROD VERSION
DATA : END OF it_headmatnr.

*-----> HEADER(DAILY ITEM)
DATA : BEGIN OF it_headitem OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "MATERIAL No
         pdatu   LIKE  ztpp_pmt07jb_c-pdatu,   "DATE
         prgrs   LIKE  ztpp_pmt07jb_c-prgrs,   "Date type
         plnmg   LIKE  ztpp_pmt07jb_c-plnmg,   "QTY
         pver    LIKE  ztpp_pmt07jb_c-pver.    "PROD VERSION
DATA : END OF it_headitem.

*-----> ITEM LINE(COLOR ITEM)
DATA : BEGIN OF it_item OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "MATERIAL No
         pdatu   LIKE  ztpp_pmt07jb_c-pdatu,   "DATE
         cogub   LIKE  ztpp_pmt07jb_c-cogub,   "EXT/INT GUBUN
         inexc   LIKE  ztpp_pmt07jb_c-inexc,   "INT-COLOR
         pver    LIKE  ztpp_pmt07jb_c-pver,    "PROD VERSION
         prgrs   LIKE  ztpp_pmt07jb_c-prgrs,   "DATE TYPE
         plnmg   LIKE  ztpp_pmt07jb_c-plnmg.   "QTY

DATA : END OF it_item.

*-----> ERROR TABLE
DATA : BEGIN OF it_error OCCURS 0,
         wo_ser  LIKE ztpp_wosum-wo_ser,
         nation  LIKE ztpp_wosum-nation,
         dealer  LIKE ztpp_wosum-dealer,
         extc  LIKE ztpp_wosum-extc,
         intc  LIKE ztpp_wosum-intc,
         modqty  LIKE ztpp_wosum-modqty,
         seqqty  LIKE ztpp_wosum-seqqty,
         planqty LIKE ztpp_wosum-planqty,
         forecastqty  LIKE ztpp_wosum-forecastqty,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "FSC
         msgty   LIKE  sy-msgty,               "STATUS
         id      LIKE  bapireturn1-id,         "Message class
         msgno   LIKE  sy-msgno,
*         msg     LIKE  cfgnl-msglin.           "MESSAGE
         msg     LIKE  bapireturn-message.
DATA : END OF it_error.

*-----> SUCCESS TABLE
DATA : BEGIN OF it_success OCCURS 0,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr.  "REQUIREMENT PLAN No
DATA : END OF it_success.

*----->
DATA : it_long  LIKE TABLE OF it_item WITH HEADER LINE,
       it_pbim  LIKE TABLE OF pbim    WITH HEADER LINE,
*       it_ausp  LIKE TABLE OF ausp    WITH HEADER LINE,
       it_vals  LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_pbed  LIKE TABLE OF pbed    WITH HEADER LINE.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_versb_nb       LIKE  pbim-versb,  "PIR VERSION
       wa_line_ix        LIKE  sy-tabix,
       wa_success_ix     LIKE  sy-tabix,    "COUNT OF SUCCESS FOR PBDNR
       wa_error_ix       LIKE  sy-tabix,    "COUNT OF ERROR FOR LINE
       wa_percentage_pd  TYPE  p       ,
       wa_fabkl          LIKE  t001w-fabkl,
       wa_meins          LIKE  mara-meins.

RANGES : r_pdatu  FOR  ztpp_pmt07jb_c-pdatu.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BAPI)
*----------------------------------------------------------------------*
*-----> BAPI INPUT/OUTPUT TABLE
DATA : it_bapisshdin   LIKE TABLE OF bapisshdin  WITH HEADER LINE,
       it_bapischarr   LIKE TABLE OF bapischarr  WITH HEADER LINE,
       it_bapireturn   LIKE TABLE OF bapireturn1 WITH HEADER LINE.

DATA : BEGIN OF it_color OCCURS 0.
        INCLUDE STRUCTURE rm60cuvt.
DATA : vtnam    LIKE  conf_out-atnam.
DATA : END OF it_color.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BAPI)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: wa_datum             LIKE sy-datum,
      wa_flag              TYPE c       ,
      wa_active(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')

DATA : l_error_chk(1).

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark   VALUE 'X',
           c_date   VALUE 'D',         "DATE TYPE
           c_mode   VALUE 'N',         "BDC MODE
           c_reqty  LIKE   t459u-bedae  VALUE 'VSE'.    "REQ TYPE

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_sum        AS CHECKBOX DEFAULT 'X'.
*---Start
*Requested by hur , changed by wskim, on 2004.10.20
PARAMETERS : p_plan   RADIOBUTTON GROUP rg1 DEFAULT 'X',
             p_conf  RADIOBUTTON GROUP rg1.
*---End
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS : p_werks      LIKE   t001w-werks DEFAULT 'P001'
                                             OBLIGATORY MEMORY ID wrk.

SELECTION-SCREEN SKIP 1.
PARAMETERS : ra_short   RADIOBUTTON GROUP ra  DEFAULT 'X',
             ra_long    RADIOBUTTON GROUP ra.
SELECTION-SCREEN END OF BLOCK b2.

PARAMETERS:  p_rver	LIKE somlreci1-receiver OBLIGATORY
                                   DEFAULT 'PP_PIR'.

SELECTION-SCREEN END OF BLOCK b0.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM at_selection-screen.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM data_arrange  .   " summarize the pmt07jb_c table
  PERFORM plan_recalc   .   " re-calculation of plan qty and forecast
  " qty of ztpp_wosum and WO master based on
  " pmt07jb_a
  PERFORM set_parameters.
  PERFORM excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM list_process.
  PERFORM send_email.

*&---------------------------------------------------------------------*
*&      Form  DATA_ARRANGE
*&---------------------------------------------------------------------*
FORM data_arrange .
  CHECK ra_short = 'X'.
  SUBMIT zapp808r_pir_summarize  AND RETURN
         WITH p_flg = p_sum .
ENDFORM.                    " DATA_ARRANGE

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM progress_indicator USING  p_text.
  IF wa_line_ix <> 0.
    wa_percentage_pd = ( sy-tabix / wa_line_ix ) * 100.
  ENDIF.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = wa_percentage_pd
      text       = p_text.

ENDFORM.                    " PROGRESS_INDICATOR

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM at_selection-screen.
  SELECT SINGLE *
                FROM t001w
                WHERE werks EQ p_werks.

  IF sy-subrc NE 0.
*    GET CURSOR FIELD 'P_WERKS'.
    MESSAGE e002 WITH p_werks text-201 .
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  PERFORM initialization.

*-----> SOURCE DATA
  PERFORM set_mode.
  PERFORM select_pmt07jb.

*-----> Delete Past PIR
  PERFORM delete_pir.

*-----> GENERATE BAPI FORMAT
  PERFORM gathering_data.

*----> BAPI LOGIC for PIR Management
  PERFORM bapi_execution.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
  DATA : l_line_ix   LIKE   sy-tabix,
         l_success_ix    LIKE   sy-tabix,
         l_line          LIKE   sy-tabix.

  DESCRIBE TABLE it_item  LINES l_line_ix.
  IF l_line_ix EQ 0.
    CASE c_mark.
      WHEN ra_short.
        WRITE :/ text-311.
      WHEN ra_long.
        WRITE :/ text-312.
    ENDCASE.
  ELSE.
    WRITE :/ text-313 ,
             wa_success_ix COLOR COL_POSITIVE.
    SKIP 2.

    LOOP AT it_error.
      AT FIRST.
        WRITE :/ '********** BEGIN OF ERROR Detail List ***********'.
      ENDAT.
      l_line = sy-tabix MOD 2.
      IF l_line EQ 1.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
      WRITE :/ it_error-pbdnr COLOR COL_KEY,
               it_error-matnr COLOR COL_KEY,
               it_error-msgty COLOR COL_NEGATIVE,
               it_error-msg   COLOR COL_NORMAL.
      AT LAST.
        FORMAT RESET INTENSIFIED ON.
        WRITE :/ '********** END OF ERROR Detail List ***********'.
      ENDAT.
    ENDLOOP.
*    ENDIF.
  ENDIF.

  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(End)' ,
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM set_mode.
*----> Factory Calendar key
  SELECT SINGLE fabkl
               INTO wa_fabkl
               FROM t001w
               WHERE werks EQ p_werks.
ENDFORM.                    " SET_MODE

*&---------------------------------------------------------------------*
*&      Form  SELECT_PMT07JB
*&---------------------------------------------------------------------*
FORM select_pmt07jb.
  DATA: wa_item  LIKE TABLE OF it_item      WITH HEADER LINE,
        l_monday LIKE sy-datum,
        l_monday_n LIKE sy-datum,
        l_monday_nn LIKE sy-datum,
        l_first LIKE sy-datum,
        l_facid LIKE tfacs-ident.

  CLEAR : it_head,      it_head[],
          it_headmatnr, it_headmatnr[],
          it_headitem,  it_headitem[],
          it_item,      it_item[],
          r_pdatu,      r_pdatu[].


  CASE c_mark.
    WHEN ra_short.
*----> SET PIR VERSION for Short Term PIR
      wa_versb_nb = '00'.      "Short Term Plan
      wa_active   = 'X'.

      r_pdatu-sign   = 'I'.
      r_pdatu-option = 'BT'.
      r_pdatu-low    = sy-datum .
      r_pdatu-high   = '99991231'.
      APPEND r_pdatu.

      SELECT *
             INTO CORRESPONDING FIELDS OF TABLE it_item
             FROM ztpp_pmt07jb_c
             WHERE werks EQ p_werks     "PLANT
               AND pdatu IN r_pdatu.     "SEQ DATE
*  requested by MY HUR, changed by chris
      PERFORM version_check_mrp.
*  end of add on 05/24/2005


    WHEN ra_long.
*----> SET PIR VERSION for Long Term PIR
      wa_versb_nb = '99'.      "Long Term Plan
      CLEAR wa_active.

      PERFORM summarize_data_vehicle.  " get sequency data

** added by Furong on 09/16/2005
      SELECT *
             APPENDING CORRESPONDING FIELDS OF TABLE it_item
             FROM ztpp_pmt07jb_c
             WHERE werks EQ p_werks.     "PLANT
*               AND pbdnr EQ 'MRP_SUM'.
*** FOR TESTING
*      DATA: W_SUM LIKE ztpp_pmt07jb_d-PLNMG.
*      LOOP AT IT_ITEM.
*         W_SUM = W_SUM + IT_ITEM-PLNMG.
*      ENDLOOP.
*      WRITE: 'BEFORE:', W_SUM.
*** END OF TEST
      SORT it_item BY pdatu.
      READ TABLE it_item INDEX 1.
      l_first = it_item-pdatu.


      SELECT SINGLE fabkl INTO l_facid
        FROM t001w
        WHERE werks = 'P001'.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_first
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_first
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date   = l_first
        IMPORTING
          monday = l_monday
          sunday = l_monday_n.

      l_monday_n = l_monday_n + 1.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_monday_n
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_monday_n
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date   = l_monday_n
        IMPORTING
          sunday = l_monday_nn.

      l_monday_nn = l_monday_nn + 1.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_monday_nn
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_monday_nn
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.














      l_monday = l_first + 7.
*      L_MONDAY_N = L_MONDAY + 7.
*      L_MONDAY_NN = L_MONDAY_N + 7.

      CLEAR: wa_item, wa_item[].

      CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
        EXPORTING
          date   = l_monday
        IMPORTING
*         WEEK   =
          monday = l_monday
*         SUNDAY =
        .
      l_monday_n = l_monday + 7.
      l_monday_nn = l_monday_n + 7.

      SELECT SINGLE fabkl INTO l_facid
        FROM t001w
        WHERE werks = 'P001'.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_first
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_first
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.


      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_monday
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_monday
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_monday_n
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_monday_n
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
        EXPORTING
          correct_option               = '+'
          date                         = l_monday_nn
          factory_calendar_id          = l_facid
        IMPORTING
          date                         = l_monday_nn
        EXCEPTIONS
          calendar_buffer_not_loadable = 1
          correct_option_invalid       = 2
          date_after_range             = 3
          date_before_range            = 4
          date_invalid                 = 5
          factory_calendar_not_found   = 6
          OTHERS                       = 7.

      LOOP AT it_item.
        wa_item = it_item.
        wa_item-prgrs = '2'.
        wa_item-pbdnr = 'MRP_SUM'.

** Changed by Furong on 10/29/08

*        IF WA_ITEM-PDATU < L_MONDAY.
*          WA_ITEM-PDATU = L_FIRST.
*        ELSEIF  WA_ITEM-PDATU >= L_MONDAY_N.
*          WA_ITEM-PDATU = L_MONDAY_N.
*        ELSE.
*          WA_ITEM-PDATU = L_MONDAY.
*        ENDIF.

        IF wa_item-pdatu < l_monday.
          wa_item-pdatu = l_first.
        ELSE.
          IF wa_item-pdatu >= l_monday
             AND wa_item-pdatu < l_monday_n.
            wa_item-pdatu = l_monday.
          ELSE.
            IF wa_item-pdatu >= l_monday_n
             AND wa_item-pdatu < l_monday_nn.
              wa_item-pdatu = l_monday_n.
            ELSE.
              wa_item-pdatu = l_monday_nn.
            ENDIF.
          ENDIF.
        ENDIF.
** End of change on 10/29/08

        COLLECT wa_item.
        CLEAR: wa_item, it_item.
      ENDLOOP.
      CLEAR: it_item, it_item[].
      it_item[] = wa_item[].
      CLEAR: wa_item, wa_item[].

*** FOR TESTING
*      CLEAR: W_SUM.
*      LOOP AT IT_ITEM.
*         W_SUM = W_SUM + IT_ITEM-PLNMG.
*      ENDLOOP.
*      WRITE: 'BEFORE:', W_SUM.
*** END OF TEST

** end of addition

      SELECT *
             APPENDING CORRESPONDING FIELDS OF TABLE it_item
             FROM ztpp_pmt07jb_d
             WHERE werks EQ p_werks     "PLANT
               AND pbdnr EQ 'LTP_SUM'.
*  requested by MY HUR, changed by chris
      PERFORM version_check_mrp.
*  end of add on 05/24/2005

*  changed on 08/28/06
*      SORT  IT_ITEM BY COGUB MATNR PVER INEXC.
      DELETE FROM ztpp_mrp_sum WHERE werks <> ' '.
      INSERT ztpp_mrp_sum FROM TABLE it_item.
      IF sy-subrc = 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.

ENDFORM.                    " SELECT_PMT07JB

*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM gathering_data.
  DATA l_tabix_ix   LIKE   sy-tabix.

  DESCRIBE TABLE it_item LINES wa_line_ix.
  LOOP AT it_item.
    l_tabix_ix = sy-tabix.
*    PERFORM PROGRESS_INDICATOR USING  'GENERATE BDC FORMAT DATA....'.
    IF it_item-cogub EQ 'I'.
      MOVE-CORRESPONDING it_item TO it_head.
      MOVE-CORRESPONDING it_item TO it_headmatnr.
      MOVE-CORRESPONDING it_item TO it_headitem.
      MOVE : wa_versb_nb  TO  it_head-versb,
             wa_versb_nb  TO  it_headmatnr-versb,
             wa_versb_nb  TO  it_headitem-versb.

      COLLECT : it_head,
                it_headmatnr,
                it_headitem.
      CLEAR : it_head, it_headmatnr, it_headitem.
    ENDIF.
    MOVE wa_versb_nb  TO  it_item-versb.
    MODIFY it_item  INDEX  l_tabix_ix.
  ENDLOOP.

  SORT it_head       BY pbdnr.
  SORT it_headmatnr  BY pbdnr matnr.
  SORT it_headitem   BY pbdnr matnr pdatu.
  SORT it_item       BY pbdnr matnr pdatu cogub inexc.
ENDFORM.                    " GATHERING_DATA

*&---------------------------------------------------------------------*
*&      Form  CHECK_COLOR_FOR_MATNR
*&---------------------------------------------------------------------*
FORM check_color_for_matnr.

  DATA: wa_objek    LIKE    inob-objek.
  DATA: wa_values   TYPE    m60vt_profil.

  DATA: it_total    TYPE  ty_total OCCURS 0 WITH HEADER LINE.
  DATA: it_return   LIKE  TABLE OF rm60cuvt WITH HEADER LINE.
  DATA: it_phwa     LIKE  tphvp.
  DATA: it_plwa     LIKE  tplvp.
  DATA: it_pswa     LIKE  tpsvp.

  DATA: BEGIN OF maint_char_rel OCCURS 0.
          INCLUDE STRUCTURE rm60rel.
  DATA: END OF   maint_char_rel.

  DATA: BEGIN OF maint_profil OCCURS   0.
          INCLUDE STRUCTURE rm60phvp.
  DATA:   END OF maint_profil.

  DATA: l_pl_rel    TYPE  tpsvp-pl_rel.

*  CLEAR: WA_VALUES.
  CLEAR : wa_objek.
  wa_objek = it_headmatnr-matnr.
  CALL FUNCTION 'M60V_PROFIL_FOR_PLAN'
       EXPORTING
            objekt      =   wa_objek  "WORK_INOB-ROBJEK
*           PROFILID    =
            i_pl_rel    = ' '
            buffer_free = 'X'
*            KEY_DATE    = SY-DATUM
       IMPORTING
            exp_value   = wa_values
*      tables
*           tab_phvp    = tplvp_value
       EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
  IF sy-subrc <> 0.
    l_error_chk = 'X'.  "05.01.2014 Victor

    it_error-pbdnr  =  it_headmatnr-pbdnr.
    it_error-matnr  =  wa_objek.
    it_error-msgty  =  sy-msgty.
    it_error-msg    = 'Planning Profile does not exist'.
    APPEND it_error.
    CLEAR it_error.
    EXIT.
  ENDIF.

  LOOP AT wa_values-headr INTO it_phwa.
    MOVE-CORRESPONDING it_phwa TO maint_profil.
    MOVE 'X' TO maint_profil-dbvkz.
    APPEND maint_profil.
  ENDLOOP.

  LOOP AT wa_values-group INTO it_plwa.
    MOVE-CORRESPONDING it_plwa TO it_total.
    IF NOT it_plwa-lkenz IS INITIAL.
      MOVE 'D' TO it_total-updkz.
    ELSE.
      MOVE space TO it_total-updkz.
    ENDIF.
    MOVE 'X' TO it_total-dbvkz.
    APPEND it_total.
  ENDLOOP.

  REFRESH: it_color, it_return.
  LOOP AT it_total.
    REFRESH : it_return.
    CALL FUNCTION 'M60V_COMBINATION_DISPLAY'
      EXPORTING
        table_line   = '00000'
        table_number = it_total-clint
      TABLES
        tab_var      = it_return.
    LOOP AT it_return.
      CLEAR l_pl_rel.
      SELECT SINGLE pl_rel
                    INTO l_pl_rel
                    FROM tpsvp
                    WHERE profilid   EQ it_total-profilid
                      AND phcounter  EQ it_total-phcounter
                      AND clint      EQ it_total-clint
                      AND lnpos      EQ it_return-slnid.
      IF sy-subrc EQ 0 AND l_pl_rel EQ c_mark.
        MOVE-CORRESPONDING it_return  TO  it_color.
        CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
          EXPORTING
            input  = it_color-atinn
          IMPORTING
            output = it_color-vtnam.

        APPEND it_color.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

**-----> COLOREXT COLORINT COLOR_MI COLOR OPT1...
*  SORT IT_COLOR BY VTNAM VALC.
*
*  CLEAR : IT_COLOREXT, IT_COLOREXT[],
*          IT_COLORINT, IT_COLORINT[].
*  LOOP AT IT_COLOR.
*    CASE IT_COLOR-VTNAM.
*      WHEN 'COLOREXT'.
*        IT_COLOREXT-EXTC = IT_COLOR-VALC+9(3).
*        APPEND IT_COLOREXT.
*      WHEN 'COLORINT'.
*        IT_COLORINT-INTC = IT_COLOR-VALC+9(3).
*        APPEND IT_COLORINT.
*    ENDCASE.
*  ENDLOOP.
*  SORT : IT_COLOREXT, IT_COLORINT.
ENDFORM.                    " CHECK_COLOR_FOR_MATNR
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  CASE c_mark.
    WHEN ra_long.
      WRITE :/ text-301.
    WHEN ra_short.
      WRITE :/ text-302.
  ENDCASE.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  BAPI_EXECUTION
*&---------------------------------------------------------------------*
FORM bapi_execution.
  CLEAR : it_success, it_success[].
*          it_error,   it_error[].
  CLEAR : wa_success_ix, wa_error_ix.

  LOOP AT it_headmatnr.
    PERFORM generate_bapi_data.
  ENDLOOP.
ENDFORM.                    " BAPI_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
FORM generate_bapi_data.

  CLEAR : bapisitemr, cm60r, wa_meins,
          it_bapisshdin, it_bapisshdin[],
          it_bapischarr, it_bapischarr[],
          it_bapireturn, it_bapireturn[].

*----> GENERATE BAPISITEMR
  bapisitemr-material   = it_headmatnr-matnr. "FSC
  bapisitemr-plant      = it_headmatnr-werks. "PLANT
  bapisitemr-requ_type  = c_reqty.            "VSE
  bapisitemr-version    = it_headmatnr-versb. "VERSION
  bapisitemr-vers_activ = wa_active.          "ACTIVE Yes/No
  bapisitemr-req_number = it_headmatnr-pbdnr. "Req plan No

*-----> UNIT & Check Material Master
  SELECT SINGLE meins
                INTO wa_meins
                FROM mara
                WHERE matnr EQ it_headmatnr-matnr .
***********       AND MTART EQ 'FERT'.
  IF sy-subrc NE 0.
    wa_error_ix = wa_error_ix + 1.
    MOVE-CORRESPONDING it_headmatnr TO it_error.
    MOVE 'E'      TO  it_error-msgty.
    MOVE text-303 TO  it_error-msg.
    APPEND it_error.
    CLEAR it_error.
  ELSE.
*-----> CHECK COLOR

    CLEAR : l_error_chk. "05.01.2014 Victor
    PERFORM check_color_for_matnr.

    IF l_error_chk = 'X'.
      EXIT.
    ENDIF.

    IF it_color[] IS INITIAL.
      wa_error_ix = wa_error_ix + 1.
      MOVE-CORRESPONDING it_headmatnr TO it_error.
      MOVE 'E'      TO  it_error-msgty.
      MOVE text-304 TO  it_error-msg.
      APPEND it_error.
      CLEAR it_error.
    ELSE.
*----> GENERATE SCHEDULE LINE & CHARACTERISTICS
      PERFORM generate_schedule_char.

*-----> Delete Inputed Material Number at past
      SORT it_bapisshdin BY date_type req_date.
      SORT it_bapischarr BY requ_date int_char char_value.
      DATA l_bdzei LIKE  pbim-bdzei.
      CLEAR l_bdzei.
      SELECT SINGLE a~bdzei
                   INTO l_bdzei
                   FROM pbim AS a INNER JOIN pbed AS b
                     ON a~bdzei EQ b~bdzei
                   WHERE a~werks EQ it_headmatnr-werks  "PLANT
                     AND a~matnr EQ it_headmatnr-matnr  "Material
                     AND a~bedae EQ c_reqty           "REQUIREMENT TYPE
                     AND a~versb EQ it_headmatnr-versb "VERSION WA_VERSB
                     AND a~pbdnr EQ it_headmatnr-pbdnr. "REQ. Plan No

      IF sy-subrc EQ 0.

        CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
          EXPORTING
            material                 = bapisitemr-material
            plant                    = bapisitemr-plant
            requirementstype         = bapisitemr-requ_type
            version                  = bapisitemr-version
            reqmtsplannumber         = bapisitemr-req_number
            vers_activ               = bapisitemr-vers_activ
          TABLES
            requirements_schedule_in = it_bapisshdin
            requirements_char_in     = it_bapischarr
            return                   = it_bapireturn.

      ELSE.
        CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
          EXPORTING
            requirements_item        = bapisitemr
          TABLES
            requirements_schedule_in = it_bapisshdin
            requirements_char_in     = it_bapischarr
            return                   = it_bapireturn.

      ENDIF.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      IF it_bapireturn[] IS INITIAL.
        wa_success_ix = wa_success_ix + 1.
      ELSE.
        LOOP AT it_bapireturn WHERE type NE 'S'.
          wa_error_ix = wa_error_ix + 1.
          MOVE-CORRESPONDING it_headitem TO it_error.
          MOVE it_bapireturn-type        TO it_error-msgty.
          MOVE it_bapireturn-id          TO it_error-id.
          MOVE it_bapireturn-number      TO it_error-msgno.
          MOVE it_bapireturn-message     TO it_error-msg.

          APPEND it_error.
          CLEAR it_error.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
*&      Form  COLOR_VALUE
*&---------------------------------------------------------------------*
FORM color_value USING p_ind.
  DATA : l_valc   LIKE  rm60cuvt-valc,
         l_vtnam  LIKE  conf_out-atnam.
** Changed by Furong on 10/08/07 for EBOM FSC
*  CONCATENATE 'COLOR' p_ind  INTO  l_vtnam.
*  CONCATENATE 'COLOR' p_ind '/' it_item-inexc INTO l_valc.
  IF it_item-matnr+13(1) = ' '.
    CONCATENATE 'COLOR' p_ind  INTO  l_vtnam.
    CONCATENATE 'COLOR' p_ind '/' it_item-inexc INTO l_valc.
  ELSE.
    CONCATENATE 'COL_' p_ind  INTO  l_vtnam.
    CONCATENATE 'COL_' p_ind '/' it_item-inexc INTO l_valc.
  ENDIF.
** End of change.

  READ TABLE it_color WITH KEY vtnam = l_vtnam
                               valc  = l_valc.
  IF sy-subrc EQ 0.
    it_bapischarr-requ_date  = it_item-pdatu.
    it_bapischarr-int_char   = it_color-vtint.
    it_bapischarr-char_value = it_color-slnid.
    it_bapischarr-ch_qty     = it_item-plnmg.
    it_bapischarr-fixing     = 'X'.
    it_bapischarr-copy_frmed = 'X'.
    it_bapischarr-flag_usage = 'X'.
    APPEND it_bapischarr.
    CLEAR it_bapischarr.
  ENDIF.
ENDFORM.                    " COLOR_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_WORK_DATE
*&---------------------------------------------------------------------*
FORM check_work_date CHANGING p_indicator p_msg.
  DATA l_plnmg(20).
  CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
    EXPORTING
      date                       = it_headitem-pdatu
      factory_calendar_id        = wa_fabkl
      message_type               = 'E'
    EXCEPTIONS
      date_after_range           = 1
      date_before_range          = 2
      date_invalid               = 3
      date_no_workingday         = 4
      factory_calendar_not_found = 5
      message_type_invalid       = 6
      OTHERS                     = 7.

  IF sy-subrc <> 0.
    p_indicator = 'E'.
    CALL FUNCTION 'RKC_MSG_STRING'
      EXPORTING
        id      = sy-msgid
        mtype   = sy-msgty
        number  = sy-msgno
        par1    = sy-msgv1
        par2    = sy-msgv2
        par3    = sy-msgv3
        par4    = sy-msgv4
      IMPORTING
        msg_lin = p_msg
      EXCEPTIONS
        OTHERS  = 1.

    WRITE: it_headitem-plnmg TO l_plnmg UNIT 'EA' LEFT-JUSTIFIED.
    CONCATENATE p_msg 'QTY:' l_plnmg INTO p_msg SEPARATED BY space.
  ENDIF.

*  DATA : IT_THOL  LIKE  TABLE OF THOL  WITH HEADER LINE.
*  CLEAR : IT_THOL, IT_THOL[].
*  CALL FUNCTION 'HOLIDAY_CHECK_AND_GET_INFO'
*    EXPORTING
*      DATE                               = IT_HEADITEM-PDATU
*      HOLIDAY_CALENDAR_ID                = WA_FABKL
**       WITH_HOLIDAY_ATTRIBUTES            = ' '
*    IMPORTING
*      HOLIDAY_FOUND                      = P_INDICATOR
*    TABLES
*      HOLIDAY_ATTRIBUTES                 = IT_THOL
*    EXCEPTIONS
*      CALENDAR_BUFFER_NOT_LOADABLE       = 1
*      DATE_AFTER_RANGE                   = 2
*      DATE_BEFORE_RANGE                  = 3
*      DATE_INVALID                       = 4
*      HOLIDAY_CALENDAR_ID_MISSING        = 5
*      HOLIDAY_CALENDAR_NOT_FOUND         = 6
*      OTHERS                             = 7.
*
*  IF SY-SUBRC <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " CHECK_WORK_DATE
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SCHEDULE_CHAR
*&---------------------------------------------------------------------*
FORM generate_schedule_char.
  DATA : l_indicator     LIKE  scal-indicator.
  DATA : l_msg           LIKE  cfgnl-msglin.
  LOOP AT it_headitem WHERE werks EQ it_headmatnr-werks
                        AND versb EQ it_headmatnr-versb
                        AND pbdnr EQ it_headmatnr-pbdnr
                        AND matnr EQ it_headmatnr-matnr
                        AND pver  EQ it_headmatnr-pver.

*----> Working date Check
    CLEAR : l_indicator, l_msg.
    PERFORM check_work_date CHANGING l_indicator l_msg.

    IF l_indicator EQ 'E'.
      wa_error_ix = wa_error_ix + 1.
      MOVE-CORRESPONDING it_headitem TO it_error.
      MOVE l_indicator   TO  it_error-msgty.
      MOVE l_msg         TO  it_error-msg.
      APPEND it_error.
      CLEAR it_error.
    ELSE.
      it_bapisshdin-date_type  = it_headitem-prgrs.   "DATE TYPE
*      IT_BAPISSHDIN-DATE_TYPE  = '1'              .   "DATE TYPE
      it_bapisshdin-req_date   = it_headitem-pdatu.   "DATE
      it_bapisshdin-req_qty    = it_headitem-plnmg.   "QTY
      it_bapisshdin-unit       = wa_meins.            "UNIT
      it_bapisshdin-prod_ves   = it_headitem-pver.    "PROD VERSION
      APPEND it_bapisshdin.
      CLEAR it_bapisshdin.

*----> COLOR EXT/INT
      LOOP AT it_item WHERE werks EQ it_headitem-werks
                        AND versb EQ it_headitem-versb
                        AND pbdnr EQ it_headitem-pbdnr
                        AND matnr EQ it_headitem-matnr
                        AND pver  EQ it_headitem-pver
                        AND pdatu EQ it_headitem-pdatu.

        CASE it_item-cogub.
          WHEN 'I'.
            PERFORM color_value USING 'INT'.
          WHEN 'E'.
            PERFORM color_value USING 'EXT'.
        ENDCASE.
      ENDLOOP.

*----> COLOR MI, OPT1~4
** Changed by Furong on 10/10/09 for EBOM
*      LOOP AT it_color WHERE vtnam NE 'COLOREXT'
*                         AND vtnam NE 'COLORINT'.
      LOOP AT it_color.
        IF it_color-vtnam EQ 'COLOREXT' OR it_color-vtnam EQ 'COL_EXT'
         OR it_color-vtnam EQ 'COLORINT' OR it_color-vtnam EQ 'COL_INT'
 .
          CONTINUE.
        ELSE.
**End of change
          it_bapischarr-requ_date  = it_headitem-pdatu.
          it_bapischarr-int_char   = it_color-vtint.
          it_bapischarr-char_value = it_color-slnid.
          it_bapischarr-ch_qty     = it_headitem-plnmg.
          it_bapischarr-fixing     = 'X'.
          it_bapischarr-copy_frmed = 'X'.
          it_bapischarr-flag_usage = 'X'.
          APPEND it_bapischarr.
          CLEAR it_bapischarr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GENERATE_SCHEDULE_CHAR
*&---------------------------------------------------------------------*
*&      Form  DELETE_PIR
*&---------------------------------------------------------------------*
FORM delete_pir.
  DATA : l_object     TYPE  ibinown-objkey.

  SELECT SINGLE *
               FROM t371f
               WHERE objtyp EQ 'PBKO'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_pbim
    FROM pbim
   WHERE versb = wa_versb_nb    "version
     AND werks = p_werks.       "Plant

  LOOP AT it_pbim.
*
    CLEAR l_object.
    l_object+0(18)  = it_pbim-matnr.
    l_object+18(4)  = it_pbim-werks.
    l_object+22(4)  = it_pbim-bedae.
    l_object+26(2)  = it_pbim-versb.
    l_object+28(10) = it_pbim-pbdnr.

    CLEAR : it_pbed, it_pbed[].
    SELECT *
      INTO TABLE it_pbed
      FROM pbed
     WHERE bdzei = it_pbim-bdzei.

    IF sy-subrc = 0.
      LOOP AT it_pbed.
        DELETE FROM pbed WHERE bdzei = it_pbed-bdzei
                           AND pdatu = it_pbed-pdatu.
        l_object+38(10) = it_pbed-pdatu.
        SELECT SINGLE *
                     FROM ibinown
                     WHERE inttyp EQ t371f-inttyp
                       AND objkey EQ l_object.
        IF sy-subrc EQ 0.
          UPDATE ibinown SET delflag = 'X'
                     WHERE inttyp  EQ t371f-inttyp
                       AND objkey EQ l_object.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DELETE FROM pbim WHERE matnr = it_pbim-matnr
                       AND werks = it_pbim-werks
                       AND bedae = it_pbim-bedae
                       AND versb = it_pbim-versb
                       AND pbdnr = it_pbim-pbdnr .
  ENDLOOP.
ENDFORM.                    " DELETE_PIR

*&---------------------------------------------------------------------*
*&      Form  summarize_data_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summarize_data_vehicle.
  DATA: l_matnr             LIKE mara-matnr   ,
        l_inexc             LIKE it_item-inexc,
        l_cogub             TYPE zcogub       ,
        l_pver              LIKE it_item-pver ,
        l_vers(2)           TYPE n            ,
        l_intc              LIKE it_item-inexc,
        l_extc              LIKE it_item-inexc,
        l_record            LIKE it_item      ,
        l_atinn             LIKE cabn-atinn   ,
        l_facid             LIKE tfacs-ident  ,
        l_datum             LIKE sy-datum     ,
        l_line              TYPE i            ,
        l_plnmg             LIKE it_item-plnmg,
        l_modl              LIKE ztpp_input_plan-modl,
        l_body_ser          LIKE ztpp_input_plan-body_ser,
        l_item              LIKE TABLE OF it_item      WITH HEADER LINE.
  DATA: l_len TYPE i,
        l_dist LIKE ztpp_pmt07jb_a-dist,
        l_dealer_old TYPE zdealer,
        l_dealer_new TYPE zdealer1,
        l_mi  LIKE ztpp_pmt07jb_a-bmdl.

  DATA : BEGIN OF it_ausp OCCURS 0,
           modl  LIKE ztpp_input_plan-modl,
           body_ser  LIKE ztpp_input_plan-body_ser,
           objek LIKE ausp-objek,
         END OF it_ausp.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_RP_STATUS'.

*  SELECT * INTO TABLE it_ausp
*    FROM ausp
*   WHERE atinn = l_atinn
*     AND klart = '002'
*     AND atwrt = '00'  .

*-< Added on 05.12.2014 Victor
*  SELECT * INTO TABLE it_ausp
*    FROM ausp
*   WHERE atinn = l_atinn
*     AND klart = '002'
*     AND atwrt <= '05'  .

  SELECT   modl body_ser
    INTO CORRESPONDING FIELDS OF TABLE it_ausp
  FROM ztpp_input_plan
  WHERE status >= '00'
    AND status <= '05'
    AND mitu   <> 'Y'.

  LOOP AT it_ausp.
    CONCATENATE it_ausp-modl it_ausp-body_ser INTO it_ausp-objek.
    MODIFY it_ausp.
  ENDLOOP.
*->

  SELECT SINGLE fabkl INTO l_facid
    FROM t001w
   WHERE werks = 'P001'.
** changed by Furong on 10/18/2005
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = '+'
      date                         = sy-datum
      factory_calendar_id          = l_facid
    IMPORTING
      date                         = l_datum
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
** end of change

  it_vals-atnam = 'P_MODEL_YEAR'.          APPEND it_vals.
*-----Start  Requested by hur, changed by wskim, on 2004.10.20
*Destination_code is the code of delivery nation
*  it_vals-atnam = 'P_DESTINATION_CODE'.    APPEND it_vals.
  it_vals-atnam = 'P_WORK_ORDER'.          APPEND it_vals.
*-----End
  it_vals-atnam = 'P_MI'        .          APPEND it_vals.
  it_vals-atnam = 'P_OCN'       .          APPEND it_vals.
  it_vals-atnam = 'P_VERSION'   .          APPEND it_vals.
  it_vals-atnam = 'P_EXT_COLOR' .          APPEND it_vals.
  it_vals-atnam = 'P_INT_COLOR' .          APPEND it_vals.
  it_vals-atnam = 'P_USAGE_CAR' .          APPEND it_vals.

  LOOP AT it_ausp.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = it_ausp-objek(18)
      TABLES
        val_table    = it_vals
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        error_value  = 4
        OTHERS       = 5.

    CLEAR:l_matnr, l_modl, l_body_ser.

*-<  Remove test car(BIW/BIP) from target list 05.12.2014
    READ TABLE it_vals WITH KEY atnam = 'P_WORK_ORDER'.
    IF sy-subrc = 0.
      IF it_vals-atwrt+12(2) = 'XX' OR it_vals-atwrt+12(2) = 'XY'.
        CONTINUE.
        CLEAR : it_vals.
      ENDIF.
    ENDIF.
*->

    READ TABLE it_vals INDEX 8.
    IF it_vals-atwrt = 'S' OR it_vals-atwrt = 'D'.
      CONTINUE.
    ENDIF.

** added by furong
    l_modl = it_ausp-objek+0(3).
    l_body_ser = it_ausp-objek+3(6).

    SELECT SINGLE rsnum INTO l_item-pdatu FROM ztpp_input_plan
                                         WHERE modl = l_modl
                                           AND body_ser = l_body_ser.
    IF l_item-pdatu IS INITIAL.
      CONTINUE.
    ELSE.
      l_item-pdatu = sy-datum.
    ENDIF.
** end of addition
*
*    SELECT SINGLE objek INTO IT_AUSP-objek
*      FROM ausp as a
*      inner join cabn as b
*      on a~atinn = b~atinn
*     WHERE objek = it_AUSP-objek
*       AND klart = '002'
*       AND ATNAM = 'P_USAGE_CAR'
*       AND atwrt IN ('S', 'D').
*    IF SY-SUBRC = 0.
*       continue.
*    ENDIF.
*
*-----Start  Requested by hur, changed by wskim, on 2004.10.20
*    LOOP AT it_vals FROM 1 TO 3   .
*      CONCATENATE l_matnr     it_vals-atwrt     INTO  l_matnr.
*    ENDLOOP.

** Changed by Furong on 12/14/07 for EBOM
    READ TABLE it_vals INDEX 1.
    CONCATENATE l_matnr it_vals-atwrt INTO  l_matnr.

    READ TABLE it_vals INDEX 3.
    l_len = strlen( it_vals-atwrt ).
    l_mi = it_vals-atwrt.

    IF l_len = 7.

      READ TABLE it_vals INDEX 2.
      CONCATENATE l_matnr it_vals-atwrt+9(5) l_mi INTO  l_matnr.

*      READ TABLE IT_VALS INDEX 3.
*      CONCATENATE L_MATNR IT_VALS-ATWRT INTO  L_MATNR.
*-----End
      READ TABLE it_vals INDEX 4.
      CONCATENATE l_matnr       it_vals-atwrt(18) INTO l_matnr
                                                  SEPARATED BY ' '.
    ELSE.

      READ TABLE it_vals INDEX 2.
      l_dist = it_vals-atwrt+9(5).

      l_dealer_old = l_dist+3(2).

      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
        EXPORTING
          old_dealer = l_dealer_old
        IMPORTING
          new_dealer = l_dealer_new.

      CONCATENATE l_matnr l_dist+0(3) l_dealer_new l_mi INTO  l_matnr.
      READ TABLE it_vals INDEX 4.
      CONCATENATE l_matnr  it_vals-atwrt(18) INTO l_matnr.
    ENDIF.
** End of change

    READ TABLE it_vals INDEX 5 .
    l_vers = it_vals-atwrt+1(2).

    READ TABLE it_vals INDEX 7.
    l_intc = it_vals-atwrt(3) .

    READ TABLE it_vals INDEX 6.
    l_extc = it_vals-atwrt(3) .
    l_item-werks = 'P001'        .
*    l_item-pdatu = l_datum      .
    l_item-matnr = l_matnr .
    l_item-cogub = 'E'     .
    l_item-inexc = l_extc  .
    l_item-pver  = l_vers        .
    l_item-plnmg = 1       .
    APPEND l_item .
    l_item-cogub = 'I'     .
    l_item-inexc = l_intc  .
    APPEND l_item .
  ENDLOOP.

  DATA: lw_pbdnr LIKE it_item-pbdnr.


  LOOP AT l_item.
    CLEAR: it_item.

    lw_pbdnr = 'SEQ_SUM'.
*    IF l_item-pver EQ '00'.
*      lw_pbdnr = 'SEQ_SUM'.
*    ELSE.
*      CONCATENATE 'SEQ_SUM' l_item-pver INTO lw_pbdnr.
*    ENDIF.

    READ TABLE it_item WITH KEY werks = 'P001'
                                versb = l_item-versb
                                pbdnr = lw_pbdnr
                                matnr = l_item-matnr
                                pdatu = l_item-pdatu
                                cogub = l_item-cogub
                                inexc = l_item-inexc
                                pver  = l_item-pver.
    IF sy-subrc NE 0.
      MOVE: 'P001'       TO it_item-werks,
            lw_pbdnr     TO it_item-pbdnr,
            l_item-matnr TO it_item-matnr,
            l_item-pdatu TO it_item-pdatu,
            l_item-cogub TO it_item-cogub,
            l_item-inexc TO it_item-inexc,
            '1'          TO it_item-prgrs,
            l_item-plnmg TO it_item-plnmg,
            l_item-pver  TO it_item-pver.

      APPEND it_item.
    ELSE.
      it_item-plnmg = it_item-plnmg + l_item-plnmg.
      MODIFY it_item INDEX sy-tabix.
    ENDIF.
  ENDLOOP.



*  SORT l_item BY matnr cogub inexc pver DESCENDING.
*  CLEAR: l_plnmg .
*  READ TABLE l_item INDEX 1.
*  l_record = l_item .
*  l_matnr  = l_item-matnr.
*  l_inexc = l_item-inexc .
*  l_cogub = l_item-cogub .
*  l_pver  = l_item-pver .
*
*  LOOP AT l_item.
*    IF l_matnr = l_item-matnr AND l_inexc = l_item-inexc AND
*       l_cogub = l_item-cogub .
*      " Accumulate the data..
*      l_plnmg = l_plnmg + l_item-plnmg .
*      CONTINUE .
*    ELSE.
*      it_item-pbdnr = 'SEQ_SUM' .
*      it_item-werks = 'P001'    .
*      it_item-pdatu = l_datum   .
*      it_item-matnr = l_matnr   .
*      it_item-cogub = l_cogub   .
*      it_item-inexc = l_inexc   .
*      it_item-prgrs = '1'       .
*      it_item-pver  = l_pver    .
*      it_item-plnmg = l_plnmg   .
*      APPEND it_item .
*      l_record = l_item.
*      l_matnr = l_item-matnr .
*      l_inexc = l_item-inexc .
*      l_cogub = l_item-cogub .
*      l_pver  = l_item-pver  .
*      l_plnmg = 1            .
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE l_item LINES l_line.
*  CHECK l_line > 0.
*  it_item-pbdnr = 'SEQ_SUM' .
*  it_item-werks = 'P001'    .
*  it_item-pdatu = l_datum   .
*  it_item-matnr = l_matnr   .
*  it_item-cogub = l_cogub   .
*  it_item-inexc = l_inexc   .
*  it_item-prgrs = '1'       .
*  it_item-pver  = l_pver    .
*  it_item-plnmg = l_plnmg   .
*  APPEND it_item .
ENDFORM.                    " summarize_data_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameters.
  SELECT MAX( sqdt ) INTO wa_datum
    FROM ztpp_pmt07jb_a
   WHERE gubb = 'A'     .
ENDFORM.                    " SET_PARAMETERS

*&---------------------------------------------------------------------*
*&      Form  PLAN_RECALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plan_recalc.
  DATA: lw_wosum        LIKE ztpp_wosum    ,
        l_matnr         LIKE mara-matnr    ,
        l_header        LIKE mara-matnr    ,
        l_int           TYPE i             ,
        lt_val          LIKE TABLE OF zspp_vin_value   WITH HEADER LINE,
        lt_wosum        LIKE TABLE OF ztpp_wosum       WITH HEADER LINE.
** Changed by Furong on 07/30/12 for ZTPP_WOSUM lock issue
  DATA: l_cn TYPE i.
** End on 07/30/12

  CLEAR : it_error[], it_error.

** Changed by Furong on 01/11/09
*  CHECK RA_SHORT EQ 'X'.
  CHECK ra_long EQ 'X'.
** End of change
  IF p_plan = 'X' .
    CLEAR: wa_flag     .
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_WOSUM
    SELECT * INTO TABLE lt_wosum
     FROM ztpp_wosum.

    SORT lt_wosum BY wo_ser nation dealer.

** Changed by Furong on 07/30/12 for ZTPP_WOSUM lock issue
    CLEAR: l_cn.
** End on 07/30/12
    LOOP AT lt_wosum.
      CONCATENATE lt_wosum-wo_ser lt_wosum-nation lt_wosum-dealer
                  lt_wosum-extc   lt_wosum-intc   INTO l_matnr   .
      IF lt_wosum-modqty <= lt_wosum-seqqty.
        CONTINUE.
      ENDIF.
      " Change the WOSUM, WOCL, WOHD...
      SELECT SUM( pqty ) INTO lt_wosum-planqty
        FROM ztpp_pmt07jb_a
       WHERE ordr = lt_wosum-wo_ser
         AND dist = l_matnr+9(5)
         AND extc = lt_wosum-extc
         AND intc = lt_wosum-intc
         AND gubb = 'A'           .
** Changed by Furong on 01/11/09
      IF sy-subrc = 0 AND lt_wosum-planqty > 0.
** End of change

        lt_wosum-forecastqty = lt_wosum-modqty
                             - lt_wosum-planqty - lt_wosum-seqqty  .
        UPDATE ztpp_wosum   SET planqty     = lt_wosum-planqty
                                forecastqty = lt_wosum-forecastqty
                          WHERE wo_ser = lt_wosum-wo_ser
                            AND nation = lt_wosum-nation
                            AND dealer = lt_wosum-dealer
                            AND extc   = lt_wosum-extc
                            AND intc   = lt_wosum-intc.

        IF sy-subrc NE 0.
          PERFORM write_error  USING text-010 l_matnr   .
          PERFORM save_error   USING lt_wosum '010' text-010.
        ENDIF.
** Changed by Furong on 07/30/12 add commit to release temp. memory
        l_cn = l_cn + 1.
        IF l_cn = 100.
          COMMIT WORK.
          CLEAR: l_cn.
        ENDIF..
** End on 07/30/12

        CLEAR: lt_val, lt_val[].
        lt_val-atwrt = l_int = lt_wosum-planqty.     CONDENSE lt_val-atwrt.
        lt_val-atnam = 'P_PLAN_QTY'.                 APPEND lt_val        .
        lt_val-atwrt = l_int = lt_wosum-forecastqty. CONDENSE lt_val-atwrt.
        lt_val-atnam = 'P_FORECAST_QTY'.             APPEND lt_val        .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object       = l_matnr
            mode         = 'W'
            ctype        = '001'
            display      = 'D'
          TABLES
            val_table    = lt_val
          EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

        IF sy-subrc <> 0  .
          PERFORM write_error  USING text-020 l_matnr   .
          PERFORM save_error USING lt_wosum '020' text-020.

        ENDIF.

        l_matnr = l_matnr(14) .
        IF l_header = l_matnr .
          CONTINUE.
        ELSE.
          l_header = l_matnr  .
        ENDIF.

        SELECT SUM( pqty ) INTO lt_wosum-planqty
          FROM ztpp_pmt07jb_a
         WHERE ordr = lt_wosum-wo_ser
           AND dist = l_matnr+9(5)
           AND gubb = 'A'           .

        SELECT SUM( modqty ) SUM( seqqty )
          INTO (lt_wosum-modqty, lt_wosum-seqqty)
          FROM ztpp_wosum
         WHERE wo_ser = lt_wosum-wo_ser
           AND nation = lt_wosum-nation
           AND dealer = lt_wosum-dealer .

        lt_wosum-forecastqty = lt_wosum-modqty
                             - lt_wosum-planqty - lt_wosum-seqqty  .

        CLEAR: lt_val, lt_val[].
        lt_val-atwrt = l_int = lt_wosum-planqty.     CONDENSE lt_val-atwrt.
        lt_val-atnam = 'P_PLAN_QTY'.                 APPEND lt_val        .
        lt_val-atwrt = l_int = lt_wosum-forecastqty. CONDENSE lt_val-atwrt.
        lt_val-atnam = 'P_FORECAST_QTY'.             APPEND lt_val        .

        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
          EXPORTING
            object       = l_matnr
            mode         = 'W'
            ctype        = '001'
            display      = 'D'
          TABLES
            val_table    = lt_val
          EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.

        IF sy-subrc <> 0  .
          PERFORM write_error  USING text-030 l_matnr   .
          PERFORM save_error   USING lt_wosum '030' text-030.
        ENDIF.
      ENDIF.
    ENDLOOP.
*----Start requested by hur, changed by wskim ,on 2004.10.20
  ELSEIF p_conf EQ 'X'.
    CLEAR: wa_flag     .
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_wosum
      FROM ztpp_wosum .
** Changed by Furong on 07/30/12 for ZTPP_WOSUM lock issue
    CLEAR: l_cn.
** End on 07/30/12

    SORT lt_wosum BY wo_ser nation dealer .
    LOOP AT lt_wosum.
      CONCATENATE lt_wosum-wo_ser lt_wosum-nation lt_wosum-dealer
                  lt_wosum-extc   lt_wosum-intc   INTO l_matnr   .

      " Change the WOSUM, WOCL, WOHD...
      SELECT SUM( pqty ) INTO lt_wosum-planqty
        FROM ztpp_pmt07jb_a
       WHERE ordr = lt_wosum-wo_ser
         AND dist = l_matnr+9(5)
         AND extc = lt_wosum-extc
         AND intc = lt_wosum-intc
         AND gubb = 'A'
         AND gub1 = 1.

      lt_wosum-forecastqty = lt_wosum-modqty
                           - lt_wosum-planqty - lt_wosum-seqqty  .
      UPDATE ztpp_wosum   SET planqty     = lt_wosum-planqty
                              forecastqty = lt_wosum-forecastqty
                        WHERE wo_ser = lt_wosum-wo_ser
                          AND nation = lt_wosum-nation
                          AND dealer = lt_wosum-dealer
                          AND extc   = lt_wosum-extc
                          AND intc   = lt_wosum-intc.

      IF sy-subrc NE 0.
        PERFORM write_error  USING text-010 l_matnr   .
        PERFORM save_error   USING lt_wosum '010' text-010.
      ENDIF.
** Changed by Furong on 07/30/12 add commit to release temp. memory
      l_cn = l_cn + 1.
      IF l_cn = 100.
        COMMIT WORK.
        CLEAR: l_cn.
      ENDIF..
** End on 07/30/12

      CLEAR: lt_val, lt_val[].
      lt_val-atwrt = l_int = lt_wosum-planqty.     CONDENSE lt_val-atwrt.
      lt_val-atnam = 'P_PLAN_QTY'.                 APPEND lt_val        .
      lt_val-atwrt = l_int = lt_wosum-forecastqty. CONDENSE lt_val-atwrt.
      lt_val-atnam = 'P_FORECAST_QTY'.             APPEND lt_val        .

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_matnr
          mode         = 'W'
          ctype        = '001'
          display      = 'D'
        TABLES
          val_table    = lt_val
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      IF sy-subrc <> 0  .
        PERFORM write_error  USING text-020 l_matnr   .
        PERFORM save_error   USING lt_wosum '020' text-020.
      ENDIF.

      l_matnr = l_matnr(14) .
      IF l_header = l_matnr .
        CONTINUE.
      ELSE.
        l_header = l_matnr  .
      ENDIF.

      SELECT SUM( pqty ) INTO lt_wosum-planqty
        FROM ztpp_pmt07jb_a
       WHERE ordr = lt_wosum-wo_ser
         AND dist = l_matnr+9(5)
         AND gubb = 'A'
         AND gub1 = 1.


      SELECT SUM( modqty ) SUM( seqqty )
        INTO (lt_wosum-modqty, lt_wosum-seqqty)
        FROM ztpp_wosum
       WHERE wo_ser = lt_wosum-wo_ser
         AND nation = lt_wosum-nation
         AND dealer = lt_wosum-dealer .

      lt_wosum-forecastqty = lt_wosum-modqty
                           - lt_wosum-planqty - lt_wosum-seqqty  .

      CLEAR: lt_val, lt_val[].
      lt_val-atwrt = l_int = lt_wosum-planqty.     CONDENSE lt_val-atwrt.
      lt_val-atnam = 'P_PLAN_QTY'.                 APPEND lt_val        .
      lt_val-atwrt = l_int = lt_wosum-forecastqty. CONDENSE lt_val-atwrt.
      lt_val-atnam = 'P_FORECAST_QTY'.             APPEND lt_val        .

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
        EXPORTING
          object       = l_matnr
          mode         = 'W'
          ctype        = '001'
          display      = 'D'
        TABLES
          val_table    = lt_val
        EXCEPTIONS
          no_data      = 1
          error_mode   = 2
          error_object = 3
          error_value  = 4
          OTHERS       = 5.

      IF sy-subrc <> 0  .
        PERFORM write_error  USING text-030 l_matnr   .
        PERFORM save_error   USING lt_wosum '030' text-030.
      ENDIF.
    ENDLOOP.
*-----End
  ENDIF.

ENDFORM.                    " PLAN_RECALC

*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_010  text
*----------------------------------------------------------------------*
FORM write_error USING    pa_text  pa_matnr.
  IF wa_flag = 'X'.
    " Write Header's Format...
    ULINE AT (85) .
    WRITE AT: /001(85) text-900.
    ULINE AT (85) .
  ENDIF.

  WRITE AT: /001(46) pa_text ,
             048(03) ' : '   ,
             051(18) pa_matnr,
             069(16) text-019.
  ULINE AT (85) .
ENDFORM.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  version_check_MRP
*&---------------------------------------------------------------------*
*  FOR SAME MATERIAL ON THE SAME DAY, DIFFERENT VERSION IS ASSIGNED TO
*  DIFFERENT REQUIREMENT NUMBER
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM version_check_mrp.
  DATA: l_pbdnr LIKE it_item-pbdnr.
  DATA: l_cnt(2) TYPE n.
  DATA: l_matnr LIKE it_item-matnr.
  DATA: l_pdatu LIKE it_item-pdatu.
  DATA: l_pver  LIKE it_item-pver.

  SORT it_item BY matnr pver.

  LOOP AT it_item.
    IF sy-tabix NE 1.
*     if current matnr and date are the same with last record
      IF it_item-matnr = l_matnr ."AND
*         IT_ITEM-PDATU = L_PDATU .
*       check the version for same matnr and same date
*       if the version is different from last one
*       change the requirement number
        IF it_item-pver  NE l_pver.
          l_cnt = l_cnt + 1.
          CONCATENATE it_item-pbdnr l_cnt INTO l_pbdnr.
          it_item-pbdnr = l_pbdnr.
          MODIFY it_item.
*       if the version is same, don't change the requirement number
*       and if the version has been changed, update the requiremtn
*       number using the same number of last record
        ELSEIF it_item-pver EQ l_pver AND
               l_cnt NE 0.
          CONCATENATE it_item-pbdnr l_cnt INTO l_pbdnr.
          it_item-pbdnr = l_pbdnr.
          MODIFY it_item.

        ENDIF.

      ELSE.
        CLEAR l_cnt.
      ENDIF.
    ENDIF.
*   save the current record data
*    L_PDATU = IT_ITEM-PDATU.
    l_matnr = it_item-matnr.
    l_pver  = it_item-pver.

  ENDLOOP.

ENDFORM.                    " version_check_MRP
*&---------------------------------------------------------------------*
*&      Form  SAVE_ERROR
*&---------------------------------------------------------------------*
FORM save_error  USING    ps_wosum STRUCTURE ztpp_wosum
                          p_para
                          p_text.
  CLEAR it_error.
  it_error-wo_ser =  ps_wosum-wo_ser.
  it_error-nation =  ps_wosum-nation.
  it_error-dealer =  ps_wosum-dealer.
  it_error-modqty =  ps_wosum-modqty.
  it_error-seqqty =  ps_wosum-seqqty.
  it_error-planqty = ps_wosum-planqty.
  it_error-forecastqty =  ps_wosum-forecastqty.

  IF p_para = '010' OR p_para = '020'.
    it_error-extc   =  ps_wosum-extc.
    it_error-intc   =  ps_wosum-intc.
  ENDIF.

  CONCATENATE p_text ' : ' it_error-wo_ser it_error-nation
              it_error-dealer it_error-extc it_error-intc
              '  Update is fail' INTO it_error-msg.
  APPEND it_error.
  CLEAR it_error.
ENDFORM.                    " SAVE_ERROR
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
FORM send_email .
  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
           it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
           it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                   WITH HEADER LINE,
           gd_cnt TYPE i,
           gd_sent_all(1) TYPE c,
           gd_doc_data LIKE sodocchgi1.

  DATA : v_qty(9).
  DATA : lv_cnt(10).
  DATA : lv_desc(100).

  CHECK it_error[] IS NOT INITIAL.

  CLEAR : it_mail[].

  IF ra_long = 'X'.
    gd_doc_data-obj_descr = 'LTP PIR Error List'.
  ELSE.
    gd_doc_data-obj_descr = 'MRP PIR Error List '.
  ENDIF.

  IF ra_long  = 'X'.
    APPEND '=================================='  TO it_mail.
    APPEND '         LTP PIR Error List       '  TO it_mail.
    APPEND '=================================='  TO it_mail.
  ELSE.
    APPEND '=================================='  TO it_mail.
    APPEND '         MRP PIR Error List       '  TO it_mail.
    APPEND '=================================='  TO it_mail.
  ENDIF.

  APPEND it_mail.  CLEAR : it_mail.

  MOVE 'Work order' TO it_mail+0(14).
  MOVE 'Ext.'       TO it_mail+15(5).
  MOVE 'Int.'       TO it_mail+21(5).
  MOVE 'Mod.Qty'    TO it_mail+27(8).
  MOVE 'Seq.Qty'    TO it_mail+36(8).
  MOVE 'Plan.Qty'   TO it_mail+46(8).
  MOVE 'Forecast'   TO it_mail+55(8).
  MOVE 'Material'   TO it_mail+66(18).
  MOVE 'Req.Plan No'   TO it_mail+85(12).
  MOVE 'Message.No.'   TO it_mail+98(10).
  MOVE 'Error Message' TO it_mail+110(100).
  APPEND it_mail.  CLEAR : it_mail.

  LOOP AT it_error.
    CONCATENATE it_error-wo_ser it_error-nation it_error-dealer
                                 INTO it_mail+0(14).
    MOVE it_error-extc TO             it_mail+15(5).
    MOVE it_error-intc TO             it_mail+21(5).
    MOVE it_error-modqty TO           it_mail+27(8).
    MOVE it_error-seqqty TO           it_mail+36(8).
    MOVE it_error-planqty TO          it_mail+46(8).
    MOVE it_error-forecastqty TO      it_mail+55(8).
    MOVE it_error-matnr TO            it_mail+66(18).
    MOVE it_error-pbdnr TO            it_mail+85(12).
    CONCATENATE it_error-id '-' it_error-msgno INTO  it_mail+98(10).
    MOVE it_error-msg   TO            it_mail+110(100).

    APPEND it_mail.  CLEAR : it_mail.
  ENDLOOP.

  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
*  gd_doc_data-obj_descr = sy-title.
  gd_doc_data-sensitivty = 'F'.


* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  CLEAR : it_receivers[].

*-  T-CODE :  mailing list  -> SO23
  it_receivers-receiver = p_rver.
  it_receivers-rec_type = 'C'.   "U - Internet address
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = ''.
  it_receivers-notif_ndel = ''.
  APPEND it_receivers. CLEAR it_receivers.

  CHECK it_receivers[] IS NOT INITIAL.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
      put_in_outbox              = ' '
      commit_work   = 'X'
*    IMPORTING
*      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.
ENDFORM.                    " SEND_EMAIL
