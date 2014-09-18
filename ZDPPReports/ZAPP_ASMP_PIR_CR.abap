************************************************************************
* Program Name      : ZAPP_ASMP_PIR_CR
* Author            : Victor
* Creation Date     : 06/12/14
* Specifications    : AS/MP PIR Creation
* Development Request No :
* Addl Documentation:
* Description       : copied from ZIPP_ASMP_PIR
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zapp_asmp_pir_cr   NO STANDARD PAGE HEADING
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
TABLES : t001w.              "Plants/Branches
TABLES : bapisitemr, "Communication fields:indep. reqmts item data table
         cm60r,      "Common work area for planned indep. req functions
         t371f,     "IB: Object Types for User (Owner/Observer)
         ibinown.   "IB: Owner of an IBase instance

DATA : BEGIN OF it_headmatnr OCCURS 0,
         werks   LIKE  ztpp_asmp_pir-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
*         pbdnr   LIKE  pbim--pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  pbim-matnr.  "MATERIAL No
*         pver    LIKE  pbim-pver.  "PROD VERSION
DATA : END OF it_headmatnr.

DATA: it_asmp_pir LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE.

DATA : BEGIN OF it_error OCCURS 0,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "FSC
         msgty   LIKE  sy-msgty,               "STATUS
         msg     LIKE  cfgnl-msglin.           "MESSAGE
DATA : END OF it_error.


DATA : BEGIN OF it_success OCCURS 0,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr.  "REQUIREMENT PLAN No
DATA : END OF it_success.

*----->

DATA:   it_pbim  LIKE TABLE OF pbim    WITH HEADER LINE,

       it_pbed  LIKE TABLE OF pbed    WITH HEADER LINE.

DATA : wa_versb_nb       LIKE  pbim-versb,  "PIR VERSION
       wa_line_ix        LIKE  sy-tabix,
       wa_success_ix     LIKE  sy-tabix,
       wa_error_ix       LIKE  sy-tabix,    "COUNT OF ERROR FOR LINE
*       WA_PERCENTAGE_PD  TYPE  P       ,
*       WA_FABKL          LIKE  T001W-FABKL,
       wa_meins          LIKE  mara-meins,
       w_type(1).


DATA : it_bapisshdin   LIKE TABLE OF bapisshdin  WITH HEADER LINE,
       it_bapireturn   LIKE TABLE OF bapireturn1 WITH HEADER LINE.

DATA: wa_datum             LIKE sy-datum,
      wa_flag              TYPE c       ,
      wa_active(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark   VALUE 'X',
           c_date   VALUE 'D',         "DATE TYPE
           c_week   VALUE 'W',         "DATE TYPE
           c_reqty  LIKE   t459u-bedae  VALUE 'BSF'.    "REQ TYPE

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS : p_werks      LIKE   t001w-werks DEFAULT 'P001'
                                             OBLIGATORY MEMORY ID wrk.

SELECTION-SCREEN SKIP 1.
PARAMETERS : ra_short   RADIOBUTTON GROUP ra  DEFAULT 'X',
             ra_long    RADIOBUTTON GROUP ra.
SELECTION-SCREEN END OF BLOCK b1.


************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.
  IF it_asmp_pir[] IS INITIAL.
    MESSAGE i000 WITH 'No data'.
  ELSE.
    PERFORM excute_process.
  ENDIF.


************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  DATA_ARRANGE
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lt_asmp_pir LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE,
*        it_asmp_pir_d LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE,
*        it_asmp_pir_w LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE,
         l_monday LIKE sy-datum,
          l_monday_n LIKE sy-datum,
          l_first LIKE sy-datum,
          l_facid LIKE tfacs-ident.

  CLEAR : it_asmp_pir[], it_asmp_pir.

*  wa_versb_nb = 'AS'.      "Short Term Plan
*  wa_active   = 'X'.  "Short :X, Weekly : '' -> check later?

  CASE c_mark.
    WHEN ra_short.
      wa_versb_nb = 'MA'.      "Short Term Plan
      wa_active   = 'X'.
    WHEN ra_long.
      wa_versb_nb = 'MB'.      "Long Term Plan
      CLEAR wa_active.
  ENDCASE.

  SELECT MAX( wdatu ) INTO wa_datum
    FROM ztpp_asmp_pir.

  IF ra_short = 'X'.

*-Daily
    w_type = '1'.
    SELECT * INTO TABLE it_asmp_pir
    FROM ztpp_asmp_pir
    WHERE entlu = w_type
     AND bpart <> ''
     AND wdatu = wa_datum
     AND pdatu >= sy-datum
     AND plnmg > 0.

  ELSE.

*-Weekly
    w_type = '2'.
    SELECT * INTO TABLE it_asmp_pir
   FROM ztpp_asmp_pir
   WHERE entlu = w_type
    AND bpart <> ''
    AND wdatu = wa_datum
    AND plnmg > 0.


    SELECT * INTO TABLE lt_asmp_pir
    FROM ztpp_asmp_pir
    WHERE entlu = '1'
     AND bpart <> ''
     AND wdatu = wa_datum
     AND pdatu >= sy-datum
     AND plnmg > 0.

    SORT lt_asmp_pir BY pdatu.
    READ TABLE lt_asmp_pir INDEX 1.
    l_first = lt_asmp_pir-pdatu.

    l_monday = l_first + 7.
    l_monday_n = l_monday + 7.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
      EXPORTING
        date   = l_monday
      IMPORTING
        monday = l_monday.

    l_monday_n = l_monday + 7.

    SELECT SINGLE fabkl INTO l_facid
      FROM t001w
      WHERE werks =  p_werks.

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

    LOOP AT lt_asmp_pir.
      it_asmp_pir = lt_asmp_pir.
      IF lt_asmp_pir-pdatu < l_monday.
        it_asmp_pir-pdatu = l_first.
      ELSEIF  lt_asmp_pir-pdatu >= l_monday_n.
        it_asmp_pir-pdatu = l_monday_n.
      ELSE.
        it_asmp_pir-pdatu = l_monday.
      ENDIF.
      COLLECT it_asmp_pir.
      CLEAR: it_asmp_pir, lt_asmp_pir.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " DATA_ARRANGE

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*FORM PROGRESS_INDICATOR USING  P_TEXT.
*  IF WA_LINE_IX <> 0.
*    WA_PERCENTAGE_PD = ( SY-TABIX / WA_LINE_IX ) * 100.
*  ENDIF.
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*       EXPORTING
*            PERCENTAGE = WA_PERCENTAGE_PD
*            TEXT       = P_TEXT.
*
*ENDFORM.                    " PROGRESS_INDICATOR


*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  PERFORM delete_pir.
  PERFORM gathering_data.
  PERFORM bapi_execution.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
  DATA : l_line_ix   LIKE   sy-tabix,
         l_success_ix    LIKE   sy-tabix,
         l_line          LIKE   sy-tabix.

  DESCRIBE TABLE it_asmp_pir  LINES l_line_ix.
  IF l_line_ix = 0.
    WRITE :/ '*********** No Data found **************'.
  ELSE.
    SKIP 1.
    WRITE :/ text-313 ,
             wa_success_ix COLOR COL_POSITIVE.
    SKIP 1.
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

    SKIP 1.
    WRITE :/ '********** END OF PROCESS ***********'.
  ENDIF.
  WRITE AT: /001(030) 'End of processing ...(End)' ,
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .

*  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM gathering_data.
  DATA l_tabix_ix   LIKE   sy-tabix.

  DESCRIBE TABLE it_asmp_pir LINES wa_line_ix.
  LOOP AT it_asmp_pir.
    it_headmatnr-werks = it_asmp_pir-werks.
*    it_headmatnr-matnr = it_asmp_pir-matnr.
    it_headmatnr-matnr = it_asmp_pir-bpart. "B Part
    MOVE : wa_versb_nb  TO  it_headmatnr-versb.
    COLLECT it_headmatnr.
    CLEAR : it_headmatnr.
  ENDLOOP.

*  SORT it_asmp_pir BY werks matnr.
  SORT it_asmp_pir BY werks bpart.
ENDFORM.                    " GATHERING_DATA

*&---------------------------------------------------------------------*
*&      Form  BAPI_EXECUTION
*&---------------------------------------------------------------------*
FORM bapi_execution.
  CLEAR : it_success, it_success[],
          it_error,   it_error[].
  CLEAR : wa_success_ix, wa_error_ix.

  LOOP AT it_headmatnr.
    PERFORM generate_bapi_data.
  ENDLOOP.
ENDFORM.                    " BAPI_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
FORM generate_bapi_data.
  DATA l_bdzei LIKE  pbim-bdzei.
*  DATA : L_INDICATOR     LIKE  SCAL-INDICATOR.
*  DATA : L_MSG           LIKE  CFGNL-MSGLIN.

  CLEAR : bapisitemr, cm60r, wa_meins,
          it_bapisshdin, it_bapisshdin[],
          it_bapireturn, it_bapireturn[].

  bapisitemr-material   = it_headmatnr-matnr. "B Part
  bapisitemr-plant      = it_headmatnr-werks. "PLANT
  bapisitemr-requ_type  = c_reqty.            "VSE
  bapisitemr-version    = it_headmatnr-versb. "VERSION
  bapisitemr-vers_activ = wa_active.          "ACTIVE Yes/No ?? 'X'
  bapisitemr-req_number = 'AS'.              "Req plan No

  SELECT SINGLE meins
                INTO wa_meins
                FROM mara
                WHERE matnr EQ it_headmatnr-matnr .

  LOOP AT it_asmp_pir WHERE werks EQ it_headmatnr-werks
                         AND bpart EQ it_headmatnr-matnr.

*    it_bapisshdin-date_type  = w_type  .   "DATE TYPE
    it_bapisshdin-date_type  = it_asmp_pir-entlu .  "DATE TYPE
    it_bapisshdin-req_date   = it_asmp_pir-pdatu.   "DATE
    it_bapisshdin-req_qty    = it_asmp_pir-plnmg.   "QTY
    it_bapisshdin-unit       = wa_meins.            "UNIT
    it_bapisshdin-prod_ves   = '01'.    "PROD VERSION
    APPEND it_bapisshdin.
    CLEAR it_bapisshdin.
  ENDLOOP.

  SORT it_bapisshdin BY date_type req_date.

  SELECT SINGLE a~bdzei
                 INTO l_bdzei
                 FROM pbim AS a INNER JOIN pbed AS b
                   ON a~bdzei EQ b~bdzei
                 WHERE a~werks EQ it_headmatnr-werks  "PLANT
                   AND a~matnr EQ it_headmatnr-matnr  "Material
                   AND a~bedae EQ c_reqty           "REQUIREMENT TYPE
                   AND a~versb EQ it_headmatnr-versb. "VERSION
*                     AND a~pbdnr EQ it_headmatnr-pbdnr. "REQ. Plan No

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
        return                   = it_bapireturn.

  ELSE.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
      EXPORTING
        requirements_item        = bapisitemr
      TABLES
        requirements_schedule_in = it_bapisshdin
        return                   = it_bapireturn.

  ENDIF.

  IF it_bapireturn[] IS INITIAL.
    wa_success_ix = wa_success_ix + 1.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        WAIT = 'X'.
  ELSE.
    LOOP AT it_bapireturn WHERE type NE 'S'.
      wa_error_ix = wa_error_ix + 1.

      it_error-matnr = it_headmatnr-matnr.
      MOVE it_bapireturn-type        TO it_error-msgty.
      MOVE it_bapireturn-message     TO it_error-msg.
      APPEND it_error.
      CLEAR it_error.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GENERATE_BAPI_DATA
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
     AND matnr LIKE 'MV%'
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
