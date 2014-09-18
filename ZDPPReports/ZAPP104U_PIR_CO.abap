************************************************************************
* Program Name      : ZAPP104U_PIR_CO
* Author            : JongOh, Kim
* Creation Date     : 2003.10.30.
* Specifications By : JongOh, Kim
* Pattern           : 2.2
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Quarterly PIR for CO
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zapp104u_pir_co  NO STANDARD PAGE HEADING
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
TABLES : t001w,     "Plants/Branches
         mara,      "General Material Data
         marc,      "Plant Data for Material
         mast,      "Material to BOM Link
         mapl,      "Assignment of Task Lists to Materials
         plko,      "Task list - header
         kssk,      "Allocation Table: Object to Class
         ksml,      "Characteristics of a Class
         inob.      "Link between Internal Number and Object

TABLES : bapisitemr, "Communication fields:indep. reqmts item data table
         cm60r.      "Common work area for planned indep. req functions
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_ksml      LIKE TABLE OF ksml  WITH HEADER LINE.
DATA : it_pmt07jb_b LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE.
DATA : it_pmt07jb_a LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE.
*-----> HEADER MATNR
DATA : BEGIN OF it_headmatnr OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr.   "MATERIAL No
DATA : END OF it_headmatnr.

*-----> HEADER(DAILY ITEM)
DATA : BEGIN OF it_headitem OCCURS 0,
         werks   LIKE  ztpp_pmt07jb_c-werks,   "PLANT
         versb   LIKE  pbim-versb,             "VERSION
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "MATERIAL No
         pdatu   LIKE  ztpp_pmt07jb_c-pdatu,   "DATE
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
         plnmg   LIKE  ztpp_pmt07jb_c-plnmg,   "QTY
         pver    LIKE  ztpp_pmt07jb_c-pver ,   "PROD VERSION
         gubb    LIKE  ztpp_pmt07jb_a-gubb.   " A:DAY, B:WEEK, C:MONTH
DATA : END OF it_item.

*-----> ERROR TABLE
DATA : BEGIN OF it_error OCCURS 0,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr,   "REQUIREMENT PLAN No
         matnr   LIKE  ztpp_pmt07jb_c-matnr,   "FSC
         msgty   LIKE  sy-msgty,               "STATUS
         msg     LIKE  cfgnl-msglin.           "MESSAGE
DATA : END OF it_error.

*-----> SUCCESS TABLE
DATA : BEGIN OF it_success OCCURS 0,
         pbdnr   LIKE  ztpp_pmt07jb_c-pbdnr.  "REQUIREMENT PLAN No
DATA : END OF it_success.

*-----> Working date of Month
DATA : BEGIN OF it_workdate OCCURS 0,
         werks   TYPE  t001w-werks,
         spmon   TYPE  spmon,
         datum   TYPE  datum.
DATA : END OF it_workdate.

*-----> Configure variant of FSC
DATA : BEGIN OF it_syval OCCURS 0,
         atinn  TYPE  v_ibin_syval-atinn,
         atwrt  TYPE  v_ibin_syval-atwrt.
DATA : END OF it_syval.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_line_ix        LIKE  sy-tabix,
       wa_success_ix     LIKE  sy-tabix,    "Count of SUCCESS FOR PBDNR
       wa_error_ix       LIKE  sy-tabix.    "Count of ERROR FOR LINE

DATA : wa_pbdnr          LIKE  pbim-pbdnr.  "Req.plan number
DATA : wa_fabkl          LIKE  t001w-fabkl. "Factory calendar key

*-----> variable for Checking configure Variant
DATA : wa_objnr         TYPE v_ibinr-objnr,
       wa_in_recno      TYPE v_ibinr-in_recno,
       wa_error_flg.         "ERROR FLAG

DATA : wa_bedae         LIKE   t459u-bedae. "Requirements type

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
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : it_bdcdata     LIKE TABLE OF bdcdata  WITH HEADER LINE.
DATA : wa_option_ds   LIKE ctu_params.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: c_mark    VALUE 'X',
           c_mode    VALUE 'N',   "BDC MODE
           c_keydate LIKE   sy-datum  VALUE '99991231',  "KEY DATE
           c_versb   LIKE   pbim-versb   VALUE 'Q1',     "QUARTERLY
           c_reqty56 LIKE   t459u-bedae  VALUE 'VSE',    "REQ TYPE ST:56
           c_reqtyot LIKE   t459u-bedae  VALUE 'VSF'.    "REQ TYPE ST:OT

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
*PARAMETERS : P_WERKS     LIKE T001W-WERKS OBLIGATORY, "MEMORY ID WRK,
*             P_VERSB     TYPE PBIM-VERSB ,
PARAMETERS : p_horiz     TYPE spmon .
PARAMETERS : p_werkse LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM initialization.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM at_selection-screen.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING  p_program
                       p_dynpro.
  CLEAR it_bdcdata.
  it_bdcdata-program  = p_program.
  it_bdcdata-dynpro   = p_dynpro.
  it_bdcdata-dynbegin = 'X'.
  APPEND it_bdcdata.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING    p_fnam
                        p_fval.
*  IF P_FVAL <> Nodata.
  CLEAR it_bdcdata.
  it_bdcdata-fnam = p_fnam.
  it_bdcdata-fval = p_fval.
  APPEND it_bdcdata.
*  ENDIF.

ENDFORM.                    " BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
*  MOVE : 'P001'     TO  P_WERKS.
*         C_VERSB    TO  P_VERSB.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM at_selection-screen.
  DATA : l_yyyymm     TYPE   spmon,
         l_yyyy(4)    TYPE   n,
         l_mm(2)      TYPE   n.
*----> Check Plant
*  SELECT SINGLE *
*                FROM T001W
*                WHERE WERKS EQ P_WERKS.
*  IF SY-SUBRC NE 0.
**    GET CURSOR FIELD 'P_WERKS'.
*    MESSAGE E003 WITH 'Plant ' P_WERKS ' is invalid!!'.
*  ENDIF.

*----> Check Version
*  IF P_VERSB IS INITIAL.
*    SET CURSOR FIELD 'P_VERSB'.
*    MESSAGE E001 WITH 'Version must be inputted!!'.
*  ENDIF.

*----> Check Planning Horizon
  IF p_horiz IS INITIAL.
    SET CURSOR FIELD 'P_HORIZ'.
    MESSAGE e001 WITH text-201.
  ELSE.
    IF p_horiz+4(2) GT '00' AND p_horiz+4(2) LE '12'.
*----> Set Period of Planning Horizon
      CLEAR : r_pdatu, r_pdatu[].
      r_pdatu-sign = 'I'.
      r_pdatu-option = 'BT'.
      CONCATENATE p_horiz  '01' INTO r_pdatu-low.
      l_yyyymm = p_horiz.

      DO 3 TIMES.
        l_yyyy = l_yyyymm(4).
        l_mm   = l_yyyymm+4(2).
        IF l_mm EQ '12'.
          l_yyyy = l_yyyy + 1.
          CONCATENATE l_yyyy '01' INTO l_yyyymm.
        ELSE.
          l_yyyymm = l_yyyymm + 1.
        ENDIF.
      ENDDO.
      CONCATENATE l_yyyymm '01' INTO r_pdatu-high.
      r_pdatu-high = r_pdatu-high - 1.
      APPEND r_pdatu.

*----> Set Req.plan number
*      CONCATENATE C_VERSB(1) P_HORIZ(4) INTO WA_PBDNR.
      MOVE c_versb  TO  wa_pbdnr.
    ELSE.
      SET CURSOR FIELD 'P_HORIZ'.
      MESSAGE e001 WITH text-202.
    ENDIF.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  PERFORM write_first.

*-----> SET BDC MODE
  PERFORM set_mode.

*-----> SELECT ZTPP_PMT07JB_A
  PERFORM select_pmt07jb.

*-----> GENERATE BAPI FORMAT
  PERFORM gathering_data.

  IF wa_error_ix EQ 0.
* Reorganization period has not been maintained for any plant
    PERFORM bdc_execution.
  ENDIF.


ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  WRITE_FIRST
*&---------------------------------------------------------------------*
FORM write_first.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_FIRST

*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM set_mode.
*----> SET BDC MODE OPTION
  CLEAR : wa_option_ds.
  wa_option_ds-dismode = c_mode.
  wa_option_ds-defsize = 'X'.
  wa_option_ds-updmode = 'S'.

ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_WORKDATE
*&---------------------------------------------------------------------*
FORM append_it_workdate.
  DATA : l_yyyymm     TYPE   spmon,
         l_yyyy(4)    TYPE   n,
         l_mm(2)      TYPE   n.
  CLEAR: it_workdate, it_workdate[].
  LOOP AT it_item.
    AT NEW werks.
      MOVE it_item-werks TO it_workdate-werks.
*      CONCATENATE P_HORIZ '01' INTO L_YYYYMM.
      l_yyyymm = p_horiz.
      it_workdate-spmon = l_yyyymm.
      APPEND it_workdate.

      DO 12 TIMES.
        l_yyyy = l_yyyymm(4).
        l_mm   = l_yyyymm+4(2).
        IF l_mm EQ '12'.
          l_yyyy = l_yyyy + 1.
          l_mm   = '00'.
          CONCATENATE l_yyyy l_mm INTO l_yyyymm.
        ENDIF.
        l_yyyymm = l_yyyymm + 1.
        IF sy-index NE 12.
          it_workdate-spmon = l_yyyymm.
          APPEND it_workdate.
        ENDIF.
      ENDDO.
      CLEAR it_workdate.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " APPEND_IT_WORKDATE

*&---------------------------------------------------------------------*
*&      Form  WORKING_DATE
*&---------------------------------------------------------------------*
FORM working_date.
  DATA : l_tabix     TYPE  sy-tabix,
         l_dd(2)     TYPE  n.

  SORT it_workdate BY werks spmon.
  LOOP AT it_workdate.
    l_tabix = sy-tabix.
    AT NEW werks.
      CLEAR wa_fabkl.
      SELECT SINGLE fabkl
                 INTO wa_fabkl
                 FROM t001w
                 WHERE werks EQ it_workdate-werks.
    ENDAT.

    DO 31 TIMES.
      l_dd = sy-index.
      CONCATENATE it_workdate-spmon l_dd INTO it_workdate-datum.
      CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
        EXPORTING
          date                       = it_workdate-datum
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
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDDO.
    MODIFY it_workdate INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  SELECT_PMT07JB
*&---------------------------------------------------------------------*
FORM select_pmt07jb.
  DATA : l_datum-low    TYPE   sy-datum,
         l_datum-high   TYPE   sy-datum.
  CLEAR : it_item, it_item[].
  CLEAR : it_pmt07jb_a, it_pmt07jb_a[].

  READ TABLE r_pdatu INDEX 1.

  DATA l_week   TYPE  scal-week.
* Week of Input date
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = r_pdatu-low
    IMPORTING
      week         = l_week
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = l_week
    IMPORTING
      date         = r_pdatu-low
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.

  MODIFY r_pdatu INDEX 1.

  SELECT *
        INTO TABLE it_pmt07jb_a
        FROM ztpp_pmt07jb_a
        WHERE sqdt  IN  r_pdatu .        "SEQ Date

*----> Gathering Source data at Daily, Weekly , Monthly
  LOOP AT it_pmt07jb_a.
    MOVE : 'P001'                 TO  it_item-werks,    "PLANT
           it_pmt07jb_a-sqdt      TO  it_item-pdatu,    "DATE
           it_pmt07jb_a-pqty      TO  it_item-plnmg,    "QTY
           it_pmt07jb_a-pver      TO  it_item-pver .    "PROD VERSION

    CONCATENATE it_pmt07jb_a-bmdl it_pmt07jb_a-ocnn
                INTO it_item-matnr SEPARATED BY space.
    CONCATENATE it_pmt07jb_a-moye it_pmt07jb_a-dist
                it_item-matnr  INTO  it_item-matnr.     "FSC

    IF it_pmt07jb_a-gubb EQ 'B'.   "WEEK
      MOVE it_pmt07jb_a-gubb  TO  it_item-gubb.
    ENDIF.

    COLLECT it_item.           CLEAR it_item.
  ENDLOOP.
  SORT it_item BY werks versb pbdnr matnr pdatu .

*----> first Working date of month
  PERFORM append_it_workdate.
  PERFORM working_date.
ENDFORM.                    " SELECT_PMT07JB
*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM gathering_data.
  DATA : l_flg.

  CLEAR : it_headmatnr, it_headmatnr[],
          it_headitem,  it_headitem[],
          it_error,     it_error[],
          wa_error_ix,  wa_success_ix.

*--->
  LOOP AT it_item .
    AT NEW werks.
      CLEAR wa_fabkl.
      SELECT SINGLE fabkl
                 INTO wa_fabkl
                 FROM t001w
                 WHERE werks EQ it_item-werks.
    ENDAT.

    IF it_item-gubb EQ 'B'.
      PERFORM devide_week.
    ELSE.
      PERFORM normal_week.
    ENDIF.
  ENDLOOP.

  SORT it_headmatnr  BY werks versb pbdnr matnr.
  SORT it_headitem   BY werks versb pbdnr matnr pdatu.

  LOOP AT it_headmatnr.
    CLEAR : marc, mast, mapl, plko.
*---> Check existence of material in plant
    SELECT SINGLE *
                  FROM marc
                  WHERE werks EQ it_headmatnr-werks
                    AND matnr EQ it_headmatnr-matnr.
    IF sy-subrc EQ 0.
*---> Check existence of material's BOM in plant
      SELECT SINGLE *
             FROM mast
             WHERE matnr EQ it_headmatnr-matnr
               AND werks EQ it_headmatnr-werks
               AND stlan EQ '1'.    "Usage
      IF sy-subrc EQ 0.
*----> Check existence of material's Rate routing in plant
        SELECT *
               FROM mapl
               WHERE matnr EQ it_headmatnr-matnr
                 AND werks EQ it_headmatnr-werks.
          SELECT SINGLE *
                   FROM plko
                   WHERE  plnnr  EQ  mapl-plnnr
                     AND  plnal  EQ  mapl-plnal
                     AND  verwe  EQ  '1'.  "production routing
          IF sy-subrc EQ 0.
            l_flg = 'X'.
          ENDIF.
        ENDSELECT.
        IF l_flg EQ space.
          wa_error_ix = wa_error_ix + 1.
          MOVE-CORRESPONDING it_headmatnr TO it_error.
          MOVE 'E'  TO  it_error-msgty.
          MOVE text-301 TO it_error-msg.
          APPEND it_error.
          CLEAR it_error.
        ENDIF.
      ELSE.
        wa_error_ix = wa_error_ix + 1.
        MOVE-CORRESPONDING it_headmatnr TO it_error.
        MOVE 'E'  TO  it_error-msgty.
        MOVE text-302 TO it_error-msg.
        APPEND it_error.
        CLEAR it_error.
      ENDIF.
    ELSE.
      wa_error_ix = wa_error_ix + 1.
      MOVE-CORRESPONDING it_headmatnr TO it_error.
      MOVE 'E'  TO  it_error-msgty.
      MOVE text-303 TO it_error-msg.
      APPEND it_error.
      CLEAR it_error.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GATHERING_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_EXECUTION
*&---------------------------------------------------------------------*
FORM bdc_execution.
** Adjusting Requirements (Plnd Ind Req) - transaction ID 'MD74'
*  PERFORM GENERATE_BDC_DATA USING 'MD74'.
*  PERFORM CALL_TRANSACTION USING 'MD74'.


* Existent all datas cannot be erased by t-code :MD74, 75, 76
* but can be erased by BDC (T-CODE:MD62)
* Only, delete processing by BDC does hard coding of PLANT
* because is processed to plant
  DATA l_bdzei   TYPE   pbed-bdzei.
  SELECT SINGLE a~bdzei
               INTO l_bdzei
               FROM pbim AS a INNER JOIN pbed AS b
                 ON a~bdzei EQ b~bdzei
               WHERE a~werks EQ 'P001'    "PLANT
                 AND a~versb EQ c_versb   "VERSION 'Y1'
                 AND a~pbdnr EQ c_versb.  "REQ PLAN No
*  SELECT SINGLE BDZEI
*               INTO L_BDZEI
*               FROM PBIM
*               WHERE WERKS EQ 'P001'    "PLANT
*                 AND VERSB EQ C_VERSB   "VERSION 'Y1'
*                 AND PBDNR EQ C_VERSB.  "REQ PLAN No
  IF sy-subrc EQ 0.
    PERFORM generate_bdc_md62 USING 'P001'.
    PERFORM call_transaction_md62 USING 'P001'.
  ELSE.
    SELECT SINGLE a~bdzei
               INTO l_bdzei
               FROM pbim AS a INNER JOIN pbed AS b
                 ON a~bdzei EQ b~bdzei
** for E002
*               WHERE A~WERKS EQ 'E001'    "PLANT
               WHERE a~werks EQ p_werkse   "PLANT
** end
                 AND a~versb EQ c_versb   "VERSION 'Y1'
                 AND a~pbdnr EQ c_versb.  "REQ PLAN No
*    SELECT SINGLE BDZEI
*                 INTO L_BDZEI
*                 FROM PBIM
*                 WHERE WERKS EQ 'E001'    "PLANT
*                   AND VERSB EQ C_VERSB   "VERSION 'Y1'
*                   AND PBDNR EQ C_VERSB.  "REQ PLAN No
    IF sy-subrc EQ 0.
** for E002
*      PERFORM GENERATE_BDC_MD62 USING 'E001'.
*      PERFORM CALL_TRANSACTION_MD62 USING 'E001'.
      PERFORM generate_bdc_md62 USING p_werkse.
      PERFORM call_transaction_md62 USING p_werkse.
** end
    ELSE.
      CLEAR wa_error_ix.
      LOOP AT it_headmatnr.
        PERFORM generate_bapi_data.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " BDC_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
FORM generate_bdc_data USING p_ind.
  DATA : l_keydate(10).

  WRITE c_keydate TO l_keydate.
  CASE p_ind.
    WHEN 'MD74'.
* Adjusting Requirements (Plnd Ind Req) - transaction ID 'MD74'
      CLEAR : it_bdcdata, it_bdcdata[].
      PERFORM bdc_dynpro  USING 'RM60RR20' '1000'.
      PERFORM bdc_field   USING :
          'BDC_CURSOR'      'PBDNR-LOW',
          'BDC_OKCODE'      '=ONLI',
*          'WERKS-LOW'	    P_WERKS,      "PLANT
*          'BEDAE-LOW'	    C_REQTY,      "Requirements type (VSF)
          'VERSB-LOW'	    c_versb,      "VERSION
*          'PBDNR-LOW'	    WA_PBDNR,     "Requirements plan number
          'HISTFLAG'	    c_mark,       "
          'INACFLAG'	    c_mark,       "
          'DATE1'           l_keydate,    "Key date
          'TESTFLAG'	    space.
    WHEN 'MD75'.
* Delete Old Requirements Records - transaction ID 'MD75'
      PERFORM bdc_dynpro  USING 'RM60RR30' '1000'.
      PERFORM bdc_field   USING :
    	    'BDC_CURSOR'	    'PBDNR-LOW',
    	    'BDC_OKCODE'	    '=ONLI',
*    	    'WERKS-LOW'	    P_WERKS,   "Plant
*    	    'BEDAE-LOW'	    C_REQTY,   "Requirements type(VSF)
    	    'VERSB-LOW'	    c_versb,   "VERSION
*    	    'PBDNR-LOW'	    WA_PBDNR,  "Requirements plan number
          'DATE1'           l_keydate, "Key date
    	    'TESTFLAG'	    space.
    WHEN 'MD76'.
* Delete History and Independent Requirements - transaction ID 'MD76'
      PERFORM bdc_dynpro  USING 'RM60RR40' '1000'.
      PERFORM bdc_field   USING :
    	    'BDC_CURSOR'	   'PBDNR-LOW',
    	    'BDC_OKCODE'	   '=ONLI',
*    	    'WERKS-LOW'	   P_WERKS,    "Plant
*    	    'BEDAE-LOW'  	   C_REQTY,    "Requirements type(VSF)
    	    'VERSB-LOW'	   c_versb,    "VERSION
*    	    'PBDNR-LOW'	   WA_PBDNR,   "Requirements plan number
          'HDATE'          l_keydate,  "Key date
    	    'TESTFLAG'       space.
  ENDCASE.
ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM call_transaction USING  p_ind.
  CASE p_ind.
    WHEN 'MD74'.
* Adjusting Requirements (Plnd Ind Req) - transaction ID 'MD74'
      CALL TRANSACTION 'MD74' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
      PERFORM error_text_md USING p_ind.

    WHEN 'MD75'.
* Delete Old Requirements Records - transaction ID 'MD75'
      CALL TRANSACTION 'MD75' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
      PERFORM error_text_md USING p_ind.

    WHEN 'MD76'.
* Delete History and Independent Requirements - transaction ID 'MD76'
      CALL TRANSACTION 'MD76' USING it_bdcdata
                          OPTIONS FROM wa_option_ds.
      PERFORM error_text_md USING p_ind.
  ENDCASE.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT_MD
*&---------------------------------------------------------------------*
FORM error_text_md USING p_ind.
  DATA l_msg  LIKE cfgnl-msglin.
  CLEAR it_bdcdata.
  REFRESH it_bdcdata.
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
      msg_lin = l_msg
    EXCEPTIONS
      OTHERS  = 1.

  CASE sy-msgty.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      wa_error_ix = wa_error_ix + 1.
      it_error-msgty = 'E'.
      it_error-msg   = l_msg.
      APPEND it_error. CLEAR it_error.
    WHEN OTHERS.   " 'I', 'S' :SUCCESS
      CASE p_ind.
        WHEN 'MD74'.
* Delete Old Requirements Records - transaction ID 'MD75'
          PERFORM generate_bdc_data USING 'MD75'.
          PERFORM call_transaction USING 'MD75'.
        WHEN 'MD75'.
* Delete History and Independent Requirements - transaction ID 'MD76'
          PERFORM generate_bdc_data USING 'MD76'.
          PERFORM call_transaction USING 'MD76'.
        WHEN 'MD76'.
          CLEAR wa_error_ix.
          LOOP AT it_headmatnr.
            PERFORM generate_bapi_data.
          ENDLOOP.
        WHEN 'P001'.
          DATA l_bdzei   TYPE   pbed-bdzei.
          SELECT SINGLE a~bdzei
                       INTO l_bdzei
                       FROM pbim AS a INNER JOIN pbed AS b
                         ON a~bdzei EQ b~bdzei
** for E002
*               WHERE A~WERKS EQ 'E001'    "PLANT
               WHERE a~werks EQ p_werkse   "PLANT
** end
                         AND a~versb EQ c_versb   "VERSION 'Q1'
                         AND a~pbdnr EQ c_versb.  "REQ PLAN No
          IF sy-subrc EQ 0.
** for E002
*            PERFORM GENERATE_BDC_MD62 USING 'E001'.
*            PERFORM CALL_TRANSACTION_MD62 USING 'E001'.
            PERFORM generate_bdc_md62 USING p_werkse.
            PERFORM call_transaction_md62 USING p_werkse.
** end
          ELSE.
            CLEAR wa_error_ix.
            LOOP AT it_headmatnr.
              PERFORM generate_bapi_data.
            ENDLOOP.
          ENDIF.
** for E002
*        WHEN 'E001'.
        WHEN 'E001' OR 'E002'.
** end
          CLEAR wa_error_ix.
          LOOP AT it_headmatnr.
            PERFORM generate_bapi_data.
          ENDLOOP.
      ENDCASE.
  ENDCASE.
  CLEAR it_error.
ENDFORM.                    " ERROR_TEXT_MD
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
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

ENDFORM.                    " CHECK_COLOR_FOR_MATNR
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
FORM generate_bapi_data.
  CLEAR : bapisitemr, mara, cm60r, wa_error_flg,
          it_bapisshdin, it_bapisshdin[],
          it_bapischarr, it_bapischarr[],
          it_bapireturn, it_bapireturn[].
*----> Check Fert or others
  CLEAR mara.
  SELECT SINGLE *
               FROM mara
               WHERE matnr EQ it_headmatnr-matnr
                 AND kzkfg EQ 'X'
                 AND mtart EQ 'FERT'.
  IF sy-subrc EQ 0.
*-----> CHECK COLOR
    PERFORM check_color_for_matnr.
*-----> Check configure variant
    PERFORM check_configure_vari USING it_headmatnr-matnr
                                 CHANGING wa_error_flg.
  ENDIF.

  IF wa_error_flg EQ space.
*----> GENERATE BAPISITEMR
    bapisitemr-material   = it_headmatnr-matnr. "FSC
    bapisitemr-plant      = it_headmatnr-werks. "PLANT
    CLEAR : marc, wa_bedae.
    SELECT SINGLE *
                  FROM marc
                  WHERE werks EQ it_headmatnr-werks
                    AND matnr EQ it_headmatnr-matnr.
    IF marc-strgr EQ '56'.
      wa_bedae = c_reqty56.
      bapisitemr-requ_type  = c_reqty56.            "VSE
    ELSE.
      wa_bedae = c_reqtyot.
      bapisitemr-requ_type  = c_reqtyot.            "VSF
    ENDIF.
    bapisitemr-version    = it_headmatnr-versb. "VERSION
    bapisitemr-vers_activ = space.          "ACTIVE Yes/No
    bapisitemr-req_number = it_headmatnr-pbdnr. "Req plan No


    LOOP AT it_headitem WHERE werks EQ it_headmatnr-werks "PLANT
                          AND versb EQ it_headmatnr-versb "VERSION
                          AND pbdnr EQ it_headmatnr-pbdnr "REQ PLAN No
                          AND matnr EQ it_headmatnr-matnr. "Material

*----> GENERATE SCHEDULE LINE
      it_bapisshdin-date_type  = '3'.      "DATE TYPE ( '3':MONTH)
      it_bapisshdin-req_date   = it_headitem-pdatu.   "DATE
      it_bapisshdin-req_qty    = it_headitem-plnmg.   "QTY
      it_bapisshdin-unit       = mara-meins.          "UNIT
      it_bapisshdin-prod_ves   = it_headitem-pver.    "PROD VERSION
      APPEND it_bapisshdin.
      CLEAR it_bapisshdin.

*----> GENERATE COLOR CHARACTERISTICS
      IF mara-kzkfg EQ 'X'.
        LOOP AT it_syval.
          READ TABLE it_color  WITH KEY valc = it_syval-atwrt.
          it_bapischarr-requ_date  = it_headitem-pdatu.
          it_bapischarr-int_char   = it_color-vtint.
          it_bapischarr-char_value = it_color-slnid.
          it_bapischarr-ch_qty     = it_headitem-plnmg.
          it_bapischarr-fixing     = 'X'.
          it_bapischarr-copy_frmed = 'X'.
          it_bapischarr-flag_usage = 'X'.
          APPEND it_bapischarr.
          CLEAR it_bapischarr.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

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
                   AND a~bedae EQ wa_bedae            "REQUIREMENT TYPE
                   AND a~versb EQ it_headmatnr-versb  "VERSION WA_VERSB
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
*         REQUIREMENT_PARAM        = CM60R
        TABLES
          requirements_schedule_in = it_bapisshdin
          requirements_char_in     = it_bapischarr
          return                   = it_bapireturn.

    ELSE.
      CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
        EXPORTING
          requirements_item        = bapisitemr
*         REQUIREMENT_PARAM        = CM60R
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
        MOVE it_bapireturn-message     TO it_error-msg.
        APPEND it_error.
        CLEAR it_error.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " GENERATE_BAPI_DATA

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
  DATA : l_line_ix   LIKE   sy-tabix,
         l_success_ix    LIKE   sy-tabix,
         l_line          LIKE   sy-tabix.

  DESCRIBE TABLE it_item  LINES l_line_ix.
  IF l_line_ix EQ 0.
    WRITE :/ 'Source data does not exist in Planning Horizon'.
  ELSE.
    WRITE :/ 'PIR for' , wa_success_ix COLOR COL_POSITIVE,
             ' of Material',
             'were created Successfully!!'.
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
*&      Form  CHECK_CONFIGURE_VARI
*&---------------------------------------------------------------------*
FORM check_configure_vari USING p_matnr
                          CHANGING p_error.
  DATA : l_atnam        TYPE   cabn-atnam,
         l_syval_tabix  TYPE   sy-tabix.
  DATA : l_datetime(14) TYPE   n,
         l_valfr        TYPE   v_ibinr-valfr.
  DATA : l_cuobj        TYPE   marc-cuobj.
  DATA : l_cnt          TYPE   sy-index .
  CLEAR : it_syval, it_syval[], wa_in_recno, wa_objnr.

  CONCATENATE 'MA' p_matnr INTO wa_objnr.
  CONCATENATE sy-datum sy-uzeit INTO l_datetime.
  l_valfr = l_datetime.
** IB: Instance (General data+configuration/no admin data)
*  SELECT SINGLE IN_RECNO
*            INTO WA_IN_RECNO
*            FROM V_IBINR
*            WHERE OBJNR EQ WA_OBJNR
*              AND VALFR <= L_VALFR
*              AND VALTO > L_VALFR.

  SELECT SINGLE cuobj
             INTO l_cuobj
             FROM marc
             WHERE matnr EQ p_matnr
               AND werks EQ it_headmatnr-werks.

  SELECT SINGLE in_recno
            INTO wa_in_recno
            FROM ibin
            WHERE instance EQ l_cuobj
              AND valfr <= l_valfr
              AND valto > l_valfr.

  IF sy-subrc EQ 0.
* IB: View from symbol and value
    SELECT atinn
           atwrt
           INTO TABLE it_syval
           FROM v_ibin_syval
           WHERE in_recno EQ wa_in_recno.

    IF sy-subrc EQ 0.
      CLEAR : it_ksml, it_ksml[], inob, kssk.
* Link between Internal Number and Object
      SELECT SINGLE *
                   FROM inob
                   WHERE objek EQ p_matnr.

* Allocation Table: Object to Class
      SELECT SINGLE *
                   FROM kssk
                   WHERE objek EQ inob-cuobj.

* Characteristics of a Class
      SELECT *
             INTO TABLE it_ksml
             FROM ksml
             WHERE clint EQ kssk-clint.

      IF sy-subrc EQ 0.
        DATA l_imerk   LIKE  ksml-imerk.
        SORT it_ksml BY posnr ASCENDING adzhl DESCENDING.
        LOOP AT it_ksml.
          MOVE it_ksml-imerk  TO  l_imerk.
          AT NEW posnr.
            l_cnt = l_cnt + 1.
            IF l_cnt LE 7 .
              READ TABLE it_syval WITH KEY atinn = l_imerk.
              IF sy-subrc NE 0.
                wa_error_ix = wa_error_ix + 1.
                MOVE wa_pbdnr       TO it_error-pbdnr.
                MOVE p_matnr        TO it_error-matnr.
                MOVE 'E'            TO it_error-msgty.
                MOVE it_error-msgty TO p_error.
                MOVE  text-305      TO it_error-msg.
                APPEND it_error.
                CLEAR it_error.
                EXIT.
              ELSE.
                l_syval_tabix = sy-tabix.
                CLEAR l_atnam.
                SELECT SINGLE atnam
                            INTO l_atnam
                            FROM cabn
                            WHERE atinn EQ it_syval-atinn.
                CONCATENATE l_atnam '/' it_syval-atwrt INTO it_syval-atwrt.
                MODIFY it_syval INDEX l_syval_tabix.
              ENDIF.
            ENDIF.

          ENDAT.
        ENDLOOP.
      ELSE.
        wa_error_ix = wa_error_ix + 1.
        MOVE wa_pbdnr       TO it_error-pbdnr.
        MOVE p_matnr        TO it_error-matnr.
        MOVE 'E'            TO it_error-msgty.
        MOVE it_error-msgty TO p_error.
        MOVE text-304       TO it_error-msg.
        APPEND it_error.
        CLEAR it_error.
      ENDIF.
    ELSE.
      wa_error_ix = wa_error_ix + 1.
      MOVE wa_pbdnr       TO it_error-pbdnr.
      MOVE p_matnr        TO it_error-matnr.
      MOVE 'E'            TO it_error-msgty.
      MOVE it_error-msgty TO p_error.
      MOVE text-304       TO it_error-msg.
      APPEND it_error.
      CLEAR it_error.
    ENDIF.
  ELSE.
    wa_error_ix = wa_error_ix + 1.
    MOVE wa_pbdnr       TO it_error-pbdnr.
    MOVE p_matnr        TO it_error-matnr.
    MOVE 'E'            TO it_error-msgty.
    MOVE it_error-msgty TO p_error.
    MOVE text-305       TO it_error-msg.
    APPEND it_error.
    CLEAR it_error.
  ENDIF.
ENDFORM.                    " CHECK_CONFIGURE_VARI
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_MD62
*&---------------------------------------------------------------------*
FORM generate_bdc_md62 USING p_werks.
  DATA : l_datum_low(10),
         l_datum_high(10).
  PERFORM bdc_dynpro  USING 'SAPMM60X'	'0106'.
  PERFORM bdc_field   USING :
	    'BDC_CURSOR'	'RM60X-DATVE',
	    'BDC_OKCODE'	'/00',
	    'AM60X-PBDAW'	'X',
	    'AM60X-PBDNR'	c_versb,  "Q1
	    'RM60X-BERID'	p_werks,
	    'AM60X-WERKS'	p_werks,
	    'AM60X-VERAW'	'X',
	    'RM60X-VERSB'	c_versb,  "Q1
*	    'RM60X-DATVE'	'01/01/2004',
*	    'RM60X-DATBE'	'12/31/2004',
	    'RM60X-ENTLU'	'M'.

  PERFORM bdc_dynpro  USING 'SAPLM60E'	'0200'.
  PERFORM bdc_field   USING :
	    'BDC_OKCODE'	'=ALMK'.

  PERFORM bdc_dynpro  USING 'SAPLM60E'	'0200'.
  PERFORM bdc_field   USING :
	    'BDC_OKCODE'	'=POLO'.

  PERFORM bdc_dynpro  USING 'SAPLSPO1'	'0500'.
  PERFORM bdc_field   USING :
	    'BDC_OKCODE'	'=OPT1'.

  PERFORM bdc_dynpro  USING 'SAPLM60E'	'0200'.
  PERFORM bdc_field   USING :       		
	    'BDC_OKCODE'	'=SICH'.

ENDFORM.                    " GENERATE_BDC_MD62
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MD62
*&---------------------------------------------------------------------*
FORM call_transaction_md62 USING    p_ind.

  CALL TRANSACTION 'MD62' USING it_bdcdata
                      OPTIONS FROM wa_option_ds.
  PERFORM error_text_md USING p_ind.
ENDFORM.                    " CALL_TRANSACTION_MD62
*&---------------------------------------------------------------------*
*&      Form  DEVIDE_WEEK
*&---------------------------------------------------------------------*
FORM devide_week.
  DATA : l_last_date_month      TYPE   scal-date,
         l_last_week_month      TYPE   scal-week,
         l_current_week         TYPE   scal-week.
  DATA : l_last_date_week       TYPE   scal-date.
  DATA : l_total_days           TYPE   sy-tabix,
         l_holidays             TYPE   sy-tabix,
         l_workingdays          TYPE   sy-tabix.

  DATA : lt_scal  LIKE  TABLE OF iscal_day  WITH HEADER LINE.

* Last day at Month of Input Date
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = it_item-pdatu
    IMPORTING
      last_day_of_month = l_last_date_month
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

* Week of Last Month date
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = l_last_date_month
    IMPORTING
      week         = l_last_week_month
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

* Week of Input date
  CALL FUNCTION 'DATE_GET_WEEK'
    EXPORTING
      date         = it_item-pdatu
    IMPORTING
      week         = l_current_week
    EXCEPTIONS
      date_invalid = 1
      OTHERS       = 2.

* Comparing Last Week at Month with Current Week &
* if Current Week is equal to last week at Month,
* you will devide weekly Qtty to daily Qtty &
* sum devided Daily qtty to Monthly Qtty
* but computed month is less than input year-month and
* the monthly qtty is except.

  IF l_last_week_month EQ l_current_week.

    PERFORM last_date_week USING l_current_week
                           CHANGING l_last_date_week.
    CLEAR : lt_scal , lt_scal[].
    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
*         HOLIDAY_CALENDAR                 = ' '
        factory_calendar                 = wa_fabkl
        date_from                        = it_item-pdatu
        date_to                          = l_last_date_week
*       IMPORTING
*        YEAR_OF_VALID_FROM               =
*        YEAR_OF_VALID_TO                 =
*        RETURNCODE                       =
      TABLES
        holidays                         = lt_scal
      EXCEPTIONS
        factory_calendar_not_found       = 1
        holiday_calendar_not_found       = 2
        date_has_invalid_format          = 3
        date_inconsistency               = 4
        OTHERS                           = 5 .


*---> Caculated parameter
    DATA : l_devide_qty    TYPE   pbed-plnmg,
           l_mod_qty       TYPE   sy-tabix  ,
           l_total_qty     TYPE   pbed-plnmg.
    DATA : l_date          TYPE   pbed-pdatu.
    DATA : l_index         TYPE   sy-index  .

    l_total_qty   = it_item-plnmg .
    l_total_days  = l_last_date_week - it_item-pdatu + 1 .
    DESCRIBE TABLE  lt_scal  LINES  l_holidays.
    l_workingdays = l_total_days - l_holidays.
*    L_DEVIDE_QTY  = L_TOTAL_DAYS / L_WORKINGDAYS.
*    L_MOD_QTY     = L_TOTAL_DAYS MOD L_WORKINGDAYS.
    l_devide_qty  = l_total_qty / l_workingdays .
    l_mod_qty     = l_total_qty MOD l_workingdays .
    l_total_qty   = it_item-plnmg.

    CALL FUNCTION 'FIMA_NUMERICAL_VALUE_ROUND'
      EXPORTING
        i_rtype     = '-'
        i_runit     = '1'
        i_value     = l_devide_qty
      IMPORTING
        e_value_rnd = l_devide_qty.

    l_date = it_item-pdatu.
    CLEAR l_index.
    DO .
      IF l_total_qty LE 0.
        EXIT.
      ENDIF.

*---> less than Current date or greater than last date of Current week.
      IF l_date LT it_item-pdatu OR
         l_date GT l_last_date_week.
        EXIT.
      ENDIF.

      READ TABLE r_pdatu INDEX 1.
      READ TABLE lt_scal WITH KEY date = l_date.
      IF sy-subrc NE 0.
        l_index = l_index + 1.
        IF l_date(6) GE p_horiz(6) AND
           l_date(6) LE r_pdatu-high(6).
          IF l_index LE l_mod_qty.
            l_total_qty = l_total_qty - ( l_devide_qty + 1  ).
            it_item-plnmg = l_devide_qty + 1.
          ELSE.
            l_total_qty = l_total_qty - l_devide_qty .
            it_item-plnmg = l_devide_qty.
          ENDIF.
          it_item-pdatu = l_date.
          PERFORM normal_week.
        ELSE.
          IF l_index LE l_mod_qty.
            l_total_qty = l_total_qty - ( l_devide_qty + 1  ).
          ELSE.
            l_total_qty = l_total_qty - l_devide_qty .
          ENDIF.
        ENDIF.
      ENDIF.

      l_date = l_date + 1.
    ENDDO .
  ELSE.
    IF  l_last_date_month(6) GE p_horiz.
      PERFORM normal_week.
    ENDIF.
  ENDIF.
ENDFORM.                    " DEVIDE_WEEK
*&---------------------------------------------------------------------*
*&      Form  NORMAL_WEEK
*&---------------------------------------------------------------------*
FORM normal_week.
  MOVE-CORRESPONDING it_item TO it_headitem.
  MOVE-CORRESPONDING it_item TO it_headmatnr.
*---> First working date in Month
  READ TABLE it_workdate WITH KEY werks = it_item-werks
                                  spmon = it_item-pdatu(6).
  IF sy-subrc EQ 0.
    MOVE it_workdate-datum TO it_headitem-pdatu.
  ENDIF.
*---> Req.plan number
  MOVE : wa_pbdnr TO  it_headmatnr-pbdnr,
         wa_pbdnr TO  it_headitem-pbdnr.

*---> Version
  MOVE : c_versb  TO  it_headmatnr-versb,
         c_versb  TO  it_headitem-versb.

  COLLECT : it_headitem.    CLEAR : it_headitem.
  COLLECT : it_headmatnr.   CLEAR : it_headmatnr.
ENDFORM.                    " NORMAL_WEEK
*&---------------------------------------------------------------------*
*&      Form  LAST_DATE_WEEK
*&---------------------------------------------------------------------*
FORM last_date_week USING    p_current_week
                    CHANGING p_last_date_week.

  DATA : l_last_date  TYPE   scal-date.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      week         = p_current_week
    IMPORTING
      date         = l_last_date
    EXCEPTIONS
      week_invalid = 1
      OTHERS       = 2.

  p_last_date_week = l_last_date + 6.

ENDFORM.                    " LAST_DATE_WEEK
