************************************************************************
* Program Name      : ZIPP101U_PMT07JB_A
* Author            : JongOh, Kim
* Creation Date     : 2003.09.03.
* Specifications By : JongOh, Kim
* Pattern           : 5.1.4.1
* Development Request No : UD1K901950
* Addl Documentation:
* Description       : Create Table ZTPP_PMT07JB_A
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/16/07   Furong       UD1K940120   FO checking and change number
*                                      if there are duplicate number
************************************************************************
REPORT zipp101u_pmt07jb_a NO STANDARD PAGE HEADING
                          LINE-SIZE 200
                          MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztpp_pmt07jb,      "Sequencing Result (D~D+3)
         ztpp_pmt07jb_a,    "SUMMARIZED PMT07JB
         ztpp_common_vals.  "PP: Common Values

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_pmt07jb         LIKE TABLE OF ztpp_pmt07jb   WITH HEADER LINE,
       it_pmt07jb_a       LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE,
       it_7jb             LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
       it_cfile           LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_cfile2          LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_item            LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_dfile           LIKE TABLE OF zspp_pmt07jb_d WITH HEADER LINE,
       it_dfile2          LIKE TABLE OF zspp_pmt07jb_d WITH HEADER LINE,
       it_wohd            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_log             LIKE TABLE OF ztca_if_log    WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_pmt07jb_a_ix    LIKE  sy-tabix,
       wa_pmt07jb_ix      LIKE  sy-tabix,
       wa_error_ix        LIKE  sy-tabix,
       wa_maxsqdt_dt      LIKE  sy-datum.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS : c_mark   VALUE 'X'.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_jobc  LIKE  tbtcjob-jobcount NO-DISPLAY.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (7) text-100.
PARAMETERS: wa_check  AS CHECKBOX DEFAULT 'X'. " RADIOBUTTON  GROUP ra.
SELECTION-SCREEN COMMENT  (25) text-101 FOR FIELD wa_check.
*PARAMETERS: r2 RADIOBUTTON GROUP ra.
*SELECTION-SCREEN COMMENT  (25) text-102 FOR FIELD r2.   "Re-process
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2.
PARAMETERS : p_fo(1).
SELECTION-SCREEN END OF BLOCK b2.
************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_process.
  CHECK wa_check = 'X'.
  PERFORM write_start.
  PERFORM select_pmt07jb.
  " After Processing.. If the result is ok, and then Update the
  " field DATES in the ZTPP_COMMON_VALS Table...
  " --> After the Sequencing Process.......
  PERFORM separate_records.
  PERFORM write_end.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SELECT_PMT07JB
*&---------------------------------------------------------------------*
FORM select_pmt07jb.
  DATA: l_dates LIKE sy-datum.
  DATA: l_tabix TYPE sy-tabix.

  CLEAR : it_pmt07jb,   it_pmt07jb[],
          it_pmt07jb_a, it_pmt07jb_a[].

  SELECT SINGLE sqdt
    INTO l_dates
    FROM ztpp_pmt07jb.

  IF sy-subrc <> 0.
    MESSAGE i001 WITH 'No Data in Input File(PMT07JB)!'.
    WRITE:/ 'No Data in Input File(PMT07JB)!'.
    PERFORM write_end.
    STOP.
  ENDIF.

  SELECT SINGLE *
         FROM ztpp_common_vals
         WHERE jobs EQ sy-repid.

  SELECT MIN( sqdt ) INTO l_dates
         FROM ztpp_pmt07jb .

  IF l_dates  >  ztpp_common_vals-item1."dates .

** changed by Furong on 03/16/07 >> UD1K940120
** Help desk ticket: 73DI49576A
    IF p_fo = 'x' OR p_fo = 'X'.
      PERFORM change_pmt07jb_fo.
    ENDIF.
** end of change

    SELECT * INTO TABLE it_pmt07jb
      FROM ztpp_pmt07jb.

    PERFORM gathering_data.
  ELSE.
    MESSAGE i001 WITH text-001 .
    DELETE FROM ztpp_pmt07jb CLIENT SPECIFIED WHERE mandt = sy-mandt.
    WRITE:/ 'Sequence Date duplicate - Job is cancelled!'.
    PERFORM write_end.
    STOP.
  ENDIF.

  PERFORM insert_process.
ENDFORM.                    " SELECT_PMT07JB
*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM gathering_data.
  DATA : l_num_nb(2)   TYPE  n.
  DATA : l_msg_tx      LIKE  cfgnl-msglin,
         l_tabix       LIKE  sy-tabix.

  DATA : l_fsc       LIKE  ztpp_wosum-fsc,
         l_version   LIKE  ztpp_wosum-version,
         l_model_year(1),
         l_prod_version(2).

  DATA: l_len TYPE i,
        l_new_dealer(1).

  CLEAR : wa_error_ix.
  DESCRIBE TABLE it_pmt07jb  LINES   wa_pmt07jb_ix.

  LOOP AT it_pmt07jb.
*--->> Changed by BS Bae. 2/6/14
*    L_TABIX = SY-TABIX.
*    MOVE-CORRESPONDING IT_PMT07JB TO IT_PMT07JB_A.
*    CLEAR : L_FSC, L_VERSION.
*** Changed by Furong on 10/10/07 for EBOM
*    L_LEN = STRLEN( IT_PMT07JB-BMDL ).
*    IF L_LEN = 7.
*      CONCATENATE IT_PMT07JB-MOYE      IT_PMT07JB-DIST
*                  IT_PMT07JB-BMDL INTO L_FSC.
*      CONCATENATE L_FSC IT_PMT07JB-OCNN INTO L_FSC SEPARATED BY SPACE.
*    ELSE.
*      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
*           EXPORTING
*                OLD_DEALER = IT_PMT07JB-DIST+3(2)
*           IMPORTING
*                NEW_DEALER = L_NEW_DEALER.
*     CONCATENATE IT_PMT07JB-MOYE      IT_PMT07JB-DIST+0(3) L_NEW_DEALER
*                   IT_PMT07JB-BMDL INTO L_FSC.
*      CONCATENATE L_FSC IT_PMT07JB-OCNN INTO L_FSC.
*    ENDIF.
*    L_VERSION = IT_PMT07JB-VERS .
*    MOVE L_FSC(1)     TO  IT_PMT07JB_A-MOYE.
*    MOVE L_VERSION    TO  L_NUM_NB.
*    MOVE L_NUM_NB     TO  IT_PMT07JB_A-PVER.
*    APPEND IT_PMT07JB_A.  CLEAR IT_PMT07JB_A.
*    CLEAR IT_PMT07JB_A.

    IF it_pmt07jb-moye IS INITIAL.
      MOVE: 'E' TO it_pmt07jb-zresult,
            'Model Year is not available.' TO it_pmt07jb-zmsg.
    ENDIF.

*    SELECT SINGLE fsc INTO l_fsc
*      FROM ztpp_wosum
*     WHERE wo_ser = it_pmt07jb-ordr
*       AND nation = it_pmt07jb-dist(3)
*       AND dealer = it_pmt07jb-dist+3(2)
*       AND extc   = it_pmt07jb-extc
*       AND intc   = it_pmt07jb-intc.
*    IF sy-subrc NE 0.
*      MOVE: 'E' TO it_pmt07jb-zresult,
*            'W/Order is not available in WOSUM.' TO it_pmt07jb-zmsg.
*    ENDIF.

    MODIFY it_pmt07jb.

    CLEAR it_pmt07jb_a.

    MOVE-CORRESPONDING it_pmt07jb TO it_pmt07jb_a.
    MOVE it_pmt07jb-vers TO  it_pmt07jb_a-pver.

    l_version = it_pmt07jb-vers .
    MOVE l_version    TO  l_num_nb.
    MOVE l_num_nb     TO  it_pmt07jb_a-pver.

    APPEND it_pmt07jb_a.
*---<< Changed by BS Bae. 2/6/14
  ENDLOOP.

*--> Search MIN Sequence date
  SORT it_pmt07jb BY sqdt ssr1 .
  READ TABLE it_pmt07jb INDEX 1.
  MOVE it_pmt07jb-sqdt  TO  ztpp_common_vals-dates.
ENDFORM.                    " GATHERING_DATA

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
*  IF r1 EQ 'X'.
*    PERFORM create_interface_log.
*  ENDIF.
ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  INSERT_PROCESS
*&---------------------------------------------------------------------*
FORM insert_process.
  DATA l_line     LIKE  sy-tabix.

  DESCRIBE TABLE it_pmt07jb_a LINES wa_pmt07jb_a_ix.

  it_pmt07jb_a-erdat  =  sy-datum.
  it_pmt07jb_a-erzet  =  sy-uzeit.
  it_pmt07jb_a-ernam  =  sy-uname.
  it_pmt07jb_a-aedat  =  sy-datum.
  it_pmt07jb_a-aezet  =  sy-uzeit.
  it_pmt07jb_a-aenam  =  sy-uname.

*----> Check Error of Creating ZTPP_PMT07JB_A
  SKIP 1.
  WRITE:/ text-314,
          wa_pmt07jb_ix   COLOR COL_NORMAL.
  SKIP 1.

  MODIFY it_pmt07jb_a TRANSPORTING erdat erzet ernam
                                   aedat aezet aenam
                      WHERE mandt EQ sy-mandt.
  DELETE FROM ztpp_pmt07jb_a
         CLIENT SPECIFIED
         WHERE mandt EQ sy-mandt.

  DELETE FROM ztpp_pmt07jb CLIENT SPECIFIED WHERE mandt EQ sy-mandt.
  INSERT ztpp_pmt07jb_a FROM TABLE it_pmt07jb_a.

  IF sy-subrc = 0.
    SKIP 1.
    WRITE:/ text-315,
            wa_pmt07jb_a_ix COLOR COL_NORMAL.
  ELSE.
    SKIP 1.
    MESSAGE i001 WITH text-316 .
    WRITE:/ text-316.
  ENDIF.

  SKIP 1.

  LOOP AT it_pmt07jb WHERE zresult EQ 'E'.
    AT FIRST.
      WRITE:/ '**************** BEGIN OF ERROR LIST ****************'.
    ENDAT.
    l_line = sy-tabix MOD 2.
    IF l_line EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/ it_pmt07jb-sqdt COLOR COL_KEY,
            it_pmt07jb-plnt COLOR COL_KEY,
            it_pmt07jb-line COLOR COL_KEY,
            it_pmt07jb-modl COLOR COL_KEY,
            it_pmt07jb-mtgu COLOR COL_KEY,
            it_pmt07jb-ssr1 COLOR COL_KEY,
            it_pmt07jb-ssr2 COLOR COL_KEY,
            it_pmt07jb-ordr COLOR COL_TOTAL,
            it_pmt07jb-dist COLOR COL_TOTAL,
            it_pmt07jb-extc COLOR COL_TOTAL,
            it_pmt07jb-intc COLOR COL_TOTAL,
            it_pmt07jb-zresult  COLOR COL_NORMAL,
            it_pmt07jb-zmsg(40) COLOR COL_NORMAL.
    AT LAST.
      WRITE:/ '**************** END OF ERROR LIST ****************'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " INSERT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_PMT07JB_A
*&---------------------------------------------------------------------*
*FORM SELECT_PMT07JB_A.
*  SELECT *
*        INTO TABLE IT_PMT07JB_A
*        FROM ZTPP_PMT07JB_A.
*
*ENDFORM.                    " SELECT_PMT07JB_A
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
FORM create_interface_log.
*  CHECK Z_TOTAL <> 0.
*  IMPORT WA_JOB FROM MEMORY ID 'XXYY'.
  it_log-tcode    = 'ZPPI1010'.
*  I_ZTCA_IF_LOG-ZSLNO    = WA_ZTPPER-ZSLNO.
  it_log-jobcount = p_jobc.
  it_log-total    = wa_pmt07jb_ix.
  it_log-error    = wa_pmt07jb_ix - wa_pmt07jb_a_ix.
  it_log-zsucc    = wa_pmt07jb_ix - it_log-error.
  it_log-erdat    = sy-datum. "Created on.
  it_log-erzet    = sy-uzeit. "Created time.
  it_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = it_log
*    IMPORTING
*      E_ZTCA_IF_LOG     =
    EXCEPTIONS
      update_failed              = 1
      number_range_error         = 2
      tcode_does_not_exist       = 3
      OTHERS                     = 4.

ENDFORM.                    " CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*&      Form  WRITE_START
*&---------------------------------------------------------------------*
FORM write_start.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
* WRITE :/ 'Create ZTPP_PMT07JB_A'.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 1.
ENDFORM.                    " WRITE_START

*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM write_end.
  FORMAT RESET INTENSIFIED ON.
  GET TIME.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(End)  ' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
* WRITE :/ 'Create ZTPP_PMT07JB_A'.
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " WRITE_END

*&---------------------------------------------------------------------*
*&      Form  SEPARATE_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM separate_records.
  DATA: l_item2               LIKE TABLE OF it_item    WITH HEADER LINE,
        l_date                LIKE sy-datum.

  SELECT SINGLE MAX( sqdt ) INTO l_date
    FROM ztpp_pmt07jb_a
   WHERE gubb = 'A'   .

  " Separated Record from ZTPP_PMT07JB to ZTPP_PMT07JB_B .
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE gubb = '*'   .

  DELETE FROM ztpp_pmt07jb_b WHERE sqdt >= space .
  DESCRIBE TABLE it_7jb LINES wa_pmt07jb_a_ix    .

  MODIFY ztpp_pmt07jb_b FROM TABLE it_7jb .
  IF sy-subrc = 0.
    SKIP 1.
    WRITE:/ text-318,
            wa_pmt07jb_a_ix COLOR COL_NORMAL.
  ELSE.
    SKIP 1.
    MESSAGE i001 WITH text-319 .
    WRITE:/ text-319.
  ENDIF.

  SKIP 1.

  " Separated Record from ZTPP_PMT07JB to ZTPP_PMT07JB_C .
  CLEAR: it_7jb, it_7jb[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE sqdt <= l_date
     AND gubb NE '*' .

  DELETE FROM ztpp_pmt07jb_c  CLIENT SPECIFIED WHERE mandt = sy-mandt .
  DELETE FROM ztpp_pmt07jb_c1 CLIENT SPECIFIED WHERE mandt = sy-mandt .
  PERFORM saving_ztpp_pmt07jb_c .

  " Separated Record from ZTPP_PMT07JB to ZTPP_PMT07JB_D .
  " Summarize the Short-Term for the LTP
  PERFORM summarize_data_short  .
  l_item2[] = it_item[].
  CLEAR: it_item, it_item[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_7jb
    FROM ztpp_pmt07jb_a
   WHERE sqdt > l_date
     AND gubb NE '*'   .

  DELETE FROM ztpp_pmt07jb_d CLIENT SPECIFIED WHERE mandt = sy-mandt .
  PERFORM saving_ztpp_pmt07jb_d .
  PERFORM summarize_data_long  .
  APPEND LINES OF l_item2 TO it_item.
  DESCRIBE TABLE it_item LINES wa_pmt07jb_a_ix   .

  MODIFY ztpp_pmt07jb_d FROM TABLE it_item.
  IF sy-subrc = 0.
    SKIP 1.
    WRITE:/ text-322,
            wa_pmt07jb_a_ix COLOR COL_NORMAL.
  ELSE.
    SKIP 1.
    MESSAGE i001 WITH text-323 .
    WRITE:/ text-323.
  ENDIF.
ENDFORM.                    " SEPARATE_RECORDS

*&---------------------------------------------------------------------*
*&      Form  saving_ztpp_pmt07jb_c
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saving_ztpp_pmt07jb_c.
  DATA: l_lines           TYPE i,
        l_plmng           LIKE ztpp_pmt07jb_c-plnmg .
  DATA: l_len TYPE i,
         l_new_dealer(1).

  LOOP AT it_7jb.
    CLEAR:it_cfile.
    CASE it_7jb-gubb .  "added coding - 2004.05.12
      WHEN 'A' .  it_cfile-prgrs = '1' .
      WHEN 'B' .  it_cfile-prgrs = '2' .
      WHEN 'C' .  it_cfile-prgrs = '3' .
    ENDCASE .
    it_cfile-werks = 'P001' .
    it_cfile-pbdnr = it_7jb-ordr .
    PERFORM call_nation USING it_7jb-dist it_cfile-pbdnr .

*--->> Changed by BS Bae. 2/6/14
** Changed by Furong on 10/09/07 for EBOM
    l_len = strlen( it_7jb-bmdl ).
    IF l_len = 7.
      CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO
      it_cfile-matnr.
      CONCATENATE it_cfile-matnr it_7jb-ocnn INTO it_cfile-matnr
                                             SEPARATED BY space.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
        EXPORTING
          old_dealer = it_7jb-dist+3(2)
        IMPORTING
          new_dealer = l_new_dealer.
      CONCATENATE it_7jb-moye it_7jb-dist+0(3) l_new_dealer it_7jb-bmdl
                    INTO it_cfile-matnr.
      CONCATENATE it_cfile-matnr it_7jb-ocnn INTO it_cfile-matnr.
    ENDIF.
** End of change
*    SELECT SINGLE fsc INTO it_cfile-matnr
*      FROM ztpp_wosum
*     WHERE wo_ser = it_7jb-ordr
*       AND nation = it_7jb-dist(3)
*       AND dealer = it_7jb-dist+3(2)
*       AND extc   = it_7jb-extc
*       AND intc   = it_7jb-intc.
*---<< Changed by BS Bae. 2/6/14

    it_cfile-pdatu = it_7jb-sqdt .
    it_cfile-pdatu = it_7jb-sqdt .
    it_cfile-cogub = 'E'         .
    it_cfile-inexc = it_7jb-extc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-pver .    APPEND it_cfile .

    it_cfile-cogub = 'I'         .
    it_cfile-inexc = it_7jb-intc .
    it_cfile-plnmg = it_7jb-pqty .
    it_cfile-pver  = it_7jb-pver .    APPEND it_cfile .

  ENDLOOP.

  SORT it_cfile BY pdatu pbdnr matnr cogub inexc .

  LOOP AT it_cfile .
    CLEAR it_cfile2 .
    MOVE-CORRESPONDING it_cfile TO it_cfile2.
    COLLECT it_cfile2 .
  ENDLOOP.

  DESCRIBE TABLE it_cfile2 LINES l_lines.
  MODIFY ztpp_pmt07jb_c  FROM TABLE it_cfile2 .
  MODIFY ztpp_pmt07jb_c1 FROM TABLE it_cfile2 .
  IF sy-subrc = 0.
    SKIP 1.
    WRITE:/ text-320,
            l_lines COLOR COL_NORMAL.
  ELSE.
    SKIP 1.
    MESSAGE i001 WITH text-321 .
    WRITE:/ text-321.
  ENDIF.

  SKIP 1.
ENDFORM.                    " saving_ztpp_pmt07jb_c

*&---------------------------------------------------------------------*
*&      Form  call_nation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_7JB_DIST  text
*      -->P_IT_CFILE_PBDNR  text
*----------------------------------------------------------------------*
FORM call_nation USING    pa_dist  pa_pbdnr.
  DATA: l_code               LIKE ztpp_nation_def-n_code .

  CALL FUNCTION 'Z_FPP_NATION_CODE'
    EXPORTING
      dist   = pa_dist
    IMPORTING
      n_code = l_code.

  CONCATENATE pa_pbdnr l_code INTO pa_pbdnr .
ENDFORM.                    " call_nation

*&---------------------------------------------------------------------*
*&      Form  saving_ztpp_pmt07jb_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM saving_ztpp_pmt07jb_d.
  DATA: l_lines           TYPE i,
        l_plmng           LIKE ztpp_pmt07jb_d-plnmg.

  DATA: l_len TYPE i,
        l_new_dealer(1).

  LOOP AT it_7jb.
    CLEAR:it_dfile.
    IF it_7jb-gubb = 'B'.   it_dfile-prgrs = '2' .   ENDIF.
    IF it_7jb-gubb = 'C'.   it_dfile-prgrs = '3' .   ENDIF.
    it_dfile-werks = 'P001' .
    it_dfile-pbdnr = it_7jb-ordr .
    PERFORM call_nation USING it_7jb-dist it_dfile-pbdnr .

*--->> Changed by BS Bae. 2/6/14
*** Changed by Furong on 10/09/07 for EBOM
**    ONCATENATE IT_7JB-MOYE IT_7JB-DIST IT_7JB-BMDL INTO IT_DFILE-MATNR
**.
**   CONCATENATE IT_DFILE-MATNR IT_7JB-OCNN          INTO IT_DFILE-MATNR
**
    l_len = strlen( it_7jb-bmdl ).
    IF l_len = 7.
    CONCATENATE it_7jb-moye it_7jb-dist it_7jb-bmdl INTO it_dfile-matnr.
    CONCATENATE it_dfile-matnr it_7jb-ocnn          INTO it_dfile-matnr
                                                     SEPARATED BY space.
    ELSE.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
        EXPORTING
          old_dealer = it_7jb-dist+3(2)
        IMPORTING
          new_dealer = l_new_dealer.
      CONCATENATE it_7jb-moye it_7jb-dist+0(3) l_new_dealer it_7jb-bmdl
                     INTO it_dfile-matnr.
      CONCATENATE it_dfile-matnr it_7jb-ocnn INTO it_dfile-matnr.
    ENDIF.
*** End of change

*    SELECT SINGLE fsc INTO it_dfile-matnr
*      FROM ztpp_wosum
*     WHERE wo_ser = it_7jb-ordr
*       AND nation = it_7jb-dist(3)
*       AND dealer = it_7jb-dist+3(2)
*       AND extc   = it_7jb-extc
*       AND intc   = it_7jb-intc.
*---<< Changed by BS Bae. 2/6/14

    it_dfile-pdatu = it_7jb-sqdt .
    it_dfile-cogub = 'E'         .
    it_dfile-inexc = it_7jb-extc .
    it_dfile-plnmg = it_7jb-pqty .
    it_dfile-pver  = it_7jb-pver .   APPEND it_dfile .
    it_dfile-cogub = 'I'         .
    it_dfile-inexc = it_7jb-intc .
    it_dfile-plnmg = it_7jb-pqty .
    it_dfile-pver  = it_7jb-pver .   APPEND it_dfile .
  ENDLOOP.

*  SORT it_dfile BY pdatu pbdnr matnr cogub inexc .
*  READ TABLE it_dfile INDEX 1.
*  it_dfile2 = it_dfile .  CLEAR: it_dfile2-plnmg, l_plmng .
*
*  LOOP AT it_dfile .
*    IF it_dfile-pdatu = it_dfile2-pdatu AND
*       it_dfile-matnr = it_dfile2-matnr AND
*       it_dfile-cogub = it_dfile2-cogub AND
*       it_dfile-inexc = it_dfile2-inexc .
*      l_plmng = it_dfile-plnmg + l_plmng.
*      it_dfile2 = it_dfile.
*      it_dfile2-plnmg = l_plmng.
**     APPEND it_dfile2.  " CLEAR: it_dfile2-plnmg.
*    ELSE.
*      it_dfile2-plnmg = l_plmng.
*      APPEND it_dfile2.
*      it_dfile2 = it_dfile.
*      l_plmng = it_dfile-plnmg .
*      CLEAR: it_dfile2-plnmg.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE it_dfile LINES l_lines.
*  IF l_lines > 0 .
*    it_dfile2-plnmg = l_plmng.
*    APPEND it_dfile2.
*  ENDIF.
* MODIFY ztpp_pmt07jb_d FROM TABLE it_dfile2 .
  DATA: lw_dfile LIKE it_dfile.

  SORT it_dfile BY werks matnr pdatu cogub inexc pver.
  LOOP AT it_dfile.
    MOVE: it_dfile TO lw_dfile.

    AT END OF pver.
      SUM.

      MOVE: lw_dfile       TO it_dfile2,
            it_dfile-plnmg TO it_dfile2-plnmg.

      APPEND it_dfile2.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " saving_ztpp_pmt07jb_c

*&---------------------------------------------------------------------*
*&      Form  summarize_data_short
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summarize_data_short.
  DATA: l_matnr             LIKE mara-matnr   ,
        l_inexc             LIKE it_item-inexc,
        l_cogub             TYPE zcogub       ,
        l_plnmg             LIKE it_item-plnmg,
        l_pver              LIKE it_item-pver ,
        l_date              TYPE d            ,
        l_cnt(2)            TYPE n            ,
        l_line              TYPE i            ,
        l_record            LIKE it_item      ,
        l_item              LIKE TABLE OF it_item      WITH HEADER LINE.

*---> After Change code
*  SORT it_cfile2 BY werks matnr cogub inexc pver pdatu DESCENDING.
*  READ TABLE it_cfile2 INDEX 1.
*  it_item = it_cfile2. l_date = it_cfile2-pdatu.
*
*  LOOP AT it_cfile2 .
*    IF it_item-matnr = it_cfile2-matnr AND
*       it_item-inexc = it_cfile2-inexc AND
*       it_item-cogub = it_cfile2-cogub AND
*       it_item-pver  = it_cfile2-pver .
*      CLEAR it_item .
*      MOVE-CORRESPONDING it_cfile2 TO it_item .
*      it_item-pdatu = l_date.
*      it_item-pbdnr = 'MRP_SUM' .
*      COLLECT it_item .
*    ELSE.
*      MOVE-CORRESPONDING it_cfile2 TO it_item .
*      l_date = it_cfile2-pdatu.
*      it_item-pbdnr = 'MRP_SUM' .
*      COLLECT it_item .
*    ENDIF.
*  ENDLOOP .

  SORT it_cfile2 BY werks matnr cogub inexc pver pdatu DESCENDING.
  LOOP AT it_cfile2.
    READ TABLE it_item WITH KEY werks = it_cfile2-werks
                                pbdnr = 'MRP_SUM'
                                matnr = it_cfile2-matnr
                                pdatu = it_cfile2-pdatu
                                cogub = it_cfile2-cogub
                                inexc = it_cfile2-inexc
                                pver  = it_cfile2-pver.
    IF sy-subrc EQ 0.
      it_item-plnmg = it_item-plnmg + it_cfile2-plnmg.

      MODIFY it_item INDEX sy-tabix.
    ELSE.
      MOVE-CORRESPONDING it_cfile2 TO it_item .
      MOVE: 'MRP_SUM' TO it_item-pbdnr.

      APPEND it_item.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SUMMARIZE_DATA_SHORT

*&---------------------------------------------------------------------*
*&      Form  summarize_data_long
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summarize_data_long.
  DATA: l_matnr             LIKE mara-matnr   ,
        l_inexc             LIKE it_item-inexc,
        l_cogub             TYPE zcogub       ,
        l_prgrs             LIKE pbed-entli   ,
        l_pver              LIKE it_item-pver ,
        l_cnt(2)            TYPE n            ,
        l_pdatu             LIKE it_item-pdatu,
        l_plnmg             LIKE it_item-plnmg,
        l_record            LIKE it_item      ,
        l_line              TYPE i            ,
        l_item              LIKE TABLE OF it_item      WITH HEADER LINE.

*  l_item[] = it_dfile2[].
*  SORT l_item BY matnr cogub inexc pdatu .
*  READ TABLE l_item INDEX 1.
*  l_record = l_item .
*  l_matnr  = l_item-matnr.
*  l_inexc = l_item-inexc .
*  l_cogub = l_item-cogub .
*  l_pdatu = l_item-pdatu .
*  l_pver  = l_item-pver  .
*  l_prgrs = l_item-prgrs .
*
*  LOOP AT l_item.
*    IF l_matnr = l_item-matnr AND l_inexc = l_item-inexc AND
*       l_cogub = l_item-cogub AND l_pdatu = l_item-pdatu AND
*       l_pver  = l_item-pver  .
*      " Accumulate the data..
*      l_plnmg = l_plnmg + l_item-plnmg .
*      CONTINUE .
*    ELSE.
*      IF l_matnr = l_item-matnr AND l_inexc = l_item-inexc AND
*         l_cogub = l_item-cogub AND l_pdatu = l_item-pdatu .
*        l_cnt   = l_cnt + 1    .
*        CONCATENATE 'LTP_SUM' l_cnt INTO it_item-pbdnr.
*      ELSE.
*        it_item-pbdnr = 'LTP_SUM' .
*        CLEAR: l_cnt              .
*      ENDIF.
*      it_item-werks = l_record-werks.
*      it_item-pdatu = l_pdatu .         "it_item-pdatu = l_record-pdatu
*      .
*      it_item-matnr = l_matnr .
*      it_item-cogub = l_cogub .
*      it_item-inexc = l_inexc .
*      it_item-pver  = l_pver  .        " it_item-pver  = l_record-pver
*      .
*      it_item-plnmg = l_plnmg .
*      it_item-prgrs = l_prgrs .
*      APPEND it_item .
*      l_record = l_item.
*      l_matnr = l_item-matnr .
*      l_inexc = l_item-inexc .
*      l_cogub = l_item-cogub .
*      l_pdatu = l_item-pdatu .
*      l_pver  = l_item-pver .
*      l_plnmg = l_item-plnmg .
*      l_prgrs = l_item-prgrs .
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE l_item LINES l_line.
*  CHECK l_line > 0 .
*  DESCRIBE TABLE it_item LINES l_line.
*  READ TABLE it_item INDEX l_line    .
*  IF l_matnr = it_item-matnr AND l_inexc = it_item-inexc AND
*     l_cogub = it_item-cogub AND l_pdatu = it_item-pdatu .
*    l_cnt   = l_cnt + 1    .
*    CONCATENATE 'LTP_SUM' l_cnt INTO it_item-pbdnr.
*  ELSE.
*    it_item-pbdnr = 'LTP_SUM' .
*    CLEAR: l_cnt              .
*  ENDIF.
*  it_item-werks = l_record-werks.
*  it_item-pdatu = l_pdatu .
*  it_item-matnr = l_matnr .
*  it_item-cogub = l_cogub .
*  it_item-inexc = l_inexc .
*  it_item-pver  = l_pver  .
*  it_item-plnmg = l_plnmg .
*  it_item-prgrs = l_prgrs .
*  APPEND it_item .

  DATA: lw_plnmg LIKE it_dfile-plnmg.
  DATA: lw_dfile2 LIKE it_dfile2.

  SORT it_dfile2 BY werks matnr pdatu cogub inexc pver.
  LOOP AT it_dfile2.
    MOVE: it_dfile2 TO lw_dfile2.

    AT NEW pver.
      CLEAR: l_cnt.
    ENDAT.

    l_cnt = l_cnt + 1.

    MOVE-CORRESPONDING it_dfile2 TO it_item.

    IF l_cnt = 1.
      it_item-pbdnr = 'LTP_SUM' .
    ELSE.
      CONCATENATE 'LTP_SUM' l_cnt INTO it_dfile2-pbdnr.
    ENDIF.

    APPEND it_item.
  ENDLOOP.
ENDFORM.                    " SUMMARIZE_DATA_LONG
*&---------------------------------------------------------------------*
*&      Form  change_pmt07jb_fo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_pmt07jb_fo.
  DATA: lt_pmt07jb LIKE TABLE OF ztpp_pmt07jb WITH HEADER LINE,
    w_ordr LIKE it_pmt07jb-ordr,
    w_dist LIKE it_pmt07jb-dist,
    w_bmdl LIKE it_pmt07jb-bmdl,
    w_ocnn LIKE it_pmt07jb-ocnn,
    w_count(02) TYPE n,
    w_ordr_new LIKE it_pmt07jb-ordr.

  SELECT * INTO TABLE it_pmt07jb
      FROM ztpp_pmt07jb
      WHERE ordr LIKE 'F%'.

  SORT it_pmt07jb BY ordr dist bmdl ocnn.
  LOOP AT it_pmt07jb.
    lt_pmt07jb-ordr = it_pmt07jb-ordr.
    lt_pmt07jb-dist = it_pmt07jb-dist.
    lt_pmt07jb-bmdl = it_pmt07jb-bmdl.
    lt_pmt07jb-ocnn = it_pmt07jb-ocnn.
    COLLECT lt_pmt07jb.
  ENDLOOP.
  w_count = '01'.
  LOOP AT lt_pmt07jb.

*    w_bmdl = lt_pmt07jb-bmdl.
*    w_ocnn = lt_pmt07jb-ocnn.
*
    IF w_ordr IS INITIAL.
      w_ordr = lt_pmt07jb-ordr.
      w_dist = lt_pmt07jb-dist.
      CONTINUE.
    ELSE.
      IF lt_pmt07jb-ordr = w_ordr AND lt_pmt07jb-dist = w_dist.
        w_ordr_new = w_ordr+4(5).
        CONCATENATE 'FA' w_count w_ordr_new INTO w_ordr_new.
        UPDATE ztpp_pmt07jb SET ordr = w_ordr_new
                  WHERE ordr = lt_pmt07jb-ordr
       AND dist = lt_pmt07jb-dist
       AND bmdl = lt_pmt07jb-bmdl
       AND ocnn = lt_pmt07jb-ocnn.
        w_count = w_count + 1.
        WRITE: /.
        WRITE AT 1 lt_pmt07jb-ordr.
        WRITE AT 11 lt_pmt07jb-dist.
        WRITE AT 21 lt_pmt07jb-bmdl.
        WRITE AT 27 lt_pmt07jb-ocnn.
        WRITE AT 40 w_ordr_new.
      ENDIF.
      w_ordr = lt_pmt07jb-ordr.
      w_dist = lt_pmt07jb-dist.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.
  CLEAR: it_pmt07jb[].

ENDFORM.                    " change_pmt07jb_fo
