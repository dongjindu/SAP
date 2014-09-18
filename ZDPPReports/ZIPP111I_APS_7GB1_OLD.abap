************************************************************************
* Program Name      : ZIPP111I_APS_7GB1
* Author            : JongOh, Kim
* Creation Date     : 2003.08.22
* Specifications By : JongOh, Kim
* Pattern           : 5.2.3
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Workorder Information Interface
*                     - ALC and HPCS Code (7GB)
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zipp111i_aps_7gb1 NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : mara,            "General Material Data
         ztpp_pmt07gb,    "ALC and HPCS Code
         ztpp_pmt07ob,    "Selected ALC Code (Inbound Interface)
         ztpp_wosum.      "ERP_WO QTY SUMMARY

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_ztpp_pmt07ob  LIKE TABLE OF ztpp_pmt07ob WITH HEADER LINE,
       it_ztpp_wosum    LIKE TABLE OF ztpp_wosum   WITH HEADER LINE,
       it_ztpp_pmt07gb  LIKE TABLE OF ztpp_pmt07gb WITH HEADER LINE,
       it_wohd          LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_wocl          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA : BEGIN OF it_error OCCURS 0,
         wohd   LIKE  mara-matnr,
         extc   LIKE  ztpp_wosum-extc,
         intc   LIKE  ztpp_wosum-intc,
         type,
         zmsg   LIKE  ztpp_pmt07ob-zmsg.
DATA : END OF it_error.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_line_nb       LIKE sy-tabix.

DATA : wa_error_flg,
       wa_error_ix      TYPE i,
       wa_wosum_ix      TYPE i.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS : c_plnt      LIKE ztpp_pmt07gb-plnt  VALUE '1'.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

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
  CHECK p_run = 'X'  .
  PERFORM excute_process.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM list_process.


*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.

*  PERFORM START_TIME.
*-----> Selected ALC Code (Inbound Interface)
  CLEAR : it_ztpp_pmt07ob, it_ztpp_pmt07ob[].
  SELECT *
         INTO TABLE it_ztpp_pmt07ob
         FROM ztpp_pmt07ob.
  IF sy-subrc EQ 0.
    PERFORM append_pmt07gb.
  ELSE.
    wa_error_flg = 'X'.
    MESSAGE w000 WITH 'No Data in The Table - 07OB'.
  ENDIF.

ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  WOCL_TO_PMT07GB
*&---------------------------------------------------------------------*
FORM wocl_to_pmt07gb USING p_ind p_atnam  p_field.
  CASE p_ind.
    WHEN 'ALC'.
      DATA : l_fname(40).
      FIELD-SYMBOLS: <fs_pmt07gb>.
      CLEAR it_wocl.
      READ TABLE it_wocl WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        CONCATENATE 'IT_ZTPP_PMT07GB' '-' p_field INTO l_fname.
        ASSIGN (l_fname)    TO  <fs_pmt07gb>.
        MOVE it_wocl-atwrt  TO  <fs_pmt07gb>.
      ENDIF.

    WHEN OTHERS.
      CLEAR it_wocl.
      READ TABLE it_wocl WITH KEY atnam = p_atnam.
      IF sy-subrc EQ 0.
        MOVE it_wocl-atwrt  TO  p_field    .
      ENDIF.
  ENDCASE.
ENDFORM.                    " WOCL_TO_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  WOHD_TO_PMT07GB
*&---------------------------------------------------------------------*
FORM wohd_to_pmt07gb USING p_atnam  p_field.
  DATA : l_fname(40).
  FIELD-SYMBOLS: <fs_pmt07gb>.
  CLEAR it_wohd.
  READ TABLE it_wohd WITH KEY atnam = p_atnam.
  IF sy-subrc EQ 0.
    CONCATENATE 'IT_ZTPP_PMT07GB' '-' p_field INTO l_fname.
    ASSIGN (l_fname)    TO  <fs_pmt07gb>.
    MOVE it_wohd-atwrt  TO  <fs_pmt07gb>.
  ENDIF.
ENDFORM.                    " WOHD_TO_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  PART_VALUE
*&---------------------------------------------------------------------*
FORM part_value USING   p_ind.
  DATA : l_index_2(2)   TYPE   n,
         l_index_1(1)   TYPE   n,
         l_index(2)     TYPE   n.
  DATA : l_fname_pmt07ob(40),
         l_fname_pmt07ob1(40),
         l_fname_pmt07gb(40),
         l_field_wo(40),
         l_field_pmt07gb(40).
  DATA : l_field_1a(10),  "PMT07OB UPxx, CPxx FIELD
         l_field_1b(10),  "PMT07GB USERxx, CSERxx FIELD
         l_field_2a(10),  "WOHD, WOCL Charicteristic name
         l_field_2b(10),  "PMT07GB UVALxx, CVALxx FIELD
         l_val1(1)     TYPE    n,
         l_val2(2)     TYPE    n,
         l_val3(3)     TYPE    n.

  FIELD-SYMBOLS : <fs_pmt07ob>,
                  <fs_pmt07gb>.

  CASE p_ind.
    WHEN 'WOHD'.
      MOVE : 'UP'        TO   l_field_1a,
             'USER'      TO   l_field_1b,
             'P_ALC_U_'  TO   l_field_2a,
             'UVAL'      TO   l_field_2b.
    WHEN 'WOCL'.
      MOVE : 'CP'        TO   l_field_1a,
             'CSER'      TO   l_field_1b,
             'P_ALC_C_'  TO   l_field_2a,
             'CVAL'      TO   l_field_2b.
  ENDCASE.

  CLEAR l_index.
  DO.
    l_index_2 = sy-index.
    CONCATENATE 'IT_ZTPP_PMT07OB' '-' l_field_1a l_index_2
                 INTO l_fname_pmt07ob.
    CONCATENATE 'IT_ZTPP_PMT07GB' '-' l_field_1b l_index_2
                 INTO l_fname_pmt07gb.
    ASSIGN (l_fname_pmt07ob)  TO <fs_pmt07ob>.
    ASSIGN (l_fname_pmt07gb)  TO <fs_pmt07gb>.
    IF <fs_pmt07ob> LE 0.
      EXIT.
    ELSE.
      IF l_index EQ '20' AND p_ind EQ 'WOHD'.
        EXIT.
      ELSEIF l_index EQ '10' AND p_ind EQ 'WOCL'.
        EXIT.
      ENDIF.
      <fs_pmt07gb> = <fs_pmt07ob>.
      IF <fs_pmt07gb> LE '009'.
        l_val1 = <fs_pmt07gb>.
        CONCATENATE l_field_2a l_val1 INTO l_field_wo.
      ELSEIF <fs_pmt07gb> GE '100'.
        l_val3 = <fs_pmt07gb>.
        CONCATENATE l_field_2a l_val3 INTO l_field_wo.
      ELSE.
        l_val2 = <fs_pmt07gb>.
        CONCATENATE l_field_2a l_val2 INTO l_field_wo.
      ENDIF.
      CONCATENATE l_field_2b l_index_2  INTO  l_field_pmt07gb.

      CASE p_ind.
        WHEN 'WOHD'.
          PERFORM wohd_to_pmt07gb
                         USING l_field_wo l_field_pmt07gb.
          IF NOT it_wohd-atwrt IS INITIAL.
            l_index = l_index + 1.
          ENDIF.
        WHEN 'WOCL'.
          PERFORM wocl_to_pmt07gb
                         USING 'ALC' l_field_wo l_field_pmt07gb.
          IF NOT it_wocl-atwrt IS INITIAL.
            l_index = l_index + 1.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDDO.
ENDFORM.                    " UNIQUE_PART_VALUE
*&---------------------------------------------------------------------*
*&      Form  PART_VALUE_WITHOUT
*&---------------------------------------------------------------------*
FORM part_value_without USING  p_ind.
  DATA : l_index(2)    TYPE    n,
         l_no          TYPE    i,
         l_val1(1)     TYPE    n,
         l_val2(2)     TYPE    n,
         l_val3(3)     TYPE    n,
         l_index_ix(3) TYPE    n.
  DATA : l_fname_pmt07gb(40),
         l_field_wo(40),
         l_field_pmt07gb(40).
  DATA : l_field_1a(10),  "PMT07OB UPxx, CPxx FIELD
         l_field_1b(10),  "PMT07GB USERxx, CSERxx FIELD
         l_field_2a(10),  "WOHD, WOCL Charicteristic name
         l_field_2b(10).  "PMT07GB UVALxx, CVALxx FIELD
  FIELD-SYMBOLS : <fs_pmt07gb>.

  CASE p_ind.
    WHEN 'WOHD'.
      l_no = 200.
      MOVE : 'USER'      TO   l_field_1b,
             'P_ALC_U_'  TO   l_field_2a,
             'UVAL'      TO   l_field_2b.
    WHEN 'WOCL'.
      l_no = 50.
      MOVE : 'CSER'      TO   l_field_1b,
             'P_ALC_C_'  TO   l_field_2a,
             'CVAL'      TO   l_field_2b.
  ENDCASE.

  l_index = '01'.
  DO 200 TIMES.
    l_index_ix = sy-index.
    CONCATENATE 'IT_ZTPP_PMT07GB' '-' l_field_1b l_index
                 INTO l_fname_pmt07gb.
    ASSIGN (l_fname_pmt07gb)  TO <fs_pmt07gb>.
    <fs_pmt07gb> = l_index_ix.
    IF <fs_pmt07gb> LE '009'.
      l_val1 = <fs_pmt07gb>.
      CONCATENATE l_field_2a l_val1 INTO l_field_wo.
    ELSEIF <fs_pmt07gb> GE '100'.
      l_val3 = <fs_pmt07gb>.
      CONCATENATE l_field_2a l_val3 INTO l_field_wo.
    ELSE.
      l_val2 = <fs_pmt07gb>.
      CONCATENATE l_field_2a l_val2 INTO l_field_wo.
    ENDIF.
    CONCATENATE l_field_2b l_index  INTO  l_field_pmt07gb.

    CASE p_ind.
      WHEN 'WOHD'.
        PERFORM wohd_to_pmt07gb
                       USING l_field_wo l_field_pmt07gb.
        IF NOT it_wohd-atwrt IS INITIAL.
          l_index = l_index + 1.
        ENDIF.
      WHEN 'WOCL'.
        PERFORM wocl_to_pmt07gb
                       USING 'ALC' l_field_wo l_field_pmt07gb.
        IF NOT it_wocl-atwrt IS INITIAL.
          l_index = l_index + 1.
        ENDIF.
    ENDCASE.

    IF l_index EQ '20' AND p_ind EQ 'WOHD'.
      EXIT.
    ELSEIF l_index EQ '10' AND p_ind EQ 'WOCL'.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " PART_VALUE_WITHOUT
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_process.
  DATA: l_text(60) TYPE c.

  IF wa_error_flg EQ 'X'.
*    WRITE :/ TEXT-303.
    MESSAGE w001 WITH text-303.
  ELSE.
    DESCRIBE TABLE it_ztpp_pmt07gb LINES wa_line_nb.
    WRITE wa_line_nb TO l_text LEFT-JUSTIFIED.
    IF wa_line_nb NE 0.
*-------> DELETE ZTPP_PMT07GB & INSERT ZTPP_PMT07GB
      DELETE FROM ztpp_pmt07gb
             CLIENT SPECIFIED
             WHERE mandt EQ sy-mandt.
      INSERT ztpp_pmt07gb FROM TABLE it_ztpp_pmt07gb.
      IF sy-subrc EQ 0.
        COMMIT WORK.
        CONCATENATE 'Created Record Count :' l_text
          INTO l_text.
        MESSAGE s001 WITH l_text .
*        FORMAT COLOR COL_HEADING.
*        WRITE :/ TEXT-311.
        IF sy-batch = ' '.
          MESSAGE s001 WITH text-311.
        ENDIF.
        FORMAT RESET INTENSIFIED ON.
*        WRITE :/5 'Created by' , SY-UNAME .
*        WRITE :/5 'Total Counted: ' , WA_WOSUM_IX COLOR COL_KEY.
*        WRITE :/5 'Created on' , SY-DATUM, SY-UZEIT.
*        WRITE :/5 'Created Data : ' , WA_LINE_NB  COLOR COL_POSITIVE.
*        WRITE :/5 'Errored Data : ' , WA_ERROR_IX COLOR COL_NEGATIVE.
*        SKIP 2.
**-----> Detailed ERROR List
*        PERFORM DETAILED_ERROR.

      ELSE.
        ROLLBACK WORK.
        IF sy-batch = ' '.
          MESSAGE w001 WITH text-303.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*  ELSE.
*    MESSAGE E001 WITH 'Relevant Work Order Summary not exist!!'.
*  ENDIF.
*  PERFORM END_TIME.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  APPEND_PMT07GB
*&---------------------------------------------------------------------*
FORM append_pmt07gb.
  DATA : l_tabix    LIKE   sy-tabix,
         l_idnum    LIKE   sy-tabix,
         l_matnr_cl LIKE   mara-matnr,
         l_matnr_hd LIKE   mara-matnr.
*-----> WORK ORDER SUMMARY TABLE (ZTPP_WOSUM)
  CLEAR : it_ztpp_wosum, it_ztpp_wosum[],
          it_error, it_error[].

  SELECT * INTO TABLE it_ztpp_wosum
    FROM ztpp_wosum AS za
   WHERE za~modqty > 0
     AND za~modqty > za~rp09tq .

  SORT it_ztpp_wosum.
  DESCRIBE TABLE it_ztpp_wosum LINES wa_wosum_ix.

  LOOP AT it_ztpp_wosum.
*-----> CREATE ZTPP_PMT07GB TABLE
    AT NEW dealer.
      CLEAR : l_matnr_hd, it_wohd, it_wohd[].
      CONCATENATE it_ztpp_wosum-wo_ser
                  it_ztpp_wosum-nation
                  it_ztpp_wosum-dealer  INTO  l_matnr_hd.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = l_matnr_hd
                ctype        = '001'
                display      = 'X'
           TABLES
                val_table    = it_wohd
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                OTHERS       = 4.

    ENDAT.
    CLEAR : it_wohd.
*---> CHECK P_PERF_YN
    READ TABLE it_wohd WITH KEY atnam = 'P_PERF_YN'.
    IF it_wohd-atwrt NE 'Y'.
      wa_error_ix = wa_error_ix + 1.
      it_error-wohd = l_matnr_hd.
      it_error-extc = it_ztpp_wosum-extc.
      it_error-intc = it_ztpp_wosum-intc.
      it_error-type = 'E'.
      it_error-zmsg = text-301.
      APPEND it_error.
    ELSE.
      CLEAR : l_matnr_cl, it_wocl, it_wocl[].
      CONCATENATE it_ztpp_wosum-wo_ser  it_ztpp_wosum-nation
                  it_ztpp_wosum-dealer  it_ztpp_wosum-extc
                  it_ztpp_wosum-intc    INTO  l_matnr_cl.

      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                object       = l_matnr_cl
                ctype        = '001'
                display      = 'X'
           TABLES
                val_table    = it_wocl
           EXCEPTIONS
                no_data      = 1
                error_mode   = 2
                error_object = 3
                OTHERS       = 4.

      IF sy-subrc EQ 0.
        CLEAR it_ztpp_pmt07gb.
        MOVE c_plnt  TO    it_ztpp_pmt07gb-plnt. "PLANT
        CONCATENATE it_ztpp_wosum-nation  it_ztpp_wosum-dealer
               INTO it_ztpp_pmt07gb-dist. "DIST(NATION+DEALER)

*------> Exterior Color , Interior Color
        MOVE : it_ztpp_wosum-extc     TO   it_ztpp_pmt07gb-extc,
               it_ztpp_wosum-intc     TO   it_ztpp_pmt07gb-intc.
        PERFORM  wocl_to_pmt07gb
                 USING : ' ' 'P_MI'      it_ztpp_pmt07gb-mo01,  "MODEL
                         ' ' 'P_WO_SER'  it_ztpp_pmt07gb-ordr,  "WORDER
                         ' ' 'P_MI'      it_ztpp_pmt07gb-bmdl,  "MI
                         ' ' 'P_OCN'     it_ztpp_pmt07gb-ocnn,  "OCN
                         ' ' 'P_VERSION' it_ztpp_pmt07gb-vers.  "VERSION

        READ TABLE it_ztpp_pmt07ob
                       WITH KEY plnt = c_plnt   "M
                                modl = it_ztpp_pmt07gb-mo01.
        IF sy-subrc EQ 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
          PERFORM part_value  USING 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
          PERFORM part_value  USING 'WOCL'.
          MOVE : sy-uname   TO  it_ztpp_pmt07gb-zuser,
                 sy-datum   TO  it_ztpp_pmt07gb-zsdat,
                 sy-uzeit   TO  it_ztpp_pmt07gb-zstim.
          APPEND it_ztpp_pmt07gb.
          CLEAR it_ztpp_pmt07gb.

        ELSE.
          READ TABLE it_ztpp_pmt07ob
                         WITH KEY plnt = c_plnt "M
                                  modl = '*'.
          IF sy-subrc EQ 0.
*------>    UNIQUE PART SERIAL FROM P_WOHD
            PERFORM part_value  USING 'WOHD'.
*------>    COLOR PART SERIAL FROM P_WOCL
            PERFORM part_value  USING 'WOCL'.
            MOVE : sy-uname   TO  it_ztpp_pmt07gb-zuser,
                   sy-datum   TO  it_ztpp_pmt07gb-zsdat,
                   sy-uzeit   TO  it_ztpp_pmt07gb-zstim.
            APPEND it_ztpp_pmt07gb.
            CLEAR it_ztpp_pmt07gb.
          ELSE.
**------>    UNIQUE PART SERIAL FROM P_WOHD WITHOUT ZTPP_PMT07OB
*          PERFORM PART_VALUE_WITHOUT USING 'WOHD'.
**------>    COLOR PART SERIAL FROM P_WOCL WITHOUT ZTPP_PMT07OB
*          PERFORM PART_VALUE_WITHOUT USING 'WOCL'.
            wa_error_ix = wa_error_ix + 1.
            it_error-wohd = l_matnr_hd.
            it_error-extc = it_ztpp_wosum-extc.
            it_error-intc = it_ztpp_wosum-intc.
            it_error-type = 'E'.
            it_error-zmsg = text-302.
            APPEND it_error.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " APPEND_PMT07GB
*&---------------------------------------------------------------------*
*&      Form  DETAILED_ERROR
*&---------------------------------------------------------------------*
FORM detailed_error.
  DATA : l_tabix    TYPE   sy-tabix.
  LOOP AT it_error.
    AT FIRST.
      FORMAT INTENSIFIED OFF.
      WRITE:/ '********* BEGIN of Detailed Error List **********'.
    ENDAT.
    l_tabix = sy-tabix MOD 2.
    IF l_tabix EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/ it_error-wohd COLOR COL_KEY,
           it_error-extc COLOR COL_KEY,
           it_error-intc COLOR COL_KEY,
           it_error-type COLOR COL_NEGATIVE,
           it_error-zmsg COLOR COL_NORMAL.
    AT LAST.
      FORMAT INTENSIFIED OFF.
      WRITE:/ '*********  END of Detailed Error List  **********'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " DETAILED_ERROR
*&---------------------------------------------------------------------*
*&      Form  START_TIME
*&---------------------------------------------------------------------*
FORM start_time.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ 'ALC and HPCS Code (7GB)'.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " START_TIME
*&---------------------------------------------------------------------*
*&      Form  END_TIME
*&---------------------------------------------------------------------*
FORM end_time.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(End)' ,
           031(010) sy-datum                    ,
           042(010) sy-uzeit                    .
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " END_TIME
