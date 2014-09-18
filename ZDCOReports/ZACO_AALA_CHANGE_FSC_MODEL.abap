************************************************************************
* Program Name      : ZACO_AALA_CHANGE_FSC_MODEL
* Author            : Chris Li
* Creation Date     : 01/12/2004
* Specifications By :
* Pattern           : Report 1-1
* Development Request No : UD1K913723
* Addl Documentation:
* Description       : THIS PROGRAM CHANGE(RECALCULATE) THE AALA DATA FOR
*                     FSC AND MODEL BASING ON THE QUANTITY AND MARK-UP
*                     RATE CHANGE BY USER.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco_aala_change_fsc_model MESSAGE-ID zmco .


************************************************************************
*      VARIABLE DECLARATION                                            *
************************************************************************
DATA: s_error.
DATA: s_no_match.
DATA: ib_year LIKE  ztco_aala_fsc-gjahr.
DATA: ib_ver LIKE   ztco_aala_fsc-versn.
DATA: ib_mark LIKE  ztco_aala_model-makrt.
DATA: ib_model      LIKE ztco_aala_model-model.
DATA: ib_year_old   LIKE ztco_aala_fsc-gjahr.
DATA: ib_ver_old    LIKE ztco_aala_fsc-versn.
DATA: ib_mark_old   LIKE ztco_aala_model-makrt.
DATA: ib_model_old  LIKE ztco_aala_model-model.
DATA: s_db_changed.
DATA: g_message(100).
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 100.
CONTROLS: tc21 TYPE TABLEVIEW USING SCREEN 101.
CONTROLS: tc22 TYPE TABLEVIEW USING SCREEN 102.
CONTROLS: tc23 TYPE TABLEVIEW USING SCREEN 103.
CONTROLS: tc24 TYPE TABLEVIEW USING SCREEN 104.
DATA: ok_code LIKE sy-ucomm.
DATA: save_code LIKE sy-ucomm.
DATA: s_already_read.
DATA: s_calculated.
************************************************************************
*      INTERNAL TABLES                                                 *
************************************************************************
DATA: it_mip LIKE ztco_aala_mip OCCURS 0 WITH HEADER LINE.
DATA: it_parts LIKE ztco_aala_parts OCCURS 0 WITH HEADER LINE.
DATA: it_fsc_t LIKE ztco_aala_fsc OCCURS 0 WITH HEADER LINE.
DATA: it_model LIKE ztco_aala_model OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_fsc OCCURS 0,
        matnr LIKE ztco_aala_fsc-matnr,
        menge LIKE ztco_aala_fsc-menge,
      END OF it_fsc.

* Data incl. inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE
INCLUDE zaco_aala_change_top .
* Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zaco_aala_change_io .


************************************************************************
*      SELECTION SCREEN                                                *
************************************************************************


************************************************************************
*      EVENTS                                                          *
************************************************************************

START-OF-SELECTION.

  PERFORM showtochange.

END-OF-SELECTION.



************************************************************************
*      FORMS                                                           *
***********************************************************************
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*
* data reaing must be all successfull or nothing changed.
* so all data read into temp table
* CHECK IF THE SAME DATA HAS BEEN READ. IF ACTION IS "SAVE"
* DATA SHOULD NOT BE READ AGAIN.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  DATA: l_text(50).
  DATA: lt_mip LIKE it_mip OCCURS 0 WITH HEADER LINE.
  DATA: lt_fsc_t LIKE it_fsc_t OCCURS 0 WITH HEADER LINE.
  DATA: lt_parts LIKE it_parts OCCURS 0 WITH HEADER LINE.
  DATA: lt_model LIKE it_model OCCURS 0 WITH HEADER LINE.
  DATA: l_mark LIKE ib_mark.

  CLEAR: s_error.
* CHECH THE COMMAND TYPE
  IF ok_code = 'TC2_FC1' OR
     ok_code = 'TC2_FC2' OR
     ok_code = 'TC2_FC3' OR
     ok_code = 'TC2_FC4'.
    EXIT.
  ENDIF.
*  CHECK THE INPUT DATA
  IF  ib_ver IS INITIAL   OR
      ib_year IS INITIAL  OR
      ib_model IS INITIAL.
    l_text = 'Please check the input'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* CHECK THE DATA READING CONDTIONS
  IF ( s_already_read  = 'X'        AND
     ib_ver_old       = ib_ver     AND
     ib_year_old      = ib_year    AND
     ib_model_old     = ib_model )    OR
     ok_code          = 'SAVE' .
    EXIT.
  ENDIF.

* READ THE MODEL DATA FOR CHANGE
  SELECT * INTO TABLE lt_model
    FROM ztco_aala_model
    WHERE gjahr = ib_year AND
          versn = ib_ver  AND
          model = ib_model.
  IF sy-subrc NE 0.
    s_error = 'X'.
    l_text = 'THERE IS NO DATA FOR MODEL'.
*    perform clear_data.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

*  read the mark-up rate
  READ TABLE lt_model INDEX 1.
  l_mark = lt_model-makrt.

* READ THE FSC DATA FOR DISPLAY
  SELECT * INTO TABLE lt_fsc_t
    FROM ztco_aala_fsc
    WHERE gjahr = ib_year AND
          versn = ib_ver.
  IF sy-subrc NE 0.
    s_error = 'X'.
    l_text = 'THERE IS NO DATA FOR FSC'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.


* READ THE MIP PRICES
  SELECT * INTO TABLE lt_mip
   FROM ztco_aala_mip
   WHERE gjahr = ib_year AND
         versn = ib_ver.
  IF sy-subrc NE 0.
    s_error = 'X'.
    l_text = 'THERE IS NO DATA FOR MIP'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* READ THE PARTS DATA FOR CHANGE
  SELECT * INTO TABLE lt_parts
    FROM ztco_aala_parts
    WHERE gjahr = ib_year AND
          versn = ib_ver.
  IF sy-subrc NE 0.
    s_error = 'X'.
    l_text = 'THERE IS NO DATA FOR PARTS'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.


* REPLACE THE OLD DATA WITH NEW DATA
  REFRESH: it_parts, it_mip, it_fsc_t, it_model.
  it_parts[] = lt_parts[].
  it_mip[]   = lt_mip[].
  it_fsc_t[] = lt_fsc_t[].
  it_model[] = lt_model[].
  ib_mark    = l_mark.
  CLEAR: lt_parts[], lt_mip[], lt_fsc_t[], lt_model[].

* READING FSC LIST
  CLEAR: it_fsc,it_fsc[].
  LOOP AT it_fsc_t.
    MOVE-CORRESPONDING it_fsc_t TO it_fsc.
    APPEND it_fsc.
  ENDLOOP.
  SORT it_fsc BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_fsc
    COMPARING matnr.

* SET THE DATA STATUS.
  s_already_read = 'X'.
  ib_ver_old     = ib_ver.
  ib_year_old    = ib_year.
  ib_model_old   = ib_model.
  CLEAR: s_data_changed .

ENDFORM.                    " read_data
*
*&---------------------------------------------------------------------*
*&      Form  SHOWTOCHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM showtochange.
  IF s_error NE 'X'.
    CALL SCREEN 100.
  ENDIF.

ENDFORM.                    " SHOWTOCHANGE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'AALA_STATUS'.
  SET TITLEBAR 'TITLE_100'.
  DESCRIBE TABLE it_fsc LINES tc1-lines.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  tc1_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc1_pai INPUT.
* MODIFY THE QUANTITY FOR IT_FSC AND IT_FSC_T.
  PERFORM check_data_change USING it_fsc.
  MODIFY it_fsc INDEX tc1-current_line.
*      PERFORM MODIFY_FSC_T USING IT_FSC.
ENDMODULE.                 " tc1_pai  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_code = ok_code.
  CLEAR ok_code.
  CASE save_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM update_database.
    WHEN 'CHNG'.
      PERFORM recalculate.


  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATABASE
*&---------------------------------------------------------------------*
*       text
* UPDATE DATA BASE CONDITION:
*    DATA HAS BEEN READ SUCCESSFULLY
*    VERSION, MODEL AND YEAR IS NOT INITIAL
*    ZTCO_AALA_MODEL HAS CORRESPONDING DATA
*    COMPARING EVERY INTERNAL TABLE WITH DATABASE TABLE
*    IF EVERYTHING IS OK, THEN UPDATE THE FOUR TABLES

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_database.
  DATA: l_text(50).
  DATA: answer.
  DATA: i_lines TYPE i.
  DATA: s_fail.
  DATA: l_backup.
* CHECK IF DATA CHANDED
  IF s_data_changed IS INITIAL AND
     s_db_changed IS INITIAL .
    MESSAGE s000 WITH 'NO DATA CHANGE'.
    EXIT.
  ENDIF.
* POP UP FOR CONFIRMATION
  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
       defaultoption = 'Y'
       diagnosetext1 = 'The Database will be Update.'
*       diagnosetext2 = ''
*       diagnosetext3 = ''
       textline1 = 'Are you sure you want to update it ? '
*       TEXTLINE2 = ' '
       titel = 'Please confirm'
      IMPORTING
       answer = answer.
  IF answer NE 'J'.
    EXIT.
  ENDIF.
* CHECK IF THE UPDATE CONDITION ARE MET.
  IF s_already_read NE 'X' OR
     s_data_changed EQ 'X'.
    l_text = 'NO VALID DATA FOR SAVE'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

  IF ib_ver   IS INITIAL OR
     ib_year  IS INITIAL OR
     ib_year  IS INITIAL .
    l_text = 'INPUT VALUE CAN NOT BE INITIAL,NEED RECALCULATION'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* CHECK IF THE INPUT VALUE DATA IS READ
  IF ib_ver NE ib_ver_old   OR
     ib_year NE ib_year_old OR
     ib_model NE ib_model_old.
    l_text = 'INPUT VALUE CHANGED, BUT NO RE-CALCULATION YET'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* CHCEK THE DATABASE DATA
  SELECT  * INTO it_model
    FROM ztco_aala_model
    WHERE gjahr = ib_year AND
          versn = ib_ver  AND
          model = ib_model.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDSELECT.

  IF sy-subrc NE 0.
    l_text = 'THERE IS NO CORRESPONDING MODEL DATA IN DB'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ELSE.
    CLEAR: it_model.
  ENDIF.

* ALL INTERNAL TABLES MUST NOT BE EMPTY
  DESCRIBE TABLE it_model LINES i_lines.
  IF i_lines = 0.
    l_text = 'NO NEW MODEL DATA FOR UPDATE'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.
  DESCRIBE TABLE it_fsc_t LINES i_lines.
  IF i_lines = 0.
    l_text = 'NO NEW FSC DATA FOR UPDATE'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.
  DESCRIBE TABLE it_mip LINES i_lines.
  IF i_lines = 0.
    l_text = 'NO NEW MIP DATA FOR UPDATE'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* BEFROE UPDATE BACKUP THE DATA
  PERFORM backup_data USING l_backup.
  IF l_backup = 'E'.
    l_text = 'DATA BACKUP FAILED. DATA HAS NOT BEEN SAVED'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* UPDATE THE DATABASE.

* ATTACH THE TIME STAMP
  PERFORM attach_time.
  UPDATE ztco_aala_mip FROM TABLE it_mip.
  IF sy-subrc NE 0.
    s_fail = 'X'.
  ENDIF.

  UPDATE ztco_aala_fsc FROM TABLE it_fsc_t.
  IF sy-subrc NE 0.
    s_fail = 'X'.
  ENDIF.

  UPDATE ztco_aala_model FROM TABLE it_model.
  IF sy-subrc NE 0.
    s_fail = 'X'.
  ENDIF.

  IF s_fail NE 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    l_text = 'DATA SAVED !'.
    CLEAR: s_db_changed.
    MESSAGE i000 WITH l_text.
  ELSE.
    s_error = 'X'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    l_text = 'DATA SAVE FAILED!'.
    MESSAGE i000 WITH l_text.
  ENDIF.

ENDFORM.                    " UPDATE_DATABASE
*&---------------------------------------------------------------------*
*&      Form  RECALCULATE
*&---------------------------------------------------------------------*
*       text
* RE-CALCULATION CONDITIONS:
*    DATA HAS BEEN READ SUCCESSFULLY
*    MODEL YEAR, VERSION ARE NOT INITIAL
*    INTERNAL TABLES ARE NOT INITIAL.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recalculate.
  DATA: l_mark LIKE it_model-makrt,
        l_year LIKE it_model-bdatj,
        l_poper LIKE it_model-poper,
        l_zver_des LIKE it_model-zver_des.
  DATA: lt_fsc_qty LIKE ztco_aala_creation_9000_tab
            OCCURS 0 WITH HEADER LINE.
  DATA: lt_fsc_t LIKE it_fsc_t OCCURS 0 WITH HEADER LINE.
  DATA: lt_mip   LIKE it_mip OCCURS 0 WITH HEADER LINE.
  DATA: lt_model LIKE it_model OCCURS 0 WITH HEADER LINE.
  DATA: l_text(50).

* CHECK THE RECALCULATION CONDITIONS
  IF ib_ver   IS INITIAL    OR
     ib_model IS INITIAL  OR
     ib_year  IS INITIAL  OR
     s_already_read NE 'X'.

    l_text = 'NO INITIAL DATA FOR RE-CALCULATION'.
    MESSAGE i000 WITH l_text.
    EXIT.
  ENDIF.

* GET THE PARAMETERS
  READ TABLE it_model INDEX 1.
  l_mark = it_model-makrt.
  l_year = it_model-bdatj.
  l_poper = it_model-poper.
  l_zver_des = it_model-zver_des.
* GET THE FSC QUANTITY
  CLEAR: lt_fsc_qty, lt_fsc_qty[].
  LOOP AT it_fsc.
    lt_fsc_qty-matnr = it_fsc-matnr.
    lt_fsc_qty-menge = it_fsc-menge.
    APPEND lt_fsc_qty.
  ENDLOOP.
* GET THE MIP INTERNAL TABLE
  lt_mip[]   = it_mip[].
*  LT_FSC_T[] = IT_FSC_T[].
*  LT_MODEL[] = IT_MODEL[].

  CALL FUNCTION 'Z_FCO_AALA_CALC_FSC_MODEL'
       EXPORTING
            i_makrt        = ib_mark
            i_bdatj        = ib_year
            i_poper        = l_poper
            i_zver_des     = l_zver_des
       TABLES
            t_fsc_qty      = lt_fsc_qty
            t_mip          = lt_mip
            t_fsc          = lt_fsc_t
            t_model        = lt_model
       EXCEPTIONS
            quantity_error = 1.

  IF sy-subrc EQ 0.
    REFRESH it_mip.
    it_mip[] = lt_mip[].
    REFRESH it_fsc_t.
    it_fsc_t[] = lt_fsc_t[].
    REFRESH it_model.
    it_model[] = lt_model[].
    REFRESH: lt_mip, lt_model, lt_fsc_t.
*   RESET THE DATA STATUS
    CLEAR: s_data_changed.
    s_db_changed = 'X'.
    l_text = 'RE-CALCULATION IS SUCCESS'.
    MESSAGE i000 WITH l_text.
  ELSE.
*  OUTPUT MESSAGE.
    l_text = 'FAILED TO DO THE RE-CALCILATION'.
    MESSAGE i000 WITH l_text.
  ENDIF.
ENDFORM.                    " RECALCULATE
*&---------------------------------------------------------------------*
*&      Module  CHECK_AND_READ_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_and_read_data INPUT.
  PERFORM read_data.
ENDMODULE.                 " CHECK_AND_READ_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_FSC_T
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_IT_FSC  text
*----------------------------------------------------------------------*
FORM modify_fsc_t USING    p_fsc LIKE it_fsc_t.
  LOOP AT it_fsc_t WHERE matnr = p_fsc-matnr.
    it_fsc_t-menge = p_fsc-menge.
    MODIFY it_fsc_t.
  ENDLOOP.
ENDFORM.                    " MODIFY_FSC_T
*&---------------------------------------------------------------------*
*&      Form  BACKUP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM backup_data USING p_status.
  DATA: lt_mip LIKE ztco_aala_mip_bk OCCURS 0 WITH HEADER LINE.
  DATA: lt_fsc LIKE ztco_aala_fsc_bk OCCURS 0 WITH HEADER LINE.
  DATA: lt_model LIKE ztco_aala_mdl_bk OCCURS 0 WITH HEADER LINE.
  DATA: s_exist.
  DATA: l_docnum(14).

  CONCATENATE sy-datum sy-uzeit INTO l_docnum.

* GET THE VERSION KEY
  READ TABLE it_model INDEX 1.

* READ DATABASE TABLE
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mip
   FROM ztco_aala_mip
   FOR ALL ENTRIES IN it_mip
   WHERE matnr = it_mip-matnr AND
         versn = it_model-versn AND
         gjahr = it_model-gjahr AND
         shop  = it_mip-shop.
  IF sy-subrc NE 0.
    p_status = 'E'.
    EXIT.
  ENDIF.
* SET THE DOC NUMBER
  READ TABLE lt_mip INDEX 1.
  lt_mip-bkdoc = l_docnum.
  MODIFY lt_mip FROM lt_mip TRANSPORTING bkdoc
         WHERE gjahr = lt_mip-gjahr.

* BACKUP THE DATA
  INSERT ztco_aala_mip_bk FROM TABLE lt_mip.
  IF sy-subrc NE 0.
    p_status = 'E'.
    PERFORM rollback_work.
    EXIT.
  ENDIF.

* READ DATABASE
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_fsc
   FROM ztco_aala_fsc
   FOR ALL ENTRIES IN it_fsc_t
   WHERE matnr = it_fsc_t-matnr AND
         versn = it_model-versn AND
         gjahr = it_model-gjahr .
  IF sy-subrc NE 0.
    p_status = 'E'.
    EXIT.
  ENDIF.
* GET THE DOC NUMBER
  READ TABLE lt_fsc INDEX 1.
  lt_fsc-bkdoc = l_docnum.
  MODIFY lt_fsc FROM lt_fsc TRANSPORTING bkdoc
        WHERE gjahr = lt_fsc-gjahr.

* BACKUP THE DATA
  INSERT ztco_aala_fsc_bk FROM TABLE lt_fsc.
  IF sy-subrc NE 0.
    p_status = 'E'.
    PERFORM rollback_work.
    EXIT.
  ENDIF.

* READ DATABASE
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_model
   FROM ztco_aala_model
   FOR ALL ENTRIES IN it_model
   WHERE kokrs = it_model-kokrs AND
         model = it_model-model AND
         versn = it_model-versn AND
         gjahr = it_model-gjahr .
  IF sy-subrc NE 0.
    p_status = 'E'.
    EXIT.
  ENDIF.

* GET THE DOC NUMBER
  READ TABLE lt_model INDEX 1.
  lt_model-bkdoc = l_docnum.
  MODIFY lt_model FROM lt_model TRANSPORTING bkdoc
         WHERE gjahr = lt_model-gjahr.

* SAVE THE DATA
  INSERT ztco_aala_mdl_bk FROM TABLE lt_model.
  IF sy-subrc NE 0.
    p_status = 'E'.
    PERFORM rollback_work.
    EXIT.
  ENDIF.

  PERFORM commit_work.

ENDFORM.                    " BACKUP_DATA
*&---------------------------------------------------------------------*
*&      Form  ROLLBACK_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rollback_work.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
ENDFORM.                    " ROLLBACK_WORK
*&---------------------------------------------------------------------*
*&      Form  COMMIT_WORK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM commit_work.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait = 'X'.
ENDFORM.                    " COMMIT_WORK
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data_change USING p_fsc LIKE it_fsc.

  DATA: wa_fsc LIKE it_fsc.

  READ TABLE it_fsc INTO wa_fsc WITH KEY matnr = p_fsc-matnr.
  IF wa_fsc-menge NE p_fsc-menge.
    s_data_changed = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_DATA_CHANGE
*&---------------------------------------------------------------------*
*&      Module  CHANGE_STATUS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_status INPUT.
  s_data_changed = 'X'.
ENDMODULE.                 " CHANGE_STATUS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_STATUS1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_status1 INPUT.
  CLEAR: s_already_read.
ENDMODULE.                 " CHANGE_STATUS1  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1080   text
*      -->P_IT_MODEL_GJAHR  text
*      -->P_IT_MODEL_VERSN  text
*----------------------------------------------------------------------*
FORM check_exist USING    p_table
                          p_gjahr
                          p_versn
                          p_exist.

  DATA: l_ver LIKE it_model-versn.

  IF p_table  = 'MIP'.
    SELECT versn INTO l_ver
     FROM ztco_aala_mip_bk
       WHERE gjahr = p_gjahr AND
           versn = p_versn.
      IF sy-subrc EQ 0.
        p_exist = 'X'.
        EXIT.
      ENDIF.
    ENDSELECT.


  ELSEIF p_table = 'FSC'.
    SELECT versn INTO l_ver
      FROM ztco_aala_fsc_bk
      WHERE gjahr = p_gjahr AND
          versn = p_versn.
      IF sy-subrc EQ 0.
        p_exist = 'X'.
        EXIT.
      ENDIF.
    ENDSELECT.


  ELSEIF p_table = 'MODEL'.
    SELECT versn INTO l_ver
      FROM ztco_aala_mdl_bk
      WHERE gjahr = p_gjahr AND
          versn = p_versn.
      IF sy-subrc EQ 0.
        p_exist = 'X'.
        EXIT.
      ENDIF.
    ENDSELECT.

  ENDIF.

ENDFORM.                    " CHECK_EXIST
*&---------------------------------------------------------------------*
*&      Form  clear_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
  CLEAR: it_parts[],
         it_mip[],
         it_fsc[],
         it_model[],
         ib_mark.
ENDFORM.                    " clear_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_lines OUTPUT.
  DESCRIBE TABLE it_parts LINES tc21-lines.
  DESCRIBE TABLE it_mip   LINES tc22-lines.
  DESCRIBE TABLE it_fsc_t LINES tc23-lines.
  DESCRIBE TABLE it_model LINES tc24-lines.
ENDMODULE.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ATTACH_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM attach_time.
  LOOP AT it_mip.
    it_mip-aedat = sy-datum.
    it_mip-aezet = sy-uzeit.
    it_mip-aenam = sy-uname.
    MODIFY it_mip TRANSPORTING aedat aezet aenam.
  ENDLOOP.
  LOOP AT it_fsc_t.
    it_fsc_t-aedat = sy-datum.
    it_fsc_t-aezet = sy-uzeit.
    it_fsc_t-aenam = sy-uname.
    MODIFY it_fsc_t TRANSPORTING aedat aezet aenam.
  ENDLOOP.
  LOOP AT it_model.
    it_model-aedat = sy-datum.
    it_model-aezet = sy-uzeit.
    it_model-aenam = sy-uname.
    MODIFY it_model TRANSPORTING aedat aezet aenam.
  ENDLOOP.

ENDFORM.                    " ATTACH_TIME
