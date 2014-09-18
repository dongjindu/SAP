************************************************************************
* Program Name      : ZRPP_XM_DUPLICATE
* Author            : Daniel
* Creation Date     : 10/07/2011
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************
REPORT zrpp_xm_duplicate .

TABLES: ausp.
DATA : l_msgtxt(100),
       l_result(1),
       w_error(1).

DATA: it_data LIKE TABLE OF ztpp_engine_dup WITH HEADER LINE.
DATA: it_data_asrs LIKE TABLE OF ztpp_engine_dup WITH HEADER LINE.

DATA: w_lines TYPE i,
      w_lines_asrs TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_date FOR sy-datum,
                s_time FOR sy-uzeit.
SELECTION-SCREEN SKIP.
PARAMETERS:  p_rver	LIKE somlreci1-receiver OBLIGATORY
             DEFAULT 'PP_XM_DUP'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM process_data.
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  DATA: BEGIN OF lt_objek OCCURS 0,
            objek    LIKE ausp-objek,
          END OF lt_objek.

  DATA: BEGIN OF lt_ztppvm OCCURS 0,
         p_airbag_no10 LIKE ztppvm-p_airbag_no10,
         p_model LIKE ztppvm-p_model,
         p_body_serial LIKE ztppvm-p_body_serial,
          zedat LIKE ztppvm-zedat,
          zetim  LIKE ztppvm-zetim,
       END OF lt_ztppvm.

  DATA: l_engine LIKE ausp-atwrt,
         l_lines TYPE i,
         l_lines_asrs TYPE i,
         l_cn(1) TYPE n,
         l_text(40).

  FIELD-SYMBOLS : <fs>.

  SELECT p_airbag_no10 p_model p_body_serial zedat zetim
    INTO TABLE lt_ztppvm
    FROM ztppvm
    WHERE zedat IN s_date
      AND zetim IN s_time
      AND  p_airbag_no10 <> ' '
      AND  p_airbag_no10 <> 'PEND'.

  SORT lt_ztppvm BY p_airbag_no10 p_model p_body_serial.
  DELETE ADJACENT DUPLICATES FROM lt_ztppvm
        COMPARING p_airbag_no10.

  LOOP AT lt_ztppvm.
    REFRESH lt_objek.
    CLEAR: it_data.
*    CONCATENATE LT_ZTPPVM-P_MODEL LT_ZTPPVM-P_BODY_SERIAL INTO L_ENGINE
*.
    l_engine = lt_ztppvm-p_airbag_no10.
    SELECT objek INTO TABLE lt_objek
    FROM ausp AS a
    INNER JOIN cabn AS b
    ON a~atinn = b~atinn
    WHERE klart = '002'
      AND b~atnam = 'P_AIRBAG_NO10'
      AND a~atwrt = l_engine.

    DESCRIBE TABLE lt_objek LINES l_lines.

    IF l_lines > 1.
      WRITE: /(18) l_engine.
      WRITE: (10) lt_ztppvm-zedat.
      WRITE: (8) lt_ztppvm-zetim.
      it_data-run_date = sy-datum.
      it_data-vm_zedat = lt_ztppvm-zedat.
      it_data-vm_zetim = lt_ztppvm-zetim.
      it_data-matnr = l_engine.
      it_data-zuser = sy-uname.
      it_data-zsdat = sy-datum.
      it_data-zstim = sy-uzeit.
      l_cn = '1'.
      LOOP AT lt_objek.
        WRITE: (18) lt_objek-objek.
        CONCATENATE 'IT_DATA-VEH' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs>.
        IF sy-subrc = 0.
          <fs> = lt_objek-objek.
        ENDIF.
        l_cn = l_cn + 1.
      ENDLOOP.
      APPEND it_data.
    ENDIF.
  ENDLOOP.

** Save to Z-TABLE
  DESCRIBE TABLE it_data LINES w_lines.

  IF w_lines > 0.
    DELETE ztpp_engine_dup FROM TABLE it_data.
    INSERT ztpp_engine_dup FROM TABLE it_data.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

    PERFORM send_email.
  ENDIF.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.

  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.
  DATA: l_subject TYPE p15_text150,
        l_p_rec_type	LIKE	somlreci1-rec_type.

  IF w_lines > 0.
    MOVE 'Duplicate XM List' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.
    MOVE '================================' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: 'XM NO' TO lt_body+0(18),
          'Date' TO lt_body+18(10),
         'Time' TO  lt_body+28(10),
         'Vehicle - 1' TO lt_body+38(18),
         'Vehicle - 2' TO lt_body+56(18).

    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: '----------------------' TO  lt_body+0(18),
          '----------' TO  lt_body+18(10),
          '----------' TO  lt_body+28(10),
          '-----------------------' TO lt_body+38(18),
          '-----------------------' TO lt_body+56(18).
    APPEND lt_body.
    CLEAR: lt_body.

    LOOP AT it_data.
      MOVE: it_data-matnr TO  lt_body+0(18),
            it_data-vm_zedat TO lt_body+18(10),
            it_data-vm_zetim  TO  lt_body+28(10),
            it_data-veh1 TO lt_body+38(18),
            it_data-veh2 TO lt_body+56(18).
      APPEND lt_body.
    ENDLOOP.

    CLEAR: lt_body.
    APPEND lt_body.
    APPEND lt_body.
    APPEND lt_body.
  ENDIF.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
       EXPORTING
            p_subject  = 'Duplicate XM List'
            p_rec_type = 'C'
            p_receiver = p_rver
       TABLES
            pt_body    = lt_body.

ENDFORM.                    " send_email
