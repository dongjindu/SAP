************************************************************************
* Program Name      : ZRPP_ENGINE_DUPLICATE
* Author            : Furong Wang
* Creation Date     : 02/17/2010
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************

REPORT zrpp_engine_duplicate NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

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
             DEFAULT 'PP_ENG_DUP'.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM process_data.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_data.

  DATA: BEGIN OF lt_objek OCCURS 0,
          objek    LIKE ausp-objek,
*          ATWRT    LIKE AUSP-ATWRT,
        END OF lt_objek.

  DATA: BEGIN OF lt_ztppvm OCCURS 0,
         p_engine_no LIKE ztppvm-p_engine_no,
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

** Duplicate Engine
  SELECT p_engine_no p_model p_body_serial zedat zetim
    INTO TABLE lt_ztppvm
    FROM ztppvm
    WHERE zedat IN s_date
      AND zetim IN s_time
* by Daniel on 07/11/11 {
*      AND P_ENGINE_NO <> ' '.
      AND  p_engine_no <> ' '
      AND  p_engine_no <> 'PEND'.
* }

  SORT lt_ztppvm BY p_engine_no p_model p_body_serial.
  DELETE ADJACENT DUPLICATES FROM lt_ztppvm
        COMPARING p_engine_no.

  LOOP AT lt_ztppvm.
    REFRESH lt_objek.
    CLEAR: it_data.
*    CONCATENATE LT_ZTPPVM-P_MODEL LT_ZTPPVM-P_BODY_SERIAL INTO L_ENGINE
*.
    l_engine = lt_ztppvm-p_engine_no.
    SELECT objek INTO TABLE lt_objek
    FROM ausp AS a
    INNER JOIN cabn AS b
    ON a~atinn = b~atinn
    WHERE klart = '002'
      AND b~atnam = 'P_ENGINE_NO'
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

** Added on 06/07/11
** Engine in ASPS already in V/M

  DATA: BEGIN OF lt_ztpperm OCCURS 0,
        eassyid LIKE ztpperm-eassyid,
        zbdat LIKE ztpperm-zbdat,
        zbtim LIKE ztpperm-zbtim,
        END OF lt_ztpperm.


  DATA: l_curr_date LIKE sy-datum,
        l_date LIKE sy-datum,
        l_end_time LIKE sy-uzeit,
        l_st_time LIKE sy-uzeit,
        l_curr_time LIKE sy-uzeit,
        l_difft LIKE sy-uzeit,
        l_duration TYPE i.

  l_curr_date = sy-datum.
  l_curr_time = sy-uzeit.
*  L_DIFFT = '010000'.
  l_duration = -1.

  CONCATENATE l_curr_time+0(2) '59' '59' INTO l_curr_time.

  CALL FUNCTION 'END_TIME_DETERMINE'
   EXPORTING
     duration                         = l_duration
     unit                             = 'H'
*   FACTORY_CALENDAR                 =
   IMPORTING
     end_date                         = l_date
     end_time                         = l_end_time
   CHANGING
     start_date                       = l_curr_date
     start_time                       = l_curr_time
   EXCEPTIONS
     factory_calendar_not_found       = 1
     date_out_of_calendar_range       = 2
     date_not_valid                   = 3
     unit_conversion_error            = 4
     si_unit_missing                  = 5
     parameters_no_valid              = 6
     OTHERS                           = 7
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CONCATENATE l_end_time+0(2) '00' '00' INTO l_st_time.

* by Daniel on 07/21/11 {
*  SELECT eassyid zbdat zbtim
*   INTO TABLE lt_ztpperm
*   FROM ztpperm
*   WHERE ( erpid = 'E06' OR
*           erpid = 'E07' )
*     AND zbdat = l_date
*     AND zbtim BETWEEN l_st_time AND l_end_time
*     AND zresult = 'S'.

  SELECT eassyid zedat zetim
   INTO TABLE lt_ztpperm
   FROM ztpper2
   WHERE ( erpid = 'E06' OR
           erpid = 'E07' )
* by Daniel on 07/22/11 {
     AND PLANT_CD = 'ENG2'
* }
     AND zedat = l_date
     AND zetim BETWEEN l_st_time AND l_end_time.
* }

  SORT lt_ztpperm BY eassyid.
  DELETE ADJACENT DUPLICATES FROM lt_ztpperm
        COMPARING eassyid.


  LOOP AT lt_ztpperm.
    REFRESH lt_objek.
    CLEAR: it_data_asrs.

    l_engine = lt_ztpperm-eassyid.
    SELECT objek INTO TABLE lt_objek
    FROM ausp AS a
    INNER JOIN cabn AS b
    ON a~atinn = b~atinn
    WHERE klart = '002'
      AND b~atnam = 'P_ENGINE_NO'
      AND a~atwrt = l_engine.

    IF sy-subrc = 0.

      WRITE: /(18) l_engine.
      WRITE: (10) lt_ztpperm-zbdat.
      WRITE: (8) lt_ztpperm-zbtim.
      it_data_asrs-run_date = sy-datum.
      it_data_asrs-vm_zedat = lt_ztpperm-zbdat.
      it_data_asrs-vm_zetim = lt_ztpperm-zbtim.
      it_data_asrs-matnr = l_engine.
      it_data_asrs-zuser = sy-uname.
      it_data_asrs-zsdat = sy-datum.
      it_data_asrs-zstim = sy-uzeit.
      l_cn = '1'.
      LOOP AT lt_objek.
        WRITE: (18) lt_objek-objek.
        CONCATENATE 'IT_DATA_ASRS-VEH' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs>.
        IF sy-subrc = 0.
          <fs> = lt_objek-objek.
        ENDIF.
        l_cn = l_cn + 1.
      ENDLOOP.
      APPEND it_data_asrs.
    ENDIF.
  ENDLOOP.
** End on 06/07/11

** Save to Z-TABLE
  DESCRIBE TABLE it_data LINES w_lines.
  DESCRIBE TABLE it_data_asrs LINES w_lines_asrs.

  IF w_lines > 0 OR w_lines_asrs > 0.
    IF w_lines > 0.
      DELETE ztpp_engine_dup FROM TABLE it_data.
      INSERT ztpp_engine_dup FROM TABLE it_data.
    ENDIF.
    IF w_lines_asrs > 0.
      DELETE ztpp_engine_dup FROM TABLE it_data_asrs.
      INSERT ztpp_engine_dup FROM TABLE it_data_asrs.
    ENDIF.

    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    PERFORM send_email.
  ENDIF.
ENDFORM.
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
    MOVE 'Duplicate Engine List' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.
    MOVE '================================' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: 'Engine NO' TO lt_body+0(18),
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

  IF w_lines_asrs > 0.

    MOVE 'Engine in ARSR already in V/M' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.
    MOVE '================================' TO lt_body.
    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: 'Engine NO' TO lt_body+0(18),
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

    LOOP AT it_data_asrs.
      MOVE: it_data_asrs-matnr TO  lt_body+0(18),
            it_data_asrs-vm_zedat TO lt_body+18(10),
            it_data_asrs-vm_zetim  TO  lt_body+28(10),
            it_data_asrs-veh1 TO lt_body+38(18),
            it_data_asrs-veh2 TO lt_body+56(18).
      APPEND lt_body.
    ENDLOOP.
  ENDIF.


  CALL FUNCTION 'ZCAF_SEND_EMAIL'
       EXPORTING
            p_subject  = 'Duplicate Engine List'
            p_rec_type = 'C'
            p_receiver = p_rver
       TABLES
            pt_body    = lt_body.


ENDFORM.                    " send_email
