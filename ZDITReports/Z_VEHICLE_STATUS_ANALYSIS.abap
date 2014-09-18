************************************************************************
* Program Name      : report Z_VEHICLE_STATUS_ANALYSIS
* Author            : IG Moon
* Creation Date     : 2011.5.11
************************************************************************
REPORT  z_plaf_resb_analysis2 MESSAGE-ID zmpp.
TABLES: plaf, resb, ztpp_vm, likp, ztpp_bfst.

DATA: BEGIN OF it_plaf OCCURS 0,
        plnum LIKE plaf-plnum,
        matnr LIKE plaf-matnr,
        psttr LIKE plaf-psttr,
        delv  TYPE c,
        bfst  TYPE c,
        cstat LIKE ztpp_vm-rp_cstatus,
        mbody TYPE equnr,
        flag,
      END OF it_plaf.

DATA: BEGIN OF it_vm OCCURS 0,
        pldord LIKE plaf-plnum,
        cstat LIKE ztpp_vm-rp_cstatus,
        model LIKE ztpp_vm-model_code,
        body  LIKE ztpp_vm-body_no,
      END OF it_vm.

DATA : g_line TYPE i,
       $text(50),
       $line(20).
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if not p_debug is initial.
    break-point.
  endif.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_plnum FOR resb-plnum,
                s_psttr FOR plaf-psttr.

PARAMETERS p_date TYPE sy-datum.

PARAMETERS : p_upd AS CHECKBOX,
             p_debug  TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  p_date = sy-datum - 7.

START-OF-SELECTION.
  PERFORM get_objects.

  DESCRIBE TABLE it_plaf LINES g_line.
  IF g_line > 0.
  ELSE.
    MESSAGE s000 WITH 'No entry was found.'.
    STOP.
  ENDIF.

END-OF-SELECTION.

  u_break.

  $line = g_line.
  IF p_upd EQ true.
    CONCATENATE  '..updating with' $line ' record(s)' INTO $text.
    __process $text '70' .

    LOOP AT it_plaf.
      UPDATE ztpp_bfst SET   sd_deli_flg = 'Y'
                             sd_deli_dat = sy-datum
                             sd_deli_tim = sy-uzeit
                       WHERE plan_ord EQ it_plaf-plnum
                         AND sd_deli_flg EQ space.
      IF sy-subrc EQ 0.
        WRITE:/  it_plaf-plnum.
      ENDIF.
    ENDLOOP.
    COMMIT WORK.

  ELSE.

    CONCATENATE  '..testing with' $line ' record(s)' INTO $text.
    __process $text '70' .

    LOOP AT it_plaf.
      SELECT SINGLE * FROM ztpp_bfst
                       WHERE plan_ord EQ it_plaf-plnum
                         AND sd_deli_flg EQ space.
      IF sy-subrc EQ 0.
        WRITE:/  it_plaf-plnum.
      ENDIF.
    ENDLOOP.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  get_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_objects.

  __cls : it_plaf, it_vm.

  __process '....finding from plaf' '0'.
  SELECT plnum matnr psttr INTO TABLE it_plaf
    FROM plaf
    WHERE plnum IN s_plnum
      AND psttr IN s_psttr
      AND paart = 'PE'
      AND knttp = 'M'.

  DATA:$ix LIKE sy-tabix.
  DATA:l_psttr LIKE sy-datum.

  CHECK NOT it_plaf[] IS INITIAL.
  SELECT pldord rp_cstatus model_code body_no INTO TABLE it_vm
     FROM ztpp_vm
      FOR ALL ENTRIES IN  it_plaf
      WHERE pldord = it_plaf-plnum.

  SORT it_vm BY pldord.

  __process '..finding from ztpp_bfst' '20'.

  LOOP AT it_plaf.
    $ix = sy-tabix.

    SELECT SINGLE sd_deli_flg INTO it_plaf-delv FROM ztpp_bfst
            WHERE plan_ord = it_plaf-plnum.
    IF sy-subrc = 0.
      MODIFY it_plaf INDEX $ix TRANSPORTING delv.
    ELSE.
      it_plaf-bfst = 'X'.
      MODIFY it_plaf INDEX $ix TRANSPORTING bfst.
    ENDIF.

    READ TABLE it_vm WITH KEY pldord = it_plaf-plnum BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE it_vm-model it_vm-body INTO it_plaf-mbody.
      it_plaf-cstat = it_vm-cstat.
      MODIFY it_plaf INDEX $ix TRANSPORTING mbody cstat.
    ENDIF.
  ENDLOOP.

  DELETE it_plaf WHERE cstat <> '25' AND cstat <> '27'.

  __process '..checking with likp' '50' .

  LOOP AT it_plaf.
    $ix = sy-tabix.

    SELECT SINGLE * FROM likp WHERE vbeln EQ it_plaf-mbody.

    IF sy-subrc EQ 0 AND likp-wadat_ist <= p_date
          and not likp-wadat_ist is initial.
    ELSE.
      DELETE it_plaf INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_objects

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
