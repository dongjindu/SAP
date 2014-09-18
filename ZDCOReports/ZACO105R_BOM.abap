************************************************************************
* Program Name      : ZACO105R_BOM
* Author            : HS JUng
* Creation Date     : 10/03/2006
* Specifications By : Andy Choi
************************************************************************
REPORT zaco105r_bom MESSAGE-ID zmco.


* Top Include
INCLUDE zaco105r_bom_top.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs     MEMORY ID cac. "OBLIGATORY
SELECT-OPTIONS : s_datuv FOR stpo-datuv  OBLIGATORY,
                 s_aennr FOR stpo-aennr.
SELECTION-SCREEN END OF BLOCK bl1.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* 1. Get BOM data
  PERFORM select_bom_data.
* 2. Get BOM Item data
  PERFORM select_stpo.
* 3. Make dispaly data
  PERFORM make_it_dispaly.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM call_alv_list.

*&---------------------------------------------------------------------*
*&      Form  select_bom_Data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_bom_data.

  DATA : l_end(1).
  DATA : it_stas_temp  LIKE stas OCCURS 0 WITH HEADER LINE.
  DATA : it_stas_temp2 LIKE stas OCCURS 0 WITH HEADER LINE.
  DATA : it_mast      LIKE mast OCCURS 0 WITH HEADER LINE.

* Same stlnr stlal aennr stvkn
* but diffrrenct lkenz  one is 'X' and the other is lkenz ='' ) are pair

* 1) Select lkenz = 'X' in Selectiond date and in Selection change no.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_stas_temp
    FROM stas
   WHERE stlty = 'M'
     AND datuv IN s_datuv
     AND aennr IN s_aennr
     AND lkenz = 'X' .

* 2) Select same, but lkenz = ''.
  CHECK NOT it_stas_temp[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_stas_temp2
    FROM stas
     FOR ALL ENTRIES IN it_stas_temp
   WHERE stlty = 'M'
     AND stlnr = it_stas_temp-stlnr
     AND stlal = it_stas_temp-stlal
     AND aennr = it_stas_temp-aennr
     AND stvkn = it_stas_temp-stvkn
     AND stlkn <> it_stas_temp-stlkn
     AND lkenz = ''.

* 3) Delete it_stas_temp if the not paired & Merge paired data
  SORT it_stas_temp2.
  LOOP AT it_stas_temp .
    CLEAR it_stas_temp2.
    READ TABLE it_stas_temp2 WITH KEY stlnr = it_stas_temp-stlnr
                                      stlal = it_stas_temp-stlal
                                      aennr = it_stas_temp-aennr
                                      stvkn = it_stas_temp-stvkn.
    IF sy-subrc <> 0 .
      DELETE it_stas_temp.
    ENDIF.
  ENDLOOP.

* Merge paired data
  LOOP AT it_stas_temp2.
    it_stas_temp = it_stas_temp2.
    APPEND it_stas_temp. CLEAR it_stas_temp.
  ENDLOOP.

  PERFORM get_werks.


* 4) Get BOM Material & werks
  SORT it_stas_temp.
  CHECK NOT it_stas_temp[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mast
    FROM mast
    FOR ALL ENTRIES IN it_stas_temp
   WHERE stlnr = it_stas_temp-stlnr
     AND stlal = it_stas_temp-stlal
     AND stlan = '1'                 "BOM USage (Production)
     AND werks IN r_werks.


  LOOP AT it_stas_temp.
    CLEAR it_mast .
    READ TABLE it_mast WITH KEY stlnr = it_stas_temp-stlnr
                                stlal = it_stas_temp-stlal.

    IF sy-subrc = 0 .
      MOVE-CORRESPONDING it_stas_temp TO it_stas.
      it_stas-matnr = it_mast-matnr.
      it_stas-werks = it_mast-werks.
      COLLECT it_stas. CLEAR it_stas.
    ENDIF.
  ENDLOOP.

  SORT it_stas.


ENDFORM.                    " select_bom_Data
*&---------------------------------------------------------------------*
*&      Form  make_it_dispaly
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_dispaly.
  DATA : l_mtart LIKE mara-mtart.
  DATA : l_subrc TYPE subrc.
  DATA : it_display_temp LIKE it_display OCCURS 0 WITH HEADER LINE.
  DATA : l_chk(1).

*  PERFORM define_changed_type.

*Old  Header or item record was changed
*New  Header or item record generated
*Ind  Item (inactive) ends with AEN
*Inn  Item (inactive) starts with AEN

  LOOP AT it_stas.
    CLEAR it_stpo.
    IF it_stas-lkenz = 'X'.
      clear it_stpo.
      READ TABLE it_stpo WITH KEY stlnr = it_stas-stlnr
                                  stlkn = it_stas-stlkn.
*     Old
      IF it_stpo-datuv LT it_stas-datuv.
        it_display-fmatnr = it_stpo-idnrk.
        l_chk = 'F'.
        it_display-type = 'N'.
      ELSE.
*     Ind
        it_display-tmatnr = it_stpo-idnrk.
        l_chk = 'T'.
        it_display-type = 'I'.
      ENDIF.
    ELSE.
      clear it_stpo.
      READ TABLE it_stpo WITH KEY stlnr = it_stas-stlnr
                                  stlkn = it_stas-stlkn.
*     New
      IF l_chk = 'T'.
        it_display-fmatnr = it_stpo-idnrk.
*     Inn
      ELSE.
        it_display-tmatnr = it_stpo-idnrk.
      ENDIF.
      CLEAR l_chk.
      PERFORM read_it_roh CHANGING l_subrc.
      CHECK l_subrc = 0 .
      it_display-datuv = it_stpo-datuv.
      it_display-aennr = it_stas-aennr.
      it_display-matnr = it_stas-matnr.
      it_display-stlnr = it_stas-stlnr.
      it_display-werks = it_stas-werks.
*     IF it_display-tmatnr <> it_display-Fmatnr.
      APPEND it_display.
*     ENDIF.
      CLEAR it_display.
    ENDIF.
  ENDLOOP.

  it_display_temp[] = it_display[].

* For test : display STATUS
  LOOP AT it_display.
    IF it_display-tmatnr = it_display-fmatnr.
      it_display-status = 'X'.
    ENDIF.

    CLEAR it_display_temp.
    READ TABLE it_display_temp WITH KEY fmatnr = it_display-tmatnr
                                        datuv  = it_display-datuv
                                        matnr  = it_display-matnr.
    IF sy-subrc = 0 .
      it_display-status = 'X'.
    ENDIF.
    MODIFY it_display . CLEAR it_display.

  ENDLOOP.


ENDFORM.                    " make_it_dispaly
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
 'FMATNR'   'Old Material'     '18' 'X' ' '  ' '  ' '  '  ' ' '  ' ',
 'TMATNR'   'New Material'     '18' 'X' ' '  ' '  ' '  '  ' ' '  ' ',
 'DATUV'    'Valid From'       '10' ' ' ' '  ' '  ' '  '  ' ' '  ' ',
 'STLNR'    'PM_STLNR'         '10' ' ' ' '  ' '  ' '  '  ' ' '  ' ',
 'AENNR'    'Change No'        '15' ' ' ' '  ' '  ' '  '  ' ' '  ' ',
 'MATNR'    'BOM'              '18' ' ' ' '  ' '  ' '  '  ' ' '  ' ',
 'STATUS'   'Status'           '6'  ' ' ' '  ' '  ' '  '  ' ' '  ' ',
 'TYPE'     'Type'             '4'  ' ' ' '  ' '  ' '  '  ' ' '  ' '.


*-- Event
  PERFORM alv_get_event    USING  gt_events.


  g_repid = sy-repid.
* CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'

        EXPORTING
             i_callback_program             = g_repid
             it_fieldcat                    = gt_fieldcat
*            I_CALLBACK_PF_STATUS_SET       = C_STATUS_SET
             i_callback_pf_status_set       = c_status_set
             i_callback_user_command        = c_user_command
             i_save             = 'A'
        TABLES
             t_outtab           = it_display
        EXCEPTIONS
             program_error      = 1
             OTHERS             = 2.

ENDFORM.                    " call_alv_list

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
FORM pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.                    " PF_STATUS_SET


*---------------------------------------------------------------------*
*      Form USER_COMMAND
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                                  rs_selfield TYPE slis_selfield.


  CASE r_ucomm.

    WHEN 'SAVE'.
      PERFORM save_ztcou105.
  ENDCASE.

ENDFORM.                    "ALV_EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  alv_get_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*----------------------------------------------------------------------*
FORM alv_get_event USING pt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CLEAR : pt_events[].

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = pt_events.

  READ TABLE pt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE slis_ev_top_of_page     TO ls_event-form.
    APPEND ls_event TO pt_events.
  ENDIF.

  READ TABLE pt_events WITH KEY name =  slis_ev_user_command
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE slis_ev_user_command    TO ls_event-form.
    APPEND ls_event TO pt_events.
  ENDIF.
ENDFORM.                    " alv_get_event
*&---------------------------------------------------------------------*
*&      Form  read_it_roh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_DISPLAY_FMATNR  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM read_it_roh CHANGING p_subrc.


  DATA : l_mtart LIKE mara-mtart.

  CLEAR l_mtart .
  SELECT SINGLE mtart INTO l_mtart FROM mara
   WHERE matnr = it_stpo-idnrk.
  IF l_mtart CS  'ROH' .
    p_subrc = 0 .
  ELSE.
    p_subrc = 4.
  ENDIF.

ENDFORM.                    " read_it_roh

*&---------------------------------------------------------------------*
*&      Form  select_stpo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_stpo.

* Get select BOM Item
  CHECK NOT it_stas[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_stpo
    FROM stpo
    FOR ALL ENTRIES IN it_stas
   WHERE stlty = 'M'
     AND stlnr = it_stas-stlnr
     AND stlkn = it_stas-stlkn.
ENDFORM.                    " select_stpo
*&---------------------------------------------------------------------*
*&      Form  define_changed_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM define_changed_type.
  BREAK-POINT.
  LOOP AT it_stas.
    CLEAR it_stpo.
    READ TABLE it_stpo WITH KEY stlnr = it_stas-stlnr
                                stlkn = it_stas-stlkn.

    IF it_stas-datub LE it_stas-datuv.
      IF it_stpo-stas_aennr = it_stas-aennr.
        SELECT stlty FROM stas
           INTO stas-stlty UP TO 1 ROWS
           WHERE stlty EQ it_stas-stlty
           AND   stlnr EQ it_stas-stlnr
           AND   stlkn EQ it_stpo-stlkn
           AND   stlal EQ it_stas-stlal
           AND   aennr EQ it_stas-aennr
           AND   lkenz EQ 'X'.
        ENDSELECT.
        IF sy-subrc EQ 0.
          it_stas-text = text-015.                           "INA
        ELSE.
          it_stas-text = text-016.                           "INN
        ENDIF.
      ELSE.
        it_stas-text = text-017.                             "INL
      ENDIF.
    ELSE.
      IF it_stpo-aennr = it_stas-aennr.
        it_stas-text = text-011.                             "NEU
      ELSE.
        IF NOT it_stpo-stvkn IS INITIAL.
          SELECT stlty FROM stas
             INTO stas-stlty UP TO 1 ROWS
             WHERE stlty EQ it_stas-stlty
             AND   stlnr EQ it_stas-stlnr
             AND   stlkn NE it_stpo-stlkn
             AND   stlal EQ it_stas-stlal
             AND   stvkn EQ it_stpo-stvkn
             AND   aennr EQ it_stas-aennr.
          ENDSELECT.

          IF sy-subrc EQ 0.
            it_stas-text = text-013.                         "ALT
          ELSE.
            it_stas-text = text-014.                         "LOE
          ENDIF.
*            ELSE.
*               IF STERNTAB-AENB1 IS INITIAL.
*                  POSTAB-TEXT = TEXT-014.                         "LOE
*                  POSTAB-STXT = '2'.
*               ELSE.
*                  POSTAB-TEXT = TEXT-013.                         "NEU
*                  POSTAB-STXT = '3'.
*               ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    MODIFY it_stas. CLEAR it_stas.
  ENDLOOP.

  BREAK-POINT.
ENDFORM.                    " define_changed_type
*&---------------------------------------------------------------------*
*&      Form  save_ztCOU105
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_ztcou105.
  DATA : it_ztcou105 LIKE ztcou105 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_display WHERE status <> 'X'.
    MOVE-CORRESPONDING it_display TO it_ztcou105.
    it_ztcou105-kokrs   = p_kokrs.
    it_ztcou105-gjahr   = it_display-datuv(4).
    it_ztcou105-monat   = it_display-datuv+4(2).
    it_ztcou105-aedat   = sy-datum.
    it_ztcou105-aenam   = sy-uname.
    COLLECT it_ztcou105. CLEAR it_ztcou105.
  ENDLOOP.


  modify ztcou105 FROM TABLE it_ztcou105.

  IF sy-subrc = 0 .
    COMMIT WORK.
    MESSAGE s000 WITH 'Inserted successfully!!!'.
  ENDIF.
ENDFORM.                    " save_ztCOU105
*&---------------------------------------------------------------------*
*&      Form  get_kokrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ZTCOU105_KOKRS  text
*----------------------------------------------------------------------*
FORM get_werks .

  SELECT b~bwkey INTO r_werks-low
    FROM tka02 AS a
   INNER JOIN t001k AS b
     ON  a~bukrs = b~bukrs
  WHERE a~kokrs = p_kokrs.

    r_werks-sign    = 'I'.
    r_werks-option  = 'EQ'.
    COLLECT r_werks.

  ENDSELECT.

ENDFORM.                    " get_kokrs
