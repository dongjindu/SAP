************************************************************************
* Program Name      : SAPMZMMPM02_MASTER
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.08.21.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : Supply to Line Master Data
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.08.21.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT  SAPMZMMPM02_MASTER  MESSAGE-ID ZMMM.

**---
INCLUDE : ZRMMPMXXR_INCL.

**---
DATA : BEGIN OF IT_ITAB OCCURS 0.
        INCLUDE STRUCTURE ZTMM_MAST.
DATA :   MAKTX LIKE MAKT-MAKTX,
         DSNAM LIKE T024D-DSNAM,
         W_SELECTED(1),
         W_NEW_ITEM(1),
         W_CHANGE(1),
         MLGT-RDMNG LIKE MLGT-RDMNG,
       END OF IT_ITAB.

DATA : BEGIN OF IT_MAST OCCURS 0,
         W_SELECTED(1),
         MATNR LIKE MARC-MATNR,
         MAKTX LIKE MAKT-MAKTX,
         DISPO LIKE MARC-DISPO,
         DSNAM LIKE T024D-DSNAM,
         WERKS LIKE T001W-WERKS,
       END OF IT_MAST.

DATA : BEGIN OF IT_DELE OCCURS 0.
        INCLUDE STRUCTURE ZTMM_MAST.
DATA : END OF IT_DELE.

DATA : IT_ITAB_COPY LIKE IT_ITAB OCCURS 0 WITH HEADER LINE.

**---
DATA : W_OKCODE LIKE SY-UCOMM,
       W_SAVE_OKCODE LIKE SY-UCOMM,
       W_LOOPC LIKE SY-LOOPC,
       W_TOT_LINES TYPE I,
       W_SELECTED(1).

**---
DATA : W_POSITION TYPE I,
       W_FOUND(1),
       W_FIND_POS TYPE I,
       W_LOOP_FIRST TYPE I.

DATA : WA_TC9000 TYPE CXTAB_COLUMN,
       WA_TC9100 TYPE CXTAB_COLUMN.

**--- Table Control
CONTROLS : TC_9000 TYPE TABLEVIEW USING SCREEN 9000,
           TC_9100 TYPE TABLEVIEW USING SCREEN 9100.

**--- Constants
CONSTANTS : C_ROH LIKE MARA-MTART VALUE 'ROH',
            C_MAX LIKE ZTMM_MAST-ZMNMX VALUE 'MAX',
            C_MIN LIKE ZTMM_MAST-ZMNMX VALUE 'MIN'.

**---
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : S_MATNR FOR MARA-MATNR,
                 S_DISPO FOR MARC-DISPO,
                 S_FEEDR FOR ZTMM_MAST-FEEDR,
                 S_WORKS FOR ZTMM_MAST-WORKS,
                 S_RH_LH FOR ZTMM_MAST-RH_LH,
                 S_SPPTL FOR ZTMM_MAST-SPPTL,
*                 s_zline for ztmm_mast-zline.
                 S_ZLINE FOR PKHD-PRVBE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN : COMMENT (10) TEXT-002,
                   POSITION 33.
PARAMETERS : P_WERKS LIKE T001W-WERKS DEFAULT 'P001' MODIF ID DIS.
SELECTION-SCREEN : POSITION 39.
PARAMETERS : P_NAME1 LIKE T001W-NAME1 MODIF ID DIS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLOCK1.

**---
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'DIS'.
      SCREEN-INPUT = 0.
*      screen-display_3d = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**---
INITIALIZATION.
  PERFORM GET_PLANT_DESC USING P_WERKS.
  MOVE : T001W-NAME1 TO P_NAME1.

**---
START-OF-SELECTION.
  PERFORM GET_CBO_DATA.
  CALL SCREEN 9000.

**---





*&---------------------------------------------------------------------*
*&      Form  get_cbo_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_CBO_DATA.
**---
  data: l_VSPVB like marc-VSPVB.

  CLEAR : IT_ITAB, IT_ITAB[], IT_ITAB_COPY, IT_ITAB_COPY[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ITAB
           FROM ZTMM_MAST
          WHERE WERKS EQ P_WERKS
            AND MATNR IN S_MATNR
            AND DISPO IN S_DISPO
            AND FEEDR IN S_FEEDR
            AND WORKS IN S_WORKS
            AND RH_LH IN S_RH_LH
            AND SPPTL IN S_SPPTL
            AND ZLINE IN S_ZLINE.

*---
  LOOP AT IT_ITAB.
    PERFORM GET_MATERIAL_DESC USING IT_ITAB-MATNR.
    PERFORM GET_MANAGER_DESC USING IT_ITAB-WERKS IT_ITAB-DISPO.
    MOVE : MAKT-MAKTX  TO IT_ITAB-MAKTX,
           T024D-DSNAM TO IT_ITAB-DSNAM.

** Changed by Furong on 01/26/10
*    IF it_itab-zline EQ space.
*      PERFORM get_control_cycle USING it_itab-matnr
*                                      it_itab-werks.
*      IF sy-subrc EQ 0.
*        MOVE : 'X' TO it_itab-w_change.
*      ENDIF.
*    ENDIF.
    SELECT SINGLE VSPVB INTO l_VSPVB
                        FROM MARC
                       WHERE MATNR = IT_ITAB-MATNR
                         AND WERKS = IT_ITAB-WERKS.
    if IT_ITAB-ZLINE <> l_VSPVB.
       IT_ITAB-ZLINE = l_VSPVB.
       MOVE : 'X' TO it_itab-w_change.
    endif.
** End of change

    MODIFY IT_ITAB.

    MOVE-CORRESPONDING IT_ITAB TO IT_ITAB_COPY.
*---
    APPEND IT_ITAB_COPY.
    CLEAR : IT_ITAB, IT_ITAB_COPY, l_VSPVB.
  ENDLOOP.

**---
*  it_itab_copy[] = it_itab[].
ENDFORM.                    " get_cbo_data





*&---------------------------------------------------------------------*
*&      Module  status_scrcom  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_SCRCOM OUTPUT.
**---
  CASE SY-DYNNR.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN '9100'.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
**---
  CASE SY-DYNNR.
    WHEN '9000'.
      CASE SY-UCOMM.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          IF IT_ITAB[] NE IT_ITAB_COPY[].
            PERFORM CONFIRM_STEP.
          ELSE.
            LEAVE TO SCREEN 0.
          ENDIF.
      ENDCASE.
    WHEN '9100'.
      CASE SY-UCOMM.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_scrcom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_SCRCOM INPUT.
**---
  MOVE : W_OKCODE TO W_SAVE_OKCODE.

  CLEAR : W_OKCODE.

  CASE SY-DYNNR.
    WHEN '9000'.
      CASE W_SAVE_OKCODE.
        WHEN 'SAVE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SAVE_ROUTINE.
        WHEN 'LIST'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM MATERIAL_MASTER_LIST.
        WHEN 'DELE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM ITEM_DELETE.
        WHEN 'SALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9000 USING 'X'.
        WHEN 'DALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9000 USING ' '.
        WHEN 'ASCE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM ASCENDING_SORT.
        WHEN 'DESC'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM DESCENDING_SORT.
        WHEN 'FIND' OR 'FIND+'.
          PERFORM FIND_STRING.
          CLEAR : W_SAVE_OKCODE.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM TABLE_CONTROL_PAGE_SCROL USING W_SAVE_OKCODE
                                                 TC_9000-TOP_LINE
                                                 W_TOT_LINES
                                                 W_LOOPC.
          CLEAR : W_SAVE_OKCODE.
      ENDCASE.
    WHEN '9100'.
      CASE W_SAVE_OKCODE.
        WHEN 'CHOS'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM CHOOSE_MATERIAL_MASTER.
        WHEN 'SALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9100 USING 'X'.
        WHEN 'DALL'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM SELECT_DESELECT_ALL_9100 USING ' '.
        WHEN 'ASCE'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM ASCENDING_SORT_9100.
        WHEN 'DESC'.
          CLEAR : W_SAVE_OKCODE.
          PERFORM DESCENDING_SORT_9100.
        WHEN 'FIND' OR 'FIND+'.
          PERFORM FIND_STRING.
          CLEAR : W_SAVE_OKCODE.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM TABLE_CONTROL_PAGE_SCROL USING W_SAVE_OKCODE
                                                 TC_9100-TOP_LINE
                                                 W_TOT_LINES
                                                 W_LOOPC.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  material_master_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MATERIAL_MASTER_LIST.
**---
  CLEAR : IT_MAST, IT_MAST[].

  SELECT B~MATNR
         B~DISPO
         B~WERKS
                 INTO CORRESPONDING FIELDS OF TABLE IT_MAST
                 FROM MARA AS A INNER JOIN MARC AS B
                   ON A~MANDT EQ B~MANDT
                  AND A~MATNR EQ B~MATNR
                WHERE A~MATNR IN S_MATNR
                  AND A~MTART EQ C_ROH
                  AND B~WERKS EQ P_WERKS
                  AND B~DISPO IN S_DISPO
                  AND NOT EXISTS
                      ( SELECT MATNR FROM ZTMM_MAST
                                    WHERE WERKS EQ B~WERKS
                                      AND MATNR EQ B~MATNR ).

*---
  IF NOT IT_DELE[] IS INITIAL.
    LOOP AT IT_DELE.
      MOVE-CORRESPONDING IT_DELE TO IT_MAST.
      APPEND IT_MAST.
      CLEAR : IT_DELE, IT_MAST.
    ENDLOOP.
  ENDIF.

*---
  IF IT_MAST[] IS INITIAL.
    MESSAGE I999 WITH TEXT-M01.
  ELSE.
    LOOP AT IT_MAST.
      PERFORM GET_MATERIAL_DESC USING IT_MAST-MATNR.
      PERFORM GET_MANAGER_DESC USING IT_MAST-WERKS IT_MAST-DISPO.
      MOVE : MAKT-MAKTX TO IT_MAST-MAKTX,
             T024D-DSNAM TO IT_MAST-DSNAM.
      MODIFY IT_MAST.
      CLEAR : IT_MAST.
    ENDLOOP.

    SORT IT_MAST BY MATNR DISPO.

    CALL SCREEN 9100 STARTING AT  14  3
                     ENDING   AT 104 20.
  ENDIF.
ENDFORM.                    " material_master_list

*&---------------------------------------------------------------------*
*&      Form  choose_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHOOSE_MATERIAL_MASTER.
**---
  LOOP AT IT_MAST WHERE W_SELECTED NE SPACE.
    READ TABLE IT_ITAB WITH KEY WERKS = P_WERKS
                                MATNR = IT_MAST-MATNR.
    IF SY-SUBRC NE 0.
      MOVE-CORRESPONDING IT_MAST TO IT_ITAB.
      MOVE : 'X'                 TO IT_ITAB-W_NEW_ITEM.
      CLEAR : IT_ITAB-W_SELECTED.
      APPEND IT_ITAB.
      CLEAR : IT_ITAB, IT_MAST.
    ENDIF.
  ENDLOOP.

*---
  IF IT_MAST[] IS INITIAL.
    MESSAGE I999 WITH TEXT-M06.
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " choose_material_master

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0307   text
*----------------------------------------------------------------------*
FORM SELECT_DESELECT_ALL_9000 USING    P_VALUE.
**---
  MOVE : P_VALUE TO IT_ITAB-W_SELECTED.

  MODIFY IT_ITAB TRANSPORTING W_SELECTED WHERE MATNR NE SPACE.
ENDFORM.                    " select_deselect_all_9000

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0351   text
*----------------------------------------------------------------------*
FORM SELECT_DESELECT_ALL_9100 USING    P_VALUE.
**---
  MOVE : P_VALUE TO IT_MAST-W_SELECTED.

  MODIFY IT_MAST TRANSPORTING W_SELECTED WHERE MATNR NE SPACE.
ENDFORM.                    " select_deselect_all_9100

*&---------------------------------------------------------------------*
*&      Form  save_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_ROUTINE.
**--- Delete
  IF NOT IT_DELE[] IS INITIAL.
    DELETE ZTMM_MAST FROM TABLE IT_DELE.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
      MESSAGE S999 WITH TEXT-M04.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE e999 WITH text-m05.
    ENDIF.
  ENDIF.

**--- Insert & Update
  LOOP AT IT_ITAB WHERE ( W_NEW_ITEM NE SPACE
                       OR W_CHANGE NE SPACE ).
    MOVE-CORRESPONDING IT_ITAB TO ZTMM_MAST.
    MOVE : SY-DATUM            TO ZTMM_MAST-AEDAT,
           SY-UZEIT            TO ZTMM_MAST-AEZET,
           SY-UNAME            TO ZTMM_MAST-AENAM.
    CASE IT_ITAB-W_NEW_ITEM.
      WHEN 'X'.     " new item
        MOVE : SY-DATUM        TO ZTMM_MAST-ERDAT,
               SY-UZEIT        TO ZTMM_MAST-ERZET,
               SY-UNAME        TO ZTMM_MAST-ERNAM.
      WHEN SPACE.   " exist item
    ENDCASE.
    MODIFY ZTMM_MAST.
    CLEAR : IT_ITAB, ZTMM_MAST.
  ENDLOOP.

  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    MESSAGE S999 WITH TEXT-M02.
*  ELSE.
*    ROLLBACK WORK.
*    MESSAGE e999 WITH text-m03.
  ENDIF.

**---
  CLEAR : IT_ITAB, IT_ITAB[], IT_MAST, IT_MAST[], IT_DELE, IT_DELE[],
          IT_ITAB_COPY, IT_ITAB_COPY[].
  LEAVE TO SCREEN 0.
ENDFORM.                    " save_routine

*&---------------------------------------------------------------------*
*&      Form  item_delete
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITEM_DELETE.
**---
  LOOP AT IT_ITAB WHERE W_SELECTED NE SPACE     " selected item
                    AND W_NEW_ITEM EQ SPACE.    " exist item
    MOVE-CORRESPONDING IT_ITAB TO IT_DELE.
    APPEND IT_DELE.
    CLEAR : IT_ITAB, IT_DELE.
  ENDLOOP.

  DELETE IT_ITAB WHERE W_SELECTED NE SPACE.
ENDFORM.                    " item_delete

*&---------------------------------------------------------------------*
*&      Module  display_data_scr9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA_SCR9000 OUTPUT.
**---
  IF SY-STEPL EQ 1.
    CALL FUNCTION 'ME_GET_TC_LINES'
         EXPORTING
              IM_LINES_TOTAL    = W_TOT_LINES
              IM_LINES_PER_PAGE = SY-LOOPC
              IM_TOP_LINE       = TC_9000-TOP_LINE
         IMPORTING
              EX_TC_LINES       = TC_9000-LINES
         EXCEPTIONS
              OTHERS            = 1.
  ENDIF.

**---
  READ TABLE IT_ITAB INDEX TC_9000-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_ITAB TO ZTMM_MAST.
    MOVE : IT_ITAB-W_SELECTED  TO W_SELECTED,
           IT_ITAB-MAKTX       TO MAKT-MAKTX,
           IT_ITAB-DSNAM       TO T024D-DSNAM.
**--- insert by stlim (2004/04/22)
    IF ZTMM_MAST-MEINS EQ SPACE.
      PERFORM GET_UNIT_OF_MEASURE USING ZTMM_MAST-MATNR.
      MOVE : MARA-MEINS          TO ZTMM_MAST-MEINS.
    ENDIF.
**--- end of insert
*    PERFORM get_material_desc USING ztmm_mast-matnr.
*    PERFORM get_manager_desc USING p_werks ztmm_mast-dispo.
** Changed by Furong on 01/27/10
    CLEAR: MLGT-RDMNG.
    IF NOT IT_ITAB-MATNR IS INITIAL.
      SELECT SINGLE RDMNG INTO MLGT-RDMNG
                           FROM MLGT
                          WHERE MATNR = IT_ITAB-MATNR
                            AND LGNUM = 'P01'
                            AND LGTYP = '422'
                            AND LVORM = ' '.
    ENDIF.
** End of change

  ENDIF.

**---
  MOVE : SY-LOOPC TO W_LOOPC.
*---
*  IF ztmm_mast-spptl EQ 'N'.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'ZTMM_MAST-ZMNMX'.
*        screen-input = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.

  IF ZTMM_MAST-SPPTL EQ 'S'.
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'ZTMM_MAST-ZTIME'.
        SCREEN-INPUT = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " display_data_scr9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  get_control_lines_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CONTROL_LINES_9000 OUTPUT.
**---
  DESCRIBE TABLE IT_ITAB LINES W_TOT_LINES.
ENDMODULE.                 " get_control_lines_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  display_data_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_DATA_SCR9100 OUTPUT.
**---
  IF SY-STEPL EQ 1.
    CALL FUNCTION 'ME_GET_TC_LINES'
         EXPORTING
              IM_LINES_TOTAL    = W_TOT_LINES
              IM_LINES_PER_PAGE = SY-LOOPC
              IM_TOP_LINE       = TC_9100-TOP_LINE
         IMPORTING
              EX_TC_LINES       = TC_9100-LINES
         EXCEPTIONS
              OTHERS            = 1.
  ENDIF.

**---
  READ TABLE IT_MAST INDEX TC_9100-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_MAST TO MARC.
    MOVE : IT_MAST-W_SELECTED  TO W_SELECTED,
           IT_MAST-MAKTX       TO MAKT-MAKTX,
           IT_MAST-DSNAM       TO T024D-DSNAM.
*    PERFORM get_material_desc USING marc-matnr.
*    PERFORM get_manager_desc USING p_werks marc-dispo.
  ENDIF.

**---
  MOVE : SY-LOOPC TO W_LOOPC.
ENDMODULE.                 " display_data_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  input_data_modify_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_DATA_MODIFY_SCR9000 INPUT.
**---
  READ TABLE IT_ITAB INDEX TC_9000-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING ZTMM_MAST TO IT_ITAB.
    MOVE : W_SELECTED            TO IT_ITAB-W_SELECTED,
           'X'                   TO IT_ITAB-W_CHANGE.
*---
*    IF it_itab-ztime IS INITIAL AND it_itab-spptl EQ 'N'.
*      SET CURSOR FIELD 'ZTMM_MAST-ZTIME' LINE sy-stepl.
*      MESSAGE e999 WITH text-m10.
*    ENDIF.
*---
    IF IT_ITAB-ZMNMX IS INITIAL AND IT_ITAB-SPPTL EQ 'S'.
      SET CURSOR FIELD 'ZTMM_MAST-ZMNMX' LINE SY-STEPL.
      MESSAGE E999 WITH TEXT-M07.
    ENDIF.
*---
    IF IT_ITAB-ZMNMX EQ C_MAX.
      IF IT_ITAB-LPMIN IS INITIAL.
        SET CURSOR FIELD 'ZTMM_MAST-LPMIN' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M08.
      ENDIF.
    ENDIF.
*---
    IF NOT IT_ITAB-FEED_CYCLE IS INITIAL AND IT_ITAB-SPPTL EQ 'S'.
      IF NOT ( IT_ITAB-FEED_CYCLE EQ '0060'
            OR IT_ITAB-FEED_CYCLE EQ '0120' ).
        SET CURSOR FIELD 'ZTMM_MAST-FEED_CYCLE' LINE SY-STEPL.
        MESSAGE E999 WITH TEXT-M09.
      ENDIF.
    ENDIF.
*---
    MODIFY IT_ITAB INDEX TC_9000-CURRENT_LINE.
  ENDIF.
ENDMODULE.                 " input_data_modify_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  get_control_lines_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CONTROL_LINES_9100 OUTPUT.
**---
  DESCRIBE TABLE IT_MAST LINES W_TOT_LINES.
ENDMODULE.                 " get_control_lines_9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  input_data_modify_scr9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INPUT_DATA_MODIFY_SCR9100 INPUT.
**---
  READ TABLE IT_MAST INDEX TC_9100-CURRENT_LINE.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING MARC TO IT_MAST.
    MOVE : W_SELECTED       TO IT_MAST-W_SELECTED.
    MODIFY IT_MAST INDEX TC_9100-CURRENT_LINE.
  ENDIF.
ENDMODULE.                 " input_data_modify_scr9100  INPUT

*&---------------------------------------------------------------------*
*&      Form  find_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_STRING.
**---
  IF W_SAVE_OKCODE EQ 'FIND'.
    MOVE : 1 TO W_POSITION.
  ELSEIF W_SAVE_OKCODE EQ 'FIND+'.
    W_POSITION = W_LOOP_FIRST + 1.
  ENDIF.

**---
  IF W_SAVE_OKCODE EQ 'FIND'.
    PERFORM POPUP_GET_VALUE(SAPFSFXX) USING    'FSTR' ' '
                                      CHANGING RSDXX-FINDSTR.
  ENDIF.

**---
  IF SY-UCOMM NE 'CANC'.
*---
    CLEAR : W_FOUND.
    IF SY-DYNNR EQ '9000'.
      LOOP AT IT_ITAB FROM W_POSITION.
        IF IT_ITAB CS RSDXX-FINDSTR OR IT_ITAB CP RSDXX-FINDSTR.
          MOVE : 'X' TO W_FOUND.
          MOVE : SY-TABIX TO W_FIND_POS.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF W_FOUND NE SPACE.
        MOVE : 1          TO TC_9000-CURRENT_LINE,
               W_FIND_POS TO TC_9000-TOP_LINE,
               W_FIND_POS TO W_LOOP_FIRST.
      ELSE.
        MESSAGE S042(E2) WITH RSDXX-FINDSTR.
      ENDIF.
*---
    ELSEIF SY-DYNNR EQ '9100'.
      LOOP AT IT_MAST FROM W_POSITION.
        IF IT_MAST CS RSDXX-FINDSTR OR IT_MAST CP RSDXX-FINDSTR.
          MOVE : 'X' TO W_FOUND.
          MOVE : SY-TABIX TO W_FIND_POS.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF W_FOUND NE SPACE.
        MOVE : 1          TO TC_9100-CURRENT_LINE,
               W_FIND_POS TO TC_9100-TOP_LINE,
               W_FIND_POS TO W_LOOP_FIRST.
      ELSE.
        MESSAGE S042(E2) WITH RSDXX-FINDSTR.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " find_string

*&---------------------------------------------------------------------*
*&      Form  confirm_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONFIRM_STEP.
**---
  DATA : L_ANSWER(1).

  CLEAR : L_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION        = 'Y'
      TEXTLINE1            = TEXT-003
*     TEXTLINE2            = ' '
      TITEL                = TEXT-004
      START_COLUMN         = 25
      START_ROW            = 6
      CANCEL_DISPLAY       = 'X'
    IMPORTING
      ANSWER               = L_ANSWER.

  CASE L_ANSWER.
    WHEN 'J'.
      PERFORM SAVE_ROUTINE.
    WHEN 'N'.
      LEAVE TO SCREEN 0.
    WHEN 'A'.
  ENDCASE.
ENDFORM.                    " confirm_step

*&---------------------------------------------------------------------*
*&      Form  get_unit_of_measure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTMM_MAST_MATNR  text
*----------------------------------------------------------------------*
FORM GET_UNIT_OF_MEASURE USING    P_ZTMM_MAST_MATNR.
*---
  CLEAR : MARA.

  SELECT SINGLE MEINS INTO MARA-MEINS
                      FROM MARA
                     WHERE MATNR EQ P_ZTMM_MAST_MATNR.
ENDFORM.                    " get_unit_of_measure

*&---------------------------------------------------------------------*
*&      Form  get_control_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ITAB_MATNR  text
*      -->P_IT_ITAB_WERKS  text
*----------------------------------------------------------------------*
FORM GET_CONTROL_CYCLE USING    P_IT_ITAB_MATNR
                                P_IT_ITAB_WERKS.
*---
  CLEAR : PKHD.

  SELECT SINGLE PRVBE INTO IT_ITAB-ZLINE
                      FROM PKHD
                     WHERE MATNR EQ P_IT_ITAB_MATNR
                       AND WERKS EQ P_IT_ITAB_WERKS.
ENDFORM.                    " get_control_cycle

*&---------------------------------------------------------------------*
*&      Form  ascending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASCENDING_SORT.
*---
  LOOP AT TC_9000-COLS INTO WA_TC9000.
    IF WA_TC9000-SELECTED = 'X'.
      SORT IT_ITAB BY (WA_TC9000-SCREEN-NAME+10) ASCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ascending_sort

*&---------------------------------------------------------------------*
*&      Form  descending_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESCENDING_SORT.
*---
  LOOP AT TC_9000-COLS INTO WA_TC9000.
    IF WA_TC9000-SELECTED = 'X'.
      SORT IT_ITAB BY (WA_TC9000-SCREEN-NAME+10) DESCENDING.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " descending_sort

*&---------------------------------------------------------------------*
*&      Form  ascending_sort_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASCENDING_SORT_9100.
*---
  LOOP AT TC_9100-COLS INTO WA_TC9100.
    IF WA_TC9100-SELECTED = 'X'.
      IF WA_TC9100-SCREEN-NAME EQ 'T024D-DSNAM'.
        SORT IT_MAST BY (WA_TC9100-SCREEN-NAME+6) ASCENDING.
      ELSE.
        SORT IT_MAST BY (WA_TC9100-SCREEN-NAME+5) ASCENDING.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ascending_sort_9100

*&---------------------------------------------------------------------*
*&      Form  descending_sort_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESCENDING_SORT_9100.
*---
  LOOP AT TC_9100-COLS INTO WA_TC9100.
    IF WA_TC9100-SELECTED = 'X'.
      IF WA_TC9100-SCREEN-NAME EQ 'T024D-DSNAM'.
        SORT IT_MAST BY (WA_TC9100-SCREEN-NAME+6) DESCENDING.
      ELSE.
        SORT IT_MAST BY (WA_TC9100-SCREEN-NAME+5) DESCENDING.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " descending_sort_9100
