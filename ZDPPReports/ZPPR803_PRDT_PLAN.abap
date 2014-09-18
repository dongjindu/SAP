************************************************************************
* Program Name      : ZPPR803_PRDT_PLAN
* Author            : Furong Wang
* Creation Date     : 07/24/2006
* Specifications By : MY Hur
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 08/20/10        Daniel           UD1K949688     Version field is added
************************************************************************

REPORT ZPPR803_PRDT_PLAN MESSAGE-ID ZMMM .

TABLES: ZTPP_WOSUM, ZTPP_INPUT_PLAN.

*---// Internal tables

DATA: BEGIN OF IT_INPUT OCCURS 0.
        INCLUDE STRUCTURE ZTPP_INPUT_PLAN.
DATA:   FSC  LIKE MARA-MATNR.
DATA: END OF IT_INPUT.

DATA: BEGIN OF IT_PMT07JB OCCURS 0.
        INCLUDE STRUCTURE ZTPP_PMT07JB_A.
DATA: FSC LIKE ZTPP_WOSUM.
DATA: END OF IT_PMT07JB.

DATA: BEGIN OF IT_DATE OCCURS 100,
      NO(2) TYPE N,
      DATE   LIKE   SY-DATUM,
      END   OF IT_DATE.

DATA: BEGIN OF IT_TAB1 OCCURS 0,
        NATION  LIKE   ZTPP_PMT07JB_A-DIST,
        FSC     LIKE   ZTPP_WOSUM-FSC,
* by Daniel on 08/20/10 {
        VERS    LIKE   ZTPP_PMT07JB_A-VERS,
* }
        EXTC    LIKE   ZTPP_INPUT_PLAN-EXTC,
        INTC    LIKE   ZTPP_INPUT_PLAN-INTC,
        DATE    LIKE   ZTPP_INPUT_PLAN-RSNUM,
        PQTY    LIKE   ZTPP_PMT07JB_A-PQTY,
      END   OF IT_TAB1.

DATA: BEGIN OF IT_TAB OCCURS 0,
        NATION  LIKE   ZTPP_PMT07JB_A-DIST,
        FSC     LIKE   ZTPP_WOSUM-FSC,
* by Daniel on 08/20/10 {
        VERS    LIKE   ZTPP_PMT07JB_A-VERS,
* }
        EXTC    LIKE   ZTPP_INPUT_PLAN-EXTC,
        INTC    LIKE   ZTPP_INPUT_PLAN-INTC,
        DATE    LIKE   ZTPP_INPUT_PLAN-RSNUM,
        QTY01    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY02    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY03    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY04    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY05    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY06    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY07    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY08    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY09    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY10    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY11    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY12    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY13    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY14    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY15    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY16    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY17    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY18    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY19    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY20    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY21    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY22    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY23    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY24    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY25    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY26    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY27    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY28    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY29    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY30    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY31    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY32    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY33    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY34    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY35    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY36    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY37    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY38    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY39    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY40    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY41    LIKE   ZTPP_PMT07JB_A-PQTY,
        QTY42    LIKE   ZTPP_PMT07JB_A-PQTY,
      END   OF IT_TAB.

*---// For Listbox variable
TYPE-POOLS: VRM.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

DATA:  WA_KALID   LIKE KAKO-KALID .
DATA:  WA_CUSTOM_CONTROL TYPE SCRFNAME VALUE 'ALV_CONTAINER',
       ALV_GRID TYPE REF TO CL_GUI_ALV_GRID,
       GRID_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA : IT_FIELDCAT  TYPE LVC_T_FCAT WITH HEADER LINE.

DATA:  WA_IS_LAYOUT TYPE LVC_S_LAYO,
       W_FIELDNAME  LIKE LINE OF IT_FIELDCAT,
       IT_FIELDNAME TYPE SLIS_T_FIELDCAT_ALV,
       WA_VARIANT TYPE DISVARIANT.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_REPID  LIKE SY-REPID,
      W_CNT       TYPE   I.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_MODEL LIKE ZTPP_INPUT_PLAN-MODL
             AS LISTBOX VISIBLE LENGTH 10.
SELECT-OPTIONS: S_FSC FOR ZTPP_WOSUM-FSC NO INTERVALS .
PARAMETERS: P_EXTC LIKE ZTPP_INPUT_PLAN-EXTC,
             P_INTC LIKE ZTPP_INPUT_PLAN-INTC,
*            p_datum LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
            P_SUM AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK BL1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_LISTBOX.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM DISPLAY_DATA.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: L_MI LIKE ZTPP_INPUT_PLAN-MI,
        L_OCNN LIKE ZTPP_INPUT_PLAN-OCNN,
        L_NATION LIKE ZTPP_PMT07JB_A-DIST,
        L_FSC LIKE ZTPP_WOSUM-FSC,
        L_EXTC LIKE ZTPP_INPUT_PLAN-EXTC,
        L_INTC LIKE ZTPP_INPUT_PLAN-INTC,
        L_RECNO(2) TYPE N,
        L_TEXT_QTY(30),
        L_DATE LIKE SY-DATUM.

  DATA: BEGIN OF LT_FSC OCCURS 0,
        NATION  LIKE   ZTPP_PMT07JB_A-DIST,
        FSC     LIKE   ZTPP_WOSUM-FSC,
* by Daniel on 08/20/10 {
        VERS    LIKE   ZTPP_PMT07JB_A-VERS,
* }
        EXTC    LIKE   ZTPP_INPUT_PLAN-EXTC,
        INTC    LIKE   ZTPP_INPUT_PLAN-INTC,
  END   OF LT_FSC.

  DATA: LT_INPUT LIKE TABLE OF IT_INPUT WITH HEADER LINE,
        LT_PMT07JB LIKE TABLE OF IT_PMT07JB WITH HEADER LINE.

  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1),
        L_OLD_DEALER(2).

  FIELD-SYMBOLS: <FS>.

  CLEAR: L_DATE.

  IF P_MODEL IS INITIAL.
    SELECT * INTO TABLE LT_INPUT
      FROM ZTPP_INPUT_PLAN
      WHERE RSNUM > L_DATE.
  ELSE.
    SELECT * INTO TABLE LT_INPUT
       FROM ZTPP_INPUT_PLAN
       WHERE MODL = P_MODEL
         AND RSNUM > L_DATE.
  ENDIF.

  IF P_MODEL IS INITIAL.
    SELECT * INTO TABLE LT_PMT07JB
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*   FROM ZTPP_PMT07JB_A
** End
   WHERE GUBB = 'B'.
  ELSE.
    SELECT * INTO TABLE LT_PMT07JB
** On 11/06/13 by Furong
  FROM ztpp_pmt07jb_a_h
*  FROM ZTPP_PMT07JB_A
** End
     WHERE MODL = P_MODEL
       AND GUBB = 'B'.
  ENDIF.
  IF S_FSC IS INITIAL.
    LOOP AT LT_INPUT.
      IF LT_INPUT-EXTC = P_EXTC OR
                  P_EXTC IS INITIAL.
        IF LT_INPUT-INTC = P_INTC OR
          P_INTC IS INITIAL.
          IT_INPUT = LT_INPUT.
          L_NATION = LT_INPUT-WORK_ORDER+9(5).
** Changed by Furong on 10/10/07 for EBOM
          L_LEN = STRLEN( LT_INPUT-MI ).
          IF L_LEN = 7.
            CONCATENATE L_NATION LT_INPUT-MI
              INTO IT_INPUT-FSC.
            CONCATENATE IT_INPUT-FSC LT_INPUT-OCNN
             INTO IT_INPUT-FSC SEPARATED BY SPACE.
          ELSE.
            CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
                 EXPORTING
                      OLD_DEALER = L_NATION+3(2)
                 IMPORTING
                      NEW_DEALER = L_NEW_DEALER.
            CONCATENATE L_NATION+0(3) L_NEW_DEALER LT_INPUT-MI
                INTO IT_INPUT-FSC.
            CONCATENATE IT_INPUT-FSC LT_INPUT-OCNN
             INTO IT_INPUT-FSC.
          ENDIF.
** End of change
          APPEND IT_INPUT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT LT_PMT07JB.
      IF LT_PMT07JB-EXTC = P_EXTC OR
                  P_EXTC IS INITIAL.
        IF LT_PMT07JB-INTC = P_INTC OR
          P_INTC IS INITIAL.
          IT_PMT07JB = LT_PMT07JB.
** Changed by Furong on 10/10/07 for EBOM
*          CONCATENATE LT_PMT07JB-DIST LT_PMT07JB-BMDL
*            INTO IT_PMT07JB-FSC.
*          CONCATENATE IT_PMT07JB-FSC LT_PMT07JB-OCNN
*           INTO IT_PMT07JB-FSC SEPARATED BY SPACE.
          L_LEN = STRLEN( LT_PMT07JB-BMDL ).
          IF L_LEN = 7.
            CONCATENATE LT_PMT07JB-DIST LT_PMT07JB-BMDL
              INTO IT_PMT07JB-FSC.
            CONCATENATE IT_PMT07JB-FSC LT_PMT07JB-OCNN
             INTO IT_PMT07JB-FSC SEPARATED BY SPACE.
          ELSE.
            CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
                 EXPORTING
                      OLD_DEALER = LT_PMT07JB-DIST+3(2)
                 IMPORTING
                      NEW_DEALER = L_NEW_DEALER.

          CONCATENATE LT_PMT07JB-DIST+0(3) L_NEW_DEALER LT_PMT07JB-BMDL
                 INTO IT_PMT07JB-FSC.
            CONCATENATE IT_PMT07JB-FSC LT_PMT07JB-OCNN
             INTO IT_PMT07JB-FSC.
          ENDIF.
** End of change
          APPEND IT_PMT07JB.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    SORT LT_INPUT BY MI OCNN.
    LOOP AT S_FSC.
      IF S_FSC-LOW+13(1) = SPACE.
        L_MI = S_FSC-LOW+6(8).
      ELSE.
        L_MI = S_FSC-LOW+5(9).
      ENDIF.
      L_OCNN = S_FSC-LOW+14(4).
      LOOP AT LT_INPUT WHERE MI = L_MI
                         AND OCNN = L_OCNN.
        IF LT_INPUT-EXTC = P_EXTC OR
                  P_EXTC IS INITIAL.
          IF LT_INPUT-INTC = P_INTC OR
                 P_INTC IS INITIAL.
            IT_INPUT = LT_INPUT.
            IT_INPUT-FSC = S_FSC-LOW+1(17).
            APPEND IT_INPUT.
          ENDIF.
        ENDIF.
        CLEAR: LT_INPUT, IT_INPUT.
      ENDLOOP.
      LOOP AT LT_PMT07JB WHERE BMDL = L_MI
                         AND OCNN = L_OCNN.
        IF LT_PMT07JB-EXTC = P_EXTC OR
                  P_EXTC IS INITIAL.
          IF LT_PMT07JB-INTC = P_INTC OR
                 P_INTC IS INITIAL.
            IT_PMT07JB = LT_PMT07JB.
            IT_PMT07JB-FSC = S_FSC-LOW+1(17).
            APPEND IT_PMT07JB.
          ENDIF.
        ENDIF.
        CLEAR: LT_PMT07JB, IT_PMT07JB.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

*  SORT it_input BY fsc extc intc.

  LOOP AT IT_INPUT.
** Changed by Furong on 10/10/07 for EBOM
    IF IT_INPUT-FSC+12(1) = SPACE.
      IT_TAB1-NATION = IT_INPUT-FSC+0(5).
      IT_TAB1-FSC = IT_INPUT-FSC+5(12).
    ELSE.
      CALL FUNCTION 'ZFEB_GET_OLD_DEALER_CODE'
           EXPORTING
                NEW_DEALER = IT_INPUT-FSC+3(1)
           IMPORTING
                OLD_DEALER = L_OLD_DEALER.

      CONCATENATE IT_INPUT-FSC+0(3) L_OLD_DEALER
             INTO IT_TAB1-NATION.
      IT_TAB1-FSC = IT_INPUT-FSC+4(13).
    ENDIF.
** End of change

* by Daniel on 08/20/10 {
    IT_TAB1-VERS = IT_INPUT-VERS.
* }
    IT_TAB1-EXTC = IT_INPUT-EXTC.
    IT_TAB1-INTC = IT_INPUT-INTC.
    IT_TAB1-DATE = IT_INPUT-RSNUM.
    IT_TAB1-PQTY = 1.
    COLLECT IT_TAB1.
    CLEAR: IT_TAB1, IT_INPUT.
  ENDLOOP.

*  SORT it_pmt07jb BY fsc extc intc.

  LOOP AT IT_PMT07JB.
** Changed by Furong on 10/10/07 for EBOM
    IF IT_PMT07JB-FSC+12(1) = SPACE.
      IT_TAB1-NATION = IT_PMT07JB-FSC+0(5).
      IT_TAB1-FSC = IT_PMT07JB-FSC+5(12).
    ELSE.
      CALL FUNCTION 'ZFEB_GET_OLD_DEALER_CODE'
           EXPORTING
                NEW_DEALER = IT_PMT07JB-FSC+3(1)
           IMPORTING
                OLD_DEALER = L_OLD_DEALER.
      CONCATENATE IT_PMT07JB-FSC+0(3) L_OLD_DEALER
             INTO IT_TAB1-NATION.
      IT_TAB1-FSC = IT_PMT07JB-FSC+4(13).
    ENDIF.
** End of change
    IT_TAB1-EXTC = IT_PMT07JB-EXTC.
    IT_TAB1-INTC = IT_PMT07JB-INTC.
* by Daniel on 08/20/10 {
    IT_TAB1-VERS = IT_PMT07JB-VERS.
* }
    IT_TAB1-DATE = IT_PMT07JB-SQDT.
    IT_TAB1-PQTY = IT_PMT07JB-PQTY.
    COLLECT IT_TAB1.
    CLEAR: IT_TAB1, IT_PMT07JB.
  ENDLOOP.

  LOOP AT IT_TAB1.
    IT_DATE-DATE = IT_TAB1-DATE.
    APPEND IT_DATE.
    CLEAR: IT_DATE, IT_TAB1.
  ENDLOOP.
  SORT IT_DATE BY DATE.
  DELETE ADJACENT DUPLICATES FROM IT_DATE.

  L_RECNO = '01'.
  LOOP AT IT_DATE.
    IT_DATE-NO = L_RECNO.
    MODIFY IT_DATE TRANSPORTING NO.
    L_RECNO = L_RECNO + 1.
  ENDLOOP.

* by Daniel on 08/20/10 {
*  SORT IT_TAB1 BY NATION FSC EXTC INTC DATE.
  SORT IT_TAB1 BY NATION FSC VERS EXTC INTC DATE.
* }

  LOOP AT IT_TAB1.
    MOVE-CORRESPONDING IT_TAB1 TO LT_FSC.
    APPEND LT_FSC.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM LT_FSC.

  LOOP AT LT_FSC.
    IT_TAB-FSC = LT_FSC-FSC.
* by Daniel on 08/20/10 {
    IT_TAB-VERS = LT_FSC-VERS.
* }
    IT_TAB-NATION = LT_FSC-NATION.
    IT_TAB-EXTC = LT_FSC-EXTC.
    IT_TAB-INTC = LT_FSC-INTC.

    LOOP AT IT_TAB1 WHERE NATION = LT_FSC-NATION
                      AND FSC = LT_FSC-FSC
* by Daniel on 08/20/10 {
                      AND VERS = LT_FSC-VERS
* }
                      AND EXTC = LT_FSC-EXTC
                      AND INTC = LT_FSC-INTC.
      READ TABLE IT_DATE WITH KEY DATE = IT_TAB1-DATE.
      IF SY-SUBRC = 0.
        CONCATENATE 'IT_TAB-QTY' IT_DATE-NO INTO L_TEXT_QTY.
        ASSIGN (L_TEXT_QTY) TO <FS>.
        IF SY-SUBRC = 0.
          <FS> = <FS> + IT_TAB1-PQTY.
        ENDIF.
      ENDIF.
    ENDLOOP.
    APPEND IT_TAB.
    CLEAR: IT_TAB.
  ENDLOOP.
  IF P_SUM = 'X'.
    DATA: LT_TAB_NOCOLOR LIKE TABLE OF IT_TAB WITH HEADER LINE.
    LOOP AT IT_TAB.
      LT_TAB_NOCOLOR = IT_TAB.
      CLEAR: LT_TAB_NOCOLOR-EXTC, LT_TAB_NOCOLOR-INTC.
      COLLECT LT_TAB_NOCOLOR.
      CLEAR: LT_TAB_NOCOLOR.
    ENDLOOP.
    CLEAR: IT_TAB, IT_TAB[].
    IT_TAB[] = LT_TAB_NOCOLOR[].
  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  set_listbox
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX.
  DATA: L_ATINN              LIKE CABN-ATINN,
        L_ATNAM              LIKE CABN-ATNAM,
        L_ATWRT              LIKE AUSP-ATWRT,
        L_ATWTB              LIKE CAWNT-ATWTB.

  CLEAR: NAME, VALUE, LIST, LIST[].

  NAME = 'P_MODEL'.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    VALUE-TEXT = L_ATWRT.  "ZTPP_VEH_MODEL-NAME.  l_atwtb
    VALUE-KEY  = L_ATWRT.  "ZTPP_VEH_MODEL-MODEL.
    APPEND VALUE TO LIST.
  ENDSELECT.

* LIST BOX SETTING
  PERFORM LIST_BOX_FUNCTION USING NAME.
  IF P_MODEL IS INITIAL.
    READ TABLE LIST INTO VALUE  INDEX 1.
    P_MODEL = VALUE-KEY.
  ENDIF.
ENDFORM.                    " set_listbox_lgort
*&---------------------------------------------------------------------*
*&      Form  displayt_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 0200.
ENDFORM.                    " displayt_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_TAB'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.

  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  if not s_fsc-low is initial.
*     CONCATENATE 'FSC:' s_fsc-low INTO wa_is_layout-GRID_TITLE
*                 SEPARATED BY SPACE.
*  ENDIF.
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0553   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING  VALUE(P_ITAB).

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_QTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :


                                  'S' 'NATION'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Nation',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'FSC  '       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'FSC',
                                  'E' 'OUTPUTLEN'   '18',

* by Daniel on 08/20/10 {
                                  'S' 'VERS  '       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'VERS',
                                  'E' 'OUTPUTLEN'   '3',
* }


                                   'S' 'EXTC'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Extc',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'INTC'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Intc',
                                  'E' 'OUTPUTLEN'   '4'.
  L_CN = '01'.
  LOOP AT IT_DATE.
    WRITE IT_DATE-DATE TO L_DATUM MM/DD/YY.
    CONCATENATE 'QTY' L_CN INTO L_QTY.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_QTY        ' ',
                                   ' ' 'KEY'        ' ',
                                   ' ' 'COLTEXT'    L_DATUM,
                                   'E' 'OUTPUTLEN'   '8'.
    L_CN = L_CN + 1.
  ENDLOOP.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = 'A'
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_TAB[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*----------------------------------------------------------------------*

FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat


*---------------------------------------------------------------------*
*       FORM list_box_function                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_NAME                                                        *
*---------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING P_NAME.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_NAME  " list box
            VALUES          = LIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.

ENDFORM.                    " list_box_function
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
