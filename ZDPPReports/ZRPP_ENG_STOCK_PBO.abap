*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
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
    PERFORM BUILD_FIELD_CATALOG_M USING 'IT_OUTPUT_MTH'.
    PERFORM ASSIGN_ITAB_TO_ALV  TABLES IT_OUTPUT_MTH .
  else.
    PERFORM BUILD_FIELD_CATALOG_m USING 'IT_OUTPUT_MTH'.
    PERFORM ASSIGN_ITAB_TO_ALV  TABLES IT_OUTPUT_MTH.
  endif.
ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
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

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
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
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  IF SY-TCODE = 'ZAPP_ENG_PIR'.
    WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  ELSE.
    WA_IS_LAYOUT-EDIT       = ' '.
  ENDIF.
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
  WA_IS_LAYOUT-NO_HGRIDLN = 'X'.
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

*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_200 USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME[].

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'BWART'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Mvr Type',
                                   'E' 'OUTPUTLEN'   '8',


                                  'S' 'TOTAL'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'TOTAL',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '6'.


  LOOP AT IT_MONTH.
    CONCATENATE 'QTY' IT_MONTH-SEQ INTO L_RQTY.
*    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RQTY        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     IT_MONTH-MONTH,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: L_RQTY.
  ENDLOOP.

ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).
  FIELD-SYMBOLS <FS>.

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
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV  TABLES P_ITAB.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = P_ITAB[].
*               it_sort          = it_sort[].

** ENTER
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
** Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*
*
  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.


ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM EXCLUDE_TB_FUNCTIONS.
*  DATA LS_EXCLUDE TYPE UI_FUNC.
*
** Row manipulation
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*
**  Sort buttons
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
***  This excludes all buttons
**  LS_EXCLUDE = '&EXCLALLFC'.
**  APPEND LS_EXCLUDE TO PT_EXCLUDE.
*ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CREATE_CONTAINER_N_OBJECT_TOT.
*  DATA:   W_REPID LIKE SY-REPID.
*  CREATE OBJECT GRID_CONTAINER_TOT
*          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_TOT
*          EXCEPTIONS
*           CNTL_ERROR = 1
*           CNTL_SYSTEM_ERROR = 2
*           CREATE_ERROR = 3
*           LIFETIME_ERROR = 4
*           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  W_REPID = SY-REPID.
*  IF SY-SUBRC NE 0.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'The control can not be created'.
*  ENDIF.
*
**- If the parameter, i_appl_events, is set, the ALV Grid Control
**  registers all events as application events. If the parameter is not
**  set, all events are registered as system events.
*  CREATE OBJECT ALV_GRID_TOT
*         EXPORTING I_PARENT = GRID_CONTAINER_TOT
*                   I_APPL_EVENTS = 'X'.
*
*
*ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_TOT
**&---------------------------------------------------------------------
**
**&      Form  SET_ATTRIBUTES_ALV_GRID_TOT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM SET_ATTRIBUTES_ALV_GRID_TOT.
*  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings
*
*  CLEAR : WA_IS_LAYOUT, WA_VARIANT.
*
**//-- Set Layout Structure
*  WA_TOT_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
*  WA_TOT_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
*  WA_TOT_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_TOT_LAYOUT-CWIDTH_OPT = ' '.   "/optimizes the column width
*  WA_TOT_LAYOUT-INFO_FNAME = 'IF'.
**//-- Set Variant Structure
*  WA_TOT_VARIANT-REPORT       = SY-REPID.
*  WA_TOT_VARIANT-USERNAME     = SY-UNAME.
*
*ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_TOT
**&---------------------------------------------------------------------
**
**&      Form  ASSIGN_ITAB_TO_ALV_TOTAL
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM ASSIGN_ITAB_TO_ALV_TOTAL.
*  CALL METHOD ALV_GRID_TOT->SET_TABLE_FOR_FIRST_DISPLAY
*   EXPORTING   IS_LAYOUT        = WA_TOT_LAYOUT
*             I_SAVE           = WA_TOT_SAVE
*             IS_VARIANT       = WA_TOT_VARIANT
**               i_default        = space
*               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_TOT
*   CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_TOT[]
*             IT_OUTTAB        = IT_TOTAL[].
*
*ENDFORM.                    " ASSIGN_ITAB_TO_ALV_TOTAL
**&---------------------------------------------------------------------
**
**&      Form  BUILD_FIELD_CATALOG_TOT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_0044   text
**----------------------------------------------------------------------
**
*FORM BUILD_FIELD_CATALOG_TOT USING P_ITAB.
*
*  DATA: LW_ITAB TYPE SLIS_TABNAME,
*        LW_WAERS LIKE T001-WAERS,
*        L_RQTY(9),
*        L_DATUM(8),
*        L_CN(2) TYPE N.
*
*  CLEAR: IT_FIELDCAT_TOT,  IT_FIELDCAT_TOT[],
*         IT_FNAME_TOT, IT_FNAME_TOT[].
*  CLEAR: W_CNT,W_REPID.
*
*  LW_ITAB = P_ITAB.
*
*  W_REPID = SY-REPID.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*       EXPORTING
*            I_PROGRAM_NAME     = W_REPID
*            I_INTERNAL_TABNAME = LW_ITAB
*            I_INCLNAME         = W_REPID
*       CHANGING
*            CT_FIELDCAT        = IT_FNAME_TOT.
*
*  PERFORM SETTING_FIELDCAT_TOT TABLES IT_FIELDCAT_TOT USING :
*
*                                  'S' 'MATNR_S'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     '  ',
*                                   'E' 'OUTPUTLEN'   '6',
*
*                                  'S' 'DESC'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  ' ' 'COLTEXT'     'Element',
*                                  'E' 'OUTPUTLEN'   '7',
*
*                                  'S' 'MAKTX'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     '    ',
*                                  'E' 'OUTPUTLEN'   '10'.
*
*  WRITE W_D-1 TO L_DATUM MM/DD/YY.
*  PERFORM SETTING_FIELDCAT_TOT TABLES IT_FIELDCAT_TOT USING :
*
*                                     'S' 'MTD'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
*                                     ' ' 'COLTEXT'     'MTD',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '6',
*
*
*                                     'S' 'D-1'        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
**                                     ' ' 'COLTEXT'     L_DATUM(5),
*
*                                     ' ' 'COLTEXT'     'Yesterday',
*                                     ' ' 'DECIMALS_O'  '0',
*                                     'E' 'OUTPUTLEN'   '5'.
*
*
*
*  LOOP AT IT_DAY.
*    CONCATENATE 'QTYD_' IT_DAY-SEQ INTO L_RQTY.
*    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.
*
*    PERFORM SETTING_FIELDCAT_TOT TABLES IT_FIELDCAT_TOT USING :
*
*                                   'S' L_RQTY        ' ',
*                                   ' ' 'COLTEXT'     L_DATUM(5),
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '5'.
*    CLEAR: L_RQTY.
*  ENDLOOP.
*
*  LOOP AT IT_WEEK.
*    CONCATENATE 'QTYW_' IT_WEEK-SEQ INTO L_RQTY.
*    WRITE IT_WEEK-DATUM TO L_DATUM MM/DD/YY.
*
*    PERFORM SETTING_FIELDCAT_TOT TABLES IT_FIELDCAT_TOT USING :
*
*                                   'S' L_RQTY        ' ',
**        ' ' 'QFIELDNAME'  'MEINS',
*                                   ' ' 'COLTEXT'     L_DATUM(5),
*                                   ' ' 'DECIMALS_O'  '0',
*                                   'E' 'OUTPUTLEN'   '5'.
*    CLEAR: L_RQTY.
*  ENDLOOP.
*
*
*ENDFORM.                    " BUILD_FIELD_CATALOG_TOT
**&---------------------------------------------------------------------
**
**&      Form  SETTING_FIELDCAT_tot
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_IT_FIELDCAT_TOT  text
**      -->P_0759   text
**      -->P_0760   text
**      -->P_0761   text
**----------------------------------------------------------------------
**
*FORM SETTING_FIELDCAT_TOT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
*                           USING    P_GUBUN
*                               P_FIELD
*                               P_VALUE.
*  DATA : L_COL(40).
*
*  FIELD-SYMBOLS <FS>.
*
** START - FIELD ATTRIBUTE SETTING
*  IF P_GUBUN = 'S'.
*    CLEAR: P_FIELDCAT.
*
*    READ TABLE IT_FNAME_TOT INTO W_FIELDNAME
*                            WITH KEY FIELDNAME  = P_FIELD.
*    IF SY-SUBRC NE 0.
*      MESSAGE E000(ZZ) WITH 'Check field catalog'.
*    ENDIF.
*
*    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
*    EXIT.
*  ENDIF.
*
** Setting The Field's Attributes
*  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
*  ASSIGN (L_COL) TO <FS>.
*  MOVE   P_VALUE TO <FS>.
*
** END - FIELD ATTRIBUTE SETTING
*  IF P_GUBUN = 'E'.
*    ADD 1 TO W_CNT.
*    P_FIELDCAT-COL_POS = W_CNT.
*    APPEND P_FIELDCAT.
*  ENDIF.
*ENDFORM.                    " SETTING_FIELDCAT_tot
*
**---------------------------------------------------------------------*
**       FORM EXCLUDE_TB_FUNCTIONS_tot                                 *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
*FORM EXCLUDE_TB_FUNCTIONS_TOT.
*  DATA LS_EXCLUDE TYPE UI_FUNC.
*
***  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE_TOT.
*ENDFORM.                    " EXCLUDE_TB_FUNCTIONS_tot
**&---------------------------------------------------------------------
**
**&      Module  INIT_PRDT_GROUP  OUTPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
MODULE INIT_DATA OUTPUT.
  IF W_FLAG IS INITIAL.
    W_FLAG = 'X'.
*    PERFORM SET_DATA.
    PERFORM SET_DAYS.
    PERFORM SET_BWART.
*    PERFORM MAKE_DROPDOWN_LIST_BOX.
    PERFORM MAKE_DROPDOWN_LIST_BOX_WERKS.
  ENDIF.
ENDMODULE.                 " INIT_PRDT_GROUP  OUTPUT
**&---------------------------------------------------------------------
**
**&      Form  make_dropdown_list_box
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
FORM MAKE_DROPDOWN_LIST_BOX.
  DATA: BEGIN OF LT_MATNR OCCURS 0,
        MATNR LIKE MARA-MATNR,
*        MAKTX LIKE MAKT-MAKTX,
        END OF LT_MATNR.

  CLEAR : XLIST[] , XVALUE.
**  SELECT A~MATNR MAKTX INTO TABLE LT_MATNR
**   FROM MARA AS A
**   INNER JOIN MAKT AS B
**   ON A~MATNR = B~MATNR.
**   WHERE WERKS = C_WERKS.
* SELECT MATNR INTO TABLE LT_MATNR
*   FROM MARA.

  LOOP AT LT_MATNR.
*    XVALUE-TEXT = LT_MATNR-MAKTX.
    XVALUE-KEY  = LT_MATNR-MATNR.
    APPEND LT_MATNR.
  ENDLOOP.

  PERFORM LIST_BOX_FUNCTION USING 'W-MATNR'.
  READ TABLE XLIST INTO XVALUE  INDEX 1.
  W-MATNR = XVALUE-KEY.

ENDFORM.                    " make_dropdown_list_box
*
**---------------------------------------------------------------------*
**       FORM list_box_function                                        *
**---------------------------------------------------------------------*
**       ........                                                      *
**---------------------------------------------------------------------*
**  -->  P_LIST_NAME                                                   *
**---------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING   P_LIST_NAME .
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_LIST_NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.
ENDFORM.                    " list_box_function
*&---------------------------------------------------------------------*
*&      Form  set_bwart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_BWART.
*  IT_BWART-SEQ = '01'.

  IT_BWART-BWART = '101'.
  IT_BWART-DESC = 'Good Receipt'.
  APPEND IT_BWART.
  IT_BWART-BWART = '102'.
  IT_BWART-DESC = 'Good Receipt'.
  APPEND IT_BWART.

  IT_BWART-BWART = '131'.
  IT_BWART-DESC = 'Good Receipt'.
  APPEND IT_BWART.
  IT_BWART-BWART = '132'.
  IT_BWART-DESC = 'Good Receipt'.
  APPEND IT_BWART.

  IT_BWART-BWART = '261'.
  IT_BWART-DESC = 'Good Issue'.
  APPEND IT_BWART.
  IT_BWART-BWART = '262'.
  IT_BWART-DESC = 'Good Issue'.
  APPEND IT_BWART.

  IT_BWART-BWART = '201'.
  IT_BWART-DESC = 'Other G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '202'.
  IT_BWART-DESC = 'Other G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '601'.
  IT_BWART-DESC = 'Other G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '602'.
  IT_BWART-DESC = 'Other G/I'.
  APPEND IT_BWART.

  IT_BWART-BWART = '551'.
  IT_BWART-DESC = 'Scrap G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '552'.
  IT_BWART-DESC = 'Scrap G/I'.
  APPEND IT_BWART.

  IT_BWART-BWART = '309'.
  IT_BWART-DESC = 'Mat to Mat'.
  APPEND IT_BWART.
  IT_BWART-BWART = '310'.
  IT_BWART-DESC = 'Mat to Mat'.
  APPEND IT_BWART.

  IT_BWART-BWART = '905'.
  IT_BWART-DESC = 'Adjust G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '906'.
  IT_BWART-DESC = 'Adjust G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '701'.
  IT_BWART-DESC = 'Adjust G/I'.
  APPEND IT_BWART.
  IT_BWART-BWART = '702'.
  IT_BWART-DESC = 'Adjust G/I'.
  APPEND IT_BWART.
ENDFORM.                    " set_bwart

MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR 'T100'.
ENDMODULE.

MODULE DISPLAY_ALV_100 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV TABLES IT_OUTPUT.
  else.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV  TABLES IT_OUTPUT.
  endif.
ENDMODULE.                 "

FORM BUILD_FIELD_CATALOG_M USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

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

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'BWART'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Mvr Type',
                                   'E' 'OUTPUTLEN'   '8',


                                  'S' 'TOTAL'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'TOTAL',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '6'.


  LOOP AT IT_MONTH.
    CONCATENATE 'QTY' IT_MONTH-SEQ INTO L_RQTY.
    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RQTY        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     IT_MONTH-MONTH,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: L_RQTY.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0716   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING  P_ITAB.
   DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME[].

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'BWART'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Mvr Type',
                                   'E' 'OUTPUTLEN'   '8',


                                  'S' 'TOTAL'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'TOTAL',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '6'.


  LOOP AT IT_day.
    CONCATENATE 'QTY' IT_DAY-SEQ INTO L_RQTY.
    WRITE IT_DAY-DATUM TO L_DATUM MM/DD/YY.

    PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                   'S' L_RQTY        ' ',
*        ' ' 'QFIELDNAME'  'MEINS',
                                   ' ' 'COLTEXT'     l_datum+0(5),
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '6'.
    CLEAR: L_RQTY.
  ENDLOOP.

ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Module  INIT_DATA_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_DATA_200 OUTPUT.
*   IF W_FLAG IS INITIAL.
*    W_FLAG = 'X'.
    PERFORM SET_MONTHS.
    PERFORM SET_BWART.
    PERFORM MAKE_DROPDOWN_LIST_BOX_WERKS.
*   endif.
ENDMODULE.                 " INIT_DATA_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form MAKE_DROPDOWN_LIST_BOX_WERKS .
 DATA: LT_WERKS LIKE TABLE OF T001W WITH HEADER LINE.

 CLEAR : XLIST[] , XVALUE.

  SELECT * INTO TABLE LT_WERKS
  FROM T001W
  WHERE FABKL = 'HE'
    AND WERKS <> ' '.
*
*  L_CN = '1'.
  LOOP AT LT_WERKS.
  XVALUE-TEXT = LT_WERKS-NAME1.
  XVALUE-KEY  = LT_WERKS-WERKS.
  APPEND XVALUE TO XLIST .
*  L_CN = L_CN + 1.
  ENDLOOP.

  PERFORM LIST_BOX_FUNCTION USING 'W_WERKS'.
  READ TABLE XLIST INTO XVALUE  INDEX 1.
  W_WERKS = XVALUE-KEY.

endform.                    " MAKE_DROPDOWN_LIST_BOX_WERKS
