*&--------------------------------------------------------------------
*& REPORT                 : ZEMMPM53C_INFO
*& Author                 : WSKIM
*& Creation Date          : 02/14/2005
*& Specification By       :
*& Pattern                : Report 1-1 1
*& Development Request No :
*& Addl documentation     :
*& Description            :  Info record creation or update
*& Modification Log
*& Date       Developer    Request ID      Description
*& 04/20/2007 Manju        UD1K940390      Program changes to fix
*&                                         BDC errors
*& 06/15/2007 Manju        UD1K940841      Program changes to create
*&                                         new pricing condition in
*&                                         ME12 for the 1st time.
*& 12/07/2010 Valerian     UD1K950369      Collect Error Records into
*&                                         table ZTMM_IF_PRC_ERR.
*&                                         Fix error message so, it
*&                                         will indicate real error.
*& 01/06/2011 Valerian     UD1K950536      Adjust error message if
*&                                         No Purch.Org data is found
*&--------------------------------------------------------------------

INCLUDE ZEMMPM53C_INFO_TOP.
*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM INITIAL.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SELECTION_OUTPUT.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN .
*  PERFORM selection_screen.
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.
  IF SY-BATCH = 'X'.
    PERFORM BATCH_PROCESS.
  ELSE.
    PERFORM MANUAL_REPROCESS.
  ENDIF.
*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
FORM ALV_EVENT_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
                                                            "#EC *
  IF WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    SET PF-STATUS 'STANDARD_GRID' ."EXCLUDING rt_extab.
  ELSE.
    SET PF-STATUS 'STANDARD' ."EXCLUDING rt_extab.
  ENDIF.
  SET TITLEBAR  'STANDARD'.
ENDFORM.                    "alv_event_pf_status_set
*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
FORM ALV_EVENT_USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM
                                      RS_SELFIELD TYPE SLIS_SELFIELD.
                                                            "#EC *
  DATA : SELTAB LIKE RSPARAMS OCCURS 0 WITH HEADER LINE.
  DATA : MESSTAB LIKE  BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

  REFRESH SELTAB.
  CASE R_UCOMM.
*   ---------------------------------- processing on double click.
    WHEN '&IC1'.
      READ TABLE GT_OUT INDEX RS_SELFIELD-TABINDEX.
      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'EBELN'.
*          seltab-selname = 'EN_EBELN'.
*          seltab-sign =  'I'.
*          seltab-option = 'EQ'.
*          seltab-low = it_out-ebeln.
*          APPEND seltab.
*
*          EXPORT  IT_OUT-EBELP  TO MEMORY ID 'BSP'.
*
*          SUBMIT rm06en00 WITH SELECTION-TABLE seltab
*                               AND RETURN.

      ENDCASE.
    WHEN '&REFRESH' OR 'REFRESH'.
      REFRESH GT_OUT.
      CLEAR GT_OUT.                                         "UD1K950369
      PERFORM SELECT_DATA.
      RS_SELFIELD-REFRESH = 'X'.
*---------------------------------- Switching view type grid or list
    WHEN 'RETRY'.
      CLEAR ALREADY_FLAG.
*      it_out[] = gt_out[].
      LOOP AT GT_OUT WHERE CHKBOX EQ 'X'.
        IF GT_OUT-ZRESULT = 'S'.
          CONTINUE.
        ENDIF.

        LOOP AT GT_OUT INTO WA_OUT
                       WHERE MATNR = GT_OUT-MATNR
                         AND LIFNR = GT_OUT-LIFNR
                         AND ERR_C = 'S'
                         AND ( ZRESULT = SPACE OR ZRESULT = 'E' ).
          IF WA_OUT-ZSEQ < GT_OUT-ZSEQ.
            ALREADY_FLAG = 'X'.
          ENDIF.
        ENDLOOP.
        IF ALREADY_FLAG = 'X'.

* BEGIN OF UD1K950369
          GT_OUT-ZMSG =
  'Previous record for this material and vendor has not been processed'.
*         GT_OUT-ZMSG = 'Not yet be processed previous recored'.
*         GT_OUT-ZMSG+45(20)   = IT_MSG-MSGV1.
* END OF UD1K950369

          GT_OUT-ZRESULT = 'E'.
          GT_OUT-ZUSER   = SY-UNAME.
          MESSAGE S999 WITH  GT_OUT-ZMSG .
        ELSE.
*Create or Update "
          PERFORM UPDATE_PROCESSING USING GT_OUT.
        ENDIF.

        MODIFY GT_OUT FROM GT_OUT.
*Update results : ztmm_if_price
        UPDATE  ZTMM_IF_PRICE SET : ZUSER   = GT_OUT-ZUSER
                                    ZRESULT = GT_OUT-ZRESULT
                                    ZBDAT   = GT_OUT-ZBDAT
                                    ZMSG    = GT_OUT-ZMSG
               WHERE MATNR     EQ GT_OUT-MATNR
                 AND LIFNR     EQ GT_OUT-LIFNR
                 AND ZSEQ      EQ GT_OUT-ZSEQ
                 AND INTF_D    EQ GT_OUT-INTF_D
                 AND INTF_TIME EQ GT_OUT-INTF_TIME.

        PERFORM insert_error_record USING gt_out.           "UD1K950369

        CLEAR : ALREADY_FLAG ,GT_OUT.

        PERFORM COMMIT_WORK ON COMMIT.
*      ENDLOOP.
*Other case : Update LDC price
        IF LD_LDC EQ 'ZLC'.
          PERFORM RETRY_LDC_UPDATE.
          CLEAR LD_LDC.
        ENDIF.
      ENDLOOP.

** Changed by Furong on 03/03/08
    WHEN 'SETERR'.
      CLEAR ALREADY_FLAG.

      LOOP AT GT_OUT WHERE CHKBOX EQ 'X'.
        IF GT_OUT-ZRESULT <> 'S'.
          CONTINUE.
        ENDIF.
        GT_OUT-ZRESULT = 'E'.
        GT_OUT-ZUSER = SY-UNAME.
        GT_OUT-ZBDAT = SY-DATUM.
        CONCATENATE 'Reset to Error manully by user ' SY-UNAME
                    INTO GT_OUT-ZMSG SEPARATED BY SPACE.
        MODIFY GT_OUT FROM GT_OUT.
*Update results : ztmm_if_price
        UPDATE  ZTMM_IF_PRICE SET : ZUSER   = GT_OUT-ZUSER
                                    ZRESULT = GT_OUT-ZRESULT
                                    ZBDAT   = GT_OUT-ZBDAT
                                    ZMSG    = GT_OUT-ZMSG
               WHERE MATNR     EQ GT_OUT-MATNR
                 AND LIFNR     EQ GT_OUT-LIFNR
                 AND ZSEQ      EQ GT_OUT-ZSEQ
                 AND INTF_D    EQ GT_OUT-INTF_D
                 AND INTF_TIME EQ GT_OUT-INTF_TIME.
        MESSAGE S999 WITH  'Successfully set to error status'.
        CLEAR : GT_OUT.

        PERFORM COMMIT_WORK ON COMMIT.

      ENDLOOP.
** End of change
    WHEN 'LIST' OR 'GRID'.
      PERFORM SWITCH_LIST_OR_GRID USING R_UCOMM.
  ENDCASE.

  CHECK R_UCOMM EQ 'LIST' OR
        R_UCOMM EQ 'GRID'.

  RS_SELFIELD-EXIT = 'X'.

ENDFORM.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
FORM SET_VARIANT CHANGING CS_VARI TYPE DISVARIANT.

*  CHECK p_layout NE space.
*
*  cs_vari-report      = sy-repid.
*  cs_vari-handle      = space.
*  cs_vari-log_group   = space.
*  cs_vari-username    = space.
*  cs_vari-variant     = p_layout.
*  cs_vari-text        = space.
*  cs_vari-dependvars  = space.

ENDFORM.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
FORM SET_EVENTS CHANGING CT_EVENTS TYPE SLIS_T_EVENT.

  FIELD-SYMBOLS: <LS_EVENT> TYPE SLIS_ALV_EVENT.

  DATA: L_EVENT TYPE LVC_FNAME.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE     = 0
       IMPORTING
            ET_EVENTS       = CT_EVENTS
       EXCEPTIONS
            LIST_TYPE_WRONG = 1
            OTHERS          = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    DELETE CT_EVENTS WHERE NAME NE 'END_OF_PAGE'
                       AND NAME NE 'TOP_OF_PAGE'
                       AND NAME NE 'TOP_OF_LIST'
                       AND NAME NE 'END_OF_LIST'.
    LOOP AT CT_EVENTS ASSIGNING <LS_EVENT>.
      CONCATENATE 'ALV_EVENT_'
                  <LS_EVENT>-NAME
                  INTO <LS_EVENT>-FORM.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " f01_set_evts
*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
FORM SET_LAYOUT CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.

*... Display options
  CS_LAYO-COLWIDTH_OPTIMIZE      = SPACE.
  CS_LAYO-NO_COLHEAD             = SPACE.
  CS_LAYO-NO_HOTSPOT             = SPACE.
  CS_LAYO-ZEBRA                  = ' '.
  CS_LAYO-NO_VLINE               = SPACE.
  CS_LAYO-CELL_MERGE             = SPACE.
  CS_LAYO-NO_MIN_LINESIZE        = SPACE.
  CS_LAYO-MIN_LINESIZE           = SPACE.
  CS_LAYO-MAX_LINESIZE           = SPACE.
  CS_LAYO-WINDOW_TITLEBAR        = SPACE.
  CS_LAYO-NO_ULINE_HS            = SPACE.
*... Edit
  CS_LAYO-EDIT                   = ' '."space.
  CS_LAYO-EDIT_MODE              = ' '."space.
*... Exceptions
  CS_LAYO-LIGHTS_FIELDNAME       = ' '.
  "=> ??? ??? ???
  CS_LAYO-LIGHTS_TABNAME         = SPACE.
  CS_LAYO-LIGHTS_ROLLNAME        = SPACE.
  CS_LAYO-LIGHTS_CONDENSE        = SPACE.
*... Sums
  CS_LAYO-NO_SUMCHOICE           = SPACE.
  CS_LAYO-NO_TOTALLINE           = SPACE.
  CS_LAYO-TOTALS_BEFORE_ITEMS    = SPACE.
  CS_LAYO-TOTALS_ONLY            = SPACE.
  CS_LAYO-TOTALS_TEXT            = SPACE.
  CS_LAYO-NO_SUBCHOICE           = SPACE.
  CS_LAYO-NO_SUBTOTALS           = SPACE.
  CS_LAYO-SUBTOTALS_TEXT         = SPACE.
  CS_LAYO-NUMC_SUM               = 'X'.
  CS_LAYO-NO_UNIT_SPLITTING      = SPACE.
*... Interaction
  CS_LAYO-BOX_FIELDNAME          = 'CHKBOX'.
  CS_LAYO-BOX_TABNAME            = SPACE.
  CS_LAYO-BOX_ROLLNAME           = SPACE.
  CS_LAYO-EXPAND_FIELDNAME       = SPACE.
  CS_LAYO-HOTSPOT_FIELDNAME      = SPACE.
  CS_LAYO-NO_INPUT               = ' '.
  CS_LAYO-F2CODE                 = SPACE.
  CS_LAYO-CONFIRMATION_PROMPT    = SPACE.
  CS_LAYO-KEY_HOTSPOT            = SPACE.
  CS_LAYO-FLEXIBLE_KEY           = SPACE.
  CS_LAYO-REPREP                 = SPACE.
  CS_LAYO-GROUP_BUTTONS          = 'X'.
  CS_LAYO-NO_KEYFIX              = SPACE.
  CS_LAYO-GET_SELINFOS           = SPACE.
  CS_LAYO-GROUP_CHANGE_EDIT      = 'X'.
  CS_LAYO-NO_SCROLLING           = SPACE.
  CS_LAYO-EXPAND_ALL             = SPACE.
  CS_LAYO-NO_AUTHOR              = SPACE.
*... Detailed screen
  CS_LAYO-DETAIL_POPUP           = 'X'.
  CS_LAYO-DETAIL_INITIAL_LINES   = SPACE.
  CS_LAYO-DETAIL_TITLEBAR        = SPACE.
*... PF-status
  CS_LAYO-DEF_STATUS             = SPACE.
*... Display variants
  CS_LAYO-HEADER_TEXT            = SPACE.
  CS_LAYO-ITEM_TEXT              = SPACE.
  CS_LAYO-DEFAULT_ITEM           = SPACE.
*... colour
  CS_LAYO-INFO_FIELDNAME         = SPACE.
  CS_LAYO-COLTAB_FIELDNAME       = 'TABCOLOR'.
*... others
  CS_LAYO-LIST_APPEND            = SPACE.

ENDFORM.                    " set_layout


*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_PAGE.                                 "#EC CALLED

ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
FORM ALV_EVENT_TOP_OF_LIST.                                 "#EC CALLED


ENDFORM.                    "alv_event_top_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_PAGE.
*
ENDFORM.                    "alv_event_end_of_page

*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_LIST.


ENDFORM.                    "alv_event_end_of_list
*&---------------------------------------------------------------------*
*&      Form  dispaly_heager
*----------------------------------------------------------------------*
FORM DISPLAY_HEADER.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
         EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
              IT_LIST_COMMENTARY = W_TOP_OF_PAGE.
ENDFORM.                    " top_of_page

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
FORM SWITCH_LIST_OR_GRID USING R_UCOMM.

  DATA: LS_VARI      TYPE DISVARIANT,
       LS_SLIS_LAYO TYPE SLIS_LAYOUT_ALV,
       LT_SLIS_FCAT TYPE SLIS_T_FIELDCAT_ALV,
       LT_SLIS_SORT TYPE SLIS_T_SORTINFO_ALV,
       LT_SLIS_FILT TYPE SLIS_T_FILTER_ALV,
       LS_SLIS_PRNT TYPE SLIS_PRINT_ALV.


  IF R_UCOMM = 'LIST' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLY'.
    EXIT.
  ENDIF.
  IF R_UCOMM = 'GRID' AND
     WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    EXIT.
  ENDIF.
  CASE WA_ALV_FUNCTION_NAME.
    WHEN 'REUSE_ALV_LIST_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    WHEN 'REUSE_ALV_GRID_DISPLAY'.
      WA_ALV_GET_INFO_NAME = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  ENDCASE.

  CALL FUNCTION WA_ALV_GET_INFO_NAME
       IMPORTING
            ES_LAYOUT     = LS_SLIS_LAYO
            ET_FIELDCAT   = LT_SLIS_FCAT
            ET_SORT       = LT_SLIS_SORT
            ET_FILTER     = LT_SLIS_FILT
            ES_VARIANT    = LS_VARI
       EXCEPTIONS
            NO_INFOS      = 1
            PROGRAM_ERROR = 2
            OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF R_UCOMM = 'LIST'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_LIST_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
              IT_EVENTS                = GT_EVENTS[]
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.
  ENDIF.
  IF R_UCOMM = 'GRID'.
    WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
    CALL FUNCTION WA_ALV_FUNCTION_NAME
         EXPORTING
              I_CALLBACK_PROGRAM       = WA_REPID
              I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
              I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
              IS_LAYOUT                = LS_SLIS_LAYO
              IT_FIELDCAT              = LT_SLIS_FCAT
              IT_SORT                  = LT_SLIS_SORT
              IT_FILTER                = LT_SLIS_FILT
              I_DEFAULT                = ' '  "gs_test-vari_default
              I_SAVE                   = WA_VAR_SAVE
              IS_VARIANT               = LS_VARI
              IS_PRINT                 = LS_SLIS_PRNT
*                it_events               = gt_events[]
         TABLES
              T_OUTTAB                 = GT_OUT
         EXCEPTIONS
              PROGRAM_ERROR            = 1
              OTHERS                   = 2.

  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM BUILD_FIELD_CATEGORY USING
                                  P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_EDIT            "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  .

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
  IF P_FIELDNAME = 'TOT'.
    LS_FIELDCAT-EMPHASIZE = 'C700'.
  ENDIF.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " fill_field_category

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
FORM F4_VARIANT CHANGING C_VARIANT TYPE DISVARIANT-VARIANT.

  DATA: LS_VARIANT TYPE DISVARIANT,
        L_EXIT     TYPE CHAR1.

  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = LS_VARIANT
            I_SAVE              = 'A'
*           it_default_fieldcat =
       IMPORTING
            E_EXIT              = L_EXIT
            ES_VARIANT          = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF L_EXIT EQ SPACE.
      C_VARIANT = LS_VARIANT-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
FORM BUILD_SORT_TABLE USING  P_SPOS
                             P_FIELDNAME
                             P_UP
                             P_SUBTOT
                             P_GROUP.
  DATA: LS_SORT TYPE SLIS_SORTINFO_ALV.

  LS_SORT-SPOS      = P_SPOS.
  LS_SORT-FIELDNAME = P_FIELDNAME.
  LS_SORT-UP        = P_UP.
  LS_SORT-SUBTOT    = P_SUBTOT.
  LS_SORT-GROUP     = P_GROUP.
  APPEND LS_SORT TO GT_SORTS.
ENDFORM.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
FORM SET_LINE_COLOR USING    P_COLOR.
  DATA: LS_FIELDCAT   TYPE SLIS_FIELDCAT_ALV,
        LT_COLOR      TYPE SLIS_T_SPECIALCOL_ALV,
        LS_COLOR      TYPE SLIS_SPECIALCOL_ALV.

  REFRESH LT_COLOR.
  CLEAR   LT_COLOR.
  LOOP AT GT_FIELDCAT INTO LS_FIELDCAT.
    LS_COLOR-FIELDNAME = LS_FIELDCAT-FIELDNAME.
    LS_COLOR-COLOR-COL = P_COLOR.
*    "cl_gui_resources=>list_col_positive.
    LS_COLOR-COLOR-INT = CL_GUI_RESOURCES=>LIST_INTENSIFIED.
    LS_COLOR-COLOR-INV = 0.
    LS_COLOR-NOKEYCOL  = 'X'.
    APPEND LS_COLOR TO LT_COLOR.
*    gt_out-tabcolor = lt_color.
  ENDLOOP.

ENDFORM.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  build_field_category1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0634   text
*      -->P_0635   text
*      -->P_0636   text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*      -->P_0640   text
*      -->P_0641   text
*      -->P_0642   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATEGORY1 USING
                                  P_FIELDNAME       " field name
                                  P_TITLE           " field title
                                  P_OUTPUTLEN       " length
                                  P_KEY             "
                                  P_JUST            "
                                  P_NOOUT           "
                                  P_EDIT            "
                                  P_CFIELD          " currency field nam
                                  P_QFIELD          " quantity field nam
                                  .

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  LS_FIELDCAT-SELTEXT_L = P_TITLE.
  LS_FIELDCAT-OUTPUTLEN = P_OUTPUTLEN.
  LS_FIELDCAT-KEY       = P_KEY.
  LS_FIELDCAT-JUST      = P_JUST.
  LS_FIELDCAT-EDIT      = P_EDIT.
  LS_FIELDCAT-NO_OUT     = P_NOOUT.
  LS_FIELDCAT-CFIELDNAME = P_CFIELD.
  LS_FIELDCAT-QFIELDNAME = P_QFIELD.
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.
ENDFORM.                    " build_field_category1
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD USING  LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
          L_MANAGER(50),
          L_DATE(50),
          L_LIST(50),
          L_DSNAM LIKE T024D-DSNAM,
          L_H_DSNAM LIKE T024D-DSNAM,
          L_LDATE(10),
          L_HDATE(10).
*-------------- HEADER
*  CLEAR ls_line.
*  ls_line-typ  = 'H'.
*  ls_line-info = text-h01.     "HEADER TITLE (H001)
*  APPEND ls_line TO lt_top_of_page.
*
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Investment program : '.
*  ls_line-info = p_prnam.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Approval Year : '.
**  ls_line-info = p_ayear.
*  APPEND ls_line TO lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Order no : '.
*  CONCATENATE   s_aufnr-low  ' ~'  s_aufnr-high INTO l_list.
*  ls_line-info = l_list.
*  APPEND ls_line TO lt_top_of_page.
*
*
ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  set_build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_BUILD_EVENT.
  W_EVENTCAT-NAME = 'TOP_OF_PAGE'.
  W_EVENTCAT-FORM = 'DISPLAY_HEADER'.
  APPEND W_EVENTCAT.
ENDFORM.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  initial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL.
* ==> Change Variant saving type
  WA_VAR_SAVE = 'A'.
* ==> Change first mode   GRID or LIST
  WA_ALV_FUNCTION_NAME = 'REUSE_ALV_GRID_DISPLAY'.
* wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : GT_FIELDCAT.
  CLEAR   : GS_LAYOUT.
  WA_REPID = SY-REPID.

ENDFORM.                    " initial

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA.
  DATA: L_INTF_D LIKE ZTMM_IF_PRICE-INTF_D.
*get data
  REFRESH: IT_IF_PRICE,IT_OUT.
  CASE 'X'.
    WHEN PB_E.
      SELECT * FROM ZTMM_IF_PRICE INTO TABLE IT_IF_PRICE
       WHERE ZRESULT EQ 'E'
         AND LIFNR IN S_LIFNR
         AND MATNR IN S_MATNR
         AND PURCH_G IN S_EKGRP
         AND INF_D IN S_INF_D.

    WHEN PB_S.
      SELECT * FROM ZTMM_IF_PRICE INTO TABLE IT_IF_PRICE
       WHERE ZRESULT EQ 'S'
         AND LIFNR IN S_LIFNR
         AND MATNR IN S_MATNR
         AND PURCH_G IN S_EKGRP
         AND INF_D IN S_INF_D.

    WHEN PB_A.
      SELECT * FROM ZTMM_IF_PRICE INTO TABLE IT_IF_PRICE
       WHERE LIFNR IN S_LIFNR
         AND MATNR IN S_MATNR
         AND PURCH_G IN S_EKGRP
         AND INF_D IN S_INF_D.

  ENDCASE.
** Changed by Furong on 12/24/09
  LOOP AT IT_IF_PRICE.
    IF IT_IF_PRICE-ZRESULT = 'E'.
      SELECT SINGLE INTF_D INTO L_INTF_D
        FROM ZTMM_IF_PRICE
        WHERE MATNR = IT_IF_PRICE-MATNR
          AND LIFNR = IT_IF_PRICE-LIFNR
          AND ZRESULT = 'S'
          AND INTF_D >= IT_IF_PRICE-INTF_D.
      IF SY-SUBRC = 0.
        DELETE IT_IF_PRICE.
      ENDIF.
    ENDIF.
  ENDLOOP.
** End of change

  DESCRIBE TABLE IT_IF_PRICE LINES W_INT.
  IF W_INT = 0.
    EXIT.
  ELSE.
    PERFORM MOVE_VALUE.
  ENDIF.

  SORT GT_OUT BY MATNR LIFNR ZSEQ SEND_D.                   "UD1K950369
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  initial_build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL_BUILD_FIELD_CATEGORY.
  PERFORM BUILD_FIELD_CATEGORY
  USING :
   'LIFNR'     'Vendor'         '10' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'MATNR'     'Material'       '18' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'ZSEQ'      'Sequence'       '08' 'X' 'L'  ' '  ' '  '  ' '  ' ,
   'ICON'      'Result'          '5'  'X' ' '  ' '  ' '  '  ' '  ' ,
   'SEND_D'    'Created On'     '10' ' ' 'C'  ' '  ' '  '  ' '  ' ,
   'ERR_C'    'Interface Status' '10' ' ' 'C'  ' '  ' '  '  ' '  ' ,
   'ZUSER'     'Creator '       '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ZRESULT'   'Tr.Status'      '10' ' ' 'C'  'X'  ' '  '  ' '  ' ,
   'ZMSG'      'Tr.Message'     '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'APP_D'     'from date'      '09' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PUM_N'     'App.Doc num'    '11' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PURCH_ORG' 'Pur.Org'        '07' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'DELY_TIME' 'Del time'       '08' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'PURCH_G'   'Pur.Grp'        '07' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'STAND_Q'   'Sta.Qty'        '07' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'GRBASED'   'Gr based'       '08' ' ' 'C'  ' '  ' '  '  ' '  ' ,
   'UNLIMITED' 'Unlimited'      '09' ' ' 'C'  ' '  ' '  '  ' '  ' ,
   'CONTRK'    'CONTRK'         '05' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'ZNUMBER'   'Num'            '03' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TAX_C'     'TAX'            '03' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'USE_G'     'Status'         '06' ' ' 'C'  ' '  ' '  '  ' '  ' .

ENDFORM.                    " initial_build_field_category
*&---------------------------------------------------------------------*
*&      Form  BATCH_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BATCH_PROCESS.

  PERFORM SELECT_DATA_BATCH.

  DESCRIBE TABLE IT_IF_PRICE LINES W_INT.
  IF W_INT = 0.
    EXIT.
  ELSE.
    PERFORM MOVE_VALUE.
    PERFORM BATCH_BDC_PROCESS.
  ENDIF.

ENDFORM.                    " BATCH_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MANUAL_REPROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MANUAL_REPROCESS.
* ==> 1. select data from db
  PERFORM SELECT_DATA.
  IF GT_OUT[] IS INITIAL.
    MESSAGE S000(ZMFI) WITH 'No found data '.
    EXIT.
  ENDIF.

* ==> 5. build field category
  PERFORM INITIAL_BUILD_FIELD_CATEGORY.
* ==> 2. set variant default
  PERFORM SET_VARIANT CHANGING WA_VAR.
* ==> 3. set layout for alv style
  PERFORM SET_LAYOUT CHANGING GS_LAYOUT.
* ==> 4. set events for alv
  PERFORM SET_EVENTS CHANGING GT_EVENTS.
*===> 5. set event for top-of-page grid.
  PERFORM SET_BUILD_EVENT.
*===>
  PERFORM COMMENT_BUILD USING  W_TOP_OF_PAGE[].

* ==> 7. call function display alv.
  CALL FUNCTION WA_ALV_FUNCTION_NAME
    EXPORTING
         I_CALLBACK_PROGRAM      = WA_REPID
         I_CALLBACK_PF_STATUS_SET = 'ALV_EVENT_PF_STATUS_SET'
         I_CALLBACK_USER_COMMAND  = 'ALV_EVENT_USER_COMMAND'
         IS_LAYOUT               = GS_LAYOUT
         IT_FIELDCAT             = GT_FIELDCAT[]
         IT_SPECIAL_GROUPS       = GT_SP_GROUP[]
         IT_SORT                 = GT_SORTS[]
*         IT_FILTER               =
         I_DEFAULT               = WA_DEFAULT
         I_SAVE                  = WA_VAR_SAVE
         IS_VARIANT              = WA_VAR
*         it_events               = gt_events[]
         IT_EVENTS               =  W_EVENTCAT[]
         IS_PRINT                = GS_PRNT
*        IT_EVENT_EXIT           =
*           I_SCREEN_START_COLUMN   = 10
*           I_SCREEN_START_LINE     = 2
*           I_SCREEN_END_COLUMN     = 80
*           I_SCREEN_END_LINE       = 23
    TABLES
         T_OUTTAB                = GT_OUT.

ENDFORM.                    " MANUAL_REPROCESS
*&---------------------------------------------------------------------*
*&      Form  move_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_VALUE.

  CLEAR IT_IF_PRICE.REFRESH GT_OUT.
  LOOP AT IT_IF_PRICE.
    MOVE-CORRESPONDING IT_IF_PRICE TO GT_OUT.
    CASE IT_IF_PRICE-ZRESULT.
      WHEN 'S'.
        GT_OUT-ICON = C_ICON_EQUAL.
      WHEN 'E'.
        GT_OUT-ICON = C_ICON_ERR.
      WHEN OTHERS.
        GT_OUT-ICON = C_ICON_DIFF.
    ENDCASE.
* BEGIN OF UD1K950536
* If interface status = 'E' use the interface error message to notify
* user about the error.
    IF IT_IF_PRICE-ERR_C = 'E' AND GT_OUT-ZMSG IS INITIAL.
      GT_OUT-ZMSG = IT_IF_PRICE-MSG_C.
    ENDIF.
* END OF UD1K950536
    APPEND GT_OUT.CLEAR GT_OUT.
  ENDLOOP.

ENDFORM.                    " move_value
*&---------------------------------------------------------------------*
*&      Form  update_rpocessing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_PROCESSING USING LT_OUT LIKE GT_OUT.

  CLEAR EXIST.

  SELECT SINGLE * FROM EINA
    WHERE MATNR EQ LT_OUT-MATNR
      AND LIFNR EQ LT_OUT-LIFNR.

  IF SY-SUBRC <> 0.
    EXIST = 1.   "initial
  ELSE.
    EXIST = 2.   "update or delete
  ENDIF.
*General
  PERFORM GENERAL_VIEW USING LT_OUT.
*Condition
  PERFORM CONDITION_VIEW USING EXIST LT_OUT.
*Call transaction
  PERFORM CALL_TRANSACTION USING  EXIST P_MODE LT_OUT.
ENDFORM.                    " update_rpocessing
*&---------------------------------------------------------------------*
*&      Form  general_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM GENERAL_VIEW USING    P_LT_OUT LIKE GT_OUT.
  REFRESH :IT_BDCDATA.
  PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMM06I'            '0100',
		      ' '  'BDC_OKCODE'	      '/00',
		      ' '  'EINA-LIFNR'	      P_LT_OUT-LIFNR,
		      ' '  'EINA-MATNR'	      P_LT_OUT-MATNR,
		      ' '  'EINE-EKORG'	      P_LT_OUT-PURCH_ORG,
                    ' '  'EINE-WERKS'          ' ',
		      ' '  'RM06I-NORMB'	      'X'.


ENDFORM.                    " general_view
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1483   text
*      -->P_1484   text
*      -->P_1485   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO_PROCESSING USING    DY_BEGIN  PG_NAME   SC_NO.
  IF DY_BEGIN = 'X'.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-PROGRAM.
    MOVE  SC_NO    TO IT_BDCDATA-DYNPRO.
    MOVE  'X'      TO IT_BDCDATA-DYNBEGIN.
    APPEND IT_BDCDATA.
  ELSE.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-FNAM.
    MOVE  SC_NO    TO IT_BDCDATA-FVAL.
    APPEND IT_BDCDATA.
  ENDIF.
ENDFORM.                    " bdc_dynpro_processing
*&---------------------------------------------------------------------*
*&      Form  condition_view
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1455   text
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM CONDITION_VIEW USING   P_IND
                            P_LT_OUT LIKE GT_OUT.

  CLEAR :C_NUM.
  PERFORM MATERIAL_LP_KD_MIP CHANGING  P_LT_OUT.
*Initial info record
  IF P_IND EQ '1'.
    PERFORM INITIAL_INFORECORD USING P_LT_OUT.
  ENDIF.
*update info record
  IF P_IND EQ '2'.
    IF  P_LT_OUT-USE_G EQ SPACE.
      PERFORM UPDATE_NORMAL_CASE USING   P_LT_OUT.

*&--In case of delete___________________________
    ELSEIF P_LT_OUT-USE_G EQ 'D'. "Delete
      PERFORM DELETE_CASE USING P_LT_OUT.

    ENDIF.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                     'X'  'SAPMV13A'            '0201',		
 	              ' '  'BDC_CURSOR'	      'RV13A-DATAB',
 	              ' '  'BDC_OKCODE'	      '/00'.
  ENDIF.
*save
  PERFORM UPDATE_PROCESS USING P_LT_OUT.

ENDFORM.                    " condition_view
*&---------------------------------------------------------------------*
*&      Form  seq_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*      -->P_P_LT_OUT_APP_D  text
*      <--P_Z_SEQ  text
*      <--P_W_DATBI  text
*----------------------------------------------------------------------*
FORM TO_DATE_CHECK USING    PP_LT_OUT LIKE GT_OUT
                            WA_DATE
                  CHANGING  W_DATBI Z_SEQ.

  DATA : L_SEQ(2) TYPE N,
         B_DATE LIKE SY-DATUM,
         A_DATE LIKE SY-DATUM.

  IF Z_SEQ = 0.
    READ TABLE IT_A018 WITH KEY DATAB =  WA_DATE.
    IF SY-SUBRC = 0.
      B_DATE = WA_DATE.

      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
           EXPORTING
                DATE      = B_DATE
                DAYS      = '01'
                MONTHS    = '00'
                SIGNUM    = '-'
                YEARS     = '00'
           IMPORTING
                CALC_DATE = A_DATE.
      W_DATBI = A_DATE.
    ENDIF.
  ELSE.
    READ TABLE IT_A018 WITH KEY DATAB =  WA_DATE.
    IF SY-SUBRC = 0.
      W_DATBI  = IT_A018-DATBI.
    ENDIF.
  ENDIF.
ENDFORM.                    " seq_check
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_DATBI  text
*      <--P_W_DATE2  text
*----------------------------------------------------------------------*
FORM DATE_CONVERSION USING    P_BEFORE_DATE
                     CHANGING P_AFTER_DATE.

  SELECT SINGLE *  FROM USR01
        WHERE BNAME = SY-UNAME.

  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+6(2).
  ENDCASE.

ENDFORM.                    " date_conversion
*&---------------------------------------------------------------------*
*&      Form  update_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_PROCESS USING P_LT_OUT LIKE GT_OUT.

  IF P_LT_OUT-USE_G EQ 'D'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
    	            ' '  'BDC_CURSOR'	     'RV13A-DATAB',
    	            ' '  'BDC_OKCODE'	     '=KDAT'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                    'X'  'SAPMV13A	'            '0200',		
 	             ' '  'BDC_OKCODE'	      '=SICH'.
  ELSE.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
    	            ' '  'BDC_CURSOR'	     'RV13A-DATAB',
    	            ' '  'BDC_OKCODE'	     '=KDAT'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                    'X'  'SAPMV13A	'            '0200',		
 	             ' '  'BDC_OKCODE'	      '=SICH',
 	             ' '  'KONH-KOSRT'	      P_LT_OUT-PUM_N,
 	             ' '  'KONH-KZUST'	      P_LT_OUT-RESN_C.

  ENDIF.

  IF LD_LDC = 'ZLC'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                    'X'  'SAPMV13A	'            '0200',		
  	             ' '  'BDC_OKCODE'	      '=SICH',
  	             ' '  'KONH-KOSRT'	      ' ',
  	             ' '  'KONH-KZUST'	      'ZLC'.
  ENDIF.
ENDFORM.                    " update_process
*&---------------------------------------------------------------------*
*&      Form  call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXIST  text
*      -->P_P_MODE  text
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION USING  P_EXIST P_MODE
                             P_LT_OUT LIKE GT_OUT.
  DATA: L_TABIX TYPE SY-TABIX.                              "UD1K950536
  DATA :OPT TYPE CTU_PARAMS.
  REFRESH IT_MSG.
  CASE P_EXIST.
    WHEN '1'.
      P_TCODE = 'ME11'.
    WHEN '2'.
      P_TCODE = 'ME12'.
  ENDCASE.

  CALL TRANSACTION P_TCODE  USING IT_BDCDATA
                                  MODE P_MODE
                                  UPDATE 'S'
                   MESSAGES INTO  IT_MSG .

*  READ TABLE it_msg WITH KEY msgtyp = 'E'.
*  IF sy-subrc <> 0.
*    READ TABLE it_msg WITH KEY msgtyp = 'A'.
*    IF sy-subrc <> 0.
**Update quantity
*      PERFORM ztmm_cond_table_update USING p_lt_out.
*      p_lt_out-zmsg = 'Executed Successfully : '.
*      READ TABLE it_msg WITH KEY msgtyp = 'S'.
*      p_lt_out-zmsg+25(25) =  it_msg-msgv1.
*      p_lt_out-zmsg+50(25) =  it_msg-msgv2.
*      p_lt_out-zresult = 'S'.
*      p_lt_out-icon = c_icon_equal.
*      p_lt_out-zbdat = sy-datum.
*      p_lt_out-zuser = sy-uname.
*      MESSAGE s999 WITH  p_lt_out-zmsg .
*    ELSE.
*      clear: l_messa.
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                MSGID               = it_msg-msgid
*                MSGNR               = it_msg-msgnr
*                MSGV1               = it_msg-msgv1
*                MSGV2               = it_msg-msgv2
*                MSGV3               = it_msg-msgv3
*                MSGV4               = it_msg-msgv4
*           IMPORTING
*                MESSAGE_TEXT_OUTPUT = l_messa.
*
*      p_lt_out-zmsg = 'BDC Error'.
*      p_lt_out-zmsg+10(100) = l_messa.
*      p_lt_out-zresult = 'E'.
*      p_lt_out-zuser   = sy-uname.
*      MESSAGE s999 WITH  p_lt_out-zmsg .
*    ENDIF.
*  ELSE.
*    clear: l_messa.
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              MSGID               = it_msg-msgid
*              MSGNR               = it_msg-msgnr
*              MSGV1               = it_msg-msgv1
*              MSGV2               = it_msg-msgv2
*              MSGV3               = it_msg-msgv3
*              MSGV4               = it_msg-msgv4
*         IMPORTING
*              MESSAGE_TEXT_OUTPUT = l_messa.
*    p_lt_out-zmsg = 'BDC Error'.
*    p_lt_out-zmsg+10(100)  = l_messa.
*    p_lt_out-zresult = 'E'.
*    p_lt_out-zuser   = sy-uname.
*    MESSAGE s999 WITH  p_lt_out-zmsg .
*  ENDIF.
*
** Changed By Furong on 02/28/08
  READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
  IF SY-SUBRC <> 0.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
    IF SY-SUBRC <> 0.
      READ TABLE IT_MSG WITH KEY MSGTYP = 'S'.
** Chnaged BY Furong on 06/03/09
*      IF SY-SUBRC = 0.
      IF SY-SUBRC = 0 AND IT_MSG-MSGNR = '335'.
        SELECT SINGLE *
        FROM EINA
        WHERE INFNR = IT_MSG-MSGV1.
        IF SY-SUBRC <> 0.
          WAIT UP TO 3 SECONDS.
          SELECT SINGLE *
            FROM EINA
            WHERE INFNR = IT_MSG-MSGV1.
          IF SY-SUBRC = 0.
            PERFORM ZTMM_COND_TABLE_UPDATE USING P_LT_OUT.
            P_LT_OUT-ZMSG = 'Executed Successfully : '.
            READ TABLE IT_MSG WITH KEY MSGTYP = 'S'.
            P_LT_OUT-ZMSG+25(25) =  IT_MSG-MSGV1.
            P_LT_OUT-ZMSG+50(25) =  IT_MSG-MSGV2.
            P_LT_OUT-ZRESULT = 'S'.
            P_LT_OUT-ICON = C_ICON_EQUAL.
            P_LT_OUT-ZBDAT = SY-DATUM.
            P_LT_OUT-ZUSER = SY-UNAME.
            MESSAGE S999 WITH  P_LT_OUT-ZMSG .
          ELSE.
            CLEAR: L_MESSA.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                 EXPORTING
                      MSGID               = IT_MSG-MSGID
                      MSGNR               = IT_MSG-MSGNR
                      MSGV1               = IT_MSG-MSGV1
                      MSGV2               = IT_MSG-MSGV2
                      MSGV3               = IT_MSG-MSGV3
                      MSGV4               = IT_MSG-MSGV4
                 IMPORTING
                      MESSAGE_TEXT_OUTPUT = L_MESSA.

            P_LT_OUT-ZMSG = 'BDC Error'.
            P_LT_OUT-ZMSG+10(100) = L_MESSA.
            P_LT_OUT-ZRESULT = IT_MSG-MSGTYP.
            P_LT_OUT-ZUSER   = SY-UNAME.
            MESSAGE S999 WITH  P_LT_OUT-ZMSG .
          ENDIF.
        ELSE.
** End of change
*Update quantity
          PERFORM ZTMM_COND_TABLE_UPDATE USING P_LT_OUT.
          P_LT_OUT-ZMSG = 'Executed Successfully : '.
          READ TABLE IT_MSG WITH KEY MSGTYP = 'S'.
          P_LT_OUT-ZMSG+25(25) =  IT_MSG-MSGV1.
          P_LT_OUT-ZMSG+50(25) =  IT_MSG-MSGV2.
          P_LT_OUT-ZRESULT = 'S'.
          P_LT_OUT-ICON = C_ICON_EQUAL.
          P_LT_OUT-ZBDAT = SY-DATUM.
          P_LT_OUT-ZUSER = SY-UNAME.
          MESSAGE S999 WITH  P_LT_OUT-ZMSG .
        ENDIF.
      ELSE.
        CLEAR: L_MESSA.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = IT_MSG-MSGID
                  MSGNR               = IT_MSG-MSGNR
                  MSGV1               = IT_MSG-MSGV1
                  MSGV2               = IT_MSG-MSGV2
                  MSGV3               = IT_MSG-MSGV3
                  MSGV4               = IT_MSG-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = L_MESSA.

        P_LT_OUT-ZMSG = 'BDC Error'.
        P_LT_OUT-ZMSG+10(100) = L_MESSA.
        P_LT_OUT-ZRESULT = IT_MSG-MSGTYP.
        P_LT_OUT-ZUSER   = SY-UNAME.
        MESSAGE S999 WITH  P_LT_OUT-ZMSG .
      ENDIF.
    ELSE.
* BEGIN OF UD1K950536
      P_LT_OUT-ZRESULT = 'A'.
      IF IT_MSG-MSGID = '00' and IT_MSG-MSGNR = '255'.
        P_LT_OUT-ZRESULT = 'E'.
        P_LT_OUT-ICON = C_ICON_ERR.
        L_TABIX = SY-TABIX - 1.
        IF L_TABIX GT 0.
          READ TABLE IT_MSG INDEX L_TABIX.
        ENDIF.
      ENDIF.
* END OF UD1K950536
      CLEAR: L_MESSA.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = IT_MSG-MSGID
                MSGNR               = IT_MSG-MSGNR
                MSGV1               = IT_MSG-MSGV1
                MSGV2               = IT_MSG-MSGV2
                MSGV3               = IT_MSG-MSGV3
                MSGV4               = IT_MSG-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = L_MESSA.

      P_LT_OUT-ZMSG = 'BDC Error'.
      P_LT_OUT-ZMSG+10(100) = L_MESSA.
*     P_LT_OUT-ZRESULT = 'A'.                               "UD1K950536
      P_LT_OUT-ZUSER   = SY-UNAME.
      MESSAGE S999 WITH  P_LT_OUT-ZMSG .
    ENDIF.
  ELSE.
    CLEAR: L_MESSA.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              MSGID               = IT_MSG-MSGID
              MSGNR               = IT_MSG-MSGNR
              MSGV1               = IT_MSG-MSGV1
              MSGV2               = IT_MSG-MSGV2
              MSGV3               = IT_MSG-MSGV3
              MSGV4               = IT_MSG-MSGV4
         IMPORTING
              MESSAGE_TEXT_OUTPUT = L_MESSA.
    P_LT_OUT-ZMSG = 'BDC Error'.
    P_LT_OUT-ZMSG+10(100)  = L_MESSA.
    P_LT_OUT-ZRESULT = 'E'.
    P_LT_OUT-ZUSER   = SY-UNAME.
    MESSAGE S999 WITH  P_LT_OUT-ZMSG .
  ENDIF.
** End of change

ENDFORM.                    " call_transaction
*&---------------------------------------------------------------------*
*&      Form  ztmm_cond_table_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM ZTMM_COND_TABLE_UPDATE USING LP_OUT LIKE GT_OUT.

  DATA : IT_ZTMM_COND LIKE ZTMM_COND OCCURS 0 WITH HEADER LINE.
  REFRESH IT_ZTMM_COND .
  IF  LP_OUT-QTY1 <> 0.
    MOVE : LP_OUT-MATNR       TO IT_ZTMM_COND-MATNR,
           LP_OUT-LIFNR       TO IT_ZTMM_COND-LIFNR,
           LP_OUT-PURCH_ORG   TO IT_ZTMM_COND-EKORG.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-ERDAT,
           SY-UZEIT           TO IT_ZTMM_COND-ERZET,
           SY-UNAME           TO IT_ZTMM_COND-ERNAM.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-AEDAT,
           SY-UZEIT           TO IT_ZTMM_COND-AEZET,
           SY-UNAME           TO IT_ZTMM_COND-AENAM.
    MOVE : 'ZP01'             TO IT_ZTMM_COND-KSCHL,
            LP_OUT-QTY1       TO IT_ZTMM_COND-MENGE,
           'EA'               TO IT_ZTMM_COND-MEINS.
    APPEND IT_ZTMM_COND.
  ENDIF.
  IF  LP_OUT-QTY2 <> 0.
    MOVE : LP_OUT-MATNR       TO IT_ZTMM_COND-MATNR,
           LP_OUT-LIFNR       TO IT_ZTMM_COND-LIFNR,
           LP_OUT-PURCH_ORG   TO IT_ZTMM_COND-EKORG.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-ERDAT,
           SY-UZEIT           TO IT_ZTMM_COND-ERZET,
           SY-UNAME           TO IT_ZTMM_COND-ERNAM.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-AEDAT,
           SY-UZEIT           TO IT_ZTMM_COND-AEZET,
           SY-UNAME           TO IT_ZTMM_COND-AENAM.
    MOVE : 'ZP02'             TO IT_ZTMM_COND-KSCHL,
            LP_OUT-QTY2       TO IT_ZTMM_COND-MENGE,
           'EA'               TO IT_ZTMM_COND-MEINS.
    APPEND IT_ZTMM_COND.
  ENDIF.
  IF  LP_OUT-QTY3 <> 0.
    MOVE : LP_OUT-MATNR       TO IT_ZTMM_COND-MATNR,
           LP_OUT-LIFNR       TO IT_ZTMM_COND-LIFNR,
           LP_OUT-PURCH_ORG   TO IT_ZTMM_COND-EKORG.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-ERDAT,
           SY-UZEIT           TO IT_ZTMM_COND-ERZET,
           SY-UNAME           TO IT_ZTMM_COND-ERNAM.
    MOVE : SY-DATUM           TO IT_ZTMM_COND-AEDAT,
           SY-UZEIT           TO IT_ZTMM_COND-AEZET,
           SY-UNAME           TO IT_ZTMM_COND-AENAM.
    MOVE : 'ZP03'             TO IT_ZTMM_COND-KSCHL,
            LP_OUT-QTY3       TO IT_ZTMM_COND-MENGE,
           'EA'               TO IT_ZTMM_COND-MEINS.
    APPEND IT_ZTMM_COND.
  ENDIF.

  DESCRIBE TABLE IT_ZTMM_COND LINES W_INT.
  IF W_INT <> 0.
    MODIFY ZTMM_COND FROM TABLE IT_ZTMM_COND.
  ENDIF.
ENDFORM.                    " ztmm_cond_table_update
*&---------------------------------------------------------------------*
*&      Form  select_data_batch
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA_BATCH.
  DATA: L_INTF_D LIKE ZTMM_IF_PRICE-INTF_D.
  REFRESH: IT_IF_PRICE,IT_OUT.

  SELECT * FROM ZTMM_IF_PRICE INTO TABLE IT_IF_PRICE
   WHERE ERR_C EQ 'S'
     AND ( ZRESULT EQ 'E' OR  ZRESULT EQ SPACE ).

** Changed by Furong on 12/24/09
  LOOP AT IT_IF_PRICE.
    IF IT_IF_PRICE-ZRESULT = 'E'.
      SELECT SINGLE INTF_D INTO L_INTF_D
        FROM ZTMM_IF_PRICE
        WHERE MATNR = IT_IF_PRICE-MATNR
          AND LIFNR = IT_IF_PRICE-LIFNR
          AND ZRESULT = 'S'
          AND INTF_D >= IT_IF_PRICE-INTF_D.
      IF SY-SUBRC = 0.
        DELETE IT_IF_PRICE.
      ENDIF.
    ENDIF.
  ENDLOOP.
** End of change
ENDFORM.                    " select_data_batch
*&---------------------------------------------------------------------*
*&      Form  BATCH_BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BATCH_BDC_PROCESS.
  CLEAR: ALREADY_FLAG, GT_OUT,LD_LDC.

  SORT GT_OUT BY MATNR LIFNR ZSEQ SEND_D.
*  it_out[] = gt_out[].
  LOOP AT GT_OUT.
    LOOP AT GT_OUT INTO WA_OUT
                   WHERE MATNR = GT_OUT-MATNR
                     AND LIFNR = GT_OUT-LIFNR
                     AND ERR_C = 'S'
                     AND ( ZRESULT = SPACE OR  ZRESULT = 'E' ).
      IF WA_OUT-ZSEQ < GT_OUT-ZSEQ.
        ALREADY_FLAG = 'X'.
      ENDIF.
    ENDLOOP.
    IF ALREADY_FLAG = 'X'.

* BEGIN OF UD1K950369
      GT_OUT-ZMSG =
  'Previous record for this material and vendor has not been processed'.
*     GT_OUT-ZMSG = 'Not yet be processed previous recored'.
*     GT_OUT-ZMSG+10(20)   = IT_MSG-MSGV1.
* END OF UD1K950369

      GT_OUT-ZRESULT = 'E'.
      GT_OUT-ZUSER   = SY-UNAME.
      MESSAGE S999 WITH  GT_OUT-ZMSG .
    ELSE.
*Create or Update "
      PERFORM UPDATE_PROCESSING USING GT_OUT.
    ENDIF.

    MODIFY GT_OUT FROM GT_OUT.
*Update results : ztmm_if_price
    UPDATE ZTMM_IF_PRICE SET : ZUSER   = SY-UNAME
                               ZRESULT = GT_OUT-ZRESULT
                               ZBDAT   = GT_OUT-ZBDAT
                               ZMSG    = GT_OUT-ZMSG
               WHERE MATNR     EQ GT_OUT-MATNR
                 AND LIFNR     EQ GT_OUT-LIFNR
                 AND ZSEQ      EQ GT_OUT-ZSEQ
                 AND INTF_D    EQ GT_OUT-INTF_D
                 AND INTF_TIME EQ GT_OUT-INTF_TIME.

    PERFORM insert_error_record USING gt_out.               "UD1K950369

    CLEAR ALREADY_FLAG .
    PERFORM COMMIT_WORK ON COMMIT.
*endloop
*other case : update LDC price
    IF LD_LDC EQ 'ZLC'.
      PERFORM RETRY_LDC_UPDATE.
      CLEAR LD_LDC.
    ENDIF.
  ENDLOOP.
*Result write
  CLEAR W_INT.
  DESCRIBE TABLE GT_OUT LINES W_INT.
  WRITE : / 'Total Count :', W_INT, 'Running date ', SY-DATUM.
  LOOP AT GT_OUT.
    WRITE : / GT_OUT-LIFNR, GT_OUT-MATNR,GT_OUT-ZSEQ,GT_OUT-ERR_C,
              GT_OUT-MSG_C, GT_OUT-ZUSER, GT_OUT-ZRESULT, GT_OUT-ZMSG.
  ENDLOOP.
ENDFORM.                    " BATCH_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  FILLED_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*      -->P_LIKE  text
*      -->P_GT_OUT  text
*----------------------------------------------------------------------*
FORM FILLED_CONDITION USING  P_LT_OUT LIKE GT_OUT.
  DATA : ZP_CONTYPE(4),
         ZP_CONVAL(13),
         C_NUM(2) TYPE N,
         Z_SEQ(2) TYPE N.
  DATA : W_DATE(8) TYPE N,
         W_DATE2(8) TYPE N,
         W_DATBI(8) TYPE N,
         VAKE_DATAB(14),
         LV_PRICE(14) TYPE C,                               "UD1K940390
         LV_PRICE_UNIT(5) TYPE C.                           "UD1K940390
  CLEAR :C_NUM, ZP_CONTYPE.
  FIELD-SYMBOLS : <ZP_VALUE> TYPE ANY.

  CLEAR : LV_PRICE,LV_PRICE_UNIT.

  IF  P_LT_OUT-PRICE <> SPACE.
    WRITE P_LT_OUT-PRICE TO LV_PRICE.                       "UD1K940390
    WRITE P_LT_OUT-PRICE_UNIT TO LV_PRICE_UNIT.             "UD1K940390

                                                            "PB00
    PERFORM BDC_DYNPRO_PROCESSING USING :
                      'X'  'SAPMV13A'          '0201',		
  	               ' '  'BDC_CURSOR'	      'KONP-KBETR(01)',
  	               ' '  'BDC_OKCODE'	      '/00',
*  		      ' '  'KONP-KBETR(01)'	p_lt_out-price, "UD1K940390
  		        ' '  'KONP-KBETR(01)'	LV_PRICE,
                      ' '  'KONP-KPEIN(01)'     LV_PRICE_UNIT.
*             ' '  'KONP-KPEIN(01)'     p_lt_out-price_unit. "UD1K940390
    CLEAR : LV_PRICE.
  ENDIF.

*Condition" ZPXX
  DO 25 TIMES.   " ZPXX
    C_NUM =  C_NUM + 1 .
    CONCATENATE 'ZP' C_NUM INTO ZP_CONTYPE.
    CONCATENATE 'P_LT_OUT-ZP' C_NUM INTO ZP_CONVAL.
    ASSIGN (ZP_CONVAL) TO <ZP_VALUE>.


    SELECT SINGLE * FROM T685
      WHERE KSCHL EQ  ZP_CONTYPE.

    CHECK SY-SUBRC = 0 AND  <ZP_VALUE> <> SPACE.
    CLEAR : LV_PRICE.                                       "UD1K940390
    WRITE <ZP_VALUE> TO LV_PRICE.                           "UD1K940390

    PERFORM BDC_DYNPRO_PROCESSING USING :
                      'X'  'SAPMV13A'          '0201',		
  	               ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
  	               ' '  'BDC_OKCODE'	      '/00',
  	               ' '  'KONP-KSCHL(02)'     ZP_CONTYPE,
*  		      ' '  'KONP-KBETR(02)'	<zp_value>.    "UD1K940390
 		        ' '  'KONP-KBETR(02)'	LV_PRICE.


    PERFORM BDC_DYNPRO_PROCESSING USING :
                      'X'  'SAPMV13A'           '0201',		
    	            ' '  'BDC_CURSOR'	    'RV13A-DATAB',
    	            ' '  'BDC_OKCODE'	    '=NEWP'.
  ENDDO.

*Condition" FRA1 ~ 2
  IF LD_LDC EQ SPACE.
    SELECT SINGLE * FROM T685
     WHERE KSCHL EQ 'FRA1'.

    IF SY-SUBRC = 0 AND P_LT_OUT-FRA1 <> SPACE.
      CLEAR : LV_PRICE.
      WRITE P_LT_OUT-FRA1 TO LV_PRICE.
      PERFORM BDC_DYNPRO_PROCESSING USING :
                        'X'  'SAPMV13A'            '0201',		
    	                   ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
    	                   ' '  'BDC_OKCODE'	      '/00',
    	                   ' '  'KONP-KSCHL(02)'       'FRA1',
   		            ' '  'KONP-KBETR(02)'	       LV_PRICE.
*   	      ' '  'KONP-KBETR(02)'	       p_lt_out-fra1.  "UD1K940390

      PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPMV13A'            '0201',		
      	              ' '  'BDC_CURSOR'	      'RV13A-DATAB',
      	              ' '  'BDC_OKCODE'	      '=NEWP'.
    ENDIF.

    SELECT SINGLE * FROM T685
     WHERE KSCHL EQ 'FRA2'.

    IF SY-SUBRC = 0 AND  P_LT_OUT-FRA2 <> SPACE.
      CLEAR : LV_PRICE.
      WRITE P_LT_OUT-FRA2 TO LV_PRICE.                      "UD1K940390

      PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPMV13A'            '0201',		
    	                    ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
    	                    ' '  'BDC_OKCODE'	      '/00',
    	                    ' '  'KONP-KSCHL(02)'      'FRA2',
*    	           ' '  'KONP-KBETR(02)'       p_lt_out-fra2. "UD1K940390
    		             ' '  'KONP-KBETR(02)'       LV_PRICE.


      PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPMV13A	'            '0201',		
      	             ' '  'BDC_CURSOR'	      'RV13A-DATAB',
      	             ' '  'BDC_OKCODE'	      '=NEWP'.
    ENDIF.
*Condition" ZOTH ZOTI
    SELECT SINGLE * FROM T685
     WHERE KSCHL EQ 'ZOTH'.

    IF SY-SUBRC = 0 AND P_LT_OUT-ZOTH <> SPACE.
      CLEAR : LV_PRICE.
      WRITE P_LT_OUT-ZOTH TO LV_PRICE.                      "UD1K940390

      PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPMV13A	'            '0201',		
         	             ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
    	                    ' '  'BDC_OKCODE'	      '/00',
    	                    ' '  'KONP-KSCHL(02)'      'ZOTH',
*                           ' '  'KONP-KBETR(02)' p_lt_out-zoth
    		      ' '  'KONP-KBETR(02)'	     LV_PRICE.
                                                            "UD1K940390

      PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPMV13A	'            '0201',		
      	             ' '  'BDC_CURSOR'	      'RV13A-DATAB',
      	             ' '  'BDC_OKCODE'	      '=NEWP'.
    ENDIF.

    SELECT SINGLE * FROM T685
        WHERE KSCHL EQ 'ZOTI'.

    IF SY-SUBRC = 0 AND P_LT_OUT-ZOTI <> SPACE.

      CLEAR : LV_PRICE.
      WRITE P_LT_OUT-ZOTI TO LV_PRICE. ""UD1K940390

      PERFORM BDC_DYNPRO_PROCESSING USING :
                  'X'  'SAPMV13A	'            '0201',		
    	             ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
    	             ' '  'BDC_OKCODE'	      '/00',
    	             ' '  'KONP-KSCHL(02)'      'ZOTI',
*          ' '  'KONP-KBETR(02)'	     p_lt_out-zoti.
""UD1K940390
   		      ' '  'KONP-KBETR(02)'	     LV_PRICE.


      PERFORM BDC_DYNPRO_PROCESSING USING :
                      'X'  'SAPMV13A	'            '0201',		
      	             ' '  'BDC_CURSOR'	      'RV13A-DATAB',
      	             ' '  'BDC_OKCODE'	      '=NEWP'.
    ENDIF.
  ENDIF.

*Condition" ZKD1
  SELECT SINGLE * FROM T685
   WHERE KSCHL EQ 'ZKD1'.

  IF SY-SUBRC = 0 AND P_LT_OUT-ZKD1 <> SPACE.

    CLEAR : LV_PRICE.
    WRITE P_LT_OUT-ZKD1 TO LV_PRICE.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'  'SAPMV13A	'            '0201',		
  	             ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
  	             ' '  'BDC_OKCODE'	      '/00',
  	             ' '  'KONP-KSCHL(02)'      'ZKD1',
* 	     ' '  'KONP-KBETR(02)'	     p_lt_out-zkd1.  ""UD1K940390
  		      ' '  'KONP-KBETR(02)'	     LV_PRICE.


    PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'  'SAPMV13A	'            '0201',		
    	      ' '  'BDC_CURSOR'	      'RV13A-DATAB',
    	      ' '  'BDC_OKCODE'	      '=NEWP'.
  ENDIF.
*Condition" ZKD2
  SELECT SINGLE * FROM T685
   WHERE KSCHL EQ 'ZKD2'.

  IF SY-SUBRC = 0 AND P_LT_OUT-ZKD2 <> SPACE.

    CLEAR : LV_PRICE.
    WRITE P_LT_OUT-ZKD2 TO LV_PRICE.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'  'SAPMV13A	'            '0201',		
  	             ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
  	             ' '  'BDC_OKCODE'	      '/00',
  	             ' '  'KONP-KSCHL(02)'      'ZKD2',
*  	   ' '  'KONP-KBETR(02)'	     p_lt_out-zkd2.  ""UD1K940390
 	      ' '  'KONP-KBETR(02)'	     LV_PRICE.


    PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'  'SAPMV13A	'            '0201',		
    	      ' '  'BDC_CURSOR'	      'RV13A-DATAB',
    	      ' '  'BDC_OKCODE'	      '=NEWP'.
  ENDIF.
*Condition" ZKD3
  SELECT SINGLE * FROM T685
   WHERE KSCHL EQ 'ZKD3'.

  IF SY-SUBRC = 0 AND P_LT_OUT-ZKD3 <> SPACE.
    CLEAR : LV_PRICE.
    WRITE P_LT_OUT-ZKD3 TO LV_PRICE.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'  'SAPMV13A	'            '0201',		
  	             ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
  	             ' '  'BDC_OKCODE'	      '/00',
  	             ' '  'KONP-KSCHL(02)'      'ZKD3',
*  	     ' '  'KONP-KBETR(02)'	      p_lt_out-zkd3. ""UD1K940390
  		      ' '  'KONP-KBETR(02)'	      LV_PRICE.


  ENDIF.
ENDFORM.                    " FILLED_CONDITION
*&---------------------------------------------------------------------*
*&      Form  selection_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_OUTPUT.

ENDFORM.                    " selection_output
*&---------------------------------------------------------------------*
*&      Form  commit_work
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMMIT_WORK.
  COMMIT WORK AND WAIT.
ENDFORM.                    " commit_work
*&---------------------------------------------------------------------*
*&      Form  seqence_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*      -->P_P_LT_OUT_APP_D  text
*      <--P_Z_SEQ  text
*----------------------------------------------------------------------*
FORM SEQENCE_CHECK USING PP_LT_OUT LIKE GT_OUT
                         WA_DATE
               CHANGING  PZ_SEQ P_LDC.

*  DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.
  DATA : L_SEQ(2) TYPE N,
         B_DATE LIKE SY-DATUM,
         A_DATE LIKE SY-DATUM.

  REFRESH IT_A018.

  SELECT * INTO TABLE IT_A018 FROM A018
   WHERE  KAPPL EQ 'M'
     AND LIFNR EQ PP_LT_OUT-LIFNR
     AND MATNR EQ PP_LT_OUT-MATNR
     AND EKORG EQ PP_LT_OUT-PURCH_ORG.

  LOOP AT IT_A018 .
    L_SEQ = L_SEQ + 1.
    IF IT_A018-DATAB EQ WA_DATE.
      EXIT.
    ENDIF.
  ENDLOOP.

  PZ_SEQ = L_SEQ - 1.
*CHECK :when the one will be deleted(2nd data),
* if afterhere sequence price(3rd)
* exist , and if  that(3rd) is result of LDC, must update price from
* previous data(1st data)  to afterhere data(3rd data)
*for example : 1st data - exist, 2nd data-delete, 3rd data-ldc data
  REFRESH :LD_TABLE,LDC_KONP.
  DATA : APP_DATE LIKE SY-DATUM.
  CLEAR APP_DATE.
  IF L_SEQ > 1.

    LOOP AT IT_A018 WHERE DATAB > WA_DATE.

      SELECT SINGLE * FROM KONH
           WHERE KNUMH EQ IT_A018-KNUMH.

      IF KONH-KZUST EQ 'ZLC'.
        APP_DATE = IT_A018-DATAB.
        READ TABLE IT_A018 INDEX PZ_SEQ.
        IF SY-SUBRC = 0.
          MOVE-CORRESPONDING PP_LT_OUT TO LD_TABLE.
          LD_TABLE-APP_D = APP_DATE.
          P_LDC = KONH-KZUST.
*          REFRESH ldc_konp.
*          SELECT  b~kschl b~kbetr
*               INTO TABLE ldc_konp
*                FROM konh AS a INNER JOIN konp AS b
*                  ON a~knumh = b~knumh
*                   WHERE a~knumh EQ it_a018-knumh.
*
*          LOOP AT ldc_konp.
*            PERFORM condition_type_filled.
*          ENDLOOP.

          APPEND LD_TABLE.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF L_SEQ = 1.
    DATA : L_INDEX TYPE I.
    L_INDEX = 1.
    LOOP AT IT_A018 WHERE DATAB > WA_DATE.
      IF L_INDEX = 1.
        SELECT SINGLE * FROM KONH
             WHERE KNUMH EQ IT_A018-KNUMH.

        IF KONH-KZUST EQ 'ZLC'.
          APP_DATE = IT_A018-DATAB.
          MOVE-CORRESPONDING PP_LT_OUT TO LD_TABLE.
          LD_TABLE-APP_D = APP_DATE.
          P_LDC = KONH-KZUST.
          APPEND LD_TABLE.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " seqence_check
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_LDC_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERSION_LDC_PRICE.


ENDFORM.                    " CONVERSION_LDC_PRICE
*&---------------------------------------------------------------------*
*&      Form  initial_inforecord
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM INITIAL_INFORECORD  USING P_LT_OUT LIKE GT_OUT.
* Begin of changes - UD1K940390
  DATA : LV_PRICE(15) TYPE C,
         LV_QTY(17) TYPE C,
         LV_PRICE_UNIT(5) TYPE C,
         L_DELY_TIME(3) TYPE C.

  CLEAR LV_QTY.
  WRITE  P_LT_OUT-STAND_Q TO LV_QTY.
  WRITE  P_LT_OUT-PRICE TO LV_PRICE.
  WRITE  P_LT_OUT-PRICE_UNIT TO LV_PRICE_UNIT.

  WRITE P_LT_OUT-DELY_TIME TO L_DELY_TIME.

* End of changes - UD1K940390

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'   'SAPMM06I'            '0101',		
	             ' '  'BDC_OKCODE'	      '=EINE',
	             ' '  'EINA-URZZT'	      P_LT_OUT-ZNUMBER.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                  'X'  'SAPMM06I'            '0102',		
	             ' '  'BDC_OKCODE'	      '=KO',
*	             ' '  'EINE-APLFZ'	      p_lt_out-dely_time,
	             ' '  'EINE-APLFZ'	      L_DELY_TIME,
	 	      ' '  'EINE-EKGRP'	      P_LT_OUT-PURCH_G,
*            ' '  'EINE-NORBM'	      p_lt_out-stand_q,"UD1K940390
	             ' '  'EINE-NORBM'	      LV_QTY,
	             ' '  'EINE-UEBTK'	      P_LT_OUT-UNLIMITED,
                    ' '  'EINE-WEBRE'          P_LT_OUT-GRBASED,
	             ' '  'EINE-BSTAE'	      P_LT_OUT-CONTRK,
	             ' '  'EINE-MWSKZ'	      P_LT_OUT-TAX_C,
*            ' '  'EINE-NETPR'	      p_lt_out-price, "UD1K940390
	             ' '  'EINE-NETPR'	      LV_PRICE,
                    ' '  'EINE-PEINH'          LV_PRICE_UNIT.
*             ' '  'EINE-PEINH'          p_lt_out-price_unit."UD1K940390

*date conversion : user format
  PERFORM DATE_CONVERSION USING P_LT_OUT-APP_D
                          CHANGING W_DATE.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                  'X'  'SAPMV13A'            '0201',		
	           ' '  'BDC_CURSOR'	    'KONP-KSCHL(02)',
	           ' '  'BDC_OKCODE'	    '/00',
	           ' '  'RV13A-DATAB'	    W_DATE.
*Filled condition
  PERFORM FILLED_CONDITION USING  P_LT_OUT .

ENDFORM.                    " initial_inforecord
*&---------------------------------------------------------------------*
*&      Form  update_normal_case
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_NORMAL_CASE USING P_LT_OUT LIKE GT_OUT.

  DATA :   LV_PRICE(14) TYPE C,                             "UD1K940390
           LV_PRICE_UNIT(5) TYPE C,                         "UD1K940390
           LV_DELY_TIME(3) TYPE C.


  CLEAR : LV_PRICE, LV_DELY_TIME.

  WRITE P_LT_OUT-STAND_Q TO LV_PRICE.
  WRITE P_LT_OUT-DELY_TIME TO LV_DELY_TIME.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'   'SAPMM06I'            '0101',		
	          ' '  'BDC_OKCODE'	          '=EINE',
                 ' '  'EINA-URZZT'	          P_LT_OUT-ZNUMBER.

* Begin of changes - UD1K940841
* Check whether Price condition exists
  SELECT SINGLE * FROM A018
                  WHERE  KAPPL = 'M' AND
                         KSCHL = 'PB00' AND
                        LIFNR = P_LT_OUT-LIFNR AND
                        MATNR = P_LT_OUT-MATNR.
  IF SY-SUBRC EQ 0.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMM06I'            '0102',		
  	          ' '  'BDC_OKCODE'	          '=KO',
*	          ' '  'EINE-APLFZ'	          p_lt_out-dely_time,
             ' '  'EINE-APLFZ'	          LV_DELY_TIME,
  	 	   ' '  'EINE-EKGRP'	          P_LT_OUT-PURCH_G,
*	          ' '  'EINE-NORBM'	          p_lt_out-stand_q,
               ' '  'EINE-NORBM'	          LV_PRICE,
  	          ' '  'EINE-UEBTK'	          P_LT_OUT-UNLIMITED,
                   ' '  'EINE-WEBRE'          P_LT_OUT-GRBASED,
  	          ' '  'EINE-BSTAE'	          P_LT_OUT-CONTRK,
  	          ' '  'EINE-MWSKZ'	          P_LT_OUT-TAX_C.
*	          ' '  'EINE-NETPR'	          p_lt_out-price.
*                 ' '  'EINE-PEINH'          p_lt_out-price_unit.
    CLEAR Z_SEQ.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X' 'SAPLV14A'             '0102',
                   ' ' 'BDC_OKCODE'           '=NEWD'.

  ELSE.                                                     "UD1K940841
* if there no price conditions maintained - Trying to create
* conditions for the first time in change mode.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMM06I'            '0102',		
  	          ' '  'BDC_OKCODE'	          '=KO',
*	          ' '  'EINE-APLFZ'	          p_lt_out-dely_time,
             ' '  'EINE-APLFZ'	          LV_DELY_TIME,
  	 	   ' '  'EINE-EKGRP'	          P_LT_OUT-PURCH_G,
*	          ' '  'EINE-NORBM'	          p_lt_out-stand_q,
               ' '  'EINE-NORBM'	          LV_PRICE,
  	          ' '  'EINE-UEBTK'	          P_LT_OUT-UNLIMITED,
                   ' '  'EINE-WEBRE'          P_LT_OUT-GRBASED,
  	          ' '  'EINE-BSTAE'	          P_LT_OUT-CONTRK,
  	          ' '  'EINE-MWSKZ'	          P_LT_OUT-TAX_C.
*	          ' '  'EINE-NETPR'	          p_lt_out-price.
*                 ' '  'EINE-PEINH'          p_lt_out-price_unit.

  ENDIF.

* END of changes - UD1K940841

*date conversion : user format
*  PERFORM to_date_normal USING p_lt_out
*                              p_lt_out-app_d
*                        CHANGING  w_datbi .

  PERFORM DATE_CONVERSION USING  P_LT_OUT-APP_D
                          CHANGING W_DATE1.


  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMV13A'     '0201',	
	          ' '  'BDC_OKCODE'   '/00',
                 ' '  'RV13A-DATAB'  W_DATE1.
*Filled condition
  PERFORM FILLED_CONDITION USING  P_LT_OUT .

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMV13A'            '0201',		
	          ' '  'BDC_CURSOR'	      'RV13A-DATAB',
	          ' '  'BDC_OKCODE'	      '/00'.
*save
  PERFORM UPDATE_PROCESS USING P_LT_OUT.

ENDFORM.                    " update_normal_case
*&---------------------------------------------------------------------*
*&      Form  delete_case
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM DELETE_CASE USING P_LT_OUT LIKE GT_OUT.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'   'SAPMM06I'            '0101',		
	          ' '  'BDC_OKCODE'	          '=EINE'.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMM06I'            '0102',		
	          ' '  'BDC_OKCODE'	          '=KO'.

  CLEAR Z_SEQ.

  PERFORM SEQENCE_CHECK USING P_LT_OUT P_LT_OUT-APP_D
                        CHANGING Z_SEQ LD_LDC.

  IF Z_SEQ EQ 0.

    PERFORM TO_DATE_CHECK USING P_LT_OUT
                                P_LT_OUT-APP_D
                          CHANGING  W_DATBI Z_SEQ.

    Z_SEQ = 1.
    CONCATENATE 'VAKE-DATAB(' Z_SEQ ')' INTO VAKE_DATAB.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X' 'SAPLV14A'             '0102',
                   ' ' 'BDC_CURSOR'           VAKE_DATAB,
                   ' ' 'BDC_OKCODE'           '=PICK'.
*date conversion : user format
    CLEAR : W_DATBI, DEL_DATE.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              DATE      = SY-DATUM
              DAYS      = '01'
              MONTHS    = '00'
              SIGNUM    = '-'
              YEARS     = '00'
         IMPORTING
              CALC_DATE = DEL_DATE.

    PERFORM DATE_CONVERSION USING DEL_DATE
                            CHANGING W_DATE2.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'     '0201',	
 	              ' '  'BDC_OKCODE'	'/00',
                 ' '  'RV13A-DATBI'	W_DATE2,
 	              ' '  'RV13A-DATAB'	W_DATE2.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMV13A'     '0201',	
 	              ' '  'BDC_OKCODE'	'=DLIN',
                 ' '  'RV130-SELKZ(01)' 'X'.

  ELSE.

    CONCATENATE 'VAKE-DATAB(' Z_SEQ ')' INTO VAKE_DATAB.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X' 'SAPLV14A'             '0102',
                   ' ' 'BDC_CURSOR'           VAKE_DATAB,
                   ' ' 'BDC_OKCODE'           '=PICK'.
*date conversion : user format
    PERFORM TO_DATE_CHECK USING P_LT_OUT
                                P_LT_OUT-APP_D
                          CHANGING  W_DATBI Z_SEQ.

    PERFORM DATE_CONVERSION USING W_DATBI
                            CHANGING W_DATE2.

    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'     '0201',	
 	              ' '  'BDC_OKCODE'	'/00',
 	              ' '  'RV13A-DATBI'	W_DATE2.
  ENDIF.
ENDFORM.                    " delete_case
*&---------------------------------------------------------------------*
*&      Form  RETRY_LDC_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETRY_LDC_UPDATE.
  DATA : Z_COUNT TYPE I.
  CLEAR :Z_COUNT,W_INT.
  DESCRIBE TABLE LD_TABLE LINES W_INT.
  IF W_INT <> 0.
    LOOP AT LD_TABLE.

      SELECT COUNT( * ) INTO  Z_COUNT  FROM A018
       WHERE  KAPPL EQ 'M'
         AND LIFNR EQ LD_TABLE-LIFNR
         AND MATNR EQ LD_TABLE-MATNR
         AND EKORG EQ LD_TABLE-PURCH_ORG.

      IF Z_COUNT = 1.
        PERFORM PROCESSING_LDC_DELETE USING LD_TABLE Z_COUNT.
      ELSE.
        PERFORM PROCESSING_LDC USING LD_TABLE Z_COUNT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " RETRY_LDC_UPDATE
*&---------------------------------------------------------------------*
*&      Form  processing_ldc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LD_TABLE  text
*----------------------------------------------------------------------*
FORM PROCESSING_LDC  USING LT_OUT LIKE GT_OUT
                           Z_COUNT.
*
  PERFORM COMPARE_CONDITION USING LT_OUT.

*General
  PERFORM GENERAL_VIEW USING LT_OUT.
*Condition
  PERFORM CONDITION_LDC USING '2' LT_OUT Z_COUNT.
*Call transaction
  PERFORM CALL_TRANSACTION USING '2' P_MODE LT_OUT.

ENDFORM.                    " processing_ldc
*&---------------------------------------------------------------------*
*&      Form  condition_ldc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3768   text
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM CONDITION_LDC  USING   P_IND
                            P_LT_OUT LIKE GT_OUT
                            Z_COUNT.
  CLEAR MARA.
  SELECT SINGLE * FROM MARA WHERE MATNR EQ P_LT_OUT-MATNR.
  SELECT SINGLE * FROM MARC WHERE MATNR EQ P_LT_OUT-MATNR
                              AND WERKS EQ 'P001'.
  CASE MARA-PROFL.
    WHEN 'K'. "KD
      P_LT_OUT-GRBASED = ' '.
      P_LT_OUT-UNLIMITED = ' '.
      P_LT_OUT-CONTRK = 'Z001'.
    WHEN 'M'. "MIP

    WHEN 'V'. "LP
*JIT
      IF MARC-FABKZ EQ '1' .
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = ' '.
        P_LT_OUT-CONTRK = 'Z001'.
      ENDIF.
*JIS
*S__Paul 06/24/11
*      IF MARA-TEMPB = '11'.
      IF MARA-TEMPB = '1' OR
         MARA-TEMPB = '2'.
*E_<
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = 'X'.
        P_LT_OUT-CONTRK = SPACE.
      ENDIF.
  ENDCASE.

  PERFORM UPDATE_NORMAL_CASE_LDC USING   P_LT_OUT Z_COUNT.

ENDFORM.                    " condition_ldc
*&---------------------------------------------------------------------*
*&      Form  CONDITION_TYPE_FILLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONDITION_TYPE_FILLED.
  IF LDC_KONP-KSCHL EQ  'ZP01'.
    LD_TABLE-ZP01 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP02'.
    LD_TABLE-ZP02 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP03'.
    LD_TABLE-ZP03 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP04'.
    LD_TABLE-ZP04 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP05'.
    LD_TABLE-ZP05 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP06'.
    LD_TABLE-ZP06 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP07'.
    LD_TABLE-ZP07 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ'ZP08'.
    LD_TABLE-ZP08 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP09'.
    LD_TABLE-ZP09 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP10'.
    LD_TABLE-ZP10 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP11'.
    LD_TABLE-ZP11 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP12'.
    LD_TABLE-ZP12 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP13'.
    LD_TABLE-ZP13 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP14'.
    LD_TABLE-ZP14 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP15'.
    LD_TABLE-ZP15 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP16'.
    LD_TABLE-ZP16 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP17'.
    LD_TABLE-ZP17 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP18'.
    LD_TABLE-ZP18 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP19'.
    LD_TABLE-ZP19 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZP20'.
    LD_TABLE-ZP20 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZKD1'.
    LD_TABLE-ZKD1 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZKD2'.
    LD_TABLE-ZKD2 = LDC_KONP-KBETR.
  ENDIF.
  IF LDC_KONP-KSCHL EQ 'ZKD3'.
    LD_TABLE-ZKD3 = LDC_KONP-KBETR.
  ENDIF.

ENDFORM.                    " CONDITION_TYPE_FILLED
*&---------------------------------------------------------------------*
*&      Form  compare_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM COMPARE_CONDITION  USING LT_OUT LIKE GT_OUT.
  REFRESH IT_COMP.

  READ TABLE IT_A018 WITH KEY DATAB = LT_OUT-APP_D.

  SELECT  B~KSCHL B~KBETR
       INTO TABLE IT_COMP
        FROM KONH AS A INNER JOIN KONP AS B
          ON A~KNUMH = B~KNUMH
           WHERE A~KNUMH EQ IT_A018-KNUMH
             AND B~KSCHL IN ('FRA1', 'ZOTH','ZOTI').


ENDFORM.                    " compare_condition
*&---------------------------------------------------------------------*
*&      Form  SEQUENCE_CHECK_LDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*      -->P_P_LT_OUT_APP_D  text
*      <--P_Z_SEQ  text
*      <--P_LD_LDC  text
*----------------------------------------------------------------------*
FORM SEQUENCE_CHECK_LDC USING PP_LT_OUT LIKE GT_OUT
                         WA_DATE
               CHANGING  PZ_SEQ.

*  DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.
  DATA : L_SEQ(2) TYPE N,
         B_DATE LIKE SY-DATUM,
         A_DATE LIKE SY-DATUM.

  REFRESH IT_A018.

  SELECT * INTO TABLE IT_A018 FROM A018
   WHERE  KAPPL EQ 'M'
     AND LIFNR EQ PP_LT_OUT-LIFNR
     AND MATNR EQ PP_LT_OUT-MATNR
     AND EKORG EQ PP_LT_OUT-PURCH_ORG.

  LOOP AT IT_A018 .
    L_SEQ = L_SEQ + 1.
    IF IT_A018-DATAB EQ WA_DATE.
      EXIT.
    ENDIF.
  ENDLOOP.

  PZ_SEQ = L_SEQ - 1.

ENDFORM.                    " SEQUENCE_CHECK_LDC
*&---------------------------------------------------------------------*
*&      Form  filled_condition_ldc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM FILLED_CONDITION_LDC USING  P_LT_OUT LIKE GT_OUT.
  DATA : RATE(10).
  CLEAR W_INT.
  DESCRIBE TABLE IT_COMP LINES W_INT.
  IF W_INT <> 0.
    IF MARA-PROFL EQ 'K'.
      PERFORM BDC_DYNPRO_PROCESSING USING :
                          'X'  'SAPMV13A'          '0201',		
      	               ' '  'BDC_CURSOR'	      'KONP-KBETR(02)',
      	               ' '  'BDC_OKCODE'	      '/00'.
      LOOP AT IT_COMP.
        CLEAR RATE.
        RATE = IT_COMP-KBETR / 10 .
        IF IT_COMP-KSCHL EQ 'FRA1' AND IT_COMP-KBETR <> 0.
          PERFORM BDC_DYNPRO_PROCESSING USING :
         		        ' '  'KONP-KBETR(02)' RATE.
        ELSEIF IT_COMP-KSCHL EQ 'ZOTH' AND IT_COMP-KBETR <> 0.
          PERFORM BDC_DYNPRO_PROCESSING USING :
         		        ' '  'KONP-KBETR(03)' RATE.
        ELSEIF IT_COMP-KSCHL EQ 'ZOTI' AND IT_COMP-KBETR <> 0.
          PERFORM BDC_DYNPRO_PROCESSING USING :
         		        ' '  'KONP-KBETR(04)' RATE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " filled_condition_ldc
*&---------------------------------------------------------------------*
*&      Form  update_normal_case_ldc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_NORMAL_CASE_LDC USING P_LT_OUT LIKE GT_OUT
                                  Z_COUNT.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'   'SAPMM06I'            '0101',		
	         ' '  'BDC_OKCODE'	          '=EINE',
	         ' '  'EINA-URZZT'	      P_LT_OUT-ZNUMBER.


  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMM06I'            '0102',		
	          ' '  'BDC_OKCODE'	       '=KO',
	          ' '  'EINE-APLFZ'	       P_LT_OUT-DELY_TIME,
	 	   ' '  'EINE-EKGRP'	       P_LT_OUT-PURCH_G,
	          ' '  'EINE-NORBM'	       P_LT_OUT-STAND_Q,
	          ' '  'EINE-UEBTK'	       P_LT_OUT-UNLIMITED,
                 ' '  'EINE-WEBRE'       P_LT_OUT-GRBASED,
	          ' '  'EINE-BSTAE'	       P_LT_OUT-CONTRK,
	          ' '  'EINE-MWSKZ'	       P_LT_OUT-TAX_C.
*	          ' '  'EINE-NETPR'	       p_lt_out-price,
*                 ' '  'EINE-PEINH'       p_lt_out-price_unit.


  CLEAR Z_SEQ.
  PERFORM SEQUENCE_CHECK_LDC USING P_LT_OUT P_LT_OUT-APP_D
                             CHANGING Z_SEQ.

  CONCATENATE 'VAKE-DATAB(' Z_SEQ ')' INTO VAKE_DATAB.
  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X' 'SAPLV14A'             '0102',
                 ' ' 'BDC_CURSOR'           VAKE_DATAB,
                 ' ' 'BDC_OKCODE'           '=NEWR'.

*date conversion : user format
  PERFORM TO_DATE_CHECK USING P_LT_OUT
                              P_LT_OUT-APP_D
                        CHANGING  W_DATBI Z_SEQ.

  PERFORM DATE_CONVERSION USING  P_LT_OUT-APP_D
                          CHANGING W_DATE1.

  PERFORM DATE_CONVERSION USING W_DATBI
                          CHANGING W_DATE2.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMV13A'     '0201',	
	          ' '  'BDC_OKCODE'   '/00',
                 ' '  'RV13A-DATAB'  W_DATE1,
	          ' '  'RV13A-DATBI'  W_DATE2.
*Filled condition
  PERFORM FILLED_CONDITION_LDC USING  P_LT_OUT .

  PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
	              ' '  'BDC_CURSOR'	      'RV13A-DATAB',
	              ' '  'BDC_OKCODE'	      '/00'.
*save
  PERFORM UPDATE_PROCESS_LDC USING P_LT_OUT Z_COUNT.

ENDFORM.                    " update_normal_case_ldc
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_LP_KD_MIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM MATERIAL_LP_KD_MIP CHANGING P_LT_OUT LIKE GT_OUT.
  SELECT SINGLE * FROM MARA WHERE MATNR EQ P_LT_OUT-MATNR.
  SELECT SINGLE * FROM MARC WHERE MATNR EQ P_LT_OUT-MATNR
                              AND WERKS EQ 'P001'.
  CASE MARA-PROFL.
    WHEN 'K'. "KD
      P_LT_OUT-GRBASED = ' '.
      P_LT_OUT-UNLIMITED = ' '.
      P_LT_OUT-CONTRK = 'Z001'.
    WHEN 'M'. "MIP

    WHEN 'V'. "LP
*JIT
      IF MARC-FABKZ EQ '1' .
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = ' '.
        P_LT_OUT-CONTRK = 'Z001'.
      ENDIF.
*JIS
*S__Paul 06/24/11
*      IF MARA-TEMPB = '11'.
      IF MARA-TEMPB = '1' OR
         MARA-TEMPB = '2'.
*E_<
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = 'X'.
        P_LT_OUT-CONTRK = SPACE.
      ENDIF.
  ENDCASE.

ENDFORM.                    " MATERIAL_LP_KD_MIP
*&---------------------------------------------------------------------*
*&      Form  PROCESSING_LDC_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LD_TABLE  text
*----------------------------------------------------------------------*
FORM PROCESSING_LDC_DELETE USING LT_OUT LIKE GT_OUT
                                 Z_COUNT.
*General
  PERFORM GENERAL_VIEW USING LT_OUT.

*Condition
  PERFORM CONDITION_LDC_DELETE USING '2' LT_OUT Z_COUNT.

*Call transaction
  PERFORM CALL_TRANSACTION USING '2' P_MODE LT_OUT.

ENDFORM.                    " PROCESSING_LDC_DELETE
*&---------------------------------------------------------------------*
*&      Form  CONDITION_LDC_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4497   text
*      -->P_LT_OUT  text
*----------------------------------------------------------------------*
FORM CONDITION_LDC_DELETE USING   P_IND
                                  P_LT_OUT LIKE GT_OUT
                                  Z_COUNT.
  CLEAR MARA.
  SELECT SINGLE * FROM MARA WHERE MATNR EQ P_LT_OUT-MATNR.
  SELECT SINGLE * FROM MARC WHERE MATNR EQ P_LT_OUT-MATNR
                              AND WERKS EQ 'P001'.
  CASE MARA-PROFL.
    WHEN 'K'. "KD
      P_LT_OUT-GRBASED = ' '.
      P_LT_OUT-UNLIMITED = ' '.
      P_LT_OUT-CONTRK = 'Z001'.
    WHEN 'M'. "MIP

    WHEN 'V'. "LP
*JIT
      IF MARC-FABKZ EQ '1' .
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = ' '.
        P_LT_OUT-CONTRK = 'Z001'.
      ENDIF.
*JIS
*S__Paul 06/24/11
*      IF MARA-TEMPB = '11'.
      IF MARA-TEMPB = '1' OR
         MARA-TEMPB = '2'.
*E_<
        P_LT_OUT-GRBASED = 'X'.
        P_LT_OUT-UNLIMITED = 'X'.
        P_LT_OUT-CONTRK = SPACE.
      ENDIF.
  ENDCASE.

  PERFORM UPDATE_NORMAL_CASE_LDC_DEL USING   P_LT_OUT  Z_COUNT.

ENDFORM.                    " CONDITION_LDC_DELETE
*&---------------------------------------------------------------------*
*&      Form  update_normal_case_ldc_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_NORMAL_CASE_LDC_DEL USING P_LT_OUT LIKE GT_OUT
                                      Z_COUNT.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                'X'   'SAPMM06I'            '0101',		
	          ' '  'BDC_OKCODE'	          '=EINE'.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMM06I'            '0102',		
	          ' '  'BDC_OKCODE'	          '=KO'.

  CLEAR Z_SEQ.
  PERFORM SEQUENCE_CHECK_LDC USING P_LT_OUT P_LT_OUT-APP_D
                             CHANGING Z_SEQ.
  Z_SEQ = Z_SEQ + 1.

  CONCATENATE 'VAKE-DATAB(' Z_SEQ ')' INTO VAKE_DATAB.
  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X' 'SAPLV14A'             '0102',
                 ' ' 'BDC_CURSOR'           VAKE_DATAB,
                 ' ' 'BDC_OKCODE'           '=PICK'.
*date conversion : user format
  CLEAR : W_DATBI, DEL_DATE.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = '01'
            MONTHS    = '00'
            SIGNUM    = '-'
            YEARS     = '00'
       IMPORTING
            CALC_DATE = DEL_DATE.

  PERFORM DATE_CONVERSION USING DEL_DATE
                          CHANGING W_DATE2.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMV13A'     '0201',	
	          ' '  'BDC_OKCODE'	'/00',
                 ' '  'RV13A-DATBI'	W_DATE2,
	          ' '  'RV13A-DATAB'	W_DATE2.

  PERFORM BDC_DYNPRO_PROCESSING USING :
               'X'  'SAPMV13A'     '0201',	
	              ' '  'BDC_OKCODE'	'=DLIN',
               ' '  'RV130-SELKZ(01)' 'X'.


  PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
	              ' '  'BDC_CURSOR'	      'RV13A-DATAB',
	              ' '  'BDC_OKCODE'	      '/00'.
*save
  PERFORM UPDATE_PROCESS_LDC USING P_LT_OUT Z_COUNT.

ENDFORM.                    " update_normal_case_ldc_del
*&---------------------------------------------------------------------*
*&      Form  to_date_NORMAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*      -->P_P_LT_OUT_APP_D  text
*      <--P_W_DATBI  text
*      <--P_Z_SEQ  text
*----------------------------------------------------------------------*
FORM TO_DATE_NORMAL USING    PP_LT_OUT LIKE GT_OUT
                            WA_DATE
                  CHANGING  W_DATBI .


ENDFORM.                    " to_date_NORMAL
*&---------------------------------------------------------------------*
*&      Form  update_process_LDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_OUT  text
*----------------------------------------------------------------------*
FORM UPDATE_PROCESS_LDC USING P_LT_OUT LIKE GT_OUT
                              Z_COUNT.

  IF Z_COUNT EQ 1.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
    	            ' '  'BDC_CURSOR'	     'RV13A-DATAB',
    	            ' '  'BDC_OKCODE'	     '=KDAT'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                    'X'  'SAPMV13A	'            '0200',		
 	             ' '  'BDC_OKCODE'	      '=SICH'.
  ELSE.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                   'X'  'SAPMV13A'            '0201',		
    	            ' '  'BDC_CURSOR'	     'RV13A-DATAB',
    	            ' '  'BDC_OKCODE'	     '=KDAT'.
    PERFORM BDC_DYNPRO_PROCESSING USING :
                    'X'  'SAPMV13A	'            '0200',		
 	             ' '  'BDC_OKCODE'	      '=SICH',
	             ' '  'KONH-KOSRT'	      '  ',
 	             ' '  'KONH-KZUST'	      'ZLC'.

  ENDIF.

*  IF ld_ldc = 'ZLC'.
*    PERFORM bdc_dynpro_processing USING :
*                    'X'  'SAPMV13A	'            '0200',		
*  	             ' '  'BDC_OKCODE'	      '=SICH',
*  	             ' '  'KONH-KOSRT'	      ' ',
*  	             ' '  'KONH-KZUST'	      'ZLC'.
*  ENDIF.

ENDFORM.                    " update_process_LDC
*&---------------------------------------------------------------------*
*&      Form  insert_error_record
*&---------------------------------------------------------------------*
*       Collect error records into table ZTMM_IF_PRC_ERR
*----------------------------------------------------------------------*
*      -->P_GT_OUT  record to be inserted
*----------------------------------------------------------------------*
FORM insert_error_record USING p_gt_out LIKE gt_out.
  DATA: l_counter          TYPE ztmm_if_prc_err-counter,
        wa_ztmm_if_prc_err TYPE ztmm_if_prc_err.

  CHECK p_gt_out-zresult = 'E'.

  SELECT MAX( counter ) INTO l_counter
    FROM ztmm_if_prc_err
   WHERE matnr     = p_gt_out-matnr
     AND lifnr     = p_gt_out-lifnr
     AND zseq      = p_gt_out-zseq
     AND intf_d    = p_gt_out-intf_d
     AND intf_time = p_gt_out-intf_time.

  l_counter = l_counter + 1.

  MOVE-CORRESPONDING p_gt_out TO wa_ztmm_if_prc_err.
  wa_ztmm_if_prc_err-counter = l_counter.
  wa_ztmm_if_prc_err-chgdt   = sy-datum.
  wa_ztmm_if_prc_err-chgtm   = sy-uzeit.

  INSERT ztmm_if_prc_err FROM wa_ztmm_if_prc_err.

ENDFORM.                    " insert_error_record
