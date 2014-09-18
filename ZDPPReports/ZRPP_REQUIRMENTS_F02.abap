*----------------------------------------------------------------------*
*   INCLUDE ZRPP_REQUIRMENTS_F02                                       *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_EVENTS.
  CONSTANTS : C_PSS TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
              C_UC  TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
              C_TOP TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'.
  REFRESH GT_EVENTS.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = GT_EVENTS.

  PERFORM MODIFY_GT_EVENTS
          TABLES  GT_EVENTS
          USING :
*            slis_ev_pf_status_set c_pss,
            slis_ev_user_command  c_uc,
            SLIS_EV_TOP_OF_PAGE   C_TOP.
ENDFORM.                    " build_events

*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_PF_STATUS_SET  text
*      -->P_C_PSS  text
*----------------------------------------------------------------------*
FORM MODIFY_GT_EVENTS TABLES P_EVENTS_T LIKE GT_EVENTS
                      USING  P_FORM P_VALUE.

  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.

  READ TABLE  P_EVENTS_T  WITH KEY  NAME = P_FORM
                          INTO LS_EVENT.
  IF SY-SUBRC EQ 0.
    MOVE     P_VALUE     TO   LS_EVENT-FORM.
    MODIFY   P_EVENTS_T  FROM LS_EVENT INDEX SY-TABIX.
  ENDIF.
ENDFORM.                    " modify_gt_events


*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       BUILD FIELD CATRGORY FOR ALV DISPLAY
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  IF P_21 = 'X'.
    PERFORM BUILD_FIELDCAT_1 USING 'IT_21LAYOUT'.
    EXIT.
  ENDIF.
  PERFORM BUILD_FIELDCAT_1  USING  'IT_REQ'.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT_1 USING P_INTAB TYPE SLIS_TABNAME.
  DATA: GS_FIELDCAT LIKE LINE OF GT_FIELDCAT.
  CLEAR   : GT_FIELDCAT, GT_FC.
  REFRESH : GT_FIELDCAT, GT_FC.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = G_REPID
            I_INTERNAL_TABNAME = P_INTAB
            I_INCLNAME         = G_REPID
       CHANGING
            CT_FIELDCAT        = GT_FC.

  GT_FIELDCAT[] = GT_FC[].
  IF P_21 = 'X'.
    PERFORM CHANGE_21HEADER.
  ELSE.
    PERFORM CHANGE_HEADER.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER
*&---------------------------------------------------------------------*
*       CHANGE THE HEADER TEXT
*----------------------------------------------------------------------*
FORM CHANGE_HEADER.
  DATA: GS_FIELDCAT LIKE LINE OF GT_FIELDCAT.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
     IF GS_FIELDCAT-FIELDNAME <> 'MATNR'.
*        CLEAR GS_FIELDCAT-KEY .
     ENDIF.
     IF GS_FIELDCAT-FIELDNAME = 'MATNR'.
       GS_FIELDCAT-OUTPUTLEN = 20.
     ELSEIF GS_FIELDCAT-FIELDNAME = 'MENGE'.
       GS_FIELDCAT-OUTPUTLEN = 13.
       GS_FIElDCAT-SELTEXT_M = 'Due-In'.
       GS_FIELDCAT-ddictxt   = 'M'.
     ELSEIF GS_FIELDCAT-FIELDNAME = 'BDMNG'.
       GS_FIELDCAT-OUTPUTLEN = 13.
       GS_FIELDCAT-SELTEXT_M = 'Requirements'.
       GS_FIELDCAT-ddictxt   = 'M'.
     ELSEIF GS_FIELDCAT-FIELDNAME = 'LABST'.
       GS_FIELDCAT-OUTPUTLEN = 13.
       GS_FIELDCAT-SELTEXT_M = 'Current Stock'.
       GS_FIELDCAT-ddictxt   = 'M'.
     ELSEIF GS_FIELDCAT-FIELDNAME = 'SHORT'.
       GS_FIELDCAT-OUTPUTLEN = 13.
       GS_FIELDCAT-SELTEXT_M = 'Available Qty'.
       GS_FIELDCAT-ddictxt   = 'M'.
     ELSEIF GS_FIELDCAT-FIELDNAME = 'PROFL'.
       GS_FIELDCAT-OUTPUTLEN = 5.
       GS_FIELDCAT-SELTEXT_M = 'LP/KD'.
       GS_FIELDCAT-ddictxt   = 'M'.
      ENDIF.
     MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
     IF GS_FIELDCAT-FIELDNAME = 'LGORT'.
       DELETE TABLE GT_FIELDCAT FROM GS_FIELDCAT.
     ENDIF.
  ENDLOOP.

ENDFORM.                 "CHANGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CHANGE_21HEADER
*&---------------------------------------------------------------------*
*       CHANGE THE HEADER TEXT
*----------------------------------------------------------------------*
FORM CHANGE_21HEADER.
  DATA: GS_FIELDCAT LIKE LINE OF GT_FIELDCAT.
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: L_DEL.

  LOOP AT GT_FIELDCAT INTO GS_FIELDCAT.
     L_TABIX = SY-TABIX.
     CLEAR: L_DEL.
     PERFORM GET_WEEK_HEAD USING GS_FIELDCAT L_DEL.
     IF L_DEL = 'X'.
       DELETE GT_FIELDCAT INDEX L_TABIX.
     ELSE.
       MODIFY GT_FIELDCAT FROM GS_FIELDCAT.
     ENDIF.

  ENDLOOP.

ENDFORM.                "CHANGE_21HEADER.

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0278   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT USING P_CB P_COLOR P_SUM.
  CLEAR GS_LAYOUT.

  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-CELL_MERGE        = SPACE.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = ' '.
  GS_LAYOUT-DEFAULT_ITEM      = 'X'.
  IF P_21 = 'X'.
  ELSE.
    GS_LAYOUT-COLTAB_FIELDNAME  = 'CFIELD'.
  ENDIF.
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------
*   FORM BUILD_COMMENT
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM BUILD_COMMENT USING    P_GT_HEADER TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE  TYPE SLIS_LISTHEADER,
        LS_COLOR TYPE SLIS_SPECIALCOL_ALV,
        L_DATE(50).
  DATA: L_TEXT(70) TYPE C.
  DATA: I_LINES(5).
  DATA: I_COUNT(5).

  DESCRIBE TABLE IT_REQ_A LINES I_LINES.
  IF P_21 = 'X'.
    CLEAR LS_LINE.
    LS_LINE-TYP  = 'S'.
    LS_LINE-INFO = '21 Days + 21 Weeks Requirements List'.
    APPEND LS_LINE TO P_GT_HEADER.

  ELSE.
    CLEAR LS_LINE.
    LS_LINE-TYP  = 'S'.
    CONCATENATE 'From :' SPACE P_DATE-LOW '~' P_DATE-HIGH INTO L_TEXT
            SEPARATED BY ' '.
    LS_LINE-INFO = L_TEXT.
    APPEND LS_LINE TO P_GT_HEADER.
  ENDIF.
   IF NOT V_STAT IS INITIAL.
    PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'Message : ' G_MESSAGE.
   ELSE.
     PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'Total Materials : ' I_LINES.
   ENDIF.

   IF NOT P_CTRL IS INITIAL.
      PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'MRP Controller : ' P_CTRL.
   ENDIF.

   IF NOT P_VEND IS INITIAL.
      PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'Vendor : ' P_VEND.
   ENDIF.

   IF NOT P_LGORT IS INITIAL.
     PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'Storage Location: ' P_LGORT.
   ENDIF .
   IF NOT P_RAUBE IS INITIAL.
      PERFORM APPEND_COMMENT USING P_GT_HEADER 'S'
                               'Shop : ' P_RAUBE.
   ENDIF.
ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&  FORM APPEND_COMMENT
*&---------------------------------------------------------------------*
FORM APPEND_COMMENT USING P_HEADER TYPE SLIS_T_LISTHEADER
                          P_TYPE
                          P_TEXT
                          P_VAR.
  DATA: LS_LINE  TYPE SLIS_LISTHEADER.
  DATA: L_TEXT(70) TYPE C.

   CLEAR LS_LINE.
      LS_LINE-TYP  = P_TYPE.
      LS_LINE-INFO = P_TEXT.
      CONCATENATE LS_LINE-INFO P_VAR INTO LS_LINE-INFO
            SEPARATED BY ' '.
      APPEND LS_LINE TO P_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       START ALV TO DISPLAY INTERNAL TABLE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM START_GRID_VIEWER.
*   SORT
    IF P_21 = 'X'.
     PERFORM START_GRID_VIEWER_1 TABLES IT_21LAYOUT.
    ELSE.
     PERFORM  START_GRID_VIEWER_1 TABLES  IT_REQ_A.
    ENDIF.

ENDFORM.                    " start_grid_viewer

*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DISPLAY  text
*----------------------------------------------------------------------*
FORM START_GRID_VIEWER_1 TABLES P_INTAB.

*** print paramter   ****************************************
  GS_PRINT-NO_COVERPAGE = 'X'.
  GS_PRINT-NO_PRINT_LISTINFOS = 'X'.
  GS_PRINT-NO_CHANGE_PRINT_PARAMS = 'X'.
  GS_PRINT-NO_PRINT_SELINFOS = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_BYPASSING_BUFFER       = 'X'
*            i_background_id          = 'ALV_BACKGROUND'
            I_CALLBACK_PROGRAM       = G_REPID
*            I_CALLBACK_PF_STATUS_SET = 'SET_STATUS'
            I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            is_layout                = GS_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
*            IT_SORT                  = GT_SORT[]
            I_SAVE                   = 'A'
            IS_VARIANT               = G_VARIANT
            IT_EVENTS                = GT_EVENTS[]
            IS_PRINT                 = GS_PRINT
            IT_LIST_COMMENTARY       = GT_HEADER
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER  = G_EXIT_CAUSED_BY_CALLER
            ES_EXIT_CAUSED_BY_USER   = GS_EXIT_CAUSED_BY_USER
       TABLES
            T_OUTTAB                 = P_INTAB.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " start_grid_viewer

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.
ENDFORM.                    " TOP_OF_PAGE

**********************************************************************
*    FORM SET_STATUS
**********************************************************************
FORM SET_STATUS USING rt_extab TYPE slis_t_extab..
  SET PF-STATUS 'STATUS' EXCLUDING rt_extab.
ENDFORM.                    " SET_STATUS

**********************************************************************
*    FORM USER_COMMAND
**********************************************************************
FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
   OK_SAVE = R_UCOMM.
   CASE OK_SAVE.
    WHEN '&IC1'.
     IF RS_SELFIELD-TABINDEX NE 0.
       PERFORM BUILD_DETAIL USING RS_SELFIELD.
       CALL SCREEN 100.
     ENDIF.
    WHEN OTHERS.
   ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
   SET PF-STATUS 'DETAIL_100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DETAIL
*&---------------------------------------------------------------------*
*       BUILD DATA FOR DETAIL SCREEN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DETAIL USING P_SELFIELD TYPE slis_selfield.
  DATA: L_LINENO LIKE SY-TABIX.
  DATA: L_REQ LIKE MDSU-MNG01,
        L_DUEIN LIKE MDSU-MNG01,
        L_STOCK LIKE MDSU-MNG01,
        L_AVAIL LIKE MDSU-MNG01,
        P_STATUS .
*-->GET THE LINE
   L_LINENO = P_SELFIELD-TABINDEX.
*-->GET THE MATERIAL
   CLEAR IT_REQ_A.
   READ TABLE IT_REQ_A INDEX L_LINENO.

   CLEAR: IT_DETAIL, IT_DETAIL[].
   PERFORM READ_QUANTITY USING IT_REQ_A-MATNR IT_REQ_A-WERKS
                      CHANGING L_REQ L_DUEIN L_STOCK
                               L_AVAIL P_STATUS.
*-->TRANSFER THE DATA INTO SCREEN VARIABLES
   SC_MATERIAL  = IT_REQ_A-MATNR.
   SC_LPKD      = IT_REQ_A-PROFL.
   SC_DATE_LOW  = P_DATE-LOW.
   SC_DATE_HIGH = P_DATE-HIGH.
   LOOP AT IT_VENDOR.
     CONCATENATE IT_VENDOR-LIFNR SC_VENDOR INTO SC_VENDOR
        SEPARATED BY ' '.
   ENDLOOP.

ENDFORM.                    " BUILD_DETAIL
*&---------------------------------------------------------------------*
*&      Module  ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALV_DISPLAY OUTPUT.
   IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = G_CONTAINER.
    CREATE OBJECT GRID1
           EXPORTING I_PARENT = G_CUSTOM_CONTAINER.
    PERFORM BUILD_DETAIL_FIELDCAT.
   ENDIF.
    CALL METHOD GRID1->SET_TABLE_FOR_FIRST_DISPLAY
*         EXPORTING
*                  I_STRUCTURE_NAME = 'IT_DETAIL'

         CHANGING  IT_OUTTAB        = IT_DETAIL
                   IT_FIELDCATALOG  = GT_FCAT[].


ENDMODULE.                 " ALV_DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_DETAIL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_DETAIL_FIELDCAT.
  CLEAR   :  GT_FCAT.
  REFRESH :  GT_FCAT.
  PERFORM SET_FIELD USING: 'DAT00' '10' 'Date' 'DAT00',
                           'ELEMT' '15' 'Element' 'Element',
                           'MNG02' '20' 'Requirement Qty' 'MNG02',
                           'MNG03' '20' 'Due-in.Qty' 'MNG02',
                           'MNG04' '20' 'Available.Qty' 'MNG02'.

ENDFORM.                    " BUILD_DETAIL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2718   text
*      -->P_2719   text
*      -->P_2720   text
*      -->P_2721   text
*----------------------------------------------------------------------*
FORM SET_FIELD USING    P_fieldname
                        P_length
                        P_text
                        P_column.
  DATA: LT_FCAT LIKE LINE OF GT_FCAT.


  LT_FCAT-FIELDNAME = P_FIELDNAME.
  LT_FCAT-OUTPUTLEN = P_LENGTH.
  LT_FCAT-SCRTEXT_S = P_TEXT.
  LT_FCAT-SCRTEXT_M = P_TEXT.
  LT_FCAT-SCRTEXT_L = P_TEXT.
  APPEND LT_FCAT TO GT_FCAT.


ENDFORM.                    " SET_FIELD
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*       FILL THE COLOR FOR CELLS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_COLOR.
   IF P_21 = 'X'.
     EXIT.
   ENDIF.
   CLEAR: G_COLOR, WA_COLOR.
   LOOP AT IT_REQ_A WHERE SHORT < 0.

       CLEAR WA_COLOR.
       WA_COLOR-FIELDNAME = 'SHORT'.
       WA_COLOR-COLOR-COL = 6.
       WA_COLOR-COLOR-INT = 1.
       APPEND WA_COLOR TO G_COLOR.
       IT_REQ_A-CFIELD = G_COLOR.
       MODIFY IT_REQ_A.

   ENDLOOP.
ENDFORM.                    " BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*&      Form  GET_WEEK_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_FIELDCAT_FIELDNAME  text
*----------------------------------------------------------------------*
form GET_WEEK_HEAD using  P_FIELDCAT LIKE LINE OF GT_FIELDCAT
                          P_DEL .
  DATA: L_WEEKS TYPE I.
  DATA: L_WTEXT(8).
* FOR WEEKS HEAD
  IF P_FIELDCAT-FIELDNAME CS 'W0' OR
     P_FIELDCAT-FIELDNAME CS 'W1' OR
     P_FIELDCAT-FIELDNAME CS 'W2'.

     L_WEEKS   = P_FIELDCAT-FIELDNAME+1(2).

     IF L_WEEKS GE G_DISPLAY_BEGIN_WEEK AND
        L_WEEKS LE G_DISPLAY_END_WEEK.

       READ TABLE IT_WEEKS WITH KEY WSEQ = P_FIELDCAT-FIELDNAME.

       IF SY-SUBRC = 0.
         CONCATENATE 'W_' IT_WEEKS-WEEK INTO L_WTEXT.
         P_FIELDCAT-OUTPUTLEN = 10.
         P_FIELDCAT-SELTEXT_M = L_WTEXT.
         P_FIELDCAT-SELTEXT_L = L_WTEXT.
         P_FIELDCAT-ddictxt   = 'M'.


         IF L_WEEKS = G_DISPLAY_BEGIN_WEEK.
           P_FIELDCAT-KEY       = 'X'.
         ENDIF.
       ENDIF.
     ELSE.
       P_DEL = 'X'.
     ENDIF.

  ENDIF.
* FOR DAYS HEAD
  IF P_FIELDCAT-FIELDNAME CS 'D0' OR
     P_FIELDCAT-FIELDNAME CS 'D1'.
    READ TABLE IT_WORKDAY WITH KEY DSEQ = P_FIELDCAT-FIELDNAME.
    IF SY-SUBRC = 0.
        P_FIELDCAT-SELTEXT_M = IT_WORKDAY-DATE.
        P_FIELDCAT-ddictxt   = 'M'.

        IF P_FIELDCAT-FIELDNAME = 'D00'.
          P_FIELDCAT-SELTEXT_M = L_TODAY.
        ENDIF.
    ELSE.
        P_DEL = 'X'.
    ENDIF.

  ENDIF.
* CHANGE THE MATERIAL FILED LENGTH
  IF P_FIELDCAT-FIELDNAME = 'MATNR'.
       P_FIELDCAT-OUTPUTLEN = 20.
  ENDIF.

endform.                    " GET_WEEK_HEAD
