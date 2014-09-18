***********************************************************
* Program Name      : ZPPA_PLANORDER_DELETE
* Author            : Furong
* Creation Date     : 08/11/10
* Specifications By : n
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Upload the retaining vacation payment
*
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZPPA_PLANORDER_DELETE MESSAGE-ID ZMPP
     NO STANDARD PAGE HEADING LINE-SIZE 105 .

*****************************************************************
*GLOBAL DATA
*****************************************************************

TYPE-POOLS SLIS .

DATA:  W_ANSWER(1),
W_TOTAL TYPE I.

DATA: OK_CODE LIKE SY-UCOMM,
      SAVE_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
      W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: BEGIN OF IT_DATA OCCURS 0,
       PLNUM TYPE PLNUM,
       RESULT(1),
       MESSAGE(80) TYPE C,
      END OF IT_DATA.

DATA: INTERN TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-100.
PARAMETERS:
 P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\.XLS' OBLIGATORY,
 P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT' NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK B2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE .

START-OF-SELECTION.
  PERFORM UPLOAD_DATA.
  DESCRIBE TABLE IT_DATA LINES W_TOTAL.
  IF W_TOTAL > 0.
    PERFORM PROCESS_DATA.
    IF W_ANSWER = 'J'.
      PERFORM DISPLAY_DATA.
    ENDIF.
  ELSE.
    MESSAGE I000 WITH  'No Data'.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      FORM upload_data
*&---------------------------------------------------------------------*
*       Read the persons specified
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_DATA.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILE
            I_BEGIN_COL             = 1
            I_BEGIN_ROW             = 2
            I_END_COL               = 3
            I_END_ROW               = 3000
       TABLES
            INTERN                  = INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.
  IF SY-SUBRC NE 0.
    MESSAGE I000 WITH  'EXEL file upload error!'.
    EXIT.
  ENDIF.

  LOOP AT INTERN.
    CASE INTERN-COL.
      WHEN 1.
        IT_DATA-PLNUM = INTERN-VALUE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
             EXPORTING
                  INPUT  = IT_DATA-PLNUM
             IMPORTING
                  OUTPUT = IT_DATA-PLNUM.

        APPEND IT_DATA.
        CLEAR IT_DATA.
      WHEN 2.

    ENDCASE.

*    CASE intern-col.
*      WHEN 1. it_person-pernr = intern-value.
*      WHEN 2. it_person-cname = intern-value.
*      WHEN 3.
*        it_person-amount = intern-value.
*        APPEND it_person. CLEAR it_person.
*    ENDCASE.

  ENDLOOP.

ENDFORM.                    " read_person_pmnt

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*
*FORM RKC_MSG_STRING CHANGING P_MSG.
*  DATA: LW_MSG LIKE CFGNL-MSGLIN.
*
*  CALL FUNCTION 'RKC_MSG_STRING'
*       EXPORTING
*            ID      = SY-MSGID
*            MTYPE   = SY-MSGTY
*            NUMBER  = SY-MSGNO
*            PAR1    = SY-MSGV1
*            PAR2    = SY-MSGV2
*            PAR3    = SY-MSGV3
*            PAR4    = SY-MSGV4
*       IMPORTING
*            MSG_LIN = LW_MSG
*       EXCEPTIONS
*            OTHERS  = 1.
**  CONCATENATE 'Update failed for' it_person-pernr
**    INTO lw_msg SEPARATED BY space.
*  CONCATENATE IT_PERSON-PERNR LW_MSG INTO LW_MSG
*     SEPARATED BY SPACE.
*  MOVE: LW_MSG TO P_MSG.
*ENDFORM.                    " RKC_MSG_STRING

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PERCENT  text
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR USING P_PERCENT.
  DATA: L_TEXT(40).
  DATA: I_MOD TYPE I.

  L_TEXT = P_PERCENT.
  CONDENSE L_TEXT.
  I_MOD = P_PERCENT MOD 5.
  IF I_MOD = 0.
    CONCATENATE L_TEXT '% PROCESSED' INTO L_TEXT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              TEXT = L_TEXT.
  ENDIF.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
       EXPORTING
            PROGRAM_NAME  = SY-CPROG
            DYNPRO_NUMBER = SY-DYNNR
            FIELD_NAME    = ' '
       IMPORTING
            FILE_NAME     = TMP_FILENAME.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ELSE.
    MESSAGE E000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 100.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SAVE_CODE.
    WHEN 'EXIT' OR 'CANCEL'.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  process_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: LW_RETURN LIKE BAPIRETURN1,
        L_MSG LIKE BAPIRET2-MESSAGE,
        L_PROCESSED  TYPE I,
        L_TOTAL TYPE I,
        L_PERCENT TYPE I.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       EXPORTING
            TITEL         = '  '
            DIAGNOSETEXT1 = 'Caution!!  '
            TEXTLINE1     = 'Are you sure to delete?'
       IMPORTING
            ANSWER        = W_ANSWER.

  IF W_ANSWER = 'J'.
    LOOP AT IT_DATA.

      UPDATE PLAF SET WEMNG = 0
            WHERE PLNUM  = IT_DATA-PLNUM.
      COMMIT WORK.
      WAIT UP TO 3 SECONDS.

      CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
        EXPORTING
          PLANNEDORDER          = IT_DATA-PLNUM
*       USE_COLL_UPDATE       = ' '
*       LAST_ORDER            = ' '
       IMPORTING
         RETURN                = LW_RETURN
                .

      IF LW_RETURN-TYPE = 'S'.
        IT_DATA-RESULT = 'S'.
        IT_DATA-MESSAGE = 'Successfully Deleted'.
        COMMIT WORK.
      ELSE.
        CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        ID         = LW_RETURN-ID
        NUMBER     = LW_RETURN-NUMBER
*                  LANGUAGE          = SY-LANGU
        TEXTFORMAT = 'ASC'
*                  LINKPATTERN       =
       MESSAGE_V1  = LW_RETURN-MESSAGE_V1
       MESSAGE_V2  = LW_RETURN-MESSAGE_V2
       MESSAGE_V3  = LW_RETURN-MESSAGE_V3
       MESSAGE_V4  = LW_RETURN-MESSAGE_V4
     IMPORTING
       MESSAGE     = L_MSG .
*                  RETURN            =
*                TABLES
*                TEXT              =
        IT_DATA-RESULT = LW_RETURN-TYPE.
        IT_DATA-MESSAGE = L_MSG.
      ENDIF.
      MODIFY IT_DATA TRANSPORTING RESULT MESSAGE.
      CLEAR: L_MSG.
      L_PROCESSED = L_PROCESSED + 1.
      L_PERCENT = ( L_PROCESSED * 100 ) / W_TOTAL .
      PERFORM PROGRESS_INDICATOR USING L_PERCENT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " process_DATA
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV TABLES IT_DATA.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV TABLES P_ITAB.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = P_ITAB[]
               IT_SORT          = IT_SORT[].
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
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

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = 'X'.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

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

                                  'S' 'PLNUM'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Plan Order',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'RESULT'       ' ',
                                  ' ' 'COLTEXT'     'Msg Type',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MESSAGE'      ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'OUTPUTLEN'   '80'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
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
