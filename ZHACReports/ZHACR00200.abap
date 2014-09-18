*&---------------------------------------------------------------------*
*& Program ID :  ZHARC00200
*& ##       :  CBO### ## Upload / Download - ZACTTATB #####
*& ####   :  2013.02.25.                      - ZACTPGML2 ####
*& ###     :  HMCDVM3
*& ##       :  CBO### ## Upload / Download# ####.
*&---------------------------------------------------------------------*
*& ####   :
*& ###     :
*& ####   :
*& ####   :
*&---------------------------------------------------------------------*

REPORT  ZHACR00200 MESSAGE-ID 00 .

*- I n c l u d e s ----------------------------------------------------*
INCLUDE <LIST>.
INCLUDE ZHACR00200_TOP.
*INCLUDE ZHARC00200_TOP.
*INCLUDE ZHACR90000_TOP.
*include YTABLEDOWNUP_TOP.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
*----------------------------------------------------------------------*
*  PERFORM QUERY_FILENAME USING GLOBAL_UPLOAD_PATH 'O'.
  PERFORM SELECTION_SCREEN_F4_HELP CHANGING P_FILE.
*  global_upload_path = p_file.

*----------------------------------------------------------------------*
AT  SELECTION-SCREEN.
*----------------------------------------------------------------------*
  IF P_TABLE(1) NE 'Z'.
    MESSAGE E001 WITH 'CBO#### #####.'.
  ENDIF.

  PERFORM SELECTION_SCREEN.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CHECK GV_START IS INITIAL.
  GV_FILENAME = P_FILE.

  IF RB_DOW  EQ 'X'.
    PERFORM   P1000_GET_DOWN_DATA.

    PERFORM SCR100_DOWN_LOAD_TO_LOCAL.

  ELSE.
    PERFORM   P1000_GET_UP_DATA.

    PERFORM SCR100_UPLOAD_TO_LOCAL.
  ENDIF.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*
*  CHECK gv_start IS INITIAL.
*
*  CALL SCREEN 100.


*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_F4_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN_F4_HELP  CHANGING    PV_FILE_NAME.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG
      DYNPRO_NUMBER = SYST-DYNNR
      FIELD_NAME    = ' '
    IMPORTING
      FILE_NAME     = PV_FILE_NAME.

ENDFORM.                    " SELECTION_SCREEN_F4_HELP
*&---------------------------------------------------------------------*
*&      Form  selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN .

  CLEAR: GV_START, GV_FIRST_DISPLAY, GT_DFIES.
  REFRESH: GT_FIELDCAT, GT_DFIES.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE              =
      I_STRUCTURE_NAME             =  P_TABLE
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_BYPASSING_BUFFER           =
    CHANGING
      CT_FIELDCAT                  = GT_FIELDCAT.
  IF SY-SUBRC <> 0.
    MESSAGE  E001 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    GV_START = GC_MARK.
    EXIT.
  ENDIF.

  PERFORM CHANGE TABLES GT_FIELDCAT.

*-Create Dynamic internal table
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELDCAT
    IMPORTING
      EP_TABLE        = LDTAB.
  ASSIGN LDTAB->* TO <GT_TABLE>.

*-Define Dynamic structure
  CREATE DATA DREF LIKE LINE OF <GT_TABLE>.
  ASSIGN DREF->* TO <GT_LINE>.

*-### / ### ## ### ###
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME              = P_TABLE
*     FIELDNAME            = ' '
*     LANGU                = SY-LANGU
*     LFIELDNAME           = ' '
*     ALL_TYPES            = ' '
*     GROUP_NAMES          = ' '
*     UCLEN                =
*   IMPORTING
*     X030L_WA             =
*     DDOBJTYPE            =
*     DFIES_WA             =
*     LINES_DESCR          =
    TABLES
      DFIES_TAB            = GT_DFIES
*     FIXED_VALUES         =
    EXCEPTIONS
      NOT_FOUND            = 1
      INTERNAL_ERROR       = 2
      OTHERS               = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    GV_START = GC_MARK.
    EXIT.
  ENDIF.

ENDFORM.                    " selection_screen
*&---------------------------------------------------------------------*
*&      Form  change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHANGE TABLES  PT_FIELDCAT3 TYPE LVC_T_FCAT.

  DATA:LS_FIELDCAT LIKE PT_FIELDCAT3.

  LOOP AT PT_FIELDCAT3 INTO LS_FIELDCAT.
    IF LS_FIELDCAT-DATATYPE = 'DATS'.
*     ls_fieldcat-INTLEN  = '8'.
    ENDIF.
    MODIFY PT_FIELDCAT3 FROM LS_FIELDCAT INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " change
*&---------------------------------------------------------------------*
*&      Form  p1000_get_down_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_GET_DOWN_DATA .

  SELECT *
    FROM (P_TABLE)
    INTO CORRESPONDING FIELDS OF TABLE  <GT_TABLE>.
  IF SY-SUBRC <> 0.
    MESSAGE S001 WITH 'Download# #### ####.!!!'.
    GV_START = GC_MARK.
    EXIT.
  ENDIF.

ENDFORM.                    " p1000_get_down_data
*&---------------------------------------------------------------------*
*&      Form  p1000_get_up_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_GET_UP_DATA .

  DATA: LV_FIELDNAME(20)  TYPE C,
        LV_FUNCNAME       LIKE TFDIR-FUNCNAME.

  FIELD-SYMBOLS: <LS>.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                      = GV_FILENAME
*     FILETYPE                      = 'ASC'
      HAS_FIELD_SEPARATOR           = 'X'
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*     CODEPAGE                      = ' '
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*   IMPORTING
*     FILELENGTH                    =
*     HEADER                        =
    TABLES
      DATA_TAB                      = <GT_TABLE>
    EXCEPTIONS
      FILE_OPEN_ERROR               = 1
      FILE_READ_ERROR               = 2
      NO_BATCH                      = 3
      GUI_REFUSE_FILETRANSFER       = 4
      INVALID_TYPE                  = 5
      NO_AUTHORITY                  = 6
      UNKNOWN_ERROR                 = 7
      BAD_DATA_FORMAT               = 8
      HEADER_NOT_ALLOWED            = 9
      SEPARATOR_NOT_ALLOWED         = 10
      HEADER_TOO_LONG               = 11
      UNKNOWN_DP_ERROR              = 12
      ACCESS_DENIED                 = 13
      DP_OUT_OF_MEMORY              = 14
      DISK_FULL                     = 15
      DP_TIMEOUT                    = 16
      OTHERS                        = 17.

  IF SY-SUBRC <> 0.
    MESSAGE S001 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    GV_START = GC_MARK.
    EXIT.
  ELSE.
    LOOP AT <GT_TABLE> INTO <GT_LINE>.
      LOOP AT GT_FIELDCAT INTO GS_FIELDCAT WHERE CONVEXIT <> SPACE.
        CLEAR: LV_FUNCNAME, LV_FIELDNAME, NAME, PARA_TAB.
        REFRESH: PARA_TAB, EXCP_TAB.
        CONCATENATE P_TABLE '-'  GS_FIELDCAT-FIELDNAME
               INTO LV_FIELDNAME.
        CREATE DATA DREF  LIKE LV_FIELDNAME.
        ASSIGN DREF->* TO <FS>.
        ASSIGN COMPONENT GS_FIELDCAT-FIELDNAME OF STRUCTURE <GT_LINE>
                      TO <LS>.

        CONCATENATE 'CONVERSION_EXIT_' GS_FIELDCAT-CONVEXIT '_INPUT'
               INTO LV_FUNCNAME.
        CONDENSE LV_FUNCNAME.
        NAME = LV_FUNCNAME.

        PARA_LINE-NAME = 'INPUT'.
        PARA_LINE-KIND = ABAP_FUNC_EXPORTING.
        GET REFERENCE OF <LS> INTO PARA_LINE-VALUE.
        APPEND PARA_LINE TO PARA_TAB.

        PARA_LINE-NAME = 'OUTPUT'.
        PARA_LINE-KIND = ABAP_FUNC_IMPORTING.
        GET REFERENCE OF <LS> INTO PARA_LINE-VALUE.
        APPEND PARA_LINE TO PARA_TAB.

        CALL FUNCTION NAME
          PARAMETER-TABLE
            PARA_TAB
          EXCEPTION-TABLE
            EXCP_TAB.
      ENDLOOP.

      LOOP AT GT_FIELDCAT INTO GS_FIELDCAT WHERE FIELDNAME = 'MANDT'.
        CLEAR: LV_FUNCNAME, LV_FIELDNAME, NAME, PARA_TAB.
        REFRESH: PARA_TAB, EXCP_TAB.
        CONCATENATE P_TABLE '-'  GS_FIELDCAT-FIELDNAME
               INTO LV_FIELDNAME.
        CREATE DATA DREF  LIKE LV_FIELDNAME.
        ASSIGN DREF->* TO <FS>.
        ASSIGN COMPONENT GS_FIELDCAT-FIELDNAME OF STRUCTURE <GT_LINE>
                      TO <LS>.

        MOVE SY-MANDT TO <LS>.

      ENDLOOP.

      MODIFY <GT_TABLE> FROM <GT_LINE>.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " p1000_get_up_data
**&--------------------------------------------------------------------*
**&      Form  QUERY_FILENAME
**&--------------------------------------------------------------------*
**       text
**---------------------------------------------------------------------*
*FORM query_filename USING def_path LIKE rlgrap-filename
*                          mode     TYPE c.
*
*  DATA: tmp_filename LIKE rlgrap-filename.
*  DATA: tmp_mask LIKE global_filemask_all.
*  FIELD-SYMBOLS: <tmp_sym>.
*
** Build Filter for Fileselektor
*
*  IF global_filemask_mask IS INITIAL.
*    tmp_mask = ',*.*,*.*.'.
**    TMP_MASK = ',*.XLS,*.XLS.'.
*  ELSE.
*    tmp_mask = ','.
*    WRITE global_filemask_text TO tmp_mask+1.
*    WRITE ',' TO tmp_mask+21.
*    WRITE global_filemask_mask TO tmp_mask+22.
*    WRITE '.' TO tmp_mask+42.
*    CONDENSE tmp_mask NO-GAPS.
*  ENDIF.
*
*  IF NOT global_filemask_all IS INITIAL.
*    tmp_mask = global_filemask_all.
*  ENDIF.
*
*  fieldln = STRLEN( def_path ) - 1.
*  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
*  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
*    CLEAR <tmp_sym>.
*  ENDIF.
*
*  CALL FUNCTION 'WS_FILENAME_GET'
*       EXPORTING
*            def_filename     = rlgrap-filename
*            def_path         = def_path
**           MASK             = ',*.*,*.*.'
*            mask             = tmp_mask
*            mode             = mode
**           TITLE            = ' '
*       IMPORTING
*            filename         = tmp_filename
**         RC               =
*       EXCEPTIONS
*            inv_winsys       = 01
*            no_batch         = 02
*            selection_cancel = 03
*            selection_error  = 04.
*
*  IF sy-subrc = 0.
*    rlgrap-filename = tmp_filename.
*    p_file = tmp_filename.
*  ELSE.
** IF SY-SUBRC = 01.    "// Does not work, why ???
**   MESSAGELINE = 'Not supported'.
** ENDIF.
*  ENDIF.
*
*ENDFORM.                    " QUERY_FILENAME
*&---------------------------------------------------------------------*
*&      Form  SCR100_DOWN_LOAD_TO_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCR100_DOWN_LOAD_TO_LOCAL.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      FILENAME                        = GV_FILENAME
*     FILETYPE                        = 'ASC'
*     APPEND                          = ' '
      WRITE_FIELD_SEPARATOR           = 'X'
*     HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
    IMPORTING
      FILELENGTH                      = GV_FILELENGTH
    TABLES
      DATA_TAB                        = <GT_TABLE>
    EXCEPTIONS
      FILE_WRITE_ERROR                = 1
      NO_BATCH                        = 2
      GUI_REFUSE_FILETRANSFER         = 3
      INVALID_TYPE                    = 4
      NO_AUTHORITY                    = 5
      UNKNOWN_ERROR                   = 6
      HEADER_NOT_ALLOWED              = 7
      SEPARATOR_NOT_ALLOWED           = 8
      FILESIZE_NOT_ALLOWED            = 9
      HEADER_TOO_LONG                 = 10
      DP_ERROR_CREATE                 = 11
      DP_ERROR_SEND                   = 12
      DP_ERROR_WRITE                  = 13
      UNKNOWN_DP_ERROR                = 14
      ACCESS_DENIED                   = 15
      DP_OUT_OF_MEMORY                = 16
      DISK_FULL                       = 17
      DP_TIMEOUT                      = 18
      FILE_NOT_FOUND                  = 19
      DATAPROVIDER_EXCEPTION          = 20
      CONTROL_FLUSH_ERROR             = 21
      OTHERS                          = 22.

  IF SY-SUBRC <> 0.
    MESSAGE S001 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    MESSAGE S001 WITH  '#### ##'   GV_FILELENGTH.
    GV_FIRST_DISPLAY = GC_MARK.
  ENDIF.

ENDFORM.                    " SCR100_DOWN_LOAD_TO_LOCAL
*&---------------------------------------------------------------------*
*&      Form  SCR100_UPLOAD_TO_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCR100_UPLOAD_TO_LOCAL.

  MODIFY  (P_TABLE)  CLIENT SPECIFIED
                     FROM  TABLE   <GT_TABLE>.
*  INSERT  (p_table)  FROM  TABLE   <gt_table>.
  IF SY-SUBRC   EQ  0.
    MESSAGE  S001 WITH  'succeed upload'   .
    GV_FIRST_DISPLAY = GC_MARK.
  ELSE.
    MESSAGE  S001 WITH  'fail upload'   .
  ENDIF.

ENDFORM.                    " SCR100_UPLOAD_TO_LOCAL
*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

* pf-status setting
  IF RB_DOW  EQ 'X'.
    SET TITLEBAR  'DOWN'.
  ELSE.
    SET TITLEBAR  'UP'.
  ENDIF.
  IF GV_FIRST_DISPLAY IS INITIAL.
    SET PF-STATUS 'STATUS'.
  ELSE.
    SET PF-STATUS 'STATUS' EXCLUDING 'EXEC'.
  ENDIF.

* Custom-control
  IF GV_DIALOG_CONTAINER IS INITIAL.
    PERFORM SCR100_CREATE_CONTAINER.               " Create Custom CTL
*    PERFORM scr100_fieldcat_init.                  " Fieldcatalog
    PERFORM SCR100_OUTPUT_LIST.                    " ALV
  ENDIF.

ENDMODULE.                 " status_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  scr100_create_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCR100_CREATE_CONTAINER .

  DATA: REPID     TYPE SY-REPID,       " Program name
        DYNNR     TYPE SY-DYNNR.       " Screen number

  DATA: L_LINK    TYPE SWD_HTMLCO.
  DATA: L_RSFB    TYPE RSFBTLTAB,
        L_DATUM(10),
        L_RSFB_HEADER     TYPE LINE OF RSFBTLTAB.

  DATA  LT_HEADER         TYPE TREEV_HHDR.

  DATA: LT_CONTROL_LIST   TYPE CNTO_CONTROL_LIST.

* create a container for the tree control
  REPID = SY-REPID.
  DYNNR = SY-DYNNR.

  CREATE OBJECT GV_DIALOG_CONTAINER
    EXPORTING
      STYLE                       = CL_GUI_CONTROL=>WS_CHILD
      REPID                       = REPID
      DYNNR                       = DYNNR
      LIFETIME                    = CL_GUI_CONTROL=>LIFETIME_IMODE
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      EVENT_ALREADY_REGISTERED    = 6
      ERROR_REGIST_EVENT          = 7
      OTHERS                      = 8.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

* Assigned ALV Grid
  CREATE OBJECT GV_GRID
    EXPORTING
      I_PARENT = GV_DIALOG_CONTAINER.
*      i_shellstyle    =  3.

ENDFORM.                    " scr100_create_container
*&---------------------------------------------------------------------*
*&      Form  scr100_fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCR100_FIELDCAT_INIT .

  DATA: LS_FIELDCAT  TYPE  LVC_S_FCAT.

  CLEAR: LS_FIELDCAT.
  LOOP AT GT_FIELDCAT INTO LS_FIELDCAT.
*    ls_fieldcat-input      = 'X'.
    LS_FIELDCAT-EDIT       = 'X'.
    MODIFY GT_FIELDCAT FROM LS_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " scr100_fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  scr100_output_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SCR100_OUTPUT_LIST .

  DATA: LV_HEADER_TEXT(70)    TYPE C.

  GS_LAYOUT-SMALLTITLE = '2'.                        "
  GS_LAYOUT-SEL_MODE   = 'A'.
  GS_LAYOUT-DETAILINIT = 'X'.                        " Detail

  CALL METHOD GV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*     i_structure_name              = p_table
      I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
    CHANGING
      IT_OUTTAB                     = <GT_TABLE>
      IT_FIELDCATALOG               = GT_FIELDCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      OTHERS                        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LV_HEADER_TEXT = P_TABLE.
  TRANSLATE LV_HEADER_TEXT TO UPPER CASE.
  IF RB_DOW  EQ 'X'.
    CONCATENATE LV_HEADER_TEXT 'Table Download'
           INTO LV_HEADER_TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE LV_HEADER_TEXT 'Table Upload'
           INTO LV_HEADER_TEXT SEPARATED BY SPACE.
  ENDIF.
  CALL METHOD GV_GRID->SET_GRIDTITLE
    EXPORTING
      I_GRIDTITLE = LV_HEADER_TEXT.

ENDFORM.                    " scr100_output_list
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  IF NOT GV_DIALOG_CONTAINER IS INITIAL.
    CALL METHOD GV_GRID->FREE.
    CALL METHOD GV_DIALOG_CONTAINER->FREE.
    CLEAR GV_GRID.
    CLEAR GV_DIALOG_CONTAINER.
  ENDIF.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR GV_OKCODE.
  GV_OKCODE = OK_CODE.
  CLEAR OK_CODE.

  CASE GV_OKCODE.
    WHEN 'EXEC'.
      IF RB_DOW  EQ 'X'.
        PERFORM SCR100_DOWN_LOAD_TO_LOCAL.
      ELSE.
        PERFORM SCR100_UPLOAD_TO_LOCAL.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  p1000_convert_to_intern
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_CONVERT_TO_INTERN.

  DATA: G_UTIL   TYPE REF TO CL_FOBU_INPUT_UTIL.
  DATA: LS_STRUC TYPE DFIES.
  DATA: DREF     TYPE REF TO DATA.
  DATA: LV_FIELDNAME(20)  TYPE C,
        LV_FUNCNAME       LIKE TFDIR-FUNCNAME.

  FIELD-SYMBOLS: <LS>,
                 <LS1>.

*  LOOP AT gt_fieldcat INTO gs_fieldcat WHERE lowercase  <> 'X'
*                                          OR convexit   <> space
*                                          OR currency   <> space
*                                          OR cfieldname <> space.
*    CLEAR: lv_funcname, lv_fieldname.
*    CONCATENATE p_table '-'  gs_fieldcat-fieldname
*           INTO lv_fieldname.
*    CREATE DATA dref  LIKE lv_fieldname.
*    ASSIGN dref->* TO <fs>.
*    ASSIGN COMPONENT gs_fieldcat-fieldname OF STRUCTURE <gt_line>
*                  TO <ls>.
*    IF gs_fieldcat-lowercase <> 'X'.
*      TRANSLATE <ls> TO UPPER CASE.
*    ENDIF.
**    IF gs_fieldcat-convexit <> space.
**      CREATE OBJECT g_util
***        EXPORTING
***          TYPENAME       =
***          TABNAME        =
***          FIELDNAME      =
***          DYNPNR         =
***          DYNPPROG       =
***          DYNPROFIELD    =
***          CURRENCY       =
***          LENGTH         =
***          DECIMALS       =
**        EXCEPTIONS
**          not_found      = 1
**          internal_error = 2
**          OTHERS         = 3.
**      IF sy-subrc <> 0.
**        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**      ENDIF.
**
**    ENDIF.
**    IF gs_fieldcat-currency   <> space OR
**       gs_fieldcat-cfieldname <> space.
**
**    ENDIF.
*    CALL FUNCTION 'SMAN_IF_CONVERT_TO_INTERN'
*      EXPORTING
*        ls_struc              = gt_dfies
*        ld_input_value        = <ls>
**       I_GET_EXCEP           = ' '
*      IMPORTING
*        ld_output_value       = <ls>
*      EXCEPTIONS
*        wrong_input           = 1
*        OTHERS                = 2.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*  ENDLOOP.
  LOOP AT GT_DFIES WHERE LOWERCASE <> 'X'
                      OR CONVEXIT  <> SPACE
                      OR REFFIELD  <> SPACE.
    CLEAR: LV_FUNCNAME, LV_FIELDNAME.
    CONCATENATE P_TABLE '-'  GT_DFIES-FIELDNAME
           INTO LV_FIELDNAME.
*    CREATE DATA dref  LIKE lv_fieldname.
*    ASSIGN dref->* TO <fs>.
    ASSIGN COMPONENT GT_DFIES-FIELDNAME OF STRUCTURE <GT_LINE>
                  TO <LS>.
    IF GT_DFIES-LOWERCASE <> 'X' AND GT_DFIES-INTTYPE = 'C'.
      TRANSLATE <LS> TO UPPER CASE.
    ENDIF.

    CHECK GT_DFIES-CONVEXIT <> SPACE OR GT_DFIES-REFFIELD <> SPACE.
    IF GT_DFIES-ROLLNAME <> SPACE.
      IF GT_DFIES-CONVEXIT <> SPACE.
        CREATE OBJECT G_UTIL
         EXPORTING
           TYPENAME       = GT_DFIES-ROLLNAME
*          TABNAME        =
*          FIELDNAME      =
*          DYNPNR         =
*          DYNPPROG       =
*          DYNPROFIELD    =
*          CURRENCY       =
*          LENGTH         =
*          DECIMALS       =
          EXCEPTIONS
            NOT_FOUND      = 1
            INTERNAL_ERROR = 2
            OTHERS         = 3.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSEIF GT_DFIES-REFFIELD <> SPACE.
        CLEAR: LV_FIELDNAME.
        CONCATENATE GT_DFIES-REFTABLE '-'  GT_DFIES-REFFIELD
               INTO LV_FIELDNAME.
        CREATE DATA DREF  LIKE LV_FIELDNAME.
        ASSIGN DREF->* TO <LS1>.
        IF P_TABLE = GT_DFIES-REFTABLE.
          ASSIGN COMPONENT GT_DFIES-REFFIELD OF STRUCTURE <GT_LINE>
                        TO <LS1>.
        ELSE.

        ENDIF.
        CREATE OBJECT G_UTIL
         EXPORTING
           TYPENAME       = GT_DFIES-ROLLNAME
*          TABNAME        =
*          FIELDNAME      =
*          DYNPNR         =
*          DYNPPROG       =
*          DYNPROFIELD    =
           CURRENCY       = <LS1>
*          LENGTH         =
*          DECIMALS       =
          EXCEPTIONS
            NOT_FOUND      = 1
            INTERNAL_ERROR = 2
            OTHERS         = 3.
        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.

*....convert to internal view, no check table checked
      CALL METHOD G_UTIL->INPUT_CONVERT
        EXPORTING
          FIELD_VALUE_EXT   = <LS>
        IMPORTING
          FIELD_VALUE_INT_C = <LS>.
    ELSE.
      CALL FUNCTION 'SMAN_IF_CONVERT_TO_INTERN'
        EXPORTING
          LS_STRUC              = GT_DFIES
          LD_INPUT_VALUE        = <LS>
*         I_GET_EXCEP           = ' '
        IMPORTING
          LD_OUTPUT_VALUE       = <LS>
        EXCEPTIONS
          WRONG_INPUT           = 1
          OTHERS                = 2.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " p1000_convert_to_intern
