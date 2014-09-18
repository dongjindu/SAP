*----------------------------------------------------------------------*
*   INCLUDE ZRPP_FUNC_ALV                                              *
*----------------------------------------------------------------------*

INCLUDE <ICON>.

*===============================================================
*_TYPE POOLS

TYPE-POOLS : SLIS,
             KKBLO.


*===============================================================
*_FIELD-SYMBOLS

FIELD-SYMBOLS : <EXCEL> TYPE ANY.


*===============================================================
*_Ranges macro

DEFINE DG_SELECTION .
  &1-SIGN   = &2.
  &1-OPTION = &3.
  &1-LOW    = &4.
  &1-HIGH   = &5.
  APPEND &1 . CLEAR &1.
END-OF-DEFINITION.


*===============================================================
*_ALV GRID/LIST 관련 변수(테이블 및 변수)

DATA: GS_DATA_CHANGEDTYPE TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.



*_Internal Table

DATA : G_EVENTS_T TYPE SLIS_T_EVENT,
       G_LIST_TOP_OF_PAGE_T     TYPE SLIS_T_LISTHEADER,
       IT_LISTBOTTOM            TYPE SLIS_T_LISTHEADER,
       G_FIELDCAT_T             TYPE SLIS_T_FIELDCAT_ALV,
       G_FIELD_T                TYPE SLIS_T_FIELDCAT_ALV,
       G_SORT_T                 TYPE SLIS_T_SORTINFO_ALV,
       G_SP_GROUP_T             TYPE SLIS_T_SP_GROUP_ALV.
DATA : G_FIELD_S TYPE SLIS_FIELDCAT_ALV.

DATA : G_LAYOUT_S TYPE SLIS_LAYOUT_ALV,
       G_EXIT_CAUSED_BY_USER_S  TYPE SLIS_EXIT_BY_USER,
       G_FIELDCAT_S             TYPE SLIS_FIELDCAT_ALV,
       G_SORT_S                 TYPE SLIS_SORTINFO_ALV,
*       G_RUNMODE_S              TYPE AQLIMODE,
       G_SP_GROUP_S             TYPE SLIS_SP_GROUP_ALV,
       G_PRINT_S                TYPE SLIS_PRINT_ALV,
       G_KEYINFO_S              TYPE SLIS_KEYINFO_ALV,
       G_SELFIELD               TYPE SLIS_SELFIELD ,
       G_EXTAB                  TYPE SLIS_T_EXTAB  .

DATA : GS_GRIDSET TYPE LVC_S_GLAY,
       GS_GLAY      TYPE LVC_S_GLAY,
       GS_LINEINFO  TYPE KKBLO_LINEINFO.

DATA : E_EXIT_CAUSED_BY_CALLER,
       ES_EXIT_CAUSED_BY_USER TYPE  SLIS_EXIT_BY_USER.

*

DATA : G_EXIT_CAUSED_BY_CALLER TYPE C,
       G_SAVE                   TYPE C,
       G_DEFAULT                TYPE C  VALUE '',
       G_REPID                  TYPE SY-REPID,
       G_PROGRAM                LIKE SY-REPID,
       G_ERROR ,
       G_RETURN(100).


*_ALV VARIANT VARIABLE

DATA : G_TABNAME_HEADER TYPE SLIS_TABNAME,
       G_TABNAME_ITEM    TYPE  SLIS_TABNAME,
       G_VARIANT_S       TYPE  DISVARIANT,
       G_EXIT(1)         TYPE  C,
       GX_VARIANT        LIKE  DISVARIANT.

DATA : WG_VARI LIKE DISVARIANT-VARIANT.

* message popup.

*DATA : GT_MESSAGE TYPE BAPIRETTAB.
DATA : GS_MESSAGE LIKE BAPIRETURN.

*DATA : G_ANSWER .
*===============================================================
*_SCREEN 변수

DATA : "OK_CODE(70) ,
       G_COUNT TYPE I,
       G_UCOMM TYPE SYUCOMM.


*===============================================================
*_ALV POPUP LIST 변수

CONSTANTS :
  C_POP_STATUS_SET TYPE SLIS_FORMNAME
                   VALUE 'POP_PF_STATUS',
  C_POP_USER_COMMAND TYPE SLIS_FORMNAME
                     VALUE 'POP_USER_COMMAND'.

DATA : GT_POP_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
       GT_CHA_FIELDCAT  TYPE KKBLO_T_FIELDCAT,
       GS_POP_LAYOUT    TYPE KKBLO_LAYOUT.
DATA : GS_POP_PRIVATE TYPE SLIS_DATA_CALLER_EXIT.
DATA : GT_POP_EXTAB TYPE KKBLO_T_EXTAB.
DATA : G_POP_TITLE TYPE REPTI.

*===============================================================
*_CONSTANTS

CONSTANTS :
  C_STATUS_SET       TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
  C_USER_COMMAND     TYPE SLIS_FORMNAME VALUE 'USER_COMMAND' ,
  C_TOP_OF_PAGE      TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE'  ,
  C_TOP_OF_LIST      TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST'  ,
  C_END_OF_LIST      TYPE SLIS_FORMNAME VALUE 'END_OF_LIST'  ,
  C_DATA_CHANGED     TYPE SLIS_FORMNAME VALUE 'DATA_CHANGED' ,
  C_DATA_EXPAND  TYPE SLIS_FORMNAME VALUE 'ITEM_DATA_EXPAND' ,
  C_SEL_MODIFY   TYPE SLIS_FORMNAME VALUE 'REPREP_SEL_MODIFY',
  C_AT_START     TYPE SLIS_FORMNAME VALUE 'CALLER_EXIT'      ,
  C_COVERPAGE    TYPE SLIS_FORMNAME VALUE 'END_OF_COVERPAGE' ,
  C_MODIFY       TYPE SLIS_FORMNAME VALUE 'LIST_MODIFY'      ,
  C_END_OF_PAGE  TYPE SLIS_FORMNAME VALUE 'END_OF_PAGE'      ,
  C_AFTER_LINE_OUTPUT
                 TYPE SLIS_FORMNAME VALUE 'AFTER_LINE_OUTPUT',
  C_BEFORE_LINE_OUTPUT
                TYPE SLIS_FORMNAME VALUE 'BEFORE_LINE_OUTPUT',
  C_SUBTOTAL_TEXT
                 TYPE SLIS_FORMNAME VALUE 'SUBTOTAL_TEXT'    ,
  C_CONTEXT_MENU TYPE SLIS_FORMNAME VALUE 'CONTEXT_MENU'     .



*===============================================================
*_LAV GRID/LIST FORM
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*

FORM SET_LAYOUT
     USING P_COPT
           P_LIGHTS
           P_COLOR
           P_BOX.

  CLEAR G_LAYOUT_S.
  G_LAYOUT_S-COLWIDTH_OPTIMIZE       = P_COPT.
  G_LAYOUT_S-LIGHTS_FIELDNAME        = P_LIGHTS.
  IF P_COLOR EQ 'COLOR'.
    G_LAYOUT_S-INFO_FIELDNAME        = P_COLOR .
  ELSE.
    G_LAYOUT_S-COLTAB_FIELDNAME      = P_COLOR.
  ENDIF.
  G_LAYOUT_S-BOX_FIELDNAME           = P_BOX.
  G_LAYOUT_S-ZEBRA                   = 'X' .

* 계층구조시 사용.
*  G_LAYOUT_S-EXPAND_FIELDNAME        = P_EXPAND.



ENDFORM. "SET_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_TOT
*&---------------------------------------------------------------------*

FORM SET_LAYOUT_TOT
     USING P_COPT
           P_LIGHTS
           P_COLOR
           P_BOX
           P_TOTAL_TEXT.

  CLEAR G_LAYOUT_S.
  G_LAYOUT_S-COLWIDTH_OPTIMIZE       = P_COPT.
  G_LAYOUT_S-LIGHTS_FIELDNAME        = P_LIGHTS.
  G_LAYOUT_S-COLTAB_FIELDNAME        = P_COLOR.
  IF P_COLOR EQ 'COLOR'.
    G_LAYOUT_S-INFO_FIELDNAME        = P_COLOR .
  ELSE.
    G_LAYOUT_S-COLTAB_FIELDNAME      = P_COLOR.
  ENDIF.

  G_LAYOUT_S-BOX_FIELDNAME           = P_BOX.

* 계층구조시 사용.
*  G_LAYOUT_S-EXPAND_FIELDNAME        = P_EXPAND.


  G_LAYOUT_S-TOTALS_TEXT = P_TOTAL_TEXT.
  G_LAYOUT_S-ITEM_TEXT = P_TOTAL_TEXT.

ENDFORM. "SET_LAYOUT_TOT



*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCATALOG
*&---------------------------------------------------------------------*

FORM GET_FILEDCATALOG
     TABLES PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
     USING P_TAB.

  CLEAR : PT_FIELDCAT[].
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME         = GV_REPID
            I_INTERNAL_TABNAME     = P_TAB
            i_buffer_active = 'X'
            I_BYPASSING_BUFFER     = 'X'
            I_INCLNAME             = GV_REPID
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

ENDFORM. " GET_FILEDCATALOG


*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCATALOG_NO
*&---------------------------------------------------------------------*

FORM GET_FILEDCATALOG_NO TABLES PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
                         USING P_TAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME         = G_REPID
            I_INTERNAL_TABNAME     = P_TAB
            I_BYPASSING_BUFFER     = 'X'
            I_INCLNAME             = G_REPID
       CHANGING
            CT_FIELDCAT            = PT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

ENDFORM. " GET_FILEDCATALOG


*---------------------------------------------------------------------*
*      Form  DISPLAY_ALV
*---------------------------------------------------------------------*
*      ALV_GRID_DISPLAY                                               *
*---------------------------------------------------------------------*

FORM DISPLAY_GRID_ALV TABLES PT_TAB.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = GV_REPID
*      I_DEFAULT          = G_DEFAULT
      I_GRID_SETTINGS    = GS_GLAY
      IS_LAYOUT          = G_LAYOUT_S
      IS_VARIANT         = G_VARIANT_S
      IT_FIELDCAT        = G_FIELDCAT_T[]
      IT_EVENTS          = G_EVENTS_T[]
      IT_SORT            = G_SORT_T[]
      I_SAVE             = 'A'
      I_BUFFER_ACTIVE    = 'X'
      I_BYPASSING_BUFFER     = 'X'
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER = P_RETURN

    TABLES
      T_OUTTAB           = PT_TAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
** EXPORTING
**   I_INTERFACE_CHECK                 = ' '
**   I_BYPASSING_BUFFER                =
**   I_BUFFER_ACTIVE                   = ' '
**   I_CALLBACK_PROGRAM                = ' '
**   I_CALLBACK_PF_STATUS_SET          = ' '
**   I_CALLBACK_USER_COMMAND           = ' '
**   I_CALLBACK_TOP_OF_PAGE            = ' '
**   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**   I_CALLBACK_HTML_END_OF_LIST       = ' '
**   I_STRUCTURE_NAME                  =
**   I_BACKGROUND_ID                   = ' '
**   I_GRID_TITLE                      =
**   I_GRID_SETTINGS                   =
**   IS_LAYOUT                         =
**   IT_FIELDCAT                       =
**   IT_EXCLUDING                      =
**   IT_SPECIAL_GROUPS                 =
**   IT_SORT                           =
**   IT_FILTER                         =
**   IS_SEL_HIDE                       =
**   I_DEFAULT                         = 'X'
**   I_SAVE                            = ' '
**   IS_VARIANT                        =
**   IT_EVENTS                         =
**   IT_EVENT_EXIT                     =
**   IS_PRINT                          =
**   IS_REPREP_ID                      =
**   I_SCREEN_START_COLUMN             = 0
**   I_SCREEN_START_LINE               = 0
**   I_SCREEN_END_COLUMN               = 0
**   I_SCREEN_END_LINE                 = 0
**   IT_ALV_GRAPHICS                   =
**   IT_ADD_FIELDCAT                   =
**   IT_HYPERLINK                      =
**   I_HTML_HEIGHT_TOP                 =
**   I_HTML_HEIGHT_END                 =
**   IT_EXCEPT_QINFO                   =
** IMPORTING
**   E_EXIT_CAUSED_BY_CALLER           =
**   ES_EXIT_CAUSED_BY_USER            =
*  TABLES
*    T_OUTTAB                          =
** EXCEPTIONS
**   PROGRAM_ERROR                     = 1
**   OTHERS                            = 2
*          .
*IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

ENDFORM. "DISPLAY_ALV


*---------------------------------------------------------------------*
*      Form  DISPLAY_ALV
*---------------------------------------------------------------------*
*      ALV_GRID_DISPLAY_R
*---------------------------------------------------------------------*

FORM DISPLAY_GRID_ALV_R TABLES PT_TAB
                        USING   P_RETURN.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = G_REPID
            I_DEFAULT               = G_DEFAULT
            I_GRID_SETTINGS         = GS_GLAY
            IS_LAYOUT               = G_LAYOUT_S
            IS_VARIANT              = G_VARIANT_S
            IT_FIELDCAT             = G_FIELDCAT_T[]
            IT_EVENTS               = G_EVENTS_T[]
            IT_SORT                 = G_SORT_T[]
            I_SAVE                  = 'A'
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER = P_RETURN
       TABLES
            T_OUTTAB                = PT_TAB
       EXCEPTIONS
            PROGRAM_ERROR           = 1
            OTHERS                  = 2.


ENDFORM. "DISPLAY_GRID_ALV_R


*---------------------------------------------------------------------*
*      Form  1000_DISPLAY_ALV
*---------------------------------------------------------------------*
*      ALV_GRID_DISPLAY                                               *
*---------------------------------------------------------------------*

FORM 1000_DISPLAY_ALV TABLES PT_TAB.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_USER_COMMAND  = C_USER_COMMAND
            I_CALLBACK_PF_STATUS_SET = C_STATUS_SET
            I_DEFAULT                = G_DEFAULT
            IS_LAYOUT                = G_LAYOUT_S
            IS_VARIANT               = G_VARIANT_S
            IT_FIELDCAT              = G_FIELDCAT_T[]
            IT_EVENTS                = G_EVENTS_T[]
            IT_SORT                  = G_SORT_T[]
            I_SAVE                   = 'A'
            I_GRID_SETTINGS          = GS_GRIDSET
       IMPORTING
            E_EXIT_CAUSED_BY_CALLER  = E_EXIT_CAUSED_BY_CALLER
            ES_EXIT_CAUSED_BY_USER   = ES_EXIT_CAUSED_BY_USER
       TABLES
            T_OUTTAB                 = PT_TAB
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

  IF ES_EXIT_CAUSED_BY_USER-BACK = 'X' OR
  ES_EXIT_CAUSED_BY_USER-EXIT = 'X' OR
  ES_EXIT_CAUSED_BY_USER-CANCEL = 'X'.

*    LEAVE PROGRAM.
*    LEAVE TO SCREEN 0.
*LEAVE TO TRANSACTION sy-CPROG AND SKIP FIRST SCREEN .

  ENDIF.

ENDFORM. "1000_DISPLAY_ALV



*---------------------------------------------------------------------*
*      Form  1000_DISPLAY_LIST_ALV
*---------------------------------------------------------------------*
*      ALV_LIST_DISPLAY                                               *
*---------------------------------------------------------------------*

FORM 1000_DISPLAY_LIST_ALV TABLES PT_TAB.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = G_REPID
      I_CALLBACK_USER_COMMAND  = C_USER_COMMAND
      I_CALLBACK_PF_STATUS_SET = C_STATUS_SET
      I_DEFAULT                = G_DEFAULT
      IS_LAYOUT                = G_LAYOUT_S
      IS_VARIANT               = G_VARIANT_S
      IT_FIELDCAT              = G_FIELDCAT_T[]
      IT_EVENTS                = G_EVENTS_T[]
      IT_SORT                  = G_SORT_T[]
      I_SAVE                   = 'A'

*      I_GRID_SETTINGS          = GS_GRIDSET

    IMPORTING
      E_EXIT_CAUSED_BY_CALLER  = E_EXIT_CAUSED_BY_CALLER
      ES_EXIT_CAUSED_BY_USER   = ES_EXIT_CAUSED_BY_USER
    TABLES
      T_OUTTAB                 = PT_TAB
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF ES_EXIT_CAUSED_BY_USER-BACK = 'X' OR
  ES_EXIT_CAUSED_BY_USER-EXIT = 'X' OR
  ES_EXIT_CAUSED_BY_USER-CANCEL = 'X'.

*    LEAVE PROGRAM.

  ENDIF.

ENDFORM. "1000_DISPLAY_LIST_ALV

*---------------------------------------------------------------------*
*      Form  1000_DISPLAY_ALV_LIST
*---------------------------------------------------------------------*
*      ALV_LIST_DISPLAY                                               *
*---------------------------------------------------------------------*

FORM 1000_DISPLAY_ALV_LIST TABLES PT_TAB.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'

    EXPORTING
      I_CALLBACK_PROGRAM = G_REPID
      I_CALLBACK_USER_COMMAND  = C_USER_COMMAND
      I_CALLBACK_PF_STATUS_SET = C_STATUS_SET
      I_DEFAULT          = G_DEFAULT
      IS_LAYOUT          = G_LAYOUT_S
      IS_VARIANT         = G_VARIANT_S

*      IS_VARIANT         = GX_VARIANT

      IT_FIELDCAT        = G_FIELDCAT_T[]
      IT_EVENTS          = G_EVENTS_T[]
      IT_SORT            = G_SORT_T[]
      I_SAVE             = 'A'

*      I_GRID_SETTINGS    = GS_GRIDSET
*    IMPORTING
*      E_EXIT_CAUSED_BY_CALLER  = E_EXIT_CAUSED_BY_CALLER
*      ES_EXIT_CAUSED_BY_USER   = ES_EXIT_CAUSED_BY_USER


    TABLES
      T_OUTTAB           = PT_TAB
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.


*  IF ES_EXIT_CAUSED_BY_USER-BACK = 'X' OR
*     ES_EXIT_CAUSED_BY_USER-EXIT = 'X' OR
*     ES_EXIT_CAUSED_BY_USER-CANCEL = 'X'.
**    LEAVE PROGRAM.
*  ENDIF.


ENDFORM. "1000_DISPLAY_ALV_LIST


*---------------------------------------------------------------------*
*      Form  DISPLAY_ALV_LIST
*---------------------------------------------------------------------*
*      ALV_LIST_DISPLAY                                               *
*---------------------------------------------------------------------*

FORM DISPLAY_LIST_ALV TABLES PT_TAB.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = G_REPID
            I_DEFAULT          = G_DEFAULT
            IS_LAYOUT          = G_LAYOUT_S
            IS_VARIANT         = G_VARIANT_S
            IT_FIELDCAT        = G_FIELDCAT_T[]
            IT_EVENTS          = G_EVENTS_T[]
            IT_SORT            = G_SORT_T[]
            I_SAVE             = 'A'
       TABLES
            T_OUTTAB           = PT_TAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.


ENDFORM. "DISPLAY_ALV_LIST


*&---------------------------------------------------------------------*
*&      Form  display_hierseq_alv
*&---------------------------------------------------------------------*

FORM DISPLAY_HIERSEQ_ALV TABLES PT_HEAD
                                   PT_BODY.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID

*           I_CALLBACK_PF_STATUS_SET = ' '
*           I_CALLBACK_USER_COMMAND  = ' '

            IS_LAYOUT                = G_LAYOUT_S
            IT_FIELDCAT              = G_FIELDCAT_T[]

*            IT_SPECIAL_GROUPS        = GT_SP_GROUP[]

            IT_SORT                  = G_SORT_T[]
            I_DEFAULT                = G_DEFAULT
            I_SAVE                   = 'A'
            IS_VARIANT               = G_VARIANT_S
            IT_EVENTS                = G_EVENTS_T[]
            I_TABNAME_HEADER         = G_TABNAME_HEADER
            I_TABNAME_ITEM           = G_TABNAME_ITEM
            IS_KEYINFO               = G_KEYINFO_S
       TABLES
            T_OUTTAB_HEADER          = PT_HEAD
            T_OUTTAB_ITEM            = PT_BODY.



ENDFORM. " display_hierseq_alv



*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*

FORM SET_SORT USING P_FNAME
                     P_TNAME
                     P_UP
                     P_SUBTOT
                     P_EXPA.
  DATA : SORT_COLUMN TYPE SLIS_SORTINFO_ALV .

  CLEAR SORT_COLUMN.
  SORT_COLUMN-FIELDNAME =  P_FNAME.
  SORT_COLUMN-TABNAME   =  P_TNAME.
  SORT_COLUMN-UP        =  P_UP.
  SORT_COLUMN-SUBTOT    =  P_SUBTOT.
  SORT_COLUMN-EXPA      =  P_EXPA  .

*  SORT_COLUMN-GROUP     = 'UL'.

   APPEND SORT_COLUMN TO G_SORT_T.

ENDFORM. " SET_SORT

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*

FORM BUILD_EVENT USING P_NAME .

  DATA : L_EVENT_S TYPE SLIS_ALV_EVENT.

*

  L_EVENT_S-NAME = P_NAME.
  L_EVENT_S-FORM = P_NAME.
  APPEND L_EVENT_S TO G_EVENTS_T.

ENDFORM. " BUILD_EVENT

*---------------------------------------------------------------------*
*      Form  HEADER_SETTING
*---------------------------------------------------------------------*

FORM HEADER_SETTING USING P_TYP P_KEY P_INFO.


* HEADER SETTING

  DATA : L_LINE_S  TYPE SLIS_LISTHEADER.
*
*  CLEAR L_LINE_S.
*  MOVE   P_TYP     TO L_LINE_S-TYP.
*  MOVE   P_KEY     TO L_LINE_S-KEY.
*  MOVE   P_INFO    TO L_LINE_S-INFO.
*  APPEND L_LINE_S  TO G_LIST_TOP_OF_PAGE_T.

ENDFORM. "HEADER_SETTING


*&--------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&--------------------------------------------------------------------*
*       ALV Double Click
*---------------------------------------------------------------------*

FORM USER_COMMAND USING L_UCOMM LIKE SY-UCOMM
                        LS_SELFIELD TYPE SLIS_SELFIELD.

*

  CASE L_UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE SCREEN .
  ENDCASE.

  PERFORM LOCAL_USERCOMMAND IN PROGRAM (SY-REPID) USING L_UCOMM
                                                        LS_SELFIELD.

ENDFORM. "USER_COMMAND

*&--------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&--------------------------------------------------------------------*
*       ALV TOP_OF_PAGE
*---------------------------------------------------------------------*

FORM TOP_OF_PAGE .
  DATA: LD_PAGE(11) TYPE C,
        SY_PAGE(5),
        PAGE(5),
        LINSZ LIKE SY-LINSZ,
        WS_PAGES TYPE I,
        WS_REM TYPE I,
        TITLE_CNT TYPE I,
        LINCT TYPE I,
        HLINSZ TYPE I,
        TCODE(12),
        FILTERED TYPE SLIS_T_FILTERED_ENTRIES,
        ALL TYPE I,
        TITLE TYPE I.
*
*  IF SY-LINCT >= 0.
*
*
***   필터 적용 시 해당 갯수만
**    CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_GET'
**      IMPORTING
**        ET_FILTERED_ENTRIES = FILTERED
**      EXCEPTIONS
**        NO_INFOS            = 1
**        OTHERS              = 3.
**    IF SY-SUBRC <> 0.
**      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
**        IMPORTING
**          ET_FILTERED_ENTRIES = FILTERED
**        EXCEPTIONS
**          NO_INFOS            = 1
**          OTHERS              = 3.
**    ENDIF.
**
**    IF NOT FILTERED[] IS INITIAL.
**      DESCRIBE TABLE FILTERED LINES ALL.
**      ALL = G_COUNT - ALL.
**    ELSE.
*
*    ALL = G_COUNT.
*
**    ENDIF.
*
*
*    LINCT = SY-LINCT - 7.
*    WS_PAGES = ALL DIV LINCT.
*    WS_REM   = ALL MOD LINCT.
*    IF WS_REM > 0.
*      ADD 1 TO WS_PAGES.
*    ENDIF.
*
*    WRITE SY-PAGNO TO SY_PAGE RIGHT-JUSTIFIED.
*    WRITE WS_PAGES TO PAGE RIGHT-JUSTIFIED.
*    CONCATENATE SY_PAGE '/' PAGE INTO LD_PAGE ."RESPECTING BLANKS.
*    CONDENSE LD_PAGE.
*    TCODE = SY-TCODE. CONDENSE TCODE.
*
*    LINSZ  = SY-LINSZ.
*    TITLE = STRLEN( SY-TITLE ).
*    LINSZ  = LINSZ - 20.
*    HLINSZ = ( LINSZ / 2 ) - ( TITLE / 2 ).
*
*
**   프린트 용 헤더
*
*    WRITE: AT HLINSZ SY-TITLE,
*           AT LINSZ 'Tcode : ', (12) TCODE,
*           / '일자/시간 : ', SY-DATUM, SY-UZEIT,
*           AT LINSZ 'Page  : ', (12) LD_PAGE.
*
*  ELSE.

    PERFORM LOCAL_TOPOFPAGE IN PROGRAM (SY-REPID)
            IF FOUND
            USING ''.

*  ENDIF.


ENDFORM. "TOP_OF_PAGE


*&--------------------------------------------------------------------*
*&      Form  END_OF_LIST
*&--------------------------------------------------------------------*
*       ALV END_OF_LIST
*---------------------------------------------------------------------*

FORM END_OF_LIST .

*

  PERFORM LOCAL_ENDOFLIST IN PROGRAM (SY-REPID)
          USING 'X'.

ENDFORM. "END_OF_LIST



*---------------------------------------------------------------------*
*       FORM DATA_CHANGED                                             *
*---------------------------------------------------------------------*

FORM DATA_CHANGED
     USING FS_DATA_CHANGED
                         TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL.

  PERFORM LOCAL_DATACHANGED IN PROGRAM (SY-REPID)
          USING FS_DATA_CHANGED 'X'.


ENDFORM. "DATA_CHANGED


*&--------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&--------------------------------------------------------------------*
*       ALV PF_STATUS_SET
*---------------------------------------------------------------------*

FORM PF_STATUS_SET USING EXTAB TYPE SLIS_T_EXTAB.
  CLEAR EXTAB[].
  PERFORM LOCAL_PFSTATUS IN PROGRAM (SY-REPID) USING EXTAB[].
ENDFORM. "TOP_OF_PAGE


*&--------------------------------------------------------------------*
*&      Form  BEFORE_LINE_OUTPUT
*&--------------------------------------------------------------------*
*       ALV BEFORE_LINE_OUTPUT
*---------------------------------------------------------------------*

FORM BEFORE_LINE_OUTPUT USING LINEINFO TYPE KKBLO_LINEINFO .
  PERFORM LOCAL_BEFORE_LINE IN PROGRAM (SY-REPID) USING LINEINFO 'X'.
ENDFORM. "BEFORE_LINE_OUTPUT


*&--------------------------------------------------------------------*
*&      Form  AFTER_LINE_OUTPUT
*&--------------------------------------------------------------------*
*       ALV AFTER_LINE_OUTPUT
*---------------------------------------------------------------------*

FORM AFTER_LINE_OUTPUT USING LINEINFO TYPE KKBLO_LINEINFO .
  PERFORM LOCAL_AFTER_LINE IN PROGRAM (SY-REPID)
                          USING LINEINFO 'X'.
ENDFORM. "AFTER_LINE_OUTPUT


*===============================================================
*_POPUP ALV LIST 관련
*---------------------------------------------------------------------*
*      Form  DISPLAY_ALV
*---------------------------------------------------------------------*
*      ALV DISPLAY POPUP
*---------------------------------------------------------------------*

FORM DISPLAY_ALV_POPUP TABLES PT_TAB
                                 PT_FIELDCAT
                         USING   P_TNAME
                                 P_TITLE.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            I_TITLE               = P_TITLE
            I_TABNAME             = P_TNAME
            I_ZEBRA               = 'X'
            I_SCREEN_START_COLUMN = 2
            I_SCREEN_START_LINE   = 2
            I_SCREEN_END_COLUMN   = 170
            I_SCREEN_END_LINE     = 25
            IT_FIELDCAT           = PT_FIELDCAT[]
       TABLES
            T_OUTTAB              = PT_TAB[]
       EXCEPTIONS
            PROGRAM_ERROR         = 1
            OTHERS                = 2.

ENDFORM. "DISPLAY_ALV_POPUP


*---------------------------------------------------------------------*
*      Form  POP_DISPLAY_ALV
*---------------------------------------------------------------------*
*      ALV DISPLAY POPUP
*---------------------------------------------------------------------*

FORM POP_DISPLAY_ALV_POPUP TABLES PT_TAB
                                 PT_FIELDCAT
                         USING   P_TNAME
                                 P_TITLE.


*_POPUP LAYOUT - MAIN PROGRAM 에서 정의 할것

  PERFORM POP_LAYOUT_SET2
         USING SPACE
               SPACE
               SPACE
               SPACE
               SPACE
               SPACE
               GS_POP_PRIVATE
               GS_POP_LAYOUT.


*_POP_PF_STATUS

  CLEAR : G_POP_TITLE.
  G_POP_TITLE = P_TITLE.
  PERFORM POP_PF_STATUS
          USING  GT_POP_EXTAB.


*_POP UP CHANGE FIELDCAT

  CLEAR : GT_POP_FIELDCAT[], GT_POP_FIELDCAT.
  GT_POP_FIELDCAT[] = PT_FIELDCAT[].
  PERFORM TRANSFER_FIELDCAT_DATA
          USING    GT_POP_FIELDCAT
          CHANGING GT_CHA_FIELDCAT.


*_USER_COMM
*  PERFORM POP_USER_COMMAND
*          USING L_UCOMM
*                LS_SELFIELD.

*_POP LIST DISPLAY

  PERFORM POP_ALV_LIST
     TABLES PT_TAB
     USING GT_CHA_FIELDCAT
           P_TNAME.


ENDFORM. "POP_DISPLAY_ALV_POPUP


*&---------------------------------------------------------------------*
*&      Form  POP_LAYOUT_SET2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM POP_LAYOUT_SET2
     USING R_CHECK              TYPE C
           R_ZEBRA              TYPE C
           R_SELECTION          TYPE C
           R_CHECKBOX_FIELDNAME TYPE KKBLO_FIELDNAME
           R_LINEMARK_FIELDNAME TYPE KKBLO_FIELDNAME
           R_UCOMM              TYPE KKBLO_FORMNAME
           R_PRIVATE TYPE SLIS_DATA_CALLER_EXIT
           R_LAYOUT             TYPE KKBLO_LAYOUT.

  CHECK NOT R_CHECK IS INITIAL.

  CLEAR : R_LAYOUT.
  IF R_ZEBRA IS INITIAL.
    R_LAYOUT-NO_ZEBRA = 'X'.
  ENDIF.

  IF R_SELECTION IS INITIAL.
    R_LAYOUT-NO_INPUT = 'X'.
  ELSE.
    CLEAR R_LAYOUT-NO_INPUT.
  ENDIF.

  IF R_PRIVATE-COLUMNOPT = 'X'.
    R_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  ENDIF.

  R_LAYOUT-NEW_FCODES           = 'X'.
  R_LAYOUT-DETAIL_POPUP         = 'X'.
  R_LAYOUT-DETAIL_INITIAL_LINES = 'X'.
  R_LAYOUT-BOX_FIELDNAME        = R_CHECKBOX_FIELDNAME.
  R_LAYOUT-INFO_FIELDNAME       = R_LINEMARK_FIELDNAME.
  R_LAYOUT-NO_MIN_LINESIZE      = 'X'.

  IF NOT R_UCOMM IS INITIAL.
    R_LAYOUT-DETAIL_EXIT = 'X'.
  ENDIF.

ENDFORM. " POP_LAYOUT_SET2


*&--------------------------------------------------------------------*
*&      Form  POP_PF_STATUS
*&--------------------------------------------------------------------*
*       ALV POPUP POP_PF_STATUS
*---------------------------------------------------------------------*

FORM POP_PF_STATUS USING EXTAB TYPE KKBLO_T_EXTAB.

  CLEAR EXTAB[].
  PERFORM LOCAL_POP_PFSTATUS IN PROGRAM (SY-REPID)
                             USING EXTAB[].

ENDFORM. "POP_PF_STATUS


*&--------------------------------------------------------------------*
*&      Form  POP_USER_COMMAND
*&--------------------------------------------------------------------*
*       ALV POPUP USER COMMAND
*---------------------------------------------------------------------*

FORM POP_USER_COMMAND USING L_UCOMM LIKE SY-UCOMM
                            LS_SELFIELD TYPE SLIS_SELFIELD.

*

  CASE L_UCOMM.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE SCREEN .
  ENDCASE.

  PERFORM LOCAL_POP_USERCOMMAND IN PROGRAM (SY-REPID)
                            USING L_UCOMM
                                  LS_SELFIELD.

ENDFORM. "POP_USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  POP_ALV_LIST
*&---------------------------------------------------------------------*

FORM POP_ALV_LIST
     TABLES R_OUTTAB TYPE STANDARD TABLE
     USING VALUE(VT_FIELDCAT)  TYPE KKBLO_T_FIELDCAT
           P_TABNAME.

  CALL FUNCTION 'K_KKB_LIST_DISPLAY'
     EXPORTING
       I_CALLBACK_PROGRAM       = G_REPID
       I_CALLBACK_USER_COMMAND  = C_POP_USER_COMMAND

*      I_CALLBACK_TOP_OF_PAGE   = 'LFIS_TOP_OF_PAGE'

       I_CALLBACK_PF_STATUS_SET = C_POP_STATUS_SET

*      I_CALLBACK_LAYOUT_SAVE   =
*      I_CALLBACK_FIELDCAT_SAVE =

       I_TABNAME                = P_TABNAME
       IS_LAYOUT                = GS_POP_LAYOUT
       IT_FIELDCAT              = VT_FIELDCAT
      IT_EXCLUDING             = GT_POP_EXTAB[]

*      I_FCTYPE                 = 'R'
*      IT_SPECIAL_GROUPS        =
*      IT_SORT                  =
*      IS_SEL_HIDE              =

       I_SCREEN_START_COLUMN    = 2
       I_SCREEN_START_LINE      = 2

*       I_SCREEN_END_COLUMN      = 100

       I_SCREEN_END_COLUMN      = 90

       I_SCREEN_END_LINE        = 25

*      IT_EXCEPT_QINFO          = LT_QINFO

     TABLES
          T_OUTTAB                 = R_OUTTAB
     EXCEPTIONS
          OTHERS                   = 1.

  IF SY-SUBRC NE 0.
  ENDIF.

ENDFORM. " POP_ALV_LIST


*&---------------------------------------------------------------------*
*&      Form  TRANSFER_FIELDCAT_DATA
*&---------------------------------------------------------------------*

FORM TRANSFER_FIELDCAT_DATA
     USING    PI_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV
     CHANGING PE_FIELDCAT TYPE KKBLO_T_FIELDCAT.

  CLEAR : PE_FIELDCAT[], PE_FIELDCAT.
  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
       EXPORTING
            IT_FIELDCAT = PI_FIELDCAT
       IMPORTING
            ET_FIELDCAT = PE_FIELDCAT
       EXCEPTIONS
            OTHERS      = 1.

ENDFORM. " TRANSFER_FIELDCAT_DATA


*===============================================================
*_FILE UPLOAD 관련
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_NAME
*&---------------------------------------------------------------------*
* 파일경로 조회
*----------------------------------------------------------------------*

FORM GET_FILE_NAME USING P_PATH.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_PATH         = 'C:\'
            MASK             = ',*.*,*.*.'
            MODE             = 'O'
       IMPORTING
            FILENAME         = P_PATH
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.

ENDFORM. " GET_FILE_NAME

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
* 엑셀 업로드 펑션
*----------------------------------------------------------------------*

FORM UPLOAD_EXCEL_DATA TABLES PT_TABLE
                 USING P_PATH .

  CLEAR : PT_TABLE[].
  DATA : GT_EXCEL TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_PATH
            I_BEGIN_COL             = 1
            I_BEGIN_ROW             = 2
            I_END_COL               = 200
            I_END_ROW               = 65536
       TABLES
            INTERN                  = GT_EXCEL
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE S000(ZMFI) WITH 'Not found Upload file'.
    STOP.
  ENDIF.


*" EXCEL => Internal Table

  LOOP AT GT_EXCEL.
    ASSIGN COMPONENT GT_EXCEL-COL OF STRUCTURE PT_TABLE TO <EXCEL>.
    <EXCEL> = GT_EXCEL-VALUE.
    AT END OF ROW.
      APPEND PT_TABLE.
      CLEAR  PT_TABLE.
    ENDAT.
  ENDLOOP.


ENDFORM. " UPLOAD_DTA

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_TEXT_DATA
*&---------------------------------------------------------------------*

FORM UPLOAD_TEXT_DATA TABLES PT_TABLE
                        USING    P_PATH.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME            = P_PATH
       TABLES
            DATA_TAB            = PT_TABLE
       EXCEPTIONS
            CONVERSION_ERROR    = 1
            INVALID_TABLE_WIDTH = 2
            INVALID_TYPE        = 3
            NO_BATCH            = 4
            UNKNOWN_ERROR       = 5
            OTHERS              = 6.



ENDFORM. " UPLOAD_TEXT_DATA

*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_MESSAGE
*&---------------------------------------------------------------------*

FORM POPUP_TO_MESSAGE USING P_DEFAULT P_TITLE P_TEXT1 P_TEXT2 P_DISPLAY
                      CHANGING P_ANSWER.

*

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            DEFAULTOPTION  = P_DEFAULT
            TEXTLINE1      = P_TEXT1
            TEXTLINE2      = P_TEXT2
            TITEL          = P_TITLE
            CANCEL_DISPLAY = P_DISPLAY
       IMPORTING
            ANSWER         = P_ANSWER
       EXCEPTIONS
            TEXT_NOT_FOUND.

ENDFORM. " POPUP_TO_CONFIRM

*===============================================================
*&---------------------------------------------------------------------
*&      Form  SET_CEL_COLOR_ALV
*&---------------------------------------------------------------------
*       CELL COLOR
*----------------------------------------------------------------------

FORM SET_CEL_COLOR_ALV
     USING PT_TABLCOLOR TYPE LVC_T_SCOL
           P_FIELD
           P_COLOR.
  DATA : WLS_LVC_S_SCOL TYPE LVC_S_SCOL.
  CLEAR : WLS_LVC_S_SCOL.
  WLS_LVC_S_SCOL-FNAME     = P_FIELD.
  WLS_LVC_S_SCOL-COLOR-COL = P_COLOR+0(1).
  WLS_LVC_S_SCOL-COLOR-INT = P_COLOR+1(1).
  WLS_LVC_S_SCOL-COLOR-INV = P_COLOR+2(1).

*  WLS_LVC_S_SCOL-NOKEYCOL  = 'X'.

  INSERT WLS_LVC_S_SCOL INTO TABLE PT_TABLCOLOR.

ENDFORM. " SET_CEL_COLOR_ALV

*&---------------------------------------------------------------------*
*&      Form  ALPHA_IN
*&---------------------------------------------------------------------*

FORM ALPHA_IN USING P_FIELD.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_FIELD
       IMPORTING
            OUTPUT = P_FIELD.

ENDFORM. " ALPHA_IN

*&---------------------------------------------------------------------*
*&      Form  ALPHA_OUT
*&---------------------------------------------------------------------*

FORM ALPHA_OUT USING P_FIELD.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = P_FIELD
       IMPORTING
            OUTPUT = P_FIELD.

ENDFORM. " ALPHA_OUT

*&---------------------------------------------------------------------*
*&      Form  INIT_VAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VARI  text
*----------------------------------------------------------------------*

FORM INIT_VAR_ALV USING P_VARI TYPE DISVARIANT-VARIANT.

*

  GX_VARIANT-REPORT = SY-CPROG.

*

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE     = G_SAVE
       CHANGING
            CS_VARIANT = GX_VARIANT
       EXCEPTIONS
            NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    P_VARI = GX_VARIANT-VARIANT.
  ENDIF.

ENDFORM. " INIT_VAR

*&---------------------------------------------------------------------*
*&      Form  F4_ALV_VARIANT
*&---------------------------------------------------------------------*

FORM F4_ALV_VARIANT CHANGING C_VARIANT TYPE DISVARIANT-VARIANT.
*  G_RUNMODE_S-PERSREPORT = SY-CPROG.
*  CALL FUNCTION 'RSAQRT_LAYOUT_VALUE_REQUEST'
*    CHANGING
*      RTMODE  = G_RUNMODE_S
*      VARIANT = C_VARIANT.
ENDFORM. " F4_ALV_VARIANT

*&---------------------------------------------------------------------*
*&      Form  SINGLE_MSG_POPUP
*&---------------------------------------------------------------------*

FORM SINGLE_MSG_POPUP USING P_TITLE
                                P_ID
                                P_TY
                                P_NO
                                P_MSGV1
                                P_MSGV2
                                P_MSGV3
                                P_MSGV4.
  DATA : L_NO TYPE SYMSGNO.
  IF NOT P_NO IS INITIAL.
    L_NO = P_NO .
  ENDIF.

  CALL FUNCTION 'POPUP_DISPLAY_MESSAGE'
       EXPORTING
            TITEL = P_TITLE
            MSGID = P_ID
            MSGTY = P_TY
            MSGNO = L_NO
            MSGV1 = P_MSGV1
            MSGV2 = P_MSGV2
            MSGV3 = P_MSGV3
            MSGV4 = P_MSGV4.

ENDFORM. " SINGLE_MSG_POPUP

*&---------------------------------------------------------------------*
*&      Form  MULTI_MSG_POPUP
*&---------------------------------------------------------------------*

FORM MULTI_MSG_POPUP USING PT_MESSAGE TYPE BAPIRETURN.
  CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
       EXPORTING
            IT_MESSAGE = PT_MESSAGE
       EXCEPTIONS
            BAL_ERROR  = 1
            OTHERS     = 2.

ENDFORM. " MULTI_MSG_POPUP

*&---------------------------------------------------------------------*
*&      Form  MOVE_MSG
*&---------------------------------------------------------------------*

FORM MOVE_MSG USING P_TYPE
                        P_ID
                        P_NUM
                        P_V1
                        P_V2
                        P_V3
                        P_V4.

*  MOVE: P_TYPE TO GS_MESSAGE-TYPE ,
*        P_ID   TO GS_MESSAGE-ID ,
*        P_NUM  TO GS_MESSAGE-NUMBER ,
*        P_V1   TO GS_MESSAGE-MESSAGE_V1 ,
*        P_V2   TO GS_MESSAGE-MESSAGE_V2 ,
*        P_V3   TO GS_MESSAGE-MESSAGE_V3 ,
*        P_V4   TO GS_MESSAGE-MESSAGE_V4 .
*  INSERT GS_MESSAGE INTO TABLE GT_MESSAGE.

ENDFORM. " MOVE_MSG

*&---------------------------------------------------------------------*
*&      Form  LAST_DAY
*&---------------------------------------------------------------------*

FORM LAST_DAY USING P_FR_DATE TYPE DATUM
                        P_TO_DATE TYPE DATUM.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = P_FR_DATE
       IMPORTING
            LAST_DAY_OF_MONTH = P_TO_DATE
       EXCEPTIONS
            DAY_IN_NO_DATE    = 1
            OTHERS            = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



ENDFORM. " LAST_DAY

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*

FORM PROGRESS_IND USING P_IND TYPE I
                            P_TEXT.
  CALL FUNCTION 'PROGRESS_INDICATOR'
       EXPORTING
            I_TEXT               = P_TEXT
            I_PROCESSED          = P_IND
            I_TOTAL              = 100
            I_OUTPUT_IMMEDIATELY = 'X'.

ENDFORM. " PROGRESS_IND

*&---------------------------------------------------------------------*
*&      Form  MONTH_COMPUTER
*&---------------------------------------------------------------------*

FORM MONTH_COMPUTER USING P_DATE
                              P_MONTH
                     CHANGING P_RESULT.

*

  CALL FUNCTION 'SG_PS_ADD_MONTH_TO_DATE'
       EXPORTING
            MONTHS  = P_MONTH
            OLDDATE = P_DATE
       IMPORTING
            NEWDATE = P_RESULT.

*

ENDFORM. " MONTH_COMPUTER


*&---------------------------------------------------------------------*
*&      Form  RETURN_BACK
*&---------------------------------------------------------------------*

FORM RETURN_BACK .
  CHECK SY-UCOMM = '&F03' OR SY-UCOMM = '&F15' OR SY-UCOMM = '&F12'.
  LEAVE TO SCREEN 0.
ENDFORM. " RETURN_BACK


*&---------------------------------------------------------------------*
*&      Form  ALV_MODIFY_CELL
*&---------------------------------------------------------------------*

FORM ALV_MODIFY_CELL USING RR_DATA_CHANGED TYPE REF TO
                                      CL_ALV_CHANGED_DATA_PROTOCOL
                            P_INDEX TYPE SY-TABIX P_FIELD P_VALUE.

  CALL METHOD RR_DATA_CHANGED->MODIFY_CELL
    EXPORTING
      I_ROW_ID    = P_INDEX
      I_FIELDNAME = P_FIELD
      I_VALUE     = P_VALUE.

ENDFORM. " ALV_MODIFY_CELL

*&---------------------------------------------------------------------*
*&      Form  SET_GLAY
*&---------------------------------------------------------------------*

FORM SET_GLAY USING P_CHK.

*

  GS_GLAY-EDT_CLL_CB = P_CHK.

*

ENDFORM. " SET_GLAY

*&---------------------------------------------------------------------*
*&      Form  F4_VALUES_READ
*&---------------------------------------------------------------------*

FORM F4_VALUES_READ TABLES PT_RETURN STRUCTURE DYNPREAD
                      USING    P_CPROG
                              P_DYNNR.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = P_CPROG
            DYNUMB               = P_DYNNR
       TABLES
            DYNPFIELDS           = PT_RETURN
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 1
            INVALID_DYNPROFIELD  = 2
            INVALID_DYNPRONAME   = 3
            INVALID_DYNPRONUMMER = 4
            INVALID_REQUEST      = 5
            NO_FIELDDESCRIPTION  = 6
            INVALID_PARAMETER    = 7
            UNDEFIND_ERROR       = 8
            DOUBLE_CONVERSION    = 9
            STEPL_NOT_FOUND      = 10
            OTHERS               = 11.

ENDFORM. " F4_VALUES_READ

*&---------------------------------------------------------------------*
*&      Form  CEL_COLOR
*&---------------------------------------------------------------------*

FORM CEL_COLOR USING P_COLOR1 TYPE SLIS_T_SPECIALCOL_ALV
                        P_FIELD.
  DATA : LS_COLOR TYPE SLIS_SPECIALCOL_ALV.
  LS_COLOR-FIELDNAME = P_FIELD.
  LS_COLOR-COLOR-COL = 5.
  LS_COLOR-COLOR-INT = 1.
  LS_COLOR-COLOR-INV = 0.
  LS_COLOR-NOKEYCOL  = 'C'.
  INSERT LS_COLOR INTO TABLE P_COLOR1.

ENDFORM. " CEL_COL
