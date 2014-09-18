*----------------------------------------------------------------------*
*   INCLUDE ZAPP102L_PP_LOG_MANAGEMENT_F01                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_IF_STATUS
*&---------------------------------------------------------------------*
FORM SELECT_IF_STATUS.
  IF IT_SCREEN[] IS INITIAL AND R_DATUM[] IS INITIAL.
    CLEAR : R_DATUM , R_DATUM[] .
    R_DATUM-SIGN   = 'I'.
    R_DATUM-OPTION = 'EQ'.
    R_DATUM-LOW    = SY-DATUM.
    APPEND R_DATUM .

    PERFORM read_tables .
    IF TC_9000-LINES = 0.
      PERFORM GENERATE_BASIC_TABLE .
    ENDIF.

*----> count in condition of Input date
    PERFORM READ_COUNT_RECORD .
  ENDIF.
ENDFORM.                    " SELECT_IF_STATUS

*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GS_LAYOUT-ZEBRA      = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN

ENDFORM.                    " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.

  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.

  L_STRUCT = WA_SCREEN-TABNAME.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

*  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
*    PERFORM SET_FIELD_INFO USING WA_FIELDCAT.
*    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
*    CLEAR WA_FIELDCAT.
*  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT
*&---------------------------------------------------------------------*
FORM SET_OBJECT.
  CASE WA_SCREEN-TABNAME .
*----> Vehicle
    WHEN 'ZPPPVP'.     " Vehicle Plan Order (Outbound)
      PERFORM SET_OBJECT_ZTPPVP .
    WHEN 'ZTPPVR'.     " Vehicle Production Result (Inbound)
      PERFORM SET_OBJECT_ZTPPVR .
    WHEN 'ZTPPVS'.     " Vehicle Production Spec (Outbound)
      PERFORM SET_OBJECT_ZTPPVS .

*----> Engine
    WHEN 'ZTPPEP'.     " Engine Production Planning (Outbound)
      PERFORM SET_OBJECT_ZTPPEP .
    WHEN 'ZTPPER'.     " Engine Production Result (Inbound)
      PERFORM SET_OBJECT_ZTPPER .
    WHEN 'ZTPPES'.     " Engine Production Spec (Outbound)
      PERFORM SET_OBJECT_ZTPPES .

*----> Press
    WHEN 'ZTPPPB'.     " Body Input Plan (Outbound)
      PERFORM SET_OBJECT_ZTPPPB .
    WHEN 'ZTPPPP'.     " Press Production Planning (Outbound)
      PERFORM SET_OBJECT_ZTPPPP .
    WHEN 'ZTPPPR'.     " Press Production Result (Inbound)
      PERFORM SET_OBJECT_ZTPPPR .
    WHEN 'ZTPPPS_BLK'. " Press Blank Spec (Outbound)
      PERFORM SET_OBJECT_ZTPPPS_BLK .
    WHEN 'ZTPPPS_DIE'. " Press Die Spec (Outbound)
      PERFORM SET_OBJECT_ZTPPPS_DIE .
    WHEN 'ZTPPPS_PNL'. " Press Panel Spec (Outbound)
      PERFORM SET_OBJECT_ZTPPPS_PNL .

  ENDCASE.
ENDFORM.                    " SET_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPER
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPER.
  CLEAR : IT_ZTPPER , IT_ZTPPER[] .

  SELECT *
         INTO TABLE IT_ZTPPER
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPER BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPER[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " SET_OBJECT_ZTPPER
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPVP
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPVP.
  CLEAR : IT_ZTPPVP , IT_ZTPPVP[].

  SELECT *
         INTO TABLE IT_ZTPPVP
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPVP BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPVP[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPVP
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPVR
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPVR.
  CLEAR : IT_ZTPPVR , IT_ZTPPVR[].

  SELECT *
         INTO TABLE IT_ZTPPVR
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPVR BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPVR[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPVR
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPVS
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPVS.
  CLEAR : IT_ZTPPVS , IT_ZTPPVS[].

  SELECT *
         INTO TABLE IT_ZTPPVS
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPVS BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPVS[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPVS
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPEP
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPEP.
  CLEAR : IT_ZTPPEP, IT_ZTPPEP[] .

  SELECT *
         INTO TABLE IT_ZTPPEP
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPEP BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPEP[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPEP
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPES
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPES.
  CLEAR : IT_ZTPPES , IT_ZTPPES[].

  SELECT *
         INTO TABLE IT_ZTPPES
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPES BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPES[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPB
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPB.
  CLEAR : IT_ZTPPPB, IT_ZTPPPB[] .

  SELECT *
         INTO TABLE IT_ZTPPPB
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPB BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPB[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPB
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPP
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPP.
  CLEAR : IT_ZTPPPP, IT_ZTPPPP[].

  SELECT *
         INTO TABLE IT_ZTPPPP
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPP BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPP[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPP
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPR
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPR.
  CLEAR : IT_ZTPPPR , IT_ZTPPPR[] .

  SELECT *
         INTO TABLE IT_ZTPPPR
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPR BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPR[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPR
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPS_BLK
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPS_BLK.
  CLEAR : IT_ZTPPPS_BLK , IT_ZTPPPS_BLK[].

  SELECT *
         INTO TABLE IT_ZTPPPS_BLK
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPS_BLK BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPS_BLK[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPS_BLK
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPS_DIE
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPS_DIE.
  CLEAR : IT_ZTPPPS_DIE , IT_ZTPPPS_DIE[] .

  SELECT *
         INTO TABLE IT_ZTPPPS_DIE
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPS_DIE BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPS_DIE[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPS_DIE
*&---------------------------------------------------------------------*
*&      Form  SET_OBJECT_ZTPPPS_PNL
*&---------------------------------------------------------------------*
FORM SET_OBJECT_ZTPPPS_PNL.

  CLEAR : IT_ZTPPPS_PNL, IT_ZTPPPS_PNL[] .

  SELECT *
         INTO TABLE IT_ZTPPPS_PNL
         FROM (WA_SCREEN-TABNAME)
         WHERE ZEDAT IN R_DATUM.
  SORT IT_ZTPPPS_PNL BY ZRESULT .

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = WA_SCREEN-TABNAME
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_ZTPPPS_PNL[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4
          .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SET_OBJECT_ZTPPPS_PNL
*&---------------------------------------------------------------------*
*&      Form  TOGGLE_PROCESS
*&---------------------------------------------------------------------*
FORM TOGGLE_PROCESS.
  DATA : L_TABIX    TYPE  SY-TABIX .
  LOOP AT IT_SCREEN WHERE CHK EQ C_MARK.
    L_TABIX = SY-TABIX .
    IF IT_SCREEN-ZGO IS INITIAL.
      IT_SCREEN-ZGO = C_MARK .
    ELSEIF IT_SCREEN-ZGO EQ C_MARK .
      CLEAR IT_SCREEN-ZGO .
    ENDIF.
    MODIFY IT_SCREEN INDEX L_TABIX .

    UPDATE ZTPP_IF_STATUS  SET ZGO = IT_SCREEN-ZGO
           WHERE TABNAME  EQ  IT_SCREEN-TABNAME.
  ENDLOOP.
ENDFORM.                    " TOGGLE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_COUNT_RECORD
*&---------------------------------------------------------------------*
FORM READ_COUNT_RECORD.
  DATA : L_TABIX   TYPE   SY-TABIX .
  LOOP AT IT_SCREEN .
    L_TABIX = SY-TABIX .
*---> Total Record Count in condition of input date
    SELECT COUNT(*)
           INTO IT_SCREEN-TOTAL
           FROM (IT_SCREEN-TABNAME)
           WHERE ZEDAT IN R_DATUM .

*---> Success Record count in condition of input date
    SELECT COUNT(*)
           INTO IT_SCREEN-SUCCESS
           FROM (IT_SCREEN-TABNAME)
           WHERE ZEDAT IN R_DATUM
             AND ZRESULT EQ 'S' .

*---> Error Record count in condition of input date
    SELECT COUNT(*)
           INTO IT_SCREEN-ERROR
           FROM (IT_SCREEN-TABNAME)
           WHERE ZEDAT IN R_DATUM
             AND ZRESULT EQ 'E' .

    MODIFY IT_SCREEN INDEX L_TABIX .
  ENDLOOP.
ENDFORM.                    " READ_COUNT_RECORD

*&---------------------------------------------------------------------*
*&      Form  SAVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_PROCESS.
  if ZTPP_IF_STATUS-TABNAME = space.
     message i001 with text-002 .
     exit.
  endif.

  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
ENDFORM.                    " SAVE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  read_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tables.
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_SCREEN
    FROM ZTPP_IF_STATUS .

  DESCRIBE TABLE IT_SCREEN LINES TC_9000-LINES.
ENDFORM.                    " read_tables

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BASIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_BASIC_TABLE.
  ZTPP_IF_STATUS-ERDAT = ZTPP_IF_STATUS-AEDAT = SY-DATUM.
  ZTPP_IF_STATUS-ERZET = ZTPP_IF_STATUS-AEZET = SY-UZEIT.
  ZTPP_IF_STATUS-ERNAM = ZTPP_IF_STATUS-AENAM = SY-UNAME.
  ZTPP_IF_STATUS-ZGO     = ' '     .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPER'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPES'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPEP'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPVR'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPVS'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPVP'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPR'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPS_BLK'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPS_DIE'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPS_PNL'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPP'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
  ZTPP_IF_STATUS-TABNAME = 'ZTPPPB'.
  insert into ZTPP_IF_STATUS values ZTPP_IF_STATUS .
ENDFORM.                    " GENERATE_BASIC_TABLE

*&---------------------------------------------------------------------*
*&      Form  DELETE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_PROCESS.
  DATA: DEL_DATE           TYPE D .

  DEL_DATE = SY-DATUM - 30        .

  LOOP AT IT_SCREEN  WHERE CHK  = 'X' .
    CASE IT_SCREEN-TABNAME.
      WHEN 'ZTPPEP'     .
        " Already Processing in Program ZIPP501L_ENGIN_PP
      WHEN 'ZTPPER'     .
        " Need Delete Process (Operated Manually..)
      WHEN 'ZTPPES'     .
        " Need Delete Process (Operated Manually..)
      WHEN 'ZTPPPB'     .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPPP'     .
        " Already Processing in Program ZAPP601R_PRESS_ZTPPPP
      WHEN 'ZTPPPR'     .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPPS_BLK' .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPPS_DIE' .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPPS_PNL' .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPVP'     .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPVR'     .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
      WHEN 'ZTPPVS'     .
        " Already Processing in Program ZIPP600I_SET_ZTPPPB
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " DELETE_PROCESS
