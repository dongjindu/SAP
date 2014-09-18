*&---------------------------------------------------------------------*
*& Report  ZACO20R_WISC**&
*
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZACO20R_WISC         .

TYPE-POOLS: SLIS.

PARAMETERS: MATERIAL TYPE MATNR OBLIGATORY MEMORY ID MAT,
            PLANT TYPE WERKS_D OBLIGATORY MEMORY ID WRK,
            PROCESS TYPE CKML_ALPROC_TEXT MODIF ID 100,
            AUFNR TYPE AUFNR NO-DISPLAY.

PARAMETERS: PERIOD TYPE CO_PERIO OBLIGATORY MEMORY ID VPE,
            GJAHR TYPE GJAHR OBLIGATORY     MEMORY ID GJR.

DATA: WIP TYPE C VALUE 'X',
      SCRAP TYPE C VALUE 'X'.

DATA: LT_LIST TYPE QRP_T_OBJNR_LIST,
      LS_LIST TYPE QRP_OBJNR_LIST,
      LS_AUFK TYPE AUFK.

DATA: LT_QUANTITIES TYPE QRP_T_QUANTITIES,
      LS_QUANTITIES TYPE QRP_QUANTITIES,
      LT_OUTPUT TYPE QRP_T_WIP_SCRAP,
      LS_OUTPUT TYPE QRP_S_WIP_SCRAP,
      L_PROCNR TYPE CKML_F_PROCNR,
      L_OBJNR TYPE J_OBJNR.

DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      LT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      LS_LISTHEADER TYPE SLIS_LISTHEADER,
      LT_LISTHEADER TYPE SLIS_T_LISTHEADER.

DATA  LS_VARIANT  TYPE DISVARIANT.


IF PROCESS IS INITIAL.
  CALL FUNCTION 'KK_F_PKOSA_LIST_ALL'
       EXPORTING
            I_MATNR        = MATERIAL
            I_WERKS        = PLANT
            I_PROCNR       = L_PROCNR
            I_NO_LOEVM     = 'X'
            I_NO_OLD_PKOSA = 'X'
       IMPORTING
            E_PKOSA        = AUFNR
            E_PROCNR       = L_PROCNR
       EXCEPTIONS
            NONE_PICKED    = 1
            OTHERS         = 2.
  IF SY-SUBRC EQ 1.
    STOP.
  ENDIF.
  IF SY-SUBRC GT 1.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.
  CALL FUNCTION 'CKML_MGV_PROCALTN_TEXT_READ'
       EXPORTING
            I_KALNR      = L_PROCNR
            I_LANGUAGE   = SY-LANGU
       IMPORTING
            E_VALID_NAME = PROCESS
       EXCEPTIONS
            OTHERS       = 1.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.
ENDIF.

CALL FUNCTION 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
     EXPORTING
          IF_PKOSA_AUFNR = AUFNR
     IMPORTING
          EF_PKOSA_OBJNR = LS_LIST-OBJNR.

LS_LIST-PLANT = PLANT.
APPEND LS_LIST TO LT_LIST.
CALL FUNCTION 'QRP_APO_REPORTINGPOINT_READ'
     EXPORTING
          IF_PERIOD         = PERIOD
          IF_GJAHR          = GJAHR
          IT_OBJNR_LIST     = LT_LIST
          IF_SELECT_WIP     = WIP
          IF_SELECT_SCRAP   = SCRAP
     IMPORTING
          ET_QUANTITY_TABLE = LT_QUANTITIES
     EXCEPTIONS
          WRONG_INPUT       = 1
          OTHERS            = 2.
IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
SORT LT_QUANTITIES BY ACT_OBJNR OBJNR KALN1.

LOOP AT LT_QUANTITIES INTO LS_QUANTITIES.
  LS_OUTPUT-GJAHR = GJAHR.
  LS_OUTPUT-PERIOD = PERIOD.
  MOVE-CORRESPONDING LS_QUANTITIES TO LS_OUTPUT.
  IF LS_QUANTITIES-ACT_OBJNR IS INITIAL.
    L_OBJNR(2) = 'VS'.
    L_OBJNR+2 = LS_QUANTITIES-KZBWS.
    L_OBJNR+3 = LS_QUANTITIES-SOBKZ.
    L_OBJNR+4 = LS_QUANTITIES-KALN1.
    CALL FUNCTION 'QRP_APO_COMP_OBJNR_DECODE'
         EXPORTING
              IF_F_OBJNR      = L_OBJNR
              IF_COMPLETE_KEY = 'X'
         IMPORTING
              EF_KALN1        = LS_OUTPUT-KALN1
              EF_KZBWS        = LS_OUTPUT-KZBWS
              EF_SOBKZ        = LS_OUTPUT-SOBKZ
              EF_MATNR        = LS_OUTPUT-MATERIAL
              EF_BWKEY        = LS_OUTPUT-BWKEY
              EF_BWTAR        = LS_OUTPUT-BWTAR
              EF_VBELN        = LS_OUTPUT-VBELN
              EF_POSNR        = LS_OUTPUT-POSNR.
    APPEND LS_OUTPUT TO LT_OUTPUT.
    CLEAR LS_OUTPUT.
  ELSE.
    CALL FUNCTION 'OBJECT_KEY_GET_KL'
         EXPORTING
              OBJNR  = LS_QUANTITIES-ACT_OBJNR
         IMPORTING
              KOSTL  = LS_OUTPUT-KOSTL
              LSTAR  = LS_OUTPUT-LSTAR
         EXCEPTIONS
              OTHERS = 1.
    IF NOT SY-SUBRC IS INITIAL.
    ELSE.
      APPEND LS_OUTPUT TO LT_OUTPUT.
    ENDIF.
  ENDIF.
ENDLOOP.

CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
     EXPORTING
          I_STRUCTURE_NAME = 'QRP_S_WIP_SCRAP'
     CHANGING
          CT_FIELDCAT      = LT_FIELDCAT
     EXCEPTIONS
          OTHERS           = 1.
IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
LOOP AT LT_FIELDCAT INTO LS_FIELDCAT.
  CASE LS_FIELDCAT-FIELDNAME.
    WHEN 'OBJNR' OR
         'SOBKZ' OR
         'KZBWS' OR
         'KALN1' OR
         'COST_CENTER' OR
         'ACTIVITY_TYPE' OR
         'GJAHR' OR
         'PERIOD' OR
         'PLANNED_SCRAP'.
      LS_FIELDCAT-NO_OUT = 'X'.
      MODIFY LT_FIELDCAT FROM LS_FIELDCAT.
  ENDCASE.
ENDLOOP.




LS_VARIANT-REPORT   = SY-REPID.
LS_VARIANT-USERNAME = SY-UNAME.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          I_GRID_TITLE           = SPACE
          IT_FIELDCAT            = LT_FIELDCAT
          I_CALLBACK_PROGRAM     = 'QRP_DISPLAY_WIP_SCRAP'
          I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
          IS_VARIANT             = LS_VARIANT
          I_SAVE                 = 'A'
     TABLES
          T_OUTTAB               = LT_OUTPUT
     EXCEPTIONS
          OTHERS                 = 1.
IF SY-SUBRC <> 0.
  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       Callback-Routine f�r HTML-Header                              *
*       call back for HTML header                                     *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  STATICS HEADER_READY.

  IF HEADER_READY IS INITIAL.

    LS_LISTHEADER-TYP = 'H'.
    LS_LISTHEADER-INFO = TEXT-001.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-002.
    LS_LISTHEADER-INFO = MATERIAL.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-003.
    LS_LISTHEADER-INFO = PLANT.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-006.
    LS_LISTHEADER-INFO = PROCESS.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-004.
    LS_LISTHEADER-INFO = PERIOD.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-005.
    LS_LISTHEADER-INFO = GJAHR.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.
    LS_LISTHEADER-TYP = 'S'.
    LS_LISTHEADER-KEY = TEXT-007.
    LS_LISTHEADER-INFO = LS_LIST-OBJNR.
    APPEND LS_LISTHEADER TO LT_LISTHEADER.

    HEADER_READY = 'X'.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_LISTHEADER.

ENDFORM.

************************************************************************

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ '100'.
      SCREEN-INPUT = '0'.
      SCREEN-VALUE_HELP = '2'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR PROCESS.

  DATA: DYNPFIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
        L_REPID LIKE SY-REPID,
        L_DYNNR LIKE SY-DYNNR,
        L_MATNR LIKE KKB0-MATNR.

  L_REPID = SY-REPID.
  L_DYNNR = SY-DYNNR.

  CLEAR DYNPFIELDS.
  REFRESH DYNPFIELDS.
  MOVE 'MATERIAL' TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.
  MOVE 'PLANT' TO DYNPFIELDS-FIELDNAME.
  APPEND DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME                   = L_REPID
            DYNUMB                   = L_DYNNR
            PERFORM_INPUT_CONVERSION = 'X'
            TRANSLATE_TO_UPPER       = 'X'
       TABLES
            DYNPFIELDS               = DYNPFIELDS
       EXCEPTIONS
            OTHERS                   = 1.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

  LOOP AT DYNPFIELDS.
    CASE DYNPFIELDS-FIELDNAME.
      WHEN 'MATERIAL'.
        MATERIAL = DYNPFIELDS-FIELDVALUE.
      WHEN 'PLANT'.
        PLANT =  DYNPFIELDS-FIELDVALUE.
    ENDCASE.
  ENDLOOP.

  CALL FUNCTION 'KK_F_PKOSA_LIST_ALL'
       EXPORTING
            I_MATNR        = MATERIAL
            I_WERKS        = PLANT
            I_PROCNR       = L_PROCNR
            I_NO_LOEVM     = 'X'
            I_NO_OLD_PKOSA = 'X'
       IMPORTING
            E_PKOSA        = AUFNR
            E_PROCNR       = L_PROCNR
       EXCEPTIONS
            NONE_PICKED    = 1
            OTHERS         = 2.
  IF SY-SUBRC EQ 1.
    STOP.
  ENDIF.
  IF SY-SUBRC GT 1.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

  CALL FUNCTION 'CKML_MGV_PROCALTN_TEXT_READ'
       EXPORTING
            I_KALNR      = L_PROCNR
            I_LANGUAGE   = SY-LANGU
       IMPORTING
            E_VALID_NAME = PROCESS
       EXCEPTIONS
            OTHERS       = 1.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

  CLEAR DYNPFIELDS.
  REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'PROCESS'.
  DYNPFIELDS-FIELDVALUE = PROCESS.
  APPEND DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'AUFNR'.
  DYNPFIELDS-FIELDVALUE = AUFNR.
  APPEND DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME     = L_REPID
            DYNUMB     = L_DYNNR
       TABLES
            DYNPFIELDS = DYNPFIELDS
       EXCEPTIONS
            OTHERS     = 1.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.