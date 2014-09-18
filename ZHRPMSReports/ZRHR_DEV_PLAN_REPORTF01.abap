*&---------------------------------------------------------------------*
*&  Include           ZRHR_DEV_PLAN_REPORTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_INIT_VALUE .

  IF P_YEAR IS INITIAL.
    P_YEAR = SY-DATUM(4).
  ENDIF.

ENDFORM.                    " SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_DROPLIST_YEAR .

  DATA: L_YEAR        TYPE ZDHR_YEAR.

  G_FIELDNAME = 'P_YEAR'.
  L_YEAR = SY-DATUM(4).

  CLEAR GT_VALUES.
  DO 10 TIMES.
    GS_VALUE-KEY = L_YEAR.
    GS_VALUE-TEXT = L_YEAR.
    APPEND GS_VALUE TO GT_VALUES.CLEAR GS_VALUE.
    L_YEAR = SY-DATUM(4) - SY-INDEX.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = G_FIELDNAME
      VALUES = GT_VALUES.

ENDFORM.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CREATE_ALV_100 .

  DATA: LT_EXCLUD TYPE UI_FUNCTIONS.
  DATA: LS_VARIANT TYPE DISVARIANT.

  IF GR_CONT IS INITIAL.
    CREATE OBJECT GR_CONT
      EXPORTING
        CONTAINER_NAME = 'CONTAINER'.

    CREATE OBJECT GR_GRID
      EXPORTING
        I_PARENT = GR_CONT.

    PERFORM SET_LAYOUT.
    PERFORM SET_FCAT.
    PERFORM SET_SORT.

    LS_VARIANT-REPORT   = SY-REPID.
    LS_VARIANT-USERNAME = SY-UNAME.

    CALL METHOD GR_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT                     = GS_LAYO
        IS_VARIANT                    = LS_VARIANT
        I_SAVE                        = 'A'
      CHANGING
        IT_OUTTAB                     = GT_RESULT
        IT_FIELDCATALOG               = GT_FCAT[]
        IT_SORT                       = GT_SORT[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD GR_GRID->REFRESH_TABLE_DISPLAY
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.
    IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_LAYOUT .

  GS_LAYO-ZEBRA = 'X'.
  GS_LAYO-SEL_MODE = 'D'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_FCAT .

  CLEAR: GT_FCAT[].

  " TM ID
  GT_FCAT-FIELDNAME = 'APPEE'.
  GT_FCAT-COLTEXT = TEXT-T04.
  GT_FCAT-COL_POS = 1.
  GT_FCAT-OUTPUTLEN = 6.
  GT_FCAT-JUST = 'R'.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " First Name
  GT_FCAT-FIELDNAME = 'VORNA'.
  GT_FCAT-COLTEXT = TEXT-T05.
  GT_FCAT-COL_POS = 2.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Last Name
  GT_FCAT-FIELDNAME = 'NACHN'.
  GT_FCAT-COLTEXT = TEXT-T06.
  GT_FCAT-COL_POS = 3.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Cost Center ID
  GT_FCAT-FIELDNAME = 'KOSTL'.
  GT_FCAT-COLTEXT = TEXT-T02.
  GT_FCAT-COL_POS = 4.
  GT_FCAT-OUTPUTLEN = 6.
  GT_FCAT-JUST = 'R'.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Cost Center Name
  GT_FCAT-FIELDNAME = 'KTEXT'.
  GT_FCAT-COLTEXT = TEXT-T03.
  GT_FCAT-COL_POS = 5.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Classification
  GT_FCAT-FIELDNAME = 'CLFTX'.
  GT_FCAT-COLTEXT = TEXT-T07.
  GT_FCAT-COL_POS = 6.
  GT_FCAT-OUTPUTLEN = 25.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Grade
  GT_FCAT-FIELDNAME = 'GRADE'.
  GT_FCAT-COLTEXT = TEXT-T08.
  GT_FCAT-COL_POS = 7.
  GT_FCAT-OUTPUTLEN = 3.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Hiring Date
  GT_FCAT-FIELDNAME = 'HIRDA'.
  GT_FCAT-COLTEXT = TEXT-T11.
  GT_FCAT-COL_POS = 8.
  GT_FCAT-OUTPUTLEN = 10.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Appraisal Year
  GT_FCAT-FIELDNAME = 'ZYEAR'.
  GT_FCAT-COLTEXT = TEXT-T12.
  GT_FCAT-COL_POS = 9.
  GT_FCAT-OUTPUTLEN = 4.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Status
  GT_FCAT-FIELDNAME = 'STATX'.
  GT_FCAT-COLTEXT = TEXT-T27.
  GT_FCAT-COL_POS = 10.
  GT_FCAT-OUTPUTLEN = 10.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Duration(From)
  GT_FCAT-FIELDNAME = 'STRDA'.
  GT_FCAT-COLTEXT = TEXT-T13.
  GT_FCAT-COL_POS = 11.
  GT_FCAT-OUTPUTLEN = 10.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Duration(To)
  GT_FCAT-FIELDNAME = 'ENDDA'.
  GT_FCAT-COLTEXT = TEXT-T14.
  GT_FCAT-COL_POS = 12.
  GT_FCAT-OUTPUTLEN = 10.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Supervisor ID
  GT_FCAT-FIELDNAME = 'APPER'.
  GT_FCAT-COLTEXT = TEXT-T09.
  GT_FCAT-COL_POS = 13.
  GT_FCAT-OUTPUTLEN = 6.
  GT_FCAT-JUST = 'R'.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Supervisor Name
  GT_FCAT-FIELDNAME = 'APPERNM'.
  GT_FCAT-COLTEXT = TEXT-T10.
  GT_FCAT-COL_POS = 14.
  GT_FCAT-OUTPUTLEN = 30.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Competency Group
  GT_FCAT-FIELDNAME = 'COMPT'.
  GT_FCAT-COLTEXT = TEXT-T15.
  GT_FCAT-COL_POS = 15.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Competency
  GT_FCAT-FIELDNAME = 'COMPG'.
  GT_FCAT-COLTEXT = TEXT-T16.
  GT_FCAT-COL_POS = 16.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Behavioral Attribute
  GT_FCAT-FIELDNAME = 'COMPR'.
  GT_FCAT-COLTEXT = TEXT-T17.
  GT_FCAT-COL_POS = 17.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Time Frame.
  GT_FCAT-FIELDNAME = 'TFTXT'.
  GT_FCAT-COLTEXT = TEXT-T30.
  GT_FCAT-COL_POS = 18.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Development Opportunity Text.
  GT_FCAT-FIELDNAME = 'DOTXT'.
  GT_FCAT-COLTEXT = TEXT-T28.
  GT_FCAT-COL_POS = 19.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Development Opportunity Note.
  GT_FCAT-FIELDNAME = 'DONOTE'.
  GT_FCAT-COLTEXT = TEXT-T29.
  GT_FCAT-COL_POS = 20.
  GT_FCAT-OUTPUTLEN = 80.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Develpment Plan Status(MY).
  GT_FCAT-FIELDNAME = 'DPSTXTM'.
  GT_FCAT-COLTEXT = TEXT-T31.
  GT_FCAT-COL_POS = 21.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Develpment Plan Status(MY) Note.
  GT_FCAT-FIELDNAME = 'DPSTNTM'.
  GT_FCAT-COLTEXT = TEXT-T32.
  GT_FCAT-COL_POS = 22.
  GT_FCAT-OUTPUTLEN = 80. "132.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Develpment Plan Status(YE)
  GT_FCAT-FIELDNAME = 'DPSTXTY'.
  GT_FCAT-COLTEXT = TEXT-T33.
  GT_FCAT-COL_POS = 23.
  GT_FCAT-OUTPUTLEN = 20.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

  " Develpment Plan Status(YE) Note.
  GT_FCAT-FIELDNAME = 'DPSTNTY'.
  GT_FCAT-COLTEXT = TEXT-T34.
  GT_FCAT-COL_POS = 24.
  GT_FCAT-OUTPUTLEN = 80.  "132.
  APPEND GT_FCAT.CLEAR: GT_FCAT.

ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_SORT .

  CLEAR GT_SORT[].
  GT_SORT-SPOS      = '1'.
  GT_SORT-FIELDNAME = 'APPEE'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '2'.
  GT_SORT-FIELDNAME = 'VORNA'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '3'.
  GT_SORT-FIELDNAME = 'NACHN'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '4'.
  GT_SORT-FIELDNAME = 'KOSTL'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '5'.
  GT_SORT-FIELDNAME = 'KTEXT'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '6'.
  GT_SORT-FIELDNAME = 'CLFTX'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '7'.
  GT_SORT-FIELDNAME = 'GRADE'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '8'.
  GT_SORT-FIELDNAME = 'HIRDA'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '9'.
  GT_SORT-FIELDNAME = 'ZYEAR'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '10'.
  GT_SORT-FIELDNAME = 'STATX'.
  GT_SORT-UP        = 'X'.
  GT_SORT-LEVEL     = '1'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '11'.
  GT_SORT-FIELDNAME = 'STRDA'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '12'.
  GT_SORT-FIELDNAME = 'ENDDA'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '13'.
  GT_SORT-FIELDNAME = 'APPER'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

  GT_SORT-SPOS      = '14'.
  GT_SORT-FIELDNAME = 'APPERNM'.
  GT_SORT-UP        = 'X'.
  GT_SORT-SUBTOT    = 'X'.
  APPEND GT_SORT.CLEAR  GT_SORT.

*  gt_sort-spos      = '15'.
*  gt_sort-fieldname = 'COMPR'.
*  gt_sort-up        = 'X'.
*  gt_sort-subtot    = 'X'.
*  APPEND gt_sort.CLEAR  gt_sort.
*
*  gt_sort-spos      = '16'.
*  gt_sort-fieldname = 'TFTXT'.
*  gt_sort-up        = 'X'.
*  gt_sort-subtot    = 'X'.
*  APPEND gt_sort.CLEAR  gt_sort.
*
*  gt_sort-spos      = '17'.
*  gt_sort-fieldname = 'DPSTXTY'.
*  gt_sort-up        = 'X'.
*  gt_sort-subtot    = 'X'.
*  APPEND gt_sort.CLEAR  gt_sort.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DATA .

  DATA: LT_APPRAISEES         TYPE HAP_T_HRSOBID,
        LT_DOCUMENTS          TYPE HAP_T_DOCUMENTS,
        LT_KTEXT              TYPE TABLE OF CSKT WITH HEADER LINE,
        LT_P0041              TYPE TABLE OF PA0041 WITH HEADER LINE.

  DATA: LS_SEL_DATES          TYPE HAP_S_SEL_DATES,
        LS_SEL_STATUS         TYPE HAP_S_SEL_STATUS,
        LS_SEL_WITH_OR_WITOUT TYPE HAP_S_SEL_WITH_OR_WITHOUT,
        LS_APPRAISEES         TYPE HRSOBID,
        LS_RETURN             TYPE BAPIRET2,
        LS_DOCUMENTS          TYPE HAP_S_DOCUMENTS.

  DATA: BEGIN OF LT_P0000 OCCURS 0,
          PERNR               TYPE P0000-PERNR,
          STAT2               TYPE P0000-STAT2,
        END OF LT_P0000.

  DATA: BEGIN OF LT_P0001 OCCURS 0,
          PERNR               TYPE PA0001-PERNR,
          KOSTL               TYPE PA0001-KOSTL,
          PLANS               TYPE PA0001-PLANS,
          STELL               TYPE PA0001-STELL,
        END OF LT_P0001.

  DATA: BEGIN OF LT_P0002 OCCURS 0,
          PERNR               TYPE PA0002-PERNR,
          VORNA               TYPE PA0002-VORNA,
          NACHN               TYPE PA0002-NACHN,
        END OF LT_P0002.

  DATA: BEGIN OF LT_CLASS OCCURS 0,
          CLFID               TYPE ZTHR_CLFAJ-CLFID,
          JOBID               TYPE ZTHR_CLFAJ-JOBID,
          CLFTX               TYPE ZTHR_CLASS-CLFTX,
        END OF LT_CLASS.

  DATA: BEGIN OF LT_HIRDA OCCURS 0,
          PERNR               TYPE PA0000-PERNR,
          BEGDA               TYPE PA0000-BEGDA,
        END OF LT_HIRDA.

  DATA: L_ENDDA               TYPE ENDDA,
        L_INDEX               TYPE N LENGTH 2,
        L_FIELDNAME           TYPE STRING,
        L_RFC_DESTINATION     TYPE RFCDEST.

  DATA: RT_KOSTL              TYPE RANGE OF KOSTL WITH HEADER LINE.

  FIELD-SYMBOLS: <FS_DAR>     TYPE ANY,
                 <FS_DAT>     TYPE ANY.

  CLEAR: GT_RESULT.

  IF P_YEAR IS INITIAL.
    MESSAGE S026 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    CONCATENATE P_YEAR '1231' INTO L_ENDDA.
  ENDIF.

************************************************
*   get data
************************************************
  " get classification text
  CLEAR LT_CLASS[].
  SELECT A~CLFID
         A~JOBID
         B~CLFTX
    FROM ZTHR_CLFAJ AS A INNER JOIN ZTHR_CLASS AS B
                                 ON A~CLFID = B~CLFID
    INTO TABLE LT_CLASS.
  SORT LT_CLASS BY JOBID.

  " check active
  CLEAR: LT_P0000[].
  SELECT PERNR STAT2
    INTO TABLE LT_P0000
    FROM PA0000
   WHERE PERNR IN S_PERNR
     AND ENDDA >= L_ENDDA
     AND BEGDA <= L_ENDDA
     AND STAT2 = '3'.

*  CHECK lines( lt_p0000 ) > 0.
  IF LT_P0000[] IS INITIAL.
    MESSAGE S027 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SORT LT_P0000 BY PERNR.
  DELETE ADJACENT DUPLICATES FROM LT_P0000 COMPARING PERNR.

  " check Employee Group, Employee Subgroup
  CLEAR LT_P0001[].
  SELECT PERNR
         KOSTL
         PLANS
         STELL
    INTO TABLE LT_P0001
    FROM PA0001
     FOR ALL ENTRIES IN LT_P0000
   WHERE PERNR = LT_P0000-PERNR
     AND ENDDA >= L_ENDDA
     AND BEGDA <= L_ENDDA
     AND PERSG = '1'
     AND PERSK IN ('U2','U3')
     AND KOSTL IN S_KOSTL.

*  CHECK lines( lt_p0001 ) > 0.
  IF LT_P0001[] IS INITIAL.
    MESSAGE S027 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SORT LT_P0001 BY PERNR.
  DELETE ADJACENT DUPLICATES FROM LT_P0001 COMPARING PERNR.

  " get first name, last name
  CLEAR LT_P0002[].
  SELECT PERNR VORNA NACHN
    INTO TABLE LT_P0002
    FROM PA0002
     FOR ALL ENTRIES IN LT_P0001
   WHERE PERNR = LT_P0001-PERNR
     AND ENDDA >= L_ENDDA
     AND BEGDA <= L_ENDDA.
  SORT LT_P0002 BY PERNR.

  CLEAR RT_KOSTL[].
  LOOP AT LT_P0001.
    RT_KOSTL-SIGN = 'I'.
    RT_KOSTL-OPTION = 'EQ'.
    RT_KOSTL-LOW = LT_P0001-KOSTL.
    APPEND RT_KOSTL.CLEAR RT_KOSTL.
  ENDLOOP.
  SORT RT_KOSTL BY LOW.
  DELETE ADJACENT DUPLICATES FROM RT_KOSTL COMPARING LOW.

  " get cost center text
  CLEAR LT_KTEXT[].
  SELECT *
    INTO TABLE LT_KTEXT
    FROM CSKT
   WHERE SPRAS = SY-LANGU
     AND KOKRS = 'H201'
     AND KOSTL IN RT_KOSTL
     AND DATBI = '99991231'.
  SORT LT_KTEXT BY KOSTL.

  " get date specifications
  CLEAR LT_P0041[].
  SELECT *
    INTO TABLE LT_P0041
    FROM PA0041
     FOR ALL ENTRIES IN LT_P0001
   WHERE PERNR = LT_P0001-PERNR
     AND ENDDA >= L_ENDDA
     AND BEGDA <= L_ENDDA.

  " set hire date
  CLEAR LT_HIRDA[].
  LOOP AT LT_P0041.
    LT_HIRDA-PERNR = LT_P0041-PERNR.
    DO 12 TIMES.
      L_INDEX = L_INDEX + 1.
      CONCATENATE 'LT_P0041-DAR' L_INDEX INTO L_FIELDNAME.
      ASSIGN (L_FIELDNAME) TO <FS_DAR>.
      IF <FS_DAR> EQ 'Z1'.
        CONCATENATE 'LT_P0041-DAT' L_INDEX INTO L_FIELDNAME.
        ASSIGN (L_FIELDNAME) TO <FS_DAT>.
        LT_HIRDA-BEGDA = <FS_DAT>.
        UNASSIGN: <FS_DAR>, <FS_DAT>.
        EXIT.
      ENDIF.

      UNASSIGN <FS_DAR>.
    ENDDO.

    APPEND LT_HIRDA.
    CLEAR: LT_P0041, LT_HIRDA, L_INDEX.
  ENDLOOP.

  " set appraisal dates
  CLEAR LS_SEL_DATES.
  LS_SEL_DATES-VALIDITY_TO_DATE = L_ENDDA.

  " set appraisal status
  LS_SEL_STATUS-AP_STATUS_1 = 'X'.
  LS_SEL_STATUS-AP_STATUS_2 = 'X'.
  LS_SEL_STATUS-AP_STATUS_3 = 'X'.
  LS_SEL_STATUS-AP_STATUS_4 = 'X'.
  LS_SEL_STATUS-AP_STATUS_5 = 'X'.
  LS_SEL_STATUS-AP_STATUS_6 = 'X'.
  LS_SEL_STATUS-AP_STATUS_7 = 'X'.

  CLEAR LS_SEL_WITH_OR_WITOUT.
  LS_SEL_WITH_OR_WITOUT-SEL_DISPLAY_EXISTING = 'X'.

  " get destination
  CALL FUNCTION 'HRHAP_GET_RFC_DESTINATION'
    IMPORTING
      RFC_DESTINATION = L_RFC_DESTINATION.

  LOOP AT LT_P0001.
    " set appraisal appraisee
    LS_APPRAISEES-PLVAR = '01'.
    LS_APPRAISEES-OTYPE = 'P'.
    LS_APPRAISEES-SOBID = LT_P0001-PERNR.
    APPEND LS_APPRAISEES TO LT_APPRAISEES.

    " get appraisal document
    CALL FUNCTION 'HRHAP_RFC_DOCUMENT_GET_LIST'
      DESTINATION L_RFC_DESTINATION
      EXPORTING
        PLAN_VERSION          = '01'
        S_SEL_DATE            = LS_SEL_DATES
        S_SEL_STATUS          = LS_SEL_STATUS
        S_SEL_WITH_OR_WITHOUT = LS_SEL_WITH_OR_WITOUT
      IMPORTING
        S_RETURN              = LS_RETURN
      TABLES
        T_APPRAISEES          = LT_APPRAISEES
        T_DOCUMENTS           = LT_DOCUMENTS.

    IF LS_RETURN-TYPE EQ 'E'.
      CONTINUE.
    ENDIF.

    " read the most recent appraisal document
    SORT LT_DOCUMENTS BY AP_END_DATE DESCENDING.
    READ TABLE LT_DOCUMENTS INTO LS_DOCUMENTS
                            INDEX 1.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    " set cost center id
    GS_RESULT-KOSTL = LT_P0001-KOSTL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_RESULT-KOSTL
      IMPORTING
        OUTPUT = GS_RESULT-KOSTL.
    " read cost center text
    READ TABLE LT_KTEXT WITH KEY KOSTL = LT_P0001-KOSTL
                        BINARY SEARCH.
    IF SY-SUBRC = 0.
      " set cost center text
      GS_RESULT-KTEXT = LT_KTEXT-KTEXT.
    ENDIF.

    GS_RESULT-STATX = LS_DOCUMENTS-AP_STATUS_NAME.

    " set appraisee id
    GS_RESULT-APPEE = LS_DOCUMENTS-APPRAISEE_ID.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_RESULT-APPEE
      IMPORTING
        OUTPUT = GS_RESULT-APPEE.

    " read first name, last name
    READ TABLE LT_P0002 WITH KEY PERNR = LT_P0001-PERNR
                        BINARY SEARCH.
    IF SY-SUBRC = 0.
      " set appraisee first name, last name
      GS_RESULT-VORNA = LT_P0002-VORNA.
      GS_RESULT-NACHN = LT_P0002-NACHN.
    ENDIF.

    " read classification
    READ TABLE LT_CLASS WITH KEY JOBID = LT_P0001-STELL
                        BINARY SEARCH.
    IF SY-SUBRC = 0.
      " set classification
      GS_RESULT-CLFID = LT_CLASS-CLFID.
      GS_RESULT-CLFTX = LT_CLASS-CLFTX.
    ENDIF.

    " get grade
    SELECT SINGLE GRADE
      INTO GS_RESULT-GRADE
      FROM HRP9870
     WHERE PLVAR = '01'
       AND OTYPE = 'S'
       AND OBJID = LT_P0001-PLANS
       AND ISTAT = '1'
       AND BEGDA <= LS_DOCUMENTS-AP_END_DATE
       AND ENDDA >= LS_DOCUMENTS-AP_END_DATE.

    " set supervisor id, name
    GS_RESULT-APPER = LS_DOCUMENTS-APPRAISER_ID.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = GS_RESULT-APPER
      IMPORTING
        OUTPUT = GS_RESULT-APPER.
    GS_RESULT-APPERNM = LS_DOCUMENTS-APPRAISER_NAME.

    " read hiring date
    READ TABLE LT_HIRDA WITH KEY PERNR = LT_P0001-PERNR
                        BINARY SEARCH.
    IF SY-SUBRC = 0.
      " set hiring date
      GS_RESULT-HIRDA = LT_HIRDA-BEGDA.
    ENDIF.

    " set appraisal period
    GS_RESULT-ZYEAR = LS_DOCUMENTS-AP_END_DATE(4).
    GS_RESULT-STRDA = LS_DOCUMENTS-AP_START_DATE.
    GS_RESULT-ENDDA = LS_DOCUMENTS-AP_END_DATE.
    GS_RESULT-PERNR = LT_P0001-PERNR.
    " get appraisal detail info
    PERFORM GET_DETAIL USING LS_DOCUMENTS.

    CLEAR: LT_P0001, LT_APPRAISEES, LS_APPRAISEES, LS_RETURN,
           LT_DOCUMENTS, LS_DOCUMENTS, GS_RESULT.
  ENDLOOP.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_DETAIL  USING    PS_DOCUMENTS TYPE HAP_S_DOCUMENTS.

  DATA: LT_BODY_ELEMENTS        TYPE HAP_T_BODY_ELEMENTS,
        LT_BODY_TMP    TYPE TABLE OF HAP_S_BODY_ELEMENTS WITH HEADER LINE,
        LT_BODY_CELLS           TYPE HAP_T_BODY_CELLS,
        LT_BODY_CELL_NOTES      TYPE HAP_T_BODY_CELL_NOTES,
        LT_BODY_COLUMNS         TYPE HAP_T_BODY_COLUMNS,
        LT_HRP1001              TYPE TABLE OF HRP1001 WITH HEADER LINE,
        LT_HRP9871              TYPE TABLE OF HRP9871 WITH HEADER LINE.

  DATA: LS_APPRAISAL_ID         TYPE HAP_S_APPRAISAL_ID,
        LS_BODY_ELEMENTS        TYPE HAP_S_BODY_ELEMENTS,
        LS_BODY                 TYPE HAP_S_BODY_ELEMENTS,
        LS_BODY_ELEMENTS_CHILD  TYPE HAP_S_BODY_ELEMENTS,
        LS_BODY_CELLS           TYPE HAP_S_BODY_CELLS,
        LS_BODY_TIME            TYPE HAP_S_BODY_CELLS,
        LS_BODY_CELL_NOTES      TYPE HAP_S_BODY_CELL_NOTES,
        LS_RETURN               TYPE BAL_S_MSG.

  DATA: LV_DOTXT                TYPE HAP_VALUE_TXT,
        LV_DPSTXTM              TYPE HAP_VALUE_TXT,
        LV_DPSTXTY              TYPE HAP_VALUE_TXT.

*  DATA: lt_notes_0011           TYPE hap_t_body_cell_notes,
*        lt_notes_0013           TYPE hap_t_body_cell_notes,
*        lt_notes_0014           TYPE hap_t_body_cell_notes.

  DATA: BEGIN OF LT_NOTES_HEAD OCCURS 0,
          ROW_IID         TYPE HAP_ROW_IID,
          COMPR           TYPE HAP_VALUE_TXT,
          TFTXT           TYPE HAP_VALUE_TXT,
          VALUE_TEXT      TYPE HAP_VALUE_TXT,
        END OF LT_NOTES_HEAD.

  DATA: BEGIN OF LT_NOTES_0011 OCCURS 0,
          COMPG           TYPE HAP_ELEMENT_NAME_XL,
          COMPT           TYPE HAP_ELEMENT_NAME_XL,
          COMPR           TYPE HAP_ELEMENT_NAME_XL,
          TFTXT           TYPE HAP_VALUE_TXT,   "Time Frame
          DOTXT           TYPE HAP_VALUE_TXT,
          ROW_IID         TYPE HAP_ROW_IID,
          COLUMN_IID      TYPE HAP_COLUMN_IID,
          TABSEQNR        TYPE HRTABSEQNR,
          TDFORMAT        TYPE TDFORMAT,
          TDLINE          TYPE TDLINE,
          LINE_STATUS     TYPE HAP_NOTE_LINE_STATUS,
        END OF LT_NOTES_0011.

  DATA: BEGIN OF LT_NOTES_0013 OCCURS 0,
          DPSTXTM         TYPE HAP_VALUE_TXT,
          ROW_IID         TYPE HAP_ROW_IID,
          COLUMN_IID      TYPE HAP_COLUMN_IID,
          TABSEQNR        TYPE HRTABSEQNR,
          TDFORMAT        TYPE TDFORMAT,
          TDLINE          TYPE TDLINE,
          LINE_STATUS     TYPE HAP_NOTE_LINE_STATUS,
        END OF LT_NOTES_0013.

  DATA: BEGIN OF LT_NOTES_0014 OCCURS 0,
          DPSTXTY         TYPE HAP_VALUE_TXT,
          ROW_IID         TYPE HAP_ROW_IID,
          COLUMN_IID      TYPE HAP_COLUMN_IID,
          TABSEQNR        TYPE HRTABSEQNR,
          TDFORMAT        TYPE TDFORMAT,
          TDLINE          TYPE TDLINE,
          LINE_STATUS     TYPE HAP_NOTE_LINE_STATUS,
        END OF LT_NOTES_0014.

  DATA LT_0011            LIKE LT_NOTES_0011 OCCURS 0 WITH HEADER LINE.
  DATA LT_0013            LIKE LT_NOTES_0013 OCCURS 0 WITH HEADER LINE.
  DATA LT_0014            LIKE LT_NOTES_0014 OCCURS 0 WITH HEADER LINE.

  DATA LS_NOTES_0011      LIKE LT_NOTES_0011.
  DATA LS_NOTES_0013      LIKE LT_NOTES_0013.
  DATA LS_NOTES_0014      LIKE LT_NOTES_0014.

  DATA: RT_OBJID                TYPE RANGE OF HROBJID WITH HEADER LINE.

  DATA: LT_RESULT               LIKE TABLE OF GS_RESULT.
  DATA: LS_RESULT               LIKE GS_RESULT.

  DATA LV_CNT1                  TYPE SY-INDEX.
  DATA LV_CNT2                  TYPE SY-INDEX.
  DATA LV_CNT3                  TYPE SY-INDEX.
  DATA LV_INDEX                 TYPE SY-INDEX.

  DATA LV_COMPG           TYPE HAP_ELEMENT_NAME_XL.
  DATA LV_COMPT           TYPE HAP_ELEMENT_NAME_XL.



  CLEAR LS_APPRAISAL_ID.
  LS_APPRAISAL_ID-APPRAISAL_ID = PS_DOCUMENTS-APPRAISAL_ID.
  LS_APPRAISAL_ID-PART_AP_ID = PS_DOCUMENTS-PART_AP_ID.

  " get appraisal detail
  CALL FUNCTION 'HRHAP_DOCUMENT_GET_DETAIL'
    EXPORTING
      PLAN_VERSION      = PS_DOCUMENTS-PLAN_VERSION
      S_APPRAISAL_ID    = LS_APPRAISAL_ID
    IMPORTING
      T_BODY_COLUMNS    = LT_BODY_COLUMNS
      T_BODY_ELEMENTS   = LT_BODY_ELEMENTS
      T_BODY_CELLS      = LT_BODY_CELLS
      T_BODY_CELL_NOTES = LT_BODY_CELL_NOTES
      S_RETURN          = LS_RETURN.

  IF LS_RETURN-MSGTY EQ 'E'.
    EXIT.
  ENDIF.

  " read VA element
  READ TABLE LT_BODY_ELEMENTS INTO LS_BODY_ELEMENTS
                              INDEX 1.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  SORT LT_BODY_ELEMENTS   BY ROW_IID.
  SORT LT_BODY_CELLS      BY ROW_IID COLUMN_IID.
  SORT LT_BODY_CELL_NOTES BY ROW_IID COLUMN_IID.

  LT_BODY_TMP[] = LT_BODY_ELEMENTS[].

  CLEAR LT_HRP9871[].
  SELECT *
    INTO TABLE LT_HRP9871
    FROM HRP9871
   WHERE PLVAR = '01'
     AND OTYPE = 'VB'
     AND ISTAT = '4'
     AND BEGDA <= SY-DATUM
     AND ENDDA >= SY-DATUM
     AND IGID =  '10'.

  SORT LT_HRP9871 BY IGID.
  LOOP AT LT_HRP9871.
    " read vb element
    READ TABLE LT_BODY_ELEMENTS INTO LS_BODY_ELEMENTS
                                WITH KEY ELEMENT_ID = LT_HRP9871-OBJID.
    IF SY-SUBRC = 0.
      CLEAR: LT_NOTES_0014, LT_NOTES_0014[].
      CLEAR: LT_NOTES_0013, LT_NOTES_0013[].
      CLEAR: LT_NOTES_0011, LT_NOTES_0011[].

      LOOP AT LT_BODY_TMP  INTO LS_BODY
                          WHERE PARENT = LS_BODY_ELEMENTS-ROW_IID.

        "Development Opportunity.
        READ TABLE LT_BODY_CELLS INTO LS_BODY_CELLS
                                 WITH KEY ROW_IID = LS_BODY-ROW_IID
                                          COLUMN_IID = '0011'
                                 BINARY SEARCH.

        IF SY-SUBRC = 0.
          "Competency/Competency Group
          CLEAR: LV_COMPG, LV_COMPT.
          PERFORM GET_COMPETENCY USING LS_BODY-FOREIGN_ID
                                       LV_COMPG
                                       LV_COMPT.

          "Time Frame.
          READ TABLE LT_BODY_CELLS INTO LS_BODY_TIME
                                   WITH KEY ROW_IID = LS_BODY-ROW_IID
                                            COLUMN_IID = '0012'
                                   BINARY SEARCH.

          LOOP AT LT_BODY_CELL_NOTES  INTO LS_BODY_CELL_NOTES
                                     WHERE ROW_IID = LS_BODY-ROW_IID
                                       AND COLUMN_IID = '0011'.
            MOVE-CORRESPONDING LS_BODY_CELL_NOTES TO LT_NOTES_0011.
            LT_NOTES_0011-DOTXT = LS_BODY_CELLS-VALUE_TEXT.
            LT_NOTES_0011-COMPG = LV_COMPG.
            LT_NOTES_0011-COMPT = LV_COMPT.
            LT_NOTES_0011-COMPR = LS_BODY-NAME.
            LT_NOTES_0011-TFTXT = LS_BODY_TIME-VALUE_TEXT.
            APPEND LT_NOTES_0011.

            MOVE-CORRESPONDING LS_BODY_CELL_NOTES TO LT_NOTES_HEAD.
            LT_NOTES_HEAD-VALUE_TEXT = LS_BODY_CELLS-VALUE_TEXT.
*            lt_notes_head-compr = ls_body-name.
*            lt_notes_head-tftxt = ls_body_time-value_text.
            APPEND LT_NOTES_HEAD.
          ENDLOOP.
        ENDIF.

        "Develpment Plan Status(MY)
        READ TABLE LT_BODY_CELLS INTO LS_BODY_CELLS
                                 WITH KEY ROW_IID = LS_BODY-ROW_IID
                                          COLUMN_IID = '0013'
                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          LOOP AT LT_BODY_CELL_NOTES  INTO LS_BODY_CELL_NOTES
                                     WHERE ROW_IID = LS_BODY-ROW_IID
                                       AND COLUMN_IID = '0013'.
            MOVE-CORRESPONDING LS_BODY_CELL_NOTES TO LT_NOTES_0013.
            LT_NOTES_0013-DPSTXTM = LS_BODY_CELLS-VALUE_TEXT.
            APPEND LT_NOTES_0013.
          ENDLOOP.
        ENDIF.

        "Develpment Plan Status(YE)
        READ TABLE LT_BODY_CELLS INTO LS_BODY_CELLS
                                 WITH KEY ROW_IID = LS_BODY-ROW_IID
                                          COLUMN_IID = '0014'
                                 BINARY SEARCH.
        IF SY-SUBRC = 0.
          LOOP AT LT_BODY_CELL_NOTES  INTO LS_BODY_CELL_NOTES
                                     WHERE ROW_IID = LS_BODY-ROW_IID
                                       AND COLUMN_IID = '0014'.
            MOVE-CORRESPONDING LS_BODY_CELL_NOTES TO LT_NOTES_0014.
            LT_NOTES_0014-DPSTXTY = LS_BODY_CELLS-VALUE_TEXT.
            APPEND LT_NOTES_0014.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT LT_NOTES_HEAD BY ROW_IID.
  DELETE ADJACENT DUPLICATES FROM LT_NOTES_HEAD COMPARING ALL FIELDS.
  LOOP AT LT_NOTES_HEAD.
    CLEAR: LV_CNT1, LV_CNT2, LV_CNT3, LT_RESULT[],
           LT_0011, LT_0011[], LT_0013, LT_0013[],
           LT_0014, LT_0014[].

    LOOP AT LT_NOTES_0011 WHERE ROW_IID = LT_NOTES_HEAD-ROW_IID.
      ADD 1 TO LV_CNT1.
      MOVE-CORRESPONDING LT_NOTES_0011 TO LT_0011.
      APPEND LT_0011.
    ENDLOOP.

    LOOP AT LT_NOTES_0013 WHERE ROW_IID = LT_NOTES_HEAD-ROW_IID.
      ADD 1 TO LV_CNT2.
      MOVE-CORRESPONDING LT_NOTES_0013 TO LT_0013.
      APPEND LT_0013.
    ENDLOOP.

    LOOP AT LT_NOTES_0014 WHERE ROW_IID = LT_NOTES_HEAD-ROW_IID.
      ADD 1 TO LV_CNT3.
      MOVE-CORRESPONDING LT_NOTES_0014 TO LT_0014.
      APPEND LT_0014.
    ENDLOOP.

    IF LV_CNT1 = 0 AND LV_CNT2 = 0 AND LV_CNT3 = 0.
      APPEND GS_RESULT TO GT_RESULT.

    ELSE.
      IF LV_CNT1 >= LV_CNT2 .
        IF LV_CNT2 >= LV_CNT3.
          DO LV_CNT1 TIMES.
*            gs_result-compr = lt_notes_head-compr.
*            gs_result-tftxt = lt_notes_head-tftxt.
            APPEND GS_RESULT TO LT_RESULT.
          ENDDO.
        ELSE.
          IF LV_CNT1 >= LV_CNT3.
            DO LV_CNT1 TIMES.
*              gs_result-compr = lt_notes_head-compr.
*              gs_result-tftxt = lt_notes_head-tftxt.
              APPEND GS_RESULT TO LT_RESULT.
            ENDDO.
          ELSE.
            DO LV_CNT3 TIMES.
*              gs_result-compr = lt_notes_head-compr.
*              gs_result-tftxt = lt_notes_head-tftxt.
              APPEND GS_RESULT TO LT_RESULT.
            ENDDO.
          ENDIF.
        ENDIF.
      ELSE.
        IF LV_CNT2 >= LV_CNT3.
          DO LV_CNT2 TIMES.
*            gs_result-compr = lt_notes_head-compr.
*            gs_result-tftxt = lt_notes_head-tftxt.
            APPEND GS_RESULT TO LT_RESULT.
          ENDDO.
        ELSE.
          DO LV_CNT3 TIMES.
*            gs_result-compr = lt_notes_head-compr.
*            gs_result-tftxt = lt_notes_head-tftxt.
            APPEND GS_RESULT TO LT_RESULT.
          ENDDO.
        ENDIF.
      ENDIF.
    ENDIF.

    IF LT_RESULT[] IS NOT INITIAL.
      SORT LT_0011 BY ROW_IID TABSEQNR.
      SORT LT_0013 BY ROW_IID TABSEQNR.
      SORT LT_0014 BY ROW_IID TABSEQNR.
      CLEAR LV_INDEX.
      LOOP AT LT_RESULT INTO LS_RESULT.
        ADD 1 TO LV_INDEX.
        READ TABLE LT_0011 INTO LS_NOTES_0011 INDEX LV_INDEX.
        IF SY-SUBRC = 0.
          IF LV_INDEX = 1.
            LS_RESULT-COMPG  = LS_NOTES_0011-COMPG.
            LS_RESULT-COMPT  = LS_NOTES_0011-COMPT.
            LS_RESULT-COMPR  = LS_NOTES_0011-COMPR.
            LS_RESULT-TFTXT  = LS_NOTES_0011-TFTXT.
            LS_RESULT-DOTXT  = LS_NOTES_0011-DOTXT.
            LS_RESULT-DONOTE = LS_NOTES_0011-TDLINE.
          ELSE.
            LS_RESULT-DONOTE = LS_NOTES_0011-TDLINE.
          ENDIF.

        ENDIF.

        READ TABLE LT_0013 INTO LS_NOTES_0013 INDEX LV_INDEX.
        IF SY-SUBRC = 0.
          IF LV_INDEX = 1.
            LS_RESULT-DPSTXTM = LS_NOTES_0013-DPSTXTM.
            LS_RESULT-DPSTNTM = LS_NOTES_0013-TDLINE.
          ELSE.
            LS_RESULT-DPSTNTM = LS_NOTES_0013-TDLINE.
          ENDIF.
        ENDIF.

        READ TABLE LT_0014 INTO LS_NOTES_0014 INDEX LV_INDEX.
        IF SY-SUBRC = 0.
          IF LV_INDEX = 1.
            LS_RESULT-DPSTXTY = LS_NOTES_0014-DPSTXTY.
            LS_RESULT-DPSTNTY = LS_NOTES_0014-TDLINE.
          ELSE.
            LS_RESULT-DPSTNTY = LS_NOTES_0014-TDLINE.
          ENDIF.
        ENDIF.

        APPEND LS_RESULT TO GT_RESULT.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  GET_COMPETENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM GET_COMPETENCY  USING    P_OBJID
                              P_COMPETENCY
                              P_COMPETENCY_GROUP.

  DATA LV_SOBID      LIKE HRP1001-SOBID.
  DATA LV_SOBID_G    LIKE HRP1001-SOBID.

  "Competency
  SELECT SINGLE SOBID
    INTO LV_SOBID
    FROM HRP1001
   WHERE SCLAS = 'Q'
     AND SUBTY = 'A030'
     AND OBJID = P_OBJID
     AND BEGDA <= SY-DATUM
     AND ENDDA >= SY-DATUM.

  SELECT SINGLE STEXT
    INTO P_COMPETENCY
    FROM HRP1000
   WHERE OBJID = LV_SOBID.

  "Competency Group
  SELECT SINGLE SOBID
    INTO LV_SOBID_G
    FROM HRP1001
   WHERE SCLAS = 'QK'
     AND SUBTY = 'A030'
     AND OBJID = LV_SOBID
     AND BEGDA <= SY-DATUM
     AND ENDDA >= SY-DATUM.

  SELECT SINGLE STEXT
    INTO P_COMPETENCY_GROUP
    FROM HRP1000
   WHERE OBJID = LV_SOBID_G.

ENDFORM.                    " GET_COMPETENCY
