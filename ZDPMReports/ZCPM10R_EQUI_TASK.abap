*&---------------------------------------------------------------------*
*& Report  ZCPM10R_EQUI_TASK                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZCPM10R_EQUI_TASK             .

FIELD-SYMBOLS : <FS>.
FIELD-SYMBOLS : <TXT>.

TABLES : EAPL.  "//Allocation of task lists to pieces of equipment

*** For File Select
DATA: WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

DATA: BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_TAB.

DATA: BEGIN OF IT_TREC OCCURS 0,
        EQUNR(18),    "//
        TL_EXTID(40), "//External ID of Tasklist
        PLNAL(2),     "//Group counter
        KTEXT(100),    "//Task list description
        ARBPL(8),     "//Work center
        WERKS(4),  "//Plant
        VAGRP(3),  "// planner group
        STATU(2),  "//Status
        VERWE(3),   "//Usage
        ANLZU(1),  "//System condition

        STEUS(4),    "//Control key
        LTXA1(100),  "//Short text


        ARBEI(9),	   "//Work involved in the activity
        ARBEH(3),	   "//Unit for work
*        DAUNO(7),	  "//Normal duration of the activity
        ANZZL(3),    "//Number of capacities required
        DAUNE(3),	  "//Normal duration/unit
*        INDET(1),   "//Key for calculation
        SAKTO(10),  "//Cost element
        MATKL(9),	  "//Material group
        EKGRP(3),	  "//Purchasing group for external processing
*        LIFNR(10),  "//Account number of vendor or creditor
        EKORG(4),	  "//Purchasing organization

*        MUSTER(10), "//No of service specifications

      END OF IT_TREC.

DATA: WA_COUNT TYPE I,
      WA_LINE_COUNT(2) TYPE N VALUE 1,
      WA_PREV_EQUNR(18),
      WA_PREV_PLNAL(2).

DATA: WA_TEXT01(50),
      WA_TEXT02(50),
      WA_TEXT03(50).

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_FILE  LIKE RLGRAP-FILENAME MODIF ID BCD.
SELECTION-SCREEN PUSHBUTTON /68(10) PH_FNAME USER-COMMAND CHG.
PARAMETER: P_GROUP LIKE APQI-GROUPID DEFAULT SY-UNAME  OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.
  MOVE 'Find' TO PH_FNAME.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CHG'.
      CLEAR: SY-UCOMM.
      PERFORM FILENAME_GET.

    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      PERFORM MAIN_PROCESS.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
******** START-OF-SELECTION *****************
*********************************************
START-OF-SELECTION.
*********************************************


******** END-OF-SELECTION *******************
*********************************************
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILENAME_GET.
*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.XLS,*.XLS.'
            MODE             = 'O'
            TITLE            = 'Select Upload File'
       IMPORTING
            FILENAME         = P_FILE
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.
ENDFORM.                    " FILENAME_GET
*&---------------------------------------------------------------------*
*&      Form  MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_PROCESS.
  CLEAR: IT_TREC,   IT_TREC[].

  IF P_FILE IS INITIAL.
    PERFORM FILENAME_GET.
  ENDIF.

  PERFORM EXCEL_FILE_UPLOAD  TABLES   IT_TREC
                             USING    P_FILE
                                      WA_RC.
  PERFORM BDC_PROCESS.

  MESSAGE S000(ZMPM) WITH WA_COUNT 'count uploading success.'.
  CALL TRANSACTION 'SM35'.
ENDFORM.                    " MAIN_PROCESS
*&---------------------------------------------------------------------*
*&      Form  EXCEL_FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TREC  text
*      -->P_WA_FNAME  text
*      -->P_WA_RC  text
*----------------------------------------------------------------------*
FORM EXCEL_FILE_UPLOAD TABLES   IT_TREC
                       USING    P_FNAME
                                P_RC.

  DATA : IT_INTERN TYPE  ZKCDE_CELLS OCCURS 0 WITH HEADER LINE.
  DATA : LV_INDEX TYPE I.
  DATA : LV_START_COL TYPE I VALUE '1',
         LV_START_ROW TYPE I VALUE '1',
         LV_END_COL   TYPE I VALUE '256',
         LV_END_ROW   TYPE I VALUE '65536'.

*** Data transfer from PC files to Internal Table.
  CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FNAME
            I_BEGIN_COL             = LV_START_COL
            I_BEGIN_ROW             = LV_START_ROW
            I_END_COL               = LV_END_COL
            I_END_ROW               = LV_END_ROW
       TABLES
            INTERN                  = IT_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2.

*  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
*       EXPORTING
*            FILENAME                = P_FNAME
*            I_BEGIN_COL             = LV_START_COL
*            I_BEGIN_ROW             = LV_START_ROW
*            I_END_COL               = LV_END_COL
*            I_END_ROW               = LV_END_ROW
*       TABLES
*            INTERN                  = IT_INTERN
*       EXCEPTIONS
*            INCONSISTENT_PARAMETERS = 1
*            UPLOAD_OLE              = 2.

* mapping Excel cells to Interanl Table fields
  MOVE : SY-SUBRC TO P_RC.
  CHECK NOT IT_INTERN[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE IT_INTERN WHERE ROW LE 2.

  SORT IT_INTERN BY ROW COL.
  LOOP AT IT_INTERN.
    MOVE : IT_INTERN-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE IT_TREC TO <FS>.
    MOVE : IT_INTERN-VALUE TO <FS>.
    AT END OF ROW.
      APPEND IT_TREC.
      CLEAR IT_TREC.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " EXCEL_FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_PROCESS.
* BDC OPEN GROUP FUNCTION
  PERFORM CALL_BDC_OPEN_GROUP.

  IF SY-SUBRC EQ 0.
*   BDC DATA ?? & INSERT ????
    PERFORM BDC_GENERATE_PROCESS.
*   BDC CLOSE GROUP FUNCTION
    PERFORM CALL_BDC_CLOSE_GROUP.
  ELSE.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BDC_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = SY-MANDT
            GROUP               = P_GROUP
            KEEP                = ''
            USER                = SY-UNAME
       EXCEPTIONS
            RUNNING             = 1
            QUEUE_ERROR         = 2
            CLIENT_INVALID      = 3
            GROUP_INVALID       = 4
            USER_INVALID        = 5
            HOLDDATE_INVALID    = 6
            DESTINATION_INVALID = 7.
ENDFORM.                    " CALL_BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*&      Form  BDC_GENERATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_GENERATE_PROCESS.
  LOOP AT IT_TREC.
    PERFORM GENERATE_BDC_DATA.
    CLEAR IT_TREC.
  ENDLOOP.

  PERFORM DYNPRO USING:
           'X' 'SAPLCPDI'    '3400',
           ' ' 'BDC_OKCODE'  '=BU'.
  PERFORM CALL_BDC_INSERT.

ENDFORM.                    " BDC_GENERATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_INSERT
*&---------------------------------------------------------------------*
FORM CALL_BDC_INSERT.
  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
           TCODE            =  'IA01'
*           POST_LOCAL       = NOVBLOCAL
*           PRINTING         = NOPRINT
       TABLES
            DYNPROTAB        =  BDC_TAB
      EXCEPTIONS
           INTERNAL_ERROR   = 1
           NOT_OPEN         = 2
           QUEUE_ERROR      = 3
           TCODE_INVALID    = 4
           PRINTING_INVALID = 5
           POSTING_INVALID  = 6
           OTHERS           = 7
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " CALL_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
FORM CALL_BDC_CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
*      EXCEPTIONS
*           NOT_OPEN    = 1
*           QUEUE_ERROR = 2
*           OTHERS      = 3
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " CALL_BDC_CLOSE_GROUP
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          DYNBEGIN TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GENERATE_BDC_DATA                                        *
*---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA.
  DATA: WA_LEN TYPE I.

  IF IT_TREC-EQUNR NE WA_PREV_EQUNR OR
     IT_TREC-PLNAL NE WA_PREV_PLNAL.
    WA_COUNT = WA_COUNT + 1.

    IF  WA_COUNT NE 1.
      PERFORM DYNPRO USING:
           'X' 'SAPLCPDI'    '3400',
           ' ' 'BDC_OKCODE'  '=BU'.
      PERFORM CALL_BDC_INSERT.
    ENDIF.

    REFRESH BDC_TAB.
    PERFORM DYNPRO USING:
           'X' 'SAPLCPDI'    '3010',
           ' ' 'RC27E-EQUNR' IT_TREC-EQUNR,    "//Equipment
           ' ' 'BDC_OKCODE'  '/00'.

    SELECT  SINGLE *
            FROM EAPL
            WHERE EQUNR = IT_TREC-EQUNR.
    IF SY-SUBRC EQ 0.
      WA_PREV_EQUNR = IT_TREC-EQUNR.
    ENDIF.

    IF IT_TREC-EQUNR EQ WA_PREV_EQUNR.
      PERFORM DYNPRO USING:
             'X' 'SAPLCPDI'    '3200',
             ' ' 'BDC_OKCODE'  '=ANLG'.
    ENDIF.

    PERFORM DYNPRO USING:
           'X' 'SAPLCPDA'        '3010',
*           ' ' 'BDC_OKCODE'      '=VOUE',
           ' ' 'PLKOD-PLNAL'     IT_TREC-PLNAL,
           ' ' 'PLKOD-TL_EXTID'  IT_TREC-TL_EXTID,
           ' ' 'PLKOD-VERWE'     IT_TREC-VERWE,    "//Usage
           ' ' 'RCR01-ARBPL'     IT_TREC-ARBPL,
           ' ' 'RCR01-WERKS'     IT_TREC-WERKS,
           ' ' 'PLKOD-VAGRP'     IT_TREC-VAGRP,
           ' ' 'PLKOD-STATU'     IT_TREC-STATU,
           ' ' 'PLKOD-ANLZU'     IT_TREC-ANLZU.

    CLEAR: WA_LEN, WA_TEXT01, WA_TEXT02, WA_TEXT03.

    WA_LEN = STRLEN( IT_TREC-KTEXT ).
    IF WA_LEN <= 40.
      PERFORM DYNPRO USING:
            ' ' 'BDC_OKCODE'      '=VOUE',
            ' ' 'PLKOD-KTEXT'     IT_TREC-KTEXT.
    ELSE.
      PERFORM SPLIT_STRING USING    IT_TREC-KTEXT
                           CHANGING WA_TEXT01
                                    WA_TEXT02
                                    WA_TEXT03.
      PERFORM DYNPRO USING:
             ' ' 'PLKOD-KTEXT'        '.',
             ' ' 'BDC_OKCODE'	       '=ATXT',

             'X' 'SAPLSTXX'          '1100',
             ' ' 'BDC_OKCODE'	       '=TXBA',
             ' ' 'RSTXT-TXLINE(03)'	 WA_TEXT01,
             ' ' 'RSTXT-TXLINE(04)'	 WA_TEXT02,
             ' ' 'RSTXT-TXLINE(05)'  WA_TEXT03,
             'X' 'SAPLCPDA'          '3010',
             ' ' 'BDC_OKCODE'        '=VOUE'.

    ENDIF.
    PERFORM DYNPRO USING:
         'X' 'SAPLCPDI'        '3400',
         ' ' 'PLPOD-STEUS(01)'     IT_TREC-STEUS,
*         ' ' 'PLPOD-LTXA1(01)' IT_TREC-LTXA1,	
         ' ' 'PLPOD-ARBEI(01)' IT_TREC-ARBEI,	
         ' ' 'PLPOD-ARBEH(01)' IT_TREC-ARBEH,	
*         ' ' 'PLPOD-DAUNO(01)'  IT_TREC-DAUNO,	"//1
         ' ' 'PLPOD-ANZZL(01)'  IT_TREC-ANZZL,
         ' ' 'PLPOD-DAUNE(01)'  IT_TREC-DAUNE,	"//h
*         ' ' 'PLPOD-INDET(01)'  '1',  "//IT_TREC-INDET,	
         ' ' 'PLPOD-SAKTO(01)' IT_TREC-SAKTO,
	
         ' ' 'PLPOD-MATKL(01)' IT_TREC-MATKL,	
         ' ' 'PLPOD-EKGRP(01)' IT_TREC-EKGRP,	
*           ' ' 'PLPOD-LIFNR(01)' IT_TREC-LIFNR,
         ' ' 'PLPOD-EKORG(01)' IT_TREC-EKORG.

    CLEAR: WA_LEN, WA_TEXT01, WA_TEXT02, WA_TEXT03.

    WA_LEN = STRLEN( IT_TREC-LTXA1 ).
    IF WA_LEN <= 40.
      PERFORM DYNPRO USING:
            ' ' 'BDC_OKCODE'      '/00',
            ' ' 'PLPOD-LTXA1(01)'     IT_TREC-LTXA1.
    ELSE.
      PERFORM SPLIT_STRING USING    IT_TREC-LTXA1
                           CHANGING WA_TEXT01
                                    WA_TEXT02
                                    WA_TEXT03.
      PERFORM DYNPRO USING:
             ' ' 'PLPOD-LTXA1(01)'  '.',
             ' ' 'BDC_CURSOR'       'RC270-TXTKZ(01)',
             ' ' 'BDC_OKCODE'	      '=PICK',

             'X' 'SAPLSTXX'          '1100',
             ' ' 'BDC_OKCODE'	       '=TXBA',
             ' ' 'RSTXT-TXLINE(03)'	 WA_TEXT01,
             ' ' 'RSTXT-TXLINE(04)'	 WA_TEXT02,
             ' ' 'RSTXT-TXLINE(05)'  WA_TEXT03.
    ENDIF.

*    IF IT_TREC-STEUS = 'PM03'.
*    PERFORM DYNPRO USING:
*            'X' 'SAPLMLSP'	'0200',
*            ' ' 'BDC_OKCODE'	'=VOKO',
*
*            'X' 'SAPLMLSP'	      '0500',
*            ' ' 'BDC_OKCODE'      '=SALL',
*
*            ' ' 'RM11P-MUST_SEL'	'X',
*            ' ' 'RM11P-MUSTER_LV'	IT_TREC-MUSTER,           "//PM_100
*
*            'X' 'SAPLMLSP'	      '0200',	
*		  ' '	'BDC_OKCODE'	'=ESB'.
*    ENDIF.

    PERFORM DYNPRO USING:
            'X' 'SAPLCPDI'    '3400',
            ' ' 'BDC_OKCODE'  '=P+'.

  ELSEIF IT_TREC-EQUNR EQ WA_PREV_EQUNR AND
         IT_TREC-PLNAL EQ WA_PREV_PLNAL.
    PERFORM DYNPRO USING:
           'X' 'SAPLCPDI'         '3400',
           ' ' 'PLPOD-STEUS(02)'  IT_TREC-STEUS,
*           ' ' 'PLPOD-LTXA1(02)' IT_TREC-LTXA1,	"//descrip
           ' ' 'PLPOD-ARBEI(02)'  IT_TREC-ARBEI,	"//1
           ' ' 'PLPOD-ARBEH(02)'  IT_TREC-ARBEH,	"//h
*           ' ' 'PLPOD-DAUNO(02)'  IT_TREC-DAUNO,	"//1
           ' ' 'PLPOD-ANZZL(02)'  IT_TREC-ANZZL,
           ' ' 'PLPOD-DAUNE(02)'  IT_TREC-DAUNE,	"//h
*           ' ' 'PLPOD-INDET(02)'  '1',    "//IT_TREC-INDET,
           ' ' 'PLPOD-SAKTO(02)'  IT_TREC-SAKTO,
           ' ' 'PLPOD-MATKL(02)' IT_TREC-MATKL,	
           ' ' 'PLPOD-EKGRP(02)'  IT_TREC-EKGRP,	
*           ' ' 'PLPOD-LIFNR(02)' IT_TREC-LIFNR,
           ' ' 'PLPOD-EKORG(02)'  IT_TREC-EKORG.

    WA_LEN = STRLEN( IT_TREC-LTXA1 ).
    IF WA_LEN <= 40.
      PERFORM DYNPRO USING:
            ' ' 'BDC_OKCODE'        '/00',
            ' ' 'PLPOD-LTXA1(02)'   IT_TREC-LTXA1.
    ELSE.
      PERFORM SPLIT_STRING USING    IT_TREC-LTXA1
                           CHANGING WA_TEXT01
                                    WA_TEXT02
                                    WA_TEXT03.
      PERFORM DYNPRO USING:
             ' ' 'PLPOD-LTXA1(02)'  '.',
             ' ' 'BDC_CURSOR'       'RC270-TXTKZ(02)',
             ' ' 'BDC_OKCODE'	      '=PICK',

             'X' 'SAPLSTXX'         '1100',
             ' ' 'BDC_OKCODE'	      '=TXBA',
             ' ' 'RSTXT-TXLINE(03)'	 WA_TEXT01,
             ' ' 'RSTXT-TXLINE(04)'	 WA_TEXT02,
             ' ' 'RSTXT-TXLINE(05)'  WA_TEXT03.
    ENDIF.

*      IF IT_TREC-STEUS = 'PM03'.
*        PERFORM DYNPRO USING:
*                'X' 'SAPLMLSP'	'0200',
*                ' ' 'BDC_OKCODE'	'=VOKO',
*
*                'X' 'SAPLMLSP'	      '0500',
*                ' ' 'BDC_OKCODE'      '=SALL',
*
*              ' ' 'RM11P-MUST_SEL'	'X',
*              ' ' 'RM11P-MUSTER_LV'	IT_TREC-MUSTER, "//PM_100
*
*                'X' 'SAPLMLSP'	      '0200',	
*  		  ' '	'BDC_OKCODE'	'=ESB'.
*
*      ENDIF.

    PERFORM DYNPRO USING:
         'X' 'SAPLCPDI'    '3400',
         ' ' 'BDC_OKCODE'  '=P+'.

  ENDIF.

  MOVE : IT_TREC-EQUNR TO WA_PREV_EQUNR,
         IT_TREC-PLNAL TO WA_PREV_PLNAL.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SPLIT_STRING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TREC_KTEXT  text
*      <--P_WA_TEXT01  text
*      <--P_WA_TEXT02  text
*      <--P_WA_TEXT03  text
*----------------------------------------------------------------------*
FORM SPLIT_STRING USING    P_KTEXT
                  CHANGING P_TEXT01
                           P_TEXT02
                           P_TEXT03.

  CLEAR: WA_TEXT01, WA_TEXT02, WA_TEXT03.

  DATA: BEGIN OF IT_TEXT OCCURS 0,
            TEXT(40),
        END OF IT_TEXT.

  DATA: WA_LEN TYPE I,
        WA_COUNT(2) TYPE N,
        WA_TXTNAME(10),
        WA_TEXT(50).

  WA_COUNT = '01'.
  CONCATENATE 'P_TEXT' WA_COUNT INTO WA_TXTNAME.
  ASSIGN (WA_TXTNAME) TO <TXT>.

  SPLIT P_KTEXT AT ' ' INTO TABLE IT_TEXT.

  LOOP AT IT_TEXT.
    CLEAR: WA_TEXT.
    CONCATENATE <TXT> IT_TEXT-TEXT INTO WA_TEXT SEPARATED BY ' '.
    WA_LEN = STRLEN( WA_TEXT ).
    IF WA_LEN < 32.
      MOVE WA_TEXT TO <TXT>.
    ELSE.
      WA_COUNT = WA_COUNT + 1.
      CONCATENATE 'P_TEXT' WA_COUNT INTO WA_TXTNAME.
      ASSIGN (WA_TXTNAME) TO <TXT>.
      CONCATENATE <TXT> IT_TEXT-TEXT INTO <TXT> SEPARATED BY ' '.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SPLIT_STRING
