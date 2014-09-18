* provided by ANDY CHOI
REPORT ZRTABLE MESSAGE-ID ZMSG0
               NO STANDARD PAGE HEADING
               LINE-SIZE 160
               LINE-COUNT 58(1).

************************************************************************
* TABLES                                                               *
************************************************************************
TABLES: DD01L,                         " µµ¸Þ?
*       DD01T,                         " R/3 DD: µµ¸ÞAI AØ½º?
*       DD02L,                         " SAP A×AI?
*       DD02T,                         " R/3-DD: SAP A×AIºi AØ½º?
        DD03L,                         " A×AIºiCE?
*       dd03t,                     " DD: CEµa¿¡ ´eCN AØ½ºÆ
        DD04L,                         " Data element
        DD04T.                         " R/3 DD: Data element AØ½º?

************************************************************************
* DATA                                                                 *
************************************************************************
* output list internal table
DATA: BEGIN OF ITAB_TAB OCCURS 0,
        TABNAME    LIKE  DD02L-TABNAME,"TABLE
        FIELDNAME  LIKE  DD03D-FIELDNAME,     "CEµaAI?
        KEYFLAG    LIKE  DD03D-KEYFLAG,"?
        ROLLNAME   LIKE  DD03D-ROLLNAME,      "Data element
        DATATYPE   LIKE  DD03D-DATATYPE,      "A??
        LENG       LIKE  DD03D-LENG,   "±æ?
        CHECKTABLE LIKE  DD03D-CHECKTABLE,    "A¡°EA×AI?
        DDTEXT     LIKE  DD03D-DDTEXT, "´U?
        ENTITYTAB  LIKE  DD03D-ENTITYTAB,     "°ªA×AI?
        REFTABLE   LIKE  DD03D-REFTABLE,      "AuA¶A×AI?
        REFFIELD   LIKE  DD03D-REFFIELD,      "AuA¶CE?
        NOTNULL    LIKE  DD03D-NOTNULL,"AE?
      END   OF ITAB_TAB.

DATA: ITAB_ELE LIKE DD04L OCCURS 0 WITH HEADER LINE,
      ITAB_DOM LIKE DD01L OCCURS 0 WITH HEADER LINE.

DATA: RCODE   LIKE  SY-SUBRC,
      DOMNAME LIKE  DD01L-DOMNAME.

DATA: TOP_TABNAM  LIKE  DD02L-TABNAME,
      TOP_DDTEXT  LIKE  DD02T-DDTEXT.
************************************************************************
* SELECT-OPTIONS                                                       *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_TABNAM  FOR  DD03L-TABNAME OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK BLK.
parameters: p_file like RLGRAP-FILENAME default 'c:\table.xls'.

************************************************************************
* PARAMETERS                                                           *
************************************************************************
*PARAMETERS:

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
*AT SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION                                                   *
************************************************************************
START-OF-SELECTION.

  PERFORM SELECT_DATA_AND_MAKE_ITAB.

  READ TABLE ITAB_TAB INDEX 1.
  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH 'A×AIºiAI A¸AcCIAo ¾E½A´I´U'.
    LEAVE LIST-PROCESSING.
  ENDIF.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.

  PERFORM WRITE_BODY.

************************************************************************
* TOP-OF-PAGE                                                          *
************************************************************************
TOP-OF-PAGE.

  PERFORM WRITE_HEADER.

************************************************************************
* END-OF-PAGE                                                          *
************************************************************************
END-OF-PAGE.

  WRITE: /1(156) SY-ULINE.

************************************************************************
* AT LINE-SELECTION                                                    *
************************************************************************
AT LINE-SELECTION.
  CASE SY-LSIND.
    WHEN 1.
      PERFORM DATA_ELEMENT_INFO.
    WHEN 2.
      PERFORM DOMAIN_INFO.
    WHEN OTHERS.

  ENDCASE.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT pf13.
  PERFORM data_download.
*&---------------------------------------------------------------------*
*&      Form  GET_TAB_TEXT
*&---------------------------------------------------------------------*
FORM GET_TAB_TEXT USING P_RCODE.

*.Table Short Text AÐ±a..
  SELECT SINGLE DDTEXT INTO TOP_DDTEXT
           FROM DD02T
          WHERE TABNAME    = TOP_TABNAM
            AND DDLANGUAGE = SY-LANGU.

  IF SY-SUBRC <> 0.
    SELECT SINGLE DDTEXT INTO TOP_DDTEXT
             FROM DD02T
            WHERE TABNAME    = TOP_TABNAM
              AND DDLANGUAGE = 'EN'.
  ENDIF.

  P_RCODE = SY-SUBRC.

ENDFORM.                               " GET_TAB_TEXT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_MAKE_ITAB
*&---------------------------------------------------------------------*
FORM SELECT_DATA_AND_MAKE_ITAB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE ITAB_TAB
    FROM DD03L
   WHERE TABNAME IN S_TABNAM.

  CHECK SY-SUBRC = 0.

  LOOP AT ITAB_TAB.

    SELECT SINGLE DDTEXT INTO ITAB_TAB-DDTEXT
             FROM DD04T
            WHERE ROLLNAME    = ITAB_TAB-ROLLNAME
              AND DDLANGUAGE  = SY-LANGU.
    IF SY-SUBRC <> 0.
      SELECT SINGLE SCRTEXT_M INTO ITAB_TAB-DDTEXT
               FROM DD04T
              WHERE ROLLNAME    = ITAB_TAB-ROLLNAME
                AND DDLANGUAGE  = 'EN'.
    ENDIF.

    SELECT SINGLE *
             FROM DD04L
            WHERE ROLLNAME    = ITAB_TAB-ROLLNAME.
    IF SY-SUBRC = 0.
      SELECT SINGLE ENTITYTAB INTO ITAB_TAB-ENTITYTAB
               FROM DD01L
              WHERE DOMNAME = DD04L-DOMNAME.
    ENDIF.

    MODIFY ITAB_TAB INDEX SY-TABIX.

  ENDLOOP.

ENDFORM.                               " SELECT_DATA_AND_MAKE_ITAB
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
FORM WRITE_HEADER.
  write:/ '(SHIFT+PF1) Execute Download'.

  PERFORM GET_TAB_TEXT USING RCODE.

  WRITE: /2(10) 'Table Name :',       TOP_TABNAM,
         /2(10) 'Short Text :',       TOP_DDTEXT.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /1(156) SY-ULINE,
         /1 SY-VLINE,     2(16)  'Fields'(012) CENTERED,
         18 SY-VLINE,    19(4)   'Key'(002) CENTERED,
         23 SY-VLINE,    24(16)  'Data element'(003) CENTERED,
         40 SY-VLINE,    41(6)   'Type'(004) CENTERED,
         47 SY-VLINE,    48(8)   'Leth.'(005) CENTERED,
         56 SY-VLINE,    57(12)  'Check Table'(006) CENTERED,
         69 SY-VLINE,    70(40)  'Short Text'(007) CENTERED,
        110 SY-VLINE,   111(12)  'Value Table'(008) CENTERED,
        123 SY-VLINE,   124(12)  'Ref.Table'(009) CENTERED,
        136 SY-VLINE,   137(12)  'Ref.Field'(010) CENTERED,
        149 SY-VLINE,   150(4)   'Initial'(011) CENTERED,
        156 SY-VLINE,
         /1(156) SY-ULINE.
  FORMAT COLOR OFF.

ENDFORM.                               " WRITE_HEADER

*&---------------------------------------------------------------------*
*&      Form  WRITE_BODY
*&---------------------------------------------------------------------*
FORM WRITE_BODY.

*  SORT ITAB_TAB BY TABNAME FIELDNAME.
  LOOP AT ITAB_TAB.
    AT NEW TABNAME.
      NEW-PAGE.
      TOP_TABNAM = ITAB_TAB-TABNAME.
    ENDAT.

    WRITE: /1 SY-VLINE,     2(16) ITAB_TAB-FIELDNAME,
           18 SY-VLINE,    19(4)  ITAB_TAB-KEYFLAG CENTERED,
           23 SY-VLINE,    24(16) ITAB_TAB-ROLLNAME,
           40 SY-VLINE,    41(6)  ITAB_TAB-DATATYPE,
           47 SY-VLINE,    48(8)  ITAB_TAB-LENG,
           56 SY-VLINE,    57(12) ITAB_TAB-CHECKTABLE,
           69 SY-VLINE,    70(40) ITAB_TAB-DDTEXT,
          110 SY-VLINE,   111(12) ITAB_TAB-ENTITYTAB,
          123 SY-VLINE,   124(12) ITAB_TAB-REFTABLE,
          136 SY-VLINE,   137(12) ITAB_TAB-REFFIELD,
          149 SY-VLINE,   150(4)  ITAB_TAB-NOTNULL CENTERED,
          156 SY-VLINE.
    HIDE: ITAB_TAB-ROLLNAME.

    AT END OF TABNAME.
      WRITE: /1(156) SY-ULINE.
    ENDAT.

  ENDLOOP.

ENDFORM.                               " WRITE_BODY
*&---------------------------------------------------------------------*
*&      Form  DATA_ELEMENT_INFO
*&---------------------------------------------------------------------*
FORM DATA_ELEMENT_INFO.

  REFRESH: ITAB_ELE.
  SELECT * INTO TABLE ITAB_ELE
           FROM DD04L
          WHERE ROLLNAME = ITAB_TAB-ROLLNAME.

  CHECK SY-SUBRC = 0.
  LOOP AT ITAB_ELE.
    WRITE:/ ITAB_ELE.
    HIDE ITAB_ELE-DOMNAME.
  ENDLOOP.

ENDFORM.                               " DATA_ELEMENT_INFO

*&---------------------------------------------------------------------*
*&      Form  DOMAIN_INFO
*&---------------------------------------------------------------------*
FORM DOMAIN_INFO.

  REFRESH: ITAB_DOM.
  SELECT * INTO TABLE ITAB_DOM
           FROM DD01L
          WHERE DOMNAME = ITAB_ELE-DOMNAME.

  CHECK SY-SUBRC = 0.
  LOOP AT ITAB_DOM.
    WRITE:/ ITAB_DOM.
  ENDLOOP.

ENDFORM.                               " DOMAIN_INFO
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.


  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = p_file
            filetype = 'WK1'
       TABLES
            data_tab = ITAB_TAB.
  write:/ p_file, ' is created...'.
ENDFORM.                    " data_download
