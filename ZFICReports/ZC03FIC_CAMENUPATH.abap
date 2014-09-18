REPORT ZC03FIC_CAMENUPATH NO STANDARD PAGE HEADING.
*Find the menu paths from a selected transaction code
*or description substring.
*by Andy Choi
*Date: 2003.04
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK entry WITH FRAME TITLE t_box.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  5(17) tlabel.
PARAMETERS tbutton RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 40(28) dlabel.
PARAMETERS dbutton RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 40(22) casens.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 40(8) llabel.
PARAMETERS limit(5) TYPE N DEFAULT 10.
SELECTION-SCREEN COMMENT 56(6) plabel.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(14) flabel.
PARAMETERS string(80) OBLIGATORY LOWER CASE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN END OF BLOCK entry.

INITIALIZATION.
  t_box  = ' Find the menu paths '.
  tlabel = 'Transaction Code'.
  dlabel = 'Menu description (sub)string'.
  casens = '(Case-sensitive)'.
  flabel = 'Search string:'.
  llabel = 'Limit to'.
  plabel = 'paths.'. TABLES: SMENSAPNEW, SMENSAPT. DATA: BEGIN OF
imensap OCCURS 20.
          INCLUDE STRUCTURE SMENSAPNEW.
  DATA  text LIKE SMENSAPT-TEXT.
  DATA  END OF imensap.
  DATA: isort LIKE SMENSAPNEW-SORT_ORDER,
        template(82).

START-OF-SELECTION.
  IF tbutton EQ 'X'. "Paths to Transaction code
    TRANSLATE string TO UPPER CASE.
    SELECT * FROM SMENSAPNEW
      WHERE REPORTTYPE = 'TR' AND REPORT = string
        AND CUSTOMIZED EQ 'S'.
      SELECT SINGLE * FROM SMENSAPT
        WHERE OBJECT_ID EQ SMENSAPNEW-OBJECT_ID AND SPRAS EQ 'E'.
      PERFORM Append_imensap.
      WHILE imensap-menu_level GT 1.
        PERFORM GetNextHigherLevel.
      ENDWHILE.
    ENDSELECT.
  ELSE. "Paths to menu description (sub)string
    CONCATENATE '%' string '%' INTO template.
    SELECT * FROM SMENSAPT UP TO limit ROWS
      WHERE TEXT LIKE template AND SPRAS EQ 'E'.
      SELECT SINGLE * FROM SMENSAPNEW
        WHERE CUSTOMIZED EQ 'S' AND OBJECT_ID EQ SMENSAPT-OBJECT_ID.
      IF SY-SUBRC EQ 0.
        PERFORM Append_imensap.
        WHILE imensap-menu_level GT 1.
          PERFORM GetNextHigherLevel.
        ENDWHILE.
      ELSE.
        CLEAR imensap.
        CONCATENATE SMENSAPT-TEXT
          '  (No menu path found for this description.)'
          INTO imensap-text.
        imensap-menu_level = 1.
        ADD 1 TO isort.
        imensap-sort_order = isort.
        APPEND imensap.
      ENDIF.
    ENDSELECT.
  ENDIF.

END-OF-SELECTION.
  PERFORM Write_Menu_Path.
*---------------------------------------------------------------------*
*       FORM Append_imensap                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM Append_imensap.
  MOVE-CORRESPONDING SMENSAPNEW TO imensap.
  IF SMENSAPNEW-REPORTTYPE = 'TR' OR  SMENSAPNEW-REPORTTYPE = 'MN'.
    CONCATENATE SMENSAPT-TEXT SMENSAPNEW-REPORT INTO imensap-text
      SEPARATED BY ' ===> '.
  ELSE.
    imensap-text = SMENSAPT-TEXT.
  ENDIF.
  ADD 1 TO isort.
  imensap-sort_order = isort.
  APPEND imensap.
ENDFORM.  "Append_imensap

*---------------------------------------------------------------------*
*       FORM GetNextHigherLevel                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GetNextHigherLevel.
  SELECT * FROM SMENSAPNEW WHERE OBJECT_ID EQ imensap-parent_id
    AND CUSTOMIZED EQ 'S'.
    SELECT SINGLE * FROM SMENSAPT
      WHERE OBJECT_ID EQ imensap-parent_id AND SPRAS EQ 'E'.
    PERFORM Append_imensap.
  ENDSELECT.
ENDFORM. "GetNextHigherLevel

*---------------------------------------------------------------------*
*       FORM Write_Menu_Path                                          *
*---------------------------------------------------------------------*
FORM Write_Menu_Path.
  DATA: nlines TYPE I, slen TYPE I, npath TYPE c, plen TYPE I,
      tlen  TYPE I, text  LIKE imensap-text,
      text1 LIKE imensap-text, text2 LIKE imensap-text.
  DESCRIBE TABLE imensap LINES nlines.
  slen = STRLEN( string ).
  IF nlines IS INITIAL.
    IF tbutton EQ 'X'.
      WRITE /'No menu path found for transaction code'.
      WRITE AT (slen) string INVERSE.
    ELSE.
      WRITE: /'No menu path found for description'.
      WRITE AT (slen) string INVERSE.
    ENDIF.
    EXIT.
  ENDIF.
  IF tbutton EQ 'X'.
    WRITE: /'Menu paths for the transaction code'.
    WRITE AT (slen) string INVERSE.
  ELSE.
    WRITE: /'Menu paths for description'.
    WRITE AT (slen) string INVERSE.
  ENDIF.
  SORT imensap BY sort_order DESCENDING.
  LOOP AT imensap.
    IF imensap-menu_level EQ 1.
      SKIP.
      ADD 1 TO npath. plen = STRLEN( npath ).
      WRITE / '--------' NO-GAP.
      WRITE AT: (plen) npath. WRITE '--------'.
    ENDIF.
    WRITE AT /imensap-menu_level ' '.
    text = imensap-text.
    REPLACE string LENGTH slen WITH '~~' INTO text.
    IF SY-SUBRC IS INITIAL.
      SPLIT text AT '~~' INTO text1 text2.
      tlen = STRLEN( text1 ).
      WRITE AT (tlen) text1 NO-GAP.
      WRITE AT (slen) string INVERSE NO-GAP.
      tlen = STRLEN( text2 ).
      WRITE AT (tlen) text2.
    ELSE.
      WRITE text.
    ENDIF.
  ENDLOOP.
ENDFORM. "Write_Menu_Path *============ End of report ============*
