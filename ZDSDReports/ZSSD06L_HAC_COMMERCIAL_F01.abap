*----------------------------------------------------------------------*
*   INCLUDE ZSSD05L_HMA_COMMERCIAL                                     *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH : G_TC_9000_ITAB.
  CLEAR   : G_TC_9000_ITAB.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE G_TC_9000_ITAB
         FROM VBRK AS K INNER JOIN VBRP AS P
           ON K~VBELN EQ P~VBELN
        WHERE K~FKART EQ 'ZVF2'
        AND   K~VKORG EQ 'E100' "HAC
        AND   K~FKDAT IN S_FKDAT.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
FORM CALL_SCREEN.
  DESCRIBE TABLE G_TC_9000_ITAB LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
    STOP.
  ENDIF.

  CALL SCREEN 9000.
ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PRIN
*&---------------------------------------------------------------------*
FORM PROCESS_PRIN.
  REFRESH : IT_LIST.
  CLEAR   : IT_LIST.

  CLEAR : W_CNT, W_TOT_FKIMG, W_TOT_NETWR.
  LOOP AT   G_TC_9000_ITAB
       INTO G_TC_9000_WA
       WHERE FLAG = 'X'.
    W_CNT = W_CNT + 1.

    MOVE-CORRESPONDING G_TC_9000_WA TO IT_LIST.
    COLLECT IT_LIST.

    W_TOT_FKIMG = W_TOT_FKIMG + G_TC_9000_WA-FKIMG.
    W_TOT_NETWR = W_TOT_NETWR + G_TC_9000_WA-NETWR.
  ENDLOOP.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M02.
    EXIT.
  ENDIF.

  PERFORM EXCEL_FILE_OPEN_CAL.  " EXCEL FILE OPEN
  PERFORM HEADER_DATA_CAL.      " HEADER
  PERFORM END_EXCEL_CAL.        " EXCEL FILE DISPLAY
ENDFORM.                    " PROCESS_PRIN
*&---------------------------------------------------------------------*
*&      Form  EXCEL_FILE_OPEN_CAL
*&---------------------------------------------------------------------*
FORM EXCEL_FILE_OPEN_CAL.
  CREATE OBJECT EXCEL 'EXCEL.APPLICATION'.
  CALL METHOD OF EXCEL 'WORKBOOKS' = BOOKS.

  CALL METHOD OF BOOKS 'OPEN'
       EXPORTING #1 = GV_FILENAME.            "Filename declaration

  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH TEXT-M03.
  ENDIF.
ENDFORM.                    " EXCEL_FILE_OPEN_CAL
*&---------------------------------------------------------------------*
*&      Form  END_EXCEL_CAL
*&---------------------------------------------------------------------*
FORM END_EXCEL_CAL.
  CALL METHOD OF EXCEL 'Worksheets' = SHEET
                  EXPORTING #1 = 1.

  CALL METHOD OF SHEET 'Activate'.

* IF SY-UCOMM EQ 'EXCEL'.
  SET PROPERTY OF EXCEL 'VISIBLE' = 1.
* ENDIF.

*--Execl Close
*    CALL METHOD OF EXCEL 'QUIT'.
    FREE OBJECT CELL.
    FREE OBJECT SHEET.
    FREE OBJECT EXCEL.
    FREE OBJECT SHEET.
ENDFORM.                    " END_EXCEL_CAL
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA_CAL
*&---------------------------------------------------------------------*
FORM FILL_DATA_CAL USING    I J VAL.
  CALL METHOD OF EXCEL 'CELLS' = CELL
      EXPORTING #1 = I #2 = J.

  SET PROPERTY OF CELL 'VALUE' = VAL.
ENDFORM.                    " FILL_DATA_CAL
*&---------------------------------------------------------------------*
*&      Form  HEADER_DATA_CAL
*&---------------------------------------------------------------------*
FORM HEADER_DATA_CAL.
  DATA : XX TYPE I.

* FISRT PAGE
  PERFORM FILL_DATA_CAL USING 41   3 W_TOT_FKIMG.
  PERFORM FILL_DATA_CAL USING 41   6 W_TOT_NETWR.
  PERFORM FILL_DATA_CAL USING 42   3 W_TOT_FKIMG.
  PERFORM FILL_DATA_CAL USING 42   6 W_TOT_NETWR.

* NEXT PAGE
  XX = 53.
  LOOP AT IT_LIST.
    PERFORM FILL_DATA_CAL USING XX   1 IT_LIST-ARKTX.
    PERFORM FILL_DATA_CAL USING XX   2 IT_LIST-MATNR.
    PERFORM FILL_DATA_CAL USING XX   3 IT_LIST-FKIMG.
    PERFORM FILL_DATA_CAL USING XX   4 IT_LIST-VRKME.
*    PERFORM FILL_DATA_CAL USING XX   5 IT_LIST-.
    PERFORM FILL_DATA_CAL USING XX   6 IT_LIST-NETWR.
    XX = XX + 1.
  ENDLOOP.

  PERFORM FILL_LINE_L USING XX.
  XX = XX + 1.

**  PERFORM FILL_DATA_CAL USING XX   1 'Total'.
**  PERFORM FILL_DATA_CAL USING XX   3 W_TOT_FKIMG.
**  PERFORM FILL_DATA_CAL USING XX   4 'UNITS'.
**  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
**  PERFORM FILL_DATA_CAL USING XX   6 W_TOT_NETWR.
**  XX = XX + 1.
**
**  PERFORM FILL_DATA_CAL USING XX   2 'U. S. SOURCED VALUE'.
**  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
**  PERFORM FILL_DATA_CAL USING XX   6 '0.00'.
**  XX = XX + 1.
**
**  PERFORM FILL_DATA_CAL USING XX   2 'NON. U. S. SOURCED VALUE'.
**  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
**  PERFORM FILL_DATA_CAL USING XX   6 W_TOT_NETWR.
**  XX = XX + 1.
**
**  PERFORM FILL_LINE_S USING XX.
**  XX = XX + 1.
**
**  PERFORM FILL_DATA_CAL USING XX   2 'EXW TOTAL'.
**  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
**  PERFORM FILL_DATA_CAL USING XX   6 W_TOT_NETWR.
**  XX = XX + 1.

  PERFORM FILL_DATA_CAL USING XX   1 'EXW MONTGOMERY'.
  PERFORM FILL_DATA_CAL USING XX   3 W_TOT_FKIMG.
  PERFORM FILL_DATA_CAL USING XX   4 'UNITS'.
  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
  PERFORM FILL_DATA_CAL USING XX   6 W_TOT_NETWR.
  XX = XX + 1.

  PERFORM FILL_LINE_L USING XX.
  XX = XX + 1.

  PERFORM FILL_DATA_CAL USING XX   1 'Total'.
  PERFORM FILL_DATA_CAL USING XX   3 W_TOT_FKIMG.
  PERFORM FILL_DATA_CAL USING XX   4 'UNITS'.
  PERFORM FILL_DATA_CAL USING XX   5 'US$'.
  PERFORM FILL_DATA_CAL USING XX   6 W_TOT_NETWR.
ENDFORM.                    " HEADER_DATA_CAL
*&---------------------------------------------------------------------*
*&      Form  FILL_LINE_L
*&---------------------------------------------------------------------*
FORM FILL_LINE_L USING XX.
  PERFORM FILL_DATA_CAL USING XX   1 W_UNDER1.
  PERFORM FILL_DATA_CAL USING XX   2 W_UNDER2.
  PERFORM FILL_DATA_CAL USING XX   3 W_UNDER3.
  PERFORM FILL_DATA_CAL USING XX   4 W_UNDER4.
  PERFORM FILL_DATA_CAL USING XX   5 W_UNDER5.
  PERFORM FILL_DATA_CAL USING XX   6 W_UNDER6.
ENDFORM.                    " FILL_LINE_L
*&---------------------------------------------------------------------*
*&      Form  FILL_LINE_S
*&---------------------------------------------------------------------*
FORM FILL_LINE_S USING XX.
  PERFORM FILL_DATA_CAL USING XX   2 W_UNDER2.
  PERFORM FILL_DATA_CAL USING XX   3 W_UNDER3.
  PERFORM FILL_DATA_CAL USING XX   4 W_UNDER4.
  PERFORM FILL_DATA_CAL USING XX   5 W_UNDER5.
  PERFORM FILL_DATA_CAL USING XX   6 W_UNDER6.
ENDFORM.                    " FILL_LINE_S















************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 FORM USER_OK_TC USING    P_TC_NAME TYPE DYNFNAM
                          P_TABLE_NAME
                          P_MARK_NAME
                 CHANGING P_OK      LIKE SY-UCOMM.

*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA: L_OK              TYPE SY-UCOMM,
         L_OFFSET          TYPE I.
*-END OF LOCAL DATA----------------------------------------------------*

* Table control specific operations                                    *
*   evaluate TC name and operations                                    *
   SEARCH P_OK FOR P_TC_NAME.
   IF SY-SUBRC <> 0.
     EXIT.
   ENDIF.
   L_OFFSET = STRLEN( P_TC_NAME ) + 1.
   L_OK = P_OK+L_OFFSET.
* execute general and TC specific operations                           *
   CASE L_OK.
     WHEN 'P--' OR                     "top of list
          'P-'  OR                     "previous page
          'P+'  OR                     "next page
          'P++'.                       "bottom of list
       PERFORM COMPUTE_SCROLLING_IN_TC USING P_TC_NAME
                                             L_OK.
       CLEAR P_OK.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     WHEN 'MARK'.                      "mark all filled lines
       PERFORM FCODE_TC_MARK_LINES USING P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME   .
       CLEAR P_OK.

     WHEN 'DMRK'.                      "demark all filled lines
       PERFORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                           P_TABLE_NAME
                                           P_MARK_NAME .
       CLEAR P_OK.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   ENDCASE.

 ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 FORM COMPUTE_SCROLLING_IN_TC USING    P_TC_NAME
                                       P_OK.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA L_TC_NEW_TOP_LINE     TYPE I.
   DATA L_TC_NAME             LIKE FELD-NAME.
   DATA L_TC_LINES_NAME       LIKE FELD-NAME.
   DATA L_TC_FIELD_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE cxtab_control.
   FIELD-SYMBOLS <LINES>      TYPE I.
*-END OF LOCAL DATA----------------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.
* get looplines of TableControl
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_TC_LINES_NAME.
   ASSIGN (L_TC_LINES_NAME) TO <LINES>.


* is no line filled?                                                   *
   IF <TC>-LINES = 0.
*   yes, ...                                                           *
     L_TC_NEW_TOP_LINE = 1.
   ELSE.
*   no, ...                                                            *
     CALL FUNCTION 'SCROLLING_IN_TABLE'
          EXPORTING
               ENTRY_ACT             = <TC>-TOP_LINE
               ENTRY_FROM            = 1
               ENTRY_TO              = <TC>-LINES
               LAST_PAGE_FULL        = 'X'
               LOOPS                 = <LINES>
               OK_CODE               = P_OK
               OVERLAPPING           = 'X'
          IMPORTING
               ENTRY_NEW             = L_TC_NEW_TOP_LINE
          EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               OTHERS                = 0.
   ENDIF.

* get actual tc and column                                             *
   GET CURSOR FIELD L_TC_FIELD_NAME
              AREA  L_TC_NAME.

   IF SYST-SUBRC = 0.
     IF L_TC_NAME = P_TC_NAME.
*     set actual column                                                *
       SET CURSOR FIELD L_TC_FIELD_NAME LINE 1.
     ENDIF.
   ENDIF.

* set the new top line                                                 *
   <TC>-TOP_LINE = L_TC_NEW_TOP_LINE.


 ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_MARK_LINES USING P_TC_NAME
                               P_TABLE_NAME
                               P_MARK_NAME.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* mark all filled lines                                                *
  LOOP AT <TABLE> ASSIGNING <WA>.

*   access to the component 'FLAG' of the table header                 *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM FCODE_TC_DEMARK_LINES USING P_TC_NAME
                                 P_TABLE_NAME
                                 P_MARK_NAME .
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  DATA L_TABLE_NAME       LIKE FELD-NAME.

  FIELD-SYMBOLS <TC>         TYPE cxtab_control.
  FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <WA>.
  FIELD-SYMBOLS <MARK_FIELD>.
*-END OF LOCAL DATA----------------------------------------------------*

  ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* demark all filled lines                                              *
  LOOP AT <TABLE> ASSIGNING <WA>.

*   access to the component 'FLAG' of the table header                 *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     <MARK_FIELD> = SPACE.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
