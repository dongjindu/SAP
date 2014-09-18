*----------------------------------------------------------------------*
*   INCLUDE ZMRF_RECIPIENT_BARCODEF01                                  *
*----------------------------------------------------------------------*

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
     WHEN 'INSR'.                      "insert row
       PERFORM FCODE_INSERT_ROW USING    P_TC_NAME
                                         P_TABLE_NAME.
       CLEAR P_OK.

     WHEN 'DELE'.                      "delete row
       PERFORM FCODE_DELETE_ROW USING    P_TC_NAME
                                         P_TABLE_NAME
                                         P_MARK_NAME.
       CLEAR P_OK.

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
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_INSERT_ROW
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME             .

*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA L_LINES_NAME       LIKE FELD-NAME.
   DATA L_SELLINE          LIKE SY-STEPL.
   DATA L_LASTLINE         TYPE I.
   DATA L_LINE             TYPE I.
   DATA L_TABLE_NAME       LIKE FELD-NAME.
   FIELD-SYMBOLS <TC>                 TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>              TYPE STANDARD TABLE.
   FIELD-SYMBOLS <LINES>              TYPE I.
*-END OF LOCAL DATA----------------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* get looplines of TableControl
   CONCATENATE 'G_' P_TC_NAME '_LINES' INTO L_LINES_NAME.
   ASSIGN (L_LINES_NAME) TO <LINES>.

* get current line
   GET CURSOR LINE L_SELLINE.
   IF SY-SUBRC <> 0.                   " append line to table
     L_SELLINE = <TC>-LINES + 1.
*&SPWIZARD: set top line and new cursor line                           *
     IF L_SELLINE > <LINES>.
       <TC>-TOP_LINE = L_SELLINE - <LINES> + 1 .
     ELSE.
       <TC>-TOP_LINE = 1.
     ENDIF.
   ELSE.                               " insert line into table
     L_SELLINE = <TC>-TOP_LINE + L_SELLINE - 1.
     L_LASTLINE = <TC>-TOP_LINE + <LINES> - 1.
   ENDIF.
*&SPWIZARD: set new cursor line                                        *
   L_LINE = L_SELLINE - <TC>-TOP_LINE + 1.
* insert initial line
   INSERT INITIAL LINE INTO <TABLE> INDEX L_SELLINE.
   <TC>-LINES = <TC>-LINES + 1.
* set cursor
   SET CURSOR LINE L_LINE.

 ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
 FORM FCODE_DELETE_ROW
               USING    P_TC_NAME           TYPE DYNFNAM
                        P_TABLE_NAME
                        P_MARK_NAME   .

*-BEGIN OF LOCAL DATA--------------------------------------------------*
   DATA L_TABLE_NAME       LIKE FELD-NAME.

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
   FIELD-SYMBOLS <TABLE>      TYPE STANDARD TABLE.
   FIELD-SYMBOLS <WA>.
   FIELD-SYMBOLS <MARK_FIELD>.
   DATA: LT_RECP TYPE ZTRF_RECIPIENT OCCURS 0 WITH HEADER LINE.
*-END OF LOCAL DATA----------------------------------------------------*

   ASSIGN (P_TC_NAME) TO <TC>.

* get the table, which belongs to the tc                               *
   CONCATENATE P_TABLE_NAME '[]' INTO L_TABLE_NAME. "table body
   ASSIGN (L_TABLE_NAME) TO <TABLE>.                "not headerline

* delete marked lines                                                  *
   DESCRIBE TABLE <TABLE> LINES <TC>-LINES.

   LOOP AT G_TC_RECP_ITAB INTO G_TC_RECP_WA.
     IF G_TC_RECP_WA-FLAG EQ 'X'.
       MOVE-CORRESPONDING G_TC_RECP_WA TO LT_RECP.
       APPEND LT_RECP. CLEAR LT_RECP.
     ENDIF.
   ENDLOOP.
   LOOP AT <TABLE> ASSIGNING <WA>.

*   access to the component 'FLAG' of the table header                 *
     ASSIGN COMPONENT P_MARK_NAME OF STRUCTURE <WA> TO <MARK_FIELD>.

     IF <MARK_FIELD> = 'X'.
       DELETE <TABLE> INDEX SYST-TABIX.
       <TC>-LINES = <TC>-LINES - 1.
     ENDIF.
   ENDLOOP.
   DELETE ZTRF_RECIPIENT FROM TABLE LT_RECP.
   IF SY-SUBRC EQ 0.
     MESSAGE S000 WITH 'Delete success'.
     COMMIT WORK.
   ELSE.
     ROLLBACK WORK.
     MESSAGE S000 WITH 'Delete error'.
   ENDIF.

 ENDFORM.                              " FCODE_DELETE_ROW

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

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
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

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
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

   FIELD-SYMBOLS <TC>         TYPE CXTAB_CONTROL.
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
*&---------------------------------------------------------------------*
*&      Form  PRINT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM PRINT_PROCESS.
   DATA: L_PERNR(10),
         L_WEMPF(16),
         L_COUNT TYPE I.
   DATA: LEN  TYPE I,
         LEN1 TYPE I,
         LEN2 TYPE I.
   LOOP AT G_TC_RECP_ITAB INTO G_TC_RECP_WA.
     IF G_TC_RECP_WA-FLAG EQ 'X'.
       L_COUNT = L_COUNT + 1.
       NEW-PAGE PRINT ON
          NO-TITLE
          NO-HEADING
          LINE-SIZE 255
          DESTINATION 'RFL'
          IMMEDIATELY 'X'
          KEEP IN SPOOL 'X'
          NEW LIST IDENTIFICATION 'X'
          NO DIALOG.
       CONCATENATE '*' G_TC_RECP_WA-PERNR '*' INTO L_PERNR .
       LEN  = STRLEN( L_PERNR ).

       LEN1 = STRLEN( G_TC_RECP_WA-WEMPF ).
       L_WEMPF = G_TC_RECP_WA-WEMPF.
       LEN2 = 16 - LEN1.
       CLEAR LEN1.
       IF LEN2 NE 0.
         LEN1 = LEN2 DIV 2 .
       ENDIF.
       DO LEN1 TIMES.
         CONCATENATE ' ' L_WEMPF INTO
                         L_WEMPF SEPARATED BY SPACE.
       ENDDO.

       WRITE: / '^XA' NO-GAP,
                '^FO' NO-GAP,
                  '40' NO-GAP,  " X
                  ',' NO-GAP,
*                  '80' NO-GAP,   " Y
                  '40' NO-GAP,   " Y

                '^BY' NO-GAP,
                  '2.5' NO-GAP,  "
                  ',' NO-GAP,
                  '1.5' NO-GAP,  "

                  ',' NO-GAP,
                  '100' NO-GAP,  "BAR

                 '^AB' NO-GAP,
                   'N' NO-GAP,
                   ',' NO-GAP,
                   '10' NO-GAP,
                   ',' NO-GAP,
                   '10' NO-GAP,

                 '^B3N' NO-GAP,  " N"
                   ',' NO-GAP,
                   'N' NO-GAP,   "MODULE 43 CHECK DIGIT
                   ',' NO-GAP,
                   '80' NO-GAP,  "BAR
                   ',' NO-GAP,
                   'N' NO-GAP,    "BAR
                   ',' NO-GAP,
                   'N' NO-GAP,    "BAR
                 '^FD' NO-GAP,
                   L_PERNR(LEN) NO-GAP,
                 '^FS' NO-GAP,
* DESCRIPTION WRITE
              /  '^FO' NO-GAP,
                   '40' NO-GAP,
                   ',' NO-GAP,
*                   '180' NO-GAP,
                   '140' NO-GAP,

                 '^AE' NO-GAP,
                   'N' NO-GAP,
                   ',' NO-GAP,
                   '8' NO-GAP,
                   ',' NO-GAP,
                   '8' NO-GAP,

                  '^FD' NO-GAP,
                    L_WEMPF,
                  '^FS' NO-GAP,

                / '^XZ' NO-GAP.
       NEW-PAGE PRINT OFF.
     ENDIF.
   ENDLOOP.
   IF L_COUNT GT 0.
     MESSAGE S000 WITH 'Persornel number barcode printing.'.
   ENDIF.
 ENDFORM.                    " PRINT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  NEW_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM NEW_LINE.
   REFRESH G_TC_RECP_ITAB.  CLEAR G_TC_RECP_WA.
   DO 21 TIMES.
     APPEND INITIAL LINE TO G_TC_RECP_ITAB .
     CHANGE_MODE = 'X'.
   ENDDO.
   TC_RECP-TOP_LINE = 1.
 ENDFORM.                    " NEW_LINE
*&---------------------------------------------------------------------*
*&      Form  DISP_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM DISP_MODE.
   CLEAR G_TC_RECP_COPIED. CLEAR CHANGE_MODE.

 ENDFORM.                    " DISP_MODE
*&---------------------------------------------------------------------*
*&      Form  SAVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM SAVE_PROCESS.
   DATA L_TABIX TYPE SY-TABIX.
   DATA: LT_RECP TYPE ZTRF_RECIPIENT OCCURS 0 WITH HEADER LINE.
   CHECK NOT G_TC_RECP_ITAB[] IS INITIAL.
   DELETE G_TC_RECP_ITAB WHERE PERNR IS INITIAL.
   SELECT *
        FROM ZTRF_RECIPIENT
        INTO TABLE IT_RECP.
   IF SY-SUBRC EQ 0.
     SORT IT_RECP BY PERNR.
     LOOP AT G_TC_RECP_ITAB INTO G_TC_RECP_WA.
       READ TABLE IT_RECP WITH KEY PERNR = G_TC_RECP_WA-PERNR
                          BINARY SEARCH.
       IF SY-SUBRC EQ 0.
         L_TABIX = SY-TABIX.
         IT_RECP-WEMPF = G_TC_RECP_WA-WEMPF.
         MODIFY IT_RECP INDEX L_TABIX TRANSPORTING WEMPF.
       ELSE.
         MOVE-CORRESPONDING G_TC_RECP_WA TO LT_RECP.
         LT_RECP-MANDT = SY-MANDT.
         LT_RECP-PASSW = 'HMMA'.
         APPEND LT_RECP.
*         SORT LT_RECP BY PERNR.
       ENDIF.
       CLEAR IT_RECP. CLEAR LT_RECP.
     ENDLOOP.
   ELSE.
     LOOP AT G_TC_RECP_ITAB INTO G_TC_RECP_WA.
       MOVE-CORRESPONDING G_TC_RECP_WA TO LT_RECP.
       LT_RECP-PASSW = 'HMMA'.
       LT_RECP-MANDT = SY-MANDT.
       APPEND LT_RECP.
     ENDLOOP.
   ENDIF.
   IF NOT IT_RECP[] IS INITIAL.
     UPDATE ZTRF_RECIPIENT CLIENT SPECIFIED FROM TABLE IT_RECP . "
     IF SY-SUBRC EQ 0.
       COMMIT WORK.
       MESSAGE S000 WITH 'DATA SAVE OK!!'.
       CLEAR G_TC_RECP_COPIED. CLEAR CHANGE_MODE.
       REFRESH IT_RECP. CLEAR IT_RECP.
     ELSE.
       ROLLBACK WORK.
       MESSAGE E000 WITH 'DATA SAVE ERROR'.
       REFRESH IT_RECP. CLEAR IT_RECP.
     ENDIF.
   ENDIF.
   IF NOT LT_RECP[] IS INITIAL.
     INSERT ZTRF_RECIPIENT CLIENT SPECIFIED FROM TABLE LT_RECP . "
     IF SY-SUBRC EQ 0.
       COMMIT WORK.
       MESSAGE S000 WITH 'DATA SAVE OK!!'.
       CLEAR G_TC_RECP_COPIED. CLEAR CHANGE_MODE.
       REFRESH LT_RECP. CLEAR LT_RECP.
     ELSE.
       ROLLBACK WORK.
       MESSAGE E000 WITH 'DATA SAVE ERROR'.
       REFRESH LT_RECP. CLEAR LT_RECP.
     ENDIF.
   ENDIF.
 ENDFORM.                    " SAVE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  ENTER_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM ENTER_PROCESS.
   DATA L_LINES TYPE I.
   IF CHANGE_MODE EQ 'X'.
     GET CURSOR LINE L_LINES.
     DELETE G_TC_RECP_ITAB WHERE PERNR IS INITIAL.
*     DESCRIBE TABLE G_TC_RECP_ITAB LINES L_LINES.
     TC_RECP-TOP_LINE = L_LINES.
     DO 21 TIMES.
       APPEND INITIAL LINE TO G_TC_RECP_ITAB .
       CHANGE_MODE = 'X'.
     ENDDO.
   ENDIF.
 ENDFORM.                    " ENTER_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_INSERT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM DATA_INSERT_CHECK.
   DATA: L_PERNR TYPE ZTRF_RECIPIENT-PERNR.
   MOVE-CORRESPONDING ZTRF_RECIPIENT TO G_TC_RECP_WA.
   SELECT SINGLE PERNR
               FROM ZTRF_RECIPIENT
               INTO L_PERNR
               WHERE PERNR EQ G_TC_RECP_WA-PERNR.
   IF SY-SUBRC EQ 0.
     DELETE G_TC_RECP_ITAB INDEX TC_RECP-CURRENT_LINE.
     MESSAGE E001 WITH 'An entry already exists with the same key'.
*         INTO G_TC_RECP_ITAB INDEX TC_RECP-CURRENT_LINE.
   ELSE.
     IF NOT G_TC_RECP_WA IS INITIAL.
       READ TABLE G_TC_RECP_ITAB INTO  G_TC_RECP_WA WITH KEY
                                        PERNR = G_TC_RECP_WA.
       IF SY-SUBRC EQ 0.
         DELETE G_TC_RECP_ITAB INDEX TC_RECP-CURRENT_LINE.
         MESSAGE E001 WITH 'An entry already exists with the same key'.
       ELSE.
         INSERT G_TC_RECP_WA
           INTO G_TC_RECP_ITAB INDEX TC_RECP-CURRENT_LINE.
       ENDIF.
     ENDIF.
   ENDIF.
 ENDFORM.                    " DATA_INSERT_CHECK
