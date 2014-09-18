*&---------------------------------------------------------------------*
*& INCLUDE ZRIMOLECOM.
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Utility function Include                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.10.25                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
INCLUDE OLE2INCL.

DATA: STANDARDFONT TYPE OLE2_OBJECT.
DATA: EXCEL        TYPE OLE2_OBJECT.
DATA: BOOKS        TYPE OLE2_OBJECT.
DATA: BOOK         TYPE OLE2_OBJECT.
DATA: CELL         TYPE OLE2_OBJECT.
DATA: CELL1        TYPE OLE2_OBJECT.
DATA: CELL2        TYPE OLE2_OBJECT.
DATA: FONT         TYPE OLE2_OBJECT.
DATA: SFONT        TYPE OLE2_OBJECT.
DATA: SMARGE       TYPE OLE2_OBJECT.
DATA: RANGE        TYPE OLE2_OBJECT.
DATA: SELECT       TYPE OLE2_OBJECT.
DATA: SELECTION    TYPE OLE2_OBJECT.
DATA: ACTIVESHEET  TYPE OLE2_OBJECT.
DATA: SAVEAS       TYPE OLE2_OBJECT.
DATA: SWRAPTEXT    TYPE OLE2_OBJECT.
DATA: APPLICATION  TYPE OLE2_OBJECT.
DATA: COLUMN       TYPE OLE2_OBJECT.
DATA: ROW          TYPE OLE2_OBJECT.
DATA: BORDERS      TYPE OLE2_OBJECT.
DATA: BORDERS1     TYPE OLE2_OBJECT.
DATA: BORDERS2     TYPE OLE2_OBJECT.
DATA: BORDERS3     TYPE OLE2_OBJECT.
DATA: ITEM         TYPE OLE2_OBJECT.

DATA: I     TYPE I VALUE '4',
      J     TYPE I,
      K     TYPE I,
      L     TYPE I,
      M     TYPE I.


*&---------------------------------------------------------------------*
*&      Form  P2000_EXCEL_INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0245   text
*      -->P_10     text
*----------------------------------------------------------------------*
FORM P2000_EXCEL_INITIAL USING    P_FONT
                                  P_FONTSIZE.

*>>Create Object , init setting
    IF NOT EXCEL IS INITIAL.
        FREE OBJECT EXCEL.
    ENDIF.

    CREATE OBJECT EXCEL 'EXCEL.APPLICATION'.
    CALL METHOD OF EXCEL 'WORKBOOKS' =  BOOKS.
    CALL METHOD OF BOOKS 'ADD'       =  BOOK.

    CALL METHOD OF EXCEL 'APPLICATION'             = APPLICATION.
    SET PROPERTY OF APPLICATION 'STANDARDFONT'     = P_FONT.
    SET PROPERTY OF APPLICATION 'STANDARDFONTSIZE' = P_FONTSIZE.

*    SET PROPERTY OF EXCEL 'VISIBLE' = 1.

ENDFORM.                    " P2000_EXCEL_INITIAL
*&---------------------------------------------------------------------*
*&      Form  P2000_FIT_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_10     text
*      -->P_0252   text
*----------------------------------------------------------------------*
FORM P2000_FIT_CELL USING      P_COL1
                             " P_ROW1
                               P_WIDTH
                              "P_HEIGHT
                               P_GUBUN.

  CALL METHOD OF EXCEL 'APPLICATION' = APPLICATION.
  IF P_GUBUN EQ 'C'.
    CALL METHOD OF APPLICATION 'CELLS' = CELL EXPORTING   #1 = P_COL1.
    SET PROPERTY OF CELL 'COLUMNWIDTH' = P_WIDTH.
  ENDIF.
  IF P_GUBUN EQ 'R'.
    CALL METHOD OF APPLICATION 'CELLS' = CELL EXPORTING   #1 = P_COL1.
    SET PROPERTY OF CELL 'ROWHEIGHT' = P_WIDTH. "HEIGHT.
  ENDIF.

ENDFORM.                    " P2000_FIT_CELL
*&---------------------------------------------------------------------*
*&      Form  P2000_FILL_CELL_FONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3      text
*      -->P_3      text
*      -->P_0283   text
*      -->P_18     text
*----------------------------------------------------------------------*
FORM P2000_FILL_CELL_FONT USING    I
                                   J
                                   P_GUBUN
                                   P_VALUE.

  CALL METHOD OF EXCEL 'CELLS' = CELL EXPORTING   #1 = I  #2 = J.
  CALL METHOD OF CELL 'FONT'   = SFONT.
  SET PROPERTY OF SFONT 'SIZE' = P_VALUE.

 CASE P_GUBUN.
    WHEN 'B'.
       SET PROPERTY OF SFONT 'FONTSTYLE' = '굵게'.
    WHEN OTHERS.
 ENDCASE.

ENDFORM.                    " P2000_FILL_CELL_FONT
*&---------------------------------------------------------------------*
*&      Form  P2000_FILL_CELL_MERGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3      text
*      -->P_3      text
*      -->P_3      text
*      -->P_4      text
*      -->P_0285   text
*----------------------------------------------------------------------*
FORM P2000_FILL_CELL_MERGE USING    P_START
                                    P_I
                                    P_VAL1
                                    P_VAL2
                                    P_VALUE.

    CALL METHOD OF EXCEL 'CELLS' = CELL1
         EXPORTING   #1 = P_START  #2 = P_VAL1.

    CALL METHOD OF EXCEL 'CELLS' = CELL2
         EXPORTING   #1 = P_I  #2 = P_VAL2.

    CALL METHOD OF APPLICATION 'RANGE'
       = RANGE EXPORTING #1 = CELL1 #2 = CELL2.
    CALL METHOD OF RANGE 'SELECT'.

    CALL METHOD OF APPLICATION 'SELECTION' = SELECTION.
    CALL METHOD OF SELECTION 'MERGE'.

*    SET PROPERTY OF SELECTION 'WRAPTEXT' = 1.

    SET PROPERTY OF SELECTION 'VALUE' = P_VALUE.
    SET PROPERTY OF SELECTION 'HorizontalAlignment' = '2'.
    SET PROPERTY OF SELECTION 'VerticalAlignment' = '1'.
*    SET PROPERTY OF SELECTION 'ColumnWidth' = '20'.
*    SET PROPERTY OF SELECTION 'RowHeight' = '10ENDFORM.

ENDFORM.   " P2000_FILL_CELL_MERGE


*&---------------------------------------------------------------------*
*&      Form  P2000_FILL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5      text
*      -->P_END  text
*      -->P_1      text
*      -->P_3      text
*      -->P_2      text
*----------------------------------------------------------------------*
FORM P2000_FILL_LINE USING    P_SCOL
                              P_ECOL
                              P_SROW
                              P_EROW
                          "   P_SEL                 "형태
                              P_WID.                "선종류.

    CALL METHOD OF EXCEL 'CELLS' = CELL1
         EXPORTING   #1 = P_SCOL  #2 = P_SROW.

    CALL METHOD OF EXCEL 'CELLS' = CELL2
         EXPORTING   #1 = P_ECOL   #2 = P_EROW.

    CALL METHOD OF APPLICATION 'RANGE'
       = RANGE EXPORTING #1 = CELL1 #2 = CELL2.
    CALL METHOD OF RANGE 'SELECT'.

    CALL METHOD OF APPLICATION 'SELECTION' = SELECTION.
    CALL METHOD OF SELECTION 'BORDERS' = BORDERS EXPORTING #1 = 7.
    SET PROPERTY OF BORDERS 'LINESTYLE' = P_WID.

    CALL METHOD OF SELECTION 'BORDERS' = BORDERS1 EXPORTING #2 = 8.
    SET PROPERTY OF BORDERS1 'LINESTYLE' = P_WID.

    CALL METHOD OF SELECTION 'BORDERS' = BORDERS2 EXPORTING #3 = 9.
    SET PROPERTY OF BORDERS2 'LINESTYLE' = P_WID.

    CALL METHOD OF SELECTION 'BORDERS' = BORDERS3 EXPORTING #4 = 10.
    SET PROPERTY OF BORDERS3 'LINESTYLE' = P_WID.

ENDFORM.                    " P2000_FILL_LINE
*&---------------------------------------------------------------------*
*&      Form  P2000_CONCATNATE_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DOWN_G1_CELL132  text
*      -->P_W_SHIP_CNT  text
*----------------------------------------------------------------------*
FORM P2000_CONCATNATE_CELL USING
                     P_IT_DOWN_G1-CELL132
                     P_MAX_CNT TYPE I.

*  IF W_TABIX EQ P_MAX_CNT.
*     MOVE P_IT_DOWN_G1-CELL132  TO W_WRAPTEXT1.
*  ELSE.
*     CONCATENATE P_IT_DOWN_G1-CELL132 IT_TAB-CR_LF INTO W_WRAPTEXT1.
*  ENDIF.
*  IF W_WRAPTEXT2 IS INITIAL.
*     MOVE   W_WRAPTEXT1 TO W_WRAPTEXT2.
*  ELSE.
*     CONCATENATE  W_WRAPTEXT2 W_WRAPTEXT1 INTO W_WRAPTEXT2.
*  ENDIF.
*
**  SHIFT IT_TAB-CELL132 RIGHT DELETING TRAILING SPACE.
**  SHIFT IT_TAB-CELL132 LEFT DELETING LEADING SPACE.
**  CONCATENATE IT_TAB-CELL132 W_RETURN INTO W_WRAPTEXT1.
**  SHIFT W_WRAPTEXT1 RIGHT DELETING TRAILING SPACE.
**  SHIFT W_WRAPTEXT1 LEFT DELETING LEADING SPACE.
**
**  IF W_WRAPTEXT2 IS INITIAL.
**     MOVE   W_WRAPTEXT1 TO W_WRAPTEXT2.
**  ELSE.
**     CONCATENATE  W_WRAPTEXT2 W_WRAPTEXT1 INTO W_WRAPTEXT2.
**  ENDIF.
*

ENDFORM.                    " P2000_CONCATNATE_CELL
*&---------------------------------------------------------------------*
*&      Form  FILL_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I  text
*      -->P_3      text
*      -->P_0283   text
*----------------------------------------------------------------------*
FORM P2000_FILL_CELL USING    I
                              J
                              P_VALUE.
    CALL METHOD OF EXCEL 'CELLS' = CELL EXPORTING   #1 = I  #2 = J.
    SET PROPERTY OF CELL 'VALUE' = P_VALUE.

*    CALL METHOD OF APPLICATION 'SELECTION' = SELECTION.
*    CALL METHOD OF SELECTION 'WRAPTEXT'.

ENDFORM.                    " P2000_FILL_CELL


*&---------------------------------------------------------------------*
*&      Form  SAVE_EXCEL
*&---------------------------------------------------------------------*
FORM P2000_SAVEAS_EXCEL  USING P_FILENAME.


    CALL METHOD OF APPLICATION 'ActiveSheet' = ACTIVESHEET.
    CALL METHOD OF ACTIVESHEET 'SAVEAS' =   SAVEAS
                                EXPORTING  #1 = P_FILENAME.

ENDFORM.                    " SAVE_EXCEL
