*&---------------------------------------------------------------------*
*& INCLUDE ZRIMSORTCOM .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 Report Sort를 위한 Include                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.28                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

* SORT 용 변?
DATA: SORT_KEY_REQ(40), SORT_DIR_REQ.
DATA: SORT_KEY_JOB(40), SORT_DIR_JOB.
DATA: SORT_KEY_PRT(40), SORT_DIR_PRT.
DATA: MARKFIELD, MARKFIELD1.
DATA: W_MARK.

FIELD-SYMBOLS: <SORT_KEY>, <SORT_DIR>, <SORT_FIELD>.

*-----------------------------------------------------------------------
* SORT 용 매크?
*-----------------------------------------------------------------------
DEFINE CONTROLLED_SORT.
  IF <SORT_KEY> IS INITIAL.
    SORT &1 &2 &3 &4.
  ELSE.
    IF <SORT_DIR> = 'U'.
      SORT &1 ASCENDING BY (<SORT_KEY>).
    ELSE.
      SORT &1 DESCENDING BY (<SORT_KEY>).
    ENDIF.
  ENDIF.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_SORT
*&---------------------------------------------------------------------*
FORM HANDLE_SORT TABLES   IT_TAB
                 USING    FCODE.

  CASE FCODE.
    WHEN 'STUP'.
       PERFORM SELECT_SORT_KEY TABLES IT_TAB
                               USING 'U'.
    WHEN 'STDN'.
       PERFORM SELECT_SORT_KEY TABLES IT_TAB
                               USING 'D'.
  ENDCASE.
ENDFORM.                    " HANDLE_SORT

*&---------------------------------------------------------------------*
*&      Form  SELECT_SORT_KEY
*&---------------------------------------------------------------------*
FORM SELECT_SORT_KEY TABLES IT_TAB
                     USING DIR.

  DATA: F LIKE SORT_KEY_REQ.
  FIELD-SYMBOLS: <C>.

  DATA: LEN TYPE I, L TYPE I.

  CLEAR F. GET CURSOR FIELD F.

  DESCRIBE FIELD F LENGTH LEN.

  IF F EQ SY-ULINE OR F EQ SY-VLINE OR F(6) NE 'IT_TAB'.
     EXIT.
  ENDIF.

  L = 0.
  WHILE L < LEN.
    ASSIGN F+L(1) TO <C>.
    IF <C> = '-'.
      L = L + 1.
      LEN = LEN - L.
      F = F+L(LEN).
      EXIT.
    ENDIF.
    L = L + 1.
  ENDWHILE.
  PERFORM UPDATE_SORT_KEY TABLES IT_TAB
                          USING  DIR F.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SORT_KEY
*&---------------------------------------------------------------------*
FORM UPDATE_SORT_KEY TABLES IT_TAB
                     USING DIR KEY.

  IF KEY IS INITIAL.  CLEAR KEY. ENDIF.

  PERFORM SET_SORT_KEY.

  IF NOT KEY IS INITIAL AND ( KEY <> <SORT_KEY> OR DIR <> <SORT_DIR> ).
    IF KEY NE 'MARKFIELD'.
       <SORT_DIR> = DIR.           " 정렬방?
       <SORT_KEY> = KEY.           " 필드?
       PERFORM SORT_LIST   TABLES   IT_TAB.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_SORT_KEY
*&---------------------------------------------------------------------*
FORM SET_SORT_KEY.

   ASSIGN SORT_KEY_REQ TO <SORT_KEY>.
   ASSIGN SORT_DIR_REQ TO <SORT_DIR>.

ENDFORM.                    " SET_SORT_KEY
*&---------------------------------------------------------------------*
*&      Form  SORT_LIST
*&---------------------------------------------------------------------*
FORM SORT_LIST    TABLES   IT_TAB.

  PERFORM SET_SORT_KEY.
  CONTROLLED_SORT IT_TAB BY <SORT_FIELD> DESCENDING.
  PERFORM RESET_LIST.

ENDFORM.                    " SORT_LIST
*&---------------------------------------------------------------------
*&      Form  P3000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_PC_DOWNLOAD.

   CALL FUNCTION 'DOWNLOAD'
        EXPORTING
        FILENAME = 'C:\TEMP.xls'
        FILETYPE = 'WK1'
   TABLES
       DATA_TAB = IT_TAB.

ENDFORM.                    " P3000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_RECORD
*&---------------------------------------------------------------------*
FORM P2000_SELECT_RECORD USING    P_SY_UCOMM.
DATA : WL_MARK.

   IF P_SY_UCOMM EQ 'MKAL'.
      WL_MARK = 'X'.
   ELSEIF P_SY_UCOMM EQ 'MKLO'.
      CLEAR : WL_MARK.
   ENDIF.
   DO.
      CLEAR MARKFIELD.
      READ LINE SY-INDEX FIELD VALUE MARKFIELD.
      IF SY-SUBRC NE 0.    EXIT.   ENDIF.
      MODIFY CURRENT LINE FIELD VALUE MARKFIELD FROM WL_MARK.
   ENDDO.

ENDFORM.
