*&---------------------------------------------------------------------*
*& Include MZIM04TOP                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 원장 관리 Main Data Define Include                *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2002.01.25                                            *
*&  적용회사PJT: Poong-San                                             *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* DESC : Table Define.
*-----------------------------------------------------------------------
TABLES: LFA1,                     " Vendor Master Table.
        SPOP,                     " POPUP_TO_CONFIRM.
        TCURC,                    " 통화코드.
        USR01,                    " 사용자마스터레코드 (실행시데이타).
        ZTREQHD,                  " 수입의뢰 Header Table.
        ZTBKPF,                   " 비용문서 Header Table.
        ZTBSEG,                   " 비용문서 Item Table.
        ZTIMIMG08,                " 관리코드 관리 Table.
        ZTPMTHD.                  " Payment Notice Header Table.

*-----------------------------------------------------------------------
* DESC : Structure Define.
*-----------------------------------------------------------------------
TABLES: ZSREQHD,                  " 수입의뢰 Header용 Structure.
        ZSBSEG,                   " 수입시스템 비용문서 Item Structure.
        ZSBSEG2,                  " 수입시스템 비용문서 Item Structure.
        ZSIMIMG08,                " 관리코드 Structure.
        ZSPMTHD.                  " Payment Notice Head Structure.

*-----------------------------------------------------------------------
*  DESC : View Define.
*-----------------------------------------------------------------------
TABLES: ZVREQHD_ST.               " 수입의뢰 Header + Status View.

DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

DATA: IT_ZSBSEG       LIKE ZSBSEG    OCCURS 0 WITH HEADER LINE.
DATA: IT_ZSBSEG1      LIKE ZSBSEG2   OCCURS 0 WITH HEADER LINE.
DATA: IT_ZSPMTHD      LIKE ZSPMTHD   OCCURS 0 WITH HEADER LINE.
DATA: IT_ZSIMIMG08    LIKE ZSIMIMG08 OCCURS 0 WITH HEADER LINE.

DATA: C_REQ_C   VALUE 'C',         " 생?
      C_REQ_U   VALUE 'U',         " 변?
      C_REQ_D   VALUE 'D',         " 조?
      C_ADD_U   VALUE 'A',         " 변경(추가)
      C_ADD_D   VALUE 'B',         " 조?
      C_OPEN_C  VALUE 'O',         " 확?
      C_OPEN_U  VALUE 'G',         " 확정 변경/상태변?
      C_OPEN_D  VALUE 'R',         " 확정 조?
      C_INSU_I  VALUE 'I',         " 보험관?
      C_BL_SEND VALUE 'S',        " 선적서류 송부.
      C_BL_COST VALUE 'T',        " 비용입력..
      C_BL_REAL VALUE 'E'.        " 실입항 입력.

DATA: W_PFSTAT(4)     TYPE C.             " PF-STATUS

FIELD-SYMBOLS : <FS_F>.
* Data Declaration.
DATA: W_CREATE(6)        TYPE     C     VALUE   'Create',
      W_CHANGE(6)        TYPE     C     VALUE   'Change',
      W_DISPLAY(7)       TYPE     C     VALUE   'Display',
      W_ADD_CHG(17)      TYPE     C     VALUE   'Additional Change',
      W_ADD_DIS(18)      TYPE     C     VALUE   'Additional Display',
      W_OPEN(7)          TYPE     C     VALUE   'Opennig',
      W_OPEN_CHANGE(11)  TYPE     C     VALUE   'Open Change',
      W_STAUTS(13)       TYPE     C     VALUE   'Status Change',
      W_OPEN_DISPLAY(12) TYPE     C     VALUE   'Open Display',
      W_INSURANCE(12)    TYPE     C     VALUE   'Insurance'.

* Data Declaration.
data: G_PARAM_LINE      LIKE  SY-TABIX.    " Table의 마지막.
DATA: CANCEL_OPTION     TYPE  C.           " 공통 popup Screen에서 사용.
DATA: W_STATUS          TYPE  C.           " MENU STATUS.
DATA: OK-CODE           LIKE  SY-UCOMM.    " OK-CODE
DATA: W_OK_CODE         LIKE  SY-UCOMM.    " OK-CODE
DATA: W_COUNT           TYPE  I.           " Line Count.
DATA: ANTWORT(1)        TYPE  C.           " 공통 popup Screen에서 사용.
data: OPTION(1)         TYPE  C.           " 공통 popup Screen에서 사용.
data: TEXTLEN           TYPE  I.           " 공통 popup Screen에서 사용.
DATA: W_ROW_MARK        TYPE  C.           " RECORD의 선택여부.
DATA: W_ROW_MARK1       TYPE  C.           " RECORD의 선택여부.
DATA: W_ROW_MARK2       TYPE  C.           " RECORD의 선택여부.
DATA: W_LOOPLINES       LIKE  SY-LOOPC.    " Loop Counter.
DATA: W_TEXT_AMOUNT(18) TYPE  C.

* Screen Text 변수 선언.
data: W_LIFNR_NM(35)    TYPE  C.
DATA: W_ZFBENI_NM(35)   TYPE  C.
DATA: W_LLIEF_NM(35)    TYPE  C.
DATA: W_ZFOPBN_NM(35)   TYPE  C.
DATA: W_OPEN_NM(35)     TYPE  C.

CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_101A    TYPE TABLEVIEW USING SCREEN 0101.
CONTROLS: TC_101B    TYPE TABLEVIEW USING SCREEN 0101.
CONTROLS: TC_101C    TYPE TABLEVIEW USING SCREEN 0101.
