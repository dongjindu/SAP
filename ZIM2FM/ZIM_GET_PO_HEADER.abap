FUNCTION ZIM_GET_PO_HEADER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EBELN) LIKE  EKKO-EBELN
*"  EXPORTING
*"     VALUE(W_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"  EXCEPTIONS
*"      NOT_INPUT
*"      NOT_FOUND
*"      NOT_RELEASED
*"      NOT_TYPE
*"      NO_TRANSACTION
*"      PO_DELETION
*"----------------------------------------------------------------------
  CLEAR : W_EKKO.
  CLEAR : EKKO, T160, LFA1.

*-----------------------------------------------------------------------
* 구매 문서번호의 입력 유무 검증
*-----------------------------------------------------------------------
  IF EBELN IS INITIAL.
     RAISE NOT_INPUT.
  ENDIF.

*-----------------------------------------------------------------------
* 구매문서 HEADER SELECT
*-----------------------------------------------------------------------
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ EBELN.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
*    MESSAGE E001 WITH EBELN.
  ENDIF.

*-----------------------------------------------------------------------
* 삭제된 상태
*-----------------------------------------------------------------------
  IF NOT EKKO-LOEKZ IS INITIAL.
     RAISE PO_DELETION.
  ENDIF.

*-----------------------------------------------------------------------
* 구매문서 범주 검증
*-----------------------------------------------------------------------
  SELECT SINGLE * FROM T160 WHERE TCODE EQ 'ME23'.
  IF SY-SUBRC NE 0.
     RAISE NO_TRANSACTION.
*    MESSAGE E000 WITH 'ME23'.
  ENDIF.

*-----------------------------------------------------------------------
* 구매문서범주가 SPACE일 경우
*-----------------------------------------------------------------------
*  IF T160-BSTYP EQ SPACE.
*     IF EKKO-BSTYP NE BSTYP-KONT AND
*        EKKO-BSTYP NE BSTYP-LFPL.
*        RAISE NOT_TYPE.
**        MESSAGE E002 WITH EBELN EKKO-BSTYP.
*     ENDIF.
*  ELSE.
*     IF EKKO-BSTYP NE T160-BSTYP.
*        RAISE NOT_TYPE.
**        MESSAGE E002 WITH EBELN EKKO-BSTYP.
*     ENDIF.
*  ENDIF.
   IF NOT ( EKKO-BSTYP EQ  BSTYP-LFPL OR        ">납품일정계획.
            EKKO-BSTYP EQ  BSTYP-BEST OR        ">구매오더.
            EKKO-BSTYP EQ  BSTYP-KONT OR        ">일괄계약.
            EKKO-BSTYP EQ  BSTYP-LERF ).        ">서비스
      RAISE NOT_TYPE.
   ENDIF.

*-----------------------------------------------------------------------
* 구매오더문서 릴리즈 체크...
* 2000/04/04   ===> E&Y 유재오 과장 DEFINE
*-----------------------------------------------------------------------
* IF NOT ( EKKO-FRGKE EQ '2' OR EKKO-FRGKE EQ SPACE ).
*> < 2002.11.05 NHJ > P/O RELEASE CHECK 여부 주석처리.
*  IF EKKO-FRGRL EQ 'X'.
*     RAISE NOT_RELEASED.
*  ENDIF.

  MOVE EKKO TO W_EKKO.

ENDFUNCTION.
