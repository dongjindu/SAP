*&---------------------------------------------------------------------*
*& Include ZRIM10TOP                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : [Include] 수입 수송 Main Data Define                  *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.17                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

CONTROLS TABSTRIP    TYPE TABSTRIP.
CONTROLS: TC_0103    TYPE TABLEVIEW USING SCREEN 0103.
CONTROLS: TC_0104    TYPE TABLEVIEW USING SCREEN 0104.
CONTROLS: TC_0104_1  TYPE TABLEVIEW USING SCREEN 0104.
CONTROLS: TC_0104_2  TYPE TABLEVIEW USING SCREEN 0104.
CONTROLS: TC_0023    TYPE TABLEVIEW USING SCREEN 0023.

*>> 데이타 교환용 Internal Table.
DATA: BEGIN OF IT_TRS OCCURS 0,
       ZFIVNO    LIKE  ZTIV-ZFIVNO,     " 통관입고 요청관리번호.
       ZFBLNO    LIKE  ZTIDS-ZFBLNO,    " B/L 관리번호.
       ZFHBLNO   LIKE  ZTIDS-ZFHBLNO,    " HBL.
       ZFCLSEQ   LIKE  ZTIDS-ZFCLSEQ.   " 통관순번.
DATA : END OF IT_TRS.

*>> 수입문서의 메뉴 상태를 상수로 정?
DATA : C_TR(15)      VALUE '보세창고 출고'.

*>> SCREEN TEXT.
DATA : WT_BUKRS(30),
       WT_PLANT(30),
       WT_ZFTRGB(4),     " 수송구분.
       WT_ZFDRMT(4),     " 수송방법.
       WT_ZFTRCO(35).    " 운송업체.

DATA : W_CREATE(6)        TYPE     C     VALUE   'Create',
       W_CHANGE(6)        TYPE     C     VALUE   'Change',
       W_DISPLAY(7)       TYPE     C     VALUE   'Display',
       W_OPEN(08)         TYPE     C     VALUE   'Openning',
       W_STAUTS(13)       TYPE     C     VALUE   'Status Change'.

*>> 수송 변경, 조회시 중복검색조건 용.
DATA : BEGIN OF IT_ZFTRNO OCCURS 0,
         ZFTRNO LIKE ZTTRHD-ZFTRNO,
       END OF IT_ZFTRNO.

*>> 중복검색조건 체크.
DATA : W_SRCH(2). " PO, HB, BL

DATA : W_POYN_CK. " 유무환 여부.
*>> 전기용.
DATA : IT_ZSBSEG      LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.
DATA : IT_ZSBSEG_ORG  LIKE ZSBSEG OCCURS 10 WITH HEADER LINE.

*>> 출고일자체크용.
DATA : W_IDSDT LIKE ZTIDS-ZFIDSDT,
       W_WDT(10).

*> 운반비, 인건비 계산용 변수.
DATA : W_THAP LIKE ZSBSEG-WRBTR,
       W_MHAP LIKE ZSBSEG-WRBTR.

DATA : LINE1   TYPE   I.

*> 적용톤과 계산톤비교를 위한 변수.
DATA : W_TOTWT LIKE ZTTRCST-ZFCTON.

*> 반출증용 RANGE.
DATA: BEGIN OF RG_SEL OCCURS 10,
         SIGN(1),
         OPTION(2),
         LOW  LIKE ZTTRHD-ZFTRNO,
         HIGH LIKE ZTTRHD-ZFTRNO,
      END   OF RG_SEL.
