*----------------------------------------------------------------------*
*INCLUDE ZRIM08TOP .
*&---------------------------------------------------------------------*
*&  프로그램명 : 기납증 Main Data Define Include                       *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.09.11                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : ZTTAXBKCST.


DATA: IT_ZSBSEG          LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.
DATA: IT_ZSBSEG_OLD      LIKE ZSBSEG OCCURS 100 WITH HEADER LINE.


CONTROLS: TC_0110    TYPE TABLEVIEW USING SCREEN 0110.


DATA : L_BKMENGE   LIKE ZTTAXBKIT-BKMENGE,
       L_ZFTBAK    LIKE ZTTAXBKIT-ZFTBAK,
       L_ZFCUAMT   LIKE ZTTAXBKIT-ZFCUAMT,
       L_ZFHMAMT   LIKE ZTTAXBKIT-ZFHMAMT,
       L_ZFEDAMT   LIKE ZTTAXBKIT-ZFEDAMT,
       L_ZFAGAMT   LIKE ZTTAXBKIT-ZFAGAMT,
       L_ZFTXAMTS  LIKE ZTTAXBKIT-ZFTXAMTS,
       L_TABIX     LIKE SY-TABIX,
       L_RATE      TYPE F,
       L_CHECK.

*> 사업영역.
DATA : BEGIN OF IT_GSBER OCCURS 5,
       GSBER    LIKE     BSEG-GSBER,
       END   OF IT_GSBER.
