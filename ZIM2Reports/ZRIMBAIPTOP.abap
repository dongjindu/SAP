*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMBAIPTOP                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : BAPIs Function용 공통 Include                         *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.27                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : BAPIACHE08,
         BAPIACHE02,
         BAPIACGL08,
         BAPIACCR08,
         BAPIRET2,
         BAPIEXTC.


DATA : DOCUMENTHEADER  LIKE  BAPIACHE08,
       OBJ_TYPE        LIKE  BAPIACHE02-OBJ_TYPE,
       OBJ_KEY         LIKE  BAPIACHE02-OBJ_KEY,
       OBJ_SYS         LIKE  BAPIACHE02-OBJ_SYS.


DATA:   BEGIN OF ACCOUNTGL OCCURS 0.        ">> 회계전기: 일반전기.
        INCLUDE STRUCTURE   BAPIACGL08.
DATA:   END   OF ACCOUNTGL.

DATA:   BEGIN OF CURRENCYAMOUNT OCCURS 0.   ">> 회계전기: 대금청구문서 .
        INCLUDE STRUCTURE   BAPIACCR08.
DATA:   END   OF CURRENCYAMOUNT.

DATA:   BEGIN OF EXTENSION1 OCCURS 0.       ">>'Customer Exit' 매개변수.
        INCLUDE STRUCTURE   BAPIEXTC.
DATA:   END   OF EXTENSION1.

DATA:   BEGIN OF RETURN OCCURS 0.          ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.
