FUNCTION ZIM_GET_NODRAFT_ACCOUNT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFCSTGRP) LIKE  ZTBKPF-ZFCSTGRP
*"     REFERENCE(ZFCD) LIKE  ZTBSEG-ZFCD
*"     REFERENCE(ZFIMDNO) LIKE  ZTBKPF-ZFIMDNO
*"  EXPORTING
*"     VALUE(NEWKO) LIKE  ZSBSEG-NEWKO
* 수정일   : 2002.05.09
* 수정자   : 나현주
* 수정내용 : 비용계정을 수입 IMG 에서 SET 하지 않고 STANDARD IMG에서
*            SET 한 내용을 GET!
*"----------------------------------------------------------------------

  CLEAR : NEWKO.

  CASE ZFCSTGRP .
     WHEN '003'.
        SELECT SINGLE * FROM ZTREQHD
                WHERE  ZFREQNO EQ ZFIMDNO.

        SELECT SINGLE * FROM  ZTIMIMG11
                 WHERE BUKRS EQ ZTREQHD-BUKRS.
        IF SY-SUBRC NE 0.
           MESSAGE E987 WITH ZTREQHD-BUKRS.
        ENDIF.

*        IF ZTREQHD-ZFTRIPLE EQ 'X'.
*          MOVE ZTIMIMG11-ZFIOCAC11 TO NEWKO. ">삼국무역.
*       ELSE.
*          MOVE ZTIMIMG11-ZFIOCAC1  TO NEWKO.
*       ENDIF.

     WHEN '004' OR '005'.
        SELECT SINGLE * FROM ZTBL
               WHERE  ZFBLNO EQ ZFIMDNO.

        SELECT SINGLE * FROM  ZTIMIMG11
                 WHERE BUKRS EQ ZTBL-BUKRS.
        IF SY-SUBRC NE 0.
           MESSAGE E987 WITH ZTBL-BUKRS.
        ENDIF.

        IF ZTBL-ZFPOYN EQ 'N'.
           IF ZTBL-ZFUPT EQ 'B'.
              CASE ZTBL-ZFPOTY.
                 WHEN 'S'.   ">Sample B/L.
                    MOVE: ZTIMIMG11-ZFIOCAC20  TO NEWKO.
                 WHEN 'P'.   ">대체용.
                    MOVE: ZTIMIMG11-ZFIOCAC14  TO NEWKO.
                 WHEN 'B'.   ">보상용.
                    MOVE: ZTIMIMG11-ZFIOCAC15  TO NEWKO.
                 WHEN 'A'.   ">수리용.
                    MOVE: ZTIMIMG11-ZFIOCAC16  TO NEWKO.
                 WHEN 'H'.   ">Ship. Back(영업반품).
                    MOVE: ZTIMIMG11-ZFIOCAC17  TO NEWKO.
                 WHEN 'Z'.   ">기타.
                    MOVE: ZTIMIMG11-ZFIOCAC18  TO NEWKO.
              ENDCASE.
           ELSE.
              CASE ZTBL-ZFPOTY.
                 WHEN 'S'.   ">Sample B/L.
                    MOVE: ZTIMIMG11-ZFIOCAC30  TO NEWKO.
                 WHEN 'P'.   ">대체용.
                    MOVE: ZTIMIMG11-ZFIOCAC24  TO NEWKO.
                 WHEN 'B'.   ">보상용.
                    MOVE: ZTIMIMG11-ZFIOCAC25  TO NEWKO.
                 WHEN 'A'.   ">수리용.
                    MOVE: ZTIMIMG11-ZFIOCAC26  TO NEWKO.
                 WHEN 'H'.   ">Ship. Back(영업반품).
                    MOVE: ZTIMIMG11-ZFIOCAC27  TO NEWKO.
                 WHEN 'Z'.   ">기타.
                    MOVE: ZTIMIMG11-ZFIOCAC28  TO NEWKO.
              ENDCASE.
           ENDIF.
*        ELSE.
*          MOVE ZTIMIMG11-ZFIOCAC2  TO NEWKO.
        ENDIF.

*> CON'T TAX
        IF ZFCSTGRP EQ '004' AND ZFCD EQ 'CTT'.
           MOVE ZTIMIMG11-ZFIOCAC19 TO NEWKO.
        ENDIF.

     WHEN '006'.
        SELECT SINGLE * FROM ZTBL
               WHERE ZFBLNO EQ ( SELECT ZFBLNO
                                 FROM ZTIV
                                 WHERE  ZFIVNO EQ ZFIMDNO ).
        SELECT SINGLE * FROM  ZTIMIMG11
                 WHERE BUKRS EQ ZTBL-BUKRS.
        IF SY-SUBRC NE 0.
           MESSAGE E987 WITH ZTBL-BUKRS.
        ENDIF.

        IF ZTBL-ZFPOYN EQ 'N'.
           IF ZTBL-ZFUPT EQ 'B'.
              CASE ZTBL-ZFPOTY.
                 WHEN 'S'.   ">Sample B/L.
                    MOVE: ZTIMIMG11-ZFIOCAC20  TO NEWKO.
                 WHEN 'P'.   ">대체용.
                    MOVE: ZTIMIMG11-ZFIOCAC14  TO NEWKO.
                 WHEN 'B'.   ">보상용.
                    MOVE: ZTIMIMG11-ZFIOCAC15  TO NEWKO.
                 WHEN 'A'.   ">수리용.
                    MOVE: ZTIMIMG11-ZFIOCAC16  TO NEWKO.
                 WHEN 'H'.   ">Ship. Back(영업반품).
                    MOVE: ZTIMIMG11-ZFIOCAC17  TO NEWKO.
                 WHEN 'Z'.   ">기타.
                    MOVE: ZTIMIMG11-ZFIOCAC18  TO NEWKO.
              ENDCASE.
           ELSE.
              CASE ZTBL-ZFPOTY.
                 WHEN 'S'.   ">Sample B/L.
                    MOVE: ZTIMIMG11-ZFIOCAC30  TO NEWKO.
                 WHEN 'P'.   ">대체용.
                    MOVE: ZTIMIMG11-ZFIOCAC24  TO NEWKO.
                 WHEN 'B'.   ">보상용.
                    MOVE: ZTIMIMG11-ZFIOCAC25  TO NEWKO.
                 WHEN 'A'.   ">수리용.
                    MOVE: ZTIMIMG11-ZFIOCAC26  TO NEWKO.
                 WHEN 'H'.   ">Ship. Back(영업반품).
                    MOVE: ZTIMIMG11-ZFIOCAC27  TO NEWKO.
                 WHEN 'Z'.   ">기타.
                    MOVE: ZTIMIMG11-ZFIOCAC28  TO NEWKO.
              ENDCASE.
           ENDIF.
           IF ZFCD EQ '003'.
              MOVE: ZTIMIMG11-ZFIOCAC6  TO NEWKO.
           ENDIF.
        ELSE.
*           MOVE ZTIMIMG11-ZFIOCAC4  TO NEWKO.

*           IF ZFCD EQ '001'.   ">
*             MOVE: ZTIMIMG11-ZFIOCAC8  TO NEWKO.
*          ENDIF.
           IF ZFCD EQ '003'.
              MOVE: ZTIMIMG11-ZFIOCAC6  TO NEWKO.
           ENDIF.
        ENDIF.

*     WHEN '007'.
*        SELECT SINGLE * FROM ZTBL
*               WHERE ZFBLNO EQ ( SELECT ZFBLNO
*                                 FROM   ZTCGHD
*                                 WHERE  ZFCGNO EQ ZFIMDNO ).
*        SELECT SINGLE * FROM  ZTIMIMG11
*                 WHERE BUKRS EQ ZTREQHD-BUKRS.
*        IF SY-SUBRC NE 0.
*           MESSAGE E987 WITH ZTBL-BUKRS.
*        ENDIF.
*
*        IF ZTBL-ZFPOYN EQ 'N'.
*           IF ZTBL-ZFUPT EQ 'B'.
*              CASE ZTBL-ZFPOTY.
*                 WHEN 'S'.   ">Sample B/L.
*                    MOVE: ZTIMIMG11-ZFIOCAC20  TO NEWKO.
*                 WHEN 'P'.   ">대체용.
*                    MOVE: ZTIMIMG11-ZFIOCAC14  TO NEWKO.
*                 WHEN 'B'.   ">보상용.
*                    MOVE: ZTIMIMG11-ZFIOCAC15  TO NEWKO.
*                 WHEN 'A'.   ">수리용.
*                    MOVE: ZTIMIMG11-ZFIOCAC16  TO NEWKO.
*                 WHEN 'H'.   ">Ship. Back(영업반품).
*                    MOVE: ZTIMIMG11-ZFIOCAC17  TO NEWKO.
*                 WHEN 'Z'.   ">기타.
*                    MOVE: ZTIMIMG11-ZFIOCAC18  TO NEWKO.
*              ENDCASE.
*           ELSE.
*              CASE ZTBL-ZFPOTY.
*                 WHEN 'S'.   ">Sample B/L.
*                    MOVE: ZTIMIMG11-ZFIOCAC30  TO NEWKO.
*                 WHEN 'P'.   ">대체용.
*                    MOVE: ZTIMIMG11-ZFIOCAC24  TO NEWKO.
*                 WHEN 'B'.   ">보상용.
*                    MOVE: ZTIMIMG11-ZFIOCAC25  TO NEWKO.
*                 WHEN 'A'.   ">수리용.
*                    MOVE: ZTIMIMG11-ZFIOCAC26  TO NEWKO.
*                 WHEN 'H'.   ">Ship. Back(영업반품).
*                    MOVE: ZTIMIMG11-ZFIOCAC27  TO NEWKO.
*                 WHEN 'Z'.   ">기타.
*                    MOVE: ZTIMIMG11-ZFIOCAC28  TO NEWKO.
*              ENDCASE.
*           ENDIF.
*        ELSE.
*           MOVE ZTIMIMG11-ZFIOCAC12  TO NEWKO.
*        ENDIF.

     WHEN '008'.  ">기납증 관련.
        SELECT SINGLE * FROM ZTREQHD
               WHERE ZFREQNO EQ ( SELECT ZFREQNO
                                  FROM   ZTTAXBKHD
                                  WHERE  ZFTBNO EQ ZFIMDNO ).
        SELECT SINGLE * FROM  ZTIMIMG11
                 WHERE BUKRS EQ ZTREQHD-BUKRS.
        IF SY-SUBRC NE 0.
           MESSAGE E987 WITH ZTBL-BUKRS.
        ENDIF.

*        MOVE ZTIMIMG11-ZFIOCAC12  TO NEWKO.
  ENDCASE.

ENDFUNCTION.
