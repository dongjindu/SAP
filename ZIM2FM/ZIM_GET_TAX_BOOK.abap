FUNCTION ZIM_GET_TAX_BOOK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZFTBNO) LIKE  ZTTAXBKHD-ZFTBNO
*"  EXPORTING
*"     VALUE(ZTTAXBKHD) LIKE  ZTTAXBKHD STRUCTURE  ZTTAXBKHD
*"  TABLES
*"      IT_ZSTAXBKIT STRUCTURE  ZSTAXBKIT OPTIONAL
*"      IT_ZSTAXBKIT_ORG STRUCTURE  ZSTAXBKIT OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_INPUT
*"----------------------------------------------------------------------
   DATA: L_MENGE       LIKE EKAB-MENGE,
         L_MENGE_H     LIKE EKAB-MENGE,
         L_MENGE_H_TOT LIKE EKAB-MENGE,
         L_MENGE_S     LIKE EKAB-MENGE,
         L_MENGE_S_TOT LIKE EKAB-MENGE.

  REFRESH : IT_ZSTAXBKIT, IT_ZSTAXBKIT_ORG.

  IF ZFTBNO IS INITIAL.
     RAISE NOT_FOUND.
  ENDIF.

  SELECT SINGLE * FROM ZTTAXBKHD
                  WHERE ZFTBNO  EQ   ZFTBNO.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  SELECT SINGLE * FROM EKKO
                  WHERE EBELN  EQ  ZTTAXBKHD-EBELN.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSTAXBKIT
           FROM ZTTAXBKIT
           WHERE ZFTBNO EQ ZFTBNO.

  LOOP AT IT_ZSTAXBKIT.
    W_TABIX = SY-TABIX.
    SELECT SINGLE MENGE INTO IT_ZSTAXBKIT-MENGE
                        FROM ZTREQIT
                        WHERE ZFREQNO EQ IT_ZSTAXBKIT-ZFREQNO
                        AND   ZFITMNO EQ IT_ZSTAXBKIT-ZFITMNO.

    SELECT SINGLE WERKS INTO IT_ZSTAXBKIT-WERKS
                        FROM EKPO
                        WHERE EBELN EQ IT_ZSTAXBKIT-EBELN
                        AND   EBELP EQ IT_ZSTAXBKIT-EBELP.

    SELECT SUM( BKMENGE ) INTO IT_ZSTAXBKIT-RMENGE
                          FROM ZTTAXBKCST
                          WHERE ZFREQNO EQ IT_ZSTAXBKIT-ZFREQNO
                          AND   ZFITMNO EQ IT_ZSTAXBKIT-ZFITMNO
                          AND   ZFTBNO  NE ZFTBNO.

    IF EKKO-BSTYP EQ 'K'.
      CLEAR :L_MENGE_H_TOT, L_MENGE_S_TOT.
      SELECT * FROM EKAB
               WHERE KONNR EQ IT_ZSTAXBKIT-EBELN
               AND   KTPNR EQ IT_ZSTAXBKIT-EBELP.

        SELECT SUM( MENGE ) INTO L_MENGE_H
               FROM EKBE
               WHERE EBELN EQ EKAB-EBELN
               AND   EBELP EQ EKAB-EBELP
               AND   VGABE EQ '1'
               AND   BEWTP EQ 'E'
               AND   SHKZG EQ 'H'.

        SELECT SUM( MENGE ) INTO L_MENGE_S
               FROM EKBE
               WHERE EBELN EQ EKAB-EBELN
               AND   EBELP EQ EKAB-EBELP
               AND   VGABE EQ '1'
               AND   BEWTP EQ 'E'
               AND   SHKZG EQ 'S'.

             L_MENGE_H_TOT = L_MENGE_H_TOT + L_MENGE_H.
             L_MENGE_S_TOT = L_MENGE_S_TOT + L_MENGE_S.
      ENDSELECT.

*-------------------------------------------------------------------
*> 입고수량.
      L_MENGE = L_MENGE_S_TOT - L_MENGE_H_TOT.
      IT_ZSTAXBKIT-GRMENGE = L_MENGE.
    ELSE.
* 입고수량 계산.
      SELECT SUM( MENGE ) INTO L_MENGE_H
             FROM EKBE
             WHERE EBELN EQ IT_ZSTAXBKIT-EBELN
             AND   EBELP EQ IT_ZSTAXBKIT-EBELP
             AND   VGABE EQ '1'
             AND   BEWTP EQ 'E'
             AND   SHKZG EQ 'H'.

      SELECT SUM( MENGE ) INTO L_MENGE_S
             FROM EKBE
             WHERE EBELN EQ IT_ZSTAXBKIT-EBELN
             AND   EBELP EQ IT_ZSTAXBKIT-EBELP
             AND   VGABE EQ '1'
             AND   BEWTP EQ 'E'
             AND   SHKZG EQ 'S'.

      IT_ZSTAXBKIT-GRMENGE = L_MENGE_S - L_MENGE_H.

    ENDIF.
*     ENDIF.

    MODIFY IT_ZSTAXBKIT INDEX W_TABIX.
  ENDLOOP.

  IT_ZSTAXBKIT_ORG[] = IT_ZSTAXBKIT[].

ENDFUNCTION.
