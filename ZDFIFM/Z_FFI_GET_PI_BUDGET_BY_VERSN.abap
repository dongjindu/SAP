FUNCTION Z_FFI_GET_PI_BUDGET_BY_VERSN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(POSID) LIKE  IMPR-POSID
*"     REFERENCE(PRNAM) LIKE  IMPR-PRNAM
*"     REFERENCE(GJAHR) LIKE  IMPR-GJAHR
*"     REFERENCE(VERSN) LIKE  BPGE-VERSN
*"     REFERENCE(CATEG) LIKE  TAI08-IPPOS
*"  TABLES
*"      OUT STRUCTURE  ZFI_PI_BUDGET
*"----------------------------------------------------------------------
  DATA: it_impr TYPE TABLE OF impr WITH HEADER LINE.

  DATA : BEGIN OF it_bpja OCCURS 0.
          INCLUDE STRUCTURE bpja.
  DATA : END OF it_bpja.
*-----------------------------------
  DATA : BEGIN OF it_bpge OCCURS 0.
          INCLUDE STRUCTURE bpge.
  DATA : END OF it_bpge.
*---Rate
  DATA : BEGIN OF it_imzo OCCURS 0,
             objnr LIKE imzo-objnr,
             gjahr LIKE imzo-gjahr,
             baprz LIKE imzo-baprz,
             prozu LIKE imzo-prozu,
         END OF it_imzo.
*---Order
  DATA : BEGIN OF it_aufk OCCURS 0,
             aufnr LIKE aufk-aufnr,
             objnr LIKE aufk-objnr,
         END OF it_aufk.

  DATA : wa_cnt TYPE i.

  IF prnam IS INITIAL.
    SELECT * INTO TABLE it_impr FROM impr
      WHERE posid EQ posid
      AND   GJAHR EQ GJAHR.
  ELSE.
    SELECT * INTO TABLE it_impr FROM impr
      WHERE posid EQ posid
      AND   GJAHR EQ GJAHR
      AND   PRNAM EQ PRNAM.
  ENDIF.

  DESCRIBE TABLE it_impr LINES wa_cnt.
  IF wa_cnt > 0.
*    EXIT.
*  ELSE.

    SELECT objnr baprz prozu INTO CORRESPONDING FIELDS OF TABLE it_imzo
    FROM imzo
    FOR ALL ENTRIES IN it_impr
    WHERE  posnr EQ it_impr-posnr.
  ELSE.
    EXIT.
  ENDIF.

*  CLEAR WA_CNT.
*  DESCRIBE  TABLE it_IMZO LINES wa_cnt.
*  IF WA_CNT > 0.
*     SELECT aufnr objnr INTO CORRESPONDING FIELDS OF TABLE it_aufk
*     FROM aufk
*     FOR ALL ENTRIES IN it_imzo
*     WHERE objnr EQ it_imzo-objnr.
**  ELSE.
**     EXIT.
*  ENDIF.
*
*  CLEAR WA_CNT.
*  DESCRIBE  TABLE it_aufk LINES wa_cnt.
*  IF WA_CNT < 1.
*     EXIT.
*  ENDIF.

  REFRESH : it_bpja.
  CLEAR   : it_bpja.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
  FROM bpja
* FOR ALL ENTRIES IN it_AUFK
 FOR ALL ENTRIES IN it_impr
  WHERE lednr EQ '0001'
* AND   objnr EQ it_AUFK-objnr.
 AND   objnr EQ it_impr-objnr
 AND   VERSN EQ VERSN.


  REFRESH : it_bpge.
  CLEAR   : it_bpge.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
  FROM bpge
* FOR ALL ENTRIES IN it_AUFK
 FOR ALL ENTRIES IN it_impr
  WHERE lednr EQ '0001'
* AND   objnr EQ it_AUFK-objnr.
 AND   objnr EQ it_impr-objnr
 AND   VERSN EQ VERSN.


*---DELETE  annual
 LOOP AT it_bpja.
   if categ = '1'.
    IF it_bpja-posit+7(1) = '2'.
      DELETE it_bpja INDEX sy-tabix.
    ENDIF.
  elseif categ = '2'.
    IF it_bpja-posit+7(1) = '1'.
      DELETE it_bpja INDEX sy-tabix.
    ENDIF.
  endif.
 ENDLOOP.
*---DELETE  overall
  LOOP AT it_bpge.
   if categ = '1'.
    IF it_bpge-posit+7(1) = '2'.
      DELETE it_bpge INDEX sy-tabix.
    ENDIF.
   elseif categ = '2'.
    IF it_bpge-posit+7(1) = '1'.
      DELETE it_bpge INDEX sy-tabix.
    ENDIF.
   endif.
  ENDLOOP.

*---Make
  LOOP AT it_bpja.
    MOVE posid  TO out-posid.
    MOVE it_bpja-gjahr  TO out-gjahr.
*--RATE CHECK
    READ TABLE it_imzo WITH KEY objnr = it_bpja-objnr.
    IF sy-subrc = 0.
      IF it_imzo-prozu = 0.
        IF it_imzo-baprz <> 0.
          it_bpja-wtjhr = ( it_bpja-wtjhr * it_imzo-baprz ) / 100.
        ENDIF.
      ELSE.
        IF it_imzo-prozu <> 0.
          it_bpja-wtjhr = ( it_bpja-wtjhr * it_imzo-prozu ) / 100.
        ENDIF.
      ENDIF.
    ENDIF.
*-----Activity check---
*      check it_bpja-vorga <> 'KSTP'.
    CASE it_bpja-vorga.
      WHEN 'KSTP'.
        MOVE it_bpja-wtjhr TO out-plan.
      WHEN 'KBUD'.  "Original
        MOVE it_bpja-wtjhr TO out-wtjhr.
        MOVE it_bpja-wtjhr TO out-org.
      WHEN 'KBN0'.
        MOVE it_bpja-wtjhr TO out-supp.
        MOVE it_bpja-wtjhr TO out-wtjhr.
      WHEN 'KBR0'.
        MOVE it_bpja-wtjhr TO out-ret.
        MOVE it_bpja-wtjhr TO out-wtjhr.
    ENDCASE.
    COLLECT out.
    CLEAR   out.
  ENDLOOP.

  LOOP AT it_bpge.
    MOVE posid  TO out-posid.
    MOVE '1111'    TO out-gjahr.
*--RATE CHECK
    READ TABLE it_imzo WITH KEY objnr = it_bpge-objnr.
    IF sy-subrc = 0.
      IF it_imzo-prozu = 0.
        IF it_imzo-baprz <> 0.
          it_bpge-wtges = ( it_bpge-wtges * it_imzo-baprz ) / 100.
        ENDIF.
      ELSE.
        IF it_imzo-prozu <> 0.
          it_bpge-wtges = ( it_bpge-wtges * it_imzo-prozu ) / 100.
        ENDIF.
      ENDIF.
    ENDIF.
*-----Activity check---
*      check it_bpge-vorga <> 'KSTP'.
    CASE it_bpge-vorga.
      WHEN 'KSTP'.
        MOVE it_bpge-wtges TO out-plan.
      WHEN 'KBUD'.  "Original
        MOVE it_bpge-wtges TO out-org.
        MOVE it_bpge-wtges TO out-wtjhr.
      WHEN 'KBN0'.
        MOVE it_bpge-wtges TO out-supp.
        MOVE it_bpge-wtges TO out-wtjhr.
      WHEN 'KBR0'.
        MOVE it_bpge-wtges TO out-ret.
        MOVE it_bpge-wtges TO out-wtjhr.
    ENDCASE.
    COLLECT out.
    CLEAR   out.
  ENDLOOP.

ENDFUNCTION.
