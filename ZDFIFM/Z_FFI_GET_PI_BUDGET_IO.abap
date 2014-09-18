FUNCTION Z_FFI_GET_PI_BUDGET_IO.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(POSID) LIKE  IMPR-POSID
*"     REFERENCE(PRNAM) LIKE  IMPR-PRNAM
*"     REFERENCE(GJAHR) LIKE  IMPR-GJAHR
*"  TABLES
*"      OUT STRUCTURE  ZFI_PI_BUDGET
*"----------------------------------------------------------------------
  DATA: it_impr TYPE TABLE OF impr WITH HEADER LINE.

*  DATA : BEGIN OF it_impr OCCURS 0,
*           posnr  LIKE impr-posnr,
*           posid LIKE impr-posid,
*           objnr LIKE impr-objnr,
*           prnam LIKE imtp-prnam,
*           ydist LIKE imtp-ydist,
*           capex LIKE imtp-capex,
*           END OF it_impr.

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
  data: l_capex like imtp-capex.
  data: l_ydist like imtp-ydist.

  IF prnam IS INITIAL.
*    SELECT  posnr posid objnr b~prnam ydist capex INTO TABLE it_impr
*      FROM impr AS a
*    INNER JOIN imtp AS b
*    ON a~gjahr = b~gjahr
*    AND a~prnam = b~prnam
*      WHERE posid EQ posid
*      AND a~gjahr EQ gjahr.

    SELECT * INTO TABLE it_impr FROM impr
      WHERE posid EQ posid
      AND   GJAHR EQ GJAHR.
  ELSE.
*    SELECT posnr  posid objnr b~prnam ydist capex INTO TABLE it_impr
*       FROM impr AS a
*     INNER JOIN imtp AS b
*     ON a~gjahr = b~gjahr
*     AND a~prnam = b~prnam
*    WHERE posid EQ posid
*       AND   a~gjahr EQ gjahr
*       AND   a~prnam EQ prnam.

    SELECT * INTO TABLE it_impr FROM impr
      WHERE posid EQ posid
      AND   GJAHR EQ GJAHR
      AND   PRNAM EQ PRNAM.
  ENDIF.


  select single ydist capex into (l_ydist, l_capex)
      from imtp
    where gjahr EQ gjahr
       AND prnam EQ prnam.
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

  CLEAR WA_CNT.
  DESCRIBE  TABLE it_IMZO LINES wa_cnt.
  IF WA_CNT > 0.
    SELECT aufnr objnr INTO CORRESPONDING FIELDS OF TABLE it_aufk
    FROM aufk
    FOR ALL ENTRIES IN it_imzo
    WHERE objnr EQ it_imzo-objnr.
  ELSE.
    EXIT.
  ENDIF.

  CLEAR WA_CNT.
  DESCRIBE  TABLE it_aufk LINES wa_cnt.
  IF WA_CNT < 1.
    EXIT.
  ENDIF.

  REFRESH : it_bpja.
  CLEAR   : it_bpja.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
  FROM bpja
  FOR ALL ENTRIES IN it_AUFK
  WHERE lednr EQ '0001'
 AND   objnr EQ it_AUFK-objnr.

  REFRESH : it_bpge.
  CLEAR   : it_bpge.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
  FROM bpge
  FOR ALL ENTRIES IN it_AUFK
  WHERE lednr EQ '0001'
  AND   objnr EQ it_AUFK-objnr.

*---DELETE  annual
  LOOP AT it_bpja.
    IF it_bpja-posit+7(1) = '2'.
      DELETE it_bpja INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
*---DELETE  overall
  LOOP AT it_bpge.
    IF it_bpge-posit+7(1) = '2'.
      DELETE it_bpge INDEX sy-tabix.
    ENDIF.
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
** By Fruong on 04/05/14 (
        IF l_capex = 'X'.  " catogery
          MOVE it_bpja-wtjhr TO out-wtjhr.
          MOVE it_bpja-wtjhr TO out-org.
        elseif l_ydist = 'X' AND it_bpja-wrttp = '47'. "distribution
          MOVE it_bpja-wtjhr TO out-wtjhr.
          MOVE it_bpja-wtjhr TO out-org.
        ELSE.
          CLEAR: out-wtjhr, out-org.
        ENDIF.
*        MOVE it_bpja-wtjhr TO out-wtjhr.
*        MOVE it_bpja-wtjhr TO out-org.
** )
      WHEN 'KBN0'.
        IF l_capex = 'X'.  " catogery
          MOVE it_bpja-wtjhr TO out-supp.
          MOVE it_bpja-wtjhr TO out-wtjhr.
        elseif l_ydist = 'X' AND it_bpja-wrttp = '47'. "distribution
          MOVE it_bpja-wtjhr TO out-supp.
          MOVE it_bpja-wtjhr TO out-wtjhr.
        else.
          CLEAR: out-wtjhr, out-supp.
        endif.
*        MOVE it_bpja-wtjhr TO out-supp.
*        MOVE it_bpja-wtjhr TO out-wtjhr.
      WHEN 'KBR0'.
        IF l_capex = 'X'.  " catogery
          MOVE it_bpja-wtjhr TO out-ret.
          MOVE it_bpja-wtjhr TO out-wtjhr.
        elseif l_ydist = 'X' AND it_bpja-wrttp = '47'. "distribution
          MOVE it_bpja-wtjhr TO out-ret.
          MOVE it_bpja-wtjhr TO out-wtjhr.
        else.
          CLEAR: out-wtjhr, out-ret.
        endif.
*        MOVE it_bpja-wtjhr TO out-ret.
*        MOVE it_bpja-wtjhr TO out-wtjhr.
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
        IF l_capex = 'X'.
          MOVE it_bpge-wtges TO out-org.
          MOVE it_bpge-wtges TO out-wtjhr.
        ELSEIF l_ydist = 'X' AND it_bpge-wrttp = '47'.
          MOVE it_bpge-wtges TO out-org.
          MOVE it_bpge-wtges TO out-wtjhr.
        ELSE.
          CLEAR: out-wtjhr, out-org.
        ENDIF.
      WHEN 'KBN0'.
        IF l_capex = 'X'.
          MOVE it_bpge-wtges TO out-supp.
          MOVE it_bpge-wtges TO out-wtjhr.
        elseif l_ydist = 'X' AND it_bpge-wrttp = '47'.
          MOVE it_bpge-wtges TO out-supp.
          MOVE it_bpge-wtges TO out-wtjhr.
        else.
          CLEAR: out-wtjhr, out-supp.
        endif.
*        MOVE it_bpge-wtges TO out-supp.
*        MOVE it_bpge-wtges TO out-wtjhr.
      WHEN 'KBR0'.
        IF l_capex = 'X'.
          MOVE it_bpge-wtges TO out-ret.
          MOVE it_bpge-wtges TO out-wtjhr.
        elseif l_ydist = 'X' AND it_bpge-wrttp = '47'.
          MOVE it_bpge-wtges TO out-ret.
          MOVE it_bpge-wtges TO out-wtjhr.
        else.
          CLEAR: out-wtjhr, out-ret.
        endif.
*        MOVE it_bpge-wtges TO out-ret.
*        MOVE it_bpge-wtges TO out-wtjhr.
    ENDCASE.
    COLLECT out.
    CLEAR   out.
  ENDLOOP.

ENDFUNCTION.
