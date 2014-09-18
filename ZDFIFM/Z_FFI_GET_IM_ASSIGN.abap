FUNCTION z_ffi_get_im_assign.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(POSID) LIKE  IMPR-POSID
*"     REFERENCE(AUFNR) LIKE  AUFK-AUFNR
*"     REFERENCE(GJAHR) LIKE  IMPR-GJAHR
*"  EXPORTING
*"     REFERENCE(AMT) LIKE  BPJA-WTJHR
*"  TABLES
*"      OUT STRUCTURE  ZFI_IM_ASSIGN
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
***********************************************************************
* Modification Log
* Date     Developer      Request ID     Description
* 12/20/05 Manjunath      UD1K918723     Ticket:5C5C325A34
*                                        Exclude all Budgeting\planning
*                                        activity other than KBUD,KBN0,
*                                        KBR0.
* 12/21/05 Manjunath      UD1K918739     Add  Approval year as input
*                                        parameter to FM (5C5C325A34)
************************************************************************
  DATA : BEGIN OF it_impr OCCURS 0,
             posnr LIKE impr-posnr,
             gjahr LIKE impr-gjahr,
         END OF it_impr.

  DATA : BEGIN OF it_imzo OCCURS 0,
             objnr LIKE imzo-objnr,
             gjahr LIKE imzo-gjahr,
             baprz LIKE imzo-baprz,
             prozu LIKE imzo-prozu,
         END OF it_imzo.

  DATA : BEGIN OF it_aufk OCCURS 0,
             aufnr LIKE aufk-aufnr,
             objnr LIKE aufk-objnr,
         END OF it_aufk.
*------
  DATA : BEGIN OF it_RATE OCCURS 0,
             aufnr LIKE aufk-aufnr,
             prozu LIKE imzo-prozu,
         END OF it_RATE.

  DATA : wa_posnr LIKE impr-posnr,
         wa_cnt TYPE i,
         wa_aufnr LIKE aufk-aufnr.

  SELECT posnr gjahr INTO CORRESPONDING FIELDS OF TABLE it_impr
  FROM impr
  WHERE posid EQ posid.

  DESCRIBE  TABLE it_impr LINES wa_cnt.
  IF wa_cnt > 0.
    SELECT objnr baprz prozu INTO CORRESPONDING FIELDS OF TABLE it_imzo
    FROM imzo
    FOR ALL ENTRIES IN it_impr
    WHERE  posnr EQ it_impr-posnr
*   AND    GJAHR EQ IT_IMPR-GJAHR.
* Begin of Changes -  UD1K918739
    and GJAHR  eq GJAHR.
* End of  Changes -   UD1K918739
  ELSE.
    EXIT.
  ENDIF.

  SELECT aufnr objnr INTO CORRESPONDING FIELDS OF TABLE it_aufk
  FROM aufk
  FOR ALL ENTRIES IN it_imzo
  WHERE objnr EQ it_imzo-objnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = aufnr
       IMPORTING
            output = wa_aufnr.

*except myself
*--2004/02/03
*  DELETE it_aufk WHERE aufnr EQ wa_aufnr.
  CLEAR wa_cnt.
  DESCRIBE  TABLE it_aufk LINES wa_cnt.
  IF wa_cnt < 1.
    EXIT.
  ENDIF.
*--GET BPJA & BPGE
*  IF GJAHR = '1111'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
  FROM bpge
  FOR ALL ENTRIES IN it_aufk
  WHERE lednr EQ '0001'
  AND   objnr EQ it_aufk-objnr.
* ELSE.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
  FROM bpja
  FOR ALL ENTRIES IN it_aufk
  WHERE lednr EQ '0001'
  AND   objnr EQ it_aufk-objnr.
* ENDIF.
*---Main process
  REFRESH : out.
  CLEAR :   out, amt.

  LOOP AT it_aufk.
*    IF gjahr = '1111'.
*----Over all
    LOOP AT it_bpge WHERE objnr = it_aufk-objnr.
      CASE it_bpge-vorga.
        WHEN 'KBUD'.        "Ori Budget
          MOVE it_bpge-wtges TO out-wtjhr.
        WHEN 'KBN0'.        "Supplement
          MOVE it_bpge-wtges TO out-wtjhr.
        WHEN 'KBR0'.        "Return
          MOVE it_bpge-wtges TO out-wtjhr.
*           out-wtjhr = it_bpGE-wtges * -1.
* Begin of changes - UD1K918723.
       WHEN OTHERS.
          continue.
* End of changes -   UD1K918723
      ENDCASE.
      MOVE posid         TO out-posid.
      MOVE it_aufk-aufnr TO out-aufnr.
      MOVE '1111'        TO out-gjahr.
      READ TABLE it_imzo WITH KEY objnr = it_aufk-objnr.
      IF sy-subrc = 0.
        IF it_imzo-prozu = 0.
           if it_imzo-baprz <> 0.
              out-wtjhr = ( out-wtjhr * it_imzo-baprz ) / 100.
           endif.
          MOVE IT_AUFK-AUFNR TO IT_RATE-AUFNR.
          MOVE IT_IMZO-BAPRZ TO IT_RATE-PROZU.
        ELSE.
          out-wtjhr = ( out-wtjhr * it_imzo-prozu ) / 100.
          MOVE IT_AUFK-AUFNR TO IT_RATE-AUFNR.
          MOVE IT_IMZO-PROZU TO IT_RATE-PROZU.
        ENDIF.
        APPEND IT_RATE.
        CLEAR  IT_RATE.
      ENDIF.

      amt = amt + out-wtjhr.
      COLLECT out.
      CLEAR   out.
    ENDLOOP.
*----Annual
*    ELSE.
    LOOP AT it_bpja WHERE objnr = it_aufk-objnr.
*--- Annual data process
      CASE it_bpja-vorga.
        WHEN 'KBUD'.        "Ori Budget
          MOVE it_bpja-wtjhr TO out-wtjhr.
        WHEN 'KBN0'.        "Supplement
          MOVE it_bpja-wtjhr TO out-wtjhr.
        WHEN 'KBR0'.        "Return
*           out-wtjhr = it_bpja-wtjhr * -1.
          MOVE it_bpja-wtjhr TO out-wtjhr.
* Begin of changes - UD1K918723.
       WHEN OTHERS.
          continue.
* End of changes -   UD1K918723

      ENDCASE.
      MOVE posid         TO out-posid.
      MOVE it_aufk-aufnr TO out-aufnr.
      MOVE it_bpja-gjahr TO out-gjahr.
      READ TABLE it_imzo WITH KEY objnr = it_aufk-objnr.
      IF sy-subrc = 0.
        IF it_imzo-prozu = 0.
           if it_imzo-baprz <> 0.
              out-wtjhr = ( out-wtjhr * it_imzo-baprz ) / 100.
           endif.
          MOVE IT_AUFK-AUFNR TO IT_RATE-AUFNR.
          MOVE IT_IMZO-BAPRZ TO IT_RATE-PROZU.
        ELSE.
          out-wtjhr = ( out-wtjhr * it_imzo-prozu ) / 100.
          MOVE IT_AUFK-AUFNR TO IT_RATE-AUFNR.
          MOVE IT_IMZO-PROZU TO IT_RATE-PROZU.
        ENDIF.
        APPEND IT_RATE.
        CLEAR  IT_RATE.
      ENDIF.
      amt = amt + out-wtjhr.
      COLLECT out.
      CLEAR   out.
    ENDLOOP.
*    ENDIF.
  ENDLOOP.
*---GET RATE
  LOOP AT out.
    READ TABLE it_RATE WITH KEY AUFNR = OUT-AUFNR.
    IF sy-subrc = 0.
        MOVE it_RATE-PROZU TO out-prozu.
        MODIFY OUT. "  INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
ENDFUNCTION.
