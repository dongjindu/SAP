*
* Clear FM Object Info from FI Document
* - Andy Choi
*
report Z_FMCLEAR no standard page heading line-size 255.

TABLES :GLT0,BSIS,BKPF,T001,SKAT,BSEG,LFA1,SKA1,KNA1,SKB1.
DATA : IBSIS  LIKE BSIS OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
         HKONT  LIKE  BSIS-HKONT,
         BELNR  LIKE  BSIS-BELNR,
         BUZEI  LIKE  BSIS-BUZEI,
         BUDAT  LIKE  BSIS-BUDAT,
         VALUT  LIKE  BSIS-VALUT,
       END OF itab.

PARAMETERS : P_BUKRS LIKE BSIS-BUKRS memory id BUK,
             P_GJAHR(4) type c.
*             P_HKONT LIKE BSIS-HKONT OBLIGATORY.
SELECT-OPTIONS : S_BELNR FOR BSIS-BELNR,
                 S_BUDAT FOR BSIS-BUDAT.
parameters: p_run as checkbox.

start-of-selection.
  data: l_fdtag like bseg-fdtag.

  SELECT * from bkpf into corresponding fields of table itab
   WHERE BUKRS = P_BUKRS
     AND GJAHR = P_GJAHR
     AND BUDAT IN S_BUDAT
     AND BELNR IN S_BELNR
     AND STBLG = SPACE.

  loop at itab.
*    if itab-valut <> itab-budat.
*      write:/ 'VALUT:', itab-belnr, itab-buzei,
*              itab-valut, ' --> ', itab-budat.
*      perform update_doc.
*    endif.

    select * from bseg
      where bukrs = p_bukrs
        and gjahr = p_gjahr
        and belnr = itab-belnr.

      if bseg-fipos <> space.
        write:/ itab-belnr, bseg-buzei, bseg-hkont,
                bseg-fipos, bseg-FISTL, bseg-geber.
        if p_run = 'X'.
          clear: bseg-fipos, bseg-FISTL, bseg-geber.
          update bseg.
          write: '... cleared'.
        endif.
      endif.
    endselect.

  endloop.
