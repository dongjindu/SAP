*
* Andy Choi
* 2003.04
* Temporary Program
*
report ZRGUREC10 no standard page heading line-size 255.

TABLES :GLT0,BSIS,BKPF,T001,SKAT,BSEG,LFA1,SKA1,KNA1,SKB1.
DATA : IBSIS  LIKE BSIS OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
         BELNR  LIKE  BSIS-BELNR,
         BUDAT  LIKE  BSIS-BUDAT,
       END OF itab.

DATA : BEGIN OF iseg OCCURS 0,
         AUFNR  LIKE bseg-aufnr,
         ANLN1  like bseg-ANLN1,
         ANLN2  LIKE bseg-ANLN2,
         BUZEI  LIKE BSIS-BUZEI,
       END OF iseg.


PARAMETERS : P_BUKRS LIKE BSIS-BUKRS memory id BUK,
             P_GJAHR(4) type c.
*             P_HKONT LIKE BSIS-HKONT OBLIGATORY.
SELECT-OPTIONS : S_BELNR FOR BSIS-BELNR,
                 S_BUDAT FOR BSIS-BUDAT.
parameters: p_run as checkbox.

start-of-selection.
  data: iorder like bseg-aufnr.


  SELECT * INTO corresponding fields of TABLE itab
    FROM bkpf
   WHERE BUKRS = P_BUKRS
     AND GJAHR = P_GJAHR
     AND BUDAT IN S_BUDAT
     AND BELNR IN S_BELNR
     and AWTYP = 'RMRP'
     AND STBLG = SPACE.

  loop at itab.
    select * into corresponding fields of table iseg
      from bseg
      where bukrs = p_bukrs
        and gjahr = p_gjahr
        and belnr = itab-belnr.

    loop at iseg where aufnr = space
                   and anln1 <> space.
      select single EAUFN into iorder
        from ANLA
        where BUKRS = p_bukrs
          and ANLN1 = iseg-ANLN1
          and ANLN2 = iseg-ANLN2.

      write:/ 'ANLN1:', itab-belnr, iseg-anln1, '-->', iorder.
      if p_run = 'X'.
        bseg-aufnr = iorder.
        select single * from bseg
           where bukrs = p_bukrs
             and gjahr = p_gjahr
             and belnr = itab-belnr
             and buzei = iseg-buzei.

        bseg-aufnr = iorder.

        update bseg.
        write: '... updated'.
      endif.
    endloop.


  endloop.
