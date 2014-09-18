REPORT ZZ_COMPLETE_ASN .

tables: ekpo, vbfa.
select-options:
s_ebeln for ekpo-ebeln.
parameters:
datum like ekes-eindt,
p_test as checkbox default 'x'.
data:  int_bstae like t163l occurs 0 with header line,
       int_ekpo like ekpo occurs 0 with header line, n like sy-tabix,
       int_ekes like ekes occurs 0 with header line,
       it_eket LIKE beket OCCURS 0 WITH HEADER LINE,
       cor_ekes like ekes occurs 0 with header line.

select * from ekpo  into table int_ekpo
                    where ebeln in s_ebeln and loekz eq ' '.

loop at int_ekpo.
  clear: int_ekes, cor_ekes. refresh: int_ekes, cor_ekes, it_eket.
  select * from ekes into table int_ekes where ebeln eq int_ekpo-ebeln
                                           and ebelp eq int_ekpo-ebelp
                                           and eindt le datum.
  loop at int_ekes.
    if int_ekes-menge = int_ekes-dabmg.
      continue.
    else.
*     Prüfen, ob bereits WEs gebucht wurden und nur dann korregieren
      if int_ekes-dabmg = 0.
        select single * from vbfa where vbelv = int_ekes-vbeln
                                  and   posnv = int_ekes-vbelp
                                  and   vbtyp_n = 'R'.
        if sy-subrc ne 0.
          continue.
        endif.
      endif.
  write:/ int_ekes-ebelp, int_ekes-ebelp, int_ekes-etens,int_ekes-vbeln,
                                   int_ekes-menge, '->', int_ekes-dabmg.
      move-corresponding int_ekes to cor_ekes.
      cor_ekes-menge = cor_ekes-dabmg.
      append cor_ekes.
      n = n + 1.
    endif.
  endloop.

  if p_test eq ' '.
    CALL FUNCTION 'ENQUEUE_EMEKPOE'
         EXPORTING
              EBELN          = int_ekpo-ebeln
              EBELP          = int_ekpo-ebelp
         EXCEPTIONS
              FOREIGN_LOCK   = 2
              SYSTEM_FAILURE = 3.
    if sy-subrc eq 0.
      loop at cor_ekes.
        Write:/'sa-/poitem:', cor_ekes-ebeln, cor_ekes-ebelp,
                cor_ekes-etens, 'updated'.
        update ekes set menge = cor_ekes-menge
        where ebeln eq cor_ekes-ebeln
          and ebelp eq cor_ekes-ebelp
          and etens eq cor_ekes-etens.
      endloop.

*Neuverteilung
      REFRESH it_eket.
      SELECT * FROM eket INTO TABLE it_eket
               WHERE ebeln = int_ekpo-ebeln
               AND   ebelp = int_ekpo-ebelp.

      CALL FUNCTION 'ME_CONFIRMATIONS_REFRESH'.
      CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
           EXPORTING
                i_bstae = int_ekpo-bstae
                i_ebeln = int_ekpo-ebeln
                i_ebelp = int_ekpo-ebelp
                i_funkt = 'DV'
                i_werks = int_ekpo-werks
           TABLES
                xeket   = it_eket.

      LOOP AT it_eket WHERE updkz <> space.
        UPDATE eket SET dabmg = it_eket-dabmg
                    WHERE ebeln = it_eket-ebeln
                    AND   ebelp = it_eket-ebelp
                    AND   etenr = it_eket-etenr.
      ENDLOOP.

      CALL FUNCTION 'DEQUEUE_EMEKPOE'
           EXPORTING
                EBELN  = int_ekpo-ebeln
                EBELP  = int_ekpo-ebelp
           EXCEPTIONS
                OTHERS = 1.
    endif.
  endif.
endloop.

write:/ n,'items selected/updated'.





















































































