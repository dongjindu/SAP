*----------------------------------------------------------------------*
*   INCLUDE ZXMRNU01                                                   *
*----------------------------------------------------------------------*
* Program Name      : ZXMRNU01
* Author            : Shiva
* Specifications    : Add additional information by using segments
*                     not used by standard SAP program.
*&--------------------------------------------------------------------&*
*& Date        Developer    RequestNo    Description
*& 06/16/2004  Shiva        UD1K911131   initial program.
*& 03/04/2005  Shiva        UD1K914704   Segment 'E1EDK02' with
*&                           qualifier '063' inserted in wrong position.
*& 04/08/2005  Shiva        UD1K915462   Clear the statics variables
*&                                   once current invoice is processed.
*& 08/30/2005  Shiva        UD1K917477  No need to check for the same
*&                                  ASN in the Invoice - as per Richard.
*& 01/25/2006  Manju        UD1K919050  Populate E1EDKT1/E1EDKT2/E1EDP05
*&                                      segments for retro pricing idoc
*&                                      which has O/P RAP6 and set
*&                                      reason code as 99 for
*&                                      retro pricing idoc in segment
*&                                      E1EDKT1 (Qualif PMD )
*&                                      Refer issue log 20060125-001
*&02/03/2006  Manju        UD1K919231   Pad Reason code with leading
*&                                      zero's
*&03/10/2006  Manju        UD1K919691   Pick latest ref material
*&                                      document with Credit
*&03/10/2006  Manju        UD1K919701   check for O/P type while picking
*&                                      Ref. material document
*&--------------------------------------------------------------------&*
data: w_vbeln like likp-vbeln,
      w_ormng like ekes-ormng,
      w_qualf like e1edk02-qualf,
      w_mblnr like mseg-mblnr,
      w_zeile like mseg-zeile,
      w_shkzg like rseg-shkzg,
      w_seg_idx type i.

statics: wa_e1edp01 like e1edp01,
         w_borgr_grp like likp-borgr_grp,
         w_belnr like rbkp-belnr,
         w_gjahr like rbkp-gjahr,
         w_ebeln like ekes-ebeln,
         w_ebelp like ekes-ebelp,
         w_bwart like mseg-bwart,
         w_grund like mseg-grund,
         w_prev_asnno like likp-borgr_grp,
         w_space(27) type c value space,
         w_p01_idx   type i.

data: wa_e1edk01 like e1edk01,
      wa_e1edk03 like e1edk03,
      wa_e1edk02 like e1edk02,
      wa_e1edp02 like e1edp02,
      wa_e1edkt1 like e1edkt1,
      wa_e1edkt2 like e1edkt2,
      wa_e1edp05 like e1edp05,                              "UD1K919050
      wa_edidd like edidd.

* Begin of changes -  UD1K919050
* check if_ident eq 'E' .  "UD1K919050
* Ehancement is extended to Revaluation  which was earlier only for ERS
check if_ident eq 'E' or                                    "UD1K919050
     ( if_ident eq 'N' and IS_DATA_RAP-S_NAST-KSCHL EQ 'RAP6' ).
* End of changes -  UD1K919050
case if_segnam.
  when 'E1EDK01'.
    read table ct_idoc_data into wa_edidd
                            with key segnam = 'E1EDK01'.
    if sy-subrc eq 0.
      wa_e1edk01 = wa_edidd-sdata.
      w_belnr    = wa_e1edk01-belnr.
    else.
      clear w_belnr.
    endif.
    clear wa_edidd.
  when 'E1EDK03'.
    check w_gjahr is initial.
    loop at ct_idoc_data into wa_edidd
                         where segnam = 'E1EDK03'.
      wa_e1edk03 = wa_edidd-sdata.
** Changed by Furong on 01/04/08  for year issue
*      if wa_e1edk03-iddat = '016'.
      if wa_e1edk03-iddat = '015'.
** End of change
        w_gjahr = wa_e1edk03-datum(4).
      endif.
    endloop.
    clear wa_edidd.
  when 'E1EDP01'.
    loop at ct_idoc_data into wa_edidd
                         where segnam = 'E1EDP01'.
      w_p01_idx = sy-tabix.
    endloop.
    wa_e1edp01 = wa_edidd-sdata.
  when 'E1EDP02'.
    describe table ct_idoc_data lines w_seg_idx.
    read table ct_idoc_data into wa_edidd index w_seg_idx.
    wa_e1edp02 = wa_edidd-sdata.
    case wa_e1edp02-qualf.
      when '001'.
        w_ebeln = wa_e1edp02-belnr.
        w_ebelp = wa_e1edp02-zeile.
      when '010'.
        w_mblnr = wa_e1edp02-belnr.
        w_zeile = wa_e1edp02-zeile.
*&---add ASN number.
        select single vbeln borgr_grp into (w_vbeln,w_borgr_grp)
                                from likp as t1
                                inner join mkpf as t2
                                on t1~vbeln = t2~xblnr
                                where mblnr = w_mblnr.
        if sy-subrc eq 0.
*&---pass the original quantity.
          select single ormng into w_ormng
                              from ekes
                              where ebeln = w_ebeln
                              and   ebelp = w_ebelp
                              and   vbeln = w_vbeln.
          if sy-subrc eq 0.
            clear wa_edidd.
            wa_e1edp01-bmng2 = w_ormng.
            wa_edidd-sdata = wa_e1edp01.
            modify ct_idoc_data from wa_edidd index w_p01_idx
                                              transporting sdata.
            clear wa_edidd.
          endif.
*&---Check for same ASN in one invoice; if not then error.
*          if w_prev_asnno is initial.
*            w_prev_asnno = w_borgr_grp.
*          elseif w_prev_asnno ne w_borgr_grp.
*            raise error_message_received.
*          endif.
        endif.
*&---pass Movement type and Reason code information.
        select single shkzg into w_shkzg
                            from rseg
                            where belnr = w_belnr
                            and   gjahr = w_gjahr.

        if w_shkzg eq 'S'.
          select single bwart grund into (w_bwart,w_grund)
                                    from mseg
                                    where mblnr = w_mblnr
                                    and   lfpos = w_zeile.
        else.
          if  IS_DATA_RAP-S_NAST-KSCHL eq 'RAP6'.           "UD1K919701

            select single bwart grund into (w_bwart,w_grund)
                                     from mseg
                                    where mblnr = ( select max( mblnr )
                                                       from mseg
                                                  where lfbnr = w_mblnr
                                                  and lfpos = w_zeile ).

          else.                                             "UD1K919701

            select single bwart grund into (w_bwart,w_grund)
                                    from mseg
                                  where mblnr = ( select max( mblnr )
                                                      from mseg
                                                where lfbnr = w_mblnr
                                                 and lfpos = w_zeile
                                                and SHKZG = w_shkzg ).
          endif.                                            "UD1K919701
* End of changes - UD1K919691
        endif.
    endcase.
* Begin of changes - UD1K919050
* Populate E1EDP05 Segment for Retro pricing IDOC's which have O/p type
* as RAP6.(Normally Retro Pricing IDOC's don't have E1EDP05 segment)
  when 'E1EDP04'.
    if if_ident eq 'N' and IS_DATA_RAP-S_NAST-KSCHL EQ 'RAP6' .
      describe table ct_idoc_data lines w_seg_idx.
      w_seg_idx = w_seg_idx + 1.
      read table ct_idoc_data into  wa_edidd index w_p01_idx.
      if sy-subrc eq 0.
        wa_e1edp01 = wa_edidd-sdata.
        wa_E1EDP05-KSCHL =  'PB00'.
        wa_E1EDP05-KOTXT = 'Gross Price'.
        wa_E1EDP05-KRATE = wa_e1edp01-netwr / wa_e1edp01-menge.
        condense wa_E1EDP05-KRATE.
        if wa_E1EDP05-KRATE <= 0.
          wa_E1EDp05-ALCKZ = '-'.
        else.
          wa_E1EDp05-ALCKZ = '+'.
        endif.
        wa_E1EDP05-UPRBS = wa_e1edp01-peinh.
        wa_E1EDP05-MEAUN = wa_e1edp01-PMENE.
        wa_edidd-segnam = 'E1EDP05'.
        wa_edidd-sdata = wa_e1edp05.
        insert wa_edidd into ct_idoc_data index w_seg_idx.
        clear w_seg_idx.
      endif. .
    endif.
* End of changes - UD1K919050
  when space.
*&---Insert ASN Number with qualifier.
    read table ct_idoc_data transporting no fields
                                 with key segnam = 'E1EDK02'.
    if sy-subrc ne 0.
      loop at ct_idoc_data transporting no fields
                           where segnam = 'E1EDKA1'.
        w_seg_idx = sy-tabix.
      endloop.
    else.
      w_seg_idx = sy-tabix.
    endif.
    w_seg_idx = w_seg_idx + 1.
    clear wa_edidd.
    wa_e1edk02-qualf = '063'.
    wa_e1edk02-belnr = w_borgr_grp.
    wa_edidd-segnam = 'E1EDK02'.
    wa_edidd-sdata = wa_e1edk02.
    insert wa_edidd into ct_idoc_data index w_seg_idx.
    clear w_seg_idx.
*&---Insert movement type with qualifier.
    read table ct_idoc_data transporting no fields
                                 with key segnam = 'E1EDKT2'.
    w_seg_idx = sy-tabix.
    w_seg_idx = w_seg_idx + 1.
    clear wa_edidd.
    wa_e1edkt1-tdid = '007'.
    wa_e1edkt1-tsspras = 'E'.
    wa_edidd-segnam = 'E1EDKT1'.
    wa_edidd-sdata = wa_e1edkt1.
    insert wa_edidd into ct_idoc_data index w_seg_idx.

    clear wa_edidd.
    w_seg_idx = w_seg_idx + 1.
    wa_e1edkt2-tdline = w_bwart.
    wa_edidd-segnam = 'E1EDKT2'.
    wa_edidd-sdata = wa_e1edkt2.
    insert wa_edidd into ct_idoc_data index w_seg_idx.
*&---Insert Reason code with qualifier.
    clear wa_edidd.
    w_seg_idx = w_seg_idx + 1.
    wa_e1edkt1-tdid = 'PMD'.
    wa_e1edkt1-tsspras = 'E'.
    wa_edidd-segnam = 'E1EDKT1'.
    wa_edidd-sdata = wa_e1edkt1.
    insert wa_edidd into ct_idoc_data index w_seg_idx.

    clear wa_edidd.
    w_seg_idx = w_seg_idx + 1.
* Begin of changes - UD1K919050
* Set reason code as 99 for retro pricing IDOC's
    if if_ident eq 'N' and IS_DATA_RAP-S_NAST-KSCHL EQ 'RAP6' .
      wa_e1edkt2-tdline = '0099'.                           "UD1K919231
    else.
      wa_e1edkt2-tdline = w_grund.
    endif.
* End of change s- UD1K919050
    wa_edidd-segnam = 'E1EDKT2'.
    wa_edidd-sdata = wa_e1edkt2.
    insert wa_edidd into ct_idoc_data index w_seg_idx.


    clear: wa_e1edp01,w_belnr,w_gjahr,w_ebeln,w_ebelp,w_prev_asnno,
           w_bwart,w_grund,w_seg_idx, w_borgr_grp, w_p01_idx.

endcase.
