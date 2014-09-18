*----------------------------------------------------------------------*
***INCLUDE ZXF08F01 .
*----------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&   Program: ZXF08F01.
*&   Author:  Shiva Gnanaguru
*&   Specification: Provide detail information supporting each FI
*&                  transaction with Material document info. Add "Z"
*&                  segment to capture quantity and UOM.
*&--------------------------------------------------------------------&*
*& Date        User         Transport        Description
*& 05/20/2004  100471       UD1K910598       initial program
*& 04/04/2005  100471       UD1K915363       Changed the logic to get
*                              the Invoice(RSEG) info. against Matl.doc.
*& 04/04/2005  100471       UD1K915365       Add year as Key.
*& 04/05/2005  100471       UD1K915379      No need to refer clearing
*&                                     document info.Add currency key.
*&--------------------------------------------------------------------&*

data: begin of wa_rseg,
           belnr like rseg-belnr,
           buzei like rseg-buzei,
           ebeln like rseg-ebeln,
           ebelp like rseg-ebelp,
           wrbtr like rseg-wrbtr,
           menge like rseg-menge,
           meins like rseg-meins,
           lfbnr like rseg-lfbnr,
           lfpos like rseg-lfpos,
           lfgja like rseg-lfgja,
           shkzg like rseg-shkzg,
           lifnr like rseg-lifnr,
      end of wa_rseg.
data: begin of wa_bseg,
        belnr like bseg-belnr,
        buzei like bseg-buzei,
        augbl like bseg-augbl,
        dmbtr like bseg-dmbtr,
        menge like bseg-menge,
        meins like bseg-meins,
        ebeln like bseg-ebeln,
        ebelp like bseg-ebelp,
        xref3 like bseg-xref3,
      end of wa_bseg.
data: begin of wa_bsas,
        zuonr like bsas-zuonr,
        gjahr like bsas-gjahr,
        belnr like bsas-belnr,
        buzei like bsas-buzei,
        xref3 like bsas-xref3,
      end of wa_bsas.
data: begin of wa_bkpf,
        belnr like bkpf-belnr,
        gjahr like bkpf-gjahr,
        bktxt like bkpf-bktxt,
        awkey like bkpf-awkey,
      end of wa_bkpf.
data: begin of wa_bseg1,
        augbl like bseg-augbl,
      end of wa_bseg1.
data: begin of wa_dep_doc,
        belnr like bsas-belnr,
        buzei like bsas-buzei,
        gjahr like bsas-gjahr,
        xref3 like bsas-xref3,
        ebeln like bseg-ebeln,
        ebelp like bseg-ebelp,
        mblnr like mkpf-mblnr,
        bldat like mkpf-bldat,
        mjahr like mkpf-mjahr,
        bwart like mseg-bwart,
        menge like mseg-menge,
        meins like mseg-meins,
        wrbtr like rseg-wrbtr,
        matnr like mseg-matnr,
        shkzg like rseg-shkzg,
        asnno(30) type c,
        grund(4)  type c, "LIKE mseg-grund,
      end of wa_dep_doc.
data: begin of wa_mkpf,
        mblnr like mkpf-mblnr,
        mjahr like mkpf-mjahr,
        bldat like mkpf-bldat,
        xblnr like mkpf-xblnr,
      end of wa_mkpf.
data: begin of wa_mseg,
       mblnr like mseg-mblnr,
       mjahr like mseg-mjahr,
       bwart like mseg-bwart,
       matnr like mseg-matnr,
       shkzg like mseg-shkzg,
       ebeln like mseg-ebeln,
       ebelp like mseg-ebelp,
       grund like mseg-grund,
      end of wa_mseg.
data: begin of wa_likp,
       vbeln like likp-vbeln,
       borgr_grp like likp-borgr_grp,
       lifnr like likp-lifnr,
      end of wa_likp.

* Begin of changes - UD1K921176
data : begin of it_ekbe occurs 0,
       ebeln like ekbe-ebeln,
       ebelp like ekbe-ebelp,
       lfbnr like ekbe-lfbnr,
       end of it_ekbe.
* End of changes - UD1K921176

data: t_rseg like table of wa_rseg,
      t_bseg like table of wa_bseg,
      t_bseg2 like table of wa_bseg,
      t_bsas like table of wa_bsas,
      t_bkpf like table of wa_bkpf,
      t_bseg1 like table of wa_bseg1,
      t_mkpf like table of wa_mkpf,
      t_mseg like table of wa_mseg,
      t_likp like table of wa_likp,
      w_augbl like bseg-augbl,
      w_xblnr like bkpf-xblnr,
      w_awkey  like bkpf-awkey,
      w_blart like bkpf-blart,
      l_shkzg like rseg-shkzg,
      w_gjahr like bkpf-gjahr,
      w_ebeln like rseg-ebeln,
      w_ebelp like rseg-ebelp,
      w_unit_price type p decimals 2,
      w_nolines type i,
      w_tcode like bkpf-tcode,                              "UD1K921176
      w_vbeln like vbrk-vbeln,                              "UD1K921176
      w_fkart like vbrk-fkart,                              "UD1K921176
      w_glvor like bkpf-glvor,
      w_bldat like bkpf-bldat.

ranges r_bschl for bseg-bschl.

*&---------------------------------------------------------------------*
*&      Form  get_additional_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_DETAIL_INFO  text
*      -->P_W_BELNR  text
*      -->P_W_GJAHR  text
*----------------------------------------------------------------------*
form get_additional_data tables p_t_result structure wa_dep_doc
                         using  p_bukrs
                                p_belnr
                                p_gjahr
                         changing p_bktxt
                                  p_retcode.

*&---Posting keys related to Material Doc.
  if r_bschl[] is initial.
    r_bschl-sign = 'I'.
    r_bschl-option = 'EQ'.
    r_bschl-low = '83'.
    append r_bschl.
    r_bschl-low = '93'.
    append r_bschl.
    r_bschl-low = '86'.
    append r_bschl.
    r_bschl-low = '96'.
    append r_bschl.
    r_bschl-low = '40'.
    append r_bschl.
    r_bschl-low = '50'.
    append r_bschl.
  endif.
*&---Get FI Doc. header informaton.
  select single blart gjahr awkey bktxt tcode glvor bldat
    into (w_blart, w_gjahr, w_awkey, p_bktxt,w_tcode, w_glvor, w_bldat)
                      from bkpf
                      where bukrs = p_bukrs
                      and   belnr = p_belnr
                      and   gjahr = p_gjahr.
  if sy-subrc ne 0.
    p_retcode = 8.
    exit.
  endif.
  w_xblnr = w_awkey(10).

  case w_glvor(2).
    when 'SD'.
* Get SD Billing Document /Type for Warrant Reason code
      select single vbeln fkart into (w_vbeln, w_fkart)
              from vbrk where vbeln = w_xblnr.

* Reason code for Warrantly claim - Misc Deduction
      if w_vbeln = w_xblnr and ( w_fkart = 'ZWCA' or
             w_fkart = 'ZWG2' or w_fkart = 'ZWL2' or
             w_fkart = 'ZWRA' ) .
        wa_dep_doc-grund = ' L7'.
* Reason code for Warrantly claim - Allowance Charge error
      elseif w_vbeln = w_xblnr and ( w_fkart = 'ZWS1' or
             w_fkart = 'ZWS2' ).
        wa_dep_doc-grund = ' 02'.
* Otherwise...- Misc Adj.
      else.
        wa_dep_doc-grund = ' CS'.
      endif.

      p_retcode = 5.
      p_t_result-grund = wa_dep_doc-grund.
      append p_t_result.


    when 'RM'.
*&---Get MM Invoice information.
      select r~belnr s~buzei s~ebeln s~ebelp s~wrbtr s~menge
                     s~meins s~lfbnr s~lfpos s~lfgja s~shkzg
                     r~lifnr
             into table t_rseg
             from rbkp as r
             inner join rseg as s
                on r~belnr = s~belnr
               and r~gjahr = s~gjahr
             where r~belnr = w_xblnr
               and r~gjahr = w_gjahr.
      if sy-subrc ne 0.
        perform init_variables.
        p_retcode = 8.
        exit.
      endif.

      loop at t_rseg into wa_rseg.
        wa_dep_doc-belnr = wa_rseg-belnr.
        wa_dep_doc-buzei = wa_rseg-buzei.
        wa_dep_doc-ebeln = wa_rseg-ebeln.
        wa_dep_doc-ebelp = wa_rseg-ebelp.
        wa_dep_doc-mblnr = wa_rseg-lfbnr.
        wa_dep_doc-mjahr = wa_rseg-lfgja.
        wa_dep_doc-menge = wa_rseg-menge.
        wa_dep_doc-meins = wa_rseg-meins.
        wa_dep_doc-wrbtr = wa_rseg-wrbtr.
        wa_dep_doc-shkzg = wa_rseg-shkzg.
        append wa_dep_doc to p_t_result.
      endloop.

* Begin of changes - UD1K921176
* PO History to check Cancelled or reversal.
      if not p_t_result[] is initial.
        select ebeln ebelp lfbnr into table it_ekbe
               from ekbe
               for all entries in p_t_result
               where ebeln = p_t_result-ebeln
               and   ebelp = p_t_result-ebelp
               and   lfbnr = p_t_result-mblnr
               and   bewtp in ('Q','R')
               and   shkzg = 'H'.
        select ebeln ebelp lfbnr appending table it_ekbe
               from ekbeh
               for all entries in p_t_result
               where ebeln = p_t_result-ebeln
               and   ebelp = p_t_result-ebelp
               and   lfbnr = p_t_result-mblnr
               and   bewtp in ('Q','R')
               and   shkzg = 'H'.
      endif.
* End of changes - UD1K921176

*      DATA: l_wadat1 LIKE sy-datum,
*            l_wadat2 LIKE sy-datum.


      if not p_t_result[] is initial.
        select mblnr mjahr bldat xblnr into table t_mkpf
                                 from mkpf
                                 for all entries in p_t_result
                                 where mblnr = p_t_result-mblnr
                                 and   mjahr = p_t_result-mjahr.
**------ get earlist date
*        SORT t_mkpf BY bldat.
*        READ TABLE t_mkpf INDEX 1 INTO wa_mkpf.
*        l_wadat1 = wa_mkpf-bldat.
*        SORT t_mkpf BY bldat descending.
*        READ TABLE t_mkpf INDEX 1 INTO wa_mkpf.
*        l_wadat2 = wa_mkpf-bldat.

        sort t_mkpf by mblnr mjahr.
      else.
        perform init_variables.
        p_retcode = 8.
        exit.
      endif.
      if sy-subrc ne 0.
        perform init_variables.
        p_retcode = 8.
        exit.
      endif.

      clear wa_rseg.

      read table t_rseg index 1 into wa_rseg.
      import t_likp from memory id 'ITAB1'.
      read table t_likp with key lifnr = wa_rseg-lifnr into wa_likp.
      if sy-subrc ne 0.
        if wa_rseg-lifnr is initial. break-point. endif.
        select vbeln borgr_grp lifnr into table t_likp
               from likp
               where lifnr = wa_rseg-lifnr
*                AND ( wadat_ist >= l_wadat1 and wadat_ist <= l_wadat2 )
                 and vbtyp = '7'.
        sort t_likp by vbeln.
        export t_likp to memory id 'ITAB1'.
      endif.

      if not p_t_result[] is initial.
        select mblnr mjahr bwart matnr shkzg ebeln ebelp grund
                                 into table t_mseg
                                 from mseg
                                 for all entries in p_t_result
                                 where mblnr = p_t_result-mblnr
                                 and   mjahr = p_t_result-mjahr
                                 and   ebeln = p_t_result-ebeln
                                 and   ebelp = p_t_result-ebelp.
*                                and   MENGE = p_t_result-MENGE.
        sort t_mseg by mblnr mjahr ebeln ebelp.

      endif.
      if sy-subrc ne 0.
        perform init_variables.
        p_retcode = 8.
        exit.
      endif.

      loop at p_t_result into wa_dep_doc.
        l_shkzg = wa_dep_doc-shkzg.
        read table t_mkpf into wa_mkpf
             with key mblnr = wa_dep_doc-mblnr
                      mjahr = wa_dep_doc-mjahr
             binary search.
        if sy-subrc eq 0.
          wa_dep_doc-bldat = wa_mkpf-bldat.


          if wa_mkpf-xblnr(3) = 'JIS' or wa_mkpf-xblnr = space.
            wa_dep_doc-asnno = wa_mkpf-xblnr.
          else.
          read table t_likp into wa_likp with key vbeln = wa_mkpf-xblnr
                      binary search.
            if sy-subrc eq 0.
              wa_dep_doc-asnno = wa_likp-borgr_grp.
            else.
**---------- if not in buffer, try to search likp
*              SELECT SINGLE vbeln borgr_grp lifnr INTO wa_likp
*                                 FROM likp
*                                 WHERE vbeln = wa_mkpf-xblnr
*                                   AND lifnr = wa_rseg-lifnr
*                                   AND wadat_ist < l_wadat1
*                                   AND vbtyp = '7'.
*              IF sy-subrc = 0.
*                wa_dep_doc-asnno = wa_likp-borgr_grp.
*              ELSE.
              wa_dep_doc-asnno = wa_mkpf-xblnr.
*              ENDIF.
            endif.
          endif.


        else.
          wa_dep_doc-bldat = space.
          wa_dep_doc-asnno = space.
        endif.
       read table t_mseg into wa_mseg with key mblnr = wa_dep_doc-mblnr
                                               mjahr = wa_dep_doc-mjahr
                                               ebeln = wa_dep_doc-ebeln
                                               ebelp = wa_dep_doc-ebelp
                                              binary search.
        if sy-subrc eq 0.
          wa_dep_doc-bwart = wa_mseg-bwart.
          wa_dep_doc-matnr = wa_mseg-matnr.
          wa_dep_doc-shkzg = wa_mseg-shkzg.
          wa_dep_doc-grund = wa_mseg-grund.
        else.
          wa_dep_doc-bwart = wa_mseg-bwart.
          wa_dep_doc-matnr = space.
          wa_dep_doc-shkzg = space.
          wa_dep_doc-grund = space.
        endif.
* Begin of changes -  UD1K921176
        read table it_ekbe with key ebeln =  wa_dep_doc-ebeln
                                    ebelp =  wa_dep_doc-ebelp
                                    lfbnr =  wa_dep_doc-mblnr.
*Reason code for Revaluation
        if  w_tcode = 'MRNB'.
          wa_dep_doc-grund = ' RA'.
*Reason code for Cancel Invoice
        elseif w_tcode = 'MR8M' and wa_dep_doc-belnr = w_xblnr
              and  l_shkzg = 'H'.
          wa_dep_doc-grund = ' 26'.
* Reason code - Return / Cancel
        elseif wa_dep_doc-ebeln = it_ekbe-ebeln and
               wa_dep_doc-ebelp = it_ekbe-ebelp and
               wa_dep_doc-mblnr = it_ekbe-lfbnr and
               wa_dep_doc-belnr = w_xblnr       and
               l_shkzg = 'H'.
          wa_dep_doc-grund = ' RD'.
* Reason code - Misc Adjustment
        elseif l_shkzg = 'H'.
          wa_dep_doc-grund = ' CS'.
        else.
*Normal MM Invoice
          wa_dep_doc-grund = '000'.
        endif.
* End of changes - UD1K921176


        modify p_t_result from wa_dep_doc
          transporting bldat asnno bwart matnr shkzg grund.
      endloop.
*& Group all related documents in sequence.
      sort p_t_result by xref3 belnr gjahr.

      perform init_variables.

    when others.
*&---Do Nothing for FI normal invoice.
      p_retcode = 4.

  endcase.

endform.                    " get_additional_data

*&---------------------------------------------------------------------*
*&      Form  init_variables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form init_variables.

  refresh: t_rseg,t_bseg,t_bseg2,t_bsas,t_bkpf,t_bseg1,t_mkpf,
           t_mseg,t_likp,it_ekbe.

  clear: wa_dep_doc, wa_rseg, wa_bseg, wa_bsas, wa_bkpf, wa_mkpf,
         wa_mseg, wa_likp,w_augbl,w_xblnr,w_blart,w_gjahr,
         w_unit_price,w_nolines,w_tcode,w_vbeln, w_fkart,l_shkzg.

endform.                    " init_variables
