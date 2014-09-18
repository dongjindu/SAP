PROGRAM ZMMKNPDF .

*---------------------------------------------------------------------*
*       FORM get_pdfbc                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_INTAB                                                      *
*  -->  IT_OUTTAB                                                     *
*---------------------------------------------------------------------*
form get_pdfbc tables it_intab structure itcsy
                      it_outtab structure itcsy.

*&--------------------------------------------------------------------&*
*&                        IMPORTANT                                   &*
*& Please don't change the special character(s) unless there is       &*
*& a change in the Auto Industry standards. These character(s) are    &*
*& industry standard for printing PDF format barcode.                 &*
*&--------------------------------------------------------------------&*
  data: wa_intab like itcsy,
        wa_outtab like itcsy.

  data: w_pknum like pkhd-pknum,
        w_pkkey like pkps-pkkey,
        w_matnr like pkhd-matnr,
        w_werks like pkhd-werks,
        w_meins like karte-meins,
        w_lgort like karte-lgort,
        w_prvbe like pkhd-prvbe,
        w_ablad like pkhd-ablad,
        w_menge type p decimals 0,
        w_behmg(13) type c,
        w_pdfbc(125) type c,
        w_ablad1(10) type c,
        w_ablad2(14) type c,
        w_pos type i,
        w_pos1 type i,
        w_pos2 type i.

  constants: c_spl_char1(10) type c value '[)>*06:15K',
             c_spl_char2(4)  type c value ':25K',
             c_spl_char3(2)  type c value ':P',
             c_spl_char4(3)  type c value ':7Q',
             c_spl_char5(4)  type c value ':20L',
             c_spl_char6(4)  type c value ':22L',
             c_spl_char7(4)  type c value ':21L',
             c_spl_char8     type c value '*'.

  loop at it_intab into wa_intab.
    case wa_intab-name.
      when 'PKPS-PKKEY'.
        w_pkkey = wa_intab-value.
      when 'PKHD-PKNUM'.
        w_pknum = wa_intab-value.
      when 'KARTE-MATNR'.
        w_matnr = wa_intab-value.
      when 'KARTE-BEHMG'.
        w_menge = wa_intab-value.
        w_behmg = w_menge.
        condense w_behmg.
      when 'KARTE-MEINS'.
        w_meins = wa_intab-value.
      when 'KARTE-LGORT'.
        w_lgort = wa_intab-value.
      when 'KARTE-WERKS'.
        w_werks = wa_intab-value.
      when 'KARTE-PRVBE'.
        w_prvbe = wa_intab-value.
      when 'PKHD-ABLAD'.
        w_ablad = wa_intab-value.
    endcase.
  endloop.
  split w_ablad at '*' into w_ablad1 w_ablad2.
  concatenate c_spl_char1 w_pkkey c_spl_char2 w_pknum
              c_spl_char3 w_matnr c_spl_char4 w_behmg w_meins
              c_spl_char5 w_werks c_spl_char6 w_prvbe
              c_spl_char7 w_lgort
              c_spl_char8 into w_pdfbc.

  loop at it_outtab into wa_outtab.
    case wa_outtab-name.
      when 'P_PDFBC'.
        wa_outtab-value = w_pdfbc.
      when 'P_ABLAD1'.
        wa_outtab-value = w_ablad1.
      when 'P_ABLAD2'.
        wa_outtab-value = w_ablad2.
    endcase.
    modify it_outtab from wa_outtab index sy-tabix
                     transporting value.
    clear wa_outtab.
  endloop.

endform.
