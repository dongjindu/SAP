*----------------------------------------------------------------------*
*   INCLUDE ZXF08U01                                                   *
*----------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*&   Program: ZXF08U01.
*&   Author:  Shiva Gnanaguru
*&   Specification: Provide detail information supporting each FI
*&                  transaction with Material document info. Add "Z"
*&                  segment to capture quantity and UOM.
*&--------------------------------------------------------------------&*
*& Date        User         Transport        Description
*& 05/20/2004  100471       UD1K910598       initial program
*& 04/04/2005  100471       UD1K915363       Get companycode.
*& 04/04/2005  100471       UD1K915365       Include year as key.
*& 04/05/2005  100471       UD1K915379       Add currency key value.
*& 05/02/2005  100471       UD1K915820       Check number of segment
*&                              for 'E1EDP02' and don't include PO info.
*& 06/27/2006  Manju        UD1K921176       Add Reason code for credit
*&                                           memo's.
*&--------------------------------------------------------------------&*

data: begin of wa_det_info,
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
        grund like mseg-grund,
      end of wa_det_info.

data: wa_edidd    like edidd,
      wa_e1edp03 like e1edp03,
      wa_e1edp02 like e1edp02,
      wa_new_e1edp02 like e1edp02,
      wa_e1idpu3 like e1idpu3,
      wa_e1iduu5 like e1iduu5,
      wa_e1iduu4 like e1iduu4,
      wa_e1edw03 like e1edw03,
      wa_e1iduu2 like e1iduu2,
      wa_ze1idu5 like ze1idu5.

data: t_det_info like table of wa_det_info,
      it_edidd like table of wa_edidd.

statics: w_gjahr like bkpf-gjahr,
         w_no_seg  type i.             "Max. no of segment = 4.

data: w_belnr like bkpf-belnr,
      w_bktxt like bkpf-bktxt,
      w_bukrs like bkpf-bukrs,
      w_waers like regup-waers,
      w_retcode type i.

case segment_name.
  when 'E1EDP03'.
    wa_e1edp03 = segment_data.
    w_no_seg = 0.
    check wa_e1edp03-iddat eq '015'.
    w_gjahr = wa_e1edp03-datum(4).
  when 'E1EDP02'.
    wa_e1edp02 = segment_data.
    w_no_seg = w_no_seg + 1.
    check wa_e1edp02-qualf eq '010'.
    w_belnr = wa_e1edp02-belnr.
    w_bukrs = reguh_data-zbukr.
    w_waers = regup_data-waers.
    perform get_additional_data tables t_det_info
                                using w_bukrs w_belnr w_gjahr
                                changing w_bktxt
                                         w_retcode.
    if w_retcode eq 4 or w_retcode eq 5.
*&-----add FI Doc.header text.
      if w_no_seg >= 4.                           "Max no of seg = 4.
      else.
        w_no_seg = w_no_seg + 1.
        wa_new_e1edp02-qualf = '070'.
        wa_new_e1edp02-ihrez = w_bktxt.
        wa_edidd-segnam = 'E1EDP02'.
        wa_edidd-sdata  = wa_new_e1edp02.
        append wa_edidd to edidd_table.
      endif.
      clear: wa_new_e1edp02, wa_edidd.
*&-----add segment e1idpu3.
      wa_e1idpu3-dlizeiqu = '001'.
      wa_e1idpu3-dlirupos = '001'.
      wa_edidd-segnam = 'E1IDPU3'.
      wa_edidd-sdata = wa_e1idpu3.
      append wa_edidd to edidd_table.
      clear: wa_new_e1edp02, wa_edidd.
**&----add segment E1IDUU2.
      if w_retcode = 5.
        read table t_det_info into wa_det_info index 1.
        wa_e1iduu2-ajtgrund = wa_det_info-grund.
      else.
        wa_e1iduu2-ajtgrund = ' L7'.
      endif.
      wa_edidd-segnam = 'E1IDUU2'.
      wa_edidd-sdata  = wa_e1iduu2.
      append wa_edidd to edidd_table.
      clear: wa_e1iduu2, wa_edidd.
      exit.
    elseif w_retcode eq 8.
      exit.
    endif.
*&-----add additional information.
    read table t_det_info into wa_det_info index 1.
*&-----add FI Doc.header text.
    if w_no_seg >= 4.
    else.
      wa_new_e1edp02-qualf = '070'.
      wa_new_e1edp02-ihrez = w_bktxt.
      wa_edidd-segnam = 'E1EDP02'.
      wa_edidd-sdata  = wa_new_e1edp02.
      append wa_edidd to edidd_table.
    endif.
    clear: wa_new_e1edp02, wa_edidd.

    loop at t_det_info into wa_det_info.
*&-----add segment e1idpu3.
      wa_e1idpu3-dlizeiqu = '001'.
      wa_e1idpu3-dlirupos = wa_det_info-bwart.
      wa_edidd-segnam = 'E1IDPU3'.
      wa_edidd-sdata = wa_e1idpu3.
      append wa_edidd to edidd_table.
      clear: wa_e1idpu3, wa_edidd.
*&-----add segment ze1idu5.
      wa_ze1idu5-menge = wa_det_info-menge.
      wa_ze1idu5-meins = wa_det_info-meins.
      wa_ze1idu5-ihrez = wa_det_info-asnno.
      wa_edidd-segnam = 'ZE1IDU5'.
      wa_edidd-sdata  = wa_ze1idu5.
      append wa_edidd to edidd_table.
      clear: wa_ze1idu5, wa_edidd.
**&-----add segment E1IDUU5.
      if wa_det_info-shkzg = 'H'.
        wa_e1iduu5-moaqual = '016'.
      else.
        wa_e1iduu5-moaqual = '004'.
      endif.
      wa_e1iduu5-moabetr = wa_det_info-wrbtr.
      wa_e1iduu5-cuxwaerz = w_waers.
      wa_edidd-segnam = 'E1IDUU5'.
      wa_edidd-sdata  = wa_e1iduu5.
      append wa_edidd to edidd_table.
      clear: wa_e1iduu5, wa_edidd.
**&----add segment E1IDUU4.
      wa_e1iduu4-pianummr = wa_det_info-matnr.
      wa_edidd-segnam = 'E1IDUU4'.
      wa_edidd-sdata  = wa_e1iduu4.
      append wa_edidd to edidd_table.
      clear: wa_e1iduu4, wa_edidd.
**&----add segment E1EDW03.
      wa_e1edw03-iddat = '032'.
      wa_e1edw03-datum = wa_det_info-bldat.
      wa_edidd-segnam = 'E1EDW03'.
      wa_edidd-sdata  = wa_e1edw03.
      append wa_edidd to edidd_table.
      clear: wa_e1edw03, wa_edidd.
**&----add segment E1IDUU2.
      wa_e1iduu2-ajtgrund = wa_det_info-grund.
      wa_e1iduu2-rffnummr = wa_det_info-mblnr.
      wa_edidd-segnam = 'E1IDUU2'.
      wa_edidd-sdata  = wa_e1iduu2.
      append wa_edidd to edidd_table.
      clear: wa_e1iduu2, wa_edidd.
    endloop.
endcase.
refresh t_det_info.
