************************************************************************
* Program Name      : ZAFI99_CLAIM_SETTLEMENT
* Author            : Furong Wang
* Creation Date     : 12/06/2005
* Specifications By : Andy Choi
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 06/12/06   Manju        UD1K921052   Report changes
* 04/02/07   Manju        UD1K940245   Program corrections
* 01/20/11   Valerian     UD1K950637   Fix logic so it will recognize
*                                      debit/credit posting
* 08/26/11   Valerian     UD1K952971   Fix issue with material that has
*                                      more than 1 supplier.
* 09/26/11   Yn.kim       UP1K920005   Upgrade Ecc6.+ BDC change.
* 10/11/11   KDM                     BDC change : When input PA segment,
*                                           Add One step(screen 0002) .
*                                      --> Index : KDM01
* 11/11/11   KDM                     Selection data control,
*                                      --> Index : KDM02
* 10/19/11   Valerian     UD1K953244   Get additional G/L account in
*                                      data selections
* 12/06/11   Valerian     UD1K953393   Remove G/L account '0000123201'
*                                      in data selections
************************************************************************
report zafi99_claim_settlement message-id zmpp.


tables: ekko, mbewh, ekbe,
        ztfi_claim_rate,bkpf.                               "UD1K921052
tables: qmel,jest.        " UD1K951235

type-pools slis .
constants: c_return(05) type c value '122',        " return to vendor
           c_return_reverse(05) type c value '123',"reverse of return
           c_scrap(05)  type c value '201',        "scrapped
           c_scrap_reverse(05) type c value '202', "reverse of scrapped
           c_gr         type c value '1',          "transaction type: GR
           c_inv        type c value '2',
           c_hkont      like bsis-hkont value '0000123200',
           c_hkont2     like bsis-hkont value '0000123201', "UD1K953244

* ig.moon 5/4/2010 {
*           c_hkont_2    LIKE bsis-hkont VALUE '0000123205',
* }
           c_ekorg      like ekko-ekorg value 'PU01'.
*-->
data: begin of it_data occurs 0,
      bwart like ekbe-bwart,
      lifnr like ekko-lifnr,
      ebeln like ekbe-ebeln,
      ebelp like ekbe-ebelp,
      vgabe like ekbe-vgabe,
      gjahr like ekbe-gjahr,
      belnr like ekbe-belnr,
      buzei like ekbe-buzei,
      budat like ekbe-budat,
      menge like ekpo-menge,
      meins like ekpo-meins,
      webre like ekpo-webre,
      matnr like ekbe-matnr,
      werks like ekbe-werks,
      dmbtr like bsis-dmbtr,
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
      shkzg like ekbe-shkzg,
      netpr like ekpo-netpr,
      peinh like ekpo-peinh,
      symsg like syst-msgv1,                                "UD1K952971
      end of it_data.

data: begin of it_output occurs 0,
      check(1),
      bwart like ekbe-bwart,
      clmtype like mara-ernam,
      lifnr like ekko-lifnr,

*      dmbtr like bsis-dmbtr,   "2
      ebeln like ekbe-ebeln,
      ebelp like ekbe-ebelp,
      matnr like ekbe-matnr,   "1
      dmbtr like bsis-dmbtr,   "2
      vgabe like ekbe-vgabe,
      gjahr like ekbe-gjahr,
      belnr like ekbe-belnr,
      buzei like ekbe-buzei,
      budat like ekbe-budat,
      meins like ekpo-meins,
      menge like ekpo-menge,
        "1
* by ig.moon 3/28/2011 {
      notif like ekbe-matnr,
* }
      maktx like makt-maktx,
      werks like ekbe-werks,
       "2
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
      kbetr like konp-kbetr,  "rate
      kpein like konp-kpein,  "Price unit
      kmein like konp-kmein,  "EA
      stprs like mbew-stprs,
      peinh like mbew-peinh,  "price unit
      amount like bsis-dmbtr,
      clmrate like ztfi_claim_rate-rate,
      claim_amount like bsis-dmbtr,
      webre like ekpo-webre,  "GR based
      symsg like syst-msgv1,                                "UD1K952971
* by ig.moon 5/15/2013 {
      awkey like bkpf-awkey,
      awkey10(10),
      bktxt like bkpf-bktxt,
* }
      end of it_output.

data: begin of it_notif occurs 0,
      matnr like ekbe-matnr,
      notif like ekbe-matnr,
      rkmng(20),
      end of it_notif.

data: begin of it_return_inv occurs 0,
*      check(1),
*      bwart LIKE ekbe-bwart,
*      clmtype LIKE mara-ernam,
      lifnr like ekko-lifnr,
      ebeln like ekbe-ebeln,
*      EBELP LIKE EKBE-EBELP,
      meins like ekpo-meins,
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
*      belnr LIKE ekbe-belnr,
*      buzei LIKE ekbe-buzei,
      matnr like ekbe-matnr,
      ebelp like ekbe-ebelp,

      budat like ekbe-budat,
      maktx like makt-maktx,
*      vgabe LIKE ekbe-vgabe,
*      gjahr LIKE ekbe-gjahr,
*      budat LIKE ekbe-budat,
      menge like ekpo-menge,
      werks like ekbe-werks,
      dmbtr like bsis-dmbtr,
*      kbetr LIKE konp-kbetr,
*      stprs LIKE mbew-stprs,
      amount like bsis-dmbtr,
*      clmrate LIKE ztfi_claim_rate-rate,
      claim_amount like bsis-dmbtr,
      end of it_return_inv.

data: begin of it_scrap_inv occurs 0,
      lifnr like ekko-lifnr,
      meins like ekpo-meins,
      matnr like ekbe-matnr,
      maktx like makt-maktx,
      menge like ekpo-menge,
      dmbtr like bsis-dmbtr,
*      kbetr LIKE konp-kbetr,
      stprs like mbew-stprs,
      amount like bsis-dmbtr,
*      clmrate LIKE ztfi_claim_rate-rate,
      claim_amount like bsis-dmbtr,
      end of it_scrap_inv.

data: begin of lt_invoice occurs 0,
      lifnr like ekko-lifnr,
      ebeln like ekbe-ebeln,
      ebelp like ekbe-ebelp,
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
*      belnr LIKE ekbe-belnr,
*      buzei LIKE ekbe-buzei,
      matnr like ekbe-matnr,
      budat like ekbe-budat,
      meins like ekpo-meins,
      menge like ekpo-menge,
      dmbtr like bsis-dmbtr,
      amount like bsis-dmbtr,
      claim_amount like bsis-dmbtr,
      webre like ekpo-webre,
      end of lt_invoice.

data: it_ztfi_claim_stl like table of ztfi_claim_stl
       with header line.

data: it_ekbe like ekbe occurs 0 with header line.
data: it_bsis like bsis occurs 0 with header line.
data: begin of it_bseg occurs 0,
        bukrs  like bseg-bukrs,
        belnr  like bseg-belnr,
        gjahr  like bseg-gjahr,
        buzei  like bseg-buzei,
        matnr  like bseg-matnr,
        menge  like bseg-menge,
        meins  like bseg-meins,
        lifnr  like bseg-lifnr,
        werks  like bseg-werks,

      end of it_bseg.


data: no_data type c.
data: w_first_date type d,
      w_last_date  type d,
      ok_code      like sy-ucomm,
      w_repid  like sy-repid,
      w_cnt       type   i,
      w_date(10),
      w_matkl like mara-matkl,
      w_waers like t001-waers,
      w_dmbtr like lt_invoice-dmbtr,
      w_claim_amount like lt_invoice-claim_amount,
      w_model_nf like lt_invoice-dmbtr,
      w_model_cm like lt_invoice-dmbtr,
      w_model_yf like lt_invoice-dmbtr. " by ig.moon 01/08/2010

data : begin of it_bdc occurs 0.
        include structure bdcdata.
data : end of it_bdc.
data : begin of it_mess occurs 0.
        include structure bdcmsgcoll.
data : end of it_mess.

data : headerdata like bapi_incinv_create_header,
       invoicedocnumber like bapi_incinv_fld-inv_doc_no,
       itemdata like table of bapi_incinv_create_item with header line,
       return like table of bapiret2 with header line,
       taxdata like bapi_incinv_create_tax,
       fiscalyear like bapi_incinv_fld-fisc_year.


data : it_message like it_mess occurs 0 with header line.

data : w_mode like ctu_params-dismode value 'E'. "'E'. "A-display 'N' *
*def 'E'

*&spwizard: declaration of tablecontrol 'TC_100' itself
controls: tc_100 type tableview using screen 0100.

data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldcat_fi  type lvc_t_fcat with header line,
       it_fieldcat_co  type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail

data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname    like line of it_fieldcat.

data: wa_save    type c   value 'A',   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

      wa_variant type disvariant.      "for parameter IS_VARIANT

data: wa_custom_control type        scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      grid_container    type ref to cl_gui_custom_container.

*ALV refresh?
data: stable        type lvc_s_stbl.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
class lcl_event_receiver definition deferred.

data: g_event_receiver type ref to lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
class lcl_event_receiver definition.

  public section.
    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
              importing er_data_changed.

    data: error_in_data type c.

endclass.                    "lcl_event_receiver DEFINITION
data :it_lvc  like lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
class lcl_event_receiver implementation.
  method handle_data_changed.

    data: ls_good type lvc_s_modi,
          lv_value type lvc_value,
          l_lifnr like ekko-lifnr,
          w_lifnr like ekko-lifnr,
          lvc_t_row type lvc_t_row.

    error_in_data = space.
    loop at er_data_changed->mt_good_cells into ls_good.
      case ls_good-fieldname.
* check if column Name1 of this row was changed
        when 'LIFNR'.
          call method er_data_changed->get_cell_value
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            importing
              e_value     = lv_value.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.

          w_lifnr = lv_value.
          select single lifnr into l_lifnr from lfa1
            where lifnr = w_lifnr.
          if sy-subrc <> 0.
            call method er_data_changed->add_protocol_entry
              exporting
                i_msgid     = 'SU'
                i_msgno     = '000'
                i_msgty     = 'E'
                i_msgv1     = 'Please input correct vendor code'
                i_msgv2     = '(Value->)'
                i_msgv3     = lv_value
                i_fieldname = ls_good-fieldname
                i_row_id    = ls_good-row_id.
          else.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = ls_good-fieldname
                i_value     = lv_value.
            call method er_data_changed->modify_cell
              exporting
                i_row_id    = ls_good-row_id
                i_fieldname = 'CHECK'
                i_value     = 'X'.

          endif.

      endcase.
    endloop.

*§7.Display application log if an error has occured.
    if error_in_data eq 'X'.
      call method er_data_changed->display_protocol.
    endif.

  endmethod.                    "handle_data_changed

endclass.                    "lcl_event_receiver IMPLEMENTATION



selection-screen begin of block b1 with frame title text-007.
parameters:   p_bukrs  type bukrs obligatory memory id buk,
          p_gjahr  type gjahr obligatory memory id gja,
          p_monat  type monat obligatory.
select-options: s_date  for ekbe-budat no-extension. "    OBLIGATORY.
select-options: s_matnr for ekbe-matnr.
select-options: s_lifnr for ekko-lifnr.
parameters: p_clgl like bseg-hkont default '126001'.
*SELECTION-SCREEN SKIP.
selection-screen begin of line.
parameters: r_1 radiobutton group gp1 ."DEFAULT 'X'.
selection-screen comment 8(8) text-m13.
selection-screen end of line.
selection-screen begin of line.
parameters: r_2 radiobutton group gp1 default 'X'.
selection-screen comment 8(8) text-m14.
selection-screen end of line.
parameters: c_1 as checkbox default  'X'.
selection-screen end of block b1.

*SELECTION-SCREEN SKIP.

selection-screen begin of block b2 with frame title text-008.
parameters: p_pdate like ekbe-budat default sy-datum.
*            p_pterm  TYPE dzterm.
parameters: p_max_l type i default 120.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-009.
selection-screen begin of line.
parameters: r_3 radiobutton group gp2 .
selection-screen comment 8(8) text-m15.
selection-screen end of line.
selection-screen begin of line.
parameters: r_4 radiobutton group gp2 default 'X'.
selection-screen comment 8(8) text-m16.
selection-screen end of line.
selection-screen end of block b3.

at selection-screen.
  perform read_period_date.

start-of-selection.
  perform initial_varible.
  perform read_data.
  if it_data[] is initial.
    message i001 with text-004.
    exit.
  endif.
  perform get_price_data.
  perform zero_balance_check.     "ADD 08/19/2014
  perform process_data.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data.
  if r_1 = 'X'.
    perform read_return_materials.
  else.
    perform read_scrapped_materials.
  endif.
endform.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  read_return_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_return_materials.

  if s_date is initial.
    select  a~bwart b~lifnr a~ebeln a~ebelp a~vgabe
            a~gjahr a~belnr a~buzei a~budat a~menge
            a~matnr a~werks a~dmbtr a~lfbnr a~lfpos
            c~meins c~webre a~shkzg c~netpr c~peinh
     into corresponding fields of table it_data
      from ekbe as a
      inner join ekko as b
         on b~ebeln = a~ebeln
      inner join ekpo as c
         on c~ebeln = a~ebeln
        and c~ebelp = a~ebelp
      where   vgabe  = c_gr         "GR
      and   gjahr  = p_gjahr      "FISCAL YEAR
      and   bwart  in (c_return, c_return_reverse)
      and   bewtp  = 'E'            "GR
      and   budat  ge w_first_date  " PERIOD
      and   budat  le w_last_date   "period
      and   b~lifnr in s_lifnr
      and   b~bukrs = p_bukrs
      and   a~matnr in s_matnr
      and c~knttp = space
      and c~loekz = space.
  else.
    select  a~bwart b~lifnr a~ebeln a~ebelp a~vgabe
            a~gjahr a~belnr a~buzei a~budat a~menge
            a~matnr a~werks a~lfbnr a~lfpos
            c~meins c~webre a~shkzg  c~netpr c~peinh
     into corresponding fields of table it_data
      from ekbe as a
      inner join ekko as b
         on a~ebeln = b~ebeln
      inner join ekpo as c
         on c~ebeln = a~ebeln
        and c~ebelp = a~ebelp
      where   vgabe  = c_gr         "GR
      and   gjahr  = p_gjahr      "FISCAL YEAR
      and   bwart  in (c_return, c_return_reverse)
      and   bewtp  = 'E'            "GR
      and   budat in s_date
      and   b~lifnr in s_lifnr
      and   b~bukrs = p_bukrs
      and   a~matnr in s_matnr
      and c~knttp = space
      and c~loekz = space.
  endif.

  data: begin of lt_ekbe occurs 0,
          bewtp  type bewtp,
          shkzg  type shkzg,
          menge  type menge_d,
        end of lt_ekbe.

  data: l_index type i.

  loop at it_data.
    l_index = sy-tabix.
    refresh lt_ekbe.
    select bewtp shkzg sum( menge )
      into table lt_ekbe
      from ekbe
      where ebeln = it_data-ebeln
        and ebelp = it_data-ebelp
        and ( bewtp = 'E' or bewtp = 'Q' )
      group by bewtp shkzg.

    loop at lt_ekbe.
      if lt_ekbe-bewtp = 'E' and lt_ekbe-shkzg = 'H'.
        lt_ekbe-menge = - lt_ekbe-menge.
      elseif lt_ekbe-bewtp = 'Q' and lt_ekbe-shkzg = 'S'.
        lt_ekbe-menge = - lt_ekbe-menge.
      endif.
      modify lt_ekbe index sy-tabix transporting menge.
    endloop.

    loop at lt_ekbe.
      at last.
        sum.
        if lt_ekbe-menge = 0.
          delete it_data index l_index.
        endif.
      endat.
    endloop.

  endloop.
endform.                    " read_return_materials
*&---------------------------------------------------------------------*
*&      Form  read_scrapped_materials
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_scrapped_materials.
  data: l_line type i,
        lt_a018 like table of a018 with header line.
* reading the scraped material doc

  select * into table it_bsis
    from bsis
    where bukrs  = p_bukrs  "company code

* ig.moon 5/4/2010 {
*     AND hkont  = c_hkont  "G/L account
      and hkont  = c_hkont                                  "UD1K953393
*     AND ( hkont  = c_hkont OR hkont  = c_hkont2 )         "UD1K953393
*     AND hkont  = c_hkont                                  "UD1K953244
*             or hkont  = c_hkont_2 ) "G/L account
* }

      and gjahr  = p_gjahr  "fiscal year
      and monat = p_monat   "posting period
      and blart = 'WC'
      and   budat  ge w_first_date  " PERIOD
      and   budat  le w_last_date.   "period

  if it_bsis[] is not initial.    ""KDM02(11/11/2011)
    select bukrs belnr gjahr buzei matnr menge meins lifnr werks
    into corresponding fields of table it_bseg
      from bseg
      for all entries in it_bsis
      where bukrs = it_bsis-bukrs
        and belnr = it_bsis-belnr
        and gjahr = it_bsis-gjahr
        and buzei = it_bsis-buzei
*        AND KOART = 'M'
        and matnr in s_matnr.
    sort it_bseg by bukrs belnr gjahr buzei.
  endif.

  loop at it_bsis.
    read table it_bseg with key bukrs = it_bsis-bukrs
                                belnr = it_bsis-belnr
                                gjahr = it_bsis-gjahr
*                                MATNR = IT_BSIS-XREF3.
                                buzei = it_bsis-buzei
                                binary search.
    if sy-subrc eq 0.
*      lifnr LIKE ekko-lifnr,
*      ebeln LIKE ekbe-ebeln,
*      ebelp LIKE ekbe-ebelp,
*      vgabe LIKE ekbe-vgabe,
      it_data-gjahr = it_bsis-gjahr.
      it_data-belnr = it_bsis-belnr.
      it_data-buzei = it_bsis-buzei.
      it_data-budat = it_bsis-budat.
      if it_bsis-shkzg = 'H'.
        it_data-dmbtr = it_bsis-dmbtr * -1.
        it_data-menge = it_bseg-menge * -1.
      else.
        it_data-dmbtr = it_bsis-dmbtr.
        it_data-menge = it_bseg-menge.
      endif.
      it_data-meins = it_bseg-meins.
      it_data-matnr = it_bseg-matnr.
      it_data-werks = it_bseg-werks.

*      lfbnr LIKE ekbe-lfbnr.
*      lfpos LIKE ekbe-lfpos.
      if it_data-matnr ='R16N'.
        select single lifnr into it_data-lifnr
        from eord
        where matnr = 'R18N'
*        AND werks = it_data-werks
          and vdatu <= it_data-budat
          and bdatu >= it_data-budat
          and lifnr in s_lifnr.
        if it_data-lifnr is initial.
          select * into table lt_a018
                       from a018
                      where kappl eq 'M'
                        and kschl eq 'PB00'
                        and matnr eq 'R18N'
                        and ekorg = 'PU01'
                        and datab <= it_data-budat
                        and datbi >= it_data-budat.
          describe table lt_a018 lines l_line.
          if l_line = 1.
            it_data-lifnr = lt_a018-lifnr.
          endif.
        endif.
      else.
*       SELECT SINGLE lifnr INTO it_data-lifnr              "UD1K952971
        select distinct lifnr into it_data-lifnr            "UD1K952971
        from eord
        where matnr = it_data-matnr
*        AND werks = it_data-werks
          and vdatu <= it_data-budat
          and bdatu >= it_data-budat.
*         AND lifnr IN s_lifnr.                             "UD1K952971
        endselect.                                          "UD1K952971
* BEGIN OF UD1K952971
        if sy-dbcnt > 1.
          it_data-symsg = 'More than one supplier has been found'.
        endif.
* END OF UD1K952971
        if it_data-lifnr is initial.
          select * into table lt_a018
                       from a018
                      where kappl eq 'M'
                        and kschl eq 'PB00'
                        and matnr eq it_data-matnr
                        and ekorg = 'PU01'
                        and datab <= it_data-budat
                        and datbi >= it_data-budat.
          describe table lt_a018 lines l_line.
          if l_line = 1.
            it_data-lifnr = lt_a018-lifnr.
          endif.
        endif.
      endif.

      check it_data-lifnr in s_lifnr.      ""KDM02(11/11/2011)

      append it_data.
      clear: it_data.
      clear: lt_a018,lt_a018[].
    endif.
  endloop.
endform.                    " read_scrapped_materials
*&---------------------------------------------------------------------*
*&      Form  initial_varible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initial_varible.
  clear: it_ekbe, it_ekbe[],
         it_bsis, it_bsis[],
         no_data.


endform.                    " initial_varible
*&---------------------------------------------------------------------*
*&      Form  read_period_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_period_date.
  data: l_month(02) type n,
        l_date type d.

  l_month = p_monat.
  concatenate p_gjahr l_month '01' into w_first_date.
  l_date = w_first_date.


  call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
    exporting
      day_in            = w_first_date
    importing
      last_day_of_month = w_last_date.
*   EXCEPTIONS
*     DAY_IN_NOT_VALID        = 1
*     OTHERS                  = 2
  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
endform.                    " read_period_date
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form process_data.
  data: begin of lt_temp occurs 0,
     clmtype like mara-ernam,
     lifnr like ekko-lifnr,
     ebeln like ekbe-ebeln,
     ebelp like ekbe-ebelp,
     matnr like ekbe-matnr,
     menge like ekpo-menge,
     end of lt_temp.
  data:wa_output like it_output.
  data: l_index like sy-tabix,
        l_nindex like sy-tabix,
        l_total like it_output-menge.

  data: l_low_d type datum,
        l_high_d type datum.

* BEGIN OF UD1K952971
  data: begin of it_supp occurs 0,
          lifnr type eord-lifnr,
        end of it_supp.
* END OF UD1K952971

  concatenate p_gjahr p_monat '01' into l_low_d.

  call function 'SG_PS_ADD_MONTH_TO_DATE'
    exporting
      months  = '1'
      olddate = l_low_d
    importing
      newdate = l_high_d.

  l_high_d = l_high_d - 1.
  data $flag.

  clear it_notif.
  refresh it_notif.

  sort it_output by clmtype lifnr ebeln ebelp matnr.
  loop at it_output.
    at new matnr.
      $flag = 'X'.
    endat.
    if $flag eq 'X'.

      select * from qmel where matnr eq it_output-matnr
                     and ( qmdab between l_low_d and l_high_d ).

        select * from jest where objnr eq qmel-objnr
                            and  stat = 'E0004'.
*                            AND  chgnr EQ qmel-rkmng.

          it_notif-matnr = it_output-matnr.
          it_notif-notif = jest-objnr.
          it_notif-rkmng = qmel-rkmng.
          collect it_notif.

        endselect.
      endselect.
      clear $flag.
    endif.

  endloop.

  sort it_notif by matnr notif rkmng.
  data $index type i.

  loop at it_output.
*{08/19/2014
    l_index = sy-tabix.
    l_nindex = l_index + 1.
*    read table it_output into wa_output index l_nindex.
*    if it_output-clmtype = wa_output-clmtype and
*       it_output-lifnr = wa_output-lifnr and
*       it_output-ebeln = wa_output-ebeln and
*       it_output-ebelp = wa_output-ebelp and
*       it_output-matnr = wa_output-matnr.
*      l_total = it_output-menge + wa_output-menge.
*      if l_total = 0.
*        delete it_output from l_index to l_nindex.
*        continue.
*      endif.
*    endif.
*}

    read table it_notif with key matnr = it_output-matnr
                                 rkmng = it_output-menge.
    if sy-subrc eq 0.
      $index = sy-tabix.
      it_output-notif = it_notif-notif+4.
* BEGIN OF UD1K952971 - Correct Supplier if more than 1
      select distinct lifnr into table it_supp
        from eord
        where matnr = it_output-matnr
          and vdatu <= it_output-budat
          and bdatu >= it_output-budat.
      if sy-dbcnt > 1.
        select parnr into it_output-lifnr
          from ihpa
        up to 1 rows
        where objnr = it_notif-notif
          and parvw = 'Z5'.
          clear it_output-symsg.
        endselect.
      endif.
* END OF UD1K952971

      modify it_output index l_index transporting notif
                                     lifnr symsg.           "UD1K952971

      clear  it_output-notif.
      delete it_notif index $index.
    endif.

    clear : it_output-awkey,it_output-bktxt.

    select single awkey bktxt into (it_output-awkey,it_output-bktxt)
        from bkpf where bukrs eq 'H201'
                    and  belnr eq it_output-belnr
                    and gjahr eq it_output-gjahr.

    it_output-awkey10 = it_output-awkey(10).
    modify it_output index l_index transporting awkey10 bktxt.

  endloop.

*
  call screen 200.

endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_price_data.
  data: l_knumh like a018-knumh.
  loop at it_data.
    move-corresponding it_data to it_output.
** Added by Furong on 11/15/07
    if r_1 = 'X' and it_data-shkzg = 'S'.
      it_data-menge = it_data-menge * -1.
      it_output-menge = it_output-menge * -1.
      it_output-dmbtr = it_output-dmbtr * -1.
    endif.
** end of addition
** for scrap only
    if r_2 = 'X'.
      if it_data-matnr ='R16N'.
        select single knumh into l_knumh
        from a018
        where kschl = 'PB00'
          and lifnr = it_data-lifnr
          and matnr = 'R18N'
          and ekorg = c_ekorg
          and datab <= it_data-budat
          and datbi >= it_data-budat.
      else.
        select single knumh into l_knumh
           from a018
           where kschl = 'PB00'
             and lifnr = it_data-lifnr
             and matnr = it_data-matnr
             and ekorg = c_ekorg
             and datab <= it_data-budat
             and datbi >= it_data-budat.
      endif.
      if sy-subrc eq 0.
        select single kbetr kpein kmein
        into (it_output-kbetr, it_output-kpein, it_output-kmein )
          from konp
          where knumh = l_knumh
           and kschl = 'PB00'
           and loevm_ko = space.
      endif.

*-- standard price
      select single stprs peinh into (it_output-stprs, it_output-peinh)
        from ckmlcr inner join ckmlhd
                       on ckmlcr~kalnr  = ckmlhd~kalnr
        where ckmlhd~matnr  = it_data-matnr
          and ckmlhd~bwkey  = it_data-werks
          and ckmlcr~bdatj  = p_gjahr
          and ckmlcr~poper  = p_monat
          and ckmlcr~untper = space
          and ckmlcr~curtp  = '10'.

*    SELECT SINGLE stprs peinh INTO (it_output-stprs, it_output-peinh)
*       FROM mbew
*       WHERE matnr = it_data-matnr
*         AND bwkey = it_data-werks
*         AND lvorm = space
*         AND lfgja = p_gjahr
*         AND lfmon <= p_monat.
*
*    IF sy-subrc <> 0.
*      CLEAR mbewh.
*      SELECT *
*       FROM mbewh
*       WHERE matnr = it_data-matnr
*         AND bwkey = it_data-werks
*         AND lfgja  = p_gjahr
*         AND lfmon <= p_monat
*       ORDER BY lfgja DESCENDING
*                lfmon DESCENDING.
*        EXIT.
*      ENDSELECT.
*      IF sy-subrc <> 0.
*        SELECT *
*           FROM mbewh
*           WHERE matnr = it_data-matnr
*             AND bwkey = it_data-werks
*             AND lfgja < p_gjahr
*           ORDER BY lfgja DESCENDING
*                    lfmon DESCENDING.
*          EXIT.
*        ENDSELECT.
*      ENDIF.
*      it_output-stprs = mbewh-stprs.

*      SELECT SINGLE stprs INTO it_output-stprs
*      FROM mbewh
*      WHERE matnr = it_data-matnr
*        AND bwkey = it_data-werks
*        AND lfgja = p_gjahr
*        AND lfmon = p_monat.
*   ENDIF.

      data: l_outqty like it_data-menge.
      if it_output-kmein is initial.

      else.
        perform unit_converion(zaco19u_shop_new)
                            using    it_data-menge
                                     it_data-meins
                                     it_output-kmein
                            changing l_outqty.

        it_output-amount = l_outqty *
                           it_output-kbetr / it_output-kpein.
      endif.
    else.
      it_output-kbetr = it_data-netpr.
      it_output-kpein = it_data-peinh.
      it_output-kmein = it_data-meins.
      it_output-amount = it_data-menge *
                         it_output-kbetr / it_output-kpein.


    endif.

* Begin of changes - UD1K921052
*    SELECT SINGLE MAX( rate ) INTO it_output-clmrate
*      FROM ztfi_claim_rate
*       WHERE bukrs = p_bukrs
*        AND lifnr = it_data-lifnr
*        AND date_from <= it_data-budat.
    select   *
      from ztfi_claim_rate
       where bukrs = p_bukrs
        and lifnr = it_data-lifnr
        and date_from <= it_data-budat
        order by date_from  descending.
      it_output-clmrate =  ztfi_claim_rate-rate.
      exit.
    endselect.
* End of changes - UD1K921052
    it_output-claim_amount = it_output-amount *
                            ( 1 + it_output-clmrate / 100 ).
    select single maktx into it_output-maktx
    from makt
    where matnr = it_data-matnr.
    if it_output-bwart = c_return or
       it_output-bwart = c_return_reverse.
      it_output-clmtype = 'Return'.
    else.
      it_output-clmtype = 'Scrap'.
    endif.
    append it_output.
    clear it_output.
  endloop.

endform.                    " read_info_record

***INCLUDE ZAFI99_CLAIM_SETTLEMENT_FM .
*&spwizard: output module for tc 'TC_100'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module tc_100_change_tc_attr output.
  describe table it_output lines tc_100-lines.
endmodule.                    "tc_100_change_tc_attr OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  data: save_okcode like sy-ucomm.
  save_okcode = ok_code.
  clear: ok_code.
  case save_okcode.
    when 'BACK' or 'EXIT'.
      leave program.
    when 'POST'.
      perform post_invocie.
    when 'SELALL'.
      perform sel_all.
    when 'DESELECT'.
      perform desel_all.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  post_invocie
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_invocie.
  loop at it_output where check  = 'X'.
  endloop.
endform.                    " post_invocie
*&---------------------------------------------------------------------*
*&      Module  set_status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_status output.
  set pf-status '100'.
  set titlebar 'T_100'.
endmodule.                 " set_status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  leave program.
endmodule.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sel_all.
  loop at it_output.
    it_output-check = 'X'.
    modify it_output transporting check.
  endloop.
endform.                    " SEL_ALL
*&---------------------------------------------------------------------*
*&      Form  DESEL_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form desel_all.
  loop at it_output.
    clear: it_output-check.
    modify it_output transporting check.
  endloop.
endform.                    "desel_all

*
**&spwizard: declaration of tablecontrol 'TC_100' itself
*CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 0100.
*
** Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
*INCLUDE zafi99_claim_settlement_fm .
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'ST200'.
  set titlebar 'T200'.
endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_alv_200 output.
  if grid_container is initial. "/Not Created Control for ALV GRID
    perform create_container_n_object_9000.
    perform set_attributes_alv_grid.
    perform build_sortcat_display.
    perform build_field_catalog using 'IT_OUTPUT'.
    perform assign_itab_to_alv_9000.
*    PERFORM sssign_event_9000.
  else.
    stable-row = 'X'.
    stable-col = 'X'.
    call method alv_grid->refresh_table_display
      exporting
        is_stable = stable.
  endif.

endmodule.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  create object grid_container
    exporting
      container_name              = wa_custom_control
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  if sy-subrc ne 0.
    call function 'POPUP_TO_INFORM'
      exporting
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object alv_grid
    exporting
      i_parent      = grid_container
      i_appl_events = 'X'.

endform.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid.

  data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

endform.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1070   text
*----------------------------------------------------------------------*
form build_field_catalog using p_itab.

  data: lw_itab type slis_tabname,
        lw_waers like t001-waers.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.

  lw_itab = p_itab.
  select single waers into lw_waers from t001
     where bukrs = p_bukrs.

  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
      i_bypassing_buffer = 'X'
    changing
      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :

                                  'S' 'CLMTYPE'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Type',
                                  'E' 'OUTPUTLEN'   '5'.
  if r_2 = 'X'.
    perform setting_fieldcat tables it_fieldcat using :
                                    'S' 'LIFNR'       ' ',
                                    ' ' 'KEY'         'X',
                                    ' ' 'EDIT'        'X',
                                    ' ' 'COLTEXT'     'Vendor',
                                    'E' 'OUTPUTLEN'   '10'.
  else.
    perform setting_fieldcat tables it_fieldcat using :
                                     'S' 'LIFNR'       ' ',
                                     ' ' 'KEY'         'X',
                                     ' ' 'COLTEXT'     'Vendor',
                                     'E' 'OUTPUTLEN'   '10'.
  endif.
  perform setting_fieldcat tables it_fieldcat using :
                                   'S' 'EBELN'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'PO Number',
                                   'E' 'OUTPUTLEN'   '10',

                                   'S' 'EBELP'       ' ',
                                   ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'I/No',
                                   'E' 'OUTPUTLEN'   '4',

*                                   'S' 'WEBRE'       ' ',
*                                   ' ' 'COLTEXT'     'GR based IV',
*                                   'E' 'OUTPUTLEN'   '01',

                                   'S' 'MATNR'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Material',
                                   'E' 'OUTPUTLEN'   '18',

                                   'S' 'NOTIF'       ' ',
*                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Notification',
                                   'E' 'OUTPUTLEN'   '18',

                                   'S' 'MAKTX'       ' ',
                                   ' ' 'KEY'         'X',
                                   ' ' 'COLTEXT'     'Text',
                                   'E' 'OUTPUTLEN'   '30',

                                   'S' 'MENGE'       ' ',
*                                  ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'Quantity',
                                   'E' 'OUTPUTLEN'   '13',

                                   'S' 'MEINS'       ' ',
*                                  ' ' 'KEY'         ' ',
                                   ' ' 'COLTEXT'     'UoM',
                                   'E' 'OUTPUTLEN'   '05',

                                   'S' 'DMBTR'       ' ',
                                   ' ' 'DO_SUM'      'X',
                                   ' ' 'COLTEXT'     'Trn Amount',
                                   'E' 'CURRENCY'    lw_waers,

                                   'S' 'KBETR'       ' ',
                                   ' ' 'COLTEXT'     'Info Price',
                                   'E' 'CURRENCY'
                                   lw_waers,

                                   'S' 'KPEIN'       ' ',
                                   ' ' 'COLTEXT'     'Per',
                                   'E' 'OUTPUTLEN'   '04',

                                   'S' 'KMEIN'       ' ',
                                   ' ' 'COLTEXT'     'PUoM',
                                   'E' 'OUTPUTLEN'   '05',

*                                  'S' 'MAKTX'       ' ',
*                                  ' ' 'KEY'         'X',
*                                  'E' 'COLTEXT'     'Description',


                                   'S' 'STPRS'       ' ',
                                   ' ' 'COLTEXT'     'Std Price',
                                   'E' 'CURRENCY'    lw_waers,
                                   'S' 'PEINH'       ' ',
                                   ' ' 'COLTEXT'     'CstLot',
                                   'E' 'OUTPUTLEN'   '04',


                                   'S' 'AMOUNT'       ' ',
                                   ' ' 'DO_SUM'      'X',
                                   ' ' 'COLTEXT'     'Amount(Info)',
                                   'E' 'CURRENCY'    lw_waers,

                                   'S' 'CLMRATE'       ' ',
                                   ' ' 'COLTEXT'     'Rate',
                                   'E' 'CURRENCY'    lw_waers.
  if r_2 = 'X'.
    perform setting_fieldcat tables it_fieldcat using :
                                     'S' 'CLAIM_AMOUNT'       ' ',
                                      ' ' 'EDIT'        'X',
                                     ' ' 'DO_SUM'      'X',
                                     ' ' 'COLTEXT'     'Claim Amount',
                                      '' 'DECIMALS'     '2',
                                      'E' 'CURRENCY'    lw_waers.

  else.
    perform setting_fieldcat tables it_fieldcat using :
                                     'S' 'CLAIM_AMOUNT'       ' ',
                                     ' ' 'DO_SUM'      'X',
                                     ' ' 'COLTEXT'     'Claim Amount',
                                      '' 'DECIMALS'     '2',
                                     'E' 'CURRENCY'    lw_waers.

  endif.

  perform setting_fieldcat tables it_fieldcat using :
                                 'S' 'BELNR'       ' ',
                                 ' ' 'COLTEXT'     'Org.Document',
                                 'E' 'OUTPUTLEN'   '10'.

* BEGIN OF UD1K952971
  perform setting_fieldcat tables it_fieldcat using :
                                   'S' 'SYMSG'       ' ',
                                   ' ' 'COLTEXT'     'Message',
                                   'E' 'OUTPUTLEN'   '50'.
* END OF UD1K952971



  perform setting_fieldcat tables it_fieldcat using :
                                   'S' 'AWKEY10'       ' ',
                                   ' ' 'COLTEXT'     'Material Doc#',
                                   'E' 'OUTPUTLEN'   '10'.

  perform setting_fieldcat tables it_fieldcat using :
                                   'S' 'BKTXT'       ' ',
                                   ' ' 'COLTEXT'     'Doc Header Text',
                                   'E' 'OUTPUTLEN'   '25'.

endform.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv_9000.

  call method alv_grid->set_table_for_first_display

*     i_structure_name = 'ZSCO_COGS_NEW'
   exporting   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     changing  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_output[]
               it_sort          = it_sort[].

** ENTER
  call method alv_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  call method alv_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  create object g_event_receiver.
  set handler g_event_receiver->handle_data_changed for alv_grid.




endform.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sssign_event_9000.
  if sy-batch is initial.
    call method alv_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  endif.

endform.                    " sssign_event_9000
*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat structure it_fieldcat
                      using    p_gubun
                               p_field
                               p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_fieldcat.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check filed catalog'.
    endif.

    move: w_fieldname-fieldname to p_fieldcat-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_FIELDCAT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to w_cnt.
    p_fieldcat-col_pos = w_cnt.
    append p_fieldcat.
  endif.
endform.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  case ok_code.
    when 'EXIT'.
      leave program.
    when 'BACK'.
      leave to screen 0.
    when 'POST'.
      perform post_invoice.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT

*---------------------------------------------------------------------*
*       FORM display_progress_bar                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TEXT                                                        *
*---------------------------------------------------------------------*
form display_progress_bar using    p_text.
  data: lw_text(50).

  move: p_text to lw_text.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = lw_text.

endform.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  post_invoice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_invoice.
  data: lt_rows type lvc_t_row with header line,
         lt_row_no type lvc_t_roid. "/Numeric IDs of Selected Rows
  data: l_line type i.

  call method alv_grid->get_selected_rows
    importing
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
      exporting
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    exit.
  endif.

*
*  CLEAR: w_select, w_success, w_fail.

  read table lt_rows index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m12.
  endif.

  perform get_posting_data tables lt_rows.

  describe table it_return_inv lines l_line.
  if l_line eq 0.
    describe table it_scrap_inv lines l_line.
    if l_line eq 0.
*      MESSAGE s000 WITH text-m01.
*      EXIT.
    endif.
  endif.

  perform bapi_bdc_invoice.

endform.                    " post_invoice

*---------------------------------------------------------------------*
*       FORM get_posting_data                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PT_ROWS                                                       *
*---------------------------------------------------------------------*
form get_posting_data tables pt_rows structure lvc_s_row.
  data: l_year(4),
  l_budat like sy-datum,
        l_stblg like rbkp-stblg.

  clear: it_return_inv, it_return_inv[],
         it_scrap_inv, it_scrap_inv[].

  loop at pt_rows where index ne 0.
    read table it_output index pt_rows-index.
    if sy-subrc ne 0.
      message e000 with text-m01.
    endif.

    if it_output-clmtype = 'Return'.
      select single inv_no budat
         into (it_return_inv-lifnr, l_budat) from ztfi_claim_stl
         where gjahr = p_gjahr
          and lfmon = p_monat
          and lifnr = it_output-lifnr
          and matnr = it_output-matnr
          and ebeln = it_output-ebeln
          and ebelp = it_output-ebelp
          and lfbnr = it_output-lfbnr
          and lfpos = it_output-lfpos.
      if sy-subrc = 0.
        l_year = l_budat+0(4).
        select single stblg into l_stblg
          from rbkp
         where belnr = it_return_inv-lifnr
          and gjahr = l_year.
        if l_stblg is initial.
          w_repid = sy-repid.
          call function 'POPUP_TO_INFORM'
            exporting
              titel = w_repid
              txt1  = 'This return has been posted:'
              txt2  = it_output-matnr
              txt3  = 'Invoice Number is '
              txt4  = it_return_inv-lifnr.
          exit.
        else.
          move-corresponding it_output to it_return_inv.
          collect it_return_inv.
        endif.
      else.
        move-corresponding it_output to it_return_inv.
        collect it_return_inv.
      endif.
      clear: it_return_inv.
    else.
      select single inv_no into it_scrap_inv-lifnr from ztfi_claim_stl
          where gjahr = p_gjahr
           and lfmon = p_monat
           and lifnr = it_output-lifnr
           and matnr = it_output-matnr.
      if sy-subrc = 0.
        w_repid = sy-repid.
        call function 'POPUP_TO_INFORM'
          exporting
            titel = w_repid
            txt1  = 'This scrap has been posted'
            txt2  = it_output-matnr
            txt3  = 'Invoice Number is '
            txt4  = it_scrap_inv-lifnr.
        exit.
      else.
        move-corresponding it_output to it_scrap_inv.
        collect it_scrap_inv.
      endif.
      clear: it_scrap_inv.
    endif.
  endloop.

endform.                    "get_posting_data
*&---------------------------------------------------------------------*
*&      Form  process_invoice_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_bdc_invoice.
  data: wa_invoice like it_return_inv,
        wa_scrap like it_scrap_inv,
        l_line type i.
  clear: w_dmbtr, w_claim_amount.

  clear: it_ztfi_claim_stl, it_ztfi_claim_stl[].
  write: p_pdate to w_date.

  select single waers into w_waers from t001
    where bukrs = p_bukrs.

  if r_1 = 'X'.
** Return
*  SORT it_return_inv BY lifnr ebeln ebelp belnr buzei matnr.

    sort it_return_inv by lifnr ebeln ebelp lfbnr lfpos matnr.
    loop at it_return_inv into wa_invoice.
      at new lifnr.
        lt_invoice-lifnr = wa_invoice-lifnr.
      endat.
      at new ebeln.
        lt_invoice-ebeln = wa_invoice-ebeln.
      endat.
*    AT NEW belnr.
*      lt_invoice-belnr = wa_invoice-belnr.
*    ENDAT.
      at new lfbnr.
        lt_invoice-lfbnr = wa_invoice-lfbnr.
      endat.

      at end of ebelp. "MATNR.
        sum.
        if wa_invoice-dmbtr = 0.
        else.
          lt_invoice-menge = wa_invoice-menge.
          lt_invoice-meins = wa_invoice-meins.
          lt_invoice-dmbtr = wa_invoice-dmbtr.
          lt_invoice-amount = wa_invoice-amount.
          lt_invoice-claim_amount = wa_invoice-claim_amount.
          lt_invoice-ebelp = wa_invoice-ebelp.
*      lt_invoice-buzei = wa_invoice-buzei.
          lt_invoice-lfpos = wa_invoice-lfpos.
          lt_invoice-matnr = wa_invoice-matnr.
          append lt_invoice.
        endif.

        clear: lt_invoice-menge,lt_invoice-dmbtr,
               lt_invoice-claim_amount,lt_invoice-matnr.
      endat.
      at end of lifnr.
        sum.
        w_dmbtr = wa_invoice-dmbtr.
        w_claim_amount = wa_invoice-claim_amount.
        describe table lt_invoice lines l_line.
        if l_line > p_max_l.
          perform call_inv_split.
        else.
          perform call_bapi_inv.
        endif.

*       PERFORM call_bapi_inv.
        clear: lt_invoice, lt_invoice[], w_claim_amount, w_dmbtr.
      endat.
      clear: wa_invoice.
    endloop.

    if not it_ztfi_claim_stl[] is initial.
      modify ztfi_claim_stl from table it_ztfi_claim_stl.
*      INSERT ZTFI_CLAIM_STL FROM TABLE IT_ZTFI_CLAIM_STL.
**                 ACCEPTING DUPLICATE KEYS.
      if sy-subrc <> 0.
        message i001 with 'Updating Z-table error'.
        rollback work.
      else.
        commit work.
      endif.
    endif.

** Scrap
  else.
    sort it_scrap_inv by lifnr matnr.
    loop at it_scrap_inv into wa_scrap.
      at new lifnr.
        lt_invoice-lifnr = wa_scrap-lifnr.
      endat.
      at end of matnr.
        sum.
        lt_invoice-menge = wa_scrap-menge.
        lt_invoice-meins = wa_scrap-meins.
        lt_invoice-dmbtr = wa_scrap-dmbtr.
        lt_invoice-amount = wa_scrap-amount.
        lt_invoice-claim_amount = wa_scrap-claim_amount.
        lt_invoice-matnr = wa_scrap-matnr.
        append lt_invoice.
        clear: lt_invoice-menge,lt_invoice-dmbtr,
               lt_invoice-claim_amount,lt_invoice-matnr.
      endat.
      at end of lifnr.
        sum.
        w_dmbtr = wa_scrap-dmbtr.
        w_claim_amount = wa_scrap-claim_amount.
        describe table lt_invoice lines l_line.
        if l_line > p_max_l.
          perform call_inv_split.
        else.
          perform call_bdc_inv.
        endif.
        clear: lt_invoice, lt_invoice[], w_claim_amount, w_dmbtr.
      endat.
      clear: wa_scrap.
    endloop.
    if not it_ztfi_claim_stl[] is initial.
      insert ztfi_claim_stl from table it_ztfi_claim_stl.
*                 ACCEPTING DUPLICATE KEYS.
*      MODIFY ZTFI_CLAIM_STL FROM TABLE IT_ZTFI_CLAIM_STL.

      if sy-subrc <> 0.
        message i001 with 'Updating Z-table error'.
        rollback work.
      else.
        commit work.
      endif.

    endif.
  endif.
  if sy-subrc = 0.
    commit work.
  else.
    rollback work.
  endif.
endform.                    " process_invoice_post
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bdc_inv.
  data: l_tabix type sy-tabix.                              "UD1K950637
  data: lw_menge_c(13),
        lw_dmbtr_c(13),
        l_meins like ekpo-meins,
        lw_claim_amount_c(13),
        lw_diff_c(13),
        lw_diff_nf_c(13),
        lw_diff_cm_c(13),
        lw_diff_yf_c(13),
        lw_diff like konp-kbetr,
        lw_todate(10),
        l_text(30),
        l_monat(2),
        l_dmbtr like lt_invoice-dmbtr,
        lw_last_rec(1).
  data: lw_paph2 like ztco_model_map-paph2.

  clear: it_bdc, it_bdc[], w_model_nf, w_model_cm, it_mess, it_mess[].
  clear: w_model_yf.

  lw_diff =  w_claim_amount - w_dmbtr.
*  LW_DIFF_C = LW_DIFF.
* lw_diff_c = abs( lw_diff_c ).
  lw_claim_amount_c = abs( w_claim_amount ).
  lw_dmbtr_c = w_dmbtr.
  write: sy-datum to lw_todate.
  l_monat = p_pdate+4(2).

  read table lt_invoice index 1.


  if r_3 = 'X'.  "Parking
    perform bdc_dynpro      using 'SAPLF040' '0100'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BKPF-BLDAT'
                                  w_date.
    perform bdc_field       using 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    perform bdc_field       using 'BKPF-BUKRS'
                                  p_bukrs.
    perform bdc_field       using 'BKPF-BUDAT'
                                 w_date.
    perform bdc_field       using 'BKPF-MONAT'
                                  p_monat.
    perform bdc_field       using 'BKPF-WAERS'
                                  w_waers.
    perform bdc_field       using 'BKPF-XBLNR'
                                  '123200'.
    perform bdc_field       using 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
    perform bdc_field       using 'VBKPF-XBWAE'
                                  'X'.
*  PERFORM bdc_field       USING 'FS006-DOCID'
*                                record-docid_010.
** Change by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05V-NEWBS'
*                                  '24'.
    perform bdc_field       using 'RF05V-NEWBS'
                                  '04'.
** end of change
    perform bdc_field       using 'RF05V-NEWKO'
                                  lt_invoice-lifnr.
** Changed By Furong on 06/29*07
*    PERFORM BDC_DYNPRO      USING 'SAPLF040' '0302'.
    perform bdc_dynpro      using 'SAPLF040' '0301'.
** end of change
    perform bdc_field       using 'BDC_CURSOR'
                                  'RF05V-NEWKO'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    perform bdc_field       using 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    perform bdc_field       using 'BSEG-ZFBDT'
                                  w_date.       "baseline date LW_TODATE
    perform bdc_field       using 'BSEG-ZUONR'
                                  '123200'.          "Assign
    perform bdc_field       using 'BSEG-SGTXT'
                                  '123200 CLAIM SETTLEMENT'.

*    PERFORM bdc_field       USING 'RF05V-NEWBS'
*                                  '50'.
*    PERFORM bdc_field       USING 'RF05V-NEWKO'
*                                  '123200'.
    clear: lw_claim_amount_c.

    loop at lt_invoice.
      l_dmbtr =  lt_invoice-dmbtr.
      at first .
        if l_dmbtr < 0 .
          perform bdc_field       using 'RF05V-NEWBS'
                                        '40'.
        else.
          perform bdc_field       using 'RF05V-NEWBS'
                                     '50'.
        endif.

        perform bdc_field       using 'RF05V-NEWKO'
                                   '123200'.
      endat.
      clear: lw_paph2, w_matkl.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      perform derive_model using lt_invoice-matnr
                           changing lw_paph2.

      case lw_paph2.
        when '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        when '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        when '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        when others.
* ??????????????????????????????????????????????????????????

      endcase.

      perform bdc_dynpro      using 'SAPLF040' '0300'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'BSEG-MENGE'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=ZK'.
      perform bdc_field       using 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      perform bdc_field       using 'BSEG-MENGE'
                                    lw_menge_c.
      perform bdc_field       using 'BSEG-MEINS'
                                    lt_invoice-meins.
      perform bdc_field       using 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      perform bdc_field       using 'BSEG-SGTXT'
                                    lt_invoice-matnr.
      perform bdc_field       using 'DKACB-FMORE'
                                    'X'.             "rd-fmore_024.

      perform bdc_dynpro      using 'SAPLKACB' '0002'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'COBL-MATNR'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=ENTE'.
      perform bdc_field       using 'COBL-MATNR'
                                    lt_invoice-matnr.
      perform bdc_dynpro      using 'SAPLF040' '0330'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'RF05V-NEWKO'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
      at last.
        lw_last_rec = 'X'.
      endat.

      if lw_last_rec is initial.
        if lt_invoice-dmbtr < 0.
          perform bdc_field       using 'RF05V-NEWBS'
                                           '40'.
        else.
          perform bdc_field       using 'RF05V-NEWBS'
                                                '50'.
        endif.
        perform bdc_field       using 'RF05V-NEWKO'
                                      '123200'.
      else.
        clear: lw_last_rec.

        if w_model_cm = 0 or w_model_nf = 0
        or w_model_yf = 0.

          if lw_diff > 0.
            perform bdc_field       using 'RF05V-NEWBS'
                                          '50'.
            lw_diff_c = lw_diff.
          else.
            perform bdc_field       using 'RF05V-NEWBS'
                                         '40'.
            lw_diff_c = abs( lw_diff ).
          endif.
          perform bdc_field       using 'RF05V-NEWKO'
                                        '531100'.
          perform bdc_dynpro      using 'SAPLF040' '0300'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'BSEG-SGTXT'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=ZK'.
          perform bdc_field       using 'BSEG-WRBTR'
                                        lw_diff_c.
          perform bdc_field       using 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          perform bdc_field       using 'DKACB-FMORE'
                                        'X'.

          perform bdc_dynpro      using 'SAPLKACB' '0002'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=ENTE'.
          perform bdc_field       using 'COBL-KOSTL'
                                        '55101'.
          perform bdc_dynpro      using 'SAPLF040' '0330'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=PBBP'.
          perform bdc_dynpro      using 'SAPLKEAK' '0300'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=WEIT'.
          if w_model_cm = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          endif.
          if w_model_nf = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          endif.
          if w_model_yf = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          endif.

        else.

          if w_model_nf > 0.
            perform bdc_field       using 'RF05V-NEWBS'
                                          '50'.
            lw_diff_nf_c = w_model_nf.
          else.
            perform bdc_field       using 'RF05V-NEWBS'
                                         '40'.
            lw_diff_nf_c = abs( w_model_nf ).

          endif.
          perform bdc_field       using 'RF05V-NEWKO'
                                        '531100'.


          perform bdc_dynpro      using 'SAPLF040' '0300'.

          perform bdc_field       using 'BDC_CURSOR'
                                        'BSEG-SGTXT'.

          perform bdc_field       using 'BDC_OKCODE'
                                        '=ZK'.
          perform bdc_field       using 'BSEG-WRBTR'
                                            lw_diff_nf_c.
          perform bdc_field       using 'BSEG-ZUONR'
                                        lt_invoice-lifnr.
          perform bdc_field       using 'DKACB-FMORE'
                                        'X'.             "rd-fmore_024.


          perform bdc_dynpro      using 'SAPLKACB' '0002'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=ENTE'.
          perform bdc_field       using 'COBL-KOSTL'
                                        '55101'.

          perform bdc_dynpro      using 'SAPLF040' '0330'.

*          PERFORM bdc_field       USING 'BDC_CURSOR'
*                                        'RF05V-NEWKO'.

          perform bdc_field       using 'BDC_OKCODE'
                                        '/00'.

** CM 553110
          if w_model_cm  > 0.
            perform bdc_field       using 'RF05V-NEWBS'
                                           '50'.
            lw_diff_cm_c = w_model_cm.
          else.
            perform bdc_field       using 'RF05V-NEWBS'
                                           '40'.
            lw_diff_cm_c = abs( w_model_cm ).
          endif.

          perform bdc_field       using 'RF05V-NEWKO'
                                        '531100'.
          perform bdc_dynpro      using 'SAPLF040' '0300'.
          perform bdc_field       using 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=BP'.
*

          perform bdc_field       using 'BSEG-WRBTR'
                                        lw_diff_cm_c.
          perform bdc_field       using 'DKACB-FMORE'
                                        'X'.

          perform bdc_dynpro      using 'SAPLKACB' '0002'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=ENTE'.
          perform bdc_field       using 'COBL-KOSTL'
                                        '55101'.

          perform bdc_dynpro      using 'SAPLF040' '0330'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '/00'.

** CM 553110
* YF
          if w_model_yf  > 0.
            perform bdc_field       using 'RF05V-NEWBS'
                                           '50'.
            lw_diff_yf_c = w_model_yf.
          else.
            perform bdc_field       using 'RF05V-NEWBS'
                                           '40'.
            lw_diff_yf_c = abs( w_model_yf ).
          endif.

          perform bdc_field       using 'RF05V-NEWKO'
                                        '531100'.
          perform bdc_dynpro      using 'SAPLF040' '0300'.
          perform bdc_field       using 'BDC_CURSOR'

                                        'BSEG-WRBTR'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=BP'.
*

          perform bdc_field       using 'BSEG-WRBTR'
                                        lw_diff_yf_c.
          perform bdc_field       using 'DKACB-FMORE'
                                        'X'.

          perform bdc_dynpro      using 'SAPLKACB' '0002'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'COBL-KOSTL'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=ENTE'.
          perform bdc_field       using 'COBL-KOSTL'
                                        '55101'.

          perform bdc_dynpro      using 'SAPLF040' '0330'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '/00'.
* end

          perform bdc_field       using 'BDC_CURSOR'
                                        'BSEG-CCBTC'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=PBBP'.
          perform bdc_dynpro      using 'SAPLKEAK' '0300'.
          perform bdc_field       using 'BDC_CURSOR'
                                        'RKEAK-FIELD(04)'.
          perform bdc_field       using 'BDC_OKCODE'
                                        '=WEIT'.
          if w_model_cm = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100001'.     "lw_paph2.
          endif.

          if w_model_nf = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100002'.     "lw_paph2.
          endif.

          if w_model_yf = 0.
            perform bdc_field       using 'RKEAK-FIELD(04)'
                                          '0000100004'.     "lw_paph2.
          endif.

        endif.
      endif.

*** CM 553110
    endloop.

    perform bdc_dynpro      using 'SAPLF040' '0330'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    perform bdc_field       using 'BDC_OKCODE'
                            '=BU'.

    call transaction 'F-66' using it_bdc
                            mode w_mode
                            update 'S'
                            messages into it_mess.

  else.
** f-41
    perform bdc_dynpro      using 'SAPMF05A' '0100'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BKPF-BLDAT'
                                  w_date.
    perform bdc_field       using 'BKPF-BLART'
** Changed by Furong on 06/29/07
*                                  'KG'.
                                  'DG'.
** End of change
    perform bdc_field       using 'BKPF-BUKRS'
                                  p_bukrs.
    perform bdc_field       using 'BKPF-BUDAT'
                                 w_date.
    perform bdc_field       using 'BKPF-MONAT'
                                  p_monat.
    perform bdc_field       using 'BKPF-WAERS'
                                  w_waers.
    perform bdc_field       using 'BKPF-XBLNR'
                                  '123200'.
    perform bdc_field       using 'BKPF-BKTXT'
                                  'CLAIM SETTLEMENT'.
*      PERFORM bdc_field       USING 'VBKPF-XBWAE'
*                                    'X'.
** Changed by Furong on 06/29/07
*    PERFORM BDC_FIELD       USING 'RF05A-NEWBS'
*                                  '24'.
* BEGIN OF UD1K950637
    if w_claim_amount > 0.
      perform bdc_field       using 'RF05A-NEWBS'
                                    '04'.
    else.
      perform bdc_field       using 'RF05A-NEWBS'
                                    '14'.
    endif.
* END OF UD1K950637

** End of change
    perform bdc_field       using 'RF05A-NEWKO'
                                  lt_invoice-lifnr.

*    PERFORM BDC_DYNPRO      USING 'SAPMF05A' '0302'.
    perform bdc_dynpro      using 'SAPMF05A' '0301'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RF05A-NEWKO'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'BSEG-HKONT'
*                                  '123201'.     "claim A/R
                                  p_clgl.
    perform bdc_field       using 'BSEG-WRBTR'
                                  lw_claim_amount_c.
    perform bdc_field       using 'BSEG-ZFBDT'
                                w_date.       "baseline date LW_TODATE
    perform bdc_field       using 'BSEG-ZUONR'
                                  '123200'.          "Assign
    perform bdc_field       using 'BSEG-SGTXT'
                                  '123200 CLAIM SETTLEMENT'.

    clear: lw_claim_amount_c.

    loop at lt_invoice.
      l_tabix = sy-tabix.                                   "UD1K950637

      clear: lw_paph2, w_matkl.
      l_dmbtr =  lt_invoice-dmbtr.
      at first .
        if l_dmbtr < 0 .
          perform bdc_field       using 'RF05A-NEWBS'
                                        '40'.
        else.
          perform bdc_field       using 'RF05A-NEWBS'
                                     '50'.
        endif.
        perform bdc_field       using 'RF05A-NEWKO'
                                   '123200'.
      endat.

      lw_menge_c = abs( lt_invoice-menge ).
      lw_dmbtr_c = abs( lt_invoice-dmbtr ).
      perform derive_model using lt_invoice-matnr
                           changing lw_paph2.

*      SELECT SINGLE meins INTO l_meins
*      FROM mara
*      WHERE matnr = lt_invoice-matnr.

*FIXME... WHY hard coding...
      case lw_paph2.
        when '0000100001'.
          w_model_nf = w_model_nf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        when '0000100002'.
          w_model_cm = w_model_cm + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.
        when '0000100004'.
          w_model_yf = w_model_yf + lt_invoice-claim_amount
                                  - lt_invoice-dmbtr.

      endcase.

      perform bdc_dynpro      using 'SAPMF05A' '0300'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'BSEG-MENGE'.

      perform bdc_field       using 'BDC_OKCODE'
                                    '=ZK'.
      perform bdc_field       using 'BSEG-WRBTR'
                                    lw_dmbtr_c.
      perform bdc_field       using 'BSEG-MENGE'
                                    lw_menge_c.
      perform bdc_field       using 'BSEG-MEINS'
                                    lt_invoice-meins.
      perform bdc_field       using 'BSEG-ZUONR'
                                    lt_invoice-lifnr.
      perform bdc_field       using 'BSEG-SGTXT'
                                    lt_invoice-matnr.

      perform bdc_dynpro      using 'SAPLKACB' '0002'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'COBL-MATNR'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '=ENTE'.
      perform bdc_field       using 'COBL-MATNR'
                                    lt_invoice-matnr.

      perform bdc_dynpro      using 'SAPMF05A' '0330'.
      perform bdc_field       using 'BDC_CURSOR'
                                    'RF05A-NEWKO'.
      perform bdc_field       using 'BDC_OKCODE'
                                    '/00'.
      at last.
        lw_last_rec = 'X'.
      endat.

      if lw_last_rec is initial.
        l_tabix = l_tabix + 1.                              "UD1K950637
        read table lt_invoice index l_tabix.                "UD1K950637

        if lt_invoice-dmbtr < 0.
          perform bdc_field       using 'RF05A-NEWBS'
                                               '40'.
        else.
          perform bdc_field       using 'RF05A-NEWBS'
                                                '50'.
        endif.
        perform bdc_field       using 'RF05A-NEWKO'
                                      '123200'.
      else.
        clear: lw_last_rec.
        if lw_diff <> 0.


          if w_model_cm = 0 or w_model_nf = 0
          or w_model_yf = 0.

            if lw_diff > 0.
              perform bdc_field       using 'RF05A-NEWBS'
                                            '50'.
              lw_diff_c = lw_diff.
            else.
              perform bdc_field       using 'RF05A-NEWBS'
                                           '40'.
              lw_diff_c = abs( lw_diff ).
            endif.

            perform bdc_field       using 'RF05A-NEWKO'
                                          '531100'.
            perform bdc_dynpro      using 'SAPMF05A' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ZK'.
            perform bdc_field       using 'BSEG-WRBTR'
                                          lw_diff_c.
            perform bdc_field       using 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=COBL_XERGO'.
            perform bdc_field       using 'COBL-KOSTL'
                                          '55101'.
            perform bdc_field       using 'DKACB-XERGO'
                                          'X'.

            perform bdc_dynpro      using 'SAPLKEAK' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RKEAK-FIELD(01)'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End
          else.

            if w_model_nf > 0.
              perform bdc_field       using 'RF05A-NEWBS'
                                            '50'.
              lw_diff_nf_c = w_model_nf.
            else.
              perform bdc_field       using 'RF05A-NEWBS'
                                           '40'.
              lw_diff_nf_c = abs( w_model_nf ).
            endif.

            perform bdc_field       using 'RF05A-NEWKO'
                                          '531100'.
            perform bdc_dynpro      using 'SAPMF05A' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'BSEG-SGTXT'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ZK'.
            perform bdc_field       using 'BSEG-WRBTR'
                                          lw_diff_nf_c.
            perform bdc_field       using 'BSEG-ZUONR'
                                          lt_invoice-lifnr.
            perform bdc_field       using 'BSEG-SGTXT'
                                         lt_invoice-matnr.

            if w_model_cm > 0.
              perform bdc_field       using 'RF05A-NEWBS'
                                             '50'.
              lw_diff_cm_c = w_model_cm.
            else.
              perform bdc_field       using 'RF05A-NEWBS'
                                           '40'.
              lw_diff_cm_c = abs( w_model_cm ).
            endif.

            perform bdc_field       using 'RF05A-NEWKO'
                                          '531100'.

            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
            perform bdc_field       using 'COBL-KOSTL'
                                          '55101'.
            perform bdc_field       using 'DKACB-XERGO'
                                           'X'.

            perform bdc_dynpro      using 'SAPLKEAK' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            perform bdc_dynpro      using 'SAPMF05A' '0330'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '/00'.

            perform bdc_dynpro      using 'SAPMF05A' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ZK'.
            perform bdc_field       using 'BSEG-WRBTR'
                                          lw_diff_cm_c.
            perform bdc_field       using 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            if w_model_yf > 0.
              perform bdc_field       using 'RF05A-NEWBS'
                                             '50'.
              lw_diff_yf_c = w_model_yf.
            else.
              perform bdc_field       using 'RF05A-NEWBS'
                                           '40'.
              lw_diff_yf_c = abs( w_model_yf ).
            endif.

            perform bdc_field       using 'RF05A-NEWKO'
                                          '531100'.

            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
            perform bdc_field       using 'COBL-KOSTL'
                                          '55101'.
            perform bdc_field       using 'DKACB-XERGO'
                                           'X'.
*            PERFORM bdc_field       USING 'BDC_OKCODE'
*                                          '=ENTE'.

            perform bdc_dynpro      using 'SAPLKEAK' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=WEIT'.
*** : KDM01(ADD) - Start
            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

            perform bdc_dynpro      using 'SAPMF05A' '0330'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RF05A-NEWKO'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '/00'.

            perform bdc_dynpro      using 'SAPMF05A' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'BSEG-ZUONR'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ZK'.
            perform bdc_field       using 'BSEG-WRBTR'
                                          lw_diff_yf_c.
            perform bdc_field       using 'BSEG-ZUONR'
                                          lt_invoice-lifnr.

*
            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
            perform bdc_field       using 'COBL-KOSTL'
                                          '55101'.
            perform bdc_field       using 'DKACB-XERGO'
                                           'X'.

            perform bdc_dynpro      using 'SAPLKEAK' '0300'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'RKEAK-FIELD(04)'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=WEIT'.

*** : KDM01(ADD) - Start
            perform bdc_dynpro      using 'SAPLKACB' '0002'.
            perform bdc_field       using 'BDC_CURSOR'
                                          'COBL-KOSTL'.
            perform bdc_field       using 'BDC_OKCODE'
                                          '=ENTE'.
*** : KDM01(ADD) - End

          endif.

        endif. " lw_diff <> 0.


      endif.
    endloop.
    perform bdc_dynpro      using 'SAPMF05A' '0330'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'BSEG-CCBTC'.
    perform bdc_field       using 'BDC_OKCODE'
                            '=BU'.

    call transaction 'F-41' using it_bdc
                            mode w_mode
                            update 'S'
                            messages into it_mess.

  endif.

  read table it_mess with key msgtyp = 'E'.

  if sy-subrc eq 0.
    read table lt_invoice index 1.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
      exporting
        titel = w_repid
        txt1  = 'Error for parking of Credit Memo (Scrap)'
        txt2  = it_mess-msgv1
        txt3  = lt_invoice-lifnr
        txt4  = it_mess-msgv2.
  else.
    if r_3 = 'X'.
*    READ TABLE it_mess WITH KEY msgtyp = 'S' msgnr = '001'.
      read table it_mess with key msgtyp = 'S' dynumb = '0300'.
      if sy-subrc <> 0.
        read table it_mess with key msgtyp = 'S' dynumb = '0330'.
      endif.
    else.
      read table it_mess with key msgtyp = 'S' dynumb = '0700'.
      if sy-subrc <> 0.
        read table it_mess with key msgtyp = 'S' dynumb = '0312'.
      endif.
    endif.
    if sy-subrc = 0.
      clear: it_ztfi_claim_stl.
**  change by Furong on 06/28/07
*      READ TABLE LT_INVOICE INDEX 1.
      loop at lt_invoice.
        it_ztfi_claim_stl-lifnr = lt_invoice-lifnr.
        it_ztfi_claim_stl-matnr = lt_invoice-matnr.
*      it_ztfi_claim_stl-ebeln = it_SCRAP_inv-ebeln.
*      it_ztfi_claim_stl-ebelp = it_SCRAP_inv-ebelp.
        it_ztfi_claim_stl-gjahr = p_gjahr.
        it_ztfi_claim_stl-lfmon = p_monat.
*      it_ztfi_claim_stl-lfbnr = it_SCRAP_inv-lfbnr.
*      it_ztfi_claim_stl-lfpos = it_SCRAP_inv-lfpos.
        it_ztfi_claim_stl-ctype = 'SCRAP'.
        it_ztfi_claim_stl-meins = lt_invoice-meins.
        it_ztfi_claim_stl-menge = lt_invoice-menge.
*        IT_ZTFI_CLAIM_STL-DMBTR = W_DMBTR.
        it_ztfi_claim_stl-dmbtr = lt_invoice-dmbtr.
        it_ztfi_claim_stl-amount = lt_invoice-amount.
*        IT_ZTFI_CLAIM_STL-CLAIM_AMOUNT = W_CLAIM_AMOUNT.
        it_ztfi_claim_stl-claim_amount = lt_invoice-claim_amount.
        it_ztfi_claim_stl-budat =  p_pdate.
        it_ztfi_claim_stl-inv_no = it_mess-msgv1.
        it_ztfi_claim_stl-ernam = sy-uname.
        it_ztfi_claim_stl-erdat = sy-datum.
        it_ztfi_claim_stl-erzet = sy-uzeit.
        append it_ztfi_claim_stl.
      endloop.
*    CONCATENATE IT_MESS-MSGV1 ' was created' INTO l_text.
*    MESSAGE s000 WITH l_text.
    endif.
  endif.
  clear: it_mess, it_mess[].
endform.                    " call_bdc_inv
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_sortcat_display.
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'CLMTYPE'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
* it_sort-spos           = 2.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'EBELN'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = 'X'.
*  APPEND it_sort.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

endform.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear it_bdc.
  it_bdc-program  = program.
  it_bdc-dynpro   = dynpro.
  it_bdc-dynbegin = 'X'.
  append it_bdc.
endform.                    "bdc_dynpro

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear it_bdc.
  it_bdc-fnam = fnam.
  it_bdc-fval = fval.
  append it_bdc.
endform.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  derive_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form derive_model using p_matnr
          changing p_model like ztco_model_map-paph2.

  data: l_model like ztco_model_map-modl1.

  select single matkl into w_matkl from mara
    where matnr = p_matnr.

*  l_model = w_matkl(2).
  concatenate w_matkl(2) '%' into l_model.
*   check the model
  select single paph2 into p_model
    from ztco_model_map
    where modl2 like l_model.

  if p_model is initial.
    select single maktx into w_matkl from makt
      where matnr = p_matnr
      and spras eq sy-langu.
    if sy-subrc eq 0.

      concatenate w_matkl(2) '%' into l_model.
*   check the model
      select single paph2 into p_model
        from ztco_model_map
        where modl2 like l_model.

    endif.
  endif.

  if p_model is initial.
*   break-point.
*********************************************************************
***** Please contact HISNA SAP Consultant ***************************
*********************************************************************
  endif.

endform.                    " derive_model
*&---------------------------------------------------------------------*
*&      Form  call_bapi_inv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bapi_inv.
  data: lw_diff like lt_invoice-dmbtr,
        lw_line type i,
        l_webre like ekpo-webre,
        l_text(30).

  clear: headerdata, itemdata,itemdata[], return, return[],
         it_ztfi_claim_stl.

  read table lt_invoice index 1.
  if w_claim_amount < 0.
    headerdata-invoice_ind = 'X'.
    headerdata-gross_amount = - w_claim_amount.
  else.
    headerdata-invoice_ind = ' '.
    headerdata-gross_amount = w_claim_amount.
  endif.
  headerdata-doc_date = p_pdate.
  headerdata-pstng_date = p_pdate.
  headerdata-comp_code = p_bukrs.
  headerdata-currency_iso = w_waers.
  headerdata-diff_inv = lt_invoice-lifnr.
** Added by Furong on 11/15/07
  if r_1 = 'X'.
    concatenate 'Return stl:' p_monat '/' p_gjahr
           into headerdata-header_txt.
  else.
    concatenate 'Claim stl: ' p_monat '/' p_gjahr
           into headerdata-header_txt.
  endif.
** end of addition
  loop at lt_invoice.
    lw_line = lw_line + 1.
*    SELECT SINGLE meins webre INTO (itemdata-po_unit, l_webre)
*      FROM ekpo
*      WHERE ebeln = lt_invoice-ebeln
*        AND ebelp = lt_invoice-ebelp.
    itemdata-invoice_doc_item = lw_line.
    itemdata-po_number = lt_invoice-ebeln.
    itemdata-po_item = lt_invoice-ebelp.
*    itemdata-ref_doc = lt_invoice-belnr.
*    itemdata-ref_doc_it = lt_invoice-buzei.
    if lt_invoice-webre = 'X'.
      itemdata-ref_doc = lt_invoice-lfbnr.
      itemdata-ref_doc_it = lt_invoice-lfpos.
      itemdata-ref_doc_year = p_gjahr.
    endif.
    itemdata-tax_code = 'U0'.

    if lt_invoice-claim_amount < 0.
      itemdata-item_amount = - lt_invoice-claim_amount.
      itemdata-quantity    = - lt_invoice-menge.
    else.
      itemdata-item_amount = lt_invoice-claim_amount.
      itemdata-quantity    = lt_invoice-menge.
    endif.
    itemdata-po_unit     = lt_invoice-meins.
    itemdata-po_unit_iso = itemdata-po_unit.
    if r_1 = 'X'.
      itemdata-item_text = 'Return Posting'.
    endif.
    append itemdata.
    clear itemdata.
  endloop.

  if r_3 = 'X'.
    call function 'BAPI_INCOMINGINVOICE_PARK'
      exporting
        headerdata          = headerdata
*       ADDRESSDATA         =
      importing
        invoicedocnumber    = invoicedocnumber
        fiscalyear          = fiscalyear
      tables
        itemdata            = itemdata
*       ACCOUNTINGDATA      =
*       GLACCOUNTDATA       =
*       MATERIALDATA        =
*       TAXDATA             =
*       WITHTAXDATA         =
*       VENDORITEMSPLITDATA =
        return              = return.
  else.
    call function 'BAPI_INCOMINGINVOICE_CREATE'
      exporting
        headerdata          = headerdata
*       ADDRESSDATA         =
      importing
        invoicedocnumber    = invoicedocnumber
        fiscalyear          = fiscalyear
      tables
        itemdata            = itemdata
*       ACCOUNTINGDATA      =
*       GLACCOUNTDATA       =
*       MATERIALDATA        =
*       TAXDATA             =
*       WITHTAXDATA         =
*       VENDORITEMSPLITDATA =
        return              = return.

  endif.
  read table return with key type = 'E'.
  if sy-subrc = 0.
    call function 'MESSAGE_TEXT_BUILD'
      exporting
        msgid               = return-id
        msgnr               = return-number
        msgv1               = return-message_v1
        msgv2               = return-message_v2
        msgv3               = return-message_v3
        msgv4               = return-message_v4
      importing
        message_text_output = return-message.

    w_repid = sy-repid.

    call function 'POPUP_TO_INFORM'
      exporting
        titel = w_repid
        txt1  = 'Error for parking of Credit Memo (Return)'
        txt2  = return-id
        txt3  = lt_invoice-lifnr
        txt4  = return-message.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
    clear: it_ztfi_claim_stl.
** changed by Furong on 06/28/07 for saving all posted data
*    READ TABLE LT_INVOICE INDEX 1.
    loop at lt_invoice.
      it_ztfi_claim_stl-lifnr = lt_invoice-lifnr.
      it_ztfi_claim_stl-matnr = lt_invoice-matnr.
      it_ztfi_claim_stl-ebeln = lt_invoice-ebeln.
      it_ztfi_claim_stl-ebelp = lt_invoice-ebelp.
      it_ztfi_claim_stl-gjahr = p_gjahr.
      it_ztfi_claim_stl-lfmon = p_monat.
      it_ztfi_claim_stl-lfbnr = lt_invoice-lfbnr.
      it_ztfi_claim_stl-lfpos = lt_invoice-lfpos.
      it_ztfi_claim_stl-ctype = 'RETURN'.
      it_ztfi_claim_stl-meins = lt_invoice-meins.
      it_ztfi_claim_stl-menge = lt_invoice-menge.
      it_ztfi_claim_stl-claim_amount = lt_invoice-claim_amount.
      it_ztfi_claim_stl-dmbtr = lt_invoice-dmbtr.
      it_ztfi_claim_stl-budat =  p_pdate.
      it_ztfi_claim_stl-inv_no = invoicedocnumber.
      it_ztfi_claim_stl-ernam = sy-uname.
      it_ztfi_claim_stl-erdat = sy-datum.
      it_ztfi_claim_stl-erzet = sy-uzeit.
      append it_ztfi_claim_stl.
    endloop.
    concatenate invoicedocnumber ' was created' into l_text.
    message s000 with l_text.
  endif.
endform.                    " call_bapi_inv
*&---------------------------------------------------------------------*
*&      Form  call_bdc_inv_split
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_inv_split.
  data: lw_invoice like table of lt_invoice with header line.
  data: l_cn type i,
        l_dmbtr like lt_invoice-dmbtr,
        l_claim_amount like lt_invoice-dmbtr.

  lw_invoice[] = lt_invoice[].
  clear: lt_invoice, lt_invoice[].
  clear: w_dmbtr, w_claim_amount, l_cn.
  loop at lw_invoice.
    l_cn = l_cn + 1.
    lt_invoice = lw_invoice.
    append lt_invoice.
    w_dmbtr = w_dmbtr + lt_invoice-dmbtr.
    w_claim_amount = w_claim_amount + lt_invoice-claim_amount.
    if l_cn >= p_max_l.
      if r_1 = 'X'.
        perform call_bapi_inv.
      else.
        perform call_bdc_inv.
      endif.
      clear: lt_invoice, lt_invoice[].
      clear: w_dmbtr, w_claim_amount, l_cn.
    endif.
  endloop.
endform.                    " call_bdc_inv_split
*&---------------------------------------------------------------------*
*&      Form  ZERO_BALANCE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zero_balance_check .


  data : begin of lt_tab occurs 9,
          lifnr like ekko-lifnr,
          ebeln like ekbe-ebeln,
          ebelp like ekbe-ebelp,
          matnr like ekbe-matnr,   "1
          dmbtr like bsis-dmbtr,   "2
         end of lt_tab.

*Zero balance = by vender + material, Trn amount = zero.
  if c_1 = 'X'.

    sort it_output by lifnr ebeln ebelp matnr.

    refresh lt_tab. clear lt_tab.
    loop at it_output.
      move-corresponding it_output  to lt_tab.
      collect lt_tab.
    endloop.

    sort lt_tab by lifnr ebeln ebelp matnr.

    loop at lt_tab where dmbtr = 0.
      read table it_output with key lifnr = lt_tab-lifnr
                                    ebeln = lt_tab-ebeln
                                    ebelp = lt_tab-ebelp
                                    matnr = lt_tab-matnr.
      if sy-subrc = 0.
        delete it_output where  lifnr = lt_tab-lifnr
                           and  ebeln = lt_tab-ebeln
                           and  ebelp = lt_tab-ebelp
                           and  matnr = lt_tab-matnr.

      endif.
    endloop.
*
*    data : l_dmbtr like it_output-dmbtr,l_flag.
*
*    loop at it_output.
*      at end of ebelp.
*        sum.
*        l_dmbtr = it_output-dmbtr.
*        l_flag = 'X'.
*      endat.
*
*      if l_flag eq 'X'.
*        if l_dmbtr = 0.
*          delete it_output where lifnr = it_output-lifnr
*                             and matnr = it_output-matnr
*                             and ebeln = it_output-ebeln
*                             and ebelp = it_output-ebelp.
*        endif.
*        clear : l_dmbtr, l_flag.
*      endif.
*    endloop.

  endif.

endform.                    " ZERO_BALANCE_CHECK
