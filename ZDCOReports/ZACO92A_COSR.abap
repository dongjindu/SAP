************************************************************************
* Program Name      : ZACO92A_COSR
* Author            : Hye Sun , Jung
* Creation Date     : 2006.10.11
* Description       : Create COSR by ZTCO_MHOS
************************************************************************

report zaco92a_cosr message-id zmco.

tables : ztco_mhos.
data : it_mhos       like ztco_mhos occurs 0 with header line.
data : it_return     like standard table of bapiret2
                          with header line.
data : gv_doc_header like bapidochdrp,
       gv_doc_NO     LIKE  BAPIDOCHDRP-DOC_NO.
data : it_doc_items  like standard table of bapiskfitm
                     with header line.

constants: c_skf12 like cosr-STAGR value 'CS012'.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters : p_kokrs like ztco_mhos-kokrs memory id cac  obligatory
               default 'H201'.
parameters : p_bdatj like keko-bdatj memory id bdtj obligatory.
parameters : p_poper like covja-perab memory id vpe
             modif id per obligatory .
parameters : p_versn like cobk-versn memory id kvt .

select-options : s_kostl   for ztco_mhos-kostl,
                 s_srvpos  for ztco_mhos-srvpos.

selection-screen end of block bl1.

selection-screen begin of block bl2 with frame title text-002.
parameters: P_retro  type P_99S_RDIFF.
parameters: P_new    as checkbox.
selection-screen end of block bl2.

parameters: P_cancel as checkbox.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

  perform select_ztco_mhos.
  if sy-subrc <> 0.
    message s000 with 'No data found'.
  else.

*-- check previous posting
    data: l_belnr type CO_BELNR.
    select single a~belnr into l_belnr
         from cobk as a
            inner join coepr as b
               on a~kokrs = b~kokrs
              and a~belnr = b~belnr
         where a~gjahr = p_bdatj
           and a~kokrs = p_kokrs
           and a~perab = p_poper
           and a~vrgng = 'RKS'
           and a~STOKZ = space
           and a~STFLG = space
           and b~STAGR = c_skf12.

*reverse
    if p_cancel = 'X'.
*----- implement BAPI BAPI_ACC_ACT_POSTINGS_REVERSE : FIXME (for Moon)
       message s000 with 'Not yet implemented to reverse: ' l_belnr.

    else.
      if sy-subrc = 0.
       message s000 with 'Already posted. Please reverse first(KB34N) '
                          l_belnr.
      else.
        perform post_bapi_skf.
      endif.

    endif.
  endif.

*&---------------------------------------------------------------------*
*&      Form  select_ztco_mhos
*&---------------------------------------------------------------------*
form select_ztco_mhos.
* P_new; not yet implemented... please fix it (for Moon)

  if p_retro = space.
    select * into corresponding fields of table it_mhos
      from ztco_mhos
     where kokrs  eq p_kokrs
       and bdatj  eq p_bdatj
       and poper  eq p_poper
       and kostl  in s_kostl
       and srvpos in s_srvpos
       and retro  eq space.
  else.
    select * into corresponding fields of table it_mhos
      from ztco_mhos
     where kokrs  eq p_kokrs
       and bdatj  eq p_bdatj
       and poper  eq p_poper
       and kostl  in s_kostl
       and srvpos in s_srvpos.
  endif.

endform.                    " select_ztco_mhos
*&---------------------------------------------------------------------*
*&      Form  post_bapi_skf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_bapi_skf.
  data : l_first_date type datum,
         l_last_date type datum.

  concatenate p_bdatj p_poper+1(2) '01' into l_first_date.
  call function 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_first_date
       IMPORTING
            last_day_of_month = l_last_date
       EXCEPTIONS
            day_in_no_date    = 1
            others            = 2.

* Fill Header DATA
  clear gv_doc_header.
  gv_doc_header-co_area           = p_kokrs.
  gv_doc_header-docdate           = l_last_date.
  gv_doc_header-postgdate         = l_last_date.
  gv_doc_header-version           = p_versn .
  gv_doc_header-variant           = 'SAP01'.
  gv_doc_header-doc_hdr_tx        = sy-repid.
  gv_doc_header-username          = sy-uname.

* Fill Object List
  clear : it_doc_items, it_doc_items[].
  loop at it_mhos.
    it_doc_items-statkeyfig = c_skf12 .
    it_doc_items-stat_qty   = it_mhos-menge.
    it_doc_items-rec_cctr   = it_mhos-kostl.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = it_doc_items-rec_cctr
         IMPORTING
              output = it_doc_items-rec_cctr.
**// End of Mod.
    collect it_doc_items.  clear it_doc_items.
  endloop.

* Call BAPI FM
  call function 'BAPI_ACC_STAT_KEY_FIG_POST'
    exporting
      doc_header            = gv_doc_header
*   IGNORE_WARNINGS       = ' '
   IMPORTING
      DOC_NO                = gv_doc_no
    tables
      doc_items             = it_doc_items
      return                = it_return.


* Check error
  clear  it_return.
  loop at it_return  where type ca 'AE'.
    message id     it_return-id
            type   it_return-type
            number it_return-number
            with   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    clear it_return.
  endloop.

  commit work.
  read table it_return  index 1.
  message s000(zmco) with it_return-message.

endform.                    " post_bapi_skf
