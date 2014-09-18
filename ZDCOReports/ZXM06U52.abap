*----------------------------------------------------------------------*
*   INCLUDE ZXM06U52                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_BQPIM) LIKE  BQPIM STRUCTURE  BQPIM
*"             VALUE(I_BANFN) LIKE  EBAN-BANFN OPTIONAL
*"             VALUE(I_BNFPO) LIKE  EBAN-BNFPO OPTIONAL
*"       TABLES
*"              T_SOURCES STRUCTURE  SRC_DETERM
*"       CHANGING
*"             VALUE(C_BQPEX) LIKE  BQPEX STRUCTURE  BQPEX
*----------------------------------------------------------------------*
*--------------------------------------
* Below logic is for CO Purpose
* - designed by Andy Choi
*--------------------------------------

data do_not_work?.
data l_text(16).
if syst-tcode eq 'MB1A' or
   syst-cprog eq 'ZFCHSGTXT' or
   syst-tcode(2) eq 'CK'.
else.
  do_not_work? = 'X'.
endif.

check do_not_work? is initial.

data: l_cnt  type i.

*KD-Scrap in Plant, Mass-change for old data
if syst-tcode = 'MB1A' or syst-cprog = 'ZFCHSGTXT'.

*.................................................. other {
  loop at t_sources.
    select count( * ) into l_cnt from a018
     where kappl =  'M'
       and kschl =  'PB00'
       and matnr =  i_bqpim-matnr    "material
       and lifnr =  t_sources-lifnr  "vendor
       and ekorg =  t_sources-ekorg  "Pur.Org
       and esokz =  '0'
       and datbi >=  i_bqpim-nedat   "Valid To
       and datab <=  i_bqpim-nedat.  "Valid from
    if l_cnt = 0.
      delete t_sources index sy-tabix.
    endif.
  endloop.

  read table t_sources index 1.
  if sy-subrc = 0.
    c_bqpex-flief = t_sources-lifnr.  "Fixed vendor.
    c_bqpex-ekorg = t_sources-ekorg.
    c_bqpex-infnr = t_sources-infnr.
  endif.
*......................................................... }

*Cost Estimates
elseif syst-tcode(2) = 'CK'.

  field-symbols: <status>, <id>, <date>.
  data: l_klvaf like kala-klvaf.
  clear l_klvaf.

*Get Costing Variant from Screen
  assign ('(SAPLCKDI)CKI64A-KLVAR') to <status>.
  l_klvaf = <status>.
  if l_klvaf is initial.
    assign ('(SAPRCK10)KALAID')  to <id>.
    assign ('(SAPRCK10)KALADAT') to <date>.

    select single klvaf into l_klvaf from kala
      where kalaid  = <id>
        and kaladat = <date>.
  endif.

  if l_klvaf = space.
    call function 'CM_F_MESSAGE'
         exporting
              arbgb            = 'ZMCO'
              msgnr            = '000'
              msgty            = 'I'
              msgv1            = i_bqpim-matnr
              msgv2            = i_bqpim-werks
              msgv3            = 'Costing Variant Not Derived'
              object_dependent = 'X'
         exceptions
              others           = 1.
    if sy-subrc <> 0.
      message i000(zmco) with i_bqpim-matnr i_bqpim-werks
                         'Costing Variant is Blank-'.
    endif.
  endif.

  case l_klvaf(2).
    when 'ZM'.   "Module Costing
      call function 'Z_CO_GET_VENDOR_SOURCE'
           exporting
                matnr           = i_bqpim-matnr
                werks           = i_bqpim-werks
                available_date  = i_bqpim-nedat
                sub_part        = 'X'
           importing
                lifnr           = c_bqpex-flief
                used_source     = l_text
                ekorg           = c_bqpex-ekorg
                infnr           = c_bqpex-infnr
           exceptions
                no_source_found = 1
                invalid_werks   = 2
                others          = 3.

      if sy-subrc <> 0.
      endif.

    when 'ZU' or 'ZR'.   "STD for RAW

* ----------------------------------------------------------------- *
*   current or future -> eord, info
*   past              -> ztcou137
* ----------------------------------------------------------------- *
      call function 'Z_CO_GET_VENDOR_SOURCE_AUTO'
           exporting
                matnr           = i_bqpim-matnr
                werks           = i_bqpim-werks
                available_date  = i_bqpim-nedat
           importing
                lifnr           = c_bqpex-flief
                used_source     = l_text
                ekorg           = c_bqpex-ekorg
                infnr           = c_bqpex-infnr
           exceptions
                no_source_found = 1
                invalid_werks   = 2
                others          = 3.

*      if i_bqpim-nedat >= sy-datum. " current or future
*
*        call function 'Z_CO_GET_VENDOR_SOURCE'
*             exporting
*                  matnr           = i_bqpim-matnr
*                  werks           = i_bqpim-werks
*                  available_date  = i_bqpim-nedat
*                  sub_part        = space
*                  use_source_list = 'X'
*             importing
*                  lifnr           = c_bqpex-flief
*                  used_source     = l_text
*                  ekorg           = c_bqpex-ekorg
*                  infnr           = c_bqpex-infnr
*             exceptions
*                  no_source_found = 1
*                  invalid_werks   = 2
*                  others          = 3.
*
*        if sy-subrc ne 0.
*
*          call function 'Z_CO_GET_VENDOR_SOURCE'
*               exporting
*                    matnr           = i_bqpim-matnr
*                    werks           = i_bqpim-werks
*                    available_date  = i_bqpim-nedat
*                    sub_part        = 'X'
*               importing
*                    lifnr           = c_bqpex-flief
*                    used_source     = l_text
*                    ekorg           = c_bqpex-ekorg
*                    infnr           = c_bqpex-infnr
*               exceptions
*                    no_source_found = 1
*                    invalid_werks   = 2
*                    others          = 3.
*
*          if sy-subrc <> 0.
*          endif.
*
*        endif.
*
*      else. " past
*
*        call function 'Z_CO_GET_VENDOR_SOURCE'
*             exporting
*                  matnr           = i_bqpim-matnr
*                  werks           = i_bqpim-werks
*                  available_date  = i_bqpim-nedat
*             importing
*                  lifnr           = c_bqpex-flief
*                  used_source     = l_text
*                  ekorg           = c_bqpex-ekorg
*                  infnr           = c_bqpex-infnr
*             exceptions
*                  no_source_found = 1
*                  invalid_werks   = 2
*                  others          = 3.
*
*        if sy-subrc ne 0.
*
*          call function 'Z_CO_GET_VENDOR_SOURCE'
*               exporting
*                    matnr           = i_bqpim-matnr
*                    werks           = i_bqpim-werks
*                    available_date  = i_bqpim-nedat
*                    sub_part        = ' '
*                    use_source_list = 'X'
*               importing
*                    lifnr           = c_bqpex-flief
*                    used_source     = l_text
*                    ekorg           = c_bqpex-ekorg
*                    infnr           = c_bqpex-infnr
*               exceptions
*                    no_source_found = 1
*                    invalid_werks   = 2
*                    others          = 3.
*
*          if sy-subrc ne 0.
*
*            call function 'Z_CO_GET_VENDOR_SOURCE'
*                 exporting
*                      matnr           = i_bqpim-matnr
*                      werks           = i_bqpim-werks
*                      available_date  = i_bqpim-nedat
*                      sub_part        = 'X'
*                 importing
*                      lifnr           = c_bqpex-flief
*                      used_source     = l_text
*                      ekorg           = c_bqpex-ekorg
*                      infnr           = c_bqpex-infnr
*                 exceptions
*                      no_source_found = 1
*                      invalid_werks   = 2
*                      others          = 3.
*
*            if sy-subrc <> 0.
*            endif.
*
*          endif.
*
*        endif.
*
*      endif.

    when others.                                            "STD PPC1

*.................................................. other {
      loop at t_sources.
        select count( * ) into l_cnt from a018
         where kappl =  'M'
           and kschl =  'PB00'
           and matnr =  i_bqpim-matnr    "material
           and lifnr =  t_sources-lifnr  "vendor
           and ekorg =  t_sources-ekorg  "Pur.Org
           and esokz =  '0'
           and datbi >=  i_bqpim-nedat   "Valid To
           and datab <=  i_bqpim-nedat.  "Valid from
        if l_cnt = 0.
          delete t_sources index sy-tabix.
        endif.
      endloop.

      read table t_sources index 1.
      if sy-subrc = 0.
        c_bqpex-flief = t_sources-lifnr.  "Fixed vendor.
        c_bqpex-ekorg = t_sources-ekorg.
        c_bqpex-infnr = t_sources-infnr.
      endif.
*......................................................... }

  endcase.

endif.

**** old logic {
*
*
*
*
*
*
*
*
***TABLES: a018, eina, eine, konp.
***DATA: i_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.
***DATA: BEGIN OF it_knumh OCCURS 0,
***        knumh  LIKE konh-knumh,
***        datab  LIKE konh-datab, "Valid-from date
***        datbi  LIKE konh-datbi,
***        lifnr  LIKE lfa1-lifnr,
***        kstbmt TYPE kstbmt,                                 "3 digit
***        kbetr  LIKE konp-kbetr, "rate
***        kpein  LIKE konp-kpein, "pricing unit
***        ekorg  LIKE eine-ekorg,
***        infnr  LIKE eina-infnr,
***      END   OF it_knumh.
***DATA: BEGIN  OF it_info OCCURS 0,
***        lifnr  LIKE eina-lifnr,
***        infnr  LIKE eina-infnr,
****       ekorg  LIKE eine-ekorg,
***      END OF it_info.
***DATA : l_ekorg type t001w-ekorg.
***
***DATA: l_cnt  TYPE i,
***      l_send_notice TYPE i.
***RANGES: r_receiv FOR ztco_mail-uname.
***
****Costing, MB1A Other GI, Mass Chg Program
****CHECK i_bqpim-beskz = 'F'   "F : External Procurement???
***
****KD-Scrap in Plant, Mass-change for old data
***IF syst-tcode = 'MB1A' OR syst-cprog = 'ZFCHSGTXT'.
***  INCLUDE zco_zxm06u52_oth.
***
****Cost Estimates
***ELSEIF syst-tcode(2) = 'CK'.
***  FIELD-SYMBOLS: <status>, <id>, <date>.
***  DATA: l_klvaf LIKE kala-klvaf.
***  CLEAR l_klvaf.
***
***
****Get Costing Variant from Screen
***  ASSIGN ('(SAPLCKDI)CKI64A-KLVAR') TO <status>.
***  l_klvaf = <status>.
***  IF l_klvaf IS INITIAL.
****Sometime...not working...
***    ASSIGN ('(SAPRCK10)KALAID')  TO <id>.
***    ASSIGN ('(SAPRCK10)KALADAT') TO <date>.
***
***    SELECT SINGLE klvaf INTO l_klvaf FROM kala
***      WHERE kalaid  = <id>
***        AND kaladat = <date>.
***  ENDIF.
***
****.Check costing variant(Parallel processing problem???)
****  DATA: l_nr LIKE cmfp-nr.
****  CALL FUNCTION 'CM_F_STORE'
****       EXPORTING
****            aplid          = 'CK'
****            in_update_task = space
****       IMPORTING
****            e_cmf_nr       = l_nr.
***
***  IF l_klvaf = space.
***    CALL FUNCTION 'CM_F_MESSAGE'
***         EXPORTING
***              arbgb            = 'ZMCO'
***              msgnr            = '000'
***              msgty            = 'I'
***              msgv1            = i_bqpim-matnr
***              msgv2            = i_bqpim-werks
***              msgv3            = 'Costing Variant Not Derived'
***              object_dependent = 'X'
***         EXCEPTIONS
***              OTHERS           = 1.
***    IF sy-subrc <> 0.
***      MESSAGE I000(zmco) WITH i_bqpim-matnr i_bqpim-werks
***                         'Costing Variant is Blank-'.
***    ENDIF.
***  ENDIF.
***
****get mail receiver, break-point, ...
***  INCLUDE zco_zxm06u52_1.
***
****get pur.org from plant
***  select single ekorg into l_ekorg from t001w
***         where werks = I_BQPIM-werks.
***  if sy-subrc <> 0. break-point. endif.
***
****.Different Logic per Costing Variant
***  CASE l_klvaf(2).
***    WHEN 'ZM'.   "Module Costing
***      INCLUDE zco_zxm06u52_mod.
****    WHEN 'ZU'.   "Unit Costing
****      INCLUDE zco_zxm06u52_unt.
***    WHEN 'ZU' or 'ZR'.   "STD for RAW
***      INCLUDE zco_zxm06u52_std.
***    WHEN OTHERS. "STD PPC1
***      INCLUDE zco_zxm06u52_oth.
***  ENDCASE.
***
****Check material status
***  include ZXM06U52_MMSTA.
***
***ENDIF.
***
****--send mail to CO users
***IF l_send_notice > 1.
***  DATA: s_txt(80) TYPE c.
***  CASE l_send_notice.
***    WHEN  2. s_txt = 'Multi source list identified'.
***    WHEN  3. s_txt = 'Multi source list with multi info-record'.
***    WHEN  4. s_txt = 'No source list'.
***    WHEN  5. s_txt = 'No source list with multi info-record'.
***    WHEN 12. s_txt = 'Multi info-record exist on sub-part'.
***  ENDCASE.
***
***  SUBMIT zmail
***          WITH p_exp = ' '   "Express mail?
***         WITH send-h01 = '***Warning: Multi-source/info identified'
***          WITH send-b01 = 'This is a message from SAP'
***          WITH send-b02 = i_bqpim-matnr
***          WITH send-b03 = s_txt
***          WITH send-b04 = 'Please check source list or info-record'
***          WITH send-b06 = 'Thanks,'
***          WITH send-b07 = 'Andy'
***          WITH s_receiv IN r_receiv
***      AND RETURN.
***ENDIF.
**** }
