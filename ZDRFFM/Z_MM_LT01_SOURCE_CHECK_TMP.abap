FUNCTION Z_MM_LT01_SOURCE_CHECK_TMP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) LIKE  MLGN-MATNR
*"     VALUE(P_TOQTY) LIKE  MLGN-LTKZA DEFAULT 0
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(P_LGTYP) LIKE  MLGT-LGTYP
*"     VALUE(P_LGPLA) LIKE  MLGT-LGPLA
*"----------------------------------------------------------------------
 data: w_vspvb like marc-vspvb.
 data: it_t334t type t334t occurs 0 with header line.
 data: w_ltkza like mlgn-ltkza,
       w_stock like lqua-verme,
       w_lgtyp(16).

 data: w_stock_char(13).

 FIELD-SYMBOLS: <FS> LIKE mlgt-LGTYp.

 data: m_sno type i,
       m_sno_char(2).

 data: it_mlgt type mlgt occurs 0 with header line,
       it_mlgt_lgtyp type mlgt occurs 0 with header line.


 constants: c_werks(4) value 'P001',
            c_lgnum(3) value 'P01'.
* data: it_pkhd type pkhd occurs 0 with header line.

* if t_list-nltyp is initial and t_list-nlpla is initial.
*    select single vspvb into w_vspvb from marc
*                     where matnr = t_list-matnr
*                       and werks = c_werks.
*    select * into table it_pkhd from pkhd where matnr = t_list-matnr
*                                      and werks = c_werks
*                                      and prvbe = w_vspvb.
*    if it_pkhd[] is initial.
*       e_mess = text-w11.
*       zresult = '1'.
*    else.
*       read table it_pkhd index 1.
*       w_NLTYP = it_PKHD-LGTYP.
*       w_NLPLA = it_PKHD-LGPLA.
*       E_MESS  = TEXT-M22.
*       zresult = '0'.
*       read table t_list index 1.
*       t_list-NLTYP = w_NLTYP.
*       t_list-NLPLA = w_NLPLA.
*       modify t_list index 1 transporting NLTYP NLPLA.
*    endif.
*endif.
*
   select single ltkza into w_ltkza from mlgn
                      where matnr = p_matnr
                        and lgnum = c_lgnum.

   select * into table it_mlgt from mlgt
                      where matnr = p_matnr
                        and lgnum = c_lgnum.

   select * into table it_t334t from t334t
                      where lgnum = c_lgnum
                        and kzear = 'A'
                        and lgtkz = w_ltkza.


   loop at it_t334t.
   clear: m_sno, m_sno_char.
      while m_sno < 10.
        m_sno_char  = m_sno.
        concatenate 'it_t334t-lgty' m_sno_char into w_lgtyp.
        assign (w_lgtyp) to <fs>.

        read table it_mlgt with key matnr = p_matnr
                                    lgnum = c_lgnum
                                    lgtyp = <fs>.
        if sy-subrc = 0.
           move: p_matnr to it_mlgt_lgtyp-matnr,
                 c_lgnum to it_mlgt_lgtyp-lgnum,
                 it_t334t-lgty0 to it_mlgt_lgtyp-lgtyp,
                 it_mlgt-lgpla to it_mlgt_lgtyp-lgpla.
           append it_mlgt_lgtyp.
           E_MESS  = TEXT-M22.
           ZRESULT = TEXT-M04.
        endif.
        m_sno = m_sno + 1.
      endwhile.
   endloop.

   if it_mlgt_lgtyp[] is initial.
      E_MESS  = TEXT-W13.
      ZRESULT = TEXT-M02.
   endif.
   clear e_mess.
   if p_toqty ne 0.
      loop at it_mlgt_lgtyp.
        select single verme into w_stock from lqua
                           where matnr = p_matnr
                             and lgnum = c_lgnum
                             and lgtyp = it_mlgt-lgtyp
                             and lgpla = it_mlgt-lgpla.
        if sy-subrc = 0.
           if p_toqty <= w_stock.
              E_MESS  = TEXT-M22.
              ZRESULT = TEXT-M04.
              p_lgtyp = it_mlgt-lgtyp.
              p_lgpla = it_mlgt-lgpla.
              exit.
           else.
              w_stock_char = w_stock.
              CONCATENATE TEXT-W12 TEXT-W14 W_STOCK_CHAR INTO E_MESS.
              ZRESULT = TEXT-M02.
              p_lgtyp = it_mlgt-lgtyp.
              p_lgpla = it_mlgt-lgpla.
           endif.
        else.
           E_MESS  = TEXT-W13.
           ZRESULT = TEXT-M02.
        endif.
      endloop.
    else.
      read table it_mlgt_lgtyp index 1.
      p_lgtyp = it_mlgt_lgtyp-lgtyp.
      p_lgpla = it_mlgt_lgtyp-lgpla.
    endif.
ENDFUNCTION.
