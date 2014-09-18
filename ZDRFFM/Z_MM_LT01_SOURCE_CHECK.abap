FUNCTION Z_MM_LT01_SOURCE_CHECK.
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
       lw_index type n,
       w_stock like lqua-verme,
       w_lgtyp like mlgt-lgtyp.
 data: w_stock_char(13), w_fs(30).

 data: it_mlgt type mlgt occurs 0 with header line,
       it_mlgt_lgtyp type mlgt occurs 0 with header line.

 field-symbols: <fs>.
 constants: c_werks(4) value 'P001',
            c_lgnum(3) value 'P01'.

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
     do 29 times.
        lw_index = sy-index - 1.
        concatenate 'it_t334t-lgty' lw_index into w_fs.
        assign (w_fs) to <fs>.
        if sy-subrc ne 0.
           exit.
        endif.
        read table it_mlgt with key matnr = p_matnr
                                    lgnum = c_lgnum
                                    lgtyp = <fs>.     "it_t334t-lgty0.
        if sy-subrc = 0.
          if not it_mlgt-lgpla is initial.
             move: p_matnr to it_mlgt_lgtyp-matnr,
                   c_lgnum to it_mlgt_lgtyp-lgnum,
                   it_mlgt-lgtyp to it_mlgt_lgtyp-lgtyp,
                   it_mlgt-lgpla to it_mlgt_lgtyp-lgpla.
             append it_mlgt_lgtyp.
          endif.
        endif.
     enddo.
 endloop.

 if it_mlgt_lgtyp[] is initial.
    E_MESS  = TEXT-W11.
    ZRESULT = TEXT-M02.
    exit.
 endif.

 clear: e_mess, zresult.
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
           E_MESS  = TEXT-W11.
           ZRESULT = TEXT-M02.
        endif.
    endloop.
 else.
    read table it_mlgt_lgtyp index 1.
    if sy-subrc eq 0.
       p_lgtyp = it_mlgt_lgtyp-lgtyp.
       p_lgpla = it_mlgt_lgtyp-lgpla.
       E_MESS  = TEXT-M22.
       ZRESULT = TEXT-M04.
    else.
       clear: p_lgtyp, p_lgpla.
       E_MESS  = TEXT-W11.
       ZRESULT = TEXT-M02.
    endif.
 endif.
ENDFUNCTION.
