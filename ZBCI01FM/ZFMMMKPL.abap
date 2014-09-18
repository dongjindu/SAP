FUNCTION ZFMMMKPL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_WERKS) TYPE  PVBE-WERKS
*"     VALUE(P_PRVBE) TYPE  PVBE-PRVBE
*"     VALUE(P_MATNR) TYPE  PKHD-MATNR
*"  EXPORTING
*"     VALUE(P_RFSTA) TYPE  CHAR1
*"     VALUE(P_SEMSG) TYPE  T100-TEXT
*"  TABLES
*"      ZTMMKPL STRUCTURE  ZSMMKPL
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*& Date         User    Transport       Description
*& 10/14/2005   Shiva   UD1K917958     Performance tunnning.
*&--------------------------------------------------------------------&*

*&--------------------------------------------------------------------&*
*&                       Note: IMPORTANT
*& We are using 'left join' if you feel we are getting kanabn card info
*& for "TO's" not created then change that to 'inner join' in selection.
*&--------------------------------------------------------------------*&
  if p_prvbe ca '*'.
    replace '*' with '%' into p_prvbe.
  endif.

  if p_matnr ca '*'.
    replace '*' with '%' into p_matnr.
    w_matnr = p_matnr.
  else.
    w_matnr = p_matnr.
  endif.

* comment by Furong
*  if not w_matnr is initial.
*    select prvbe pkbst matnr behaz behmg
*                       into table it_kan_info
*                       from pkhd as t1
*                       inner join pkps as t2
*                       on t2~pknum = t1~pknum
*                       where werks = p_werks
*                       and   prvbe like p_prvbe
*                       and   matnr like w_matnr.
*  else.
*    select prvbe pkbst matnr behaz behmg
*                       into table it_kan_info
*                       from pkhd as t1
*                       inner join pkps as t2
*                       on t2~pknum = t1~pknum
*                       where werks = p_werks
*                       and   prvbe like p_prvbe.
*  endif.

* Changed by Furong on 06/20/05
  if not w_matnr is initial.
    select prvbe stuzt  t1~matnr behaz behmg pkbst
                       into table it_kan_info
                       from pkhd as t1
                       inner join pkps as t2
                       on t2~pknum = t1~pknum
                      inner join ltbp as t3
                       on t3~tbnum = t2~tbnum
                       and t3~matnr = t1~matnr
                       left join ltak as t4
                       on t4~tbnum = t3~tbnum
                       and t4~lgnum = t3~lgnum
                       where t1~werks = p_werks
                       and   prvbe like p_prvbe
                       and   t1~matnr like w_matnr
                       and t3~lgnum = c_p01.
  else.
    select prvbe stuzt t1~matnr behaz behmg pkbst
                       into table it_kan_info
                       from pkhd as t1
                       inner join pkps as t2
                       on t2~pknum = t1~pknum
                       inner join ltbp as t3
                       on t3~tbnum = t2~tbnum
                       and t3~matnr = t1~matnr
                       left join ltak as t4
                       on t4~tbnum = t3~tbnum
                       and t4~lgnum = t3~lgnum
                       where t1~werks = p_werks
                       and   prvbe like p_prvbe
                       and t3~lgnum = c_p01.
  endif.
  if sy-subrc ne 0.
    p_rfsta = 'E'.
    p_semsg = text-007.
    exit.
  else.
    sort it_kan_info by prvbe stuzt matnr .
    loop at it_kan_info into wa_kan_info.
      at new matnr.
        w_matcnt = w_matcnt + 1.
      endat.
      case wa_kan_info-pkbst.
        when 2.
          w_empcnt = w_empcnt + 1.
        when 4.
          w_intcnt = w_intcnt + 1.
        when 5.
          w_fulcnt = w_fulcnt + 1.
      endcase.
      wa_sum_result-prvbe = wa_kan_info-prvbe.
      wa_sum_result-stuzt = wa_kan_info-stuzt.
      at end of stuzt.
        wa_sum_result-matcnt = w_matcnt.
        wa_sum_result-empcnt = w_empcnt.
        wa_sum_result-intcnt = w_intcnt.
        wa_sum_result-fulcnt = w_fulcnt.
        append wa_sum_result to it_sum_result.

        clear: wa_sum_result, w_matcnt, w_empcnt,
               w_intcnt, w_fulcnt.
      endat.
      at end of prvbe.
        clear: wa_sum_result, w_matcnt, w_empcnt,
               w_intcnt, w_fulcnt.
      endat.
*      at end of prvbe.
*        wa_sum_result-prvbe = wa_kan_info-prvbe.
*        wa_sum_result-matcnt = w_matcnt.
*        wa_sum_result-empcnt = w_empcnt.
*        wa_sum_result-intcnt = w_intcnt.
*        wa_sum_result-fulcnt = w_fulcnt.
*        append wa_sum_result to it_sum_result.
*
*        clear: wa_sum_result, w_matcnt, w_empcnt,
*               w_intcnt, w_fulcnt.
*      endat.
    endloop.
    p_rfsta = 'S'.
    p_semsg = space.
    ZTMMKPL[] = it_sum_result[].
  endif.


ENDFUNCTION.
