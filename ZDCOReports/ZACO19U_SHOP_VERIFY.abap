REPORT ZACO19U_SHOP_VERIFY .

parameters : p_bdatj like ztco_shop_cc-bdatj,
             p_poper like ztco_shop_cc-poper.

data : Diff like ztco_shop_cc-wip_amt,
       l_abs_diff like ztco_shop_cc-wip_amt.


DATA : filename LIKE rcgwwipath-valpath.

data : it_shop_cc  like ztco_shop_cc occurs 0 with header line,
       it_shop_cc2 like ztco_shop_cc occurs 0 with header line,
       result   like ztco_shop_cc occurs 0 with header line,
       it_shopcst  like ztco_shopcost_a3 occurs 0 with header line.



  select *
     into corresponding fields of table it_shop_cc
     from ztco_shop_cc
     where BDATJ = '2005' "p_bdatj
       and POPER = '09' . " p_poper .
*       and aufnr = '000001000080'.


    select *
     into corresponding fields of table it_shopcst
     from ztco_shopcost_a3
     where BDATJ = '2005' " p_bdatj
       and POPER = '09' . "p_poper.
*       and aufnr = '000001000080'.



  loop at it_shop_cc .
      move-corresponding it_shop_cc to it_shop_cc2.
      clear it_shop_cc2-elemt.
      collect it_shop_cc2.
      clear it_shop_cc2.
  endloop.

sort it_shopcst.
data : l_amt type p decimals 4.

  loop at it_shop_cc2.
     clear it_shopcst.
     read table it_shopcst with key AUFNR = it_shop_cc2-AUFNR
                                    RESOU = it_shop_cc2-resou.
     clear diff.
     diff = it_shop_cc2-WKGBTR - it_shopcst-WKGBTR.
     l_abs_diff = abs( diff ) .
     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '001'.
          collect result. clear result.
     endif.


     clear diff.
     diff = it_shop_cc2-WKGBTR2 - it_shopcst-WKGBTR2.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '002'.
          collect result. clear result.
     endif.

     clear diff.
     diff = it_shop_cc2-ADD_WKGBTR - it_shopcst-ADD_WKGBTR.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '003'.
          collect result. clear result.
     endif.


*     clear diff.
*     diff = it_shop_cc2-WIP_AMT - it_shopcst-WIP_AMT.
*     l_abs_diff = abs( diff ) .
*     if  l_abs_diff > 1.
*          move-corresponding   it_shop_cc2 to result.
*          result-elemt = '004'.
*          collect result. clear result.
*     endif.
*
*
*     clear diff.
*     diff = it_shop_cc2-WIP_pAMT - it_shopcst-WIP_pAMT.
*     l_abs_diff = abs( diff ) .
*
*     if  l_abs_diff > 1.
*          move-corresponding   it_shop_cc2 to result.
*          result-elemt = '005'.
*          collect result. clear result.
*     endif.
*

     clear diff.
     diff = it_shop_cc2-SCRAP_AMT - it_shopcst-SCRAP_AMT.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '006'.
          collect result. clear result.
     endif.


     clear diff.
     diff = it_shop_cc2-GR_AMT - it_shopcst-GR_AMT.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '007'.
          collect result. clear result.
     endif.

     clear diff.
     diff = it_shop_cc2-MULTI_SAMT - it_shopcst-MULTI_SAMT.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '008'.
          collect result. clear result.
     endif.

     clear diff.
     diff = it_shop_cc2-MULTI_MAMT - it_shopcst-MULTI_MAMT.
     l_abs_diff = abs( diff ) .

     if  l_abs_diff > 10.
          move-corresponding   it_shop_cc2 to result.
          result-elemt = '009'.
          collect result. clear result.
     endif.

endloop.


  CONCATENATE 'C:\' 'VERIFY'  '.xls' INTO filename .
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            filename = filename
            filetype = 'DAT'
            mode     = 'A'
       TABLES
            data_tab = RESULT.
BREAK-POINT.
loop at result.
   write : / result-aufnr, result-resou, result-elemt.
endloop.
