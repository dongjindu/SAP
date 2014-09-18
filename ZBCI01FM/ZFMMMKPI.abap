FUNCTION ZFMMMKPI.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) TYPE  MARA-MATNR OPTIONAL
*"     VALUE(P_PRVBE) TYPE  PKHD-PRVBE
*"     VALUE(P_PKBST) TYPE  PKPS-PKBST
*"  EXPORTING
*"     VALUE(P_RFSTA) TYPE  CHAR1
*"     VALUE(P_SEMSG) TYPE  T100-TEXT
*"  TABLES
*"      ZTMMKPI STRUCTURE  ZSMMKPI
*"----------------------------------------------------------------------
**"     VALUE(P_STUZT) TYPE  LTAK-STUZT
*"  EXPORTING
*"     VALUE(P_RFSTA) TYPE  CHAR1
*"     VALUE(P_SEMSG) TYPE  T100-TEXT
*"  TABLES
*"      ZTMMKPI STRUCTURE  ZSMMKPI
*"----------------------------------------------------------------------
* *"     VALUE(P_STUZT) TYPE  LTAK-STUZT    "UD1K940229
*&--------------------------------------------------------------------&*
*&  Date         User     Transport       Description
*&  10/05/2005   Shiva   UD1K917841    1.select material with wild card
*&                                      and have as optional parameter.
*&                                     2. Filter the selection with
*&                                      supply area and time.
*&                                     3. split ablad at *
*&  10/13/2005   Shiva   UD1K917958      Performance tunning.
*&  04/02/2007   Manju   UD1K940229    Remove time as input and return
*                                      first record which meets input
*                                      Criteria
*&--------------------------------------------------------------------&*

  data: w_ablad1 like ltap-ablad,
        w_ablad2 like ltap-ablad.

  clear: w_matnr.

  if p_matnr ca '*'.
    replace '*' with '%' into p_matnr.
    w_matnr = p_matnr.
  else.
    w_matnr = p_matnr.
  endif.

  if w_matnr is initial.
    select t1~pknum t1~ablad t2~pkkey stdat stuzt ausfb
                    t5~matnr vltyp vlpla nltyp maktx t5~vsolm
           into table it_kbn_matinfo1 up to 1 rows
           from pkhd as t1
           inner join pkps as t2
             on t2~pknum = t1~pknum
           inner join ltbp as t3
             on t3~tbnum = t2~tbnum
             and t3~matnr = t1~matnr
           inner join ltak as t4
             on t4~tanum = t3~tanum
             and t4~lgnum = t3~lgnum
           inner join ltap as t5
             on  t5~lgnum = t4~lgnum
             and t5~tanum = t4~tanum
             and t5~matnr = t1~matnr
             and t5~pquit = t4~kquit
           where t1~prvbe = p_prvbe
           and   pkbst    = p_pkbst
*          and   stuzt    = p_stuzt
           and   kquit    = space
           and   t3~lgnum = c_p01.
  else.
    select t1~pknum t1~ablad t2~pkkey stdat stuzt ausfb
                    t5~matnr vltyp vlpla nltyp maktx t5~vsolm
           into table it_kbn_matinfo1
           from pkhd as t1
           inner join pkps as t2
             on t2~pknum = t1~pknum
           inner join ltbp as t3
             on t3~tbnum = t2~tbnum
             and t3~matnr = t1~matnr
           inner join ltak as t4
             on t4~tanum = t3~tanum
             and t4~lgnum = t3~lgnum
           inner join ltap as t5
             on  t5~lgnum = t4~lgnum
             and t5~tanum = t4~tanum
             and t5~matnr = t1~matnr
             and t5~pquit = t4~kquit
           where t1~prvbe = p_prvbe
           and   t1~matnr like w_matnr
           and   pkbst    = p_pkbst
*          and   stuzt    = p_stuzt
           and   kquit    = space
           and   t3~lgnum = c_p01.
  endif.

  if sy-subrc ne 0.
    p_rfsta = 'E'.
    p_semsg = text-009.
  else.
    loop at it_kbn_matinfo1 into wa_kbn_matinfo_1 .
      move-corresponding wa_kbn_matinfo_1 to wa_kbn_matinfo.
      split wa_kbn_matinfo_1-ablad at '*' into w_ablad1 w_ablad2 .
      wa_kbn_matinfo-nlpla = w_ablad1.
      append wa_kbn_matinfo to ZTMMKPI.
      clear wa_kbn_matinfo_1.
      clear wa_kbn_matinfo.
    endloop.
*    ZTMMKPI[] = it_kbn_matinfo1[].
    p_rfsta = 'S'.
    p_semsg = space.
  endif.

ENDFUNCTION.
