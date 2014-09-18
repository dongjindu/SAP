FUNCTION ZFMMMAT_LIST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_PRVBE) TYPE  PKHD-PRVBE DEFAULT 'BXS'
*"     VALUE(P_PKBST) TYPE  PKPS-PKBST DEFAULT '2'
*"     VALUE(P_WERKS) TYPE  PKHD-WERKS DEFAULT 'E001'
*"  TABLES
*"      ZTMMATLIST STRUCTURE  ZSMMATLIST
*"----------------------------------------------------------------------
************************************************************************
*  Date         Developer   Request         Description
* 04/20/2007    Manju       UD1K940395      Initial Coding ( Return
*                                           Kanban list for given input)
* 95/03/2007    Manju       UD1K940472      Sort output by storage BIN
************************************************************************
  data : begin of it_tab occurs 0,
          matnr like pkhd-matnr,
          count type i,
          BEHMG  like pkhd-BEHMG,
         end of it_tab,
         begin of it_mlgt occurs 0,
          matnr like mlgt-matnr,
          lgpla like mlgt-lgpla,
         end of it_mlgt.

  data: v_lgtyp like mlgt-lgtyp,
        v_lgort like pvbe-lgort.


  if not ( p_werks is initial and P_PRVBE is initial  and
           P_PKBST is initial ).

* Get Material wise count from PKHD & PKPS for the given input
    select  a~matnr count( * ) a~BEHMG
           into table it_tab from pkhd as a inner join pkps as b on
                                  a~pknum = b~pknum
           where werks eq p_werks and
                 PRVBE eq P_PRVBE and
                 PKBST eq P_PKBST
            group by   a~matnr a~BEHMG   .

    if not it_tab[] is initial.

** Changed by Furong on 12/18/08
      select single lgort into v_lgort
        from pvbe
        where werks = p_werks
          and PRVBE eq P_PRVBE.
     if v_lgort = 'E200'.
       v_lgtyp = '623'.
     else.
       v_lgtyp = '523'.
     endif.
** End of change

* Get Storage BIN for the above selected materials
      select matnr lgpla into table it_mlgt
             from mlgt for all entries in it_tab
              where matnr = it_tab-matnr and
                    lgnum = 'P01'  and
** Changed by Furong on 12/17/08
*                     lgtyp = '523'
                    lgtyp = v_lgtyp.
** End of change
    endif.
    sort it_mlgt by matnr.
* Return Output
    loop at it_tab.
      ZTMMATLIST-matnr = it_tab-matnr.
      ZTMMATLIST-BEHAZ = it_tab-count.
      read table it_mlgt with key matnr = it_tab-matnr.
      if sy-subrc eq 0.
        ZTMMATLIST-LGPLA = it_mlgt-lgpla.
      endif.
        ZTMMATLIST-BEHMG = it_tab-BEHMG * ZTMMATLIST-BEHAZ.
      collect ZTMMATLIST. clear ZTMMATLIST.

    endloop.
* Begin of changes - UD1K940472
   sort ZTMMATLIST by LGPLA.
* End of changes - UD1K940472

  endif.

ENDFUNCTION.
