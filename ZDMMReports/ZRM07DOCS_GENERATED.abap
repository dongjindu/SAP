*19991111180301
* SELECT command improved with note  555893

* Generated include for RM07DOCS
* Please do not change manually as any changes here will
* be completely overwritten when the settings in view
* V_MMIM_REP_CUST are changed for this report.

* Selection screen
selection-screen begin of block mseg with frame title text-001.
  select-options MATNR for MSEG-MATNR memory id MAT.
  select-options WERKS for MSEG-WERKS memory id WRK.
  select-options LGORT for MSEG-LGORT memory id LAG.
  select-options CHARG for MSEG-CHARG memory id CHA.
  select-options LIFNR for MSEG-LIFNR memory id LIF.
  select-options KUNNR for MSEG-KUNNR memory id KUN.
  select-options BWART for MSEG-BWART memory id BWA.
  select-options SOBKZ for MSEG-SOBKZ.
selection-screen end of block mseg.
selection-screen begin of block mkpf with frame title text-002.
  select-options BUDAT for MKPF-BUDAT.
  select-options USNAM for MKPF-USNAM memory id USR.
  select-options VGART for MKPF-VGART.
  select-options XBLNR for MKPF-XBLNR.
selection-screen end of block mkpf.

* Internal table for data selection
data: begin of itab occurs 0.
  data: ANLN1 like MSEG-ANLN1 .
  data: ANLN2 like MSEG-ANLN2 .
  data: APLZL like MSEG-APLZL .
  data: AUFNR like MSEG-AUFNR .
  data: AUFPL like MSEG-AUFPL .
  data: BKTXT like MKPF-BKTXT .
  data: BLDAT like MKPF-BLDAT .
  data: BPMNG like MSEG-BPMNG .
  data: BPRME like MSEG-BPRME .
  data: BSTME like MSEG-BSTME .
  data: BSTMG like MSEG-BSTMG .
  data: BUDAT like MKPF-BUDAT .
  data: BUKRS like MSEG-BUKRS .
  data: BWART like MSEG-BWART .
  data: BWTAR like MSEG-BWTAR .
  data: CHARG like MSEG-CHARG .
  data: CPUDT like MKPF-CPUDT .
  data: CPUTM like MKPF-CPUTM .
  data: DMBTR like MSEG-DMBTR .
  data: EBELN like MSEG-EBELN .
  data: EBELP like MSEG-EBELP .
  data: ERFME like MSEG-ERFME .
  data: ERFMG like MSEG-ERFMG .
  data: EXBWR like MSEG-EXBWR .
  data: EXVKW like MSEG-EXVKW .
  data: GRUND like MSEG-GRUND .
  data: KDAUF like MSEG-KDAUF .
  data: KDEIN like MSEG-KDEIN .
  data: KDPOS like MSEG-KDPOS .
  data: KOSTL like MSEG-KOSTL .
  data: KUNNR like MSEG-KUNNR .
  data: KZBEW like MSEG-KZBEW .
  data: KZVBR like MSEG-KZVBR .
  data: KZZUG like MSEG-KZZUG .
  data: LGORT like MSEG-LGORT .
  data: LIFNR like MSEG-LIFNR .
  data: MATNR like MSEG-MATNR .
  data: MBLNR like MKPF-MBLNR .
  data: MEINS like MSEG-MEINS .
  data: MENGE like MSEG-MENGE .
  data: MJAHR like MKPF-MJAHR .
  data: NPLNR like MSEG-NPLNR .
  data: PS_PSP_PNR like MSEG-PS_PSP_PNR .
  data: RSNUM like MSEG-RSNUM .
  data: RSPOS like MSEG-RSPOS .
  data: SHKZG like MSEG-SHKZG .
  data: SOBKZ like MSEG-SOBKZ .
  data: USNAM like MKPF-USNAM .
  data: VGART like MKPF-VGART .
  data: VKWRT like MSEG-VKWRT .
  data: WAERS like MSEG-WAERS .
  data: WERKS like MSEG-WERKS .
  data: XABLN like MKPF-XABLN .
  data: XAUTO like MSEG-XAUTO .
  data: XBLNR like MKPF-XBLNR .
  data: ZEILE like MSEG-ZEILE .
data: end of itab.

* Data selection routine
form data_selection.

* define local working fields
data : l_cnt_matnr_i_eq     type i.
data : l_cnt_matnr_total    type i.

* working table for the fields to be transported
types : begin of stype_fields,
          fieldname         type  name_feld,
        end of stype_fields.
types : stab_fields         type standard table
                            of stype_fields
                            with default key.
data : l_t_fields           type stab_fields.

* create table with the fields to be transported
append 'MSEG~ANLN1' to l_t_fields.
append 'MSEG~ANLN2' to l_t_fields.
append 'MSEG~APLZL' to l_t_fields.
append 'MSEG~AUFNR' to l_t_fields.
append 'MSEG~AUFPL' to l_t_fields.
append 'MKPF~BKTXT' to l_t_fields.
append 'MKPF~BLDAT' to l_t_fields.
append 'MSEG~BPMNG' to l_t_fields.
append 'MSEG~BPRME' to l_t_fields.
append 'MSEG~BSTME' to l_t_fields.
append 'MSEG~BSTMG' to l_t_fields.
append 'MKPF~BUDAT' to l_t_fields.
append 'MSEG~BUKRS' to l_t_fields.
append 'MSEG~BWART' to l_t_fields.
append 'MSEG~BWTAR' to l_t_fields.
append 'MSEG~CHARG' to l_t_fields.
append 'MKPF~CPUDT' to l_t_fields.
append 'MKPF~CPUTM' to l_t_fields.
append 'MSEG~DMBTR' to l_t_fields.
append 'MSEG~EBELN' to l_t_fields.
append 'MSEG~EBELP' to l_t_fields.
append 'MSEG~ERFME' to l_t_fields.
append 'MSEG~ERFMG' to l_t_fields.
append 'MSEG~EXBWR' to l_t_fields.
append 'MSEG~EXVKW' to l_t_fields.
append 'MSEG~GRUND' to l_t_fields.
append 'MSEG~KDAUF' to l_t_fields.
append 'MSEG~KDEIN' to l_t_fields.
append 'MSEG~KDPOS' to l_t_fields.
append 'MSEG~KOSTL' to l_t_fields.
append 'MSEG~KUNNR' to l_t_fields.
append 'MSEG~KZBEW' to l_t_fields.
append 'MSEG~KZVBR' to l_t_fields.
append 'MSEG~KZZUG' to l_t_fields.
append 'MSEG~LGORT' to l_t_fields.
append 'MSEG~LIFNR' to l_t_fields.
append 'MSEG~MATNR' to l_t_fields.
append 'MKPF~MBLNR' to l_t_fields.
append 'MSEG~MEINS' to l_t_fields.
append 'MSEG~MENGE' to l_t_fields.
append 'MKPF~MJAHR' to l_t_fields.
append 'MSEG~NPLNR' to l_t_fields.
append 'MSEG~PS_PSP_PNR' to l_t_fields.
append 'MSEG~RSNUM' to l_t_fields.
append 'MSEG~RSPOS' to l_t_fields.
append 'MSEG~SHKZG' to l_t_fields.
append 'MSEG~SOBKZ' to l_t_fields.
append 'MKPF~USNAM' to l_t_fields.
append 'MKPF~VGART' to l_t_fields.
append 'MSEG~VKWRT' to l_t_fields.
append 'MSEG~WAERS' to l_t_fields.
append 'MSEG~WERKS' to l_t_fields.
append 'MKPF~XABLN' to l_t_fields.
append 'MSEG~XAUTO' to l_t_fields.
append 'MKPF~XBLNR' to l_t_fields.
append 'MSEG~ZEILE' to l_t_fields.

* analyse the select-option table for material
* numbers
  clear : l_cnt_matnr_total, l_cnt_matnr_i_eq.

  loop at matnr.
    add  1             to  l_cnt_matnr_total.

    if  not matnr-low     is initial  and
            matnr-sign    =  c_i      and
            matnr-option  =  c_eq     and
            matnr-high    is initial.
*     the table contains single a material number
      add  1           to  l_cnt_matnr_i_eq.
    else.
      exit.
    endif.
  endloop.

* process SELECT command depending on the
* required material selection
  if  l_cnt_matnr_total  > 0                 and
      l_cnt_matnr_total  = l_cnt_matnr_i_eq.
*   work with .. for all entries ...
    select (l_t_fields)
    into corresponding fields of table itab
    from mkpf inner join mseg
    on    mkpf~mandt = mseg~mandt
      and mkpf~mblnr = mseg~mblnr
      and mkpf~mjahr = mseg~mjahr
    for all entries in matnr
    where matnr = matnr-low
      and MKPF~BUDAT in BUDAT
      and MSEG~BWART in BWART
      and MSEG~CHARG in CHARG
      and MSEG~KUNNR in KUNNR
      and MSEG~LGORT in LGORT
      and MSEG~LIFNR in LIFNR
      and MSEG~SOBKZ in SOBKZ
      and MKPF~USNAM in USNAM
      and MKPF~VGART in VGART
      and MSEG~WERKS in WERKS
      and MKPF~XBLNR in XBLNR
.
  else.
*   work with the select command as usual
    select (l_t_fields)
    into corresponding fields of table itab
    from mkpf inner join mseg
    on    mkpf~mandt = mseg~mandt
      and mkpf~mblnr = mseg~mblnr
      and mkpf~mjahr = mseg~mjahr
       WHERE MKPF~BUDAT in BUDAT
         and MSEG~BWART in BWART
         and MSEG~CHARG in CHARG
         and MSEG~KUNNR in KUNNR
         and MSEG~LGORT in LGORT
         and MSEG~LIFNR in LIFNR
         and MSEG~MATNR in MATNR
         and MSEG~SOBKZ in SOBKZ
         and MKPF~USNAM in USNAM
         and MKPF~VGART in VGART
         and MSEG~WERKS in WERKS
         and MKPF~XBLNR in XBLNR
.
  endif.
endform.

* Data selection routine for short documents
form shortdocument_read.
select * from mari
  into corresponding fields of table maritab where
MATNR in MATNR
and
WERKS in WERKS
and
LGORT in LGORT
and
CHARG in CHARG
and
BWART in BWART
and
SOBKZ in SOBKZ
and
VGART in VGART
and
BUDAT in BUDAT
and
LIFNR in LIFNR
and
USNAM in USNAM
.
endform.

* Checking selections for archives documents
form archive_check_selections changing rc.
if
ITAB-BUDAT in BUDAT
and
ITAB-BWART in BWART
and
ITAB-CHARG in CHARG
and
ITAB-KUNNR in KUNNR
and
ITAB-LGORT in LGORT
and
ITAB-LIFNR in LIFNR
and
ITAB-MATNR in MATNR
and
ITAB-SOBKZ in SOBKZ
and
ITAB-USNAM in USNAM
and
ITAB-VGART in VGART
and
ITAB-WERKS in WERKS
and
ITAB-XBLNR in XBLNR
. rc = 0. else. rc = 4. endif.
endform.

* Control data for fieldcatalog, color, authorization, ...
form build_runtimetable.
  rx'MSEG MATNR       +01+01'.
  rx'MSEG WERKS       +02+02'.
  rx'MSEG LGORT       +03+03'.
  rx'MSEG BWART       +07+04'.
  rx'MSEG SOBKZ       +08+05'.
  rx'MKPF MBLNR        00+06'.
  rx'MSEG ZEILE        00+07'.
  rx'MKPF BUDAT       +09+08'.
  rx'MSEG ERFMG        00+09QERFME-'.
  rx'MSEG ERFME        00+10      +'.
  rx'MSEG ANLN1        00 00'.
  rx'MSEG ANLN2        00 00'.
  rx'MSEG APLZL        00+00'.
  rx'MSEG AUFNR        00 00'.
  rx'MSEG AUFPL        00+00'.
  rx'MKPF BKTXT        00 00'.
  rx'MKPF BLDAT        00 00'.
  rx'MSEG BPMNG        00+00QBPRME-'.
  rx'MSEG BPRME        00+00      +'.
  rx'MSEG BSTME        00 00      +'.
  rx'MSEG BSTMG        00 00QBSTME-'.
  rx'MSEG BUKRS        00 00'.
  rx'MSEG BWTAR        00 00'.
  rx'MSEG CHARG       +04+00'.
  rx'MKPF CPUDT        00+00'.
  rx'MKPF CPUTM        00+00'.
  rx'MSEG DMBTR        00 00CWAERS-X'.
  rx'MSEG EBELN        00+00'.
  rx'MSEG EBELP        00+00'.
  rx'MSEG EXBWR        00 00CWAERS-'.
  rx'MSEG EXVKW        00 00CWAERS-'.
  rx'MSEG GRUND        00 00'.
  rx'MSEG KDAUF        00 00'.
  rx'MSEG KDEIN        00 00'.
  rx'MSEG KDPOS        00 00'.
  rx'MSEG KOSTL        00 00'.
  rx'MSEG KUNNR       +06+00'.
  rx'MSEG KZBEW        00+00'.
  rx'MSEG KZVBR        00+00'.
  rx'MSEG KZZUG        00+00'.
  rx'MSEG LIFNR       +05+00'.
  rx'MSEG MEINS        00+00      +'.
  rx'MSEG MENGE        00+00QMEINS-'.
  rx'MKPF MJAHR        00+00'.
  rx'MSEG NPLNR        00+00'.
  rx'MSEG PS_PSP_PNR   00+00'.
  rx'MSEG RSNUM        00 00'.
  rx'MSEG RSPOS        00 00'.
  rx'MSEG SHKZG        00+00'.
  rx'MKPF USNAM       +10+00'.
  rx'MKPF VGART       +11+00'.
  rx'MSEG VKWRT        00 00CWAERS-X'.
  rx'MSEG WAERS        00 00      +X'.
  rx'MKPF XABLN        00 00'.
  rx'MSEG XAUTO        00 00'.
  rx'MKPF XBLNR        00 00'.
endform.

