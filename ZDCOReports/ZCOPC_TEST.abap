REPORT ZCOPC_TEST .
*
* Andy Choi; Testing purpose
*

PARAMETERS:
*  Material
*d pm_mtnrv like mara-matnr memory id mat obligator           "HGC059252
   pm_mtnrv LIKE mara-matnr MEMORY ID mat,                    "HGC059252
*  Werk
   pm_werks LIKE marc-werks MEMORY ID wrk,
*  Stuecklistenalternative
   pm_stlal LIKE stko-stlal,
*  Stuecklistenverwendung
   pm_stlan LIKE stzu-stlan default '1',  "production
*  Stuecklistenanwendung
   pm_capid LIKE tc04-capid default 'PP01',
*  Datum gueltig ab
   pm_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

DATA:  lf_bqpim LIKE bqpim,
       lf_bqpex LIKE bqpex.
DATA:  w_kis1 LIKE kis1 OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF itab OCCURS 0,
    kstar  LIKE kis1-kstar ,
    typps  LIKE kis1-typps ,
    matnr(19) TYPE c, "kis1-matnr ,
    ltext  LIKE kis1-ltext ,
*    lstar  LIKE kis1-lstar ,
*    kostl  LIKE kis1-kostl ,
    menge  LIKE kis1-menge ,
    meeht  LIKE kis1-meeht ,
    wertn  LIKE kis1-wertn ,
    gpreis LIKE kis1-gpreis,
    strat  LIKE kis1-strat ,
    lifnr  LIKE kis1-lifnr ,
    name1  LIKE lfa1-name1 ,  "vendor name
    infnr  LIKE kis1-infnr ,
    upgvc  LIKE kis1-matnr ,  "UPG
    upg(1) TYPE C,            "Multiple UPG-VC
    upgtx  LIKE kis1-ltext ,  "UPG TEXT
END OF itab.

DATA: BEGIN OF i_stb OCCURS 1000.
         INCLUDE STRUCTURE stpox.
DATA: END OF i_stb.

DATA: BEGIN OF lt_bom OCCURS 0,
         INDEX  like stpox-index,
         STUFE  like stpox-stufe,
         DISST  like stpox-DISST, "low-level code
         IDNRK  like stpox-IDNRK, "Object(Mat)
         POSNR  like stpox-posnr,
         HDNFO  like stpox-hdnfo,
         UPGVC  like stpox-IDNRK, "UPG
         MTART  like stpox-MTART, "mat type
         XCHAR  like stpox-XCHAR, "batch-mgt
         DUMPS  like stpox-DUMPS, "phantom
         STKKZ  like stpox-STKKZ, "assemble ind
         MSTAE  like stpox-MSTAE, "mat-status
         MMSTA  like stpox-MMSTA, "mat-status(plant)
         MENGE  like stpox-MENGE, "Qty
         AUSSS  like stpox-AUSSS, "assembly scrap
         KAUSF  like stpox-kausf, "component scrap
         BAUSF  like stpox-BAUSF, "assembly scrap%
         MEINS  like stpox-MEINS, "UoM
*         MNGLG  like stpox-MNGLG, "qty (UoM)
*         MNGKO  like stpox-MNGKO, "qty (Component UoM)
         SOBSL  like stpox-SOBSL, "Special.Proc.
         RGEKZ  like stpox-RGEKZ, "b/f ind.
         LGPRO  like stpox-LGPRO, "S.Loc
         MATMK  like stpox-MATMK, "Mat.Group
         POSTP  like stpox-POSTP, "Type
         SORTF  like stpox-SORTF, "SortString
         stawn  LIKE stpox-stawn, "Duty Code
         XTLTY  like stpox-XTLTY, "BOM category (next level)
         XTLNR  like stpox-XTLNR, "BOM no.
         EITM   like stpox-EITM,  "EndItem
         STGB   like stpox-STGB,  "Str.Type
         OJTXB  like stpox-ojtxb, "description
         UPGTX  like stpox-ojtxb, "description
      END OF lt_bom.

DATA: BEGIN OF lt_upg OCCURS 0,
         idnrk LIKE stpo-idnrk,
         maktx LIKE makt-maktx,
      END OF lt_upg.

*get 1st level BOM
*SELECT idnrk maktx INTO CORRESPONDING FIELDS OF TABLE lt_upg
*  FROM stpo AS s
*  INNER JOIN makt AS t
*     ON t~matnr = s~idnrk
*    AND t~spras = sy-langu
*  WHERE s~stlty = 'M'
*    AND s~STLNR =
*    AND s~datuv   <= pm_datuv.

tables: mast.
select single * from mast
   where matnr = pm_mtnrv
     and werks = pm_werks
     and stlal = pm_stlal
     and stlan = pm_stlan.
check sy-subrc = 0.

*get 1st level BOM
SELECT idnrk maktx INTO CORRESPONDING FIELDS OF TABLE lt_upg
  FROM stpo AS s
  INNER JOIN makt AS t
     ON t~matnr = s~idnrk
    AND t~spras = sy-langu
  WHERE stlty = 'M'
    AND stlnr =  mast-stlnr
    AND datuv <= pm_datuv.

CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
  EXPORTING
    capid = pm_capid
    datuv = pm_datuv
    werks = pm_werks
    mtnrv = pm_mtnrv
    stlan = pm_stlan
    stlal = pm_stlal
    mehrs = 'X'           "Multi-level explosion
    mmory = '1'  "Memory use On(1)
    SANKA = 'X'  "Only Costing Relevency(inc.Phantom)
  TABLES
    stb                         = i_stb.

loop at i_stb.
  move-corresponding i_stb to lt_bom. append lt_bom.
endloop.

*get UPG
DATA: l_cnt   TYPE i.

SORT i_stb BY index stufe ASCENDING.
DATA: w_upg LIKE i_stb.
LOOP AT i_stb.
  IF i_stb-stufe = 1.
    w_upg = i_stb.
  ENDIF.

  READ TABLE lt_bom WITH KEY INDEX = i_stb-index.
  IF sy-subrc = 0.
    l_cnt = sy-tabix.
    lt_bom-upgvc = w_upg-idnrk.
    READ TABLE lt_upg WITH KEY idnrk = w_upg-idnrk.
    lt_bom-upgtx = lt_upg-maktx.

    MODIFY lt_bom INDEX l_cnt.
  ELSE.
    l_cnt = 0.
  ENDIF.
ENDLOOP.

PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
'INDEX'     'INDEX'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STUFE'     'STUFE'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'DISST'     'LvCode'        '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'IDNRK'     'Object(Mat)'   '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGVC'     'UPG'           '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'POSNR'     'POSNR'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'HDNFO'     'HDNFO'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MTART'     'Mat type'      '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'XCHAR'     'batch-mgt'     '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'DUMPS'     'Phantom'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STKKZ'     'AssyInd'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MSTAE'     'Stat1'         '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MMSTA'     'Stat2'         '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MENGE'     'Qty        '   '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'AUSSS'     'Assy.Scrap'    '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KAUSF'     'Comp.Scrap'    '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'BAUSF'     'Scrap%'        '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEINS'     'UoM        '   '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SOBSL'     'Sp.Prc'        '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'RGEKZ'     'BF ind'        '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LGPRO'     'S.Loc'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MATMK'     'Mat.Grp'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'POSTP'     'Type'          '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SORTF'     'SortStr'       '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'XTLTY'     'BOM categor'   '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'XTLNR'     'BOM no.    '   '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'EITM'      'EndItem    '   '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STGB'      'Str.Type   '   '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STAWN'     'DutyCd'        '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SORTF'     'SortStr'       '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGTX'     'UPG Desc'      '30' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'OJTXB'     'Description'   '30' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

g_repid = sy-repid.
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
          i_callback_program = g_repid
          it_fieldcat        = gt_fieldcat
          i_save             = 'A'
     TABLES
          t_outtab           = lt_bom
     EXCEPTIONS
          program_error      = 1
          OTHERS             = 2.
