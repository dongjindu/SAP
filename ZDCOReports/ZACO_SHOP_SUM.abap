*--------------------------------------------------------------------*
* Prgram  : ZACO20R_SHOP_NEW                                         *
* Date    : 2006.08.08                                               *
* Author  : JHS                                                      *
*--------------------------------------------------------------------*

REPORT  zaco20r_shop_new MESSAGE-ID zmco NO STANDARD PAGE HEADING.

*--- ALV
TYPE-POOLS: SLIS.
DATA : W_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
       W_EVENTCAT TYPE SLIS_T_EVENT WITH HEADER LINE,
       W_SELFIELD TYPE SLIS_SELFIELD,
       W_SORTCAT  TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
       W_COL_POS  TYPE I,
       W_PROGRAM  LIKE SY-REPID,
       W_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER,
       W_LINE1 TYPE SLIS_LISTHEADER.

DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      GS_LAYOUT   TYPE SLIS_LAYOUT_ALV,
      GT_SP_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GT_EVENTS   TYPE SLIS_T_EVENT,
      GT_SORTS    TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      GS_PRNT     TYPE SLIS_PRINT_ALV,
      G_REPID     LIKE SY-REPID.
*---- ALV

tables: ztco_shop_cc.

data: begin of itab occurs 0,
        WKGBTR     like ztco_shop_cc-WKGBTR    ,
        WKGBTR2    like ztco_shop_cc-WKGBTR2   ,
        ADD_WKGBTR like ztco_shop_cc-ADD_WKGBTR,
        WIP_AMT    like ztco_shop_cc-WIP_AMT   ,
        WIP_PAMT   like ztco_shop_cc-WIP_PAMT  ,
        SCRAP_AMT  like ztco_shop_cc-SCRAP_AMT ,
        MANU_AMT   like ztco_shop_cc-MANU_AMT  ,
        GR_AMT     like ztco_shop_cc-GR_AMT    ,
        SINGLE_AMT like ztco_shop_cc-SINGLE_AMT,
        MULTI_AMT  like ztco_shop_cc-MULTI_AMT ,
        MULTI_SAMT like ztco_shop_cc-MULTI_SAMT,
        MULTI_MAMT like ztco_shop_cc-MULTI_MAMT,
        MISC       like ztco_shop_cc-MISC      ,
        TARGET_AMT like ztco_shop_cc-TARGET_AMT,
      end of itab.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
PARAMETERS     : p_kokrs LIKE ztco_shop_cc-kokrs  MEMORY ID cac,
                 p_bdatj LIKE ztco_shop_cc-bdatj  MEMORY ID bdtj.
SELECT-OPTIONS : s_poper FOR  ztco_shop_cc-poper  MEMORY ID popr,
                 s_TYPPS for  ztco_shop_cc-TYPPS,
                 s_KSTAR for  ztco_shop_cc-KSTAR,
                 s_RESOU for  ztco_shop_cc-RESOU,
                 s_ARTNR for  ztco_shop_cc-ARTNR,
                 s_elemt FOR  ztco_shop_cc-elemt.

  select
        sum( WKGBTR      )
        sum( WKGBTR2     )
        sum( ADD_WKGBTR  )
        sum( WIP_AMT     )
        sum( WIP_PAMT    )
        sum( SCRAP_AMT   )
        sum( MANU_AMT    )
        sum( GR_AMT      )
        sum( SINGLE_AMT  )
        sum( MULTI_AMT   )
        sum( MULTI_SAMT  )
        sum( MULTI_MAMT  )
        sum( MISC        )
        sum( TARGET_AMT  )
       into table itab
       from ztco_shop_cc
       where kokrs = p_kokrs
         and bdatj = p_bdatj
         and poper in s_poper
         and typps in s_typps
         and kstar in s_kstar
         and resou in s_resou
         and artnr in s_artnr
         and elemt in s_elemt.



  PERFORM FIELD_SETTING(ZCOGSREV) TABLES GT_FIELDCAT USING :
  'WKGBTR'      'Current '    '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WKGBTR2'     'Add.In 1'    '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'ADD_WKGBTR'  'Add.In 2'    '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WIP_AMT'     'WIP'         '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'WIP_PAMT'    'WIP prev'    '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'SCRAP_AMT'   'Scrap'       '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MANU_AMT'    'Manuf.'      '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'GR_AMT'      'GR$'         '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'SINGLE_AMT'  'Single'      '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MULTI_AMT'   'Multi'       '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MULTI_SAMT'  'Multi-L'     '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MULTI_MAMT'  'Multi-M'     '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'MISC'        'Misc'        '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
  'TARGET_AMT'  'Target'      '18' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.


  G_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM = G_REPID
            IT_FIELDCAT        = GT_FIELDCAT
            I_SAVE             = 'A'
       TABLES
            T_OUTTAB           = ITAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
