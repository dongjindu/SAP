FUNCTION ZQM_SCRAP_MAT_OVERSEAS_201.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '0'
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL
*"     VALUE(BWARTWA_003) LIKE  BDCDATA-FVAL DEFAULT '551'
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(LGORT_005) LIKE  BDCDATA-FVAL DEFAULT 'P400'
*"     VALUE(XFULL_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(WVERS2_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_01_008) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(ERFMG_01_009) LIKE  MSEG-ERFMG OPTIONAL
*"     VALUE(AUFNR_010) LIKE  BDCDATA-FVAL DEFAULT 'CP001'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"     VALUE(I_CUSTOMIZING) LIKE  V_TQ85 STRUCTURE  V_TQ85 OPTIONAL
*"     VALUE(I_MANUM) LIKE  QMSM-MANUM OPTIONAL
*"     VALUE(I_FBCALL) LIKE  ZTMM_MARA-FABKZ OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"     VALUE(E_QNQMASM0) LIKE  QNQMASM0 STRUCTURE  QNQMASM0
*"     VALUE(E_QNQMAQMEL0) LIKE  QNQMAQMEL0 STRUCTURE  QNQMAQMEL0
*"     VALUE(E_BUCH) TYPE  QKZ
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      TI_IVIQMFE STRUCTURE  WQMFE OPTIONAL
*"      TI_IVIQMUR STRUCTURE  WQMUR OPTIONAL
*"      TI_IVIQMSM STRUCTURE  WQMSM OPTIONAL
*"      TI_IVIQMMA STRUCTURE  WQMMA OPTIONAL
*"      TI_IHPA STRUCTURE  IHPA OPTIONAL
*"      TE_CONTAINER STRUCTURE  SWCONT OPTIONAL
*"      TE_LINES STRUCTURE  TLINE OPTIONAL
*"----------------------------------------------------------------------
  tables: Mbew.
  data: zlgort like mard-lgort.
  data: zaufnr like cobl-aufnr.
  data: zERFMG(12) type c.
  data: it_mbew like mbew occurs 0 with header line.
  data: check, n(3).
  data: zebeln like ekbe-ebeln,
          zebelp like ekbe-ebelp,
          zbudat like ekbe-budat.

  data: z_zdate(10) type c.
  data: z_zdate1(132) type c.


  WRITE SY-DATUM TO Z_ZDATE.

*
  clear: n.
  select single * from mbew into it_mbew where matnr = i_viqmel-matnr
                                   and  bwkey = i_viqmel-mawerk
                                   and  bklas = '3000'.

  append it_mbew.
  DESCRIBE TABLE it_mbew lines n.





  clear: zebeln, zebelp, check.
  select   ebeln ebelp  budat
    into (zebeln, zebelp, zbudat) from ekbe up to 1 rows
                                where bewtp = 'E'
                                and bwart = '101'
                             and matnr = i_viqmel-matnr
          order by  budat descending.
  endselect.
  if zebeln+0(2) = '42'.
    check = 'X'.
  endif.


  if check <> 'X' or n = 0.
*MESSAGE ID ZMQM TYPE W NUMBER 005.
    exit.
  else.

* If there are multiple storage locations for the same part then
* add logic for selecting storage loc
* Check with Quentin Henry

 select single lgort into zlgort from mard where matnr = i_viqmel-matnr
                                          and werks = i_viqmel-mawerk
                                          and lgort <> '9999'
                                          and labst >= i_viqmel-rkmng.

* For CKD parts
* If no storage location with stock then cannot scrap

    if sy-subrc <> 0.

   Move 'No Stock in any Storage Location' to messtab-msgv1.
 append messtab.
endif.

* End condition to check storage locn






    clear: XFULL_006.
    subrc = 0.
    perform bdc_nodata      using NODATA.

    perform open_group      using GROUP USER KEEP HOLDDATE CTU.

    perform bdc_dynpro      using 'SAPMM07M' '0400'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'XFULL'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '/00'.
    perform bdc_field       using 'MKPF-BLDAT'
*                              BLDAT_001.
*                                   sy-datum.
                                z_zdate.
    perform bdc_field       using 'MKPF-BUDAT'
*                              BUDAT_002.
*                                   sy-datum.
                                  z_zdate.
    perform bdc_field       using 'RM07M-BWARTWA'
*                              BWARTWA_003.
                                   '201'.
*perform bdc_field       using 'DKACB-FMORE'
*                              ' '.
    perform bdc_field       using 'RM07M-WERKS'
*                              WERKS_004.
                                  i_viqmel-mawerk.
    perform bdc_field       using 'RM07M-LGORT'
*                              LGORT_005.
                                  zlgort.
*perform bdc_field       using 'RM07M-XNUVR'
*                              XNUVR_006.
    perform bdc_field       using 'XFULL'
                                  XFULL_006.
    perform bdc_field       using 'RM07M-WVERS2'
                                  WVERS2_007.


    perform bdc_dynpro      using 'SAPMM07M' '0421'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'MSEG-ERFMG(01)'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=BU'.
    move i_viqmel-rkmng to zERFMG.
    perform bdc_field       using 'MSEG-MATNR(01)'
*                              MATNR_01_008.
                                   i_viqmel-matnr.
*                               '312103K600'.
    perform bdc_field       using 'MSEG-ERFMG(01)'
                                     zerfmg.
*                               i_viqmel-rkmng.
*                              ERFMG_01_009.


    perform bdc_field       using 'MSEGK-KONTO'
                                   '123200'.

    perform bdc_transaction tables messtab
    using                         'MB1A'
                                  CTU
                                  MODE
                                  UPDATE.
    if sy-subrc <> 0.
      subrc = sy-subrc.
      exit.
    endif.



*TABLES: VIQMEL, ZSRF_PICKER_LIST, MSEG.
    .
    DATA: ZMATNR LIKE VIQMEL-MATNR,
          ZRKMNG LIKE VIQMEL-RKMNG,
*      ZRKMNG(11) TYPE C,
          ZMGEIN LIKE VIQMEL-MGEIN,
          ZMAWERK LIKE VIQMEL-MAWERK.
    DATA: T_LIST1 LIKE ZSRF_PICKER_LIST OCCURS 0 WITH HEADER LINE.
    DATA: ZNLPLA LIKE ZSRF_PICKER_LIST-NLPLA,
          ZNLTYP LIKE ZSRF_PICKER_LIST-NLTYP,
          ZMBLNR LIKE MSEG-MBLNR.
    DATA: ZTBNUM LIKE MSEG-TBNUM,
          ZLGNUM LIKE MSEG-LGNUM.



    loop at messtab where msgtyp = 'S'.
      move messtab-msgv1 to zmblnr.
    endloop.

    SELECT SINGLE TBNUM LGNUM INTO (ZTBNUM, ZLGNUM) FROM MSEG
    WHERE MBLNR = ZMBLNR.


    ZMATNR = I_VIQMEL-MATNR.

    ZRKMNG = I_VIQMEL-RKMNG.
    ZMGEIN = I_VIQMEL-MGEIN.
    ZMAWERK = I_VIQMEL-MAWERK.

*LOOP AT T_LIST1.
    T_LIST1-MATNR =  Zmatnr.
    APPEND T_LIST1.
*ENDLOOP.

* Do not pick up Control cycle bin
* Use Constant values
*    CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT01_CHECK'
** EXPORTING
**   ZINTCALL       =
** IMPORTING
**   E_MESS         =
**   ZRESULT        =
*      TABLES
*        T_LIST         =  T_LIST1.
*
*    LOOP AT T_LIST1.
*      MOVE T_LIST1-NLPLA TO ZNLPLA.
*      MOVE T_LIST1-NLTYP TO ZNLTYP.
*    ENDLOOP.

    CALL FUNCTION 'ZQ3_TRANSFER_ORDER_CREATE'
     EXPORTING
       CTU             = 'X'
       MODE            = 'E'
       UPDATE          = 'L'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
       LGNUM_001       =  ZLGNUM
*    'P01'
       TBNUM_002       =  ZTBNUM
*   " '72907'
       ALAKT_003       = 'X'
       DUNKL_004       = 'H'
       ANFME_005       = ZRKMNG
*   '1'
       ALTME_006       =  ZMGEIN
*   'EA'
       SQUIT_007       = 'X'
       VLTYP_008       = '999' " ZNLTYP
*  "'435'
       VLPLA_009       = 'SCRAP_RF' "ZNLPLA
*   "'C1-01-01'
       LGTY0_010       = '431'
       LGTY1_011       = '432'
       LGTY2_012       = '433'
       LGTY3_013       = '434'
       LGTY4_014       = '435'
       LGTY5_015       = '436'
       LGTY6_016       = '437'
       LGTY7_017       = '441'
       LGTY8_018       = '442'
       LGTY9_019       = '443'
       LGT10_020       = '444'
       LGT11_021       = '445'
* IMPORTING
*   SUBRC           =
* TABLES
*   MESSTAB         = ZMESSTAB
              .

  endif.
  perform close_group using     CTU.

ENDFUNCTION.
*INCLUDE BDCRECXY .
