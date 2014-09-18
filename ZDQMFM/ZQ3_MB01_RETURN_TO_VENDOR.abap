FUNCTION ZQ3_MB01_RETURN_TO_VENDOR.
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
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE OPTIONAL
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL DEFAULT '02/09/2007'
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL DEFAULT '02/09/2007'
*"     VALUE(BKTXT_003) LIKE  BDCDATA-FVAL DEFAULT 'Test Document'
*"     VALUE(BWARTWE_004) LIKE  BDCDATA-FVAL DEFAULT '122'
*"     VALUE(EBELN_005) LIKE  BDCDATA-FVAL DEFAULT '4600000160'
*"     VALUE(EBELP_006) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(WERKS_007) LIKE  BDCDATA-FVAL DEFAULT 'p001'
*"     VALUE(GRUND_008) LIKE  BDCDATA-FVAL DEFAULT '4001'
*"     VALUE(LGORT_009) LIKE  BDCDATA-FVAL DEFAULT 'p400'
*"     VALUE(XNUVO_010) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(XFULL_011) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(WVERS1_012) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(ZEILE_013) LIKE  BDCDATA-FVAL DEFAULT '105'
*"     VALUE(XSELK_01_014) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(ERFMG_01_015) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  data: z_zdate(10) type c.
  data: z_zdate1(132) type c.
*   Tables: EKBE, EKPO, EKES, VBFA, viqmfe, mkpf.

  data: X.
  data: zebeln like ekbe-ebeln,
        zebelp like ekbe-ebelp,
        zbelnr like ekbe-belnr,
        zbuzei like ekbe-buzei,
        zbudat like ekbe-budat,
        zvbeln like ekes-vbeln,
        zvbelp like ekes-vbelp,
        zeindt like ekes-eindt,
        zPOSNN like vbfa-POSNN,
        zfollowondoc like vbfa-vbeln.

  data: it_ekbe like ekbe occurs 0 with header line.

   data: it_rtv_ekbe like ekbe occurs 0 with header line.
  data: zlineno(4) type c.
  data: zERFMG(12) type c,
        zbktxt like gohead-bktxt,
        zfecod like viqmfe-fecod.

  data: zcount_RTV(4) type c,
  zcount_EKBE(4) type c,
zcount_TOTAL(4) type c.
*
  data: zmjahr like mkpf-mjahr.
  data: check, n(3).
  data: it_mbew like mbew occurs 0 with header line.
  data: zdate1(10) type c.

* End data declaration

  WRITE SY-DATUM TO Z_ZDATE.

  clear: n, zcount_total, zcount_RTV, zcount_EKBE.
  select single * from mbew into it_mbew where matnr = i_viqmel-matnr
                                   and  bwkey = i_viqmel-mawerk
                                   and  bklas = '3000'.
  if sy-subrc = 0.
    append it_mbew.
  endif.
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
* do not do return to Vendor for CKD parts
  if check = 'X' or n = 1.
    exit.
  endif.

  clear: zebeln, zebelp, zbudat, zvbeln, zvbelp, zeindt,
  zPOSNN, zfollowondoc, zfecod.
  select   ebeln ebelp  budat
    into (zebeln, zebelp, zbudat) from ekbe up to 1 rows
                                where bewtp = 'E'
                                and bwart = '101'
                             and matnr = i_viqmel-matnr
          order by  budat descending.
  endselect.

*  select  vbeln vbelp  Eindt  into
*    (zvbeln, zvbelp, zeindt) from ekes up to 1 rows
*                                where ebeln = zebeln
*                                and ebelp = zebelp
*                                and DABMG >= i_viqmel-rkmng
*      order by  eindt descending.
*  endselect.
*  select  POSNN vbeln  into
*    (zPOSNN, zfollowondoc) from vbfa up to 1 rows
*                                where vbelv = zvbeln
*                                and POSNV = zvbelp
*                                and VBTYP_N = 'R'
*                                and bwart = '101'
*      order by vbeln POSNN descending.
*  endselect.

*  select mjahr into zmjahr from mkpf up to 1 rows
*                                where mblnr = zfollowondoc
*                 order by mjahr descending.
*  endselect.

  select single fecod  into (zfecod) from viqmfe
  where qmnum = i_viqmel-qmnum.

  select   * from ekbe into it_ekbe
                                  where bewtp = 'E'
                                  and bwart = '101'
                               and matnr = i_viqmel-matnr
                               and ebeln = zebeln
                               and ebelp = zebelp.
    append it_ekbe.
  endselect.

  select   * from ekbe into it_rtv_ekbe
                                  where bewtp = 'E'
     and ( ( bwart =  '102' ) or ( bwart = '122' ) )
                               and matnr = i_viqmel-matnr
                               and ebeln = zebeln
                               and ebelp = zebelp.
    append it_rtv_ekbe.
  endselect.
* This is short cut to get exact number of lines for MB01
* MM is going to change logic later
  describe table  it_rtv_ekbe lines zcount_RTV.
  describe table  it_ekbe lines zcount_ekbe.
  zcount_total =  zcount_EKBE - zcount_RTV.
  if zcount_total < 1.
  zcount_total = 1.
  endif.
** end shortcut
*  loop at it_ekbe.
*
*    if it_ekbe-lfbnr = zfollowondoc.
*      zlineno = sy-tabix.
*    endif.
*  endloop.



  subrc = 0.

  perform bdc_nodata      using NODATA.

  perform open_group      using GROUP USER KEEP HOLDDATE CTU.

  perform bdc_dynpro      using 'SAPMM07M' '0200'.
  perform bdc_field       using 'BDC_CURSOR'
                                'MKPF-BKTXT'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.
  perform bdc_field       using 'MKPF-BLDAT'
*\                              BLDAT_001.
                               Z_ZDATE.


  perform bdc_field       using 'MKPF-BUDAT'
*\                              BUDAT_002.
                                Z_ZDATE.
  concatenate 'Q3' i_viqmel-qmnum into zbktxt separated by space.


  perform bdc_field       using 'MKPF-BKTXT'
*\                              BKTXT_003.
                                  zbktxt.

  perform bdc_field       using 'RM07M-BWARTWE'
                                BWARTWE_004.
  perform bdc_field       using 'RM07M-EBELN'
*\                              EBELN_005.
                                 ZEBELN.
  perform bdc_field       using 'RM07M-EBELP'
*\                              EBELP_006.
                                ZEBELP.

  perform bdc_field       using 'RM07M-WERKS'
*\                              WERKS_007.
                                 i_viqmel-mawerk.
  move i_viqmel-rkmng to zERFMG.
  perform bdc_field       using 'RM07M-GRUND'
*\                              GRUND_008.
                                zfecod.
* No need to pass storage locn as system defaults it
* from purchasing doc

*  perform bdc_field       using 'RM07M-LGORT'
*                                LGORT_009.
  perform bdc_field       using 'RM07M-XNUVO'
                                XNUVO_010.
  perform bdc_field       using 'XFULL'
                                XFULL_011.
  perform bdc_field       using 'RM07M-WVERS1'
                                WVERS1_012.
*\perform bdc_dynpro      using 'SAPMM07M' '0225'.


    perform bdc_dynpro      using 'SAPMM07M' '0221'.

    perform bdc_field       using 'BDC_CURSOR'
                                  'MSEG-ERFMG(01)'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=SELN'.
*\perform bdc_dynpro      using 'SAPMM07M' '0225'.
    perform bdc_dynpro      using 'SAPMM07M' '0221'.

    perform bdc_field       using 'BDC_CURSOR'
                                  'MSEG-ERFMG(01)'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=KLA'.
    perform bdc_dynpro      using 'SAPMM07M' '1501'.
    perform bdc_field       using 'BDC_CURSOR'
                                  'RM07M-ZEILE'.
    perform bdc_field       using 'BDC_OKCODE'
                                  '=OK'.
    perform bdc_field       using 'RM07M-ZEILE'
*\                              ZEILE_013.
                                  Zcount_total.
*\perform bdc_dynpro      using 'SAPMM07M' '0225'.

  perform bdc_dynpro      using 'SAPMM07M' '0221'.

  perform bdc_field       using 'BDC_CURSOR'
                                'MSEG-ERFMG(01)'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=BU'.
  perform bdc_field       using 'RM07M-XSELK(01)'
                                XSELK_01_014.
  perform bdc_field       using 'MSEG-ERFMG(01)'
*\                              ERFMG_01_015.
                                ZERFMG.

  perform bdc_transaction tables messtab
  using                         'MB01'
                                CTU
                                MODE
                                UPDATE.
  if sy-subrc <> 0.

    subrc = sy-subrc.
    exit.
  endif.

* add logic to remove all warning messages

  delete messtab where MSGTYP = 'W'.

* add logic to create Transfer Order
*  TABLES: VIQMEL, ZSRF_PICKER_LIST, MSEG.
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


  T_LIST1-MATNR =  Zmatnr.
  APPEND T_LIST1.

* Use Fixed values
*  CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT01_CHECK'
** EXPORTING
**   ZINTCALL       =
** IMPORTING
**   E_MESS         =
**   ZRESULT        =
*    TABLES
*      T_LIST         =  T_LIST1.
*
*  LOOP AT T_LIST1.
*    MOVE T_LIST1-NLPLA TO ZNLPLA.
*    MOVE T_LIST1-NLTYP TO ZNLTYP.
*  ENDLOOP.

  CALL FUNCTION 'ZQ3_TRANSFER_ORDER_CREATE'
   EXPORTING
     CTU             = 'X'
     MODE            = 'N'
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
     VLTYP_008       =  '999' "ZNLTYP
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


* End logic for Transfer Order
  perform close_group using     CTU.

ENDFUNCTION.
