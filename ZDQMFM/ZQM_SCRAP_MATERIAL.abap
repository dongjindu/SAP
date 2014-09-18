FUNCTION ZQM_SCRAP_MATERIAL.
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
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(BWARTWA_003) LIKE  BDCDATA-FVAL DEFAULT '551'
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(LGORT_005) LIKE  BDCDATA-FVAL DEFAULT 'P400'
*"     VALUE(XFULL_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(WVERS2_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_01_008) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(ERFMG_01_009) LIKE  MSEG-ERFMG OPTIONAL
*"     VALUE(AUFNR_010) LIKE  BDCDATA-FVAL DEFAULT 'CP001'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL OPTIONAL
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
  tables: Mard, viqmfe, qmfe, viqmur, qmel.
  data: zlgort like mard-lgort,
        zlabst like mard-labst.
  data: zaufnr like cobl-aufnr.
  data: zERFMG(12) type c.
  data: zfecod(4) type c.
  data: zzfecod(4) type n.
  data: zqmnum(12) type c.
  data: zdate like BDCDATA-FVAL .
*{   REPLACE        UP1K900139                                       11
*\ data: check, n(3).
  data: check, n(3).
*}   REPLACE
*{   REPLACE        UP1K900137                                        1
*\ data: z_zdate type d.
  data: z_zdate(10) type c.
  data: z_zdate1(132) type c.
*}   REPLACE
  data: it_mbew like mbew occurs 0 with header line.
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
  data: zcause_code like viqmur-urcod.
  data: zmaterial like qmel-matnr,
        zquantity like qmel-rkmng.
  data:zshort_text(132) type c.
  data:zNotification(132) type c.
  data: ztext(40) type c.
  write sy-datum  to z_zdate .

****Check to see if Cause code is not '04'
***'04' is Return to Vendor.
  clear: messtab[].
  if i_viqmel-qmnum+0(2) <> '00'.
    concatenate '00' i_viqmel-qmnum into i_viqmel-qmnum.
    move i_viqmel-qmnum to znotification.
  else.
    move i_viqmel-qmnum to znotification.
  endif.

* check to see if part is already scrapped.

  clear: ztext.
* Check if short text field is blank
  select single qmtxt into (ztext)
  from qmel
 where qmnum = i_viqmel-qmnum.

  if ztext <> space.
    move 'E' to messtab-msgtyp.
    move 'Part already scrapped' to messtab-msgv1.
    append messtab.
    exit.
  endif.


* check to see if the Material and Qty in Notif match the label
  clear: Zmaterial, zquantity.
  select single matnr rkmng into (Zmaterial, zquantity)
  from viqmel
 where qmnum = i_viqmel-qmnum.

  if i_viqmel-matnr <> zmaterial.
    move 'E' to messtab-msgtyp.
    move 'Material does not match Notification' to messtab-msgv1.
    append messtab.
    exit.
  elseif i_viqmel-rkmng <> zquantity.
    move 'E' to messtab-msgtyp.
    move 'Quantity does not match Notification' to messtab-msgv1.
    append messtab.
    exit.
  endif.



  select single urcod into (Zcause_code)
  from viqmur
 where qmnum = i_viqmel-qmnum
 and urgrp = 'CAUS'.

* Cause Code '04' is return to vendor
* this condition is if HMMA Scrap
  if zcause_code <> '04'.

* If there are multiple storage locations for the same part then
* add logic for selecting storage loc
* Check with Quentin Henry

 select single lgort into zlgort from mard where matnr = i_viqmel-matnr
                                       and werks = i_viqmel-mawerk
                                       and lgort <> '9999'
                                       and labst >= i_viqmel-rkmng.

* If JIS Part then all storage locn are -ve or 0.

    if sy-subrc <> 0.
    refresh it_mbew.
    select single * from mbew into it_mbew where matnr = i_viqmel-matnr
                                     and  bwkey = i_viqmel-mawerk.
*                                     and  bklas = '3005'.
 append it_mbew.

 loop at it_mbew.
 if it_mbew-bklas = '3005'.
select single lgpro into zlgort from marc where matnr = i_viqmel-matnr
                                       and werks = i_viqmel-mawerk.

endif.
endloop.
endif.

* End condition to check storage locn

    zqmnum = i_viqmel-qmnum.


    select single fecod  into (zfecod) from viqmfe
    where qmnum = i_viqmel-qmnum.

    zzfecod = zfecod.

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
*                               sy-datum.
                                   z_zdate.
    perform bdc_field       using 'MKPF-BUDAT'
*                              BUDAT_002.
*                               sy-datum.
                                    z_zdate.
    perform bdc_field       using 'RM07M-BWARTWA'
*                              BWARTWA_003.
                                    '551'.
*perform bdc_field       using 'DKACB-FMORE'
*                              ' '.
    perform bdc_field       using 'RM07M-WERKS'
*                              WERKS_004.
                                  i_viqmel-mawerk.
    perform bdc_field       using 'RM07M-LGORT'
*                              LGORT_005.
                                  zlgort.



    if zzfecod is initial.
      perform bdc_field       using 'RM07M-GRUND'
                                    '1001'.
    else.

      perform bdc_field       using 'RM07M-GRUND'
*                              '1001'.
                                     zzfecod.
    endif.

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
*                                '/00'.
    move i_viqmel-rkmng to zERFMG.
    perform bdc_field       using 'MSEG-MATNR(01)'
*                              MATNR_01_008.
                                   i_viqmel-matnr.
*                               '312103K600'.
    perform bdc_field       using 'MSEG-ERFMG(01)'
                                     zerfmg.
*                               i_viqmel-rkmng.
*                              ERFMG_01_009.
    if sy-sysid = 'UD1'.
      zaufnr = '5000055881'.
    else.
      If i_viqmel-mawerk = 'P001'.

        zaufnr = 'CP001'.

      elseif i_viqmel-mawerk = 'E001'.

        zaufnr = 'CE001'.

** Added for E002 on 12/21/11
      elseif i_viqmel-mawerk = 'E002'.

        zaufnr = 'CE002'.
** End

      endif.
    endif.

    perform bdc_field       using 'COBL-AUFNR'
*                              AUFNR_010.
                                  zaufnr.
** add screen for reason code**
*perform bdc_dynpro      using 'SAPMM07M' '0410'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'MSEG-GRUND'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=BU'.
*
*perform bdc_field       using 'MSEG-GRUND'
*                              '1001'.
*
*****end add screen for reason code

    perform bdc_transaction tables messtab
    using                         'MB1A'
                                  CTU
                                  MODE
                                  UPDATE.
    if sy-subrc <> 0.
      subrc = sy-subrc.
      exit.
    endif.



    TABLES: VIQMEL, ZSRF_PICKER_LIST, MSEG.
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

*** commented fn mod because now using default bin
*CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT01_CHECK'
** EXPORTING
**   ZINTCALL       =
** IMPORTING
**   E_MESS         =
**   ZRESULT        =
*  TABLES
*    T_LIST         =  T_LIST1.
*
*    LOOP AT T_LIST1.
*    MOVE T_LIST1-NLPLA TO ZNLPLA.
*    MOVE T_LIST1-NLTYP TO ZNLTYP.
*    ENDLOOP.
*
* Wait for PC to decide if pull from 999 INV_COUNT or error message

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
       VLTYP_008       = '999' "ZNLTYP
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

*perform close_group using     CTU.

  elseif zcause_code = '04'.
***Add logic to check what kind of part is it
***** add overseas check
    clear: n.
    refresh it_mbew.
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

*** End logic to check what kind of part is it
* do not do return to Vendor for CKD parts
    if check = 'X' or n = 1.
      z_zdate1 = z_zdate.
*zdate = sy-datum.
      CALL FUNCTION 'ZQM_SCRAP_MAT_OVERSEAS_201'
        EXPORTING
         CTU                 = 'X'
         MODE                = 'N'
         UPDATE              = 'L'
*   GROUP               =
*   USER                =
*   KEEP                =
*   HOLDDATE            =
*   NODATA              = '0'
*{   REPLACE        UP1K900137                                        4
*\   BLDAT_001           = z_zdate
         BLDAT_001           = z_zdate1
*}   REPLACE
*{   REPLACE        UP1K900137                                        5
*\   BUDAT_002           = z_zdate
         BUDAT_002           = z_zdate1
*}   REPLACE
         BWARTWA_003         = '201'
         WERKS_004           = 'P001'
         LGORT_005           = 'P400'
         XFULL_006           = ''
         WVERS2_007          = 'X'
*   MATNR_01_008        =
*   ERFMG_01_009        =
         AUFNR_010           = 'CP001'
          I_VIQMEL            = i_viqmel
*   I_CUSTOMIZING       =
*   I_MANUM             =
*   I_FBCALL            =
* IMPORTING
*   SUBRC               =
*   E_QNQMASM0          =
*   E_QNQMAQMEL0        =
*   E_BUCH              =
       TABLES
         MESSTAB             = messtab
*   TI_IVIQMFE          =
*   TI_IVIQMUR          =
*   TI_IVIQMSM          =
*   TI_IVIQMMA          =
*   TI_IHPA             =
*   TE_CONTAINER        =
*{   REPLACE        UP1K900137                                        7
*\*   TE_LINES            =
*  TE_LINES            = tline
*}   REPLACE
                .




    else.
*{   INSERT         UP1K900137                                       10
*CALL FUNCTION 'ZQ3_TEST'
* EXPORTING
*   CTU                = 'X'
*   MODE               = 'N'
*   UPDATE             = 'L'
**   GROUP              =
**   USER               =
**   KEEP               =
**   HOLDDATE           =
**   NODATA             = '/'
*   BLDAT_001          = z_zdate1
*   BUDAT_002          = z_zdate1
*   BKTXT_003          = 'Q3 notif'
*   BWARTWE_004        = '122'
*   EBELN_005          = '4600000160'
*   EBELP_006          =  '1'
*   WERKS_007          = 'P001'
*   GRUND_008          = '4001'
*   LGORT_009          = 'P400'
*   XFULL_010          = ''
*   WVERS1_011         = 'X'
*   XSELK_08_012       = 'X'
*   ERFMG_08_013       = '1'
*   I_VIQMEL               = i_viqmel
* IMPORTING
*   SUBRC              = subrc
* TABLES
*   MESSTAB            = messtab
*          .

      CALL FUNCTION 'ZQ3_MB01_RETURN_TO_VENDOR'
        EXPORTING
         CTU                = 'X'
         MODE               = 'N'
         UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
*   NODATA             =
         BLDAT_001          = '02/09/2007'
         BUDAT_002          = '02/09/2007'
         BKTXT_003          = 'Test Document'
         BWARTWE_004        = '122'
         EBELN_005          = '4600000160'
         EBELP_006          = '1'
         WERKS_007          = 'p001'
         GRUND_008          = '4001'
         LGORT_009          = 'p400'
         XNUVO_010          = 'X'
         XFULL_011          = 'X'
         WVERS1_012         = 'X'
         ZEILE_013          = '105'
         XSELK_01_014       = 'X'
         ERFMG_01_015       = '1'
          I_VIQMEL           = i_viqmel
       IMPORTING
         SUBRC              = subrc
       TABLES
         MESSTAB            = messtab
                .



*}   INSERT

*{   REPLACE        UP1K900137                                        9
*\CALL FUNCTION 'ZQ3_RETURN_TO_VENDOR'
*\  EXPORTING
*\   CTU                    = 'X'
*\   MODE                   = 'N'
*\   UPDATE                 = 'L'
*\*   GROUP                  =
*\*   USER                   =
*\*   KEEP                   =
*\*   HOLDDATE               =
*\*   NODATA                 =
*\   ACTION_001             = 'A02'
*\   REFDOC_002             = 'R02'
*\   MAT_DOC_003            = '5100479362'
*\   DOC_YEAR_004           = '2007'
*\   BUDAT_005              = '2006/11/20'
*\   BLDAT_006              = '2006/04/12'
*\   LFSNR_007              = '0180474947'
*\   BUDAT_008              = '2006/11/20'
*\   FRBNR_009              = '79222807'
*\   WEVER_010              = '1'
*\   DETAIL_ZEILE_011       = '   1'
*\   ERFME_012              = 'EA'
*\   ERFMG_013              = '1'
*\   BWART_014              = '122'
*\   LGOBE_015              = 'W/M Total'
*\   GRUND_016              = '0010'
*\   SGTXT_017              = 'scrap'
*\   DETAIL_TAKE_018        = 'X'
*\    I_VIQMEL               = i_viqmel
*\*    I_CUSTOMIZING          =
*\*    I_MANUM                =
*\*   I_FBCALL               =
*\* IMPORTING
*\*   SUBRC                  =
*\*   E_QNQMASM0             =
*\*   E_QNQMAQMEL0           =
*\*   E_BUCH                 =
*\  TABLES
*\   MESSTAB                = messtab
*\*    TI_IVIQMFE             =
*\*    TI_IVIQMUR             =
*\*    TI_IVIQMSM             =
*\*    TI_IVIQMMA             =
*\*    TI_IHPA                =
*CALL FUNCTION 'ZQ3_RETURN_TO_VENDOR'
*  EXPORTING
*   CTU                    = 'X'
*   MODE                   = 'N'
*   UPDATE                 = 'L'
**   GROUP                  =
**   USER                   =
**   KEEP                   =
**   HOLDDATE               =
**   NODATA                 =
*   ACTION_001             = 'A02'
*   REFDOC_002             = 'R02'
*   MAT_DOC_003            = '5100479362'
*   DOC_YEAR_004           = '2007'
*   BUDAT_005              = '2006/11/20'
*   BLDAT_006              = '2006/04/12'
*   LFSNR_007              = '0180474947'
*   BUDAT_008              = '2006/11/20'
*   FRBNR_009              = '79222807'
*   WEVER_010              = '1'
*   DETAIL_ZEILE_011       = '   1'
*   ERFME_012              = 'EA'
*   ERFMG_013              = '1'
*   BWART_014              = '122'
*   LGOBE_015              = 'W/M Total'
*   GRUND_016              = '0010'
*   SGTXT_017              = 'scrap'
*   DETAIL_TAKE_018        = 'X'
*    I_VIQMEL               = i_viqmel
*    I_CUSTOMIZING          =
*    I_MANUM                =
*   I_FBCALL               =
* IMPORTING
*   SUBRC                  =
*   E_QNQMASM0             =
*   E_QNQMAQMEL0           =
*   E_BUCH                 =
*  TABLES
*   MESSTAB                = messtab
*    TI_IVIQMFE             =
*    TI_IVIQMUR             =
*    TI_IVIQMSM             =
*    TI_IVIQMMA             =
*    TI_IHPA                =
*}   REPLACE
*   TE_CONTAINER           =
*{   REPLACE        UP1K900137                                        8
*\*   TE_LINES               =
*   TE_LINES               = te_lines
*}   REPLACE
      .

    endif.

  endif.

  loop at messtab.
    move messtab-msgv1 to zshort_text.
    move messtab-msgv1 to i_viqmel-qmtxt.
  endloop.


  CALL FUNCTION 'ZQ3_CHANGE_NOTIFICATION'
   EXPORTING
     CTU                = 'X'
     MODE               = 'N'
     UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
*   NODATA             = '/'
     QMNUM_001          = znotification
*   QMGRP_002          = 'MXTX53'
*   QMCOD_003          = 'TX53'
     HEADKTXT_004       = zshort_text
*   MATNR_005          = '312103K600'
*   MAWERK_006         = 'P001'
*   RKMNG_007          = '13'
*   BZMNG_008          = '13'
*   OTGRP_009          = 'MXBXB1'
*   OTEIL_010          = 'BXB1'
*   FEGRP_011          = '4'
*   FECOD_012          = '4002'
*   URCOD_013          = '01'
*   URGRP_014          = 'CAUS'
  I_VIQMEL           = i_viqmel

   IMPORTING
     SUBRC              = subrc
   TABLES
     MESSTAB            = messtab
            .






  perform close_group using     CTU.
ENDFUNCTION.
INCLUDE BDCRECXY .
