FUNCTION ZQ3_RETURN_TO_VENDOR.
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
*"     VALUE(ACTION_001) LIKE  BDCDATA-FVAL DEFAULT 'A02'
*"     VALUE(REFDOC_002) LIKE  BDCDATA-FVAL DEFAULT 'R02'
*"     VALUE(MAT_DOC_003) LIKE  BDCDATA-FVAL DEFAULT '5100479362'
*"     VALUE(DOC_YEAR_004) LIKE  BDCDATA-FVAL DEFAULT '2007'
*"     VALUE(BUDAT_005) LIKE  BDCDATA-FVAL DEFAULT '2006/11/20'
*"     VALUE(BLDAT_006) LIKE  BDCDATA-FVAL DEFAULT '2006/04/12'
*"     VALUE(LFSNR_007) LIKE  BDCDATA-FVAL DEFAULT '0180474947'
*"     VALUE(BUDAT_008) LIKE  BDCDATA-FVAL DEFAULT '2006/11/20'
*"     VALUE(FRBNR_009) LIKE  BDCDATA-FVAL DEFAULT '79222807'
*"     VALUE(WEVER_010) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(DETAIL_ZEILE_011) LIKE  BDCDATA-FVAL DEFAULT '   1'
*"     VALUE(ERFME_012) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"     VALUE(ERFMG_013) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(BWART_014) LIKE  BDCDATA-FVAL DEFAULT '122'
*"     VALUE(LGOBE_015) LIKE  BDCDATA-FVAL DEFAULT 'W/M Total'
*"     VALUE(GRUND_016) LIKE  BDCDATA-FVAL DEFAULT '0010'
*"     VALUE(SGTXT_017) LIKE  BDCDATA-FVAL DEFAULT 'scrap'
*"     VALUE(DETAIL_TAKE_018) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"     VALUE(I_CUSTOMIZING) LIKE  V_TQ85 STRUCTURE  V_TQ85 OPTIONAL
*"     VALUE(I_MANUM) LIKE  QMSM-MANUM OPTIONAL
*"     VALUE(I_FBCALL) OPTIONAL
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
  Tables: EKBE, EKPO, EKES, VBFA, viqmfe, mkpf.

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

  data: zERFMG(12) type c,
        zbktxt like gohead-bktxt,
        zfecod like viqmfe-fecod.

*
  data: zmjahr like mkpf-mjahr.
  data: check, n(3).
  data: it_mbew like mbew occurs 0 with header line.
  data: zdate1(10) type c.
*  select single   ebeln ebelp belnr buzei max( budat )
*  into (zebeln, zebelp, zbelnr, zbuzei, zbudat) from ekbe
*                              where bewtp = 'E'
*                              and bwart = '101'
*                            and matnr = i_viqmel-matnr
*        group by ebeln ebelp belnr buzei budat.
*

***** add overseas check
clear: n.
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

***commented out pop up option
*  if check = 'X' or n = 1.
*
* CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
*   EXPORTING
*    DEFAULTOPTION       = 'Y'
*     TEXTLINE1           =  'Overseas part do you want to continue'
**    TEXTLINE2           = ' '
*     TITEL               = 'Choose'
**    START_COLUMN        = 25
**    START_ROW           = 6
*  IMPORTING
*    ANSWER              = X
*           .
*
*
*endif.
*
*if X = 'N'.
*exit.
*endif.
****endcheck

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

*  select single vbeln vbelp max( Eindt ) into
*  (zvbeln, zvbelp, zeindt) from ekes
*                              where ebeln = zebeln
*                              and ebelp = zebelp
*                              and DABMG >= i_viqmel-rkmng
*    group by vbeln vbelp eindt.

*DOC_YEAR_004 = sy-datum+0(4).

select  vbeln vbelp  Eindt  into
  (zvbeln, zvbelp, zeindt) from ekes up to 1 rows
                              where ebeln = zebeln
                              and ebelp = zebelp
                              and DABMG >= i_viqmel-rkmng
    order by  eindt descending.
endselect.

*  select single POSNN max( vbeln ) into
*  (zPOSNN, zfollowondoc) from vbfa
*                              where vbelv = zvbeln
*                              and POSNV = zvbelp
*                              and VBTYP_N = 'R'
*                              and bwart = '101'
*    group by vbeln POSNN .

select  POSNN vbeln  into
  (zPOSNN, zfollowondoc) from vbfa up to 1 rows
                              where vbelv = zvbeln
                              and POSNV = zvbelp
                              and VBTYP_N = 'R'
                              and bwart = '101'
    order by vbeln POSNN descending.
endselect.

select mjahr into zmjahr from mkpf up to 1 rows
                              where mblnr = zfollowondoc
               order by mjahr descending.
 endselect.

select single fecod  into (zfecod) from viqmfe
where qmnum = i_viqmel-qmnum.

*  subrc = 0.
*
*  perform bdc_nodata      using NODATA.
*
*  perform open_group      using GROUP USER KEEP HOLDDATE CTU.
*
*  perform bdc_dynpro      using 'SAPLMIGO' '0001'.
*  perform bdc_field       using 'BDC_OKCODE'
**                                '=OK_GO'.
*                                 '/00'.
*  perform bdc_field       using 'GODYNPRO-ACTION'
*                                ACTION_001.
*  perform bdc_field       using 'GODYNPRO-REFDOC'
*                                REFDOC_002.
*  perform bdc_field       using 'BDC_CURSOR'
*                                'GODYNPRO-MAT_DOC'.
*  perform bdc_field       using 'GODYNPRO-MAT_DOC'
**                              MAT_DOC_003.
*                                 zfollowondoc.
*  perform bdc_field       using 'GODYNPRO-DOC_YEAR'
**                                DOC_YEAR_004.
*                                 zmjahr.
*
*   perform bdc_dynpro      using 'SAPLMIGO' '0001'.
*  perform bdc_field       using 'BDC_OKCODE'
**                                '/00'.
*                                 '=OK_GO'.
*
*  perform bdc_field       using 'GOHEAD-BUDAT'
**                              BUDAT_005.
*                                 sy-datum.
** Add enter to fill all values*
*
**  perform bdc_field       using 'BDC_OKCODE'
**                                '=OK_GO'.
** end change
*
*  perform bdc_dynpro      using 'SAPLMIGO' '0001'.
*  perform bdc_field       using 'BDC_OKCODE'
*                                '=OK_POST'.
*  perform bdc_field       using 'GOHEAD-BLDAT'
**                              BLDAT_006.
*                                 sy-datum.
**perform bdc_field       using 'GOHEAD-LFSNR'
**                              LFSNR_007.
*  perform bdc_field       using 'GOHEAD-BUDAT'
**                              BUDAT_008.
*                                 sy-datum.
**perform bdc_field       using 'GOHEAD-FRBNR'
**                              FRBNR_009.
*concatenate 'Q3' i_viqmel-qmnum into zbktxt separated by space.
*  perform bdc_field       using 'GOHEAD-BKTXT'
*                                zbktxt.
*  perform bdc_field       using 'GOHEAD-WEVER'
*                                WEVER_010.
** add naterial number
** perform bdc_field       using 'GOITEM-MAKTX'
**                                i_viqmel-matnr.
** end addition
*
*  perform bdc_field       using 'GODYNPRO-DETAIL_ZEILE'
**                              DETAIL_ZEILE_011.
*                                 zPOSNN.
**perform bdc_field       using 'GOITEM-ERFME'
**                              ERFME_012.
*  move i_viqmel-rkmng to zERFMG.
*  perform bdc_field       using 'GOITEM-ERFMG'
**                              ERFMG_013.
*                                  zerfmg.
*  perform bdc_field       using 'GOITEM-BWART'
*                                BWART_014.
*  perform bdc_field       using 'GOITEM-LGOBE'
*                                LGOBE_015.
*  perform bdc_field       using 'GOITEM-GRUND'
**                              GRUND_016.
*                               zfecod.
*  perform bdc_field       using 'GOITEM-SGTXT'
*                                SGTXT_017.
*  perform bdc_field       using 'BDC_CURSOR'
*                                'GODYNPRO-DETAIL_TAKE'.
*  perform bdc_field       using 'GODYNPRO-DETAIL_TAKE'
*                                DETAIL_TAKE_018.
**  perform bdc_dynpro      using 'SAPLMIGO' '0001'.
**  perform bdc_field       using 'BDC_OKCODE'
**                                '=OK_POST'.
*  perform bdc_transaction tables messtab
*  using                         'MIGO'
*                                CTU
*                                MODE
*                                UPDATE.
*  if sy-subrc <> 0.
*    subrc = sy-subrc.
*    exit.
*  endif.

subrc = 0.

zdate1 = sy-datum.
perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MIGO_OK_GO'.
perform bdc_field       using 'GODYNPRO-ACTION'
                              ACTION_001.
perform bdc_field       using 'GODYNPRO-REFDOC'
                              REFDOC_002.
perform bdc_field       using 'BDC_CURSOR'
                              'GODYNPRO-MAT_DOC'.
perform bdc_field       using 'GODYNPRO-MAT_DOC'
*                              MAT_DOC_003.
                               zfollowondoc.
perform bdc_field       using 'GODYNPRO-DOC_YEAR'
*                              DOC_YEAR_004.
                               zmjahr.
perform bdc_field       using 'GOHEAD-BUDAT'
*                              BUDAT_005.
*                               sy-datum.
                                zdate1.
perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OK_POST'.
perform bdc_field       using 'GOHEAD-BLDAT'
*                              BLDAT_006.
*                               sy-datum.
                                zdate1.
perform bdc_field       using 'GOHEAD-LFSNR'
                              LFSNR_007.
perform bdc_field       using 'GOHEAD-BUDAT'
*                              BUDAT_008.
*                               sy-datum.
                               zdate1.
perform bdc_field       using 'GOHEAD-FRBNR'
                              FRBNR_009.
concatenate 'Q3' i_viqmel-qmnum into zbktxt separated by space.
  perform bdc_field       using 'GOHEAD-BKTXT'
                                zbktxt.
perform bdc_field       using 'GOHEAD-WEVER'
                              WEVER_010.
perform bdc_field       using 'GODYNPRO-DETAIL_ZEILE'
                              DETAIL_ZEILE_011.
perform bdc_field       using 'GOITEM-ERFME'
                              ERFME_012.
move i_viqmel-rkmng to zERFMG.
perform bdc_field       using 'GOITEM-ERFMG'
*                              ERFMG_013.
                               zerfmg.
*perform bdc_field       using 'GOITEM-MIGO_ELIKZ'
*                              MIGO_ELIKZ_014.
perform bdc_field       using 'BDC_CURSOR'
                              'GOITEM-GRTXT'.
perform bdc_field       using 'GOITEM-BWART'
                              BWART_014.
perform bdc_field       using 'GOITEM-LGOBE'
                              LGOBE_015.
perform bdc_field       using 'GOITEM-GRUND'
*                              GRUND_017.
                               zfecod.
perform bdc_field       using 'GOITEM-SGTXT'
                              SGTXT_017.
perform bdc_field       using 'GODYNPRO-DETAIL_TAKE'
                              DETAIL_TAKE_018.
perform bdc_transaction tables messtab
using                         'MIGO'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.







****end new recording
* add logic to create Transfer Order
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

*LOOP AT T_LIST1.
  T_LIST1-MATNR =  Zmatnr.
  APPEND T_LIST1.
*ENDLOOP.

  CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT01_CHECK'
* EXPORTING
*   ZINTCALL       =
* IMPORTING
*   E_MESS         =
*   ZRESULT        =
    TABLES
      T_LIST         =  T_LIST1.

  LOOP AT T_LIST1.
    MOVE T_LIST1-NLPLA TO ZNLPLA.
    MOVE T_LIST1-NLTYP TO ZNLTYP.
  ENDLOOP.

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
     VLTYP_008       = ZNLTYP
*  "'435'
     VLPLA_009       = ZNLPLA
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
