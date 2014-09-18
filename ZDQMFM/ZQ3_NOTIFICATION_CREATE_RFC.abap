FUNCTION zq3_notification_create_rfc.
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
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(QMART_001) LIKE  BDCDATA-FVAL DEFAULT 'q3'
*"     VALUE(QMGRP_002) LIKE  BDCDATA-FVAL
*"     VALUE(QMCOD_003) LIKE  BDCDATA-FVAL
*"     VALUE(MATNR_004) LIKE  BDCDATA-FVAL
*"     VALUE(MAWERK_005) LIKE  BDCDATA-FVAL
*"     VALUE(RKMNG_006) LIKE  BDCDATA-FVAL
*"     VALUE(OTGRP_007) LIKE  BDCDATA-FVAL
*"     VALUE(OTEIL_008) LIKE  BDCDATA-FVAL
*"     VALUE(FEGRP_009) LIKE  BDCDATA-FVAL
*"     VALUE(FECOD_010) LIKE  BDCDATA-FVAL
*"     VALUE(URCOD_011) LIKE  BDCDATA-FVAL
*"     VALUE(URGRP_012) LIKE  BDCDATA-FVAL
*"     VALUE(QMGRP_013) LIKE  BDCDATA-FVAL
*"     VALUE(QMCOD_014) LIKE  BDCDATA-FVAL
*"     VALUE(MATNR_015) LIKE  BDCDATA-FVAL
*"     VALUE(MAWERK_016) LIKE  BDCDATA-FVAL
*"     VALUE(RKMNG_017) LIKE  BDCDATA-FVAL
*"     VALUE(BZMNG_018) LIKE  BDCDATA-FVAL
*"     VALUE(OTGRP_019) LIKE  BDCDATA-FVAL
*"     VALUE(OTEIL_020) LIKE  BDCDATA-FVAL
*"     VALUE(FEGRP_021) LIKE  BDCDATA-FVAL
*"     VALUE(FECOD_022) LIKE  BDCDATA-FVAL
*"     VALUE(URCOD_023) LIKE  BDCDATA-FVAL
*"     VALUE(URGRP_024) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
* Date           Developer      Request         Description
* 12/10/07       Rakesh         UD1K942366      Add Vendor Code and User
*                Gandhi                         ID to Q3 Scrap Notif.
*"----------------------------------------------------------------------
  TABLES: mara,lqua, mbew, mlgt.
  DATA: BEGIN OF wa_txtline,
           tdline LIKE tline-tdline,
        END OF wa_txtline.
  DATA it_txtline LIKE TABLE OF wa_txtline.
  DATA: zmatnr LIKE marc-matnr,
        zmmsta like marc-mmsta.
  DATA: zz_matnr LIKE marc-matnr.
  DATA: zverme LIKE lqua-verme.

  DATA: zbklas LIKE mbew-bklas.
*DATA: WA_QMNUM LIKE MESSTAB-MSGV1.
*
*DATA: BEGIN OF IT_QMNUM OCCURS 0,
*      QMNUM LIKE  MESSTAB-MSGV1,
*      END OF IT_QMNUM.
  DATA: t_list1 LIKE zsrf_picker_list OCCURS 0 WITH HEADER LINE.

  DATA: znlpla LIKE zsrf_picker_list-nlpla,
         znltyp LIKE zsrf_picker_list-nltyp,
         zmblnr LIKE mseg-mblnr.
  DATA: ztbnum LIKE mseg-tbnum,
        zlgnum LIKE mseg-lgnum,
        zmeins LIKE mara-meins,
        zrkmng LIKE viqmel-rkmng,
        zwerks LIKE marc-werks.

  DATA : options	LIKE	itcpo.

  DATA: w_lifnr      LIKE eord-lifnr     , " +UD1K942366
        w_parvw_lief LIKE tq80-parvw_lief, " +UD1K942366
        w_parvw_her  LIKE tq80-parvw_her . " +UD1K942366
  CLEAR: zverme.
*********End Data Declaration**
  fecod_022 =   fecod_010.
  qmgrp_013 =   qmgrp_002.
  qmcod_014 =   qmcod_003.
  otgrp_019 =   otgrp_007.
  oteil_020 =   oteil_008.
  fegrp_021 =   fegrp_009.
  urcod_023 =   urcod_011.
  urgrp_024 =   urgrp_012.


  SELECT SINGLE matnr mmsta INTO (zmatnr, zmmsta)
  FROM marc
 WHERE matnr = matnr_004
 AND werks = mawerk_005.

  IF sy-subrc <> 0.
    MOVE 'E' TO messtab-msgtyp.
    MOVE 'Invalid Material in Plant' TO messtab-msgv1.
    APPEND messtab.
    EXIT.
  ENDIF.

** Change by Furong on 06/16/08  "UD1K943877
  if not ( zmmsta = '11' or zmmsta = '12' or zmmsta = '13' ).
    MOVE 'E' TO messtab-msgtyp.
    MOVE 'Invalid Material Number. Please recheck or contact PC' TO
         messtab-msgv1.
    APPEND messtab.
    EXIT.
  endif.

** End of change

*-Start of changes +UD1K942366
  SELECT lifnr UP TO 1 ROWS
                  INTO w_lifnr
                  FROM eord
                  WHERE matnr = matnr_004 AND
                        werks = mawerk_005 AND
                        ebeln BETWEEN 4600000000 AND 4659999999.
  ENDSELECT.
  IF ( NOT w_lifnr IS INITIAL ) OR ( NOT user IS INITIAL ).
    SELECT SINGLE parvw_lief parvw_her
                  INTO (w_parvw_lief,w_parvw_her)
                  FROM tq80
                  WHERE qmart = 'Q3'.
  ENDIF.
*-End of changes +UD1K942366
  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMART'
                                qmart_001.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=COWO'.        " -UD1K942366
  IF NOT w_lifnr IS INITIAL.                     " +UD1K942366
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=10\TAB02'.   " +UD1K942366
  ENDIF.                                         " +UD1K942366
  PERFORM bdc_field       USING 'VIQMEL-QMGRP'
                                qmgrp_002.
  PERFORM bdc_field       USING 'VIQMEL-QMCOD'
                                qmcod_003.
  PERFORM bdc_field       USING 'RQM00-MATNR'
                                matnr_004.
  PERFORM bdc_field       USING 'RQM00-MAWERK'
                                mawerk_005.
  PERFORM bdc_field       USING 'VIQMEL-RKMNG'
                                rkmng_006.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMUR-URCOD'.
  PERFORM bdc_field       USING 'VIQMFE-OTGRP'
                                otgrp_007.
  PERFORM bdc_field       USING 'VIQMFE-OTEIL'
                                oteil_008.
  PERFORM bdc_field       USING 'VIQMFE-FEGRP'
                                fegrp_009.
  PERFORM bdc_field       USING 'VIQMFE-FECOD'
                                fecod_010.
  PERFORM bdc_field       USING 'VIQMUR-URCOD'
                                urcod_011.
  PERFORM bdc_field       USING 'VIQMUR-URGRP'
                                urgrp_012.
*-Start of changes +UD1K942366
  IF NOT w_lifnr IS INITIAL.
    PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IHPA-PARNR(02)'.
    PERFORM bdc_field       USING 'IHPA-PARVW(02)'
                                  w_parvw_lief.
    PERFORM bdc_field       USING 'IHPA-PARNR(02)'
                                  w_lifnr.
  ENDIF.
  IF NOT user IS INITIAL.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=PART'.
    PERFORM bdc_dynpro      USING 'SAPLIPAR' '0200'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BACK'.
    PERFORM bdc_field       USING 'IHPA-PARVW(03)'
                                  w_parvw_her.
    PERFORM bdc_field       USING 'IHPA-PARNR(03)'
                                    user.
    PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
    IF NOT w_lifnr IS INITIAL.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=10\TAB01'.
      PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
    ENDIF.
  ENDIF.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=COWO'.

*-End of changes +UD1K942366

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'VIQMEL-QMGRP'
*                              QMGRP_013.
                                   qmgrp_002.
  PERFORM bdc_field       USING 'VIQMEL-QMCOD'
*                              QMCOD_014.
                                 qmcod_003.
  PERFORM bdc_field       USING 'RQM00-MATNR'
*                              MATNR_015.
                                  matnr_004.
  PERFORM bdc_field       USING 'RQM00-MAWERK'
*                              MAWERK_016.
                                 mawerk_005.
  PERFORM bdc_field       USING 'VIQMEL-RKMNG'
*                              RKMNG_017.
                                 rkmng_006.
  bzmng_018        =  rkmng_006. "added 100565
  PERFORM bdc_field       USING 'VIQMEL-BZMNG'
                                bzmng_018.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMUR-URCOD'.
  PERFORM bdc_field       USING 'VIQMFE-OTGRP'
*                              OTGRP_019.
                                 otgrp_007.
  PERFORM bdc_field       USING 'VIQMFE-OTEIL'
*                              OTEIL_020.
                                 oteil_008.
  PERFORM bdc_field       USING 'VIQMFE-FEGRP'
*                              FEGRP_021.
                                 fegrp_009.
  PERFORM bdc_field       USING 'VIQMFE-FECOD'
*                              FECOD_022.
                                 fecod_010.
  PERFORM bdc_field       USING 'VIQMUR-URCOD'
*                              URCOD_023.
                                  urcod_011.
  PERFORM bdc_field       USING 'VIQMUR-URGRP'
*                              URGRP_024.
                                  urgrp_012.
  PERFORM bdc_transaction TABLES messtab
  USING                         'QM01'
                                ctu
                                mode
                                update.


  IF sy-subrc <> 0.
    subrc = sy-subrc.
    LOOP AT messtab.
      CONCATENATE 'Invalid Data:' messtab-msgv1 messtab-msgv2
      INTO messtab-msgv1 SEPARATED BY space.
      MODIFY messtab.
    ENDLOOP.
    EXIT.
  ENDIF.
**********Pass variables


  LOOP AT messtab.
    wa_qmnum = messtab-msgv1.
    MOVE messtab-msgv1 TO it_qmnum-qmnum.
    MOVE matnr_004 TO it_qmnum-matnr.
    APPEND it_qmnum.
  ENDLOOP.


  matnr = matnr_004.
*call transaction 'ZSCRAP' using IT_QMNUM AND SKIP FIRST SCREEN.
*SUBMIT ZQMSCRAP_LABEL WITH qmnum IN IT_QMNUM
*
*         AND RETURN.

  SET PARAMETER ID 'IQM' FIELD it_qmnum-qmnum.
  SET PARAMETER ID 'MAT' FIELD it_qmnum-matnr.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
  EXPORT it_qmnum TO MEMORY ID 'M1'.
  SUBMIT zqmscrap_label AND RETURN.
*****end code to pass variables

** Add code to check for Valuation Class
  SELECT SINGLE bklas INTO zbklas FROM mbew WHERE
     matnr = matnr_004 AND
     bwkey = mawerk_005.

  IF zbklas = '3000' OR zbklas = '3001'.

    SELECT SINGLE matnr INTO zz_matnr FROM mlgt WHERE
     matnr = matnr_004.
    IF sy-subrc <> 0.
*        LOOP AT MESSTAB.
      MOVE 'E' TO messtab-msgtyp.
      MOVE 'Material not set up for WM' TO messtab-msgv1 .
      APPEND messtab.
*      endloop.
      EXIT.
    ENDIF.
****Add code for Transfer Material to 999
    SELECT SINGLE meins INTO zmeins FROM mara WHERE
       matnr = matnr_004.
    t_list1-matnr =  matnr_004.
    APPEND t_list1.


    CALL FUNCTION 'Z_FRF_MM_LIST_BDC_LT01_CHECK'
* EXPORTING
*   ZINTCALL       =
* IMPORTING
*   E_MESS         =
*   ZRESULT        =
        TABLES
          t_list         =  t_list1.

    LOOP AT t_list1.
      MOVE t_list1-nlpla TO znlpla.
      MOVE t_list1-nltyp TO znltyp.
    ENDLOOP.

    zrkmng   = rkmng_006.
    zwerks   = mawerk_005.
* To check if there is stock in WM Bin
* Change code later to search for warehouse number
    SELECT SINGLE verme INTO zverme
    FROM lqua
   WHERE lgnum = 'P01'
   AND matnr = matnr_004
   AND lgtyp = znltyp
   AND lgpla = znlpla.

    IF zverme < rkmng_006.
      MOVE 'INV_COUNT' TO znlpla.
      MOVE '999' TO znltyp.
    ENDIF.



    CALL FUNCTION 'L_TO_CREATE_SINGLE'
      EXPORTING
        i_lgnum                     = 'P01'
        i_bwlvs                     = '951'
*   I_BETYP                     = ' '
*   I_BENUM                     = ' '
        i_matnr                     = matnr
        i_werks                     = zwerks
*   I_LGORT                     = ' '
*   I_CHARG                     = ' '
*   I_BESTQ                     = ' '
*   I_SOBKZ                     = ' '
*   I_SONUM                     = ' '
*   I_LETYP                     = ' '
        i_anfme                     = zrkmng
        i_altme                     = zmeins
*   I_WDATU                     = INIT_DATUM
*   I_VFDAT                     = INIT_DATUM
*   I_ZEUGN                     = ' '
*   I_LZNUM                     = ' '
        i_squit                     = 'X'
*   I_NIDRU                     = ' '
*   I_DRUKZ                     = ' '
*   I_LDEST                     = ' '
*   I_WEMPF                     = ' '
*   I_ABLAD                     = ' '
        i_vltyp                     = znltyp
*   I_VLBER                     = ' '
        i_vlpla                     = znlpla
*   I_VPPOS                     = ' '
*   I_VLENR                     = ' '
*   I_VLQNR                     = ' '
        i_nltyp                     = '999'
*   I_NLBER                     = ' '
        i_nlpla                     = 'SCRAP_RF'
*   I_NPPOS                     = ' '
*   I_NLENR                     = ' '
*   I_NLQNR                     = ' '
*   I_RLTYP                     = ' '
*   I_RLBER                     = ' '
*   I_RLPLA                     = ' '
*   I_RLQNR                     = ' '
*   I_UPDATE_TASK               = ' '
*   I_COMMIT_WORK               = 'X'
*   I_BNAME                     = SY-UNAME
*   I_KOMPL                     = 'X'
*   I_SOLEX                     = 0
*   I_PERNR                     = 0
*   I_AUSFB                     = ' '
* IMPORTING
*   E_TANUM                     =
*   E_LTAP                      =
* TABLES
*   T_LTAK                      =
*   T_LTAP_VB                   =
* EXCEPTIONS
*   NO_TO_CREATED               = 1
*   BWLVS_WRONG                 = 2
*   BETYP_WRONG                 = 3
*   BENUM_MISSING               = 4
*   BETYP_MISSING               = 5
*   FOREIGN_LOCK                = 6
*   VLTYP_WRONG                 = 7
*   VLPLA_WRONG                 = 8
*   VLTYP_MISSING               = 9
*   NLTYP_WRONG                 = 10
*   NLPLA_WRONG                 = 11
*   NLTYP_MISSING               = 12
*   RLTYP_WRONG                 = 13
*   RLPLA_WRONG                 = 14
*   RLTYP_MISSING               = 15
*   SQUIT_FORBIDDEN             = 16
*   MANUAL_TO_FORBIDDEN         = 17
*   LETYP_WRONG                 = 18
*   VLPLA_MISSING               = 19
*   NLPLA_MISSING               = 20
*   SOBKZ_WRONG                 = 21
*   SOBKZ_MISSING               = 22
*   SONUM_MISSING               = 23
*   BESTQ_WRONG                 = 24
*   LGBER_WRONG                 = 25
*   XFELD_WRONG                 = 26
*   DATE_WRONG                  = 27
*   DRUKZ_WRONG                 = 28
*   LDEST_WRONG                 = 29
*   UPDATE_WITHOUT_COMMIT       = 30
*   NO_AUTHORITY                = 31
*   MATERIAL_NOT_FOUND          = 32
*   LENUM_WRONG                 = 33
*   OTHERS                      = 34
              .
    IF sy-subrc <> 0.
*      LOOP AT MESSTAB.
      MOVE 'E' TO messtab-msgtyp.
      MOVE 'WM TO not created' TO messtab-msgv1 .
      APPEND messtab.
*      endloop.

    ENDIF.

  ELSE.
* Material is not warehouse managed

  ENDIF.

  PERFORM close_group USING     ctu.

ENDFUNCTION.
