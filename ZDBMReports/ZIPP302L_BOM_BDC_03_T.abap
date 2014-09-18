*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_BDC_03_T                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: cukb,
        cuob,
        marc,
        mara,
        mast,
        stpo,
        stas,
        ztbm_abxebmdt.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
* EXCEL UPLOAD
DATA: BEGIN OF it_excl OCCURS 0,
        matnr TYPE mara-matnr,
        werks TYPE t001w-werks,
        stlan TYPE rc29n-stlan,
        stlal TYPE rc29n-stlal,
        posnr TYPE rc29p-posnr,
        idnrk TYPE rc29p-idnrk,
        zsuff TYPE zsuff,
        zsequ(04),
        zseqc(04),
        aennr TYPE rc29n-aennr,
        bmeng(20),
        bmein TYPE rc29k-bmein,
        stlst TYPE rc29k-stlst,
        postp TYPE rc29p-postp,
        menge(20),
        zstgb(20),
        meins TYPE rc29p-meins,
        itsob TYPE rc29p-itsob,
        zeitm TYPE zeitm,
        clpt(01),
        dpid  TYPE rcukd-knnam,
        upct(01),
        zupgn TYPE zupgn,
        zinfo TYPE zinfo,
        zresult LIKE sy-msgty,
        zmsg LIKE cfgnl-msglin,
      END   OF it_excl.
DATA: BEGIN OF it_ncol OCCURS 0,
        upct(01),
        aennr TYPE rc29n-aennr,
        matnr TYPE mara-matnr,
        werks TYPE t001w-werks,
        stlan TYPE rc29n-stlan,
        stlal TYPE rc29n-stlal,
        posnr TYPE rc29p-posnr,
        idnrk TYPE rc29p-idnrk,
        zsuff TYPE zsuff,
        zseqc(04),
        zsequ(04),
        bmeng(20),
        bmein TYPE rc29k-bmein,
        stlst TYPE rc29k-stlst,
        postp TYPE rc29p-postp,
        menge(20),
        zstgb(20),
        meins TYPE rc29p-meins,
        itsob TYPE rc29p-itsob,
        zeitm TYPE zeitm,
        clpt(01),
        dpid  TYPE rcukd-knnam,
        zupgn TYPE zupgn,
        zinfo TYPE zinfo,
        zresult LIKE sy-msgty,
        zmsg LIKE cfgnl-msglin,
      END   OF it_ncol.
DATA: wa_ncol LIKE it_ncol.
DATA: BEGIN OF it_colo OCCURS 0,
        upct(01),
        matnr TYPE mara-matnr,
        werks TYPE t001w-werks,
        stlan TYPE rc29n-stlan,
        stlal TYPE rc29n-stlal,
        posnr TYPE rc29p-posnr,
        idnrk TYPE rc29p-idnrk,
        zsuff TYPE zsuff,
        zseqc(04),
        zsequ(04),
        aennr TYPE rc29n-aennr,
        bmeng(20),
        bmein TYPE rc29k-bmein,
        stlst TYPE rc29k-stlst,
        postp TYPE rc29p-postp,
        menge(20),
        zstgb(20),
        meins TYPE rc29p-meins,
        itsob TYPE rc29p-itsob,
        zeitm TYPE zeitm,
        clpt(01),
        dpid  TYPE rcukd-knnam,
        zupgn TYPE zupgn,
        zinfo TYPE zinfo,
        zresult LIKE sy-msgty,
        zmsg LIKE cfgnl-msglin,
      END   OF it_colo.
DATA: wa_colo LIKE it_colo.

* TABLE SELECTION
DATA: it_bmdt LIKE ztbm_abxebmdt OCCURS 0 WITH HEADER LINE.
DATA: it_abxebmdt LIKE ztbm_abxebmdt OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_knnam OCCURS 0,
        knnam LIKE cukb-knnam,
      END   OF it_knnam.

DATA: BEGIN OF it_bmco OCCURS 0,
        mandt TYPE ztbm_abxebmdt-mandt,
        upct TYPE ztbm_abxebmdt-upct,
        mtno TYPE ztbm_abxebmdt-mtno,
        plnt TYPE ztbm_abxebmdt-plnt,
        usag TYPE ztbm_abxebmdt-usag,
        altn TYPE ztbm_abxebmdt-altn,
        pref TYPE ztbm_abxebmdt-pref,
        comp TYPE ztbm_abxebmdt-comp,
        suff TYPE ztbm_abxebmdt-suff,
        sequ TYPE ztbm_abxebmdt-sequ,
        seqc TYPE ztbm_abxebmdt-seqc,
        eono TYPE ztbm_abxebmdt-eono,
        bqty TYPE ztbm_abxebmdt-bqty,
        hunt TYPE ztbm_abxebmdt-hunt,
        stat TYPE ztbm_abxebmdt-stat,
        itca TYPE ztbm_abxebmdt-itca,
        qnty TYPE ztbm_abxebmdt-qnty,
        stgb TYPE ztbm_abxebmdt-stgb,
        unit TYPE ztbm_abxebmdt-unit,
        sppr TYPE ztbm_abxebmdt-sppr,
        eitm TYPE ztbm_abxebmdt-eitm,
        clpt TYPE ztbm_abxebmdt-clpt,
        dpid TYPE ztbm_abxebmdt-dpid,
        upgn TYPE ztbm_abxebmdt-upgn,
        zuser TYPE ztbm_abxebmdt-zuser,
        zsdat TYPE ztbm_abxebmdt-zsdat,
        zstim TYPE ztbm_abxebmdt-zstim,
        zedat TYPE ztbm_abxebmdt-zedat,
        zetim TYPE ztbm_abxebmdt-zetim,
        zbdat TYPE ztbm_abxebmdt-zbdat,
        zbtim TYPE ztbm_abxebmdt-zbtim,
        zbnam TYPE ztbm_abxebmdt-zbnam,
        zmode TYPE ztbm_abxebmdt-zmode,
        zresult TYPE ztbm_abxebmdt-zresult,
        zmsg TYPE ztbm_abxebmdt-zmsg,
      END OF it_bmco.
DATA: wa_bmco LIKE it_bmco.
DATA: BEGIN OF it_bmnc OCCURS 0,
        mandt TYPE ztbm_abxebmdt-mandt,
        upct TYPE ztbm_abxebmdt-upct,
        eono TYPE ztbm_abxebmdt-eono,
        mtno TYPE ztbm_abxebmdt-mtno,
        plnt TYPE ztbm_abxebmdt-plnt,
        usag TYPE ztbm_abxebmdt-usag,
        altn TYPE ztbm_abxebmdt-altn,
        pref TYPE ztbm_abxebmdt-pref,
        comp TYPE ztbm_abxebmdt-comp,
        suff TYPE ztbm_abxebmdt-suff,
        seqc TYPE ztbm_abxebmdt-seqc,
        sequ TYPE ztbm_abxebmdt-sequ,
        bqty TYPE ztbm_abxebmdt-bqty,
        hunt TYPE ztbm_abxebmdt-hunt,
        stat TYPE ztbm_abxebmdt-stat,
        itca TYPE ztbm_abxebmdt-itca,
        qnty TYPE ztbm_abxebmdt-qnty,
        stgb TYPE ztbm_abxebmdt-stgb,
        unit TYPE ztbm_abxebmdt-unit,
        sppr TYPE ztbm_abxebmdt-sppr,
        eitm TYPE ztbm_abxebmdt-eitm,
        clpt TYPE ztbm_abxebmdt-clpt,
        dpid TYPE ztbm_abxebmdt-dpid,
        upgn TYPE ztbm_abxebmdt-upgn,
        zuser TYPE ztbm_abxebmdt-zuser,
        zsdat TYPE ztbm_abxebmdt-zsdat,
        zstim TYPE ztbm_abxebmdt-zstim,
        zedat TYPE ztbm_abxebmdt-zedat,
        zetim TYPE ztbm_abxebmdt-zetim,
        zbdat TYPE ztbm_abxebmdt-zbdat,
        zbtim TYPE ztbm_abxebmdt-zbtim,
        zbnam TYPE ztbm_abxebmdt-zbnam,
        zmode TYPE ztbm_abxebmdt-zmode,
        zresult TYPE ztbm_abxebmdt-zresult,
        zmsg TYPE ztbm_abxebmdt-zmsg,
      END OF it_bmnc.
DATA: wa_bmnc LIKE it_bmnc.
DATA: BEGIN OF it_mast OCCURS 0,
        matnr TYPE mast-matnr,  "MATNRIAL NO
        werks TYPE mast-werks,  "PLANT
        stlan TYPE mast-stlan,  "BOM USAGE
        stlal TYPE mast-stlal,  "ALTERNATIVE BOM
        posnr TYPE stpo-posnr,  "BOM item number(PREFIX)
        idnrk TYPE stpo-idnrk,  "BOM component
        zsuff TYPE stpo-suff ,  "BOM item text1(SUFFIX)
        zsequ TYPE stpo-sequ,
        stlnr TYPE mast-stlnr,  "Bill of material
        stlkn TYPE stpo-stlkn,  "BOM item node number
      END OF it_mast.
DATA: BEGIN OF it_stpo OCCURS 0,
        posnr TYPE stpo-posnr,  "BOM item number(PREFIX)
        idnrk TYPE stpo-idnrk,  "BOM component
        zsuff TYPE stpo-suff,  "BOM item text1(SUFFIX)
        zsequ TYPE stpo-sequ,
        stlkn TYPE stpo-stlkn,
*        ZINFO TYPE STPO-ZINFO, "COLOR SEQUENCE
      END OF it_stpo.
DATA: BEGIN OF it_subm OCCURS 0,
        matnr LIKE mast-matnr,
        werks LIKE mast-werks,
        stlan LIKE mast-stlan,
        stlal LIKE mast-stlal,
      END   OF it_subm.
DATA: BEGIN OF it_engb OCCURS 0,
        matnr TYPE ztbm_abxebmdt-mtno,
        werks TYPE ztbm_abxebmdt-plnt,
        stlan TYPE ztbm_abxebmdt-usag,
        stlal TYPE ztbm_abxebmdt-altn,
        zmode,
        zmsg  LIKE cfgnl-msglin,
      END OF it_engb.
DATA: wa_engb LIKE it_engb.
DATA: it_vel TYPE ztbm_sub_bom_vel OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_mm02 OCCURS 0,
        comp       TYPE ztbm_abxebmdt-comp,  "Compenente
        plnt       TYPE ztbm_abxebmdt-plnt,  "Compenente
        stgb       TYPE ztbm_abxebmdt-stgb,  "STRUCTURE TYPE
        sppr       TYPE ztbm_abxebmdt-sppr,  "Special procurement
        normt      TYPE mara-normt,    "Industry Standard Descriptio
        sobsl      TYPE marc-sobsl,    "Special procurement type
        beskz      TYPE marc-beskz,    "Procurement type
        ncost      TYPE marc-ncost,    "No Costing
        dispo      TYPE marc-dispo,    "MRP controller
      END OF it_mm02.
DATA: BEGIN OF it_mass OCCURS 0,
        comp       TYPE ztbm_abxebmdt-comp,  "Compenente
        plnt       TYPE ztbm_abxebmdt-plnt,  "Compenente
        stgb       TYPE ztbm_abxebmdt-stgb,  "STRUCTURE TYPE
        sppr       TYPE ztbm_abxebmdt-sppr,  "Special procurement
        normt      TYPE mara-normt,    "Industry Standard Descriptio
        sobsl      TYPE marc-sobsl,    "Special procurement type
        ncost      TYPE marc-ncost,    "No Costing
        dispo      TYPE marc-dispo,    "MRP controller
        msg        LIKE cfgnl-msglin,
      END OF it_mass.

*/// START : Issue:20041115-003 Changed on 2004.11.24, Changed by BSBAE
CONSTANTS: c_capid   LIKE rc29l-capid VALUE 'PP01'. "Application
*/// START : Issue:20041115-003 Changed on 2004.11.24, Changed by
*----------------------------------------------------------------------*
* JOB DATA
*----------------------------------------------------------------------*
DATA: wa_jobcount LIKE  tbtcjob-jobcount ,
      wa_jobname  LIKE  tbtcjob-jobname,
*     WA_JOBNAME  LIKE  TBTCJOB-JOBNAME VALUE 'YTEST_KJO_007',
      wa_report   LIKE  sy-repid,
      wa_dbcnt_ix LIKE  sy-tabix.

RANGES r_datum FOR sy-datum.
*----------------------------------------------------------------------*
* BOM EXPLOSION
*----------------------------------------------------------------------*
DATA : BEGIN OF it_bom_exploded OCCURS 0,
        matnr      LIKE   stpov-matnr,   "Material
        werks      LIKE   stpov-werks,   "Plant
        kzkfg      TYPE   mara-kzkfg,    "Configurable Material
       END   OF it_bom_exploded.
DATA : BEGIN OF it_exp_mess OCCURS 0,
        matnr      LIKE   stpov-matnr,   "Material
        werks      LIKE   stpov-werks,   "Plant
        kzkfg      TYPE   mara-kzkfg,    "Configurable Material
        msg        LIKE   cfgnl-msglin,
       END   OF it_exp_mess.
DATA : it_mc29s   LIKE mc29s   OCCURS 0 WITH HEADER LINE,
       it_stpov   LIKE stpov   OCCURS 0 WITH HEADER LINE,
       it_cscequi LIKE cscequi OCCURS 0 WITH HEADER LINE,
       it_cscknd  LIKE cscknd  OCCURS 0 WITH HEADER LINE,
       it_cscmat  LIKE cscmat  OCCURS 0 WITH HEADER LINE,
       it_cscstd  LIKE cscstd  OCCURS 0 WITH HEADER LINE,
       it_csctpl  LIKE csctpl  OCCURS 0 WITH HEADER LINE.
DATA:  wa_last.

*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_mess.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: wa_line_idx TYPE i,
      wa_erro_idx TYPE i,
      wa_check.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-200.
*PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
*                                   USER-COMMAND UCOM,
*            P_RDO2 RADIOBUTTON GROUP R1.
*SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-200.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_zedat LIKE ztbm_abxebmdt-zedat DEFAULT sy-datum,
  p_zbtim LIKE ztbm_abxebmdt-zbtim,
  p_submit NO-DISPLAY.


* EXCEL DATA UPLOAD
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT',
  p_tcode LIKE tstc-tcode DEFAULT 'CS01 & CS02'.
SELECTION-SCREEN END   OF BLOCK b1.
