*----------------------------------------------------------------------*
*   INCLUDE MZSARAPTOP                                                 *
*----------------------------------------------------------------------*
TABLES: mara, marc, lfa1, makt, mard, riwo00, rmmg1,t100, t001l,rm07m,
        pvbe,lagp, qmel, tj30t,jest,itobattr, mchb,mseg.

TYPE-POOLS: vrm.
DATA: matnr TYPE mara-matnr,
      qmgrp TYPE qmgrp,
      qmcod TYPE qmcod,
*      RKMNG(10) TYPE N,
      rkmng  TYPE char10,
      otgrp TYPE otgrp,
      oteil TYPE oteil,
      fegrp TYPE fegrp,
      fecod TYPE fecod,
      urcod TYPE urcod,
      urgrp TYPE urgrp,
      bwart TYPE bwart,
      qmnum TYPE qmel-qmnum,
      vn_matnr TYPE matnr,
      v_fecod  LIKE qmfe-fecod, "03.28.2014 Victor Added
      lifnr LIKE qmel-lifnum,
      ernam TYPE ernam,
      erdat(10),
      mzeit(8),
      lgort TYPE lgort,
      cuasetxt(40),
      p_txt04 LIKE tj30t-txt04,
      w_ans(2),
      mblnr TYPE mseg-mblnr,
      zbudat TYPE mseg-zbudat,
      zlocation type ZSQM_CI_QALS-QCODEGRP_LOC.

DATA:  zsqm_ci_qmel-codegrp_vh LIKE zsqm_ci_qmel-codegrp_vh,
       zsqm_ci_qmel-code_vh LIKE zsqm_ci_qmel-code_vh.

DATA:  codegrp_vh LIKE zsqm_ci_qmel-codegrp_vh,
       code_vh LIKE zsqm_ci_qmel-code_vh.

DATA:  ctumode LIKE ctu_params-dismode VALUE 'N',
       cupdate LIKE ctu_params-updmode VALUE 'L',
       bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
*      messages of call transaction
       messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
         mdoc LIKE bapi2017_gm_head_ret-mat_doc,
         myear LIKE bapi2017_gm_head_ret-doc_year.

DATA: BEGIN OF it_qmnum OCCURS 0,
      qmnum LIKE  bdcmsgcoll-msgv1,
      matnr LIKE mara-matnr,
      zerdat LIKE qmel-erdat,
      maktx LIKE makt-maktx,
      matkl LIKE mara-matkl,
      zmeins LIKE mara-meins,
      rkmng LIKE qmel-rkmng,
      zreason LIKE qpct-kurztext,
      otgrp LIKE qmfe-otgrp,
      txtcdur LIKE qpct-kurztext,
      zrootwc LIKE qpct-kurztext,
      pdfbc(250) TYPE c,
      flag(1),
      END OF it_qmnum.

DATA: eflag,
      p_cursor(20).

DATA : name TYPE zscrap_var-name.

DATA: p_qmnum LIKE qmel-qmnum,
      p_matnr LIKE qmel-matnr,
      p_ndate  LIKE qmel-qmdat,                             "(10),
      p_nuser LIKE qmel-ernam,
      p_ntime(8),   " like qmel-mzeit,
      p_status LIKE tj30t-txt30,
      p_copy(3),
      p_ucomm LIKE sy-ucomm.

DATA: w_or_replace LIKE sy-ucomm.
DATA: w_call(4),
      w_sub_call(4),
      c3(60),
      w_repid LIKE sy-repid,
      w_dynnr LIKE sy-dynnr,
      w_print_error(1).

DATA: return_tab LIKE TABLE OF ddshretval WITH HEADER LINE.
