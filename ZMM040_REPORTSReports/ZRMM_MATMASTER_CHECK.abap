REPORT zrmm_matmaster_check.
*&--------------------------------------------------------------------&*
*&  Program id   : ZRMM_MATMASTER_CHECK (production material).
*&  Developer    : Furong
*&  Description  : Check material master data
*&
*&--------------------------------------------------------------------&*
*& Date        Transport        Description
*& 02/08/2005  UD1K914090       initial program.
*& 02/14/2005                   check plant/storage view
*&--------------------------------------------------------------------&*

*tables: mard.
DATA: BEGIN OF it_disp OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE marc-werks,
        lgort LIKE mard-lgort,
        mstae LIKE mara-mstae,
        matkl LIKE mara-matkl,
        mstde LIKE mara-mstde,
        brgew LIKE mara-brgew,
        ntgew LIKE mara-ntgew,
        gewei LIKE mara-gewei,
        profl LIKE mara-profl,
        ferth LIKE mara-ferth,
        ekgrp LIKE marc-ekgrp,
        mmsta LIKE marc-mmsta,
        kordb LIKE marc-kordb,
        fabkz LIKE marc-fabkz,
        stawn LIKE marc-stawn,
        herkl LIKE marc-herkl,
        dismm LIKE marc-dismm,
        dispo LIKE marc-dispo,
        disls LIKE marc-disls,
        bstmi LIKE marc-bstmi,
        bstma LIKE marc-bstma,
        bstrf LIKE marc-bstrf,
        beskz LIKE marc-beskz,
        sobsl LIKE marc-sobsl,
        rgekz LIKE marc-rgekz,
        lgpro LIKE marc-lgpro,
        vspvb LIKE marc-vspvb,
        lgfsb LIKE marc-lgfsb,
        fhori LIKE marc-fhori,
        plifz LIKE marc-plifz,
        mrppp LIKE marc-mrppp,
        eisbe LIKE marc-eisbe,
        ausdt LIKE marc-ausdt,
        lgpbe LIKE mard-lgpbe,
        tempb LIKE mara-tempb,
        abcin LIKE marc-abcin,
        raube LIKE mara-raube,
        xmcng LIKE marc-xmcng,
        bklas LIKE mbew-bklas,
        stprs LIKE mbew-stprs,
        ltkza LIKE mlgn-ltkza,
        ltkze LIKE mlgn-ltkze,
        lgtyp LIKE mlgt-lgtyp,
        lgpla LIKE mlgt-lgpla,
        rdmng LIKE mlgt-rdmng,
        error(1),
        remarks(255),
        ct TYPE lvc_t_scol,
        END OF it_disp.

DATA: BEGIN OF it_mara OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE marc-werks,
        mstae LIKE mara-mstae,
        matkl LIKE mara-matkl,
        mstde LIKE mara-mstde,
        brgew LIKE mara-brgew,
        ntgew LIKE mara-ntgew,
        gewei LIKE mara-gewei,
        profl LIKE mara-profl,
        ferth LIKE mara-ferth,
        ekgrp LIKE marc-ekgrp,
        mmsta LIKE marc-mmsta,
        kordb LIKE marc-kordb,
        fabkz LIKE marc-fabkz,
        stawn LIKE marc-stawn,
        herkl LIKE marc-herkl,
        dismm LIKE marc-dismm,
        dispo LIKE marc-dispo,
        disls LIKE marc-disls,
        bstmi LIKE marc-bstmi,
        bstma LIKE marc-bstma,
        bstrf LIKE marc-bstrf,
        beskz LIKE marc-beskz,
        sobsl LIKE marc-sobsl,
        rgekz LIKE marc-rgekz,
        lgpro LIKE marc-lgpro,
        vspvb LIKE marc-vspvb,
        lgfsb LIKE marc-lgfsb,
        fhori LIKE marc-fhori,
        plifz LIKE marc-plifz,
        mrppp LIKE marc-mrppp,
        eisbe LIKE marc-eisbe,
        ausdt LIKE marc-ausdt,
        tempb LIKE mara-tempb,
        abcin LIKE marc-abcin,
        raube LIKE mara-raube,
        xmcng LIKE marc-xmcng,
        bklas LIKE mbew-bklas,
        stprs LIKE mbew-stprs,
        lgort LIKE mard-lgort,
        lgpbe LIKE mard-lgpbe,
        ltkza LIKE mlgn-ltkza,
        ltkze LIKE mlgn-ltkze,
        lgtyp LIKE mlgt-lgtyp,
        lgpla LIKE mlgt-lgpla,
        rdmng LIKE mlgt-rdmng,
        error(1),
        remarks(255),
        END OF it_mara.

DATA: BEGIN OF it_main OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE marc-werks,
        lgort LIKE mard-lgort,
        mstae LIKE mara-mstae,
        matkl LIKE mara-matkl,
        mstde LIKE mara-mstde,
        brgew LIKE mara-brgew,
        ntgew LIKE mara-ntgew,
        gewei LIKE mara-gewei,
        profl LIKE mara-profl,
        ferth LIKE mara-ferth,
        ekgrp LIKE marc-ekgrp,
        mmsta LIKE marc-mmsta,
        kordb LIKE marc-kordb,
        fabkz LIKE marc-fabkz,
        stawn LIKE marc-stawn,
        herkl LIKE marc-herkl,
        dismm LIKE marc-dismm,
        dispo LIKE marc-dispo,
        disls LIKE marc-disls,
        bstmi LIKE marc-bstmi,
        bstma LIKE marc-bstma,
        bstrf LIKE marc-bstrf,
        beskz LIKE marc-beskz,
        sobsl LIKE marc-sobsl,
        rgekz LIKE marc-rgekz,
        lgpro LIKE marc-lgpro,
        vspvb LIKE marc-vspvb,
        lgfsb LIKE marc-lgfsb,
        fhori LIKE marc-fhori,
        plifz LIKE marc-plifz,
        mrppp LIKE marc-mrppp,
        eisbe LIKE marc-eisbe,
        ausdt LIKE marc-ausdt,
        lgpbe LIKE mard-lgpbe,
        tempb LIKE mara-tempb,
        abcin LIKE marc-abcin,
        raube LIKE mara-raube,
        xmcng LIKE marc-xmcng,
        bklas LIKE mbew-bklas,
        stprs LIKE mbew-stprs,
        ltkza LIKE mlgn-ltkza,
        ltkze LIKE mlgn-ltkze,
        lgtyp LIKE mlgt-lgtyp,
        lgpla LIKE mlgt-lgpla,
        rdmng LIKE mlgt-rdmng,
        error(1),
        remarks(255),
        END OF it_main.

DATA: BEGIN OF it_mard OCCURS 0,
        matnr LIKE mard-matnr,
        werks LIKE mard-werks,
        lgort LIKE mard-lgort,
        lgpbe LIKE mard-lgpbe,
        END OF it_mard.

DATA: it_error LIKE it_mara OCCURS 0 WITH HEADER LINE,
      it_mara1 LIKE it_mara OCCURS 0 WITH HEADER LINE,
      it_mara2 LIKE it_mara OCCURS 0 WITH HEADER LINE,
      wa_disp LIKE it_disp OCCURS 0 WITH HEADER LINE,
      it_disp1 LIKE TABLE OF it_disp,
      it_color TYPE lvc_t_scol,
      wa_color LIKE LINE OF it_color.
DATA: m_nlines TYPE i,
      m_werks LIKE marc-werks,
      m_remarks(255).

DATA: ok_code LIKE sy-ucomm,
      g_container TYPE scrfname VALUE 'CC_CONTAINER',
      grid1  TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container.

*DATA: it_fieldcat1 TYPE slis_t_fieldcat_alv WITH HEADER LINE,
DATA: it_fieldcat1 TYPE lvc_t_fcat WITH HEADER LINE,
      it_fieldcat LIKE TABLE OF it_fieldcat1,
      is_layout TYPE lvc_s_layo.
*      it_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE.

DATA: g_repid LIKE sy-repid,
      x_save,                     "for Parameter I_SAVE
      gs_variant TYPE disvariant. "for parameter IS_VARIANT

DATA: row_table TYPE lvc_t_row.

g_repid = sy-repid.
gs_variant-report = g_repid.



SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_matnr FOR it_mara-matnr,
*                s_mtart for wa_sa-mtart default 'ROH',
                s_profl FOR it_mara-profl,
                s_werks FOR it_mara-werks DEFAULT 'P001',
*                s_ekgrp for wa_sa-ekgrp,
                s_dispo FOR it_mara-dispo.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_error  RADIOBUTTON GROUP rgrp,
            p_all   RADIOBUTTON GROUP rgrp DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK block1.


SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_mstde TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(27) text-021.
*selection-screen position 30.
PARAMETERS: p_bstma TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 33(27) text-022.
*selection-screen position 60.
PARAMETERS: p_ausdt TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 63(27) text-023.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_fhori TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(27) text-024.
PARAMETERS: p_ferth TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 33(27) text-025.
PARAMETERS: p_sobsl TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 63(27) text-026.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_abcin TYPE c AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN COMMENT 3(27) text-027.
SELECTION-SCREEN END OF LINE.

*p_fhori
*p_ferth
*p_sobsl
*p_abcin


SELECTION-SCREEN END OF BLOCK block2.


START-OF-SELECTION.

  PERFORM process_data.

  CALL SCREEN 100.

END-OF-SELECTION.


****************
* Forms
********

FORM process_data.
  SELECT a~matnr g~maktx b~werks  a~mstae a~matkl a~mstde a~brgew a~ntgew
         a~gewei a~profl
         a~ferth b~ekgrp b~mmsta b~kordb b~fabkz b~stawn b~herkl b~dismm
         b~dispo b~disls b~bstmi b~bstma b~bstrf b~beskz b~sobsl b~rgekz
         b~lgpro b~vspvb b~lgfsb b~fhori b~plifz b~mrppp b~eisbe b~ausdt
          a~tempb b~abcin a~raube b~xmcng
         d~bklas d~stprs c~lgort c~lgpbe e~ltkza e~ltkze
         f~lgtyp f~lgpla f~rdmng
         INTO TABLE it_mara
         FROM mara AS a
         INNER JOIN marc AS b ON a~matnr = b~matnr AND b~lvorm EQ space
         INNER JOIN makt AS g ON a~matnr = g~matnr
         INNER JOIN mard AS c ON a~matnr = c~matnr AND b~werks = c~werks
               AND c~lvorm EQ space
         INNER JOIN mbew AS d ON a~matnr = d~matnr AND b~werks = d~bwkey
               AND d~lvorm EQ space
         INNER JOIN mlgn AS e ON a~matnr = e~matnr AND e~lvorm EQ space
         INNER JOIN mlgt AS f ON a~matnr = f~matnr AND f~lvorm EQ space
         WHERE a~mtart = 'ROH' AND a~matnr IN s_matnr
               AND b~werks IN s_werks AND c~lgort <> '9999'
               AND b~dispo IN s_dispo
               AND a~profl IN s_profl
               AND d~bwkey IN s_werks
               AND a~lvorm EQ space.

  SELECT i~matnr m~maktx j~werks i~mstae i~matkl i~mstde i~brgew
  i~ntgew i~gewei i~profl i~ferth j~ekgrp j~mmsta j~kordb
  j~fabkz j~stawn j~herkl j~dismm j~dispo j~disls j~bstmi
  j~bstma j~bstrf j~beskz j~sobsl j~rgekz j~lgpro j~vspvb
  j~lgfsb j~fhori j~plifz j~mrppp j~eisbe j~ausdt
  i~tempb j~abcin i~raube j~xmcng
  l~bklas l~stprs
  INTO TABLE it_mara1
  FROM mara AS i
  INNER JOIN marc AS j ON i~matnr = j~matnr AND j~lvorm EQ space
  INNER JOIN makt AS m ON i~matnr = m~matnr
  INNER JOIN mbew AS l ON i~matnr = l~matnr AND j~werks = l~bwkey
                                            AND l~lvorm EQ space
  WHERE i~mtart = 'ROH' AND i~matnr IN s_matnr
        AND j~werks IN s_werks
        AND i~profl IN s_profl
        AND j~dispo IN s_dispo
        AND l~bwkey IN s_werks
        AND i~lvorm EQ space.


  SELECT matnr werks lgort lgpbe INTO TABLE it_mard FROM mard
                     WHERE matnr IN s_matnr
                       AND werks IN s_werks
                       AND lgort <> '9999'
                       AND lvorm EQ space.

  LOOP AT it_mard.
    READ TABLE it_mara1 WITH KEY matnr = it_mard-matnr
                                 werks = it_mard-werks.
    IF sy-subrc = 0.
      it_mara2 = it_mara1.
      it_mara2-lgort = it_mard-lgort.
      it_mara2-lgpbe = it_mard-lgpbe.
      APPEND it_mara2.
      CLEAR it_mara2.
    ENDIF.
  ENDLOOP.

  LOOP AT it_mara1.
    READ TABLE it_mara2 WITH KEY matnr = it_mara1-matnr
                                 werks = it_mara1-werks.
    IF sy-subrc = 0.
      DELETE it_mara1.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF it_mara2 TO it_mara1.

  LOOP AT it_mara1.
    READ TABLE it_mara WITH KEY matnr = it_mara1-matnr
                                werks = it_mara1-werks
                                lgort = it_mara1-lgort.
    IF sy-subrc <> 0.
      it_mara = it_mara1.
      APPEND it_mara.
      CLEAR it_mara.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE it_mara LINES m_nlines.
  IF m_nlines = 0 .
    MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '009' WITH text-001.
    EXIT.
  ENDIF.

  SORT it_mara BY bklas matnr werks.

  LOOP AT it_mara.

** X-plant Status **
    IF it_mara-mstae <> '12'.
      it_error-mstae = it_mara-mstae.
      IF it_mara-mstae = '  '.
        it_error-mstae = '*'.
      ENDIF.
      it_mara-error = 'X'.
      it_mara-remarks = 'X-plt Status'.
      it_error-remarks = 'X-plt Status'.
    ENDIF.

** Material Group **
    CASE it_mara-bklas.
      WHEN '3001'.
        IF it_mara-matkl <> 'NF-LP    '.
          IF it_mara-dispo <> 'M02'.
            it_error-matkl = it_mara-matkl.
            IF it_mara-matkl = '  '.
              it_error-matkl = '*'.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Matl Grp'
                           INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
      WHEN '3005'.
        IF it_mara-matkl <> 'NF-LP    '.
          it_error-matkl = it_mara-matkl.
          IF it_mara-matkl = '  '.
            it_error-matkl = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'Matl Grp'
                      INTO it_mara-remarks.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN '3000'.
        IF it_mara-matkl <> 'NF-KD    '.
          IF it_mara-dispo <> 'M02'.
            it_error-matkl = it_mara-matkl.
            IF it_mara-matkl = '  '.
              it_error-matkl = '*'.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Matl Grp'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Valid from **
    IF it_mara-mstde = '00000000'.
      IF it_mara-dispo <> 'M02'.
        it_error-mstde = '99999999'.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Val fr'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Gross Weigh **
    IF it_mara-brgew = 0.
      IF it_mara-dispo <> 'M02'.
        it_error-brgew = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Grs W'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Net Weigh **
    IF it_mara-ntgew = 0.
      IF it_mara-dispo <> 'M02'.
        it_error-ntgew = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Net W'
                       INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Weight UoM **
    IF it_mara-gewei <> 'KG'.
      IF it_mara-dispo <> 'M02'.
        it_error-gewei = it_mara-gewei.
        IF it_mara-gewei = '  '.
          it_error-gewei = '*'.
        ENDIF.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'W UoM'
                       INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** LP/KD/MIP **
    CASE it_mara-bklas.
      WHEN '3001' OR '3005'.
        IF it_mara-profl <> 'V'.
          it_error-profl = it_mara-profl.
          IF it_mara-profl = ' '.
            it_error-profl = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'LP/KD/MIP'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN '3000'.
        IF it_mara-profl <> 'K'.
          it_error-profl = it_mara-profl.
          IF it_mara-profl = ' '.
            it_error-profl = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'LP/KD/MIP'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Prod./Insp. memo **
*** no check **

** Purchase Group **
    IF it_mara-ekgrp = ' '.
      it_error-ekgrp = '*'.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Pur Grp'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
    ENDIF.

** Plant-sp mat status **
    IF it_mara-mmsta <> '12'.
*        if it_mara-dispo <> 'M02'.
      it_error-mmsta = it_mara-mmsta.
      IF it_mara-mmsta = '  '.
        it_error-mmsta = '*'.
      ENDIF.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Mat Sta'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
*        endif.
    ENDIF.

** Source List **
    IF it_mara-kordb = ' '.
      it_error-kordb = '*'.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'S Lst'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
    ENDIF.

** JIT Indicator **
    CASE it_mara-bklas.
      WHEN '3001'.
        IF it_mara-dispo <> 'M02'.
          IF it_mara-fabkz <> '1'.
            it_error-fabkz = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'JIT'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
      WHEN '3005' OR '3000'.
        IF it_mara-fabkz <> ' '.
          it_error-fabkz = it_mara-fabkz.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'JIT'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Comm./imp. Code **
    IF it_mara-dispo <> 'M02'.
      CASE it_mara-bklas.
        WHEN '3000'.
          IF it_mara-stawn = ' '.
            it_error-stawn = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Com/imp.Cd'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN '3005' OR '3001'.
          IF it_mara-stawn <> ' '.
            it_error-stawn = it_mara-stawn.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Com/imp.Cd'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN OTHERS.
          it_error-bklas = it_mara-bklas.
          it_mara-error = 'X'.
          it_mara-remarks = 'Valuation Class'.
          it_error-remarks = it_mara-remarks.
      ENDCASE.
    ENDIF.

** Country **
    IF it_mara-dispo <> 'M02'.
      CASE it_mara-bklas.
        WHEN '3000'.
          IF it_mara-herkl = ' '.
            it_error-herkl = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Country'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN '3005' OR '3001'.
          IF it_mara-herkl <> ' '.
            it_error-herkl = it_mara-herkl.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Country'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN OTHERS.
          it_error-bklas = it_mara-bklas.
          it_mara-error = 'X'.
          it_mara-remarks = 'Valuation Class'.
          it_error-remarks = it_mara-remarks.
      ENDCASE.
    ENDIF.

** MRP Type **
    IF it_mara-dismm <> 'PD'.
      IF it_mara-dispo <> 'M02'.
        it_error-dismm = it_mara-dismm.
        IF it_mara-dismm = '  '.
          it_error-dismm = '*'.
        ENDIF.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'MRP Ty'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ELSE.
        IF it_mara-dismm <> 'ND'.
          it_error-dismm = it_mara-dismm.
          IF it_mara-dismm = '  '.
            it_error-dismm = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'MRP Ty'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      ENDIF.
    ENDIF.

** MRP Controller **
    IF it_mara-dispo = ' '.
      it_error-dispo = '*'.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'MRP Ctler'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
    ENDIF.

** Lot Size **
    CASE it_mara-bklas.
      WHEN '3000'.
        IF it_mara-disls <> 'WB'.
          it_error-disls = it_mara-disls.
          IF it_mara-disls = '  '.
            it_error-disls = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'Lot S'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN '3005'.
        IF it_mara-disls <> 'EX'.
          it_error-disls = it_mara-disls.
          IF it_mara-disls = '  '.
            it_error-disls = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'Lot S'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN '3001'.
        IF it_mara-disls <> 'EX' AND it_mara-disls <> 'PK'.
          it_error-disls = it_mara-disls.
          IF it_mara-disls = '  '.
            it_error-disls = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'Lot S'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Min Lot Size **
    IF it_mara-bstmi = 0.
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E001'.
        it_error-bstmi = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Min Lot S'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** FOR E002
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E002'.
        it_error-bstmi = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Min Lot S'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** END E002
    ENDIF.

** Max Lot Size **
    IF it_mara-bstma = 0.
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E001'.
        it_error-bstma = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Max Lot S'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** FOR E002
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E002'.
        it_error-bstma = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Max Lot S'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** END E002
    ENDIF.

** Round Value **
    IF it_mara-bstrf = 0.
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E001'.
        it_error-bstrf = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Round Val'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** FOR E002
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E002'.
        it_error-bstrf = 999999999.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Round Val'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** END E002

    ENDIF.

** Procurement Type **
    IF it_mara-beskz <> 'F'.
      it_error-beskz = it_mara-beskz.
      IF it_mara-beskz = ' '.
        it_error-beskz = '*'.
      ENDIF.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Proc Type'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
    ENDIF.

** Special Procurement Key **
    IF it_mara-werks = 'E001' AND it_mara-sobsl <> '40'.
      it_error-sobsl = it_mara-sobsl.
      IF it_mara-sobsl = ' '.
        it_error-sobsl = '*'.
      ENDIF.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Spe Proc Key'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
** for e002. ????????????????????
    ELSEIF it_mara-werks = 'E002' AND it_mara-sobsl <> '40'.
      it_error-sobsl = it_mara-sobsl.
      IF it_mara-sobsl = ' '.
        it_error-sobsl = '*'.
      ENDIF.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Spe Proc Key'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
** end e002
    ELSE.
      IF it_mara-werks = 'P001' AND it_mara-sobsl <> ' '.
        it_error-sobsl = it_mara-sobsl.
        IF it_mara-sobsl = ' '.
          it_error-sobsl = '*'.
        ENDIF.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Spe Proc Key'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Backflush Indicator **
    IF it_mara-rgekz <> '1'.
      CASE it_mara-dispo.
        WHEN 'M02'.
          IF it_mara-rgekz <> ' '.
            it_error-rgekz = it_mara-rgekz.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'BF Ind'
                INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN OTHERS.
**** check for EC in p001 *
          IF it_mara-werks = 'P001'.
            SELECT SINGLE werks INTO m_werks FROM marc
** FOR E002
*                   WHERE matnr = it_mara-matnr AND werks = 'E001'.
                   WHERE matnr = it_mara-matnr
                     AND ( werks = 'E001' OR WERKS = 'E002' ).
** END E002
            IF sy-subrc <> 0.
              it_error-rgekz = it_mara-rgekz.
              IF it_mara-rgekz = ' '.
                it_error-rgekz = '*'.
              ENDIF.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & ' 'BF Ind'
                          INTO it_mara-remarks SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
*** end of check *
          ELSE.
            it_error-rgekz = it_mara-rgekz.
            IF it_mara-rgekz = ' '.
              it_error-rgekz = '*'.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'BF Ind'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
      ENDCASE.
    ENDIF.

** Issue stor. location **
    IF it_mara-werks = 'E001'.
      IF it_mara-lgpro <> 'E100'.
        it_error-lgpro = it_mara-lgpro.
        IF it_mara-lgpro = '    '.
          it_error-lgpro = '*'.
        ENDIF.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Iss stor loc'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** for e002   ???????????????????????????
    ELSEIF it_mara-werks = 'E002'.
      IF it_mara-lgpro <> 'E100'.
        it_error-lgpro = it_mara-lgpro.
        IF it_mara-lgpro = '    '.
          it_error-lgpro = '*'.
        ENDIF.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Iss stor loc'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** end e002
    ELSE.
      IF it_mara-dispo = 'M02'.
        IF it_mara-lgpro <> ' '.
          it_error-lgpro = it_mara-lgpro.
          IF it_mara-lgpro = '    '.
            it_error-lgpro = '*'.
          ENDIF.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & ' 'Iss stor loc'
                      INTO it_mara-remarks SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      ELSE.
**** check for EC in p001 *
*         select werks into m_werks from marc
*                      where matnr = it_mara-matnr and werks = 'E001'.
*         endselect.
        SELECT SINGLE werks INTO m_werks FROM marc
** For e002
*        where matnr = it_mara-matnr and werks = 'E001'.
        WHERE matnr = it_mara-matnr
          AND ( werks = 'E001' OR werks = 'E002' ).
** end e002
        IF sy-subrc = 0.
          IF it_mara-lgpro <> ' '.
            it_error-lgpro = it_mara-lgpro.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Iss stor loc'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ELSE.
          CASE it_mara-bklas.
            WHEN '3000'.
              IF it_mara-lgpro <> 'P400'.
                it_error-lgpro = it_mara-lgpro.
                IF it_mara-lgpro = '    '.
                  it_error-lgpro = '*'.
                ENDIF.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & ' 'Iss stor loc'
                            INTO it_mara-remarks SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN '3001'.
              IF it_mara-lgpro = ' '.
                it_error-lgpro = '*'.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                            'Iss stor. loc' INTO it_mara-remarks
                            SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN '3005'.
              IF it_mara-lgpro <> 'P500'.
                it_error-lgpro = it_mara-lgpro.
                IF it_mara-lgpro = '    '.
                  it_error-lgpro = '*'.
                ENDIF.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                            'Iss stor. loc' INTO it_mara-remarks
                            SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN OTHERS.
              it_error-bklas = it_mara-bklas.
              it_mara-error = 'X'.
              it_mara-remarks = 'Valuation Class'.
              it_error-remarks = it_mara-remarks.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.

** Default supply area **
    CASE it_mara-dispo.
      WHEN 'M02'.
        IF it_mara-vspvb <> ' '.
          it_error-vspvb = it_mara-vspvb.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
                      'Supp area' INTO it_mara-remarks
                      SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
**** check for EC in p001 *
        m_werks = '    '.
        IF it_mara-werks = 'P001'.
          SELECT SINGLE werks INTO m_werks FROM marc
** For e002
*        where matnr = it_mara-matnr and werks = 'E001'.
         WHERE matnr = it_mara-matnr
           AND ( werks = 'E001' OR werks = 'E002' ).
** end e002
          IF sy-subrc = 0.
            IF it_mara-vspvb <> ' '.
              it_error-vspvb = it_mara-vspvb.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                     'Supp area' INTO it_mara-remarks
                      SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ELSE.
            IF it_mara-vspvb = ' '.
              it_error-vspvb = '*'.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                          'Supp area' INTO it_mara-remarks
                          SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        ELSE.
          IF it_mara-vspvb = ' '.
            it_error-vspvb = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & '
                        'Supp area' INTO it_mara-remarks
                        SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
    ENDCASE.

** Storage loc. for EP **
    IF it_mara-werks = 'E001' OR it_mara-dispo = 'M02'.
      IF it_mara-lgfsb <> ' '.
        it_error-lgfsb = it_mara-lgfsb.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & '
              'Stor loc EP' INTO it_mara-remarks
              SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** FOR E002
    ELSEIF it_mara-werks = 'E002' OR it_mara-dispo = 'M02'.
      IF it_mara-lgfsb <> ' '.
        it_error-lgfsb = it_mara-lgfsb.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & '
              'Stor loc EP' INTO it_mara-remarks
              SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
** END E002
    ELSE.
      CASE it_mara-bklas.
        WHEN '3000'.
          IF it_mara-lgfsb <> 'P400'.
            IF it_mara-dispo <> 'M02'.
              it_error-lgfsb = it_mara-lgfsb.
              IF it_mara-lgfsb = '    '.
                it_error-lgfsb = '*'.
              ENDIF.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                   'Stor loc EP' INTO it_mara-remarks
                   SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        WHEN '3001'.
          IF it_mara-lgfsb = ' '.
            IF it_mara-dispo <> 'M02'.
              it_error-lgfsb = '*'.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & ' 'Stor loc EP'
                  INTO it_mara-remarks SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        WHEN '3005'.
          IF it_mara-lgfsb <> 'P500'.
            IF it_mara-dispo <> 'M02'.
              it_error-lgfsb = it_mara-lgfsb.
              IF it_mara-lgfsb = '    '.
                it_error-lgfsb = '*'.
              ENDIF.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & ' 'Stor loc EP'
                          INTO it_mara-remarks SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        WHEN OTHERS.
          it_error-bklas = it_mara-bklas.
          it_mara-error = 'X'.
          it_mara-remarks = 'Valuation Class'.
          it_error-remarks = it_mara-remarks.
      ENDCASE.
    ENDIF.

** Sched Margin Key **
    IF it_mara-fhori <> '000'.
      it_error-fhori = it_mara-fhori.
      it_mara-error = 'X'.
      CONCATENATE it_mara-remarks ' & ' 'Sch Mg Key'
                  INTO it_mara-remarks SEPARATED BY ' '.
      it_error-remarks = it_mara-remarks.
    ENDIF.

**Plnd delivery time **
    IF it_mara-dispo = 'M02'.
      IF it_mara-plifz <> 0.
        it_error-plifz = it_mara-plifz.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Plnd D time'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ELSE.
      CASE it_mara-bklas.
        WHEN '3000'.
          IF it_mara-plifz <> 65.
            it_error-plifz = it_mara-plifz.
            IF it_mara-plifz = 0 OR it_mara-plifz ='0'.
              it_error-plifz = 999.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Plnd D time'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN '3001'.
          IF it_mara-plifz <> 0 AND it_mara-plifz <> 1.
            it_error-plifz = it_mara-plifz.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Plnd D time'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN '3005'.
          IF it_mara-plifz <> '0'.
            it_error-plifz = it_mara-plifz.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Plnd D time'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN OTHERS.
          it_error-bklas = it_mara-bklas.
          it_mara-error = 'X'.
          it_mara-remarks = 'Valuation Class'.
          it_error-remarks = it_mara-remarks.
      ENDCASE.
    ENDIF.

** Planning Calendar **
    IF it_mara-dispo = 'M02'.
      IF it_mara-mrppp <> ' '.
        it_error-mrppp = it_mara-mrppp.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Plng Cal'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ELSE.
      CASE it_mara-bklas.
        WHEN '3000' OR '3005'.
          IF it_mara-mrppp <> ' '.
            it_error-mrppp = it_mara-mrppp.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Plng Cal'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN '3001'.
          IF it_mara-mrppp = ' '.
            it_error-mrppp = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & ' 'Plng Cal'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        WHEN OTHERS.
          it_error-bklas = it_mara-bklas.
          it_mara-error = 'X'.
          it_mara-remarks = 'Valuation Class'.
          it_error-remarks = it_mara-remarks.
      ENDCASE.
    ENDIF.

** Safety Stock **

** Eff. Out **
    IF  it_mara-ausdt = '00000000'.
      IF it_mara-dispo <> 'M02'.
        it_error-ausdt = '99999999'.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & ' 'Eff Out'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Storage Bin **
    IF it_mara-lgpbe = ' '.
      IF it_mara-dispo <> 'M02' AND it_mara-werks <> 'E001'.
        it_error-lgpbe = '*'.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks '&'  'Stor Bin'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

* Backflush Cycle **
    CASE it_mara-bklas.
      WHEN '3000' OR '3001'.
        IF it_mara-tempb = '  '.
          IF it_mara-dispo <> 'M02'.
            it_error-tempb = '*'.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & '  'BF Cyc'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
      WHEN '3005'.
*S__by Paul
*        if it_mara-tempb <> '11'.
        IF it_mara-tempb EQ '3' OR
           it_mara-tempb EQ '4' OR
           it_mara-tempb EQ '9' OR
           it_mara-tempb EQ ' ' .
          IF it_mara-dispo <> 'M02'.
            it_error-tempb = it_mara-tempb.
            IF it_mara-tempb = '  '.
              it_error-tempb = '*'.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & '  'BF Cyc'
                        INTO it_mara-remarks SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Cycle Count **
    IF it_mara-abcin = ' '.
      IF it_mara-dispo <> 'M02'.
        it_error-abcin = '*'.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & '  'Cyc C'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Shop **
    IF it_mara-raube = ' '.
      IF it_mara-dispo <> 'M02'.
        it_error-raube = '*'.
        it_mara-error = 'X'.
        CONCATENATE it_mara-remarks ' & '  'Shop'
                    INTO it_mara-remarks SEPARATED BY ' '.
        it_error-remarks = it_mara-remarks.
      ENDIF.
    ENDIF.

** Neg stocks in plant **
    CASE it_mara-bklas.
      WHEN '3000' OR '3001'.
        IF it_mara-xmcng <> ' '.
          it_error-xmcng = it_mara-xmcng.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
                'Neg stk' INTO it_mara-remarks
                 SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN '3005'.
        IF it_mara-xmcng = ' '.
          it_error-xmcng = '*'.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
               'Neg stk' INTO it_mara-remarks
                SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        it_error-bklas = it_mara-bklas.
        it_mara-error = 'X'.
        it_mara-remarks = 'Valuation Class'.
        it_error-remarks = it_mara-remarks.
    ENDCASE.

** Stock removal strategry **
    CASE it_mara-werks.
      WHEN 'P001'.
        IF it_mara-dispo <> 'M02'.
          CASE it_mara-bklas.
            WHEN '3000' OR '3001'.
              IF it_mara-ltkza = ' '.
                it_error-ltkza = '*'.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                    'Stk rmvl stratgy' INTO it_mara-remarks
                     SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN '3005'.
              IF it_mara-ltkza <> ' '.
                it_error-ltkza = it_mara-ltkza.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                     'Stk rmvl stratgy' INTO it_mara-remarks
                      SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
                it_error-lgtyp = it_mara-lgtyp.
                IF it_mara-lgtyp = ' '.
                  it_error-lgtyp = '*'.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              it_error-bklas = it_mara-bklas.
              it_mara-error = 'X'.
              it_mara-remarks = 'Valuation Class'.
              it_error-remarks = it_mara-remarks.
          ENDCASE.
        ELSE.
          IF it_mara-ltkza <> ' '.
            it_error-ltkza = it_mara-ltkza.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & '
                 'Stk rmvl stratgy' INTO it_mara-remarks
                  SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
            it_error-remarks = it_mara-remarks.
            it_error-lgtyp = it_mara-lgtyp.
            IF it_mara-lgtyp = ' '.
              it_error-lgtyp = '*'.
            ENDIF.
          ENDIF.
        ENDIF.
** FOR E002
*   when 'E001'.
      WHEN 'E001' OR 'E002'.
** END
        IF it_mara-ltkza <> ' '.
          it_error-ltkza = it_mara-ltkza.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
               'Stk rmvl stratgy' INTO it_mara-remarks
                SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
          it_error-lgtyp = it_mara-lgtyp.
          IF it_mara-lgtyp = ' '.
            it_error-lgtyp = '*'.
          ENDIF.
        ENDIF.
    ENDCASE.

** Stock placement strategry **
    CASE it_mara-dispo.
      WHEN 'M02'.
        IF it_mara-ltkze <> ' '.
          it_error-ltkze = it_mara-ltkze.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
                  'Stk plcm stratgy' INTO it_mara-remarks
                  SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        IF it_mara-werks = 'P001'.
          IF ( it_mara-ltkze <> it_mara-ltkza ) OR
                 ( it_mara-ltkze = it_mara-ltkza AND it_error-ltkza <> ' ' ).
            it_error-ltkze = it_mara-ltkze.
            IF it_mara-ltkze = ' '.
              it_error-ltkze = '*'.
            ENDIF.
            it_mara-error = 'X'.
            CONCATENATE it_mara-remarks ' & '
                  'Stk plcm stratgy' INTO it_mara-remarks
                   SEPARATED BY ' '.
            it_error-remarks = it_mara-remarks.
          ENDIF.
        ELSE.
** FOR E002
*          IF it_mara-werks = 'E001'.
          IF it_mara-werks = 'E001' OR
             it_mara-werks = 'E002'.
** END E002
            IF it_mara-ltkze <> ' '.
              it_error-ltkze = it_mara-ltkze.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                     'Stk plcm stratgy' INTO it_mara-remarks
                     SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

** WM2 Storage Bin **
    CASE it_mara-dispo.
      WHEN 'M02'.
        IF it_mara-lgpla <> ' '.
          it_error-lgpla = it_mara-lgpla.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
                'WM2 Stor Bin' INTO it_mara-remarks
                 SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        IF it_mara-werks = 'P001'.
          CASE it_mara-bklas.
            WHEN '3000' OR '3001'.
              IF it_mara-lgpla = '  '.
                it_error-lgpla = '*'.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                     'WM2 Stor Bin' INTO it_mara-remarks
                      SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN '3005'.
              IF it_mara-lgpla <> ' '.
                it_error-lgpla = it_mara-lgpla.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                     'WM2 Stor Bin' INTO it_mara-remarks
                      SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN OTHERS.
              it_error-bklas = it_mara-bklas.
              it_mara-error = 'X'.
              it_mara-remarks = 'Valuation Class'.
              it_error-remarks = it_mara-remarks.
          ENDCASE.
        ELSE.
* FOR E002
*          IF it_mara-werks = 'E001'.
          IF it_mara-werks = 'E001' OR
             it_mara-werks = 'E002'.
** END E002
            IF it_mara-lgpla <> ' '.
              it_error-lgpla = it_mara-lgpla.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                   'WM2 Stor Bin' INTO it_mara-remarks
                    SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

** WM2 Rounding qty **
    CASE it_mara-dispo.
      WHEN 'M02'.
        IF it_mara-rdmng <> ' '.
          it_error-rdmng = it_mara-rdmng.
          it_mara-error = 'X'.
          CONCATENATE it_mara-remarks ' & '
                 'WM2 Rounding qty' INTO it_mara-remarks
                 SEPARATED BY ' '.
          it_error-remarks = it_mara-remarks.
        ENDIF.
      WHEN OTHERS.
        IF it_mara-werks = 'P001'.
          CASE it_mara-bklas.
            WHEN '3000' OR '3001'.
              IF it_mara-rdmng = 0.
                it_error-rdmng = 999999999.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                     'WM2 Rounding qty' INTO it_mara-remarks
                      SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN '3005'.
              IF it_mara-rdmng <> ' '.
                it_error-rdmng = it_mara-rdmng.
                it_mara-error = 'X'.
                CONCATENATE it_mara-remarks ' & '
                     'WM2 Rounding qty' INTO it_mara-remarks
                      SEPARATED BY ' '.
                it_error-remarks = it_mara-remarks.
              ENDIF.
            WHEN OTHERS.
              it_error-bklas = it_mara-bklas.
              it_mara-error = 'X'.
              it_mara-remarks = 'Valuation Class'.
              it_error-remarks = it_mara-remarks.
          ENDCASE.
        ELSE.
* FOR E002
*          IF it_mara-werks = 'E001'.
          IF it_mara-werks = 'E001' OR
             it_mara-werks = 'E002'.
** END E002
            IF it_mara-rdmng <> ' '.
              it_error-rdmng = it_mara-rdmng.
              it_mara-error = 'X'.
              CONCATENATE it_mara-remarks ' & '
                   'WM2 Rounding qty' INTO it_mara-remarks
                    SEPARATED BY ' '.
              it_error-remarks = it_mara-remarks.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

    IF it_mara-error = 'X'.
      it_error-matnr = it_mara-matnr.
      it_error-werks = it_mara-werks.
      it_error-lgort = it_mara-lgort.
      APPEND it_error.
      MODIFY it_mara.
    ENDIF.
    CLEAR it_error.
    CLEAR it_mara.
  ENDLOOP.

  SORT it_error BY matnr werks lgort.

  LOOP AT it_mara.
    MOVE-CORRESPONDING it_mara TO it_main.
    APPEND it_main.
    CLEAR it_main.
  ENDLOOP.

  IF p_all = 'X'.
    PERFORM disp_all.
  ELSE.
    PERFORM disp_error.
  ENDIF.
ENDFORM.                    "process_data

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'MAIN100'.
  SET TITLEBAR 'T_100'.
  PERFORM build_fieldcat.
  PERFORM build_layout.
  APPEND LINES OF it_fieldcat1 TO it_fieldcat.
  g_repid = sy-repid.
  gs_variant-report = g_repid.

*  PERFORM build_top_of_page.
*  PERFORM build_sort.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.
    CALL METHOD grid1->set_table_for_first_display
*         EXPORTING I_STRUCTURE_NAME = 'ZMMMATCHECK'
          EXPORTING is_layout = is_layout
                    i_save                   = 'A'
                    is_variant               = gs_variant
          CHANGING it_fieldcatalog = it_fieldcat
          it_outtab = it_disp1.
  ELSE.
    CALL METHOD grid1->refresh_table_display.
  ENDIF.
ENDMODULE.                    "PBO OUTPUT
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                    "PAI INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code .
    WHEN 'CHANGE'.
      PERFORM change_master.
    WHEN 'DISPLAY'.
      PERFORM display_master.
    WHEN 'REFRESH'.
      PERFORM refresh_master.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT


*&---------------------------------------------------------------------*
*&      Form  disp_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM disp_all.
  LOOP AT it_main.
    REFRESH it_color.
    READ TABLE it_error WITH KEY matnr = it_main-matnr
                                 werks = it_main-werks
                                 lgort = it_main-lgort.
    IF sy-subrc = 0.
      IF it_error-mstae <> ' '.
        PERFORM build_color_all USING it_main-mstae 'MSTAE'.
      ENDIF.
      IF it_error-matkl <> ' '.
        PERFORM build_color_all USING it_main-matkl 'MATKL'.
      ENDIF.
      IF it_error-mstde = '99999999'.
        PERFORM build_color_all USING it_main-mstde 'MSTDE'.
      ENDIF.
      IF it_error-brgew <> 0.
        PERFORM build_color_all USING it_main-brgew 'BRGEW'.
      ENDIF.
      IF it_error-ntgew <> 0.
        PERFORM build_color_all USING it_main-ntgew 'NTGEW'.
      ENDIF.
      IF it_error-gewei <> ' '.
        PERFORM build_color_all USING it_main-gewei 'GEWEI'.
      ENDIF.
      IF it_error-profl <> ' '.
        PERFORM build_color_all USING it_main-profl 'PROFL'.
      ENDIF.
      IF it_error-ferth <> ' '.
        PERFORM build_color_all USING it_main-ferth 'FERTH'.
      ENDIF.
      IF it_error-ekgrp <> ' '.
        PERFORM build_color_all USING it_main-ekgrp 'EKGRP'.
      ENDIF.
      IF it_error-mmsta <> ' '.
        PERFORM build_color_all USING it_main-mmsta 'MMSTA'.
      ENDIF.
      IF it_error-kordb <> ' '.
        PERFORM build_color_all USING it_main-kordb 'KORDB'.
      ENDIF.
      IF it_error-fabkz <> ' '.
        PERFORM build_color_all USING it_main-fabkz 'FABKZ'.
      ENDIF.
      IF it_error-stawn <> ' '.
        PERFORM build_color_all USING it_main-stawn 'STAWN'.
      ENDIF.
      IF it_error-herkl <> ' '.
        PERFORM build_color_all USING it_main-herkl 'HERKL'.
      ENDIF.
      IF it_error-dismm <> ' '.
        PERFORM build_color_all USING it_main-dismm 'DISMM'.
      ENDIF.
      IF it_error-dispo <> ' '.
        PERFORM build_color_all USING it_main-dispo 'DISPO'.
      ENDIF.
      IF it_error-disls <> ' '.
        PERFORM build_color_all USING it_main-disls 'DISLS'.
      ENDIF.
      IF it_error-bstmi <> 0.
        PERFORM build_color_all USING it_main-bstmi 'BSTMI'.
      ENDIF.
      IF it_error-bstma <> 0.
        PERFORM build_color_all USING it_main-bstma 'BSTMA'.
      ENDIF.
      IF it_error-bstrf <> 0.
        PERFORM build_color_all USING it_main-bstrf 'BSTRF'.
      ENDIF.
      IF it_error-beskz <> ' '.
        PERFORM build_color_all USING it_main-beskz 'BESKZ'.
      ENDIF.
      IF it_error-sobsl <> ' '.
        PERFORM build_color_all USING it_main-sobsl 'SOBSL'.
      ENDIF.
      IF it_error-rgekz <> ' '.
        PERFORM build_color_all USING it_main-rgekz 'RGEKZ'.
      ENDIF.
      IF it_error-lgpro <> ' '.
        PERFORM build_color_all USING it_main-lgpro 'LGPRO'.
      ENDIF.
      IF it_error-vspvb <> ' '.
        PERFORM build_color_all USING it_main-vspvb 'VSPVB'.
      ENDIF.
      IF it_error-lgfsb <> ' '.
        PERFORM build_color_all USING it_main-lgfsb 'LGFSB'.
      ENDIF.
      IF it_error-fhori <> ' '.
        PERFORM build_color_all USING it_main-fhori 'FHORI'.
      ENDIF.
      IF it_error-plifz <> 0.
        PERFORM build_color_all USING it_main-plifz 'PLIFZ'.
      ENDIF.
      IF it_error-mrppp <> ' '.
        PERFORM build_color_all USING it_main-mrppp 'MRPPP'.
      ENDIF.
      IF it_error-eisbe <> 0.
        PERFORM build_color_all USING it_main-eisbe 'EISBE'.
      ENDIF.
      IF it_error-ausdt = '99999999'.
        PERFORM build_color_all USING it_main-ausdt 'AUSDT'.
      ENDIF.
      IF it_error-lgpbe <> ' '.
        PERFORM build_color_all USING it_main-lgpbe 'LGPBE'.
      ENDIF.
      IF it_error-tempb <> ' '.
        PERFORM build_color_all USING it_main-tempb 'TEMPB'.
      ENDIF.
      IF it_error-abcin <> ' '.
        PERFORM build_color_all USING it_main-abcin 'ABCIN'.
      ENDIF.
      IF it_error-raube <> ' '.
        PERFORM build_color_all USING it_main-raube 'RAUBE'.
      ENDIF.
      IF it_error-xmcng <> ' '.
        PERFORM build_color_all USING it_main-xmcng 'XMCNG'.
      ENDIF.
      IF it_error-bklas <> ' '.
        PERFORM build_color_all USING it_main-bklas 'BKLAS'.
      ENDIF.
      IF it_error-stprs <> 0.
        PERFORM build_color_all USING it_main-stprs 'STPRS'.
      ENDIF.
      IF it_error-ltkza <> ' '.
        PERFORM build_color_all USING it_main-ltkza 'LTKZA'.
      ENDIF.
      IF it_error-ltkze <> ' '.
        PERFORM build_color_all USING it_main-ltkze 'LTKZE'.
      ENDIF.
      IF it_error-lgtyp <> ' '.
        PERFORM build_color_all USING it_main-lgtyp 'LGTYP'.
      ENDIF.
      IF it_error-lgpla <> ' '.
        PERFORM build_color_all USING it_main-lgpla 'LGPLA'.
      ENDIF.
      IF it_error-rdmng <> 0.
        PERFORM build_color_all USING it_main-rdmng 'RDMNG'.
      ENDIF.
      MOVE-CORRESPONDING it_main TO it_disp.
      it_disp-ct = it_color.
      APPEND it_disp TO it_disp1.
      CLEAR wa_color.
      CLEAR it_disp.
    ELSE.
      MOVE-CORRESPONDING it_main TO it_disp.
      APPEND it_disp TO it_disp1.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "disp_all

*&---------------------------------------------------------------------*
*&      Form  disp_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM disp_error.
  LOOP AT it_main.
    REFRESH it_color.
    READ TABLE it_error WITH KEY matnr = it_main-matnr
                                 werks = it_main-werks
                                 lgort = it_main-lgort.
    IF sy-subrc = 0.
      IF it_error-mstae <> ' '.
        PERFORM build_color_all USING it_main-mstae 'MSTAE'.
        it_error-mstae = it_main-mstae.
      ENDIF.
      IF it_error-matkl <> ' '.
        PERFORM build_color_all USING it_main-matkl 'MATKL'.
        it_error-matkl = it_main-matkl.
      ENDIF.
      IF it_error-mstde = '99999999'.
        PERFORM build_color_all USING it_main-mstde 'MSTDE'.
        it_error-mstde = it_main-mstde.
      ENDIF.
      IF it_error-brgew <> 0.
        PERFORM build_color_all USING it_main-brgew 'BRGEW'.
        it_error-brgew = it_main-brgew.
      ENDIF.
      IF it_error-ntgew <> 0.
        PERFORM build_color_all USING it_main-ntgew 'NETGEW'.
        it_error-ntgew = it_main-ntgew.
      ENDIF.
      IF it_error-gewei <> ' '.
        PERFORM build_color_all USING it_main-gewei 'GEWEI'.
        it_error-gewei = it_main-gewei.
      ENDIF.
      IF it_error-profl <> ' '.
        PERFORM build_color_all USING it_main-profl 'PROFL'.
        it_error-profl = it_main-profl.
      ENDIF.
      IF it_error-ferth <> ' '.
        PERFORM build_color_all USING it_main-ferth 'FERTH'.
        it_error-ferth = it_main-ferth.
      ENDIF.
      IF it_error-ekgrp <> ' '.
        PERFORM build_color_all USING it_main-ekgrp 'EKGRP'.
        it_error-ekgrp = it_main-ekgrp.
      ENDIF.
      IF it_error-mmsta <> ' '.
        PERFORM build_color_all USING it_main-mmsta 'MMSTA'.
        it_error-mmsta = it_main-mmsta.
      ENDIF.
      IF it_error-kordb <> ' '.
        PERFORM build_color_all USING it_main-kordb 'KORDB'.
        it_error-kordb = it_main-kordb.
      ENDIF.
      IF it_error-fabkz <> ' '.
        PERFORM build_color_all USING it_main-fabkz 'FABKZ'.
        it_error-fabkz = it_main-fabkz.
      ENDIF.
      IF it_error-stawn <> ' '.
        PERFORM build_color_all USING it_main-stawn 'STAWN'.
        it_error-stawn = it_main-stawn.
      ENDIF.
      IF it_error-herkl <> ' '.
        PERFORM build_color_all USING it_main-herkl 'HERKL'.
        it_error-herkl = it_main-herkl.
      ENDIF.
      IF it_error-dismm <> ' '.
        PERFORM build_color_all USING it_main-dismm 'DISMM'.
        it_error-dismm = it_main-dismm.
      ENDIF.
      IF it_error-dispo <> ' '.
        PERFORM build_color_all USING it_main-dispo 'DISPO'.
        it_error-dispo = it_main-dispo.
      ENDIF.
      IF it_error-disls <> ' '.
        PERFORM build_color_all USING it_main-disls 'DISLS'.
        it_error-disls = it_main-disls.
      ENDIF.
      IF it_error-bstmi <> 0.
        PERFORM build_color_all USING it_main-bstmi 'BSTMI'.
        it_error-bstmi = it_main-bstmi.
      ENDIF.
      IF it_error-bstma <> 0.
        PERFORM build_color_all USING it_main-bstma 'BSTMA'.
        it_error-bstma = it_main-bstma.
      ENDIF.
      IF it_error-bstrf <> 0.
        PERFORM build_color_all USING it_main-bstrf 'BSTRF'.
        it_error-bstrf = it_main-bstrf.
      ENDIF.
      IF it_error-beskz <> ' '.
        PERFORM build_color_all USING it_main-beskz 'BESKZ'.
        it_error-beskz = it_main-beskz.
      ENDIF.
      IF it_error-sobsl <> ' '.
        PERFORM build_color_all USING it_main-sobsl 'SOBSL'.
        it_error-sobsl = it_main-sobsl.
      ENDIF.
      IF it_error-rgekz <> ' '.
        PERFORM build_color_all USING it_main-rgekz 'RGEKZ'.
        it_error-rgekz = it_main-rgekz.
      ENDIF.
      IF it_error-lgpro <> ' '.
        PERFORM build_color_all USING it_main-lgpro 'LGPRO'.
        it_error-lgpro = it_main-lgpro.
      ENDIF.
      IF it_error-vspvb <> ' '.
        PERFORM build_color_all USING it_main-vspvb 'VSPVB'.
        it_error-vspvb = it_main-vspvb.
      ENDIF.
      IF it_error-lgfsb <> ' '.
        PERFORM build_color_all USING it_main-lgfsb 'LGFSB'.
        it_error-lgfsb = it_main-lgfsb.
      ENDIF.
      IF it_error-fhori <> ' '.
        PERFORM build_color_all USING it_main-fhori 'FHORI'.
        it_error-fhori = it_main-fhori.
      ENDIF.
      IF it_error-plifz <> 0.
        PERFORM build_color_all USING it_main-plifz 'PLIFZ'.
        it_error-plifz = it_main-plifz.
      ENDIF.
      IF it_error-mrppp <> ' '.
        PERFORM build_color_all USING it_main-mrppp 'MRPPP'.
        it_error-mrppp = it_main-mrppp.
      ENDIF.
      IF it_error-eisbe <> 0.
        PERFORM build_color_all USING it_main-eisbe 'EISBE'.
        it_error-eisbe = it_main-eisbe.
      ENDIF.
      IF it_error-ausdt = '99999999'.
        PERFORM build_color_all USING it_main-ausdt 'AUSDT'.
        it_error-ausdt = it_main-ausdt.
      ENDIF.
      IF it_error-lgpbe <> ' '.
        PERFORM build_color_all USING it_main-lgpbe 'LGPBE'.
        it_error-lgpbe = it_main-lgpbe.
      ENDIF.
      IF it_error-tempb <> ' '.
        PERFORM build_color_all USING it_main-tempb 'TEMPB'.
        it_error-tempb = it_main-tempb.
      ENDIF.
      IF it_error-abcin <> ' '.
        PERFORM build_color_all USING it_main-abcin 'ABCIN'.
        it_error-abcin = it_main-abcin.
      ENDIF.
      IF it_error-raube <> ' '.
        PERFORM build_color_all USING it_main-raube 'RAUBE'.
        it_error-raube = it_main-raube.
      ENDIF.
      IF it_error-xmcng <> ' '.
        PERFORM build_color_all USING it_main-xmcng 'XMCNG'.
        it_error-xmcng = it_main-xmcng.
      ENDIF.
      IF it_error-bklas <> ' '.
        PERFORM build_color_all USING it_main-bklas 'BKLAS'.
        it_error-bklas = it_main-bklas.
      ENDIF.
      IF it_error-stprs <> 0.
        PERFORM build_color_all USING it_main-stprs 'STPRS'.
        it_error-stprs = it_main-stprs.
      ENDIF.
      IF it_error-ltkza <> ' '.
        PERFORM build_color_all USING it_main-ltkza 'LTKZA'.
        it_error-ltkza = it_main-ltkza .
      ENDIF.
      IF it_error-ltkze <> ' '.
        PERFORM build_color_all USING it_main-ltkze 'LTKZE'.
        it_error-ltkze = it_main-ltkze.
      ENDIF.
      IF it_error-lgtyp <> ' '.
        PERFORM build_color_all USING it_main-lgtyp 'LGTYP'.
        it_error-lgtyp = it_main-lgtyp.
      ENDIF.
      IF it_error-lgpla <> ' '.
        PERFORM build_color_all USING it_main-lgpla 'LGPLA'.
        it_error-lgpla = it_main-lgpla.
      ENDIF.
      IF it_error-rdmng <> 0.
        PERFORM build_color_all USING it_main-rdmng 'RDMNG'.
        it_error-rdmng = it_main-rdmng.
      ENDIF.
      MOVE-CORRESPONDING it_error TO it_disp.
      it_disp-ct = it_color.
      APPEND it_disp TO it_disp1.
      CLEAR wa_color.
      CLEAR it_disp.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "disp_error

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcat.

  CLEAR : it_fieldcat1.
  REFRESH : it_fieldcat1.

  CHECK it_fieldcat1[] IS INITIAL.

*PERFORM append_fieldcat USING  p_fieldname
*                               p_tabname
*                               p_outputlen
*                               p_text_l
*                               p_text_m
*                               p_text_s
*                               p_datatype
*                               p_key
*                               p_no_out
*                               p_unit
*                               p_currency
*                               p_text_field.

  PERFORM append_fieldcat USING 'MATNR'
                                'IT_DISP'
                                18
                                'Material No'
                                'Material No'
                                'Material No'
                                'CHAR'
                                'X'
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'MAKTX'
                                'IT_DISP'
                                40
                                'Description'
                                'Description'
                                'Description'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'WERKS'
                                'IT_DISP'
                                4
                                'Plant'
                                'Plant'
                                'Plant'
                                'NUMC'
                                ' '
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'LGORT'
                                'IT_DISP'
                                4
                                'Storage'
                                'Storage.'
                                'Stor.'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'MSTAE'
                                'IT_DISP'
                                2
                                'X-Plant STATUS'
                                'X-Plant STATUS'
                                'X-Plant ST'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'MATKL'
                                'IT_DISP'
                                9
                                'Material Group'
                                'Material Group'
                                'Matl GRP'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  IF p_mstde = 'X'.
    PERFORM append_fieldcat USING 'MSTDE'
                                 'IT_DISP'
                                 8
                                 'Valid From'
                                 'Valid From'
                                 'Valid From'
                                 'DATS'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.
  ELSE.
    PERFORM append_fieldcat USING 'MSTDE'
                                  'IT_DISP'
                                  8
                                  'Valid From'
                                  'Valid From'
                                  'Valid From'
                                  'DATS'
                                  ' '
                                  ''
                                  'X'
                                  ''
                                  ''.
  ENDIF.
  PERFORM append_fieldcat USING 'BRGEW'
                                 'IT_DISP'
                                 13
                                 'Gross Weight'
                                 'Gross Weight'
                                 'Gross Weight'
                                 'QUAN'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.

  PERFORM append_fieldcat USING 'NTGEW'
                                'IT_DISP'
                                13
                                'Net Weight'
                                'Net Weight'
                                'Net Weight'
                                'QUAN'
                                ''
                                ''
                                ''
                                'X'
                                ''.

  PERFORM append_fieldcat USING 'GEWEI'
                                'IT_DISP'
                                3
                                'Weight Unit'
                                'Weight Unit'
                                'UoM'
                                'UNIT'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'PROFL'
                                'IT_DISP'
                                3
                                'Source'
                                'Source'
                                'Source'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  IF p_ferth = 'X'.
    PERFORM append_fieldcat USING 'FERTH'
                                  'IT_DISP'
                                  18
                                  'Prod./Insp. Memo'
                                  'Prod./Insp. Memo'
                                  'Prod./Insp. Memo'
                                  'CHAR'
                                  ''
                                  ''
                                  ''
                                  ''
                                  ''.
  ELSE.
    PERFORM append_fieldcat USING 'FERTH'
                                   'IT_DISP'
                                   18
                                   'Prod./Insp. Memo'
                                   'Prod./Insp. Memo'
                                   'Prod./Insp. Memo'
                                   'CHAR'
                                   ''
                                   ''
                                   'X'
                                   ''
                                   ''.

  ENDIF.
  PERFORM append_fieldcat USING 'EKGRP'
                                 'IT_DISP'
                                 3
                                 'Purchase Group'
                                 'Purchase Group'
                                 'Pur Grp'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'MMSTA'
                                'IT_DISP'
                                2
                                'Plant-sp. matl status'
                                'Plant-sp. matl status'
                                'Plant-sp. matl st'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'KORDB'
                                'IT_DISP'
                                1
                                'Source list'
                                'Source list'
                                'S list'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'FABKZ'
                                'IT_DISP'
                                1
                                'JIT Del Indicator'
                                'JIT Del Indicator'
                                'JIT'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.
  PERFORM append_fieldcat USING 'STAWN'
                                'IT_DISP'
                                17
                                'Comm./Imp.Code'
                                'Comm./Imp.Code'
                                'Comm./Imp.Code'
                                'CHAR'
                                ''
                                ''
                                ''
                                ''
                                ''.

  PERFORM append_fieldcat USING 'HERKL'
                                 'IT_DISP'
                                 3
                                 'Country'
                                 'Country'
                                 'Country'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'DISMM'
                                 'IT_DISP'
                                 2
                                 'MRP Type'
                                 'MRP Type'
                                 'MRP Type'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.
  PERFORM append_fieldcat USING 'DISPO'
                                 'IT_DISP'
                                 3
                                 'MRP Controller'
                                 'MRP Controller'
                                 'MRP Ctrler'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'DISLS'
                                 'IT_DISP'
                                 2
                                 'Lot Size'
                                 'Lot Size'
                                 'Lot Size'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'BSTMI'
                                'IT_DISP'
                                13
                                'Minimum lot size'
                                'Minimum lot size'
                                'Min lot size'
                                'QUAN'
                                ''
                                ''
                                ''
                                'X'
                                ''.
  IF p_bstma = 'X'.
    PERFORM append_fieldcat USING 'BSTMA'
                                 'IT_DISP'
                                 13
                                 'Maximum lot size'
                                 'Maximum lot size'
                                 'Max lot size'
                                 'QUAN'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.
  ELSE.
    PERFORM append_fieldcat USING 'BSTMA'
                                 'IT_DISP'
                                 13
                                 'Maximum lot size'
                                 'Maximum lot size'
                                 'Max lot size'
                                 'QUAN'
                                 ''
                                 ''
                                 'X'
                                 'X'
                                 ''.
  ENDIF.

  PERFORM append_fieldcat USING 'BSTRF'
                                'IT_DISP'
                                13
                                'Round Value'
                                'Round Value'
                                'Round Value'
                                'QUAN'
                                ''
                                ''
                                ''
                                'X'
                                ''.

  PERFORM append_fieldcat USING 'BESKZ'
                                 'IT_DISP'
                                 1
                                 'Purcurement Type'
                                 'Purcurement Type'
                                 'Pur Type'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  IF p_sobsl = 'X'.
    PERFORM append_fieldcat USING 'SOBSL'
                                    'IT_DISP'
                                    2
                                    'Special Purcurement Type'
                                    'Spec Proc Type'
                                    'Spec Pur Type'
                                    'CHAR'
                                    ''
                                    ''
                                    ''
                                    ''
                                    ''.
  ELSE.
    PERFORM append_fieldcat USING 'SOBSL'
                                    'IT_DISP'
                                    2
                                    'Special Purcurement Type'
                                    'Spec Proc Type'
                                    'Spec Pur Type'
                                    'CHAR'
                                    ''
                                    ''
                                    'X'
                                    ''
                                    ''.

  ENDIF.
  PERFORM append_fieldcat USING 'RGEKZ'
                                 'IT_DISP'
                                 1
                                 'Backflush'
                                 'Backflush'
                                 'Backflush'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'LGPRO'
                                 'IT_DISP'
                                 4
                                 'Issue Stor.Location'
                                 'Iss.Stor.Loca'
                                 'Iss Stor Loca'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'VSPVB'
                                 'IT_DISP'
                                 10
                                 'Default Supply Area'
                                 'Default Supp Area'
                                 'D Supp Area'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'LGFSB'
                                 'IT_DISP'
                                 4
                                 'Storage Location for EP'
                                 'Storage Loca for EP'
                                 'Stor Loc EP'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  IF p_fhori = 'X'.
    PERFORM append_fieldcat USING 'FHORI'
                                   'IT_DISP'
                                   3
                                   'Sched Margin Key'
                                   'Sched Margin Key'
                                   'Sched Margin Key'
                                   'CHAR'
                                   ''
                                   ''
                                   ''
                                   ''
                                   ''.
  ELSE.
    PERFORM append_fieldcat USING 'FHORI'
                                   'IT_DISP'
                                   3
                                   'Sched Margin Key'
                                   'Sched Margin Key'
                                   'Sched Margin Key'
                                   'CHAR'
                                   ''
                                   ''
                                   'X'
                                   ''
                                   ''.
  ENDIF.

  PERFORM append_fieldcat USING 'PLIFZ'
                                 'IT_DISP'
                                 3
                                 'Plnd Delivery Time'
                                 'Plnd Delivery Time'
                                 'Plnd D Time'
                                 'DEC'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.

  PERFORM append_fieldcat USING 'MRPPP'
                                'IT_DISP'
                                 3
                                 'Planning Calendar'
                                 'Planning Calendar'
                                 'Planning Cal.'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'EISBE'
                                'IT_DISP'
                                 13
                                 'Safety Stock'
                                 'Safety Stock'
                                 'Sfy Stock'
                                 'QUAN'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.

  IF p_ausdt = 'X'.
    PERFORM append_fieldcat USING 'AUSDT'
                                 'IT_DISP'
                                  8
                                  'Effective Out'
                                  'Effective Out'
                                  'Eff Out'
                                  'DATS'
                                  ''
                                  ''
                                  ''
                                  'X'
                                  ''.
  ELSE.
    PERFORM append_fieldcat USING 'AUSDT'
                                 'IT_DISP'
                                  8
                                  'Effective Out'
                                  'Effective Out'
                                  'Eff Out'
                                  'DATS'
                                  ''
                                  ''
                                  'X'
                                  'X'
                                  ''.
  ENDIF.

  PERFORM append_fieldcat USING 'LGPBE'
                                'IT_DISP'
                                 10
                                 'Storage Bin'
                                 'Storage Bin'
                                 'Storage Bin'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'TEMPB'
                                'IT_DISP'
                                 2
                                 'Backflush Cycle'
                                 'Backflush Cycle'
                                 'Bf Cyc'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  IF p_abcin = 'X'.
    PERFORM append_fieldcat USING 'ABCIN'
                                  'IT_DISP'
                                   1
                                   'Cycle Count'
                                   'Cycle Count'
                                   'Cyc Count'
                                   'CHAR'
                                   ''
                                   ''
                                   ''
                                   ''
                                   ''.
  ELSE.
    PERFORM append_fieldcat USING 'ABCIN'
                                  'IT_DISP'
                                   1
                                   'Cycle Count'
                                   'Cycle Count'
                                   'Cyc Count'
                                   'CHAR'
                                   ''
                                   ''
                                   'X'
                                   ''
                                   ''.
  ENDIF.
  PERFORM append_fieldcat USING 'RAUBE'
                                'IT_DISP'
                                 2
                                 'Shop'
                                 'Shop'
                                 'Shop'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'XMCNG'
                                'IT_DISP'
                                 1
                                 'Negtive Stocks'
                                 'Negtive Stocks'
                                 'Neg Stocks'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'BKLAS'
                                'IT_DISP'
                                 4
                                 'Valuation Class'
                                 'Valuation Class'
                                 'Val.Class'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'STPRS'
                                'IT_DISP'
                                 11
                                 'Standard Price'
                                 'Standard Price'
                                 'Standard Price'
                                 'CURR'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.


  PERFORM append_fieldcat USING 'LTKZA'
                                'IT_DISP'
                                 3
                                 'Stock Removal Strategy'
                                 'Stock Removal Strategy'
                                 'Stk Rem S'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'LTKZE'
                                'IT_DISP'
                                 3
                                 'Stock Placement Strategy'
                                 'Stock Placement Strategy'
                                 'Stk Pl S'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'LGTYP'
                                'IT_DISP'
                                 3
                                 'WM Storage Type'
                                 'WM Storage Type'
                                 'Stor. Type'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'LGPLA'
                                'IT_DISP'
                                 10
                                 'WM Storage Bin'
                                 'WM Storage Bin'
                                 'Stor. Bin'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

  PERFORM append_fieldcat USING 'RDMNG'
                                'IT_DISP'
                                 13
                                 'Rounding Quantity'
                                 'Rounding Qty'
                                 'Rounding Qty'
                                 'QUAN'
                                 ''
                                 ''
                                 ''
                                 'X'
                                 ''.

  PERFORM append_fieldcat USING 'ERROR'
                                'IT_DISP'
                                 1
                                 'Error'
                                 'Error'
                                 'Error'
                                 'CHAR'
                                 ''
                                 ''
                                 ''
                                 ''
                                 ''.

* PERFORM append_fieldcat USING 'REMARKS'
*                               'IT_DISP'
*                                255
*                                'Where'
*                                'Where'
*                                'Where'
*                                'CHAR'
*                                ''
*                                ''
*                                ''
*                                ''
*                                ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDNAME   text
*      -->P_TABNAME     text
*      -->P_OUTPUTLEN   text
*      -->P_TEXT_L      text
*      -->P_TEXT_M      text
*      -->P_TEXT_S      text
*      -->P_DATATYPE    text
*      -->P_KEY         text
*      -->P_KEY_SEL     text
*      -->P_NO_OUT      text
*      -->P_NO_ZERO     text
*      -->P_TEXT_FIELD  text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_tabname
                              p_outputlen
                              p_text_l
                              p_text_m
                              p_text_s
                              p_datatype
                              p_key
                              p_key_sel
                              p_no_out
                              p_no_zero
                              p_text_field.
  it_fieldcat1-fieldname      = p_fieldname.
  it_fieldcat1-tabname        = p_tabname.
  it_fieldcat1-outputlen      = p_outputlen.
  it_fieldcat1-scrtext_l      = p_text_l.
  it_fieldcat1-scrtext_m      = p_text_m.
  it_fieldcat1-scrtext_s      = p_text_s.
  it_fieldcat1-datatype       = p_datatype.
  it_fieldcat1-key            = p_key.
  it_fieldcat1-key_sel        = p_key_sel.
  it_fieldcat1-no_out         = p_no_out.
  it_fieldcat1-no_zero        = p_no_zero.
*  it_fieldcat1-text_fieldname = p_text_field.
  APPEND it_fieldcat1. CLEAR it_fieldcat1.
ENDFORM.                    " append_fieldcat1

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_layout.
  is_layout-zebra = 'X'.
  is_layout-ctab_fname = 'CT'.
  is_layout-grid_title = 'Material Master Validation'.
ENDFORM.                    "BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->P_FNAME    text
*----------------------------------------------------------------------*
FORM build_color_all USING p_field p_fname.
  wa_color-color-col = 6.
  wa_color-color-int = 1.
  wa_color-fname = p_fname.
  APPEND wa_color TO it_color.
  CLEAR wa_color.
ENDFORM.                    "BUILD_COLOR_ALL

*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->P_FNAME    text
*----------------------------------------------------------------------*
FORM build_color_char USING p_field p_fname.
  IF p_field <> ' '.
    wa_color-color-col = 6.
    wa_color-color-int = 1.
    wa_color-fname = p_fname.
    APPEND wa_color TO it_color.
    CLEAR wa_color.
  ENDIF.
ENDFORM.                    "BUILD_COLOR_CHAR

*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->P_FNAME    text
*----------------------------------------------------------------------*
FORM build_color_num USING p_field p_fname.
  IF p_field <> 0.
    wa_color-color-col = 6.
    wa_color-color-int = 1.
    wa_color-fname = p_fname.
    APPEND wa_color TO it_color.
    CLEAR wa_color.
  ENDIF.
ENDFORM.                    "BUILD_COLOR_NUM

*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_DAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->P_FNAME    text
*----------------------------------------------------------------------*
FORM build_color_dat USING p_field p_fname.
  IF p_field = '99999999'.
    wa_color-color-col = 6.
    wa_color-color-int = 1.
    wa_color-fname = p_fname.
    APPEND wa_color TO it_color.
    CLEAR wa_color.
  ENDIF.
ENDFORM.                    "BUILD_COLOR_DAT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_master.
  DATA : wa_row_table LIKE LINE OF row_table.
  CLEAR : wa_disp.
  CLEAR wa_row_table.

  READ TABLE it_disp1 INTO wa_disp INDEX sy-tabix.
  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = row_table[].
* only one selected row!
  READ TABLE row_table INTO wa_row_table INDEX 1.
  IF sy-subrc = 0.
    READ TABLE it_disp1 INTO wa_disp INDEX wa_row_table-index.
    IF sy-subrc = 0.
      SET PARAMETER ID 'MAT' FIELD wa_disp-matnr.
      CALL TRANSACTION 'MM02' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.
*    MESSAGE I181.
  ENDIF.

ENDFORM.                    " CHANGE_MASTER


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_master.
  DATA : wa_row_table LIKE LINE OF row_table.
  CLEAR : wa_disp.
  CLEAR wa_row_table.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = row_table[].
* only one selected row!
  READ TABLE row_table INTO wa_row_table INDEX 1.
  IF sy-subrc = 0.
    READ TABLE it_disp1 INTO wa_disp INDEX wa_row_table-index.
    IF sy-subrc = 0.
      SET PARAMETER ID 'MAT' FIELD wa_disp-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ENDIF.
  ELSE.
    MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '181'.
  ENDIF.
ENDFORM.                    "DISPLAY_MASTER

*&---------------------------------------------------------------------*
*&      Form  DATA_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_initialization.
  CLEAR: it_disp, it_disp[], it_mara, it_mara[], it_main, it_main[],
         it_mard, it_mard[].

  CLEAR: it_error, it_error[], it_mara1, it_mara1[],
         it_mara2, it_mara2[], wa_disp, wa_disp[],
         it_disp1, it_disp1[].

  CLEAR: m_nlines, m_werks, m_remarks.
ENDFORM.                    " DATA_INITIALIZATION


*&---------------------------------------------------------------------*
*&      Form  REFRESH_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_master.
  PERFORM data_initialization.
  PERFORM process_data.
ENDFORM.                    "REFRESH_MASTER
