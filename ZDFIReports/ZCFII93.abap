************************************************************************
* Program Name      : zcfii93
* Author            : WSKIM
* Creation Date     : 2005.01.07.
* Specifications By : YCY
* Pattern           :
* Development Request No:
* Addl Documentation:
* Description       :
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zcfii93  NO STANDARD PAGE HEADING
                 LINE-SIZE  1023  LINE-COUNT 65
                 MESSAGE-ID zmfi.
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: v_anepk,cobrb,anlz,anla,anlb,usr01.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF iexcel OCCURS 0.
        INCLUDE STRUCTURE alsmex_tabline.
DATA: END OF iexcel.
DATA: zwfeld(19).
FIELD-SYMBOLS: <fs1>.
DATA: BEGIN OF data_tab,
       value_0001(50),
       value_0002(50),
       value_0003(50),
       value_0004(50),
       value_0005(50),
       value_0006(50),
       value_0007(50),
       value_0008(50),
       value_0009(50),
       value_0010(50).
DATA: END OF data_tab.


DATA : BEGIN OF it_data OCCURS 0,
       aufnr LIKE aufk-aufnr,    "internal order
       empge LIKE dkobr-empge,   "General settlement receiver
       prozs LIKE cobrb-prozs ,  "Settlement percentage rate
       aqzif(10)," LIKE cobrb-aqzif,   "Equivalence number for order
       betrr LIKE cobrb-betrr,   "Amount for amount rule
       kostl LIKE anlz-kostl,    "Cost Center
       kostlv LIKE anlz-kostlv,  "Cost center responsible for asset
       izwek LIKE anla-izwek,    "Reason for investment
       ord41 LIKE anla-ord41,
       eigkz LIKE anla-eigkz,
       result(2),
       END OF it_data.

DATA : it_order LIKE it_data OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_spi OCCURS 0,
        anln1 LIKE raifp2-anln1,
        anln2 LIKE raifp2-anln2,
        bldat(10)," LIKE raifp1-bldat,
        sgtxt LIKE raifp2-sgtxt,
        anln3 LIKE raifp2-anln1,
        anln4 LIKE raifp2-anln2,
        monat LIKE raifp2-monat,
        anbtr(10)," LIKE raifp2-anbtr,
        prozs(10)," LIKE raifp2-prozs,
        ind,
        result(2),
       END OF it_spi.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.

DATA : BEGIN OF it_anepk OCCURS 0,
        anln1 LIKE v_anepk-anln1,
        bukrs LIKE v_anepk-bukrs,
        gjahr LIKE v_anepk-gjahr,
        afabe LIKE v_anepk-afabe,
        belnr LIKE v_anepk-belnr,
        awtyp LIKE v_anepk-awtyp,
        result(2),
       END OF it_anepk.
DATA : BEGIN OF it_sdat OCCURS 0,
        anln1 LIKE anla-anln1,
        aktiv LIKE anla-aktiv,
        result(2),
       END OF it_sdat.
*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA : tind   LIKE iexcel-col,
       p_file LIKE rlgrap-filename.


**----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-555.
*SELECTION-SCREEN BEGIN OF LINE.
PARAMETER : p_acl RADIOBUTTON GROUP r1.
* SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP 1.
PARAMETER : p_ord RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP 1.

PARAMETER:  p_cap RADIOBUTTON GROUP r1.
SELECT-OPTIONS: s_anln1 FOR anla-anln1.
SELECTION-SCREEN SKIP 1.

PARAMETER : p_spi RADIOBUTTON GROUP r1.
SELECTION-SCREEN SKIP 1.

PARAMETER : p_sdat RADIOBUTTON GROUP r1.
SELECT-OPTIONS: s_anln2 FOR anla-anln1 NO INTERVALS,
                s_aktiv FOR anla-aktiv NO INTERVALS NO-EXTENSION,
                s_afabg FOR anlb-afabg NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-555.
PARAMETER : c_mode TYPE c DEFAULT 'N' .
SELECTION-SCREEN END OF BLOCK b3.

*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  IF p_acl EQ 'X' OR p_ord EQ 'X'.
*GET FILE
    PERFORM file_upload.
    PERFORM asset_update.
  ELSEIF p_cap EQ 'X'.
    PERFORM cap_date_update.
  ELSEIF p_spi EQ 'X'.
    PERFORM file_upload.
    PERFORM asset_split.
  ELSEIF  p_sdat EQ 'X'.
    IF s_afabg-low IS INITIAL.
      MESSAGE i010 WITH 'start date '.
      STOP.
    ELSEIF s_anln2-low IS INITIAL.
      MESSAGE i010 WITH 'Asset number'.
      STOP.
    ELSEIF s_aktiv-low IS INITIAL.
      MESSAGE i010 WITH 'Capitalized on date'.
      STOP.
    ELSE.
      PERFORM cap_date_update_direct.
    ENDIF.
  ENDIF.

END-OF-SELECTION.
  PERFORM write_list.
*&---------------------------------------------------------------------*
*&      Form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_upload.
  REFRESH :iexcel,it_data.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.xls.'
            mode             = 'O'
            title            = 'PC File'
       IMPORTING
            filename         = p_file
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.

  IF sy-subrc = 1.  MESSAGE w011 WITH 'ERROR'.  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 1
            i_end_col               = 100
            i_end_row               = 30000
       TABLES
            intern                  = iexcel
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.
  IF sy-subrc <> 0.
    WRITE: / 'EXCEL UPLOAD FAILED ', sy-subrc.
  ELSE.
    SORT iexcel BY row col.
    IF p_spi = 'X'.
      LOOP AT iexcel.
        IF iexcel-row = 1.
          CONTINUE.
        ENDIF.
        tind = iexcel-col.
        CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
        ASSIGN (zwfeld) TO <fs1>.
        <fs1> = iexcel-value.
        AT END OF row.
          it_spi-anln1  = data_tab-value_0001.
          it_spi-anln2  = data_tab-value_0002.
          it_spi-bldat  = data_tab-value_0003.
          it_spi-sgtxt  = data_tab-value_0004.
          it_spi-anln3  = data_tab-value_0005.
          it_spi-anln4  = data_tab-value_0006.
          it_spi-monat  = data_tab-value_0007.
          it_spi-anbtr  = data_tab-value_0008.
          it_spi-prozs  = data_tab-value_0009.
          it_spi-ind    = data_tab-value_0010.
          APPEND it_spi.
          CLEAR data_tab.
        ENDAT.
      ENDLOOP.
    ELSE.
      LOOP AT iexcel.
        IF iexcel-row = 1.
          CONTINUE.
        ENDIF.
        tind = iexcel-col.
        CONCATENATE 'DATA_TAB-VALUE_' tind INTO zwfeld.
        ASSIGN (zwfeld) TO <fs1>.
        <fs1> = iexcel-value.
        AT END OF row.
          it_data-aufnr   = data_tab-value_0001.
          it_data-empge   = data_tab-value_0002.
          it_data-prozs   = data_tab-value_0003.
          it_data-aqzif   = data_tab-value_0004.
          it_data-betrr   = data_tab-value_0005.
          it_data-kostl   = data_tab-value_0006.
          it_data-kostlv  = data_tab-value_0007.
          it_data-izwek   = data_tab-value_0008.
          it_data-eigkz   = data_tab-value_0009.

          APPEND it_data.
          CLEAR data_tab.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " file_upload
*&---------------------------------------------------------------------*
*&      Form  ASSET_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM asset_update.
  IF  p_acl EQ 'X'.
    PERFORM asset_class_update.
  ELSEIF p_ord EQ 'X'.
    PERFORM asset_settlement_update.
  ENDIF.
ENDFORM.                    " ASSET_UPDATE
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0328   text
*      -->P_0329   text
*      -->P_0330   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  ASSET_CLASS_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM asset_class_update.
  DATA : g_empge LIKE anlz-anln1.
  LOOP AT it_data.
    REFRESH :it_bdc,it_message.CLEAR it_bdc.
    PERFORM dynpro USING:
           'X' 'SAPLAIST'        '0100',
           ' ' 'BDC_OKCODE'      '/00',
           ' ' 'ANLA-ANLN1'      it_data-empge, "ASSET NO
           ' ' 'ANLA-ANLN2'      '0',
           ' ' 'ANLA-BUKRS'      'H201'.

    PERFORM dynpro USING:
        'X' 'SAPLAIST'        '1000',
        ' ' 'BDC_OKCODE'      '=TAB02'.

    PERFORM dynpro USING:
        'X' 'SAPLAIST'        '1000',
        ' ' 'BDC_OKCODE'      '=TAB03',
        ' ' 'ANLZ-KOSTL'      it_data-kostl, "
        ' ' 'ANLZ-KOSTLV'     it_data-kostlv.
*COst center check
    CLEAR g_empge.
    g_empge = it_data-empge.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = g_empge
         IMPORTING
              output = g_empge.
    SELECT SINGLE * FROM anlz
        WHERE bukrs EQ 'H201'
          AND anln1 EQ  g_empge.

    IF anlz-kostl <>  it_data-kostl.
      PERFORM dynpro USING:
       'X' 'SAPLAIST'        '3020',
       ' ' 'BDC_OKCODE'      '=YES'.
    ENDIF.

    PERFORM dynpro USING:
         'X' 'SAPLAIST'        '1000',
         ' ' 'BDC_OKCODE'      '=TAB04',
         ' ' 'ANLA-ORD41'      it_data-ord41,
         ' ' 'ANLA-IZWEK'      it_data-izwek. " REASON

    PERFORM dynpro USING:
        'X' 'SAPLAIST'        '1000',
        ' ' 'BDC_OKCODE'      '=TAB05',
        ' ' 'ANLA-EAUFN'      it_data-aufnr.

    PERFORM dynpro USING:
      'X' 'SAPLAIST'        '1000',
      ' ' 'BDC_OKCODE'      '=BUCH',
      ' ' 'ANLA-EIGKZ'      it_data-eigkz.

    CALL TRANSACTION 'AS02' USING it_bdc
                             MODE c_mode
                             UPDATE 'S'
                              MESSAGES INTO it_message.
    READ TABLE it_message WITH KEY msgspra = 'E'.
    IF sy-subrc = 0.
      it_anepk-result = 'E'.
    ELSE.
      it_anepk-result = 'S'.
    ENDIF.
    MODIFY it_data FROM it_data.
    IF it_data-result EQ 'S'.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ASSET_CLASS_UPDATE
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_list.
  IF  p_acl EQ 'X'.
    LOOP AT it_data.
      WRITE : / it_data-aufnr, it_data-empge,it_data-prozs,
                it_data-betrr CURRENCY 'USD', it_data-kostl,
                it_data-kostlv,it_data-izwek, it_data-result.
    ENDLOOP.
  ELSEIF p_ord EQ 'X'.
    LOOP AT it_order.
      WRITE : / it_order-aufnr, it_order-empge,it_order-prozs,
                it_order-betrr CURRENCY 'USD', it_order-kostl,
                it_order-kostlv,it_order-izwek, it_order-result.
    ENDLOOP.
  ELSEIF p_cap EQ 'X'." or p_sdat eq 'X'.
    LOOP AT it_anepk.
      WRITE : / it_anepk-bukrs, it_anepk-gjahr,it_anepk-afabe,
                it_anepk-belnr, it_anepk-awtyp,it_anepk-result.
    ENDLOOP.
  ELSEIF p_sdat EQ 'X' AND NOT s_afabg-low IS INITIAL.
    LOOP AT it_sdat.
      WRITE : / it_sdat-anln1, it_sdat-aktiv,it_sdat-result.
    ENDLOOP.
  ELSEIF p_spi EQ 'X'.
    LOOP AT it_spi.
      WRITE : / it_spi-anln1,it_spi-anln2,it_spi-bldat,
                it_spi-sgtxt,it_spi-anln3,it_spi-anln4,it_spi-monat,
                it_spi-anbtr,it_spi-prozs,it_spi-result.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  asset_settlement_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM asset_settlement_update.
  DATA  :z_count(2)  TYPE n,
         z_objnr(22).
  DATA : c_num(2) TYPE n,
        konty(15),
        empge(15),
        prozs(15),
        aqzif(15),
        output(12),
        w_int TYPE i.
  DATA :  g_anln1 LIKE cobrb-anln1.
  DATA : it_cobrb LIKE cobrb OCCURS 0 WITH HEADER LINE.

  REFRESH it_cobrb.

  CLEAR : z_count,z_objnr,konty,empge,prozs.

  SORT it_data BY  aufnr empge.
  it_order[] = it_data[].
  DELETE ADJACENT DUPLICATES FROM it_order COMPARING aufnr.

  LOOP AT it_order.
    REFRESH :it_bdc,it_message.CLEAR it_bdc.
    PERFORM dynpro USING:
           'X' 'SAPMKAUF'        '0110',
           ' ' 'BDC_OKCODE'      '/00',
           ' ' 'COAS-AUFNR'      it_order-aufnr. "ORDER NO

    PERFORM dynpro USING:
           'X' 'SAPMKAUF'        '0600',
           ' ' 'BDC_OKCODE'      '=ABVO'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = it_order-aufnr
         IMPORTING
              output = output.

    CONCATENATE 'OR' output INTO z_objnr.
    SELECT  * INTO TABLE it_cobrb
     FROM cobrb
       WHERE objnr EQ z_objnr.
    DESCRIBE TABLE it_cobrb LINES z_count.

    LOOP AT it_data WHERE aufnr EQ it_order-aufnr.

      g_anln1 = it_data-empge(8).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = g_anln1
           IMPORTING
                output = g_anln1.

      READ TABLE it_cobrb WITH KEY objnr = z_objnr
                                   perbz = 'GES'
                                   anln1 = g_anln1.

      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      IF z_count = 14.
        PERFORM dynpro USING:
            'X' 'SAPLKOBS'        '0130',
            ' ' 'BDC_OKCODE'      '=P+'.
        CLEAR :z_count.
        z_count = z_count + 1.
        CONCATENATE 'COBRB-KONTY(' z_count ')' INTO konty.
        CONCATENATE 'DKOBR-EMPGE(' z_count ')' INTO empge.
        CONCATENATE 'DKOBR-PROZS(' z_count ')' INTO prozs.
        CONCATENATE 'COBRB-AQZIF(' z_count ')' INTO aqzif.

        PERFORM dynpro USING:
            'X' 'SAPLKOBS'        '0130',
            ' ' 'BDC_OKCODE'      '/00',
            ' '  konty            'FXA'.
        PERFORM dynpro USING:
            ' '  empge             it_data-empge.
        IF it_data-prozs <> 0.
          PERFORM  dynpro USING:
              ' '  prozs           it_data-prozs.
        ENDIF.
        PERFORM dynpro USING:
            ' '  aqzif            it_data-aqzif.

        CLEAR :konty,empge,prozs.
      ELSE.
        z_count = z_count + 1.
        CONCATENATE 'COBRB-KONTY(' z_count ')' INTO konty.
        CONCATENATE 'DKOBR-EMPGE(' z_count ')' INTO empge.
        CONCATENATE 'DKOBR-PROZS(' z_count ')' INTO prozs.
        CONCATENATE 'COBRB-AQZIF(' z_count ')' INTO aqzif.

        PERFORM dynpro USING:
            'X' 'SAPLKOBS'        '0130',
            ' ' 'BDC_OKCODE'      '/00',
            ' '  konty            'FXA'.
        PERFORM dynpro USING:
            ' '  empge             it_data-empge.
        IF it_data-prozs <> 0.
          PERFORM  dynpro USING:
              ' '  prozs            it_data-prozs.
        ENDIF.
        PERFORM dynpro USING:
              ' '  aqzif            it_data-aqzif.

        CLEAR :konty,empge,prozs,aqzif.
      ENDIF.
      CLEAR it_data.
    ENDLOOP.
    PERFORM dynpro USING:
         'X' 'SAPLKOBS'        '0130',
         ' ' 'BDC_OKCODE'      '=SICH'.

    CALL TRANSACTION 'KO02' USING it_bdc
                             MODE c_mode
                             UPDATE 'S'
                              MESSAGES INTO it_message.
    READ TABLE it_message WITH KEY msgspra = 'E'.
    IF sy-subrc = 0.
      it_anepk-result = 'E'.
    ELSE.
      it_anepk-result = 'S'.
    ENDIF.
    MODIFY it_order FROM it_order.
    CLEAR it_order.
    IF it_order-result EQ 'S'.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " asset_settlement_update
*&---------------------------------------------------------------------*
*&      Form  CAP_DATE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cap_date_update.
  DATA : w_int TYPE i,
         z_belnr(10),
         z_awkey(18),
         z_anln1 LIKE v_anepk-anln1,
         k_belnr LIKE bkpf-belnr.
  DATA :z_aktiv LIKE anla-aktiv,
        z_afasl LIKE anlb-afasl,
        z_afabg LIKE anlb-afabg.

  CLEAR : w_int,z_belnr,z_awkey, z_anln1.

  REFRESH it_anepk.

  IF s_anln1-low IS INITIAL.
    SELECT  anln1  bukrs  gjahr belnr afabe awtyp
          INTO CORRESPONDING FIELDS OF TABLE it_anepk
             FROM v_anepk
              WHERE bukrs EQ 'H201'
                AND bwasl EQ '300'.
  ELSE.
    SELECT  anln1  bukrs  gjahr belnr afabe awtyp
          INTO CORRESPONDING FIELDS OF TABLE it_anepk
             FROM v_anepk
              WHERE bukrs EQ 'H201'
                AND anln1 IN s_anln1
                AND bwasl EQ '300'.
  ENDIF.
  DESCRIBE TABLE it_anepk LINES w_int.
  IF w_int <> 0.
    LOOP AT it_anepk.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = it_anepk-belnr
           IMPORTING
                output = z_belnr.

      CONCATENATE  z_belnr it_anepk-bukrs it_anepk-gjahr
           INTO z_awkey.
      SELECT SINGLE belnr INTO k_belnr FROM bkpf
        WHERE awtyp EQ it_anepk-awtyp
          AND awkey EQ z_awkey.

      SELECT SINGLE anln1 INTO z_anln1 FROM bseg
              WHERE bukrs EQ it_anepk-bukrs
                AND belnr EQ k_belnr
                AND gjahr EQ it_anepk-gjahr
                AND anln1 <> it_anepk-anln1.
*original asset information
*Capitalized on
      SELECT SINGLE aktiv INTO z_aktiv  FROM anla
              WHERE anln1 EQ it_anepk-anln1
                AND bukrs EQ it_anepk-bukrs.
*
      SELECT SINGLE afasl afabg INTO (z_afasl,z_afabg)
           FROM anlb
                WHERE  bukrs EQ it_anepk-bukrs
                  AND  anln1 EQ it_anepk-anln1
                  AND  afabe EQ it_anepk-afabe.
*bdc
      PERFORM cap_bdc_process USING z_anln1 z_aktiv z_afasl z_afabg
                                    it_anepk.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CAP_DATE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  cap_bdc_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cap_bdc_process USING p_anln1 p_aktiv p_afasl p_afabg
                           p_anepk LIKE it_anepk.
  DATA : c_aktiv(8) TYPE n,c_afabg(10) TYPE n.

  REFRESH :it_bdc,it_message.CLEAR : it_bdc, c_aktiv,c_afabg.

  SELECT SINGLE *
          FROM usr01
         WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      c_aktiv+4(4) = p_aktiv+0(4).
      c_aktiv+2(2) = p_aktiv+4(2).
      c_aktiv+0(2) = p_aktiv+6(2).
      c_afabg+4(4) = p_afabg+0(4).
      c_afabg+2(2) = p_afabg+4(2).
      c_afabg+0(2) = p_afabg+6(2).

    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      c_aktiv+4(4) = p_aktiv+0(4).
      c_aktiv+0(2) = p_aktiv+4(2).
      c_aktiv+2(2) = p_aktiv+6(2).
      c_afabg+4(4) = p_afabg+0(4).
      c_afabg+0(2) = p_afabg+4(2).
      c_afabg+2(2) = p_afabg+6(2).
  ENDCASE.


  PERFORM dynpro USING:
         'X' 'SAPLAIST'        '0100',
         ' ' 'BDC_OKCODE'      '/00',
         ' ' 'ANLA-ANLN1'       p_anln1,
         ' ' 'ANLA-ANLN2'      '0',
         ' ' 'ANLA-BUKRS'      'H201'.

  PERFORM dynpro USING:
         'X' 'SAPLAIST'        '1000',
         ' ' 'BDC_OKCODE'      '=TAB08',
         ' ' 'ANLA-AKTIV'       c_aktiv.
  CASE p_anepk-afabe.
    WHEN '01'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(01)'   p_afasl,
             ' ' 'ANLB-AFABG(01)'   c_afabg.
    WHEN '10'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(02)'   p_afasl,
             ' ' 'ANLB-AFABG(02)'   c_afabg.
    WHEN '20'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(03)'   p_afasl,
             ' ' 'ANLB-AFABG(03)'   c_afabg.
    WHEN '30'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(04)'   p_afasl,
             ' ' 'ANLB-AFABG(04)'   c_afabg.
    WHEN '40'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(05)'   p_afasl,
             ' ' 'ANLB-AFABG(05)'   c_afabg.
    WHEN '50'.
      PERFORM dynpro USING:
             'X' 'SAPLAIST'        '1000',
             ' ' 'BDC_OKCODE'      '=BUCH',
             ' ' 'ANLB-AFASL(06)'   p_afasl,
             ' ' 'ANLB-AFABG(06)'   c_afabg.
  ENDCASE.



  CALL TRANSACTION 'AS02' USING it_bdc
                           MODE c_mode
                           UPDATE 'S'
                           MESSAGES INTO it_message.
  READ TABLE it_message WITH KEY msgspra = 'E'.
  IF sy-subrc = 0.
    it_anepk-result = 'E'.
  ELSE.
    it_anepk-result = 'S'.
  ENDIF.
  MODIFY it_anepk FROM it_anepk.
  CLEAR it_anepk.
  IF it_anepk-result EQ 'S'.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " cap_bdc_process
*&---------------------------------------------------------------------*
*&      Form  asset_split
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM asset_split.
  LOOP AT it_spi.
    REFRESH :it_bdc,it_message.CLEAR it_bdc.
    PERFORM dynpro USING:
           'X' 'SAPLAMDP'        '0100',
           ' ' 'BDC_OKCODE'      '=TAB02',
           ' ' 'RAIFP2-ANLN1'      it_spi-anln1,
           ' ' 'RAIFP2-ANLN2'      it_spi-anln2,
           ' ' 'RAIFP1-BLDAT'      it_spi-bldat,
           ' ' 'RAIFP1-BUDAT'      it_spi-bldat,
           ' ' 'RAIFP1-BZDAT'      it_spi-bldat,
           ' ' 'RAIFP3-XBANL'      'X',
           ' ' 'RAIFP3-ANLN1'      it_spi-anln3,
           ' ' 'RAIFP3-ANLN2'      it_spi-anln4.

    PERFORM dynpro USING:
           'X' 'SAPLAMDP'        '0100',
           ' ' 'BDC_OKCODE'      '=TAB03',
           ' ' 'RAIFP2-MONAT'      it_spi-monat.

    PERFORM dynpro USING:
           'X' 'SAPLAMDP'        '0100',
           ' ' 'BDC_OKCODE'      '=SAVE',
           ' ' 'RAIFP2-ANBTR'      it_spi-anbtr,
           ' ' 'RAIFP2-PROZS'      it_spi-prozs.
    IF it_spi-ind = '1'.
      PERFORM dynpro USING:
             ' ' 'RAIFP2-XAALT'     'X'.
    ELSE.
      PERFORM dynpro USING:
             ' ' 'RAIFP2-XANEU'     'X'.
    ENDIF.
    CALL TRANSACTION 'ABUMN' USING it_bdc
                             MODE c_mode
                             UPDATE 'S'
                              MESSAGES INTO it_message.
    READ TABLE it_message WITH KEY msgspra = 'E'.
    IF sy-subrc = 0.
      it_anepk-result = 'E'.
    ELSE.
      it_anepk-result = 'S'.
    ENDIF.
    MODIFY it_spi FROM it_spi.
    IF it_spi-result EQ 'S'.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " asset_split
*&---------------------------------------------------------------------*
*&      Form  cap_date_update_direct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cap_date_update_direct.
  DATA : w_int TYPE i,
         z_belnr(10),
         z_awkey(18),
         z_anln1 LIKE v_anepk-anln1,
         k_belnr LIKE bkpf-belnr.
  DATA :z_aktiv LIKE anla-aktiv,
        z_afasl LIKE anlb-afasl,
        z_afabg LIKE anlb-afabg.

  CLEAR : w_int,z_belnr,z_awkey, z_anln1.

  REFRESH it_sdat.

  IF s_anln2-low IS INITIAL.
    SELECT anln1 aktiv
          INTO CORRESPONDING FIELDS OF TABLE it_sdat
             FROM anla
              WHERE bukrs EQ 'H201'
                AND aktiv IN s_aktiv.
  ELSE.
    SELECT anln1 aktiv
           INTO CORRESPONDING FIELDS OF TABLE it_sdat
              FROM anla
               WHERE bukrs EQ 'H201'
                 AND anln1 IN s_anln2
                 AND aktiv IN s_aktiv.
  ENDIF.

  DESCRIBE TABLE it_sdat LINES w_int.
  IF w_int <> 0.
    LOOP AT it_sdat.
*bdc
      PERFORM cap_bdc_process_direct USING  it_sdat.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " cap_date_update_direct
*&---------------------------------------------------------------------*
*&      Form  cap_bdc_process_direct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Z_ANLN1  text
*      -->P_Z_AKTIV  text
*      -->P_Z_AFASL  text
*      -->P_Z_AFABG  text
*      -->P_IT_ANEPK  text
*----------------------------------------------------------------------*
FORM cap_bdc_process_direct USING p_sdat LIKE it_sdat.
  DATA : c_date(8) TYPE n.

  REFRESH :it_bdc,it_message.CLEAR : it_bdc, c_date.

  SELECT SINGLE *
          FROM usr01
         WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      c_date+4(4) = s_afabg-low+0(4).
      c_date+2(2) = s_afabg-low+4(2).
      c_date+0(2) = s_afabg-low+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      c_date+4(4) = s_afabg-low+0(4).
      c_date+0(2) = s_afabg-low+4(2).
      c_date+2(2) = s_afabg-low+6(2).
  ENDCASE.


  PERFORM dynpro USING:
         'X' 'SAPLAIST'        '0100',
         ' ' 'BDC_OKCODE'      '/00',
         ' ' 'ANLA-ANLN1'       p_sdat-anln1,
         ' ' 'ANLA-ANLN2'      '0',
         ' ' 'ANLA-BUKRS'      'H201'.

  PERFORM dynpro USING:
         'X' 'SAPLAIST'        '1000',
         ' ' 'BDC_OKCODE'      '=TAB08'.

  PERFORM dynpro USING:
         'X' 'SAPLAIST'        '1000',
         ' ' 'BDC_OKCODE'      '=BUCH'.
  PERFORM dynpro USING:
          ' ' 'ANLB-AFABG(01)'   c_date.

  SELECT SINGLE  * FROM anlb
    WHERE anln1 EQ p_sdat-anln1
      AND afabe EQ '20'.
  IF sy-subrc = 0.
    PERFORM dynpro USING:
           ' ' 'ANLB-AFABG(03)'   c_date.
  ENDIF.

  SELECT SINGLE  * FROM anlb
    WHERE anln1 EQ p_sdat-anln1
      AND afabe EQ '50'.
  IF sy-subrc = 0.
    PERFORM dynpro USING:
           ' ' 'ANLB-AFABG(06)'   c_date.
  ENDIF.

  CALL TRANSACTION 'AS02' USING it_bdc
                           MODE c_mode
                           UPDATE 'S'
                           MESSAGES INTO it_message.
  READ TABLE it_message WITH KEY  msgtyp = 'E'.
  IF sy-subrc = 0.
    it_sdat-result = 'E'.
  ELSE.
    it_sdat-result = 'S'.
  ENDIF.
  MODIFY it_sdat FROM it_sdat.
  CLEAR it_sdat.
  IF it_sdat-result EQ 'S'.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " cap_bdc_process_direct
