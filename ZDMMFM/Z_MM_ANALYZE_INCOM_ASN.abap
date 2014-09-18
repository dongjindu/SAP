FUNCTION z_mm_analyze_incom_asn.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(S_DOCNUM) TYPE  EDI_DOCNUM
*"     REFERENCE(S_STATUS) TYPE  EDI_STATUS
*"     REFERENCE(P_TRAID) LIKE  LIKP-TRAID OPTIONAL
*"     REFERENCE(P_LIFEX) LIKE  LIKP-LIFEX OPTIONAL
*"     REFERENCE(P_EBELN) LIKE  EKKO-EBELN OPTIONAL
*"     REFERENCE(P_MATNR) LIKE  EKPO-MATNR OPTIONAL
*"     REFERENCE(CHECK_ASN_EXIST) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(ONLY_LIFEX_TRAID) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     REFERENCE(MESSAG2) LIKE  ZMMS0027-MESSAG2
*"     REFERENCE(FIND_TRAID) TYPE  CHAR1
*"     REFERENCE(R_LIKP) LIKE  LIKP STRUCTURE  LIKP
*"     REFERENCE(ID_TRAID) LIKE  LIKP-TRAID
*"     REFERENCE(ID_LIFEX) LIKE  LIKP-LIFEX
*"  TABLES
*"      IT_ID STRUCTURE  ZMMS0027
*"----------------------------------------------------------------------


  DATA:BEGIN OF it_sg OCCURS 1,
        segnam LIKE edid4-segnam,
        fld LIKE  dfies-fieldname,
       END OF it_sg.
  DATA:flg_add,ii TYPE i,
       segnam LIKE dntab-tabname,offs TYPE i,
       BEGIN OF it_sd4 OCCURS 1,
        docnum LIKE edid4-docnum,
        counter LIKE edid4-counter,
        segnum LIKE edid4-segnum,
        segnam LIKE edid4-segnam,
        sdata LIKE edid4-sdata,
       END OF it_sd4,
       BEGIN OF it_traid OCCURS 1,
        traid LIKE likp-traid,
       END OF it_traid,
       BEGIN OF it_lifex OCCURS 1,
        lifexd LIKE likp-lifex,
       END OF it_lifex,
       BEGIN OF it_ebeln OCCURS 1,
        ebeln LIKE ekko-ebeln,
       END OF it_ebeln,
       BEGIN OF it_matnr OCCURS 1,
        matnr LIKE ekpo-matnr,
       END OF it_matnr,
       BEGIN OF int_dfies OCCURS 50.
          INCLUDE STRUCTURE dfies.
  DATA:END OF int_dfies,
       h_lifex LIKE it_id-lifex,
       h_traid LIKE it_id-traid,
       h_cont LIKE it_id-exidv,
       BEGIN OF lnid OCCURS 1,
        counter LIKE edid4-counter,
        or_segnum LIKE edid4-segnum,
        segnum LIKE edid4-segnum,
        abcd,
        segnam LIKE edid4-segnam,
        fld(20),
        val(30),
       END OF lnid,
       akt_posnr TYPE posnr,
       BEGIN OF it_qt OCCURS 1,
        posnr TYPE posnr,
        lfimg TYPE lfimg,
        vemng TYPE lfimg,
       END OF it_qt,
       BEGIN OF bs_sg OCCURS 1,
        segnum LIKE edid4-segnum,
       END OF bs_sg.
  DATA:cntt TYPE i,
       p_matnr_txt(30),
       r_zterm LIKE ekko-zterm,r_inco1 LIKE ekko-inco1,
       r_inco2 LIKE ekko-inco2,
       r_lgort LIKE ekpo-lgort.
  DATA:BEGIN OF it_msg OCCURS 1,
        prior(1) TYPE n,
        msg LIKE zmms0027-messag2,
       END OF it_msg.
  DATA:ln TYPE i,t250(250),first_char(1).
  DATA:it_case LIKE zmms0027-exidv OCCURS 1 WITH HEADER LINE,
       it_duhu LIKE zmms0027-exidv OCCURS 1 WITH HEADER LINE,
       it_dbl LIKE zmms0027-exidv OCCURS 1 WITH HEADER LINE,
       flg_container,flg_exp.
  TABLES:likp."M_1
  FIELD-SYMBOLS: <fld>.
  MOVE:'E1EDL20' TO it_sg-segnam,'LIFEX' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL20' TO it_sg-segnam,'TRAID' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL24' TO it_sg-segnam,'POSNR' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL24' TO it_sg-segnam,'KDMAT' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL24' TO it_sg-segnam,'LFIMG' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL24' TO it_sg-segnam,'VRKME' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL41' TO it_sg-segnam,'BSTNR' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL41' TO it_sg-segnam,'POSEX' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL37' TO it_sg-segnam,'EXIDV' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL44' TO it_sg-segnam,'EXIDV' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL37' TO it_sg-segnam,'VHILM_KU' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL44' TO it_sg-segnam,'POSNR' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL44' TO it_sg-segnam,'VEMNG' TO it_sg-fld.
  APPEND it_sg.
  MOVE:'E1EDL44' TO it_sg-segnam,'VEMEH' TO it_sg-fld.
  APPEND it_sg.
*
  SELECT * FROM edsappl FOR ALL ENTRIES IN it_sg
                              WHERE fieldname = it_sg-fld.
    MOVE edsappl-segtyp TO it_sg-segnam.
    COLLECT it_sg.
  ENDSELECT.

  SELECT docnum sdata segnum segnam FROM edid4
       INTO CORRESPONDING FIELDS OF TABLE it_sd4 FOR ALL ENTRIES IN
it_sg
       WHERE docnum = s_docnum AND segnam = it_sg-segnam.
*
  FREE it_id.
  CLEAR:it_id,messag2,id_traid,id_lifex.
*
  SORT it_sd4 BY docnum counter segnum.
*
  LOOP AT it_sd4.
    MOVE:it_sd4-segnam TO segnam.
    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname   = segnam
      TABLES
        dfies_tab = int_dfies
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      CLEAR offs.
      SORT int_dfies BY position.
      LOOP AT int_dfies.
        READ TABLE it_sg WITH KEY fld = int_dfies-fieldname.
        IF sy-subrc = 0.
          ASSIGN it_sd4-sdata+offs(int_dfies-leng) TO <fld>
                                  TYPE int_dfies-inttype.
          MOVE:it_sd4-counter TO lnid-counter,
               it_sd4-segnum TO lnid-segnum,
               it_sd4-segnum TO lnid-or_segnum,
               it_sd4-segnam TO lnid-segnam,
               int_dfies-fieldname TO lnid-fld,
               <fld> TO lnid-val.
          flg_add = 'X'.
          CASE int_dfies-fieldname.
            WHEN 'LIFEX'.
              MOVE <fld> TO h_lifex.CLEAR flg_add.
              MOVE <fld> TO it_lifex.
              APPEND it_lifex.
              lnid-abcd = 'A'.
            WHEN 'TRAID'.
              MOVE <fld> TO h_traid.CLEAR flg_add.
              MOVE <fld> TO it_traid.
              APPEND it_traid.
              lnid-abcd = 'A'.
            WHEN 'POSNR'.
              lnid-abcd = 'B'.
            WHEN 'EXIDV'.
              lnid-abcd = 'C'.
              IF lnid-segnam = 'E1EDL37'.
                READ TABLE it_duhu WITH KEY = lnid-val.
                IF sy-subrc = 0.
                  MOVE lnid-val TO it_dbl.APPEND it_dbl.
                ENDIF.
                MOVE lnid-val TO it_duhu.
                APPEND it_duhu.
              ELSE.
                CLEAR flg_add.
                IF lnid-segnam = 'E1EDL44' AND flg_container = 'X'.
                  MOVE lnid-val TO it_case.
                  APPEND it_case.
                ENDIF.
              ENDIF.
            WHEN 'BSTNR'.
              MOVE <fld> TO it_ebeln.
              APPEND it_ebeln.
              lnid-abcd = 'C'.
            WHEN 'KDMAT'.
              MOVE <fld> TO it_matnr.
              APPEND it_matnr.
              lnid-abcd = 'C'.
            WHEN 'VHILM_KU'.
              IF lnid-val = 'CONTAINER' AND flg_exp = 'X'.
                flg_container = 'X'.
              ENDIF.
              IF lnid-val = 'EXPENDABLE'.
                flg_exp = 'X'.
                IF flg_container = 'X'.
                CLEAR flg_container. "container must be in last E1EDL37
                ENDIF.
              ENDIF.
              lnid-abcd = 'C'.
            WHEN OTHERS.
              lnid-abcd = 'C'.
          ENDCASE.
          IF flg_add = 'X' AND lnid-val <> ''.
            APPEND lnid.
          ENDIF.
        ENDIF.
        offs = offs + int_dfies-leng.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
*
  id_traid = h_traid.  "M_3
  id_lifex = h_lifex.
  IF only_lifex_traid = 'X'. "M_3
    EXIT.
  ENDIF.

  SORT lnid.
  find_traid = 'X'.
* External Trail Id
  IF NOT p_traid IS INITIAL.
    READ TABLE it_traid WITH KEY = p_traid.
    IF sy-subrc <> 0.
      CLEAR find_traid.
      EXIT.
    ENDIF.
  ENDIF.
* Checking External Identification of Delivery Note
  IF NOT p_lifex IS INITIAL.
    READ TABLE it_lifex WITH KEY = p_lifex.
    IF sy-subrc <> 0.
      CLEAR find_traid.
      EXIT.
    ENDIF.
  ENDIF.
* Checking Purchasing Document No
  IF NOT p_ebeln IS INITIAL.
    READ TABLE it_ebeln WITH KEY = p_ebeln.
    IF sy-subrc <> 0.
      CLEAR find_traid.
      EXIT.
    ENDIF.
  ENDIF.
* Checking Material No
  IF  NOT p_matnr IS INITIAL.
    READ TABLE it_matnr WITH KEY = p_matnr.
    IF sy-subrc <> 0.
      WRITE p_matnr TO p_matnr_txt.
      READ TABLE it_matnr WITH KEY = p_matnr_txt.
      IF sy-subrc <> 0.
        CLEAR find_traid.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
*
  LOOP AT lnid WHERE segnam = 'E1EDL24' OR segnam = 'E1EDL37'.
    MOVE lnid-segnum TO bs_sg-segnum.
    COLLECT bs_sg.
  ENDLOOP.
*
  LOOP AT lnid WHERE segnam <> 'E1EDL24' AND segnam <> 'E1EDL37'.
    IF lnid-segnum IS INITIAL.
      DELETE lnid.
    ELSE.
      WHILE lnid-segnum > 0.
        lnid-segnum = lnid-segnum - 1.
        READ TABLE bs_sg WITH KEY = lnid-segnum.
        IF sy-subrc = 0.
          EXIT.
        ENDIF.
      ENDWHILE.
      MODIFY lnid.
    ENDIF.
  ENDLOOP.
*
  SORT lnid.
  LOOP AT lnid.
    CLEAR:lnid-or_segnum,lnid-abcd.
    MODIFY lnid.
  ENDLOOP.
*
  LOOP AT lnid.
    AT NEW segnum.
      CLEAR it_id.
      MOVE:h_lifex TO it_id-lifex,
           h_traid TO it_id-traid,
           'EA' TO it_id-unit.
    ENDAT.
    MOVE lnid-segnum TO it_id-segnum.
    CASE lnid-fld.
      WHEN 'KDMAT'.MOVE lnid-val TO it_id-matnr.
      WHEN 'VRKME' OR 'VEMEH'.
* check unit of measure
        IF lnid-val <> 'EA'.
          it_id-unit = '??'.
        ENDIF.
      WHEN 'BSTNR'.MOVE lnid-val TO it_id-ebeln.
      WHEN 'POSEX'.MOVE lnid-val TO it_id-ebelp.
      WHEN 'POSNR'.
        MOVE lnid-val TO akt_posnr.
        IF lnid-segnam = 'E1EDL24'.
          MOVE lnid-val TO it_id-posnr.
        ENDIF.
      WHEN 'EXIDV'.MOVE lnid-val TO it_id-exidv.
      WHEN 'VHILM_KU'.MOVE lnid-val(4) TO it_id-vhilm_ku.
      WHEN 'LFIMG'.
        CLEAR it_qt-vemng.
        MOVE akt_posnr TO it_qt-posnr.
        MOVE lnid-val TO it_qt-lfimg.
        COLLECT it_qt.
      WHEN 'VEMNG'.
        CLEAR it_qt-lfimg.
        MOVE akt_posnr TO it_qt-posnr.
        MOVE lnid-val TO it_qt-vemng.
        COLLECT it_qt.
    ENDCASE.
    AT END OF segnum.
      APPEND it_id.
    ENDAT.
  ENDLOOP.
*
  FREE lnid.
  LOOP AT it_id WHERE posnr <> ''.
    READ TABLE it_qt WITH KEY posnr = it_id-posnr.
    IF sy-subrc = 0.
      MOVE it_qt-lfimg TO ii.WRITE ii TO it_id-lfimg.
      MOVE it_qt-vemng TO ii.WRITE ii TO it_id-vemng.
      MODIFY it_id.
    ENDIF.
  ENDLOOP.
*
*Additional fields for Idocs It_id lines & check
  SORT it_id.
  CLEAR ii.
  LOOP AT it_id WHERE ebeln <> '' OR matnr <> ''.
    CLEAR:ekko,ekpo.
    SELECT SINGLE * FROM ekko WHERE ebeln = it_id-ebeln.
    IF sy-subrc <> 0.
      it_id-po_doesnt_exist = 'X'.
      CONCATENATE 'Purch.Doc:' it_id-ebeln '-Doesn''t exist'
                   INTO it_msg-msg.
      it_msg-prior = 2.
      APPEND it_msg.
    ELSE.
      CLEAR it_id-po_doesnt_exist.
    ENDIF.
    IF it_id-matnr CS '-'.
      CONCATENATE it_id-matnr 'Don''t use hyphen ''-'' in Mater.Number'
                  INTO it_msg-msg SEPARATED BY space.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    SELECT SINGLE * FROM ekpo WHERE ebeln = it_id-ebeln
        AND ebelp = it_id-ebelp AND matnr = it_id-matnr.
    IF sy-subrc = 0.
      IF ekpo-loekz <> ''.
        MOVE '@11@' TO it_id-icon_del.
        CONCATENATE 'Item:' it_id-ebeln '/' it_id-ebelp
                      'Deleted from Pur.Doc' INTO it_msg-msg.
        it_msg-prior = 2.
        APPEND it_msg.
      ENDIF.
    ELSE.
      it_id-po_item_incons = 'X'.
      CONCATENATE 'Purch.D.' it_id-ebeln '/' it_id-ebelp '<>'
it_id-matnr
                    INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    IF ii = 0.
      r_zterm = ekko-zterm.
      r_inco1 = ekko-inco1.
      r_inco2 = ekko-inco2.
      r_lgort = ekpo-lgort.
    ENDIF.
    ii = ii + 1.
    IF it_id-unit <> 'EA'.
      CONCATENATE 'Mater.:' it_id-matnr '-Unit of measure<>EA'
                   r_inco2 INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    IF it_id-lfimg <> it_id-vemng.
      CONCATENATE 'Item:' it_id-matnr 'Qty-Mat:' it_id-lfimg
                 '<>Qty-HU' it_id-vemng INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    IF ekko-zterm <> r_zterm.
      CONCATENATE 'Different Paym.cond in PO:' ekko-zterm '<>'
                   r_zterm INTO it_msg-msg.
      it_msg-prior = 3.
      APPEND it_msg.
    ENDIF.
    IF ekko-inco1 <> r_inco1.
      CONCATENATE 'Different Incoter.1.' ekko-inco1 '<>'
                   r_inco1 INTO it_msg-msg.
      it_msg-prior = 3.
      APPEND it_msg.
    ENDIF.
    IF ekko-inco2 <> r_inco2.
      CONCATENATE 'Different Incoter.2.' ekko-inco2 '<>'
                  r_inco2 INTO it_msg-msg.
      it_msg-prior = 2.
      APPEND it_msg.
    ENDIF.
    IF ekpo-lgort <> r_lgort.
      CONCATENATE 'Different Stor.location:' ekpo-lgort '<>'
                   r_lgort INTO it_msg-msg.
      it_msg-prior = 3.
      APPEND it_msg.
    ENDIF.
    MOVE:ekko-zterm TO it_id-zterm,ekko-inco1 TO it_id-inco1,
         ekko-inco2 TO it_id-inco2,ekpo-lgort TO it_id-lgort.
    MODIFY it_id.
  ENDLOOP.
*
  LOOP AT it_id WHERE ebeln = '' AND matnr = ''.
    first_char = it_id-exidv(1).
    TRANSLATE first_char TO UPPER CASE.
    IF first_char < 'A' OR first_char > 'Z'.
      MOVE 'X' TO it_id-f_double_hu_idoc.
      CONCATENATE 'Wrong HU numer:' it_id-exidv
               '-must start with ''A''-''Z''' INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    IF it_id-unit <> 'EA'.
      CONCATENATE 'HU:' it_id-exidv '-Unit of measure<>EA'
                    r_inco2 INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    SELECT COUNT( * ) FROM vekp INTO cntt
                WHERE exidv = it_id-exidv AND status <> '0060'.
    IF cntt > 0.
      it_id-flg_alr_ex = 'X'.
      IF s_status <> 53.
        CONCATENATE 'HU:' it_id-exidv '-already exists' INTO it_msg-msg.
        it_msg-prior = 1.
        APPEND it_msg.
      ENDIF.
    ENDIF.
    IF flg_container = 'X' AND it_id-vhilm_ku = 'EXPE'.
      READ TABLE it_case WITH KEY = it_id-exidv.
      IF sy-subrc <> 0.
        it_id-f_hu_not_in_cont = 'X'.
        CONCATENATE 'HU:' it_id-exidv '-missing in container'
                          INTO it_msg-msg.
        it_msg-prior = 1.
        APPEND it_msg.
      ENDIF.
    ENDIF.
    IF flg_container = 'X' AND it_id-vhilm_ku = 'CONT'.
      h_cont = it_id-exidv.
    ENDIF.
*
    READ TABLE it_dbl WITH KEY = it_id-exidv.
    IF sy-subrc = 0.
      MOVE 'X' TO it_id-f_double_hu_idoc.
      CONCATENATE 'Double use HU:' it_id-exidv '-in Idoc:' s_docnum
                         INTO it_msg-msg.
      it_msg-prior = 1.
      APPEND it_msg.
    ENDIF.
    MODIFY it_id.
  ENDLOOP.
*
  LOOP AT it_case.
    READ TABLE it_id WITH KEY exidv = it_case.
    IF sy-subrc <> 0.
      CONCATENATE 'CASE:' it_case '-is in container, but not as HU'
                          INTO it_msg-msg.
      it_msg-prior = 4.
      APPEND it_msg.
    ENDIF.
  ENDLOOP.
*
  IF flg_container = 'X'.
    CLEAR ln.
    LOOP AT it_id WHERE vhilm_ku = 'CONT'
                 AND ebeln = '' AND exidv = h_traid(10).
      ln = ln + 1.
    ENDLOOP.
    IF ln <> 1.
      CONCATENATE 'Container:' h_cont '-TRAID-' h_traid
                  'not synchronized'
                         INTO it_msg-msg.
      it_msg-prior = 3.
      APPEND it_msg.
    ENDIF.
  ENDIF.
*
  SORT it_msg BY prior.
  LOOP AT it_msg.
    ln = STRLEN( t250 ).
    IF ln > 80.
      EXIT.
    ENDIF.
    IF ln = 0.
      MOVE it_msg-msg TO t250.
    ELSE.
      CONCATENATE t250 '|.|'  it_msg-msg INTO t250.
    ENDIF.
  ENDLOOP.
  MOVE t250(110) TO messag2.
*
  SORT it_id.
*M_1 insert *Begin**
  CLEAR r_likp.
  IF NOT h_traid IS INITIAL AND NOT h_lifex IS INITIAL
     AND  NOT check_asn_exist IS INITIAL AND s_status <> 53.
    SELECT * FROM likp WHERE lifex = h_lifex AND traid = h_traid
                         ORDER BY erdat DESCENDING.
      MOVE likp TO r_likp.
      EXIT.
    ENDSELECT.
  ENDIF.
*M_1 insert *End**

ENDFUNCTION.
