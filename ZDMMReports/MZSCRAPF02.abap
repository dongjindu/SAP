*---------------------------------------------------------------------*
*       FORM DISPLAY_VALUES                                           *
*---------------------------------------------------------------------*
* Calvin asks to change costcenter for VPC,                                                    *
*---------------------------------------------------------------------*
FORM display_values.
  DATA: it_reptag LIKE TABLE OF zscrap_reptag WITH HEADER LINE.
  DATA:  l_stat LIKE jest-stat,
           l_txt30 LIKE tj30t-txt30,
           l_stsma LIKE jsto-stsma,
           l_date LIKE sy-datum.
  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA:  ret_tab LIKE ddshretval OCCURS 0 WITH HEADER LINE,
         it_dsel LIKE dselc OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF it_dynpfields OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF it_dynpfields.

  dyname = 'SAPMZSCRAP'.
  dynumb = '1801'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
    EXPORTING
      input  = qmel-qmdat
    IMPORTING
      output = l_date.

  IF p_qmnum IS INITIAL.
    IF p_matnr IS INITIAL.
      IF p_nuser IS INITIAL AND qmel-qmdat IS INITIAL.
        MESSAGE i026 WITH 'No data selected'.
        EXIT.
      ENDIF.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
      FROM qmel.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REPTAG
*      FROM QMEL
*      WHERE MATNR = P_MATNR
*      AND AEDAT = L_DATE
*      AND AENAM = P_NUSER.
*      IF SY-SUBRC <> 0.
*        EXIT.
*      ENDIF.
    ELSE.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
               FROM qmel
              WHERE matnr = p_matnr.
    ENDIF.
    IF NOT p_nuser IS INITIAL.
      DELETE it_reptag WHERE aenam <> p_nuser.
    ENDIF.
    IF NOT qmel-qmdat IS INITIAL.
      DELETE it_reptag WHERE aedat <> qmel-qmdat.
    ENDIF.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
               FROM qmel
              WHERE qmnum = p_qmnum.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.

  LOOP AT it_reptag.
    SELECT SINGLE stat FROM jest INTO l_stat
                       WHERE objnr = it_reptag-objnr
                         AND inact = ' '.
    SELECT SINGLE stsma FROM jsto INTO l_stsma
                       WHERE objnr = jest-objnr.
    SELECT SINGLE txt30 FROM tj30t INTO l_txt30
                       WHERE stsma = l_stsma
                         AND estat = l_stat.
    it_reptag-txt30 = l_txt30.
    MODIFY it_reptag.
    CLEAR: it_reptag, l_stat, l_txt30, l_stsma.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM it_reptag.
  it_dsel-fldname = 'QMNUM'.
  it_dsel-dyfldname = 'P_QMNUM'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'P_MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'AENAM'.
  it_dsel-dyfldname = 'P_USER'.
  APPEND it_dsel.

  it_dsel-fldname = 'AEDAT'.
  it_dsel-dyfldname = 'QMEL-QMDAT'.
  APPEND it_dsel.

  it_dsel-fldname = 'MZEIT'.
  it_dsel-dyfldname = 'P_NTIME'.
  APPEND it_dsel.

  it_dsel-fldname = 'TXT30'.
  it_dsel-dyfldname = 'P_STATUS'.
  APPEND it_dsel.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    ddic_structure         = 'ZSCRAP_REPTAG'
       retfield               = 'QMEL_QMNUM'
*   PVALKEY                = ' '
     dynpprog               = dyname
     dynpnr                 = dynumb
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     value_tab              = it_reptag
*    field_tab             = fld_tab
     return_tab             = ret_tab
     dynpfld_mapping        = it_dsel
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ret_tab.
    CASE ret_tab-fieldname.
      WHEN 'QMNUM'.
        p_qmnum = ret_tab-fieldval.
      WHEN 'MATNR'.
        p_matnr = ret_tab-fieldval.
      WHEN 'AENAM'.
        p_nuser = ret_tab-fieldval.
      WHEN 'AEDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            input  = ret_tab-fieldval
          IMPORTING
            output = qmel-qmdat.
      WHEN 'MZEIT'.
        p_ntime = ret_tab-fieldval.
      WHEN 'TXT30'.
        p_status = ret_tab-fieldval.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    "DISPLAY_VALUES
*&---------------------------------------------------------------------*
*&      Form  find_QMNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_qmnum.
  DATA: it_reptag LIKE TABLE OF zscrap_reptag WITH HEADER LINE.
  DATA:  l_stat LIKE jest-stat,
         l_txt30 LIKE tj30t-txt30,
         l_stsma LIKE jsto-stsma,
         l_date LIKE sy-datum.
  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA:  ret_tab LIKE ddshretval OCCURS 0 WITH HEADER LINE,
         it_dsel LIKE dselc OCCURS 0 WITH HEADER LINE.

  dyname = 'SAPMZSCRAP'.
  dynumb = '1801'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

  IF p_qmnum IS INITIAL.
    IF p_matnr IS INITIAL.
      IF p_nuser IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
        FROM qmel.
      ELSE.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
           FROM qmel
           WHERE aenam = p_nuser.
      ENDIF.
      IF NOT qmel-qmdat IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*             EXPORTING
*                  INPUT  = QMEL-QMDAT
*             IMPORTING
*                  OUTPUT = L_DATE.
        DELETE it_reptag WHERE aedat <> qmel-qmdat.
      ENDIF.
    ELSE.
      DO.
        REPLACE '*' WITH '%' INTO p_matnr.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF  ( qmel-qmdat = '00000000' OR qmel-qmdat = ' ' )
          AND p_nuser IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
        FROM qmel
        WHERE matnr LIKE p_matnr.
      ELSE.
        IF  qmel-qmdat = '00000000' OR qmel-qmdat = ' '.
          DO.
            REPLACE '*' WITH '%' INTO p_nuser.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ENDDO.
          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
            FROM qmel
           WHERE matnr LIKE p_matnr
             AND aenam LIKE p_nuser.
        ELSE.
          IF p_nuser IS INITIAL.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
             FROM qmel
            WHERE matnr LIKE p_matnr
              AND aedat = qmel-qmdat.
          ELSE.
            DO.
              REPLACE '*' WITH '%' INTO p_nuser.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
            ENDDO.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
              FROM qmel
             WHERE matnr LIKE p_matnr
               AND aedat = qmel-qmdat
               AND aenam LIKE p_nuser.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    DO.
      REPLACE '*' WITH '%' INTO p_qmnum.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_reptag
               FROM qmel
              WHERE qmnum LIKE p_qmnum.
  ENDIF.

  LOOP AT it_reptag.
    SELECT SINGLE stat FROM jest INTO l_stat
                       WHERE objnr = it_reptag-objnr
                         AND inact = ' '.
    SELECT SINGLE stsma FROM jsto INTO l_stsma
                       WHERE objnr = jest-objnr.
    SELECT SINGLE txt30 FROM tj30t INTO l_txt30
                       WHERE stsma = l_stsma
                         AND estat = l_stat.
    it_reptag-txt30 = l_txt30.
    MODIFY it_reptag.
    CLEAR: it_reptag, l_stat, l_txt30, l_stsma.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM it_reptag.

  it_dsel-fldname = 'QMNUM'.
  it_dsel-dyfldname = 'P_QMNUM'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'P_MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'AENAM'.
  it_dsel-dyfldname = 'P_USER'.
  APPEND it_dsel.

  it_dsel-fldname = 'AEDAT'.
  it_dsel-dyfldname = 'QMEL-QMDAT'.
  APPEND it_dsel.

  it_dsel-fldname = 'MZEIT'.
  it_dsel-dyfldname = 'P_NTIME'.
  APPEND it_dsel.

  it_dsel-fldname = 'TXT30'.
  it_dsel-dyfldname = 'P_STATUS'.
  APPEND it_dsel.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    ddic_structure         = 'ZSCRAP_REPTAG'
       retfield               = 'QMEL_QMNUM'
*   PVALKEY                = ' '
     dynpprog               = dyname
     dynpnr                 = dynumb
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     value_tab              = it_reptag
*    field_tab             = fld_tab
     return_tab             = ret_tab
     dynpfld_mapping        = it_dsel
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ret_tab.
    CASE ret_tab-fieldname.
      WHEN 'QMNUM'.
        p_qmnum = ret_tab-fieldval.
      WHEN 'MATNR'.
        p_matnr = ret_tab-fieldval.
      WHEN 'AENAM'.
        p_nuser = ret_tab-fieldval.
      WHEN 'AEDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            input  = ret_tab-fieldval
          IMPORTING
            output = qmel-qmdat.
      WHEN 'MZEIT'.
        p_ntime = ret_tab-fieldval.
      WHEN 'TXT30'.
        p_status = ret_tab-fieldval.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " find_QMNUM
*&---------------------------------------------------------------------*
*&      Form  rePRINT_LABEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reprint_label.
*DATA: BEGIN OF LT_MESS OCCURS 5,
*      MESS(255),
*      END OF LT_MESS.
*DATA: L_MESS LIKE LT_MESS.
  DATA: l_mess(255).
  IF p_qmnum IS INITIAL AND
     p_matnr IS INITIAL.
    EXIT.
  ELSEIF p_qmnum IS INITIAL OR p_matnr IS INITIAL.
    PERFORM display_values.
  ENDIF.
  SET PARAMETER ID 'IQM' FIELD p_qmnum.
  SET PARAMETER ID 'MAT' FIELD p_matnr.
  CLEAR: it_qmnum, it_qmnum[].
  it_qmnum-qmnum = p_qmnum.
  it_qmnum-matnr = p_matnr.
  APPEND it_qmnum.
  EXPORT it_qmnum TO MEMORY ID 'M1'.
  DO p_copy TIMES.
    SUBMIT zqmscrap_label AND RETURN.
    IMPORT l_mess FROM MEMORY ID 'ME'.
    IF NOT l_mess IS INITIAL.
      MESSAGE i026 WITH l_mess.
    ELSE.
      MESSAGE s026 WITH 'Reprint label complete'.
    ENDIF.
    FREE MEMORY ID 'ME'.
  ENDDO.
ENDFORM.                    " rePRINT_LABEL
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  IF p_copy IS INITIAL.
    p_copy = 1.
  ENDIF.
ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_value.
  CLEAR: p_qmnum, p_matnr, p_nuser, p_ndate, p_ntime, p_status,
         qmel-qmdat.
ENDFORM.                    " CLEAR_VALUE
*&---------------------------------------------------------------------*
*&      Form  release_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QMNUM  text
*----------------------------------------------------------------------*
FORM release_lock USING    p_qmnum.
  WAIT UP TO 1 SECONDS.
  IF NOT p_qmnum IS INITIAL.
    CALL FUNCTION 'DEQUEUE_EIQMEL'
      EXPORTING
*       MODE_QMEL = 'E'
*       MANDT     = SY-MANDT
        qmnum     = p_qmnum
*       X_QMNUM   = ' '
*       _SCOPE    = '3'
*       _SYNCHRON = ' '
*       _COLLECT  = ' '
      .

  ENDIF.

ENDFORM.                    " release_lock
*&---------------------------------------------------------------------*
*&      Form  code_check1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM code_check1.
  IF qmnum IS INITIAL.
    MESSAGE e024.
  ELSE.
    SELECT SINGLE * FROM qmel WHERE qmnum = qmnum.
    IF sy-subrc <> 0.
      MESSAGE e025.
    ENDIF.
  ENDIF.
ENDFORM.                    " code_check1
*&---------------------------------------------------------------------*
*&      Form  GET_CURSOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cursor.
  GET CURSOR FIELD  p_cursor.
  IF  p_cursor = 'RKMNG'.
    p_cursor = 'RKMNG'.
  ENDIF.
ENDFORM.                    " GET_CURSOR
*&---------------------------------------------------------------------*
*&      Form  update_nopr_hmma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_nopr_hmma TABLES it_tline STRUCTURE tline
                      USING p_v_qmnum.
  DATA: msg(60).

*    L_TXT04 = 'HMMA'.
*    L_STAT = 'E0005'.

  CALL FUNCTION 'IQS0_CHANGE_NOTIF_USER_STATUS'
     EXPORTING
         i_qmnum                    = p_v_qmnum
         i_user_stat_intern         = 'E0005'
         i_user_stat_extern         = 'HMMA'
         i_langu                    = sy-langu
* IMPORTING
*   E_USER_STAT_INTERN         =
*   E_USER_STAT_EXTERN         =
     EXCEPTIONS
         foreign_lock               = 1
*   SYST_FAILURE               = 2
         invalid_notification       = 3
*   OBJECT_NOT_FOUND           = 4
         status_inconsistant        = 5
         status_not_allowed         = 6
*   OTHERS                     = 7
        .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    CONCATENATE 'User Status:' p_v_qmnum sy-msgv1 INTO msg
           SEPARATED BY space.
  ELSE.
*    MESSAGE I000 WITH TEXT-M02.
    COMMIT WORK AND WAIT.
    CONCATENATE 'User status HMMA sucessfully updated' p_v_qmnum
           INTO msg SEPARATED BY space.
    PERFORM release_lock USING p_v_qmnum.
    PERFORM update_system_status TABLES it_tline
                                USING p_v_qmnum.
  ENDIF.
  it_tline-tdformat = 'U' .
  it_tline-tdline = msg.
  APPEND it_tline.
  CLEAR it_tline.
ENDFORM.                    " update_nopr_hmma
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SYSTEM_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_system_status TABLES it_tline  STRUCTURE  tline
                          USING p_v_qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMNUM'
                                p_v_qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=COWO'.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM bdc_transaction TABLES it_tline
                          USING 'QM02'.

ENDFORM.                    " UPDATE_SYSTEM_STATUS
*&---------------------------------------------------------------------*
*&      Form  clear_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_screen_1003.
  CLEAR: qmnum,vn_matnr,ernam,mzeit,tj30t-txt30, it_qmnum..
  REFRESH: it_qmnum.
ENDFORM.                    " clear_screen
*&---------------------------------------------------------------------*
*&      Form  clear_screen_1001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_screen_1001.
  CLEAR: matnr,makt-maktg,mard-lgpbe,lfa1-lifnr,mara-mfrpn,marc-vspvb,
lfa1-name1,lgort, mdoc.
  CLEAR: rmmg1-lgnum,t001l-lgort,rmmg1-lgort,qmgrp,qmcod,rkmng,
  bwart,rm07m-grund,riwo00-qmart, it_qmnum,itobattr-equnr,
  otgrp,oteil,fegrp,fecod,urgrp,urcod, p_ucomm, cuasetxt,qmel-erdat,c3,
  qmnum, lifnr.
  REFRESH: it_qmnum.
** LTAK-BWLVS,COBL-AUFNR,
ENDFORM.                    " clear_screen_1001
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_1004.
  CLEAR eflag.

  DATA: v_lgtyp LIKE pkhd-lgtyp,
        v_lgpla LIKE pkhd-lgpla,
        v_vspvb LIKE marc-vspvb,
        v_lgber LIKE lagp-lgber,
        v_verme LIKE lqua-verme,
        v_negat TYPE t331-negat,
        v_qmnum LIKE qmel-qmnum,
        v_ltkze LIKE mlgn-ltkze.

  DATA: v_sub(20) TYPE c,
  l_atwrt LIKE ausp-atwrt.

  DATA: it_tline TYPE tline OCCURS 0 WITH HEADER LINE.

  CLEAR: it_tline, it_tline[].
  IF rkmng IS INITIAL.
    MESSAGE e026 WITH 'Quantity is required'.
  ENDIF.
  IF itobattr-equnr IS INITIAL.
    MESSAGE e026 WITH 'Body # is required'.
  ENDIF.

  IF NOT itobattr-equnr IS INITIAL.
    l_equnr = itobattr-equnr.
    IF l_equnr+3(1) = ' '.
      CONCATENATE l_equnr+0(3) l_equnr+4(14) INTO l_equnr.
      itobattr-equnr = l_equnr.
    ENDIF.
    SELECT SINGLE equnr INTO l_equnr
    FROM equi
      WHERE equnr = l_equnr.
    IF sy-subrc = 0.
    ELSE.
      MESSAGE e026 WITH 'Invalid body number entered'.
    ENDIF.
  ENDIF.

** Added on 09/15/09 by furong; Check RP>=19
  IF w_call = 'VPC'.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp AS a
      INNER JOIN cabn AS b
      ON a~atinn = b~atinn
      WHERE a~objek = itobattr-equnr
        AND klart = '002'
        AND atnam = 'P_RP_STATUS'.

    IF l_atwrt < 19.
      MESSAGE e026 WITH 'RP status is invalid for scrapping'.
    ENDIF.
  ENDIF.
*  PERFORM CODE_CHECK.

*  IT_TLINE-TDFORMAT = '*' .
*  CONCATENATE 'Plant' MARC-WERKS INTO IT_TLINE-TDLINE.
*  APPEND IT_TLINE.
*  CLEAR IT_TLINE.
*
*  IT_TLINE-TDFORMAT = '*' .
*  CONCATENATE 'Storage Location' T001L-LGORT INTO IT_TLINE-TDLINE.
*  APPEND IT_TLINE.
*  CLEAR IT_TLINE.

  IF t001l-lgort = 'P400'.

    PERFORM creat_mvt_201 TABLES it_tline   " creation of movement
                          USING itobattr-equnr.

    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                     WHERE matnr = matnr
                                       AND werks = marc-werks.

    SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                               WHERE matnr = matnr
                                 AND werks = marc-werks
                                 AND prvbe = v_vspvb.

    IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).

*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
      MOVE '999' TO v_lgtyp.

    ELSE.

      IF rmmg1-lgnum IS INITIAL.
        rmmg1-lgnum = 'P01'.
      ENDIF.
      SELECT SINGLE lgber INTO v_lgber FROM lagp
                                       WHERE lgnum = rmmg1-lgnum
                                         AND lgtyp = v_lgtyp
                                         AND lgpla = v_lgpla.


      SELECT SINGLE verme INTO v_verme FROM lqua
                            WHERE lgnum = rmmg1-lgnum
                              AND matnr = matnr
                              AND lgtyp = v_lgtyp
                              AND lgpla = v_lgpla.

      IF v_verme < rkmng.

        SELECT SINGLE negat FROM t331 INTO v_negat
                    WHERE lgnum = rmmg1-lgnum AND lgtyp = v_lgtyp.

        IF v_negat = 'X'.

        ELSE.

*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO v_lgpla.
          MOVE '999' TO v_lgtyp.
        ENDIF.

      ENDIF.

    ENDIF.

** Furong on 07/19/11
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                            USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*
*    ENDIF.
** End on 07/19/11

*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.

    PERFORM creat_mvt_201 TABLES it_tline     " creation of movement
                                 USING itobattr-equnr.
  ENDIF.

  CLEAR eflag.

  READ TABLE it_tline INDEX 1.
  MESSAGE s026 WITH it_tline-tdline.

*  IF NOT LFA1-LIFNR IS INITIAL.
*
*    PERFORM CREAT_NOTIF.                  " creation of notification
*
*  ELSE.
*
*    IT_TLINE-TDLINE = 'Vendor Not available'.
*    APPEND IT_TLINE.
*    CLEAR IT_TLINE.
*
*    PERFORM CREAT_NOTIF1.                " creation of notification
*
*  ENDIF.

*  READ TABLE IT_QMNUM INDEX 1.
*  V_QMNUM = IT_QMNUM-QMNUM.
*
*  PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE    " updating QN# long text
*                            USING V_QMNUM.
*  READ TABLE IT_TLINE INDEX 3.
*
*  IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
*    PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM. " updating QN# status
*  ELSE.
*    READ TABLE IT_TLINE INDEX 4.
*    IF IT_TLINE-TDLINE+0(5) = 'ERROR'.
*      PERFORM UPDATE_NOTIF_STATUS USING V_QMNUM.  " updating QN# status
*    ELSE.
*** Changed by Furong on 03/03/09
*      IF FEGRP = '4' AND URGRP <> '04'.
*
*        CLEAR: IT_TLINE, IT_TLINE[].
*        PERFORM UPDATE_NOPR_HMMA TABLES IT_TLINE
*                                 USING V_QMNUM.
*        WAIT UP TO 2 SECONDS.
*     PERFORM UPDATE_NOTIF_TEXT TABLES IT_TLINE   "updating QN# long
*text
*                                                      USING V_QMNUM.
*      ENDIF.
*** End of change
*    ENDIF.
*ENDIF.

********  Printing Label ********

*  PERFORM PRINT_LABEL.

*****   Send mail logic  ********
*  IF T001L-LGORT = 'P400'.
*
*    IF V_LGTYP = '422'.
*
*      SELECT SINGLE LTKZE FROM MLGN INTO V_LTKZE
*                        WHERE MATNR = MATNR AND LGNUM = RMMG1-LGNUM.
*      IF V_LTKZE = '003'.
*
*        IF QMGRP = 'MXTX53'.
*
*          V_SUB = 'PDI reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*
*        ELSE.
*
*          V_SUB = 'Line side Reorder'.
*
*          PERFORM SEND_MAIL USING V_SUB.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*  ELSEIF T001L-LGORT = 'P500'.
*
*    IF QMGRP = 'MXTX53'.
*
*      V_SUB = 'PDI reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*
*    ELSE.
*
*      V_SUB = 'Line side Reorder'.
*
*      PERFORM SEND_MAIL USING V_SUB.
*    ENDIF.
*
*  ENDIF.

  IF t001l-lgort = 'P500'.
    v_sub = 'Manager Vehicle Repair Reorder'.
    PERFORM send_mail_1004 USING v_sub.
  ENDIF.

  CLEAR: v_lgtyp, v_lgpla, v_vspvb, v_lgber, v_sub, v_verme,
         v_qmnum,v_ltkze,v_qmnum,v_negat,it_tline, it_tline[].
ENDFORM.                    " PROCESS_1004
*&---------------------------------------------------------------------*
*&      Form  CREAT_MVT_201
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TLINE  text
*----------------------------------------------------------------------*
FORM creat_mvt_201 TABLES p_it_tline STRUCTURE tline
                   USING p_equnr.


  DATA : it_header LIKE bapi2017_gm_head_01,
           code LIKE bapi2017_gm_code VALUE '03',
           v_uom LIKE mara-meins.

  DATA: it_item LIKE bapi2017_gm_item_create OCCURS 0 WITH HEADER LINE,
        it_ret LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  SELECT SINGLE meins FROM mara INTO v_uom WHERE matnr = matnr.

  it_header-pstng_date  = sy-datum.
  it_header-doc_date     = sy-datum.
  IF w_call = 'SHD' OR w_call = 'STPT'.
    it_header-header_txt = c3.
  ELSE.
    it_header-header_txt = p_equnr.
  ENDIF.

  it_item-material      = matnr.
  it_item-plant         = marc-werks.
  it_item-stge_loc      = t001l-lgort.
  it_item-move_type     = bwart.
  it_item-entry_qnt     = rkmng.
  it_item-entry_uom     = v_uom.
  it_item-move_plant    = marc-werks.
  it_item-move_stloc    = rmmg1-lgort.

** Changed by Furong on 05/19/10
  CASE w_call.
    WHEN 'VPC'.
      it_item-gl_account = '0000531350'.
    WHEN 'SHD'.
      it_item-gl_account = '0000123205'.
      it_item-move_reas = rm07m-grund.
    WHEN 'STPT'.
      it_item-gl_account = '0000608310'.
      it_item-move_reas = rm07m-grund.
    WHEN OTHERS.
      it_item-gl_account = '0000603020'.
  ENDCASE.

*  IF W_CALL = 'VPC'.
*    IT_ITEM-GL_ACCOUNT = '0000531350'.
*  ELSE.
*    IT_ITEM-GL_ACCOUNT = '0000603020'.
*  ENDIF.
** End of change

  IF  w_call = 'SHD' OR  w_call = 'STPT'.
    CASE it_item-move_reas.
      WHEN '9006'.
        it_item-costcenter = '0000055001'.
      WHEN '9007'.
        it_item-costcenter = '0000055002'.
      WHEN '9008'.
        it_item-costcenter = '0000055003'.
      WHEN '9009'.
        it_item-costcenter = '0000055004'.
      WHEN '9010'.
        it_item-costcenter = '0000055005'.
      WHEN '9011'.
        it_item-costcenter = '0000055015'.
      WHEN '9012'.
        it_item-costcenter = '0000055101'.
      WHEN '9013'.
        it_item-costcenter = '0000055102'.
      WHEN '9014'.
        it_item-costcenter = '0000055103'.
      WHEN OTHERS.
        it_item-costcenter = '0000033003'.
    ENDCASE.
** CHANGED ON 01/18/12 FOR VPC REQUESTED BY CALVIN
  ELSEIF w_call = 'VPC'.
    it_item-costcenter = '0000055101'.
** END ON 01/18/2012
  ELSE.
    it_item-costcenter = '0000033003'.
  ENDIF.
  it_item-mvt_ind       = ' '.
  APPEND it_item.
  CLEAR it_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header       = it_header
      goodsmvt_code         = code
*     TESTRUN               = ' '
    IMPORTING
*     GOODSMVT_HEADRET      =
      materialdocument      = mdoc
      matdocumentyear       = myear
    TABLES
      goodsmvt_item         = it_item
*     GOODSMVT_SERIALNUMBER =
      return                = it_ret.


  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
             wait          = 'X'
*           IMPORTING
*             RETURN        =
            .

  IF mdoc IS INITIAL.
    p_it_tline-tdformat = '*' .
    CONCATENATE 'ERROR: ' it_ret-message INTO p_it_tline-tdline.
    APPEND p_it_tline.
    CLEAR p_it_tline.
    eflag = 'X'.
  ELSE.
    p_it_tline-tdformat = '*' .
    CONCATENATE '@1 Material DOC:' mdoc myear INTO p_it_tline-tdline
           SEPARATED BY space.
*    CONCATENATE '@1 Material DOC:' MDOC INTO P_IT_TLINE-TDLINE.
    APPEND p_it_tline.
    CLEAR p_it_tline.
  ENDIF.

  CLEAR v_uom.

ENDFORM.                    " CREAT_MVT_201
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CAUSE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_cause_text.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMNUM'
                                qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              record-QMGRP_002.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              record-QMCOD_003.
*perform bdc_field       using 'RQM00-MATNR'
*                              record-MATNR_004.
*perform bdc_field       using 'RQM00-MAWERK'
*                              record-MAWERK_005.
*perform bdc_field       using 'VIQMEL-RKMNG'
*                              record-RKMNG_006.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              record-BZMNG_007.
*perform bdc_field       using 'BDC_CURSOR'
*                              'VIQMUR-URTXT'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              record-OTGRP_008.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              record-OTEIL_009.
*perform bdc_field       using 'VIQMFE-FEGRP'
*                              record-FEGRP_010.
*perform bdc_field       using 'VIQMFE-FECOD'
*                              record-FECOD_011.
*perform bdc_field       using 'VIQMUR-URCOD'
*                              record-URCOD_012.
*perform bdc_field       using 'VIQMUR-URGRP'
*                              record-URGRP_013.
  PERFORM bdc_field       USING 'VIQMUR-URTXT'
                                cuasetxt.
  PERFORM bdc_transaction1 USING 'QM02'.

ENDFORM.                    " UPDATE_CAUSE_TEXT
*&---------------------------------------------------------------------*
*&      Form  SET_REORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_reorder USING p_qmnum.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QMNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMNUM'
                                p_qmnum.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VWML'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '8040'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMNAM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=WEIT'.
  PERFORM bdc_field       USING 'VIQMEL-QMNAM'
                                'REORDER'.
*perform bdc_field       using 'VIQMEL-QMDAT'
*                              record-QMDAT_011.
*perform bdc_field       using 'VIQMEL-MZEIT'
*                              record-MZEIT_012.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.
*  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'
*                                  CUASETXT.

  PERFORM bdc_transaction1 USING 'QM02'.

ENDFORM.                    " SET_REORDER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_1005.
  CLEAR eflag.

  DATA: v_lgtyp LIKE pkhd-lgtyp,
        v_lgpla LIKE pkhd-lgpla,
        v_vspvb LIKE marc-vspvb,
        v_lgber LIKE lagp-lgber,
        v_verme LIKE lqua-verme,
        v_negat TYPE t331-negat,
        v_qmnum LIKE qmel-qmnum,
        v_ltkze LIKE mlgn-ltkze.

  DATA: v_sub(20) TYPE c,
  l_atwrt LIKE ausp-atwrt,
  l_mdoc_matl LIKE bapi2017_gm_head_ret-mat_doc.

  DATA: it_tline TYPE tline OCCURS 0 WITH HEADER LINE.

  CLEAR: it_tline, it_tline[].
  IF rkmng IS INITIAL.
    MESSAGE e026 WITH 'Quantity is required'.
  ENDIF.
  IF qmgrp IS INITIAL.
    MESSAGE e026 WITH 'Work Center is required'.
  ENDIF.
  IF c3 IS INITIAL.
    MESSAGE e026 WITH 'Comment field is required'.
  ENDIF.
  IF rm07m-grund IS INITIAL.
    MESSAGE e026 WITH 'Cost Center is required'.
  ENDIF.

  IF NOT itobattr-equnr IS INITIAL.
    l_equnr = itobattr-equnr.
    IF l_equnr+3(1) = ' '.
      CONCATENATE l_equnr+0(3) l_equnr+4(14) INTO l_equnr.
      itobattr-equnr = l_equnr.
    ENDIF.
    SELECT SINGLE equnr INTO l_equnr
    FROM equi
      WHERE equnr = l_equnr.
    IF sy-subrc = 0.
    ELSE.
      MESSAGE e026 WITH 'Invalid body number entered'.
    ENDIF.
  ENDIF.

** Added on 09/15/09 by furong; Check RP>=19
  IF w_call = 'VPC'.
    SELECT SINGLE atwrt INTO l_atwrt
      FROM ausp AS a
      INNER JOIN cabn AS b
      ON a~atinn = b~atinn
      WHERE a~objek = itobattr-equnr
        AND klart = '002'
        AND atnam = 'P_RP_STATUS'.

    IF l_atwrt < 19.
      MESSAGE e026 WITH 'RP status is invalid for scrapping'.
    ENDIF.
  ENDIF.

  MESSAGE i026 WITH 'Material will be consumed  from HMMA inventory'.

  IF t001l-lgort = 'P400'.

    PERFORM creat_mvt_201 TABLES it_tline   " creation of movement
                          USING itobattr-equnr.

    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                     WHERE matnr = matnr
                                       AND werks = marc-werks.

    SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                               WHERE matnr = matnr
                                 AND werks = marc-werks
                                 AND prvbe = v_vspvb.

    IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).

*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
      MOVE '999' TO v_lgtyp.
    ELSE.
      IF rmmg1-lgnum IS INITIAL.
        rmmg1-lgnum = 'P01'.
      ENDIF.
      SELECT SINGLE lgber INTO v_lgber FROM lagp
                                       WHERE lgnum = rmmg1-lgnum
                                         AND lgtyp = v_lgtyp
                                         AND lgpla = v_lgpla.

      SELECT SINGLE verme INTO v_verme FROM lqua
                            WHERE lgnum = rmmg1-lgnum
                              AND matnr = matnr
                              AND lgtyp = v_lgtyp
                              AND lgpla = v_lgpla.
      IF v_verme < rkmng.
        SELECT SINGLE negat FROM t331 INTO v_negat
                    WHERE lgnum = rmmg1-lgnum AND lgtyp = v_lgtyp.
        IF v_negat = 'X'.
        ELSE.
*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO v_lgpla.
          MOVE '999' TO v_lgtyp.
        ENDIF.
      ENDIF.
    ENDIF.

    l_mdoc_matl = mdoc.

** Furong on 07/19/11
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                            USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*    ENDIF.
** End on 07/19/11

*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.
    PERFORM creat_mvt_201 TABLES it_tline     " creation of movement
                                 USING itobattr-equnr.
    l_mdoc_matl = mdoc.
  ENDIF.

  CLEAR eflag.

  READ TABLE it_tline INDEX 1.
  MESSAGE s026 WITH it_tline-tdline.


********  Printing Label ********
  mdoc = l_mdoc_matl.

  IF mdoc IS INITIAL.
  ELSE.
    PERFORM print_label_1.
*    REFRESH: IT_QMNUM.
*    CLEAR: IT_QMNUM.
*    IT_QMNUM-QMNUM = MDOC.
*    IT_QMNUM-MATNR = MATNR.
*    IT_QMNUM-MAKTX = QMCOD.
*    IT_QMNUM-RKMNG = RKMNG.
*    IT_QMNUM-ZERDAT = SY-DATUM.
*    IT_QMNUM-ZREASON = RM07M-GRUND.
*    IT_QMNUM-OTGRP = W_CALL.
*    IT_QMNUM-TXTCDUR = QMGRP.  " QPCT-KURZTEXT.
**    IT_QMNUM-ZROOTWC LIKE QPCT-KURZTEXT.
*    APPEND IT_QMNUM.
**    PERFORM PRINT_LABEL.
*    PERFORM PRINT_LABEL_OTHERS.
  ENDIF.
*****   Send mail logic  ********

*  IF T001L-LGORT = 'P500'.
*    V_SUB = 'Supplier Handling Damage at HMMA'.
*    PERFORM SEND_MAIL_1004 USING V_SUB.
*  ENDIF.

  CLEAR: v_lgtyp, v_lgpla, v_vspvb, v_lgber, v_sub, v_verme,
         v_qmnum,v_ltkze,v_qmnum,v_negat,it_tline, it_tline[].

ENDFORM.                    " PROCESS_1005
*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL_other
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_label_other.

  SET PARAMETER ID 'MBN' FIELD mdoc.   "IT_QMNUM-QMNUM.
  SET PARAMETER ID 'MAT' FIELD it_qmnum-matnr.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
  REFRESH it_qmnum.
  it_qmnum-matnr = matnr.
  it_qmnum-qmnum = mdoc.
  it_qmnum-rkmng = rkmng.

  SELECT SINGLE meins INTO it_qmnum-zmeins
  FROM mara WHERE matnr = matnr.


*   it_QMNUM-MAKTX
*  IT_QMNUM-MATKL
*IT_QMNUM-PDFBC
*IT_QMNUM-TXTCDUR
*IT_QMNUM-ZERDAT
*
*iT_QMNUM-ZREASON
*IT_QMNUM-ZROOTWC

  EXPORT it_qmnum TO MEMORY ID 'M1'.
  SUBMIT zqmscrap_label AND RETURN.

ENDFORM.                    " PRINT_LABEL_other
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_box.
  DATA: xname    TYPE vrm_id,
      xlist    TYPE vrm_values,
      xvalue   LIKE LINE OF xlist,
      l_cn LIKE xvalue-key.

  DATA: lt_temp LIKE TABLE OF t157e WITH HEADER LINE.

  CLEAR : xlist[] , xvalue.

  SELECT grund INTO CORRESPONDING FIELDS OF TABLE lt_temp
    FROM t157e
    WHERE spras ='EN'
      AND bwart ='201'
      AND grund > '9000'.

  l_cn = '1'.
  LOOP AT lt_temp.
    xvalue-text = lt_temp-grund.
    xvalue-key  = l_cn.
    APPEND xvalue TO xlist.
    l_cn = l_cn + 1.
  ENDLOOP.

  xvalue-text = 'ALL'.
  xvalue-key  = '4'.
  APPEND xvalue TO xlist .


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'RM07M-GRUND'
      values          = xlist
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.


  READ TABLE xlist INTO xvalue  INDEX 1.
  rm07m-grund = xvalue-text.

ENDFORM.                    " LIST_BOX
*&---------------------------------------------------------------------*
*&      Form  PRINT_LABEL_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_label_others.
* SET PARAMETER ID 'IQM' FIELD IT_QMNUM-QMNUM.
  SET PARAMETER ID 'MAT' FIELD it_qmnum-matnr.
*it_qmnum-QMNUM = QMNUM.
*append it_qmnum.
*  SORT IT_QMNUM BY QMNUM.
  DELETE ADJACENT DUPLICATES FROM it_qmnum COMPARING qmnum.
  EXPORT it_qmnum TO MEMORY ID 'M2'.
  SUBMIT zqmscrap_label_others AND RETURN.
*****end code to pass variables

ENDFORM.                    " PRINT_LABEL_OTHERS
*&---------------------------------------------------------------------*
*&      Form  print_label_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_label_1.
  REFRESH: it_qmnum.
  CLEAR: it_qmnum.
  it_qmnum-qmnum = mdoc.
  it_qmnum-matnr = matnr.
  it_qmnum-maktx = qmcod.
  it_qmnum-rkmng = rkmng.
  it_qmnum-zerdat = sy-datum.
  it_qmnum-zreason = rm07m-grund.
  it_qmnum-otgrp = w_call.
  it_qmnum-txtcdur = qmgrp.  " QPCT-KURZTEXT.
*    IT_QMNUM-ZROOTWC LIKE QPCT-KURZTEXT.
  APPEND it_qmnum.
*    PERFORM PRINT_LABEL.
  PERFORM print_label_others.
ENDFORM.                    " print_label_1
*&---------------------------------------------------------------------*
*&      Form  clear_valuse_1005_1006
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_valuse_1005_1006.
  CLEAR: qmgrp, qmcod, rkmng,rm07m-grund.
ENDFORM.                    " clear_valuse_1005_1006
*&---------------------------------------------------------------------*
*&      Form  FIND_MDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_mdoc.
  DATA: it_reptag LIKE TABLE OF zscrap_reprinto WITH HEADER LINE.
*  DATA:  L_STAT LIKE JEST-STAT,
*         L_TXT30 LIKE TJ30T-TXT30,
*         L_STSMA LIKE JSTO-STSMA,
  DATA:  l_date LIKE sy-datum,
         l_glacc LIKE mseg-sakto.

  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA:  ret_tab LIKE ddshretval OCCURS 0 WITH HEADER LINE,
         it_dsel LIKE dselc OCCURS 0 WITH HEADER LINE.

  RANGES: r_grund FOR mseg-grund.

  dyname = 'SAPMZSCRAP'.
  dynumb = '1802'.
  IF w_call = 'SHD'.
    l_glacc = '123205'.
  ELSE.
    l_glacc = '608310'.
  ENDIF.

  r_grund-option = 'BT'.
  r_grund-sign = 'I'.
  r_grund-low = '9006'.
  r_grund-high = '9014'.
  APPEND  r_grund.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = mblnr
    IMPORTING
      output = mblnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_glacc
    IMPORTING
      output = l_glacc.

  IF mblnr IS INITIAL.
    IF p_matnr IS INITIAL.
      IF p_nuser IS INITIAL.
        SELECT a~mblnr matnr zbudat menge usnam grund
         INTO CORRESPONDING FIELDS OF TABLE it_reptag
           FROM mseg AS a
           INNER JOIN mkpf AS b
           ON a~mblnr = b~mblnr
          WHERE bwart = '201'
            AND grund IN r_grund
            AND sakto = l_glacc.
      ELSE.
        SELECT a~mblnr matnr zbudat menge usnam grund
        INTO CORRESPONDING FIELDS OF TABLE it_reptag
          FROM mseg AS a
          INNER JOIN mkpf AS b
          ON a~mblnr = b~mblnr
           WHERE usnam = p_nuser
               AND bwart = '201'
            AND grund IN r_grund
            AND sakto = l_glacc.
      ENDIF.
      IF NOT mseg-zbudat IS INITIAL.
*        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*             EXPORTING
*                  INPUT  = QMEL-QMDAT
*             IMPORTING
*                  OUTPUT = L_DATE.
        DELETE it_reptag WHERE zbudat <> mseg-zbudat.
      ENDIF.
    ELSE.
      DO.
        REPLACE '*' WITH '%' INTO p_matnr.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDDO.
      IF  ( mseg-zbudat = '00000000' OR mseg-zbudat = ' ' )
          AND p_nuser IS INITIAL.
        SELECT a~mblnr matnr zbudat menge usnam grund
          INTO CORRESPONDING FIELDS OF TABLE it_reptag
            FROM mseg AS a
            INNER JOIN mkpf AS b
            ON a~mblnr = b~mblnr
            WHERE bwart = '201'
           AND grund IN r_grund
           AND sakto = l_glacc
           AND matnr LIKE p_matnr.
      ELSE.
        IF  mseg-zbudat = '00000000' OR mseg-zbudat = ' '.
          DO.
            REPLACE '*' WITH '%' INTO p_nuser.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
          ENDDO.

          SELECT a~mblnr matnr zbudat menge usnam grund
          INTO CORRESPONDING FIELDS OF TABLE it_reptag
            FROM mseg AS a
            INNER JOIN mkpf AS b
            ON a~mblnr = b~mblnr
             WHERE usnam = p_nuser
                 AND bwart = '201'
              AND grund IN r_grund
              AND sakto = l_glacc
              AND matnr LIKE p_matnr.

        ELSE.
          IF p_nuser IS INITIAL.
            SELECT a~mblnr matnr zbudat menge usnam grund
            INTO CORRESPONDING FIELDS OF TABLE it_reptag
            FROM mseg AS a
            INNER JOIN mkpf AS b
            ON a~mblnr = b~mblnr
                WHERE bwart = '201'
               AND grund IN r_grund
               AND sakto = l_glacc
                AND  matnr LIKE p_matnr
                  AND zbudat = mseg-zbudat.
          ELSE.
            DO.
              REPLACE '*' WITH '%' INTO p_nuser.
              IF sy-subrc <> 0.
                EXIT.
              ENDIF.
            ENDDO.
*            CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                 EXPORTING
*                      INPUT  = QMEL-QMDAT
*                 IMPORTING
*                      OUTPUT = L_DATE.
            SELECT a~mblnr matnr zbudat menge usnam grund
              INTO CORRESPONDING FIELDS OF TABLE it_reptag
              FROM mseg AS a
              INNER JOIN mkpf AS b
              ON a~mblnr = b~mblnr
              WHERE usnam = p_nuser
                AND bwart = '201'
                AND grund IN r_grund
                AND sakto = l_glacc
                AND matnr LIKE p_matnr
                AND zbudat = mseg-zbudat.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    DO.
      REPLACE '*' WITH '%' INTO p_qmnum.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDDO.
    SELECT a~mblnr matnr zbudat menge usnam grund
       INTO CORRESPONDING FIELDS OF TABLE it_reptag
         FROM mseg AS a
         INNER JOIN mkpf AS b
         ON a~mblnr = b~mblnr
       WHERE bwart = '201'
         AND grund IN r_grund
         AND sakto = l_glacc
         AND a~mblnr LIKE mblnr.
  ENDIF.

*  LOOP AT IT_REPTAG.
*    SELECT SINGLE STAT FROM JEST INTO L_STAT
*                       WHERE OBJNR = IT_REPTAG-OBJNR
*                         AND INACT = ' '.
*    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
*                       WHERE OBJNR = JEST-OBJNR.
*    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
*                       WHERE STSMA = L_STSMA
*                         AND ESTAT = L_STAT.
*    IT_REPTAG-TXT30 = L_TXT30.
*    MODIFY IT_REPTAG.
*    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
*  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM it_reptag.

  it_dsel-fldname = 'MBLNR'.
  it_dsel-dyfldname = 'MBLNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'P_MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'GRUND'.
  it_dsel-dyfldname = 'MSEG-GRUND'.
  APPEND it_dsel.

  it_dsel-fldname = 'ZBUDAT'.
  it_dsel-dyfldname = 'MSEG-ZBUDAT'.
  APPEND it_dsel.

  it_dsel-fldname = 'USNAM'.
  it_dsel-dyfldname = 'P_NUSER'.
  APPEND it_dsel.

  it_dsel-fldname = 'MENGE'.
  it_dsel-dyfldname = 'MSEG-MENGE'.
  APPEND it_dsel.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    ddic_structure         = 'ZSCRAP_REPRINTO'
       retfield               = 'MBLNR'
*   PVALKEY                = ' '
     dynpprog               = dyname
     dynpnr                 = dynumb
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     value_tab              = it_reptag
*    field_tab             = fld_tab
     return_tab             = ret_tab
     dynpfld_mapping        = it_dsel
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ret_tab.
    CASE ret_tab-fieldname.
      WHEN 'MBLNR'.
        mblnr = ret_tab-fieldval.
      WHEN 'MATNR'.
        p_matnr = ret_tab-fieldval.
      WHEN 'USNAM'.
        p_nuser = ret_tab-fieldval.
      WHEN 'ZBUDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            input  = ret_tab-fieldval
          IMPORTING
            output = mseg-zbudat.
      WHEN 'MENGE'.
        mseg-menge = ret_tab-fieldval.
      WHEN 'GRUND'.
        mseg-grund = ret_tab-fieldval.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FIND_MDOC
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MDOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_mdoc.
  DATA: it_reptag LIKE TABLE OF zscrap_reprinto WITH HEADER LINE.
*  DATA:  L_STAT LIKE JEST-STAT,
*         L_TXT30 LIKE TJ30T-TXT30,
*         L_STSMA LIKE JSTO-STSMA,
  DATA:  l_date LIKE sy-datum,
         l_glacc LIKE mseg-sakto.

  DATA : dyname LIKE d020s-prog , dynumb LIKE d020s-dnum .
  DATA:  ret_tab LIKE ddshretval OCCURS 0 WITH HEADER LINE,
         it_dsel LIKE dselc OCCURS 0 WITH HEADER LINE.

  RANGES: r_grund FOR mseg-grund.

  dyname = 'SAPMZSCRAP'.
  dynumb = '1802'.
  IF w_call = 'SHD'.
    l_glacc = '123205'.
  ELSE.
    l_glacc = '608310'.
  ENDIF.

  r_grund-option = 'BT'.
  r_grund-sign = 'I'.
  r_grund-low = '9006'.
  r_grund-high = '9014'.
  APPEND  r_grund.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = mblnr
    IMPORTING
      output = mblnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_glacc
    IMPORTING
      output = l_glacc.

  IF mblnr IS INITIAL.
    IF p_matnr IS INITIAL.
      IF p_nuser IS INITIAL AND mseg-zbudat IS INITIAL.
        MESSAGE i026 WITH 'No data selected'.
        EXIT.
      ENDIF.
      SELECT a~mblnr matnr zbudat menge usnam grund
        INTO CORRESPONDING FIELDS OF TABLE it_reptag
          FROM mseg AS a
          INNER JOIN mkpf AS b
          ON a~mblnr = b~mblnr
        WHERE bwart = '201'
          AND grund IN r_grund
          AND sakto = l_glacc.
    ELSE.
      SELECT a~mblnr matnr zbudat menge usnam grund
       INTO CORRESPONDING FIELDS OF TABLE it_reptag
         FROM mseg AS a
         INNER JOIN mkpf AS b
         ON a~mblnr = b~mblnr
     WHERE bwart = '201'
       AND matnr = p_matnr
       AND grund IN r_grund
       AND sakto = l_glacc.
    ENDIF.
    IF NOT p_nuser IS INITIAL.
      DELETE it_reptag WHERE usnam <> p_nuser.
    ENDIF.
    IF NOT mseg-zbudat IS INITIAL.
      DELETE it_reptag WHERE zbudat <> mseg-zbudat.
    ENDIF.
  ELSE.
    SELECT a~mblnr matnr zbudat menge usnam grund
       INTO CORRESPONDING FIELDS OF TABLE it_reptag
         FROM mseg AS a
         INNER JOIN mkpf AS b
         ON a~mblnr = b~mblnr
         WHERE b~mblnr = mblnr.

    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
  ENDIF.

*  LOOP AT IT_REPTAG.
*    SELECT SINGLE STAT FROM JEST INTO L_STAT
*                       WHERE OBJNR = IT_REPTAG-OBJNR
*                         AND INACT = ' '.
*    SELECT SINGLE STSMA FROM JSTO INTO L_STSMA
*                       WHERE OBJNR = JEST-OBJNR.
*    SELECT SINGLE TXT30 FROM TJ30T INTO L_TXT30
*                       WHERE STSMA = L_STSMA
*                         AND ESTAT = L_STAT.
*    IT_REPTAG-TXT30 = L_TXT30.
*    MODIFY IT_REPTAG.
*    CLEAR: IT_REPTAG, L_STAT, L_TXT30, L_STSMA.
*  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM it_reptag.

  it_dsel-fldname = 'MBLNR'.
  it_dsel-dyfldname = 'MBLNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'MATNR'.
  it_dsel-dyfldname = 'P_MATNR'.
  APPEND it_dsel.

  it_dsel-fldname = 'GRUND'.
  it_dsel-dyfldname = 'MSEG-GRUND'.
  APPEND it_dsel.

  it_dsel-fldname = 'ZBUDAT'.
  it_dsel-dyfldname = 'MSEG-ZBUDAT'.
  APPEND it_dsel.

  it_dsel-fldname = 'USNAM'.
  it_dsel-dyfldname = 'P_NUSER'.
  APPEND it_dsel.

  it_dsel-fldname = 'MENGE'.
  it_dsel-dyfldname = 'MSEG-MENGE'.
  APPEND it_dsel.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
     EXPORTING
    ddic_structure         = 'ZSCRAP_REPRINTO'
       retfield               = 'MBLNR'
*   PVALKEY                = ' '
     dynpprog               = dyname
     dynpnr                 = dynumb
*   DYNPROFIELD            = ' '
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
     value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
     TABLES
     value_tab              = it_reptag
*    field_tab             = fld_tab
     return_tab             = ret_tab
     dynpfld_mapping        = it_dsel
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
             .

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ret_tab.
    CASE ret_tab-fieldname.
      WHEN 'MBLNR'.
        mblnr = ret_tab-fieldval.
      WHEN 'MATNR'.
        p_matnr = ret_tab-fieldval.
      WHEN 'USNAM'.
        p_nuser = ret_tab-fieldval.
      WHEN 'ZBUDAT'.
        CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
          EXPORTING
            input  = ret_tab-fieldval
          IMPORTING
            output = mseg-zbudat.
      WHEN 'MENGE'.
        mseg-menge = ret_tab-fieldval.
      WHEN 'GRUND'.
        mseg-grund = ret_tab-fieldval.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " DISPLAY_MDOC
*&---------------------------------------------------------------------*
*&      Form  CLEAR_1802
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_1802.
  CLEAR: mblnr, p_matnr, p_nuser, qmgrp, qmcod,
        mseg-zbudat, mseg-grund, mseg-menge.

ENDFORM.                    " CLEAR_1802
*&---------------------------------------------------------------------*
*&      Form  REPRINT_LABEL_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reprint_label_others.
  DATA: l_mess(255).
  IF qmgrp IS INITIAL.
    MESSAGE e026 WITH 'Please input work center'.
  ENDIF.
  IF mblnr IS INITIAL AND
     p_matnr IS INITIAL.
    EXIT.
  ELSEIF mblnr IS INITIAL OR p_matnr IS INITIAL.
    PERFORM display_mdoc.
  ENDIF.
  SET PARAMETER ID 'MAT' FIELD p_matnr.

  REFRESH: it_qmnum.
  CLEAR: it_qmnum.
  it_qmnum-qmnum = mblnr.
  it_qmnum-matnr = p_matnr.
  it_qmnum-maktx = qmcod.
  it_qmnum-rkmng = mseg-menge.
  it_qmnum-zerdat = sy-datum.
  it_qmnum-zreason = mseg-grund.
  it_qmnum-otgrp = w_call.
  it_qmnum-txtcdur = qmgrp.  " QPCT-KURZTEXT.

  APPEND it_qmnum.


  SET PARAMETER ID 'MAT' FIELD it_qmnum-matnr.

  DELETE ADJACENT DUPLICATES FROM it_qmnum COMPARING qmnum.
  EXPORT it_qmnum TO MEMORY ID 'M2'.

  DO p_copy TIMES.
    SUBMIT zqmscrap_label_others AND RETURN.
    IMPORT l_mess FROM MEMORY ID 'ME'.
    IF NOT l_mess IS INITIAL.
      MESSAGE i026 WITH l_mess.
    ELSE.
      MESSAGE s026 WITH 'Reprint label complete'.
    ENDIF.
    FREE MEMORY ID 'ME'.
  ENDDO.

*  CLEAR: IT_QMNUM, IT_QMNUM[].
*  IT_QMNUM-QMNUM = P_QMNUM.
*  IT_QMNUM-MATNR = P_MATNR.
*  APPEND IT_QMNUM.
*  EXPORT IT_QMNUM TO MEMORY ID 'M1'.
*  DO P_COPY TIMES.
*    SUBMIT ZQMSCRAP_LABEL AND RETURN.
*    IMPORT L_MESS FROM MEMORY ID 'ME'.
*    IF NOT L_MESS IS INITIAL.
*      MESSAGE I026 WITH L_MESS.
*    ELSE.
*      MESSAGE S026 WITH 'Reprint label complete'.
*    ENDIF.
*    FREE MEMORY ID 'ME'.
*  ENDDO.

ENDFORM.                    " REPRINT_LABEL_others
*&---------------------------------------------------------------------*
*&      Form  CALL_1802
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_1202.
  DATA: l_txt04 LIKE tj02t-txt04,
        l_qmart LIKE qmel-qmart,
         l_stat LIKE tj30t-estat.

  SELECT SINGLE lifnum qmart stat INTO (lifnr, l_qmart, l_stat)
         FROM qmel AS a
         INNER JOIN jest AS d
         ON a~objnr = d~objnr
         WHERE a~qmnum = qmnum
           AND inact = ' '.


*  SELECT SINGLE LIFNUM TXT04 QMART INTO (LIFNR, L_TXT04, L_QMART)
*         FROM QMEL AS A
*         INNER JOIN JEST AS D
*         ON A~OBJNR = D~OBJNR
*         INNER JOIN TJ30T AS E
*         ON D~STAT = E~ESTAT
*         WHERE A~QMNUM = QMNUM
*           AND E~STSMA = 'ZQNSCRP1'
*           AND INACT = ' '.

  IF l_qmart = 'Q4'.
    IF lifnr IS INITIAL.
      MESSAGE i026 WITH 'Venodr not available'.
      EXIT.
    ENDIF.
  ELSE.
    SELECT SINGLE txt04 INTO l_txt04
        FROM tj30t
         WHERE stsma = 'ZQNSCRP1'
             AND estat = l_stat.

    IF l_txt04 <> 'VEND'.
      MESSAGE i026 WITH 'User status must be VEND'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE txt04 INTO l_txt04
        FROM qmel AS a
        INNER JOIN jest AS d
        ON a~objnr = d~objnr
        INNER JOIN tj02t AS e
        ON d~stat = e~istat
        WHERE a~qmnum = qmnum
           AND spras = 'EN'
          AND inact = ' '
          AND txt04 = 'DLFL'.

  IF sy-subrc = 0.
    MESSAGE i026 WITH 'System status CANNOT be DLFL'.
    EXIT.
  ENDIF.

  LEAVE TO SCREEN '1202'.
ENDFORM.                                                    " CALL_1802
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PRIMARY_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_primary_value.
  SELECT SINGLE *  FROM qmel
   WHERE qmnum = qmnum.
  IF sy-subrc  = 0.
    lfa1-lifnr = qmel-lifnum.
    qmgrp = qmel-qmgrp.
    qmcod = qmel-qmcod.
    itobattr-equnr = qmel-kdmat.

    SELECT SINGLE otgrp oteil INTO (otgrp, oteil)
     FROM qmfe
     WHERE qmnum = qmnum.

    fegrp = '0'.
    fecod = '0015'.
    urgrp = 'CAUS'.
    urcod = '04'.
  ENDIF.
ENDFORM.                    " UPDATE_PRIMARY_VALUE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_1203.
  CLEAR eflag.

  DATA: v_lgtyp LIKE pkhd-lgtyp,
        v_lgpla LIKE pkhd-lgpla,
        v_vspvb LIKE marc-vspvb,
        v_lgber LIKE lagp-lgber,
        v_verme LIKE lqua-verme,
        v_negat TYPE t331-negat,
        v_qmnum LIKE qmel-qmnum,
        v_ltkze LIKE mlgn-ltkze.

  DATA: v_sub(20) TYPE c.

  DATA: it_tline TYPE tline OCCURS 0 WITH HEADER LINE.

  CLEAR: it_tline, it_tline[].

  PERFORM code_check.

  it_tline-tdformat = '*' .
  CONCATENATE 'Plant' marc-werks INTO it_tline-tdline.
  APPEND it_tline.
  CLEAR it_tline.

  it_tline-tdformat = '*' .
  CONCATENATE 'Storage Location' t001l-lgort INTO it_tline-tdline.
  APPEND it_tline.
  CLEAR it_tline.

  IF t001l-lgort = 'P400'.

    PERFORM creat_mvt TABLES it_tline.   " creation of movement


    SELECT SINGLE vspvb INTO v_vspvb FROM marc
                                     WHERE matnr = matnr
                                       AND werks = marc-werks.

    SELECT SINGLE lgtyp lgpla  INTO (v_lgtyp, v_lgpla) FROM pkhd
                               WHERE matnr = matnr
                                 AND werks = marc-werks
                                 AND prvbe = v_vspvb.

    IF ( v_lgtyp IS INITIAL AND v_lgpla IS INITIAL ).
** Changed by Furong on 06/21/10
*      MOVE 'INV_COUNT' TO V_LGPLA.
      MOVE 'HMMA ADJ' TO v_lgpla.
** End of change
      MOVE '999' TO v_lgtyp.

    ELSE.

      SELECT SINGLE lgber INTO v_lgber FROM lagp
                                       WHERE lgnum = rmmg1-lgnum
                                         AND lgtyp = v_lgtyp
                                         AND lgpla = v_lgpla.


      SELECT SINGLE verme INTO v_verme FROM lqua
                            WHERE lgnum = rmmg1-lgnum
                              AND matnr = matnr
                              AND lgtyp = v_lgtyp
                              AND lgpla = v_lgpla.

      IF v_verme < rkmng.

        SELECT SINGLE negat FROM t331 INTO v_negat
                    WHERE lgnum = rmmg1-lgnum AND lgtyp = v_lgtyp.

        IF v_negat = 'X'.

        ELSE.

*          MOVE 'INV_COUNT' TO V_LGPLA.
          MOVE 'HMMA ADJ' TO v_lgpla.
          MOVE '999' TO v_lgtyp.
        ENDIF.

      ENDIF.

    ENDIF.

*S__BY PAUL
*    IF EFLAG IS INITIAL.
*
*      PERFORM CREATE_TO TABLES IT_TLINE     " creation of transfer
*order
*                        USING V_VSPVB V_LGTYP V_LGPLA V_LGBER.
*
*    ENDIF.

** Changed by Furong on 10/19/09
*  ELSEIF T001L-LGORT = 'P500'.
  ELSE.
** End of change on 10/19/09

    PERFORM creat_mvt TABLES it_tline.     " creation of movement

  ENDIF.

  CLEAR eflag.

  PERFORM creat_notif_1203.                " creation of notification


  READ TABLE it_qmnum INDEX 1.
  v_qmnum = it_qmnum-qmnum.
  PERFORM delete_longtext USING v_qmnum.

  PERFORM update_notif_text TABLES it_tline    " updating QN# long text
                            USING v_qmnum.
  READ TABLE it_tline INDEX 3.

  IF it_tline-tdline+0(5) = 'ERROR'.
    PERFORM update_notif_status USING v_qmnum. " updating QN# status
    READ TABLE it_qmnum INDEX 1.
    it_qmnum-flag = 'X'.
    MODIFY it_qmnum INDEX 1.

  ELSE.
    READ TABLE it_tline INDEX 4.
    IF it_tline-tdline+0(5) = 'ERROR'.
      PERFORM update_notif_status USING v_qmnum.  " updating QN# status
      READ TABLE it_qmnum INDEX 1.
      it_qmnum-flag = 'X'.
      MODIFY it_qmnum INDEX 1.

    ELSE.
** Changed by Furong on 03/03/09
      IF fegrp = '4' AND urgrp <> '04'.

        CLEAR: it_tline, it_tline[].
        PERFORM update_nopr_hmma TABLES it_tline
                                 USING v_qmnum.
        WAIT UP TO 2 SECONDS.
        PERFORM update_notif_text TABLES it_tline   "updating QN# long text
                                                             USING v_qmnum.
      ENDIF.
** End of change
    ENDIF.
  ENDIF.

** Changed by Furong on 09/16/09
  IF w_ans = '1'.
    PERFORM set_reorder USING v_qmnum.
    CLEAR: w_ans.
  ENDIF.
** Changed by Furong on 11/04/09
*  QMNUM = V_QMNUM.
*  PERFORM UPDATE_CAUSE_TEXT.
** End of change on 11/04/09
** end of change

********  Printing Label ********

  PERFORM print_label.

  CLEAR: v_lgtyp, v_lgpla, v_vspvb, v_lgber, v_sub, v_verme,
         v_qmnum,v_ltkze,v_qmnum,v_negat,it_tline, it_tline[].

*  IF SY-UNAME <> '101457'  or SY-UNAME <> '100794'.
*    CALL 'SYST_LOGOFF'.
*  ELSE.
*    LEAVE PROGRAM.
*  ENDIF.

ENDFORM.                    " PROCESS_1203
*&---------------------------------------------------------------------*
*&      Form  CREAT_NOTIF_1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM creat_notif_1203.
  DATA: l_date_c(8).
  REFRESH: bdcdata.

***************  Initial screen **************
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0200'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RIWO00-QMART'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'RIWO00-QMART'
*                                 RIWO00-QMART.
*
*****   Second screen   ***********************
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=10\TAB02'.
*  PERFORM BDC_FIELD       USING 'VIQMEL-QMGRP'
*                                 QMGRP.           " Author work centre
*  PERFORM BDC_FIELD       USING 'VIQMEL-QMCOD'
*                                 QMCOD.
*
*  PERFORM BDC_FIELD       USING 'RQM00-MATNR'     " material
*                                 MATNR.
*  PERFORM BDC_FIELD       USING 'RQM00-MAWERK'
*                                 MARC-WERKS.      " Plant
*  PERFORM BDC_FIELD       USING 'VIQMEL-RKMNG'
*                                 RKMNG.           " quantity
*
*
*** Changed by Furong on 02/10/10
*  IF NOT ITOBATTR-EQUNR IS INITIAL.
*    PERFORM BDC_FIELD       USING 'VIQMEL-KDMAT'
*                                   ITOBATTR-EQUNR.    " BODY NO
*  ENDIF.
*** End of change
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTGRP'
*                                 OTGRP.           " root cause
*  PERFORM BDC_FIELD       USING 'VIQMFE-OTEIL'
*                                 OTEIL.
*  PERFORM BDC_FIELD       USING 'VIQMFE-FEGRP'
*                                 FEGRP.           " Defect type
*  PERFORM BDC_FIELD       USING 'VIQMFE-FECOD'
*                                 FECOD.
*  PERFORM BDC_FIELD       USING 'VIQMUR-URCOD'    " cause code
*                                 URCOD.
*** Changed by Furong on 11/04/09
*  PERFORM BDC_FIELD       USING 'VIQMUR-URTXT'    " Cause text
*                                CUASETXT.
*** End of change
*  PERFORM BDC_FIELD       USING 'VIQMUR-URGRP'
*                                 URGRP.
*  IF MARC-XCHAR = 'X'.
*    PERFORM BDC_FIELD       USING 'VIQMEL-CHARG'
*                                   MSEG-CHARG.
*  ENDIF.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'IHPA-PARNR(02)'.
*  PERFORM BDC_FIELD       USING 'IHPA-PARVW(02)'
*                                'Z5'.
*  PERFORM BDC_FIELD       USING 'IHPA-PARNR(02)'
*                                 LFA1-LIFNR.
*** Changed by Furong on 02/10/10
*  IF NOT QMEL-ERDAT IS INITIAL.
*    WRITE: QMEL-ERDAT TO L_DATE_C MMDDYY.
*    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                  'VIQMEL-AUSVN'.
*    PERFORM BDC_FIELD       USING 'VIQMEL-AUSVN'
*                                  L_DATE_C.
*  ENDIF.
*** End of change
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=BUCH'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'IHPA-PARVW(03)'.
*

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RIWO00-QWRNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RIWO00-QMART'
                                'Q3'.           "record-QMART_001.
  PERFORM bdc_field       USING 'RIWO00-QWRNUM'
                                qmnum.          "record-QWRNUM_002.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=AWST'.
  PERFORM bdc_field       USING 'VIQMEL-QMGRP'
                                qmgrp.
  PERFORM bdc_field       USING 'VIQMEL-QMCOD'
                                qmcod.
  PERFORM bdc_field       USING 'RQM00-MATNR'
                                matnr.
  PERFORM bdc_field       USING 'RQM00-MAWERK'
                                marc-werks.
  IF NOT itobattr-equnr IS INITIAL.
    PERFORM bdc_field       USING 'VIQMEL-KDMAT'
                                  itobattr-equnr.
  ENDIF.
  PERFORM bdc_field       USING 'VIQMEL-RKMNG'
                                rkmng.
  PERFORM bdc_field       USING 'VIQMEL-BZMNG'
                                '0'.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              OTGRP.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              OTEIL.
  PERFORM bdc_field       USING 'VIQMFE-FEGRP'
                                fegrp.
  PERFORM bdc_field       USING 'VIQMFE-FECOD'
                                fecod.
  PERFORM bdc_field       USING 'VIQMUR-URCOD'
                                urcod.
  PERFORM bdc_field       USING 'VIQMUR-URGRP'
                                urgrp.
  PERFORM bdc_field       USING 'VIQMUR-URTXT'
                                cuasetxt.

** Changed by Furong on 07/14/14 (
  IF riwo00-qmart = 'Q3' OR  riwo00-qmart = 'Q4'.
   PERFORM bdc_field       USING 'ZSQM_CI_QMEL-QCODEGRP_LOC'
                                  ZLOCATION.
  ENDIF.
** )

  PERFORM bdc_dynpro      USING 'SAPLBSVA' '0201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'J_STMAINT-ANWSO(06)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OKAY'.
  PERFORM bdc_field       USING 'J_STMAINT-ANWSO(06)'
                                'X'.

  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=10\TAB02'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-QMGRP'.
  PERFORM bdc_dynpro      USING 'SAPLIQS0' '7200'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BUCH'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'VIQMEL-PRIOK'.
*perform bdc_field       using 'VIQMEL-STRMN'
*                              record-STRMN_032.
*perform bdc_field       using 'VIQMEL-STRUR'
*                              record-STRUR_033.
*perform bdc_field       using 'VIQMEL-LTRUR'
*                              record-LTRUR_034.
*perform bdc_field       using 'VIQMEL-AUSVN'
*                              record-AUSVN_035.
*perform bdc_field       using 'VIQMEL-AUZTV'
*                              record-AUZTV_036.

  PERFORM bdc_transaction1 USING 'QM01'.

ENDFORM.                    " CREAT_NOTIF_1203
*&---------------------------------------------------------------------*
*&      Form  DELETE_LONGTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_longtext USING p_qmnum.
  DATA: l_qmnum LIKE thead-tdname.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qmnum
    IMPORTING
      output = p_qmnum.

  l_qmnum = p_qmnum.
  CALL FUNCTION 'DELETE_TEXT'
    EXPORTING
      client          = sy-mandt
      id              = 'LTQM'
      language        = sy-langu
      name            = l_qmnum
      object          = 'QMEL'
      savemode_direct = 'X'
      textmemory_only = ' '
      local_cat       = ' '
    EXCEPTIONS
      not_found       = 1
      OTHERS          = 2.

ENDFORM. " DELETE_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SEARCH_HELP_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_KATALOGART  text
*      -->P_P_FIELDNAME  text
*      -->P_P_FIELDNAME1  text
*----------------------------------------------------------------------*
FORM display_search_help_read USING    p_katalogart
                                  p_fieldname
                                  p_fieldname1.

  DATA:
       l_tq15t                 LIKE tq15t,
       l_qpk1gr                LIKE qpk1gr,
       l_repid                 LIKE d020s-prog,
       l_dynnr                 LIKE sy-dynnr,
       l_qmgrp                 LIKE qmgrp.

  DATA: i_katalogart TYPE qpgr-katalogart,
        l_qpk1cd          LIKE qpk1cd,
        i_codegruppe LIKE  qpgr-codegruppe,
        i_code LIKE  qpcd-code VALUE '*',
        w_ans(1) TYPE c.

  DATA : t_codegrptab LIKE qpk1codegrp OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF l_dynfieldtab OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END   OF l_dynfieldtab.

  MOVE : sy-repid TO l_repid,
         sy-dynnr TO l_dynnr.



  IF p_fieldname = 'FEGRP'.


    MOVE 'QMGRP' TO l_dynfieldtab-fieldname.
    APPEND l_dynfieldtab.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname                         = l_repid
        dynumb                         = l_dynnr
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
      TABLES
        dynpfields                     = l_dynfieldtab
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    READ TABLE l_dynfieldtab INDEX 1.
    l_qmgrp = l_dynfieldtab-fieldvalue.


    CLEAR: l_dynfieldtab, l_dynfieldtab[].



    IF l_qmgrp = 'MXTX10' OR l_qmgrp = 'MXTX11' OR
       l_qmgrp = 'MXTX12' OR l_qmgrp = 'MXTX13' OR
       l_qmgrp = 'MXTX15' OR l_qmgrp = 'MXTX19' OR
       l_qmgrp = 'MXTX51' OR l_qmgrp = 'MXTX53'.

      t_codegrptab = '0'.
      APPEND t_codegrptab.
      t_codegrptab = '4'.
      APPEND t_codegrptab.
      t_codegrptab = '9'.
      APPEND t_codegrptab.
** Chnaged by Furong on 10/19/09
    ELSEIF l_qmgrp+0(4) = 'MXPX'.
      t_codegrptab = '0'.
      APPEND t_codegrptab.
      t_codegrptab = '1'.
      APPEND t_codegrptab.
      t_codegrptab = '2'.
      APPEND t_codegrptab.
      t_codegrptab = '3'.
      APPEND t_codegrptab.
      t_codegrptab = '9'.
      APPEND t_codegrptab.
** End of change
    ELSE.

      t_codegrptab = '*'.
      APPEND t_codegrptab.

    ENDIF.

    CLEAR : l_qmgrp.

  ELSE.

    t_codegrptab = '*'.
    APPEND t_codegrptab.

  ENDIF.

  i_katalogart = p_katalogart.

  CALL FUNCTION 'QPK1_GP_CODE_PICKUP'
    EXPORTING
      i_katalogart                 = i_katalogart
      i_codegruppe                 = i_codegruppe
      i_code                       = i_code
      i_sprache                    = sy-langu
      i_winx1                      = 10
      i_winx2                      = 68
      i_winy1                      = 5
      i_winy2                      = 27
*   I_DISPLAY_MODE               =
*   I_RETURN_IF_ONE              = 'X'
*   I_RETURN_IF_MANY             =
*   I_NO_USAGEINDICATION         =
*   I_NO_AUTHORITY_CHECK         =
    IMPORTING
      e_qpk1cd                     = l_qpk1cd
    TABLES
      t_codegrptab                 = t_codegrptab
* EXCEPTIONS
*   NO_MATCH_IN_RANGE            = 1
*   NO_USER_SELECTION            = 2
*   NO_AUTHORIZATION             = 3
*   NO_SELECTION_SPECIFIED       = 4
*   OBJECT_LOCKED                = 5
*   LOCK_ERROR                   = 6
*   OBJECT_MISSING               = 7
*   OTHERS                       = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  MOVE p_fieldname TO l_dynfieldtab-fieldname.
  MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
  APPEND l_dynfieldtab.

  MOVE p_fieldname1 TO l_dynfieldtab-fieldname.
  MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
  APPEND l_dynfieldtab.

  IF p_fieldname = 'QMGRP'.

    MOVE  'OTGRP' TO l_dynfieldtab-fieldname.
    MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE  'OTEIL' TO l_dynfieldtab-fieldname.
    MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

  ENDIF.

  IF p_fieldname = 'FEGRP' AND l_qpk1cd-codegruppe = '0'.

    MOVE 'URGRP' TO l_dynfieldtab-fieldname.
    MOVE  'CAUS' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE 'URCOD' TO l_dynfieldtab-fieldname.
    MOVE  '04' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    LOOP AT SCREEN.
      IF screen-name = 'URGRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF p_fieldname = 'FEGRP' AND l_qpk1cd-codegruppe = '4'.

    MOVE 'URGRP' TO l_dynfieldtab-fieldname.
    MOVE  'CAUS' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    MOVE 'URCOD' TO l_dynfieldtab-fieldname.
    MOVE  '02' TO l_dynfieldtab-fieldvalue.
    APPEND l_dynfieldtab.

    LOOP AT SCREEN.
      IF screen-name = 'URGRP'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

*  CALL FUNCTION 'DYNP_VALUES_UPDATE'
*       EXPORTING
*            DYNAME               = L_REPID
*            DYNUMB               = L_DYNNR
*       TABLES
*            DYNPFIELDS           = L_DYNFIELDTAB
*       EXCEPTIONS
*            INVALID_ABAPWORKAREA = 01
*            INVALID_DYNPROFIELD  = 02
*            INVALID_DYNPRONAME   = 03
*            INVALID_DYNPRONUMMER = 04
*            INVALID_REQUEST      = 05
*            NO_FIELDDESCRIPTION  = 06
*            UNDEFIND_ERROR       = 07.

*  IF SY-SUBRC <> 0.
*    MESSAGE S099(Q3) .
*    EXIT.
*  ENDIF.

ENDFORM.                    " DISPLAY_SEARCH_HELP_READ
*&---------------------------------------------------------------------*
*&      Form  get_var1203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_var1203.
  DATA: val_tab2 LIKE zscrap_var. " OCCURS 0 WITH HEADER LINE.


*  SELECT * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF TABLE VAL_TAB2

* IF SY-TCODE = 'ZSCRAP1_ENG'.
  IF marc-werks = 'E001'.
    SELECT SINGLE * FROM zscrap_var INTO CORRESPONDING FIELDS OF val_tab2
                 WHERE lgort = 'E499'.
  ELSEIF marc-werks = 'E002'.
    SELECT SINGLE * FROM zscrap_var INTO CORRESPONDING FIELDS OF val_tab2
                 WHERE lgort = 'N499'.
  ELSE.
    SELECT SINGLE * FROM zscrap_var INTO CORRESPONDING FIELDS OF val_tab2
               WHERE lgort = 'P499'.
  ENDIF.

*  SELECT SINGLE * FROM ZSCRAP_VAR INTO CORRESPONDING FIELDS OF VAL_TAB2
*.

*read table VAL_TAB2 index 1.

  rmmg1-lgnum = val_tab2-lgnum.

  bwart = val_tab2-bwart.

  rmmg1-lgort = val_tab2-lgort.


ENDFORM.                    " get_var1203
*&---------------------------------------------------------------------*
*&      Module  DISPALY_VALUES_CODEGRP_VH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dispaly_values_codegrp_vh INPUT.
  DATA : BEGIN OF value_table OCCURS 0,
         codegruppe  LIKE qpcd-codegruppe,
         END OF value_table.

  DATA : BEGIN OF scr_fields OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF scr_fields.

  SELECT DISTINCT codegruppe
    INTO TABLE value_table
     FROM qpcd
     WHERE katalogart = 'Q'.

  DELETE value_table WHERE codegruppe = ' '.

  REFRESH return_tab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODEGRP_VH'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'CODEGRP_VH'
      window_title    = 'Vehicle/Engine Group'
      value_org       = 'S'
    TABLES
      value_tab       = value_table
      return_tab      = return_tab
    EXCEPTIONS
      parameter_error = 1.

* READ TABLE RETURN_TAB INDEX 1.
*  IF SY-SUBRC EQ 0.
*
** Set values for Industry Type and Description.
*    SCR_FIELDS-FIELDNAME  = 'CODEGRP_VH'.
*    SCR_FIELDS-FIELDVALUE = RETURN_TAB-FIELDVAL.
*    APPEND SCR_FIELDS.
*
**  Update back Screen with Values
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*         EXPORTING
*              DYNAME               = SY-CPROG
*              DYNUMB               = SY-DYNNR
*         TABLES
*              DYNPFIELDS           = SCR_FIELDS
*         EXCEPTIONS
*              INVALID_ABAPWORKAREA = 1
*              INVALID_DYNPROFIELD  = 2
*              INVALID_DYNPRONAME   = 3
*              INVALID_DYNPRONUMMER = 4
*              INVALID_REQUEST      = 5
*              NO_FIELDDESCRIPTION  = 6
*              UNDEFIND_ERROR       = 7
*              OTHERS               = 8.
*  ENDIF.

ENDMODULE.                 " DISPALY_VALUES_CODEGRP_VH  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPALY_VALUES_CODE_VH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dispaly_values_code_vh INPUT.
  DATA : BEGIN OF value_tab1 OCCURS 0,
         code LIKE qpcd-code,
         END OF value_tab1.

  DATA : BEGIN OF l_dynfieldtab OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF l_dynfieldtab.

  MOVE 'CODEGRP_VH' TO l_dynfieldtab-fieldname.
  APPEND l_dynfieldtab.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                         = w_repid
      dynumb                         = w_dynnr
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
    TABLES
      dynpfields                     = l_dynfieldtab
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE l_dynfieldtab INDEX 1.
  codegrp_vh = l_dynfieldtab-fieldvalue.

  SELECT DISTINCT code
  INTO TABLE value_tab1
   FROM qpcd
   WHERE katalogart = 'Q'
     AND codegruppe = codegrp_vh.

  REFRESH return_tab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODE_VH'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'CODE_VH'
      window_title    = 'Vehicle/Engine Code'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab1
      return_tab      = return_tab
    EXCEPTIONS
      parameter_error = 1.

  READ TABLE return_tab INDEX 1.
  IF sy-subrc EQ 0.

* Set values for Industry Type and Description.
    scr_fields-fieldname  = 'CODE_VH'.
    scr_fields-fieldvalue = return_tab-fieldval.
    APPEND scr_fields.

*  Update back Screen with Values
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = scr_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
  ENDIF.

ENDMODULE.                 " DISPALY_VALUES_CODE_VH  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_LIMITATION_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_limitation_100 .

  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.
  DATA: l_rver LIKE somlreci1-receiver.
  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  SELECT SINGLE zemail INTO l_rver
  FROM ztqm_scrap_check
    WHERE ztype = 'MSC'
     AND zplant = marc-werks
      AND zuname = sy-uname.

  IF sy-subrc <> 0.

    SELECT SINGLE zemail INTO l_rver
    FROM ztqm_scrap_check
     WHERE ztype = 'MSC'
       AND zplant = marc-werks.

    MOVE 'Team Number ID:' TO lt_body+0(15).
    MOVE sy-uname TO lt_body+15(10).
    APPEND lt_body.
    CLEAR: lt_body.

    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: 'Part No' TO lt_body+0(20),
          'Quantity' TO lt_body+20(10),
          'Work Center' TO lt_body+30(10).

    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: '====================' TO lt_body+0(20),
          '==========' TO lt_body+20(10),
          '==========' TO lt_body+30(10).
    APPEND lt_body.
    CLEAR: lt_body.

    MOVE: matnr TO lt_body+0(20),
      rkmng TO lt_body+20(10),
      qmgrp TO lt_body+30(10).
    APPEND lt_body.
    CLEAR: lt_body.

    CALL FUNCTION 'ZCAF_SEND_EMAIL'
      EXPORTING
        p_subject  = 'Scrap Tag Quantity Limit Exceeded'
        p_rec_type = 'C'
        p_receiver = l_rver
      TABLES
        pt_body    = lt_body.

    MESSAGE e032.

  ENDIF.

ENDFORM.                    " CHECK_LIMITATION_100
*&---------------------------------------------------------------------*
*&      Module  DISPALY_VALUES_LOCATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dispaly_values_location INPUT.
  TYPE-POOLS:            vrm.

  DATA: l_name TYPE vrm_id .
  DATA: xlist TYPE vrm_values.

  l_name = 'ZLOCATION'.

  SELECT DISTINCT codegruppe AS key kurztext AS text
    INTO TABLE xlist
     FROM qpgt
     WHERE katalogart = '#'.

  DELETE xlist WHERE key = ' '.
  SORT xlist BY key.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_name
      values = xlist.
ENDMODULE.                 " DISPALY_VALUES_LOCATION  INPUT
