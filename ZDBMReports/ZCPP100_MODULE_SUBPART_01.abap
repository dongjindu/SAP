REPORT zcpp100_module_subpart_01 LINE-SIZE 255.

*-------------------------------------------------------------------*
* Date        Developer     Request          Description
* 09/13/06    Manju         UD1K922133       Bug Fix
*--------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE AREA
*----------------------------------------------------------------------*
TABLES: ztbm_abxduldt,  "MODULE vs SUB-PART
*        ztbm_abxebmdt,  "ECM BOM STRUCTURE
        aenr,  "Change Master
        mast,  "Material to BOM Link
        stko,  "BOM Header
        stpo,  "BOM item (NONE VERSION)
        stas.  "BOMs - Item Selection (HISTORY)
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA  :BEGIN OF it_stpo OCCURS 0,
        matnr LIKE mast-matnr,
        werks LIKE mast-werks,
        stlan LIKE mast-stlan, " BOM USAGE
        stlal LIKE mast-stlal, " Alternative BOM
        stlnr LIKE stpo-stlnr,
        stlkn LIKE stpo-stlkn,
        posnr LIKE stpo-posnr,
        idnrk LIKE stpo-idnrk,
        suff  LIKE stpo-suff,
        zinfo LIKE stpo-zinfo,
        sequ  LIKE stpo-sequ,
        menge LIKE stpo-menge,
        meins LIKE stpo-meins,
        stgb  LIKE stpo-stgb,
        datuv LIKE stpo-datuv,
        datub LIKE stpo-datuv,
        aennr LIKE stpo-aennr, "Change no from
        aenra LIKE stpo-aennr, "Change no to
        andat LIKE sy-datum,
        annam LIKE sy-uname,
       END OF it_stpo,
       st_stpo LIKE it_stpo.

DATA: BEGIN OF it_abxduldt OCCURS 0.
        INCLUDE STRUCTURE ztbm_abxduldt.
DATA:   flag,
      END   OF it_abxduldt.

DATA : w_int   TYPE i,
       w_error TYPE i,
       p_aennr LIKE stpo-aennr.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_werks TYPE mast-werks DEFAULT 'P001' OBLIGATORY,
            p_stlan TYPE mast-stlan DEFAULT '2' MODIF ID gr1 OBLIGATORY.
*SELECT-OPTIONS:S_MTNO FOR ZTBM_ABXEBMDT-MTNO.
SELECTION-SCREEN END   OF BLOCK b1.

*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

START-OF-SELECTION.
*  data : it_temp like ZTBM_ABXDULDT occurs 0 with header line.
*  select * into table it_temp from ZTBM_ABXDULDT.
*  DELETE ZTBM_ABXDULDT FROM TABLE it_temp.

  PERFORM read_process.
  PERFORM data_process.

END-OF-SELECTION.

TOP-OF-PAGE.
*  PERFORM TOP_OF_PAGE.

END-OF-PAGE.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_process.
*Read
  SELECT   c~matnr c~werks c~stlan c~stlnr c~stlal
           b~posnr b~idnrk b~suff b~sequ b~menge b~meins
           b~datuv b~aennr b~zinfo  b~stlkn b~stgb
            APPENDING CORRESPONDING FIELDS OF TABLE it_stpo
            FROM mast AS c INNER JOIN stko AS a
              ON c~stlnr = a~stlnr
               INNER JOIN stpo AS b
             ON a~stlnr = b~stlnr
              WHERE c~werks EQ p_werks
                AND c~stlan EQ p_stlan
                AND b~stlty EQ 'M'.
  DESCRIBE TABLE it_stpo LINES w_int.
  IF w_int = 0.
    WRITE: / 'NO DATA'.
  ENDIF.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_process.
  PERFORM data_move.
  PERFORM table_update.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_AENR
*&---------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_IT_EBMDT_EONO  text
*      <--P_L_DATUV  text
*----------------------------------------------------------------------*
FORM read_aenr USING    p_eono
               CHANGING p_datuv.
  SELECT SINGLE datuv
     FROM aenr
      INTO p_datuv
      WHERE aennr EQ p_eono.

  IF sy-subrc EQ 0.

  ELSE.
    CLEAR p_datuv.
  ENDIF.


ENDFORM.                    " READ_AENR
*&---------------------------------------------------------------------*
*&      Form  DATA_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_move.
  DATA: l_datuv TYPE sy-datum,
*Issue #20041115-008 Requested by IM Choi
*Changed by Wskim, on 2004/11/29
*---Start
        l_datub TYPE sy-datum,
*---End
        l_eono(20).

  LOOP AT it_stpo.
    CLEAR l_datuv.
    PERFORM read_aenr USING   it_stpo-aennr
                      CHANGING  l_datuv.
    it_stpo-datuv = l_datuv.
    PERFORM times_out_check USING 'M'  it_stpo-stlnr
                                       it_stpo-stlkn
                                    CHANGING p_aennr.
    IF p_aennr NE space.
*Issue #20041115-008 Requested by IM Choi
*Changed by Wskim, on 2004/11/29
*---Start
      PERFORM read_aenr_to USING p_aennr
                           CHANGING  l_datub.
*      it_stpo-datub = l_datuv .
      it_stpo-datub = l_datub .
*---End
    ELSE.
      it_stpo-datub =  '99991231'.
    ENDIF.
    it_stpo-andat = sy-datum.
    it_stpo-annam = sy-uname.
    MODIFY it_stpo FROM it_stpo INDEX sy-tabix.
    CLEAR : it_stpo.
  ENDLOOP.

ENDFORM.                    " DATA_MOVE
*&---------------------------------------------------------------------*
*&      Form  times_out_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_STPO  text
*      -->P_STLTY  text
*      -->P_IT_STPO_STLNR  text
*      -->P_IT_STPO_STLKN  text
*----------------------------------------------------------------------*
FORM times_out_check USING  p_stlty
                            p_stlnr
                            p_stlkn
                     CHANGING p_aennr.

data : l_STASZ like stas-STASZ.  "UD1K922133

  SELECT  aennr stasz INTO (p_aennr,l_stasz)  "UD1K922133
        FROM stas
       WHERE stlty EQ p_stlty
         AND stlnr EQ p_stlnr
         AND stlkn EQ p_stlkn
         AND lkenz EQ 'X'
         order by STASZ descending.      "UD1K922133
   exit.                                 "UD1K922133
  endselect.                             "UD1K922133



ENDFORM.                    " times_out_check
*&---------------------------------------------------------------------*
*&      Form  read_aenr_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_AENNR  text
*      <--P_L_DATUB  text
*----------------------------------------------------------------------*
FORM read_aenr_to USING    p_eono
                  CHANGING p_datub.

  SELECT SINGLE datuv
     FROM aenr
      INTO p_datub
      WHERE aennr EQ p_eono.

  IF sy-subrc EQ 0.

  ELSE.
    CLEAR p_datub.
  ENDIF.

ENDFORM.                    " read_aenr_to
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  table_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM table_update.
  PERFORM set_it_abxduldt.
  PERFORM update_abxduldt.
  PERFORM display_rtn.
ENDFORM.                    " table_update
*&---------------------------------------------------------------------*
*&      Form  set_it_abxduldt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_abxduldt.
  LOOP AT it_stpo.
    CLEAR: it_abxduldt.

    MOVE : it_stpo-matnr TO it_abxduldt-mtno,
           it_stpo-werks TO it_abxduldt-plnt,
           it_stpo-stlan TO it_abxduldt-usag,
           it_stpo-stlal TO it_abxduldt-altn,
           it_stpo-posnr TO it_abxduldt-pref,
           it_stpo-idnrk TO it_abxduldt-comp,
           it_stpo-suff  TO it_abxduldt-suff,
           it_stpo-zinfo TO it_abxduldt-seqc,
           it_stpo-sequ  TO it_abxduldt-sequ,
           it_stpo-menge TO it_abxduldt-qnty,
           it_stpo-meins TO it_abxduldt-unit,
           it_stpo-datuv TO it_abxduldt-datuv,
           it_stpo-datub TO it_abxduldt-datub,
           it_stpo-stgb  TO it_abxduldt-stgb,
           it_stpo-andat TO it_abxduldt-andat,
           it_stpo-annam TO it_abxduldt-annam,
           sy-uname      TO it_abxduldt-ernam,
           sy-datum      TO it_abxduldt-erdat,
           sy-uzeit      TO it_abxduldt-erzet,
           sy-uname      TO it_abxduldt-aenam,
           sy-datum      TO it_abxduldt-aedat,
           sy-uzeit      TO it_abxduldt-aezet.

    APPEND it_abxduldt.
  ENDLOOP.
ENDFORM.                    " set_it_abxduldt
*&---------------------------------------------------------------------*
*&      Form  update_abxduldt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_abxduldt.
  DELETE FROM ztbm_abxduldt WHERE mtno >= space.

  LOOP AT it_abxduldt.
    CLEAR: ztbm_abxduldt.

    MOVE-CORRESPONDING: it_abxduldt TO ztbm_abxduldt.

    INSERT ztbm_abxduldt.
    IF sy-subrc NE 0.
      MOVE: 'X' TO it_abxduldt-flag.
      w_error = w_error + 1.
    ENDIF.

    w_int = w_int + 1.

    MODIFY it_abxduldt.
  ENDLOOP.
ENDFORM.                    " update_abxduldt
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  WRITE:/ 'Total :', w_int,
          'Error :',   w_error,
        / '*************************************************'.

  SORT it_abxduldt BY flag mtno plnt usag altn pref comp suff seqc sequ.
  LOOP AT it_abxduldt.
    ON CHANGE OF it_abxduldt-flag.
      WRITE:/ '*************************************************'.
    ENDON.
    WRITE : / it_abxduldt-mtno,it_abxduldt-plnt, it_abxduldt-usag,
              it_abxduldt-altn,it_abxduldt-pref, it_abxduldt-comp,
              it_abxduldt-suff,it_abxduldt-sequ, it_abxduldt-qnty,
              it_abxduldt-unit,it_abxduldt-datuv,it_abxduldt-datub.
  ENDLOOP.

  IF w_error > 0.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m03.
  ELSE.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " display_rtn
