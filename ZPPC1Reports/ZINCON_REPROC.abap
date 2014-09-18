REPORT zincon_reproc .
************************************************************************
* Version 1.01 *
****************
* This report selects and displays all reprocessing records with
* inconsistencies between RESB and AFFW.
* When del_upd is set, the following actions are taken:
*
*   - inconsistent records are deleted from the database when the
*     cumulated quantity of AFFW differs from the corresponding
*     RESB-quantity
*
*   - When the AFFW-quantity is equal to the RESB-quantity, but the
*     logical key (plant, storage location, etc.) differs, the AFFW
*     records are updated so that all fields of the logical key are
*     equal to the corresponding RESB-fields
*
* The report only works for releases >= 4.5B!!!
************************************************************************
TYPES: BEGIN OF i_type,
         rsnum LIKE resb-rsnum,
         rspos LIKE resb-rspos,
         werks LIKE resb-werks,
         baugr LIKE resb-baugr,
         bdter LIKE resb-bdter,
         matnr LIKE resb-matnr,
         lgort LIKE resb-lgort,
         charg LIKE resb-charg,
         prvbe LIKE resb-prvbe,
         bwart LIKE resb-bwart,
         shkzg LIKE resb-shkzg,
         sobkz LIKE resb-sobkz,
         meins LIKE resb-meins,
         kdauf LIKE resb-kdauf,
         kdpos LIKE resb-kdpos,
         pspel LIKE resb-pspel,
         bdmng LIKE resb-bdmng,
         aufnr LIKE safk-aufnr,
         objnr LIKE resco-objnr,
       END OF i_type.

TABLES: affw, resb.

DATA: i_resb LIKE TABLE OF resb WITH HEADER LINE,
      i_affw LIKE TABLE OF affw WITH HEADER LINE.

DATA: t_affw LIKE affw.

DATA:    sum_affw LIKE resb-bdmng,
         diff     LIKE resb-bdmng,
         p_rsnum  LIKE resb-rsnum,
         p_rspos  LIKE resb-rspos,
         p_rempro LIKE t437s-sfpro,
         p_rretr  LIKE t437s-rretr,
         p_baugr  LIKE resb-baugr,
         p_werk   LIKE resb-werks,
         lost_affw TYPE c.

DATA: wa_info TYPE i_type.

PARAMETERS: del_upd AS CHECKBOX DEFAULT space.

SELECT-OPTIONS: plant FOR resb-werks,
                material FOR resb-matnr.

START-OF-SELECTION.

*****************************************************************
  WRITE 'Reprocessing records with RESB-quantitiy neq 0 :'
           COLOR COL_KEY.
  ULINE.
*****************************************************************

* records against cost collectors
  SELECT b~rsnum b~rspos b~werks b~baugr b~bdter b~matnr b~lgort
         b~charg b~prvbe b~bwart b~shkzg b~sobkz b~meins b~kdauf
         b~kdpos b~pspel b~bdmng a~objnr
         INTO CORRESPONDING FIELDS OF TABLE i_resb
         FROM resco AS a INNER JOIN resb AS b
         ON a~rsnum = b~rsnum
         WHERE b~werks IN plant
         AND   b~matnr IN material.

* records against run schedule headers = records from <= 4.0B
  SELECT b~rsnum b~rspos b~werks b~baugr b~bdter b~matnr b~lgort
         b~charg b~prvbe b~bwart b~shkzg b~sobkz b~meins b~kdauf
         b~kdpos b~pspel b~bdmng a~aufnr
         APPENDING CORRESPONDING FIELDS OF TABLE i_resb
         FROM safk AS a INNER JOIN resb AS b
         ON a~rsnum = b~rsnum
         WHERE b~werks IN plant
         AND   b~matnr IN material.

  LOOP AT i_resb.

    SHIFT i_resb-objnr BY 2 PLACES LEFT.
    CLEAR i_affw. REFRESH i_affw.
    SELECT * FROM affw INTO TABLE i_affw
             WHERE rsnum = i_resb-rsnum
             AND   rspos = i_resb-rspos.

* Read REM-profile in case no AFFW's found. Cannot simply use
* I_RESB-BAUGR to determine REM-profile since BAUGR could be a
* phantom.
    IF NOT sy-subrc IS INITIAL
       AND NOT i_resb-rsnum IS INITIAL.

      CLEAR: p_rempro, p_rretr, p_baugr.

      IF NOT i_resb-objnr IS INITIAL.

        SELECT SINGLE matnr dwerk FROM afpo INTO (p_baugr, p_werk)
               WHERE aufnr = i_resb-objnr.
      ELSEIF NOT i_resb-aufnr IS INITIAL.
        SELECT SINGLE matnr werks FROM safk INTO (p_baugr, p_werk)
               WHERE aufnr = i_resb-aufnr.
      ENDIF.
      CLEAR i_resb-aufnr.

      SELECT SINGLE sfepr FROM marc INTO p_rempro
             WHERE matnr = p_baugr
             AND   werks = p_werk.

      SELECT SINGLE rretr FROM t437s INTO p_rretr
             WHERE sfpro = p_rempro.

      IF NOT ( p_rretr EQ '3' ) AND sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR sum_affw.
    LOOP AT i_affw.
      sum_affw = sum_affw + i_affw-erfmg.
    ENDLOOP.
* Difference between RESB and AFFW (Quantity or logical key)?
    CHECK NOT sum_affw = i_resb-bdmng
       OR NOT i_affw-matnr = i_resb-matnr
       OR NOT i_affw-werks = i_resb-werks
       OR NOT i_affw-lgort = i_resb-lgort
       OR NOT i_affw-charg = i_resb-charg
       OR NOT i_affw-prvbe = i_resb-prvbe
       OR NOT i_affw-sobkz = i_resb-sobkz
       OR NOT i_affw-lifnr = i_resb-lifnr
       OR NOT i_affw-bwart = i_resb-bwart
       OR NOT i_affw-shkzg = i_resb-shkzg
       OR NOT i_affw-erfme = i_resb-meins
       OR NOT i_affw-kdauf = i_resb-kdauf
       OR NOT i_affw-kdpos = i_resb-kdpos
       OR NOT i_affw-ps_psp_pnr = i_resb-pspel.

    diff = i_resb-bdmng - sum_affw.
    CLEAR wa_info.
    MOVE-CORRESPONDING i_resb TO wa_info.
    PERFORM dif_output USING wa_info i_resb-bdmng sum_affw diff.

    IF NOT del_upd IS INITIAL
       AND NOT i_resb-rsnum IS INITIAL
       AND NOT i_resb-rspos IS INITIAL.
      IF ( diff = 0 ) AND ( i_affw-matnr = i_resb-matnr ).
        UPDATE affw SET   werks = i_resb-werks
                          lgort = i_resb-lgort
                          charg = i_resb-charg
                          prvbe = i_resb-prvbe
                          sobkz = i_resb-sobkz
                          lifnr = i_resb-lifnr
                          bwart = i_resb-bwart
                          shkzg = i_resb-shkzg
                          erfme = i_resb-meins
                          kdauf = i_resb-kdauf
                          kdpos = i_resb-kdpos
                          ps_psp_pnr = i_resb-pspel
                    WHERE rsnum = i_resb-rsnum
                    AND   rspos = i_resb-rspos.
      ELSE.
        DELETE FROM resb WHERE rsnum = i_resb-rsnum
                         AND   rspos = i_resb-rspos.
        DELETE FROM affw WHERE rsnum = i_resb-rsnum
                         AND   rspos = i_resb-rspos.
      ENDIF.
      COMMIT WORK.
    ENDIF.

  ENDLOOP.

*****************************************************************
  WRITE 'Reprocessing records with RESB-quantitiy eq 0'
           COLOR COL_KEY.
  ULINE.
*****************************************************************
  CLEAR i_affw. REFRESH i_affw.
* records against cost collectors
  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_affw
           FROM resco INNER JOIN affw
           ON resco~rsnum = affw~rsnum
           WHERE werks IN plant
           AND   matnr IN material.
* records against run schedule headers = records from <= 4.0B
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE i_affw
           FROM safk INNER JOIN affw
           ON safk~rsnum = affw~rsnum
           WHERE affw~werks IN plant
           AND   affw~matnr IN material
           AND   NOT affw~rsnum EQ space.

  SORT i_affw BY rsnum rspos.
  CLEAR sum_affw.

  LOOP AT i_affw.

    ON CHANGE OF i_affw-rsnum OR i_affw-rspos.
      IF NOT lost_affw IS INITIAL.
        diff = - sum_affw.
        CLEAR wa_info.
        SELECT SINGLE matnr FROM afpo INTO wa_info-baugr
               WHERE aufnr = t_affw-aufnr.
        MOVE-CORRESPONDING t_affw TO wa_info.
        MOVE t_affw-ersda TO wa_info-bdter.
        PERFORM dif_output USING wa_info 0 sum_affw diff.
        CLEAR sum_affw.
        CLEAR lost_affw.
      ENDIF.
    ENDON.

    SELECT SINGLE * FROM resb
             WHERE rsnum = i_affw-rsnum
             AND   rspos = i_affw-rspos.
    IF sy-subrc NE 0
       OR NOT i_affw-matnr = resb-matnr
       OR NOT i_affw-werks = resb-werks
       OR NOT i_affw-lgort = resb-lgort
       OR NOT i_affw-charg = resb-charg
       OR NOT i_affw-prvbe = resb-prvbe
       OR NOT i_affw-sobkz = resb-sobkz
       OR NOT i_affw-lifnr = resb-lifnr
       OR NOT i_affw-bwart = resb-bwart
       OR NOT i_affw-shkzg = resb-shkzg
       OR NOT i_affw-erfme = resb-meins
       OR NOT i_affw-kdauf = resb-kdauf
       OR NOT i_affw-kdpos = resb-kdpos
       OR NOT i_affw-ps_psp_pnr = resb-pspel.
      lost_affw = 'X'.
      sum_affw = sum_affw + i_affw-erfmg.
      IF NOT del_upd IS INITIAL.
        DELETE affw FROM i_affw.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    MOVE i_affw TO t_affw.
  ENDLOOP.
  IF NOT lost_affw IS INITIAL.
    diff = - sum_affw.
    CLEAR wa_info.
    SELECT SINGLE matnr FROM afpo INTO wa_info-baugr
           WHERE aufnr = t_affw-aufnr.
    MOVE-CORRESPONDING t_affw TO wa_info.
    MOVE t_affw-ersda TO wa_info-bdter.
    PERFORM dif_output USING wa_info 0 sum_affw diff.
    CLEAR sum_affw.
    CLEAR lost_affw.
  ENDIF.

  IF NOT del_upd IS INITIAL.
    SKIP.
    WRITE:/ 'All displayed inconsistencies have been deleted !'.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  DIF_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_RESB  text
*----------------------------------------------------------------------*
FORM dif_output USING  p_info TYPE i_type p_resbq p_affwq p_diff.

  WRITE:
  /01(05) 'RSNUM',
   12(05) 'RSPOS',
   19(05) 'Plant',
   26(08) 'Assembly',
   46(09) 'Component',
   66(08) 'Stor.Loc,',
   75(05) 'Batch',
  /01(10) p_info-rsnum,
   12(04) p_info-rspos,
   19(04) p_info-werks,
   26(18) p_info-baugr,
   46(18) p_info-matnr,
   66(05) p_info-lgort,
   75(10) p_info-charg.
  ULINE.
  WRITE:
  /01(12) 'Cost Coll.',
   15(09) 'Req.Date',
   24(14) 'RESB-QUANTITY',
   39(14) 'AFFW-QUANTITY',
   54(19) 'QUANTITY DIFFERENCE'.
  IF p_info-aufnr IS INITIAL.
    WRITE  /01(12) p_info-objnr.
  ELSE.
    WRITE  /01(12) p_info-aufnr.
  ENDIF.
  WRITE:
   15(09) p_info-bdter,
   24(14) p_resbq,
   39(14) p_affwq,
   54(14) p_diff.
  ULINE. ULINE.

ENDFORM.                               " DIF_OUTPUT
