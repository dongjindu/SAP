************************************************************************
* Author                 : Catherine Sjolander
* Creation Date          : 04/05/2004
* Specifications By      :
* Development Request No :
* Addl documentation     :
* Description            : STOCK BY VIN FOR APS
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************

REPORT ZRPPMM05AC_VIN_TO_APS   MESSAGE-ID zmpp.
*
TABLES: ausp, cabn, itob.


*
DATA : BEGIN OF it_ausp OCCURS 0,
        objek LIKE ausp-objek,
        atinn LIKE ausp-atinn,
        atwrt LIKE ausp-atwrt,
        atflv LIKE ausp-atflv,
      END OF it_ausp.
DATA: rp_status(2).
DATA: w_error(1).
DATA: w_message(50).
RANGES r_atinn FOR ausp-atinn OCCURS 0.
RANGES r_atflv FOR ausp-atflv.
RANGES: r_atnam FOR cabn-atnam,
        r_status FOR rp_status,
        r_rp     FOR ausp-atinn.

DATA : dsn(90).
DATA : BEGIN OF it_cabn OCCURS 0,
       atinn LIKE cabn-atinn,
       atnam LIKE cabn-atnam,
       END OF it_cabn.


DATA : BEGIN OF it_list OCCURS 0,
         vin(17),
         body1(4),
         body2(6),
         wordr(14),
         mi(15),
         ocn(4),
         versn(3),
         extcl(3),
         intcl(3),
         rp(3),
         ptime(14),
       END OF it_list.
DATA : BEGIN OF it_status OCCURS 0,
        sap(2),
        aps(3),
       END OF it_status.
DATA : w_cnt TYPE i,
       w_atinn LIKE cabn-atinn,
       w_atnam LIKE cabn-atnam,
       w_atflv(10).


*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_date  FOR sy-datum OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.


*
START-OF-SELECTION.
  PERFORM set_initial.
  PERFORM read_data.
  PERFORM modify_data.
*  PERFORM BUILD_MODEL.



*
END-OF-SELECTION.
  IF w_error = 'X'.
    MESSAGE s000 WITH w_message.
  ELSE.
    PERFORM download_data.
    MESSAGE s000 WITH text-m03.
  ENDIF.




************************************************************************
*                            FORMS                                     *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM read_data.
  DATA: lt_objek  LIKE it_ausp OCCURS 0 WITH HEADER LINE.
  DATA: lt_proddt LIKE it_ausp OCCURS 0 WITH HEADER LINE.
  data :l_cnt type i,
   wa_objek  LIKE it_ausp OCCURS 0 WITH HEADER LINE.

  CLEAR: it_cabn.

*  READ THE OBJECT WITH STATUS BETWEEN 20 AND 28

  READ TABLE it_cabn WITH KEY atnam = 'P_RP_STATUS'.

  SELECT objek atinn atwrt
    INTO CORRESPONDING FIELDS OF TABLE lt_objek
    FROM ausp
    WHERE klart  = '002'
      AND atinn  = it_cabn-atinn
      AND atwrt IN r_status.

  IF sy-subrc NE 0.
    w_error   = 'X'.
    w_message = 'No entry beween status 20 to 28'.
    EXIT.
  ENDIF.

*  READING THE PRODUCTION DATE
  PERFORM read_production_date TABLES lt_objek lt_proddt.

*  PERFORM get_atinn USING 'P_RP18_SHOP_DATE'.
*
*  SELECT OBJEK ATINN ATWRT ATFLV
*      INTO CORRESPONDING FIELDS OF TABLE it_ausp
*      FROM ausp
*      WHERE objek IN ( select OBJEK
*                               from AUSP
*                               WHERE atinn in w_atinn
*                                 AND klart = '002'
*                                 AND atflv IN R_ATFLV )
*          and ATINN in R_ATINN
*          AND klart = '002'.


  loop at lt_objek.
    l_cnt = l_cnt + 1.
    move lt_objek to wa_objek.
    append wa_objek.
    if l_cnt = 10000.
*  READ CHARACTERISTIC VALUE
      SELECT objek atinn atwrt atflv
        appending  CORRESPONDING FIELDS OF TABLE it_ausp
        FROM ausp
        FOR ALL ENTRIES IN wa_objek
        WHERE objek = wa_objek-objek
          AND atinn IN r_atinn
          AND klart =  '002'.
      clear l_cnt.
      refresh wa_objek. clear wa_objek.

    endif.
  endloop.

  if not wa_objek[] is initial.
    SELECT objek atinn atwrt atflv
          appending  CORRESPONDING FIELDS OF TABLE it_ausp
          FROM ausp
          FOR ALL ENTRIES IN wa_objek
          WHERE objek = wa_objek-objek
            AND atinn IN r_atinn
            AND klart =  '002'.
  endif.


*  READ CHARACTERISTIC VALUE
*  SELECT objek atinn atwrt atflv
*    INTO CORRESPONDING FIELDS OF TABLE it_ausp
*    FROM ausp
*    FOR ALL ENTRIES IN lt_objek
*    WHERE objek = lt_objek-objek
*      AND atinn IN r_atinn
*      AND klart =  '002'.

*  COMBINE THE STATUS and date VALUE.
  APPEND LINES OF lt_objek  TO it_ausp.
  APPEND LINES OF lt_proddt TO it_ausp.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_INITIAL
*&---------------------------------------------------------------------*
FORM set_initial.
  DATA: l_num(2) TYPE n.

* set the status conversion rule.
  PERFORM set_conversion_rule.

* MAKE THE CHARACTERISTIC LIST
  CLEAR: r_atnam, r_atnam[].
  PERFORM build_atnam USING 'P_VIN'.
  PERFORM build_atnam USING 'P_SEQUENCE_CODE'.
  PERFORM build_atnam USING 'P_BODY_SERIAL'.
  PERFORM build_atnam USING 'P_WORK_ORDER'.
  PERFORM build_atnam USING 'P_MI'.
  PERFORM build_atnam USING 'P_OCN'.
  PERFORM build_atnam USING 'P_VERSION'.
  PERFORM build_atnam USING 'P_EXT_COLOR'.
  PERFORM build_atnam USING 'P_INT_COLOR'.
  PERFORM build_atnam USING 'P_RP_STATUS '.
  PERFORM build_atnam USING 'P_RP20_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP20_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP21_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP21_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP22_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP22_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP23_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP23_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP24_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP24_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP25_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP25_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP26_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP26_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP27_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP27_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_RP28_SHOP_DATE'.
  PERFORM build_atnam USING 'P_RP28_ACTUAL_DATE'.
  PERFORM build_atnam USING 'P_MODEL'.


  REFRESH : it_cabn, r_atinn.
  CLEAR   : it_cabn, r_atinn.
* FIND THE CHARACTERISTIC INTERNAL NUMBER LIST
  SELECT atinn atnam INTO TABLE it_cabn
     FROM cabn
     WHERE atnam IN r_atnam.


  r_atinn-sign = 'I'.
  r_atinn-option = 'EQ'.

  LOOP AT it_cabn.
    IF NOT it_cabn-atnam CS '_DATE'.
      r_atinn-low = it_cabn-atinn.
      APPEND r_atinn.
    ENDIF.
  ENDLOOP.

* CONVERT THE DATE RANGE FORMAT
  REFRESH r_atflv. CLEAR r_atflv.
  LOOP AT s_date.
    r_atflv-sign   = s_date-sign.
    r_atflv-option = s_date-option.
    w_atflv = s_date-low.
    r_atflv-low = w_atflv.

    w_atflv = s_date-high.
    r_atflv-high = w_atflv.

    APPEND r_atflv. CLEAR r_atflv.
  ENDLOOP.
*
* SET THE SEARCHING RP POINT
*  PERFORM GET_RP_RANGE tables R_RP.

* SET STATUS VALUES
  l_num = '20'.
  CLEAR: r_status, r_status[].
  r_status-sign    = 'I'.
  r_status-option  = 'EQ'.
  DO 9 TIMES.
    r_status-low = l_num.
    l_num = l_num + 1.
    APPEND r_status.
  ENDDO.
  CLEAR: r_status.
ENDFORM.                    " SET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  GET_ATINN
*&---------------------------------------------------------------------*
FORM get_atinn USING p_atnam.
  READ TABLE it_cabn WITH KEY atnam = p_atnam.
  IF sy-subrc = 0.
    w_atinn = it_cabn-atinn.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  GET_ATNAM
*&---------------------------------------------------------------------*
FORM get_atnam USING p_atinn.
  READ TABLE it_cabn WITH KEY atinn = p_atinn.
  IF sy-subrc = 0.
    w_atnam = it_cabn-atnam.
  ENDIF.
ENDFORM.                    " GET_ATINN
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data.
  DATA : w_objek LIKE ausp-objek.
  DATA : wa_ausp LIKE it_ausp.
  DATA : l_tabix LIKE sy-tabix.
  DATA :  w_delete(1).
  DATA : l_date  TYPE i.
  DATA : lt_ausp  LIKE it_ausp OCCURS 0 WITH HEADER LINE,
         f_idx like sy-tabix,
         t_idx like sy-tabix,
         flag.


  REFRESH it_list. CLEAR it_list.

  SORT it_ausp BY objek atinn.

  export it_ausp to memory id 'man'.

  import it_ausp from memory id 'man'.

*  LT_AUSP[] = IT_AUSP[].
  f_idx = 1.
* delete the RP25/RP27 with past date
  CLEAR: lt_ausp, lt_ausp[].
  LOOP AT it_ausp.
    AT NEW objek.
      f_idx = sy-tabix.
    endat.

    t_idx = sy-tabix.
    PERFORM get_atnam USING it_ausp-atinn.

    IF ( w_atnam = 'P_RP25_ACTUAL_DATE' OR
         w_atnam = 'P_RP27_ACTUAL_DATE' ) AND
         it_ausp-atwrt(8) NE sy-datum  .
          flag = 'X'.
    ENDIF.
    at end of objek.
      if flag = 'X'.
        DELETE  It_ausp   from f_idx to t_idx.
        clear flag.
      endif.
    endat.
  ENDLOOP.



* TRANSFER THE VALUE TO OUTPUT TABLE
  LOOP AT it_ausp.

*    AT NEW objek.
*      READ TABLE lt_ausp WITH KEY objek = it_ausp-objek.
*      IF sy-subrc EQ 0.
*        w_delete = 'X'.
*      ELSE.
*        CLEAR w_delete.
*      ENDIF.
*    ENDAT.
*    IF w_delete IS INITIAL.
*      l_tabix = sy-tabix + 1.

    PERFORM get_atnam USING it_ausp-atinn.

    CASE w_atnam.
      WHEN 'P_VIN'.
        it_list-vin      = it_ausp-atwrt.

      WHEN 'P_MODEL'.
        it_list-body1     = it_ausp-atwrt.

      WHEN 'P_BODY_SERIAL'.
        it_list-body2    = it_ausp-atwrt.

      WHEN 'P_WORK_ORDER'.
        it_list-wordr    = it_ausp-atwrt.

      WHEN 'P_MI'.
        it_list-mi       = it_ausp-atwrt.

      WHEN 'P_OCN'.
        it_list-ocn      = it_ausp-atwrt.

      WHEN 'P_VERSION'.
        it_list-versn    = it_ausp-atwrt.

      WHEN 'P_EXT_COLOR'.
        it_list-extcl    = it_ausp-atwrt.

      WHEN 'P_INT_COLOR'.
        it_list-intcl    = it_ausp-atwrt.

      WHEN 'P_RP_STATUS'.
        PERFORM covert_status USING it_ausp-atwrt.
        it_list-rp   = it_ausp-atwrt.

*      WHEN 'P_RP18_ACTUAL_DATE'.
*        IT_LIST-PTIME  = IT_AUSP-ATWRT.
      WHEN OTHERS.
        IF w_atnam CS 'ACTUAL_DATE'.
          it_list-ptime  = it_ausp-atwrt.
        ENDIF.
    ENDCASE.




    AT END OF objek.
*      IF w_delete IS INITIAL.
        w_objek = it_ausp-objek.
        APPEND it_list.
        CLEAR it_list.
*      ENDIF.
      CLEAR w_delete.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM download_data.
  DATA: l_date(06).

  DESCRIBE TABLE it_list LINES w_cnt.
  IF w_cnt = 0.
    MESSAGE i000 WITH text-m02.
    STOP.
  ENDIF.

  l_date = s_date-low+2(6).

  IF S_DATE-HIGH IS INITIAL.
    L_DATE = S_DATE-LOW.
  ELSE.
   CONCATENATE S_DATE-LOW '_' S_DATE-HIGH+4(4) INTO L_DATE.
  ENDIF.

  CONCATENATE  '/usr/sap/EDI_SAP/'
               'PMM05AC'
               '.txt'
               INTO dsn.

  OPEN DATASET dsn IN TEXT MODE FOR OUTPUT.
  LOOP AT it_list.
    OPEN DATASET dsn IN TEXT MODE FOR APPENDING.
    TRANSFER it_list TO dsn.
  ENDLOOP.
*
  CLOSE DATASET dsn.

  IF sy-subrc = 0.

    WRITE: /10 'FILE IS DOWNLOADED SUCCESSFULLY.'.
    SKIP.
    WRITE: /10 'File Name:', dsn.
    SKIP.
    WRITE: /10 'TOTAL RECORDS:', w_cnt.


  ELSE.

    FORMAT COLOR 6.
    WRITE: /10 'TOTAL RECORDS: ', w_cnt.
    SKIP.
    WRITE: /10 'FILE DOWNLOAD FAILED!'.
    FORMAT COLOR OFF.
    MESSAGE e000 WITH 'FILE DOWLOAD FAILED.'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_ATNAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0197   text
*----------------------------------------------------------------------*
FORM build_atnam USING  p_atnam.
  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.
  r_atnam-low = p_atnam.
  APPEND r_atnam.
ENDFORM.                    " BUILD_ATNAM
*&---------------------------------------------------------------------*
*&      Form  BUILD_MODEL
*&---------------------------------------------------------------------*
FORM build_model.
  LOOP AT it_list.
    SELECT * FROM itob
       WHERE mapar = it_list-vin
       AND serge = it_list-body2.
      it_list-body1 = itob-typbz.
    ENDSELECT.
    MODIFY it_list.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_PRODUCTION_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK  text
*      -->P_LT_PRODDT  text
*----------------------------------------------------------------------*
FORM read_production_date TABLES   pt_objek STRUCTURE it_ausp
                                   pt_proddt STRUCTURE it_ausp.
  DATA: lt_ausp LIKE it_ausp OCCURS 0 WITH HEADER LINE.
  DATA: l_text(20).

*  GET THE STATUS POINT AND IT'S PRODUCTION DATE CHARATERISTIC
  LOOP AT pt_objek.
    lt_ausp-objek = pt_objek-objek.
    CONCATENATE 'P_RP' pt_objek-atwrt '_ACTUAL_DATE' INTO l_text.
    READ TABLE it_cabn WITH KEY atnam = l_text.
    lt_ausp-atinn = it_cabn-atinn.
    APPEND lt_ausp.
  ENDLOOP.

* READ THE PRODUCTION DATE VALUE
  SELECT objek atinn atwrt atflv
     INTO CORRESPONDING FIELDS OF TABLE pt_proddt
     FROM ausp
     FOR ALL ENTRIES IN lt_ausp
     WHERE objek = lt_ausp-objek
          AND klart = '002'
          and atinn = lt_ausp-atinn.

  IF sy-subrc NE 0.
    MESSAGE i000 WITH
     'No production date at status point, check ausp data'.
  ENDIF.

ENDFORM.                    " READ_PRODUCTION_DATE
*&---------------------------------------------------------------------*
*&      Form  covert_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM covert_status USING p_rp.
  READ TABLE it_status WITH KEY sap = p_rp.
  p_rp = it_status-aps.
ENDFORM.                    " covert_status
*&---------------------------------------------------------------------*
*&      Form  set_conversion_rule
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_conversion_rule.
  DATA: l_num(2) TYPE n.
  DATA: l_sap(2) TYPE n.
  DATA: l_aps(3).

  l_num = '00'.
  l_sap = '20'.
  DO 9 TIMES.
    CONCATENATE 'V' l_num INTO l_aps.
    it_status-sap = l_sap.
    it_status-aps = l_aps.
    APPEND it_status.
    l_num = l_num + 1.
    l_sap = l_sap + 1.
  ENDDO.
ENDFORM.                    " set_conversion_rule
