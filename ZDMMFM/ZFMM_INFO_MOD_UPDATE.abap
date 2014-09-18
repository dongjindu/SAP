FUNCTION zfmm_info_mod_update .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RESULT) TYPE  ZRESULT
*"     VALUE(E_MSG) TYPE  ZMSG
*"  TABLES
*"      IT_BODY STRUCTURE  ZSMM_INFO_MOD_UPDATE
*"----------------------------------------------------------------------

  DATA : it_m009 TYPE TABLE OF ztmm_info_mod_up WITH HEADER LINE,
         it_reval TYPE TABLE OF ztmm_mod_reval WITH HEADER LINE.

  DATA : l_mtart LIKE mara-mtart.

  DATA : it_m009_mail TYPE TABLE OF ztmm_info_mod_up WITH HEADER LINE,
        lw_ovelap LIKE zsmm_info_update.

  DATA: BEGIN OF lt_over OCCURS 0,
        matnr LIKE it_body-matnr,
        lifnr LIKE it_body-lifnr,
*        zseq  LIKE it_body-zseq,
*        ztseq LIKE it_body-ztseq,
        END OF lt_over.

  DATA: l_term_from LIKE it_body-term_from,
        l_term_to LIKE it_body-term_to.

  DATA : l_index LIKE sy-tabix.

  DATA : l_str1 TYPE string,
        l_str2 TYPE string,
        l_string TYPE string.

  DATA: l_num TYPE i,
        l_char(1),
        l_ln_body TYPE i,
        l_ln_over TYPE i,
        l_flag(1).

  CONSTANTS: l_dec2 LIKE ekpo-netpr VALUE '0.01'.

  CLEAR : it_m009, it_m009[]." , it_m009_tmp[].

** Check SUB

  LOOP AT it_body.
    IF it_body-sub <> 'SUB'.
      e_result = 'E'.
      CONCATENATE it_body-matnr 'has incorrect value in field SUB' INTO e_msg
         SEPARATED BY space .
      EXIT.
    ENDIF.
  ENDLOOP.

  CHECK e_result <> 'E'.
**
*  SORT it_body BY pum_n lifnr matnr zseq ztseq term_from term_to.
  SORT it_body BY pum_n lifnr matnr term_from term_to.

  LOOP AT it_body.  "copy data because of different field length
    it_body-price_unit  =  1.

    IF it_body-price <> 0.
      l_string = it_body-price.

      IF it_body-price < 0.
        SHIFT l_string UP TO '-' LEFT CIRCULAR.
      ENDIF.

      CONDENSE l_string NO-GAPS.
      l_num = strlen( l_string ).

      DO l_num TIMES.
        l_num = l_num - 1.
        l_char = l_string+l_num(1).
        IF l_char = '0'.
          l_string = l_string+0(l_num).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      SPLIT l_string AT '.' INTO: l_str1 l_str2.
      CONDENSE l_str2 NO-GAPS.
      l_num = strlen( l_str2 ).

      IF l_num > 2.
        l_index = sy-tabix.
        WHILE l_num > 2.
          it_body-price       = it_body-price * 10.
          it_body-price_unit  = it_body-price_unit * 10.
          l_num = l_num - 1.
        ENDWHILE.
      ELSE.
      ENDIF.

    ELSE.
    ENDIF.

    IF it_body-term_price <> 0.

      l_string = it_body-term_price.

      IF it_body-term_price < 0.
        SHIFT l_string UP TO '-' LEFT CIRCULAR.
      ENDIF.

      CONDENSE l_string NO-GAPS.
      l_num = strlen( l_string ).

      DO l_num TIMES.
        l_num = l_num - 1.
        l_char = l_string+l_num(1).
        IF l_char = '0'.
          l_string = l_string+0(l_num).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      SPLIT l_string AT '.' INTO: l_str1 l_str2.
      CONDENSE l_str2 NO-GAPS.
      l_num = strlen( l_str2 ).

      IF l_num > 2.
        l_index = sy-tabix.
        WHILE l_num > 2.
          it_body-term_price = it_body-term_price * 10.
          it_body-price_unit  = it_body-price_unit * 10.
          l_num = l_num - 1.
        ENDWHILE.
      ELSE.
      ENDIF.

    ELSE.
    ENDIF.

    MODIFY it_body.

  ENDLOOP.

  LOOP AT it_body.

*--> upper case
    IF it_body-resn_c IS NOT INITIAL.
      TRANSLATE it_body-resn_c  TO UPPER CASE.
    ENDIF.
    IF it_body-pum_n IS NOT INITIAL.
      TRANSLATE it_body-pum_n  TO UPPER CASE.
    ENDIF.
    IF it_body-grbased IS NOT INITIAL.
      TRANSLATE it_body-grbased  TO UPPER CASE.
    ENDIF.
    IF it_body-unlimited IS NOT INITIAL.
      TRANSLATE it_body-unlimited  TO UPPER CASE.
    ENDIF.
    IF it_body-contrk IS NOT INITIAL.
      TRANSLATE it_body-contrk  TO UPPER CASE.
    ENDIF.
    IF it_body-znumber IS NOT INITIAL.
      TRANSLATE it_body-znumber  TO UPPER CASE.
    ENDIF.
    IF it_body-subcon IS NOT INITIAL.
      TRANSLATE it_body-subcon TO UPPER CASE.
    ENDIF.
    IF it_body-use_g IS NOT INITIAL.
      TRANSLATE it_body-use_g TO UPPER CASE.
    ENDIF.

    IF NOT it_body-ztseq IS INITIAL.
      it_body-app_d = it_body-term_from.
      it_body-app_t = it_body-term_to.
      it_body-price = it_body-term_price.
    ENDIF.


    it_body-inf_d         = sy-datum.
    it_body-inf_time      = sy-uzeit.
    it_body-zuser         = sy-uname.

*======> Save to Database table
    CLEAR : it_m009.
    MOVE-CORRESPONDING it_body TO it_m009.

    CLEAR : l_mtart.
    SELECT SINGLE mtart INTO l_mtart
    FROM mara
    WHERE matnr = it_m009-matnr.
    IF sy-subrc = 0.
      it_m009-mtart =  l_mtart.
    ENDIF.


    APPEND it_m009.
    IF it_m009-zresult IS INITIAL.
      it_m009-zresult = 'S'.
    ENDIF.
    it_m009-zbdat = sy-datum.

    MOVE-CORRESPONDING it_m009 TO it_reval.

    APPEND it_reval.

    CLEAR: it_reval, it_m009.
  ENDLOOP.

  CHECK it_m009[] IS NOT INITIAL.

  PERFORM send_mod_email TABLES: it_m009 it_m009_mail.

*--> save to table

  MODIFY ztmm_info_mod_up FROM TABLE it_m009.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    e_result = 'E'.
    e_msg = 'Interface table update error'.
    EXIT.
  ENDIF.

  MODIFY ztmm_mod_reval FROM TABLE it_reval.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    e_result = 'S'.
  ELSE.
    ROLLBACK WORK.
    e_result = 'E'.
    e_msg = 'Interface table update error'.
    EXIT.
  ENDIF.

  PERFORM update_module_type TABLES it_reval.

ENDFUNCTION.
