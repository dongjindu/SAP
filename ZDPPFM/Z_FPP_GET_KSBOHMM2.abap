FUNCTION z_fpp_get_ksbohmm2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(C_FLAG) LIKE  ZSPP_KSBOHMM-ZZRET
*"  TABLES
*"      T_ZSPP_KSBOHMM STRUCTURE  ZSPP_KSBOHMM2
*"----------------------------------------------------------------------
*  Date        Developer    Request      Description
* 08/05/2010   Furong                  Copy from Z_FPP_GET_KSBOHMM
* 10/01/2010   Daniel       UD1K949869   Added others for dealer code
* 02.12.2014   Victor                    Added Spec Nation Dealer field
*-------------------------------------------------------------------
  DATA: l_date LIKE sy-datum.
  DATA : it_zspp_ksbohmm LIKE TABLE OF ztpp_ksbohmm WITH HEADER LINE,
       l_dealer(2) TYPE c.

** Added by Furong on 10/15/10
  DATA: lt_log LIKE TABLE OF ztpp_ksbohmm_log WITH HEADER LINE.
  LOOP AT t_zspp_ksbohmm.
    MOVE-CORRESPONDING t_zspp_ksbohmm TO lt_log.
    lt_log-zedat  = sy-datum. "added Timestamp on 02.12.2014 Victor
    lt_log-zetim  = sy-uzeit.
    lt_log-ZZRET  = 'S'.
    APPEND lt_log.
  ENDLOOP.
  INSERT ztpp_ksbohmm_log FROM TABLE lt_log
                      ACCEPTING DUPLICATE KEYS .
** end of addition

  l_date = sy-datum.

  DELETE FROM ztpp_ksbohmm CLIENT SPECIFIED WHERE zsdat < l_date
                                              AND mandt = sy-mandt.
*
*------> MOVE INBOUNDED TABLE TO ITAB

  LOOP AT t_zspp_ksbohmm.
    CLEAR: it_zspp_ksbohmm.
    MOVE-CORRESPONDING t_zspp_ksbohmm TO it_zspp_ksbohmm.

    CONCATENATE t_zspp_ksbohmm-nation t_zspp_ksbohmm-dealer
                                INTO it_zspp_ksbohmm-snatdl.

    IF  c_flag  EQ 'N'.
      CASE t_zspp_ksbohmm-usag.
        WHEN 'S'.
          it_zspp_ksbohmm-dealer = t_zspp_ksbohmm-dest+3(2).
        WHEN 'Z'.
          it_zspp_ksbohmm-dealer = 'PP'.
        WHEN 'U'.
          IF  t_zspp_ksbohmm-uslv = 'B'.
            it_zspp_ksbohmm-dealer = 'XX'.
          ELSEIF  t_zspp_ksbohmm-uslv = 'P'.
            it_zspp_ksbohmm-dealer = 'XY'.
          ELSEIF  t_zspp_ksbohmm-uslv = 'T'.
            it_zspp_ksbohmm-dealer = 'XA'.
          ENDIF.
        WHEN OTHERS.
* by Daniel on 10/10/10 {
          it_zspp_ksbohmm-dealer = t_zspp_ksbohmm-dest+3(2).
* }
      ENDCASE.

* by Daniel on 10/20/10 {
** Added by Furong on 10/15/10
*      if IT_ZSPP_KSBOHMM-DEALER = 'AB'
*         OR IT_ZSPP_KSBOHMM-DEALER = 'AC'.
*         IT_ZSPP_KSBOHMM-DEALER = 'AA'.
*      ENDIF.
** End of change

      IF it_zspp_ksbohmm-nation = 'A31' AND
          ( it_zspp_ksbohmm-dealer = 'AB'
            OR it_zspp_ksbohmm-dealer = 'AC').
        it_zspp_ksbohmm-dealer = 'AA'.
      ENDIF.
* }

* Version Conversion
      IF     it_zspp_ksbohmm-vers IS INITIAL.
        it_zspp_ksbohmm-vers = '000'.
      ELSE.
        UNPACK it_zspp_ksbohmm-vers TO it_zspp_ksbohmm-vers.
      ENDIF.
    ENDIF.

    CONCATENATE it_zspp_ksbohmm-nation it_zspp_ksbohmm-dealer
                INTO it_zspp_ksbohmm-dest.
    it_zspp_ksbohmm-zsdat = sy-datum.
    it_zspp_ksbohmm-zstim = sy-uzeit.
    it_zspp_ksbohmm-zedat  = sy-datum. "added Timestamp on 02.12.2014
    it_zspp_ksbohmm-zetim  = sy-uzeit.
    it_zspp_ksbohmm-ZRESULT = 'S'.

    APPEND it_zspp_ksbohmm.
    MODIFY ztpp_ksbohmm FROM it_zspp_ksbohmm.
    IF sy-subrc = 0.
      MOVE  'S' TO t_zspp_ksbohmm-zzret.
    ELSE.
      MOVE  'E' TO t_zspp_ksbohmm-zzret.
    ENDIF.

  ENDLOOP.

  COMMIT WORK.
ENDFUNCTION.
