FUNCTION z_frf_mm_list_bdc_lt01_check.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZINTCALL) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_LIST STRUCTURE  ZSRF_PICKER_LIST
*"----------------------------------------------------------------------
  DATA: w_vspvb LIKE marc-vspvb,
        w_nltyp LIKE t_list-nltyp,
        w_nlpla LIKE t_list-nlpla.

  DATA: it_pkhd TYPE pkhd OCCURS 0 WITH HEADER LINE.

  CONSTANTS: c_werks(4) VALUE 'P001',
             c_lgnum(3) VALUE 'P01'.

  DATA: z_table(50).
  DATA: it_ztrf_mw_er_log LIKE ztrf_mw_er_log. " OCCURS 0 WITH HEADER.
*
*  z_table =  'checking destination'.
*  CONCATENATE z_table t_list-matnr INTO z_table.
*  it_ztrf_mw_er_log-MANDT = SY-MANDT.
*  it_ztrf_mw_er_log-datum = sy-datum.
*  it_ztrf_mw_er_log-uzeit = sy-uzeit.
*  it_ztrf_mw_er_log-midwa = 'RF'.
*  it_ztrf_mw_er_log-messa = z_table.
**  MODIFY it_ztrf_mw_er_log.
*  INSERT ztrf_mw_er_log from it_ztrf_mw_er_log.
*  commit work.
  IF t_list-nltyp IS INITIAL AND t_list-nlpla IS INITIAL.
    SELECT SINGLE vspvb INTO w_vspvb FROM marc
                     WHERE matnr = t_list-matnr
                       AND werks = c_werks.
    SELECT * INTO TABLE it_pkhd FROM pkhd WHERE matnr = t_list-matnr
                                      AND werks = c_werks
                                      AND prvbe = w_vspvb.
    IF it_pkhd[] IS INITIAL.
      e_mess = text-w11.
      zresult = '1'.
    ELSE.
      READ TABLE it_pkhd INDEX 1.
      w_nltyp = it_pkhd-lgtyp.
      w_nlpla = it_pkhd-lgpla.
      e_mess  = text-m22.
      zresult = '0'.
      READ TABLE t_list INDEX 1.
      t_list-nltyp = w_nltyp.
      t_list-nlpla = w_nlpla.
      MODIFY t_list INDEX 1 TRANSPORTING nltyp nlpla.
*       PERFORM MIDDLEWARE_UPDATE TABLES T_LIST.
    ENDIF.
  ENDIF.

*if zintcall is initial and t_list-vltyp is initial
*                       and t_list-vlpla is initial
*                       and zresult = '0'.
*
*  CLEAR: it_ztrf_mw_er_log, it_ztrf_mw_er_log.
*  z_table =  'checking source'.
*  CONCATENATE z_table t_list-matnr t_list-nltyp t_list-nlpla
*               INTO z_table.
*  it_ztrf_mw_er_log-MANDT = SY-MANDT.
*  it_ztrf_mw_er_log-datum = sy-datum.
*  it_ztrf_mw_er_log-uzeit = sy-uzeit.
*  it_ztrf_mw_er_log-midwa = 'RF1'.
*  it_ztrf_mw_er_log-messa = z_table.
**  MODIFY it_ztrf_mw_er_log.
*  INSERT ztrf_mw_er_log from it_ztrf_mw_er_log.
*  commit work.

  IF t_list-vltyp IS INITIAL AND t_list-vlpla IS INITIAL
                             AND zresult = '0'.

    DATA: it_t334t TYPE t334t OCCURS 0 WITH HEADER LINE.
    DATA: w_sno TYPE i VALUE 0,
          w_sno_char(5),
          lw_index TYPE n,
          w_ltkza LIKE mlgn-ltkza,
          w_stock LIKE lqua-verme,
          w_lgtyp LIKE mlgt-lgtyp.
    DATA: w_stock_char(13),
          w_fs(30).
    data: l_index like sy-tabix,
          l_flag(1).

    DATA: it_mlgt TYPE mlgt OCCURS 0 WITH HEADER LINE,
          it_mlgt_lgtyp TYPE mlgt OCCURS 0 WITH HEADER LINE.

    FIELD-SYMBOLS: <fs>.

    SELECT SINGLE ltkza INTO w_ltkza FROM mlgn
                       WHERE matnr = t_list-matnr
                         AND lgnum = c_lgnum.

    SELECT * INTO TABLE it_mlgt FROM mlgt
                       WHERE matnr = t_list-matnr
                         AND lgnum = c_lgnum.

    SELECT * INTO TABLE it_t334t FROM t334t
                       WHERE lgnum = c_lgnum
                         AND kzear = 'A'
                         AND lgtkz = w_ltkza.

    SORT it_mlgt BY matnr lgnum lgtyp.

    LOOP AT it_t334t.
      clear: l_flag.
      DO 29 TIMES.
        lw_index = sy-index - 1.
        CONCATENATE 'it_t334t-lgty' lw_index INTO w_fs.
        ASSIGN (w_fs) TO <fs>.
        IF sy-subrc NE 0.
          EXIT.
        ENDIF.
        READ TABLE it_mlgt WITH KEY matnr = t_list-matnr
                                    lgnum = c_lgnum
                                    lgtyp = <fs>.     "it_t334t-lgty0.
*        IF sy-subrc = 0.
*          MOVE: t_list-matnr TO it_mlgt_lgtyp-matnr,
*                    c_lgnum TO it_mlgt_lgtyp-lgnum,
*                    it_mlgt-lgtyp TO it_mlgt_lgtyp-lgtyp,
*                    it_mlgt-lgpla TO it_mlgt_lgtyp-lgpla.
*          APPEND it_mlgt_lgtyp.
*          EXIT.
*        ENDIF.
*      ENDDO.
*    ENDLOOP.

** changed by Furong on 02/07/2007
        IF sy-subrc = 0.
          if it_mlgt-lgpla is initial.
            l_index = sy-tabix + 1.
            loop at it_mlgt from l_index  where matnr = t_list-matnr
                              and lgnum = c_lgnum
                              and lgtyp = <fs>
                              and lgpla > ' '.
              l_flag = 'X'.
              exit.
            endloop.
            if l_flag = 'X'.
              MOVE: t_list-matnr TO it_mlgt_lgtyp-matnr,
                    c_lgnum TO it_mlgt_lgtyp-lgnum,
                    it_mlgt-lgtyp TO it_mlgt_lgtyp-lgtyp,
                    it_mlgt-lgpla TO it_mlgt_lgtyp-lgpla.
              APPEND it_mlgt_lgtyp.
              EXIT.
            else.
              e_mess  = text-w23.
              zresult = text-m02.

            endif.
          else.
            MOVE: t_list-matnr TO it_mlgt_lgtyp-matnr,
                  c_lgnum TO it_mlgt_lgtyp-lgnum,
                  it_mlgt-lgtyp TO it_mlgt_lgtyp-lgtyp,
                  it_mlgt-lgpla TO it_mlgt_lgtyp-lgpla.
            APPEND it_mlgt_lgtyp.
            EXIT.
          endif.
        ENDIF.
      ENDDO.
    ENDLOOP.
** end of change
    IF it_mlgt_lgtyp[] IS INITIAL.
      e_mess  = text-w13.
      zresult = text-m02.
      EXIT.
    ELSE.
      e_mess  = text-m22.
      zresult = text-m04.
      READ TABLE it_mlgt_lgtyp INDEX 1.
      t_list-vltyp = it_mlgt_lgtyp-lgtyp.
      t_list-vlpla = it_mlgt_lgtyp-lgpla.
*      t_list-vltyp = it_mlgt-lgtyp.
*      t_list-vlpla = it_mlgt-lgpla.
      MODIFY t_list INDEX 1 TRANSPORTING vltyp vlpla.
    ENDIF.

*    CLEAR: it_ztrf_mw_er_log, it_ztrf_mw_er_log.
*    z_table =  'checking qty'.
*    CONCATENATE z_table t_list-matnr t_list-nltyp t_list-nlpla
*                 t_list-vltyp t_list-vlpla
*                 INTO z_table.
*    it_ztrf_mw_er_log-MANDT = SY-MANDT.
*    it_ztrf_mw_er_log-datum = sy-datum.
*    it_ztrf_mw_er_log-uzeit = sy-uzeit.
*    it_ztrf_mw_er_log-midwa = 'RF2'.
*    it_ztrf_mw_er_log-messa = z_table.
**    MODIFY it_ztrf_mw_er_log.
*  INSERT ztrf_mw_er_log from it_ztrf_mw_er_log.
*  commit work.

    IF zintcall EQ 'N'.
      CLEAR e_mess.
      LOOP AT it_mlgt_lgtyp.
        SELECT SINGLE verme INTO w_stock FROM lqua
                           WHERE matnr = t_list-matnr
                             AND lgnum = c_lgnum
                             AND lgtyp = it_mlgt-lgtyp
                             AND lgpla = it_mlgt-lgpla.
        IF sy-subrc = 0.
          IF t_list-nsolm <= w_stock.
            e_mess  = text-m22.
            zresult = text-m04.
            t_list-vltyp = it_mlgt-lgtyp.
            t_list-vlpla = it_mlgt-lgpla.
            MODIFY t_list INDEX 1 TRANSPORTING vltyp vlpla.
            EXIT.
          ELSE.
            w_stock_char = w_stock.
            CONCATENATE text-w12 text-w14 w_stock_char INTO e_mess.
            zresult = text-m02.
            t_list-vltyp = it_mlgt-lgtyp.
            t_list-vlpla = it_mlgt-lgpla.
            MODIFY t_list INDEX 1 TRANSPORTING vltyp vlpla.
          ENDIF.
        ELSE.
          e_mess  = text-w13.
          zresult = text-m02.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFUNCTION.
