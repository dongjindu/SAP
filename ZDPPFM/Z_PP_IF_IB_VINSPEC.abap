FUNCTION z_pp_if_ib_vinspec.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      T_DATA STRUCTURE  ZSPP_VIN_IF
*"----------------------------------------------------------------------

  DATA : BEGIN OF it_vin OCCURS 0.
          INCLUDE STRUCTURE ztpp_vin_if.
          INCLUDE STRUCTURE zmms0053.
  DATA : END OF it_vin.

  DATA : it_vin_if  LIKE ztpp_vin_if OCCURS 0 WITH HEADER LINE.
*  DATA : it_wosum    LIKE ztpp_wosum   OCCURS 0 WITH HEADER LINE.
  DATA : it_save    LIKE ztpp_vin_if OCCURS 0 WITH HEADER LINE.
  DATA : it_save_log LIKE ztpp_vin_if_l OCCURS 0 WITH HEADER LINE.
  DATA : l_zseq(10) TYPE n,
         l_index    TYPE sy-tabix.


  CHECK t_data[]  IS NOT INITIAL.

  LOOP AT t_data.
    it_vin-wo_serial = t_data-wo_spec+0(9).
    it_vin-wo_nation = t_data-wo_spec+9(3).
    it_vin-wo_dealer = t_data-wo_spec+12(2).
    it_vin-vin_spec  = t_data-vin_spec.

    APPEND it_vin.
  ENDLOOP.

  SELECT zseq INTO l_zseq
    FROM ztpp_vin_if_l
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_wosum
*  FROM ztpp_wosum
*    FOR ALL ENTRIES IN it_vin
*  WHERE wo_ser =  it_vin-wo_serial
*    AND nation =  it_vin-wo_nation
*    AND dealer =  it_vin-wo_dealer.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vin_if
  FROM ztpp_vin_if
    FOR ALL ENTRIES IN it_vin
  WHERE wo_serial =  it_vin-wo_serial
    AND wo_nation =  it_vin-wo_nation
    AND wo_dealer =  it_vin-wo_dealer.

*  SORT it_wosum   BY  wo_ser    nation dealer.
  SORT it_vin_if BY  wo_serial wo_nation wo_dealer.

*  DELETE  ADJACENT DUPLICATES FROM it_wosum
*                                      COMPARING wo_ser nation dealer.

*-save data
  LOOP AT it_vin.
    l_index   =  sy-tabix.

    CLEAR :  it_vin_if.
*    READ TABLE it_wosum  WITH KEY wo_ser = it_vin-wo_serial
*                                  nation = it_vin-wo_nation
*                                  dealer = it_vin-wo_dealer
*                                  BINARY SEARCH.
*    IF sy-subrc <> 0.
*      it_vin-type    = 'E'.
*      it_vin-message = 'W/O does not exist in ztpp_wohd'.
*    ELSE.
      READ TABLE it_vin_if WITH KEY wo_serial = it_vin-wo_serial
                                    wo_nation = it_vin-wo_nation
                                    wo_dealer = it_vin-wo_dealer
                                    BINARY SEARCH.
      IF sy-subrc = 0.
        it_vin-aenam  = sy-uname.
        it_vin-aedat  = sy-datum.
        it_vin-aezet  = sy-uzeit.
        it_vin-ernam  = it_vin_if-ernam.
        it_vin-erdat  = it_vin_if-erdat.
        it_vin-erzet  = it_vin_if-erzet.
      ELSE.
        it_vin-ernam  = sy-uname.
        it_vin-erdat  = sy-datum.
        it_vin-erzet  = sy-uzeit.
      ENDIF.

      MOVE-CORRESPONDING it_vin TO it_save.
      APPEND it_save.
*    ENDIF.

*-< save in the log
    MOVE-CORRESPONDING it_vin TO it_save_log.
    it_save_log-zdate = sy-datum.
    it_save_log-zseq  = l_zseq + l_index.
    it_save_log-ztime = sy-uzeit.
    it_save_log-ernam = sy-uname.
    IF it_vin-type = 'E'.
      it_save_log-zrslt = 'E'.
      it_save_log-zmsg  = it_vin-message.
    ELSE.
      it_save_log-zrslt = 'S'.
    ENDIF.
    APPEND it_save_log.
*->

    MODIFY it_vin.
    CLEAR : it_save, it_save_log.
  ENDLOOP.

  MODIFY ztpp_vin_if   FROM TABLE it_save.
  INSERT ztpp_vin_if_l FROM TABLE it_save_log
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFUNCTION.
