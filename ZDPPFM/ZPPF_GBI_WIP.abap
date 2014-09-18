FUNCTION zppf_gbi_wip.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_IFRESULT) TYPE  ZRESULT
*"     VALUE(E_IFFAILMSG) TYPE  BAPIRETURN-MESSAGE
*"  TABLES
*"      PT_WIP STRUCTURE  ZSPP_GBI_WIP
*"----------------------------------------------------------------------
  DATA: it_data LIKE TABLE OF zspp_gbi_wip WITH HEADER LINE.

  DATA: BEGIN OF it_wip OCCURS 0,
          objek         LIKE   ausp-objek,
          klart         LIKE   ausp-klart,
          rpid          LIKE   ausp-atwrt,
          usage         LIKE   ausp-atwrt,
          worder        LIKE   ausp-atwrt,
          model        LIKE   ausp-atwrt,
       END OF it_wip.

  DATA: l_rp_status LIKE cabn-atinn,
        l_usage_car LIKE cabn-atinn,
        l_worder    LIKE cabn-atinn,
        l_model     LIKE cabn-atinn,
        l_nation(3).

  RANGES: r_usage FOR ausp-atwrt.

  CONSTANTS: c_rp_status LIKE cabn-atnam VALUE 'P_RP_STATUS',
             c_usage_car LIKE cabn-atnam VALUE 'P_USAGE_CAR',
             c_worder    LIKE cabn-atnam VALUE 'P_WORK_ORDER',
             c_model     LIKE cabn-atnam VALUE 'P_MODEL',
             c_werks     LIKE t001w-werks VALUE 'P001'.

  SELECT SINGLE atinn INTO l_rp_status
     FROM cabn WHERE atnam = c_rp_status.

  SELECT SINGLE atinn INTO l_usage_car
    FROM cabn WHERE atnam = c_usage_car.

  SELECT SINGLE atinn INTO l_worder
    FROM cabn WHERE atnam = c_worder.

  SELECT SINGLE atinn INTO l_model
    FROM cabn WHERE atnam = c_model.

  SELECT a~objek a~klart a~atwrt AS rpid
         b~atwrt AS usage
         c~atwrt AS worder
         d~atwrt AS model
    INTO CORRESPONDING FIELDS OF TABLE it_wip
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                      ON b~objek = a~objek
                     AND b~atinn = l_usage_car
                     AND b~mafid = a~mafid
                     AND b~klart = a~klart
                   LEFT OUTER JOIN ausp AS c
                      ON c~objek = a~objek
                     AND c~atinn = l_worder
                     AND c~mafid = a~mafid
                     AND c~klart = a~klart
                     LEFT OUTER JOIN ausp AS d
                      ON d~objek = a~objek
                     AND d~atinn = l_model
                     AND d~mafid = a~mafid
                     AND d~klart = a~klart
   WHERE a~klart EQ '002'
     AND a~atinn EQ l_rp_status
     AND a~atwrt IN ('01','02','03','04','05',
                     '06','07','08','09','10',
                     '11','12','13','14','15',
                     '16','17','18','19','20',
                     '21','22','23','24','26').

  r_usage-option = 'EQ'.
  r_usage-sign = 'I'.
  r_usage-low = 'D'.
  APPEND r_usage.
  r_usage-low = 'S'.
  APPEND r_usage.
  r_usage-low = '2'.
  APPEND r_usage.

  DELETE it_wip WHERE usage = ' '.
  DELETE it_wip WHERE usage IN r_usage.

  LOOP AT it_wip.
    CASE it_wip-worder+12(2).
      WHEN 'XX'.         "BIW
        IF it_wip-rpid > '01'.
          DELETE it_wip. CONTINUE.
        ENDIF.
      WHEN 'XY'.         "BIP
        IF it_wip-rpid > '05'.
          DELETE it_wip. CONTINUE.
        ENDIF.
      WHEN 'XA'.         "Pilot Car
        IF it_wip-rpid > '17'.
          DELETE it_wip. CONTINUE.
        ENDIF.
    ENDCASE.

    CLEAR: it_data, l_nation.
    it_data-mandt = sy-mandt.
    it_data-bwerk = c_werks.
    it_data-model_code = it_wip-model+0(2).
    l_nation = it_wip-worder+9(3).
    CASE it_wip-rpid.
      WHEN 1.
        it_data-body_d = it_data-body_d + 1.
      WHEN 2 OR 3 OR 4.
        it_data-paint_d = it_data-paint_d + 1.
      WHEN  5 or 6.
        it_data-pbs_d = it_data-pbs_d + 1.
      WHEN 18.
        it_data-signoff_d = it_data-signoff_d + 1.
* changed by wayne by GBI's request on 8/20/2013
        it_data-inv_d = it_data-inv_d + 1.
*
      WHEN OTHERS.
        IF it_wip-rpid < 18.
          it_data-trim_d = it_data-trim_d + 1.
        ELSE.
          IF l_nation <> 'B28'.
*            it_data-inv_d = it_data-inv_d + 1.
             it_data-longinv_d = it_data-longinv_d + 1.
          ELSE.
            IF  it_wip-rpid < 23.
*              it_data-inv_d = it_data-inv_d + 1.
               it_data-longinv_d = it_data-longinv_d + 1.
            ELSE.
* changed by wayne to exclude HMA stock by GBI's request on 8/20/2013
*              it_data-longinv_d = it_data-longinv_d + 1.
*
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
    COLLECT it_data.
  ENDLOOP.
  it_data-ernam = sy-uname.
  it_data-erdat = sy-datum.
  it_data-erzet = sy-uzeit.
  it_data-AENAM = it_data-ernam.
  it_data-AEDAT = it_data-erdat.
  it_data-AEZET =  it_data-erzet.
  MODIFY it_data TRANSPORTING ernam erdat erzet
        AENAM AEDAT AEZET
      WHERE model_code <> ' '.

  pt_wip[] = it_data[].

  IF sy-subrc = 0.
    e_ifresult = 'S'.
    e_iffailmsg = 'Success'.
  ELSE.
    e_ifresult = 'E'.
    e_iffailmsg = 'No Data found'.
  ENDIF.
ENDFUNCTION.
