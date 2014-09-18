FUNCTION z_fbm_abxtrtdt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXTRTDT STRUCTURE  ZSBM_ABXTRTDT_RFC
*"----------------------------------------------------------------------
  DATA: it_abxtrtdt TYPE ztbm_abxtrtdt OCCURS 0 WITH HEADER LINE.
  DATA: ls_abxtrtdt TYPE ztbm_abxtrtdt.
  DATA: l_subrc TYPE sy-subrc.
*Issue num : PP-20040728-001 :2004.08.12
*Changed by wskim, Requested by iychoi
*---start
*  LOOP AT t_abxtrtdt.
*    MOVE-CORRESPONDING t_abxtrtdt TO it_abxtrtdt.
*    APPEND it_abxtrtdt.
*  ENDLOOP.
***Added By Tonkey 05/21/2004.
*  LOOP AT it_abxtrtdt.
*    SELECT SINGLE *
*      INTO ls_abxtrtdt
*      FROM ztbm_abxtrtdt
*      WHERE natn_c = it_abxtrtdt-natn_c AND
*            delr_c = it_abxtrtdt-delr_c   .
*    IF sy-subrc = 0.
*      READ TABLE t_abxtrtdt WITH KEY natn_c = it_abxtrtdt-natn_c
*                                     delr_c = it_abxtrtdt-delr_c.
*      t_abxtrtdt-zzret = 'E'.
*      MODIFY t_abxtrtdt INDEX sy-tabix .
*      CONTINUE.
*    ENDIF.
**
*    INSERT INTO ztbm_abxtrtdt VALUES it_abxtrtdt.
**
*    IF sy-subrc EQ 0.
*      COMMIT WORK.
*      READ TABLE t_abxtrtdt WITH KEY natn_c = it_abxtrtdt-natn_c
*                                     delr_c = it_abxtrtdt-delr_c.
*      t_abxtrtdt-zzret = 'S'.
*      MODIFY t_abxtrtdt INDEX sy-tabix .
*    ELSE.
*      ROLLBACK WORK.
*      t_abxtrtdt-zzret = 'E'.
*      MODIFY t_abxtrtdt TRANSPORTING zzret
*                               WHERE natn_c GE space
*                               AND   delr_c GE space
*                               and   zzret  <> 'S' .
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.
*----end
* Inactivated By Tonkey 05/21/2004.

  LOOP AT t_abxtrtdt.
    MOVE-CORRESPONDING t_abxtrtdt TO it_abxtrtdt.
    APPEND it_abxtrtdt.
  ENDLOOP.

*Change : because of duplicate
*  INSERT ZTBM_ABXTRTDT FROM TABLE IT_ABXTRTDT ACCEPTING DUPLICATE KEYS.
  MODIFY ztbm_abxtrtdt FROM TABLE it_abxtrtdt.

  IF sy-subrc EQ 0.
    COMMIT WORK.
    t_abxtrtdt-zzret = 'S'.
    MODIFY t_abxtrtdt TRANSPORTING zzret
                             WHERE natn_c GE space
                             AND   delr_c GE space.
  ELSE.
    ROLLBACK WORK.
    t_abxtrtdt-zzret = 'E'.
    MODIFY t_abxtrtdt TRANSPORTING zzret
                             WHERE natn_c GE space
                             AND   delr_c GE space.
  ENDIF.
ENDFUNCTION.
