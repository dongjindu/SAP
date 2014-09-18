*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU01                                                   *
*----------------------------------------------------------------------*

**---
*  LOOP AT t_ltap_vb.
*    MOVE : t_ltap_vb-nltyp TO t_ltap_vb-vltyp,     " Storage Type
*           t_ltap_vb-nlpla TO t_ltap_vb-vlpla.     " Storage Bin
*    SELECT SINGLE nltyp
*                  nlpla
*                        INTO (t_ltap_vb-nltyp, t_ltap_vb-nlpla)
*                        FROM t333
*                       WHERE lgnum EQ 'P01'
*                         AND bwlvs EQ '311'.
*    MODIFY t_ltap_vb.
*  ENDLOOP.
*
***---
*  TABLES : ztmm_dock.
*
*  MOVE : '1234567890' TO ztmm_dock-zdock.
*
*  MODIFY ztmm_dock.


*call function 'Y_TEST_TO_CREATION' in update task
*     exporting
*          i_ltak_vb = i_ltak_vb
*     tables
*          t_ltap_vb = t_ltap_vb.




*
