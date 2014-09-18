REPORT zco_ppcstep2_del .

PARAMETERS: p_id LIKE ppc_step2-matpostid,
            p_run(6)  TYPE c.

IF p_run = 'PPCGO2'.
  DELETE FROM ppc_step2 WHERE matpostid = p_id.
  WRITE:/ sy-subrc, '...DELETED'.
ENDIF.
