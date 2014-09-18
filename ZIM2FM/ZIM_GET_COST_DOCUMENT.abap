FUNCTION ZIM_GET_COST_DOCUMENT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFIMDNO) LIKE  ZTBKPF-ZFIMDNO
*"     VALUE(ZFCSTGRP) LIKE  ZTBKPF-ZFCSTGRP
*"     VALUE(ZFCSTGRP1) LIKE  ZTBKPF-ZFCSTGRP DEFAULT SPACE
*"  TABLES
*"      IT_ZSIMCOST STRUCTURE  ZSIMCOST
*"----------------------------------------------------------------------

   REFRESH : IT_ZSIMCOST.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIMCOST
                               FROM   ZVIMCOST
                               WHERE ( ZFCSTGRP EQ ZFCSTGRP
                               OR      ZFCSTGRP EQ ZFCSTGRP1 )
                               AND     ZFIMDNO  EQ ZFIMDNO.

ENDFUNCTION.
