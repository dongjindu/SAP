FUNCTION Z_FPM_EXIT_SAPMIEQ0_001.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(DATA_EQUI) LIKE  EQUI STRUCTURE  EQUI
*"  TABLES
*"      E_EQUI STRUCTURE  EQUI
*"----------------------------------------------------------------------
  DATA: SHOP LIKE	EQUI-SHOP,
        LINE LIKE	EQUI-LINE,
        REMAIN(10).

  MOVE-CORRESPONDING DATA_EQUI TO E_EQUI.

  SPLIT DATA_EQUI-EQUNR AT '-' INTO E_EQUI-SHOP E_EQUI-LINE REMAIN.

  APPEND E_EQUI.

  UPDATE EQUI SET: SHOP  = E_EQUI-SHOP
                   LINE  = E_EQUI-LINE
            WHERE  EQUNR = DATA_EQUI-EQUNR.

ENDFUNCTION.
