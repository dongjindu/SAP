FUNCTION Z_FBM_ABXCNTDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_ABXCNTDT STRUCTURE  ZTBM_ABXCNTDT
*"----------------------------------------------------------------------
************************************************************************
* Author                 : Haseeb Mohammad
* Creation Date          : 01/13/2006
* Specifications By      : Mr.IM CHOI (BOM)
* Development Request No :
* Addl documentation     :
* Description            : DATA count from HMC for BOM interfaces
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************


  DATA: it_abxcntdt type ztbm_abxcntdt occurs 0 with header line.


  LOOP at t_abxcntdt.
    MOVE-CORRESPONDING t_abxcntdt TO it_abxcntdt.
    MOVE : sy-uname TO it_abxcntdt-zuser,
           sy-datum TO it_abxcntdt-zsdat,
           sy-uzeit TO it_abxcntdt-zstim.
    APPEND it_abxcntdt.

  ENDLOOP.

INSERT  ztbm_abxcntdt FROM table it_abxcntdt.

IF sy-subrc = 0.
  t_abxcntdt-zresult = 'S'.
  MODIFY t_abxcntdt TRANSPORTING zresult WHERE zcnt_crdt <> space.
ELSE.
  t_abxcntdt-zresult = 'E'.
  MODIFY t_abxcntdt TRANSPORTING zresult WHERE zcnt_crdt <> space.

ENDIF.

ENDFUNCTION.
