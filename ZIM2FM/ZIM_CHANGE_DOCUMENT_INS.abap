FUNCTION ZIM_CHANGE_DOCUMENT_INS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTINS) LIKE  ZTINS STRUCTURE  ZTINS
*"     REFERENCE(O_ZTINS) LIKE  ZTINS STRUCTURE  ZTINS
*"     REFERENCE(N_ZTINSRSP) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"     REFERENCE(O_ZTINSRSP) LIKE  ZTINSRSP STRUCTURE  ZTINSRSP
*"     REFERENCE(N_ZTINSSG3) LIKE  ZTINSSG3 STRUCTURE  ZTINSSG3
*"     REFERENCE(O_ZTINSSG3) LIKE  ZTINSSG3 STRUCTURE  ZTINSSG3
*"----------------------------------------------------------------------
   DATA : OBJECID     LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTINS+3(20).

   CALL FUNCTION 'ZIINDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTINS            =    N_ZTINS
            O_ZTINS            =    O_ZTINS
            UPD_ZTINS          =    UPD_CHNGIND
            N_ZTINSRSP         =    N_ZTINSRSP
            O_ZTINSRSP         =    O_ZTINSRSP
            UPD_ZTINSRSP       =    UPD_CHNGIND
            N_ZTINSSG3         =    N_ZTINSSG3
            O_ZTINSSG3         =    O_ZTINSSG3
            UPD_ZTINSSG3       =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIINDOC      = ''
      tables
            ICDTXT_ZIINDOC     =    it_CdtXT.

ENDFUNCTION.
