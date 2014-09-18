
FUNCTION ZIM_CHANGE_DOCUMENT_ZTBWHD .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBWHD) LIKE  ZTBWHD STRUCTURE  ZTBWHD
*"     REFERENCE(O_ZTBWHD) LIKE  ZTBWHD STRUCTURE  ZTBWHD
*"----------------------------------------------------------------------

 DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR:    IT_CDTXT.
   OBJECID = N_ZTBWHD+3(13).

   CALL FUNCTION 'ZTBWHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBWHD           =    N_ZTBWHD
            O_ZTBWHD           =    O_ZTBWHD
            UPD_ZTBWHD         =    UPD_CHNGIND
            object_change_indicator =  'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''

      tables
            ICDTXT_ZTBWHD           =    IT_CDTXT.


ENDFUNCTION.
