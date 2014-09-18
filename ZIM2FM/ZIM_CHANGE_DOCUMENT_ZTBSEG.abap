FUNCTION ZIM_CHANGE_DOCUMENT_ZTBSEG.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBSEG) LIKE  ZTBSEG STRUCTURE  ZTBSEG
*"     REFERENCE(O_ZTBSEG) LIKE  ZTBSEG STRUCTURE  ZTBSEG
*"----------------------------------------------------------------------
   DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR: IT_CDTXT.
   OBJECID = N_ZTBSEG+3(21).

   CALL FUNCTION 'ZTBSEG_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBSEG           =    N_ZTBSEG
            O_ZTBSEG           =    O_ZTBSEG
            UPD_ZTBSEG         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTBSEG       = ''
      tables
            ICDTXT_ZTBSEG           =    IT_CDTXT.

ENDFUNCTION.
