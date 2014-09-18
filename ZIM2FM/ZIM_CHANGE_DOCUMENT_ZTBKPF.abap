FUNCTION ZIM_CHANGE_DOCUMENT_ZTBKPF.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBKPF) LIKE  ZTBKPF STRUCTURE  ZTBKPF
*"     REFERENCE(O_ZTBKPF) LIKE  ZTBKPF STRUCTURE  ZTBKPF
*"----------------------------------------------------------------------
   DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR: IT_CDTXT.
   OBJECID = N_ZTBKPF+3(18).

   CALL FUNCTION 'ZTBKPF_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBKPF           =    N_ZTBKPF
            O_ZTBKPF           =    O_ZTBKPF
            UPD_ZTBKPF         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTBKPF       = ''
      tables
            ICDTXT_ZTBKPF           =    IT_CDTXT.

ENDFUNCTION.
