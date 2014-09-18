FUNCTION ZIM_CHANGE_DOCUMENT_ZTCCHD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCCHD) LIKE  ZTCCHD STRUCTURE  ZTCCHD
*"     REFERENCE(O_ZTCCHD) LIKE  ZTCCHD STRUCTURE  ZTCCHD
*"----------------------------------------------------------------------
   DATA : OBJECID  LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR: IT_CDTXT.
   OBJECID = N_ZTCCHD+3(10).

   CALL FUNCTION 'ZTCCHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCCHD           =    N_ZTCCHD
            O_ZTCCHD           =    O_ZTCCHD
            UPD_ZTCCHD         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTCCHD       = ''
      tables
            ICDTXT_ZTCCHD           =    IT_CDTXT.

ENDFUNCTION.
