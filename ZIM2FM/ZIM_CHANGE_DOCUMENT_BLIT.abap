FUNCTION ZIM_CHANGE_DOCUMENT_BLIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLIT) LIKE  ZTBLIT STRUCTURE  ZTBLIT
*"     REFERENCE(O_ZTBLIT) LIKE  ZTBLIT STRUCTURE  ZTBLIT
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTBLIT+3(15).

   CALL FUNCTION 'ZIBLIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLIT           =    N_ZTBLIT
            O_ZTBLIT           =    O_ZTBLIT
            UPD_ZTBLIT         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLIT       = ''
      tables
            ICDTXT_ZIBLIT           =    it_CDTXT.

ENDFUNCTION.
