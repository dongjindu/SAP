FUNCTION ZIM_CHANGE_DOCUMENT_CIVIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCIVIT) LIKE  ZTCIVIT STRUCTURE  ZTCIVIT
*"     REFERENCE(O_ZTCIVIT) LIKE  ZTCIVIT STRUCTURE  ZTCIVIT
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTCIVIT+3(15).

   CALL FUNCTION 'ZICIVIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCIVIT          =    N_ZTCIVIT
            O_ZTCIVIT          =    O_ZTCIVIT
            UPD_ZTCIVIT        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICIVIT      = ''
      tables
            ICDTXT_ZICIVIT          =    it_CdtXT.

ENDFUNCTION.
