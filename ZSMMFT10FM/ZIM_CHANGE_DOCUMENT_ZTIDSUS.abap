FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDSUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDSUS) LIKE  ZTIDSUS STRUCTURE  ZTIDSUS
*"     REFERENCE(O_ZTIDSUS) LIKE  ZTIDSUS STRUCTURE  ZTIDSUS
*"----------------------------------------------------------------------
   DATA : OBJECID      LIKE    CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTIDSUS+3(15).

   CALL FUNCTION 'ZTIDSUS_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            PLANNED_CHANGE_NUMBER   = ''
            OBJECT_CHANGE_INDICATOR = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            N_ZTIDSUS          =    N_ZTIDSUS
            O_ZTIDSUS          =    O_ZTIDSUS
            UPD_ZTIDSUS        =    UPD_CHNGIND.

ENDFUNCTION.
