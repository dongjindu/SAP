FORM CD_CALL_ZTIMIMGTX                     .
   IF   ( UPD_ZTIMIMGTX                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMGTX       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMGTX_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMGTX
                      = *ZTIMIMGTX
                  N_ZTIMIMGTX
                      = ZTIMIMGTX
                  UPD_ZTIMIMGTX
                      = UPD_ZTIMIMGTX
                  UPD_ICDTXT_ZTIMIMGTX
                      = UPD_ICDTXT_ZTIMIMGTX
          TABLES  ICDTXT_ZTIMIMGTX
                      = ICDTXT_ZTIMIMGTX
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
