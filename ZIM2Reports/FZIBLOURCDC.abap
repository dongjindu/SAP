FORM CD_CALL_ZIBLOUR                       .
   IF   ( UPD_ZTBLOUR                        NE SPACE )
     OR ( UPD_ICDTXT_ZIBLOUR         NE SPACE )
   .
     CALL FUNCTION 'ZIBLOUR_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTBLOUR
                      = *ZTBLOUR
                  N_ZTBLOUR
                      = ZTBLOUR
                  UPD_ZTBLOUR
                      = UPD_ZTBLOUR
                  UPD_ICDTXT_ZIBLOUR
                      = UPD_ICDTXT_ZIBLOUR
          TABLES  ICDTXT_ZIBLOUR
                      = ICDTXT_ZIBLOUR
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
