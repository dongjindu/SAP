************************************************************************
* Program Name      : ZAPP801R_WO_DAILY_CLOSING
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.01.26.
* Specifications By : B. Choi
* Pattern           : 2.1
* Development Request No : UD1K906372
* Addl Documentation:
* Description       : Daily Closing For Work Order Summary
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zapp801r_wo_daily_closing MESSAGE-ID zmpp.

TABLES : ztpp_wosum.  "ERP_WO QTY SUMMARY
INCLUDE <icon>.
INCLUDE <list>.

SELECTION-SCREEN  BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_flg  TYPE c .
SELECTION-SCREEN  END OF BLOCK blk1.

***********************************************************************
INITIALIZATION.
***********************************************************************

***********************************************************************
AT SELECTION-SCREEN.
***********************************************************************
  IF p_flg = space.
    MESSAGE e000 WITH 'Set The Flag !!!'.
    EXIT.
  ENDIF.

***********************************************************************
START-OF-SELECTION.
***********************************************************************
  COMMIT WORK.
  SELECT *
    FROM ztpp_wosum.

    MOVE: ztpp_wosum-t01dq TO ztpp_wosum-t01pq,
          ztpp_wosum-t06dq TO ztpp_wosum-t06pq,
          ztpp_wosum-t08dq TO ztpp_wosum-t08pq,
          ztpp_wosum-t12dq TO ztpp_wosum-t12pq,
          ztpp_wosum-t17dq TO ztpp_wosum-t17pq,
          ztpp_wosum-t20dq TO ztpp_wosum-t20pq,

          sy-datum         TO ztpp_wosum-aedat,
          sy-uzeit         TO ztpp_wosum-aezet,
          sy-uname         TO ztpp_wosum-aenam.

    CLEAR: ztpp_wosum-rp01dq,
           ztpp_wosum-rp02dq,
           ztpp_wosum-rp03dq,
           ztpp_wosum-rp04dq,
           ztpp_wosum-rp05dq,
           ztpp_wosum-rp06dq,
           ztpp_wosum-rp07dq,
           ztpp_wosum-rp08dq,
           ztpp_wosum-rp09dq,
           ztpp_wosum-rp10dq,
           ztpp_wosum-rp11dq,
           ztpp_wosum-rp12dq,
           ztpp_wosum-rp13dq,
           ztpp_wosum-rp14dq,
           ztpp_wosum-rp15dq,
           ztpp_wosum-rp16dq,

           ztpp_wosum-t01dq,
           ztpp_wosum-t06dq,
           ztpp_wosum-t08dq,
           ztpp_wosum-t12dq,
           ztpp_wosum-t17dq,
           ztpp_wosum-t20dq.

    MODIFY ztpp_wosum .
  ENDSELECT.
  IF sy-subrc <> 0.
    ROLLBACK WORK.
    MESSAGE e000 WITH TEXT-002 .
  ELSE.
    COMMIT WORK.
    MESSAGE i001 WITH TEXT-003 .
  ENDIF.

***********************************************************************
END-OF-SELECTION.
***********************************************************************
*
***********************************************************************
TOP-OF-PAGE.
***********************************************************************
*
