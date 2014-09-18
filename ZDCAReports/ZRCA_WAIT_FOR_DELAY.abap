************************************************************************
* Program Name      : ZRCA_WAIT_FOR_DELAY
* Remarks: This program is designed for delay other program's running
*  time in batchjob
************************************************************************

REPORT ZRCA_WAIT_FOR_DELAY NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID ZMMM.


SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
parameters: P_delay(5) DEFAULT '1'.
SELECTION-SCREEN END OF BLOCK BLOCK1.
CONDENSE P_delay .

START-OF-SELECTION.
  write: 'Waiting for '.
  write: 14 p_delay no-gap, 'seconds'.
  wait up to p_delay seconds.

END-OF-SELECTION.
