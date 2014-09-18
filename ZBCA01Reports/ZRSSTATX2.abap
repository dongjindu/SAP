***INCLUDE RSSTATX2.

* --- Extended single record statistic ------------------------------- *

DATA  V2_NORMAL_RECORDS                LIKE SAPWLPFNRM OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_BTC_STEP_RECORDS              LIKE SAPWLPFBTC OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_TABLE_RECORDS                 LIKE SAPWLPFTAB    OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_RFC_CLIENT_RECORDS            LIKE SAPWLPFRC  OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_RFC_SERVER_RECORDS            LIKE SAPWLPFRS  OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_RFC_CLIENT_DEST_RECORDS       LIKE SAPWLPFRCD OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_RFC_SERVER_DEST_RECORDS       LIKE SAPWLPFRSD OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_SPOOL_PRINT_RECORDS           LIKE SAPWLPFSPP OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_SPOOL_ACTIVITY_RECORDS        LIKE SAPWLPFSPA OCCURS 0
                                            WITH HEADER LINE.
DATA  NORM_SUBRECORD_INDEX             LIKE SAPWLSRCIX OCCURS 0
                                            WITH HEADER LINE.
DATA  V2_RFC_TIME_INT_RECORDS          LIKE SAPWLPFTII OCCURS 0
                                            WITH HEADER LINE.

* --- Transaction ID for the link RFC => ALE ------------------------- *

DATA  TID                              LIKE SAPWLPFRC-TID.
