REPORT ztbm_bom_test
LINE-SIZE 100
LINE-COUNT 100.

DATA w_one LIKE mara-matnr.
DATA w_time(5) TYPE c.
DATA w_date(5) TYPE c.

MOVE 'TIME' TO w_time.
MOVE 'DATE' TO w_date.

WRITE w_time NO-GAP.

WRITE sy-uzeit NO-GAP.
ULINE (15).
WRITE w_date.
POSITION 4.
SKIP.
WRITE sy-datum MM/DD/YYYY.
