*  69970  27.01.1999  CF
*--------------------------------------------------------------------*
*        COMMON DATA                                                 *
*--------------------------------------------------------------------*
*        Selektionsbedingungen 1                                     *
*--------------------------------------------------------------------*

DATA:    BEGIN OF COMMON PART FM06LCS1.

PARAMETERS:     LISTU LIKE T160O-LISTU.
SELECT-OPTIONS: SELPA FOR T160T-SELPA MEMORY ID SEL,
                S_BSART FOR EKKO-BSART,
                S_EKGRP FOR EKKO-EKGRP MEMORY ID EKG.            "69970

DATA:    END OF COMMON PART.
