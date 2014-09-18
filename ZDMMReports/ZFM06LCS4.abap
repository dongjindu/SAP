*eject
*--------------------------------------------------------------------*
*        COMMON DATA                                                 *
*--------------------------------------------------------------------*
*        Selektionsbedingungen Teil 4                                *
*--------------------------------------------------------------------*

DATA:    BEGIN OF COMMON PART FM06LCS4.

SELECT-OPTIONS: S_PSTYP FOR RM06E-EPSTP,
                S_KNTTP FOR EKPO-KNTTP,
                S_EINDT FOR EKET-EINDT,
                S_ANGDT FOR EKKO-ANGDT.
PARAMETERS:     P_GULDT LIKE RM06A-P_GULDT,
                P_RWEIT LIKE RM06A-P_RWEIT.

*- für Positionstyp im internen Format --------------------------------*
RANGES: R_PSTYP FOR EKPO-PSTYP,
        R_BSTYP FOR EKKO-BSTYP,
        R_WEPOS FOR EKPO-WEPOS,
        R_REPOS FOR EKPO-REPOS,
        R_RTPOS FOR EKPO-RETPO,
        R_LOEKZ FOR EKPO-LOEKZ,
*{   REPLACE        KA5K001235                                        1
*\        R_MATNR FOR EKPO-MATNR,
        R_MATNR FOR EKPO-MATNR,
        R_MFRPN FOR MARA-MFRPN,
*}   REPLACE
        R_EKORG FOR EKKO-EKORG.

DATA:    END OF COMMON PART.

