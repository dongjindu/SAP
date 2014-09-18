*----------------------------------------------------------------------*
*   INCLUDE RM07DOCS_CONTROL                                           *
*   This file contains data that are used commonly by RM07DOCS
*   at runtime and the customizing transaction for this report
*   (V_MMIM_REP_CUST).
*   This avoids data replication and inconsistencies.
*----------------------------------------------------------------------*

* The control structure holds the information describing field
* properties.
* Possible values for STATUS: + = mandatory, not removable
*                             - = not selectable
*                     COLOR:  + = colorize according to debit/cridit ind
*                             - = colorize AND swap sign
*                     FIAUTH: X = field blinded without auth F_BKPF_BUK
DATA: BEGIN OF cs,
        tabname(5),                  "MKPF or MSEG
        fieldname(12),               "fieldname
        selection(1),                "status for selection screen
        selection_position(2),       "mandatory position on sel screen
        output(1),                   "status for output list
        output_position(2),          "mandatory position on output list
        cqindicator(1),              "does it need a UNIT/CURR field?
        cqfieldname(5),              "what it's name
        color(1),                    "does it need to be colorized?
        fiauth(1),                   "does it require auth. checks?
      END OF cs.

DATA: BEGIN OF ct OCCURS 0.
        INCLUDE STRUCTURE cs.
DATA: END OF ct.

DATA: BEGIN OF rtt OCCURS 0.
        INCLUDE STRUCTURE cs.
DATA: END OF rtt.

DEFINE ax.
  cs = &1.
  append cs to ct.
END-OF-DEFINITION.

DEFINE rx.
  cs = &1.
  append cs to rtt.
END-OF-DEFINITION.

FORM build_controltable.
  REFRESH ct.
* Completely deactivated
* Reasons: Dublettes (e.g. MBLNR), obsolete (PLPLA), not filled (EQUNR)
  ax 'MKPF MANDT       -00-00'.
  ax 'MSEG MANDT       -00-00'.
  ax 'MSEG MBLNR       -00-00'.
  ax 'MSEG MJAHR       -00-00'.
  ax 'MSEG PLPLA       -00-00'.
  ax 'MSEG EQUNR       -00-00'.
  ax 'MSEG PROJN       -00-00'.
  ax 'MSEG BELNR       -00-00'.
  ax 'MSEG BUZEI       -00-00'.
  ax 'MSEG BELUM       -00-00'.
  ax 'MSEG BUZUM       -00-00'.
  ax 'MSEG ZEKKN       -00-00'.
  ax 'MSEG XSKST       -00-00'.
  ax 'MSEG XSAUF       -00-00'.
  ax 'MSEG XSPRO       -00-00'.
  ax 'MSEG XSERG       -00-00'.
  ax 'MSEG PBAMG       -00-00'.

* Fields from default selection screen
  ax 'MSEG MATNR       +01+01'.
  ax 'MSEG WERKS       +02+02'.
  ax 'MSEG LGORT       +03+03'.
  ax 'MSEG CHARG       +04+00'.
  ax 'MSEG LIFNR       +05+00'.
  ax 'MSEG KUNNR       +06+00'.
  ax 'MSEG BWART       +07+04'.
  ax 'MSEG SOBKZ       +08+05'.
  ax 'MKPF BUDAT       +09+08'.
  ax 'MKPF USNAM       +10+00'.
  ax 'MKPF VGART       +11+00'.
* Remaining fields necessary for selections
  ax 'MKPF MBLNR        00+06'.
  ax 'MKPF MJAHR        00+00'.
  ax 'MKPF CPUDT        00+00'.
  ax 'MKPF CPUTM        00+00'.
  ax 'MSEG ZEILE        00+07'.
  ax 'MSEG SHKZG        00+00'.
  ax 'MSEG EBELN        00+00'.
  ax 'MSEG EBELP        00+00'.
  ax 'MSEG KZBEW        00+00'.
  ax 'MSEG KZZUG        00+00'.
  ax 'MSEG KZVBR        00+00'.
  ax 'MSEG PS_PSP_PNR   00+00'.
  ax 'MSEG NPLNR        00+00'.                                  "215929
  ax 'MSEG APLZL        00+00'.                                  "215929
  ax 'MSEG AUFPL        00+00'.                                  "215929
* Quantity fields and references
  ax 'MSEG MENGE        00+00QMEINS-'.
  ax 'MSEG MEINS        00+00      +'.
  ax 'MSEG ERFMG        00+09QERFME-'.
  ax 'MSEG ERFME        00+10      +'.
  ax 'MSEG BPMNG        00+00QBPRME-'.
  ax 'MSEG BPRME        00+00      +'.
  ax 'MSEG BSTMG        00 00QBSTME-'.
  ax 'MSEG BSTME        00 00      +'.
  ax 'MSEG LBKUM        00 00QMEINS '.
  ax 'MSEG LSMNG        00 00QLSMEH-'.
  ax 'MSEG LSMEH        00 00      +'.
* Currency fields
  ax 'MSEG WAERS        00 00      +X'.
  ax 'MSEG DMBTR        00 00CWAERS-X'.
  ax 'MSEG BNBTR        00 00CWAERS-X'.
  ax 'MSEG BUALT        00 00CWAERS-X'.
  ax 'MSEG DMBUM        00 00CWAERS-X'.
  ax 'MSEG EXBWR        00 00CWAERS- '.
  ax 'MSEG VKWRT        00 00CWAERS-X'.
  ax 'MSEG EXVKW        00 00CWAERS- '.
  ax 'MSEG SALK3        00 00CWAERS X'.
  ax 'MSEG VKWRA        00 00CWAERS-X'.
  ax 'MSEG J_1BEXBASE   00 00CWAERS-X'.
ENDFORM.
