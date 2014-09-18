*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM01R_6005TOP                                          *
*----------------------------------------------------------------------*
TABLES: zbdcmsgcoll.
DATA w_subrc LIKE sy-subrc.          "For Global Return code

DATA xekpo      LIKE TABLE OF uekpo.  "Item data OF Purchasing Doc.
DATA i_ekko     LIKE ekko.            "Heade data OF Purchasing Doc.
DATA: it_i_ekko LIKE TABLE OF i_ekko.
DATA: wa_xekpo  LIKE LINE OF xekpo.   "Structure for Item data
DATA: wa_eine   LIKE eine.      "Structure for Purchasing Info Record

**** Constants&Vars for Number range object ****************************
  CONSTANTS: nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "AppDocNo Part.
* Number range object
  DATA:      nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      nro_number  TYPE num10.      " Same type of nro_object

  DATA: w_zdocno TYPE num10.     "Application Doc no.
