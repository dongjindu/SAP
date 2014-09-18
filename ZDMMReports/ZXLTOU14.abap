*----------------------------------------------------------------------*
*   INCLUDE ZXLTOU14                                                   *
*----------------------------------------------------------------------*


**--- input Storage Type & Storage Bin => Source
  MOVE : i_ltap-nltyp TO i_ltap-vltyp,     " Storage Type
         i_ltap-nlpla TO i_ltap-vlpla.     " Storage Bin


**--- default Storage Type & Storage Bin => Dest.
  MOVE : i_t333-nltyp TO i_ltap-nltyp,     " Storage Type
         i_t333-nlpla TO i_ltap-nlpla.     " Storage Bin


**---     End      ---**
