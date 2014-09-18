*----------------------------------------------------------------------*
*   INCLUDE ZEPP309L_HPCS_T                                            *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES : ZTBM_ABXHPCDT,
         ZTBM_ABXHPCDT01,
         AUSP,
         CABN.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_HPCS OCCURS 0,
        PQBG(1), "TYPEZTBM_ABXHPCDT-PQBG, "Type
        ZWORK(18), "TYPEZTBM_ABXHPCDT-ZWORK, "Work order
        CEXT(3), "TYPEZTBM_ABXHPCDT-CEXT, "External Color
        CINT(3), "TYPEZTBM_ABXHPCDT-CINT, "Internal Color
        SEQU(4), "TYPEZTBM_ABXHPCDT-ZSEQU, "
        FSCC(23), "TYPEZTBM_ABXHPCDT-ZFSCC, "
        STAT(20), "TYPEZTBM_ABXHPCDT-STAT, "Material status
        C001(4), "TYPEZTBM_ABXHPCDT-C001, "Construct Code 001
        C002(4), "TYPEZTBM_ABXHPCDT-C002, "Construct Code 002
        C003(4), "TYPEZTBM_ABXHPCDT-C003, "Construct Code 003
        C004(4), "TYPEZTBM_ABXHPCDT-C004, "Construct Code 004
        C005(4), "TYPEZTBM_ABXHPCDT-C005, "Construct Code 005
        C006(4), "TYPEZTBM_ABXHPCDT-C006, "Construct Code 006
        C007(4), "TYPEZTBM_ABXHPCDT-C007, "Construct Code 007
        C008(4), "TYPEZTBM_ABXHPCDT-C008, "Construct Code 008
        C009(4), "TYPEZTBM_ABXHPCDT-C009, "Construct Code 009
        C010(4), "TYPEZTBM_ABXHPCDT-C010, "Construct Code 010
        C011(4), "TYPEZTBM_ABXHPCDT-C011, "Construct Code 011
        C012(4), "TYPEZTBM_ABXHPCDT-C012, "Construct Code 012
        C013(4), "TYPEZTBM_ABXHPCDT-C013, "Construct Code 013
        C014(4), "TYPEZTBM_ABXHPCDT-C014, "Construct Code 014
        C015(4), "TYPEZTBM_ABXHPCDT-C015, "Construct Code 015
        C016(4), "TYPEZTBM_ABXHPCDT-C016, "Construct Code 016
        C017(4), "TYPEZTBM_ABXHPCDT-C017, "Construct Code 017
        C018(4), "TYPEZTBM_ABXHPCDT-C018, "Construct Code 018
        C019(4), "TYPEZTBM_ABXHPCDT-C019, "Construct Code 019
        C020(4), "TYPEZTBM_ABXHPCDT-C020, "Construct Code 020
        C021(4), "TYPEZTBM_ABXHPCDT-C021, "Construct Code 021
        C022(4), "TYPEZTBM_ABXHPCDT-C022, "Construct Code 022
        C023(4), "TYPEZTBM_ABXHPCDT-C023, "Construct Code 023
        C024(4), "TYPEZTBM_ABXHPCDT-C024, "Construct Code 024
        C025(4), "TYPEZTBM_ABXHPCDT-C025, "Construct Code 025
        C026(4), "TYPEZTBM_ABXHPCDT-C026, "Construct Code 026
        C027(4), "TYPEZTBM_ABXHPCDT-C027, "Construct Code 027
        C028(4), "TYPEZTBM_ABXHPCDT-C028, "Construct Code 028
        C029(4), "TYPEZTBM_ABXHPCDT-C029, "Construct Code 029
        C030(4), "TYPEZTBM_ABXHPCDT-C030, "Construct Code 030
        C031(4), "TYPEZTBM_ABXHPCDT-C031, "Construct Code 031
        C032(4), "TYPEZTBM_ABXHPCDT-C032, "Construct Code 032
        C033(4), "TYPEZTBM_ABXHPCDT-C033, "Construct Code 033
        C034(4), "TYPEZTBM_ABXHPCDT-C034, "Construct Code 034
        C035(4), "TYPEZTBM_ABXHPCDT-C035, "Construct Code 035
        C036(4), "TYPEZTBM_ABXHPCDT-C036, "Construct Code 036
        C037(4), "TYPEZTBM_ABXHPCDT-C037, "Construct Code 037
        C038(4), "TYPEZTBM_ABXHPCDT-C038, "Construct Code 038
        C039(4), "TYPEZTBM_ABXHPCDT-C039, "Construct Code 039
        C040(4), "TYPEZTBM_ABXHPCDT-C040, "Construct Code 040
        C041(4), "TYPEZTBM_ABXHPCDT-C041, "Construct Code 041
        C042(4), "TYPEZTBM_ABXHPCDT-C042, "Construct Code 042
        C043(4), "TYPEZTBM_ABXHPCDT-C043, "Construct Code 043
        C044(4), "TYPEZTBM_ABXHPCDT-C044, "Construct Code 044
        C045(4), "TYPEZTBM_ABXHPCDT-C045, "Construct Code 045
        C046(4), "TYPEZTBM_ABXHPCDT-C046, "Construct Code 046
        C047(4), "TYPEZTBM_ABXHPCDT-C047, "Construct Code 047
        C048(4), "TYPEZTBM_ABXHPCDT-C048, "Construct Code 048
        C049(4), "TYPEZTBM_ABXHPCDT-C049, "Construct Code 049
        C050(4), "TYPEZTBM_ABXHPCDT-C050, "Construct Code 050
        C051(4), "TYPEZTBM_ABXHPCDT-C051, "Construct Code 051
        C052(4), "TYPEZTBM_ABXHPCDT-C052, "Construct Code 052
        C053(4), "TYPEZTBM_ABXHPCDT-C053, "Construct Code 053
        C054(4), "TYPEZTBM_ABXHPCDT-C054, "Construct Code 054
        C055(4), "TYPEZTBM_ABXHPCDT-C055, "Construct Code 055
        C056(4), "TYPEZTBM_ABXHPCDT-C056, "Construct Code 056
        C057(4), "TYPEZTBM_ABXHPCDT-C057, "Construct Code 057
        C058(4), "TYPEZTBM_ABXHPCDT-C058, "Construct Code 058
        C059(4), "TYPEZTBM_ABXHPCDT-C059, "Construct Code 059
        C060(4), "TYPEZTBM_ABXHPCDT-C060, "Construct Code 060
        C061(4), "TYPEZTBM_ABXHPCDT-C061, "Construct Code 061
        C062(4), "TYPEZTBM_ABXHPCDT-C062, "Construct Code 062
        C063(4), "TYPEZTBM_ABXHPCDT-C063, "Construct Code 063
        C064(4), "TYPEZTBM_ABXHPCDT-C064, "Construct Code 064
        C065(4), "TYPEZTBM_ABXHPCDT-C065, "Construct Code 065
        C066(4), "TYPEZTBM_ABXHPCDT-C066, "Construct Code 066
        C067(4), "TYPEZTBM_ABXHPCDT-C067, "Construct Code 067
        C068(4), "TYPEZTBM_ABXHPCDT-C068, "Construct Code 068
        C069(4), "TYPEZTBM_ABXHPCDT-C069, "Construct Code 069
        C070(4), "TYPEZTBM_ABXHPCDT-C070, "Construct Code 070
        C071(4), "TYPEZTBM_ABXHPCDT-C071, "Construct Code 071
        C072(4), "TYPEZTBM_ABXHPCDT-C072, "Construct Code 072
        C073(4), "TYPEZTBM_ABXHPCDT-C073, "Construct Code 073
        C074(4), "TYPEZTBM_ABXHPCDT-C074, "Construct Code 074
        C075(4), "TYPEZTBM_ABXHPCDT-C075, "Construct Code 075
        C076(4), "TYPEZTBM_ABXHPCDT-C076, "Construct Code 076
        C077(4), "TYPEZTBM_ABXHPCDT-C077, "Construct Code 077
        C078(4), "TYPEZTBM_ABXHPCDT-C078, "Construct Code 078
        C079(4), "TYPEZTBM_ABXHPCDT-C079, "Construct Code 079
        C080(4), "TYPEZTBM_ABXHPCDT-C080, "Construct Code 080
        C081(4), "TYPEZTBM_ABXHPCDT-C081, "Construct Code 081
        C082(4), "TYPEZTBM_ABXHPCDT-C082, "Construct Code 082
        C083(4), "TYPEZTBM_ABXHPCDT-C083, "Construct Code 083
        C084(4), "TYPEZTBM_ABXHPCDT-C084, "Construct Code 084
        C085(4), "TYPEZTBM_ABXHPCDT-C085, "Construct Code 085
        C086(4), "TYPEZTBM_ABXHPCDT-C086, "Construct Code 086
        C087(4), "TYPEZTBM_ABXHPCDT-C087, "Construct Code 087
        C088(4), "TYPEZTBM_ABXHPCDT-C088, "Construct Code 088
        C089(4), "TYPEZTBM_ABXHPCDT-C089, "Construct Code 089
        C090(4), "TYPEZTBM_ABXHPCDT-C090, "Construct Code 090
        C091(4), "TYPEZTBM_ABXHPCDT-C091, "Construct Code 091
        C092(4), "TYPEZTBM_ABXHPCDT-C092, "Construct Code 092
        C093(4), "TYPEZTBM_ABXHPCDT-C093, "Construct Code 093
        C094(4), "TYPEZTBM_ABXHPCDT-C094, "Construct Code 094
        C095(4), "TYPEZTBM_ABXHPCDT-C095, "Construct Code 095
        C096(4), "TYPEZTBM_ABXHPCDT-C096, "Construct Code 096
        C097(4), "TYPEZTBM_ABXHPCDT-C097, "Construct Code 097
        C098(4), "TYPEZTBM_ABXHPCDT-C098, "Construct Code 098
        C099(4), "TYPEZTBM_ABXHPCDT-C099, "Construct Code 099
        C100(4), "TYPEZTBM_ABXHPCDT-C100, "Construct Code 100
        C101(4), "TYPEZTBM_ABXHPCDT-C101, "Construct Code 101
        C102(4), "TYPEZTBM_ABXHPCDT-C102, "Construct Code 102
        C103(4), "TYPEZTBM_ABXHPCDT-C103, "Construct Code 103
        C104(4), "TYPEZTBM_ABXHPCDT-C104, "Construct Code 104
        C105(4), "TYPEZTBM_ABXHPCDT-C105, "Construct Code 105
        C106(4), "TYPEZTBM_ABXHPCDT-C106, "Construct Code 106
        C107(4), "TYPEZTBM_ABXHPCDT-C107, "Construct Code 107
        C108(4), "TYPEZTBM_ABXHPCDT-C108, "Construct Code 108
        C109(4), "TYPEZTBM_ABXHPCDT-C109, "Construct Code 109
        C110(4), "TYPEZTBM_ABXHPCDT-C110, "Construct Code 110
        C111(4), "TYPEZTBM_ABXHPCDT-C111, "Construct Code 111
        C112(4), "TYPEZTBM_ABXHPCDT-C112, "Construct Code 112
        C113(4), "TYPEZTBM_ABXHPCDT-C113, "Construct Code 113
        C114(4), "TYPEZTBM_ABXHPCDT-C114, "Construct Code 114
        C115(4), "TYPEZTBM_ABXHPCDT-C115, "Construct Code 115
        C116(4), "TYPEZTBM_ABXHPCDT-C116, "Construct Code 116
        C117(4), "TYPEZTBM_ABXHPCDT-C117, "Construct Code 117
        C118(4), "TYPEZTBM_ABXHPCDT-C118, "Construct Code 118
        C119(4), "TYPEZTBM_ABXHPCDT-C119, "Construct Code 119
        C120(4), "TYPEZTBM_ABXHPCDT-C120, "Construct Code 120
        C121(4), "TYPEZTBM_ABXHPCDT-C121, "Construct Code 121
        C122(4), "TYPEZTBM_ABXHPCDT-C122, "Construct Code 122
        C123(4), "TYPEZTBM_ABXHPCDT-C123, "Construct Code 123
        C124(4), "TYPEZTBM_ABXHPCDT-C124, "Construct Code 124
        C125(4), "TYPEZTBM_ABXHPCDT-C125, "Construct Code 125
        C126(4), "TYPEZTBM_ABXHPCDT-C126, "Construct Code 126
        C127(4), "TYPEZTBM_ABXHPCDT-C127, "Construct Code 127
        C128(4), "TYPEZTBM_ABXHPCDT-C128, "Construct Code 128
        C129(4), "TYPEZTBM_ABXHPCDT-C129, "Construct Code 129
        C130(4), "TYPEZTBM_ABXHPCDT-C130, "Construct Code 130
        C131(4), "TYPEZTBM_ABXHPCDT-C131, "Construct Code 131
        C132(4), "TYPEZTBM_ABXHPCDT-C132, "Construct Code 132
        C133(4), "TYPEZTBM_ABXHPCDT-C133, "Construct Code 133
        C134(4), "TYPEZTBM_ABXHPCDT-C134, "Construct Code 134
        C135(4), "TYPEZTBM_ABXHPCDT-C135, "Construct Code 135
        C136(4), "TYPEZTBM_ABXHPCDT-C136, "Construct Code 136
        C137(4), "TYPEZTBM_ABXHPCDT-C137, "Construct Code 137
        C138(4), "TYPEZTBM_ABXHPCDT-C138, "Construct Code 138
        C139(4), "TYPEZTBM_ABXHPCDT-C139, "Construct Code 139
        C140(4), "TYPEZTBM_ABXHPCDT-C140, "Construct Code 140
        C141(4), "TYPEZTBM_ABXHPCDT-C141, "Construct Code 141
        C142(4), "TYPEZTBM_ABXHPCDT-C142, "Construct Code 142
        C143(4), "TYPEZTBM_ABXHPCDT-C143, "Construct Code 143
        C144(4), "TYPEZTBM_ABXHPCDT-C144, "Construct Code 144
        C145(4), "TYPEZTBM_ABXHPCDT-C145, "Construct Code 145
        C146(4), "TYPEZTBM_ABXHPCDT-C146, "Construct Code 146
        C147(4), "TYPEZTBM_ABXHPCDT-C147, "Construct Code 147
        C148(4), "TYPEZTBM_ABXHPCDT-C148, "Construct Code 148
        C149(4), "TYPEZTBM_ABXHPCDT-C149, "Construct Code 149
        C150(4), "TYPEZTBM_ABXHPCDT-C150, "Construct Code 150
        C151(4), "TYPEZTBM_ABXHPCDT-C151, "Construct Code 151
        C152(4), "TYPEZTBM_ABXHPCDT-C152, "Construct Code 152
        C153(4), "TYPEZTBM_ABXHPCDT-C153, "Construct Code 153
        C154(4), "TYPEZTBM_ABXHPCDT-C154, "Construct Code 154
        C155(4), "TYPEZTBM_ABXHPCDT-C155, "Construct Code 155
        C156(4), "TYPEZTBM_ABXHPCDT-C156, "Construct Code 156
        C157(4), "TYPEZTBM_ABXHPCDT-C157, "Construct Code 157
        C158(4), "TYPEZTBM_ABXHPCDT-C158, "Construct Code 158
        C159(4), "TYPEZTBM_ABXHPCDT-C159, "Construct Code 159
        C160(4), "TYPEZTBM_ABXHPCDT-C160, "Construct Code 160
        C161(4), "TYPEZTBM_ABXHPCDT-C161, "Construct Code 161
        C162(4), "TYPEZTBM_ABXHPCDT-C162, "Construct Code 162
        C163(4), "TYPEZTBM_ABXHPCDT-C163, "Construct Code 163
        C164(4), "TYPEZTBM_ABXHPCDT-C164, "Construct Code 164
        C165(4), "TYPEZTBM_ABXHPCDT-C165, "Construct Code 165
        C166(4), "TYPEZTBM_ABXHPCDT-C166, "Construct Code 166
        C167(4), "TYPEZTBM_ABXHPCDT-C167, "Construct Code 167
        C168(4), "TYPEZTBM_ABXHPCDT-C168, "Construct Code 168
        C169(4), "TYPEZTBM_ABXHPCDT-C169, "Construct Code 169
        C170(4), "TYPEZTBM_ABXHPCDT-C170, "Construct Code 170
        C171(4), "TYPEZTBM_ABXHPCDT-C171, "Construct Code 171
        C172(4), "TYPEZTBM_ABXHPCDT-C172, "Construct Code 172
        C173(4), "TYPEZTBM_ABXHPCDT-C173, "Construct Code 173
        C174(4), "TYPEZTBM_ABXHPCDT-C174, "Construct Code 174
        C175(4), "TYPEZTBM_ABXHPCDT-C175, "Construct Code 175
        C176(4), "TYPEZTBM_ABXHPCDT-C176, "Construct Code 176
        C177(4), "TYPEZTBM_ABXHPCDT-C177, "Construct Code 177
        C178(4), "TYPEZTBM_ABXHPCDT-C178, "Construct Code 178
        C179(4), "TYPEZTBM_ABXHPCDT-C179, "Construct Code 179
        C180(4), "TYPEZTBM_ABXHPCDT-C180, "Construct Code 180
        C181(4), "TYPEZTBM_ABXHPCDT-C181, "Construct Code 181
        C182(4), "TYPEZTBM_ABXHPCDT-C182, "Construct Code 182
        C183(4), "TYPEZTBM_ABXHPCDT-C183, "Construct Code 183
        C184(4), "TYPEZTBM_ABXHPCDT-C184, "Construct Code 184
        C185(4), "TYPEZTBM_ABXHPCDT-C185, "Construct Code 185
        C186(4), "TYPEZTBM_ABXHPCDT-C186, "Construct Code 186
        C187(4), "TYPEZTBM_ABXHPCDT-C187, "Construct Code 187
        C188(4), "TYPEZTBM_ABXHPCDT-C188, "Construct Code 188
        C189(4), "TYPEZTBM_ABXHPCDT-C189, "Construct Code 189
        C190(4), "TYPEZTBM_ABXHPCDT-C190, "Construct Code 190
        C191(4), "TYPEZTBM_ABXHPCDT-C191, "Construct Code 191
        C192(4), "TYPEZTBM_ABXHPCDT-C192, "Construct Code 192
        C193(4), "TYPEZTBM_ABXHPCDT-C193, "Construct Code 193
        C194(4), "TYPEZTBM_ABXHPCDT-C194, "Construct Code 194
        C195(4), "TYPEZTBM_ABXHPCDT-C195, "Construct Code 195
        C196(4), "TYPEZTBM_ABXHPCDT-C196, "Construct Code 196
        C197(4), "TYPEZTBM_ABXHPCDT-C197, "Construct Code 197
        C198(4), "TYPEZTBM_ABXHPCDT-C198, "Construct Code 198
        C199(4), "TYPEZTBM_ABXHPCDT-C199, "Construct Code 199
        C200(4), "TYPEZTBM_ABXHPCDT-C200, "Construct Code 200
        C201(4), "TYPEZTBM_ABXHPCDT-C201, "Construct Code 201
        C202(4), "TYPEZTBM_ABXHPCDT-C202, "Construct Code 202
        C203(4), "TYPEZTBM_ABXHPCDT-C203, "Construct Code 203
        C204(4), "TYPEZTBM_ABXHPCDT-C204, "Construct Code 204
        C205(4), "TYPEZTBM_ABXHPCDT-C205, "Construct Code 205
        C206(4), "TYPEZTBM_ABXHPCDT-C206, "Construct Code 206
        C207(4), "TYPEZTBM_ABXHPCDT-C207, "Construct Code 207
        C208(4), "TYPEZTBM_ABXHPCDT-C208, "Construct Code 208
        C209(4), "TYPEZTBM_ABXHPCDT-C209, "Construct Code 209
        C210(4), "TYPEZTBM_ABXHPCDT-C210, "Construct Code 210
        C211(4), "TYPEZTBM_ABXHPCDT-C211, "Construct Code 211
        C212(4), "TYPEZTBM_ABXHPCDT-C212, "Construct Code 212
        C213(4), "TYPEZTBM_ABXHPCDT-C213, "Construct Code 213
        C214(4), "TYPEZTBM_ABXHPCDT-C214, "Construct Code 214
        C215(4), "TYPEZTBM_ABXHPCDT-C215, "Construct Code 215
        C216(4), "TYPEZTBM_ABXHPCDT-C216, "Construct Code 216
        C217(4), "TYPEZTBM_ABXHPCDT-C217, "Construct Code 217
        C218(4), "TYPEZTBM_ABXHPCDT-C218, "Construct Code 218
        C219(4), "TYPEZTBM_ABXHPCDT-C219, "Construct Code 219
        C220(4), "TYPEZTBM_ABXHPCDT-C220, "Construct Code 220
        C221(4), "TYPEZTBM_ABXHPCDT-C221, "Construct Code 221
        C222(4), "TYPEZTBM_ABXHPCDT-C222, "Construct Code 222
        C223(4), "TYPEZTBM_ABXHPCDT-C223, "Construct Code 223
        C224(4), "TYPEZTBM_ABXHPCDT-C224, "Construct Code 224
        C225(4), "TYPEZTBM_ABXHPCDT-C225, "Construct Code 225
        C226(4), "TYPEZTBM_ABXHPCDT-C226, "Construct Code 226
        C227(4), "TYPEZTBM_ABXHPCDT-C227, "Construct Code 227
        C228(4), "TYPEZTBM_ABXHPCDT-C228, "Construct Code 228
        C229(4), "TYPEZTBM_ABXHPCDT-C229, "Construct Code 229
        C230(4), "TYPEZTBM_ABXHPCDT-C230, "Construct Code 230
        C231(4), "TYPEZTBM_ABXHPCDT-C231, "Construct Code 231
        C232(4), "TYPEZTBM_ABXHPCDT-C232, "Construct Code 232
        C233(4), "TYPEZTBM_ABXHPCDT-C233, "Construct Code 233
        C234(4), "TYPEZTBM_ABXHPCDT-C234, "Construct Code 234
        C235(4), "TYPEZTBM_ABXHPCDT-C235, "Construct Code 235
        C236(4), "TYPEZTBM_ABXHPCDT-C236, "Construct Code 236
        C237(4), "TYPEZTBM_ABXHPCDT-C237, "Construct Code 237
        C238(4), "TYPEZTBM_ABXHPCDT-C238, "Construct Code 238
        C239(4), "TYPEZTBM_ABXHPCDT-C239, "Construct Code 239
        C240(4), "TYPEZTBM_ABXHPCDT-C240, "Construct Code 240
        C241(4), "TYPEZTBM_ABXHPCDT-C241, "Construct Code 241
        C242(4), "TYPEZTBM_ABXHPCDT-C242, "Construct Code 242
        C243(4), "TYPEZTBM_ABXHPCDT-C243, "Construct Code 243
        C244(4), "TYPEZTBM_ABXHPCDT-C244, "Construct Code 244
        C245(4), "TYPEZTBM_ABXHPCDT-C245, "Construct Code 245
        C246(4), "TYPEZTBM_ABXHPCDT-C246, "Construct Code 246
        C247(4), "TYPEZTBM_ABXHPCDT-C247, "Construct Code 247
        C248(4), "TYPEZTBM_ABXHPCDT-C248, "Construct Code 248
        C249(4), "TYPEZTBM_ABXHPCDT-C249, "Construct Code 249
        C250(4), "TYPEZTBM_ABXHPCDT-C250, "Construct Code 250
        C251(4), "TYPEZTBM_ABXHPCDT-C251, "Construct Code 251
        C252(4), "TYPEZTBM_ABXHPCDT-C252, "Construct Code 252
        C253(4), "TYPEZTBM_ABXHPCDT-C253, "Construct Code 253
        C254(4), "TYPEZTBM_ABXHPCDT-C254, "Construct Code 254
        C255(4), "TYPEZTBM_ABXHPCDT-C255, "Construct Code 255
        C256(4), "TYPEZTBM_ABXHPCDT-C256, "Construct Code 256
        C257(4), "TYPEZTBM_ABXHPCDT-C257, "Construct Code 257
        C258(4), "TYPEZTBM_ABXHPCDT-C258, "Construct Code 258
        C259(4), "TYPEZTBM_ABXHPCDT-C259, "Construct Code 259
        C260(4), "TYPEZTBM_ABXHPCDT-C260, "Construct Code 260
        C261(4), "TYPEZTBM_ABXHPCDT-C261, "Construct Code 261
        C262(4), "TYPEZTBM_ABXHPCDT-C262, "Construct Code 262
        C263(4), "TYPEZTBM_ABXHPCDT-C263, "Construct Code 263
        C264(4), "TYPEZTBM_ABXHPCDT-C264, "Construct Code 264
        C265(4), "TYPEZTBM_ABXHPCDT-C265, "Construct Code 265
        C266(4), "TYPEZTBM_ABXHPCDT-C266, "Construct Code 266
        C267(4), "TYPEZTBM_ABXHPCDT-C267, "Construct Code 267
        C268(4), "TYPEZTBM_ABXHPCDT-C268, "Construct Code 268
        C269(4), "TYPEZTBM_ABXHPCDT-C269, "Construct Code 269
        C270(4), "TYPEZTBM_ABXHPCDT-C270, "Construct Code 270
        C271(4), "TYPEZTBM_ABXHPCDT-C271, "Construct Code 271
        C272(4), "TYPEZTBM_ABXHPCDT-C272, "Construct Code 272
        C273(4), "TYPEZTBM_ABXHPCDT-C273, "Construct Code 273
        C274(4), "TYPEZTBM_ABXHPCDT-C274, "Construct Code 274
        C275(4), "TYPEZTBM_ABXHPCDT-C275, "Construct Code 275
        C276(4), "TYPEZTBM_ABXHPCDT-C276, "Construct Code 276
        C277(4), "TYPEZTBM_ABXHPCDT-C277, "Construct Code 277
        C278(4), "TYPEZTBM_ABXHPCDT-C278, "Construct Code 278
        C279(4), "TYPEZTBM_ABXHPCDT-C279, "Construct Code 279
        C280(4), "TYPEZTBM_ABXHPCDT-C280, "Construct Code 280
        C281(4), "TYPEZTBM_ABXHPCDT-C281, "Construct Code 281
        C282(4), "TYPEZTBM_ABXHPCDT-C282, "Construct Code 282
        C283(4), "TYPEZTBM_ABXHPCDT-C283, "Construct Code 283
        C284(4), "TYPEZTBM_ABXHPCDT-C284, "Construct Code 284
        C285(4), "TYPEZTBM_ABXHPCDT-C285, "Construct Code 285
        C286(4), "TYPEZTBM_ABXHPCDT-C286, "Construct Code 286
        C287(4), "TYPEZTBM_ABXHPCDT-C287, "Construct Code 287
        C288(4), "TYPEZTBM_ABXHPCDT-C288, "Construct Code 288
        C289(4), "TYPEZTBM_ABXHPCDT-C289, "Construct Code 289
        C290(4), "TYPEZTBM_ABXHPCDT-C290, "Construct Code 290
        C291(4), "TYPEZTBM_ABXHPCDT-C291, "Construct Code 291
        C292(4), "TYPEZTBM_ABXHPCDT-C292, "Construct Code 292
        C293(4), "TYPEZTBM_ABXHPCDT-C293, "Construct Code 293
        C294(4), "TYPEZTBM_ABXHPCDT-C294, "Construct Code 294
        C295(4), "TYPEZTBM_ABXHPCDT-C295, "Construct Code 295
        C296(4), "TYPEZTBM_ABXHPCDT-C296, "Construct Code 296
        C297(4), "TYPEZTBM_ABXHPCDT-C297, "Construct Code 297
        C298(4), "TYPEZTBM_ABXHPCDT-C298, "Construct Code 298
        C299(4), "TYPEZTBM_ABXHPCDT-C299, "Construct Code 299
        C300(4), "TYPEZTBM_ABXHPCDT-C300, "Construct Code 300
        C301(4), "TYPEZTBM_ABXHPCDT-C301, "Construct Code 301
        C302(4), "TYPEZTBM_ABXHPCDT-C302, "Construct Code 302
        C303(4), "TYPEZTBM_ABXHPCDT-C303, "Construct Code 303
        C304(4), "TYPEZTBM_ABXHPCDT-C304, "Construct Code 304
        C305(4), "TYPEZTBM_ABXHPCDT-C305, "Construct Code 305
        C306(4), "TYPEZTBM_ABXHPCDT-C306, "Construct Code 306
        C307(4), "TYPEZTBM_ABXHPCDT-C307, "Construct Code 307
        C308(4), "TYPEZTBM_ABXHPCDT-C308, "Construct Code 308
        C309(4), "TYPEZTBM_ABXHPCDT-C309, "Construct Code 309
        C310(4), "TYPEZTBM_ABXHPCDT-C310, "Construct Code 310
        C311(4), "TYPEZTBM_ABXHPCDT-C311, "Construct Code 311
        C312(4), "TYPEZTBM_ABXHPCDT-C312, "Construct Code 312
        C313(4), "TYPEZTBM_ABXHPCDT-C313, "Construct Code 313
        C314(4), "TYPEZTBM_ABXHPCDT-C314, "Construct Code 314
        C315(4), "TYPEZTBM_ABXHPCDT-C315, "Construct Code 315
        C316(4), "TYPEZTBM_ABXHPCDT-C316, "Construct Code 316
        C317(4), "TYPEZTBM_ABXHPCDT-C317, "Construct Code 317
        C318(4), "TYPEZTBM_ABXHPCDT-C318, "Construct Code 318
        C319(4), "TYPEZTBM_ABXHPCDT-C319, "Construct Code 319
        C320(4), "TYPEZTBM_ABXHPCDT-C320, "Construct Code 320
        C321(4), "TYPEZTBM_ABXHPCDT-C321, "Construct Code 321
        C322(4), "TYPEZTBM_ABXHPCDT-C322, "Construct Code 322
        C323(4), "TYPEZTBM_ABXHPCDT-C323, "Construct Code 323
        C324(4), "TYPEZTBM_ABXHPCDT-C324, "Construct Code 324
        C325(4), "TYPEZTBM_ABXHPCDT-C325, "Construct Code 325
        C326(4), "TYPEZTBM_ABXHPCDT-C326, "Construct Code 326
        C327(4), "TYPEZTBM_ABXHPCDT-C327, "Construct Code 327
        C328(4), "TYPEZTBM_ABXHPCDT-C328, "Construct Code 328
        C329(4), "TYPEZTBM_ABXHPCDT-C329, "Construct Code 329
        C330(4), "TYPEZTBM_ABXHPCDT-C330, "Construct Code 330
        C331(4), "TYPEZTBM_ABXHPCDT-C331, "Construct Code 331
        C332(4), "TYPEZTBM_ABXHPCDT-C332, "Construct Code 332
        C333(4), "TYPEZTBM_ABXHPCDT-C333, "Construct Code 333
        C334(4), "TYPEZTBM_ABXHPCDT-C334, "Construct Code 334
        C335(4), "TYPEZTBM_ABXHPCDT-C335, "Construct Code 335
        C336(4), "TYPEZTBM_ABXHPCDT-C336, "Construct Code 336
        C337(4), "TYPEZTBM_ABXHPCDT-C337, "Construct Code 337
        C338(4), "TYPEZTBM_ABXHPCDT-C338, "Construct Code 338
        C339(4), "TYPEZTBM_ABXHPCDT-C339, "Construct Code 339
        C340(4), "TYPEZTBM_ABXHPCDT-C340, "Construct Code 340
        C341(4), "TYPEZTBM_ABXHPCDT-C341, "Construct Code 341
        C342(4), "TYPEZTBM_ABXHPCDT-C342, "Construct Code 342
        C343(4), "TYPEZTBM_ABXHPCDT-C343, "Construct Code 343
        C344(4), "TYPEZTBM_ABXHPCDT-C344, "Construct Code 344
        C345(4), "TYPEZTBM_ABXHPCDT-C345, "Construct Code 345
        C346(4), "TYPEZTBM_ABXHPCDT-C346, "Construct Code 346
        C347(4), "TYPEZTBM_ABXHPCDT-C347, "Construct Code 347
        C348(4), "TYPEZTBM_ABXHPCDT-C348, "Construct Code 348
        C349(4), "TYPEZTBM_ABXHPCDT-C349, "Construct Code 349
        C350(4), "TYPEZTBM_ABXHPCDT-C350, "Construct Code 350
        C351(4), "TYPEZTBM_ABXHPCDT-C351, "Construct Code 351
        C352(4), "TYPEZTBM_ABXHPCDT-C352, "Construct Code 352
        C353(4), "TYPEZTBM_ABXHPCDT-C353, "Construct Code 353
        C354(4), "TYPEZTBM_ABXHPCDT-C354, "Construct Code 354
        C355(4), "TYPEZTBM_ABXHPCDT-C355, "Construct Code 355
        C356(4), "TYPEZTBM_ABXHPCDT-C356, "Construct Code 356
        C357(4), "TYPEZTBM_ABXHPCDT-C357, "Construct Code 357
        C358(4), "TYPEZTBM_ABXHPCDT-C358, "Construct Code 358
        C359(4), "TYPEZTBM_ABXHPCDT-C359, "Construct Code 359
        C360(4), "TYPEZTBM_ABXHPCDT-C360, "Construct Code 360
        C361(4), "TYPEZTBM_ABXHPCDT-C361, "Construct Code 361
        C362(4), "TYPEZTBM_ABXHPCDT-C362, "Construct Code 362
        C363(4), "TYPEZTBM_ABXHPCDT-C363, "Construct Code 363
        C364(4), "TYPEZTBM_ABXHPCDT-C364, "Construct Code 364
        C365(4), "TYPEZTBM_ABXHPCDT-C365, "Construct Code 365
        C366(4), "TYPEZTBM_ABXHPCDT-C366, "Construct Code 366
        C367(4), "TYPEZTBM_ABXHPCDT-C367, "Construct Code 367
        C368(4), "TYPEZTBM_ABXHPCDT-C368, "Construct Code 368
        C369(4), "TYPEZTBM_ABXHPCDT-C369, "Construct Code 369
        C370(4), "TYPEZTBM_ABXHPCDT-C370, "Construct Code 370
        C371(4), "TYPEZTBM_ABXHPCDT-C371, "Construct Code 371
        C372(4), "TYPEZTBM_ABXHPCDT-C372, "Construct Code 372
        C373(4), "TYPEZTBM_ABXHPCDT-C373, "Construct Code 373
        C374(4), "TYPEZTBM_ABXHPCDT-C374, "Construct Code 374
        C375(4), "TYPEZTBM_ABXHPCDT-C375, "Construct Code 375
        C376(4), "TYPEZTBM_ABXHPCDT-C376, "Construct Code 376
        C377(4), "TYPEZTBM_ABXHPCDT-C377, "Construct Code 377
        C378(4), "TYPEZTBM_ABXHPCDT-C378, "Construct Code 378
        C379(4), "TYPEZTBM_ABXHPCDT-C379, "Construct Code 379
        C380(4), "TYPEZTBM_ABXHPCDT-C380, "Construct Code 380
        C381(4), "TYPEZTBM_ABXHPCDT-C381, "Construct Code 381
        C382(4), "TYPEZTBM_ABXHPCDT-C382, "Construct Code 382
        C383(4), "TYPEZTBM_ABXHPCDT-C383, "Construct Code 383
        C384(4), "TYPEZTBM_ABXHPCDT-C384, "Construct Code 384
        C385(4), "TYPEZTBM_ABXHPCDT-C385, "Construct Code 385
        C386(4), "TYPEZTBM_ABXHPCDT-C386, "Construct Code 386
        C387(4), "TYPEZTBM_ABXHPCDT-C387, "Construct Code 387
        C388(4), "TYPEZTBM_ABXHPCDT-C388, "Construct Code 388
        C389(4), "TYPEZTBM_ABXHPCDT-C389, "Construct Code 389
        C390(4), "TYPEZTBM_ABXHPCDT-C390, "Construct Code 390
        C391(4), "TYPEZTBM_ABXHPCDT-C391, "Construct Code 391
        C392(4), "TYPEZTBM_ABXHPCDT-C392, "Construct Code 392
        C393(4), "TYPEZTBM_ABXHPCDT-C393, "Construct Code 393
        C394(4), "TYPEZTBM_ABXHPCDT-C394, "Construct Code 394
        C395(4), "TYPEZTBM_ABXHPCDT-C395, "Construct Code 395
        C396(4), "TYPEZTBM_ABXHPCDT-C396, "Construct Code 396
        C397(4), "TYPEZTBM_ABXHPCDT-C397, "Construct Code 397
        C398(4), "TYPEZTBM_ABXHPCDT-C398, "Construct Code 398
        C399(4), "TYPEZTBM_ABXHPCDT-C399, "Construct Code 399
        C400(4), "TYPEZTBM_ABXHPCDT-C400, "Construct Code 400
*        C401(4), "TYPEZTBM_ABXHPCDT-C401, "Construct Code 401
*        C402(4), "TYPEZTBM_ABXHPCDT-C402, "Construct Code 402
*        C403(4), "TYPEZTBM_ABXHPCDT-C403, "Construct Code 403
*        C404(4), "TYPEZTBM_ABXHPCDT-C404, "Construct Code 404
*        C405(4), "TYPEZTBM_ABXHPCDT-C405, "Construct Code 405
*        C406(4), "TYPEZTBM_ABXHPCDT-C406, "Construct Code 406
*        C407(4), "TYPEZTBM_ABXHPCDT-C407, "Construct Code 407
*        C408(4), "TYPEZTBM_ABXHPCDT-C408, "Construct Code 408
*        C409(4), "TYPEZTBM_ABXHPCDT-C409, "Construct Code 409
*        C410(4), "TYPEZTBM_ABXHPCDT-C410, "Construct Code 410
*        C411(4), "TYPEZTBM_ABXHPCDT-C411, "Construct Code 411
*        C412(4), "TYPEZTBM_ABXHPCDT-C412, "Construct Code 412
*        C413(4), "TYPEZTBM_ABXHPCDT-C413, "Construct Code 413
*        C414(4), "TYPEZTBM_ABXHPCDT-C414, "Construct Code 414
*        C415(4), "TYPEZTBM_ABXHPCDT-C415, "Construct Code 415
*        C416(4), "TYPEZTBM_ABXHPCDT-C416, "Construct Code 416
*        C417(4), "TYPEZTBM_ABXHPCDT-C417, "Construct Code 417
*        C418(4), "TYPEZTBM_ABXHPCDT-C418, "Construct Code 418
*        C419(4), "TYPEZTBM_ABXHPCDT-C419, "Construct Code 419
*        C420(4), "TYPEZTBM_ABXHPCDT-C420, "Construct Code 420
*        C421(4), "TYPEZTBM_ABXHPCDT-C421, "Construct Code 421
*        C422(4), "TYPEZTBM_ABXHPCDT-C422, "Construct Code 422
*        C423(4), "TYPEZTBM_ABXHPCDT-C423, "Construct Code 423
*        C424(4), "TYPEZTBM_ABXHPCDT-C424, "Construct Code 424
*        C425(4), "TYPEZTBM_ABXHPCDT-C425, "Construct Code 425
*        C426(4), "TYPEZTBM_ABXHPCDT-C426, "Construct Code 426
*        C427(4), "TYPEZTBM_ABXHPCDT-C427, "Construct Code 427
*        C428(4), "TYPEZTBM_ABXHPCDT-C428, "Construct Code 428
*        C429(4), "TYPEZTBM_ABXHPCDT-C429, "Construct Code 429
*        C430(4), "TYPEZTBM_ABXHPCDT-C430, "Construct Code 430
*        C431(4), "TYPEZTBM_ABXHPCDT-C431, "Construct Code 431
*        C432(4), "TYPEZTBM_ABXHPCDT-C432, "Construct Code 432
*        C433(4), "TYPEZTBM_ABXHPCDT-C433, "Construct Code 433
*        C434(4), "TYPEZTBM_ABXHPCDT-C434, "Construct Code 434
*        C435(4), "TYPEZTBM_ABXHPCDT-C435, "Construct Code 435
*        C436(4), "TYPEZTBM_ABXHPCDT-C436, "Construct Code 436
*        C437(4), "TYPEZTBM_ABXHPCDT-C437, "Construct Code 437
*        C438(4), "TYPEZTBM_ABXHPCDT-C438, "Construct Code 438
*        C439(4), "TYPEZTBM_ABXHPCDT-C439, "Construct Code 439
*        C440(4), "TYPEZTBM_ABXHPCDT-C440, "Construct Code 440
*        C441(4), "TYPEZTBM_ABXHPCDT-C441, "Construct Code 441
*        C442(4), "TYPEZTBM_ABXHPCDT-C442, "Construct Code 442
*        C443(4), "TYPEZTBM_ABXHPCDT-C443, "Construct Code 443
*        C444(4), "TYPEZTBM_ABXHPCDT-C444, "Construct Code 444
*        C445(4), "TYPEZTBM_ABXHPCDT-C445, "Construct Code 445
*        C446(4), "TYPEZTBM_ABXHPCDT-C446, "Construct Code 446
*        C447(4), "TYPEZTBM_ABXHPCDT-C447, "Construct Code 447
*        C448(4), "TYPEZTBM_ABXHPCDT-C448, "Construct Code 448
*        C449(4), "TYPEZTBM_ABXHPCDT-C449, "Construct Code 449
*        C450(4), "TYPEZTBM_ABXHPCDT-C450, "Construct Code 450
*        C451(4), "TYPEZTBM_ABXHPCDT-C451, "Construct Code 451
*        C452(4), "TYPEZTBM_ABXHPCDT-C452, "Construct Code 452
*        C453(4), "TYPEZTBM_ABXHPCDT-C453, "Construct Code 453
*        C454(4), "TYPEZTBM_ABXHPCDT-C454, "Construct Code 454
*        C455(4), "TYPEZTBM_ABXHPCDT-C455, "Construct Code 455
*        C456(4), "TYPEZTBM_ABXHPCDT-C456, "Construct Code 456
*        C457(4), "TYPEZTBM_ABXHPCDT-C457, "Construct Code 457
*        C458(4), "TYPEZTBM_ABXHPCDT-C458, "Construct Code 458
*        C459(4), "TYPEZTBM_ABXHPCDT-C459, "Construct Code 459
*        C460(4), "TYPEZTBM_ABXHPCDT-C460, "Construct Code 460
*        C461(4), "TYPEZTBM_ABXHPCDT-C461, "Construct Code 461
*        C462(4), "TYPEZTBM_ABXHPCDT-C462, "Construct Code 462
*        C463(4), "TYPEZTBM_ABXHPCDT-C463, "Construct Code 463
*        C464(4), "TYPEZTBM_ABXHPCDT-C464, "Construct Code 464
*        C465(4), "TYPEZTBM_ABXHPCDT-C465, "Construct Code 465
*        C466(4), "TYPEZTBM_ABXHPCDT-C466, "Construct Code 466
*        C467(4), "TYPEZTBM_ABXHPCDT-C467, "Construct Code 467
*        C468(4), "TYPEZTBM_ABXHPCDT-C468, "Construct Code 468
*        C469(4), "TYPEZTBM_ABXHPCDT-C469, "Construct Code 469
*        C470(4), "TYPEZTBM_ABXHPCDT-C470, "Construct Code 470
*        C471(4), "TYPEZTBM_ABXHPCDT-C471, "Construct Code 471
*        C472(4), "TYPEZTBM_ABXHPCDT-C472, "Construct Code 472
*        C473(4), "TYPEZTBM_ABXHPCDT-C473, "Construct Code 473
*        C474(4), "TYPEZTBM_ABXHPCDT-C474, "Construct Code 474
*        C475(4), "TYPEZTBM_ABXHPCDT-C475, "Construct Code 475
*        C476(4), "TYPEZTBM_ABXHPCDT-C476, "Construct Code 476
*        C477(4), "TYPEZTBM_ABXHPCDT-C477, "Construct Code 477
*        C478(4), "TYPEZTBM_ABXHPCDT-C478, "Construct Code 478
*        C479(4), "TYPEZTBM_ABXHPCDT-C479, "Construct Code 479
*        C480(4), "TYPEZTBM_ABXHPCDT-C480, "Construct Code 480
*        C481(4), "TYPEZTBM_ABXHPCDT-C481, "Construct Code 481
*        C482(4), "TYPEZTBM_ABXHPCDT-C482, "Construct Code 482
*        C483(4), "TYPEZTBM_ABXHPCDT-C483, "Construct Code 483
*        C484(4), "TYPEZTBM_ABXHPCDT-C484, "Construct Code 484
*        C485(4), "TYPEZTBM_ABXHPCDT-C485, "Construct Code 485
*        C486(4), "TYPEZTBM_ABXHPCDT-C486, "Construct Code 486
*        C487(4), "TYPEZTBM_ABXHPCDT-C487, "Construct Code 487
*        C488(4), "TYPEZTBM_ABXHPCDT-C488, "Construct Code 488
*        C489(4), "TYPEZTBM_ABXHPCDT-C489, "Construct Code 489
*        C490(4), "TYPEZTBM_ABXHPCDT-C490, "Construct Code 490
*        C491(4), "TYPEZTBM_ABXHPCDT-C491, "Construct Code 491
*        C492(4), "TYPEZTBM_ABXHPCDT-C492, "Construct Code 492
*        C493(4), "TYPEZTBM_ABXHPCDT-C493, "Construct Code 493
*        C494(4), "TYPEZTBM_ABXHPCDT-C494, "Construct Code 494
*        C495(4), "TYPEZTBM_ABXHPCDT-C495, "Construct Code 495
*        C496(4), "TYPEZTBM_ABXHPCDT-C496, "Construct Code 496
*        C497(4), "TYPEZTBM_ABXHPCDT-C497, "Construct Code 497
*        C498(4), "TYPEZTBM_ABXHPCDT-C498, "Construct Code 498
*        C499(4), "TYPEZTBM_ABXHPCDT-C499, "Construct Code 499
*        C500(4), "TYPEZTBM_ABXHPCDT-C500, "Construct Code 500
*        C501(4), "TYPEZTBM_ABXHPCDT-C501, "Construct Code 501
*        C502(4), "TYPEZTBM_ABXHPCDT-C502, "Construct Code 502
*        C503(4), "TYPEZTBM_ABXHPCDT-C503, "Construct Code 503
*        C504(4), "TYPEZTBM_ABXHPCDT-C504, "Construct Code 504
*        C505(4), "TYPEZTBM_ABXHPCDT-C505, "Construct Code 505
*        C506(4), "TYPEZTBM_ABXHPCDT-C506, "Construct Code 506
*        C507(4), "TYPEZTBM_ABXHPCDT-C507, "Construct Code 507
*        C508(4), "TYPEZTBM_ABXHPCDT-C508, "Construct Code 508
*        C509(4), "TYPEZTBM_ABXHPCDT-C509, "Construct Code 509
*        C510(4), "TYPEZTBM_ABXHPCDT-C510, "Construct Code 510
*        C511(4), "TYPEZTBM_ABXHPCDT-C511, "Construct Code 511
*        C512(4), "TYPEZTBM_ABXHPCDT-C512, "Construct Code 512
*        C513(4), "TYPEZTBM_ABXHPCDT-C513, "Construct Code 513
*        C514(4), "TYPEZTBM_ABXHPCDT-C514, "Construct Code 514
*        C515(4), "TYPEZTBM_ABXHPCDT-C515, "Construct Code 515
*        C516(4), "TYPEZTBM_ABXHPCDT-C516, "Construct Code 516
*        C517(4), "TYPEZTBM_ABXHPCDT-C517, "Construct Code 517
*        C518(4), "TYPEZTBM_ABXHPCDT-C518, "Construct Code 518
*        C519(4), "TYPEZTBM_ABXHPCDT-C519, "Construct Code 519
*        C520(4), "TYPEZTBM_ABXHPCDT-C520, "Construct Code 520
*        C521(4), "TYPEZTBM_ABXHPCDT-C521, "Construct Code 521
*        C522(4), "TYPEZTBM_ABXHPCDT-C522, "Construct Code 522
*        C523(4), "TYPEZTBM_ABXHPCDT-C523, "Construct Code 523
*        C524(4), "TYPEZTBM_ABXHPCDT-C524, "Construct Code 524
*        C525(4), "TYPEZTBM_ABXHPCDT-C525, "Construct Code 525
*        C526(4), "TYPEZTBM_ABXHPCDT-C526, "Construct Code 526
*        C527(4), "TYPEZTBM_ABXHPCDT-C527, "Construct Code 527
*        C528(4), "TYPEZTBM_ABXHPCDT-C528, "Construct Code 528
*        C529(4), "TYPEZTBM_ABXHPCDT-C529, "Construct Code 529
*        C530(4), "TYPEZTBM_ABXHPCDT-C530, "Construct Code 530
*        C531(4), "TYPEZTBM_ABXHPCDT-C531, "Construct Code 531
*        C532(4), "TYPEZTBM_ABXHPCDT-C532, "Construct Code 532
*        C533(4), "TYPEZTBM_ABXHPCDT-C533, "Construct Code 533
*        C534(4), "TYPEZTBM_ABXHPCDT-C534, "Construct Code 534
*        C535(4), "TYPEZTBM_ABXHPCDT-C535, "Construct Code 535
*        C536(4), "TYPEZTBM_ABXHPCDT-C536, "Construct Code 536
*        C537(4), "TYPEZTBM_ABXHPCDT-C537, "Construct Code 537
*        C538(4), "TYPEZTBM_ABXHPCDT-C538, "Construct Code 538
*        C539(4), "TYPEZTBM_ABXHPCDT-C539, "Construct Code 539
*        C540(4), "TYPEZTBM_ABXHPCDT-C540, "Construct Code 540
*        C541(4), "TYPEZTBM_ABXHPCDT-C541, "Construct Code 541
*        C542(4), "TYPEZTBM_ABXHPCDT-C542, "Construct Code 542
*        C543(4), "TYPEZTBM_ABXHPCDT-C543, "Construct Code 543
*        C544(4), "TYPEZTBM_ABXHPCDT-C544, "Construct Code 544
*        C545(4), "TYPEZTBM_ABXHPCDT-C545, "Construct Code 545
*        C546(4), "TYPEZTBM_ABXHPCDT-C546, "Construct Code 546
*        C547(4), "TYPEZTBM_ABXHPCDT-C547, "Construct Code 547
*        C548(4), "TYPEZTBM_ABXHPCDT-C548, "Construct Code 548
*        C549(4), "TYPEZTBM_ABXHPCDT-C549, "Construct Code 549
*        C550(4), "TYPEZTBM_ABXHPCDT-C550, "Construct Code 550
*        C551(4), "TYPEZTBM_ABXHPCDT-C551, "Construct Code 551
*        C552(4), "TYPEZTBM_ABXHPCDT-C552, "Construct Code 552
*        C553(4), "TYPEZTBM_ABXHPCDT-C553, "Construct Code 553
*        C554(4), "TYPEZTBM_ABXHPCDT-C554, "Construct Code 554
*        C555(4), "TYPEZTBM_ABXHPCDT-C555, "Construct Code 555
*        C556(4), "TYPEZTBM_ABXHPCDT-C556, "Construct Code 556
*        C557(4), "TYPEZTBM_ABXHPCDT-C557, "Construct Code 557
*        C558(4), "TYPEZTBM_ABXHPCDT-C558, "Construct Code 558
*        C559(4), "TYPEZTBM_ABXHPCDT-C559, "Construct Code 559
*        C560(4), "TYPEZTBM_ABXHPCDT-C560, "Construct Code 560
*        C561(4), "TYPEZTBM_ABXHPCDT-C561, "Construct Code 561
*        C562(4), "TYPEZTBM_ABXHPCDT-C562, "Construct Code 562
*        C563(4), "TYPEZTBM_ABXHPCDT-C563, "Construct Code 563
*        C564(4), "TYPEZTBM_ABXHPCDT-C564, "Construct Code 564
*        C565(4), "TYPEZTBM_ABXHPCDT-C565, "Construct Code 565
*        C566(4), "TYPEZTBM_ABXHPCDT-C566, "Construct Code 566
*        C567(4), "TYPEZTBM_ABXHPCDT-C567, "Construct Code 567
*        C568(4), "TYPEZTBM_ABXHPCDT-C568, "Construct Code 568
*        C569(4), "TYPEZTBM_ABXHPCDT-C569, "Construct Code 569
*        C570(4), "TYPEZTBM_ABXHPCDT-C570, "Construct Code 570
*        C571(4), "TYPEZTBM_ABXHPCDT-C571, "Construct Code 571
*        C572(4), "TYPEZTBM_ABXHPCDT-C572, "Construct Code 572
*        C573(4), "TYPEZTBM_ABXHPCDT-C573, "Construct Code 573
*        C574(4), "TYPEZTBM_ABXHPCDT-C574, "Construct Code 574
*        C575(4), "TYPEZTBM_ABXHPCDT-C575, "Construct Code 575
*        C576(4), "TYPEZTBM_ABXHPCDT-C576, "Construct Code 576
*        C577(4), "TYPEZTBM_ABXHPCDT-C577, "Construct Code 577
*        C578(4), "TYPEZTBM_ABXHPCDT-C578, "Construct Code 578
*        C579(4), "TYPEZTBM_ABXHPCDT-C579, "Construct Code 579
*        C580(4), "TYPEZTBM_ABXHPCDT-C580, "Construct Code 580
*        C581(4), "TYPEZTBM_ABXHPCDT-C581, "Construct Code 581
*        C582(4), "TYPEZTBM_ABXHPCDT-C582, "Construct Code 582
*        C583(4), "TYPEZTBM_ABXHPCDT-C583, "Construct Code 583
*        C584(4), "TYPEZTBM_ABXHPCDT-C584, "Construct Code 584
*        C585(4), "TYPEZTBM_ABXHPCDT-C585, "Construct Code 585
*        C586(4), "TYPEZTBM_ABXHPCDT-C586, "Construct Code 586
*        C587(4), "TYPEZTBM_ABXHPCDT-C587, "Construct Code 587
*        C588(4), "TYPEZTBM_ABXHPCDT-C588, "Construct Code 588
*        C589(4), "TYPEZTBM_ABXHPCDT-C589, "Construct Code 589
*        C590(4), "TYPEZTBM_ABXHPCDT-C590, "Construct Code 590
*        C591(4), "TYPEZTBM_ABXHPCDT-C591, "Construct Code 591
*        C592(4), "TYPEZTBM_ABXHPCDT-C592, "Construct Code 592
*        C593(4), "TYPEZTBM_ABXHPCDT-C593, "Construct Code 593
*        C594(4), "TYPEZTBM_ABXHPCDT-C594, "Construct Code 594
*        C595(4), "TYPEZTBM_ABXHPCDT-C595, "Construct Code 595
*        C596(4), "TYPEZTBM_ABXHPCDT-C596, "Construct Code 596
*        C597(4), "TYPEZTBM_ABXHPCDT-C597, "Construct Code 597
*        C598(4), "TYPEZTBM_ABXHPCDT-C598, "Construct Code 598
*        C599(4), "TYPEZTBM_ABXHPCDT-C599, "Construct Code 599
*        C600(4), "TYPEZTBM_ABXHPCDT-C600, "Construct Code 600
        ZUSER(12), "TYPEZTBM_ABXHPCDT-ZUSER, "Data Creator
        ZSDAT(8), "TYPEZTBM_ABXHPCDT-ZSDAT, "SEND FILE CREATED DATE
        ZSTIM(6), "TYPEZTBM_ABXHPCDT-ZSTIM, "SEND FILE CREATED TIME
        ZEDAT(8), "TYPEZTBM_ABXHPCDT-ZEDAT, "SAP INTERFACE DATE
        ZETIM(6), "TYPEZTBM_ABXHPCDT-ZETIM, "SAP INTERFACE TIME
        ZBDAT(8), "TYPEZTBM_ABXHPCDT-ZBDAT, "SAP BDC EXECUTED DATE
        ZBTIM(6), "TYPEZTBM_ABXHPCDT-ZBTIM, "SAP BDC EXECUTED TIME
        ZBNAM(12), "TYPEZTBM_ABXHPCDT-ZBNAM, "BDC User ID
        ZMODE(1), "TYPEZTBM_ABXHPCDT-ZMODE, "Characteristic Flag
        ZRESULT(1), "TYPEZTBM_ABXHPCDT-ZRESULT, "Result Processing
        ZMSG(220), "TYPEZTBM_ABXHPCDT-ZMSG, "Message text
      END   OF IT_HPCS.
* Assembly work spec Create by HPCS(CODE 1 - 300)
DATA: IT_HPCDT TYPE ZTBM_ABXHPCDT OCCURS 0 WITH HEADER LINE.
* Assembly work spec Create by HPCS(CODE 301 - 600)
DATA: IT_HPCDT01 TYPE ZTBM_ABXHPCDT01 OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* FUNCTION DATA
*----------------------------------------------------------------------*
DATA IT_VALUE TYPE ZSPP_VIN_VALUE OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* BDC DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
                                   USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_ZEDAT LIKE ZTBM_ABXHPCDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABXHPCDT-ZBTIM.

* EXCEL DATA UPLOAD
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM02 Classification'.
SELECTION-SCREEN END   OF BLOCK B1.
