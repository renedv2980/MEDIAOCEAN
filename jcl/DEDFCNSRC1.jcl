* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
 OPTION SDB=INPUT                * (so we don't have to change the JCL) 00003   
 INREC BUILD=(MITREC,SEQNUM,8,ZD)      *** Add seqnum to each record    00004   
 OUTFIL FNAMES=TYPE0406,               *** All "04" and "06" records    00005   
        INCLUDE=(MIT_SEQUENCE_CODE,EQ,PROGRAM_DATA,OR,                  00006   
                 MIT_SEQUENCE_CODE,EQ,COMMERCIAL_AUDIENCE_DATA)         00007   
 OUTFIL FNAMES=TYPE04D,                *** "04" / "D" records (or H)    00008   
        INCLUDE=(MIT_SEQUENCE_CODE,EQ,PROGRAM_DATA,AND,                 00009   
                (MIT_RECORD_TYPE,EQ,PROGRAM_DESCRIPTOR_RECORD,OR,       00010   
                 MIT_RECORD_TYPE,EQ,HOUSEHOLD_RECORD))                  00011   
 OUTFIL FNAMES=TYPE04P,                *** "04" / "P" records           00012   
        INCLUDE=(MIT_SEQUENCE_CODE,EQ,PROGRAM_DATA,AND,                 00013   
                 MIT_RECORD_TYPE,EQ,PERSONS_DETAIL_RECORD)              00014   
 OUTFIL FNAMES=TYPE06C,                *** "06" / "C" records           00015   
        INCLUDE=(MIT_SEQUENCE_CODE,EQ,COMMERCIAL_AUDIENCE_DATA,AND,     00016   
                 MIT_RECORD_TYPE,EQ,COMMERCIAL_DESCRIPTOR_RECORD)       00017   
 OUTFIL FNAMES=TYPE06P,                *** "06" / "P" records           00018   
        INCLUDE=(MIT_SEQUENCE_CODE,EQ,COMMERCIAL_AUDIENCE_DATA,AND,     00019   
                 MIT_RECORD_TYPE,EQ,PERSONS_DETAIL_RECORD)              00020   
 OUTFIL FNAMES=T1,SAVE                 *** All other records            00021   
