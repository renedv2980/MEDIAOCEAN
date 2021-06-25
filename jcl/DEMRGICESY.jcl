*===================================================================    00001   
* THESE ARE DFSORT SYMBOL STATEMENTS FOR DEMRGICE.                      00002   
* THIS MEMBER IS ALSO REFERENCED DIRECTLY IN JCL (USED BY DEMCNV).      00003   
*===================================================================    00004   
*                                                                       00005   
* THESE CONSTANT VALUES ARE DEFINED SUCH THAT FOR A GIVEN MAJOR KEY,    00006   
* RECORDS MARKED FOR DELETION WILL ALWAYS SORT *AFTER* THOSE NOT MARKED 00007   
* FOR DELETION.                                                         00008   
*                                                                       00009   
MARKED_KEEP,C'0'                                                        00010   
MARKED_DELETED,C'1'                                                     00011   
FORCE_FIRST,X'00'                                                       00012   
FORCE_AFTER,X'FF'                                                       00013   
NTI_BITMAP_KEY,C'PNNPPPP'  NTI BITMAP RECORD KEY                        00014   
*                            PRKEY = C'PNN'                             00015   
*                            PRSTAT STARTS WITH C'PPPP'                 00016   
EMPTY_NTI_RECORD_FMS,X'FEFEFE'                                          00017**2
*                                                                       00018   
* INPUT AND OUTPUT RECORD DEFINITION                                    00019   
*                                                                       00020   
RDW,1,4,BI             RDW                                              00021   
RECORD,5               INPUT/OUTPUT RECORD                              00022   
*                                                                       00023   
* SORT WORK RECORD DEFINITION                                           00024   
*                                                                       00025   
S_PASSIVE_LENGTH,+44   *** KEEP THIS IN SYNC WITH "S_" FIELDS BELOW *** 00026**4
*                      *** (LENGTH OF PASSIVE SORT WORK RECORD) ***     00027**4
S_RDW,1,4,BI           RDW                                              00028   
S_RDW_RECLEN,=,2,FI    VARIABLE RECLEN                                  00029**3
 SKIP,2                                                                 00030**3
S_DELETE_FLAG,*,1,CH   C'0' = KEEP, C'1' = MARKED FOR DELETION          00031   
S_SEQNUM,*,8,PD        SEQUENCE NUMBER IS RESET WITH EACH NEW MAJOR KEY 00032   
S_MAJOR_KEY_SEQ,*,1,BI X'00' = FORCE AHEAD OF OTHER MAJOR KEYS          00033   
*                       (FOR NTI PROGRAM NUMBER BITMAP RECORDS)         00034   
*                      X'FF' = DO NOT FORCE FIRST                       00035   
S_PUSH_FMT_AND_STATION,*,7,CH                                           00036**2
S_RECORD,*             COMPLETE INPUT RECORD                            00037   
 POSITION,S_RECORD                                                      00038   
S_MAJOR_KEY,*,18,BI    MAJOR KEY                                        00039   
S_UPD_DIR,=,23,BI      ENTIRE DIRECTORY RECORD (IN **UPDATE** FORMAT)   00040**4
S_FMS,=,3,CH           FILE/MEDIA/SOURCE                                00041**4
S_FILE,=,1,CH          FILE                                             00042**4
S_MEDIA,=,1,CH         MEDIA                                            00043**4
S_SOURCE,=,1,CH        SOURCE                                           00044**4
S_FMS_AND_STATION,=,7,CH   FILE/MEDIA/SOURCE/STATION PREFIX             00045**2
 SKIP,11                                                                00046   
S_MINOR_KEY,*,2,BI     MINOR KEY                                        00047   
 SKIP,2                 2-BYTE RECORD LENGTH (OR PASSIVE INDICATOR)     00048**3
S_STATUS_BYTE,*,1,BI                                                    00049**3
MARKED_FOR_DELETION,X'80'                                               00050**3
EXTENDED_PASSIVE,X'40'                                                  00051**3
*                                                                       00052   
*                                                                       00053**4
* SEE DEDEMFILE FOR ORIGINAL DSECTS                                     00054**4
*                                                                       00055**4
PMCODE,1,1,CH                      'Q' RECORD TYPE                      00056**4
PMCODEQU,C'Q'                      NETWORK PROGRAM RECORD               00057**4
PMMEDIA,2,1,CH                     MEDIA                                00058**4
PMSRC,3,1,CH                       SOURCE                               00059**4
PMBOOK,4,2,BI                      BOOK                                 00060**4
PMSTAT,6,5,CH                      STATION                              00061**4
PMSTYP,11,1,BI                     STATION TYPE                         00062**4
PMBTYP,12,1,BI                     BOOK TYPE                            00063**4
PMOPIID,13,6,BI                    OPI PROGRAM ID                       00064**4
*                                                                       00065**4
PZCODE,1,1,CH                      'Z' RECORD TYPE                      00066**4
PZCODEQU,C'Z'                      NIELSEN PROGRAM# MAPPING (TO NTI#)   00067**4
PZMEDIA,2,1,CH                     MEDIA                                00068**4
PZSRC,3,1,CH                       SOURCE                               00069**4
PZSTAT,4,4,CH                      NETWORK                              00070**4
PZSTAT3,4,3,CH                     NETWORK(3)                           00071**4
PZSUBTYP,8,1,CH                    H/N/S                                00072**4
PZEXTNUM,12,5,BI                   NIELSEN PROGRAM NUMBER               00073**4
PZINTNUM,17,2,BI                   ASSIGNED NTI#                        00074**4
* WATCH OUT: THESE FIELDS ARE DEFINED AS THEY APPEAR IN THE CONVERSION  00075**4
*            OUTPUT FILE (AS OPPOSED TO THE ACTUAL DEMO DIRECTORY)!     00076**4
PZUPFDAT,19,2,BI                   1ST LOAD DATE FOR NIELSEN# (TYPE14)  00077**4
PZUPLDAT,21,2,BI                   LAST LOAD DATE FOR NIELSEN# (TYPE14) 00078**4
PZSTATUS,23,1,BI                   X'40' (EXTENDED PASSIVE)             00079**4
*                                                                       00080   
PJCODE,1,1,CH                      'J' RECORD TYPE                      00081**4
PJCODEQU,C'J'                      NIELSEN PROGRAM# MAPPING (TO NTI#)   00082**4
PJMEDIA,2,1,CH                     MEDIA                                00083**4
PJSRC,3,1,CH                       SOURCE                               00084**4
PJSTAT,4,4,CH                      NETWORK                              00085**4
PJEXTNUM,12,5,BI                   NIELSEN PROGRAM NUMBER               00086**4
PJINTNUM,17,2,BI                   ASSIGNED NTI#                        00087**4
