*          DATA SET SRGAP00    AT LEVEL 006 AS OF 11/02/17                      
*PHASE T16F00A                                                                  
T16F00   TITLE 'SRGAP00 ($GAP) - GAP UPDATE FACILITY'                           
T16F00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$GAP**,RA,CLEAR=YES,RR=RE                          
         USING WORKD,RC            RC = A(WORKING STORAGE)                      
         ST    RE,RELO                                                          
         ST    RD,SAVERD                                                        
*                                                                               
         MVC   SRPARMS(8*4),0(R1)  SAVE SERVICE REQUEST PARAMETER LIST          
SRPARMSD USING SRPARMD,SRPARMS                                                  
*                                                                               
         L     R9,SRPARMSD.SRQASYSF                                             
         USING SYSFACD,R9          R9 = A(SYSTEM FACILITIES)                    
*                                                                               
         MVC   AUTL,SRPARMSD.SRQAUTL                                            
         L     RF,AUTL                                                          
         MVC   ATBUFF,TBUFF-UTLD(RF)  GETTING OUR MSG FROM TSAR BUFFER          
         L     RF,ATBUFF                                                        
         SHI   RF,2                                                             
         XR    RE,RE                                                            
         ICM   RE,3,0(RF)          GET THE LENGTH OF THE MESSAGE                
         STCM  RE,3,MQMSGLEN                                                    
*                                                                               
         L     R7,ATBUFF                                                        
         USING EVNTDSCT,R7         DSECT STARTS AT MESSAGE TYPE                 
         XC    DMCB(24),DMCB                                                    
*                                                                               
         MVI   DMCB,X'01'          SRGAP01                                      
         CLC   =C'CANSPTOM',EVSRVHDR                                            
         BE    GAPCALLV                                                         
*                                                                               
         MVI   DMCB,X'02'          SRGAP02                                      
         CLC   =C'WBRQUEST',EVSRVHDR                                            
         BE    GAPCALLV                                                         
*                                                                               
         MVI   DMCB,X'03'          SRGAP03                                      
         CLC   =C'OEXPRESS',EVSRVHDR                                            
         BE    GAPCALLV                                                         
*                                                                               
         B     NO                                                               
*                                                                               
GAPCALLV GOTO1 VCALLOV,DMCB                  GET THE OVERLAY                    
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,SRPARMS             START UP THE OVERLAY               
*                                                                               
YES      SR    RC,RC               SET CC TO EQ                                 
NO       LTR   RC,RC               SET CC TO NEQ                                
XIT      XIT1                      RETURN TO CALLER                             
*                                                                               
ON31     O     RE,=XL4'80000000'                                                
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=XL4'7FFFFFFF'                                                
         BSM   0,RE                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
EVNTOBJD DSECT                                                                  
EOBJTID  DS    CL1                 TRANSMISSION ID                              
EOBJRTN  DS    AL4                 A(ROUTINE)                                   
EOBJNXT  DS    0C                                                               
*                                                                               
BIZNZIDD DSECT              *** 32 BYTES -  AAMCCCTCAMP#ORDER#                  
BZIAALPH DS    CL2                 AGENCY ALPHA                                 
BZIMEDIA DS    C                   MEDIA                                        
BZICLTCD DS    CL3                 CLIENT CODE                                  
BZIACTYP DS    C                   ACTION TYPE (BOOKING/CANCELLATION)           
BZICMPNM DS    CL6                 CAMPAIGN NUMBER WITH LEADING ZEROS           
BZIORDNM DS    CL7                 ORDER NUMBER WITH LEADING ZEROS              
BZIREVNM DS    CL2                 REVISION NUMBER WITH LEADING 0'S             
         DS    CL10                SPARE                                        
*                                                                               
EVNTDSCT DSECT              *** WE DO NOT GET THE FP=AGY=SJSYS=SPT              
EVSRVHDR DS    CL8                 'CANSPTOM'                                   
EVTYPE   DS    CL1                 EVENT TYPE                                   
EVDCBZID DS    CL32                DOCUMENT BUSINESS ID                         
EVEVENTS DS    0C                                                               
*******                                                                         
* EVTYPE = C'R' - RESPONSE EVENT                                                
*******                                                                         
EVRRBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVRRSPNS DS    CL32                RESPONSE                                     
EVRDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVRTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVRIP    DS    CL15                XXX.XXX.XXX.XXX                              
EVRCMMNT DS    CL1024              COMMENT                                      
*******                                                                         
* EVTYPE = C'A' - ACCESS EVENT                                                  
*******                                                                         
         ORG   EVEVENTS                                                         
EVARBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVADSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVATSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVAIP    DS    CL15                XXX.XXX.XXX.XXX                              
*******                                                                         
* EVTYPE = C'C' - CONCLUSION EVENT                                              
*******                                                                         
         ORG   EVEVENTS                                                         
EVCDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVCTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVCCAUSE DS    CL11                CAUSE                                        
*******                                                                         
* EVTYPE = C'D' - DECACTIVATION EVENT                                           
*******                                                                         
         ORG   EVEVENTS                                                         
EVDRBZID DS    CL128               RECIPIENT BUSINESS ID                        
EVDDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVDTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
*******                                                                         
* EVTYPE = C'N' - NO REPSONSE EVENT                                             
*******                                                                         
         ORG   EVEVENTS                                                         
EVNDSTMP DS    CL8                 DATE STAMP  YYYYMMDD                         
         DS    C                                                                
EVNTSTMP DS    CL6                 TIME STAMP  HHMMSS                           
EVNRBZID DS    CL128               RECIPIENT BUSINESS ID                        
                                                                                
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
*                                                                               
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
AUTL     DS    A                                                                
ATBUFF   DS    A                                                                
DAFILT   DS    A                   DISK ADDRESS FILTER                          
DSKAD    DS    A                   LAST PAGE'S LAST DISK ADDRESS                
AIO      DS    A                                                                
AIO1     DS    A                   A(IOAREA #1)                                 
AIO2     DS    A                   A(IOAREA #2)                                 
ASPLAREA DS    A                   A(SPOOL AREA)                                
AWRKRIOA DS    A                   A(IO AREA USED BY EDICT)                     
AWRKRBUF DS    A                   A(WORKER BUFFER AREA)                        
AHUGEBLK DS    A                   A(HUGE BLOCK)                                
*                                                                               
VADDAY   DS    V                                                                
VDATCON  DS    V                                                                
VGETFACT DS    V                                                                
VHELLO   DS    V                                                                
VHEXIN   DS    V                                                                
VHEXOUT  DS    V                                                                
VLOCKET  DS    V                                                                
VRECUP   DS    V                                                                
VCLUNPK  DS    V                                                                
VMSPACK  DS    V                                                                
VMSUNPK  DS    V                                                                
VSWITCH  DS    V                                                                
VCLPACK  DS    V                                                                
*                                                                               
ASPOOL   DS    A                                                                
ASTAPACK DS    A                                                                
AREPFACS DS    A                                                                
*                                                                               
SRPARMS  DS    8F                  SERVICE REQUEST PARAMETERS                   
EDCTFDSK DS    F                   EDICTFIL DISK ADDRESS   TTTTBB00             
EDCTFDSP DS    F                   EDICTFIL DISP ADDRESS   DD999999             
DMCB     DS    6F                                                               
FULL     DS    F                                                                
SVORDDA  DS    A                   SAVED DISK ADDRESS OF ORDER                  
SVMKNDA  DS    A                   SAVED DISK ADDRESS OF MKGD NOTICE            
*                                                                               
DMINBTS  DS    X                                                                
EDCTFRPT DS    H                   EDICTFIL PHYSICAL RECORDS PER TRACK          
EDCTFTPD DS    H                   EDICTFIL TRACKS PER DAY                      
EDCTFATQ EQU   40                     ON ADV                                    
EDCTFRTQ EQU   40                     ON REP                                    
EDCTFTTQ EQU   2                      ON TEST                                   
EDCTFMTQ EQU   2                      ON MEL                                    
EDCTRPBQ DS    H                   EDICTFIL LOGICAL RECORDS PER BLOCK           
EDCTLRCL DS    H                   EDICTFIL LOGICAL RECORD LENGTH               
EDCTFLST DS    H                   LAST TRACK FOR GIVEN DAY                     
FRSTTRK  DS    H                   FIRST TRAC FOR GIVEN DAY                     
RECLEN   DS    H                                                                
TERMNUM  DS    H                   TERMINAL NUMBER                              
USERNUM  DS    H                   USER ID NUMBER                               
TRNNUM   DS    H                   TRANSACTION COUNT                            
HALF     DS    H                                                                
DATADISP DS    H                                                                
MQMSGLEN DS    H                                                                
*                                                                               
AGENCY   DS    XL2                 AGENCY POWER CODE                            
SIGNON2H DS    XL2                 2 BYTE HEX AGENCY ID                         
USERID   DS    CL10                CONTROL USER ID                              
BAGYMD   DS    XL1                 BINARY AGENCY/MEDIA                          
BCLT     DS    XL2                 BINARY CLIENT CODE                           
BEST     DS    XL1                 BINARY ESTIMATE                              
BMKTSTA  DS    XL5                 BINARY MARKET STATION                        
BPRD     DS    XL1                                                              
BINCAMPN DS    XL4                 CAMPAIGN NUMBER (FF COMPLEMENT)              
BINORDER DS    XL4                 ORDER NUMBER (FF COMPLEMENT)                 
BINREVSN DS    XL2                 REVISION # (FF COMPLEMENT)                   
SVAPRF07 DS    XL1                 SAVED COPY OF APROF+07                       
SVCPRF00 DS    XL1                 SAVED COPY OF CPROF+00                       
SVCOFFC  DS    XL1                 CLIENT OFFICE                                
         DS    0F                                                               
PACKOF4B DS    PL4                 PACKED NUMBER OF 4 BYTES                     
JDTTODAY DS    PL4                 JULIAN DATE OF TODAY                         
SVDATTIM DS    CL5                 SAVED DATE AND TIME OF MG CANCEL             
*                                                                               
QMED     DS    CL1                 EBCDIC MEDIA                                 
QBUYER   DS    CL12                       BUYER CODE                            
QCLT     DS    CL3                        CLIENT                                
QPRD1    DS    CL3                        PRODUCT 1                             
QPRD2    DS    CL3                        PRODUCT 2                             
QEST1    DS    CL3                        ESTIMATE 1                            
QSTA     DS    CL8                        STATION                               
QACTTYPE DS    CL1                        ACTION TYPE (B OR C)                  
*                                                                               
ESTPW    DS    CL3                 PW PERCENTAGE                                
ESTCOST2 DS    XL4                 COST2 PERCENTAGE                             
ESTLOKYM DS    XL2                 BYTE 0 - YEAR, BYTE 1 MONTH                  
*                                                 X'80' - PREVIOUS              
*                                                 X'40' - SUBSEQUENT            
*                                                                               
THESYSID DS    XL1                 SAVED SYSTEM ID                              
*                                                                               
SPTSENUM DS    XL1                 SPOT SYSTEM SENUM                            
BYTE     DS    C                                                                
*                                                                               
PIGPRD   DS    XL1                 PIGGYBACK PRODUCT BINARY CODE                
PIGEST   DS    XL1                 PIGGYBACK ESTIMATE                           
*                                                                               
MISCFLG1 DS    XL1                 VARIOUS BIT FLAGS FOR X'11' ELEM             
MF1XMTUP EQU   X'80'                -  XMT HAS BEEN UPDATED MUST PUTREC         
MF1NOXMT EQU   X'40'                -  NO TRANSMISSION ELEMENT FOUND            
*                                                                               
BITFLAG1 DS    XL1                 VARIOUS BIT FLAGS                            
BF1SKPTB EQU   X'80'                - SKIP TRACK/BLOCK                          
BF1SKIPB EQU   X'40'                - SKIP RECORD BUMP                          
BF1FRSTR EQU   X'20'                - FIRST RECORD IN BLOCK                     
BF1DPEND EQU   X'10'                - DARE RECORD PENDING                       
BF1NWCOM EQU   X'08'                - NEED TO ADD REP COMMENT RECORD            
BF1IGRCM EQU   X'04'                - IGNORE REST OF REP COMMENTS               
BF1PSSWD EQU   X'02'                - PASSWORD REQUIRED                         
BF1YSDAY EQU   X'01'                - USING PRIOR BUSINESS DAY'S INFO           
*                                                                               
EOBJNUMB DS    XL1                 DARE OBJECT NUMBER                           
EOBJDLNQ EQU   1                   DARE DELIVERY NOTIFICATION                   
EOBJOAPQ EQU   2                   DARE ORDER APPROVAL                          
EOBJORJQ EQU   3                   DARE ORDER REJECTION                         
EOBJOCMQ EQU   4                   DARE ORDER REJECTION COMMENT                 
EOBJOTRQ EQU   5                   DARE ORDER TRAILER                           
EOBJOCFQ EQU   6                   DARE ORDER CONFIRMATION                      
EOBJOLNQ EQU   7                   DARE ORDER LINE NUMBER EQUIVALENTS           
EOBJERRQ EQU   8                   DARE ERROR NOTIFICATION                      
EOBJARCQ EQU   9                   DARE AGENCY RECALL                           
EOBJORAQ EQU   10                  DARE ORDER RECALL ACKNOWLEDGEMENT            
EOBJMOKQ EQU   11                  MAKEGOOD CONFIRMATION                        
EOBJMCNQ EQU   12                  MAKEGOOD CANCELLATION                        
EOBJDFXQ EQU   13                  DARE FAX DELIVERY NOTIFICATION               
EOBJCFXQ EQU   14                  DARE FAX CANCELLATION                        
EOBJMKGQ EQU   15                  MAKEGOOD HEADER                              
EOBJEFXQ EQU   16                  DARE FAX ERROR                               
EOBJSALE EQU   17                  SALESPERSON REASSIGNMENT                     
EOBJURL  EQU   18                  URL CONFIRMATION                             
*                                                                               
BTODAY   DS    XL3                 TODAY'S DATE IN BINARY (YMD)                 
PRIORDAT DS    XL3                 YESTERDAY'S OR SOME PRIOR DAY'S DATE         
SYSNAME  DS    CL3                 3 CHR FACPAK NAME                            
SYSN1    DS    CL1                 1 CHR FACPAK NAME                            
WRKFILNO DS    XL2                 WORK FILE NUMBER                             
WRKRECNO DS    F                   WORKER FILE RECORD NUMBER                    
*                                                                               
WORK     DS    XL64                                                             
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DKEY     DS    XL64                KEY OF LAST DA RECORD ADDED                  
HEADER   DS    XL32                                                             
DMWORK   DS    12D                                                              
*                                                                               
FILENAME DS    CL7                 DMGR FILENAME                                
LASTFILE DS    CL1                 PREVIOUS FILE NUMBER                         
DALINK   DS    CL4                 DISK ADDRESS OF LAST ADDREC                  
DDA      DS    CL4                 DISK ADDRESS OF GETREC                       
DALAST   DS    CL1                 LAST DA FILE ADDED TO                        
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
LASTACTN DS    CL1                 LAST TYPE (COPY CHNG ADD)                    
DLNFRID  DS    XL2                 DELIVERY NOTICE SENDER ID NUM                
ROUTNGCD DS    CL5                 ROUTING CODE                                 
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
ELCODE   DS    XL1                                                              
REVISION DS    XL1                 REVISION NUMBER                              
ORDAUDTR DS    X                   ORDER AUDIT TRAIL ACTION                     
REPORDTP DS    XL1                 REP ORDER TYPE                               
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
NUMSPOTS DS    XL1                                                              
*                                                                               
TRDEMTHD DS    CL1                 TRADE METHOD                                 
TRDEDATA DS    CL8                 DATA TO DETERMINE TRADE                      
*                                                                               
RECCOUNT DS    F                   RECORD COUNT                                 
*                                                                               
DEMSAVE  DS    XL6                 DEMO EXIT SAVE AREA FOR DEMPROS              
*                                                                               
SAVEBUFF DS    0XL12                                                            
SBVPQTAB DS    A                   A(PQTAB)                                     
SBVPQFIL DS    A                   A(PQFILE DTF)                                
SBSAVE   DS    A                   DISPLACEMENT TO START OF SAVE AREA           
*                                                                               
SAVESIN  DS    F                                                                
NEXTSIN  DS    F                                                                
QSIN     DS    CL6                                                              
PRTQID   DS    CL8                                                              
*                                                                               
PROFDAR  DS    CL16                DAR PROFILE                                  
PDARONHD EQU   PROFDAR+0            - ON-HOLD IF MKGD OKAYED BY REP?            
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
REMUSER  DS    CL3                                                              
BIGSPLKY DS    CL144                                                            
*                                                                               
ACURELEM DS    F                   ADDRESS OF CURRENT ELEMENT                   
ELEM     DS    CL256                                                            
CMTELEM  DS    CL256                                                            
*                                                                               
WORKX    EQU   *                                                                
*                                                                               
REPDKEYD DSECT                     DSECT TO USE OVER WRKRIOA IN AGYRCL          
RKEYDA   DS    XL4                 DISK ADDRESS OF AGENCY ORDER RECORD          
ROLDKEYS DS    XL800               TO REBUILD REP EDI PASSIVE KEYS              
RNEWKEYS DS    XL800                                                            
RKEYIO   DS    XL4000                                                           
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE FAPQPL                                                         
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENDARE                                                      
       ++INCLUDE CTGENRAD                                                       
       ++INCLUDE DMGREQUS                                                       
       ++INCLUDE DMPRTQD                                                        
       ++INCLUDE DMPRTQS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMFILTABD                                                      
       ++INCLUDE TASYSWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
RQHHDRD  DSECT                                                                  
       ++INCLUDE DMREQHDRA                                                      
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE DDEDICTFIL                                                     
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE SRGAPFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRGAP00   11/02/17'                                      
         END                                                                    
