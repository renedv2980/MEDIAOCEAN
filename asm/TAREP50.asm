*          DATA SET TAREP50    AT LEVEL 006 AS OF 04/08/14                      
*PHASE T70350A                                                                  
         TITLE 'T70350 - REPORT OF DELETED AND CHANGED CAST RECORDS'            
T70350   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70350                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING RECD,R7             R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         IF CALLED WITH VALIDATE KEY                  
         BNE   *+12                                                             
         BAS   RE,VK               VALIDATE THE KEY                             
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       IF CALLED WITH PRINTREP                      
         BNE   *+8                                                              
         BAS   RE,PREP             PROCESS THE REPORT                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VK       NTR1                                                                   
         BAS   RE,VALOPT           MERELY VALIDATE THE OPTIONS                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO VALIDATE OPTIONS                                      
VALOPT   NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0 = NUM OF SCAN BLOCK ENTRIES               
*                                                                               
VOPT4    CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'        IF TRACING REQUESTED                         
         BNE   VOPT8                                                            
         OI    OPTION,X'80'        SAVE INDICATOR                               
*                                                                               
VOPT8    LA    R3,SCANNEXT         BUMP TO NEXT ELEMENT                         
         BCT   R0,VOPT4                                                         
*                                                                               
VOPTX    B     XIT                                                              
         DROP  R3                  DROP THE SCAND DSECT                         
         EJECT                                                                  
         SPACE 1                                                                
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OPEN  (RECVIN,(INPUT))    OPEN THE INPUT FILE                          
         BAS   RE,PUTSORT          RTN TO READ REC & PUT TO SORTER              
         BAS   RE,GETSORT          RTN TO GET RECS FROM SORTER & PRINT          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT DATA TO SORTER                                    
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
PUT05    XC    SORTREC,SORTREC                                                  
         LA    R2,RCVREC           R2 = A(INPUT AREA)                           
         GET   RECVIN,(R2)                                                      
         LA    R2,4(R2)            R2 = A(RECOVERY HEADER)                      
         USING RCVD,R2                                                          
         LA    R5,SORTREC          R5 = A(RECORD TO BE SENT TO SORTER)          
         USING SORTD,R5                                                         
*                                                                               
         CLI   RFILTY,X'72'        ONLY INTERESTED IN TALFILE                   
         BNE   PUT05                                                            
*                                                                               
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         USING TLCAD,R4                                                         
         CLI   TLCACD,TLCACDQ      ONLY INTERESTED IN CAST RECORDS              
         BNE   PUT05                                                            
*                                                                               
         CLI   RRECTY,X'01'        IF THIS IS A COPY                            
         BNE   PUT10                                                            
*                                                                               
         LA    R0,RDATA            SAVE COPY RECORD INTO AIO2 I/O AREA          
         LA    R1,2500                                                          
         L     RE,AIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         B     PUT05                                                            
*                                                                               
PUT10    CLI   RRECTY,X'02'        IF THIS IS A CHANGE                          
         BNE   PUT05                                                            
         TM    TLCASTAT,X'40'      IS IT MARKED FOR CAST DELETE?                
         BNO   PUT45                                                            
         BAS   RE,DELTACRD         CHK FOR DELETED TACRD ON CAST DELETE         
         BNE   PUT05                                                            
         B     PUT55                                                            
PUT45    BAS   RE,CHNTACRD         CHK FOR DELETED TACRD ON CHANGE REC          
         BNE   PUT05                                                            
PUT55    BAS   RE,COMREC           GET THE COMMERCIAL RECORD DETAILS            
         BNE   PUT05                                                            
*                                                                               
         BAS   RE,CAMRTN           GET SORTCAM FIELD                            
         MVC   SORTSSN,TLCASSN                                                  
         MVC   SORTCAT,TLCACAT                                                  
         BAS   RE,W4REC            GET PERFORMER NAME                           
         BAS   RE,PGMRTN           FIND NAME OF PGM THAT MADE CHANGE            
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     PUT05                                                            
         DROP  R2                                                               
         EJECT                                                                  
*              RTN TO GET PERFORMER NAME FROM THE W4 RECORD                     
         SPACE 1                                                                
W4REC    NTR1                                                                   
         MVC   SORTPERF(14),=CL14'NOT FOUND'                                    
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A8',TLCASSN),0                            
         BNE   XIT                                                              
         MVC   SORTPERF,TGNAME                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              RTN TO GET DETAILS FROM THE COMMERCIAL RECORD                    
*                                  WILL RETURN NOT EQUAL IF COMMERCIAL          
*                                  RECORD NOT FOUND OR IF IT IS A SOAP          
*                                                                               
         SPACE 1                                                                
COMREC   NTR1                                                                   
         MVC   SORTCID,=CL14'NOT FOUND'                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A8',TLCACOM),0                           
         BNE   XIT                                                              
         DROP  R4                                                               
         MVC   SORTNAME,TGNAME                                                  
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   SORTAGY,TLCOAGY                                                  
         MVI   ELCODE,TACOELQ      BEGIN SEARCH FOR COMMERCIAL ELEMENT          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TACOD,R4                                                         
         CLI   TACOTYPE,CTYSOAP    IS IT A SOAP?                                
         BE    NO                                                               
         MVC   SORTCID,TACOCID                                                  
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              RTN TO FIND NAME OF PGM EFFECTING CHANGE                         
         SPACE 1                                                                
PGMRTN   NTR1                                                                   
         USING TLCAD,R4                                                         
         MVC   SORTPGM,=CL23'UNKNOWN'                                           
         TM    TLCASTAT,X'40'                                                   
         BNO   *+14                                                             
         MVC   SORTPGM,=CL23'CAST DELETE'                                       
         B     XIT                                                              
         DROP  R4                                                               
         ZIC   R0,TWASCR                                                        
         LA    R3,RDATA                                                         
         ST    R3,AIO              AIO = A(RDATA)                               
         MVI   TWASCR,X'FF'                                                     
         GOTO1 ACTVOUT,DMCB,(X'A0',0)                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
         STC   R0,TWASCR           RESTORE TWASCR                               
         ICM   R4,15,DMCB          R4 = A(TAACD ELEMENT)                        
         BZ    XIT                                                              
         USING TAACD,R4                                                         
         LA    R2,REASDATA         R2 = A(TABLE WITH PGM NAMES)                 
         USING REASD,R2                                                         
PGM10    CLI   REASSCR,X'FF'       IF AT END OF TABLE                           
         BE    XIT                                                              
         CLC   TAACTSCR,REASSCR                                                 
         BNE   *+14                                                             
         MVC   SORTPGM,REASPGM                                                  
         B     XIT                                                              
         LA    R2,REASDLQ(R2)      BUMP TO NEXT TABLE ELEMENT                   
         B     PGM10                                                            
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO GET ON/OFF CAMERA DETAILS                             
         SPACE 1                                                                
CAMRTN   NTR1                                                                   
         MVC   SORTCAM,=C'***'                                                  
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TACAD,R4                                                         
         MVC   SORTCAM,TACAONOF                                                 
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              RTN TO CHECK TACRD ELEMENTS OF COPY/CHANGE RECORDS               
*                                  WILL RETURN EQUAL IF MORE TACRD              
*                                  ELEMENTS IN COPY THAN IN CHANGE              
         SPACE 1                                                                
CHNTACRD NTR1                                                                   
         L     R4,AIO2             R4 = A(COPY RECORD)                          
         BAS   RE,COUNTTAC         COUNT # TACRD IN COPY RECORD                 
         MVC   COPYCNT,TACOUNT                                                  
         LA    R4,RDATA            R4 = A(CHANGE RECORD)                        
         BAS   RE,COUNTTAC                                                      
         CLC   COPYCNT,TACOUNT     IS # TACRD IN COPY > # IN CHANGE?            
         BNH   NO                                                               
*                                                                               
         SR    R6,R6               R6 = COUNT OF TACRD ELEMENTS                 
         MVI   ELCODE,TACRELQ                                                   
         USING TACRD,R4                                                         
         L     R4,AIO2             R4 = A(COPY RECORD)                          
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHN35    BAS   RE,NEXTEL                                                        
         BNE   NO                                                               
         LA    R6,1(R6)                                                         
         LR    R2,R4               R2 = A('COPY' RECORD TACRD ELEMENT)          
         LA    R4,RDATA            R4 = A(CHANGE RECORD)                        
         BAS   RE,GETEL            START NESTED GETEL ON CHANGE RECORD          
         B     *+8                                                              
CHN45    BAS   RE,NEXTEL                                                        
         BE    CHN55                                                            
         LR    R4,R2               R4 = A(UNMATCHED COPY TACRD ELEM)            
         BAS   RE,TACDET           PUT TACRD DETAILS IN SORTREC                 
         B     YES                                                              
CHN55    CLC   TACREL(TACRAPPL-TACRD),0(R2)                                     
         BNE   CHN45                                                            
         CLC   TACRUSE(TACRTYPE-TACRUSE),TACRUSE-TACRD(R2)                      
         BNE   CHN45                                                            
         LR    R4,R2               IF SAME, R4 = (TACRD IN COPY REC)            
         B     CHN35                                                            
         EJECT                                                                  
*              RTN TO CHK TACRD ELS & SAVE LAST EL OF DELETED RECORD            
         SPACE 1                                                                
DELTACRD NTR1                                                                   
         SR    R6,R6               R6 = COUNT OF TACRD ELEMENTS                 
         LA    R4,RDATA            R4 = A(CHANGE RECORD)                        
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DEL10    BAS   RE,NEXTEL                                                        
         BNE   DEL99                                                            
         LA    R6,1(R6)                                                         
         BAS   RE,TACDET           PUT TACRD DETAILS IN SORTREC                 
         B     DEL10                                                            
DEL99    LTR   R6,R6                                                            
         BZ    NO                                                               
         B     YES                                                              
         SPACE 2                                                                
*              RTN TO COUNT TACRD ELEMENTS OF A CAST RECORD                     
         SPACE 1                                                                
COUNTTAC NTR1                                                                   
         SR    R6,R6               R6 = COUNT OF TACRD ELEMENTS                 
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
COU35    BAS   RE,NEXTEL                                                        
         BNE   COU99                                                            
         LA    R6,1(R6)                                                         
         B     COU35                                                            
COU99    STC   R6,TACOUNT                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              RTN TO MOVE DETAILS OF TACRD ELEMENT TO SORTREC                  
         SPACE 1                                                                
TACDET   NTR1                                                                   
         MVC   SORTUSE,TACRUSE                                                  
         OC    TACRINV,TACRINV                                                  
         BZ    TACDET5                                                          
         GOTO1 TINVCON,DMCB,TACRINV,SORTINV,DATCON                              
TACDET5  GOTO1 DATCON,DMCB,(X'21',TACRSTRT),(8,SORTCYCL),(1,TACREND)            
         EDIT  TACRAPPL,SORTAPPL,2,COMMAS=YES,ZERO=NOBLANK                      
         EDIT  TACRBAL,SORTBAL,2,COMMAS=YES,ZERO=NOBLANK                        
         STC   R6,SORTNUM                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO GET RECORDS FROM SORTER & PRINT THEM                  
         SPACE 1                                                                
GETSORT  NTR1                                                                   
         USING PRNTD,R3                                                         
*                                                                               
GET60    LA    R3,P                R3 = A(ADDRESS OF PRINT LINE)                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R5,15,4(R1)         R5 = A(SORTED RECORD)                        
         BZ    GETXIT                                                           
*                                                                               
         MVC   PRNTAGY,SORTAGY                                                  
         MVC   PRNTCMID,SORTCID                                                 
         MVC   PRNTSSN,SORTSSN                                                  
         MVC   PRNTCAT,SORTCAT                                                  
         MVC   PRNTCAM,SORTCAM                                                  
         MVC   PRNTPGM,SORTPGM                                                  
         MVC   PRNTAPPL,SORTAPPL                                                
         MVC   PRNTBAL,SORTBAL                                                  
         MVC   PRNTINV,SORTINV                                                  
         MVC   PRNTCYCL,SORTCYCL                                                
*        MVC   PRNTTRAN,SORTTRAN                                                
         MVC   PRNTUSE,SORTUSE                                                  
         LA    R3,L'P(R3)          R3 = A(P2)                                   
*                                                                               
         MVC   PRNTNAME,SORTNAME                                                
         MVC   PRNTPERF,SORTPERF                                                
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT          FOR BLANK LINE                               
         B     GET60                                                            
*                                                                               
GETXIT   GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
*              HEADLINE HOOK (HEADHOOK)                                         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE OF OUTPUT                                  
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ERROR ROUTINES                                                   
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
*              CONDITION CODE & EXIT ROUTINE                                    
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              STANDARD ROUTINES                                                
         SPACE 1                                                                
         GETEL  R4,DATADISP,ELCODE                                              
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,                                 X        
               MACRF=GM,EODAD=XIT                                               
SORTCARD DC    CL80'SORT FIELDS=(1,27,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
REASDATA DS    0C                                                               
         DC    X'8B',CL23'FTRACK DELETE'                                        
         DC    X'44',CL23'INVOICE CANCEL/REOPEN'                                
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECIFICATIONS                                            
         SPACE 1                                                                
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,117,PAGE                                                      
         SSPEC H2,100,REQUESTOR                                                 
*                                                                               
         SSPEC H1,46,C'REPORT OF DELETED AND CHANGED CAST RECORDS'              
*                                                                               
         SSPEC H7,02,C'AGENCY'                                                  
         SSPEC H8,02,C'------'                                                  
         SSPEC H7,10,C'COMMERCIAL ID'                                           
         SSPEC H8,10,C'-------------'                                           
         SSPEC H7,31,C'SS NUMBER'                                               
         SSPEC H8,31,C'---------'                                               
         SSPEC H7,41,C'CAT'                                                     
         SSPEC H8,41,C'---'                                                     
         SSPEC H7,48,C'CAM'                                                     
         SSPEC H8,48,C'---'                                                     
         SSPEC H7,55,C'APPLIED'                                                 
         SSPEC H8,55,C'-------'                                                 
         SSPEC H7,68,C'BALANCE'                                                 
         SSPEC H8,68,C'-------'                                                 
         SSPEC H7,84,C'CYCLE'                                                   
         SSPEC H8,78,C'-----------------'                                       
         SSPEC H7,97,C'USE'                                                     
         SSPEC H8,97,C'---'                                                     
         SSPEC H7,102,C'INVOICE'                                                
         SSPEC H8,102,C'-------'                                                
         SSPEC H7,111,C'REASON'                                                 
         SSPEC H8,111,C'------'                                                 
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
RECD     DSECT                                                                  
COPYCNT  DS    X                                                                
TACOUNT  DS    X                                                                
SORTREC  DS    XL200                                                            
RCVREC   DS    0H                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RHEAD    DS    XL24                                                             
RDATA    DS    2500C                                                            
         SPACE 2                                                                
*              DSECT TO COVER REASON TABLE                                      
         SPACE 1                                                                
REASD    DSECT                                                                  
REASSCR  DS    X                                                                
REASPGM  DS    CL23                                                             
REASDLQ  EQU   *-REASD                                                          
         SPACE 2                                                                
*              DSECT TO COVER SORT KEY                                          
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTAGY  DS    CL6                 AGENCY                                       
SORTCID  DS    CL12                COMMERCIAL ID                                
SORTSSN  DS    CL9                 PERFORMER SOC SEC NUMBER                     
SORKEYQ  EQU   *-SORTD                                                          
SORTNAME DS    CL35                COMMERCIAL NAME                              
SORTCAT  DS    CL3                 NAME OF COMMERCIAL                           
SORTPGM  DS    CL23                PGM RESPONSIBLE FOR CHANGE                   
SORTCAM  DS    CL3                 ON OR OFF CAMERA                             
SORTPERF DS    CL35                PERFORMNER NAME                              
SORTNUM  DS    X                   NUMBER OF TACRD ELEMENTS                     
SORTAPPL DS    CL10                AMT TO BE APPLIED                            
SORTBAL  DS    CL10                BALANCE                                      
SORTINV  DS    CL6                 INVOICE                                      
SORTCYCL DS    CL18                CYCLE START DATE                             
SORTUSE  DS    CL3                 USE NUMBER                                   
SORTDQ   EQU   *-SORTD                                                          
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
         DS    CL1                                                              
PRNTAGY  DS    CL6        AGENCY                                                
         DS    CL2                                                              
PRNTCMID DS    CL12       COMMERCIAL ID                                         
         DS    CL9                                                              
PRNTSSN  DS    CL9        S/S NUMBER                                            
         DS    CL2                                                              
PRNTCAT  DS    CL3        CATEGORY                                              
         DS    CL3                                                              
PRNTCAM  DS    CL3        CAMERA                                                
         DS    CL1                                                              
PRNTAPPL DS    CL10       AMT TO BE APPLIED                                     
         DS    CL3                                                              
PRNTBAL  DS    CL10       BALANCE                                               
         DS    CL3                                                              
PRNTCYCL DS    CL17       CYCLE START DATE                                      
         DS    CL2                                                              
PRNTUSE  DS    CL3        TACRD USE                                             
         DS    CL2                                                              
PRNTINV  DS    CL6        INVOICE                                               
         DS    CL3                                                              
PRNTPGM  DS    CL23       PROGRAM EFFECTING CHANGE                              
         ORG   PRNTCMID                                                         
PRNTNAME DS    CL20       NAME OF COMMERCIAL                                    
         DS    C                                                                
PRNTPERF DS    CL35       PERFORMER NAME                                        
         ORG                                                                    
         EJECT                                                                  
RCVD     DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006TAREP50   04/08/14'                                      
         END                                                                    
