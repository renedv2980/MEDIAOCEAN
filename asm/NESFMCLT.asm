*          DATA SET NESFMCLT   AT LEVEL 012 AS OF 05/01/02                      
*PHASE T31CCTA                                                                  
*INCLUDE RECUP                                                                  
         TITLE 'NESFMCLT - CLIENT HEADER'                                       
         PRINT NOGEN                                                            
T31CCT   CSECT                                                                  
         NMOD1 0,T31CCT,RR=R2                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R7,AIO1                                                          
         USING CLTHDRD,R7                                                       
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,XRECADD        ADD PASSIVE KEY                              
         BE    AK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    FMT                                                              
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************                         
* DISPLAY RECORD                                                                
*                                                                               
FMT      DS    0H                                                               
         MVI   ERROR,NOCLTACC                                                   
         OC    T31CFFD+6(2),T31CFFD+6                                           
         BZ    FMT0                                                             
         CLI   T31CFFD+6,C'*'                                                   
         BNE   FMT0                                                             
         CLC   T31CFFD+7(1),COFFICE                                             
         BE    FMT0                                                             
****     NI    LFMKEYH+4,X'DF'     RESET 'KEY VALID'                            
         B     LFMERR                                                           
*                                                                               
FMT0     DS    0H                                                               
         FOUT  CLTNAMEH,CNAME,20                                                
         FOUT  CLTONUMH,COFFICE,1                                               
         MVC   CLTIFCD,CCLTIFC                                                  
         FOUT  CLTIFCDH                                                         
*                                                                               
FMT2     LA    R1,CPROF                                                         
         LA    R4,15               FOR BCT                                      
         LA    R2,CLTOP1H                                                       
FMT2B    FOUT  (R2),(R1),1                                                      
         BAS   RE,FNDNXUF                                                       
         LA    R1,1(R1)                                                         
         BCT   R4,FMT2B                                                         
*                                                                               
FMT3     DS    0H                                                               
         LA    R1,CEXTRA           EXTRA PROFILE BYTES                          
         CLI   CEXTRA+2,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+2,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+3,C'0'       DISPLAY AS N                                 
         BNE   *+8                                                              
         MVI   CEXTRA+3,C'N'                                                    
         CLI   CEXTRA+5,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+5,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+6,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+6,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+7,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+7,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+8,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+8,C'N'       CHANGE 0 TO N ON DISPLAY                     
         CLI   CEXTRA+9,C'0'                                                    
         BNE   *+8                                                              
         MVI   CEXTRA+9,C'N'       CHANGE 0 TO N ON DISPLAY                     
         LA    R4,10               FOR BCT                                      
         LA    R2,CLTEX1H                                                       
FMT3B    FOUT  (R2),(R1),1                                                      
         BAS   RE,FNDNXUF                                                       
         LA    R1,1(R1)                                                         
         BCT   R4,FMT3B                                                         
*                                                                               
FMT6     DS    0H                                                               
         FOUT  CLTTTLEH,CTITLE,10                                               
FMTX     B     EXIT                                                             
         EJECT                                                                  
*********************************************************                       
* VALIDATE RECORD                                                               
*                                                                               
EDT      DS    0H                                                               
         LA    R2,CLTNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   CNAME,CLTNAME                                                    
         OC    CNAME,SPACES                                                     
         CLC   =C'DELETE',CNAME                                                 
         BNE   EDT0X                                                            
*                           *****  DELETE RECORD  ******                        
         MVI   ERROR,X'02'                                                      
         CLI   MODE,ACTADD         NO DELETE ON ADD                             
         BE    LFMERR                                                           
         MVC   SVKEY,KEY                                                        
         MVI   ERROR,X'E1'                                                      
         OC    CLIST(100),CLIST    MAKE SURE NO PRDS LEFT                       
         BNZ   LFMERR                                                           
*                                  CHECK FOR DIV/DST DEF RECS                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D01'                                                  
         MVC   KEY+2(3),SVKEY+1    AGY/MED/CLT                                  
         GOTO1 HIGH                                                             
         B     EDT0C                                                            
EDT0B    GOTO1 SEQ                                                              
EDT0C    CLC   KEYSAVE(5),KEY                                                   
         BNE   EDT0F                                                            
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EDT0B                                                            
*                                                                               
EDT0F    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),SVKEY+1                                                 
         GOTO1 HIGH                                                             
         B     EDT0H                                                            
EDT0G    GOTO1 SEQ                                                              
EDT0H    CLC   KEY(5),KEYSAVE                                                   
         BNE   EDT0K                                                            
         CLI   KEY+8,C'G'                                                       
         BL    EDT0I                                                            
*                            SEE IF I NEED TO ADD TO SCHEME TABLE               
         LA    R5,ELEM                                                          
EDT0H2   CLI   0(R5),0                                                          
         BE    EDT0H4                                                           
         CLC   0(1,R5),KEY+8                                                    
         BE    EDT0I               FOUND                                        
         LA    R5,1(R5)                                                         
         B     EDT0H2                                                           
*                                                                               
EDT0H4   DS    0H                                                               
         MVC   0(1,R5),KEY+8       SAVE LIST OF SCHEMES                         
EDT0I    DS    0H                                                               
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EDT0G                                                            
*                                                                               
EDT0K    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D03'         MKTGRPS ASSGNS                           
         MVC   KEY+8(3),SVKEY+1        AGY/MED,CLT                              
         GOTO1 HIGH                                                             
         B     EDT0M                                                            
EDT0L    GOTO1 SEQ                                                              
EDT0M    CLC   KEY(11),KEYSAVE                                                  
         BNE   EDT0P                                                            
         MVI   KEY+13,X'DD'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     EDT0L                                                            
*                                                                               
EDT0P    DS    0H                                                               
         LA    R5,ELEM                                                          
EDT0Q    DS    0H                                                               
         CLI   0(R5),0             END OF SCHEME LIST                           
         BE    EDT0S                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),SVKEY+1        AGY/MED                                  
         MVC   KEY+8(1),0(R5)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                MUST FIND SCHEME                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
EDT0R    BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND EXCEPTION ELEM                     
         CLC   2(3,R6),QCLT                                                     
****     CLC   2(3,R6),SVEBCCLT                                                 
         BNE   EDT0R                                                            
         GOTO1 =V(RECUP),DMCB,(0,AIO),0(R6),0,RR=RELO                           
         GOTO1 PUTREC                                                           
         LA    R5,1(R5)            NEXT SCHEME                                  
         B     EDT0Q                                                            
*                                                                               
EDT0S    DS    0H                                                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         GOTO1 GETREC              MUST REREAD CLIENT HEADER                    
         MVI   KEY+13,X'DD'        SET DIR DELETED                              
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         L     R1,AIO                                                           
         MVI   15(R1),X'C0'                                                     
         GOTO1 PUTREC                                                           
         MVC   CLTNAME(20),=C'** CLIENT DELETED **'                             
         FOUT  (R2)                                                             
         B     EXIT                                                             
         EJECT                                                                  
EDT0X    DS    0H                                                               
         LA    R2,CLTONUMH                                                      
         MVI   COFFICE,0                                                        
         CLI   SVAPROF+13,C'Y'     TEST OFFICE REQUIRED                         
         BE    EDT1A                                                            
         CLI   5(R2),0                                                          
         BE    EDT1B                                                            
EDT1A    GOTO1 ANY                                                              
         MVC   COFFICE,CLTONUM                                                  
*                                                                               
EDT1B    LA    R2,CLTIFCDH                                                      
         CLI   5(R2),0                                                          
         BE    EDT2                                                             
         MVC   CCLTIFC,8(R2)                                                    
         B     EDTP                                                             
*                                                                               
EDT2     LA    R1,AGYTAB              TABLE OF AGENCIES THAT REQUIRE            
         LA    R4,AGYTABN             NUMBER OF AGYS                            
EDT2A    CLC   AGYALPHA,0(R1)                                                   
         BE    EDT2B                                                            
         LA    R1,2(R1)                                                         
         BCT   R4,EDT2A                                                         
         B     EDTP                                                             
*                                                                               
EDT2B    MVI   ERROR,MSSNGERR                                                   
         B     LFMERR                                                           
*                                  EDIT CLIENT PROFILE                          
EDTP     LA    R2,CLTOP1H                                                       
         LA    R4,PROFTAB                                                       
         LA    R6,WORK                                                          
         MVC   WORK(30),=30C'0'    INITIALIZE TO 0'S                            
EDTP1    CLI   0(R4),0             END OF TABLE                                 
         BE    EDTP10                                                           
         CLI   2(R4),X'FF'                                                      
         BE    EDTP3            GO TO SPECIAL EDIT FOR 1-9 OR A-Z               
         SR    R5,R5                                                            
         IC    R5,0(R4)         LENGTH OF ENTRY                                 
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LA    R7,2(R4)       SET R7 TO FIRST ENTRY                             
*                 R5 NOW SET FOR BCT-NUMBER OF VALID CODES                      
EDTP2    CLC   8(1,R2),0(R7)                                                    
         BE    EDTP4               VALID VALUE FOUND                            
         LA    R7,1(R7)                                                         
         BCT   R5,EDTP2                                                         
EDTPERR  MVI   ERROR,INVERR        INVALID INPUT                                
         B     LFMERR                                                           
EDTP3    CLI   8(R2),C'0'               0-9                                     
         BL    EDTP3A                                                           
         CLI   8(R2),C'9'                                                       
         BH    EDTPERR                                                          
         B     EDTP4                                                            
*                                                                               
EDTP3A   CLI   8(R2),C'A'               A-Z                                     
         BL    EDTPERR                                                          
         CLI   8(R2),C'Z'                                                       
         BH    EDTPERR                                                          
         B     EDTP4                                                            
*                                                                               
EDTP4    MVC   0(1,R6),8(R2)        PUT VALIDATED VALUE INTO WORK               
         LA    R6,1(R6)                                                         
         SR    R0,R0                                                            
         IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BAS   RE,FNDNXUF            NEXT UNPROTECTED FIELD                     
         B     EDTP1                                                            
         EJECT                                                                  
*                        NOW CHECK NEW PROFILE VS OLD                           
EDTP10   DS    0H        AND CROSS-CHECK NEW PROFILE                            
         CLI   MODE,ACTADD                                                      
         BE    EDTP10A                                                          
*                                  BILLING PROF CHK BYPASSED                    
         B     EDTP10A                                                          
*                                                                               
         CLC   WORK+5(1),CPROF+5                                                
         BE    EDTP10A                                                          
         LA    R2,CLTOP6H                                                       
         MVI   ERROR,NOCHGERR                                                   
         B     LFMERR                                                           
*                             GET AGYHEADER TO CROSS-CHECK PROFILES             
EDTP10A  DS    0H                                                               
         CLI   WORK+3,C'0'                                                      
         BNE   CKARB                                                            
         CLI   SVAPROF,C'0'                                                     
         BE    EDTP10D                                                          
         CLI   SVAPROF,C'2'                                                     
         BE    EDTP10D                                                          
PROFERR  LA    R2,CLTOP4H                                                       
         MVI   ERROR,INVERR                                                     
         B     LFMERR                                                           
*                                                                               
CKARB    CLI   SVAPROF,C'1'                                                     
         BE    EDTP10D                                                          
         CLI   SVAPROF,C'2'                                                     
         BE    EDTP10D                                                          
         B     PROFERR                                                          
*                                                                               
EDTP10D  MVC   CPROF,WORK                                                       
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BE    EDTP10F                                                          
         LA    R2,CLTEX1H                                                       
         CLI   WORK+15,C'0'        ELSE MUST BE 0                               
         BNE   EDTPERR             MUST BE 0                                    
         LA    R2,CLTEX2H                                                       
         CLI   WORK+16,C'0'                                                     
         BNE   EDTPERR             MUST BE 0                                    
*                                                                               
EDTP10F  DS    0H                                                               
         MVC   CEXTRA,WORK+15                                                   
         CLI   SVAPROF+12,C'Y'     SEE IF MGD IN MISSED MTH ALLOWED             
         BE    EDTP15                                                           
         LA    R2,CLTEX8H                                                       
         CLI   CEXTRA+7,C'Y'       NO - THEN DISALLOW AT CLIENT                 
         BE    EDTPERR                                                          
EDTP15   DS    0H                                                               
         LA    R2,CLTTTLEH                                                      
         XC    CTITLE,CTITLE                                                    
         CLI   5(R2),0                                                          
         BE    EDTP15D                                                          
         MVI   ERROR,INVERR                                                     
         CLI   CEXTRA+2,C'0'                                                    
         BE    LFMERR                                                           
         CLI   CEXTRA+2,C'N'                                                    
         BE    LFMERR                                                           
         MVC   CTITLE,CLTTTLE                                                   
         OC    CTITLE,SPACES                                                    
         B     EDTP15X                                                          
*                                                                               
EDTP15D  MVI   ERROR,MSSNGERR                                                   
         CLI   CEXTRA+2,C'0'                                                    
         BE    EDTP15X                                                          
         CLI   CEXTRA+2,C'N'                                                    
         BNE   LFMERR              Y OR * REQUIRE TITLE                         
EDTP15X  DS    0H                                                               
         B     OUTPUT                                                           
         EJECT                                                                  
NEXTEL   CLI   0(R6),0             END                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                 SAFETY CATCH                                 
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   ELCODE,0(R6)        RIGHT ELEMENT                                
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                  NOT FOUND EXIT                               
         SPACE 2                                                                
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BCR   8,RE                                                             
FNDNXUF  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'                END OF SCREEN                                
*                                                                               
OUTPUT   MVC   KEY,SVKEY                                                        
         ST    R8,AREC                                                          
         CLI   MODE,ACTADD                                                      
         BNE   OUT1                                                             
         MVC   REC(13),SVKEY                                                    
******                                                                          
         MVC   REC+13(2),=H'1000'                                               
******                                                                          
         GOTO1 ADDREC                                                           
         MVC   SVKEY,KEY                                                        
         GOTO1 CNADDSPT                                                         
         B     REQREC                                                           
*                                                                               
OUT1     DS    0H                                                               
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         GOTO1 GETREC              REREAD CLTREC BEFORE PUTREC                  
         ST    R8,AREC                                                          
         GOTO1 PUTREC                                                           
         GOTO1 CNCHASPT                                                         
         B     REQREC                                                           
*                        GENERATE REQUEST RECORD                                
REQREC   XC    REC(150),REC                                                     
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),SVEBCMED                                                 
         MVC   5(3,R1),SVEBCCLT                                                 
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'C'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   SVACT,C'A'                                                       
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                       
         B     EXIT                                                             
         EJECT                                                                  
******************************************************                          
* DATAMGR CALLS                                                                 
*                                                                               
DIR      NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,FILENAME,KEY,KEY                            
         B     DMCHECK                                                          
*                                                                               
DMCHECK  DS    0H                                                               
         TM    8(R1),X'FD'         TEST ALL BUT DELETED                         
         BZ    DMC2                                                             
         CLI   COMMAND+2,C'R'      TEST READ COMMAND                            
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT ON READ/WRITE ERROR               
DMC2     MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BZ    DMCX                                                             
         MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
DMCX     B     EXIT                                                             
*                                                                               
LFMERR   GOTO1 ERROR                                                            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
NOCLTACC EQU   178                 ACCESS TO CLIENT NOT AUTHORIZED              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                         TABLE OF AGENCIES THAT REQUIRE INTERFACE CDS          
AGYTAB   DC    C'JW'                                                            
         DC    C'JT'                                                            
         DC    C'AR'                                                            
         DC    C'NH'                                                            
         DC    C'HC'                                                            
         DC    C'CE'                                                            
*                                                                               
AGYTABN  EQU   (*-AGYTAB)/2                                                     
         SPACE 2                                                                
*                TABLE OF PROFILE VALUES                                        
*                        LENGTH OF ENTRY,POSITION,VALUES  FF =1-9 OR            
PROFTAB  DC    AL1(05),AL1(01),C'012'                           A-Z             
         DC    AL1(03),AL1(02),X'FF'                                            
         DC    AL1(06),AL1(03),C'0123'                                          
         DC    AL1(04),AL1(04),C'01'                                            
         DC    AL1(09),AL1(05),C'0123459'                                       
         DC    AL1(05),AL1(06),C'012'                                           
         DC    AL1(08),AL1(07),C'0123YN'   Y IS ONLY REAL VALUE                 
         DC    AL1(13),AL1(08),C'0123456789*'                                   
         DC    AL1(04),AL1(09),C'01'                                            
         DC    AL1(05),AL1(10),C'012'                                           
         DC    AL1(05),AL1(11),C'012'                                           
         DC    AL1(06),AL1(12),C'01YN'     Y IS ONLY REAL VALUE                 
         DC    AL1(05),AL1(13),C'0YN'                                           
         DC    AL1(06),AL1(14),C'0123'                                          
         DC    AL1(13),AL1(15),C'0123456789*'                                   
*        THE FOLLOWING VALUES GO INTO CEXTRA                                    
         DC    AL1(05),AL1(16),C'0UC'                                           
         DC    AL1(04),AL1(17),C'01'                                            
         DC    AL1(16),AL1(18),C'ABCDEFGHIJKNY*'                                
         DC    AL1(04),AL1(19),C'NY'                                            
         DC    AL1(04),AL1(20),C'0E'                                            
         DC    AL1(05),AL1(21),C'NYD'                                           
         DC    AL1(04),AL1(22),C'NY'                                            
         DC    AL1(04),AL1(23),C'NY'                                            
         DC    AL1(04),AL1(24),C'NY'                                            
         DC    AL1(04),AL1(25),C'NY'                                            
         DC    X'0000'                                                          
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE NESFMWORKD                                                     
         ORG     SYSSPARE                                                       
*                                                                               
WORKAREA DS    0CL500                ******* T31C WORK AREA *****               
RELO     DS    F                                                                
FILENAME DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMFCD                                                       
         EJECT                                                                  
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*                                                                               
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012NESFMCLT  05/01/02'                                      
         END                                                                    
