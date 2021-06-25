*          DATA SET PPJOB02    AT LEVEL 030 AS OF 06/17/08                      
*PHASE T40F02B                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPJOB02 - PRINTPAK JOB FILE - INSTRUCTION RECORD'               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE  4/05    ALLOW AD-ID ALONE OR JOBCODE ALONE OR BOTH TOGETHER             
*                 AS "KEY" ENTRIES                                              
*                                                                               
* SMYE 4/23/02  PUBVAL AND PUBEDIT AS CORE-RESIDENT CALLS                       
*                                                                               
* KWAN 09/21/01 REORGANIZED PPJOBWRK                                            
*                                                                               
* SMYE 4/15/98  COSMETIC FIXES TO INS SCREEN PUB LISTING (DSPUBN)               
*                                                                               
* BOBY 6/14/94  WORD PROCESSING PFKEYS ADDED                                    
*                                                                               
* BPLA 8/23/91  PUB NAME SEARCHING ADDED                                        
*                                                                               
* BPLA 9/27/91 DON'T DIE IN CHA IF RECORD NOT FOUND-JUST SEND MESSAGE           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40F02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T40F02,R9                                                      
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T40FFFD,RA                                                       
         RELOC RELO02                                                           
*                                                                               
         CLI   SCRNTYP,2           TEST HAVE INST SCREEN                        
         BE    JFM2                YES                                          
*                                  NO - FETCH SCREEN                            
         GOTO1 VCALLOV,DMCB,JOBLAST,X'D9040FFD'                                 
*                                                                               
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNTYP,2                                                        
         MVI   CHGSW,0                                                          
         MVI   NEWSCRN,C'N'                                                     
         MVI   JOBPUBH+5,0                                                      
*                                                                               
         CLI   RMDATSW,C'Y'            ARE THERE MORE PUBS TO DISPLAY.          
         BNE   JFM2                                                             
         BAS   RE,DSPUBN               DISPLAY REMAINING PUBS.                  
         LA    R2,JOBACTH              SET CURSOR.                              
         CLI   RMDATSW,C'Y'            STILL MORE TO DISPLAY.                   
         BNE   JOBEXIT                                                          
         LA    R3,JOBMSGH              POINT TO SERVICE                         
         ZIC   R0,0(R3)                REQUEST                                  
         AR    R3,R0                   FIELD.                                   
         OI    6(R3),X'81'             CHANGE TO MODIFIED & TRANSMIT.           
         B     JOBEXIT                                                          
*                                                                               
JFM2     DS    0H                     VALIDATE PUB                              
         LA    R2,JOBPUBH                                                       
         TM    4(R2),X'20'                                                      
         BO    JFM4C                                                            
         MVI   RMDATSW,C'N'       PUB CHANGED MUST CLEAR                        
**                                                                              
**       PUB NAME SEARCHING                                                     
**                                                                              
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,SVMED                                                   
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO02                       
         DROP  R3                                                               
         LA    R2,JOBPUBH                                                       
*                                                                               
         CLI   5(R2),0                                                          
         BNE   JFM2B                                                            
         MVC   JOBPUB(3),=C'ALL'                                                
         FOUT  JOBPUBH                                                          
*                                                                               
JFM2B    MVC   SVPUB,=6X'FF'                                                    
         CLC   =C'ALL',JOBPUB                                                   
         BE    JFM4                                                             
*                                                                               
         CLI   JOBPUB,C'?'                                                      
         BNE   JFM2B1                                                           
         OI    4(R2),X'20'             VALIDATE PUB                             
         MVI   CHGSW,0                 DISALLOW CHANGES                         
*                                                                               
JFM2BX   DS    0H                      SHOULD GET HERE IF PUB ?                 
*                                      PREVIOUSLY VALIDATED                     
         BAS   RE,DSPUBN               DISPLAY PUBS FOR THAT ADCODE.            
         LA    R2,JOBACTH              SET CURSOR.                              
         CLI   RMDATSW,C'Y'            ARE THERE MORE PUBS TO DISPLAY.          
         BNE   JOBEXIT                                                          
         LA    R3,JOBMSGH              POINT TO SERVICE                         
         ZIC   R0,0(R3)                REQUEST                                  
         AR    R3,R0                   FIELD.                                   
         OI    6(R3),X'81'             CHANGE TO MODIFIED & TRANSMIT.           
         B     JOBEXIT                                                          
*                                                                               
JFM2B1   IC    R0,5(R2)                                                         
*NOP*    GOTO1 =V(PUBVAL),DMCB,((R0),JOBPUB),SVPUB,RR=RELO02                    
         GOTO1 VPUBVAL,DMCB,((R0),JOBPUB),SVPUB                                 
*                                                                               
         CLI   DMCB,X'FF'                                                       
         BNE   JFM2D                                                            
         LA    R3,PUBERR1                                                       
         B     ERROR                                                            
*                                                                               
JFM2D    DS    0H                                                               
         LA    R3,PUBERR2                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(1),SVMED                                                     
         MVC   KEY+1(6),SVPUB                                                   
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         B     *+8                                                              
JFM2F    DS    0H                                                               
         BAS   RE,SEQPUB                                                        
         CLC   KEY(7),KEYSAVE                                                   
         BNE   ERROR                                                            
         CLC   KEY+7(2),AGYALPHA                                                
         BE    JFM2H                                                            
         CLI   SVAGPROF+16,C'0'    TEST SRDS DEFAULT                            
         BE    ERROR               NO                                           
         CLC   KEY+7(2),=C'ZZ'                                                  
         BNE   JFM2F                                                            
*                                                                               
JFM2H    DS    0H                                                               
         CLI   KEY+9,X'81'                                                      
         BNE   JFM2F                                                            
*                                                                               
         BAS   RE,GETPUB                                                        
         MVC   JOBPUBN,PUBNAME                                                  
         MVC   JOBPUBZ,PUBZNAME                                                 
         FOUT  JOBPUBNH                                                         
         FOUT  JOBPUBZH                                                         
*                                                                               
         CLI   ADBSW,0             ADBUYER UPLOAD?                              
         BNE   *+8                                                              
         MVI   CHGSW,0             DISALLOW CHANGE                              
         OI    4(R2),X'20'                                                      
         B     JFM4C                                                            
*                                                                               
JFM4     DS    0H                   "ALL" PUB INQUIRES                          
         MVC   JOBPUBN,SPACES       CLEAR PUB NAME AND ZONE FOR                 
         MVC   JOBPUBZ,SPACES                                                   
         FOUT  JOBPUBNH                                                         
         FOUT  JOBPUBZH                                                         
         B     JFM4D                                                            
*                                                                               
JFM4C    DS    0H                                                               
         CLI   JOBPUB,C'?'         PUB INQUIRY                                  
         BE    JFM2BX                                                           
*                                                                               
JFM4D    DS    0H                                                               
         CLC   =C'ADD',JOBACT                                                   
         BE    ADDR                                                             
         CLC   =C'CHA',JOBACT                                                   
         BE    CHA                                                              
         CLC   =C'DIS',JOBACT                                                   
         BE    DIS                                                              
         CLC   =C'INS',JOBACT                                                   
         BE    DIS                                                              
         CLC   =C'DEL',JOBACT                                                   
         BE    CHA                                                              
JFM5     DS    0H                                                               
         LA    R3,ACTERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
         EJECT                                                                  
*                                  ADD ROUTINES                                 
ADDR     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PRD/JOB                              
         MVI   KEY+3,X'15'                                                      
         LA    R2,JOBJOBH                                                       
         CLI   5(R2),0             AD-ID ONLY ?                                 
         BNE   ADDR4               NO - HAVE JOB CODE                           
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),SVADID   AD-ID                                        
         LA    R2,JOBADIDH                                                      
ADDR4    BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ADD2                                                             
         LA    R3,RECERR                                                        
         B     ERROR                                                            
*                                                                               
ADD2     DS    0H                                                               
         XC    ADIDJOB,ADIDJOB     CLEAR                                        
         CLI   JOBJOBH+5,0         AD-ID ONLY ?                                 
         BNE   ADD2G               NO - HAVE JOB CODE                           
         BAS   RE,GETREC           NEED RECORD FOR AD-ID ONLY JOB CODE          
         MVC   ADIDJOB,PJOBKJOB    SAVE "SPECIAL" JOB CODE                      
         MVC   KEY+2(14),PJOBKMED  RESET KEY                                    
ADD2G    MVC   KEY+16(6),SVPUB                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADD4                                                             
         TM    KEY+25,X'80'                                                     
         BZ    ADD3                                                             
         BAS   RE,GETREC                                                        
         B     ADD4                                                             
ADD3     DS    0H                                                               
         LA    R3,ADDERR                                                        
         LA    R2,JOBPUBH                                                       
         B     ERROR                                                            
*                                                                               
ADD4     DS    0H                                                               
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
*                                                                               
         CLI   SVPUB,X'FF'                                                      
         BE    ADD5                                                             
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+16(6),=6X'FF'                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    ADD5                                                             
         LA    R3,NOINSERR                                                      
         LA    R2,JOBPUBH                                                       
         B     ERROR                                                            
*                                                                               
ADD5     DS    0H                                                               
         CLI   NEWSCRN,C'N'                                                     
         BNE   ADD6                                                             
*                                                                               
         LA    R2,JOBPUBH                                                       
ADD5A    DS    0H                                                               
         MVC   JOBMSG(L'EINMSG),EINMSG                                          
         FOUT  JOBMSGH                                                          
         B     JOBEXIT                                                          
*                                                                               
ADD6     DS    0H                                                               
         LA    RE,PJOBREC                                                       
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         MVC   PJOBKAGY,AGYALPHA                                                
         MVC   PJOBKMED(20),SVMED                                               
         MVI   PJOBKRCD,X'15'                                                   
         MVC   PJOBRLEN,=H'53'                                                  
         MVC   PJOBELEM(2),=X'0114'     CONTROL ELEM                            
         MVC   PJOBELEM+2(3),BTODAY                                             
         MVI   PJOBELEM+5,C'N'          NEW                                     
*                                                                               
         CLI   JOBJOBH+5,0              AD-ID ONLY ?                            
         BNE   ADD6E                    NO - HAVE JOB CODE                      
         MVC   PJOBKJOB,ADIDJOB         "AD-ID ONLY" JOB CODE                   
*                                                                               
ADD6E    DS    0H                                                               
         BAS   RE,INEDIT                                                        
         OI    DMINBTS,X'08'                                                    
         MVC   KEY(25),PJOBKEY                                                  
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ADD7                                                             
         NI    KEY+25,X'3F'                                                     
         BAS   RE,WRITE                                                         
         BAS   RE,PUTREC                                                        
         B     ADD8                                                             
*                                                                               
ADD7     DS    0H                                                               
         BAS   RE,ADDREC                                                        
ADD8     DS    0H                                                               
         MVI   CHGSW,1                                                          
         MVC   JOBMSG(L'IRAMSG),IRAMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBMEDH                                                       
         B     JOBEXIT                                                          
         EJECT                                                                  
*                                  CHANGE ROUTINES                              
CHA      DS    0H                                                               
         CLI   CHGSW,1             IF NOT DISPLAYED FOR CHANGE                  
         BE    CHA2                                                             
         B     DIS                    DISPLAY RECORD INSTEAD                    
*                                                                               
CHAERR   LA    R3,CHGERR                                                        
         LA    R2,JOBACTH                                                       
         B     ERROR                                                            
*                                                                               
CHA2     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(20),SVMED     M/C/CLT/PRD/JOB/PUB                          
         MVI   KEY+3,X'15'                                                      
*                                                                               
         OC    KEY+10(6),KEY+10    JOB CODE SET?                                
         BNZ   CHA2_H                                                           
         OC    ADIDJOB,ADIDJOB     HAVE X'FF' AD CODE?                          
         BZ    *+14                                                             
         MVC   KEY+10(6),ADIDJOB                                                
         B     CHA2_H                                                           
         MVI   KEY+03,PADIKRCQ                                                  
         MVC   KEY+10(12),SVADID                                                
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+16                                                             
         LA    R2,JOBADIDH                                                      
         LA    R3,RECERR                                                        
         B     ERROR                                                            
         BRAS  RE,GETREC                                                        
         MVC   ADIDJOB,PJOBKJOB    SET 'XFF' AD CODE                            
         B     CHA2                                                             
*                                                                               
CHA2_H   BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   CHAERR                                                           
         BAS   RE,GETREC                                                        
         BAS   RE,INEDIT                                                        
         BAS   RE,PUTREC                                                        
         CLC   =C'DEL',JOBACT                                                   
         BNE   CHA4                                                             
         CLI   PJOBKPUB,X'FF'      DEL NOT OK FOR ALL OUB REC                   
         BE    JFM5                                                             
         MVC   KEY(25),PJOBKEY                                                  
         BAS   RE,READ                                                          
         OI    KEY+25,X'80'                                                     
         BAS   RE,WRITE                                                         
         MVC   JOBMSG(L'IRDLMSG),IRDLMSG                                        
         B     CHA5                                                             
CHA4     DS    0H                                                               
*                                                                               
         MVC   JOBMSG(L'IRCMSG),IRCMSG                                          
CHA5     DS    0H                                                               
         FOUT  JOBMSGH                                                          
*                                                                               
         LA    R2,JOBMEDH                                                       
         B     JOBEXIT                                                          
         EJECT                                                                  
*                                  DISPLAY ROUTINES                             
DIS      DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(14),SVMED     M/C/CLT/PROD/JOB                             
         MVI   KEY+3,X'15'                                                      
         LA    R2,JOBJOBH                                                       
         CLI   5(R2),0             AD-ID ONLY ?                                 
         BNE   DISR                NO - HAVE JOB CODE                           
         MVI   KEY+3,PADIKRCQ      'C1' PASSIVE KEY CODE                        
         MVC   KEY+10(12),SVADID   AD-ID                                        
         LA    R2,JOBADIDH                                                      
DISR     BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    DIS2                                                             
         LA    R3,RECERR                                                        
         B     ERROR                                                            
*                                                                               
DIS2     DS    0H                                                               
         BAS   RE,GETREC           NEED RECORD FOR AD-ID HANDLING               
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BE    DIS2B               YES                                          
         MVC   SVJOB,PJOBKJOB      NO - "REGULAR" JOB CODE                      
DIS2B    OC    PJOBKPUB,PJOBKPUB   HEADER RECORD ?                              
         BNZ   DIS2D               NO                                           
         MVC   ADIDID,PJOBADID     SAVE AD-ID                                   
DIS2D    DS    0H                                                               
         XC    ADIDJOB,ADIDJOB     CLEAR                                        
         CLI   JOBJOBH+5,0         AD-ID ONLY ?                                 
         BNE   DIS2G               NO - HAVE JOB CODE                           
         MVC   ADIDJOB,PJOBKJOB    SAVE "SPECIAL" JOB CODE                      
         MVC   KEY+2(14),PJOBKMED  RESET KEY                                    
*                                                                               
DIS2G    DS    0H                                                               
         CLI   PJOBKJOB,X'FF'      AD-ID ONLY ?                                 
         BE    DIS2P               YES                                          
         MVC   JOBJOB,PJOBKJOB     NO - "REGULAR" JOB CODE                      
         FOUT  JOBJOBH                                                          
DIS2P    MVC   KEY+16(6),SVPUB                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         BAS   RE,HIGH                                                          
         NI    DMINBTS,X'F7'       NO DELETES                                   
         CLC   KEY(25),KEYSAVE                                                  
         BE    DIS3                                                             
         CLC   =C'DIS',JOBACT      ERROR IF DISPLAYING                          
         BE    *+10                                                             
         CLC   =C'CHA',JOBACT      OR DISPLAYING FOR CHANGE                     
         BNE   DIS6                                                             
*                                                                               
         LA    R2,JOBPUBH                                                       
         LA    R3,RECERR                                                        
         B     ERROR                                                            
*                                                                               
DIS3     DS    0H                                                               
         TM    KEY+25,X'C0'                                                     
         BZ    DIS4                                                             
         LA    R3,DELERR                                                        
         LA    R2,JOBPUBH                                                       
         B     ERROR                                                            
*                                                                               
DIS4     DS    0H                                                               
         MVC   JOBADID,ADIDID                                                   
         FOUT  JOBADIDH                                                         
         BAS   RE,GETREC                                                        
         BAS   RE,INFMT                                                         
         MVI   CHGSW,1             SET TO ALLOW CHANGE NEXT TIME                
*                                                                               
         MVC   JOBMSG(L'IRDMSG),IRDMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBACTH                                                       
*****    XC    JOBACT,JOBACT                                                    
*****    FOUT  JOBACTH                                                          
         B     JOBEXIT                                                          
*                                                                               
DIS6     DS    0H                  'INS' INPUT AND NO INSTRUCTS EXIST           
         XC    JOBACT,JOBACT       SWITCH TO 'ADD'                              
         MVC   JOBACT(3),=C'ADD'                                                
         FOUT  JOBACTH                                                          
*                                                                               
         LA    R2,JOBINS1H       CURSOR TO FIRST INS LINE                       
         B     ADD5A                                                            
*                                                                               
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'IRDMSG),IRDMSG                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBACTH                                                       
         XC    JOBACT,JOBACT                                                    
         FOUT  JOBACTH                                                          
         B     JOBEXIT                                                          
*                                                                               
JOBEXIT  DS    0H                  EXIT FROM MODULE                             
*                                                                               
         LA    R3,JOBMSGH              POINT TO SERVICE                         
         ZIC   R0,0(R3)                REQUEST                                  
         AR    R3,R0                   FIELD.                                   
         OI    6(R3),X'81'             CHANGE TO MODIFIED & TRANSMIT.           
*                                                                               
*        IF PFKEY HIT THEN CURSOR REMAINS WHERE IT WAS                          
*                                                                               
         ICM   RF,15,ATIOB         POINT TO TIOB                                
*                                                                               
         CLI   TIOBAID-TIOBD(RF),0 SKIP IF ENTER HIT                            
         BE    *+12                                                             
         OI    TIOBINDS-TIOBD(RF),TIOBSETC LEAVES CURSOR WHERE IT WAS           
         B     *+8                                                              
         OI    6(R2),OI1C .        INSERT CURSOR                                
*                                                                               
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                  EDIT ROUTINES                                
INEDIT   DS    0H                                                               
         ST    RE,SAVRE                                                         
*                                                                               
*        FIND NUMBER OF LAST LINE SCREEN WITH DATA                              
*                                                                               
         LR    R5,R2               SAVE R2                                      
*                                                                               
         LA    R2,JOBINS1H         POINT TO FIRST COMMENT LINE                  
         MVI   LASTLIN,0           INIT LINE NUMBER SAVEAREA                    
         SR    R6,R6                                                            
*                                                                               
INEDLP   DS    0H                                                               
*                                                                               
         AH    R6,=H'1'            BUMP LINE COUNTER                            
*                                                                               
         CLI   5(R2),0             IF LINE HAS DATA                             
         BE    INEDCN                                                           
*                                                                               
         OC    8(L'JOBINS1,R2),SPACES MUST BE GREATER THAN SPACES               
         CLC   8(L'JOBINS1,R2),SPACES                                           
         BNH   INEDCN                                                           
*                                                                               
         STC   R6,LASTLIN             UPDATE LAST LINE NUMBER                   
*                                                                               
INEDCN   DS    0H                                                               
*                                                                               
         BAS   RE,BMPFLD           BUMP TO NEXT COMMENT                         
         BNE   INEDLP                                                           
*                                  END OF SCREEN                                
         LR    R2,R5               RESTORE R2                                   
*                                                                               
         ICM   R5,15,ATIOB         POINT TO TIOB                                
         USING TIOBD,R5            ESTABLISH TIOB                               
*                                                                               
         CLI   TIOBAID,0           IF PFKEY ENTERED                             
         BE    *+12                                                             
         BAS   RE,PFKEYS              GO ANALYZE                                
         BNE   ERROR                  ERROR FOUND                               
*                                                                               
         DROP  R5                                                               
*                                                                               
*                                  DELETE ALL '66' ELEMS FROM RECORD            
         LA    R4,PJOBREC+33                                                    
INE2     DS    0H                                                               
         CLI   0(R4),X'66'                                                      
         BE    INE4                                                             
         CLI   0(R4),0             EOR                                          
         BE    INE6                                                             
*                                                                               
INE2A    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     INE2                                                             
*                                                                               
INE4     DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PJOBREC),(R4)                                     
*                                                                               
         B     INE2                                                             
*                                                                               
INE6     DS    0H                                                               
         LA    R2,JOBINS1H                                                      
         MVI   CURLIN,0            INIT CURRENT LINE SAVEAREA                   
*                                                                               
INE7     DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         IC    R6,CURLIN                                                        
         LA    R6,1(R6)            BUMP LINE COUNTER                            
         STC   R6,CURLIN                                                        
*                                                                               
         CLI   5(R2),0                                                          
         BE    INE7A                                                            
*                                                                               
         OC    8(L'JOBINS1,R2),SPACES                                           
         CLC   8(L'JOBINS1,R2),SPACES                                           
         BH    INE7B                                                            
*                                                                               
INE7A    DS    0H                  NO DATA ON LINE                              
*                                                                               
         CLC   CURLIN,LASTLIN      IF MORE LINES WITH DATA TO FOLLOW            
         BNH   INE7B                  TREAT AS A LINE WITH DATA                 
*                                                                               
         B     INE50               ELSE IGNORE LINE                             
*                                                                               
INE7B    DS    0H                                                               
*                                                                               
         XC    WORKA,WORKA         INIT ELEMENT BUILD AREA                      
*                                  GET LENGTH                                   
         LA    R5,L'JOBINS1-1+8(R2)  POINT TO LAST BYTE OF LINE                 
         LA    RF,L'JOBINS1        COMMENT LENGTH                               
*                                                                               
         CLI   0(R5),C' '          FIND END OF COMMENT                          
         BH    INE7C                                                            
         BCTR  R5,0                BACK UP A POSITION                           
         BCT   RF,*-10                                                          
*                                  NO DATA ON LINE                              
         LA    RF,1                FORCE AT LEAST 1 BYTE OF COMMENT             
         MVI   8(R2),0                THAT IS FORCED TO NULLS                   
*                                                                               
INE7C    DS    0H                                                               
*                                                                               
         MVI   WORKA,X'66'         SET INSTRUCTION ELEMENT ID                   
*                                                                               
         LA    R5,2(RF)            ADD ID AND LENGTH TO COMMENT LENGTH          
         STC   R5,WORKA+1          SET COMMENT ELEMENT LENGTH                   
*                                                                               
         SH    R5,=H'3'            DECREMENT FOR EXECUTE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORKA+2(0),8(R2)    MOVE INSTRUCTION TO ELEMENT                  
*                                                                               
         CLC   WORKA+2(13),=C'&&&&INSERTION&&&&' TRANSLATE TO //'S              
         BNE   *+16                SINCE TELEX TERMS CANNOT HANDLE //'S         
         MVC   WORKA+2(2),=C'//'   CHANGE TO //'S IN RECORD                     
         MVC   WORKA+13(2),=C'//'                                               
*                                  TEST RECORD LENGTH                           
         MVC   HALF,PJOBRLEN                                                    
         LH    RE,HALF                                                          
         LA    RE,3(R5,RE)                                                      
         CHI   RE,975                                                           
         BL    INE8                                                             
         LA    R3,RLNERR                                                        
         B     ERROR                                                            
*                                                                               
INE8     DS    0H                                                               
         GOTO1 VRECUP,DMCB,(1,PJOBREC),WORKA,(R4)                               
*                                                                               
         LA    R4,3(R5,R4)         BUMP PLACE IN RECORD                         
*                                                                               
*                                  CHECK FOR STAND COMMENTS                     
*                                                                               
         LA    R6,8(R2)                                                         
         LA    R5,8+L'JOBINS1(R2)  EOL                                          
INE10    DS    0H                                                               
         CLC   0(4,R6),=C'COM='                                                 
         BNE   INE20                                                            
         LA    R6,4(R6)                                                         
         LR    R7,R6               SAVE START                                   
INE12    DS    0H                                                               
         CR    R6,R5                                                            
         BNL   INE14                                                            
         CLI   0(R6),C','                                                       
         BE    INE14                                                            
         CLC   0(2,R6),SPACES                                                   
         BE    INE14                                                            
         LA    R6,1(R6)                                                         
         B     INE12                                                            
*                                                                               
INE14    DS    0H                                                               
         SR    R6,R7               R6 = LENGTH                                  
         BNP   INE24                                                            
         CH    R6,=H'6'                                                         
         BH    INE24                                                            
         MVC   WORK,SPACES                                                      
         LA    R8,WORK+6                                                        
         SR    R8,R6                                                            
         BCTR  R6,R0                                                            
         EX    R6,*+8              MOVE TO WORK+6-LENGTH                        
         B     *+10                                                             
         MVC   0(0,R8),0(R7)                                                    
*                                  LOOK FOR RECORD                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    INE16                                                            
*                                                                               
         MVI   ERRAREA,X'FF'                                                    
         MVC   JOBMSG(L'COMERM),COMERM                                          
         MVC   JOBMSG+L'COMERM(6),WORK                                          
         FOUT  JOBMSGH                                                          
         B     JOBEXIT                                                          
INE16    DS    0H                                                               
         LA    R6,2(R6,R7)         POINT TO NEXT 'COM='                         
         B     INE10                                                            
INE20    DS    0H                                                               
         LA    R0,8(R2)                                                         
         CR    R6,R0               IF NO 'COM=' AT START OF LINE                
         BE    INE26               TREAT AS FREE FORM                           
         LA    R7,7+L'JOBINS1(R2)                                               
         CLI   0(R7),C' '          REST OF LINE MUST BE SPACES                  
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         CR    R7,R6                                                            
         BNL   INE24                                                            
         B     INE50                                                            
INE24    DS    0H                                                               
         LA    R3,COMERR                                                        
         B     ERROR                                                            
*                                                                               
INE26    DS    0H                                                               
         CLC   0(4,R6),=C'COM='    IMBEDDED 'COM=' IS ERROR                     
         BE    INE24                                                            
         LA    R6,1(R6)                                                         
         CR    R6,R5               CHECK EOL                                    
         BNL   INE30                                                            
         B     INE26                                                            
*                                  CHECK FOR // KEYWORDS                        
*                                                                               
INE30    DS    0H                                                               
         LA    R5,8(R2)                                                         
         LA    R6,1                                                             
         LA    R7,8+L'JOBINS1-1(R2)                                             
INE31    DS    0H                                                               
         CLC   0(2,R5),KDELIM                                                   
         BE    INE32                                                            
         BXLE  R5,R6,*-10                                                       
         B     INE48               END OF LINE                                  
*                                                                               
INE32    DS    0H                                                               
         LA    R5,2(R5)                                                         
         LR    R8,R5                                                            
         CLC   0(2,R5),KDELIM                                                   
         BE    INE34                                                            
         BXLE  R5,R6,*-10                                                       
INE32B   LA    R3,KWDERR                                                        
         B     ERROR                                                            
*                                                                               
INE34    DS    0H                                                               
         SR    R5,R8               R5 = LENGTH                                  
         LA    RE,KWRDLST                                                       
INE34B   SR    RF,RF                                                            
         CLI   0(RE),X'FF'                                                      
         BE    INE32B                                                           
         IC    RF,0(RE)                                                         
         CR    R5,RF                                                            
         BE    INE36                                                            
INE34D   LA    RE,1(RE,RF)                                                      
         B     INE34B                                                           
INE36    DS    0H                                                               
         BCTR  R5,R0                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R8),1(RE)                                                    
         BE    INE38                                                            
         LA    R5,1(R5)                                                         
         B     INE34D                                                           
*                                                                               
INE38    DS    0H                                                               
         LA    R5,3(R5,R8)                                                      
         BE    INE31                                                            
*                                                                               
INE48    DS    0H                                                               
*                                                                               
INE50    DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),L'JOBINS1+8                                                
         BE    INE7                                                             
INEEXT   DS    0H                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
*                                                                               
KWRDLST  DS    0C                                                               
         DC    AL1(09),C'INSERTION'                                             
         DC    AL1(255)                                                         
*                                                                               
KDELIM   DC    C'&&&&'                                                          
         EJECT                                                                  
*                                  FORMAT ROUTINES                              
INFMT    DS    0H                                                               
         ST    RE,SAVRE                                                         
*                                                                               
         LA    R4,PJOBREC+33                                                    
         LA    R2,JOBINS1H                                                      
*                                                                               
INF2     DS    0H                                                               
         CLI   0(R4),X'66'                                                      
         BE    INF3                                                             
INF2A    DS    0H                                                               
         XC    WORKA,WORKA                                                      
         CLI   0(R4),0             EOR                                          
         BE    INF4                                                             
         SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0               NEXT ELEM                                    
         B     INF2                                                             
*                                                                               
INF3     DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORKA(0),2(R4)                                                   
         CLC   WORKA(13),=C'//INSERTION//' CHK FOR KEYWORK                      
         BNE   *+16                                                             
         MVC   WORKA(2),=C'&&&&'     DISPLAY AS &&'S                            
         MVC   WORKA+11(2),=C'&&&&'                                             
INF4     DS    0H                                                               
         CLC   8(L'JOBINS1,R2),WORKA                                            
         BE    INF6                                                             
         MVC   8(L'JOBINS1,R2),WORKA                                            
         FOUT  (R2)                                                             
*                                                                               
INF6     DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),L'JOBINS1+8   TEST END OF COMM LINES                       
         BE    INF2A                                                            
*                                                                               
INFEXT   DS    0H                                                               
         L     RE,SAVRE                                                         
         BR    RE                                                               
         SPACE 3                                                                
EINMSG   DC    C'ENTER INSTRUCTION DATA'                                        
IRAMSG   DC    C'INSTRUCTION DATA ADDED'                                        
IRCMSG   DC    C'INSTRUCTION DATA CHANGED'                                      
IRDMSG   DC    C'INSTRUCTION DATA DISPLAYED'                                    
IRDLMSG  DC    C'INSTRUCTION DATA DELETED'                                      
INQMSG   DC    C'INSTRUCTION INQUIRY '                                          
COMERM   DC    C'RECORD NOT FOUND, COMMENT NUMBER'                              
REMDATA  DC    C'HIT ENTER TO VIEW REMAINING DATA'                              
PUBDATDS DC    C'PUBLICATION DATA DISPLAYED'                                    
         EJECT                                                                  
*                                                                               
*                                  ROUTINE TO GET NEXT INPUT FIELD              
BMPFLD   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BER   RE                                                               
         CLI   0(R2),0                                                          
         BR    RE                                                               
         SPACE 2                                                                
DSPUBN   NTR1                                                                   
         LA    R2,JOBPUBH                                                       
         MVC   8(17,R2),SPACES                                                  
         MVI   8(R2),C'?'                                                       
         OI    6(R2),X'80'                                                      
         LA    R6,16                        CLEAR                               
         LA    R2,JOBINS1H                  ALL                                 
DSPUBN1  MVC   8(65,R2),SPACES              16                                  
         OI    6(R2),X'80'                  COMMENT                             
         ZIC   R0,0(R2)                     LINES.                              
         AR    R2,R0                                                            
         BCT   R6,DSPUBN1                                                       
         ZAP   LINCT,=P'0'                  CLEAR LINE COUNT.                   
*                                                                               
         CLI   RMDATSW,C'Y'                 IS THIS SECOND TIME THRU.           
         BNE   DSPUBN3                                                          
         MVC   KEY,SAVEKEY                  IF YES RESET                        
         GOTO1 HIGH                         SEQ READ POINTER.                   
         LA    R2,JOBINS1H                                                      
         MVI   RMDATSW,C'N'                 RESET SWITCH.                       
         B     DSPUBN8                                                          
*                                                                               
DSPUBN3  LA    R2,JOBPUBNH                  SHOW 'INSTRUCTION INQUIRY'          
         XC    JOBPUBN,JOBPUBN              CLEAR PUB NAME                      
         MVC   8(L'INQMSG,R2),INQMSG        NEXT TO PUBLICATION FIELD.          
         OI    6(R2),X'80'                                                      
         LA    R2,JOBPUBZH                  CLEAR ZONE.                         
         MVC   8(20,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
*                                                                               
DSPUBN5  XC    KEY,KEY                      CLEAR KEY AND KEYSAVE               
         XC    KEYSAVE,KEYSAVE              AREAS.                              
*                                                                               
         MVC   KEY(2),AGYALPHA              BUILD KEY TO READ PRTDIR.           
         MVC   KEY+2(1),JOBMED                                                  
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),JOBCLT                                                  
         MVC   KEY+7(3),JOBPRD                                                  
         MVC   KEY+10(6),JOBJOB                                                 
         OC    KEY+4(12),SPACES                                                 
         MVC   SAVEKEY,KEY                                                      
         LA    R2,JOBINS1H                                                      
DSPUBN6  GOTO1 HIGH                                                             
         B     DSPUBN9                                                          
DSPUBN8  GOTO1 SEQ                                                              
         MVC   SAVEKEY,KEY                                                      
DSPUBN9  CLC   KEYSAVE(16),KEY              IF KEY CHANGES EXIT.                
         BNE   DSPUBN20                                                         
         CLC   KEY+16(6),=6X'00'            IS IT JOB HEADER RECORD.            
         BE    DSPUBN8                                                          
         CLI   KEY+16,X'FF'                 IS IT "ALL" RECORD.                 
         BNE   DSPUBN12                                                         
         MVC   8(3,R2),=C'ALL'              FOR ALL PUBLICATIONS.               
         OI    6(R2),X'80'                                                      
         AP    LINCT,=P'1'                                                      
         B     DSPUBN18                                                         
DSPUBN12 ZIC   R5,SVAGPROF+12               PUB EDIT SPECIFICATION.             
*NOP*    GOTO1 =V(PUBEDIT),DMCB,((R5),KEY+16(6)),(0,8(R2)),RR=RELO02            
         GOTO1 VPUBEDIT,DMCB,((R5),KEY+16(6)),(0,8(R2))                         
         XC    KEY,KEY                                                          
         MVC   KEY(1),SAVEKEY+2             BUILD KEY TO READ PUBDIR            
         MVC   KEY+1(6),SAVEKEY+16          FOR PUB NAME AND ZONE.              
         MVC   KEY+7(2),AGYALPHA                                                
         MVI   KEY+9,X'81'                                                      
         BAS   RE,HIGHPUB                                                       
         B     DSPUBN15                                                         
DSPUBN14 BAS   RE,SEQPUB                                                        
DSPUBN15 CLC   KEY(7),KEYSAVE                                                   
         BNE   DSPUBN17                                                         
         CLC   KEY+7(2),AGYALPHA                                                
         BE    DSPUBN16                                                         
         CLI   SVAGPROF+16,C'0'             TEST SRDS DEFAULT                   
         BE    DSPUBN17                     NO                                  
         CLC   KEY+7(2),=C'ZZ'                                                  
         BNE   DSPUBN14                                                         
*                                                                               
DSPUBN16 CLI   KEY+9,X'81'                                                      
         BNE   DSPUBN14                                                         
         BAS   RE,GETPUB                                                        
         MVC   27(20,R2),PUBNAME            PUT OUT PUB NAME                    
         MVC   49(20,R2),PUBZNAME           AND ZONE.                           
         OI    6(R2),X'80'                                                      
         AP    LINCT,=P'1'                                                      
DSPUBN17 MVC   KEY,SAVEKEY                  POINT READ BACK TO LAST             
         GOTO1 HIGH                         PUB READ IN PUBDIR.                 
DSPUBN18 ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CP    LINCT,=P'16'                 IF LINECOUNT > 16 CAN'T             
         BL    DSPUBN8                      FIT ANYMORE ON SCREEN.              
*                                                                               
         LA    R2,JOBMSGH                   IF SCREEN FULL PUT OUT              
         MVC   8(L'REMDATA,R2),REMDATA      MESSAGE TO DISPLAY                  
         OI    6(R2),X'80'                  REMAINING DATA.                     
         MVI   RMDATSW,C'Y'                 SET SWITCH TO YES.                  
         XIT                                                                    
DSPUBN20 XC    JOBMSG,JOBMSG                    WHEN ALL DATA DISPLAYED         
         MVC   JOBMSG(L'PUBDATDS),PUBDATDS      PUT OUT MESSAGE                 
         FOUT  JOBMSGH                                                          
         LA    R2,JOBACTH                                                       
         XC    JOBACT,JOBACT                    CLEAR ACTION FIELD.             
         OI    6(R2),X'80'                                                      
         MVI   RMDATSW,C'N'                     SET SWITCH TO NO.               
         XIT                                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - PFKEYS'               
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*              PF4  - INSERT LINE BEFORE CURRENT ONE                  *         
*              PF5  - SPLIT LINE DOWN TO NEXT ONE                     *         
*              PF6 -  JOIN LINE WITH NEXT                             *         
*              PF9  - DELETE LINE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1                                                                   
         ICM   R5,15,ATIOB          ESTABLISH TIOB                              
         USING TIOBD,R5                                                         
*                                                                               
         CLC   JOBACT(3),=C'ADD'   IF ACTION 'ADD'                              
         BNE   PFKEYS1                                                          
*                                                                               
         FOUT  JOBACTH,=C'CHA'        CHANGE TO 'CHANGE'                        
*                                                                               
PFKEYS1  DS    0H                                                               
*                                                                               
         LA    R6,LNTBL            POINT TO START OF TABLE                      
*                                                                               
         CLC   0(2,R6),TIOBCURD    FIND LINE WITH CURSOR                        
         BE    *+16                                                             
         BH    PFKEYSX             IGNORE IF CURSOR NOT ON A LINE               
         LA    R6,2(R6)            BUMP TO NEXT TABLE ENTRY                     
         B     *-18                                                             
*                                                                               
         MVC   ALINCUR,0(R6)       SAVE A(LINE WITH CURSOR)                     
*                                                                               
         CLI   TIOBAID,4           PF4 OR PF16                                  
         BE    *+8                                                              
         CLI   TIOBAID,16                                                       
         BE    LNADD                  ADD LINE                                  
*                                                                               
         CLI   TIOBAID,5           PF5 OR PF17                                  
         BE    *+8                                                              
         CLI   TIOBAID,17                                                       
         BE    LNADD                  SPLIT LINE                                
*                                                                               
         CLI   TIOBAID,6           PF6 OR PF18                                  
         BE    *+8                                                              
         CLI   TIOBAID,18                                                       
         BE    LNJOIN                 JOIN LINE                                 
*                                                                               
         CLI   TIOBAID,9           PF9 OR PF21                                  
         BE    *+8                                                              
         CLI   TIOBAID,21                                                       
         BE    LNDEL                  DELETE LINE                               
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - LNADD'                
***********************************************************************         
*                                                                     *         
*        ADD/CHANGE A LINE                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNADD    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
         CLC   JOBINSL,SPACES      LAST LINE MUST BE EMPTY                      
         BNH   LNADD1                                                           
*                                                                               
         LA    R3,LNADDERR         NO ROOM ON SCREEN                            
         B     PFKEYERX                                                         
*                                                                               
LNADD1   DS    0H                                                               
*                                                                               
         LA    R6,LNTBLLS          POINT TO LAST ENTRY IN TABLE                 
         LR    R7,R6                                                            
         SH    R7,=H'2'            BACK UP A TABLE ENTRY                        
*                                                                               
LNADDLP  DS    0H                                                               
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R7)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         MVC   8(L'JOBINS1,RF),8(RE)  COPY LINE DOWN ONE                        
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNADDCN  DS    0H                                                               
*                                                                               
         LR    R6,R7               BACK UP ENTRIES IN TABLE                     
         SH    R7,=H'2'                                                         
*                                                                               
         CLC   0(2,R7),ALINCUR     STOP IF PASSED LINE WITH CURSOR              
         BNL   LNADDLP                                                          
*                                                                               
LNADDDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,ALINCUR        POINT TO LINE WITH CURSOR                    
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         CLI   TIOBAID,5           SKIP IF SPLITTING LINE                       
         BE    *+8                                                              
         CLI   TIOBAID,17                                                       
         BE    LNADDSP                                                          
*                                                                               
         MVC   8(L'JOBINS1,R6),SPACES   CLEAR FIELD                             
         MVI   5(R6),0             INDICATE NO INPUT                            
         OI    6(R6),X'80'         TRANSMIT FIELD                               
         MVI   7(R6),0             INDICATE NO OUTPUT                           
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
LNADDSP  DS    0H                                                               
*                                                                               
         LA    RF,L'JOBINS1-1+8(R6) POINT TO LAST BYTE OF FIELD                 
*                                                                               
         SR    RE,RE                                                            
         IC    RE,TIOBCURI         DISPLACEMENT OF CURSOR INTO FLD              
         LA    RE,8(RE,R6)         CURSOR ADDRESS                               
*                                                                               
         CR    RF,RE               DONE IF CURSOR PASSED                        
         BL    *+12                                                             
         MVI   0(RF),C' '          CLEAR OUT END OF FIELD                       
         BCT   RF,*-10                                                          
*                                                                               
         LA    RE,JOBINS2H-JOBINS1H(RE)    POINT TO CURSOR IN NEXT LINE         
         LA    RF,8+JOBINS2H-JOBINS1H(R6)  START OF NEXT FIELD                  
*                                                                               
         CR    RF,RE               CLEAR OUT TO CURSOR                          
         BNL   *+16                                                             
         MVI   0(RF),C' '                                                       
         LA    RF,1(RF)            NEXT POSITION                                
         B     *-14                                                             
*                                                                               
         OI    6(R6),X'80'         TRANSMIT SPLIT LINE - OTHERS DONE            
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - LNDEL'                
***********************************************************************         
*                                                                     *         
*        DELETE A LINE                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNDEL    DS    0H                  ADD LINE BEFORE CURSOR LINE                  
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         LR    R7,R6                                                            
         AH    R7,=H'2'            POINT TO NEXT TABLE ENTRY                    
*                                                                               
LNDELLP  DS    0H                                                               
*                                                                               
         CLC   0(2,R7),LNTBLLS     STOP IF PASSED END OF TABLE                  
         BH    LNDELDN                                                          
*                                                                               
         LH    RF,0(R6)                                                         
         LA    RF,0(RF,RA)         POINT TO LINES IN TWA                        
         LH    RE,0(R7)                                                         
         LA    RE,0(RE,RA)                                                      
*                                                                               
         MVC   8(L'JOBINS1,RF),8(RE)  COPY LINE UP ONE                          
         MVC   4(2,RF),4(RE)       COPY INPUT INDICATORS AND LENGTH             
         OI    6(RF),X'80'         TRANSMIT FIELD                               
         MVC   7(1,RF),5(RF)       SET OUTPUT LENGTH                            
*                                                                               
LNDELCN  DS    0H                                                               
*                                                                               
         LR    R6,R7               ADVANCE ENTRIES IN TABLE                     
         AH    R7,=H'2'                                                         
*                                                                               
         B     LNDELLP                                                          
*                                                                               
LNDELDN  DS    0H                                                               
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,3,LNTBLLS        POINT TO LAST LINE IN TABLE                  
         AR    R6,RA               POINT TO FIELD HEADER                        
*                                                                               
         MVC   8(L'JOBINS1,R6),SPACES   CLEAR LAST LINE                         
         MVI   5(R6),0             INDICATE NO INPUT                            
         OI    6(R6),X'80'         TRANSMIT FIELD                               
         MVI   7(R6),0             INDICATE NO OUTPUT                           
*                                                                               
         B     PFKEYSX                                                          
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - LNJOIN'               
***********************************************************************         
*                                                                     *         
*        JOIN LINE WITH CURSOR TO ONE BELOW                           *         
*              FIND LAST NON-BLANK AND FILL REMAINDER OF LINE WITH    *         
*              DATA FROM NEXT LINE :*                                           
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LNJOIN   DS    0H                                                               
*                                                                               
*        ON ENTRY R6==> TABLE ENTRY FOR LINE WITH CURSOR                        
*                                                                               
         LH    RF,0(R6)            POINT TO CURSOR LINE                         
         LA    RF,0(RF,RA)                                                      
         OI    6(RF),X'80'         FORCE RE-DISPLAY OF LINE                     
*                                                                               
*        MOVE RIGHT PART OF LINE OVER TO CURSOR                                 
*                                                                               
         ICM   RE,15,ATIOB         GET DISPLACEMENT OF CURSOR INTO LINE         
         SR    R1,R1                                                            
         ICM   R1,1,TIOBCURI-TIOBD(RE)                                          
*                                                                               
         LA    R1,8(R1,RF)         POINT TO CURSOR POSITION                     
*                                                                               
         LA    RF,L'JOBINS1-1+8(RF) POINT TO LAST BYTE OF CURSOR LINE           
*                                                                               
         CLI   0(R1),C' '          FIND FIRST BLANK AT OR AFTER CURSOR          
         BNH   *+18                                                             
         LA    R1,1(R1)                                                         
         CR    R1,RF               CHECK FOR END OF LINE                        
         BL    *-14                                                             
         B     LNJOIN0             LINE FULL                                    
*                                                                               
         LA    R1,1(R1)            MOVE EVERYTHING UP TO NEXT POSITION          
*                                                                               
         CR    R1,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
*                                                                               
         CLI   0(R1),C' '          DONE IF NON-BLANK                            
         BH    LNJOIN0                                                          
*                                                                               
         LA    RE,1(R1)            SEARCH FOR NEXT NON-BLANK                    
*                                                                               
         CR    RE,RF               DONE IF END OF LINE REACHED                  
         BH    LNJOIN0                                                          
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         B     *-18                                                             
*                                                                               
         MVC   0(1,R1),0(RE)       MOVE RIGHT END OVER TO THE LEFT              
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   *-16                                                             
*                                  END OF LINE REACHED                          
         MVI   0(R1),C' '          FILL OUT REST OF LINE WITH BLANKS            
         LA    R1,1(R1)                                                         
         CR    R1,RF                                                            
         BNH   *-10                                                             
*                                                                               
LNJOIN0  DS    0H                                                               
*                                  RF POINTS TO LAST BYTE OF LINE               
         LA    R0,L'JOBINS1        NUMBER OF BYTES ON LINE                      
*                                                                               
         CLI   0(RF),C' '          IF LINE FULL NOTHING TO DO                   
         BH    LNJOIN1                                                          
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-BLANK                          
         BH    *+14                                                             
         BCTR  RF,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
         B     LNJOIN1             EMPTY LINE NO SPACE TO START                 
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
         MVI   1(RF),C' '          FORCE SPACE SEPARATOR                        
         AH    R0,=H'1'            RECTIFY BYTE COUNT                           
*                                                                               
LNJOIN1  DS    0H                                                               
*                                                                               
         LA    RF,1(RF)            BUMP TO FIRST AVAILABLE SPOT                 
*                                                                               
         CLC   2(2,R6),=X'FFFF'    SKIP IF AT END OF TABLE                      
         BE    LNJOINX                                                          
*                                                                               
         LH    RE,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RE,0(RE,RA)         START OF NEXT LINE                           
         OI    6(RE),X'80'         FORCE RE-DISPLAY OF LINE                     
         LA    RE,8(RE)            FIRST BYTE OF LINE                           
*                                                                               
         LA    R2,L'JOBINS1        LENGTH OF LINE                               
*                                                                               
         CLI   0(RE),C' '          FIND FIRST NON-SPACE                         
         BH    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   R2,*-12                                                          
         B     LNJOINX              LINE IS BLANK                               
*                                                                               
         LR    R7,RE               SAVE DATA START ADDRESS                      
*                                                                               
         LA    R1,L'JOBINS1        CALCULATE SPACE LEFT ON CURSOR LINE          
*                                                                               
         SR    R1,R0                                                            
         BNP   LNJOIN3             NO ROOM ON CURSOR LINE                       
*                                                                               
         CR    R2,R1                                                            
         BH    *+10                                                             
         LR    R1,R2               USE SMALLER LENGTH                           
         B     LNJOIN2             GO MOVE EVERYTHING                           
*                                                                               
         LA    RE,0(R1,RE)         POINT TO LAST BYTE PLUS ONE                  
*                                    TO BE MOVED TO CURSOR LINE                 
*                                                                               
         CLI   0(RE),C' '          MUST BE A SPACE                              
         BNH   *+14                                                             
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   R1,*-10                                                          
         B     LNJOIN3             NO ROOM TO MOVE ANYTHING                     
*                                                                               
LNJOIN2  DS    0H                                                               
*                                  R1 HAS NUMBER OF BYTES TO MOVE               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R7)       MOVES NEXT LINE TO CURSOR LINE               
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),SPACES      CLEARS START OF NEXT LINE                    
*                                                                               
         LA    R1,1(R1)            RESTORE BYTE COUNTER                         
*                                                                               
*                                  R2 HAS REMAINING BYTES ON LINE               
*                                                                               
LNJOIN3  DS    0H                                                               
*                                                                               
         SR    R2,R1               NUMBER OF BYTES TO BE JUSTIFIED              
         BNP   LNJOINX             NONE - ALL MOVED                             
*                                                                               
         CLI   0(RE),C' '          FIND NEXT NON-BLANK                          
         BH    *+16                                                             
         LA    RE,1(RE)            BUMP POINTER                                 
         BCT   R2,*-12             CONTINUE IF MORE ON LINE                     
         B     LNJOINX             NOTHING LEFT ON LINE TO MOVE                 
*                                                                               
         LH    RF,2(R6)            DISPLACEMENT OF NEXT LINE                    
         LA    RF,8(RF,RA)         START OF NEXT LINE                           
*                                                                               
         BCTR  R2,0                DECREMENT FOR EXECUTE                        
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       LEFT JUSTIFIES 'NEXT' LINE                   
*                                                                               
         LA    RF,1(R2,RF)         END OF WHAT WAS JUSTIFIED                    
*                                                                               
         LA    R2,1(R2)            RESTORE BYTE COUNTER                         
*                                                                               
         LA    R1,L'JOBINS1                                                     
         SR    R1,R2               AMOUNT TO BE CLEARED AFTERWARDS              
         BNP   LNJOINX             NOTHING                                      
*                                                                               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES      CLEAR OUT REST OF 'NEXT' LINE                
*                                                                               
LNJOINX  DS    0H                                                               
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - EXITS'                
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1  REGS=(R3)           RETURN R3                                    
*                                                                               
         TITLE 'PPJOB02 - JOB FILE - INSTRUCTION RECORD - LNTBL'                
***********************************************************************         
*                                                                     *         
*        TABLE OF LINE DISPLACEMENTS INTO SCREEN                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                  ALIGNMENT                                    
         DC    H'0'                EXTRA ENTRY FOR SPLIT LOGIC                  
LNTBL    DS    0H                                                               
         DC    Y(JOBINS1H-T40FFFD)  INSTRUCTION 1                               
         DC    Y(JOBINS2H-T40FFFD)  INSTRUCTION 2                               
         DC    Y(JOBINS3H-T40FFFD)  INSTRUCTION 3                               
         DC    Y(JOBINS4H-T40FFFD)  INSTRUCTION 4                               
         DC    Y(JOBINS5H-T40FFFD)  INSTRUCTION 5                               
         DC    Y(JOBINS6H-T40FFFD)  INSTRUCTION 6                               
         DC    Y(JOBINS7H-T40FFFD)  INSTRUCTION 7                               
         DC    Y(JOBINS8H-T40FFFD)  INSTRUCTION 8                               
         DC    Y(JOBINS9H-T40FFFD)  INSTRUCTION 9                               
         DC    Y(JOBINSAH-T40FFFD)  INSTRUCTION 10                              
         DC    Y(JOBINSBH-T40FFFD)  INSTRUCTION 11                              
         DC    Y(JOBINSCH-T40FFFD)  INSTRUCTION 12                              
         DC    Y(JOBINSDH-T40FFFD)  INSTRUCTION 13                              
         DC    Y(JOBINSEH-T40FFFD)  INSTRUCTION 14                              
         DC    Y(JOBINSFH-T40FFFD)  INSTRUCTION 15                              
LNTBLLS  DC    Y(JOBINSLH-T40FFFD)  COMMENT LAST                                
         DC    4X'FF'               END OF TABLE                                
         EJECT                                                                  
*                                                                               
* INITIALISATION CODE                                                           
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(44),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LA    R3,IOAREA                                                        
         ST    R3,AREC                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
         SPACE 2                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                        OR NON NUMERIC                       
         CHI   R1,10              WAS A BUG IF LENGTH IS BIGGER                 
         BL    PACK01             THEN 10                                       
         BR    RE                                                               
PACK01   BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
         SPACE 2                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         SPACE 2                                                                
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
         SPACE 2                                                                
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
         SPACE 3                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
         SPACE 2                                                                
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
         SPACE 2                                                                
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
         SPACE 3                                                                
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
         SPACE 2                                                                
FILE     NTR                                                                    
         LA    R2,KEY+27                                                        
         CLC   COMMAND(5),=C'DMDEL'                                             
         BE    *+12                                                             
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE',            X        
               (R2),AREC,(TERMNAL,DMWORK)                                       
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBDIR)                     
         SPACE 3                                                                
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
SEQPUB   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
HIGHPUB  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
         SPACE 2                                                                
WRITEPUB MVC   COMMAND,=C'DMWRT'                                                
         B     PUBDIRY                                                          
         SPACE 2                                                                
ADDPUBD  MVC   COMMAND,=C'DMADD '                                               
         B     PUBDIRY                                                          
         SPACE 2                                                                
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                  COMMUNICATION WITH DATA MANAGER (PUBFILE)                    
         SPACE 3                                                                
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
         SPACE 2                                                                
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                  DATA MANAGER ERRORS AND EXIT                                 
         SPACE 3                                                                
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
         SPACE 2                                                                
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   STCM  R2,15,SVERRCUR                                                   
         XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPJOBWRK          DUMMY SECTIONS START HERE                    
*                                                                               
T40FFFD  DSECT                                                                  
         ORG                                                                    
SAVEKEY  DS    CL32                                                             
RMDATSW  DS    CL1                                                              
LASTLIN  DS    XL1                 NUMBER OF LAST USED LINE                     
CURLIN   DS    XL1                 NUMBER OF CURRENT   LINE                     
LINCT    DS    PL2                                                              
ALINCUR  DS    A                   A(LINE WITH CURSOR)                          
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
*                                                                               
*PADIRECD (DSECT)                                                               
       ++INCLUDE PPGENPADID                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPJOB02   06/17/08'                                      
         END                                                                    
