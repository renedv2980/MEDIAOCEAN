*          DATA SET PPJOB00    AT LEVEL 046 AS OF 07/07/14                      
*PHASE T40F00B                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPJOB00 - PRINTPAK JOB HEADER MAINTENANCE - BASE'               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 05/22/14 ALLOW 11 OR 12 CHARACTERS AD-ID                                 
*                                                                               
* KWAN 06/25/08 BUG FIX FOR TRAFFIC USERS                                       
*                                                                               
* SMYE  12/07   NEW SUB-ADCODE "RECORD" (PPJOB03) ACTION=SUB                    
*                                                                               
* SMYE 10/21/05 DEACTIVATE SPECIAL MINDSHARE PRD SECURITY IN PRD                
*                                                                               
* SMYE 4/05-9/05 ALLOW AD-ID OR JOBCODE OR BOTH AS "KEY" ENTRIES                
*                                                                               
* BPLA  11/03   DISALLOW "ALL" FOR JOB CODE INPUT                               
*                                                                               
* SMYE  5/02    ADD FULL LIMIT ACCESS SECURITY INCLUDING CLIENT STRINGS         
*                                                                               
* SMYE  4/02    SPECIAL PRODUCT SECURITY FOR CERTAIN MINDSHARE ID'S             
*               AND SET ADDRESSES FOR CORE-RESIDENT PUBVAL AND PUBEDIT          
*                                                                               
* KWAN 09/21/01 REORGANIZED PPJOBWRK AND SPACE DESP VALIDATION                  
*                                                                               
* KWAN 08/23/01 CLIENT TRAFFIC OFFICE SECURITY                                  
*                                                                               
* SMYE  1/27/99 FIX MEDIA NAME DISPLAY                                          
*                                                                               
* SMYE  1/97    ADD CLIENT GROUP SECURITY                                       
*                                                                               
* BPLA  4/96    ALLOW ACTION "AD"  OR "ADF" TO RETURN TO AD SCREEN              
*               (USED TO ALLOW ONLY "JOB")                                      
*                                                                               
* BPLA  8/26/90 SAVE A(COMFACS) - NEEDED FOR NAME SEARCHING IN T40F02           
*                                                                               
* ROSA  2/9/90  ADD CLIENT SECURITY                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T40F00   CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB1Q,JOBEXWKD,LIOBSB2Q,T40FFFD)              
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 PPJOB00X-PPJOB00D,T40F00,RR=R9,CLEAR=YES                         
*                                                                               
         USING GENOLD,RC                                                        
         L     RA,4(R1)            GET A(TWA)                                   
         USING T40FFFD,RA                                                       
*                                                                               
         MVC   ATIOB,0(R1)         SAVE A(TIOB)                                 
         MVI   ATIOB,0                                                          
         BRAS  RE,INITL                                                         
*                                                                               
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         ST    RD,SYSRD                                                         
         ST    R9,RELO00                                                        
*                                                                               
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         USING GLOBALS,R9          R9=A(GLOBAL LITERALS)                        
*                                                                               
         LR    R8,RC                                                            
         A     R8,=A(JOBEXWKS-PPJOB00D)                                         
         USING JOBEXWKD,R8                                                      
*                                                                               
         BRAS  RE,INITWKST         INITIALIZE WORKING STORAGE AREAS             
*                                                                               
         BRAS  RE,CKADBYER         ANY GLOBBER CALLS?                           
         BNE   JBTOP90                                                          
         BRAS  RE,LKIO_GET         LINKIO WILL SET FLDS                         
         BRAS  RE,SETADFLD         SET AD CODE FIELDS                           
         BRAS  RE,LKIO_GET         PREPARE FOR MULTIPLE REPLY RECORDS           
*                                                                               
JBTOP90  DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MED      LA    R2,JOBMEDH          MEDIA                                        
         TM    4(R2),X'20'                                                      
         BNZ   CLT                                                              
         BRAS  RE,CLRMED                                                        
*                                                                               
         LA    R3,MEDERR                                                        
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),JOBMED                                                  
         MVI   KEY+3,1                                                          
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BRAS  RE,GETREC                                                        
         MVC   SVAGPROF,PAGYPROF                                                
         MVC   JOBMEDN(L'PAGYMED),PAGYMED                                       
         FOUT  JOBMEDNH                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         MVC   SVMED,JOBMED                                                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLT      LA    R2,JOBCLTH          CLIENT                                       
         TM    4(R2),X'20'                                                      
         BNZ   PRD                                                              
         BRAS  RE,CLRCLT                                                        
*                                                                               
         LA    R3,CLTERR                                                        
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),SVMED                                                   
         MVI   KEY+3,2                                                          
         OC    JOBCLT,SPACES                                                    
         MVC   KEY+4(3),JOBCLT                                                  
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BRAS  RE,GETREC                                                        
*                                                                               
         MVC   SVCLTOFF,PCLTOFF    SAVE CLIENT OFFICE CODE                      
         MVC   BYTE3,PCLTOFF       FOR LIMIT ACCESS TESTING                     
*                                                                               
         OC    T40FFFD+4(2),T40FFFD+4   TEST AGENCY ON NEW SECURITY             
         BNZ   *+14                                                             
         OC    T40FFFD+6(2),T40FFFD+6   TEST ANY LIMIT ACCESS                   
         BZ    CLT4                     NO LIMIT ACCESS                         
*                                                                               
         MVC   BYTE3,PCLTOFF       FOR LIMIT ACCESS TESTING                     
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
         BNE   CLT3                NO                                           
         BRAS  RE,TRAFFACC         PUT TRAFFIC OFFICE CODE (IF FOUND)           
*                                  INTO BYTE3                                   
CLT3     DS    0H                  LIMIT ACCESS TESTING                         
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,T40FFFD+6                                                
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,BYTE3       CLT OR CLT TRAFFIC OFFICE CODE                
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),T40FFFD+6                                              
         MVC   OFCSECD,APUBIO    A(SECBLK) IS IN APUBIO WHICH IS                
         DROP  R1                POINTING TO IOAREA2                            
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),ACOMFACS    ACCESS AWARE CALL              
         CLI   0(R1),0                                                          
         BE    CLT4               OK                                            
CLT3E    LA    R3,PPECLTNA        ACCESS TO CLT NOT AUTHORIZED                  
         B     ERROR                                                            
*                                                                               
CLT4     DS    0H                                                               
*                                                                               
CLT10    MVC   SVCLPROF,PCLTPROF                                                
         MVC   JOBCLTN,PCLTNAME                                                 
         FOUT  JOBCLTNH                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         MVC   SVCLT,JOBCLT                                                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRD      LA    R2,JOBPRDH          PRODUCT                                      
         TM    4(R2),X'20'                                                      
         BNZ   JOB                                                              
         BRAS  RE,CLRPRD                                                        
*                                                                               
         LA    R3,PRDERR                                                        
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         CLI   JOBPRD,C'*'         DON'T ALLOW OTHER AGY PRD                    
         BE    ERROR                                                            
*                                                                               
         CLC   =C'SUB',JOBACT      "SUB-CODE" ACTION ?                          
         BNE   PRDGET              NO - CONTINUE                                
         CLC   JOBPRD,=C'ZZZ'      DON'T ALLOW "POOL" PRD                       
         BE    ERROR                                                            
*                                                                               
PRDGET   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(5),SVMED                                                   
         MVI   KEY+3,6                                                          
         OC    JOBPRD,SPACES                                                    
         MVC   KEY+7(3),JOBPRD                                                  
         BRAS  RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         BRAS  RE,GETREC                                                        
*                                  SPECIAL MINDSHARE PRD SECUTITY               
*  10/21/05 - BELOW LOGIC DISCONTINUED AT AGENCY REQUEST                        
*                                                                               
         B     PRDOK               BRANCH AROUND DISCONTINUED LOGIC             
*                                                                               
         LR    RE,RA                                                            
         USING TWAD,RE                                                          
         ZICM  RF,TWAUSRID,2                                                    
         DROP  RE                                                               
         CHI   RF,10296            "MSSROC" USER ID ?                           
         BE    PRDCKB              YES                                          
         CHI   RF,10297            "MSSRYC" USER ID ?                           
         BE    PRDCKC              YES                                          
         B     PRDOK                                                            
PRDCKB   DS    0H                                                               
         CLI   PPRDTRAF,C'6'       TRAFFIC OFFICE = 6 ?                         
         BE    PRDOK               YES - OK                                     
         B     PRDCKNG             SECURITY LOCKOUT                             
PRDCKC   DS    0H                                                               
         CLI   PPRDTRAF,C'7'       TRAFFIC OFFICE = 7 ?                         
         BE    PRDOK               YES - OK                                     
PRDCKNG  LA    R3,PPESECLK         SECURITY LOCKOUT                             
         B     ERROR                                                            
*                                                                               
*   10/21/05 - ABOVE LOGIC DISCONTINUED AT AGENCY REQUEST                       
*                                                                               
PRDOK    DS    0H                                                               
         MVC   JOBPRDN,PPRDNAME                                                 
         FOUT  JOBPRDNH                                                         
*                                                                               
         OI    4(R2),X'20'                                                      
         MVC   SVPRD,JOBPRD                                                     
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOB      LA    R2,JOBJOBH          JOB CODE                                     
         LA    R3,2                INVALID INPUT ERROR MSG                      
         CLC   =C'NONE',8(R2)      CAN'T ACCEPT NONE AS INPUT                   
         BNE   *+8                                                              
         B     ERROR                                                            
*                                                                               
         CLC   =C'*ADID*',8(R2)    DISALLOW AS INPUT                            
         BNE   *+8                                                              
         B     ERROR                                                            
*                                                                               
         CLI   5(R2),3             INPUT LENGTH 3?                              
         BNE   JOB5                                                             
         CLC   =C'ALL',8(R2)       DISALLOW "ALL" AS INPUT                      
         BNE   JOB5                                                             
         B     ERROR                                                            
*                                                                               
JOB5     DS    0H                                                               
*                                                                               
         CLI   JOBJOB,C'='         BROWSE ?                                     
         BNE   JOB5D               NO                                           
         MVC   BROWSDTA,SVCLT      CLT/PRD                                      
*                                                                               
         GOTOR VPBROWSE,DMCB,ACOMFACS,SYSRD,(R2),                      +        
               BROWSDTA,(SVMED,C' JOB'),0                                       
*                                                                               
         DC    H'0'                BROWSE KNOWS WHERE TO GET BACK               
*                                                                               
JOB5D    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BNZ   ADID                                                             
         BRAS  RE,CLRJOB                                                        
*                                                                               
         LA    R3,MISSERR                                                       
         CLI   5(R2),0                                                          
*SMY*    BE    ERROR                                                            
         BNE   JOB9                                                             
         CLI   JOBADIDH+5,0        AD-ID ENTERED ?                              
         BE    ERROR               NO - THEN JOB CODE REQUIRED                  
         XC    SVJOB,SVJOB         AD-ID ONLY (NO JOB CODE)                     
         B     JOB9D                                                            
JOB9     OC    JOBJOB,SPACES                                                    
         CLI   JOBJOB,C' '         LEADING SPACES IN AD CODE?                   
         BH    JOB9C                                                            
         LHI   R0,NLSADCER                                                      
         BRAS  RE,GET_ETXT                                                      
         J     EXIT                                                             
JOB9C    MVC   SVJOB,JOBJOB                                                     
JOB9D    DS    0H                                                               
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADID     LA    R2,JOBADIDH         AD-ID                                        
*                                                                               
         CLI   JOBADID,C'='        BROWSE ?                                     
         BNE   ADID2               NO                                           
         MVC   BROWSDTA,SVCLT      CLT/PRD                                      
*                                                                               
         GOTOR VPBROWSE,DMCB,ACOMFACS,SYSRD,(R2),                      +        
               BROWSDTA,(SVMED,C' ADI'),0                                       
*                                                                               
         DC    H'0'                BROWSE KNOWS WHERE TO GET BACK               
*                                                                               
ADID2    DS    0H                                                               
         TM    4(R2),X'20'                                                      
         BNZ   ACT                                                              
         BRAS  RE,CLRADID                                                       
*                                                                               
*SMY*    XC    SVADID,SVADID                                                    
*                                                                               
         LA    R3,MISSERR                                                       
         CLI   5(R2),0             ANYTHING INPUT ?                             
         BNE   ADID5               YES                                          
         LA    R2,JOBJOBH                                                       
         CLI   5(R2),0             ANYTHING INPUT IN JOB CODE ?                 
         BNE   ADIDX               YES - AD ID NOT REQUIRED                     
         B     ERROR               EITHER JOB CODE OR ADID REQUIRED             
ADID5    DS    0H                                                               
         LA    R2,JOBADIDH                                                      
         LA    R3,INVERR           INVALID INPUT ERROR MSG                      
         CLI   5(R2),11                                                         
         JE    *+12                                                             
         CLI   5(R2),12                                                         
         JNE   ERROR               AD-ID IS 11 OR 12 CHARACTERS                 
*                                                                               
         MVC   WORK(12),JOBADID    WORK USED IN EDIT                            
         BRAS  RE,EDTADID          TEST FOR VALID AD ID FORMAT                  
         BE    ERROR                (ONLY CHECKS FOR BLANKS NOW)                
*                                                                               
         MVC   SVADID,JOBADID      SAVE                                         
         OI    4(R2),X'20'                                                      
*                                                                               
ADIDX    DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACT      LA    R2,JOBACTH          ACTION                                       
         LA    R3,ACTERR                                                        
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
*                                                                               
         MVI   ROUTE,1                                                          
         CLC   =C'JOB',JOBACT      RETURN TO AD SCREEN?                         
         BE    GETOVLY                                                          
         OI    JOBACT+2,C' '                                                    
         CLC   =C'AD ',JOBACT      ALLOW AD?                                    
         BE    GETOVLY                                                          
         CLC   =C'ADF',JOBACT      ADF?                                         
         BE    GETOVLY                                                          
*                                                                               
         MVI   ROUTE,2                                                          
         CLC   =C'INS',JOBACT                                                   
         BE    GETOVLY                                                          
*                                                                               
         MVI   ROUTE,3                                                          
         CLC   =C'SUB',JOBACT                                                   
         BNE   ACTX                                                             
*                                                                               
         CLI   JOBJOB,JBSUB1Q      "SUB" AD CODE MUST BEGIN WITH !              
         BE    GETOVLY                                                          
         XC    JOBMSG,JOBMSG                                                    
         MVC   JOBMSG(L'SUBERR),SUBERR                                          
         FOUT  JOBMSGH                                                          
         LA    R2,JOBJOBH                                                       
         B     EXIT                                                             
*                                                                               
ACTX     DS    0H                                                               
         MVI   ROUTE,1                                                          
         CLI   SCRNTYP,0                                                        
         BE    GETOVLY                                                          
         MVC   ROUTE,SCRNTYP                                                    
*                                                                               
GETOVLY  DS    0H                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB(1),ROUTE                                                    
         MVI   ERRAREA,0                                                        
*                                                                               
         GOTO1 VCALLOV,DMCB,,(RA)                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(RC),(RA)                                              
*                                                                               
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
         CLC   =C'ADD',JOBACT                                                   
         BE    REQ                                                              
         CLC   =C'CHA',JOBACT                                                   
         BNE   EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REQ      XC    QCTL,QCTL                                                        
         MVI   QAREA,C' '                                                       
         MVC   QAREA+1(79),QAREA                                                
         MVC   QAREA(2),=C'73'                                                  
         MVC   QAREA+2(2),AGYALPHA                                              
         MVC   QAREA+4(1),JOBMED                                                
         MVC   QAREA+5(3),JOBCLT                                                
         MVC   QAREA+11(3),JOBPRD                                               
         MVC   QAREA+14(6),JOBJOB     SEE "AD-ID ONLY" BELOW                    
         MVC   QAREA+50(2),=C'NO'     SET QSORT TO NO                           
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
         OC    QAREA(80),SPACES                                                 
*                                                                               
         CLI   SVJOB,X'FF'         "AD-ID ONLY" RECORD ?                        
         BE    REQ2                YES                                          
         CLI   SVJOB,X'40'         "AD-ID ONLY" RECORD ?                        
         BH    *+10                NO                                           
REQ2     MVC   QAREA+14(6),ADIDJOB   X'FF..' JOB CODE SAVED IN 01               
*                                                                               
         CLI   ROUTE,2                                                          
         BNE   REQ4                                                             
         CLI   SVPUB,X'FF'                                                      
         BE    REQ4                                                             
         GOTO1 VPUBEDIT,DMCB,SVPUB,(C'Q',QAREA+26)                              
*                                                                               
REQ4     DS    0H                                                               
*                                                                               
         MVI   QCTL+10,73                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                   
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         BZ    EXXMOD                                                           
         SR    R3,R3                                                            
         B     ERROR                                                            
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRMED   XC    JOBMEDN,JOBMEDN                                                  
         FOUT  JOBMEDNH                                                         
         NI    JOBMEDH+4,X'DF'                                                  
         CLI   SCRNTYP,2                                                        
         JNE   CLRCLT                                                           
         XC    JOBPUBN,JOBPUBN                                                  
         XC    JOBPUBZ,JOBPUBZ                                                  
         FOUT  JOBPUBNH                                                         
         FOUT  JOBPUBZH                                                         
         NI    JOBPUBH+4,X'DF'                                                  
*                                                                               
CLRCLT   XC    JOBCLTN,JOBCLTN                                                  
         FOUT  JOBCLTNH                                                         
         NI    JOBCLTH+4,X'DF'                                                  
*                                                                               
CLRPRD   XC    JOBPRDN,JOBPRDN                                                  
         FOUT  JOBPRDNH                                                         
         NI    JOBPRDH+4,X'DF'                                                  
*                                                                               
CLRJOB   NI    JOBJOBH+4,X'DF'                                                  
         CLI   ADBSW,0             ADBUYER UPLOAD?                              
         JNE   *+8                                                              
         MVI   CHGSW,0                                                          
         BR    RE                                                               
*                                                                               
CLRADID  NI    JOBADIDH+4,X'DF'                                                 
         CLI   JOBJOBH+5,0         JOB CODE ENTERED?                            
         BNER  RE                                                               
         CLI   ADBSW,0             ADBUYER UPLOAD?                              
         JNE   *+8                                                              
         MVI   CHGSW,0             DISPLAY REQUIRED BEFORE CHANGE               
         BR    RE                                                               
*                                                                               
NXTELEM  SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
NXTWFELM SR    R0,R0                                                            
         ICM   R0,3,(LQ_LN-LQ_D)(R3)                                            
         AR    R3,R0                                                            
         BR    RE                                                               
*                                                                               
BUMPFLDS SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCTR  RF,0                                                             
         CHI   RF,0                                                             
         JH    BUMPFLDS                                                         
         BR    RE                                                               
*                                                                               
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BRAS  RE,CLEARWRK                                                      
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
*                                                                               
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CHI   R5,250                                                           
         JNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SHI   R5,250                                                           
         J     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
*                                                                               
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         J     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         J     ERROR                                                            
*                                                                               
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
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
MOVE     MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         BCTR  R1,R0                                                            
         EX    R1,VARMOVE                                                       
         BR    RE                                                               
*                                                                               
VARMOVE  MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (DIRECTORY)                                   
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRCTRY                                                          
*                                                                               
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
*                                                                               
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* COMMUNICATION WITH DATA MANAGER (FILE)                                        
*                                                                               
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
*                                                                               
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
*                                                                               
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
*                                                                               
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
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBDIR)                                      
*                                                                               
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
*                                                                               
* COMMUNICATION WITH DATA MANAGER (PUBFILE)                                     
*                                                                               
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
*                                                                               
ADDPUB   MVC   COMMAND,=C'ADDREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R2,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),APUBIO,(TERMNAL,DMWORK)                                     
         B     DMCHECK                                                          
         EJECT                                                                  
*                                                                               
* DATA MANAGER ERRORS AND EXIT                                                  
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD) .          UNWIND WITHOUT XIT                           
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3 .             LET GETMSG SORT IT OUT                       
         B     ERROR                                                            
         EJECT                                                                  
*                                                                               
* EXITS FROM PROGRAM                                                            
*                                                                               
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
*                                                                               
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
EXXMOD   TM    ADBSW,AS_ADBRQ      ADBUYER REQUEST?                             
         BZ    *+8                                                              
         BRAS  RE,SETRPLYR         SET AD FILE UPLOAD REPLY RECORD              
         XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
X_XIT1   XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LKIO_GET LR    R0,RE                                                            
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         GOTOR ALINKIO,DMCB,('LIOAGET',LIOBD)                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
GET_ITXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    JOBMSG,JOBMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'I',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
GET_ETXT ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    JOBMSG,JOBMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R0),0,(C'E',DMCB),0,0,0                            
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITWKST NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,=V(PPBROWSE)                                                  
         A     R0,RELO00                                                        
         ST    R0,VPBROWSE                                                      
*                                                                               
         LA    RE,IOAREA2                                                       
         ST    RE,APUBIO                                                        
*                                                                               
         LR    RF,RC                                                            
         A     RF,=A(WRKRECA-PPJOB00D)                                          
         ST    RF,AWRKREC                                                       
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
         MVC   ALINKIO,CLINKIO-COMFACSD(RF)                                     
*                                                                               
         OC    T40FFFD+4(2),T40FFFD+4    TEST AGENCY ON NEW SECURITY            
         BNZ   *+14                                                             
         OC    T40FFFD+6(2),T40FFFD+6    TEST ANY LIMIT ACCESS                  
         BZ    INIWK20                                                          
*                                                                               
         LA    R0,IOAREA2          INIT SECRET BLOCK                            
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',IOAREA2),0                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
INIWK20  DS    0H                                                               
**  BELOW ROUTINE WILL GET TWO BYTES FROM FATBLOCK                              
**  WHICH ARE "PERSONAL ID"                                                     
*                                                                               
         XC    SVPID,SVPID         PASSWORD ID NUMBER CLEARED                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0                                          
         DROP  RF                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC                                                
         TM    FATFLAG,X'08'       CHECK IF SECRET CODE IS THERE                
         BZ    *+10                                                             
         MVC   SVPID,FAPASSWD      SAVE PASSWORD ID NUMBER                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB8'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBVAL,DMCB        STORE PUBVAL ADDRESS                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AB9'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPUBEDIT,DMCB       STORE PUBEDIT ADDRESS                        
*                                                                               
         XC    JOBMSG,JOBMSG                                                    
         OI    JOBMSGH+6,X'80'                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(3,BTODAY)                                    
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         MVI   ADBSW,0             INITIALIZE ADBUYER SWITCH                    
         MVI   ABRECTYP,0          INITIALIZE ADBUYER RECORD TYPE               
         XC    SVERRFLD,SVERRFLD                                                
         XC    SVERRCUR,SVERRCUR                                                
*                                                                               
         LA    R0,A_MAPSTR         INIT DATA MAPS FROM UPLOAD RECORD            
         LHI   R1,A_MAPLNQ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     X_XIT1                                                           
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    RE,TRTMPREC                                                      
         LA    RF,400*4                                                         
         XCEF                                                                   
*                                                                               
         LA    R4,TRTMPREC                                                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA                                                       
CKTRA10  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
         CLI   0(RE),CTAGYELQ      AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRA20                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     CKTRA10                                                          
*                                                                               
CKTRA20  DS    0H                                                               
         USING CTAGYD,RE                                                        
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         JNE   SETCCNEQ                                                         
         DROP  R4,RE                                                            
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         LA    R5,PCLTREC+33                                                    
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R5)         REPLACE PCLTOFF WITH TRAFFIC OFFICE          
*                                  FOR CALL TO OFFICER                          
TRACCX   XIT1                                                                   
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                EDIT AD ID FIELD                                     *         
*    WORK HAS THE 12-BYTE AD ID FROM THE CURRENT SCREEN               *         
*    FIRST 4 BYTES MUST BE ALPHA, NEXT 8 CAN BE ALPHANUMERIC          *         
*                                                                     *         
***********************************************************************         
*** A/O 5/15/06 - ONLY DISALLOWS BLANKS IN THE 12-CHARACTER FIELD   ***         
***********************************************************************         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
EDTADID  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,WORK             POINT R4 TO 12-BYTE AD ID ENTRY              
         LA    R5,12               LOOP COUNTER                                 
EDTAD10  DS    0H                                                               
*                                                                               
         CLI   0(R4),C' '                                                       
         BE    EDTADER             NO GOOD                                      
*                                                                               
*NOP*    CLI   0(R4),C'A'                                                       
*NOP*    BL    EDTADER             NO GOOD                                      
*NOP*    CLI   0(R4),C'I'                                                       
*NOP*    BNH   EDTAD20             CHARACTER IS OK                              
*NOP*    CLI   0(R4),C'J'                                                       
*NOP*    BL    EDTADER             NO GOOD                                      
*NOP*    CLI   0(R4),C'R'                                                       
*NOP*    BNH   EDTAD20             CHARACTER IS OK                              
*NOP*    CLI   0(R4),C'S'                                                       
*NOP*    BL    EDTADER             NO GOOD                                      
*NOP*    CLI   0(R4),C'Z'                                                       
*NOP*    BNH   EDTAD20             CHARACTER IS OK                              
*NOP*    CHI   R5,8                FIRST 4 CHARACTERS MUST BE ALPHA             
*NOP*    BH    EDTADER             NO GOOD                                      
*NOP*    CLI   0(R4),C'0'                                                       
*NOP*    BL    EDTADER             NO GOOD                                      
*NOP*    CLI   0(R4),C'9'                                                       
*NOP*    BH    EDTADER             NO GOOD                                      
EDTAD20  DS    0H                                                               
         LA    R4,1(R4)            BUMP TO NEXT CHARACTER                       
         BCT   R5,EDTAD10                                                       
         B     EDTADOK             NO EDIT ERRORS                               
*                                                                               
EDTADER  DS    0H                  ERROR EXIT                                   
         CR    RB,RB               SET CC EQUAL                                 
         B     EDTADXIT                                                         
*                                                                               
EDTADOK  DS    0H                                                               
         LTR   RB,RB               SET CC NOT EQUAL                             
         B     EDTADXIT                                                         
*                                                                               
EDTADXIT DS    0H                                                               
         XIT1  REGS=(R3)           "MAY" NEED R3 FOR ERROR NO                   
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK FOR CALL FROM LINK TO INSERTION ORDER                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKADBYER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    VGLOBBER,VGLOBBER   MUST HAVE GLOBBER ADDRESS                    
         BZ    CKCALLER            RETURN NO CALLS                              
*                                                                               
         XC    GLOBWORK,GLOBWORK                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',GLOBWORK,GLVXLENQ,GLVXCTL                 
         CLI   DMCB+8,GLEGNF                                                    
         BE    CKCALLER            RETURN NO CALLS                              
         CLI   DMCB+8,0            CAN'T HANDLE OTHER ERRORS                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GLOBWORK(12),=C'PRILINPRIADF'                                    
         BNE   CKCALLER                                                         
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         XC    LIOBD(LIOBVIS-LIOBD),LIOBD                                       
         LA    R0,LIOBD+L'LIOB                                                  
         ST    R0,LIOBAREC                                                      
         AHI   R0,3000             MAX SIZE FOR LINKIO REC USES                 
         ST    R0,LIOBABUF                                                      
         MVC   LIOBACOM,ACOMFACS                                                
         LA    RF,MAP                                                           
         STCM  RF,15,LIOBAMAP                                                   
         MVI   LIOBMSYS,4          PRINT SYSTEM MSGS                            
         LA    R0,JOBEXWKD                                                      
         ST    R0,LIOBASB1                                                      
         LA    R0,T40FFFD                                                       
         ST    R0,LIOBASB2                                                      
         OI    LIOBINDS,LIOBIMLT+LIOBINRM                                       
         LHI   RE,2997             MAXIMUM REPLY RECORD LENGTH                  
         STCM  RE,3,LIOBRECL                                                    
*                                                                               
         GOTOR ALINKIO,DMCB,('LIOAINI',LIOBD)                                   
         BNE   CKCALLER                                                         
         OI    ADBSW,AS_ADBRQ      INDICATE CALLED BY ADBUYER                   
*                                                                               
CKCALLX  J     SETCCEQ             EQUAL                                        
*                                                                               
CKCALLER J     SETCCNEQ            NOT EQUAL (NO CALLS FROM DDLINK)             
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETADFLD NTR1  BASE=*,LABEL=*      SET AD CODE FIELDS                           
*                                                                               
         CLC   =C'ADD',JOBACT      ADD ACTION?                                  
         BNE   *+8                                                              
         OI    ADBSW,AS_ADD_Q                                                   
         CLC   =C'CHA',JOBACT      CHA ACTION?                                  
         BNE   *+8                                                              
         OI    ADBSW,AS_CHA_Q                                                   
         CLC   =C'DEL',JOBACT      DEL ACTION?                                  
         BNE   *+8                                                              
         OI    ADBSW,AS_DEL_Q                                                   
*                                                                               
         L     R3,A_RECTYP         TYPE OF AD FILE RECORD TO UPLOAD             
         CLI   6(R3),1             AD CODE RECORD?                              
         BNE   S_ADF04                                                          
         MVC   FULL,=X'D9040FFE'                                                
         BRAS  RE,S_SCRCOV                                                      
         MVI   SCRNTYP,1                                                        
         B     S_ADF20                                                          
S_ADF04  CLI   6(R3),2             INSTRUCTION RECORD?                          
         BNE   S_ADF05                                                          
         MVC   FULL,=X'D9040FFD'                                                
         BRAS  RE,S_SCRCOV                                                      
         MVI   SCRNTYP,2                                                        
         B     S_ADF20                                                          
S_ADF05  CLI   6(R3),3             SUB AD CODE INFORMATION?                     
         BNE   S_ADF10                                                          
         MVC   FULL,=X'D9040FFB'                                                
         BRAS  RE,S_SCRCOV                                                      
         MVI   SCRNTYP,3                                                        
         B     S_ADF20                                                          
*                                                                               
S_ADF10  DC    H'0'                NO OTHER RECORD TYPE YET                     
*                                                                               
S_ADF20  MVC   ABRECTYP,6(R3)      SAVE UPLOAD RECORD TYPE                      
         XC    JOBACT,JOBACT                                                    
         MVI   JOBACTH+5,3                                                      
         TM    ADBSW,AS_ADD_Q      ADD ACTION?                                  
         BZ    *+10                                                             
         MVC   JOBACT(3),=C'ADD'                                                
         TM    ADBSW,AS_CHA_Q      CHA ACTION?                                  
         BZ    *+14                                                             
         MVC   JOBACT(3),=C'CHA'                                                
         MVI   CHGSW,1             SET TO ALLOW CHANGE                          
         TM    ADBSW,AS_DEL_Q      DEL ACTION?                                  
         BZ    *+14                                                             
         MVC   JOBACT(3),=C'DEL'                                                
         MVI   CHGSW,1             SET TO ALLOW CHANGE                          
         OI    JOBACTH+6,X'80'     DISPLAY ACTION                               
*                                                                               
         CLI   ABRECTYP,ABRTADCQ   AD CODE RECORD?                              
         BNE   S_ADF30                                                          
         LA    R2,JOBCAP1H         CAPTION 1                                    
         L     R3,A_ADCAP1                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBCAP2H         CAPTION 2                                    
         L     R3,A_ADCAP2                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBCPYH          COPY NUMBER                                  
         L     R3,A_CPYNUM                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBSPCH          SPACE DESCRIPTION                            
         L     R3,A_SPCDES                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBLINH          NUMBER OF LINES                              
         L     R3,A_NUMLIN                                                      
         BRAS  RE,FILL_FLD                                                      
*                                                                               
         LA    R2,JOBCOLH          NUMBER OF COLUMNS                            
         L     R3,A_NUMCOL                                                      
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,F_NUMRIC                                                      
*                                                                               
         LA    R2,JOBPRMH          PREMIUM                                      
         L     R3,A_PREMIU                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBSTAH          START DATE                                   
         L     R3,A_STRDAT                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBENDH          END DATE                                     
         L     R3,A_ENDDAT                                                      
         BRAS  RE,FILL_FLD                                                      
*                                                                               
         LA    R2,JOBPRODH         PRODUCTION HOUSE                             
         L     R3,A_PRDHOU                                                      
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,F_NUMRIC                                                      
*                                                                               
         LA    R2,JOBSIGH          AGENCY SIGNATURE                             
         L     R3,A_AGYSIG                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBPJOBH         PRODUCTION JOB                               
         L     R3,A_PRDJOB                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBALOH          ALLOCATION                                   
         L     R3,A_PRALLC                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBBLCCH         BILLING CONTACT                              
         L     R3,A_BILCON                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBFILTH         FILTER                                       
         L     R3,A_ADFILT                                                      
         BRAS  RE,FILL_FLD                                                      
         LA    R2,JOBFSIH          FREE STANDING INSERT AD? Y/N                 
         L     R3,A_FSI_YN                                                      
         BRAS  RE,FILL_FLD                                                      
*                                                                               
         LA    R2,JOBBREPH         BILLING REP                                  
         L     R3,A_BILREP                                                      
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,F_NUMRIC                                                      
*                                                                               
         LA    R2,JOBPLISH         PUBLICATION LIST                             
         L     R3,A_PUBLST                                                      
         BRAS  RE,FILL_FLD                                                      
         B     S_ADF80                                                          
*                                                                               
S_ADF30  CLI   ABRECTYP,ABRTINSQ   INSTRUCTION RECORD?                          
         BNE   S_ADF40                                                          
         LA    R2,JOBPUBH                                                       
         L     R3,A_PUBCOD                                                      
         LTR   R3,R3                                                            
         BNZ   S_ADF32                                                          
         XC    WORK,WORK                                                        
         LA    R3,WORK                                                          
         MVI   1+1(R3),6+3         CREATE A DUMMY ALL PUB FLD                   
         MVC   6(3,R3),=C'ALL'                                                  
S_ADF32  BRAS  RE,FILL_FLD                                                      
         L     R3,A_COMLIN                                                      
         LTR   R3,R3                                                            
         BZ    S_ADF80                                                          
         LA    R2,JOBINS1H         POINT TO 1ST COMMENT LINE                    
S_ADF34  LA    RE,JOBINSLH         POINT TO LAST COMMENT LINE                   
         CR    R2,RE                                                            
         BH    S_ADF80                                                          
         CLI   0(R3),LQ_RQSTQ                                                   
         BNE   S_ADF80                                                          
         CLC   =AL2(D#COMMNT),3(R3)                                             
         BNE   *+12                                                             
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,BUMPFLD                                                       
         BRAS  RE,NXTWFELM         NEXT MAP CODE ELEM IN WORK RECORD            
         B     S_ADF34                                                          
*                                                                               
S_ADF40  CLI   ABRECTYP,ABRTSUBQ   SUB AD CODE INFORMATION?                     
         BNE   S_ADF60                                                          
         L     R3,A_SUBADC                                                      
         LTR   R3,R3                                                            
         BZ    S_ADF80                                                          
         LA    R2,JOBCOD1H         POINT TO 1ST AD CODE/AD ID FLD               
S_ADF44  LA    RE,JOBPCTAH         POINT TO LAST % SHARE FLD                    
         CR    R2,RE                                                            
         BH    S_ADF80                                                          
         CLI   0(R3),LQ_RQSTQ                                                   
         BNE   S_ADF80                                                          
         CLC   =AL2(D#SUBADC),3(R3)                                             
         BE    *+14                                                             
         CLC   =AL2(D#SUBADI),3(R3)                                             
         BNE   S_ADF80                                                          
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,NXTWFELM         NEXT MAP CODE ELEM IN WORK RECORD            
         BRAS  RE,BUMPFLD                                                       
         CLI   0(R3),LQ_RQSTQ                                                   
         BNE   S_ADF80                                                          
         CLC   =AL2(D#PCTSHR),3(R3)                                             
         BNE   *+12                                                             
         BRAS  RE,FILL_FLD                                                      
         BRAS  RE,NXTWFELM         NEXT MAP CODE ELEM IN WORK RECORD            
         BRAS  RE,BUMPFLD                                                       
         B     S_ADF44                                                          
*                                                                               
S_ADF60  DC    H'0'                NO OTHER RECORD TYPE YET                     
*                                                                               
S_ADF80  DS    0H                                                               
*                                                                               
         J     X_XIT1                                                           
*                                                                               
S_SCRCOV LR    R0,RE                                                            
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),FULL                                                   
         GOTO1 VCALLOV,DMCB,JOBLAST                                             
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
FILL_FLD LR    R0,RE                                                            
         LTR   R3,R3                                                            
         BZ    F_FLD20                                                          
         ZICM  RF,1(R3),(3)        TWO BYTES LENGTH                             
         AHI   RF,-6               MINUS OVER HEAD                              
         BP    F_FLD30                                                          
*                                                                               
F_FLD20  ZIC   RF,0(R2)            TOTAL FIELD LENGTH                           
         BZ    F_FLD_X             DONE IF SCREEN END REACHED                   
         AHI   RF,-8               HEADER LENGTH                                
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BZ    *+8                                                              
         AHI   RF,-8               EXTENSION LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         MVI   4(R2),0             CLEAR INPUT INDICATORS                       
         MVI   5(R2),0             LENGTH 0                                     
         NI    6(R2),X'BF'         UNSET CURSOR                                 
         B     F_FLD_X                                                          
*                                                                               
F_FLD30  ZIC   RE,0(R2)            COMPUTE MAX FLD LENGTH                       
         AHI   RE,-8               MAX FLD MINUS HEADER                         
         TM    1(R2),X'02'         EXTENDED HEADER?                             
         BNO   *+8                                                              
         AHI   RE,-8               MINUS EXTENSION LENGTH                       
*                                                                               
         CR    RF,RE               DATA LENGTH > THAN MAX FLD LENGTH?           
         BNH   *+6                                                              
         LR    RF,RE               CAN ONLY MOVE MAX FLD LENGTH                 
*                                                                               
         BCTR  RE,0                FOR EX INSTRUCTION                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD FIRST                            
         NI    4(R2),X'FF'-X'20'   RESET PREVIOUSLY VALIDATED                   
*                                                                               
         STC   RF,5(R2)            INPUT LENGTH                                 
         BCTR  RF,0                FOR EX INSTRUCTION                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),6(R3)       FIELD DATA                                   
*                                                                               
F_FLD_X  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
F_NUMRIC LR    R0,RE                                                            
         CLI   5(R2),0             ANY INPUTS?                                  
         JNH   F_NUMR_X                                                         
         ZIC   RF,5(R2)                                                         
         GOTOR VCASHVAL,DMCB,(0,8(R2)),(X'40',(RF))                             
         CLI   0(R1),X'FF'                                                      
         JE    *+8                                                              
         OI    4(R2),X'08'         VALID NUMERIC FOR COLUMNS                    
F_NUMR_X LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETRPLYR NTR1  BASE=*,LABEL=*      SET AD FILE UPLOAD REPLY RECORD              
*                                                                               
         L     R3,AWRKREC                                                       
         USING LIOBD,R3            LINKIO INTERFACE BLOCK                       
         CLI   ERRAREA,X'FF'       ANY ERRORS?                                  
         BE    SETRR60                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#ADFRPY)              
         BRAS  RE,R_AD_KEY                                                      
         B     SETRR_X                                                          
*                                                                               
SETRR60  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',E#ADFERR)              
         BRAS  RE,R_AD_KEY                                                      
*                                                                               
         BRAS  RE,R_ERRMAP                                                      
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRNUM),    +        
               ('LD_UBINQ',SVERRFLD),(L'SVERRFLD,0)                             
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ERRDSC),    +        
               ('LD_CHARQ',JOBMSG),(L'JOBMSG,0)                                 
*                                                                               
SETRR_X  GOTOR ALINKIO,DMCB,('LIOACLO',LIOBD)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         J     X_XIT1                                                           
*                                                                               
R_AD_KEY LR    R0,RE                                                            
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#MEDCOD),    +        
               ('LD_CHARQ',JOBMED),(L'JOBMED,0)                                 
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#CLTCOD),    +        
               ('LD_CHARQ',JOBCLT),(L'JOBCLT,0)                                 
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PRDCOD),    +        
               ('LD_CHARQ',JOBPRD),(L'JOBPRD,0)                                 
         CLC   JOBJOB,SPACES                                                    
         JNH   R_ADK10                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#ADCODE),    +        
               ('LD_CHARQ',JOBJOB),(L'JOBJOB,0)                                 
         J     R_ADK20                                                          
R_ADK10  GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#AD_ID),     +        
               ('LD_CHARQ',JOBADID),(L'JOBADID,0)                               
R_ADK20  CLI   ABRECTYP,ABRTINSQ                                                
         JNE   R_ADK_X                                                          
         GOTOR ALINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#PUBCOD),    +        
               ('LD_CHARQ',JOBPUB),(L'JOBPUB,0)                                 
R_ADK_X  LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
R_ERRMAP LR    R0,RE                                                            
         OC    SVERRCUR,SVERRCUR                                                
         JZ    *+8                                                              
         ICM   R2,15,SVERRCUR                                                   
         LA    RE,ERRF_HDR         CK HEADER FLDS                               
R_ERRM10 CLC   =AL2(0),0(RE)       END OF TABLE?                                
         JE    R_ERRM20                                                         
         ZICM  RF,2(RE),(3)                                                     
         AR    RF,RA               ADDRESS OF SCREEN FLD                        
         CR    R2,RF               ERR FLD ADDRESS MATCH THAT IN TABLE?         
         JE    R_ERRM80                                                         
         LA    RE,2+2(RE)          NEXT ENTRY IN TABLE                          
         J     R_ERRM10                                                         
*                                                                               
R_ERRM20 CLI   ABRECTYP,ABRTADCQ                                                
         JNE   *+8                                                              
         LA    RE,ERRF_ADC         CK AD CODE FLDS                              
         CLI   ABRECTYP,ABRTINSQ                                                
         JNE   *+8                                                              
         LA    RE,ERRF_INS         CK INSTRUCTION FLDS                          
         CLI   ABRECTYP,ABRTSUBQ                                                
         JNE   *+8                                                              
         LA    RE,ERRF_SUB         CK SUB AD CODE INFORMATION FLDS              
R_ERRM24 CLC   =AL2(0),0(RE)       END OF TABLE?                                
         JE    R_ERRM84                                                         
         ZICM  RF,2(RE),(3)                                                     
         AR    RF,RA               ADDRESS OF SCREEN FLD                        
         CLI   ABRECTYP,ABRTADCQ   AD CODE RECORD?                              
         JNE   R_ERRM26                                                         
         CR    R2,RF               ERR FLD ADDRESS MATCH THAT IN TABLE?         
         JE    R_ERRM80                                                         
         J     R_ERRM28                                                         
R_ERRM26 CR    R2,RF               ERR FLD ADDRESS IS A REPEATING FLD?          
         JNL   R_ERRM80                                                         
R_ERRM28 LA    RE,2+2(RE)          NEXT ENTRY IN TABLE                          
         J     R_ERRM24                                                         
*                                                                               
R_ERRM80 MVC   SVERRFLD,0(RE)      ERROR MAP CODE                               
R_ERRM84 OC    SVERRFLD,SVERRFLD                                                
         JNZ   *+10                                                             
         MVC   SVERRFLD,=AL2(D#DACTN)                                           
R_ERRM_X LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ERRF_HDR DS    0H                                                               
         DC    AL2(D#MEDCOD),AL2(JOBMEDH-T40FFFD)                               
         DC    AL2(D#CLTCOD),AL2(JOBCLTH-T40FFFD)                               
         DC    AL2(D#PRDCOD),AL2(JOBPRDH-T40FFFD)                               
         DC    AL2(D#ADCODE),AL2(JOBJOBH-T40FFFD)                               
         DC    AL2(D#AD_ID),AL2(JOBADIDH-T40FFFD)                               
         DC    AL2(D#DACTN),AL2(JOBACTH-T40FFFD)                                
         DC    AL2(D#PUBCOD),AL2(JOBPUBH-T40FFFD)                               
         DC    AL2(0)                                                           
*                                                                               
ERRF_ADC DS    0H                                                               
         DC    AL2(D#ADCAP),AL2(JOBCAP1H-T40FFFD)                               
         DC    AL2(D#ADCAP2),AL2(JOBCAP2H-T40FFFD)                              
         DC    AL2(D#CPYNUM),AL2(JOBCPYH-T40FFFD)                               
         DC    AL2(D#SPCDSC),AL2(JOBSPCH-T40FFFD)                               
         DC    AL2(D#NUMLIN),AL2(JOBLINH-T40FFFD)                               
         DC    AL2(D#NUMCOL),AL2(JOBCOLH-T40FFFD)                               
         DC    AL2(D#PREMUM),AL2(JOBPRMH-T40FFFD)                               
         DC    AL2(D#STRDAT),AL2(JOBSTAH-T40FFFD)                               
         DC    AL2(D#ENDDAT),AL2(JOBENDH-T40FFFD)                               
         DC    AL2(D#PRDUCD),AL2(JOBPRODH-T40FFFD)                              
         DC    AL2(D#AGYSIG),AL2(JOBSIGH-T40FFFD)                               
         DC    AL2(D#PRDJOB),AL2(JOBPJOBH-T40FFFD)                              
         DC    AL2(D#ALLOCS),AL2(JOBALOH-T40FFFD)                               
         DC    AL2(D#BILCON),AL2(JOBBLCCH-T40FFFD)                              
         DC    AL2(D#ADFILT),AL2(JOBFILTH-T40FFFD)                              
         DC    AL2(D#FSI_YN),AL2(JOBFSIH-T40FFFD)                               
         DC    AL2(D#BILREP),AL2(JOBBREPH-T40FFFD)                              
         DC    AL2(D#PUBLST),AL2(JOBPLISH-T40FFFD)                              
         DC    AL2(0)                                                           
*                                                                               
ERRF_INS DS    0H                                                               
         DC    AL2(D#COMMNT),AL2(JOBINS1H-T40FFFD)                              
         DC    AL2(0)                                                           
*                                                                               
ERRF_SUB DS    0H                                                               
         DC    AL2(D#SUBADC),AL2(JOBCOD1H-T40FFFD)                              
         DC    AL2(D#PCTSHR),AL2(JOBPCT1H-T40FFFD)                              
         DC    AL2(0)                                                           
*                                                                               
         DROP  RB,R3                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
MAP      DC    0XL(LIORL)                                                       
         DC    AL2(M#UL_ADF,E#ADFRPY,ADFUPLD-MAP)                               
MAPX     DC    AL2(0)                                                           
*                                                                               
       ++INCLUDE PPMAPJOB          MAP CODES FOR AD FILE                        
*                                                                               
SUBERR   DC    C'** AD CODE FOR "SUB" ACTION MUST BEGIN WITH ! **'              
*                                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPJOB00D DSECT                                                                  
*                                                                               
PPJOBWKQ EQU   PPJOBWKX-GENOLD                                                  
         DS    (PPJOBWKQ)X                                                      
*                                                                               
WRKRECA  DS    (WRKRECAL)X                                                      
WRKRECAL EQU   18432                                                            
WRKRECAX EQU   *                                                                
*                                                                               
JOBEXWKL EQU   JOBEXWKX-JOBEXWKD                                                
JOBEXWKS DS    (JOBEXWKL)X                                                      
*                                                                               
PPJOB00X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
JOBEXWKD DSECT                     EXTENDED WORKING STORAGE                     
*                                                                               
A_MAPSTR DS    0H                                                               
A_PUBCOD DS    A                   ADDRESS OF PUB CODE MAP                      
A_RECTYP DS    A                   ADDRESS OF RECORD TYPE MAP                   
A_ADCAP1 DS    A                   ADDRESS OF CAPTION 1 MAP                     
A_ADCAP2 DS    A                   ADDRESS OF CAPTION 2 MAP                     
A_CPYNUM DS    A                   ADDRESS OF COPY NUMBER MAP                   
A_SPCDES DS    A                   ADDRESS OF SPACE DESCRIPTION MAP             
A_NUMLIN DS    A                   ADDRESS OF # OF LINES MAP                    
A_NUMCOL DS    A                   ADDRESS OF # OF COLUMNS MAP                  
A_PREMIU DS    A                   ADDRESS OF PREMIUM MAP                       
A_STRDAT DS    A                   ADDRESS OF START DATE MAP                    
A_ENDDAT DS    A                   ADDRESS OF END DATE MAP                      
A_PRDHOU DS    A                   ADDRESS OF PRODUCTION HOUSE MAP              
A_AGYSIG DS    A                   ADDRESS OF AGENCY SIGNATURE MAP              
A_PRDJOB DS    A                   ADDRESS OF PRODUCTION JOB MAP                
A_PRALLC DS    A                   ADDRESS OF PRD ALLOCATION MAP                
A_BILCON DS    A                   ADDRESS OF BILLING CONTACT MAP               
A_ADFILT DS    A                   ADDRESS OF FILTER MAP                        
A_FSI_YN DS    A                   ADDRESS OF FSI Y/N MAP                       
A_BILREP DS    A                   ADDRESS OF BILLING REP MAP                   
A_PUBLST DS    A                   ADDRESS OF PUBLICATION LIST MAP              
A_COMLIN DS    A                   ADDRESS OF 1ST COMMENT LINE MAP              
A_SUBADC DS    A                   ADDRESS OF 1ST SUB AD CODE MAP               
A_SUBPCT DS    A                   ADDRESS OF 1ST SUB AD CODE % MAP             
A_MAPEND DS    0H                                                               
A_MAPLNQ EQU   A_MAPEND-A_MAPSTR   TOTAL LENGTH OF DATA MAPS                    
A_MAP_#Q EQU   A_MAPLNQ/4          NUMBER OF DATA MAPS                          
*                                                                               
TRTMPREC DS    400F                                                             
*                                                                               
JOBEXWKX EQU   *                                                                
*                                                                               
       ++INCLUDE PPJOBWRK          DSECT AREAS START HERE                       
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDOFFICED                                                      
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE PGENGRP                                                        
*                                                                               
       ++INCLUDE CTGENFILE         FOR SECURITY CHECKINGS                       
*                                                                               
       ++INCLUDE FATWA             FOR LIMIT ACCESS ("SPECIAL-APR/02")          
*                                                                               
       ++INCLUDE PPERREQUS         (P)RINT SYSTEM ERROR MSG EQUATES             
*                                                                               
       ++INCLUDE FASECRETD                                                      
*                                                                               
       ++INCLUDE DDGLVXCTLD        DSECT FOR TRANSFER CONTROLS                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBAL VARIABLES                   
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046PPJOB00   07/07/14'                                      
         END                                                                    
