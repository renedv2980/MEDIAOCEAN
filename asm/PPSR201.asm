*          DATA SET PPSR201    AT LEVEL 006 AS OF 03/26/08                      
*PHASE T42101A                                                                  
*INCLUDE SRCHCALL                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T42101- ENHANCED SPACE RESERVATION - SCREEN VALIDATION'         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 05/30/07 ALLOW STEWARDSHIP ESTIMATES                                     
*                                                                               
* SMYE 10/21/05 DEACTIVATE SPECIAL MINDSHARE PRD SECURITY IN PRD                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T42101   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PPSR201X-PPSR201D,T42101,RR=R2,CLEAR=YES                         
*                                                                               
         LR    R7,RC                                                            
         USING PPSR201D,R7                                                      
*                                                                               
         ST    R2,RELO01                                                        
*                                                                               
         L     RC,0(R1)                                                         
         USING POLWRKD,RC                                                       
         USING T421FFD,RA                                                       
         USING ESRWORKD,R8                                                      
*                                                                               
         LA    R6,1(R9)                                                         
         LA    R6,4095(R6)                                                      
         USING POLFILE,R9,R6                                                    
*                                                                               
         BRAS  RE,INITL2                                                        
         MVI   MAXLINES,75                                                      
         MVI   RCWRITE,C'Y'                                                     
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         XC    PRQMSG,PRQMSG                                                    
         OI    PRQMSGH+6,X'80'                                                  
*                                                                               
         MVC   ETODAY+0(2),RCDATE+6                                             
         MVC   ETODAY+2(2),RCDATE+0                                             
         MVC   ETODAY+4(2),RCDATE+3                                             
         GOTO1 DATCON,DMCB,(0,ETODAY),(3,BTODAY)                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,ETODAY),(5,CTODAY)                                
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         OI    DMOUTBTS,X'FD'                                                   
*                                                                               
         MVC   QRECORD,SPACES                                                   
         MVC   QAGENCY,AGYALPHA                                                 
*                                                                               
         LA    R2,PRQUIDH          VALIDATE ID                                  
         LA    R3,INVERR                                                        
         LHI   RF,D#REQID                                                       
         STCM  RF,3,SVERRFLD                                                    
         CLI   5(R2),3                                                          
         BE    MED                 OK                                           
         CLI   5(R2),2                                                          
         BL    ERROR                                                            
         MVI   10(R2),C'.'         MOVE "PERIOD" TO BLANK 3RD POSITION          
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE MEDIA                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
MED      LA    R2,PRQMEDH                                                       
         LA    R3,MEDERR                                                        
         LHI   RF,D#MEDCOD                                                      
         STCM  RF,3,SVERRFLD                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),AGYALPHA                                                  
         MVC   KEY+2(1),PRQMED                                                  
         MVI   KEY+3,1                                                          
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         MVC   AGYDA,KEY+27        SAVE DA                                      
         LA    R0,PAGYREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   QMEDIA,PRQMED                                                    
*                                                                               
         LA    R4,TMPWKAIO                                                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),T421FFD+10                                           
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'CTFILE',CTIREC,CTIREC                   
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,CTIDATA                                                       
MED5     CLI   0(R5),X'36'                                                      
         BE    MED8                                                             
         CLI   0(R5),0             EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     MED5                                                             
*                                                                               
         USING CTORGD,R5                                                        
MED8     MVC   SVAGYNAM,CTORGNAM                                                
         MVC   SVAGYADR,CTORGADD                                                
         DROP  R5                                                               
*                                                                               
         LA    R4,TMPWKAIO                                                      
         LA    R5,CTIDATA                                                       
MED8A    CLI   0(R5),X'02'                                                      
         BE    MED8D                                                            
         CLI   0(R5),0             EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     MED8A                                                            
*                                                                               
         USING CTDSCD,R5                                                        
MED8D    MVC   AGYORIG,CTDSC       ORIGIN ID - NEEDED FOR WESTERN UNION         
         DROP  R5,R4                                                            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLT      LA    R2,PRQCLTH                                                       
         LA    R3,CLTERR                                                        
         LHI   RF,D#CLTCOD                                                      
         STCM  RF,3,SVERRFLD                                                    
         OC    PRQCLT,SPACES                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY                                                  
         MVI   KEY+3,2                                                          
         MVC   KEY+4(3),PRQCLT                                                  
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
         LA    RE,PBUYREC          INIT SECBLK (TEMP USE)                       
         LHI   RF,1024                                                          
         XCEFL                                                                  
*                                                                               
         OC    4(2,RA),4(RA)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RA),6(RA)       TEST ANY LIMIT ACCESS                        
         BZ    CLT2M                                                            
         L     RF,VCOMFACS                                                      
         L     RF,CSECRET-COMFACSD(RF)                                          
         LA    R3,PBUYREC                                                       
         GOTO1 (RF),DMCB,('SECPINIT',(R3)),0                                    
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
CLT2M    MVC   BYTE2,PCLTOFF       SAVE ORIGINAL CLT OFF CODE                   
         LA    R3,CACCERR                                                       
         LA    R4,T421FFD+6                                                     
         OC    0(2,R4),0(R4)                                                    
         BZ    CLT4                NO RESTRICTIONS                              
*                                                                               
         BRAS  RE,CKTRAFID         TRAFFIC ID SIGN-ON?                          
         BNE   CLT3                NO                                           
         BRAS  RE,TRAFFACC         LOOK FOR CLIENT TRAFFIC OFFICE CODE          
         CLI   BYTE3,0             ANYTHING FOUND?                              
         BE    CLT3                NO                                           
         MVC   PCLTOFF,BYTE3       USE CLIENT TRAFFIC OFFICE CODE               
*                                                                               
CLT3     XC    WORK,WORK           WORK MUST BE AT LEAST 48 BYTES               
         LA    R1,WORK             (LENGTH OF OFFICED IS 48 BYTES)              
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         MVC   OFCAUTH,6(RA)                                                    
         MVC   OFCAGY,AGYALPHA                                                  
         MVC   OFCOFC,PCLTOFF                                                   
         MVC   OFCCLT,PCLTKCLT                                                  
         OC    OFCCLT,=3C' '                                                    
         MVC   OFCPMED,PCLTKMED                                                 
         MVC   OFCLMT(4),6(RA)                                                  
         LA    RE,PBUYREC                                                       
         STCM  RE,15,OFCSECD       ADDRESS OF SECRET BLOCK                      
         DROP  R1                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),255           ADDRESS OF OFFICER FOUND?                    
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',WORK),VCOMFACS                                   
         CLI   0(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
CLT4     MVC   PCLTOFF,BYTE2       RESTORE CLT OFF CODE                         
         BRAS  RE,CLRCLT                                                        
         MVC   PRQCLTN,PCLTNAME                                                 
         OI    PRQCLTNH+6,X'80'                                                 
         BRAS  RE,SETPROFV         SET PROFILE VALUES                           
         MVC   QCLIENT,PRQCLT                                                   
*                                                                               
         CLI   SHWUIDAD,C'Y'       SHOW ADDRESS FROM USER ID?                   
         BNE   CLT6                                                             
         MVC   PAGYNAME,SVAGYNAM                                                
         MVC   PAGYADDR,SVAGYADR                                                
*                                                                               
CLT6     DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRD      LA    R2,PRQPRDH                                                       
         LA    R3,PRDERR                                                        
         LHI   RF,D#PRDCOD                                                      
         STCM  RF,3,SVERRFLD                                                    
         CLI   PRQPRD,C'*'         DON'T ALLOW OTHER AGY PRD                    
         BE    ERROR                                                            
         OC    PRQPRD,SPACES                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(7),PCLTKEY                                                   
         MVI   KEY+3,6                                                          
         MVC   KEY+7(3),PRQPRD                                                  
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
*                                                                               
         B     PRDOK               BRANCH AROUND DISCONTINUED LOGIC             
* 10/21/05 - BELOW LOGIC DISCONTINUED AT AGENCY REQUEST                         
*                                                                               
         LR    RE,RA               SPECIAL MINDSHARE PRD SECUTITY               
         USING TWAD,RE                                                          
         ZICM  RF,TWAUSRID,2                                                    
         DROP  RE                                                               
         CHI   RF,10296            "MSSROC" USER ID ?                           
         BE    PRDCKB              YES                                          
         CHI   RF,10297            "MSSRYC" USER ID ?                           
         BE    PRDCKC              YES                                          
         B     PRDOK                                                            
*                                                                               
PRDCKB   DS    0H                                                               
         CLI   PPRDTRAF,C'6'       TRAFFIC OFFICE = 6 ?                         
         BE    PRDOK               YES - OK                                     
         B     PRDCKNG             SECURITY LOCKOUT                             
*                                                                               
PRDCKC   DS    0H                                                               
         CLI   PPRDTRAF,C'7'       TRAFFIC OFFICE = 7 ?                         
         BE    PRDOK               YES - OK                                     
PRDCKNG  LA    R3,PPESECLK         SECURITY LOCKOUT                             
         B     ERROR                                                            
*                                                                               
PRDOK    DS    0H                                                               
         BRAS  RE,CLRPRD                                                        
         MVC   PRQPRDN,PPRDNAME                                                 
         OI    PRQPRDNH+6,X'80'                                                 
         MVC   QPRODUCT,PRQPRD                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
EST      LA    R2,PRQESTH                                                       
         LA    R3,ESTERR                                                        
         LHI   RF,D#ESTNUM                                                      
         STCM  RF,3,SVERRFLD                                                    
         XC    PRQESTN,PRQESTN                                                  
         OI    PRQESTNH+6,X'80'                                                 
*                                                                               
         OC    PRQEST,SPACES                                                    
         CLC   PRQEST,SPACES                                                    
         BNE   EST2                                                             
         MVC   PRQEST,=C'ALL'                                                   
         OI    PRQESTH+6,X'80'                                                  
*                                                                               
EST2     CLC   PRQEST,=C'ALL'                                                   
         BE    EST3                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(10),PPRDKEY                                                  
         MVI   KEY+3,7                                                          
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         CHI   R1,0                                                             
         BNH   EST2H                                                            
         TM    4(R2),X'08'                                                      
         BZ    EST2H                       OR NON NUMERIC                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
EST2H    STCM  R0,3,KEY+10                                                      
         GOTOR HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    R0,PESTREC                                                       
         ST    R0,AREC                                                          
         GOTOR GETPRT                                                           
         TM    PESTTEST,X'80'          SEE IF TEST ESTIMATE                     
         BZ    EST3H                                                            
         TM    PESTTEST,X'40'          STEWARDSHIP ESTIMATE?                    
         BNZ   EST3H                                                            
         MVC   PRQESTN(20),=C'*THIS IS A TEST EST*'                             
         OI    PRQESTNH+6,X'80'                                                 
         LA    R3,INVERR               NO INS ORDERS FOR TEST ESTS              
         B     ERROR                                                            
*                                                                               
EST3     XC    PRQESTN,PRQESTN                                                  
         MVC   PRQESTN(13),=C'ALL ESTIMATES'                                    
         OI    6(R2),X'80'                                                      
         B     EST4                                                             
*                                                                               
EST3H    UNPK  PRQEST,DUB                                                       
         OI    PRQESTH+6,X'80'                                                  
         MVC   PRQESTN,PESTNAME                                                 
         OI    PRQESTNH+6,X'80'                                                 
*                                                                               
EST4     MVC   QEST,PRQEST                                                      
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE PUBLICATION                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUB      MVI   ZEOPT,C'N'          MULTI-E/Z OPT                                
         LA    R2,PRQPUBH                                                       
*                                                                               
* NAME SEARCH CALL                                                              
*                                                                               
         SR    R2,RA               GET DISPLACEMENT INTO TWA OF PUB             
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,PRQMED                                                  
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),VCOMFACS,       +        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO01                       
         DROP  R3                                                               
*                                                                               
         LA    R2,PRQPUBH                                                       
         LA    R3,PUBERR                                                        
         LHI   RF,D#PUBCOD                                                      
         STCM  RF,3,SVERRFLD                                                    
         OC    PRQPUB,SPACES                                                    
         CLC   PRQPUB,SPACES                                                    
         BE    ERROR                                                            
         CLC   PRQPUB(3),=C'ALL'                                                
         BNE   PUB2                                                             
         OI    PRQPUBH+6,X'80'                                                  
         XC    PRQPUBN,PRQPUBN                                                  
         OI    PRQPUBNH+6,X'80'                                                 
*                                                                               
PUB2     SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         MVC   WORK(17),PRQPUB                                                  
         LA    RF,WORK                                                          
         AR    RF,R0                                                            
         AHI   RF,-4                                                            
         CLC   0(4,RF),=C',ALL'                                                 
         BNE   PUB3                                                             
*                                                                               
         CLC   PRQPUB(3),=C'ALL'   DON'T ALLOW 'ALL,ALL'                        
         BE    ERROR                                                            
*                                                                               
         MVC   0(4,RF),SPACES                                                   
         AHI   R0,-4                                                            
         MVI   ZEOPT,C'Y'          SET DOING MULTI EDITS/ZONES                  
*                                                                               
PUB3     XC    BPUB,BPUB                                                        
         XC    PUBREC(50),PUBREC                                                
         XC    PRQPUBN,PRQPUBN                                                  
         CLC   WORK(3),=C'ALL'                                                  
         BE    PUB3D                                                            
*                                                                               
         GOTOR APUBVAL,DMCB,((R0),WORK),DUB                                     
*                                                                               
         MVC   BPUB,DUB                                                         
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         BRAS  RE,FNDPUB                                                        
         BNE   ERROR                                                            
*                                                                               
         MVC   PRQPUBN,PUBNAME                                                  
PUB3D    OI    PRQPUBNH+6,X'80'                                                 
*                                                                               
         MVC   QPUB(3),=C'ALL'                                                  
         CLC   PRQPUB(3),=C'ALL'                                                
         BE    PER                                                              
         GOTO1 APUBEDIT,DMCB,BPUB,(C'Q',QPUB)                                   
         CLI   ZEOPT,C'Y'                                                       
         BNE   *+10                                                             
         MVC   QPUB+8(3),=C'ZZZ'   INDICATE MULTIPLE ZONES/EDITIONS             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE START AND END DATES                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PER      LA    R2,PRQPERH                                                       
         LA    R3,DATERR                                                        
         LHI   RF,D#STEND                                                       
         STCM  RF,3,SVERRFLD                                                    
         GOTO1 DATVAL,DMCB,8(R2),QSTART                                         
*                                                                               
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         ZIC   R0,5(R2)                                                         
         S     R0,DMCB             SUBTRACT LENGHT OF FIRST DATE                
         CHI   R0,0                                                             
         BE    PER6                                                             
         CHI   R0,4                                                             
         BL    PER4                LINE NUMBER GIVEN                            
         L     R5,DMCB                                                          
         LA    R5,9(R2,R5)                                                      
         GOTO1 (RF),(R1),(R5),QEND                                              
*                                                                               
         CLI   DMCB+3,0                                                         
         BE    ERROR                                                            
         B     PER8                                                             
*                                                                               
PER4     L     R5,DMCB                                                          
         LA    R5,9(R2,R5)                                                      
         CLC   PRQEST(3),=C'ALL'                                                
         BE    ERROR                                                            
         MVC   QBUYLIN+1(1),0(R5)                                               
         CLI   0(R5),C'0'                                                       
         BL    ERROR               CHK FOR NUMERICS                             
         CLI   0(R5),C'9'                                                       
         BH    ERROR                                                            
         CHI   R0,2                SEE IF -N ENTERED                            
         BNE   PER5                                                             
         MVI   QBUYLIN,C'0'                                                     
         B     PER6                                                             
*                                                                               
PER5     CHI   R0,3                SEE IF -NN ENTERED                           
         BNE   ERROR                                                            
         CLI   1(R5),C'0'                                                       
         BL    ERROR               CHK FOR NUMERICS                             
         CLI   1(R5),C'9'                                                       
         BH    ERROR                                                            
         MVC   QBUYLIN,0(R5)                                                    
*                                                                               
PER6     MVC   QEND,QSTART                                                      
*                                                                               
PER8     GOTO1 DATCON,DMCB,(0,QSTART),(3,BSTART)                                
*                                                                               
         GOTO1 DATCON,(R1),(0,QEND),(3,BEND)                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATION TEST RUN FIELD                                                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRUN     LA    R2,PRQRUNH          EDIT TEST RUN FIELD                          
         LA    R3,INVERR                                                        
         LHI   RF,D#TSTYN                                                       
         STCM  RF,3,SVERRFLD                                                    
         CLI   5(R2),0                                                          
         BE    TRUN5                                                            
         CLI   8(R2),C'N'                                                       
         BE    TRUN5               LIVE RUN                                     
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR                                                            
         MVI   RCWRITE,C'N'        DON'T MARK FILE                              
         B     TRUNX                                                            
*                                                                               
TRUN5    L     RF,VCOMFACS         "NON-UPDATIVE" CHECK                         
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,X'E0'       READ ONLY OR WRONG FACPAK?                   
         BZ    TRUNX                                                            
         LA    R3,NOTUPD           NO UPDATES IN THIS ONLINE APPL               
         J     GET_ETXT                                                         
*                                                                               
TRUNX    DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE STANDARD COMMENT CODE                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
STDCOMCD LA    R3,INVCOM#Q                                                      
         LHI   RF,D#STDCOM                                                      
         STCM  RF,3,SVERRFLD                                                    
         MVC   SVSTDCMC,SPACES                                                  
         CLI   PRQSCCH+5,0         COMMENT CODE PRESENT?                        
         BE    STDCOMCX                                                         
         CLI   PRQSCCH+5,L'PCOMKNUM                                             
         BH    ERROR                                                            
*                                                                               
         SR    RE,RE                                                            
         IC    RE,PRQSCCH+5                                                     
         LA    RF,SVSTDCMC                                                      
         LHI   R1,L'PCOMKNUM                                                    
         SR    R1,RE                                                            
         AR    RF,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),PRQSCC      RIGHT ALIGNED, SPACE PADDED COM CODE         
*                                                                               
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PCOMKEY,RE                                                       
         MVC   PCOMKAGY,QAGENCY                                                 
         MVC   PCOMKMED,QMEDIA                                                  
         MVI   PCOMKRCD,X'40'      STANDARD COMMENT RECORD CODE                 
         MVC   PCOMKNUM,SVSTDCMC                                                
*                                                                               
         GOTOR HIGH                                                             
         CLC   KEY(L'PCOMKEY),KEYSAVE                                           
         BNE   ERROR                                                            
*                                                                               
STDCOMCX DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* VALIDATE ESR COMMENTS ON SCREEN                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SR2COM   DS    0H                                                               
         XC    SR2COMM,SR2COMM                                                  
         CLI   PRQCOM1H+5,0                                                     
         BNH   *+10                                                             
         MVC   SR2COMM+000(50),PRQCOM1                                          
*                                                                               
         CLI   PRQCOM2H+5,0                                                     
         BNH   *+10                                                             
         MVC   SR2COMM+050(50),PRQCOM2                                          
*                                                                               
         CLI   PRQCOM3H+5,0                                                     
         BNH   *+10                                                             
         MVC   SR2COMM+100(50),PRQCOM3                                          
*                                                                               
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRMED   DS    0H                                                               
         XC    PRQMEDN,PRQMEDN                                                  
         OI    PRQMEDNH+6,X'80'                                                 
         NI    PRQMEDH+4,X'DF'                                                  
         XC    PRQPUBN,PRQPUBN                                                  
         OI    PRQPUBNH+6,X'80'                                                 
         NI    PRQPUBH+4,X'DF'                                                  
*                                                                               
CLRCLT   DS    0H                                                               
         XC    PRQCLTN,PRQCLTN                                                  
         OI    PRQCLTNH+6,X'80'                                                 
         NI    PRQCLTH+4,X'DF'                                                  
*                                                                               
CLRPRD   DS    0H                                                               
         XC    PRQPRDN,PRQPRDN                                                  
         OI    PRQPRDNH+6,X'80'                                                 
         NI    PRQPRDH+4,X'DF'                                                  
*                                                                               
         XC    PRQESTN,PRQESTN                                                  
         OI    PRQESTNH+6,X'80'                                                 
         NI    PRQESTH+4,X'DF'                                                  
*                                                                               
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NXTELEM  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
       ++INCLUDE POLINITL2         IN-LINE CODES                                
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
         OI    6(R2),X'40'         INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         OI    6(R4),X'80'                                                      
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
EXIT     XIT1                                                                   
*                                                                               
GET_ETXT DS    0H                                                               
         L     RF,VCOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         MVI   ERRAREA,X'FF'                                                    
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAFID NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         LA    R0,PBUYREC+1500     INIT WK AREA (TEMP USE)                      
         LHI   R1,400*4                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         LA    R5,PBUYREC+1500                                                  
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,10(RA)      ID NUMBER                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R5),(R5)                 
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
*                                                                               
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDER                                                         
         DROP  R5,RE                                                            
*                                                                               
CKTRIDX  CR    RB,RB               EQUAL                                        
         B     *+6                                                              
CKTRIDER LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TRAFFACC NTR1  BASE=*,LABEL=*      CHECKING FOR TRAFFIC LIMIT ACCESS            
*                                                                               
         MVI   BYTE3,0             WILL RETURN CODE                             
*                                                                               
         MVI   ELCODE,X'50'        CLIENT TRAFFIC OFFICE ELEM CODE              
         LA    R5,PCLTREC+33                                                    
         CLC   ELCODE,0(R5)        FOUND IN FIRST ELEM?                         
         BE    *+12                                                             
         BRAS  RE,NXTELEM                                                       
         BNE   TRACCX              NO CLIENT TRAFFIC OFFICE CODE ELEM           
         MVC   BYTE3,2(R5)         RETURN CLT TRAFFIC OFFICE CODE               
*                                                                               
TRACCX   J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETPROFV NTR1  BASE=*,LABEL=*      SET PROFILE VALUES                           
*                                                                               
         MVC   SVMCCNAM,SPACES     MASTER CLT'S NAME                            
         MVC   SVMCSCMC,SPACES     MASTER CLT'S CON STD COMM CODE               
         XC    SVMCOFFC,SVMCOFFC   MASTER CLT'S OFFICE CODE                     
*                                                                               
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   SETPV30                                                          
         MVC   SVTMPKEY,KEY        READ MASTER CLIENT RECORD                    
         XC    KEY,KEY                                                          
         MVC   KEY+00(02),QAGENCY                                               
         MVC   KEY+02(01),QMEDIA                                                
         MVI   KEY+03,X'02'                                                     
         MVC   KEY+04(L'PCLTKCLT),PCLTPROF+6                                    
         GOTOR HIGH                                                             
         CLC   KEY(L'PCLTKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                MASTER CLIENT NOT ON FILE                    
         MVC   WRKFULL1,AREC       SAVE ORIGINAL AIO POINTER                    
         LA    RE,TMPWKAIO                                                      
         ST    RE,AREC                                                          
         GOTOR GETPRT                                                           
         MVC   AREC,WRKFULL1       RESTORE ORIGINAL AIO POINTER                 
         MVC   KEY,SVTMPKEY        RESTORE KEY                                  
*                                                                               
         LA    R5,TMPWKAIO+33                                                   
         CLI   0(R5),X'02'                                                      
         BE    *+6                                                              
         DC    H'0'                INVALID CLIENT RECORD                        
         USING PCLTELEM,R5                                                      
         MVC   SVMCCNAM,PCLTNAME                                                
         MVC   SVMCOFFC,PCLTOFF                                                 
         DROP  R5                                                               
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   SETPV30                                                          
         MVC   SVMCSCMC,2(R5)      MASTER CLT'S CONTRACT STD COMM CODE          
*                                                                               
SETPV30  XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P012'                                                 
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   SETPV36                                                          
         MVC   WORK+7(3),PCLTPROF+6                                             
         CLI   SVMCOFFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVMCOFFC                                              
         B     SETPV38                                                          
SETPV36  CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
SETPV38  L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,P012PROF,VDATAMGR                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P12A'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   SETPV46                                                          
         MVC   WORK+7(3),PCLTPROF+6                                             
         CLI   SVMCOFFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVMCOFFC                                              
         B     SETPV48                                                          
SETPV46  CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
SETPV48  L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,P12APROF,VDATAMGR                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P12B'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),AGYALPHA                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTPROF+5,C'2'     SUB CLIENT?                                  
         BNE   SETPV56                                                          
         MVC   WORK+7(3),PCLTPROF+6                                             
         CLI   SVMCOFFC,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVMCOFFC                                              
         B     SETPV58                                                          
SETPV56  CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
SETPV58  L     RF,VCOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETPROF,DMCB,WORK,P12BPROF,VDATAMGR                             
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RF                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FNDPUB   NTR1  BASE=*,LABEL=*      GET PUB RECORD(S)- PUB CODE IN DUB           
*                                                                               
         XC    PUBREC(50),PUBREC                                                
         MVC   WORK(64),KEY                                                     
*                                                                               
         LHI   R4,7-1              FOR KEY COMPARE EX INSTRUCTION               
         CLI   ZEOPT,C'Y'                                                       
         BNE   *+8                                                              
         LHI   R4,5-1                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PRQMED                                                    
         MVC   KEY+1(6),DUB                                                     
         MVC   KEY+7(2),AGYALPHA                                                
         GOTOR HIGHPB                                                           
         B     FP2K                                                             
FP2      GOTOR SEQPB                                                            
FP2K     EX    R4,KEYCOMP                                                       
         BNE   FP8                                                              
                                                                                
         CLC   KEY+7(2),KEYSAVE+7  CHECK AGENCY                                 
         BE    FP4                                                              
         CLC   KEY+7(2),=C'ZZ'                                                  
         BNE   FP2                                                              
         CLI   PAGYPROF+16,C'0'    TEST DEFAULT                                 
         BE    FP8                 NO                                           
*                                                                               
FP4      CLI   KEY+9,X'81'         RECORD TYPE IS STILL PUB?                    
         BNE   FP2                                                              
         CLI   PUBREC,0                                                         
         BNE   FP10                                                             
         LA    R0,PUBREC                                                        
         ST    R0,AREC                                                          
         GOTOR GETPUB                                                           
         B     FP2                                                              
*                                                                               
FP8      MVC   KEY(64),WORK                                                     
         CLI   PUBREC,0                                                         
         JE    SETCCNEQ                                                         
*                                                                               
FP10     MVC   KEY(64),WORK                                                     
         J     SETCCEQ                                                          
*                                                                               
KEYCOMP  CLC   KEY(0),KEYSAVE      EXECUTED                                     
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPSR201D DSECT                                                                  
*                                                                               
RELO01   DS    F                                                                
*                                                                               
WRKFULL1 DS    F                                                                
WRKFULL2 DS    F                                                                
WRKDUB_1 DS    D                                                                
WRKDUB_2 DS    D                                                                
*                                                                               
SVAGYNAM DS    CL(L'PAGYNAME)                                                   
SVAGYADR DS    CL(L'PAGYADDR)                                                   
*                                                                               
SVTMPKEY DS    XL(L'KEY)                                                        
*                                                                               
TMPWKAIO DS    XL4096              TEMP WORKING AIO                             
*                                                                               
PPSR201X EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPSR2WRK1                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSR2WRK2                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENGRP                                                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FATWA                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPERREQUS         (P)RINT SYSTEM ERROR MSG EQUATES             
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPSR201   03/26/08'                                      
         END                                                                    
