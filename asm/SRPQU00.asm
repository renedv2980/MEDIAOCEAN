*          DATA SET SRPQU00    AT LEVEL 008 AS OF 09/22/20                      
*PHASE T13100A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE TIMBER                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
         TITLE '$PQ - ROOT CONTROLLER'                                          
         PRINT NOGEN                                                            
PQCNTL   CSECT                                                                  
         NMODL PQUWKX-PQUWKD,**$PQ0**,RA,R9,RR=R4,CLEAR=YES                     
         USING PQUWKD,RC                                                        
         ST    R4,RELOBASE                                                      
         LA    RE,*+6                                                           
         BSM   0,RE                SET IN 24-BIT MODE                           
*                                                                               
         ST    RB,ABASE                                                         
         STM   R9,RB,ABASES        SAVE BASE VALUES                             
         ST    RD,SAVERD                                                        
         ST    R1,APARM                                                         
*                                                                               
PQCNTL0  L     R2,4(R1)                                                         
         ST    R2,ATIA             SAVE A(TIA)                                  
         LA    R2,SRCOMWRK-SRSD(R2)                                             
         LA    R2,200(R2)          PQ SAVE IS AT SRCOMWRK+200                   
         ST    R2,ASAVESTR                                                      
         USING PQSAVED,R2          R2=A(SAVE STORAGE)                           
         L     RF,8(R1)                                                         
         ST    RF,AUTL             SAVE A(UTL)                                  
         L     RF,28(R1)                                                        
         ST    RF,ATIOB            SAVE A(TIOB)                                 
         L     RF,12(R1)                                                        
         ST    RF,ACOMFACS         SAVE A(COMFACS)                              
         L     R3,20(R1)                                                        
         USING SRPQUFFD,R3         R3=A(TWA)                                    
         L     R8,00(R1)                                                        
         ST    R8,ASYSFAC                                                       
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
*                                                                               
PQCNTL1  L     RF,ACOMFACS         SAVE COMFACS ROUTINES                        
         USING COMFACSD,RF                                                      
         MVC   AGETFACT,CGETFACT                                                
         MVC   ADATCON,CDATCON                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   AGETTXT,CGETTXT                                                  
         MVC   AGETHELP,CGETHELP                                                
         MVC   ADICTATE,CDICTATE                                                
         MVC   ATERMVAL,CTERMVAL                                                
         MVC   AUNSCAN,CUNSCAN                                                  
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ADATVAL,CDATVAL                                                  
         MVC   APERVERT,CPERVERT                                                
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   AGLOBBER,CGLOBBER                                                
         DROP  RF                                                               
*                                                                               
         LT    RF,=V(DATCON)       TEST IF DATCON INCLUDED                      
         BZ    *+12                                                             
         A     RF,RELOBASE                                                      
         ST    RF,ADATCON                                                       
*                                                                               
         LT    RF,=V(DATVAL)       TEST IF DATVAL INCLUDED                      
         BZ    *+12                                                             
         A     RF,RELOBASE                                                      
         ST    RF,ADATVAL                                                       
*                                                                               
         MVC   AVUTL,VUTL          SAVE SYSFACS ADDRESSES                       
         MVC   ATERMBLD,VTERMBLD                                                
         MVC   APRQ,VPRQ                                                        
         MVC   ASSB,VSSB                                                        
         MVC   APRQENTS,VPRQENTS                                                
         MVC   ALCM,VLCM                                                        
         MVC   ATICTOC,VTICTOC                                                  
*                                                                               
         L     RF,=V(SQUASHER)     SAVE OTHER ROUTINES                          
         A     RF,RELOBASE                                                      
         ST    RF,ASQUASH                                                       
         L     RF,=V(TIMBER)                                                    
         A     RF,RELOBASE                                                      
         ST    RF,ATIMBER                                                       
         L     RF,=A(QSCAN)                                                     
         A     RF,RELOBASE                                                      
         ST    RF,AQSCAN                                                        
*&&UK*&& GOTO1 VCALLOV,DMCB,0,X'D9000AF9',0                                     
*&&US*&& GOTO1 VCALLOV,DMCB,0,X'D9000AFA',0                                     
         MVC   AGETIDS,0(R1)                                                    
         GOTO1 VCALLOV,DMCB,0,X'D9000AFC',0                                     
         MVC   AGENIDS,0(R1)                                                    
         LA    RF,GETUSER                                                       
         ST    RF,AGETUSER                                                      
         L     RF,=A(SELTABL)                                                   
         A     RF,RELOBASE                                                      
         ST    RF,ASELTABL                                                      
         LA    RF,SECVAL                                                        
         ST    RF,ASECVAL                                                       
*                                                                               
         L     RF,=A(CTREC-PQUWKD) SAVE OUT OF RANGE WORK                       
         LA    RF,PQUWKD(RF)                                                    
         ST    RF,ACTREC                                                        
         L     RF,=A(CIREC-PQUWKD)                                              
         LA    RF,PQUWKD(RF)                                                    
         ST    RF,ACIREC                                                        
         ST    RF,AREPTAB                                                       
         L     RF,=A(CIREC1-PQUWKD)                                             
         LA    RF,PQUWKD(RF)                                                    
         ST    RF,ACIREC1                                                       
*                                                                               
         MVI   LOCKT,0             SET INITIAL VALUES                           
         MVI   DDS,0                                                            
         MVI   DDS1,0                                                           
         MVI   INTFLAG,0                                                        
         MVI   PAGFLAG,FF                                                       
         MVC   HELPKEY,HELPID                                                   
         MVC   PRTQID,=CL8'PRTQUE'                                              
         MVC   DDSPSWD,=CL8'DDPQ  '                                             
*                                                                               
         L     RF,VSSB             SAVE SSB DATA                                
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL      TWA LEN                                      
         L     R1,SSBTKADR                                                      
         ST    R1,ATCB             SAVE A(TCB)                                  
*        TM    TCBFLAG1-TCBD(R1),TCBENQPQ                                       
*        BNO   *+6                                                              
*        DC    H'0'                MAKE SURE NO PQS ENQD                        
         SR    R1,R1                                                            
         IC    R1,SSBSYSID                                                      
         SLL   R1,4                                                             
         STC   R1,SYSID            SET FACPAK SYSID                             
         MVC   SYSFLG,SSBSYSFL                                                  
         DROP  RF                                                               
*                                                                               
         L     RF,AUTL             SAVE UTL DATA                                
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         MVC   TRMTYP,TSTAT                                                     
         MVC   TRMTYP1,TTYPE                                                    
         MVC   TRMTYP2,TTYPE2                                                   
         MVC   TRMSTAT7,TSTAT7                                                  
         MVC   TRMLANG,TLANG                                                    
         MVC   TRMCTRY,TAGCTRY                                                  
         MVC   TRMPID+0(2),TAGYPER                                              
         MVC   TRMPID+2(2),TPERSON                                              
         MVC   USERID,TUSER                                                     
         MVC   LOGONID,TUSER                                                    
         MVC   GRPUSER,TPQGRPID                                                 
*                                                                               
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    TSVCREQ,X'02'       SET =XRSOR FLAG                              
         OI    DDS,DDSNEW          FORCE INIT                                   
         TM    TSTAT1,X'60'        TEST DDS TERMINAL                            
         BZ    *+8                                                              
         OI    DDS,DDSTRM                                                       
         TM    TRMSTAT7,TST7PQPU   TEST PRIVILEGED USER ID                      
         BZ    *+8                                                              
         OI    DDS,DDSPRV                                                       
         TM    TRMSTAT7,TST7PQPW   TEST PASSWORD REQUIRED                       
         BZ    *+8                                                              
         OI    DDS,DDSPWD                                                       
         TM    TSTAT6,TST6STRO     TEST FRONT ENDED WITH PC APPLICATION         
         BZ    *+8                                                              
         OI    DDS,DDSPCAPP                                                     
         DROP  RF                                                               
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         ICM   R0,3,TIOBCURD                                                    
         AR    R0,R3               ADD TWA ADDRESS                              
         ST    R0,CURSOR                                                        
         MVC   CURADR,TIOBCURS                                                  
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   PQCNTL1A                                                         
         SR    R0,R0               CONVERT TO PF 1 - 12                         
         IC    R0,PFKEY                                                         
         AHI   R0,-12                                                           
         STC   R0,PFKEY                                                         
PQCNTL1A EQU   *                                                                
         DROP  RF                                                               
*                                                                               
         L     RF,=A(DDDCLST)      RESOLVE DICTIONARY REFERENCES                
         A     RF,RELOBASE                                                      
         GOTO1 ADICTATE,DMCB,C'LU  ',(RF),DDDSLST                               
         MVC   DS4NCD,DC@NCD                                                    
         MVC   DS4OCD,DC@OCD                                                    
*                                                                               
PQCNTL2  LA    RF,L'PQINDEX        SAVE PRTQUE DATA                             
         STH   RF,CINDXLN                                                       
         L     RF,VISGENQ                                                       
         ST    RF,FIWENQ                                                        
*                                                                               
         GOTO1 ADATAMGR,DMCB,(0,=CL8'SHMUSS'),(0,=CL8'ATTACH'),        +        
               (0,=CL8'PRTQ'),0,0                                               
         ICM   R1,15,DMCB+12       A(SHARED MEMORY)                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R1,FIWSHA                                                        
*                                                                               
         SR    RF,RF               FIND PUBLICID FOR THIS CTRY                  
         IC    RF,TRMCTRY                                                       
         SLL   RF,1                                                             
         LA    RF,PUBIDTAB(RF)                                                  
         MVC   PUBLICID,0(RF)                                                   
*                                                                               
PQCNTL3  L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,PQDMCB,(X'00',GLIST),PRTQID,NDX,SAVE,(R5)               
         L     RE,NXUSRINF                                                      
         SR    R1,R1               R1=NUM OF FILES IN LIST                      
         IC    R1,0(RE)                                                         
         LA    R1,2(R1)            ADD TWO FOR HDR AND TRL                      
         SLL   R1,3                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTQLST(0),0(RE)    COPY PRTQ LIST TO MY OWN AREA                
         XC    PRTQLSTX,PRTQLSTX   SET END OF MAXIMUM LIST                      
*                                                                               
         LA    R1,17               FIND DELIMITER                               
         LA    R4,SRVID                                                         
         MVC   ACTNSR(2),=C'PQ'    SET PQ S/R ID CHRS                           
         MVC   ACTNSR+2(2),SPACES                                               
PQSRV1   CLI   0(R4),C','                                                       
         BE    PQSRV2                                                           
         CLI   0(R4),C'.'                                                       
         BE    PQSRV2                                                           
         CLI   0(R4),C' '                                                       
         BE    PQSRV2                                                           
         CLI   0(R4),X'00'                                                      
         BE    PQSRV2                                                           
         LA    R4,1(R4)                                                         
         BCT   R1,PQSRV1                                                        
         B     ERR2                                                             
*                                                                               
PQSRV2   ST    R4,FULL             SAVE A(DELIMITER) IN FULL                    
         BCTR  R4,0                                                             
         CLC   0(1,R4),DC@PUB      TEST FOR =PQP                                
         BNE   PQSRV3                                                           
         MVC   ACTNSR+2(1),DC@PUB                                               
         OI    DDS,DDSPUB          SET PUBLICID                                 
         MVC   USERID,PUBLICID                                                  
         B     PQSRV4                                                           
PQSRV3   CLC   0(1,R4),DC@GRP      TEST FOR =PQG                                
         BNE   PQSRV4                                                           
         OC    GRPUSER,GRPUSER     TEST IF GROUP USER ID DEFINED                
         BZ    PQSRV4                                                           
         MVC   ACTNSR+2(1),DC@GRP                                               
         OI    DDS,DDSGRP          SET GROUP ID                                 
         MVC   USERID,GRPUSER                                                   
PQSRV4   CLC   1(3,R4),=C',RE'     TEST FOR ,RE RETURN TO PREV SCREEN           
         BNE   *+10                                                             
         MVC   1(3,R4),=C',#R'                                                  
         CLC   1(3,R4),=C',#R'     TEST FOR SPECIAL DQU RETURN                  
         BNE   PQSRV6                                                           
PQSRV5   NI    DDS,255-DDSNEW      CLEAR NEW FLAG                               
         MVI   PFKEY,4                                                          
         B     PQSRVX                                                           
*                                                                               
PQSRV6   CLC   1(2,R4),=C',#'      PAGE NUMBER SUPPLIED                         
         BNE   PQSRVX                                                           
         MVC   HALF,3(R4)                                                       
         OC    HALF,=X'0F0F'       TEST VALID NUMERIC                           
         CLC   HALF,=X'FFFF'                                                    
         BNE   PQSRVX                                                           
*                                                                               
         PACK  DUB,3(2,R4)                                                      
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         ST    R1,FULL1                                                         
         MVI   FULL1,1                                                          
         MVI   PFKEY,4                                                          
         NI    DDS,255-DDSNEW      CLEAR NEW FLAG                               
PQSRVX   EQU   *                                                                
*                                                                               
PQCNTL5  LA    R4,SRVIDH           SET R4 FOR ERR EXIT                          
         OC    USERID,USERID       NON DDS TERMINALS MUST BE LOGGED ON          
         BNZ   PQCNTL6                                                          
         TM    DDS,DDSTRM                                                       
         BZ    ERR0                                                             
         MVC   PRTQID+4(1),PRTQNTRY+1 DEFAULT TO FIRST PRTQ FILE                
         MVI   PRTQID+5,C' '                                                    
         MVC   PRTQIDSV,PRTQID                                                  
         B     PQCNTL7                                                          
*                                                                               
PQCNTL6  MVC   NXSRCID,USERID      GET PRTQ ID FROM USER ID                     
         GOTO1 VDATAMGR,PQDMCB,(X'00',GFILE)                                    
         MVC   PRTQID,NXUSRINF                                                  
         MVC   PRTQIDSV,PRTQID                                                  
*                                                                               
PQCNTL7  GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER)                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ACIREC                                                        
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ                 
*                                                                               
PQCNTL8  XC    DMCB(4),DMCB        GET TODAYS DATE                              
         GOTO1 AGETFACT,DMCB                                                    
         L     R1,0(R1)                                                         
         MVC   DUB,4(R1)                                                        
         GOTO1 ADATCON,DMCB,(4,DUB),(0,DATE)                                    
         GOTO1 (RF),(R1),,(01,DATE1)                                            
         GOTO1 (RF),(R1),,(30,DATEC)  GET NEW FORMAT COMPRSD DATE               
         GOTO1 (RF),(R1),,(2,DATECO)  GET OLD FORMAT COMPRSD DATE               
*                                                                               
PQCNTL9  L     R5,ATIA             READ AND LOCK TWA11 INTO TIA                 
         USING SRSD,R5                                                          
         LA    R4,SRPAGENO                                                      
         SLL   R4,32-8                                                          
         ICM   R4,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R4),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SRCOMWRK(200),SRCOMWRK   CLEAR FIRST 200 BYTES                   
         CLC   PQSID,ACTNSR             ID MUST BE GOOD ELSE INIT               
         BE    *+8                                                              
         OI    DDS,DDSNEW                                                       
*                                                                               
         CLI   PQSACT,X'31'        IGNORE IF LIST MODE                          
         BE    PQCNTL10                                                         
         ICM   R0,15,PQSPREV       PREV TRANSACTION TIME                        
         L     R1,ATCB                                                          
         ICM   R1,15,TCBSTTM-TCBD(R1) TIME NOW                                  
         SR    R1,R0                                                            
         L     RF,VSSB                                                          
         TM    SSBSTAT3-SSBD(RF),SSBMVSTU                                       
         BO    *+12                                                             
         C     R1,=AL4(15*60*100)   IF > 15 MINS OLD                            
         B     *+8                                                              
         C     R1,=AL4(15*60*38000) IF > 15 MINS OLD                            
         BL    *+8                                                              
         OI    DDS,DDSNEW          INIT                                         
*                                                                               
PQCNTL10 LA    R0,PQSAVED          TEST FOR INITIALISE SAVE STORAGE             
         LHI   R1,PQSLEN                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         TM    DDS,DDSNEW                                                       
         BZ    *+6                                                              
         MVCL  R0,RE               CLEAR SAVE STORAGE                           
         MVC   PQSID,ACTNSR        SET OR RESET PQ ID                           
         L     R1,ATCB                                                          
         ICM   R1,15,TCBSTTM-TCBD(R1)   TIME NOW                                
         STCM  R1,15,PQSPREV            SAVE TIME                               
*                                                                               
         OC    FULL1,FULL1                                                      
         BZ    *+10                                                             
         MVC   PQSPAGE,FULL1+3                                                  
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM                                                                  
***********************************************************************         
         L     RF,=A(GETUPOP)                                                   
         A     RF,RELOBASE                                                      
         BASR  RE,RF               GET CONNECTED USERID PNAME OPTIONS           
*                                                                               
         L     RF,=A(HELPSCAN)                                                  
         A     RF,RELOBASE                                                      
         BASR  RE,RF               TEST IF ? IN FIELDS                          
*                                                                               
SRVAL    L     R4,FULL             $PQ,ABC                                      
         CLC   0(4,R4),=C',CLI'    $PQ,CLI                                      
         BNE   SRVAL01                                                          
         NI    DDS,255-DDSTRM      CLIENT MODE                                  
         MVC   FULL,SPACES                                                      
         B     SRVAL02             DONT CLEAN UP                                
*                                                                               
SRVAL01  MVC   FULL,0(R4)          SAVE ",ABC" IN FULL                          
         CLI   FULL+3,0                                                         
         BNE   *+8                                                              
         OI    FULL+3,X'40'        MAKE IT A SPACE                              
         LA    R1,SRVID+16                                                      
         SR    R1,R4                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R4),0(R4)                                                    
         OI    SRVIDH+6,X'80'      TRANSMIT CLEANED $PQ OR $PQP FLD             
*                                                                               
SRVAL02  CLI   FULL,C','           USER TERMINALS MUST BE PQ,                   
         BE    SRVAL010                                                         
         TM    DDS,DDSTRM                                                       
         BNO   SRVALX                                                           
         CLI   FULL,C'.'           DDS TERMINALS PQ. MEANS U=ALL                
         BNE   SRVALX                                                           
         MVI   PQSDDSFN,C'U'                                                    
*                                                                               
SRVAL010 CLI   FULL+1,C'#'         TEST FOR SPECIAL FUNCTIONS                   
         BNE   SRVAL050                                                         
         CLC   FULL+2(2),=C'1 '    TEST FOR START PFKEY HIT                     
         BNE   SRVAL020                                                         
         MVC   PQSACT(2),=X'1104'  START,PFKEY                                  
         LA    R6,SRVP2H                                                        
         GOTO1 AGLOBBER,DMCB,=C'GETF',(R6),,GLVSPID                             
         L     R1,AUTL                                                          
         MVI   TSVCREQ-UTLD(R1),0                                               
         B     SRVAL990                                                         
*                                                                               
SRVAL020 CLC   FULL+2(2),=C'2 '    STATUS PFKEY HIT                             
         BNE   SRVAL030                                                         
         MVI   PQSACT,X'12'        ACTION STATUS                                
         MVI   PQSACT1,0                                                        
         B     SRVAL990                                                         
*                                                                               
SRVAL030 CLI   FULL+2,C'R'         SPECIAL RETURN CODE                          
         BNE   SRVALX                                                           
         MVC   SRVP3(15),PQSP3FLD  PUT BACK 15 CHRS                             
         MVC   SRVP4,PQSP4FLD                                                   
         MVC   SRVP3H+5(1),PQSP3LEN                                             
         MVC   SRVP4H+5(1),PQSP4LEN                                             
         B     SRVAL990                                                         
*                                                                               
SRVAL050 CLI   FULL+1,C'0'         0=SIZE                                       
         BNE   SRVAL060                                                         
         MVI   PQSACT,X'28'                                                     
         MVI   PQSACT1,0                                                        
         B     SRVAL800                                                         
*                                                                               
SRVAL060 CLI   FULL+1,C'1'         1 DISPLAY ALL                                
         BNE   SRVAL070                                                         
         MVI   PQSACT,X'21'                                                     
         MVI   PQSACT1,0                                                        
         MVC   PQSSUBID,SR@ALL                                                  
         B     SRVAL800                                                         
*                                                                               
SRVAL070 CLI   FULL+1,C'2'         2 DISPLAY ALL DATE=TODAY                     
         BNE   SRVAL080                                                         
         MVI   PQSACT,X'21'                                                     
         MVI   PQSACT1,0                                                        
         MVC   PQSSUBID,SR@ALL                                                  
         MVC   SRVP4(12),SR@DTTOD                                               
         B     SRVAL800                                                         
*                                                                               
SRVAL080 CLI   FULL+1,C'3'                                                      
         BNE   SRVAL090                                                         
         MVI   PQSACT,X'11'                                                     
         MVI   PQSACT1,0                                                        
         B     SRVAL800                                                         
*                                                                               
SRVAL090 CLI   FULL+1,C'G'         G MEANS DISP/ALL/G/HOLD/DATA=TODAY           
         BNE   SRVAL095                                                         
         MVI   PQSACT,X'21'                                                     
         MVI   PQSACT1,0                                                        
         MVC   PQSSUBID,SR@ALL                                                  
         MVC   SRVP4(12),SR@DTTOD                                               
         B     SRVAL800                                                         
*                                                                               
SRVAL095 CLC   FULL,=C',JOB'       TEST FOR HOT SWAP FROM =RUN                  
         BNE   SRVAL100                                                         
         MVI   PQSACT,X'21'                                                     
         MVI   PQSACT1,0                                                        
         MVC   PQSSUBID,=X'FFFFFF'                                              
         B     SRVAL800                                                         
*                                                                               
SRVAL100 CLI   FULL+1,C'$'         X=DISP/C WHERE C IS CLASS                    
         BE    SRVAL110                                                         
         CLI   FULL+1,C'A'                                                      
         BL    SRVALX                                                           
         CLI   FULL+1,C'Z'                                                      
         BH    SRVALX                                                           
SRVAL110 MVI   PQSACT,X'21'                                                     
         MVI   PQSACT1,0                                                        
         MVC   PQSCLASS,FULL+1                                                  
         B     SRVAL800                                                         
*                                                                               
SRVAL800 CLC   FULL+3(1),SR4ACTV   STATUS FILTER                                
         BNE   *+10                                                             
         MVC   SRVP3(4),SR4ACTV                                                 
         CLC   FULL+3(1),SR@HOLD                                                
         BNE   *+10                                                             
         MVC   SRVP3(4),SR@HOLD                                                 
         CLC   FULL+3(1),SR@SENT                                                
         BNE   *+10                                                             
         MVC   SRVP3(4),SR@SENT                                                 
         CLC   FULL+3(1),SR4PRTD                                                
         BNE   *+10                                                             
         MVC   SRVP3(4),SR4PRTD                                                 
         CLI   SRVP3,C'A'                                                       
         BL    *+8                                                              
         MVI   SRVP3H+5,4                                                       
*                                                                               
SRVAL810 CLI   FULL+2,C'0'         IF PQ,?N                                     
         BL    SRVAL990                                                         
         CLI   FULL+2,C'F'                                                      
         BH    SRVAL990                                                         
         MVC   PQSQUEUE,FULL+2     N IS QUEUE NUMBER                            
         B     SRVAL990                                                         
*                                                                               
SRVAL990 CLI   PQSACT,0            TEST FOR INVALID                             
         BE    SRVALX                                                           
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY ACTION                             
         CLI   PQSACT,X'11'        IF GLOBBER JUST PUT THE FIELD THERE          
         BE    SRVALX              ONLY A CUNTHEAD WOULD CLOBBER IT             
         MVI   RIDFLAG,X'FF'                                                    
         GOTO1 RIDXPND,SRVP2H      REDISPLAY REPID                              
SRVALX   XC    PQSCHANG,PQSCHANG   SET NO CHANGE                                
         B     PQ010                                                            
*                                                                               
PQ010    MVC   HALF+1(1),PQSACT                                                 
         GOTO1 P1VAL,SRVP1H        VALIDATE P1                                  
         XC    SRVP1,SRVP1                                                      
         MVI   SRVP1H+5,0                                                       
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY                                    
*                                                                               
         CLI   PQSACT,X'21'        IF DISPLAY IS INPUT                          
         BNE   PQ011                                                            
         LA    R1,SRVP2H           TEST FOR CURSOR POS                          
         CLC   CURADR,2(R1)                                                     
         BNL   PQ011                                                            
         OI    DDS,DDSNEW                                                       
*                                                                               
PQ011    MVC   HALF(1),PQSACT      TEST THIS ACT WITH LAST ACT                  
         NI    HALF,X'F0'                                                       
         NI    HALF+1,X'F0'                                                     
         CLC   HALF(1),HALF+1      SAME ACTION TYPE OR                          
         BE    ACTV01                                                           
         CLI   ACTN1,X'03'         IS SUB ACTION SEL                            
         BE    ACTV01                                                           
         LA    RF,SRVP4H           NO TEST P4 FOR CURSOR                        
         CLC   CURADR,2(RF)                                                     
         BH    ACTV01              CURSOR IS PAST P4                            
         XC    SRVP4,SRVP4         ELSE LOSE P4                                 
         MVI   SRVP4H+5,0                                                       
         LA    RF,SRVP3H           TEST P3 FOR CURSON                           
         CLC   CURADR,2(RF)                                                     
         BH    ACTV01              CURSOR IS PAST P3                            
         XC    SRVP3,SRVP3         ELSE LOSE P3                                 
         MVI   SRVP3H+5,0                                                       
*                                                                               
ACTV01   MVC   ACTN,PQSACT         COPY SAVED VALUES TO WORK                    
         MVC   ACTN1,PQSACT1                                                    
         MVC   ACTN2,PQSACT2                                                    
*                                                                               
PQ020    GOTO1 P2VAL,SRVP2H        VALIDATE P2                                  
PQ021    XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         MVI   RIDFLAG,X'FF'                                                    
         GOTO1 RIDXPND,SRVP2H      REDISPLAY                                    
         CLI   PQSDDSFN,C'V'                                                    
         BNE   *+8                                                              
         OI    DDS1,DDSTOT         FLAG TOTALS                                  
*                                                                               
         LA    R4,SRVP1H           SET CURSOR TO P1                             
         ST    R4,CURSOR                                                        
         CLI   ACTN1,X'03'         IS SUB ACT SEL                               
         BNE   PQ022                                                            
         CLI   PQSACT1,X'03'                                                    
         BNE   ERR2                NOTHING TO DO                                
         OI    INTFLAG,INTRUN                                                   
         B     SETUSR                                                           
PQ022    CLI   ACTN,X'20'          TEST SELECT ACTION                           
         BNE   P3CHECK                                                          
         OC    PQSTOTL,PQSTOTL                                                  
         BZ    ERR2                                                             
         CLI   PQSACT,X'20'                                                     
         BNE   ERR2                NOTHING TO DO                                
         OI    INTFLAG,INTRUN                                                   
         B     P3VSEL                                                           
*                                                                               
P3VSEL   LA    R1,SRVP3H           GET NEW STATUS                               
         CLI   HLPFLD,4            TEST STATUS HELP                             
         BE    HELPOUT                                                          
         CLI   5(R1),0             IF ANYTHING IN P3                            
         BE    P3CHECK                                                          
         GOTO1 P1VAL,SRVP3H        GET NEW STATUS                               
         MVC   ACTN,PQSACT         COPY SAVED VALUES TO WORK                    
         MVC   ACTN1,PQSACT1                                                    
         MVC   ACTN2,PQSACT2                                                    
         XC    SRVP3,SRVP3                                                      
         MVI   SRVP3H+5,0                                                       
         OI    INTFLAG,INTCONT     CONTINUE AFTER NEW STATUS                    
         MVC   HALF,PQSLAST                                                     
         BAS   RE,GETREPT          READ RECORD INTO INDEX AREA                  
P3VSEL1  OI    INTFLAG,INTSEL      FLAG NEW STATUS                              
         B     SETUSR                                                           
         EJECT                                                                  
P3CHECK  SR    R1,R1               HAS P3 CHANGED                               
         MVI   PQSCHP3,X'FF'                                                    
         LA    R4,SRVP3H                                                        
         CLC   PQSP3LEN,5(R4)                                                   
         BNE   P4CHECK                                                          
         ICM   R1,1,5(R4)                                                       
         BZ    P3CHKX                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PQSP3FLD(0),8(R4)                                                
         BNE   P4CHECK                                                          
P3CHKX   MVI   PQSCHP3,0                                                        
*                                                                               
P4CHECK  SR    R1,R1               HAS P4 CHANGED                               
         MVI   PQSCHP4,X'FF'                                                    
         LA    R4,SRVP4H                                                        
         CLC   PQSP4LEN,5(R4)                                                   
         BNE   SETP34                                                           
         ICM   R1,1,5(R4)                                                       
         BZ    P4CHKX                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PQSP4FLD(0),8(R4)                                                
         BNE   SETP34                                                           
P4CHKX   MVI   PQSCHP4,0                                                        
*                                                                               
SETP34   LA    R4,SRVP3H           SAVE P3 & P4                                 
         MVC   PQSP3LEN,5(R4)                                                   
         MVC   PQSP3FLD,8(R4)                                                   
         LA    R4,SRVP4H                                                        
         MVC   PQSP4LEN,5(R4)                                                   
         MVC   PQSP4FLD,8(R4)                                                   
*                                                                               
         OC    PQSCHANG,PQSCHANG   HAVE P1-P4 CHANGED                           
         BZ    SETUSR                                                           
*                                                                               
         LA    R1,SRVSA1H          CLEAR SELECT DATA                            
         SR    RE,RE                                                            
SETP35   IC    RE,0(R1)                                                         
         TM    1(R1),X'20'         TEST PROT                                    
         BO    SETP36                                                           
         MVI   5(R1),0             SET INPUT LENGTH TO 0 CUNTHEAD               
         AHI   RE,-9               SET FOR EX                                   
         BM    SETP36                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         IC    RE,0(R1)            RESTORE FIELD LENGTH                         
*                                                                               
SETP36   AR    R1,RE                                                            
         CLI   0(R1),0                                                          
         BNE   SETP35                                                           
         EJECT                                                                  
SETUSR   CLI   PQSQUEUE,0          TEST IF SPECIFIC PRTQ NAMED BY Q=N           
         BE    SETUSR1             NO                                           
         CLI   PQSQUEUE,C'U'                                                    
         BE    SETUSR1                                                          
         MVC   PRTQID,=CL8'PRTQ'                                                
         MVC   PRTQID+4(1),PQSQUEUE                                             
         MVC   PRTQIDSV,PRTQID                                                  
         L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER)                                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ                 
*                                                                               
SETUSR1  CLC   PQSUSER,PUBLICID    TEST IF USER TERM ON PUBLIC ID               
         BNE   SETUSR2                                                          
         TM    DDS,DDSTRM                                                       
         BO    SETUSR2                                                          
         CLI   ACTN,X'21'          USERS CANT CHANGE STATUS OF PUBLIC           
         BNH   SETUSR2                                                          
         CLI   ACTN,X'2F'                                                       
         BH    SETUSR2                                                          
         LA    R4,SRVP1H           INVALID ACTION                               
         ST    R4,CURSOR                                                        
         B     ERR2                                                             
*                                                                               
SETUSR2  TM    DDS,DDSGRP          TEST IF USER TERM ON GROUP ID                
         BZ    SETUSR3                                                          
         CLC   LOGONID,GRPUSER                                                  
         BE    SETUSR3                                                          
         CLI   ACTN,X'21'          GROUP USERS CANT CHANGE STATUS               
         BNH   SETUSR3                                                          
         CLI   ACTN,X'2F'                                                       
         BH    SETUSR3                                                          
         LA    R4,SRVP1H           INVALID ACTION                               
         ST    R4,CURSOR                                                        
         B     ERR2                                                             
*                                                                               
SETUSR3  OC    USERID,USERID       MUST INPUT U=... IF NOT LOGGED ON            
         BNZ   *+12                                                             
         CLI   PQSDDSFN,0          WAS U=... INPUT TO OVERRIDE LOGON            
         BE    ERR7                NO                                           
         CLI   PQSDDSFN,0                                                       
         BE    SETUSRX                                                          
         TM    PQSUSER,X'80'       IGNORE GENERIC IDS                           
         BO    SETUSRX                                                          
         MVC   USERID,PQSUSER      SET LOGON ID FROM U=ID                       
         OC    USERID,USERID                                                    
         BNZ   SETUSR4                                                          
         CLI   ACTN,X'20'          TEST PRINTER FUNCTIONS                       
         BL    ERR6                                                             
         B     SETUSRX                                                          
*                                                                               
SETUSR4  MVC   NXSRCID,USERID      GET PRTQ ID FROM U=... VALUE                 
         L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,PQDMCB,(X'00',GFILE),PRTQUE                             
         MVC   PRTQID,NXUSRINF                                                  
         MVC   PRTQIDSV,PRTQID                                                  
*                                                                               
SETUSR5  GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER),PRTQID                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,ACIREC                                                        
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ                 
*                                                                               
SETUSRX  EQU   *                                                                
         TM    INTFLAG,INTRUN      TEST FOR INTERNAL                            
         BO    *+10                                                             
         MVC   PQSVP12A,PQSVP12    SAVE INPUT VALUES                            
         EJECT                                                                  
SAVCHK1  LA    R7,WORK                                                          
         CLI   HLPFLD,6                                                         
         BNL   LOADER              INTERCEPT HELP CALLS                         
         TM    INTFLAG,INTSEL      NEW STATUS                                   
         BO    LOADER                                                           
         CLC   ACTN(2),=X'3103'    TEST ACTION LIST,SEL                         
         BNE   SAVCHK2                                                          
         OI    INTFLAG,INTSEL                                                   
         CLC   PQSLISFM,SRVP3      COMPARE PREV                                 
         BNE   LOADER                                                           
         CLC   PQSLISPL,SRVP4                                                   
         BNE   LOADER                                                           
         NI    INTFLAG,255-INTSEL                                               
SAVCHK2  TM    INTFLAG,INTRUN      ALREADY CHECKED                              
         BO    ACT00               PROCESS NEXT ACTION                          
*                                                                               
         CLI   ACTN,X'21'          CHECK ACTION IS DISPLAY TYPE                 
         BNE   SAVCHKN                                                          
         CLC   PQSVP12A,PQSVP12    CHECK LAST REPORT ID                         
         BNE   SAVCHKN                                                          
         OC    PQSCHANG,PQSCHANG                                                
         BZ    SAVACT1                                                          
SAVCHKN  B     SAVACTX             NORMAL ACTION                                
*                                                                               
SAVACT1  SR    RE,RE                                                            
         IC    RE,PQSPAGE                                                       
         MH    RE,=H'96'           RE=INDEX TO CI ADDRS                         
         LA    R7,PQSCIADS                                                      
         AR    R7,RE               R7= CI ADDRS                                 
         USING CISAVED,R7                                                       
         OC    CISADR,CISADR                                                    
         BZ    SAVACTX                                                          
         CLC   CISADR,FFS          TEST FOR FFS                                 
         BE    SAVACTX                                                          
*                                                                               
         LA    R6,SRVSA1H          R6=FIRST SEL FIELD                           
         XC    HALF2,HALF2                                                      
         XC    BYTE1,BYTE1         CLR CONTINUATION FLAG                        
*                                                                               
*                                  CLEAR PRINTER NAME SELECT TABLE              
         TM    PNMUOPTS,CTIDOFPN   IF USING PRINTER NAMES                       
         BZ    SAVACT2                                                          
         LA    RF,PNMSELTN*(L'PNMSELT)                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    PNMSELT(0),PNMSELT                                               
         LA    RF,PNMSELT                                                       
         ST    RF,PNMSELTP         INITIALISE PNMSEL TABLE POINTER              
*                                                                               
SAVACT2  LA    R1,SRVPFKH          R1=END OF SCREEN                             
         CR    R6,R1                                                            
         BNL   SAVACTX                                                          
         SPACE 1                                                                
SAVACT3  L     R1,ASELTABL                                                      
         XC    HALF,HALF                                                        
         XC    HALF1,HALF1                                                      
         TM    CISACTN,ACTDONE     CHECK FOR PROCESSED ACT                      
         BZ    SAVACT4                                                          
         MVC   HALF1,CISACTN       SAVE ACTION                                  
         NI    HALF1,(X'FF'-X'70') WITHOUT FLAGS SET                            
SAVACT4  NI    CISACTN,ACTPNTG     CLEAR ACTION                                 
*                                                                               
         CLI   PFKEY,4             PF4=DQU                                      
         BNE   *+14                                                             
         CLC   2(2,R6),CURADR                                                   
         BE    ACTNOW                                                           
*                                                                               
         CLI   PFKEY,9             PF9=RUN                                      
         BE    ACTNOW                                                           
*                                                                               
         CLI   BYTE1,1             TEST FOR CONTINUATION                        
         BE    SAVACTR                                                          
SAVACT4A SR    RE,RE                                                            
         ICM   RE,1,5(R6)                                                       
         BZ    SAVACT9                                                          
         CLI   8(R6),C'*'          CHECK PROCESSED FIELD                        
         BNE   *+14                                                             
         MVC   CISACTN,HALF1       RESTORE ACTION                               
         B     SAVACT9                                                          
         CLI   8(R6),C'.'          TEST FOR CONTINUATION                        
         BE    SAVACTR                                                          
         CLI   8(R6),C'"'                                                       
         BNE   *+14                                                             
SAVACTR  MVC   CISACTN(2),HALF2    RESTORE PREVIOUS ACTION                      
         B     SAVACT9B                                                         
         LR    RF,RE                                                            
         CLI   8(R6),C'+'          TEST FOR + OPTION                            
         BNE   *+14                                                             
         MVC   8(1,R6),SR4PRINT    CHANGE TO PRINT                              
         MVI   BYTE1,1             SET REPEAT FLAG                              
*                                                                               
         TM    PNMUOPTS,CTIDOFPN   TEST IF USING PRINTER NAMES                  
         BO    SAVACT4B                                                         
*                                                                               
         TM    8(R6),X'F0'         TEST ALL NUMERIC                             
         BNO   SAVACT4B                                                         
         BCT   RF,*+8                                                           
         B     SAVACT5A                                                         
*                                                                               
         TM    9(R6),X'F0'         TEST FOR PNN                                 
         BNO   SAVACT4B                                                         
         BCT   RF,*+8                                                           
         B     SAVACT5A                                                         
*                                                                               
         TM    10(R6),X'F0'        TEST FOR PNN                                 
         BNO   SAVACT4B                                                         
         BCT   RF,*+8                                                           
         B     SAVACT5A                                                         
*                                                                               
         TM    11(R6),X'F0'        TEST FOR PNNN                                
         BO    SAVACT5A                                                         
*                                                                               
SAVACT4B XC    HALF1,HALF1         FORCE NO MATCH AT SAVACT9                    
         MVC   HALF+1(1),5(R1)                                                  
         NC    HALF,=X'0003'                                                    
         CH    RE,HALF             CHECK MIN LENGTH                             
         BL    SAVACT5                                                          
         TM    5(R1),X'80'         DDS ONLY ACTION                              
         BNO   *+12                                                             
         TM    DDS,DDSTRM          CHECK DDS TERM                               
         BNO   SAVACT5                                                          
         TM    5(R1),X'40'         IF NUMERIC                                   
         BNO   *+8                                                              
         LA    RE,1                JUST VALIDATE 1 CHR                          
         BCTR  RE,0                                                             
         EX    0,0(R1)             RF=A(KEYWORD)                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R6),0(RF)                                                    
         BE    SAVACT6                                                          
*                                                                               
SAVACT5  LA    R1,L'SELTABL(R1)    NEXT TABLE ENTRY                             
         CLI   0(R1),0                                                          
         BNE   SAVACT4A                                                         
*                                  HERE IF SELECT ACTION DOESNT MATCH           
*                                  SELECT TABLE VALUES                          
*                                                                               
         TM    PNMUOPTS,CTIDOFPN   TEST IF USING PRINTER NAMES                  
         BO    *+12                                                             
         OI    CISACTN,ACTDONE+ACTERR  INVALID INPUT                            
         B     SAVACT9                                                          
*                                  ASSUME PRINTER NAME INPUT                    
         L     RF,PNMSELTP         POINT TO PRINTER NAME SELCET TABLE           
         MVC   0(2,RF),CISSEQ      SAVE XREF CI TABLE SEQUENCE NUMBER           
         MVC   2(4,RF),8(R6)       SAVE PRINTER NAME INPUT                      
         LA    RF,L'PNMSELT(RF)    BUMP PNMSEL TABLE POINTER                    
         ST    RF,PNMSELTP                                                      
         LA    RE,PNMSELT                                                       
         SR    RF,RE                                                            
         CLM   RF,3,=YL2(PNMSELTN*(L'PNMSELT))                                  
         BL    *+6                                                              
         DC    H'0'                DIE IF PNMSELT OVERFLOW                      
         MVI   CISACTN,X'05'       HARD CODED START THIS PRINTER NAME           
         OI    CISACTN,ACTPNM      FLAG PRINTER NAME ACTION SELECTED            
         B     SAVACT9                                                          
*                                                                               
SAVACT5A MVI   CISACTN,X'05'       HARD CODED START                             
         BCTR  RE,0                SUB 1 FOR EXECUTE                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R6)                                                   
         CVB   R0,DUB                                                           
         STC   R0,CISNUM           PUT NUMBER INTO CI TAB                       
         B     SAVACT9                                                          
*                                                                               
SAVACT6  CLI   6(R1),X'FF'         SEE IF SPECIAL ACTION                        
         BE    ACTNOW              GO AND PROCESS NOW                           
*                                                                               
         OC    CISACTN,6(R1)       MOVE IN ACTION CODE                          
         TM    5(R1),X'40'         ARE NUMBERS ALLOWED                          
         BNO   SAVACT9                                                          
         CLI   4(R1),X'31'         TEST LIST                                    
         BNE   SAVACT7                                                          
         CLC   9(1,R6),SR@1ST      LF = LIST 1                                  
         BNE   *+12                                                             
         MVI   CISNUM,X'01'                                                     
         B     SAVACT9                                                          
         CLC   9(1,R6),SR@LAST     LL = LIST -0                                 
         BNE   *+12                                                             
         MVI   CISNUM,X'80'                                                     
         B     SAVACT9                                                          
*                                                                               
SAVACT7  LA    RF,9(R6)            POSSIBLE START OF NUMBER                     
         TM    0(RF),X'F0'         TEST FOR NUMBER                              
         BO    *+10                                                             
         SR    RE,RE                                                            
         B     SAVACT7A                                                         
         TM    1(RF),X'F0'                                                      
         BO    *+12                                                             
         LA    RE,1                                                             
         B     SAVACT7A                                                         
         TM    2(RF),X'F0'                                                      
         BO    *+12                                                             
         LA    RE,2                RE = LEN OF NUMBER                           
         B     SAVACT7A                                                         
         LA    RE,3                                                             
SAVACT7A LTR   RE,RE                                                            
         BNZ   SAVACT8                                                          
         MVI   CISNUM,X'00'        DEFAULT = 00                                 
         B     SAVACT9                                                          
*                                                                               
SAVACT8  TM    0(RF),X'F0'         IS NUMBER IN 1 OR 2                          
         BO    *+8                                                              
         LA    RF,1(RF)                                                         
         BCTR  RE,0                SUB 1 FOR EXECUTE                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),0(0,RF)                                                   
         CVB   R0,DUB                                                           
         STC   R0,CISNUM           PUT NUMBER INTO CI TAB                       
         TM    5(R1),X'20'         IS NEG ALLOWED                               
         BNO   SAVACT9                                                          
         TM    CISNUM,X'80'        IF NEG IS ALLOWED                            
         BNO   *+8                                                              
         MVI   CISNUM,X'7F'        127 IS HIGHEST NUMBER                        
         CLI   9(R6),C'-'                                                       
         BNE   SAVACT9                                                          
         OI    CISNUM,X'80'        SET NEGATIVE FLAG                            
*                                                                               
SAVACT9  OC    HALF1,HALF1         WAS THERE AN ACTION                          
         BZ    SAVACT9A                                                         
         CLC   HALF1,CISACTN       IS IT SAME AS THIS ONE                       
         BNE   SAVACT9A                                                         
         OI    CISACTN,ACTDONE     SET PROCESSED                                
*                                                                               
SAVACT9A TM    CISACTN,ACTACTN                                                  
         BZ    SAVACT9C                                                         
         TM    CISACTN,ACTDONE                                                  
         BO    SAVACT9C                                                         
SAVACT9B OI    INTFLAG,INTRUN      FLAG SOMETHING TO DO                         
         MVC   HALF2,CISACTN       SAVE DETAILS IN HALF2                        
SAVACT9C LA    R7,L'CISAVE(R7)     NEXT CI ADDR                                 
         LA    R6,93(R6)                                                        
         CLC   CISAVE,FFS                                                       
         BNE   SAVACT2                                                          
*                                                                               
SAVACTX  TM    INTFLAG,INTRUN                                                   
         BO    ACT00                                                            
         B     LOADER                                                           
                                                                                
***********************************************************************         
* ACTION                                                                        
***********************************************************************         
ACTNOW   CLI   PFKEY,9             PF9=RUN                                      
         BE    ACTRUN                                                           
         XC    8(4,R6),8(R6)       CLR FIELD                                    
         MVC   BYTE,4(R1)          SAVE ACTION                                  
         CLI   4(R1),1             IF ACTION IS DQU                             
         BNE   *+10                                                             
         SR    R6,R3               CALCULATE & SAVE CURSOR POS                  
         STCM  R6,3,PQSCURS                                                     
         MVC   HALF,CISADR                                                      
         BAS   RE,GETREPT          READ RECORD INTO INDEX AREA                  
         LA    R6,CXREC                                                         
         USING PQRECD,R6                                                        
*                                                                               
         CLI   BYTE,1                                                           
         BE    ACTDQU                                                           
         CLI   BYTE,2                                                           
         BE    ACTUSR                                                           
         DC    H'0'                                                             
*                                                                               
ACTRUN   L     R1,APARM                                                         
         L     R1,8(R1)            LOCATE UTL                                   
         L     R1,TBUFF-UTLD(R1)   LOCATE TERMINAL BUFFER                       
         MVI   0(R1),X'0B'         LENGTH FOR =RUN/ACL                          
         MVC   1(2,R1),SRVIDH+2    INSERT SCREEN ADDRESSES                      
         MVC   9(2,R1),SRVP1H+2                                                 
         MVC   3(8,R1),=C'=RUN,CLI'                                             
         MVI   11(R1),0                                                         
         MVC   SRVID(8),=C'=GOBACK '                                            
         B     INTSAVE                                                          
*                                                                               
ACTDQU   LA    RF,WORK             BUILD =DQU REPORT ID STRING IN WORK          
         MVC   WORK,SPACES                                                      
*                                                                               
ACTDQU1  OC    PQSPIN,PQSPIN       TEST IF A PIN IS DEFINED                     
         BZ    ACTDQU2                                                          
         MVC   WORK(2),=C'N='      N=1234                                       
         MVC   WORK+2(4),PQSPIN                                                 
         CLC   WORK+2(4),FFS                                                    
         BNE   *+10                                                             
         MVC   WORK+2(4),DDSPSWD                                                
         TM    DDS,DDSPCAPP        TEST IF FRONTED BY PC APPLICATION            
         BZ    ACTDQU1A                                                         
         MVI   WORK,C'P'           SET SCRAMBLED PIN                            
         MVC   PININ,WORK+2                                                     
         MVI   PINACT,01                                                        
         L     RF,=A(PINMIX)                                                    
         A     RF,RELOBASE                                                      
         BASR  RE,RF                                                            
         MVC   WORK+2(4),PINOUT                                                 
ACTDQU1A LA    RF,WORK+2           FIND END OF PIN                              
         CLI   0(RF),C' '                                                       
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     *-12                                                             
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
ACTDQU2  CLI   PQSDDSFN,C'U'       TEST IF DDS IN U= MODE                       
         BE    ACTDQU3                                                          
         CLC   USERID,PQSRCID      CHECK REPORT ID VALID                        
         BNE   ACT001A                                                          
         CLC   CISSEQ,PQREPNO      CHECK SEQUENCE                               
         BNE   ACT001A                                                          
         B     ACTDQU4                                                          
*                                                                               
ACTDQU3  MVC   GIUSER,PQSRCID      U=USERID                                     
         LR    R0,RF                                                            
         GOTO1 AGETUSER                                                         
         LR    RF,R0                                                            
         MVC   0(2,RF),=C'U='                                                   
         MVC   2(8,RF),GIUSERID                                                 
         SR    R1,R1                                                            
         IC    R1,GIULEN                                                        
         LA    RF,2(RF,R1)                                                      
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
*                                                                               
ACTDQU4  MVC   0(3,RF),PQSUBID     ABC,12345                                    
         MVI   3(RF),C','                                                       
         EDIT  (B2,PQREPNO),(5,4(RF)),ALIGN=LEFT,WRK=WORK1                      
         LR    R1,R0                                                            
         LA    RF,4(R1,RF)                                                      
         LA    RE,WORK                                                          
         SR    RF,RE               RF=LEN OF REPID STRING                       
*                                                                               
ACTDQU5  LA    RF,3(RF)            BUILD STRING IN TBUFF                        
         L     R1,APARM                                                         
         L     R1,8(R1)            LOCATE UTL                                   
         L     R1,TBUFF-UTLD(R1)   LOCATE TERMINAL BUFFER                       
         MVI   0(R1),X'08'         LENGTH FOR =DQU                              
         STC   RF,8(R1)            LENGTH FOR REPID                             
         MVC   1(2,R1),SRVIDH+2    INSERT SCREEN ADDRESSES                      
         MVC   9(2,R1),SRVP1H+2                                                 
         MVC   3(5,R1),DC5DQU      =DQU IN SRVID FIELD                          
         TM    DDS,DDSPUB                                                       
         BNO   *+8                                                              
         MVI   7(R1),C'P'          SET TO =DQUP FOR PUBLIC                      
         TM    DDS,DDSGRP                                                       
         BNO   *+8                                                              
         MVI   7(R1),C'G'          SET TO =DQUG FOR GROUP                       
         MVC   11(L'WORK,R1),WORK                                               
*                                                                               
ACTDQUX  LA    RF,8(R1,RF)                                                      
         MVI   0(RF),0             END MARKER                                   
         MVC   SRVID(8),=C'=GOBACK '                                            
         B     INTSAVE                                                          
*                                                                               
ACTUSR   MVI   INTFLAG,0                                                        
         NI    DDS1,255-DDSTOT     REMOVE TOTALS FLAG IF SET                    
         OI    DDS,DDSNEW          MUST REREAD PQ                               
         MVI   PQSPAGE,0                                                        
         MVC   PQSUSER,PQSRCID                                                  
         MVI   PQSDDSFN,C'U'                                                    
         B     PQ021                                                            
*                                                                               
ACT00    LA    R7,PQSCIADS                                                      
         TM    INTFLAG,INTERR      TEST FOR ERROR                               
         BO    ACTXIT                                                           
ACT001   TM    CISACTN,ACTACTN     TEST FOR ACTION                              
         BZ    ACT001A                                                          
         TM    CISACTN,ACTDONE     SEE IF PROCESSED                             
         BNO   ACT002                                                           
ACT001A  LA    R7,L'CISAVE(R7)                                                  
         CLC   CISAVE,FFS                                                       
         BNE   ACT001                                                           
         B     ACTXIT                                                           
*                                                                               
ACT002   MVC   HALF,CISADR         READ RECORD INTO INDEX AREA                  
         MVC   PQSLAST,CISADR                                                   
         BAS   RE,GETREPT                                                       
         LA    R6,CXREC                                                         
         USING PQRECD,R6                                                        
         IC    R1,CISACTN                                                       
         N     R1,=F'15'                                                        
         LA    R1,ACTMATCH(R1)     FIND REAL ACTION                             
         MVC   ACTN,0(R1)                                                       
         MVC   PQSACT,0(R1)                                                     
         MVI   ACTN1,0                                                          
         MVI   PQSACT1,0                                                        
         CLI   ACTN,X'31'          IF LIST                                      
         BNE   *+12                                                             
         MVI   ACTN1,3             SET LIST,SEL                                 
         MVI   PQSACT1,X'03'                                                    
*                                                                               
         CLC   USERID,PQSRCID      CHECK USER ID                                
         BNE   ACT001A                                                          
         CLC   CISSEQ,PQREPNO      CHECK SEQUENCE                               
         BNE   ACT001A                                                          
*                                                                               
         MVC   USERID,PQSRCID      SAVE REPORT DATA                             
         MVC   PQSSUBID,PQSUBID                                                 
         MVC   PQSSEQ,PQREPNO                                                   
         MVI   PQSCLASS,0                                                       
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         MVI   RIDFLAG,X'FF'                                                    
         GOTO1 RIDXPND,SRVP2H      REDISPLAY P2                                 
         XC    SRVP1,SRVP1                                                      
         MVI   SRVP1H+5,0                                                       
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY P1                                 
*                                                                               
         XC    SRVP3,SRVP3         CLEAR P3 P4                                  
         XC    SRVP4,SRVP4                                                      
         MVI   SRVP3H+5,0                                                       
         MVI   SRVP4H+5,0                                                       
         OI    SRVP3H+6,X'40'                                                   
*                                                                               
         LA    RE,SRVP1                                                         
         CLI   ACTN,X'21'          SELECT ACTION                                
         BNE   ACT003                                                           
         XC    SRVP1,SRVP1                                                      
         MVC   0(8,RE),SR@SLECT                                                 
         OI    SRVP1H+6,X'80'                                                   
         B     ACT00X                                                           
*                                                                               
ACT003   CLI   ACTN,X'31'          LIST SELECT ACTION                           
         BE    ACT006                                                           
*                                                                               
ACT004   CLI   ACTN,X'11'          SCHEDULE OR START                            
         BE    ACT005                                                           
         CLI   ACTN,X'14'                                                       
         BNE   ACT00X                                                           
*                                                                               
ACT005   CLI   CISNUM,0            SCHEDULE OR START                            
         BE    ACT005A             TRY FOR PRINTER NAME SELECT                  
         EDIT  (B1,CISNUM),(3,SRVP3),ALIGN=LEFT                                 
         STC   R0,SRVP3H+5                                                      
         OI    SRVP3H+6,X'80'                                                   
         MVI   SRVP3H+4,X'08'      FORCE NUMERIC                                
         B     ACT00X                                                           
*                                                                               
ACT005A  LA    RF,PNMSELT          START OF PRINTER NAME SELECT TABLE           
         LA    RE,PNMSELTN         MAX. NUMBER OF ENTRIES IN PNMSELT            
ACT005B  OC    0(2,RF),0(RF)       TEST END OF TABLE                            
         BZ    ACT00X                                                           
         CLC   CISSEQ,0(RF)        MATCH CI SEQUENCE NUMBER                     
         BE    ACT005C                                                          
         LA    RF,L'PNMSELT(RF)                                                 
         BCT   RE,ACT005B                                                       
         B     ACT00X                                                           
ACT005C  MVC   SRVP3(4),2(RF)       MOVE PRINTER NAME INTO FIELD P3             
         MVI   SRVP3H+5,4                                                       
         OI    SRVP3H+6,X'80'                                                   
         B     ACT00X                                                           
*                                                                               
ACT006   CLI   CISNUM,0            LIST                                         
         BNE   *+8                                                              
         MVI   CISNUM,X'01'                                                     
         TM    CISNUM,X'80'                                                     
         BO    ACT006A                                                          
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),CISNUM                                                 
         B     ACT006B                                                          
ACT006A  MVI   HALF,0                                                           
         MVC   HALF+1(1),CISNUM                                                 
         NI    HALF+1,(X'FF'-X'80')                                             
         SR    R0,R0                                                            
         ICM   R0,3,PQPAGES                                                     
         SH    R0,HALF                                                          
         STH   R0,HALF                                                          
ACT006B  EDIT  (B2,HALF),(4,SRVP4),ALIGN=LEFT                                   
         STC   R0,SRVP4H+5                                                      
         LR    R1,R0                                                            
         LA    R1,SRVP4(R1)                                                     
         XC    0(4,R1),0(R1)                                                    
         MVC   SRVP3+0(1),SR@LEFT                                               
         MVC   SRVP3+1(1),SR@CMPCT                                              
         MVI   SRVP3H+5,2                                                       
         OI    SRVP4H+6,X'80'                                                   
         OI    SRVP3H+6,X'80'                                                   
*                                                                               
ACT00X   L     R1,ASELTABL                                                      
         NI    INTFLAG,255-INTCONT                                              
ACT00X1  CLC   ACTN,4(R1)          FIND TABLE ENTRY                             
         BE    ACT00X2                                                          
         LA    R1,L'SELTABL(R1)                                                 
         B     ACT00X1                                                          
ACT00X2  TM    5(R1),X'10'         CONTINUE AFTER ACTION                        
         BNO   LOADER                                                           
         OI    INTFLAG,INTCONT                                                  
         B     LOADER                                                           
*                                                                               
ACTXIT   MVC   PQSVP12,PQSVP12A    RESTORE ORIGIONAL PARMS                      
         MVI   ACTN,X'21'          FORCE TO DISPLAY                             
         XC    SRVP2,SRVP2                                                      
         MVI   SRVP2H+5,0                                                       
         MVI   RIDFLAG,X'FF'                                                    
         GOTO1 RIDXPND,SRVP2H      REDISPLAY P2                                 
         XC    SRVP1,SRVP1                                                      
         MVI   SRVP1H+5,0                                                       
         GOTO1 ACTNXPND,SRVP1H     REDISPLAY P1                                 
*                                                                               
         MVC   SRVP3(15),PQSP3FLD                                               
         MVC   SRVP4,PQSP4FLD                                                   
         MVC   SRVP3H+5(1),PQSP3LEN                                             
         MVC   SRVP4H+5(1),PQSP4LEN                                             
         MVI   PAGFLAG,X'FF'                                                    
         NI    INTFLAG,INTERR      CLEAR ALL FLAGS EXCEPT ERROR                 
         OI    INTFLAG,INTLAST     AND SET LAST                                 
         NI    DDS,255-DDSNEW      ENSURE RELOAD FLAG HAS CLEARED               
         SPACE 2                                                                
LOADER   EQU   *                                                                
*                                                                               
LOADER0  L     RF,=A(TRAPPER)      TEST TRAPS FOR PQ SPECIALS                   
         A     RF,RELOBASE                                                      
         BASR  RE,RF                                                            
*                                                                               
LOADER1  CLI   ACTN,X'31'          TEST ACTION DISPLAY                          
         BNE   LOADER2                                                          
         MVC   PQSLISFM,SRVP3      SAVE FOR NEXT TIME IN                        
         MVC   PQSLISPL,SRVP4                                                   
LOADER2  XC    DMCB+8(4),DMCB+8    LOAD SCREEN AND CODE FOR FUNCTION            
         CLI   ACTN,X'20'                                                       
         BL    LOADPGM             PRINTER ACTIONS - WE HAVE SCREEN             
         CLI   ACTN,X'28'                                                       
         BNE   *+14                SIZE ACTION - NO FURTHER CHECKING            
         XC    PQSSEQ,PQSSEQ       SET PQSSEQ TO ZERO                           
         B     LOADSCR                                                          
         OC    PQSREPID,PQSREPID                                                
         BZ    ERR4                OTHER ACTIONS MUST HAVE REPORT ID            
*                                                                               
LOADCHK  CLI   ACTN,X'31'          TEST LIST                                    
         BE    LOADCHKX                                                         
         CLI   ACTN,X'29'          TEST RETAIN                                  
         BNE   *+12                                                             
         TM    DDS,DDSTRM+DDSPRV                                                
         BZ    LOADCHKX                                                         
         CLC   ACTN(2),=X'2401'    TEST ACTIVATE,RETAIN                         
         BNE   *+12                                                             
         TM    DDS,DDSTRM+DDSPRV                                                
         BZ    LOADCHKX                                                         
         CLI   ACTN,X'2B'          TEST UNERROR                                 
         BNE   *+12                                                             
         TM    DDS,DDSTRM                                                       
         BZ    LOADCHKX                                                         
         B     LOADSCR                                                          
LOADCHKX OC    PQSSEQ,PQSSEQ       TEST ACTION ONLY ON SPECIFIC ID              
         BZ    ERR3                                                             
*                                                                               
LOADSCR  LA    R6,SRVEX2H          LOAD SCREEN                                  
         MVC   DMCB+4(4),=X'D90131FF'                                           
         ST    R6,DMCB                                                          
         OC    PQSSEQ,PQSSEQ                                                    
         BZ    *+8                                                              
         MVI   DMCB+7,X'FD'        SET SPECIFIC REPORT SCREEN                   
*                                                                               
         TM    INTFLAG,INTRUN      TEST INTERNAL                                
         BNO   LOADSCRA                                                         
         MVCDD SRVP3A,SR#NSTAT                                                  
         MVC   SRVP4A,SPACES                                                    
         B     LOADSCRB                                                         
LOADSCRA MVCDD SRVP3A,SR#REPTY                                                  
         MVCDD SRVP4A,SR#RPTFL                                                  
LOADSCRB OI    SRVP3AH+6,X'80'                                                  
         OI    SRVP4AH+6,X'80'                                                  
*                                                                               
         CLI   ACTN,X'30'                                                       
         BL    LOADSCR1                                                         
         CLI   ACTN,X'3F'                                                       
         BH    LOADSCR1                                                         
         MVI   DMCB+7,X'FE'        SET LIST FUNCTION SCREEN                     
*                                                                               
         MVCDD SRVP3A,SR#LSTFM     LIST FORMAT                                  
         OI    SRVP3AH+6,X'80'                                                  
         MVCDD SRVP4A,SR#PGLN      PAGE,LINE                                    
         OI    SRVP4AH+6,X'80'                                                  
*                                                                               
LOADSCR1 CLC   DMCB+4(4),=X'D90131FF'                                           
         BE    LOADPGM                                                          
         TM    INTFLAG,INTCONT                                                  
         BO    LOADPGM                                                          
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOADPGM  CLI   HLPFLD,0            CHECK HELP ACTION                            
         BNE   HELPOUT                                                          
         SR    R0,R0                                                            
         IC    R0,ACTN             LOAD OVERLAY PHASE                           
         SRL   R0,4                                                             
*                                                                               
         STC   R0,HALF             ACTIONS 40-4F ARE OVERLAY 2                  
         CLI   HALF,4                                                           
         BNE   *+8                                                              
         LA    R0,2                                                             
*                                                                               
         SLL   R0,28                                                            
         SRL   R0,4                                                             
         GOTO1 VCALLOV,DMCB,(R0),(R3)                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            PASS CONTROL TO OVERLAY                      
         LR    R1,RC               R1=A(ROOT WORKING STORAGE)                   
         BASR  RE,RF                                                            
*                                                                               
         CLC   PRTQID,PRTQIDSV     RESTORE PQ IF CHANGED **NEW APR94**          
         BE    NEXT010                                                          
         MVC   PRTQID,PRTQIDSV                                                  
         L     R5,ACIREC                                                        
         GOTO1 VDATAMGR,PQDMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,0(R5)      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,12(R5)       SAVE FILE DATA FOR THIS PRTQ                 
         L     RE,8(R5)            POINT TO SAVE AREA IN BUFFER                 
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
*                                                                               
NEXT010  TM    INTFLAG,INTRUN                                                   
         BNO   INTSAVE             EXIT NORMAL ACTION                           
         TM    INTFLAG,INTERR                                                   
         BNO   INTOK                                                            
         TM    INTFLAG,INTSEL                                                   
         BO    EXIT                EXIT SELECT NEW STATUS                       
         SPACE 1                                                                
         OI    CISACTN,ACTERR      FLAG REPORT WITH ERROR                       
         L     RE,ATCB             RESET SYSTEM IN TCB                          
         MVI   TCBOVSYS-TCBD(RE),X'01'    AFTER GETXT CALL                      
         SR    RE,RE                                                            
         B     ACT00               RETURN TO DISPLAY SCREEN                     
*                                                                               
INTOK    OI    CISACTN,ACTDONE     SET ACTION OK                                
         NI    INTFLAG,255-INTSEL  CLEAR SELECT IF SET                          
         TM    INTFLAG,INTCONT                                                  
         BO    ACT00                                                            
         CLI   ACTN,X'21'                                                       
         BNE   INTSAVE                                                          
         GOTO1 AGETTXT,DMCB,178,0,(C'I',0),0,,X'00010000'                       
*                                                                               
INTSAVE  L     R5,ATIA             WRITE SAVE PAGE                              
         USING SRSD,R5                                                          
         LA    R4,SRPAGENO                                                      
         SLL   R4,32-8                                                          
         ICM   R4,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R4),SRSD                            
*                                                                               
EXIT     TM    DDS,DDSERR          DID WE ENCOUNTER A FORMAT ERR                
         BNO   EXIT0                                                            
         NI    DDS,255-DDSERR      REPORT IT ON EXIT                            
         B     ERR12                                                            
*                                                                               
EXIT0    CLC   ACTN(2),=X'1104'    TEST FOR START,PFKEY                         
         BNE   EXIT1                                                            
         MVC   SAVE(68),SRVMSGH    SAVE MESSAGE DATA                            
         SR    R4,R4               READ TWA0 INTO TWA                           
         ICM   R4,3,TRM                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',(R4),(R3)                   
         MVC   SRVMSGH(68),SAVE    RESTORE MESSAGE DATA                         
         B     EXIT2                                                            
*                                                                               
EXIT1    GOTO1 ASQUASH,DMCB,SRVP1,(C',',15)                                     
*                                                                               
EXIT2    L     R1,ATCB             UNLOCK PRTQ                                  
         TM    TCBFLAG1-TCBD(R1),TCBENQPQ                                       
         BNO   EXITXX                                                           
         MVC   CFPQID,=C'PRTQU'                                                 
         L     R1,ATCB             USE TCB TO GET PRTQ CHR                      
         CLI   TCBPQCHR-TCBD(R1),0                                              
         BE    EXIT3                                                            
         MVC   CFPQID+4(1),TCBPQCHR-TCBD(R1)                                    
*                                                                               
         GOTO1 CIENQDEQ,DMCB,(C'D',CFPQID)                                      
         MVI   LOCKT,0                                                          
         B     EXITXX                                                           
*                                                                               
EXIT3    DC    H'0'                SOMETHINGS WRONG                             
*                                                                               
EXITXX   XMOD1 1                                                                
         ORG   *-2                                                              
         BSM   0,RE                RESTORE CALLER'S ADDRESSING MODE             
         EJECT                                                                  
*************************************************************                   
*                CALL GETHELP AND EXIT                      *                   
*************************************************************                   
HELPOUT  LA    R1,HLPIND6                                                       
         CLI   HLPFLD,6            CHECK FOR SEL FIELDS                         
         BNL   HELP005                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
         LA    R1,HLPIND4                                                       
         LA    R1,HLPIND5                                                       
*                                                                               
HELP005  L     RF,APARM                                                         
         L     RF,28(RF)           TIOB                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
         MVI   TIOBCURI,X'01'      ONE PAST                                     
         LA    RE,SRVP4H           REPORT FILT                                  
         SR    RE,R3                                                            
         STCM  RE,3,TIOBCURD                                                    
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
*                                                                               
HELP010  CLC   0(2,R1),=H'0'                                                    
         BE    HELP020                                                          
         CLC   0(1,R1),ACTN        MATCH ACTION                                 
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BE    HELP020                                                          
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
HELP020  MVC   SBYTE,1(R1)         SBYTE=INDEX                                  
         CLI   SBYTE,X'15'         PRINTER ID                                   
         BNE   HELP025                                                          
         MVC   HALF,LOGONID                                                     
         TM    HLPFLG,X'80'        TEST DDS HELP                                
         BNO   *+10                                                             
         MVC   HALF,PQSUSER        DDS SEE PRINTERS FOR U=USER                  
         L     RF,=A(GETPRNT)                                                   
         A     RF,RELOBASE                                                      
         BASR  RE,RF                                                            
HELP025  LA    R1,HELPTAB                                                       
HELP030  CLC   SBYTE,0(R1)                                                      
         BNE   HELP040                                                          
         TM    1(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP031                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   HELP040                                                          
HELP031  MVC   HELPNUM,2(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         SR    RF,RF                                                            
         TM    1(R1),X'40'         EXTRA TEXT ?                                 
         BNO   *+8                                                              
         L     RF,ACIREC           EXTRA TEXT IS IN CIREC                       
         L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         CLI   LOCKT,0                                                          
         BE    *+6                                                              
         DC    H'0'                PQ "MUST" BE UNLOCKED                        
         GOTO1 AGETHELP,DMCB,(X'50',HELPKEY),QHDR,(C'B',0),(RF),0               
         DC    H'0'                GETHELP EXITS TO MONITOR                     
HELP040  LA    R1,L'HELPTAB(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   HELP030                                                          
         B     INF0                                                             
         EJECT                                                                  
GETREPT  NTR1                      PUT CIADDR INTO HALF                         
         LA    R6,CXREC                                                         
         USING PQRECD,R6                                                        
         OC    HALF,HALF                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   CIADDR(2),HALF                                                   
         MVI   CIADDR+2,1                                                       
         MVI   CIADDR+3,0                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R6)                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R6)            SET DISK ADDR IN SAVE AREA                   
         AR    RE,R6                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
         XC    SAVE(12),SAVE                                                    
         MVC   SAVE+4(4),=C'LINE'                                               
         GOTO1 VDATAMGR,DMCB,(X'00',RANDOM)                                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        ACTION EXPAND    NTR  R1=A(FLDHDR)                 *                   
*************************************************************                   
ACTNXPND NTR1                                                                   
         ST    R1,SFULL                                                         
         LA    R4,SWORK            USE SWORK FOR EXPAND                         
         XC    SWORK,SWORK                                                      
*                                                                               
         CLI   PQSQUEUE,0          TEST FOR Q=U                                 
         BE    ACTX010                                                          
         MVC   0(4,R4),=C'Q=U '                                                 
         MVC   2(1,R4),PQSQUEUE                                                 
         LA    R4,4(R4)                                                         
*                                                                               
ACTX010  L     R7,=A(ACTNTBL)      EXPAND ACTION                                
         A     R7,RELOBASE                                                      
ACTX011  CLC   4(1,R7),PQSACT                                                   
         BE    ACTX020                                                          
         LA    R7,L'ACTNTBL(R7)    TRY NEXT                                     
         CLI   0(R7),0                                                          
         BNE   ACTX011                                                          
         B     ERR2                                                             
*                                                                               
ACTX020  EX    0,0(R7)             RF=A(ACTION)                                 
         MVC   0(8,R4),0(RF)       MOVE KEYWORD                                 
         MVI   9(R4),C' '                                                       
         LA    R4,9(R4)                                                         
*                                                                               
         CLI   PQSACT1,0           ARE WE FINISHED YET                          
         BE    ACTX990                                                          
         CLI   PQSACT1,X'FF'       TEST FOR NUMERIC SUB ACTION                  
         BNE   ACTX030                                                          
         EDIT  (B1,PQSACT2),(3,0(R4)),WRK=SWORK1,DUB=SDUB                       
         B     ACTX990                                                          
*                                                                               
ACTX030  ICM   R7,7,7(R7)          LOCATE SUB ACTION KEYWORD                    
         A     R7,RELOBASE                                                      
ACTX031  CLC   5(1,R7),PQSACT1                                                  
         BE    ACTX040                                                          
         LA    R7,L'SACTLEN(R7)                                                 
         CLI   0(R7),0                                                          
         BNE   ACTX031                                                          
         DC    H'0'                                                             
*                                                                               
ACTX040  EX    0,0(R7)             RF=A(SACTION)                                
         MVC   0(8,R4),0(RF)       MOVE KEYWORD                                 
         MVI   8(R4),C' '                                                       
*                                                                               
ACTX990  L     R4,SFULL            RECOVER FIELD                                
         GOTO1 ASQUASH,DMCB,SWORK,(C',',25)                                     
         LA    RF,16                                                            
         CLI   7(R1),16            15 IS MAXIMUM LEN                            
         BH    *+8                                                              
         IC    RF,7(R1)                                                         
         STC   RF,5(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SWORK       MOVE SWORK INTO FIELD                        
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        ACTION VALIDATION    R1=A(FLDHDR)                  *                   
*************************************************************                   
P1VAL    NTR1                                                                   
         LR    R4,R1                                                            
         ST    R4,CURSOR                                                        
         MVC   SWORK1(4),PQSVACT   SAVE OLD VALUES IN SWORK1                    
         XC    PQSVACT,PQSVACT     CLEAR RETURN AREA                            
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    INF1                MISSING ACTION                               
         L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(3,(R6))                                      
         MVC   SBYTE,4(R1)                                                      
         CLI   SBYTE,0                                                          
         BE    ERR2                INVALID ACTION                               
         TM    DDS,DDSTRM                                                       
         BNO   P1V1                                                             
*                                                                               
P1VPQ    CLI   1(R6),0             FIRST PARM CAN BE Q=PRTQID                   
         BE    P1V1                                                             
         CLI   12(R6),C'Q'                                                      
         BNE   ERR2                                                             
         CLI   1(R6),1             PRTQID IS 1 CHR FIELD                        
         BNE   ERR2                                                             
         CLI   22(R6),C'U'         Q=U MEANS THIS USERS QUEUE                   
         BE    P1V015                                                           
         LA    RE,PRTQLST+8        MATCH CHR AGAINST PRTQ LIST                  
P1V010   CLI   0(RE),0                                                          
         BE    ERR10                                                            
         CLC   22(1,R6),1(RE)                                                   
         BE    P1V015                                                           
         LA    RE,8(RE)                                                         
         B     P1V010                                                           
*                                                                               
P1V015   MVC   PQSQUEUE,22(R6)     SAVE Q=CHR                                   
         SR    RF,RF               DROP Q=PRTQID FIELD                          
         IC    RF,SBYTE                                                         
         AHI   RF,-1               MUST HAVE AT LEAST ONE MORE                  
         BZ    ERR2                                                             
         STC   RF,SBYTE            RESET NUMBER OF FIELDS                       
         LA    R6,32(R6)                                                        
*                                                                               
P1V1     ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         CLI   1(R6),0             PART#1 IS ACTION                             
         BNE   ERR2                                                             
         CHI   R1,2                ACTION IS 3 THRU 8 CHRS                      
         BL    ERR2                                                             
         CHI   R1,7                                                             
         BL    *+8                                                              
         LA    R1,7                                                             
         L     R7,=A(ACTNTBL)      EXPAND ACTION                                
         A     R7,RELOBASE                                                      
P1V1A    CLI   0(R7),0             SEARCH ACTION TABLE                          
         BE    ERR2                                                             
         CLM   R1,1,5(R7)          IS I/P LEN > KEYWORD                         
         BNL   P1V1A1              IF SO DONT COMPARE                           
         TM    6(R7),X'40'         TEST FOR DDS ONLY ACTION                     
         BNO   *+12                                                             
         TM    DDS,DDSTRM          MUST BE DDS TERMINAL                         
         BNO   P1V1A1                                                           
         TM    6(R7),X'80'         TEST FOR VALID NEW STATUS                    
         BO    *+12                                                             
         TM    INTFLAG,INTRUN                                                   
         BO    P1V1A1                                                           
         EX    0,0(R7)             RF=A(ACTION)                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)      COMPARE KEYWORD                              
         BNE   P1V1A1                                                           
         MVC   PQSACT,4(R7)        SAVE ACTION VALUE                            
         B     P1V2                                                             
*                                                                               
P1V1A1   LA    R7,L'ACTNTBL(R7)    TRY NEXT                                     
         B     P1V1A                                                            
*                                                                               
P1V2     CLI   SBYTE,1             PART#2 IS SUB ACTION                         
         BE    P1VX                                                             
         LA    R6,32(R6)                                                        
         ICM   R7,7,7(R7)          TEST IF SUB ACTION ALLOWED                   
         BZ    ERR8                                                             
         C     R7,=F'1'            1 MEANS INTEGER VALUE ALLOWED                
         BE    P1V3                                                             
         A     R7,RELOBASE         ELSE IS ADDRESS OF A TABLE                   
         CLI   1(R6),0                                                          
         BNE   ERR8                                                             
         CLI   0(R6),1             SUB ACTION IS 1 THRU 8 CHRS                  
         BL    ERR8                                                             
         CLI   0(R6),8                                                          
         BH    ERR8                                                             
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
P1V2A    CLI   0(R7),0             SEARCH SUB ACTION TABLE                      
         BE    ERR8                                                             
         EX    0,0(R7)             RF=A(SACTION)                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(RF)                                                   
         BE    P1V2B                                                            
         LA    R7,L'SACTLEN(R7)                                                 
         B     P1V2A                                                            
P1V2B    MVC   PQSACT1,5(R7)       SAVE SUB ACTION VALUE                        
         B     P1VX                                                             
*                                                                               
P1V3     MVI   PQSACT1,X'FF'       SET INTEGER VALUE IN ACTN2                   
         CLI   1(R6),0                                                          
         BNE   ERR8                                                             
         CLI   0(R6),1             SUB ACTION IS INTEGER 1-3 DIGITS             
         BL    ERR2                                                             
         CLI   0(R6),3                                                          
         BH    ERR8                                                             
         TM    2(R6),X'80'         MUST BE NUMERIC IN RANGE 1-255               
         BZ    ERR8                                                             
         L     RE,4(R6)                                                         
         CHI   RE,1                                                             
         BL    ERR8                                                             
         CHI   RE,255                                                           
         BH    ERR8                                                             
         STC   RE,PQSACT2          SAVE INTEGER VALUE IN ACTN2                  
P1VX     CLC   PQSVACT,SWORK1                                                   
         BE    XIT1                                                             
         MVI   PQSCHACT,X'FF'      FLAG ACTION CHANGED                          
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        EXPAND REPORT ID  R1=A(FLDHDR) & RIDFLAG=OPTIONS   *                   
*************************************************************                   
RIDXPND  NTR1                                                                   
         ST    R1,SFULL                                                         
         LA    R4,SWORK            USE SWORK FOR EXPAND                         
         XC    SWORK,SWORK                                                      
*                                                                               
RIDX005  OC    PQSPIN,PQSPIN       TEST IF PIN IS ACTIVE                        
         BZ    RIDX005X                                                         
         MVC   0(6,R4),=C'N=****'                                               
         TM    DDS,DDSPCAPP        TEST IF FRONTED BY PC APPLICATION            
         BZ    RIDX005A                                                         
         MVI   0(R4),C'P'          DISPLAY SCRAMBLED PIN                        
         MVC   2(4,R4),PQSPIN                                                   
         CLC   PQSPIN,FFS          TEST IF DDS PIN                              
         BNE   *+10                                                             
         MVC   2(4,R4),DDSPSWD                                                  
         MVC   PININ,2(R4)                                                      
         MVI   PINACT,01                                                        
         L     RF,=A(PINMIX)                                                    
         A     RF,RELOBASE                                                      
         BASR  RE,RF                                                            
         MVC   2(4,R4),PINOUT                                                   
RIDX005A LA    R4,7(R4)                                                         
RIDX005X EQU   *                                                                
*                                                                               
RIDX010  TM    RIDFLAG,DDSUID      TEST TO DISPLAY U=.....                      
         BZ    RIDX010X                                                         
         CLI   PQSDDSFN,0          ANY U= OR V=                                 
         BE    RIDX010X                                                         
         CLI   PQSDDSFN,C'P'                                                    
         BNE   RIDX010A                                                         
         MVC   0(8,R4),SR@PUBLC    P MEANS PUBLIC ENTERED                       
         LA    R4,8(R4)                                                         
         B     RIDX010X                                                         
RIDX010A CLI   PQSDDSFN,C'G'                                                    
         BNE   RIDX010B                                                         
         MVC   0(8,R4),DC@GRP      G MEANS GROUP ENTERED                        
         LA    R4,8(R4)                                                         
         B     RIDX010X                                                         
RIDX010B MVC   0(1,R4),PQSDDSFN                                                 
         MVI   1(R4),C'='                                                       
         OC    PQSUSER,PQSUSER     TEST FOR ALL                                 
         BZ    RIDX010C                                                         
         MVC   GIUSER,PQSUSER                                                   
         BAS   RE,GETUSER          GET USER ID                                  
         MVC   2(8,R4),GIUSERID                                                 
         B     RIDX010D                                                         
*                                                                               
RIDX010C MVC   2(8,R4),SR8ALL      SET U=ALL OR V=ALL                           
RIDX010D LA    R4,11(R4)                                                        
RIDX010X EQU   *                                                                
*                                                                               
RIDX020  TM    RIDFLAG,DDSAGY      TEST TO DISPLAY A=XX                         
         BZ    RIDX020X                                                         
         CLI   PQSRAGY,0           TEST IF AGENCY WAS INPUT                     
         BE    RIDX020X                                                         
         MVC   0(2,R4),=C'A='                                                   
         MVC   2(2,R4),PQSRAGY                                                  
         LA    R4,5(R4)                                                         
RIDX020X EQU   *                                                                
*                                                                               
RIDX021  TM    RIDFLAG,DDSPID      TEST TO DISPLAY I=XXX....                    
         BZ    RIDX021X                                                         
         OC    PQSRPID,PQSRPID     TEST IF PID WAS INPUT                        
         BZ    RIDX021X                                                         
         CLC   PQSRPID,TRMPID                                                   
         BNE   RIDX021A                                                         
         MVC   0(3,R4),=C'I=*'                                                  
         LA    R4,4(R4)                                                         
         B     RIDX021X                                                         
RIDX021A CLC   PQSRPID,FFS                                                      
         BNE   RIDX021B                                                         
         MVC   0(2,R4),=C'I=*'                                                  
         MVC   2(4,R4),DDSPSWD                                                  
         LA    R4,7(R4)                                                         
         B     RIDX021X                                                         
RIDX021B EQU   *                                                                
RIDX021X EQU   *                                                                
*                                                                               
RIDX022  TM    RIDFLAG,DDSREF      TEST TO DISPLAY R=XXX                        
         BZ    RIDX022X                                                         
         OC    PQSRREF,PQSRREF                                                  
         BE    RIDX022X                                                         
         MVC   0(2,R4),=C'R='                                                   
         MVC   2(3,R4),PQSRREF                                                  
         LA    R4,6(R4)                                                         
RIDX022X EQU   *                                                                
*                                                                               
RIDX023  TM    RIDFLAG,DDSTYP      TEST TO DISPLAY T=X                          
         BZ    RIDX023X                                                         
         CLI   PQSRTYP,0                                                        
         BE    RIDX023X                                                         
         MVC   0(2,R4),=C'T='                                                   
         MVC   2(1,R4),PQSRTYP                                                  
         LA    R4,4(R4)                                                         
RIDX023X EQU   *                                                                
*                                                                               
RIDX030  CLI   PQSCLASS,0          ANY CLASS ?                                  
         BE    RIDX040                                                          
         LR    RF,R4                                                            
         MVC   0(1,R4),PQSCLASS    MOVE IN CLASS VALUE                          
         LA    R4,2(R4)                                                         
         TM    PQSCLASS,X'40'                                                   
         BO    RIDX040                                                          
         MVI   0(RF),C'-'          IF LOWER CASE SET -CLASS                     
         MVC   1(1,RF),PQSCLASS                                                 
         OI    1(RF),X'40'                                                      
         LA    R4,1(R4)                                                         
*                                                                               
RIDX040  MVC   0(3,R4),PQSSUBID    ELSE MOVE IN 3 CHRS                          
         CLC   PQSSUBID,=X'FFFFFF'                                              
         BNE   *+10                                                             
         MVC   0(4,R4),DC@JOBS     SPECIAL =RUN LINK                            
*                                                                               
RIDX045  CLC   0(3,R4),SR8ALL                                                   
         BNE   RIDX04X                                                          
         CLI   TRMLANG,X'07'       GERMANS & DUTCH HAVE 4CHR ALLE               
         BE    *+12                                                             
         CLI   TRMLANG,X'03'                                                    
         BNE   RIDX04X                                                          
         MVC   0(4,R4),SR8ALL                                                   
         LA    R4,1(R4)                                                         
RIDX04X  LA    R4,4(R4)                                                         
*                                                                               
RIDX050  OC    PQSSEQ,PQSSEQ       EDIT SEQUENCE NUMBER                         
         BZ    RIDX060                                                          
         SR    R0,R0                                                            
         ICM   R0,3,PQSSEQ                                                      
         EDIT  (R0),(6,0(R4)),ZERO=NOBLANK,DUB=SDUB,WRK=SWORK1                  
         B     RIDX990                                                          
*                                                                               
RIDX060  OC    PQSTIMES,PQSTIMES   OR TIMBER OUT TIME VALUES                    
         BZ    RIDX990                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,PQSTIMES                                                    
         SLL   RF,2                * 4                                          
         D     RE,=F'180'                                                       
         STCM  RF,3,SDUB                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,PQSTIMES+2                                                  
         SLL   RF,2                * 4                                          
         D     RE,=F'180'                                                       
         STCM  RF,3,SDUB+2                                                      
         LA    R1,20                                                            
         ST    R1,PARMS                                                         
         OC    PQSTIMES(2),PQSTIMES                                             
         BNZ   *+14                                                             
         OI    PARMS,X'40'                                                      
         MVC   SDUB(2),SDUB+2                                                   
         GOTO1 ATIMBER,PARMS,,(X'02',SDUB),(4,SWORK1)                           
         MVC   0(12,R4),SWORK1                                                  
*                                                                               
RIDX990  L     R4,SFULL            RECOVER FIELD                                
         GOTO1 ASQUASH,DMCB,SWORK,(C',',32)                                     
         LA    RF,16                                                            
         CLI   7(R1),32            32 IS MAXIMUM LEN                            
         BH    *+8                                                              
         ICM   RF,1,7(R1)                                                       
         STC   RF,5(R4)                                                         
         BZ    XIT1                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),SWORK       MOVE SWORK INTO FIELD                        
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE REPORT ID  R1=A(FLDHDR)                   *                   
*************************************************************                   
P2VAL    NTR1                                                                   
         LR    R4,R1                                                            
         ST    R4,CURSOR                                                        
         MVC   SWORK(L'PQSREPID),PQSREPID                                       
         MVI   DDS2,0                                                           
         XC    PQSREPID,PQSREPID    CLEAR RETURN AREA                           
         MVI   PQSRIDIF,0                                                       
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BNE   P2V0                                                             
         CLI   PQSACT,X'14'         SCHEDULE MUST HAVE P2                       
         BE    ERR4                                                             
         CLI   PQSACT,X'28'         SIZE DOES NOT NEED P2                       
         BE    P2V110                                                           
         CLI   PQSACT,X'1F'                                                     
         BH    ERR4                 SO MUST REPORT ACTIONS                      
         B     P2V110               DON'T SET ALL IF NO INPUT                   
*                                                                               
P2V0     L     R6,ACIREC                                                        
         GOTO1 ASCANNER,DMCB,(R4),(8,(R6))                                      
         MVC   SBYTE,4(R1)                                                      
         CLI   SBYTE,0                                                          
         BE    ERR3                INVALID REPORT ID                            
*                                                                               
P2V1     CLI   1(R6),0             TEST KEYWORD=VALUE TYPE FIELD                
         BE    P2V1X                                                            
         CLC   12(8,R6),DC@PIN     CAN BE PIN=                                  
         BE    P2V1A                                                            
         CLC   12(2,R6),=C'N '     OR N=                                        
         BE    P2V1A                                                            
         CLC   12(2,R6),=C'P '     OR P= FOR SCRAMBLED PIN                      
         BNE   P2V1X                                                            
         TM    DDS,DDSPCAPP        MUST BE FRONT ENDED BY PC APP                
         BZ    P2V1A                                                            
         CLI   1(R6),4                                                          
         BNE   P2V1A                                                            
         MVC   PININ,22(R6)        UNSCRAMBLE PIN                               
         MVI   PINACT,2                                                         
         L     RF,=A(PINMIX)                                                    
         A     RF,RELOBASE                                                      
         BASR  RE,RF                                                            
         MVC   22(4,R6),PINOUT                                                  
P2V1A    CLI   1(R6),1             PIN IS 1 OR 4 CHRS                           
         BL    P2V1U                                                            
         BH    P2V1B                                                            
         CLI   22(R6),C'*'         PIN=* MEANS CURRENT PIN                      
         BNE   *+12                                                             
         OI    PQSPIF,X'10'        SET PIN INPUT FLAG                           
         B     P2V1W                                                            
         CLI   22(R6),C'0'         PIN=0 MEANS NO PIN                           
         BNE   P2V1U                                                            
         XC    PQSPIN,PQSPIN       SET PIN TO ZERO                              
         OI    PQSPIF,X'11'        SET PIN INPUT AND SET TO ZERO                
         B     P2V1W                                                            
P2V1B    CLI   1(R6),4             PIN=XXXX                                     
         BNE   P2V1U                                                            
         CLC   22(4,R6),=C'****'   PIN=**** MEANS CURRENT PIN                   
         BNE   *+12                                                             
         OI    PQSPIF,X'10'        SET PIN INPUT FLAG                           
         B     P2V1W                                                            
         CLC   22(4,R6),=C'0000'   PIN=0000 MEANS NO PIN                        
         BNE   P2V1C                                                            
         XC    PQSPIN,PQSPIN       SET PIN TO ZERO                              
         OI    PQSPIF,X'11'        SET PIN INPUT AND SET TO ZERO                
         B     P2V1W                                                            
P2V1C    MVC   PQSPIN,22(R6)       SAVE PIN                                     
         OI    PQSPIF,X'10'        SET PIN INPUT FLAG                           
         TM    DDS,DDSTRM          IF DDS TERMINAL                              
         BNO   P2V1W                                                            
         CLC   DDSPSWD,22(R6)      TEST IF DDS VALUE TO MATCH ALL               
         BNE   P2V1W                                                            
         MVC   PQSPIN,FFS          SET PIN TO FF'S                              
         OI    PQSPIF,X'80'        SET DDS PID/PIN INPUT                        
         B     P2V1W                                                            
*                                                                               
P2V1U    B     ERR3                INVALID PIN                                  
P2V1W    BAS   RE,P2VDROP          DROP FIELD                                   
P2V1X    EQU   *                                                                
*                                                                               
P2V2     CLI   1(R6),0             FIRST FIELD CAN BE PUBLIC/GROUP ID           
         BNE   P2V2B                                                            
         CLI   0(R6),3             IF 1ST FIELD > 3 CHRS                        
         BNH   P2V2B                                                            
         CLC   12(8,R6),SR@PUBLC   TEST FOR PUBLIC ID                           
         BNE   P2V2A                                                            
         OI    DDS,DDSPUB                                                       
         OI    DDS2,DDSUID                                                      
         MVI   PQSDDSFN,C'P'       FLAG PUBLIC ID INPUT                         
         MVC   PQSUSER,PUBLICID    SAVE PUBLICID                                
         B     P2V2W                                                            
P2V2A    CLC   12(8,R6),DC@GRP     TEST FOR GROUP ID                            
         BNE   P2V2B                                                            
         OC    GRPUSER,GRPUSER                                                  
         BZ    P2V2B                                                            
         OI    DDS,DDSGRP                                                       
         OI    DDS2,DDSUID                                                      
         MVI   PQSDDSFN,C'G'       FLAG GROUP ID INPUT                          
         MVC   PQSUSER,GRPUSER     SAVE GROUP USER ID                           
         B     P2V2W                                                            
*                                                                               
P2V2B    CLI   1(R6),0             TEST FOR KEYWORD=VALUE TYPE FIELD            
         BE    P2V020                                                           
         CLI   12(R6),C'U'         CAN BE U= OR V=                              
         BE    *+12                                                             
         CLI   12(R6),C'V'                                                      
         BNE   P2V2X                                                            
         OI    DDS2,DDSUID                                                      
         TM    DDS,DDSTRM          MUST BE A DDS TERMINAL                       
         BNO   P2V2C                                                            
         MVC   PQSDDSFN,12(R6)                                                  
         TM    3(R6),X'80'         TEST U=NNNNNN                                
         BZ    P2V2F                                                            
         CLI   1(R6),6                                                          
         BNE   ERR6                MUST BE 6 CHRS                               
         MVC   PQSUSER,10(R6)      SAVE U=VALUE                                 
         B     P2V2W                                                            
P2V2C    TM    DDS,DDSPRV          A PRIVILIGED USER CAN DO U=                  
         NOP   ERR3                *NOP* BNO                                    
         CLI   12(R6),C'U'                                                      
         BNE   ERR3                                                             
         CLC   22(8,R6),SR8ALL     TEST U=ALL                                   
         BE    ERR6                                                             
         LA    RE,INVGRPID                                                      
         SR    R1,R1                                                            
P2V2D    ICM   R1,1,0(RE)          SEARCH TABLE OF INVALID USER IDS             
         BZ    P2V2E                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   22(0,R6),1(RE)                                                   
         BE    ERR6                                                             
         LA    RE,L'INVGRPID(RE)                                                
         B     P2V2D                                                            
P2V2E    MVC   PQSDDSFN,12(R6)                                                  
*                                                                               
P2V2F    CLC   22(8,R6),SR8ALL     TEST U=ALL                                   
         BE    P2V2W                                                            
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,22(R6)                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         BNE   ERR6                                                             
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
P2V2G    AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST NUMBER ELEMENT                          
         BNE   *+10                                                             
         MVC   PQSUSER,2(R7)       SAVE NUMBER                                  
         CLI   0(R7),X'07'         TEST 07 ELEMENT                              
         BL    P2V2H                                                            
         BH    P2V2J                                                            
         TM    2(R7),X'40'         TEST GENERIC ID                              
         BZ    P2V2J                                                            
         OI    PQSUSER,X'80'       SET GENERIC FLAG                             
P2V2H    ICM   RE,1,1(R7)                                                       
         BNZ   P2V2G                                                            
*                                                                               
P2V2J    TM    DDS,DDSTRM          DDS USER CAN INPUT ANY VALID ID              
         BO    P2V2W                                                            
         OC    LOGONID,LOGONID     USER MUST BE LOGGED ON                       
         BZ    ERR0                                                             
         CLC   PQSUSER,LOGONID     TEST IF U=LOGONID                            
         BE    P2V2W                                                            
         TM    DDS,DDSPRV          MUST BE PRIVILEDGED IF DIFFERENT             
         BNO   ERR3                                                             
         L     R7,ACTREC                                                        
         XC    CTIKEY,CTIKEY       READ LOGON ID RECORD                         
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),LOGONID                                              
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),X'00'                                                      
         BNE   ERR6                                                             
         MVI   DMCB,C'C'           SET COMPATIBLE ID MATCH                      
         STCM  R7,7,DMCB+1                                                      
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+8,C'A'         SET PASSING ID ALPHA IN P4                   
         MVC   DMCB+9(3),VDATAMGR+1                                             
         LA    RE,22(R6)                                                        
         ST    RE,DMCB+12                                                       
         GOTO1 AGETIDS,DMCB                                                     
         TM    DMCB+12,X'01'                                                    
         BZ    ERR6                U=XXXXX MUST BE COMPATIBLE ID                
*                                                                               
P2V2W    BAS   RE,P2VDROP          DROP FIRST FIELD                             
P2V2X    EQU   *                                                                
*                                                                               
P2V3     CLI   1(R6),0             TEST KEYWORD=VALUE                           
         BE    P2V3X                                                            
         CLC   12(8,R6),DC@AGY     CAN BE AGY=                                  
         BE    P2V3A                                                            
         CLC   12(2,R6),=C'A '     OR A=                                        
         BE    P2V3A                                                            
         B     P2V3X                                                            
P2V3A    OI    DDS2,DDSAGY         SET AGY=XX INPUT                             
         CLI   1(R6),2                                                          
         BNE   P2V3U                                                            
         MVC   PQSRAGY,22(R6)                                                   
         B     P2V3W                                                            
P2V3U    B     ERR3                INVALID AGENCY ALPHA                         
P2V3W    BAS   RE,P2VDROP          DROP FIELD                                   
P2V3X    EQU   *                                                                
*                                                                               
P2V4     CLI   1(R6),0             TEST KEYWORD=VALUE                           
         BE    P2V4X                                                            
         CLC   12(8,R6),DC@PID     CAN BE PID=                                  
         BE    P2V4A                                                            
         CLC   12(2,R6),=C'I '     OR I=                                        
         BE    P2V4A                                                            
         B     P2V4X                                                            
P2V4A    OI    DDS2,DDSPID         SET PID=XXX... INPUT                         
         CLI   1(R6),1             PID=XXX... TO DEFINE A PID                   
         BL    P2V4U                                                            
         BH    P2V4C                                                            
         CLI   22(R6),C'0'         PID=0 MEANS NO PID                           
         BNE   P2V4B                                                            
         XC    PQSPID,PQSPID       SET PID TO ZERO                              
         OI    PQSPIF,X'22'        SET PID INPUT AND SET TO ZERO                
         MVC   PQSRPID,PQSPID      PID IS PART OF THE SAVED REPORT ID           
         B     P2V4W                                                            
P2V4B    CLI   22(R6),C'*'         PID=* MEANS MY PID                           
         BNE   P2V4C                                                            
         MVC   PQSPID,TRMPID       SET PID FROM TERMINAL LOGON                  
         OI    PQSPIF,X'20'        SET PID INPUT FLAG                           
         MVC   PQSRPID,PQSPID      PID IS PART OF THE SAVED REPORT ID           
         B     P2V4W                                                            
P2V4C    CLI   1(R6),8             PID=DDPQ FOR ANY PID MATCH                   
         BH    P2V4U                                                            
         TM    DDS,DDSTRM          IF DDS TERMINAL                              
         BNO   P2V4D                                                            
         CLC   DDSPSWD,22(R6)      DDSPSWD MATCHES ALL                          
         BNE   P2V4D                                                            
         MVC   PQSPID,FFS          SET PID TO FF'S                              
         OI    PQSPIF,X'80'        SET DDS PID/PIN INPUT                        
         MVC   PQSRPID,PQSPID      PID IS PART OF THE SAVED REPORT ID           
         B     P2V4W                                                            
P2V4D    EQU   *                                                                
P2V4U    B     ERR3                INVALID PID                                  
P2V4W    BAS   RE,P2VDROP          DROP FIELD                                   
P2V4X    EQU   *                                                                
*                                                                               
P2V6     CLI   1(R6),0             TEST KEYWORD=VALUE                           
         BE    P2V6X                                                            
         CLC   12(8,R6),DC@REF     CAN BE REF=                                  
         BE    P2V6A                                                            
         CLC   12(2,R6),=C'R '     OR R=                                        
         BE    P2V6A                                                            
         B     P2V6X                                                            
P2V6A    OI    DDS2,DDSREF         SET REF=XXX INPUT                            
         CLI   1(R6),3                                                          
         BH    P2V6U                                                            
         MVC   PQSRREF,22(R6)                                                   
         B     P2V6W                                                            
P2V6U    B     ERR3                INVALID REPORT REFERENCE                     
P2V6W    BAS   RE,P2VDROP          DROP FIELD                                   
P2V6X    EQU   *                                                                
*                                                                               
P2V7     CLI   1(R6),0             TEST KEYWORD=VALUE                           
         BE    P2V7X                                                            
         CLC   12(8,R6),DC@TYPE    CAN BE TYPE=                                 
         BE    P2V7A                                                            
         CLC   12(2,R6),=C'T '     OR T=                                        
         BE    P2V7A                                                            
         B     P2V7X                                                            
P2V7A    OI    DDS2,DDSTYP         SET TYPE=X INPUT                             
         CLI   1(R6),1                                                          
         BNE   P2V7U                                                            
         CLI   22(R6),C'A'                                                      
         BL    P2V7U                                                            
         MVC   PQSRTYP,22(R6)                                                   
         B     P2V7W                                                            
P2V7U    B     ERR3                INVALID TYPE                                 
P2V7W    BAS   RE,P2VDROP          DROP FIELD                                   
P2V7X    EQU   *                                                                
*                                                                               
P2V020   CLI   1(R6),0             PART#1 IS (CLASS,)SUBID                      
         BNE   ERR3                                                             
         CLI   0(R6),0             MUST BE > 0                                  
         BE    ERR3                                                             
         CLI   0(R6),8             MUST BE < 8                                  
         BH    ERR3                                                             
         CLI   TRMLANG,X'03'       GERMANS AND DUTCH HAVE 4CHR ALLE             
         BE    P2V030                                                           
         CLI   TRMLANG,X'07'                                                    
         BE    P2V030                                                           
         CLI   0(R6),8             MUST BE < 8                                  
         BH    ERR3                                                             
         BE    P2V030                                                           
*                                                                               
         CLI   0(R6),1                                                          
         BNE   P2V021                                                           
         MVC   PQSCLASS,12(R6)     SINGLE CHR IS POSITIVE CLASS FILTER          
         CLI   PQSCLASS,C'*'                                                    
         BE    P2V02X                                                           
         CLI   PQSCLASS,C'$'                                                    
         BE    P2V02X                                                           
         CLI   PQSCLASS,C'A'                                                    
         BL    ERR5                                                             
         B     P2V02X                                                           
*                                                                               
P2V021   CLI   12(R6),C'-'         ALLOW NEGATIVE CLASS FILTER                  
         BNE   P2V030                                                           
         MVC   PQSCLASS,13(R6)                                                  
         CLI   PQSCLASS,C'*'                                                    
         BE    P2V022                                                           
         CLI   PQSCLASS,C'$'                                                    
         BE    P2V022                                                           
         CLI   PQSCLASS,C'A'                                                    
         BL    ERR5                                                             
P2V022   NI    PQSCLASS,X'BF'      NEGATIVE FILTERS ARE LOWER CASE              
*                                                                               
P2V02X   BAS   RE,P2VDROP          DROP CLASS FIELD                             
*                                                                               
P2V030   EQU   *                                                                
         CLI   0(R6),8             SUBID MUST BE 2/8 CHRS                       
         BH    ERR3                                                             
         CLI   0(R6),2                                                          
         BL    ERR3                                                             
         MVC   PQSSUBID,12(R6)     SAVE SUBID                                   
         CLI   0(R6),4                                                          
         BNH   P2V038                                                           
*                                                                               
         CLC   12(4,R6),DC@JOBS                                                 
         BNE   *+14                                                             
         MVC   PQSSUBID,=X'FFFFFF'                                              
         B     P2VX                                                             
*                                                                               
P2V038   LA    RE,PQSSUBID                                                      
         LA    RF,3                                                             
P2V039   CLI   0(RE),C'A'          MASSAGE SUBID                                
         BNL   *+16                                                             
         CLI   0(RE),C'*'                                                       
         BE    *+8                                                              
         MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,P2V039                                                        
*                                                                               
P2V03X   BAS   RE,P2VDROP          DROP FIELD                                   
*                                                                               
P2V040   OC    PQSSUBID,PQSSUBID   TEST FOR ALL                                 
         BZ    P2V040A                                                          
         CLC   PQSSUBID,SR8ALL                                                  
         BE    P2V040A                                                          
         TM    2(R6),X'80'         MUST BE NUMERIC SEQ                          
         BNO   P2V040A                                                          
         MVC   PQSSEQ,6(R6)                                                     
         OI    DDS,DDSNEW          ALWAYS READ SPECIFIC REPORTS                 
         B     P2V050                                                           
*                                                                               
P2V040A  SR    RF,RF               ELSE LOOK FOR TIME                           
         IC    RF,0(R6)                                                         
         MVC   SWORK1(22),12(R6)                                                
         GOTO1 ATIMBER,PARMS,(X'80',(RF)),(X'02',SFULL),SWORK1                  
         CLI   0(R1),0             WAS IT A VALID TIME PERIOD                   
         BE    P2V041                                                           
         OI    0(R1),X'40'         TRY FOR SINGLE TIME                          
         OI    DDS1,DDSTIM         AND FLAG THIS FACT                           
         GOTO1 (RF),(R1)                                                        
         CLI   0(R1),0                                                          
         BNE   ERR3                                                             
         MVC   SFULL+2(2),SFULL                                                 
         XC    SFULL(2),SFULL                                                   
P2V041   TM    DDS1,DDSTIM         SINGLE TIME IS ALWAYS VALID                  
         BO    P2V51A                                                           
         CLI   ACTN,X'11'                                                       
         BE    ERR11                                                            
         CLI   ACTN,X'14'                                                       
         BE    ERR11                                                            
P2V51A   SR    RF,RF                                                            
         ICM   RF,3,SFULL                                                       
         MH    RF,=H'180'          CONVERT TO SECS * 3                          
         SRL   RF,2                DIVIDE BY 4                                  
         STCM  RF,3,PQSTIMES                                                    
         ICM   RF,3,SFULL+2                                                     
         MH    RF,=H'180'          CONVERT TO SECS * 3                          
         SRL   RF,2                DIVIDE BY 4                                  
         A     RF,=F'44'           ADD 59 SECS TO T2                            
         STCM  RF,3,PQSTIMES+2                                                  
*                                                                               
P2V050   CLI   SBYTE,1                                                          
         BNE   ERR3                                                             
*                                                                               
P2V100   OC    PQSSUBID,PQSSUBID                                                
         BNZ   *+10                                                             
         MVC   PQSSUBID,SR@ALL                                                  
*                                                                               
P2V110   TM    DDS,DDSTRM                                                       
         BNO   P2V120              IF A DDS TERMINAL                            
         OC    USERID,USERID                                                    
         BNZ   P2V120              HAS NOT LOGGED ON                            
         CLI   PQSDDSFN,0                                                       
         BNE   P2V120              AND HAS FORGOTTEN TO INPUT U=?               
         MVI   PQSDDSFN,C'U'                                                    
         OI    DDS1,DDSUSR                                                      
*&&UK*&& MVC   PQSUSER,=X'0026'    SET U=DDS1 FOR UK                            
*&&US*&& MVC   PQSUSER,=X'002B'    SET U=TCH1 FOR US                            
*                                                                               
P2V120   CLI   PQSACT,X'11'        START/SCHEDULE ACTIONS                       
         BE    *+12                                                             
         CLI   PQSACT,X'14'        CANT HAVE NEW FILTERS                        
         BNE   P2VX                                                             
         TM    DDS2,DDSAGY+DDSPID+DDSREF+DDSTYP                                 
         BNZ   ERR3                                                             
*                                                                               
P2VX     MVC   PQSRIDIF,DDS2       SAVE INPUT FIELD FLAGS                       
         CLC   PQSREPID,SWORK                                                   
         BE    XIT1                                                             
         MVI   PQSCHREP,X'FF'      FLAG REPID CHANGED                           
         B     XIT1                                                             
*                                                                               
P2VDROP  SR    RF,RF               DROP THE FIELD FROM THE INPUT STRING         
         IC    RF,SBYTE                                                         
         AHI   RF,-1                                                            
         BZ    P2V100                                                           
         STC   RF,SBYTE            RESET NUMBER OF FIELDS                       
         LA    R6,32(R6)                                                        
         BR    RE                                                               
*                                                                               
       ++INCLUDE DMPRTQC                                                        
         EJECT                                                                  
*************************************************************                   
*        GET USERID FROM 2 CHR ID NUMBER                    *                   
*************************************************************                   
GETUSER  NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         L     R8,ASYSFAC                                                       
         CLC   GIUSER,GIPREV                                                    
         BE    GETUSRX                                                          
         MVC   GIPREV,GIUSER                                                    
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GIUSER                                               
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         BNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         BNE   *+14                                                             
         MVC   GIUSERID,2(R7)      GET ID NAME                                  
         B     GETUSR20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         BNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,GIUSER),(6,GIUSERID),FILL=0,DUB=SDUB,WRK=SWORK1              
*                                                                               
GETUSR20 LA    RF,0                                                             
         LA    RE,GIUSERID                                                      
GETUSR21 CLI   0(RE),X'40'                                                      
         BE    GETUSR30                                                         
         CLI   0(RE),0                                                          
         BE    GETUSR30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CHI   RF,8                                                             
         BL    GETUSR21                                                         
GETUSR30 STC   RF,GIULEN                                                        
GETUSRX  B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SECURITY INFO FOR REPORT                  *                   
*************************************************************                   
SECVAL   NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         MVI   GSECRES,0           SET OK RETURN CODE                           
*                                                                               
SECVAL1  TM    GSECF1,PQSINONO     EXIT IF SECURITY FLAGS NOT VALID             
         BO    SECVALX             YES                                          
*                                                                               
SECVAL2  TM    GSECF1,PQSIPIN      TEST IF PIN ATTACHED TO REPORT               
         BZ    SECVAL3                                                          
         CLC   PQSPIN,FFS          OK IF DDS PIN IS INPUT                       
         BE    SECVALX                                                          
         OC    PQSPIN,PQSPIN       TEST IF PIN IS DEFINED                       
         BNZ   *+12                                                             
         MVI   GSECRES,197         PIN REQUIRED FOR THIS REPORT                 
         B     SECVALX                                                          
         CLC   GSECPSWD,PQSPIN     TEST PIN WITH INPUT                          
         BE    SECVALX                                                          
         MVI   GSECRES,198         PIN INCORRECT FOR THIS REPORT                
         B     SECVALX                                                          
*                                                                               
SECVAL3  TM    GSECF1,PQSIPID      TEST IF PID ATTACHED TO REPORT               
         BZ    SECVAL4                                                          
         CLC   PQSPID,FFS          OK IF DDS PID IS INPUT                       
         BE    SECVALX                                                          
         CLC   GSECPSWD,TRMPID     TEST PID WITH LOGGED ON PID                  
         BE    SECVALX                                                          
         MVI   GSECRES,199         PID INCORRECT FOR THIS REPORT                
         B     SECVALX                                                          
*                                                                               
SECVAL4  B     SECVALX             REPORT CONTAINS SENSITIVE DATA               
*                                                                               
SECVALX  CLI   GSECRES,0           EXIT WITH CC NEQ IF SECURITY ERROR           
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SCRAMBLE PIN FOR SECURITY                          *                   
*************************************************************                   
PINMIX   NTR1  BASE=ABASE                                                       
         LM    R9,RB,ABASES                                                     
         MVC   PINOUT,PININ                                                     
         TM    PINACT,X'01'        TEST SCRAMBLE                                
         BZ    *+12                                                             
         LA    RF,PINMIXS                                                       
         B     PINMIX1                                                          
         TM    PINACT,X'02'        TEST UNSCRAMBLE                              
         BZ    *+12                                                             
         LA    RF,PINMIXS                                                       
         B     PINMIX1                                                          
         B     PINMIXX             EXIT WITH PIN UNCHANGED                      
*                                                                               
PINMIX1  AHI   RF,-192             LOWEST CHR IS C'A'                           
         TR    PINOUT(4),0(RF)                                                  
*                                                                               
PINMIX2  XC    PINOUT+0(1),PINOUT+2 SWAP 1ST AND 3RD CHRS                       
         XC    PINOUT+2(1),PINOUT+0                                             
         XC    PINOUT+0(1),PINOUT+2                                             
         XC    PINOUT+1(1),PINOUT+3 SWAP 2ND AND 4TH CHRS                       
         XC    PINOUT+3(1),PINOUT+1                                             
         XC    PINOUT+1(1),PINOUT+3                                             
*                                                                               
PINMIXX  XIT1                                                                   
*                                                                               
PINMIXS  DC    C'.U4OXP8ZRT......' C0-CF                                        
         DC    C'.31NYLCE2H......' DO-DF                                        
         DC    C'.5IA70DMG.......' E0-EF                                        
         DC    C'WKQJBS9VF6......' F0-FF                                        
*                                                                               
         LTORG                                                                  
*                                                                               
ACTMATCH DC    XL16'00212224141131494A00000000000000'                           
*----------------------------------------------------------------------         
*HELP INDEX 2 BYTES ACTION-INDEX 00=NO ACTION FF=ANY ACTION                     
*----------------------------------------------------------------------         
HLPIND1  DC    X'FF010000'                                                      
*                                                                               
HLPIND2  DC    X'00021103120413051406150716081709180A210B'                      
         DC    X'220C230D240E250F260F27102810291131120000'                      
*                                                                               
HLPIND3  DC    X'31132913FF140000'                                              
*                                                                               
HLPIND4  DC    X'00151115121513151415151516151715181519151A151B15'              
         DC    X'31162D21FF170000'                                              
*                                                                               
HLPIND5  DC    X'001812181618171818181119131A151B141C311D'                      
         DC    X'FF1E0000'                                                      
*                                                                               
HLPIND6  DC    X'121F161FFF200000'                                              
*                                                                               
*----------------------------------------------------------------------         
*FLAGS X'80'   DDS ONLY PANEL                                                   
*      X'40'   EXTRA TEXT BLOCK IN CIREC                                        
*----------------------------------------------------------------------         
HELPTAB  DS    0CL3                    INDEX/FLAGS/PANEL                        
         DC    X'01',X'80',X'1F'                                                
         DC    X'01',X'00',X'01'                                                
         DC    X'02',X'80',X'10'                                                
         DC    X'02',X'00',X'02'                                                
         DC    X'03',X'00',X'03'                                                
         DC    X'04',X'00',X'04'                                                
         DC    X'05',X'00',X'05'                                                
         DC    X'06',X'00',X'06'                                                
         DC    X'07',X'00',X'07'                                                
         DC    X'08',X'00',X'08'                                                
         DC    X'09',X'00',X'09'                                                
         DC    X'0A',X'00',X'0A'                                                
         DC    X'0B',X'00',X'0B'                                                
         DC    X'0C',X'00',X'0C'                                                
         DC    X'0D',X'00',X'0D'                                                
         DC    X'0E',X'00',X'0E'                                                
         DC    X'0F',X'00',X'0F'                                                
         DC    X'10',X'80',X'10'                                                
         DC    X'11',X'80',X'20'                                                
         DC    X'11',X'00',X'11'                                                
         DC    X'12',X'00',X'12'                                                
         DC    X'13',X'00',X'13'                                                
         DC    X'14',X'80',X'21'                                                
         DC    X'14',X'00',X'14'                                                
         DC    X'15',X'C0',X'22'                                                
         DC    X'15',X'40',X'15'                                                
         DC    X'16',X'00',X'16'                                                
         DC    X'17',X'00',X'17'                                                
         DC    X'18',X'00',X'18'                                                
         DC    X'19',X'00',X'19'                                                
         DC    X'1A',X'00',X'1A'                                                
         DC    X'1B',X'00',X'1B'                                                
         DC    X'1C',X'00',X'1C'                                                
         DC    X'1D',X'00',X'1D'                                                
         DC    X'1E',X'80',X'23'                                                
         DC    X'1E',X'00',X'1E'                                                
         DC    X'1F',X'00',X'24'                                                
         DC    X'20',X'80',X'25'                                                
         DC    X'20',X'00',X'26'                                                
         DC    X'21',X'00',X'27'                                                
         DC    X'00'                                                            
*                                                                               
HELPID   DC    XL10'0131FF00000000000000'                                       
*                                                                               
PUBIDTAB DC    AL2(32000)          PUBLIC PQ ENGLAND                            
         DC    AL2(32000)                                                       
         DC    AL2(32000)                                                       
         DC    AL2(32001)          PUBLIC PQ GERMANY                            
         DC    AL2(32000)                                                       
         DC    AL2(32000)                                                       
         DC    AL2(32000)                                                       
         DC    AL2(32000)                                                       
*                                                                               
GLIST    DC    CL8'GLIST'                                                       
GFILE    DC    CL8'GFILE'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
RANDOM   DC    CL8'RANDOM'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
*                                                                               
TEMPSTR  DC    CL8'TEMPSTR'                                                     
CTFILE   DC    CL8'CTFILE '                                                     
PRTQUE   DC    CL8'PRTQUE'                                                      
*                                                                               
DC@PUB   DC    CL8'PUBLIC'                                                      
DC@GRP   DC    CL8'GROUP'                                                       
DC@PFKEY DC    CL8'PFKEY'                                                       
DC3DQU   DC    CL8'DQU  '                                                       
DC5DQU   DC    CL8'=DQU '                                                       
DC3USE   DC    CL8'USE  '                                                       
DC@PC    DC    CL8'%'                                                           
DC@SCAN  DC    CL8'SCAN'                                                        
DC@FILE  DC    CL8'FILE'                                                        
DC@JOBS  DC    CL8'JOBS'                                                        
DC@NCD   DC    CL8'NCD '                                                        
DC@OCD   DC    CL8'OCD '                                                        
*                                                                               
DC@PIN   DC    CL8'PIN'            N=1234 MUST BE FIRST REPORT ID PARAM         
DC@AGY   DC    CL8'AGY'            A=                                           
DC@PID   DC    CL8'PID'            I=                                           
DC@REF   DC    CL8'REF'            R=                                           
DC@TYPE  DC    CL8'TYPE'           T=                                           
*                                                                               
DC@ARXA  DC    CL8'ARCHIVAB'       ARCHIVABLE                                   
DC@UARXA DC    CL8'UNARCHAB'                                                    
DC@ARXD  DC    CL8'ARCHIVED'       ARCHIVED                                     
DC@UARXD DC    CL8'UNARCHED'                                                    
DC@ARXE  DC    CL8'ARCHIVEL'       ARCHIVEL                                     
DC@UARXE DC    CL8'UNARCHEL'                                                    
DC@ARCA  DC    CL8'ARCA    '       ARCHIVABLE SHORT FORM                        
DC@UARCA DC    CL8'UNARCA  '                                                    
DC@ARCD  DC    CL8'ARCD    '       ARCHIVED SHORT FORM                          
DC@UARCD DC    CL8'UNARCD  '                                                    
DC@ARCE  DC    CL8'ARCE    '       ARCHIVEL SHORT FORM                          
DC@UARCE DC    CL8'UNARCE  '                                                    
*                                                                               
INVGRPID DS    0CL8                INVALID GROUP IDS - EXECUTED CLC             
         DC    AL1(3),CL7'DDS'                                                  
*&&UK*&& DC    AL1(3),CL7'TST'                                                  
*&&US*&& DC    AL1(3),CL7'SJR'                                                  
*&&US*&& DC    AL1(3),CL7'TCH'                                                  
INVGRPIX DC    AL1(0),CL7' '                                                    
*                                                                               
SPACES   DC    CL80' '                                                          
FFS      DC    16X'FF'                                                          
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
ERR0     LA    RE,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERR2     LA    RE,SREACT           INVALID ACTION                               
         B     ERRX                                                             
ERR3     LA    RE,SREIRI           INVALID REPORT ID                            
         B     ERRX                                                             
ERR4     LA    RE,SREMRI           MISSING REPORT ID                            
         B     ERRX                                                             
ERR5     LA    RE,180              INVALID REPORT CLASS                         
         B     ERRX                                                             
ERR6     LA    RE,SREUID           INVALID USER ID                              
         B     ERRX                                                             
ERR7     LA    RE,181              NOT LOGGED ON AND NO U=USER ID               
         B     ERRX                                                             
ERR8     LA    RE,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR9     LA    RE,SREPSW           INVALID PASSWORD                             
         B     ERRX                                                             
ERR10    LA    RE,58               INVALID PRINT QUEUE ID                       
         B     ERRX                                                             
ERR11    LA    RE,271              ONLY ONE TIME IS VALID                       
         B     ERRX                                                             
ERR12    LA    RE,298              FORMAT ERROR - CONTACT DDS                   
         B     ERRX                                                             
*************************************************************                   
*         OUTPUT ERROR MESSAGES   RE=MSG NUMBER             *                   
*************************************************************                   
ERRX     CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         L     RD,SAVERD           ERRORS CAN BE CALLED FROM ANYWHERE           
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         CURSOR POS                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETTXT,DMCB,(RE),0,(C'E',0)                                     
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        INFO MESSAGES                                      *                   
*************************************************************                   
INF0     LA    RE,1                NO MORE HELP                                 
         B     INFX                                                             
INF1     LA    RE,156              ENTER ACTION                                 
         B     INFX                                                             
*************************************************************                   
*        OUTPUT INFO MESSAGES   RE=MSG NUMBER               *                   
*************************************************************                   
INFX     L     RD,SAVERD                                                        
         CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         CURSOR POS                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETTXT,DMCB,(RE),0,(C'I',0)                                     
         B     INTSAVE                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*(LA RF) A(ACTION),ACTNUM,LEN,FLAG,A(SUB ACT TAB)                               
*FLAGS  X'80'=NEW STATUS,X'40'=DDS                                              
*                                                                               
         DS    0H                                                               
ACTNTBL  DS    0CL10                                                            
         DC    X'41F0',S(SR@START),X'11',X'08',X'00',AL3(SACTNT11)              
         DC    X'41F0',S(SR@STAT),X'12',X'08',X'00',AL3(1)                      
         DC    X'41F0',S(SR@STOP),X'13',X'08',X'00',AL3(0)                      
         DC    X'41F0',S(SR@SCHD),X'14',X'08',X'00',AL3(SACTNT14)               
         DC    X'41F0',S(SR@FLUSH),X'15',X'08',X'00',AL3(0)                     
         DC    X'41F0',S(SR@KILL),X'17',X'08',X'00',AL3(SACTNT17)               
         DC    X'41F0',S(SR@MODE),X'18',X'08',X'00',AL3(SACTNT18)               
         DC    X'41F0',S(SR@RLEAS),X'19',X'08',X'00',AL3(0)                     
         DC    X'41F0',S(SR@PRR),X'1A',X'08',X'00',AL3(0)                       
         DC    X'41F0',S(SR@SCAN),X'1B',X'08',X'00',AL3(0)                      
*                                                                               
         DC    X'41F0',S(SR@SLECT),X'20',X'08',X'00',AL3(0)                     
         DC    X'41F0',S(SR@DSP),X'21',X'08',X'00',AL3(1)                       
         DC    X'41F0',S(SR@HOLD),X'22',X'08',X'80',AL3(SACTNT22)               
         DC    X'41F0',S(SR@PURGE),X'23',X'08',X'80',AL3(0)                     
         DC    X'41F0',S(SR@ACTVT),X'24',X'08',X'80',AL3(SACTNT24)              
         DC    X'41F0',S(SR@KEEP),X'25',X'08',X'00',AL3(0)                      
         DC    X'41F0',S(SR@UKEEP),X'26',X'08',X'00',AL3(0)                     
         DC    X'41F0',S(SR@CLEAR),X'27',X'08',X'40',AL3(0)                     
         DC    X'41F0',S(SR@SIZE),X'28',X'08',X'40',AL3(SACTNT28)               
         DC    X'41F0',S(SR@RTAIN),X'29',X'08',X'80',AL3(1)                     
         DC    X'41F0',S(SR@PRTD),X'2A',X'08',X'80',AL3(0)                      
         DC    X'41F0',S(SR@UNERR),X'2B',X'08',X'80',AL3(0)                     
         DC    X'41F0',S(SR@VIS),X'2C',X'08',X'80',AL3(0)                       
         DC    X'41F0',S(SR@INVIS),X'2D',X'08',X'80',AL3(0)                     
         DC    X'41F0',S(SR@UNSEC),X'2E',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(SR@SEC),X'2F',X'08',X'C0',AL3(0)                       
*                                                                               
         DC    X'41F0',S(DC@ARXA),X'41',X'08',X'80',AL3(0)                      
         DC    X'41F0',S(DC@UARXA),X'42',X'08',X'80',AL3(0)                     
         DC    X'41F0',S(DC@ARXD),X'43',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(DC@UARXD),X'44',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(DC@ARXE),X'45',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(DC@UARXE),X'46',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(DC@ARCA),X'41',X'08',X'80',AL3(0)                      
         DC    X'41F0',S(DC@UARCA),X'42',X'08',X'80',AL3(0)                     
         DC    X'41F0',S(DC@ARCD),X'43',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(DC@UARCD),X'44',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(DC@ARCE),X'45',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(DC@UARCE),X'46',X'08',X'C0',AL3(0)                     
         DC    X'41F0',S(SR@BKUP),X'47',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(SR@UNBK),X'48',X'08',X'C0',AL3(0)                      
         DC    X'41F0',S(DC@NCD),X'49',X'08',X'C0',AL3(0)                       
         DC    X'41F0',S(DC@OCD),X'4A',X'08',X'C0',AL3(0)                       
*                                                                               
         DC    X'41F0',S(SR@LIST),X'31',X'08',X'00',AL3(SACTNT31)               
*                                                                               
         DC    X'41F0',S(SR4START),X'11',X'04',X'00',AL3(SACTNT11)              
         DC    X'41F0',S(SR4SCHD),X'14',X'04',X'00',AL3(SACTNT14)               
         DC    X'41F0',S(SR4FLUSH),X'15',X'04',X'00',AL3(0)                     
         DC    X'41F0',S(SR4DSP),X'21',X'04',X'00',AL3(0)                       
         DC    X'41F0',S(SR4ACTVT),X'24',X'04',X'80',AL3(SACTNT24)              
         DC    X'41F0',S(SR4RTAIN),X'29',X'04',X'80',AL3(1)                     
         DC    X'41F0',S(SR4PRTD),X'2A',X'08',X'80',AL3(0)                      
*                                                                               
         DC    F'00'                                                            
         EJECT                                                                  
SACTLEN  DS    0CL6                                                             
SACTNT11 DC    X'41F0',S(SR@MANUL),H'01'   START SUB ACTIONS                    
         DC    X'41F0',S(SR@AUTO),H'02'                                         
         DC    X'41F0',S(SR@SLECT),H'03'                                        
         DC    X'41F0',S(DC@PFKEY),H'04'                                        
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT14 DC    X'41F0',S(SR@TEMP),H'01'   SCHEDULE SUB ACTIONS                  
         DC    X'41F0',S(SR@PERM),H'02'                                         
         DC    X'41F0',S(SR@SLECT),H'03'                                        
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT17 DC    X'41F0',S(SR@CLEAR),H'01'   KILL SUB ACTIONS                     
         DC    X'41F0',S(SR@RLEAS),H'02'                                        
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT18 DC    X'41F0',S(SR@MANUL),H'01'   MODE SUB ACTIONS                     
         DC    X'41F0',S(SR@AUTO),H'02'                                         
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT22 DC    X'41F0',S(SR@SLECT),H'03'   HOLD,SEL                             
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT24 DC    X'41F0',S(SR@RTAIN),H'01'   ACTV SUB ACTIONS                     
         DC    X'41F0',S(SR@SLECT),H'03'                                        
         DC    H'00'                                                            
SACTNT28 DC    X'41F0',S(DC@PC),H'01'      ACTV SUB ACTIONS                     
         DC    X'41F0',S(DC@FILE),H'02'                                         
         DC    X'41F0',S(DC@SCAN),H'04'                                         
         DC    H'00'                                                            
         SPACE 2                                                                
SACTNT31 DC    X'41F0',S(SR@SLECT),H'03'   LIST,SEL                             
         DC    H'00'                                                            
         SPACE 2                                                                
*(LA RF),A(ACTION),ACTNUM,FLAGS,ACT-CODE,SPARE                                  
*                                                                               
*FLAGS X'80'   DDS ONLY                                                         
*FLAGS X'40'   NUMERIC SUFFIX                                                   
*FLAGS X'20'   NEGATIVE NUMERIC ALLOWED                                         
*FLAGS X'10'   DON'T EXIT AFTER ACTION                                          
*FLAGS X'02-01'MIN INPUT LEN 1-3                                                
*                                                                               
         DS    0H                  SELECT ACTIONS                               
SELTABL  DS    0CL8                                                             
         DC    X'41F0',S(DC3DQU),X'01',X'03',X'FF',X'00'                        
         DC    X'41F0',S(DC3USE),X'02',X'81',X'FF',X'00'                        
         DC    X'41F0',S(SR4DSP),X'21',X'01',X'01',X'00'                        
         DC    X'41F0',S(SR@SLECT),X'21',X'01',X'01',X'00'                      
         DC    X'41F0',S(SR4HOLD),X'22',X'11',X'02',X'00'                       
         DC    X'41F0',S(SR4ACTVT),X'24',X'11',X'03',X'00'                      
         DC    X'41F0',S(SR4SCHD),X'14',X'51',X'04',X'00'                       
         DC    X'41F0',S(SR4PRINT),X'11',X'51',X'05',X'00'                      
         DC    X'41F0',S(SR@START),X'11',X'51',X'05',X'00'                      
         DC    X'41F0',S(SR4LIST),X'31',X'61',X'06',X'00'                       
         DC    X'41F0',S(DS4NCD),X'49',X'51',X'07',X'00'                        
         DC    X'41F0',S(DS4OCD),X'4A',X'51',X'08',X'00'                        
SELTABLX DC    X'0000'                                                          
         EJECT                                                                  
*************************************************************                   
*   SCAN PQ    P1=WS P2=A(QSBLOCK) P3=A(QCOUNT)             *                   
*************************************************************                   
QSCAN    CSECT                                                                  
         NMOD1 0,**QSCN**,RA                                                    
         L     RC,0(R1)            GET WORKING STORAGE                          
         L     R4,4(R1)                                                         
         USING QSBLOCKD,R4                                                      
         L     R7,8(R1)                                                         
         USING QSCOUNTD,R7                                                      
         ZAP   0(3,R7),=P'0'       ZAP ALL THE COUNTS                           
         MVC   3(45,R7),0(R7)                                                   
         TBIN  SECS                                                             
         SR    R0,R0                                                            
         D     R0,=F'600'                                                       
         ST    R1,FULL1            FULL1 = TIME NOW BIN 10MINS                  
*                                                                               
SRCH0    L     R6,AREPTAB          R6=A(SAVE TABLE)                             
         USING QSEDATAD,R6                                                      
         MVC   QSEDATA,FFS         MARK LAST ENTRY                              
*                                                                               
         XC    NDX,NDX             FIND WHICH PRTQ FILE TO SEARCH               
         XC    APRTQLST,APRTQLST                                                
         CLI   QSQUEUE,X'FF'       TEST FOR SEARCH ALL QUEUES                   
         BE    SRCH0A                                                           
         CLI   QSQUEUE,0           SEE IF SPECIFIC QUEUE                        
         BE    SRCH00                                                           
         MVC   PRTQID+4(1),QSQUEUE SET SPECIFIC                                 
         MVI   PRTQID+5,C' '                                                    
         B     SRCH0B                                                           
*                                                                               
SRCH00   TM    QSSRCID,X'80'       IF GENERIC ID SEARCH ALL                     
         BO    SRCH0A                                                           
*                                                                               
         MVC   NXSRCID,QSSRCID     PRTQ ID FROM SPECIFIC USER ID                
         GOTO1 ADATAMGR,PQDMCB,(0,=C'GFILE'),PRTQUE,NDX,SAVE,CXREC              
         MVC   PRTQID,NXUSRINF                                                  
         B     SRCH0B                                                           
*                                                                               
SRCH0A   MVI   QSQUEUE,X'FF'       SET TO ALL QUEUES IF NOT ALREADY             
         LA    RE,PRTQLST+8        POINT TO FIRST PRTQ FILE                     
         ST    RE,APRTQLST                                                      
         MVC   PRTQID+4(1),1(RE)   SET PRTQ ID FROM LIST                        
         MVI   PRTQID+5,C' '                                                    
*                                                                               
SRCH0B   MVI   QIND,1              SET SEARCHING PART1 INDEX                    
         GOTO1 ADATAMGR,PQDMCB,(0,=C'BUFFER'),PRTQID,,,CXREC                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFDATA,CXREC      SAVE V(PQTAB)/V(PQFILE)                      
         MVC   CIDATA,CXREC+12     SAVE FILE DATA FOR THIS PRTQ FILE            
         SAM31                                                                  
         MVC   FIWRES,PRTQID       SET RESOURCE                                 
         BRAS  RE,FIRSET           SET ADDRESSES FOR THIS RESOURCE              
         JNE   *+2                 INVALID PRTQ                                 
         MVC   FIWNDA,FIWP1A       A(START OF PART1 INDEXES)                    
         LA    R5,FIWNDX                                                        
         USING PQRECD,R5           R5=A(PRTQUE INDEX ENTRY)                     
         B     SRCH1A                                                           
*                                                                               
SRCH1    SAM31                                                                  
         CLI   QIND,2                                                           
         BE    SRCH1B                                                           
         BRAS  RE,FIRNSN                                                        
         BNE   SRCHXX                                                           
SRCH1A   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI1NDX-SI1PARD(R1)                             
         B     SRCH1D                                                           
SRCH1B   BRAS  RE,FIRNSN2                                                       
         BNE   SRCHXX                                                           
SRCH1C   L     R1,FIWNDA                                                        
         MVC   FIWNDX(L'PQINDEX),SI2NDX-SI2PARD(R1)                             
SRCH1D   BRAS  RE,FIRNC            A(NODE) TO A(CI)                             
         SAM24                                                                  
*                                                                               
         OC    PQKEY,PQKEY         IGNORE PURGED                                
         BZ    SRCHX                                                            
         CLI   PQAGERT,X'FF'       TEST IF TEMPORARY                            
         BE    SRCH2D              COUNT AS NOT ACTIVE & NOT AVAIL              
*                                                                               
         TM    PQSTAT,PQSTAC+PQSTKE     ACTIVE KEEPS DON'T TEST RETAIN          
         BO    SRCH2A                                                           
         LA    RF,DATEC            SET RF TO NEW/OLD CMPRSD DATE                
         TM    PQTYP1,PQTYNCD                                                   
         BO    *+8                                                              
         LA    RF,DATECO                                                        
         CLC   PQAGERD,0(RF)       TEST RETAIN DATE WITH TODAY                  
         BL    SRCH2G                                                           
         BH    SRCH2A                                                           
         CLI   PQAGERT,X'FE'       TEST TIME NOT AVAILABLE                      
         BE    SRCH2A                                                           
         CLC   PQAGERT,FULL1+3     TEST RETAIN TIME VALUE                       
         BL    SRCH2G                                                           
SRCH2A   TM    PQSTAT,PQSTKE       UNEXPIRED KEEPS ALWAYS COUNT ACTIVE          
         BO    *+12                                                             
         TM    PQSTAT,PQSTAC       TEST FOR ACTIVE CI                           
         BZ    SRCH2D                                                           
         CLI   QIND,1              TEST PART 1 OR 2                             
         BNE   *+14                                                             
         AP    QSC3,=P'1'          BUMP PART1 ACTIVE                            
         B     SRCH2D                                                           
         AP    QSC4,=P'1'          BUMP PART2 ACTIVE                            
*                                                                               
SRCH2D   CLI   QIND,1              TEST PART 1 OR 2                             
         BNE   SRCH2E                                                           
         AP    QSC1,=P'1'          BUMP PART1 NOTAVAIL                          
         B     SRCH2G                                                           
SRCH2E   AP    QSC2,=P'1'          BUMP PART2 NOTAVAIL                          
         B     SRCHX                                                            
*                                                                               
SRCH2G   EQU   *                                                                
*                                                                               
SRCH3    OC    QSSRCID,QSSRCID     TEST USER ID DEFINED                         
         BZ    SRCH3G                                                           
         TM    QSSRCID,X'80'       TEST GENERIC USER ID                         
         BZ    SRCH3B                                                           
         CLI   AGENIDS,X'FF'       TEST VGENIDS PRESENT                         
         BE    SRCHX                                                            
         GOTO1 AGENIDS,DMCB,QSSRCID,ADATAMGR                                    
         BNE   SRCHX                                                            
         LM    RE,RF,0(R1)         RE=N'ENTRIES,RF=A(ENTRIES)                   
         CLC   PQSRCID,0(RF)                                                    
         BE    SRCH3G              MATCH FOUND                                  
         LA    RF,2(RF)                                                         
         BCT   RE,*-14                                                          
         B     SRCHX               NO MATCH ON GENERIC                          
*                                                                               
SRCH3B   CLC   PQSRCID,QSSRCID     ID WAS SPECIFIED                             
         BNE   SRCHX                                                            
         CLI   PQSTAT,PQSTPU       IGNORE PURGED                                
         BE    SRCHX                                                            
*                                                                               
SRCH3G   OC    QSCLASN,QSCLASN     FILTER ON CLASS                              
         BZ    SRCH3GA                                                          
         LA    R1,8                QSCLASN IS 8 CHRS                            
         LA    RF,QSCLASN-1(R1)                                                 
         CLC   PQCLASS,0(RF)       TEST FOR EXCLUDE                             
         BE    SRCHX                                                            
         BCT   R1,*-14                                                          
SRCH3GA  OC    QSCLAS,QSCLAS       ZERO MEANS INCLUDE ALL                       
         BZ    SRCH3H                                                           
         LA    R1,8                QSCLAS IS 8 CHRS                             
         LA    RF,QSCLAS-1(R1)                                                  
         CLC   PQCLASS,0(RF)       TEST FOR INCLUDE                             
         BE    SRCH3H                                                           
         BCT   R1,*-14                                                          
         B     SRCHX                                                            
*                                                                               
SRCH3H   OC    QSSUBID,QSSUBID     TEST ALL                                     
         BE    SRCH3I                                                           
         CLC   QSSUBID,SR@ALL      TEST ALL                                     
         BE    SRCH3I                                                           
         CLC   QSSUBID(1),PQSUBID  TEST 1ST CHR                                 
         BNE   SRCHX                                                            
         CLI   QSSUBID+1,C'*'      TEST A**                                     
         BE    SRCH3I                                                           
         CLC   QSSUBID+1(1),PQSUBID+1                                           
         BNE   SRCHX                                                            
         CLI   QSSUBID+2,C'*'      TEST AA*                                     
         BE    SRCH3I                                                           
         CLC   QSSUBID+2(1),PQSUBID+2                                           
         BNE   SRCHX                                                            
*                                                                               
SRCH3I   AP    QSR,=P'1'           BUMP TOTAL REPORTS                           
         TM    PQSTAT,PQSTAC                                                    
         BZ    *+10                                                             
         AP    QSA,=P'1'           BUMP ACTIVE                                  
         TM    PQSTAT,PQSTHO                                                    
         BZ    *+10                                                             
         AP    QSH,=P'1'           BUMP HOLD                                    
         CLI   PQAGERT,X'FE'                                                    
         BNE   *+10                                                             
         AP    QSG,=P'1'           BUMP PRINTING                                
         TM    PQSTAT,PQSTPR                                                    
         BZ    *+10                                                             
         AP    QSD,=P'1'           BUMP PRINTED                                 
         TM    PQSTAT,PQSTSE                                                    
         BZ    *+10                                                             
         AP    QSE,=P'1'           BUMP SENT                                    
         TM    PQSTAT,PQSTKE                                                    
         BZ    *+10                                                             
         AP    QSK,=P'1'           BUMP KEEP                                    
*                                                                               
PQSRCH4  SR    R1,R1               ANY TYPES TO EXCLUDE                         
         IC    R1,QSTYPEN                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQTYPE,0            IF ANY BITS MATCH EXCLUDE IT                 
         BNZ   SRCHX                                                            
         ICM   R1,1,QSTYPE                                                      
         BZ    PQSRCH4A            ZERO MEANS INCLUDE ALL                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQTYPE,0            IF ANY BITS MATCH INCLUDE IT                 
         BZ    SRCHX                                                            
*                                                                               
PQSRCH4A SR    R1,R1               ANY ATTRIBS TO EXCLUDE                       
         IC    R1,QSATTBN                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQATTB,0            IF ANY BITS MATCH EXCLUDE IT                 
         BNZ   SRCHX                                                            
         ICM   R1,1,QSATTB                                                      
         BZ    PQSRCH4B            ZERO MEANS INCLUDE ALL                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQATTB,0            IF ANY BITS MATCH INCLUDE IT                 
         BZ    SRCHX                                                            
*                                                                               
PQSRCH4B SR    R1,R1               ANY STATUS TO EXCLUDE                        
         IC    R1,QSSTATN                                                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQSTAT,0            IF ANY BITS MATCH EXCLUDE IT                 
         BNZ   SRCHX                                                            
         ICM   R1,1,QSSTAT                                                      
         BZ    PQSRCH4X            ZERO MEANS INCLUDE ALL                       
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    PQSTAT,0            IF ANY BITS MATCH INCLUDE IT                 
         BZ    SRCHX                                                            
PQSRCH4X EQU   *                                                                
*                                                                               
PQSRCH5  CLI   QSAGESH,0           TEST FOR NO UPPER LIMIT                      
         BE    *+14                                                             
         CLC   PQAGES,QSAGESH      TEST HIGH                                    
         BH    SRCHX                                                            
         CLC   PQAGES,QSAGESL      TEST LOW                                     
         BL    SRCHX                                                            
*                                                                               
         MVC   FULL(2),PQAGELD     TEST LIVE DATE TIME                          
         MVC   FULL+2(2),PQAGELT                                                
         OC    QSAGELH,QSAGELH                                                  
         BZ    *+14                                                             
         CLC   FULL,QSAGELH                                                     
         BH    SRCHX                                                            
         CLC   FULL,QSAGELL                                                     
         BL    SRCHX                                                            
*                                                                               
         OC    QSAGEDH,QSAGEDH     TEST DEAD DATE                               
         BZ    *+14                                                             
         CLC   PQAGEDD,QSAGEDH                                                  
         BH    SRCHX                                                            
         CLC   PQAGEDD,QSAGEDL                                                  
         BL    SRCHX                                                            
*                                                                               
         XC    FULL,FULL           TEST RETAIN DATE TIME                        
         MVC   FULL(2),PQAGERD                                                  
         MVC   FULL+2(1),PQAGERT                                                
         OC    QSAGERH,QSAGERH                                                  
         BZ    *+14                                                             
         CLC   FULL(3),QSAGERH                                                  
         BH    SRCHX                                                            
         CLC   FULL(3),QSAGERL                                                  
         BL    SRCHX                                                            
*                                                                               
SRCHT    CP    QST,QTMAX           ROOM IN TABLE                                
         BL    SRCHU               YES                                          
SRCHT1   AP    QSX,=P'1'           QSX=COUNT OF NO ROOM ENTRYS                  
         B     SRCHX                                                            
*                                                                               
SRCHU    XC    0(L'QSEDATA,R6),0(R6) SET UP TABLE ENTRY                         
         MVC   QSEUSER,PQSRCID                                                  
         MVC   QSESORT+0(2),PQAGELD                                             
         MVC   QSESORT+2(2),PQAGELT                                             
         MVC   QSESTAT,PQSTAT                                                   
         MVC   QSEATTB,PQATTB                                                   
         MVC   QSECIAD,FIWCIA                                                   
*                                                                               
         LA    R6,L'QSEDATA(R6)    NEXT ENTRY                                   
         AP    QST,=P'1'                                                        
*                                                                               
SRCHX    B     SRCH1               BUMP TO NEXT INDEX ENTRY                     
*                                                                               
SRCHXX   SAM24                                                                  
         CLI   QIND,2              ARE WE FINISHED SEARCHING PART2S?            
         BE    SRCHY               YES: THEN DONE                               
         CLI   ACTN,X'28'          DO WE NEED TO SEARCH PART2S?                 
         BNE   SRCHY               NO: THEN DONE                                
         MVI   QIND,2              SET TO SEARCH PART2 (SIZE ACTION)            
         MVC   FIWNDA,FIWP2A       START WITH FIRST PART2                       
         SAM31                                                                  
         B     SRCH1C                                                           
*                                                                               
SRCHY    ICM   RE,15,APRTQLST      TEST FOR MULTIPLE QUEUE SEARCH               
         BZ    SORT                                                             
         LA    RE,8(RE)            BUMP TO NEXT QUEUE                           
         ST    RE,APRTQLST                                                      
         CLI   0(RE),0             TEST FOR LAST QUEUE                          
         BE    SORT                                                             
         MVC   PRTQID+4(1),1(RE)                                                
         MVI   PRTQID+5,C' '                                                    
         MVC   FULL(3),QST         SAVE QST                                     
         LA    R7,L'QSCOUNT(R7)    POINT TO NEXT COUNT AREA                     
         ZAP   0(3,R7),=P'0'       ZAP ALL THE COUNTS                           
         MVC   3(45,R7),0(R7)                                                   
         MVC   QST(3),FULL         RESTORE QST                                  
         B     SRCH0B              START AGAIN                                  
*                                                                               
SRCHURR  DC    H'0'                                                             
*                                                                               
SORT     MVC   0(L'QSEDATA,R6),FFS SET END OF TABLE AND SORT ON KEY             
         XC    FULL,FULL                                                        
         ZAP   DUB,QST                                                          
         CVB   R6,DUB                                                           
         STH   R6,PQSTOTL                                                       
         LA    R6,1(R6)                                                         
         CHI   R6,2                                                             
         BNH   SORTX                                                            
         L     R0,AREPTAB                                                       
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+15,7           LENGTH                                       
         MVI   DMCB+19,0           DISPLACEMENT                                 
         CLI   BYTE1,C'X'                                                       
         BNE   SORT1               EXCLUDE USERID                               
         MVI   DMCB+15,5           ON XSORT                                     
         MVI   DMCB+19,2                                                        
         LR    R1,R6                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'16'           L'QSEDATA                                    
         AR    R1,R0                                                            
         XC    0(L'QSEDATA,R1),0(R1)  XC END OF TABLE                           
         ST    R1,FULL                                                          
SORT1    GOTO1 =V(XSORT),DMCB,(R0),(R6),12,,,RR=RELOBASE                        
         ICM   R1,15,FULL                                                       
         BZ    SORTX                                                            
         MVC   0(L'QSEDATA,R1),FFS RESET END OF TABLE                           
SORTX    XIT1                                                                   
*                                                                               
QTMAX    DC    PL3'800'            MAX NUMBER OF REPORTS                        
*                                                                               
       ++INCLUDE DDSHFIR           SHARED MEMORY ROUTINES                       
         LTORG                                                                  
*                                                                               
         DROP  RB,RA,R9                                                         
         EJECT                                                                  
***********************************************************************         
* SCAN INPUT FIELDS FOR ? HELP                                        *         
***********************************************************************         
         DS    0D                                                               
HELPSCAN NTR1  BASE=*                                                           
*                                                                               
         LA    R4,64(R3)           R4=A(FIRST FIELD)                            
         SR    R2,R2               CLEAR FIELD COUNT                            
SCAN1    SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    SCANX                                                            
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   SCAN3                                                            
         AR    R4,R0               NEXT FIELD                                   
         B     SCAN1                                                            
SCAN2    LTR   R0,R0                                                            
         BZ    SCAN2A                                                           
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
SCAN2A   SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     SCAN1                                                            
*                                                                               
SCAN3    EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         AHI   R0,-8                                                            
         SR    R5,R5               POS COUNT ZERO                               
SCAN4    CLI   PFKEY,1                                                          
         BNE   SCAN4A                                                           
         C     R4,CURSOR                                                        
         BE    SCAN5                                                            
SCAN4A   CLI   0(R1),C'?'                                                       
         BE    SCAN5               HELP REQUIRED                                
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,SCAN4                                                         
         B     SCAN2               NEXT FIELD                                   
*                                                                               
SCAN5    XC    HELP,HELP           CLEAR HELP                                   
         MVI   HLPPAG,1            DEFAULT PAGE 1                               
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDS,DDSTRM                                                       
         BNO   SCAN6                                                            
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   SCAN6                                                            
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
SCAN6    SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   SCAN2                                                            
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   SCAN7                                                            
         LA    R3,1(R3)                                                         
*                                                                               
SCAN7    BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     SCAN2                                                            
*                                                                               
SCANX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PRINTER NAME OPTIONS FROM CONNECTED USER ID RECORD                        
***********************************************************************         
         DS    0D                                                               
GETUPOP  NTR1  BASE=*                                                           
*                                                                               
         MVI   PNMUOPTS,0                                                       
         L     R5,ACIREC           READ USER ID RECORD                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         OC    LOGONID,LOGONID                                                  
         BZ    GPOPOK                                                           
         MVC   CTIKID+8(2),LOGONID                                              
         GOTO1 ADATAMGR,DMCB,=CL8'DMREAD',=CL8'CTFILE ',(R5),(R5)               
         CLI   8(R1),0                                                          
         BNE   GPOPOK                                                           
         LA    R5,CTIDATA                                                       
         SR    RF,RF                                                            
GPOP010  CLI   0(R5),0                                                          
         BE    GPOPOK                                                           
         CLI   0(R5),CTIDOELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     GPOP010                                                          
         MVC   PNMUOPTS,CTIDOFL2-CTIDOD(R5)                                     
         B     GPOPOK                                                           
*                                                                               
GPOPOK   SR    RC,RC                                                            
GPOPNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET LIST OF PRINTER NAMES AND NUMBERS FOR HELP PANEL                *         
***********************************************************************         
GETPRNT  NTR1  BASE=*                                                           
         SR    R4,R4               R4=NUMBER OF PRINTERS                        
         MVI   BYTE,0                                                           
         MVC   FULL(1),HLPPAG      1 BYTE ONLY                                  
         L     R7,ACIREC                                                        
         L     R5,ACTREC           READ USER ID RECORD                          
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),HALF    HALF=LOGONID OR REPUSER                      
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R5),(R5)                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,CTIDATA                                                       
         USING CTPRND,R5                                                        
*                                                                               
GETP01   CLI   0(R5),0                                                          
         BE    GETPXX                                                           
         CLI   0(R5),X'3A'                                                      
         BNE   GETP05                                                           
         OC    CTPRNFLG,CTPRNFLG    CHECK FOR LIST ENTRY                        
         BZ    GETP02                                                           
         MVC   0(25,R7),=CL80' '                                                
         CLI   BYTE,0                                                           
         BNE   *+8                                                              
         MVI   0(R7),C'|'                                                       
         EDIT  (B1,CTPRNNUM),(3,4(R7))                                          
         MVI   8(R7),C'-'                                                       
         MVC   10(8,R7),CTPRNLIN                                                
         LA    R7,25(R7)                                                        
         LA    R4,1(R4)                                                         
         SR    R1,R1                                                            
         CLI   BYTE,2                                                           
         BE    *+12                                                             
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         B     GETP05                                                           
*                                                                               
GETP02   LA    R6,CXREC             READ LIST RECORD                            
         USING CTWREC,R6                                                        
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'R'                                                     
         MVC   CTWKID,CTPRNLST                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',(R6),(R6)                    
         CLI   8(R1),0                                                          
         BNE   GETP05                                                           
         LA    R6,CTWDATA                                                       
GETP03   CLI   0(R6),0                                                          
         BE    GETP05                                                           
         CLI   0(R6),X'A4'                                                      
         BNE   GETP04                                                           
         MVC   0(25,R7),=CL80' '                                                
         CLI   BYTE,0                                                           
         BNE   *+8                                                              
         MVI   0(R7),C'|'                                                       
         EDIT  (B1,11(R6)),(3,4(R7))                                            
         MVI   8(R7),C'-'                                                       
         MVC   10(8,R7),3(R6)                                                   
         LA    R7,25(R7)                                                        
         LA    R4,1(R4)                                                         
         SR    R1,R1                                                            
         CLI   BYTE,2                                                           
         BE    *+12                                                             
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
*                                                                               
GETP04   ZIC   R0,1(R6)            NEXT LIST ELEMENT                            
         AR    R6,R0                                                            
         CHI   R4,30               MAX NUMBER FOR PANEL                         
         BNE   GETP03                                                           
         LA    R0,1                HORRIBLE FLAG                                
         B     GETP06                                                           
*                                                                               
GETP05   ZIC   R0,1(R5)            NEXT ID ELEMENT                              
         AR    R5,R0                                                            
         CHI   R4,30               MAX NUMBER FOR PANEL                         
         BNE   GETP01                                                           
         SR    R0,R0               HORRIBLE FLAG                                
*                                                                               
GETP06   MVC   0(75,R7),=CL80' '                                                
         LA    R7,75(R7)                                                        
         MVC   0(75,R7),=CL80' '                                                
         MVC   55(3,R7),=C'?2 '                                                 
         ICM   R1,1,HLPPAG                                                      
         BZ    GETP06A                                                          
         LA    R1,1(R1)                                                         
         STC   R1,56(R7)                                                        
         OI    56(R7),X'F0'                                                     
GETP06A  MVCDD 58(8,R7),SR#MORE                                                 
         MVC   66(9,R7),=C'----->>  '                                           
         LA    R7,75(R7)                                                        
         MVI   BYTE,0                                                           
         SR    R1,R1                                                            
         ICM   R1,1,FULL           TEST FOR PAGES                               
         BZ    GETPXX                                                           
*                                                                               
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    GETPXX                                                           
         STC   R1,FULL                                                          
         L     R7,ACIREC           PAGE N                                       
         MVI   BYTE,0                                                           
         SR    R4,R4                                                            
         LTR   R0,R0               TEST HORRIBLE FLAG                           
         BZ    GETP01                                                           
         B     GETP03                                                           
*                                                                               
GETPXX   SR    R1,R1                                                            
         ICM   R1,1,BYTE                                                        
         BZ    GETPXX1                                                          
         MVC   0(25,R7),=CL80' '                                                
         LA    R7,25(R7)                                                        
         BCT   R1,GETPXX1                                                       
         MVC   0(25,R7),=CL80' '                                                
         LA    R7,25(R7)                                                        
GETPXX1  MVI   0(R7),C' '                                                       
         MVC   1(74,R7),0(R7)                                                   
         MVI   75(R7),0                                                         
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPECIAL CODE TO TRAP/SPOT PURGES                                    *         
***********************************************************************         
         DS    0D                                                               
TRAPPER  NTR1  BASE=*                                                           
*&&US                                                                           
TRAP1    TM    SYSFLG,X'80'        IS THIS A TEST FACPAK                        
         BZ    TRAP1P                                                           
         CLC   USERID,=AL2(7088)   USERID STOKELY                               
         BE    TRAP1A                                                           
         CLC   USERID,=AL2(7367)   USERID QACOMM                                
         BE    TRAP1A                                                           
         CLC   USERID,=AL2(7371)   USERID PCAPPS01                              
         BE    TRAP1A                                                           
         B     TRAP1X                                                           
*                                                                               
TRAP1A   CLI   ACTN,X'23'          TRAP ATTEMPTS TO PURGE                       
         BNE   TRAP1X                                                           
         DC    H'0'                                                             
*                                                                               
TRAP1P   CLC   USERID,=AL2(9158)   USER GMMRE                                   
         NOP   TRAP1A              *NOP* BE                                     
TRAP1X   EQU   *                                                                
*&&                                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SRPQUDD           DICTIONARY REFS                              
         DS    0H                                                               
         EJECT                                                                  
       ++INCLUDE SRPQUWK                                                        
         EJECT                                                                  
SRPQUFFD DS    CL64                                                             
       ++INCLUDE SRPQUFFD                                                       
         EJECT                                                                  
*FADSECTS                                                                       
*DDCOMFACS                                                                      
*SRERREQUS                                                                      
*DDFLDHDR                                                                       
*CTGENFILE                                                                      
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDGLOBEQUS                                                     
         IHAASCB LIST=YES                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SRPQU00   09/22/20'                                      
         END                                                                    
