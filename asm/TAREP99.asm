*          DATA SET TAREP99    AT LEVEL 076 AS OF 04/17/18                      
*PHASE T70399C                                                                  
         TITLE 'T70399 - MOVE DATA FROM WORKER FILES TO MQ'                     
T70399   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70399                                                         
*                                                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING T70399+4096,RA                                                   
*                                                                               
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
*                                                                               
         L     R9,ATWA             R9=A(SCREEN)                                 
         USING T703FFD,R9          ESTABLISH SCREEN                             
*                                                                               
         L     R7,ASUBSYSD                                                      
         USING SUBSYSD,R7                                                       
*                                                                               
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY                                                      
         JE    VK                                                               
*                                                                               
         CLI   MODE,PRINTREP                                                    
         JNE   EXIT                                                             
                                                                                
*==========================================================                     
* MAIN PROCESSING LOOP -                                                        
* GET (NEXT) WORKER FILE INDEX AND READ FIRST RECORD                            
* CONVERT WORKER FILE RECORDS TO CLARUS FORMAT AND PUT TO MQ                    
* CLOSE MQ AND GET NEXT FILE                                                    
*==========================================================                     
                                                                                
*                                                                               
PROC10   L     RE,TWAMASTC                                                      
         USING MASTD,RE                                                         
         MVC   AMQRPT,MCVMQRPT                                                  
         MVC   TODAY,MCDATE                                                     
         MVC   QAGENCY,MCUSER                                                   
         MVC   VSSB,MCSSB                                                       
*                                                                               
         MVI   POSTOPT,0                                                        
         CLI   MCPOSTNG,C'N'       IF POSTING = N IN MASTER                     
         BNE   *+8                                                              
         MVI   POSTOPT,C'N'        THEN SET POST = N                            
         DROP  RE                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(4,TODAY),(1,TODAYP)                                 
         GOTO1 DATCON,DMCB,,(X'20',UNIQID)                                      
*                                                                               
         CLC   SYSTYPE,=C'OX'                                                   
         JNE   PROC12                                                           
                                                                                
* FOR OX, SEND WORKER FILES FROM YESTERDAY THAT ARE ON KEEP                     
                                                                                
         GOTO1 DATCON,DMCB,(4,TODAY),WORK   CHANGE MM/DD/YY TO YYMMDD           
         SR    R0,R0                                                            
         BCTR  R0,0                SET TO -1                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+8,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+8,(1,TODAYP)                                    
*                                                                               
PROC12   L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         DROP  RE                                                               
*                                                                               
         LARL  RE,WRKBUFF                                                       
         ST    RE,AWRKBUFF                                                      
         XC    INDEX,INDEX                                                      
         XC    SVWKUSID,SVWKUSID                                                
         XC    SVSYSPRG,SVSYSPRG                                                
*                                                                               
         BRAS  RE,GETINDX                                                       
         JNE   EXIT                                                             
* DO NOT REQUEST CR/LF = X'40'                                                  
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),MQDSN,(X'A0',0),0                       
         MVC   TCAUTREA,TCOPFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
*                                                                               
         BAS   RE,PUTXMLH          PUT XML HEADLINES                            
*                                                                               
PROC30   BRAS  RE,GETWREC          GET FIRST RECORD                             
         JNE   *+2                                                              
*                                                                               
         MVC   MDCPYID,SPACES      CLEAR OUT MEDIA CPY ID & NAME                
         MVC   MDCPYNM,SPACES                                                   
*                                                                               
         CLC   SYSTYPE,=C'OX'                                                   
         BE    PROC33                                                           
                                                                                
         XC    WORK(10),WORK       CALL USERVAL TO GET ALPHA USER ID            
         MVC   WORK+8(2),SVUSRID                                                
         MVC   AIO,AIO3                                                         
         GOTO1 USERVAL,DMCB,(X'A0',WORK)                                        
         MVC   MDCPYNM,TGNAME                                                   
         MVC   MDCPYID(L'TGUSERI2),TGUSERI2                                     
         J     PROC35                                                           
*                                                                               
PROC33   MVI   ELCODE,TANXELQ      FIND MEDIA COMPANY NAME                      
         LARL  R6,WREC                                                          
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
         USING TANXD,R6                                                         
         MVC   MDCPYID(L'TANXUID),TANXUID                                       
                                                                                
PROC35   CLC   SYSTYPE,=C'OX'                                                   
         BNE   PROC40                                                           
                                                                                
         MVI   ELCODE,TAFNELQ      FIND MEDIA COMPANY NAME                      
         LARL  R6,WREC                                                          
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
PROC38   BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
*                                                                               
         USING TAFND,R6                                                         
         CLI   TAFNTYPE,TAFNTMCN   TEST MEDIA COMPANY NAME                      
         JNE   PROC38                                                           
*                                                                               
         LLC   RE,TAFNLEN                                                       
         SHI   RE,TAFNLNQ                                                       
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MDCPYNM(0),TAFNNAME                                              
                                                                                
PROC40   BAS   RE,BLDHDR                                                        
         SR    R0,R0                                                            
         ICM   R0,3,CLARECLN                                                    
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),CLAREC,(R0),0                            
         MVC   TCAUTREA,TCPTFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
*                                                                               
         BRAS  RE,CNVWF            CONVERT TO CLARUS UPLOAD FORMAT              
         BRAS  RE,WKHOLD           MARK WORKER FILE AS HOLD                     
*                                                                               
PROC50   BRAS  RE,GETINDX                                                       
         JNE   PROCX                                                            
*                                                                               
         BRAS  RE,GETWREC                                                       
         JNE   *+2                                                              
*                                                                               
         BRAS  RE,CNVWF            CONVERT TO CLARUS UPLOAD FORMAT              
         BRAS  RE,WKHOLD           MARK WORKER FILE AS HOLD                     
*                                                                               
         J     PROC50                                                           
         EJECT                                                                  
*==============================================================                 
* INSERT A TRAILER RECORD                                                       
*==============================================================                 
                                                                                
X        USING NDTRD,CLAREC                                                     
*                                                                               
PROCX    MVC   CLAREC,SPACES                                                    
*                                                                               
         MVC   X.NDTRID,=C'999'                                                 
         OI    COUNT+3,X'0F'                                                    
         UNPK  X.NDTRCT,COUNT                                                   
         MVC   X.NDTRCRLF,CRLF                                                  
*                                                                               
         LA    R0,NDTRLN                                                        
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),CLAREC,(R0),0                            
         MVC   TCAUTREA,TCPTFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
*                                                                               
         BAS   RE,PUTXMLT          PUT XML TRAILER                              
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         MVC   TCAUTREA,TCCLFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
         J     EXIT                                                             
*                                                                               
YES      CR    RB,RB                                                            
         J     *+6                                                              
NO       LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
MQDSN    DC    CL16'ER_OUTPUT*******'                                           
         EJECT                                                                  
VK       DS    0H                                                               
         MVC   SYSTYPE,=C'OX'                                                   
         CLC   =C'STAN',XFRTYPE    SPOT TRAFFIC (NON-CLARUS)                    
         JE    VK10                                                             
         CLC   =C'NE1N',XFRTYPE    NET TRAFFIC (NON-CLARUS)                     
         JE    VK10                                                             
*                                                                               
         MVC   SYSTYPE,=C'DS'                                                   
         CLC   =C'STAL',XFRTYPE    SPOT TRAFFIC (STRANS)                        
         JE    VK10                                                             
         CLC   =C'NE1',XFRTYPE     NET TRAFFIC TTRANS                           
         JE    VK10                                                             
         DC    H'0'                INVALID TYPE!                                
*                                                                               
VK10     BRAS  RE,VOPT                                                          
         J     EXIT                                                             
                                                                                
         EJECT                                                                  
*=========================================================                      
* ROUTINE TO LOOK FOR  A WORKER FILE                                            
*=========================================================                      
                                                                                
GETINDX  NTR1                                                                   
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
*                                                                               
         OC    INDEX,INDEX         TEST FIRST TIME                              
         JZ    GETIND2             YES - NO REREAD                              
         MVC   SVINDEX,INDEX       SAVE THIS INDEX                              
                                                                                
* REREAD INDEX RECORD - IF FOUND, GET NEXT. IF NOT, PROCESS IT!                 
                                                                                
         MVC   WCOMMAND,=CL8'INDEX'                                             
         MVC   WRKFILEN,=CL8'WKFILE'                                            
         LARL  R0,WREC                                                          
         GOTO1 DATAMGR,DMCB,WCOMMAND,WRKFILEN,INDEX,(R0),AWRKBUFF               
         CLI   8(R1),0             TEST FOR ERROR                               
         JNE   NO                  IF ERROR, WE'RE DONE                         
*                                                                               
         CLC   SVINDEX,INDEX       TEST GOT BACK THE SAME ONE                   
         JNE   GETIND6             NO - PROCESS IT!                             
*                                                                               
GETIND2  MVC    WCOMMAND,=CL8'INDEX'  ELSE GET THE NEXT ONE                     
         MVC   WRKFILEN,=CL8'WKFILE'                                            
*                                                                               
GETIND4  LARL  R0,WREC                                                          
         GOTO1 DATAMGR,DMCB,WCOMMAND,WRKFILEN,INDEX,(R0),AWRKBUFF               
         CLI   8(R1),0             TEST FOR ERROR                               
         JNE   NO                                                               
*                                                                               
GETIND6  CLC   UKUSRID,SVUSRID    RIGHT USER-ID                                 
         JE    GETIND8             YES- PROCESS                                 
         OC    SVUSRID,SVUSRID     IF NO USERID, PROCESS OX REQUEST             
         JNZ   GETIND2             ELSE SKIP                                    
*                                                                               
GETIND8  CLC   UKSYSPRG,XFRTYPE    MATCH 3 CHAR NAME                            
         JNE   GETIND4                                                          
*                                                                               
         CLC   UKSUBPRG,XFRTYPE+3  MATCH SUBPRG                                 
         JNE   GETIND4                                                          
*                                                                               
         MVC   SVTRACE,TRACESW                                                  
         MVI   TRACESW,C'Y'                                                     
         LA    R0,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'POSSIBLE INDEX',INDEX,(R0)                       
         MVC   TRACESW,SVTRACE                                                  
*                                                                               
***NO-OP CLC   SYSTYPE,=C'OX'                                                   
***      JNE   GETIND10                                                         
***      TM    UKSTAT,X'08'        TEST ON KEEP                                 
***      JO    GETIND12            YES - PROCESS                                
***      J     GETIND4                                                          
***                                                                             
***IND10 TM    UKSTAT,X'08'        TEST WORKER FILE ON KEEP                     
***      JZ    GETIND12                                                         
***      CLI   DOKEEP,C'Y'         TEST TO INCLUDE KEEP FILES                   
***11/17 JNE   GETIND4                                                          
*                                                                               
GETIND12 TM    UKSTAT,X'40'        TEST WORKER FILE ON HOLD                     
         JNZ   GETIND4             SKIP IF ALREADY ON HOLD                      
*                                                                               
         CLI   UKCLASS,C'T'        MUST BE CLASS T                              
         JNE   GETIND4                                                          
*                                                                               
***NO-OP CLC   UKDAY,TODAYP+2      MUST BE TODAY'S DATE                         
***10/17 JNE   GETIND4                                                          
*                                                                               
         MVC   SVTRACE,TRACESW                                                  
         MVI   TRACESW,C'Y'                                                     
         LA    R0,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'GOOD INDEX',INDEX,(R0)                           
         MVC   TRACESW,SVTRACE                                                  
*                                                                               
         EDIT  UKUSRID,(5,SVWKUSID),ALIGN=RIGHT,FILL=0                          
         MVC   SVSYSPRG,UKSYSPRG                                                
         OC    SVSYSPRG,SPACES                                                  
         BAS   RE,RDCNTL           READ CONTROL FILE                            
         J     YES                                                              
         EJECT                                                                  
*=============================================================                  
* READ CONTROL FILE TO GET USER ID OF WORKER FILE                               
*=============================================================                  
RDCNTL   NTR1                                                                   
         USING UKRECD,R2                                                        
         LA    R2,INDEX                                                         
                                                                                
         MVC   FILENAME,=CL8'CTFILE' SET TO READ CONTROL FILE                   
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING CTIREC,R5                                                        
         MVI   CTIKTYP,C'I'        BUILD KEY FOR ID RECORD                      
         MVC   CTIKNUM,UKUSRID                                                  
         GOTO1 HIGH                                                             
*                                                                               
         L     R5,AIO                                                           
         LA    R4,CTIDATA                                                       
RCNTL10  CLI   0(R4),0                                                          
         JE    RCNTL30                                                          
         CLI   0(R4),CTDSCELQ                                                   
         JE    RCNTL20                                                          
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         J     RCNTL10                                                          
         DROP  R2,R5                                                            
*                                                                               
RCNTL20  MVC   SVWKUSID,2(R4)                                                   
RCNTL30  J     EXIT                                                             
*=============================================================                  
* MARK WORKER FILE ON HOLD AFTER PUTTING TO THE MQ                              
*=============================================================                  
WKHOLD   NTR1                                                                   
         CLI   POSTOPT,C'N'        IF POSTING = Y                               
         JE    EXIT                                                             
*                                                                               
         MVC   WCOMMAND,=CL8'HOLD'                                              
         LARL  R0,WREC                                                          
         GOTO1 DATAMGR,DMCB,WCOMMAND,WRKFILEN,INDEX,(R0),AWRKBUFF               
         CLI   8(R1),0             TEST FOR ERROR                               
         JNE   *+2                 IF ERROR, DIE?                               
         J     EXIT                                                             
         EJECT                                                                  
*=============================================================                  
* CONVERT WORKER RECORDS TO CLARUS FORMAT AND PUT TO MQ                         
* FIRST DETERMINE TYPE FROM TANPEL                                              
*=============================================================                  
                                                                                
CNVWF    NTR1                                                                   
*                                                                               
         J     CNVWF04             FIRST RECORD ALREADY READ                    
*                                                                               
CNVWF02  BRAS  RE,GETWREC          GET NEXT WORKER REC                          
         JNE   EXIT                                                             
*                                                                               
CNVWF04  MVI   ELCODE,TANPELQ                                                   
         LARL  R6,WREC                                                          
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
         ST    R6,ATANPEL                                                       
                                                                                
         BRAS  RE,CNVFLDS          EXTRACT COMMON FIELDS                        
*                                                                               
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
*                                                                               
         CLI   XFRTYPE,C'S'        TEST SPOT XFR                                
         JE    CNVWF10             YES                                          
*                                                                               
         CLI   TANPTYP,TANPNET     TEST NET CABLE                               
         JNE   CNVNET                                                           
         TM    TANPSTAT,TANPPAX    IF NETWORK IS PAX,                           
         JO    CNVNET              DON'T TREAT AS CABLE                         
         TM    TANPSTAT,TANPFLAT   IF FLAT RATE (LNA, LNN, LNF, LNC)            
         JO    CNVNET              DON'T TREAT AS CABLE                         
         J     CNVNCB              ELSE NET CABLE                               
*                                                                               
CNVWF10  CLI   TANPTYP,TANPCSYS    TEST LOCAL CABLE                             
         JE    CNVLCB                                                           
         DROP  R6                                                               
*                                                                               
         L     R6,ATANXEL                                                       
         USING TANXD,R6                                                         
*                                                                               
         CLI   TANXMED,TANXMTV     TEST SPOT TV                                 
         JE    CNVTVW                                                           
         CLI   TANXMED,TANXMRAD    TEST RADIO                                   
         JE    CNVRDW                                                           
         DC    H'0'                                                             
         DROP  R6                                                               
*                                                                               
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
*                                                                               
PUTMQ    NTR1                                                                   
         SR    R0,R0                                                            
         ICM   R0,3,CLARECLN                                                    
         LA    RF,CLAREC                                                        
         AR    RF,R0                                                            
         MVC   0(2,RF),CRLF        ADD CR/LF TO EOL                             
         AHI   R0,2                UPDATE LENGTH                                
*                                                                               
         CLI   DOPRINT,C'Y'                                                     
         JNE   PUTMQ2                                                           
* REMEMBER LEN IN REC DOES NOT INCLUDE RECLEN - IT'S NOT OUTPUT                 
         LARL  R4,CLARECLN         A(RECORD)                                    
         AHI   R0,4                ADD 4 FOR RECLN FIELD                        
         GOTO1 PRNTBL,DMCB,=C'RECOUT',(R4),C'DUMP',(R0),=C'2D'                  
         J     EXIT                                                             
*                                                                               
PUTMQ2   GOTO1 AMQRPT,DMCB,(0,=C'PUT'),CLAREC,(R0),0                            
         MVC   TCAUTREA,TCPTFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
         AP    COUNT,=P'1'                                                      
         J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* TV WILDSPOT                                                                   
*===========================================================                    
                                                                                
*                                                                               
CNVTVW   LARL  R4,CLAREC                                                        
         USING NDTWD,R4                                                         
*                                                                               
         MVC   NDTWID,=C'001'                                                   
*                                                                               
         LA    R0,NDTWLN                                                        
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
*                                                                               
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
         CLI   TANPTYP,C'S'                                                     
         JNE   *+2                                                              
         MVC   NDTWMKCD,TANPMKT                                                 
         OC    NDTWMKCD,SPACES                                                  
         MVC   NDTWMKNM,TANPMKTN                                                
         OC    NDTWMKNM,SPACES                                                  
*                                                                               
         BAS   RE,GETACTV          GET ACTIVITY DATES                           
         MVC   NDTWASTR(16),WORK                                                
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
         DROP  R6                                                               
                                                                                
*===========================================================                    
* RADIO WILDSPOT                                                                
*===========================================================                    
                                                                                
*                                                                               
CNVRDW   LARL  R4,CLAREC                                                        
         USING NDRWD,R4                                                         
         MVC   NDRWID,=C'002'                                                   
         LA    R0,NDRWLN                                                        
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
*                                                                               
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
         CLI   TANPTYP,C'S'                                                     
         JNE   *+2                                                              
         MVC   NDRWMKCD,TANPMKT                                                 
         OC    NDRWMKCD,SPACES                                                  
         MVC   NDRWMKNM,TANPMKTN                                                
         OC    NDRWMKNM,SPACES                                                  
*                                                                               
         BAS   RE,GETACTV          GET ACTIVITY DATES                           
         MVC   NDRWASTR(16),WORK                                                
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
         DROP  R6                                                               
                                                                                
*===========================================================                    
* LOCAL CABLE                                                                   
*===========================================================                    
                                                                                
*                                                                               
CNVLCB   LARL  R4,CLAREC                                                        
         USING NDLCD,R4                                                         
*                                                                               
         MVC   NDLCID,=C'003'                                                   
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
*                                                                               
         MVC   NDLCSYCD,TANPSYS                                                 
         OC    NDLCSYCD,SPACES                                                  
         MVC   NDLCSYNM,TANPMKTN      ASSUME MKTNAME = SYSTEM NAME              
         OC    NDLCSYNM,SPACES                                                  
*                                                                               
         BAS   RE,GETACTV          GET ACTIVITY DATES                           
         MVC   NDLCASTR(16),WORK                                                
*                                                                               
         LA    R0,NDLCLN                                                        
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
         DROP  R6                                                               
                                                                                
*===========================================================                    
* NATIONAL CABLE                                                                
*===========================================================                    
                                                                                
         USING NDCBD,R4                                                         
CNVNCB   LARL  R4,CLAREC                                                        
         USING NDCBD,R4                                                         
         MVC   NDCBID,=C'004'                                                   
*                                                                               
         BAS   RE,GETUSE                                                        
         MVC   NDCBUDTE,DUB                                                     
*                                                                               
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
*                                                                               
         MVC   NDCBSTCD,TANPSYS                                                 
         OC    NDCBSTCD,SPACES                                                  
         MVC   NDCBSTNM,TANPMKTN      ASSUME MKTNAME = SYSTEM NAME              
         OC    NDCBSTNM,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         LA    R0,NDCBLN                                                        
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
                                                                                
*===========================================================                    
* NETWORK                                                                       
*===========================================================                    
                                                                                
CNVNET   LA    R4,CLAREC                                                        
         USING NDNTD,R4                                                         
*                                                                               
         MVC   NDNTID,=C'005'                                                   
*                                                                               
         BAS   RE,GETUSE                                                        
         MVC   NDNTUDTE,DUB                                                     
*                                                                               
*                                                                               
         BRAS  RE,SETSTAT          SET STATUSES                                 
*                                                                               
         LA    R0,NDNTLN                                                        
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
         BAS   RE,PUTMQ                                                         
         J     CNVWF02                                                          
         DROP  R4                                                               
         EJECT                                                                  
*============================================================                   
* FIRST 9 FIELDS ARE COMMON TO ALL CLARUS RECTYPES                              
*============================================================                   
                                                                                
CNVFLDS  NTR1                                                                   
         LARL  R4,CLAREC                                                        
         USING NDTWD,R4                                                         
*                                                                               
         MVI   0(R4),C' '                                                       
         MVC   1(L'CLAREC-1,R4),0(R4)                                           
*                                                                               
         MVC   NDTWUPID,UNIQID                                                  
         UNPK  NDTWUPID+6(4),COUNT                                              
         OI    NDTWUPID+9,X'F0'                                                 
*                                                                               
         LARL  R6,WREC                                                          
         MVI   ELCODE,TANXELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
*                                                                               
         ST    R6,ATANXEL                                                       
         USING TANXEL,R6                                                        
*                                                                               
         MVC   NDTWAGY,TANXAGY      TALENT AGENCY                               
*                                                                               
         MVC   NDTWCMID(L'TANXNCID),TANXNCID    USE CMML CODE                   
         CLI   TANXLEN,TANXLNQ     TEST ADID PRESENT                            
         JNH   *+10                                                             
         MVC   NDTWCMID,TANXADID    COMMERCIAL ID                               
         OC    NDTWCMID,SPACES                                                  
*                                                                               
         LLC   R0,TANXSEC           COMMERCIAL LEN                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  NDTWCMLN,DUB                                                     
*                                                                               
         LARL  R6,WREC                                                          
         MVI   ELCODE,TAFNELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         JE    CNVFLD04                                                         
         DC    H'0'                                                             
*                                                                               
         USING TAFNEL,R6                                                        
*                                                                               
CNVFLD02 BRAS  RE,NEXTEL                                                        
         JNE   CNVFLD20                                                         
*                                                                               
CNVFLD04 CLI   TAFNTYPE,C'C'       TEST CLIENT NAME                             
         JNE   CNVFLD06                                                         
         LA    R1,NDTWCLNM                                                      
         BAS   RE,MVFLD                                                         
         J     CNVFLD02                                                         
*                                                                               
CNVFLD06 CLI   TAFNTYPE,C'P'       TEST PRODUCT NAME                            
         JNE   CNVFLD08                                                         
         LA    R1,NDTWPRNM                                                      
         BAS   RE,MVFLD                                                         
         J     CNVFLD02                                                         
*                                                                               
CNVFLD08 CLI   TAFNTYPE,C'T'       TEST CMML TITLE                              
         JNE   CNVFLD10                                                         
         LA    R1,NDTWCMTL                                                      
         BAS   RE,MVFLD                                                         
         J     CNVFLD02                                                         
*                                                                               
CNVFLD10 J     CNVFLD02                                                         
*                                                                               
CNVFLD20 LARL  R6,WREC                                                          
         MVI   ELCODE,TACTELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
*                                                                               
         USING TACTEL,R6                                                        
         MVC   NDTWCLCD,TACTCLI                                                 
*                                                                               
         LARL  R6,WREC                                                          
         MVI   ELCODE,TAPRELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
*                                                                               
         USING TAPREL,R6                                                        
         MVC   NDTWPRCD,TAPRPRD                                                 
         J     EXIT                                                             
*                                                                               
                                                                                
MVFLD    LLC   RF,1(R6)                                                         
         CHI   RF,39                                                            
         JNH   *+8                                                              
         LA    RF,39                                                            
         AHI   RF,-4               ADJUST FOR CD/LN/TYPE                        
         EX    RF,MVFLDMVC                                                      
         EX    RF,MVFLDOC                                                       
         BR    RE                                                               
MVFLDMVC MVC   0(0,R1),TAFNNAME-TAFNEL(R6)                                      
MVFLDOC  OC    0(0,R1),SPACES                                                   
*                                                                               
GETUSE   NTR1                                                                   
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(20,DUB)                                
         J     EXIT                                                             
         EJECT                                                                  
*===================================================================            
* FOR RADIO AND TV WILDSPOT, FIRST WORKER RECORD HAS ACTIVITY START             
* FOLLOWING RECORD HAS ACTIVITY END DATE                                        
*===================================================================            
                                                                                
GETACTV  NTR1                                                                   
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(20,WORK)                               
*                                                                               
         BAS   RE,GETWREC                                                       
*                                                                               
         LARL  R6,WREC                                                          
         MVI   ELCODE,TANPELQ                                                   
         BRAS  RE,FIRSTEL                                                       
         JNE   *+2                                                              
         USING TANPD,R6                                                         
         GOTO1 DATCON,DMCB,(1,TANPDATE),(20,WORK+8)                             
         J     EXIT                                                             
         EJECT                                                                  
*================================================================               
* ROUTINE TO READ A RECORD FROM THE WORKER FILE                                 
*================================================================               
                                                                                
GETWREC  NTR1                                                                   
*                                                                               
         MVC   WCOMMAND,=CL8'READ'                                              
         LARL  R0,WRECLN                                                        
         GOTO1 DATAMGR,DMCB,WCOMMAND,WRKFILEN,INDEX,(R0),AWRKBUFF               
         CLI   DMCB+8,0            IF RECORD FOUND                              
         JNE   NO                                                               
*                                                                               
         LARL  RE,WRECLN                                                        
         ST    RE,DMCB+4           SET A(RECORD)                                
*                                                                               
         LH    R0,0(RE)            LENGTH OF RECORD                             
         ST    R0,DMCB+8           SET L'RECORD                                 
         AR    RE,R0                                                            
         XC    0(2,RE),0(RE)        CLEAR EOF                                   
*                                                                               
         GOTO1 MYTRACE,DMCB,=C'READ WORKER RECORD'                              
         J     YES                                                              
         EJECT                                                                  
*=========================================================                      
* SEND HDR FOR THIS WORKER FILE                                                 
*=========================================================                      
         USING NDHDD,CLAREC                                                     
*                                                                               
BLDHDR   NTR1                                                                   
*                                                                               
         LARL  R4,CLAREC                                                        
         USING NDHDD,R4                                                         
*                                                                               
         MVC   0(L'CLAREC,R4),SPACES                                            
         MVC   NDHDID,=C'000'                                                   
         MVC   NDHDMDCO,MDCPYID    MEDIA COMPANY ID                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(20,NDHDDATE)                                  
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   NDHDTIME(8),DUB    MOVE HHMMSSHH                                 
*                                                                               
         MVC   NDHDFLNM(22),=C'CLARUS+XX+YYMMDD+HHMMSS'                         
         MVC   NDHDFLNM+7(2),QAGENCY                                            
         CLC   SYSTYPE,=C'OX'                                                   
         JNE   *+10                                                             
         MVC   NDHDFLNM+7(2),=C'OX'                                             
         MVC   NDHDFLNM+10(6),UNIQID                                            
         MVC   NDHDFLNM+17(6),NDHDTIME                                          
*                                                                               
         MVC   NDHDMDNM,MDCPYNM    MEDIA COMPANY NAME                           
*                                                                               
         MVC   NDHDCRLF,CRLF                                                    
*                                                                               
         LA    R0,NDHDRLN                                                       
         SLL   R0,16                                                            
         ST    R0,CLARECLN                                                      
         J     EXIT                                                             
         EJECT                                                                  
*============================================================                   
* SET INFO FOR TRACE ROUTINE                                                    
*============================================================                   
                                                                                
MYTRACE  NTR1                                                                   
         CLI   TRACESW,C'Y'        IF TRACE IS ON                               
         JNE   EXIT                                                             
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OPTIONS                                             *         
***********************************************************************         
                                                                                
VOPT     NTR1                                                                   
*                                                                               
         MVI   TRACESW,0           INIT                                         
         MVI   EMAILOPT,0                                                       
         XC    SVEMAIL,SVEMAIL                                                  
*                                                                               
         LA    R2,XFROPTH          POINT TO OPTIONS                             
         CLI   5(R2),0             test no options                              
         BE    VOPTDONE                                                         
*                                                                               
         USING SCAND,R3            ESTABLISH SCAN BLOCK                         
         LA    R3,BLOCK                                                         
                                                                                
* SCAN OPTIONS - UP TO 40 BYTES, SECOND FIELD                                   
                                                                                
         GOTO1 SCANNER,DMCB,(40,0(R2)),(R3),0                                   
*                                                                               
         LLC   R0,4(R1)            GET NUMBER OF OPTIONS                        
*                                                                               
VOPTLOOP DS    0H                                                               
*                                                                               
         CLC   =C'ID',SCDATA1      ID=12345                                     
         JNE   VOPT2                                                            
         PACK  DUB,SCDATA2(5)                                                   
         CVB   RF,DUB                                                           
         STH   RF,SVUSRID                                                       
         B     VOPTCONT                                                         
*                                                                               
VOPT2    CLC   =C'TRACE',SCDATA1   CHECK FOR TRACE OPTION                       
         BNE   *+12                                                             
         MVI   TRACESW,C'Y'                                                     
         B     VOPTCONT                                                         
*                                                                               
         CLC   =C'RERUN',SCDATA1    CHECK FOR RERUN OPTION                      
         BNE   *+12                                                             
         MVI   RERUN,C'Y'                                                       
         B     VOPTCONT                                                         
*                                                                               
***NO-OP CLC   =C'KEEP',SCDATA1    CHECK PROCESS FILES ON KEEP                  
***      BNE   *+12                                                             
***      MVI   DOKEEP,C'Y'                                                      
***11/17 B     VOPTCONT                                                         
*                                                                               
         CLC   =C'PRINT',SCDATA1                                                
         JNE   *+12                                                             
         MVI   DOPRINT,C'Y'                                                     
         B     VOPTCONT                                                         
*                                                                               
         CLC   =C'TEST',SCDATA1    CHECK GENERATE A TEST FILE ONLY              
         BNE   VOPT10                                                           
         LARL  RE,MQDSN                                                         
         MVC   0(16,RE),=CL16'TESTMSG*********'                                 
         B     VOPTCONT                                                         
*                                                                               
VOPT10   DS    0H                                                               
INVERR   MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
VOPTCONT LA    R3,62(R3)                                                        
         BCT   R0,VOPTLOOP                                                      
*                                                                               
VOPTDONE CLC   SYSTYPE,=C'OX'                                                   
         JE    VOPTX                                                            
         OC    SVUSRID,SVUSRID                                                  
         JZ    *+2                                                              
*                                                                               
VOPTX    DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* PUT XML HEADER DATA TO MQ FILE                                                
*==============================================================                 
                                                                                
*                                                                               
*<MESSAGE>                                                                      
*<KEY>                                                                          
*<MOSYSTEM>OX/DS</MOSYSTEM>                                                     
*<HOSTENVIRONMENT>TST OR FQA OR CSC</HOSTENVIRONMENT>                           
*<TENANT>ALPHA CODE</TENANT>                                                    
*<CLIENT>DON'T THINK THIS IS APPLICABLE TO YOU</CLIENT>                         
*<HOSTUNIQUEREFERENCE>NOT APPLICABLE TO YOU</HOSTUNIQUEREFERENCE>               
*<TRAFFICENVIRONMENT>NOT APPLICABLE TO YOU</TRAFFICENVIRONMENT>                 
*<UUID>DON'T THINK THIS IS APPLICABLE TO YOU</UUID>                             
*</KEY>                                                                         
*                                                                               
*<REQUESTPAYLOAD><CLARUS>  <!CDATA                                            
*000SJ        2016112905454138CLARUS+SJ+161129+054541                           
*0011611290002LUCEN LCEN444444  030STRANS-TV                                    
*0011611290003LUCEN LCEN444444  030STRANS-TV                                    
*9990000000005                                                                  
*ÙÙ> </CLARUS></REQUESTPAYLOAD>                                                 
*</MESSAGE>                                                                     
*====================================================================           
                                                                                
PUTXMLH  NTR1                                                                   
*                                                                               
         MVC   HXML3TXT(2),=C'OX'                                               
         CLI   XFRTYPE+3,C'N'      TEST NON-CLARUS                              
         JE    *+10                                                             
         MVC   HXML3TXT(2),=C'DS'                                               
*                                                                               
         MVC   HXML5TXT(2),QAGENCY                                              
*                                                                               
         L     RE,VSSB                                                          
         USING SSOOFF,RE                                                        
*                                                                               
         MVC   HXML4TXT(3),=C'ADV'                                              
         CLI   SSODSPAC,C'A'                                                    
         JE    PUTXMLH2                                                         
         MVC   HXML4TXT(3),=C'TST'                                              
         CLI   SSODSPAC,C'T'                                                    
         JE    PUTXMLH2                                                         
         MVC   HXML4TXT(3),=C'CSC'                                              
         CLI   SSODSPAC,C'C'                                                    
         JE    PUTXMLH2                                                         
         MVC   HXML4TXT(3),=C'FQA'                                              
         CLI   SSODSPAC,C'Q'                                                    
         JE    PUTXMLH2                                                         
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
PUTXMLH2 LA    R4,HXMLTAB                                                       
         XR    R5,R5                                                            
*                                                                               
PUTXMLH4 LLC   R0,0(R4)                                                         
         ICM   R5,7,1(R4)                                                       
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),(R5),(R0),0                              
         MVC   TCAUTREA,TCPTFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
*                                                                               
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         JNE   PUTXMLH4                                                         
         J     EXIT                                                             
*                                                                               
HXMLTAB  DC    AL1(HXML1X-HXML1),AL3(HXML1)                                     
         DC    AL1(HXML2X-HXML2),AL3(HXML2)                                     
         DC    AL1(HXML3X-HXML3),AL3(HXML3)                                     
         DC    AL1(HXML4X-HXML4),AL3(HXML4)                                     
         DC    AL1(HXML5X-HXML5),AL3(HXML5)                                     
         DC    AL1(HXML6X-HXML6),AL3(HXML6)                                     
         DC    AL1(HXML7X-HXML7),AL3(HXML7)                                     
HXMLTABX DC    X'FF'                                                            
*                                                                               
HXML1    DC    C'<message>'                                                     
HXML1X   EQU   *                                                                
*                                                                               
HXML2    DC    C'<key>'                                                         
HXML2X   EQU   *                                                                
*                                                                               
HXML3    DC    C'<moSystem>'                                                    
HXML3TXT DC    C'XX</moSystem>'       OX OR DS REPLACES XX                      
HXML3X   EQU   *                                                                
*                                                                               
HXML4    DC    C'<hostEnvironment>'                                             
HXML4TXT DC    C'XXX</hostEnvironment>'   FQA/CSC/ADV                           
HXML4X   EQU   *                                                                
*                                                                               
HXML5    DC    C'<tenant>'                                                      
HXML5TXT DC    C'AA</tenant>'      AGENCY ALPHA CODE                            
HXML5X   EQU   *                                                                
*                                                                               
HXML6    DC    C'</key>'                                                        
HXML6X   EQU   *                                                                
*                                                                               
HXML7    DC    C'<requestPayload><clarus> <!'                                   
         DC    AL1(CHARLSQB)       LEFT SQ BRACKET                              
         DC    C'CDATA'                                                         
         DC    AL1(CHARLSQB)       LEFT SQ BRACKET                              
HXML7X   EQU   *                                                                
                                                                                
*==========================================================                     
* PUT XML TRAILER DATA                                                          
*==========================================================                     
                                                                                
PUTXMLT  NTR1                                                                   
         LA    R4,TXMLTAB                                                       
         XR    R5,R5                                                            
*                                                                               
PUTXMLT4 LLC   R0,0(R4)                                                         
         ICM   R5,7,1(R4)                                                       
*                                                                               
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),(R5),(R0),0                              
         MVC   TCAUTREA,TCPTFAIL                                                
         MVC   TCAUTUSR,SVWKUSID                                                
         MVI   TCAUTSPA,C' '                                                    
         MVC   TCAUTSPR,SVSYSPRG                                                
         CLI   DMCB+8,0                                                         
         JNE   ERREND                                                           
*                                                                               
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         JNE   PUTXMLT4                                                         
*                                                                               
         J     EXIT                                                             
*                                                                               
TXMLTAB  DC    AL1(TXML1X-TXML1),AL3(TXML1)                                     
         DC    X'FF'                                                            
*                                                                               
TXML1    DC    AL1(CHARRSQB)                                                    
         DC    AL1(CHARRSQB)                                                    
         DC    C'>'                                                             
         DC    C'</clarus></requestPayload>'                                    
         DC    C'</message>'                                                    
TXML1X   EQU   *                                                                
*                                                                               
CHARLSQB EQU   X'BA'               LEFT  SQUARE BRACKET                         
CHARRSQB EQU   X'BB'               RIGHT SQUARE BRACKET                         
CRLF     DC    X'0D25'                                                          
*                                                                               
***********************************************************************         
                                                                                
***********************************************************************         
*        ROUTINE TO SEND OUT FAILURE EMAIL AND DUMP                             
***********************************************************************         
                                                                                
ERREND   GOTO1 DATAMGR,DMCB,=C'OPMSG',(=AL1(TCAUTLEN),TCANOTE)                  
         DC    H'00'                                                            
         EJECT                                                                  
TCANOTE  DC    C'AUTONOTE*ERCLARUSALERT@MEDIAOCEAN.COM:'                        
**TCANOTE  DC  C'AUTONOTE*MBLUME@MEDIAOCEAN.COM:'                               
TCAUTREA DS    CL15                                                             
TCAUTUSR DS    CL6                                                              
TCAUTSPA DS    CL1                                                              
TCAUTSPR DS    CL4                                                              
TCAUTLEN EQU   *-TCANOTE                                                        
TCOPFAIL DC    CL(L'TCAUTREA)'MQ OPEN FAILED'                                   
TCPTFAIL DC    CL(L'TCAUTREA)'MQ PUT ERROR'                                     
TCCLFAIL DC    CL(L'TCAUTREA)'MQ CLOSE FAILED'                                  
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* SET STATUSES IN TANX AND TANP ELEMENTS                                        
*=========================================================                      
         USING NDNTD,R4                                                         
SETSTAT  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ATANPEL                                                       
         USING TANPD,R6                                                         
*                                                                               
         MVC   NDNTPGNM,TANPPNME                                                
         OC    NDNTPGNM,SPACES                                                  
         MVC   NDNTNTCD,TANPNWK                                                 
         OC    NDNTNTCD,SPACES                                                  
* SET ALL TRUE/FALSE FIELDS FALSE                                               
         MVI   NDNTUNMK,C'F'                                                    
         MVI   NDNTNAIR,C'F'                                                    
         MVI   NDNTLCHG,C'F'                                                    
         MVI   NDNTPCHG,C'F'                                                    
         MVI   NDNTDCHG,C'F'                                                    
         MVI   NDNTCCHG,C'F'                                                    
         MVI   NDNTFLAT,C'F'                                                    
         MVI   NDNTMRUN,C'F'                                                    
*                                                                               
         CLI   TANPAIR,C' '        TEST ANY REASON NOT AIRED                    
         JNH   SSTAT10             no                                           
**NO-OP  MVI   NDNTNAIR,C'T'       SET UNAIRED=TRUE                             
* NOW SET REASON                                                                
         MVC   NDNTNWHY,TANPAIR    MOVE D/E/M                                   
         CLI   NDNTNWHY,C'E'       BUT IF IT'S E                                
         JNE   *+8                                                              
         MVI   NDNTNWHY,C'P'       MAKE IT PRE-EMPTED                           
*                                                                               
SSTAT10  TM    TANPSTAT,TANPFLAT   TEST FLAT RATE                               
         JZ    *+8                                                              
         MVI   NDNTFLAT,C'T'       SET FLAT RATE=TRUE                           
*                                                                               
         TM    TANPSTAT,TANPMR     TEST MULTIPLE RUN                            
         JZ    *+8                                                              
         MVI   NDNTMRUN,C'T'       SET MULITPLE RUN =TRUE                       
         DROP  R6                                                               
*                                                                               
         L     R6,ATANXEL                                                       
         USING TANXD,R6                                                         
*                                                                               
         TM    TANXCCDE,TANXCUNM   TEST UNMARKED                                
         JZ    *+8                                                              
         MVI   NDNTUNMK,C'T'                                                    
*                                                                               
         TM    TANXCCDE,TANXCAIR   TEST NOT AIRED                               
         JZ    *+8                                                              
         MVI   NDNTNAIR,C'T'                                                    
*                                                                               
         TM    TANXCCDE,TANXCLEN   TEST LENGTH CHANGED                          
         JZ    *+8                                                              
         MVI   NDNTLCHG,C'T'                                                    
*                                                                               
         TM    TANXCCDE,TANXCPRD   TEST PRODUCT CHANGED                         
         JZ    *+8                                                              
         MVI   NDNTPCHG,C'T'                                                    
*                                                                               
         TM    TANXCCDE,TANXCDTE   TEST DATE CHANGED                            
         JZ    *+8                                                              
         MVI   NDNTDCHG,C'T'                                                    
*                                                                               
         TM    TANXCCDE,TANXCCID   TEST COMMERCIAL CHANGED                      
         JZ    *+8                                                              
         MVI   NDNTCCHG,C'T'                                                    
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
VSSB     DS    V                                                                
INDEX    DS    CL32                WORKER FILE INDEX                            
SVINDEX  DS    CL32                                                             
WCOMMAND DS    CL8                 WORKER FILE COMMAND                          
WRKFILEN DS    CL8                 WORKER FILE FILE                             
TODAY    DS    CL8                                                              
SVUSRID  DS    H                                                                
TODAYP   DS    XL3                 TODAY PACKED                                 
UNIQID   DS    CL10                                                             
TRACESW  DS    CL1                                                              
SVTRACE  DS    CL1                                                              
EMAILOPT DS    CL1                                                              
SVEMAIL  DS    CL20                                                             
RERUN    DS    CL1                                                              
DOKEEP   DS    CL1                                                              
DOPRINT  DS    CL1                                                              
POSTOPT  DS    CL1                                                              
QAGENCY  DS    CL2                                                              
SYSTYPE  DS    CL2                 OX/DS                                        
WRKNM    DS    CL4                 WRK FILE SYS/PROGRAM/SUB-PROGRAM             
SVWKUSID DS    CL6                                                              
SVSYSPRG DS    CL4                                                              
*                                                                               
MDCPYID  DS    CL10                                                             
*                                                                               
MDCPYNMH DS    XL8                                                              
MDCPYNM  DS    CL36                                                             
*                                                                               
COUNTS   DS    0CL24                                                            
COUNT    DC    PL4'0',CL20'FILE COUNT'  START COUNTER AT 1!                     
COUNTX   EQU   *                                                                
*                                                                               
AWRKBUFF DS    A                   A(WORKER FILE BUFFER)                        
PRNTBL   DS    A                   A(PRNTBL)                                    
AMQRPT   DS    A                                                                
ATANXEL  DS    A                                                                
ATANPEL  DS    A                                                                
*                                                                               
         DS    0D                                                               
         DC    C'*CLAREC*'         CLARUS REC                                   
CLARECLN DS    H                   ** NOTE RECLN NOT WRITTEN TO MQ **           
         DS    H                                                                
CLAREC   DS    CL256                                                            
*                                                                               
         DS    0D                  WORKER FILE RECORD                           
         DC    C'**WREC**'                                                      
WRECLN   DS    H                                                                
         DS    H                                                                
WREC     DS    768C                                                             
WRECX    EQU   *                                                                
         DS    0D                                                               
         DC    C'*WRKBUFF'                                                      
WRKBUFF  DS    14336C                                                           
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPB9D                                                       
* DDGENTWA MUST FOLLOW LAST SCREEN                                              
       ++INCLUDE DDGENTWA                                                       
*                                                                               
*              DSECT TO COVER 32-BYTE SCAN BLOCK                                
         SPACE 1                                                                
SCAND    DSECT                                                                  
SCLEN1   DS    XL1  L'FIELD (OR L'FIRST HALF OF DIVIDED FIELD).                 
SCLEN2   DS    XL1  L'SECOND HALF OF DIVIDED FIELD OR ZERO.                     
SCVAL1   DS    XL1  VALIDITY BITS (X'80'=NUMERIC X'40'=ALPHA X'20'=HEX)         
SCVAL2   DS    XL1  VALIDITY BITS FOR SECOND HALF OF DIVIDED FIELDS.            
SCDISP1  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF LHS                   
         ORG   *-1                                                              
SCBIN1   DS    F    BINARY VALUE OF VALID NUMERIC FIELDS.                       
SCDISP2  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF RHS                   
         ORG   *-1                                                              
SCBIN2   DS    F    BINARY VALUE OF SECOND HALF OF DIVIDED FIELDS.              
SCDATA1  DS    CL10 LEFT JUSTIFIED FIELD DATA PADDED WITH SPACES.               
SCDATA2  DS    CL10 DATA FOR SECOND HALF OF DIVIDED FIELDS.                     
SCANNEXT EQU   *  (NOTE - UNDIVIDED FIELDS MAY BE UP TO 20 CHARACTERS.)         
         SPACE 3                                                                
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD HEADER RECORD                          *         
*                                                                     *         
***********************************************************************         
                                                                                
NDHDD    DSECT                     FILE HEADER RECORD                           
NDHDR    DS    0C                  FILE HEADER RECORD                           
NDHDID   DS    CL3                 RECORD ID                                    
NDHDIDQ  EQU   C'000'                FILE HEADER                                
NDHDMDCO DS    CL10                MEDIA COMPANY ID                             
NDHDDATE DS    CL8                 DATE - YYYYMMDD                              
NDHDTIME DS    CL8                 TIME - HH:MM:SS                              
NDHDFLNM DS    CL36                UNIQUE FILE NAME                             
NDHDMDNM DS    CL36                MEDIA COMPANY NAME                           
NDHDCRLF DS    XL2                 0D/25                                        
NDHDRLN  EQU   *-NDHDR             LENGTH OF RECORD                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD TV -WILDSPOT                           *         
*                                                                     *         
***********************************************************************         
                                                                                
NDTWD    DSECT                     TV WILD SPOT DSECT                           
NDTWREC  DS    0C                  TV WILD SPOT RECORD                          
NDTWID   DS    CL3                 RECORD ID                                    
NDTWIDQ  EQU   C'001'              TV WILDSPOT                                  
NDTWUPID DS    CL10                UNIQUE ID                                    
NDTWAGY  DS    CL6                 TALENT AGENCY CODE                           
NDTWCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDTWCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDTWCMTL DS    CL36                COMMERCIAL TITLE                             
NDTWCLCD DS    CL6                 CLIENT  CODE                                 
NDTWCLNM DS    CL36                CLIENT  NAME                                 
NDTWPRCD DS    CL6                 PRODUCT CODE                                 
NDTWPRNM DS    CL36                PRODUCT NAME                                 
NDTWASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDTWAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDTWMKCD DS    CL4                 MARKET CODE - TALENT                         
NDTWMKNM DS    CL15                MARKET NAME                                  
NDTWLN   EQU   *-NDTWREC           LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - RADIO WILDSPOT                       *         
*                                                                     *         
***********************************************************************         
                                                                                
NDRWD    DSECT                     RADIO WILDSPOT DSECT                         
NDRWREC  DS    0C                  RADIO WILD SPOT RECORD                       
NDRWID   DS    CL3                 RECORD ID                                    
NDRWIDQ  EQU   C'002'              RADIO WILDSPOT                               
NDRWUPID DS    CL10                UNIQUE ID                                    
NDRWAGY  DS    CL6                 TALENT AGENCY CODE                           
NDRWCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDRWCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDRWCMTL DS    CL36                COMMERCIAL TITLE                             
NDRWCLCD DS    CL6                 CLIENT  CODE                                 
NDRWCLNM DS    CL36                CLIENT  NAME                                 
NDRWPRCD DS    CL6                 PRODUCT CODE                                 
NDRWPRNM DS    CL36                PRODUCT NAME                                 
NDRWASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDRWAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDRWMKCD DS    CL4                 MARKET CODE - ALPHA                          
NDRWMKNM DS    CL15                MARKET NAME                                  
NDRWLN   EQU   *-NDRWREC           LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - LOCAL CABLE                          *         
*                                                                     *         
***********************************************************************         
                                                                                
NDLCD    DSECT                     LOCAL CABLE DSECT                            
NDLCREC  DS    0C                  LOCAL CABLE RECORD                           
NDLCID   DS    CL3                 RECORD ID                                    
NDLCIDQ  EQU   C'003'              LOCAL CABLE                                  
NDLCUPID DS    CL10                UNIQUE ID                                    
NDLCAGY  DS    CL6                 TALENT AGENCY CODE                           
NDLCCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDLCCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDLCCMTL DS    CL36                COMMERCIAL TITLE                             
NDLCCLCD DS    CL6                 CLIENT  CODE                                 
NDLCCLNM DS    CL36                CLIENT  NAME                                 
NDLCPRCD DS    CL6                 PRODUCT CODE                                 
NDLCPRNM DS    CL36                PRODUCT NAME                                 
NDLCASTR DS    CL8                 ACTIVE START DATE - YYYYMMDD                 
NDLCAEND DS    CL8                 ACTIVE END   DATE - YYYYMMDD                 
NDLCSYCD DS    CL6                 LOCAL CABLE SYSTEM - CODE                    
NDLCSYNM DS    CL15                LOCAL CABLE SYSTEM - NAME                    
NDLCLN   EQU   *-NDLCREC           LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - CABLE                                *         
*                                                                     *         
***********************************************************************         
                                                                                
NDCBD    DSECT                     CABLE DSECT                                  
NDCBREC  DS    0C                  CABLE RECORD                                 
NDCBID   DS    CL3                 RECORD ID                                    
NDCBIDQ  EQU   C'004'               CABLE                                       
NDCBUPID DS    CL10                UNIQUE ID                                    
NDCBAGY  DS    CL6                 TALENT AGENCY CODE                           
NDCBCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDCBCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDCBCMTL DS    CL36                COMMERCIAL TITLE                             
NDCBCLCD DS    CL6                 CLIENT  CODE                                 
NDCBCLNM DS    CL36                CLIENT  NAME                                 
NDCBPRCD DS    CL6                 PRODUCT CODE                                 
NDCBPRNM DS    CL36                PRODUCT NAME                                 
NDCBUDTE DS    CL8                 USE DATE - YYYYMMDD                          
NDCBSTCD DS    CL4                 CABLE STATION CODE - NTI                     
NDCBSTNM DS    CL15                CABLE STATION NAME                           
NDCBLN   EQU   *-NDCBREC           LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - NETWORK                              *         
*                                                                     *         
***********************************************************************         
                                                                                
NDNTD    DSECT                     NETWORK DSECT                                
NDNTREC  DS    0C                  NETWORK RECORD                               
NDNTID   DS    CL3                 RECORD ID                                    
NDNTIDQ  EQU   C'005'               CABLE                                       
NDNTUPID DS    CL10                UNIQUE ID                                    
NDNTAGY  DS    CL6                 TALENT AGENCY CODE                           
NDNTCMID DS    CL12                COMMERCIAL ID/AD-ID                          
NDNTCMLN DS    CL3                 COMMERCIAL LENGTH IN SECONDS                 
NDNTCMTL DS    CL36                COMMERCIAL TITLE                             
NDNTCLCD DS    CL6                 CLIENT  CODE                                 
NDNTCLNM DS    CL36                CLIENT  NAME                                 
NDNTPRCD DS    CL6                 PRODUCT CODE                                 
NDNTPRNM DS    CL36                PRODUCT NAME                                 
NDNTUDTE DS    CL8                 USE DATE - YYYYMMDD                          
NDNTPGNM DS    CL15                PROGRAM NAME                                 
NDNTNTCD DS    CL1                 NETWORK CODE                                 
*                                    A - ABC                                    
*                                    C - CBS                                    
*                                    F - FOX                                    
*                                    I - ITN                                    
*                                    M - MNT OR MY                              
*                                    N - NBC                                    
*                                    X - PAX                                    
*                                    U - UPN                                    
*                                    W - WB OR CW                               
*                                    S - SYNDICATED                             
NDNTUNMK DS    CL1                 UNMARKED - T/F                               
NDNTNAIR DS    CL1                 UNAIRED  - T/F                               
NDNTLCHG DS    CL1                 LENGTH     CHANGED  - T/F                    
NDNTPCHG DS    CL1                 PRODUCT    CHANGED  - T/F                    
NDNTDCHG DS    CL1                 DATE       CHANGED  - T/F                    
NDNTCCHG DS    CL1                 COMMERCIAL CHANGED  - T/F                    
NDNTFLAT DS    CL1                 FLAT RATE/LATE NIGHT- T/F                    
NDNTNWHY DS    CL1                 REASON NOT AIRED                             
*                                    D - DELETED                                
*                                    P - PREEMPTED                              
*                                    M - MISSED                                 
NDNTMRUN DS    CL1                 MULTIPLE RUN        - T/F                    
NDNTLN   EQU   *-NDNTREC           LENGTH OF RECORD                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        NON-DDS CLARUS UPLOAD - TRAILER                              *         
*                                                                     *         
***********************************************************************         
                                                                                
NDTRD    DSECT                     TRAILER DSECT                                
NDTRREC  DS    0C                  TRAILER RECORD                               
NDTRID   DS    CL3                 RECORD ID                                    
NDTRIDQ  EQU   C'999'               CABLE                                       
NDTRCT   DS    CL10                RECORD COUNT                                 
NDTRCRLF DS    XL2                                                              
NDTRLN   EQU   *-NDTRREC           LENGTH OF RECORD                             
         EJECT                                                                  
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DDSOFDATD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE TAREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076TAREP99   04/17/18'                                      
         END                                                                    
