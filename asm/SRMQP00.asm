*          DATA SET SRMQP00    AT LEVEL 012 AS OF 07/30/12                      
*PHASE T16B00A                                                                  
         PRINT NOGEN                                                            
         TITLE '$MQP - MQ SERIES FACPAK TEST PROCESSING PROGRAM'                
MQP      CSECT                                                                  
         NMOD1 WORKL,**$MQP**,RA,CLEAR=YES,RR=RE                                
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RE,RELO                                                          
         ST    R1,APARMS                                                        
         ST    RD,SAVERD                                                        
         USING SRPARMD,R1                                                       
         L     R9,SRQAUTL                                                       
         USING UTLD,R9             R9=A(UTL)                                    
         ST    R9,AUTL                                                          
         BRAS  RE,INIT                                                          
         BRAS  RE,VALCMD                                                        
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* COMMAND VALIDATION AND PROCESSING                                   *         
* MQ MESSAGE HAS A CL8 HEADER THAT TELLS US WHAT TO DO (MSG IN TBUFF) *         
***********************************************************************         
VALCMD   NTR1  ,                                                                
         ICM   R2,15,TBUFF         R2 = A(HEADER)                               
*                                                                               
         CLC   SVMQION,=F'2'       EXTENDED TRACING?                            
         BNE   VCMD02              NO                                           
         MVC   OPMSG,SPACES                                                     
         MVC   OPMSG(L'WCQ),WCQ                                                 
         LR    RF,R2                                                            
         AHI   RF,-2               GET INPUT LENGTH                             
         SR    RE,RE                                                            
         ICM   RE,3,0(RF)                                                       
         BZ    VCMD01                                                           
         CHI   RE,L'OPMSG-L'WCQ    WILL IT ALL FIT?                             
         BL    *+8                 YES                                          
         LHI   RE,L'OPMSG-L'WCQ                                                 
         BCTR  RE,0                                                             
         MVC   OPMSG+L'WCQ(0),0(R2)                                             
         EX    RE,*-6                                                           
VCMD01   BRAS  RE,WTO                                                           
*                                                                               
VCMD02   LA    RF,CMDTAB           NOW SEE WHAT WE WANT TO DO                   
*                                                                               
VCMD04   CLI   0(RF),0                                                          
         BE    NO                                                               
         CLC   0(8,RF),0(R2)       MATCH                                        
         BE    *+12                                                             
         AHI   RF,L'CMDTAB                                                      
         B     VCMD04                                                           
*                                                                               
         ICM   RF,15,8(RF)         GET ON WITH IT...                            
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     YES                                                              
*                                                                               
CMDTAB   DS    0XL12                                                            
         DC    CL8'OPENSYS ',AL4(PROCOSYS)                                      
         DC    CL8'FROMMQCT',AL4(PROCMQCT)                                      
         DC    CL8'MQDONE  ',AL4(PROCDONE)                                      
         DC    X'0000'                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS MQDONE COMMAND FOR PUTTING PASS/FAIL MESSAGE                *         
* R2=A(TBUFF)  P1  CL8'MQDONE'                                                  
*              P2  CL4'PASS' OR CL4'FAIL'                                       
*              P3  CL12-MY_CORRL_ID=TSYS(1)+TSIN(4)+TTIMETU(4)                  
*                                   +TMQTYP(1)+TMQSEQN(2)                       
***********************************************************************         
         USING MQUOWD,R2                                                        
PROCDONE NTR1  ,                                                                
         LA    R3,MQUOWMSL         UNIT OF WORK MESSAGE LENGTH                  
         LA    RE,MQUOWCI2                                                      
         ST    RE,DMCB+4*7                                                      
         OI    DMCB+4*7,X'80'      SET THE 8TH PARM FOR MQIO                    
         GOTO1 MQIO,DMCB,=CL8'PUT',MQUOWID,(R3),0,0,0,C'DONE'                   
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS OPEN SYSTEM MESSAGE - FOR STUFF NOT IN A TASK TO CALL       *         
***********************************************************************         
PROCOSYS NTR1  ,                                                                
         GOTO1 MQIO,DMCB,=CL8'OPENSYS',0,0,0                                    
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS MESSAGE FROM DDMQIO CONTROL QUEUE                           *         
***********************************************************************         
PROCMQCT NTR1  ,                                                                
         L     R2,TBUFF            R2 = A(DATA)                                 
         AHI   R2,-2                                                            
         LH    R3,0(R2)            R3 = L'DATA                                  
         AHI   R2,2                                                             
*                                                                               
         AHI   R2,8                CORRECT FOR FROMMQCT HEADER                  
         AHI   R3,-8                                                            
*                                                                               
         CLC   =C'FACPAK=',0(R2)   ROUTING INFORMATION?                         
         BNE   *+12                                                             
         AHI   R2,16               CORRECT FOR FACPAK=XXXX***** HDR             
         AHI   R3,-16                                                           
*                                                                               
         CLC   =C'FP=',0(R2)       ROUTING INFORMATION?                         
         BE    *+14                                                             
         CLC   =C'FQ=',0(R2)         SPECIAL FOR FQA?                           
         BNE   PMQCT02             NO                                           
         MVC   USERID,3(R2)                                                     
         MVC   SYSC3,13(R2)                                                     
         AHI   R2,16               CORRECT FOR FP=UUUUUUUUUUSSS HDR             
         AHI   R3,-16                                                           
*                                                                               
PMQCT02  CLC   =C'ERRORTO=',0(R2)                                               
         BNE   PMQCT03             NO                                           
         AHI   R2,24               CORRECT FOR ERRORTO=????????????????         
         AHI   R3,-24                                                           
*                                                                               
PMQCT03  BRAS  RE,CANIGO           CAN THIS MESSAGE BE PROCESSED?               
         BNE   YES                 NO - IT'S ON WORK QUEUE NOW                  
*                                                                               
         LA    R4,MQMESST          EACH MESSAGE HAS A CL8 HEADER                
         USING MQMESSTD,R4                                                      
PMQCT04  CLI   MQMID,X'FF'         RIGHT AFTER THE "FROMMQCT"                   
         BE    PMQCT08                                                          
         CLC   MQMID,0(R2)         DO WE HAVE A MATCH?                          
         BE    *+12                YES                                          
         AHI   R4,MQMESSTL                                                      
         B     PMQCT04                                                          
*                                                                               
         TM    MQMFLAG,MQMFISVC    LAUNCHING A SERVICE REQUEST?                 
         BZ    PMQCT06             NO                                           
         B     SVCGO                                                            
*                                                                               
PMQCT06  ICM   RF,15,MQMRTN        PROCESSING ROUTINE                           
         A     RF,RELO                                                          
         BR    RF                                                               
*                                                                               
PMQCT08  CLC   SVMQION,=F'2'       EXTENDED TRACING?                            
         BNE   YES                 NO                                           
         MVC   OPMSG(L'WNOCQ),WNOCQ                                             
         MVC   OPMSG+L'WNOCQ(L'OPMSG-L'WNOCQ),0(R2)                             
         BRAS  RE,WTO                                                           
         B     YES                                                              
*                                                                               
MQMESST  DC    CL8'DAREMSG1',AL1(0,0),XL2'0000',AL4(INDAREM1)                   
         DC    CL8'PRTADB  ',AL1(MQMFISVC,0),XL2'0172',AL4(0)                   
         DC    CL8'REDIXML ',AL1(MQMFISVC,0),XL2'0163',AL4(0)                   
         DC    CL8'NETCLIST',AL1(MQMFISVC,0),XL2'0182',AL4(0)                   
         DC    CL8'CANSPTOM',AL1(MQMFISVC,0),XL2'016F',AL4(0)                   
         DC    CL8'WBRQUEST',AL1(MQMFISVC,0),XL2'016F',AL4(0)                   
         DC    CL8'OEXPRESS',AL1(MQMFISVC,0),XL2'016F',AL4(0)                   
         DC    CL8'TRFNSEBI',AL1(MQMFISVC,0),XL2'016F',AL4(0)                   
         DC    CL8'TRFNOEBI',AL1(MQMFISVC,0),XL2'016F',AL4(0)                   
         DC    CL8'REPRMQ  ',AL1(MQMFISVC,0),XL2'0174',AL4(0)                   
         DC    X'FF'                                                            
*                                                                               
MQMESSTD DSECT                                                                  
MQMID    DS    CL8                 IDENTIFIER                                   
MQMFLAG  DS    X                   FLAGS                                        
MQMFISVC EQU   X'80'               THIS LAUNCHES A SERVICE REQUEST              
         DS    X                                                                
MQMSVC   DS    XL2                 SVC TO LAUNCH (MQMFISVC SET)                 
MQMRTN   DS    AL4                 A(PROCESSING ROUTINE)                        
MQMESSTL EQU   *-MQMESSTD                                                       
*                                                                               
MQP      CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK IF THIS MQ MESSAGE CAN BE PROCESSED                *         
* IF MESSAGE IS OK TO GO RETURN OK                                    *         
* IF NOT THEN MESSAGE IS PLACED ON WORK QUEUE UNTIL REQUIRED          *         
***********************************************************************         
CANIGO   NTR1  ,                                                                
         STM   R2,R3,DUB                                                        
*                                                                               
         L     R2,ASELIST          YOU MUST HAVE THE CONTROL SYSTEM             
         LH    RE,0(R2)            AS A MINIMUM                                 
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         CLI   SESYS,X'0A'                                                      
         BE    *+10                                                             
         BXLE  R2,RE,*-8                                                        
         DC    H'0'                                                             
*                                                                               
         TM    SEIND,SEISTRT       CONTROL SYSTEM STARTED                       
         BZ    CNG10               NO                                           
         TM    SEIND,SEINOP        AND AVAILABLE                                
         BO    CNG10               NO                                           
*                                                                               
         OC    USERID,USERID       SUPPRESS BASED ON USER/SYS?                  
         BNZ   CNG08               YES                                          
*                                                                               
         L     R2,DUB              CHECK IF WE WANT TO SUPPRESS                 
         LA    RF,GOTAB                                                         
         USING GOTABD,RF                                                        
         XR    RE,RE                                                            
*                                                                               
CNG02    CLI   GOMCHL,X'FF'        EOT                                          
         BE    YES                 PASS THROUGH TO PROCESS                      
         IC    RE,GOMCHL                                                        
         EX    RE,*+8                                                           
         BE    CNG04                                                            
         CLC   GOMCH(0),0(R2)      MATCH THIS MESSAGE HEADER                    
         AHI   RF,GOTABL                                                        
         B     CNG02                                                            
*                                                                               
CNG04    MVC   SYSOV,GOSE          OV SYSTEM                                    
         MVC   TSYS,GOSE           ALSO SET UTL'S PHYSICAL SYS NUMBER           
*                                                                               
         ICM   RE,1,GODISP                                                      
         BZ    CNG06                                                            
         AR    RE,R2                                                            
         MVC   AGYALF,0(RE)        AGENCY ALPHA                                 
         B     CNG08                                                            
*                                                                               
CNG06    IC    RE,GOUDISP                                                       
         AR    RE,R2                                                            
         MVC   USERID,0(RE)        USER ID                                      
         XC    SYSC3,SYSC3                                                      
         DROP  RF                                                               
*                                                                               
CNG08    BRAS  RE,CHECKSE                                                       
         BE    YES                                                              
*                                                                               
CNG10    SAM31                     GET XA TBUFF WITH FULL MSG                   
         ICM   RF,15,ATCB                                                       
         L     R2,TCBRBUFF-TCBD(RF)                                             
         LR    R1,R2                                                            
         AHI   R1,-8                                                            
         AHI   R2,8                R2 = REMOVE FROMMQCT                         
         LH    R0,6(R1)                                                         
         AHI   R0,-8                                                            
*                                                                               
         L     R1,ASSB                                                          
         ICM   R1,15,SSBAMQWK-SSBD(R1)                                          
         MVC   WORK(48),0(R1)                                                   
*                                                                               
         GOTO1 MQIO,DMCB,=CL8'PUTQ',(R2),(R0),WORK                              
*                                                                               
         CLC   SVMQION,=F'2'       EXTENDED TRACING?                            
         BNE   NO                  NO                                           
*                                                                               
         MVC   OPMSG,SPACES                                                     
         MVC   OPMSG(L'WWQ),WWQ                                                 
         ICM   RF,15,ATCB          GET XA TBUFF WITH FULL MSG                   
         L     R2,TCBRBUFF-TCBD(RF)                                             
         LR    RF,R2                                                            
         AHI   RF,-2               GET INPUT LENGTH                             
         LH    RE,0(RF)                                                         
         CHI   RE,L'OPMSG-L'WWQ    WILL IT ALL FIT?                             
         BL    *+8                 YES                                          
         LHI   RE,L'OPMSG-L'WWQ                                                 
         BCTR  RE,0                                                             
         MVC   OPMSG+L'WWQ(0),0(R2)                                             
         EX    RE,*-6                                                           
         BRAS  RE,WTO                                                           
         B     NO                  FLAG TO NOT CONTINUE                         
*                                                                               
         DS    0D                                                               
GOTAB    DC    AL1(10),CL16'PRTADB  ESR     ',AL1(04,13,00),XL12'00'            
         DC    AL1(12),CL16'PRTADB  WEBIO   ',AL1(04,13,00),XL12'00'            
         DC    AL1(05),CL16'PRTADB          ',AL1(04,00,08),XL12'00'            
         DC    X'FF'                                                            
*                                                                               
GOTABD   DSECT                                                                  
GOMCHL   DS    X                   L'-1 OF MATCH STRING                         
GOMCH    DS    CL16                MATCH STRING                                 
GOSE     DS    X                   SEOVSYS TO MATCH ON                          
GODISP   DS    X                   DISPLACEMENT TO 2 BYTE ALPHA                 
GOUDISP  DS    X                   N/D                                          
         DS    XL12                N/D                                          
GOTABL   EQU   *-GOTABD                                                         
*                                                                               
MQP      CSECT                                                                  
         EJECT                                                                  
**********************************************************************          
* CHECK SE IS AVAILABLE                                              *          
* NTRY: USERID = USER ID                                             *          
*       SYSC3  = SYSTEM SHORT NAME (PRINT=PRT, SPOT=SPT ETC)         *          
* OR:   AGYALF = 2 CHAR AGENCY ALPHA                                 *          
*       SYSOV  = OV SYSTEM NUMBER  (PRINT=04, SPOT=02 ETC)           *          
* EXIT: CC OK  = IF OK TO PROCEED                                    *          
**********************************************************************          
CHECKSE  NTR1  ,                                                                
         OC    USERID,USERID      DID WE COME IN WITH USERID/SYS                
         BZ    CKSE04             NO - MUST BE ALPHA THEN                       
         OC    SYSC3,SYSC3                                                      
         BZ    CKSE01                                                           
*                                                                               
         LA    R1,SYSLST                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,RELO                                                          
         AHI   R1,6                                                             
         USING SYSLSTD,R1                                                       
         CLC   SYSLSHRT,SYSC3     MATCH SYSTEM                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     NO                                                               
*                                                                               
         MVC   SYSOV,SYSLNUM      OV SYSTEM NUMBER                              
         MVC   TSYS,SYSLNUM       ALSO SET UTL'S PHYSICAL SYS NUMBER            
         DROP  R1                                                               
*                                                                               
CKSE01   CLC   =C'AGY=',USERID                                                  
         BNE   *+14                                                             
         MVC   AGYALF,USERID+4                                                  
         B     CKSE04                                                           
*                                                                               
         LA    R4,IO                                                            
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,USERID                                                    
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,IO,IO                                 
         CLI   8(R1),0                                                          
         BNE   NO                  BAD USERID                                   
*                                                                               
         LA    R4,CTIDATA                                                       
         XR    RF,RF                                                            
CKSE02   CLI   0(R4),0             E-O-R                                        
         BE    NO                                                               
         CLI   0(R4),CTDSCELQ      X'02' - DESCRIPTION ELEM                     
         BE    CKSE03DS                                                         
         CLI   0(R4),CTAGYELQ      X'06' - AGENCY ALPHA ID ELEM                 
         BE    CKSE03AA                                                         
CKSE03   LLC   RF,1(R4)                                                         
         BXH   R4,RF,CKSE02                                                     
*                                                                               
         USING CTDSCD,R4           PULL OUT USER ID NUM                         
CKSE03DS MVC   TUSER,CTDSC         SET USER ID NUM                              
         B     CKSE03                                                           
         DROP  R4                                                               
*                                                                               
         USING CTAGYD,R4           PULL OUT AGY ALPHA                           
CKSE03AA MVC   AGYALF,CTAGYID      SET AGENCY ALPHA                             
         DROP  R4                                                               
*                                                                               
CKSE04   LA    R4,IO               READ AGY ALPHA RECORD                        
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ    RECORD ID                                    
         MVC   CT5KALPH,AGYALF     SET AGENCY ALPHA                             
         GOTO1 DATAMGR,DMCB,DMREAD,CTFILE,(R4),(R4)                             
         CLI   8(R1),0                                                          
         BNE   NO                                                               
*                                                                               
         LA    R1,CT5DATA                                                       
         XR    RF,RF                                                            
         USING CTSYSD,R1                                                        
CKSE06   CLI   CTSYSEL,0           MUST FIND AUTH ELEMENT                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CTSYSEL,CTSYSELQ    FIND SYSTEM AUTH ELEMENT                     
         BNE   *+14                                                             
         CLC   CTSYSNUM,SYSOV      MATCH SYSTEM                                 
         BE    *+12                                                             
         IC    RF,CTSYSLEN                                                      
         BXH   R1,RF,CKSE06                                                     
*                                                                               
         MVC   SYSSE,CTSYSSE       OVERLAY REAL SYSTEM                          
         MVC   TOVSYS,CTSYSSE      ALSO SET UTL'S LOGICAL SYS NUMBER            
         DROP  R4,R1                                                            
*                                                                               
         L     R2,ASELIST          CHECK REAL SYSTEM THERE AND UP               
         LH    RE,0(R2)                                                         
         L     RF,2(R2)                                                         
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         CLC   SESYS,SYSSE         MATCH SE IN SELIST                           
         BE    *+10                                                             
         BXLE  R2,RE,*-10                                                       
         DC    H'0'                                                             
*                                                                               
         TM    SEIND,SEISTRT       USER SE SYSTEM STARTED                       
         BZ    NO                                                               
         TM    SEIND,SEINOP        AND AVAILABLE                                
         BO    NO                                                               
         B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO PROCESS CONTROL Q INPUT DATA FOR 'DAREMSG1'             *          
* NTRY: R2 = A(DATA BUFFER)                                          *          
*       R3 = L'DATA BUFFER                                           *          
*                                                                    *          
* INCOMING MESSAGE FORMAT IS AS FOLLOWS:                             *          
* CL08 DAREMSG1                                                      *          
* CL05 USER ID NUMBER                                                *          
* CL03 USER INITIALS                                                 *          
* CL04 TIME HHMM                                                     *          
* CL01 MAIL TYPE                                                     *          
**********************************************************************          
INDAREM1 CLC   SVMQION,=F'2'     WRITE OUT MESSAGE AND DATA TO JESMSG           
         BNE   IDM102                                                           
         MVC   OPMSG,SPACES                                                     
         MVC   OPMSG(L'WDAREM),WDAREM                                           
*                                                                               
         LR    RE,R3                                                            
         CHI   RE,L'OPMSG-L'WDAREM                                              
         BL    *+8                                                              
         LHI   RE,L'OPMSG-L'WDAREM                                              
         BCTR  RE,0                                                             
         MVC   OPMSG+L'WDAREM(0),0(R2)                                          
         EX    RE,*-6                                                           
         BRAS  RE,WTO                                                           
*                                                                               
NEW      USING TBDDARD,WORK                                                     
IDM102   AHI   R2,8                POINT TO MESSAGE                             
         XC    WORK,WORK                                                        
         CLC   =C'***',0(R2)       SPECIAL FOR ADV SYSTEM                       
         BNE   IDM104              NEXT 2 CHARACTERS ARE THE AGENCY             
         MVC   NEW.TBDAGY,3(R2)    CODE IN EBCDIC                               
         B     IDM106                                                           
*                                                                               
IDM104   PACK  DUB,0(5,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,NEW.TBDUSER    SET USER-ID                                  
*                                                                               
IDM106   MVC   NEW.TBDINIT,5(R2)   COPY IN INITIALS OR SET ZERO                 
         CLC   NEW.TBDINIT,=CL3' '                                              
         BH    *+10                                                             
         XC    NEW.TBDINIT,NEW.TBDINIT                                          
*                                                                               
         PACK  DUB,8(4,R2)         NEED TIME HHMM AS PWOS PL2                   
         SRP   DUB,11,0                                                         
         MVC   NEW.TBDTIME,DUB                                                  
*                                                                               
         MVC   NEW.TBDTYPE,12(R2)  SET MAIL TYPE                                
         BRAS  RE,ARSOFF                                                        
*                                                                               
         ICM   R2,15,=A(DTDARE)                                                 
         N     R2,=X'000000FF'     GET DISPLACEMENT TO DARE TABLE               
         MHI   R2,L'DSPHDR                                                      
*                                                                               
         L     RE,ASSB                                                          
         LAM   AR2,AR2,SSBTBLET-SSBD(RE)                                        
         SAC   512                                                              
         ICM   R2,15,DSPTFRST-DMSPACED(R2)                                      
         USING TABDARED,R2                                                      
*                                                                               
IDM108   CLC   TBDMAX,TBDNOW       FREE SLOTS?                                  
         BNH   IDM110              NO - JUST EXIT + IGNORE POSTING              
*                                                                               
         ICM   RE,15,TBDNOW        FIND CURRENT HIGH WATER                      
         LA    RF,1(RE)                                                         
         CS    RE,RF,TBDNOW                                                     
         BNE   IDM108                                                           
*                                                                               
         LA    RF,TBDDARL          INDEX INTO UNSORTED ARRAY                    
         MSR   RE,RF                                                            
         ICM   R2,15,TBDUNSRT                                                   
         AR    R2,RE               R2 POINTS TO THIS ENTRY                      
         MVC   0(TBDDARL,R2),NEW.TBDDARD                                        
*                                                                               
IDM110   BRAS  RE,ARSOFF                                                        
         B     YES                                                              
         DROP  NEW,R2                                                           
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO LAUNCH A SERVICE REQUEST                                *          
* NTRY: R2 = A(DATA BUFFER)                                          *          
*       R3 = L'DATA BUFFER                                           *          
*       R4 = MQMESST TABLE ENTRY                                     *          
**********************************************************************          
         USING MQMESSTD,R4                                                      
SVCGO    CLC   SVMQION,=F'2'     WRITE OUT MESSAGE AND DATA TO JESMSG           
         BNE   SVCG02                                                           
*                                                                               
         MVC   OPMSG,SPACES                                                     
         MVC   OPMSG(L'WSVCGO),WSVCGO                                           
         MVC   OPMSG+L'WSVCGO(L'MQMID),MQMID                                    
*                                                                               
         LA    RF,OPMSG+L'WSVCGO+L'MQMID                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C':'                                                       
         AHI   RF,3                                                             
         LR    R0,RF                                                            
         LA    R1,OPMSG+L'OPMSG                                                 
         SR    R1,R0               R1 = AMOUNT REMAINING                        
*                                                                               
         CR    R1,R3               MOVE FOR AMOUNT REMAINING                    
         BL    *+6                                                              
         LR    R1,R3               OR LENGTH OF MESSAGE IF SHORTER              
         BCTR  R1,0                                                             
         MVC   0(0,RF),0(R2)                                                    
         EX    R1,*-6                                                           
         BRAS  RE,WTO                                                           
*                                                                               
SVCG02   MVC   HALF,MQMSVC         SET SPECIAL S/R                              
         DROP  R4                                                               
*                                                                               
         GOTO1 ATICTOC,DUB,C'SSET'                                              
*                                                                               
         LHI   R0,1                                                             
         GOTO1 ALCM,DMCB,VTGETBUF  GET A LCBUFF BUFFER                          
         LTR   R4,R1                                                            
         BNZ   *+12                                                             
         BRAS  RE,NOUTL                                                         
         B     SVCG10                                                           
*                                                                               
         LHI   R0,2                                                             
         GOTO1 ALCM,DMCB,VTGETUTL  GET A UTL FOR THIS MESSAGE                   
         LTR   R5,R1                                                            
         BNZ   SVCG04                                                           
         GOTO1 ALCM,DMCB,VTRELBUF,(R4)                                          
         BRAS  RE,NOUTL                                                         
         B     SVCG10                                                           
*                                                                               
N        USING UTLD,R5                                                          
SVCG04   SAM31                                                                  
         STCM  R4,15,N.TBUFF       SET A(TBUFF)                                 
         AHI   R4,-2                                                            
         STH   R3,0(R4)            SET LENGTH OF MESSAGE                        
         AHI   R4,2                                                             
*                                                                               
         ICM   RF,15,ATCB                                                       
         S     R2,TBUFF                                                         
         A     R2,TCBRBUFF-TCBD(RF)                                             
*                                                                               
         LR    R0,R4               COPY DATA INTO BUFFER                        
         LR    R1,R3                                                            
         MVCL  R0,R2               NOTE - ORIGINAL BUFFER INVALID               
*                                                                               
         L     R2,ASELIST          SE1                                          
         AHI   R2,6                                                             
         USING SELISTD,R2                                                       
         MVC   N.TLUID,=CL8'NONENONE'                                           
         MVC   N.TXNEXTIN,SEFIRST  SET LINK TO NEXT UTL ENTRY                   
         MVC   N.TNEXTIN,SEFIRST+1 .....                                        
         MVC   N.TSVCREQ,HALF      SET SPECIAL S/R                              
         OI    N.TSTATC,TSTCMQ                                                  
*****                                                                           
         MVC   N.TUSER,TUSER                                                    
         MVC   N.TSYS,TSYS                                                      
         MVC   N.TOVSYS,TOVSYS                                                  
         MVC   N.TAGY,AGYALF                                                    
*                                                                               
         TIME  TU                                                               
         ST    R0,N.TTIMETU        SET ARRIVAL TIME TO NOW                      
*                                                                               
         STCM  R5,15,SEFIRST       SET AT TOP OF SE1 QUEUE                      
         XR    RF,RF                                                            
         ICM   RF,3,SEQLEN                                                      
         BNZ   *+8                                                              
         STCM  R5,15,SELAST                                                     
         AHI   RF,1                                                             
         STH   RF,SEQLEN                                                        
*                                                                               
         L     R2,ASSB                                                          
         USING SSBD,R2                                                          
*                                                                               
SVCG06   ICM   RE,15,SSBMQNUM      INCREMENT CURRENT/TOTAL COUNTERS             
         LA    RF,1(RE)                                                         
         CS    RE,RF,SSBMQNUM                                                   
         BNE   SVCG06                                                           
*                                                                               
SVCG08   ICM   RE,15,SSBMQTDY                                                   
         LA    RF,1(RE)                                                         
         CS    RE,RF,SSBMQTDY                                                   
         BNE   SVCG08                                                           
*                                                                               
SVCG10   GOTO1 ATICTOC,DUB,C'RSET'                                              
         B     YES                                                              
         DROP  N,R2                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO OUTPUT 'RAN OUT OF UTL ENTRIES' MESSAGE                  *         
***********************************************************************         
NOUTL    GOTO1 ATICTOC,DUB,C'RSET'                                              
         DC    H'0'       <<< WE OUGHT TO DO BETTER THAN THIS...                
*                         <<< BUT THEN AGAIN THE SYSTEM IS LIKELY DO            
*                         <<< FALL OVER SO WGAF                                 
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
WTO      NTR1  ,                                                                
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPMSG,L'OPMSG,C'LVL1'                      
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         MVC   OPMSG,SPACES                                                     
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
***********************************************************************         
INIT     NTR1  ,                                                                
         L     R1,APARMS                                                        
         USING SRPARMD,R1                                                       
         MVC   ATWA,SRQATWA                                                     
         L     RF,SRQASYSF                                                      
         USING SYSFACD,RF                                                       
         ST    RF,ASYSFACS                                                      
         MVC   ADTFIOA,VDTFIOA                                                  
         MVC   ASELIST,VSELIST                                                  
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ADMOD000,VDMOD000                                                
         MVC   ALCM,VLCM                                                        
*                                                                               
         L     RE,VSSB                                                          
         ST    RE,ASSB                                                          
         USING SSBD,RE                                                          
         MVC   SVMQION,SSBMQION                                                 
         MVC   WCQ+4(4),SSBSYSN4                                                
         MVC   WNOCQ+4(4),SSBSYSN4                                              
         MVC   WDAREM+4(4),SSBSYSN4                                             
         MVC   WSVCGO+4(4),SSBSYSN4                                             
         L     RF,SSBTKADR                                                      
         DROP  RE,RF                                                            
*                                                                               
         ST    RF,ATCB                                                          
         L     RF,SRQATIA                                                       
         ST    RF,ATIA                                                          
         L     RF,SRQACOMF                                                      
         ST    RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETFACT,CGETFACT                                                 
         MVC   MQIO,CMQIO                                                       
         B     YES                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,=16F'0'                                                  
         BR    RE                                                               
*                                                                               
YES      CR    RB,RB               RETURN CC EQUAL                              
         B     EXIT                                                             
*                                                                               
NO       CLI   *,255               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
WCQ      DC    CL16'*FACPAK* $MQP : '                                           
WWQ      DC    CL20'*FACPAK* TO WORK Q: '                                       
WNOCQ    DC    CL32'*FACPAK* $MQP UNKNOWN CQ INPUT: '                           
WDAREM   DC    CL24'*FACPAK* $MQP DAREMSG1: '                                   
WSVCGO   DC    C'*FACPAK* $MQP SVC '                                            
*                                                                               
DMREAD   DC    C'DMREAD '                                                       
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    CL126' '                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
       ++INCLUDE FASYSLST                                                       
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
RELO     DS    A                                                                
DMCB     DS    8F                                                               
FULL     DS    F                                                                
SAVERD   DS    F                                                                
SVMQION  DS    F                                                                
*                                                                               
USERID   DS    CL10                                                             
AGYALF   DS    CL2                                                              
SYSC3    DS    CL3                                                              
SYSOV    DS    X                                                                
SYSSE    DS    X                                                                
*                                                                               
HALF     DS    H                                                                
BYTE     DS    X                                                                
WORK     DS    XL80                                                             
OPMSG    DS    CL126               126=MAX WTO LENGTH                           
*                                                                               
APARMS   DS    A                   A(PARAMETER LIST)                            
*                                                                               
ASYSFACS DS    A                   A(SYSFACS)                                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL)                                       
ACOMFACS DS    A                   A(COMFACS)                                   
ATWA     DS    A                   A(TWA)                                       
ATICTOC  DS    A                   A(TICTOC)                                    
AWCTYPE  DS    A                   A(WCTYPE)                                    
ADMOD000 DS    A                   A(DMOD000)                                   
ASSB     DS    A                   A(SSB)                                       
ALCM     DS    A                   A(LCM)                                       
ATCB     DS    A                   A(TCB)                                       
ADTFIOA  DS    A                   A(DTFIOA)                                    
ASELIST  DS    A                   A(SELIST)                                    
*                                                                               
CALLOV   DS    A                                                                
DATAMGR  DS    A                                                                
GETFACT  DS    A                                                                
MQIO     DS    A                                                                
*                                                                               
IO       DS    2048X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
* SRMQPFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRMQPFFD                                                       
         PRINT ON                                                               
* FAMQMSGD                         MQ UNIT OF WORK                              
         PRINT OFF                                                              
       ++INCLUDE FAMQUOWD                                                       
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FATBHD                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATBHD                                                         
         PRINT ON                                                               
* DDMQBMD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMQBMD                                                        
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* FATABSDAR                                                                     
         PRINT OFF                                                              
       ++INCLUDE FATABSDAR                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SRMQP00   07/30/12'                                      
         END                                                                    
