*          DATA SET TAREP49    AT LEVEL 143 AS OF 05/01/02                      
*PHASE T70349A,*                                                                
*                                                                               
         TITLE 'T70349 - NETWORK CLA INF FROM DISK'                             
T70349   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70349,R9                                                      
         SPACE 2                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         LA    R7,BUFF             R7=A(LOCAL W/S)                              
         LA    R7,8(R7)                                                         
         USING TNGD,R7             R7=A(LOCAL W/S)                              
         EJECT                                                                  
         SPACE 2                                                                
*              PROGRAM GENERATES AN NE1 WORKER FILE                             
*              BASED ON INPUT TAPE INFORMATION                                  
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 2                                                                
         L     R2,TWADCONS         GET ADDRESS OF PRNTBL                        
         USING TWADCOND,R2                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         SPACE 1                                                                
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         CLI   MCPOSTNG,C'N'       IF POSTINGS=N IN MASTER                      
         BNE   *+8                                                              
         MVI   WRKOPT,C'N'         THEN SET NOT TO GENERATE WORKER FILE         
         DROP  R1,R2                                                            
         SPACE 2                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE2    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,VREC                                                          
         BAS   RE,OPENTAPE         OPEN INPUT                                   
         BAS   RE,PREP             PROCESS INPUT/GENERATE WORKER FILE           
         BAS   RE,CLOSTAPE         CLOSE INPUT                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE RECORD                                                  
         SPACE 3                                                                
VREC     NTR1                                                                   
         LA    R2,SPLOPTH          VALIDATE OPTIONS                             
         BAS   RE,VOPTS                                                         
         B     XIT                                                              
         SPACE 2                                                                
*              VALIDATE OPTIONS                                                 
         SPACE 3                                                                
VOPTS    NTR1                                                                   
         MVI   TRACOPT,C'N'                                                     
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         LA    R3,BLOCK                                                         
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    INVERR                                                           
         SPACE 1                                                                
OPT2     CLC   =C'TRACE',SCDATA1    TRACE OPTION                                
         BNE   INVERR                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   INVERR                                                           
         MVI   TRACOPT,C'Y'                                                     
         B     OPTEND                                                           
         SPACE 1                                                                
OPTEND   LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,OPT2                                                          
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              PROCESS INPUT AND GENERATE WORKER FILE                           
         SPACE 2                                                                
PREP     NTR1                                                                   
         MVI   OPENWK,C'N'         SET WORKER FILE NOT OPENED                   
         LA    R1,MYSPECS          SET A(SPECS FOR PRINTING)                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK             SET A(HEADLINE HOOK)                         
         ST    R1,HEADHOOK                                                      
*                                                                               
PREP5    BAS   RE,GETTAPE          GET FIRST(NEXT) INPUT RECORD                 
         LA    R2,TAPEREC          R2=A(INPUT RECORD)                           
         USING TAPED,R2                                                         
*                                                                               
         CLI   TAPAGY,X'FF'        IF END OF INPUT                              
         BNE   PREP30                                                           
         BAS   RE,CLOSWK           CLOSE WORKER FILE                            
         B     XIT                 AND EXIT                                     
*                                                                               
PREP30   CLI   OPENWK,C'Y'         IF WORKER FILE NOT OPENED                    
         BE    *+12                                                             
         BAS   RE,GETUID           GET USERID FOR WORKER FILE                   
         BAS   RE,OPNWK            OPEN WORKER FILE                             
         MVI   OPENWK,C'Y'         SET PREVIOUSLY OPENED                        
*                                                                               
         LA    R6,WREC             R6=A(BUILD WORKER FILE RECORD)               
         MVC   0(4,R6),=F'4'       INITIALIZE RECORD LENGTH                     
*                                                                               
         LA    R4,4(R6)            R4=A(FIRST ELEMENT)                          
         BAS   RE,ADDTANX          ADD NETWORK DETAILS ELEMENT                  
         BAS   RE,ADDTACT          ADD CLIENT CODE ELEMENT                      
         BAS   RE,ADDTAFNC         ADD CLIENT NAME ELEMENT                      
         BAS   RE,ADDTAFNP         ADD PRODUCT NAME ELEMENT                     
         BAS   RE,ADDTAFNT         ADD COMMERCIAL TITLE ELEMENT                 
         BAS   RE,ADDTANP          ADD CLA NETWORK ELEMENT                      
*                                                                               
         MVC   0(2,R6),2(R6)       SET LENGTH IN FIRST 2 BYTES                  
         XC    2(2,R6),2(R6)                                                    
         BAS   RE,PUTWK            PUT RECORD TO WORKER FILE                    
*                                                                               
         BAS   RE,PLINE            PUT NETWORK INFO TO PRINT LINE               
         B     PREP5                                                            
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE GETS USER ID OF WORKER FILE FROM AGENCY RECORD           
         SPACE 1                                                                
         USING TAPED,R2                                                         
GETUID   NTR1                                                                   
         MVC   TGAGY(L'TAPAGY),TAPAGY                                           
         OC    TGAGY,SPACES                                                     
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',0)                                    
         BNE   ERRAGY                                                           
*                                  SAVE AGENCY NAME                             
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   AGYNM,TGNAME                                                     
*                                                                               
         LA    R3,AGYTAB           R3=A(AGENCY TABLE)                           
GETUID5  CLI   0(R3),X'FF'                                                      
         BE    ERRAGY                                                           
         CLC   TGAGY,0(R3)                                                      
         BE    *+12                                                             
         LA    R3,L'AGYTAB(R3)                                                  
         B     GETUID5                                                          
*                                                                               
         GOTO1 USERVAL,DMCB,(X'A0',6(R3))                                       
         BNE   ERRAGY                                                           
         B     XIT                                                              
         DROP  R2                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
         USING TAPED,R2                                                         
PLINE    NTR1                                                                   
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PLINED,R3                                                        
*                                                                               
         MVC   PCID,TAPNID         COMMERCIAL ID                                
         MVC   PTTL,TAPTTL         COMMERCIAL TITLE                             
         MVC   PSEC,TAPSEC         COMMERCIAL LENGTH                            
         MVC   PCLI,TAPCLI         CLIENT CODE                                  
         MVC   PCLIN,TAPCLIN       CLIENT NAME                                  
         MVC   PPRDN,TAPPRDN       PRODUCT NAME                                 
         GOTO1 DATCON,DMCB,(0,TAPUSEDT),(8,PUSEDATE)                            
         MVC   PPGMNAME,TAPPGM     PROGRAM NAME                                 
         MVC   PNETWORK,TAPNWK     NETWORK                                      
         BAS   RE,SPLAT                                                         
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
         USING TAPED,R2                                                         
         SPACE 1                                                                
*              ADD NETWORK TRANSFER DETAILS ELEMENT TO WORKER FILE              
         SPACE 1                                                                
         USING TANXD,R4                                                         
ADDTANX  NTR1                                                                   
         MVI   TANXEL,TANXELQ      SET ELEMENT CODE                             
         MVI   TANXLEN,TANXLNQ     SET ELEMENT LENGTH                           
         MVC   TANXAGY(L'TAPAGY),TAPAGY                                         
         OC    TANXAGY,SPACES      TALENT AGENCY                                
         MVC   TANXNCID,TAPNID     CID                                          
         MVC   TANXUID(L'TGUSERID),TGUSERID                                     
         OC    TANXUID,SPACES      USER ID OF WORKER FILE                       
         PACK  DUB,TAPSEC          LENGTH IN SECONDS                            
         CVB   R1,DUB                                                           
         STC   R1,TANXSEC                                                       
         OI    TANXSTAT,TANXSAGY   SET AGENCY TRANSFER                          
         BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         SPACE 2                                                                
*              ADD CLIENT CODE ELEMENT TO WORKER FILE                           
         SPACE 1                                                                
         USING TACTD,R4                                                         
ADDTACT  NTR1                                                                   
         MVI   TACTEL,TACTELQ      SET ELEMENT CODE                             
         MVI   TACTLEN,TACTLNQ     SET ELEMENT LENGTH                           
         MVC   TACTCLI(L'TAPCLI),TAPCLI                                         
         OC    TACTCLI,SPACES      CLIENT CODE                                  
         BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         SPACE 2                                                                
*              ADD CLIENT NAME ELEMENT TO WORKER FILE                           
         SPACE 1                                                                
         USING TAFND,R4                                                         
ADDTAFNC NTR1                                                                   
         MVI   TAFNEL,TAFNELQ      SET ELEMENT CODE                             
         MVI   TAFNLEN,TAFNLNQ+L'TAFNNAME                                       
         MVI   TAFNTYPE,TAFNTCLI                                                
         MVC   TAFNNAME(L'TAPCLIN),TAPCLIN                                      
         OC    TAFNNAME,SPACES     CLIENT NAME                                  
         BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         EJECT                                                                  
*              ADD PRODUCT NAME ELEMENT TO WORKER FILE                          
         SPACE 1                                                                
         USING TAFND,R4                                                         
ADDTAFNP NTR1                                                                   
         MVI   TAFNEL,TAFNELQ      SET ELEMENT CODE                             
         MVI   TAFNLEN,TAFNLNQ+L'TAFNNAME                                       
         MVI   TAFNTYPE,TAFNTPRD                                                
         MVC   TAFNNAME(L'TAPPRDN),TAPPRDN                                      
         OC    TAFNNAME,SPACES     PRODUCT NAME                                 
         BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         SPACE 2                                                                
*              ADD COMMERCIAL TITLE NAME TO WORKER FILE                         
         SPACE 1                                                                
         USING TAFND,R4                                                         
ADDTAFNT NTR1                                                                   
         MVI   TAFNEL,TAFNELQ      SET ELEMENT CODE                             
         MVI   TAFNLEN,TAFNLNQ+L'TAFNNAME                                       
         MVI   TAFNTYPE,TAFNTTTL                                                
         MVC   TAFNNAME(L'TAPTTL),TAPTTL                                        
         OC    TAFNNAME,SPACES     COMMERCIAL TITLE NAME                        
         BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         SPACE 2                                                                
*              ADD NETWORK USE DETAILS ELEMENT TO WORKER FILE                   
         SPACE 1                                                                
         USING TANPD,R4                                                         
ADDTANP  NTR1                                                                   
         MVI   TANPEL,TANPELQ      SET ELEMENT CODE                             
         MVI   TANPLEN,TANPLNQ2                LENGTH                           
         GOTO1 DATCON,DMCB,(0,TAPUSEDT),(1,TANPDATE)                            
         MVC   TANPPNME,TAPPGM     PROGRAM NAME                                 
*                                                                               
         LA    RE,CALLTAB                                                       
ADDTANP5 CLI   0(RE),X'FF'                                                      
         BNE   *+12                                                             
         MVI   TANPNWK,C'S'        DEFAULT TO SYNDICATION                       
         B     ADDTANPX                                                         
*                                                                               
ADDTANP8 CLC   TAPNWK,0(RE)        MATCH ON NETWORK CALL LETTERS                
         BE    *+12                                                             
         LA    RE,L'CALLTAB(RE)                                                 
         B     ADDTANP5                                                         
         MVC   TANPNWK,0(RE)       USE FIRST LETTER                             
*                                                                               
ADDTANPX BAS   RE,BUMPEL           BUMP TO NEXT POSITION & UPDATE LEN           
         B     XITR4                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
*              MISC ROUTINES                                                    
         SPACE 2                                                                
*              ROUTINE UPDATES LENGTH & POINTS TO NEXT POSITION IN REC          
         SPACE 1                                                                
BUMPEL   DS    0H                                                               
         ZIC   R1,1(R4)            L'ELEMENT + L'WORKER FILE RECORD             
         L     R2,0(R6)                                                         
         AR    R2,R1                                                            
         ST    R2,0(R6)            IS NEW WORKER FILE RECORD LENGTH             
         AR    R4,R1               R4=A(NEXT POSITION IN WREC)                  
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO PRINT LINE                                            
         SPACE 1                                                                
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT HEADLINE                                        
         SPACE 1                                                                
HOOK     NTR1                                                                   
         MVC   H2+7(L'TGAGY),TGAGY  SET TALENT AGENCY                           
         MVC   H2+14(L'AGYNM),AGYNM AND AGENCY NAME                             
         CLI   WRKOPT,C'N'          IF NOT GENERATING WORKER FILE               
         BNE   XIT                                                              
         MVC   H3+57(8),=CL8'TEST RUN'                                          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRACE                                                 
         SPACE 1                                                                
MYTRACE  NTR1                                                                   
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            LENGTH OF RECORD                             
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         B     XIT                                                              
         EJECT                                                                  
*              TAPE ROUTINES                                                    
         SPACE 2                                                                
*              ROUTINE TO OPEN THE TAPE FOR INPUT                               
         SPACE 1                                                                
OPENTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         OPEN  ((2),INPUT)                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLOSE THE TAPE                                        
         SPACE 1                                                                
CLOSTAPE NTR1                                                                   
         L     R2,=A(TAPEIN)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO GETA RECORD FROM THE TAPE                             
         SPACE 1                                                                
GETTAPE  NTR1                                                                   
         LA    R0,TAPEREC                                                       
         L     R1,=A(TAPEIN)                                                    
         GET   (1),(0)                                                          
         CLI   TRACOPT,C'Y'                                                     
         BNE   XIT                                                              
         GOTO1 MYTRACE,DMCB,=C'TAPEIN',TAPEREC,L'TAPEREC                        
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET END-OF-FILE ON TAPE                               
         SPACE 1                                                                
TAPEEOF  MVI   TAPEREC,X'FF'       SET END OF INPUT                             
         B     XIT                                                              
         EJECT                                                                  
*              WORKER FILE ROUTINES                                             
         SPACE 2                                                                
*              ROUTINE TO OPEN THE WORKER FILE                                  
         SPACE 1                                                                
OPNWK    NTR1                                                                   
         CLI   WRKOPT,C'N'                                                      
         BE    XIT                                                              
         XC    WID,WID                                                          
         LA    R2,WID                                                           
         USING UKRECD,R2                                                        
         MVC   UKUSRID,TGUSER      USER ID NUMBER FOR WORKER FILE               
         MVC   UKSYSPRG,=C'NE1'    FILE NAME                                    
         MVC   UKDAY,TGTODAY1+2    TODAY'S DATE                                 
         MVI   UKCLASS,C'T'        CLASS                                        
         MVI   UKFLAG,X'01'        ALLOW DUPLICATE FILES                        
         OI    UKFLAG,X'10'        RETENTION DAYS SET IN REC                    
         MVC   COMMAND(6),=CL6'OPEN'                                            
         LA    R3,WREC+28                                                       
         USING WKRECD,R3                                                        
         MVC   WKRETN,=H'7'        RETAIN FOR 7 DAYS                            
         B     WKFILE                                                           
         DROP  R2,R3                                                            
         SPACE 2                                                                
*              ROUTINE TO ADD A RECORD TO THE WORKER FILE                       
         SPACE 1                                                                
PUTWK    NTR1                                                                   
         CLI   TRACOPT,C'Y'                                                     
         BNE   PUTWK10                                                          
         XR    R2,R2               LENGTH OF ACTUAL WORKER FILE RECORD          
         ICM   R2,3,WREC                                                        
         S     R2,=F'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'WREC',WREC+4,(R2)                                
PUTWK10  MVC   COMMAND(6),=CL6'ADD'                                             
         B     WKFILE                                                           
         SPACE 2                                                                
*              ROUTINE TO CLOSE WORKER FILE                                     
         SPACE 1                                                                
CLOSWK   NTR1                                                                   
         MVC   COMMAND(6),=CL6'CLOSE'                                           
         B     WKFILE                                                           
         SPACE 2                                                                
*              COMMON WORKER FILE CALL                                          
         SPACE 1                                                                
WKFILE   DS    0H                                                               
         CLI   WRKOPT,C'N'                                                      
         BE    XIT                                                              
         L     R4,=A(POSTBUFF)     A(4K BUFFER)                                 
         LA    R5,WID              A(WORKER FILE ID)                            
         LA    R3,WREC             A(REC) TO ADD TO WORKER FILE                 
         GOTO1 WORKER,DMCB,COMMAND,(R4),(R5),(R3)                               
         TM    DMCB+8,X'C0'                                                     
         BZ    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              ODD ROUTINES                                                     
         SPACE 3                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRORXIT                                                         
*                                                                               
ERRAGY   MVI   ERROR,ERINVRAG      INVALID FOR THIS AGENCY                      
         B     ERRORXIT                                                         
*                                                                               
ERRORXIT GOTO1 ERREX                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
XITR4    XIT1 REGS=(R4)                                                         
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              AGENCY TABLE                                                     
         SPACE 1                                                                
AGYTAB   DS    0CL12                                                            
         DC    C'0050  ',C'DCDE  '                                              
         DC    C'0162  ',C'DCDE  '                                              
         DC    C'0601  ',C'DCDE  '                                              
         DC    C'0602  ',C'DCDE  '                                              
         DC    C'1150  ',C'DCDE  '                                              
         DC    C'0004  ',C'FLNY  '                                              
         DC    X'FF'                                                            
         SPACE 2                                                                
*              NETWORK TABLE                                                    
         SPACE 1                                                                
CALLTAB  DS    0CL5                                                             
         DC    CL5'ABC'            ABC                                          
         DC    CL5'CBS'            CBS                                          
         DC    CL5'NBC'            NBC                                          
         DC    CL5'FOX'            FOX                                          
         DC    CL5'UPN'            UPN                                          
         DC    CL5'WB'             WB                                           
         DC    X'FF'                                                            
         SPACE 2                                                                
*              HEADLINES ETC                                                    
         SPACE 1                                                                
MYSPECS  SSPEC H1,1,RUN                                                         
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H3,112,PAGE                                                      
         SSPEC H2,1,C'AGENCY'                                                   
         SPACE 1                                                                
         SSPEC H1,53,C'NETWORK GENERATOR'                                       
         SSPEC H2,53,17X'BF'                                                    
         SPACE 1                                                                
         SSPEC H7,02,C'COMML ID  COMML TITLE'                                   
         SSPEC H8,02,C'--------  -----------'                                   
         SSPEC H7,44,C'LEN  CLI  CLIENT/PRODUCT NAME'                           
         SSPEC H8,44,C'---  ---  -------------------'                           
         SSPEC H7,86,C'USE DATE  PROGRAM NAME     NETWORK'                      
         SSPEC H8,86,C'--------  ------------     -------'                      
         DC    H'0'                                                             
         SPACE 2                                                                
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=TAPEEOF,        X        
               RECFM=FB,LRECL=165                                               
         SPACE 2                                                                
         DS    0D                                                               
         DC    CL8'**PBUFF*'                                                    
POSTBUFF DC    4500X'00'                                                        
         EJECT                                                                  
*              MY WORKING STORAGE                                               
         SPACE 1                                                                
TNGD     DSECT                                                                  
PRNTBL   DS    F                   A(PRNTBL)                                    
*                                                                               
TRACOPT  DS    CL1                 Y=TRACE                                      
WRKOPT   DS    CL1                 N=DON'T GENERATE WORKER FILE                 
OPENWK   DS    CL1                 Y=WORKER FILE OPENED                         
AGYNM    DS    CL36                TALENT AGENCY NAME                           
*                                                                               
WID      DS    CL16                WORKER FILE ID                               
TAPEREC  DS    CL(TAPERLNQ)        INPUT RECORD                                 
WREC     DS    CL1024              WORKER FILE RECORD                           
TNGDX    EQU   *                                                                
         SPACE 2                                                                
*              DSECT TO COVER INPUT RECORD                                      
         SPACE 1                                                                
TAPED    DSECT                                                                  
TAPAGY   DS    CL4                 AGENCY CODE                                  
TAPCLI   DS    CL3                 CLIENT CODE                                  
TAPCLIN  DS    CL30                CLIENT NAME                                  
TAPPRDN  DS    CL30                PRODUCT NAME                                 
TAPNID   DS    CL8                 CID                                          
TAPTTL   DS    CL30                COMMERCIAL TITLE                             
TAPSEC   DS    CL3                 LENGTH IN SECONDS                            
         DS    XL1                 IGNORED                                      
TAPUSEDT DS    CL6                 USE DATE YYMMDD                              
TAPPGM   DS    CL25                PROGRAM NAME                                 
TAPNWK   DS    CL5                 NETWORK                                      
         DS    CL1                 IGNORED                                      
         DS    CL6                 IGNORED                                      
         DS    CL2                 IGNORED                                      
         DS    CL6                 IGNORED                                      
         DS    CL1                 IGNORED                                      
         DS    CL4                 IGNORED                                      
TAPERLNQ EQU   *-TAPAGY                                                         
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PCID     DS    CL8                                                              
         DS    CL2                                                              
PTTL     DS    CL30                                                             
         DS    CL2                                                              
PSEC     DS    CL3                                                              
         DS    CL2                                                              
PCLI     DS    CL3                                                              
         DS    CL2                                                              
PCLIN    DS    CL30                                                             
         DS    CL2                                                              
PUSEDATE DS    CL8                                                              
         DS    CL2                                                              
PPGMNAME DS    CL15                                                             
         DS    CL2                                                              
         DS    CL3                                                              
PNETWORK DS    CL1                                                              
         DS    CL3                                                              
         ORG   PLINED+132                                                       
         DS    CL53                                                             
PPRDN    DS    CL30                                                             
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         SPACE 2                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPE2D                                                       
         EJECT                                                                  
         SPACE 1                                                                
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDGENTWA (MUST FOLLOW LAST SCREEN)                                             
*TAREPWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDTWADCOND                                                                     
*DDBIGBOX                                                                       
*DMWRKRK                                                                        
*DMWRKRD                                                                        
*DDMASTD                                                                        
*TASYSEQUS                                                                      
*TAGENFILE                                                                      
*TASYSDSECT                                                                     
*TAREPFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTWADCOND                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAREPWORKD                                                     
         SPACE 1                                                                
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'143TAREP49   05/01/02'                                      
         END                                                                    
