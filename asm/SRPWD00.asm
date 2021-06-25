*          DATA SET SRPWD00    AT LEVEL 001 AS OF 12/03/13                      
*PHASE T1FD00A                                                                  
         TITLE '$PWD - RE-VALIDATE A USER PASSWORD'                             
***********************************************************************         
* THIS PROGRAM ENTERED WHEN A TERMINAL LOGGED ON WITH A PASSWORD      *         
* COMES THROUGH THE VTAM LOGON XIT. TSTAT5=TST5PSWD IS ON.            *         
* MSGQIN DIRECTS INPUT TO HERE AFTER RESETTING TST5PSWD AND TSTATNIT. *         
* ON EXIT TST5PSWD IS ON IF USER HAS FAILED ELSE SET TO =RE OR =DONE  *         
***********************************************************************         
         PRINT NOGEN                                                            
PSWD     CSECT                                                                  
         NMOD1 SRWORKX-SRWORKD,**$PWD**                                         
         USING SRWORKD,RC                                                       
         LR    R7,R1               R7=A(S/R PARAM LIST)                         
         USING SRPARMD,R7                                                       
         L     RA,SRQATWA          RA=A(TWA)                                    
         USING SRPWDFFD,RA                                                      
         L     R8,SRQATIA          R8=A(TIA)                                    
         L     R9,SRQASYSF         R9=A(SYSFACS)                                
         USING SYSFACD,R9                                                       
         L     R3,SRQAUTL          R3=A(UTL ENTRY)                              
         USING UTLD,R3                                                          
*                                                                               
PSWD1    XC    SRVSREQ,SRVSREQ     CLEAR S/R FIELDS                             
         XC    SRVP2,SRVP2                                                      
         XC    SRVP3,SRVP3                                                      
         XC    SRVP4,SRVP4                                                      
*                                                                               
PSWD2    L     RE,TBUFF            TRY TO STOP REQUEUE BACK TO S/R              
         AHI   RE,-8                                                            
         XC    6(2,RE),6(RE)       SET INPUT LENGTH TO ZERO                     
*                                                                               
PSWD3    LLC   R0,TSTATSVC         BUMP VIOLATION COUNT                         
         AHI   R0,1                                                             
         STC   R0,TSTATSVC                                                      
         CHI   R0,TSTATRSM+1       TEST IF REACHED MAXIMUM LAST TIME            
         BNH   PSWD5               NO                                           
*                                                                               
PSWD4    NI    TSTAT5,255-TST5PSWD RESET CONTROLLING BITS                       
         NI    TSTAT2,255-TSTATNIT                                              
         MVI   TSTATSVC,0          SET VIOLATION COUNT TO ZERO                  
         MVC   TSVCREQ,$$DONE      DISCONNECT                                   
         B     EXIT                                                             
*                                                                               
PSWD5    CLI   SRVP1H+5,0          TEST IF A PASSWORD WAS ENTERED               
         BNE   VPW                                                              
         MVC   SRVP1(3),FIRST                                                   
         OI    SRVP1H+6,X'81'      TRANSMIT AND SET MODIFIED                    
         OI    TSTAT5,TST5PSWD     SET PASSWORD STILL REQUIRED                  
         OI    TSTAT2,TSTATNIT     AND TERMINAL NOT INITIALIZED                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PASSWORD WAS INPUT                                                  *         
***********************************************************************         
VPW      MVC   PWDINP,SRVP1        EXTRACT PASSWORD INPUT                       
         LA    R0,L'PWDINP                                                      
         LA    RE,PWDINP+L'PWDINP-1                                             
VPW1     CLI   0(RE),0             CONVERT TRAILING NULLS TO SPACES             
         BNE   VPW2                                                             
         MVI   0(RE),C' '                                                       
         AHI   RE,-1                                                            
         BCT   R0,VPW1                                                          
*                                                                               
VPW2     L     RF,VSSB             GET A(COUNTRY TRANSLATE TABLES)              
         L     RF,SSBXLAT-SSBD(RF)                                              
         LLC   R1,TCTRY                                                         
         CHI   R1,15                                                            
         BNH   *+6                                                              
         XR    R1,R1                                                            
         SLL   R1,4                                                             
         L     RF,8(RF,R1)         RF=A(COUNTRY LOWER TO UPPER)                 
         MVC   PWDINU,PWDINP                                                    
         TR    PWDINU,0(RF)        CONVERT TO UPPER CASE                        
*                                                                               
VPW3     NI    TSTAT5,255-TST5PSWD RESET CONTROLLING BITS                       
         NI    TSTAT2,255-TSTATNIT                                              
         CLI   SRVP1H+5,3          TEST IF INPUT IS WHAT THEY WERE FED          
         BNE   VPW4                                                             
         CLC   PWDINP(3),FIRST                                                  
         BNE   VPW4                                                             
         MVI   TSTATSVC,0          SET VIOLATION COUNT TO ZERO                  
         MVC   TSVCREQ,$$DONE      DISCONNECT                                   
         B     EXIT                                                             
*                                                                               
VPW4     TM    TSTAT1,TSTATDDS     TEST DDS TERMINAL                            
         BZ    VPW5                                                             
         CLC   PWDINU,=CL10'DDS'   DDS IS GOOD ENOUGH                           
         BE    *+14                                                             
         CLC   PWDINU,=CL10'MEDIAOCEAN'                                         
         BNE   VPW5                                                             
         MVI   TSTATSVC,0          SET VIOLATION COUNT TO ZERO                  
         NI    TSSVNUM,255-X'80'   SET NO LONGER ACTIVE                         
         MVC   TSVCREQ,$$RE        SET TO RECALL LAST SCREEN                    
         B     EXIT                                                             
*                                                                               
VPW5     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* READ SECRET CODE RECORD AND CHECK PASSWORD MATCHES                  *         
***********************************************************************         
VSEC     LA    R4,KEY              BUILD AUTH(NUMERIC) KEY                      
         USING CT0REC,R4                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,TAGYSEC                                                  
         OC    TAGYSEC,TAGYSEC                                                  
         BNZ   *+10                                                             
         MVC   CT0KAGY,TAGY                                                     
         MVC   CT0KNUM,TPASSWD                                                  
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,REC                      
         CLI   8(R1),0                                                          
         BNE   VSEC4                                                            
         CLC   KEY(L'CT0KEY),REC   TEST IF AUTH RECORD FOUND                    
         BNE   VSEC4                                                            
*                                                                               
VSEC1    LA    R4,CT0DATA-CT0REC+REC                                            
         SR    R1,R1                                                            
VSEC2    CLI   0(R4),0             FIND AUTH(ALPHA) ELEMENT                     
         BE    VSEC4                                                            
         CLC   0(2,R4),=X'030C'                                                 
         BE    VSEC3                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VSEC2                                                            
*                                                                               
VSEC3    CLC   2(10,R4),PWDINP     TEST SAME PASSWORD                           
         BE    VSEC4                                                            
         CLC   2(10,R4),PWDINU                                                  
         BNE   ERROR1                                                           
*                                                                               
VSEC4    MVI   TSTATSVC,0          SET VIOLATION COUNT TO ZERO                  
         NI    TSSVNUM,255-X'80'   SET NO LONGER ACTIVE                         
         MVC   TSVCREQ,$$RE        SET TO RECALL LAST SCREEN                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PASSWORD DOES NOT MATCH. TEST IF REACHED MAXIMUM ATTEMPTS.          *         
* IF EXCEEDS MAXIMUM THEN DISCONNECT TERMINAL FROM FACPAK SYSTEM.     *         
***********************************************************************         
ERROR1   MVC   SRVP1U,DASHES       UNDERLINE WHOLE PASSWORD INPUT FIELD         
         OI    SRVP1UH+6,X'80'                                                  
         LLC   RF,SRVP1H+5         GET LENGTH OF PASSWORD                       
         CHI   RF,3                                                             
         BNE   ERROR1B             NOT SAME LENGTH AS FIRST FIELD               
         MVC   FULL(3),PWDINP                                                   
         CLC   FULL+2(1),FIRST+2                                                
         BNE   ERROR1B                                                          
         MVI   FULL+2,0            THEY ONLY INPUT TWO CHRS                     
         LHI   RF,2                                                             
         CLC   FULL+1(1),FIRST+1                                                
         BNE   ERROR1A                                                          
         MVI   FULL+1,0            THEY ONLY INPUT ONE CHR                      
         LHI   RF,1                                                             
ERROR1A  MVC   SRVP1(3),FULL                                                    
         OI    SRVP1H+6,X'80'                                                   
ERROR1B  AHI   RF,-1                                                            
         BM    ERROR2                                                           
         EX    RF,*+8              SHOW HOW LONG THE INPUT WAS                  
         B     ERROR2                                                           
         MVC   SRVP1U(0),STARS                                                  
*                                                                               
ERROR2   OI    TSTAT5,TST5PSWD     SET PASSWORD STILL REQUIRED                  
         OI    TSTAT2,TSTATNIT     AND TERMINAL NOT INITIALIZED                 
*                                                                               
         LA    R0,128              WRONG PASSWORD                               
         XC    DMCB(24),DMCB                                                    
         L     RF,SRQACOMF                                                      
         L     RF,CGETTXT-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(R0)                                                   
*                                                                               
ERROR3   CLI   TSTATSVC,TSTATRSM+1 TEST IF THIS IS THE LAST ATTEMPT             
         BL    EXIT                                                             
         NI    TSTAT5,255-TST5PSWD RESET CONTROLLING BITS                       
         NI    TSTAT2,255-TSTATNIT                                              
         MVI   SECWHY,TSSVRGTM     MAX 3 ATTEMPTS ON PASSWORD (R=3)             
         BRAS  RE,LOGSEC           LOG SECURITY EVENT                           
         MVI   TSTATSVC,0          SET VIOLATION COUNT TO ZERO                  
         MVC   TSVCREQ,$$DONE      DISCONNECT                                   
*                                                                               
EXIT     NI    SRVSREQH+6,255-X'40'                                             
         OI    SRVP1H+6,X'40'      POSITION CURSOR TO PASSWORD FIELD            
         XMOD1 1                                                                
         EJECT                                                                  
$$CT     DC    X'0110'                                                          
$$DONE   DC    X'0112'                                                          
$$RE     DC    X'0120'                                                          
*                                                                               
FIRST    DC    C'<=>'                                                           
*                                                                               
SPACES   DC    16C' '                                                           
DASHES   DC    16C'-'                                                           
STARS    DC    16C'*'                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOG SECURITY VIOLATION DATA - SECWHY HAS THE REASON CODE            *         
***********************************************************************         
LOGSEC   NTR1  BASE=*,LABEL=*                                                   
         L     RE,VSSB             BUMP FACPAK SECURITY VIOLATION COUNT         
         LLC   R0,SSBSECV-SSBD(RE)                                              
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         BH    *+8                                                              
         STC   R0,SSBSECV-SSBD(RE)                                              
         MVC   TSSVNUM,SECWHY      SET SUSPECTED SECURITY VIOLATION NUM         
         OI    TSSVNUM,X'80'       SET CURRENTLY ACTIVE                         
         CHI   R0,255                                                           
         BH    LOGSX               ONLY LOG FIRST 255 VIOLATIONS                
*                                                                               
         SR    R0,R0               WRITE TWA TO TEMPSTR                         
         ICM   R0,3,TNUM                                                        
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,(R0)),(RA)                
*                                                                               
         LA    R4,WORK             R4=A($CT LOG RECORD)                         
         USING LOGRECD,R4                                                       
         MVC   LOGID,=CL4'$CT0'    IDENTIFIER FOR SECURITY VIOLATION            
         MVC   LOGID+3(1),SECWHY                                                
         OI    LOGID+3,C'0'                                                     
         MVC   LOGLUID,TLUID                                                    
         GOTO1 VTICTOC,DMCB,C'SGET'                                             
         MVC   LOGTIME,DMCB        TIME P'0HHMMSS+'                             
*                                                                               
         LA    RE,LOGTEXT          USER ID NUMBER                               
         MVC   0(16,RE),BLANKS                                                  
         SR    R0,R0                                                            
         ICM   R0,3,TUSER                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(4,RE),=C'UID='                                                 
         UNPK  4(6,RE),DUB+4(4)                                                 
*                                                                               
         LA    RE,LOGTEXT+16       SYSTEM NUMBER                                
         MVC   0(16,RE),BLANKS                                                  
         LLC   R0,TSYS                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(4,RE),=C'SYS='                                                 
         UNPK  4(3,RE),DUB+4(4)                                                 
*                                                                               
         LA    RE,LOGTEXT+32       PROGRAM NUMBER                               
         MVC   0(16,RE),BLANKS                                                  
         LLC   R0,TPRG                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   0(4,RE),=C'PRG='                                                 
         UNPK  4(3,RE),DUB+4(4)                                                 
*                                                                               
         MVC   LOGTEXT1,BLANKS                                                  
         MVC   LOGPWD,PWDINP       INPUT PASSWORD                               
*                                                                               
         L     RF,VSSB             FACPAK AOR/TOR ID                            
         MVC   LOGSYSIX,SSBSYSIX-SSBD(RF)                                       
         GOTO1 VLOGGER,LOGREC      OUTPUT LOGREC TO ADRFILE                     
*                                                                               
LOGSX    XIT1                                                                   
*                                                                               
BLANKS   DC    CL16' '                                                          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER LOG RECORD                                           *         
***********************************************************************         
LOGRECD  DSECT                     LOG RECORD FOR SECURITY VIOLATIONS           
LOGREC   DS    0CL80               CURRENT ADRFILE RECORD SIZE                  
LOGID    DS    CL4                 $CT1 IDENTIFIES A SRCON LOG REC              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    CL48                FREE FORM TEXT STARTS HERE                   
LOGTEXT1 DS    0CL15                                                            
LOGPWD   DS    CL10                INPUT PASSWORD                               
         DS    CL5                                                              
LOGSYSIX DS    XL1                 FACPAK AOR/TOR ID                            
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER WORKING STORAGE                                      *         
***********************************************************************         
SRWORKD  DSECT                                                                  
DMCB     DS    6F                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
SECWHY   DS    X                                                                
         DS    X                                                                
PWDINP   DS    CL10                                                             
PWDINU   DS    CL10                                                             
WORK     DS    CL64                                                             
KEY      DS    CL64                                                             
REC      DS    1000C                                                            
SRWORKX  DS    0C                                                               
                                                                                
*FADSECTS                                                                       
       ++INCLUDE FADSECTS                                                       
                                                                                
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
                                                                                
*CTGENFILE                                                                      
       ++INCLUDE CTGENFILE                                                      
                                                                                
SRPWDFFD DSECT                                                                  
         DS    CL64                                                             
*SRPWDFFD                                                                       
       ++INCLUDE SRPWDFFD                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SRPWD00   12/03/13'                                      
         END                                                                    
