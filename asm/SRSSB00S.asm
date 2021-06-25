*          DATA SET SRSSB00S   AT LEVEL 023 AS OF 05/01/02                      
*PHASE T13000A                                                                  
         PRINT NOGEN                                                            
         TITLE '$SSB - DISPLAY SYSTEM STATUS'                                   
SSB      CSECT                                                                  
         NMOD1 WRKL,**$SSB**,R7,CLEAR=YES                                       
         USING WRKD,RC             RC=A(W/S)                                    
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRQATWA                                                       
         USING SRSSBFFD,RA         RA=A(TWA)                                    
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
*                                                                               
         L     RE,SRQACOMF         SAVE VARIOUS ENTRYS                          
         USING COMFACSD,RE                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         L     RE,SRQASEL                                                       
         USING SELISTD,RE                                                       
         L     RE,SEFILES                                                       
         MVC   VFILETAB,0(RE)                                                   
         DROP  RE                                                               
         SPACE 1                                                                
         MVC   OPFACID+0(8),=C'+FACPAK+'                                        
         MVC   OPFACID+4(3),SSBSYSNA                                            
         MVC   OPSSB,=C'=SSB'                                                   
         LA    R5,SRVMSG           SET HEAD LINE                                
         MVC   SRVMSG,SPACES                                                    
         MVC   0(23,R5),=C'Facpak#n (XXX) SSB data'                             
         EDIT  SSBSYSID,(2,7(R5)),ALIGN=LEFT                                    
         MVC   10(3,R5),SSBSYSNA   SET FACPAK NAME                              
         LA    R5,24(R5)                                                        
         MVC   0(8,R5),SSBDATE                                                  
         LA    R5,9(R5)                                                         
         CLI   SSBVTID,C' '        SET VTAM LUID                                
         BNH   SRVAL                                                            
         MVC   0(17,R5),=C'vtam application '                                   
         MVC   17(8,R5),SSBVTID                                                 
         EJECT                                                                  
SRVAL    CLI   SRVSRV+4,C','       TEST IF =SSB,X...                            
         BNE   SRVALX                                                           
         CLI   SRVSRV+5,C'D'       =SSB,D TO RETURN THE DUMP SCREEN             
         BNE   SRVALX                                                           
         MVC   WORK(17),SRVSRV                                                  
         XC    DMCB(24),DMCB       LOAD VIRGIN ABEND SCREEN                     
         LA    RE,64(RA)                                                        
         ST    RE,DMCB                                                          
         MVI   DMCB+4,C'R'                                                      
         MVC   DMCB+5(3),$ABEND                                                 
         GOTO1 VCALLOV,DMCB                                                     
         MVC   SRVSRV,WORK                                                      
         B     EXIT                                                             
SRVALX   EQU   *                                                                
         EJECT                                                                  
P1VAL    LA    R4,SRVP1H           P1 CAN BE GO/STOP FOR INHIBIT INPUT          
         CLI   5(R4),0                                                          
         BE    P1VX                                                             
         CLC   8(2,R4),=C'GO  '                                                 
         BNE   P1V010                                                           
         NI    SSBSTAT1,255-SSBUII                                              
         MVC   OPCMND,=CL4'GO  '                                                
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V010   CLC   8(4,R4),=C'STOP'                                                 
         BNE   P1V020                                                           
         OI    SSBSTAT1,SSBUII                                                  
         MVC   OPCMND,=CL4'STOP'                                                
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V020   CLC   8(10,R4),=C'FACWRKUPD='                                          
         BNE   P1V040                                                           
         CLC   18(2,R4),=C'ON'                                                  
         BNE   P1V030              ENABLE UPDATES FROM FACWRK FILES             
         NI    SSBJFLAG,X'FF'-SSBJFNUP                                          
         MVC   OPCMND,=C'FWON'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V030   CLC   18(3,R4),=C'OFF'                                                 
         BNE   P1VX                                                             
         OI    SSBJFLAG,SSBJFNUP   DISABLE UPDATES FROM FACWRK FILES            
         MVC   OPCMND,=C'FWOF'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V040   CLC   8(3,R4),=C'POP'                                                  
         BNE   P1V050                                                           
         OI    SSBT2,X'80'         FORCE GLOBAL TIMER POP                       
         B     P1VX                                                             
*                                                                               
P1V050   CLC   8(8,R4),=C'LUNATIC='                                             
         BNE   P1V070                                                           
         CLC   16(2,R4),=C'ON'                                                  
         BNE   P1V060              ENABLE LUNATIC TRANMISSIONS                  
         NI    SSBJFLAG,X'FF'-SSBJFNLU                                          
         MVC   OPCMND,=C'LUON'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V060   CLC   16(3,R4),=C'OFF'                                                 
         BNE   P1VX                                                             
         OI    SSBJFLAG,SSBJFNLU   DISABLE LUNATIC TRANSMISSIONS                
         MVC   OPCMND,=C'LUOF'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V070   CLC   8(5,R4),=C'SOON='   SOON=ON/OFF                                  
         BNE   P1V090                                                           
         CLC   13(2,R4),=C'ON'                                                  
         BNE   P1V080              ENABLE SOON SUBMISSIONS                      
         NI    SSBJFLG2,X'FF'-SSBJFSN                                           
         MVC   OPCMND,=C'SNON'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V080   CLC   13(3,R4),=C'OFF'                                                 
         BNE   P1VX                                                             
         OI    SSBJFLG2,SSBJFSN    DISABLE SOON SUBMISSIONS                     
         MVC   OPCMND,=C'SNOF'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V090   CLC   8(8,R4),=C'BACKSOON' RESTORE SOON SUBMISSION                     
         BNE   P1V100                                                           
         L     R2,SSBAATC          GET A(ATTACHED TASK CONTROL)                 
         USING ATCD,R2                                                          
         LTR   R2,R2               IGNORE IF MONSOON IS IN CONTROL              
         BZ    P1VX                                                             
         CLI   ATCSTAT1,ATCSATCH+ATCSSACT                                       
         BNE   P1VX                MUST BE ATTACHED+SYNC ACTIVE                 
         OC    ATCATCB,ATCATCB                                                  
         BNZ   P1VX                MUST HAVE NO OWNER TCB                       
         MVI   ATCSTAT1,ATCSATCH   OK RESET STAT1 TO ATTACHED (X'80')           
         DROP  R2                                                               
*                                                                               
P1V100   CLC   8(4,R4),=C'PAY='    PAY=Y/N                                      
         BNE   P1V110                                                           
         CLI   12(R4),C'Y'                                                      
         BNE   P1V100A                                                          
         NI    SSBSTAT3,X'FF'-SSBNOPAY                                          
         MVC   OPCMND,=C'PAYY'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V100A  CLI   12(R4),C'N'                                                      
         BNE   P1VX                                                             
         OI    SSBSTAT3,SSBNOPAY                                                
         MVC   OPCMND,=C'PAYN'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V110   CLC   8(9,R4),=C'DUPDUMPS=' DUPDUMPS=Y/N                               
         BNE   P1V120                                                           
         CLI   17(R4),C'Y'                                                      
         BNE   P1V110A                                                          
         OI    SSBSTAT3,SSBDUPDP                                                
         MVC   OPCMND,=C'DUPY'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
P1V110A  CLI   17(R4),C'N'                                                      
         BNE   P1VX                                                             
         NI    SSBSTAT3,X'FF'-SSBDUPDP                                          
         MVC   OPCMND,=C'DUPN'                                                  
         BAS   RE,WTO                                                           
         B     P1VX                                                             
*                                                                               
P1V120   EQU   *                                                                
*                                                                               
P1VX     EQU   *                                                                
         EJECT                                                                  
LNA      LA    R5,SRVL4                                                         
         USING LINEA,R5                                                         
         MVC   IIH,=C'GO  '        USER INPUT=GO/STOP                           
         TM    SSBSTAT1,SSBUII                                                  
         BZ    *+10                                                             
         MVC   IIH,=C'STOP'                                                     
*                                                                               
LNA2     MVC   FSTRT,MSGSTRT       START                                        
         SR    R0,R0                                                            
         L     R1,SSBSTIME         R1=START TIME IN TU'S (1/100 SEC)            
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+0(2),DUB                                                    
         MVI   WORK+2,C'.'                                                      
         MVI   WORK+5,C'.'                                                      
         MVC   FSTRTTIM(8),WORK                                                 
*                                                                               
LNA3     SR    R0,R0                                                            
         ICM   R1,15,SSBRTIME      R1=RESTART TIME IN TU'S (1/100 SEC)          
         BZ    LNA3X                                                            
         MVC   FRSTR(16),MSGRSTRT  RESTART                                      
         D     R0,=F'100'                                                       
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+0(2),DUB                                                    
         MVI   WORK+2,C'.'                                                      
         MVI   WORK+5,C'.'                                                      
         MVC   FRSTRTIM(8),WORK                                                 
LNA3X    EQU   *                                                                
*                                                                               
LNA4     SR    R0,R0                                                            
         ICM   R0,1,SSBRCNT                                                     
         BZ    LNA4X                                                            
         MVC   FDOWN(16),MSGDOWN                                                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FDOWN+5(2),DUB                                                   
         SR    R0,R0                                                            
         ICM   R1,3,SSBDTIME        R1=DOWN TIME IN SECONDS                     
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+6(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+3(2),DUB                                                    
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+0(2),DUB                                                    
         MVI   WORK+2,C'.'                                                      
         MVI   WORK+5,C'.'                                                      
         MVC   FDOWNTIM(8),WORK                                                 
LNA4X    EQU   *                                                                
         SPACE 2                                                                
LNB      LA    R5,SRVL6                                                         
         USING LINEB,R5                                                         
         EDIT  SSBSIN,(8,SINO),ALIGN=LEFT                                       
         CLI   SSBJESIO,C' '       IS MONSOON RUNNING?                          
         BNE   LNB0                                                             
         MVC   LJOBTXT1,MSGMONS    YES                                          
         MVC   LJOBNAME(13),MSGSOONE                                            
         TM    SSBJFLG2,SSBJFSN    ARE SOON REQUESTS DISABLED?                  
         BZ    *+10                                                             
         MVC   LJOBNAME(14),MSGSOOND                                            
         B     LNBX                                                             
         SPACE 2                                                                
LNB0     OC    SSBJESNO,SSBJESNO   TEST JES JOB NUM                             
         BNZ   LNB1                                                             
         MVC   LJOBTXT1,MSGNOJ     NO JOBS SUBMITTED                            
         MVC   LJOBNAME,SPACES                                                  
         B     LNBX                                                             
LNB1     MVC   LJOBNAME,SSBMVSID   MVS JOB ID & JES NUMBER                      
         MVC   LJOBTXT2,=C' (J'                                                 
         EDIT  SSBJESNO,(5,LJOBNO),ALIGN=LEFT                                   
         LA    R1,LJOBNO+L'LJOBNO-1                                             
LNB2     CLI   0(R1),C' '          CLOSE BRACKETS AFTER JES NO                  
         BNE   LNB3                                                             
         BCT   R1,LNB2                                                          
LNB3     MVI   1(R1),C')'                                                       
LNBX     EDIT  SSBTASKS,(2,TASKCT),ALIGN=LEFT                                   
         SPACE 2                                                                
LNC      LA    R5,SRVL8                                                         
         USING LINEC,R5                                                         
*                                                                               
         STAR  CLEAR=ARZERO                                                     
         LAM   R2,R2,SSBTBLET                                                   
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,TABSDUMP-FATABSD(R2)                                       
         USING TORDUMPD,R2                                                      
         LA    R2,TDLENQ(,R2)      FIRST SLOT EMPTY                             
         L     RE,VSSB                                                          
LNC1     OC    TDSYSNA,TDSYSNA                                                  
         BZ    LNC1A                                                            
         CLC   TDSYSNA,SSBSYSN4-SSBD(RE)                                        
         BE    LNC1B                                                            
         LA    R2,TDLENQ(,R2)                                                   
         B     LNC1                                                             
*                                                                               
LNC1A    XR    R2,R2               DEFAULT TO FIRST IN LIST                     
         ICM   R2,15,TABSDUMP-FATABSD(R2)                                       
         LA    R2,TDLENQ(,R2)      FIRST SLOT EMPTY                             
*                                                                               
LNC1B    ICM   R0,15,TDDUMPMX                                                   
         ICM   R1,15,TDDUMPFS                                                   
         AH    R0,=H'1'            NUMBERS ARE 1'S BASED                        
         SR    R0,R1                                                            
         EDIT  (R0),(2,MAXDMP),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         ICM   R0,15,TDDUMPNO                                                   
         EDIT  (R0),(2,CURDMP),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
         MVC   FULL,TDDUMPTM                                                    
         REAR  ARS=OFF                                                          
*                                                                               
LNC2     OI    FULL+3,X'0F'                                                     
         MVC   WORK(10),=X'402021214B21214B2121'                                
         CP    FULL,=P'0'                                                       
         BE    *+16                                                             
         ED    WORK(10),FULL                                                    
         MVC   CURDMPTM,WORK+2                                                  
*                                                                               
         MVC   FULL,SSBT1          LOOP TIMER                                   
         NI    FULL,X'7F'          RESET EXPIRATION BIT                         
         EDIT  FULL,(8,LOOPTMR),ALIGN=LEFT                                      
         TM    SSBT1,X'80'         IS THE TIMER ABOUT TO POP?                   
         BZ    *+16                                                             
         LA    RE,LOOPTMR          YES                                          
         AR    RE,R0                                                            
         MVC   0(4,RE),=C'*POP'                                                 
*                                                                               
         MVC   FULL,SSBT2          GLOBAL TIMER                                 
         NI    FULL,X'7F'          RESET EXPIRATION BIT                         
         EDIT  FULL,(8,GLOBTMR),ALIGN=LEFT                                      
         TM    SSBT2,X'80'         IS THE TIMER ABOUT TO POP?                   
         BZ    *+16                                                             
         LA    RE,GLOBTMR          YES                                          
         AR    RE,R0                                                            
         MVC   0(4,RE),=C'*POP'                                                 
         SPACE 2                                                                
LND      LA    R5,SRVL10                                                        
         USING LINED,R5                                                         
*                                                                               
         LA    R2,SSBHIADR         HI CORE                                      
         LA    R3,HICORE                                                        
         LA    R4,4                                                             
         BAS   RE,ADDRCONV                                                      
*                                                                               
         SR    R0,R0               REGION                                       
         L     R1,SSBHIADR                                                      
         AH    R1,=H'1'                                                         
         S     R1,SSBLOADR                                                      
         D     R0,=F'1024'                                                      
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         AH    R1,=H'1'                                                         
         LR    R0,R1                                                            
         EDIT  (R0),(7,REGION),ALIGN=LEFT                                       
         LA    R1,REGION                                                        
         AR    R1,R0                                                            
         MVI   0(R1),C'K'                                                       
*                                                                               
         MVC   FULL,SSBTPOPT                                                    
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),FULL                                                    
         OC    WORK+2(8),=C'00.00.00'                                           
         MVC   LASTPOP,WORK+2                                                   
*                                                                               
         GOTO1 VTICTOC,FULL,C'SGET'                                             
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),FULL                                                    
         OC    WORK+2(8),=C'00.00.00'                                           
         MVC   TIMENOW,WORK+2                                                   
         SPACE 2                                                                
LNE      LA    R5,SRVL12           TEMPSTR INFO                                 
         USING LINEE,R5                                                         
*                                                                               
LNE1     CLI   SSBVTID,C' '        MAX NUM ENTRIES IN UTL                       
         BH    *+12                                                             
         LH    R0,SSBTRMS          FOR BTAM MAX IS SAME AS ACTUAL               
         B     LNE1A                                                            
         L     RE,VUTL             FOR VTAM IS AT VUTL-2(2)                     
         SH    RE,=H'2'                                                         
         LH    R0,0(RE)                                                         
LNE1A    EDIT  (R0),(5,MAXUTL),ALIGN=LEFT                                       
*                                                                               
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,IOA                        
         EDIT  (2,IOA+6),(5,MAXTMPR),ALIGN=LEFT                                 
*                                                                               
         EDIT  (2,SSBTRMS),(5,ACTTMPR),ALIGN=LEFT                               
*                                                                               
         OC    SSBSSMAX,SSBSSMAX                                                
         BZ    LNF                                                              
         MVC   MAXSESS,SSBSSMAX+1                                               
         OI    MAXSESS,C'0'                                                     
         SPACE 2                                                                
LNF      LA    R5,SRVL14           TEMPEST INFO                                 
         USING LINEF,R5                                                         
         XC    IOA,IOA                                                          
         MVC   DMCB+8(4),=X'0000FFFF'                                           
         GOTO1 VDATAMGR,DMCB,=C'DMREAD',=C'TEMPEST',,IOA                        
         PUSH  USING                                                            
         USING TMSHDR,IOA                                                       
         EDIT  (B2,TMSMAX),(5,MAXTMPT),ALIGN=LEFT                               
         EDIT  (B4,TMSMAXCI),(5,MAXTMPS),ALIGN=LEFT                             
         EDIT  (B4,TMSUSECI),(5,ACTTMPS),ALIGN=LEFT,ZERO=NOBLANK                
         POP   USING                                                            
         LA    R4,TSARBUFF                                                      
         TM    SSBTSAR,X'80'       TEST 2 BUFFERS                               
         BZ    *+14                                                             
         MVC   0(2,R4),=C'2*'                                                   
         LA    R4,2(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,7,SSBTSAR+1                                                   
         EDIT  (R0),(7,(R4)),ALIGN=LEFT                                         
         SPACE 2                                                                
         LA    R5,SRVL16                                                        
         USING LINE16,R5                                                        
         OC    SSBVTHNG,SSBVTHNG   TEST ANY VTAM HANGS                          
         BZ    LNF1                                                             
*                                                                               
         ICM   R0,15,SSBVTHNG                                                   
         SRL   R0,4                                                             
         ST    R0,FULL                                                          
         MVC   WORK(10),=X'402020204B20204B2020'                                
         ED    WORK(10),FULL                                                    
         OC    WORK+2(8),=C'00.00.00'                                           
         MVC   LASTHNG,WORK+2                                                   
*                                                                               
LNF1     MVC   FACWKUPD,=C'ON '    FACWRK UPDATES ENABLED/DISABLED              
         TM    SSBJFLAG,SSBJFNUP                                                
         BZ    *+10                                                             
         MVC   FACWKUPD,=C'OFF'                                                 
*                                                                               
         ICM   RF,15,SSBPQXPE                                                   
         BZ    LNF17                                                            
         MVC   FULL,08(RF)         FULL=NUM OF PART1 INDEX SEARCHES             
         MVC   FUL1,12(RF)         FUL1=NUM OF PART1 INDEX I/OS                 
         EDIT  (4,FULL),(5,PQNEW),ALIGN=LEFT                                    
         SR    R0,R0                                                            
         L     R1,FUL1                                                          
         MH    R1,=H'200'                                                       
         ICM   RF,15,FULL                                                       
         BZ    LNF17                                                            
         DR    R0,RF               R1=AVG NUM OF INDEX I/OS                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,FUL1                                                          
         EDIT  (4,FUL1),(5,PQAVG),2,ALIGN=LEFT                                  
*                                                                               
LNF17    LA    R5,SRVL17                                                        
         USING LINE17,R5                                                        
         MVC   FACPROT,=C'OFF'     FACWRK MEMORY PROTECTION ON/OFF              
         TM    SSBPROT,SSBPONQ                                                  
         BZ    *+10                                                             
         MVC   FACPROT,=C'ON '                                                  
*                                                                               
         MVC   FACTOR(3),=C'AOR'   TEST AOR OR TOR                              
         TM    SSBSTAT4,SSBSAOR                                                 
         BO    LNF18                                                            
         MVC   FACTOR(3),=C'TOR'   IF TOR SHOW ATTACHED AOR COUNT               
*                                                                               
         LAM   R2,R2,SSBALET                                                    
         ICM   R2,15,SSBATOR                                                    
         LA    R2,TORFACLQ(,R2)    INDEX TO FACPAK EXCHANGE GRID                
         SAC   512                                                              
         USING SBEXCHD,R2                                                       
         LH    R4,0(,R2)                                                        
         L     R5,2(,R2)                                                        
         LA    R2,6(,R2)                                                        
         AR    R2,R4               FIRST ENTRY IS ALWAYS TOR                    
*                                                                               
LNF17A   OC    SBSTOKEN,SBSTOKEN   AOR IN SLOT?                                 
         BZ    LNF17B              NO                                           
         CLI   SBAVLBL,SBYES       AOR WORKING?                                 
         BNE   LNF17B              NO                                           
*                                                                               
         LH    RF,HALF             INCREMENT NUMBER OF AORS RUNNING             
         LA    RF,1(RF)                                                         
         STH   RF,HALF                                                          
*                                                                               
LNF17B   BXLE  R2,R4,LNF17A                                                     
         SAC   0                                                                
         LAM   R2,R2,=F'0'                                                      
*                                                                               
         OC    HALF,HALF                                                        
         BZ    LNF18               NO AORS                                      
         LA    R5,SRVL17                                                        
         LA    R5,FACTOR+3                                                      
         MVI   0(R5),C'+'                                                       
         LA    R5,1(R5)            OUTPUT COUNT OF AORS                         
*                                                                               
         EDIT  (2,HALF),(3,0(R5)),ALIGN=LEFT                                    
         AR    R5,R0                                                            
         MVC   0(4,R5),=CL4'*AOR'                                               
         CLC   HALF,=H'1'                                                       
         BNH   *+8                                                              
         MVI   4(R5),C'S'                                                       
*                                                                               
LNF18    LA    R5,SRVL18                                                        
         USING LINE18,R5                                                        
         L     R1,X'10'            POINT TO CPU ID                              
         L     R1,X'C4'(R1)                                                     
         LA    R1,X'10'(R1)        A(FOUR CHARACTER CPU ID)                     
         MVC   SMFID,0(R1)                                                      
*                                                                               
         LA    R2,SSBCTBUF         CTFILE PROFILE RECORD BUFFER                 
         OC    0(4,R2),0(R2)       TEST ADDRESS PRESENT                         
         BZ    LNF19                                                            
         LA    R3,URECADDR                                                      
         LA    R4,4                                                             
         BAS   RE,ADDRCONV                                                      
*                                                                               
         EDIT  (4,SSBCTACT),(7,URECLEN),ALIGN=LEFT                              
*                                                                               
         TM    SSBSTAT3,SSBNOPAY                                                
         BZ    LNF19                                                            
         MVC   MEDPAY,MSGMPAY                                                   
*                                                                               
LNF19    LA    R5,SRVL19                                                        
         USING LINE19,R5                                                        
         TM    SSBSTAT3,SSBDUPDP                                                
         BZ    *+10                                                             
         MVC   DUPDUMP,MSGDUPDY                                                 
         MVC   PIND,NO             OWNER PRGMS=YES/NO                           
         CLI   SSBPGMUP,C'Y'                                                    
         BNE   *+10                                                             
         MVC   PIND,YES                                                         
         SR    R0,R0                                                            
         ICM   R0,3,SSBVARIO                                                    
         BNZ   *+8                                                              
         ICM   R0,3,SSBMAXIO                                                    
         EDIT  (R0),(5,MAXIOCT),ALIGN=LEFT                                      
         LH    R0,SSBGFREQ         GET REQUESTED COUNT                          
         SH    R0,SSBGFUSD         LESS USED COUNT                              
         EDIT  (R0),(5,PGFRECS),ALIGN=LEFT                                      
*                                                                               
LNF20    LA    R2,SRVL20           START LINE FOR ENQUEUE INFO                  
         LA    R0,SRVL21-SRVL20                                                 
         ST    R0,LENQ                                                          
         ST    R2,AENQA                                                         
         AR    R2,R0                                                            
         ST    R2,AENQB                                                         
         AR    R2,R0                                                            
         ST    R2,AENQC                                                         
         AR    R2,R0                                                            
         ST    R2,AENQD                                                         
         SPACE 2                                                                
ENQA     OC    SSBCTTSK,SSBCTTSK   CONTROL SYSTEM ENQUEUE STATUS                
         BZ    ENQAX                                                            
         L     R2,AENQA                                                         
         XC    0(20,R2),0(R2)                                                   
ENQA1    MVC   0(7,R2),=C'CONTRL=' INSERT FILE NAME                             
         LA    R2,7(R2)                                                         
         EDIT  (1,SSBCTTSK),(2,(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R2,R0                                                            
         OC    SSBCTTSK+1(3),SSBCTTSK+1                                         
         BZ    ENQAX                                                            
         MVC   0(7,R2),=C'(task )'                                              
         SR    R6,R6                                                            
         ICM   R6,7,SSBCTTSK+1     SET HOLDING TASK ID                          
         MVC   5(1,R2),6(R6)                                                    
ENQAX    EQU   *                                                                
         SPACE 2                                                                
ENQB     OC    SSBWKTSK,SSBWKTSK   WKFILE ENQUEUE STATUS                        
         BZ    ENQBX                                                            
         L     R2,AENQB                                                         
         XC    0(20,R2),0(R2)                                                   
ENQB1    MVC   0(7,R2),=C'WKFILE=' INSERT FILE NAME                             
         LA    R2,7(R2)                                                         
         EDIT  (1,SSBWKTSK),(2,(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R2,R0                                                            
         OC    SSBWKTSK+1(3),SSBWKTSK+1                                         
         BZ    ENQBX                                                            
         MVC   0(7,R2),=C'(task )'                                              
         SR    R6,R6                                                            
         ICM   R6,7,SSBWKTSK+1     SET HOLDING TASK ID                          
         MVC   5(1,R2),6(R6)                                                    
ENQBX    EQU   *                                                                
         SPACE 2                                                                
ENQC     OC    SSBFWTSK,SSBFWTSK   FACWRK ENQUEUE STATUS                        
         BZ    ENQCX                                                            
         L     R2,AENQC                                                         
         XC    0(20,R2),0(R2)                                                   
         MVC   0(7,R2),=C'FACWRK='                                              
         LA    R2,7(R2)                                                         
         EDIT  (1,SSBFWTSK),(2,(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R2,R0                                                            
         OC    SSBFWTSK+1(3),SSBFWTSK+1                                         
         BZ    ENQCX                                                            
         MVC   0(7,R2),=C'(task )'                                              
         SR    R6,R6                                                            
         ICM   R6,7,SSBFWTSK+1     SET HOLDING TASK ID                          
         MVC   5(1,R2),6(R6)                                                    
ENQCX    EQU   *                                                                
         SPACE 2                                                                
ENQD     OC    SSBPQINF(32),SSBPQINF PRTQUE ENQUEUE STATUS                      
         BZ    ENQDX                                                            
         L     R2,AENQD                                                         
         XC    0(20,R2),0(R2)                                                   
ENQD1    ICM   RE,15,VFILETAB      POINT TO SYSTEM FILES IN DMGR                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'01'         FIRST ENTRY MUST BE SERVICE SYSTEM           
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    R0,2(RE)            SET NUMBER OF FILES IN LIST                  
         LA    RE,4(RE)            POINT TO FIRST FILE IN LIST                  
         LA    R5,PRTQDTFL                                                      
ENQD2    TM    0(RE),X'10'         TEST FOR PRTQ FILE                           
         BZ    *+14                                                             
         MVC   0(4,R5),4(RE)       SAVE PRTQ FILE DTF ADR                       
         LA    R5,4(R5)                                                         
         LA    RE,8(RE)            BUMP TO NEXT FILE IN LIST                    
         BCT   R0,ENQD2                                                         
*                                                                               
ENQD3    LA    R3,4                SET MAX NUM ENTRIES PER LINE                 
         LA    R4,SSBPQINF         ENQUEUE DETAILS                              
         LA    R5,PRTQDTFL         DTF ADDR TABLE                               
         SR    R6,R6                                                            
         MVI   COMMA,C' '                                                       
*                                                                               
ENQD5    OC    0(2,R4),0(R4)       TEST IF THIS PRTQ FILE INUSE                 
         BZ    ENQD9                                                            
         CLI   COMMA,C' '          SKIP OUT COMMA IF FIRST ON LINE              
         BE    ENQD6                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
ENQD6    MVI   COMMA,C','                                                       
         ICM   R6,7,1(R5)          GET FILE NAME FROM DTF                       
         MVC   0(7,R2),22(R6)                                                   
         LA    R2,6(R2)                                                         
ENQD7    CLI   0(R2),C' '          SHUFFLE OUT TRAILING BLANKS                  
         BNE   *+8                                                              
         BCT   R2,ENQD7                                                         
         MVI   1(R2),C'='                                                       
         LA    R2,2(R2)                                                         
         EDIT  (1,0(R4)),(2,(R2)),ALIGN=LEFT,ZERO=NOBLANK                       
         AR    R2,R0                                                            
         CLI   1(R4),0             CHECK IF TCB ID PRESENT                      
         BE    ENQD8                                                            
         MVC   0(7,R2),=C'(task )'                                              
         MVC   5(1,R2),1(R4)       SET HOLDING TASK ID                          
         LA    R2,7(R2)                                                         
ENQD8    BCT   R3,ENQD9            MAX NUM ENTRIES PER LINE                     
         L     R2,AENQD                                                         
         A     R2,LENQ             BUMP TO NEXT LINE                            
         LA    R3,4                                                             
         MVI   COMMA,C' '                                                       
ENQD9    LA    R4,2(R4)            BUMP TO NEXT PRTQ DTF                        
         LA    R5,4(R5)                                                         
         LA    R0,PRTQDTFX                                                      
         CR    R5,R0                                                            
         BL    ENQD5               BACK FOR NEXT PRTQ ENTRY                     
ENQDX    EQU   *                                                                
         SPACE 2                                                                
ENQE     B     EXIT                                                             
         EJECT                                                                  
******************************************************                          
*        WRITE TO OPERATOR                           *                          
******************************************************                          
         SPACE 1                                                                
WTO      NTR1                                                                   
         OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,DMCB,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         XIT1                                                                   
         EJECT                                                                  
* SUBROUTINE ADDRCONV - EDITS F'WORD INTO 6 BYTES                               
* R2=A(F'WORD+1), R3=A(O/P), R4=LENTH, RE=RETURN ADDR                           
*                                                                               
ADDRCONV DS    0H'0'                                                            
         ST    RE,SAVERE                                                        
         GOTO1 VHEXOUT,DMCB,(R2),(R3),(R4)                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
ARZERO   DC    16F'0'                                                           
SPACES   DC    CL80' '                                                          
$ABEND   DC    X'01FFFF'                                                        
NO       DC    C'no '                                                           
YES      DC    C'yes'                                                           
MSGSTRT  DC    C'Started'                                                       
MSGRSTRT DC    C'Restart         '                                              
MSGDOWN  DC    C'Down#00         '                                              
MSGNQW   DC    C'WKFILE not enqueued'                                           
MSGNQF   DC    C'FACWRK not enqueued'                                           
MSGNQP   DC    C'PRTQUE not enqueued'                                           
MSGNOJ   DC    C'No jobs submitted    '                                         
MSGMONS  DC    C'MONSOON running      '                                         
MSGSOONE DC    C'SOONs enabled'                                                 
MSGSOOND DC    C'SOONS disabled'                                                
MSGMPAY  DC    C'Media Pay disabled'                                            
MSGDUPDY DC    C'Write Dup dumps '                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
LINEA    DSECT                                                                  
         DC    C'USER INPUT='                                                   
IIH      DS    CL4                                                              
         DS    CL2                                                              
FSTRT    DC    C'STARTED'                                                       
         DS    CL1                                                              
FSTRTTIM DS    CL8                                                              
         DS    CL4                                                              
FRSTR    DC    C'RESTART'                                                       
         DS    CL1                                                              
FRSTRTIM DS    CL8                                                              
         DS    CL4                                                              
FDOWN    DC    C'DOWN#00'                                                       
         DS    CL1                                                              
FDOWNTIM DS    CL8                                                              
         SPACE 2                                                                
LINEB    DSECT                                                                  
         DC    C'SIN='                                                          
SINO     DS    CL8                                                              
         DS    CL5                                                              
LJOBTXT1 DC    C'LAST JOB SUBMITTED  '                                          
LJOBNAME DS    CL8                                                              
LJOBTXT2 DS    CL3                                                              
LJOBNO   DS    CL5                                                              
         DS    CL4                                                              
         DC    C'NUM TASKS='                                                    
TASKCT   DS    CL2                                                              
         SPACE 2                                                                
LINEC    DSECT                                                                  
         DC    C'MAX DUMPS='                                                    
MAXDMP   DS    CL2                                                              
         DS    CL5                                                              
         DC    C'DUMP#'                                                         
CURDMP   DS    CL2                                                              
         DS    CL1                                                              
CURDMPTM DS    CL8                                                              
         DS    CL4                                                              
         DC    C'LOOPTIMER='                                                    
LOOPTMR  DS    CL8                                                              
         DS    CL2                                                              
         DC    C'GLOBALTIMER='                                                  
GLOBTMR  DS    CL8                                                              
         SPACE 2                                                                
LINED    DSECT                                                                  
         DC    C'HICORE='                                                       
HICORE   DS    CL8                                                              
         DS    CL2                                                              
         DC    C'REGION='                                                       
REGION   DS    CL8                                                              
         DS    CL5                                                              
         DC    C'LAST POP='                                                     
LASTPOP  DS    CL8                                                              
         DS    CL3                                                              
         DC    C'TIME NOW='                                                     
TIMENOW  DS    CL8                                                              
         SPACE 2                                                                
LINEE    DSECT                                                                  
         DC    C'MAX UTL='                                                      
MAXUTL   DS    CL5                                                              
         DS    CL4                                                              
         DC    C'MAX TEMPSTR='                                                  
MAXTMPR  DS    CL5                                                              
         DS    CL3                                                              
         DC    C'CUR UTL USED='                                                 
ACTTMPR  DS    CL5                                                              
         DS    CL2                                                              
         DC    C'MAX SESSIONS='                                                 
MAXSESS  DS    CL1                                                              
         SPACE 2                                                                
LINEF    DSECT                                                                  
         DC    C'MAX PAGES='                                                    
MAXTMPT  DS    CL5                                                              
         DS    CL2                                                              
         DC    C'MAX TEMPEST='                                                  
MAXTMPS  DS    CL7                                                              
         DS    CL1                                                              
         DC    C'CUR CIS USED='                                                 
ACTTMPS  DS    CL5                                                              
         DS    CL2                                                              
         DC    C'TSAR BUFF='                                                    
TSARBUFF DS    CL9                                                              
         SPACE 2                                                                
LINE16   DSECT                                                                  
         DC    C'VTAM HG='                                                      
LASTHNG  DS    CL8                                                              
         DS    CL1                                                              
         DC    C'FACWRK UPD='                                                   
FACWKUPD DS    CL3                                                              
         DS    CL6                                                              
         DC    C'PQ NEW REPTS='                                                 
PQNEW    DS    CL5                                                              
         DS    CL2                                                              
         DC    C'PQ AVG I/OS='                                                  
PQAVG    DS    CL4                                                              
         SPACE 2                                                                
LINE17   DSECT                                                                  
         DC    C'PROTECTION='                                                   
FACPROT  DS    CL3                                                              
         DS    CL3                                                              
FACTOR   DC    C'TOR+AOR'                                                       
         SPACE 2                                                                
LINE18   DSECT                                                                  
         DC    C'SMFID='                                                        
SMFID    DS    CL4                                                              
         DS    CL7                                                              
         DC    C'Ubuffadr='                                                     
URECADDR DS    CL8                                                              
         DS    CL3                                                              
         DS    C'Ubufflen='                                                     
URECLEN  DS    CL7                                                              
         DS    CL4                                                              
MEDPAY   DS    CL18                                                             
         SPACE 2                                                                
LINE19   DSECT                                                                  
DUPDUMP  DS    CL16                                                             
         DS    CL1                                                              
         DC    C'OWNER PRGMS='                                                  
PIND     DS    CL3                                                              
         DS    CL5                                                              
         DC    C'MAXIO='                                                        
MAXIOCT  DS    CL5                                                              
         DS    CL9                                                              
         DC    C'Unused GFrecs='                                                
PGFRECS  DS    CL5                                                              
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
FUL1     DS    F                                                                
HALF     DS    H                                                                
HALF1    DS    H                                                                
SAVERE   DS    F                                                                
DMCB     DS    6F                                                               
VHEXOUT  DS    A                                                                
VFILETAB DS    A                                                                
PRTQDTFL DS    16F                                                              
PRTQDTFX EQU   *                                                                
*                                                                               
LENQ     DS    F                                                                
AENQA    DS    A                                                                
AENQB    DS    A                                                                
AENQC    DS    A                                                                
AENQD    DS    A                                                                
*                                                                               
ANYWHERE DS    CL2                                                              
WORK     DS    CL20                                                             
IOA      DS    CL80                                                             
COMMA    DS    C                                                                
*                                                                               
OPMSG    DS    0CL18               +FACPAK+ =SSB STOP                           
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPSSB    DS    CL4                                                              
         DS    CL1                                                              
OPCMND   DS    CL4                                                              
OPMSGL   EQU   *-OPMSG                                                          
*                                                                               
WRKL     EQU   *-WRKD                                                           
         EJECT                                                                  
SRSSBFFD DSECT                                                                  
         DS    CL64                                                             
* SRSSBFFD                                                                      
       ++INCLUDE SRSSBFFD                                                       
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* DMTMSHDR                                                                      
       ++INCLUDE DMTMSHDR                                                       
         EJECT                                                                  
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
         EJECT                                                                  
* FATABSDMP                                                                     
       ++INCLUDE FATABSDMP                                                      
         EJECT                                                                  
* FAPIGFACD                                                                     
       ++INCLUDE FAPIGFACD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SRSSB00S  05/01/02'                                      
         END                                                                    
