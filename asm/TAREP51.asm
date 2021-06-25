*          DATA SET TAREP51    AT LEVEL 093 AS OF 12/16/16                      
*PHASE T70351C,*                                                                
*INCLUDE TALIM                                                                  
         TITLE 'T70351 - PR WITHHOLDING FORMS'                                  
T70351   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70351                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING LOCALD,R7           R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,OPENEQFX                                                      
                                                                                
         L     RE,=A(QXRREC)       RE=A(DATA TO MOVE FROM)                      
         LA    R0,TAPEIO           R0=A(DATA TO MOVE TO)                        
         LHI   R1,2104             R1 AND RF = LENGTH TO MOVE                   
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE CAN$ AMOUNTS IN SVTAB                   
         BAS   RE,PUTEQFX                                                       
                                                                                
         BAS   RE,PREP                                                          
         BAS   RE,CLOSEQFX                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
*                                                                               
VKEY     NTR1                                                                   
         LA    R2,SPUYEARH         VALIDATE YEAR                                
         GOTO1 ANY                                                              
         CLI   5(R2),4                                                          
         BNE   FLDINV                                                           
         MVC   WORK(4),=C'0000'                                                 
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=C'0000'                                                 
         BNE   FLDINV                                                           
         MVC   TIFYEAR(2),10(R2)                                                
         MVI   TIFYEAR+2,C' '                                                   
         SPACE 1                                                                
         LA    R2,SPUSSNH          VALIDATE SSN                                 
         XC    SPUSSNN,SPUSSNN                                                  
         XC    TIFSSN,TIFSSN                                                    
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SPUSSNH),SPUSSNNH                     
         MVC   TIQSTART,TGSSN                                                   
         MVC   TIFSSN,TGSSN                                                     
         SPACE 1                                                                
VK30     BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES OPTIONS FIELD                                  
         SPACE                                                                  
         USING SCAND,R3                                                         
VALOPT   NTR1                                                                   
         LA    R2,SPUOPTH          VALIDATE OPTIONS                             
         XC    OPTS,OPTS                                                        
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            NUMBER OF SCANNER ENTRIES                    
         SPACE 1                                                                
VOPT10   CLC   =C'TRACE',SCDATA1                                                
         BNE   FLDINV                                                           
         OI    OPTS,OPTRACE                                                     
         B     VOPTNEXT                                                         
         SPACE 1                                                                
VOPTNEXT LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10                                                        
         SPACE 1                                                                
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              PRINT REPORT                                                     
         SPACE 1                                                                
PREP     NTR1                                                                   
         MVI   FRSTIME,C'Y'        SET FIRST TIME INDICATOR                     
         BAS   RE,GETEMP           GET TP NAME AND ADDRESS                      
*        BAS   RE,LINEUP           PRINT LINE UP PATTERNS                       
*                                                                               
         XC    PERFSSN,PERFSSN     CLEAR SSN                                    
         ZAP   TOTPRWAG,=P'0'      PRE-CLEAR TOTALS                             
         ZAP   TOTPRTAX,=P'0'                                                   
         MVI   FORMCNT,0           RESET FORM COUNT (MAX 41)                    
         MVC   TIACOMFC,ACOMFACS   SET UP FOR TASYSIO                           
         LA    R1,IOHOOK           SET HOOK FOR W2                              
         ST    R1,TIHOOK                                                        
         MVI   TIFCUR,C'U'         ONLY INTERESTED IN $US                       
         MVC   TIFEMP,TGTPEMP      FOR EMPLOYER TP                              
         MVI   TIREAD,TLW2CDQ      SET TO READ W2 RECORDS                       
         OI    TIQFLAGS,TIQFDIR    WE'RE FILTERING DIRECTORY                    
                                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
                                                                                
         MVI   FORMCNT,FORMC41     FORCE SUBTOTAL PRINT                         
         BAS   RE,FORMS41          PRINT 41ST FORM IF NECESSARY                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PROCESSES EMPLOYER INFO                                  
         SPACE 1                                                                
GETEMP   NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'88',TGTPEMP),0                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EMPNAME,TGNAME                                                   
         CLC   TIFYEAR,=C'27'                                                   
         BH    *+10                                                             
         MVC   EMPNAME,=CL36'TALENT PARTNERS COM SERV, LLC'                     
*                                                                               
         MVC   EMPTEL,=C'312-923-7900'                                          
         BAS   RE,GETID            GET FEDERAL ID NUMBER                        
         BAS   RE,GETADDR          GET EMPLOYER ADDRESS                         
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO GET FEDERAL ID NUMBER FROM EMPLOYER                   
         SPACE 1                                                                
GETID    NTR1                                                                   
         MVC   EMPFDID,SPACES                                                   
         MVI   ELCODE,TATIELQ                                                   
         MVI   WORK,TATITYUN                                                    
         MVC   WORK+1(3),=C'FD '                                                
         GOTO1 GETL,DMCB,(4,WORK)                                               
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TATID,R4                                                         
         MVC   EMPFDID,TATIID                                                   
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE GETS ADDRESS ELEMENT IN AIO AND SAVES THE                
*              ADDRESS AT EMPADDR                                               
         SPACE 1                                                                
         USING TAADD,R4                                                         
GETADDR  NTR1                                                                   
         MVC   EMPADDR,SPACES                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAADELQ      GET ADDRESS ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R1,TAADLEN                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   EMPADDR(0),TAADADD                                               
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE UP PATTERN                               
         SPACE 1                                                                
LINEUP   NTR1                                                                   
         MVC   PRWAGE,=F'1000000'                                               
         MVC   PRTAX,=F'100'                                                    
         MVC   FICAWAGE,=F'1000000'                                             
         MVC   FICATAX,=F'100'                                                  
         MVC   MEDWAGE,=F'1000000'                                              
         MVI   PERFFNM,C'F'                                                     
         MVC   PERFFNM+1(L'PERFFNM-1),PERFFNM                                   
         MVI   PERFLNM,C'L'                                                     
         MVC   PERFLNM+1(L'PERFLNM-1),PERFLNM                                   
         MVI   PERFADDR,C'P'                                                    
         MVC   PERFADDR+1(L'PERFADDR-1),PERFADDR                                
         SPACE                                                                  
         MVC   PERFSSN,=C'111111111'                                            
         BAS   RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'222222222'                                            
         BAS   RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'333333333'                                            
         BAS   RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'444444444'                                            
         BAS   RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'555555555'                                            
         BAS   RE,PRNTFORM                                                      
         MVC   PERFSSN,=C'666666666'                                            
         BAS   RE,PRNTFORM                                                      
         B     XIT                                                              
         EJECT                                                                  
*              SYSIO HOOK FOR W2 RECS                                           
         SPACE 1                                                                
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      IF PROCDIR                                   
         BNE   IOHK5                                                            
         CLC   TISSN,PERFSSN       ONLY WANT FIRST W2 REC FOR SSN               
         BE    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
IOHK5    CLI   TIMODE,PROCREC      TEST FOR SYSIO MODE PROCREC                  
         BNE   XIT                                                              
         SPACE 1                                                                
         MVI   ELCODE,TAW2ELQ      ONLY INTERESTED IN PR W2'S                   
         MVC   FULL(3),=C'PR '                                                  
         MVC   AIO,TIAREC                                                       
         GOTO1 GETL,DMCB,(3,FULL)                                               
         MVC   AIO,AIO1                                                         
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         USING TAW2D,R4                                                         
         MVC   PRWAGE,TAW2EARN                                                  
         MVC   PRTAX,TAW2TAX                                                    
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TISSN)                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYIN   THAT ARE INDIVIDUALS                         
         BNE   XIT                                                              
         SPACE 1                                                                
         MVC   PERFFNM(L'TAW4NAM1),TAW4NAM1                                     
         MVC   PERFLNM(L'TAW4NAM2),TAW4NAM2                                     
         MVC   W4FIRST(L'TAW4NAM1),TAW4NAM1                                     
         MVC   W4LAST(L'TAW4NAM2),TAW4NAM2                                      
         MVC   PERFMID,SPACES                                                   
         MVI   W4MIDI,C' '                                                      
         CLI   TAW4LEN,TAW4LN2Q    HAVE MIDDLE NAME?                            
         BNE   IOHK10                                                           
         MVC   PERFMID(L'TAW4MIDN),TAW4MIDN                                     
         MVC   W4MIDI(1),TAW4MIDN                                               
         GOTO1 SQUASHER,DMCB,PERFFNM,33                                         
IOHK10   BAS   RE,GETW4ADD                                                      
         MVC   PERFSSN,TISSN       SAVE CURRENT PERFORMER                       
         SPACE 1                                                                
         BAS   RE,PROCW2           PROCESS W2 RECORD AND PRINT FORMS            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE GETS ADDRESS FROM W4 RECORD AND SAVES                    
*              IT IN PERFADDR                                                   
         SPACE                                                                  
GETW4ADD NTR1                      GET ADDRESS FROM W4 RECORD                   
         MVC   PERFADDR,SPACES                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TAA2ELQ      SEE IF THERE'S NEW STYLE ADDRESS             
         BAS   RE,GETEL                                                         
         BNE   GETW4A10                                                         
         SPACE                                                                  
         USING TAA2D,R4                                                         
         LA    R2,PERFADDR         R2=WHERE TO SAVE IT IN PERFADDR              
         LA    R6,TAA2ADD1                                                      
         LA    R0,3                ASSUME 3 ADDRESS LINES                       
         CLI   TAA2LEN,TAA2LNQ     FOR OLD STYLE ADDRESSES                      
         BL    GETW4A2                                                          
         CLC   TAA2CTRY,=C'US'     AND NEW STYLE US ADDRESSES                   
         BE    GETW4A2                                                          
         LHI   R0,2                ELSE USE 2                                   
GETW4A2  CLC   0(L'TAA2ADD1,R6),SPACES  TEST FOR ANY DATA LEFT                  
         BNH   GETW4A5                                                          
         MVC   0(L'TAA2ADD1,R2),0(R6)                                           
         LA    R2,L'PERFADD1(R2)   BUMP IN PERFADDR                             
         LA    R6,L'TAA2ADD1(R6)   BUMP TO NEXT ADDRESS LINE                    
         BCT   R0,GETW4A2                                                       
         SPACE                                                                  
GETW4A5  XC    WORK(33),WORK                                                    
         MVC   WORK(24),TAA2CITY      CITY (ONLY FITS L'24)                     
         MVC   WORK+25(2),TAA2ST      STATE                                     
         MVC   WORK+28(5),TAA2ZIP     ZIP (ONLY USE L'5)                        
         MVC   PERFCITY,TAA2CITY                                                
         MVC   PERFST,TAA2ST                                                    
         MVC   PERFZIP,TAA2ZIP                                                  
         GOTO1 SQUASHER,DMCB,WORK,33  SQUASH IT                                 
         MVC   0(33,R2),WORK                                                    
         SPACE                                                                  
         BRAS  RE,GETCTRY                                                       
         B     XIT                                                              
         SPACE                                                                  
         USING TAADD,R4                                                         
GETW4A10 MVI   ELCODE,TAADELQ      GET OLD STYLE ADDRESS ELEMENT                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ZIC   R0,TAADLNES         R0=N'ADDRESS LINES                           
         LA    R4,TAADADD                                                       
         LA    R3,PERFADDR                                                      
         SPACE                                                                  
GETW4A15 MVC   0(L'TAADADD,R3),0(R4)                                            
         LA    R3,L'PERFADD1(R3)   BUMP TO NEXT SAVED ADDRESS LINE              
         LA    R4,L'TAADADD(R4)                                                 
         BCT   R0,GETW4A15         LOOP FOR NUMBER OF LINES                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A FORM FOR A W2 RECORD                          
         SPACE 1                                                                
PROCW2   NTR1                                                                   
         BAS   RE,FORMS41          PRINT 41ST FORM IF NECESSARY                 
*                                                                               
         BAS   RE,GETAMTS          GET AMOUNTS                                  
*                                                                               
         BAS   RE,PRNTFORM         PRINT FORM                                   
*                                                                               
         L     R1,PRWAGE           ADD PUERTO RICO WAGES TO SUB-TOTAL           
         CVD   R1,DUB                                                           
         AP    TOTPRWAG,DUB        ADD TO 41ST SUBTOTAL FOR WAGES               
         L     R1,PRTAX            ADD PUERTO RICO TAXES TO SUB-TOTAL           
         CVD   R1,DUB                                                           
         AP    TOTPRTAX,DUB        ADD TO 41ST SUBTOTAL FOR TAXES               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AMOUNTS.  FICA INFO, ETC..                        
         SPACE 3                                                                
GETAMTS  NTR1                                                                   
         MVI   ELCODE,TAW2ELQ      GET FEDERAL EARNINGS                         
         MVC   FULL(3),=C'FD '                                                  
         MVC   AIO,TIAREC                                                       
         GOTO1 GETL,DMCB,(3,FULL)                                               
         MVC   AIO,AIO1                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         USING TAW2D,R4                                                         
*                                                                               
         LA    R0,LIMBLOCK                                                      
         LHI   R1,L'LIMBLOCK                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R2,LIMBLOCK                                                      
         USING TMD,R2                                                           
         ST    RC,TMRC                                                          
         MVC   WORK(2),TIFYEAR                                                  
         MVC   WORK+2(4),=C'1231'                                               
         GOTO1 DATCON,DMCB,(0,WORK),(0,TMEFDTE0)  SET INTERNAL FORMAT           
         MVC   TMEMP,TGTPEMP                                                    
         MVC   TMUNIT,=C'FD'                                                    
         OI    TMSTAT,TMSBSRT                                                   
         GOTO1 =V(TALIM),DMCB,(R2)                                              
         SPACE 1                                                                
         MVC   FICAWAGE,TAW2EARN                                                
         CLC   TAW2EARN,TMBFICA                                                 
         BL    *+10                                                             
         MVC   FICAWAGE,TMBFICA                                                 
         SPACE 1                                                                
         MVC   FICATAX,TAW2FICA                                                 
         SPACE 1                                                                
         MVC   MEDWAGE,TAW2EARN                                                 
         CLC   TAW2EARN,TMBMED                                                  
         BL    *+10                                                             
         MVC   MEDWAGE,TMBMED                                                   
         SPACE 1                                                                
         L     R1,MEDWAGE          COMPUTE MEDICARE TAX                         
         M     R0,TMRMED                                                        
         D     R0,=F'50000'                                                     
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         C     R1,FICATAX          INSURE NOT > TOTAL                           
         BNH   *+8                                                              
         L     R1,FICATAX                                                       
         ST    R1,MEDTAX                                                        
         L     R0,FICATAX          FICA = TOTAL - MEDICARE                      
         SR    R0,R1                                                            
         ST    R0,FICATAX                                                       
         SPACE 1                                                                
         B     XIT                                                              
         DROP  R2,R4                                                            
         SPACE 1                                                                
         DS    0F                                                               
LIMBLOCK DS    0CL(TMLNQ)                                                       
       ++INCLUDE TALIMD                                                         
         EJECT                                                                  
*              ROUTINE TO PRINT A FORM                                          
         SPACE 2                                                                
PRNTFORM NTR1                                                                   
         L     RE,=A(QDRREC)       RE=A(DATA TO MOVE FROM)                      
         LA    R0,TAPEIO           R0=A(DATA TO MOVE TO)                        
         LHI   R1,2104             R1 AND RF = LENGTH TO MOVE                   
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE CAN$ AMOUNTS IN SVTAB                   
                                                                                
         USING QDRREC,R2                                                        
         LA    R2,TAPEIO                                                        
         MVC   QDRFIRST(L'PERFFNM),W4FIRST                                      
         MVC   QDRMID(1),W4MIDI                                                 
         MVC   QDRLAST(L'PERFLNM),W4LAST                                        
         MVC   QDRADDR1(L'PERFADD1),PERFADD1                                    
         MVC   QDRADDR2(L'PERFADD2),PERFADD2                                    
         MVC   QDRCITY,PERFCITY                                                 
         MVC   QDRSTATE,PERFST                                                  
         MVC   QDRZIP,PERFZIP                                                   
                                                                                
         MVC   QDRSSN(3),PERFSSN                                                
         MVC   QDRSSN+4(2),PERFSSN+3                                            
         MVC   QDRSSN+7(4),PERFSSN+5                                            
         MVC   QDREIN,EMPFDID                                                   
                                                                                
         EDIT  PRWAGE,(10,QDRWAGES),0,FILL=0                                    
         MVC   QDRTOTW,QDRWAGES                                                 
         EDIT  PRTAX,(10,QDRTAX),0,FILL=0                                       
         EDIT  FICAWAGE,(10,QDRSSWAG),0,FILL=0                                  
         EDIT  FICATAX,(10,QDRSSTAX),0,FILL=0                                   
         EDIT  MEDWAGE,(10,QDRMDWAG),0,FILL=0                                   
         EDIT  MEDTAX,(10,QDRMDTAX),0,FILL=0                                    
*                                                                               
PRFRM900 BAS   RE,PUTEQFX                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT 41ST SUBTOTAL FORM IF NECESSARY                 
         SPACE 1                                                                
FORMS41  NTR1                                                                   
         LA    R2,P                R2=A(PRINT LINE)                             
         USING LINED,R2                                                         
         SPACE 1                                                                
         MVI   LINE,0              ENSURE WE DON'T SKIP                         
         MVI   FORCEHED,C'N'                                                    
         SPACE 1                                                                
         CLI   FORMCNT,FORMC41     EVERY 41ST FORM IS A TOTAL FORM              
         BNE   XIT                                                              
         SPACE 1                                                                
         MVI   SPACING,3                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         EDIT  TOTPRWAG,LINPRWAG,2 PRINT PR WAGE SUBTOTAL                       
         MVI   LINX41,C'X'         CHECK OFF BOX                                
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVI   SPACING,17                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
         EDIT  TOTPRTAX,LINPRTAX,2  PRINT PR WAGE SUBTOTAL                      
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVI   SPACING,11                                                       
         BAS   RE,PRNTIT                                                        
*                                                                               
         MVI   FORMCNT,0           RESET FORM COUNT (MAX 41)                    
         ZAP   TOTPRWAG,=P'0'                                                   
         ZAP   TOTPRTAX,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INCREMENT FORM COUNT FOR 41 SUB TOTAL                 
         SPACE 1                                                                
INCFCNT  NTR1                                                                   
         ZIC   R1,FORMCNT          ADD ONE TO FORM COUNT                        
         LA    R1,1(R1)                                                         
         STC   R1,FORMCNT                                                       
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
OPENEQFX NTR1                                                                   
         L     R2,=A(W2EQFX)                                                    
         OPEN  ((2),OUTPUT)                                                     
         B     XIT                                                              
                                                                                
CLOSEQFX NTR1                                                                   
         L     R2,=A(W2EQFX)                                                    
         CLOSE ((2))                                                            
         B     XIT                                                              
                                                                                
PUTEQFX  NTR1                                                                   
         LA    R5,TAPEIO                                                        
         L     R1,=A(W2EQFX)                                                    
         PUT   (1),(5)                                                          
         B     XIT                                                              
         EJECT                                                                  
         DROP  R2                                                               
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
THEEND   GOTO1 ERREX                                                            
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         ENTRY W2EQFX                                                           
W2EQFX   DCB   DDNAME=W2EQFX,DSORG=PS,MACRF=(PM),                      X        
               RECFM=VB,LRECL=2104,BUFNO=2,BLKSIZE=32760                        
         SPACE 3                                                                
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              XR - HEADER RECORD                                     *         
*---------------------------------------------------------------------*         
         DS    0D                                                               
QXRREC   DC    H'2104'                                                          
         DC    H'0'                                                             
QXR      DS    0CL2104                                                          
QXRID    DC    CL2'XR'             RECORD TYPE 'XR' HEADER                      
QXRFFMT  DC    CL4'P16 '           FILE FORMAT                                  
QXRFFMTV DC    CL8'1       '       FILE FORMAT VERSION                          
                                                                                
QXRLNQ   EQU   *-QXR                                                            
QXRFILL  DC    (2104-QXRLNQ)C' '                                                
         ORG                                                                    
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DR - DATA RECORD                                       *         
*---------------------------------------------------------------------*         
         DS    0D                                                               
QDRREC   DC    H'2104'                                                          
         DC    H'0'                                                             
QDR      DS    0CL2104                                                          
QDRID    DC    CL2'DR'             RECORD TYPE 'XR' HEADER                      
         DC    C' '                                                             
QDRFIRST DC    CL28' '             EMPLOYEE FIRST NAME                          
         DC    C' '                                                             
QDRMID   DC    C' '                EMPLOYEE MIDDLE INITIAL                      
         DC    C' '                                                             
QDRLAST  DC    CL28' '             EMPLOYEE LAST NAME                           
         DC    3C' '                                                            
QDRADDR1 DC    CL50' '             ADDRESS LINE 1                               
         DC    3C' '                                                            
QDRADDR2 DC    CL50' '             ADDRESS LINE 2                               
         DC    3C' '                                                            
QDRCITY  DC    CL14' '             CITY                                         
         DC    3C' '                                                            
QDRSTATE DC    CL2'  '             STATE                                        
         DC    3C' '                                                            
QDRZIP   DC    CL10' '             ZIP                                          
         DC    3C' '                                                            
QDRENAMM DC    CL30'TALENT PARTNERS COM SERV, LLC'                              
         DC    C' '                                                             
QDREADD1 DC    CL28'111 W JACKSON BLVD'   EMPLOYER ADDR 1                       
         DC    3C' '                                                            
QDREADD2 DC    CL28'STE 1525'      EMPLOYER ADDR 2                              
         DC    3C' '                                                            
QDRECITY DC    CL14'CHICAGO'       EMPLOYER CITY                                
         DC    3C' '                                                            
QDREST   DC    CL2'IL'             EMPLOYER STATE                               
         DC    3C' '                                                            
QDREZIP  DC    CL10'60604'         EMPLOYER ZIP                                 
         DC    3C' '                                                            
QDRSSN   DC    CL11'000-00-0000'   SSN                                          
         DC    C' '                                                             
QDRMRTLS DC    C' '                MARITAL SINGLE                               
QDRMRTLM DC    C' '                MARITAL MARRIED                              
QDREXSL1 DC    CL10'0000000000'    SALARY EXEMPT 1                              
         DC    2C' '                                                            
QDREIN   DC    CL10' '             EIN                                          
         DC    CL10'     90550'    EQUIFAX EMPLOYER CODE                        
QDRWAGES DC    CL10'0000000000'    WAGES                                        
QDRCOMM  DC    CL10'0000000000'    COMMISSIONS                                  
QDRALLOW DC    CL10'0000000000'    ALLOWANCES                                   
QDRTIPS  DC    CL10'0000000000'    TIPS                                         
QDRTOTW  DC    CL10'0000000000'    TOTW                                         
QDRFBEN  DC    CL10'0000000000'    FRINGE BENEFITS                              
QDRTAX   DC    CL10'0000000000'    TAX WITHHELD                                 
QDRGRF   DC    CL10'0000000000'    GOVT REITREMENT FUND                         
QDRCODA  DC    CL10'0000000000'    CODA-CONTRIBUTING                            
QDRSSWAG DC    CL10'0000000000'    SS WAGES                                     
QDRSSTAX DC    CL10'0000000000'    SS TAX WITHHELD                              
QDRMDWAG DC    CL10'0000000000'    MEDICARE WAGES                               
QDRMDTAX DC    CL10'0000000000'    MEDICARE TAX                                 
QDRSSTIP DC    CL10'0000000000'    SS TIPS                                      
QDRSSTTX DC    CL10'0000000000'    SS TAX ON TIPS                               
QDRMDTTX DC    CL10'0000000000'    MED TAX ON TIPS                              
         DC    C' '                MAIL FLAG                                    
         DC    CL15' '             SORT KEY 1                                   
         DC    CL15' '             SORT KEY 2                                   
         DC    CL15' '             SORT KEY 3                                   
QDREPHON DC    CL15'3129237900'           EMP PHONE                             
         DC    4C' '                                                            
         DC    11C' '              INTERNAL SEQ NUMBER                          
         DC    C' '                INTERNAL BOX BREAK                           
         DC    C' '                INTERNAL TEST SELECTION FLAG                 
         DC    9C' '               CONTROL NUMBER                               
         DC    2C' '                                                            
         DC    5C' '                                                            
         DC    8C' '               EQUIFAX EMPLOYEE PIN                         
         DC    64C' '              EMPLOYEE ID                                  
         DC    C' '                EMPLOYEE STATUS CODE                         
         DC    C' '                                                             
QDRECSTH DC    CL10'0000000000'    COST OF EMPLOYER SPON HEALTH                 
         DC    C' '                                                             
QDRCONTR DC    CL10'0000000000'    CONTRIBIUTIONS TO SAVE AND DUPLICAT          
         DC    C' '                                                             
QDRCHARI DC    CL10'0000000000'    CHARITIES                                    
         DC    83C' '                                                           
         DC    C' '                DATA PRINT PROOF                             
         DC    8C' '               DATE CEASE OF OPERATIONS                     
QDREXSC1 DC    C' '                EXEMPT SALARIES CODE 1                       
QDRELECT DC    CL7' '              ELECTRONIC FILING CONFIRMATION NO            
QDRDOB   DC    CL8' '              DATE OF BIRTH                                
QDREXSL2 DC    CL10'0000000000'    EXEMPT SALARIES 2                            
QDREXSC2 DC    C' '                EXEMPT SALARIES CODE 2                       
                                                                                
QDRLNQ   EQU   *-QDR                                                            
QDRFILL  DC    (2104-QDRLNQ)C' '                                                
         ORG                                                                    
         EJECT                                                                  
*              ROUTINE TO GET COUNTRY CODE                                      
*              ON ENTRY ... R2 = A(PRINT LINE)                                  
*                           R4 = A(ADDRESS ELEMENT)                             
         SPACE                                                                  
         USING TAA2D,R4                                                         
GETCTRY  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAA2LEN,TAA2LNQ        FOR OLD STYLE ADDRESSES                   
         JL    XIT                                                              
         CLC   TAA2CTRY,=C'US'        AND NEW STYLE US ADDRESSES                
         JE    XIT                                                              
         GOTO1 VALCTRY,DMCB,(X'80',TAA2CTRY)                                    
         JNE   XIT                                                              
         DROP  R4                                                               
         SPACE 1                                                                
         USING CTRYTABD,R1                                                      
         L     R1,TGACTRY                                                       
         ZIC   RE,CTRYDSP                                                       
         LTR   RE,RE                                                            
         JNZ   *+8                                                              
         IC    RE,CTRYLEN                                                       
         SHI   RE,CTRYDESC-CTRYTABD+1                                           
         MVC   WORK(33),SPACES                                                  
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   WORK(0),CTRYDESC                                                 
         EX    RE,*+8                                                           
         J     *+10                                                             
         OC    WORK(0),SPACES                                                   
         MVC   34(33,R2),WORK                                                   
         J     XIT                                                              
         DROP  R1                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
FRSTIME  DS    CL1                 FIRST TIME INDICATOR                         
FORMCNT  DS    XL1                 FORM COUNT                                   
FORMC41  EQU   41                  PRINT SUBTOTAL AT 41 AND RESET               
         SPACE 1                                                                
TOTAMTS  DS    0PL8                TOTALS FOR FORM 41 SUBTOTAL                  
TOTPRWAG DS    PL8                 TOTAL PUERTO RICO WAGES                      
TOTPRTAX DS    PL8                 TOTAL PUERTO RICO TAX                        
         SPACE 1                                                                
AMTS     DS    0F                                                               
PRWAGE   DS    F                   PUERTO RICO WAGES                            
PRTAX    DS    F                   PUERTO RICO TAXES WITHHELD                   
FICAWAGE DS    F                   SOCIAL SECURITY WAGES                        
FICATAX  DS    F                   SOCIAL SECURITY TAXES                        
MEDWAGE  DS    F                   MEDICARE WAGES                               
MEDTAX   DS    F                   MEDICARE TAXES                               
NAMTS    EQU   (*-AMTS)/L'AMTS                                                  
         SPACE                                                                  
OPTS     DS    XL1                 OPTIONS                                      
OPTRACE  EQU   X'80'               TRACE RECORDS                                
         SPACE                                                                  
EMPNAME  DS    CL36                EMPLOYER NAME                                
EMPTEL   DS    CL12                EMPLOYER TELEPHONE                           
EMPFDID  DS    CL14                EMPLOYER FEDERAL ID NO                       
EMPADDR  DS    0CL(4*30)                    ADDRESS                             
EMPADD1  DS    CL30                                                             
EMPADD2  DS    CL30                                                             
EMPADD3  DS    CL30                                                             
EMPADD4  DS    CL30                                                             
EMPCITY  DS    CL14'CHICAGO'                                                    
EMPSTATE DS    CL2'IL'                                                          
EMPZIP   DS    CL10'60604'                                                      
         SPACE                                                                  
PERFSSN  DS    CL9                 PERFORMER SSN                                
PERFFNM  DS    CL16                PERFORMER FIRST NAME                         
         DS    CL1                 SPACE FOR SQUASHER                           
PERFMID  DS    CL16                PERFORMER MIDDLE NAME                        
PERFLNM  DS    CL16                          LAST NAME                          
PERFADDR DS    0CL(4*30)           ADDRESS                                      
PERFADD1 DS    CL30                                                             
PERFADD2 DS    CL30                                                             
PERFADD3 DS    CL30                                                             
PERFADD4 DS    CL30                                                             
PERFCITY DS    CL14                                                             
PERFST   DS    CL2                                                              
PERFZIP  DS    CL10                                                             
W4FIRST  DS    CL16                PERFORMER FIRST NAME                         
W4LAST   DS    CL16                PERFORMER LAST NAME                          
W4MIDI   DS    CL1                 PERFORMER MIDDLE INITIAL                     
                                                                                
         DS    0D                                                               
TAPEIO   DS    CL2108                                                           
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL8                 4TH LINE                                     
LINPFNM  DS    CL33                PERFORMER FIRST/MID NAME                     
         DS    CL13                                                             
LINSSN   DS    CL9                 SOCIAL SECURITY NUMBER                       
         DS    CL17                                                             
LINPRWAG DS    CL14                PR STATE WAGES                               
         DS    CL10                                                             
LINX41   DS    XL1                 CHECK OFF BOX FOR EVERY 41                   
*                                                                               
         ORG   LINED               7TH LINE                                     
         DS    CL8                                                              
LINPLNM  DS    CL16                PERFORMER LAST NAME                          
         DS    CL80                                                             
LINFWAGE DS    CL14                SOCIAL SECURITY WAGES                        
*                                                                               
         ORG   LINED               9TH LINE                                     
         DS    CL8                                                              
LINPADDR DS    CL30                PERFORMER ADDRESS                            
*                                                                               
         ORG   LINED               10TH LINE                                    
         DS    CL8                                                              
LINPADD2 DS    CL30                PERFORMER ADDRESS LINE 2                     
         DS    CL66                                                             
LINFTAX  DS    CL14                SOCIAL SECURITY TAXES                        
*                                                                               
         ORG   LINED               11TH LINE                                    
         DS    CL8                                                              
LINPADD3 DS    CL30                PERFORMER ADDRESS LINE 3                     
*                                                                               
         ORG   LINED               12TH LINE                                    
         DS    CL8                                                              
LINPADD4 DS    CL30                PERFORMER ADDRESS LINE 4                     
*                                                                               
         ORG   LINED               13TH LINE                                    
         DS    CL54                                                             
LINEFDID DS    CL14                EMPLOYER FEDERAL ID NUMBER                   
         DS    CL36                                                             
LINMWAGE DS    CL14                MEDICARE WAGES AND TIPS                      
*                                                                               
         ORG   LINED               18TH LINE                                    
         DS    CL8                                                              
LINENAME DS    CL36                EMPLOYER NAME                                
         DS    CL36                                                             
LINTOT   DS    CL14                SAME AS WAGES                                
         DS    CL10                                                             
LINMTAX  DS    CL14                MEDICARE TAXES                               
*                                                                               
         ORG   LINED               19TH LINE                                    
         DS    CL8                                                              
LINEADDR DS    CL30                EMPLOYER ADDRESS                             
*                                                                               
         ORG   LINED               20TH LINE                                    
         DS    CL8                                                              
LINEADD2 DS    CL30                EMPLOYER ADDRESS LINE 2                      
*                                                                               
         ORG   LINED               21TH LINE                                    
         DS    CL8                                                              
LINEADD3 DS    CL30                EMPLOYER ADDRESS LINE 3                      
*                                                                               
         ORG   LINED               22TH LINE                                    
         DS    CL8                                                              
LINEADD4 DS    CL30                EMPLOYER ADDRESS LINE 4                      
*                                                                               
         ORG   LINED               23TH LINE                                    
         DS    CL80                                                             
LINPRTAX DS    CL14                PUERTO RICO TAXES WITHHELD                   
*                                                                               
         ORG   LINED               24TH LINE                                    
         DS    CL31                                                             
LINETEL  DS    CL12                EMPLOYER TELEPHONE NUMBER                    
*                                                                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC4D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093TAREP51   12/16/16'                                      
         END                                                                    
