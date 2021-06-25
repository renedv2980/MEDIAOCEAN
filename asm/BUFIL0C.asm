*          DATA SET BUFIL0C    AT LEVEL 003 AS OF 05/01/02                      
*PHASE T5020AA                                                                  
         TITLE 'T5020C - BUDGET CONTROL LFM - FIX BWD AND FWD POINTERS'         
T5020C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FIX***,RA,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R5,ANODBLK          R5=A(NODIO BLOCK)                            
         USING NODBLKD,R5                                                       
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
*                                                                               
         GOTO1 VSETADD                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BE    VKEY                                                             
         CLI   MODE,VALREC                                                      
         BE    FIX                                                              
         CLI   MODE,PRINTREP                                                    
         BE    FIX                                                              
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VKEY     GOTO1 VGETFLD,DMCB,FIXKEYH                                             
         MVC   NODKEY,FLD          NO VERIFY THE KEY AGAINST THE FILE           
         XC    NDHOOK,NDHOOK                                                    
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    VKEY2                                                            
         LA    R2,FIXKEYH                                                       
         GOTO1 VNODERR                                                          
*                                                                               
VKEY2    MVI   ERROR,INVALID                                                    
         CLI   NDLEV,2                                                          
         BH    SPERR                                                            
         MVC   PARLEV,NDLEV                                                     
         ZIC   R1,NDLEV                                                         
         LA    R1,1(R1)                                                         
         STC   R1,THISLEV                                                       
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   PARDA,NDLVDA                                                     
         MVC   PARCODE,NDLVCOD                                                  
         MVC   PARNODE,NDLVNOD                                                  
         MVC   PARKEY,NDLVKEY                                                   
         LA    R3,NDLVTABL(R3)                                                  
         MVC   PARFRST,NDLVFRST                                                 
         MVC   PARLAST,NDLVLAST                                                 
         MVC   PARNEXT,NDLVNOD                                                  
         MVI   ERROR,INVALID                                                    
         OC    NDLVNOD,NDLVNOD     TEST IF RECORD IS A PARENT                   
         BZ    SPERR                                                            
         MVC   SVNKEY,FLD          SAVE THE PARENT'S KEY                        
*                                                                               
VKEY4    MVI   TEST,YES                                                         
         GOTO1 VGETFLD,DMCB,FIXMODEH                                            
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    VKEYX               YES                                          
         CLI   FLD,C'T'                                                         
         BE    VKEYX                                                            
         CLI   FLD,C'L'                                                         
         BNE   SPERR                                                            
         MVI   TEST,NO                                                          
*                                                                               
VKEYX    TM    WHEN,X'58'          TEST REPORT REQUESTED                        
         BNZ   *+8                 YES                                          
         OI    WHENOK,X'01'        BYPASS GENCON REPORT MAINTENANCE             
         B     XIT                                                              
         EJECT                                                                  
* FIX FORWARD AND BACKWARD POINTERS (VALREC AND PRINTREP)                       
*                                                                               
FIX      CLI   MODE,VALREC         TEST PROCESSING ON-LINE                      
         BE    FIX1                                                             
         MVI   RCSUBPRG,0                                                       
         LA    R1,HEDSPECS         INITIALIZE FOR REPORT                        
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
FIX1     XC    KEY,KEY                                                          
         L     R3,AIO3             R3=TABLE POINTER                             
         LA    RF,LIOS                                                          
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR TABLE AREA                             
         LA    R4,KEY                                                           
         USING BURECD,R4                                                        
         MVC   BUKEY(BUKNODE-BUKEY),PARKEY                                      
         MVC   BUKNODE,PARNEXT                                                  
*                                                                               
FIX2     GOTO1 HIGH                                                             
         B     FIX4                                                             
*                                                                               
FIX3     LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
FIX4     CLC   BUKEY(BUKCODE-BUKEY),KEYSAVE TEST SAME NODE                      
         BNE   FIX10                                                            
         CLI   BUKSUB,0            TEST FOR FILE DEFINITION RECORD              
         BNE   FIX3                NO-SKIP OVER IT                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         XC    ENTRY,ENTRY                                                      
         LA    R2,ENTRY                                                         
         USING ENTD,R2                                                          
         MVC   ENTCODE,BUKCODE                                                  
         MVC   ENTDA,DMDSKADD                                                   
         MVI   ELCODE,X'B8'        FORWARD POINTER                              
         BAS   RE,GETEL                                                         
         BNE   FIX5                                                             
*                                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'2'                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     FIX5                                                             
         MVC   ENTAFT(0),2(R6)     EXTRACT FORWARD POINTER                      
*                                                                               
FIX5     MVI   ELCODE,X'B9'        BACKWARD POINTER                             
         BAS   RE,GETEL                                                         
         BNE   FIX6                                                             
*                                                                               
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'                                                         
         CH    R1,=H'2'                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     FIX6                                                             
         MVC   ENTBEF(0),2(R6)     EXTRACT BACKWARD POINTER                     
*                                                                               
FIX6     ZIC   R1,NENTS                                                         
         LA    R1,1(R1)            BUMP TABLE ENTRY COUNT                       
         CH    R1,=Y(MAXENTS)                                                   
         BNL   FIXR                TOO MANY ENTRIES                             
         STC   R1,NENTS                                                         
         MVC   0(ENTL,R3),ENTRY    ADD NEW ENTRY                                
         LA    R3,ENTL(R3)                                                      
         B     FIX3                READ ANOTHER RECORD                          
         DROP  R2                                                               
*                                                                               
FIX10    CLI   NENTS,2             TEST AT LEAST TWO ENTRIES                    
         BL    FIXX                NO-EXIT RIGHT NOW                            
*                                                                               
         L     R3,AIO3             R3=A(ENTRY)                                  
         USING ENTD,R3                                                          
         ST    R3,AFIRST                                                        
         MVC   FIRST,ENTCODE       EXTRACT FIRST CODE                           
         ZIC   R4,NENTS            R4=COUNTER                                   
         LR    RE,R4                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(ENTL)                                                      
         LA    RE,0(R3,RE)                                                      
         ST    RE,ALAST                                                         
         MVC   LAST,ENTCODE-ENTD(RE)                                            
         LA    R2,P                                                             
         USING PLIND,R2                                                         
*                                                                               
FIX12    XC    KEY,KEY                                                          
         LA    RE,KEY+(BUKDA-BUKEY)                                             
         MVC   0(L'BUKDA,RE),ENTDA   SLOT THE DISK ADDRESS                      
         GOTO1 GETREC                                                           
         MVI   UPDATE,NO           INITIALIZE UPDATE SWITCH                     
         MVC   PCODE,ENTCODE       INITIALIZE PRINT LINE                        
         MVC   PBEFORE,ENTBEF                                                   
         MVC   PAFTER,ENTAFT                                                    
*                                                                               
         C     R3,AFIRST           TEST IF FIRST ENTRY                          
         BNE   FIX14               NO                                           
         OC    ENTBEF,ENTBEF       TEST IF BACK POINTER SET                     
         BZ    FIX16               NO-THAT'S OK                                 
         MVI   ELCODE,X'B9'                                                     
         GOTO1 REMELEM             DELETE THE ELEMENT                           
         B     FIX15                                                            
*                                                                               
FIX14    LR    R6,R3                                                            
         SH    R6,=Y(ENTL)         RE=A(PREVIOUS ENTRY)                         
         CLC   ENTBEF,ENTCODE-ENTD(R6) TEST IF BACK POINTER RIGHT               
         BE    FIX16               YES                                          
*                                                                               
         MVC   PBACK,ENTCODE-ENTD(R6) SHOW THE CORRECT CODE                     
         MVI   ELCODE,X'B9'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELEM,X'B9'                                                       
         MVC   ELEM+2(L'ENTCODE),ENTCODE-ENTD(R6)                               
         LA    RE,4                                                             
         CLI   ELEM+4,0            TEST FOR 2 OR 3 BYTE CODE                    
         BE    *+8                                                              
         LA    RE,5                                                             
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
FIX15    MVC   PNOTE1(L'BACKMSG),BACKMSG                                        
         MVI   UPDATE,YES          MUST WRITE BACK RECORD                       
*                                                                               
FIX16    C     R3,ALAST            TEST IF LAST ENTRY                           
         BNE   FIX18               NO                                           
         OC    ENTAFT,ENTAFT       TEST FOR FORWARD POINTER                     
         BZ    FIX20               NO-GOOD                                      
         MVI   ELCODE,X'B8'                                                     
         GOTO1 REMELEM                                                          
         B     FIX19                                                            
*                                                                               
FIX18    LA    R6,ENTL(R3)         R6=A(NEXT ENTRY)                             
         CLC   ENTAFT,ENTCODE-ENTD(R6) TEST IF POINTER IS RIGHT                 
         BE    FIX20               YES                                          
*                                                                               
         MVC   PFORWARD,ENTCODE-ENTD(R6)                                        
         MVI   ELCODE,X'B8'        FORWARD POINTER ELEMENT                      
         GOTO1 REMELEM                                                          
         MVI   ELEM,X'B8'                                                       
         MVC   ELEM+2(L'ENTCODE),ENTCODE-ENTD(R6)                               
         LA    RE,4                                                             
         CLI   ELEM+4,0                                                         
         BE    *+8                                                              
         LA    RE,5                                                             
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
FIX19    MVC   PNOTE2(L'FWDMSG),FWDMSG                                          
         MVI   UPDATE,YES                                                       
*                                                                               
FIX20    CLI   TEST,YES            TEST RUN                                     
         BE    FIX22               YES                                          
         CLI   UPDATE,YES          TEST TO PUT RECORD                           
         BNE   FIX22               NO                                           
         GOTO1 PUTREC                                                           
*                                                                               
FIX22    CLI   MODE,VALREC         TEST ON-LINE UPDATE                          
         BE    FIX24               YES                                          
         OC    P,SPACES            SPACE PAD DATA                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         BASR  RE,RF               SKIP A LINE                                  
*                                                                               
FIX24    LA    R3,ENTL(R3)                                                      
         BCT   R4,FIX12                                                         
*                                                                               
FIX25    XC    KEY,KEY                                                          
         LA    RE,KEY+(BUKDA-BUKEY)                                             
         MVC   0(L'BUKDA,RE),PARDA GET THE PARENT RECORD                        
         GOTO1 GETREC                                                           
         MVI   UPDATE,NO                                                        
         CLI   MODE,VALREC                                                      
         BE    FIX25A                                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+10(15),=C'*PARENT RECORD*'                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
FIX25A   MVC   PCODE,PARCODE                                                    
         MVC   PBEFORE,PARFRST                                                  
         MVC   PAFTER,PARLAST                                                   
*                                                                               
FIX26    CLC   FIRST,PARFRST       TEST IF FIRST IS RIGHT                       
         BE    FIX28                                                            
*                                                                               
         MVC   PBACK,FIRST                                                      
         MVC   PNOTE1(L'FSTMSG),FSTMSG                                          
         MVI   UPDATE,YES                                                       
         MVI   ELCODE,X'B6'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELEM,X'B6'                                                       
         MVC   ELEM+2(L'FIRST),FIRST                                            
         LA    RE,4                                                             
         CLI   FIRST+2,0                                                        
         BE    *+8                                                              
         LA    RE,5                                                             
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
FIX28    CLC   LAST,PARLAST                                                     
         BE    FIX30                                                            
*                                                                               
         MVC   PFORWARD,LAST                                                    
         MVC   PNOTE2(L'LASTMSG),LASTMSG                                        
         MVI   UPDATE,YES                                                       
         MVI   ELCODE,X'B7'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELEM,X'B7'                                                       
         MVC   ELEM+2(L'LAST),LAST                                              
         LA    RE,4                                                             
         CLI   LAST+2,0                                                         
         BE    *+8                                                              
         LA    RE,5                                                             
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
*                                                                               
FIX30    CLI   TEST,YES                                                         
         BE    FIX32                                                            
         MVI   NDREREAD,YES        FORCE RE-READ NEXT TIME IN SO                
         CLI   UPDATE,YES          NODIO BLOCK WILL REFLECT FIXES               
         BNE   FIX32                                                            
         GOTO1 PUTREC                                                           
*                                                                               
FIX32    CLI   MODE,VALREC         TEST ON-LINE UPDATE                          
         BE    FIXX                YES-ALL DONE                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
         CLI   TEST,YES            TEST RUN                                     
         BE    *+8                 YES-SKIP DIAGNOSTIC                          
         BAS   RE,FWD              CHECK LSEQ READ                              
*                                                                               
FIXX     MVC   CONHEAD(L'FIXMSG),FIXMSG                                         
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
FIXR     MVC   CONHEAD(L'BIGMSG),BIGMSG                                         
         LA    R2,CONACTH                                                       
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         SPACE 2                                                                
         EJECT                                                                  
* SUB-ROUTINE TO PRINT A FORWARD POINTER DIAGNOSTIC LISTING                     
*                                                                               
FWD      NTR1                                                                   
         MVI   FORCEHED,YES        BREAK A PAGE                                 
         MVI   RCSUBPRG,1          SET SUB-PROGRAM                              
         XC    NDHOOK,NDHOOK                                                    
         MVC   NODKEY,SVNKEY       SET PARENT'S KEY                             
         GOTO1 VNODIO,DMCB,NODBLKD,=C'READ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    FWD2                                                             
         DC    H'0'                                                             
*                                                                               
FWD2     LA    R1,PRTOUT                                                        
         ST    R1,NDHOOK                                                        
         MVC   NDSQBACK,PARLEV                                                  
         GOTO1 VNODIO,DMCB,NODBLKD,=C'LSEQ',NODKEY,0                            
         CLI   NDERR,0                                                          
         BE    FWDX                                                             
         DC    H'0'                                                             
*                                                                               
FWDX     B     XIT                                                              
         SPACE 2                                                                
* SUB-ROUTINE TO PRINT OUT LINE                                                 
*                                                                               
PRTOUT   ST    RE,SAVERE                                                        
         CLI   NDMODE,NDFRST                                                    
         BNE   PRTOUT2                                                          
         CLC   NDLEV,THISLEV                                                    
         BNE   *+8                                                              
         MVI   NDSKIP,YES                                                       
         B     PRTOUTX                                                          
*                                                                               
PRTOUT2  CLI   NDMODE,NDPROC                                                    
         BNE   PRTOUTX                                                          
         CLC   NDLEV,THISLEV                                                    
         BNE   PRTOUTX                                                          
         LA    R2,P                                                             
         USING PLIND,R2                                                         
         L     R3,NDLEVPTR                                                      
         USING NDLVTABD,R3                                                      
         MVC   PCODE,NDLVCOD       EXTRACT CODE                                 
         MVC   PBEFORE,NDLVBACK    BACKWARD POINTER                             
         MVC   PAFTER,NDLVFWRD     AND FORWARD POINTER                          
         OC    P,SPACES                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRTOUTX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* HEADLINE PRINTING ROUTINE HOOK                                                
*                                                                               
HOOK     NTR1                                                                   
*                                                                               
         ICM   R3,15,ABOX                                                       
         BZ    HOOKX                                                            
         USING BOXD,R3                                                          
         MVI   BOXROWS+6,C'T'      SET UP FOR BOXES                             
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         LA    R2,BOXCOLS                                                       
         MVI   0(R2),C'L'                                                       
         MVI   7(R2),C'C'                                                       
         MVI   14(R2),C'C'                                                      
         MVI   21(R2),C'C'                                                      
         MVI   28(R2),C'C'                                                      
         MVI   35(R2),C'C'                                                      
         MVI   69(R2),C'C'                                                      
         MVI   100(R2),C'R'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,YES                                                      
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
*                                                                               
HOOKX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         SPACE 1                                                                
GETEL    LR    R0,RE                                                            
         GOTO1 HELLO,PARAS,(C'G',SYSFIL),(ELCODE,(R4)),0,0                      
         L     R6,12(R1)                                                        
         LR    RE,R0                                                            
         CLI   12(R1),0                                                         
         BR    RE                                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
FIXMSG   DC    C'** FILE FIX COMPLETED **'                                      
BIGMSG   DC    C'* PARENT HAS TOO MANY CHILDREN TO RUN FIX ON LINE *'           
FWDMSG   DC    C'** FORWARD POINTER IS WRONG **'                                
BACKMSG  DC    C'** BACKWARD POINTER IS WRONG **'                               
FSTMSG   DC    C'**FIRST AT NEXT LEVEL IS WRONG**'                              
LASTMSG  DC    C'**LAST AT NEXT LEVEL IS WRONG**'                               
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* REPORT SPEC POOL                                                              
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,C'ABC - ACCOUNT BUDGET SYSTEM'                              
         SSPEC H2,2,C'---------------------------'                              
         SSPEC H1,50,C'FIX REPORT'                                              
         SSPEC H2,50,C'----------'                                              
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,75,REPORT                                                     
         SSPEC H4,87,RUN                                                        
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,100,PAGE                                                      
*                                                                               
         SSPEC H8,2,C'CODE'                                                     
         SSPEC H8,9,C'BACK'                                                     
         SSPEC H8,16,C'FWD'                                                     
         SSPEC H8,23,C'*BACK*'                                                  
         SSPEC H8,30,C'*FWD*'                                                   
         SSPEC H8,37,C'NOTE 1'                                                  
         SSPEC H8,71,C'NOTE 2'                                                  
*                                                                               
         SSPEC H9,2,C'----'                                                     
         SSPEC H9,9,C'----'                                                     
         SSPEC H9,16,C'---'                                                     
         SSPEC H9,23,C'------'                                                  
         SSPEC H9,30,C'-----'                                                   
         SSPEC H9,37,C'------'                                                  
         SSPEC H9,71,C'------'                                                  
*                                                                               
         SPROG 1                                                                
         SSPEC H4,2,C'LOGICAL SEQUENTIAL DIAGNOSTIC'                            
         SSPEC H5,2,C'-----------------------------'                            
         DC    X'00'                                                            
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE BUFILWORKD                                                     
         EJECT                                                                  
* DSECT TO COVER FIX SCREEN                                                     
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE BUFILFAD                                                       
         EJECT                                                                  
* WORKING STORAGE VALUES                                                        
*                                                                               
SYSD     DSECT                                                                  
         ORG   OVWORK              LOCAL WORKING STORAGE                        
PARLEV   DS    X                   PARENT'S LEVEL                               
PARKEY   DS    CL(L'BUKEY)         DIRECTORY KEY OF PARENT                      
PARDA    DS    CL4                 PARENT'S DISK ADDRESS                        
PARNODE  DS    CL(L'BUKNODE)       PARENT'S NODE                                
PARCODE  DS    CL(L'BUKCODE)       PARENT'S CODE                                
PARNEXT  DS    CL(L'BUKNODE)       NODE ESTABLISHED BY PARENT                   
PARFRST  DS    CL3                 FIRST CODE AT NEXT LEVEL                     
PARLAST  DS    CL3                 LAST CODE AT NEXT LEVEL                      
*                                                                               
THISLEV  DS    X                   LEVEL FOR FIX                                
*                                                                               
TEST     DS    C                   Y=TEST,N=LIVE                                
UPDATE   DS    C                   Y=YES,N=NO                                   
*                                                                               
ENTRY    DS    CL(ENTL)                                                         
NENTS    DS    X                                                                
*                                                                               
FIRST    DS    CL3                                                              
LAST     DS    CL3                                                              
*                                                                               
AFIRST   DS    A                   A(FIRST ENTRY)                               
ALAST    DS    A                   A(LAST ENTRY)                                
*                                                                               
         DS    CL(L'OVWORK-(*-OVWORK)) SPARE                                    
         SPACE 2                                                                
* DSECT TO COVER PRINT LINE                                                     
*                                                                               
PLIND    DSECT                                                                  
         DS    C                                                                
PCODE    DS    CL3                 RECORD CODE                                  
         DS    CL4                                                              
PBEFORE  DS    CL3                 CODE BEFORE                                  
         DS    CL4                                                              
PAFTER   DS    CL3                 CODE AFTER                                   
         DS    CL4                                                              
PBACK    DS    CL3                 REAL BACKWARD POINTER                        
         DS    CL4                                                              
PFORWARD DS    CL3                 REAL FORWARD POINTER                         
         DS    CL4                                                              
PNOTE1   DS    CL30                NOTE 1                                       
         DS    CL4                                                              
PNOTE2   DS    CL30                NOTE 2                                       
         SPACE 2                                                                
* DSECT TO COVER TABLE ENTRY                                                    
*                                                                               
ENTD     DSECT                                                                  
ENTCODE  DS    CL3                 CODE                                         
ENTDA    DS    CL4                 DISK ADDRESS                                 
ENTBEF   DS    CL3                 CODE BEFORE                                  
ENTAFT   DS    CL3                 CODE AFTER                                   
ENTL     EQU   *-ENTD              TABLE ENTRY LENGTH                           
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
MAXENTS  EQU   LIOS/ENTL          MAXIMUM TABLE SIZE                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003BUFIL0C   05/01/02'                                      
         END                                                                    
