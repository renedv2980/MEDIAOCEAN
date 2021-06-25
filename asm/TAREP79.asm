*          DATA SET TAREP79    AT LEVEL 010 AS OF 02/10/17                      
*PHASE T70379C,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70379 - MED09 REPORT'                                          
T70379   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70379,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         JNE   *+8                                                              
         BRAS  RE,VK                                                            
                                                                                
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         JNE   *+8                                                              
         BRAS  RE,PR                                                            
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         ZAP   TOTUEARN,=P'0'                                                   
         ZAP   TOTUTAX,=P'0'                                                    
         ZAP   TOTOEARN,=P'0'                                                   
         ZAP   TOTOTAX,=P'0'                                                    
                                                                                
         LA    R2,MEDEMPH          EMPLOYER                                     
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',(R2)),MEDEMPNH                        
                                                                                
         LA    R2,MEDAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),MEDAGYNH                        
                                                                                
VK10     LA    R2,MEDCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         LA    R2,MEDAGYH                                                       
         CLI   5(R2),0                                                          
         BE    ERMIS                                                            
         LA    R2,MEDCLIH                                                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),MEDCLINH                        
                                                                                
VK20     LA    R2,MEDPERH          PERIOD                                       
         GOTO1 ANY                                                              
         GOTO1 VALPERD                                                          
                                                                                
         USING MDRATESD,RF                                                      
         LA    RF,MDRATES                                                       
VK22     CLI   0(RF),X'FF'         GET RATES FOR REQUESTED PERIOD               
         JE    ERRTMSG                                                          
         CLC   TIQPSTR,MDRSTR                                                   
         JL    VR24                                                             
         CLC   TIQPEND,MDREND                                                   
         JH    VR24                                                             
                                                                                
         MVC   MEDLIMIT,MDRLIMIT   MEDICARE LIMIT                               
         ICM   R1,15,MDRRATEU                                                   
         CVD   R1,DUB                                                           
         ZAP   MEDRATE,DUB         RATE                                         
         ICM   R1,15,MDRRATEO                                                   
         CVD   R1,DUB                                                           
         ZAP   MEDRATEO,DUB        RATE OVER LIMIT                              
         J     VKX                                                              
                                                                                
VR24     AHI   RF,MDRATELQ                                                      
         J     VK22                                                             
         DROP  RF                                                               
                                                                                
VKX      J     XIT                                                              
                                                                                
***********************************************************************         
*        ERRORS                                                       *         
***********************************************************************         
                                                                                
ERINV    MVI   ERROR,INVALID                                                    
         J     EREND                                                            
                                                                                
ERMIS    MVI   ERROR,MISSING                                                    
         J     EREND                                                            
                                                                                
EREND    GOTO1 ERREX                                                            
                                                                                
ERRTMSG  MVC   CONHEAD(L'RTMSG),RTMSG                                           
         B     USERERR                                                          
                                                                                
RTMSG    DC    C'** ERROR ** Rate missing for period, contact Mediaoceax        
               n'                                                               
                                                                                
USERERR  MVI   ERROR,X'FE'                                                      
         L     R2,EFHWHEN                                                       
         GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
         J     XIT                                                              
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PR       NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
                                                                                
         CLI   ACTEQU,ACTDOWN                                                   
         JE    PREP05                                                           
         LA    R1,MYSPECS          SET A(SPECS) FOR PRINTING                    
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
                                                                                
PREP05   MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
                                                                                
         XC    MYCOUNT,MYCOUNT                                                  
         XC    CKCURR,CKCURR                                                    
         XC    CKCTMED,CKCTMED                                                  
         XC    CKPREV,CKPREV                                                    
         XC    CKPTMED,CKPTMED                                                  
         XC    PREVVAL(PREVVALQ),PREVVAL                                        
                                                                                
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKPCD,TLCKECDQ                                                 
         GOTO1 HIGH                                                             
         J     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   CLC   KEY(TLCKESSN-TLCKPCD),KEYSAVE                                    
         JE    *+12                                                             
         BAS   RE,TOTALS                                                        
         J     PRX                                                              
                                                                                
         LA    R3,KEY                                                           
                                                                                
         CLC   TLCKEEMP,TGEMP      REQUESTED EMPLOYER?                          
         JNE   PREP10                                                           
                                                                                
         CLI   TLCKEDTE,0                                                       
         JE    PREP10                                                           
         MVC   TGDATE,TLCKEDTE                                                  
         XC    TGDATE,=3X'FF'                                                   
         CLC   TGDATE,TIQPSTR      REQUESTED PERIOD RANGE?                      
         JL    PREP10                                                           
         CLC   TGDATE,TIQPEND                                                   
         JH    PREP10                                                           
                                                                                
         OC    TGAGY,TGAGY                                                      
         JZ    *+14                                                             
         CLC   TLCKEAGY,TGAGY      REQUESTED AGENCY?                            
         JNE   PREP10                                                           
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         MVI   TGBYTE,0                                                         
         MVI   BYTE,0                                                           
                                                                                
         OC    TGCLI,TGCLI                                                      
         JZ    PREP30                                                           
         USING TAPDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAPDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
         CLI   TAPDW4TY,C'I'       INDIVIDUAL?                                  
         JE    *+12                                                             
         CLI   TAPDW4TY,C'E'       ESTATE?                                      
         JNE   PREP10                                                           
         CLC   TAPDCLI,TGCLI       REQUESTED CLIENT?                            
         JNE   PREP10                                                           
                                                                                
         USING TACDD,R4                                                         
PREP30   L     R4,AIO              ONLY CHECKS THAT HAVE RUN                    
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
*        OC    TACDCHK,TACDCHK     TRANSFERS DON'T HAVE CHECK NUMS              
*        JZ    PREP10                                                           
         OC    TACDDTE,TACDDTE                                                  
         JZ    PREP10                                                           
         CLC   TACDDTE,TIQPSTR     REQUESTED PERIOD RANGE?                      
         JL    PREP10                                                           
         CLC   TACDDTE,TIQPEND                                                  
         JH    PREP10                                                           
                                                                                
         USING TAYED,R4                                                         
PREP40   MVI   ELCODE,TAYEELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAYETCHK))                                     
         BNE   PREP10                                                           
         L     R4,TGELEM                                                        
                                                                                
PREP50   MVC   CKCURR,TAYEMED                                                   
         MVC   CKCTMED,TAYETMED                                                 
                                                                                
         CLC   PREVKEY,KEY         SAME SSN+CURR+EMP?                           
         JE    PREP60                                                           
                                                                                
         BAS   RE,TOTALS           RETURN TOTALS                                
                                                                                
PREP60   BAS   RE,CALCMED          CALCULATE MEDICARE                           
         J     PREP10                                                           
                                                                                
PRX      MVC   OLINSSN,=CL9'TOTALS'                                             
         MVC   OLINFNAM,SPACES                                                  
         MVC   OLINLNAM,SPACES                                                  
         EDIT  TOTUEARN,OLINUDIF,2,MINUS=YES,ZERO=NOBLANK                       
         EDIT  TOTUTAX,OLINUTAX,2,MINUS=YES,ZERO=NOBLANK                        
         EDIT  TOTOEARN,OLINODIF,2,MINUS=YES,ZERO=NOBLANK                       
         EDIT  TOTOTAX,OLINOTAX,2,MINUS=YES,ZERO=NOBLANK                        
         BAS   RE,PRNTIT                                                        
         J     XIT                                                              
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H2+10(L'MEDEMP),MEDEMP                                           
         MVC   H2+13(L'MEDEMPN),MEDEMPN                                         
         MVC   H2+52(L'MEDPER),MEDPER                                           
         J     XIT                                                              
*                                                                               
MEDLIMIT DS    F                   MEDICARE LIMIT                               
MEDRATE  DS    PL2                 MEDICARE TAX RATE UNDER LIMIT                
MEDRATEO DS    PL2                 MEDICARE TAX RATE OVER LIMIT                 
*                                                                               
CKPREV   DS    F                                                                
CKCURR   DS    F                                                                
CKPTMED  DS    F                                                                
CKCTMED  DS    F                                                                
SVDIFF   DS    F                                                                
SVTAX    DS    F                                                                
*                                                                               
MEDVALS  DS    0X                                                               
MEDYTD   DS    F                                                                
MEDEARN  DS    F                                                                
MEDUEARN DS    F                                                                
MEDOEARN DS    F                                                                
MEDVALSQ EQU   *-MEDVALS                                                        
*                                                                               
TOTUEARN DS    PL16                                                             
TOTUTAX  DS    PL16                                                             
TOTOEARN DS    PL16                                                             
TOTOTAX  DS    PL16                                                             
*                                                                               
TOTFLAG  DS    XL1                                                              
TOTCROSS EQU   X'80'               CROSSES OVER MED LIMIT                       
*                                                                               
OLINSSN  DS    CL9                                                              
OLINLNAM DS    CL16                                                             
OLINFNAM DS    CL16                                                             
OLINUDIF DS    CL15                                                             
OLINUTAX DS    CL15                                                             
OLINODIF DS    CL15                                                             
OLINOTAX DS    CL15                                                             
*                                                                               
PREVVAL  DS    0X                                                               
PREVSSN  DS    CL9                 SSN                                          
PREVPID  DS    CL6                 PID                                          
PREVCUR  DS    CL1                 CURRENCY                                     
PREVEMP  DS    CL3                 EMPLOYER                                     
PREVKEY  DS    CL14                                                             
PREVFNAM DS    CL16                FIRST NAME                                   
PREVLNAM DS    CL16                LAST NAME                                    
PREVVALQ EQU   *-PREVVAL                                                        
*                                                                               
SAVEKEY  DS    XL32                                                             
*                                                                               
MDRATES  DC    X'01312D00',X'00000091',X'0000005A',X'000000',X'B21231'          
         DC    X'01312D00',X'00000091',X'0000005A',X'B30101',X'B31231'          
         DC    X'01312D00',X'00000091',X'0000005A',X'B40101',X'B41231'          
         DC    X'01312D00',X'00000091',X'0000005A',X'B50101',X'B51231'          
         DC    X'01312D00',X'00000091',X'0000005A',X'B60101',X'B61231'          
         DC    X'01312D00',X'00000091',X'0000005A',X'B70101',X'B71231'          
*        DC    X'01312D00',X'00000091',X'0000005A',X'B80101',X'B81231'          
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
GETNAME  NTR1                                                                   
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   SAVEKEY,KEY                                                      
                                                                                
         USING TLW4D,R3                                                         
         LA    R3,KEY                                                           
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ                                                   
         MVC   TLW4SSN,PREVSSN                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLW4KEY),KEYSAVE                                           
         JNE   GETNAMEX                                                         
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TAW4D,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   GETNAMEX                                                         
         MVC   PREVFNAM,TAW4NAM1                                                
         MVC   PREVLNAM,TAW4NAM2                                                
         DROP  R4                                                               
*                                                                               
GETNAMEX MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         XC    KEY,KEY                                                          
         MVC   KEY(L'SAVEKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         J     XIT                                                              
                                                                                
TOTALS   NTR1                                                                   
         OC    MEDEARN(MEDVALSQ-4),MEDEARN                                      
         JZ    TOTALSX                                                          
                                                                                
         BAS   RE,GETNAME          GET FIRST/LAST NAME                          
                                                                                
         MVC   OLINSSN,PREVSSN                                                  
         MVC   OLINFNAM,PREVFNAM                                                
         MVC   OLINLNAM,PREVLNAM                                                
                                                                                
         MVC   MEDUEARN,MEDEARN                                                 
         XC    MEDOEARN,MEDOEARN                                                
*&&DO                                                                           
         L     R1,MEDEARN                                                       
         C     R1,MEDLIMIT                                                      
         BNH   TOT05                                                            
         S     R1,MEDLIMIT                                                      
         ST    R1,MEDOEARN                                                      
         MVC   MEDUEARN,MEDLIMIT                                                
*&&                                                                             
*=========================*                                                     
* AS PER TALENT, APR 2015 *                                                     
*=========================*                                                     
         L     R1,MEDYTD                                                        
         C     R1,MEDLIMIT                                                      
         BNH   TOT05                                                            
         S     R1,MEDLIMIT                                                      
         ST    R1,MEDOEARN                                                      
         C     R1,MEDEARN                                                       
         BNH   *+10                                                             
         MVC   MEDOEARN,MEDEARN                                                 
         MVC   MEDUEARN,MEDLIMIT                                                
                                                                                
TOT05    L     R1,MEDEARN                                                       
         ST    R1,FULL                                                          
                                                                                
         CVD   R1,DUB                                                           
         ZAP   PL16,DUB                                                         
         MP    PL16,MEDRATE                                                     
         DP    PL16,=PL3'10000'                                                 
         ZAP   TAXP,PL16(13)                                                    
         CP    PL16+13(3),=P'5000'                                              
         JL    TOT10                                                            
         AP    TAXP,=P'1'                                                       
                                                                                
TOT10    EDIT  FULL,OLINUDIF,2,MINUS=YES,ZERO=NOBLANK                           
         EDIT  TAXP,OLINUTAX,2,MINUS=YES,ZERO=NOBLANK                           
                                                                                
         L     R1,FULL                                                          
         CVD   R1,DUB                                                           
         AP    TOTUEARN,DUB                                                     
         AP    TOTUTAX,TAXP                                                     
                                                                                
         L     R1,MEDOEARN                                                      
         CVD   R1,DUB                                                           
         ZAP   PL16,DUB                                                         
         MP    PL16,MEDRATEO                                                    
         DP    PL16,=PL3'10000'                                                 
         ZAP   TAXP,PL16(13)                                                    
         CP    PL16+13(3),=P'5000'                                              
         JL    TOT20                                                            
         AP    TAXP,=P'1'                                                       
                                                                                
TOT20    EDIT  MEDOEARN,OLINODIF,2,MINUS=YES,ZERO=NOBLANK                       
         EDIT  TAXP,OLINOTAX,2,MINUS=YES,ZERO=NOBLANK                           
                                                                                
         L     R1,MEDOEARN                                                      
         CVD   R1,DUB                                                           
         AP    TOTOEARN,DUB                                                     
         AP    TOTOTAX,TAXP                                                     
                                                                                
         BAS   RE,PRNTIT                                                        
                                                                                
         XC    PREVVAL(PREVVALQ),PREVVAL                                        
         XC    MEDVALS(MEDVALSQ),MEDVALS                                        
                                                                                
TOTALSX  J     XIT                                                              
                                                                                
TAXP     DS    PL16                                                             
PL16     DS    PL16                                                             
                                                                                
CALCMED  NTR1                                                                   
         USING TLCKPD,R3                                                        
         LA    R3,KEY                                                           
         MVC   PREVSSN,TLCKESSN                                                 
         GOTO1 SSNPACK,DMCB,PREVSSN,PREVPID                                     
         MVC   PREVCUR,TLCKECUR                                                 
         MVC   PREVEMP,TLCKEEMP                                                 
         MVC   PREVKEY,KEY                                                      
                                                                                
         OC    MEDEARN(MEDVALSQ-4),MEDEARN                                      
         JNZ   *+10                                                             
         MVC   MEDYTD,CKCURR                                                    
                                                                                
         L     RF,MEDEARN                                                       
         A     RF,CKCTMED                                                       
         ST    RF,MEDEARN                                                       
                                                                                
         TM    CKCURR,X'80'        NEGATIVE AMOUNT?                             
         JO    *+14                                                             
         CLC   CKCURR,MEDLIMIT     < YTD MEDICARE EARNINGS LIMIT?               
         JNL   CMED50                                                           
                                                                                
         L     RF,CKCTMED          CURRNET CHECK'S MEDICARE EARNINGS            
         OC    CKPREV,CKPREV       FIRST CHECK?                                 
         JZ    CMED10                                                           
         L     RF,CKPTMED          PREVIOUS CHECK'S MEDICARE EARNINGS           
         L     RE,CKCTMED          CURRENT CHECK'S MEDICARE EARNINGS            
                                                                                
         SR    RF,RE                                                            
CMED10   ST    RF,SVDIFF                                                        
                                                                                
         L     RF,MEDUEARN                                                      
         L     RE,SVDIFF                                                        
         AR    RF,RE                                                            
         ST    RF,MEDUEARN                                                      
         J     CALCMEDX                                                         
                                                                                
CMED50   MVI   TOTFLAG,0                                                        
         L     RF,CKCURR           CURRENT CHECK'S YTD MED EARNINGS             
         L     RE,CKCTMED          CURRENT CHECK'S MEDICARE EARNINGS            
         SR    RF,RE                                                            
                                                                                
         MVC   SVDIFF,CKCTMED                                                   
         C     RF,MEDLIMIT         < MEDICARE LIMIT?                            
         JH    CMED60                                                           
         OI    TOTFLAG,TOTCROSS    CROSSES MEDICARE LIMIT                       
                                                                                
         L     RF,CKCURR           CURRENT CHECK'S YTD MED EARNINGS             
         L     RE,MEDLIMIT         MEDICARE LIMIT                               
         SR    RF,RE                                                            
         ST    RF,SVDIFF                                                        
                                                                                
CMED60   L     RF,MEDOEARN                                                      
         L     RE,SVDIFF                                                        
         AR    RF,RE                                                            
         ST    RF,MEDOEARN                                                      
                                                                                
         TM    TOTFLAG,TOTCROSS    CROSSED MEDICARE LIMIT?                      
         JZ    CALCMEDX                                                         
         L     RF,CKCURR           CURRENT CHECK'S YTD MED EARNINGS             
         L     RE,CKCTMED          CURRENT CHECK'S MEDICARE EARNINGS            
         SR    RF,RE                                                            
                                                                                
         L     RE,MEDLIMIT                                                      
         SR    RE,RF                                                            
         ST    RE,SVDIFF                                                        
                                                                                
         L     RF,MEDUEARN                                                      
         L     RE,SVDIFF                                                        
         AR    RF,RE                                                            
         ST    RF,MEDUEARN                                                      
                                                                                
CALCMEDX XC    CKCURR,CKCURR                                                    
         XC    CKCTMED,CKCTMED                                                  
         J     XIT                                                              
                                                                                
PRNTIT   NTR1                                                                   
         CLI   ACTEQU,ACTDOWN                                                   
         JE    PRNT10                                                           
                                                                                
         LA    R2,P                                                             
         USING PLINED,R2                                                        
                                                                                
         MVC   PLINSSN,OLINSSN                                                  
         MVC   PLINFNAM,OLINFNAM                                                
         MVC   PLINLNAM,OLINLNAM                                                
         MVC   PLINUDIF,OLINUDIF                                                
         MVC   PLINUTAX,OLINUTAX                                                
         MVC   PLINODIF,OLINODIF                                                
         MVC   PLINOTAX,OLINOTAX                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
         DROP  R2                                                               
                                                                                
PRNT10   GOTO1 OUTPDOWN,DMCB,(C'T',OLINSSN),L'OLINSSN                           
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINLNAM),L'OLINLNAM                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINFNAM),L'OLINFNAM                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINUDIF),L'OLINUDIF                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINUTAX),L'OLINUTAX                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINODIF),L'OLINODIF                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OLINOTAX),L'OLINOTAX                         
         BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
***********************************************************************         
*        ROUTINE TRACES RECORD GETS/PUTS/ADDS                         *         
*        ON ENTRY ... P1 = A(TRACE LABEL)                             *         
***********************************************************************         
                                                                                
PTRACE   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         MVI   FORCEHED,C'N'                                                    
         GOTO1 TRACE,DMCB,AIO,0,0(R2),(0,7)                                     
         MVI   FORCEHED,C'Y'                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R3=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
*                                                                               
         CLI   ACTEQU,ACTDOWN                                                   
         JNE   XIT                                                              
         MVC   OUTFLD,=CL10'SSN'                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',OUTFLD),L'OUTFLD                             
         MVC   OUTFLD,=CL10'LAST NAME'                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',OUTFLD),L'OUTFLD                             
         MVC   OUTFLD,=CL10'FIRST NAME'                                         
         GOTO1 OUTPDOWN,DMCB,(C'T',OUTFLD),L'OUTFLD                             
         MVC   WORK(15),=CL15'MED WAGES 1.45%'                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),15                                     
         MVC   WORK(15),=CL15'MED TAXES 1.45%'                                  
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),15                                     
         MVC   WORK(14),=CL14'MED WAGES 0.9%'                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),14                                     
         MVC   WORK(14),=CL14'MED TAXES 0.9%'                                   
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),14                                     
         BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
MYSPECS  SSPEC H1,2,RUN                                                         
         SSPEC H1,55,C'MED09 REPORT'                                            
         SSPEC H1,100,REPORT                                                    
         SSPEC H1,120,PAGE                                                      
         SSPEC H2,2,C'EMPLOYER'                                                 
         SSPEC H2,100,REQUESTOR                                                 
         SSPEC H7,1,C'   SSN        LAST NAME         FIRST NAME'               
         SSPEC H7,48,C'MED WAGES 1.45%  MED TAXES 1.45%'                        
         SSPEC H7,82,C'MED WAGES 0.9%   MED TAXES 0.9%'                         
         SSPEC H8,1,C'---------  ----------------  ----------------'            
         SSPEC H8,48,C'---------------  ---------------'                        
         SSPEC H8,82,C'---------------  ---------------'                        
         DC    H'0'                                                             
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
MYCOUNT  DS    F                                                                
MYCOUNT2 DS    F                                                                
OUTFLD   DS    CL10                                                             
OUTFLD2  DS    CL15                                                             
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPDDD                                                       
         EJECT                                                                  
*                                                                               
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
*                                                                               
MDRATESD DSECT                                                                  
MDRLIMIT DS    XL4                 MEDICARE CUTOFF LIMIT                        
MDRRATEU DS    XL4                 RATE                                         
MDRRATEO DS    XL4                 RATE OVER LIMIT                              
MDRSTR   DS    XL3                 START DATE                                   
MDREND   DS    XL3                 END DATE                                     
MDRATELQ EQU   *-MDRLIMIT                                                       
*                                                                               
PLINED   DSECT                                                                  
PLINSSN  DS    CL9                                                              
         DS    CL2                                                              
PLINLNAM DS    CL16                                                             
         DS    CL2                                                              
PLINFNAM DS    CL16                                                             
         DS    CL3                                                              
PLINUDIF DS    CL15                                                             
         DS    CL2                                                              
PLINUTAX DS    CL15                                                             
         DS    CL2                                                              
PLINODIF DS    CL15                                                             
         DS    CL2                                                              
PLINOTAX DS    CL15                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010TAREP79   02/10/17'                                      
         END                                                                    
