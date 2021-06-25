*          DATA SET SPSI2GEN   AT LEVEL 010 AS OF 05/28/19                      
*          DATA SET SPSI2GEN   AT LEVEL 011 AS OF 10/23/14                      
*PHASE SPSI2GNA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRTREC                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOADER                                                                 
*INCLUDE CALLOFF                                                                
*INCLUDE GETPROF                                                                
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE XSORT                                                                  
                                                                                
*=============================================================                  
* INPUT CARDS:                                                                  
* DSPACE=   REQUIRED AND MUST BE FIRST                                          
* DDSIO=                                                                        
* ORIG=     PRINCIPAL ID OF AGENCY REQUESTED                                    
* TRACE=Y/N                                                                     
* I2NPROF=                                                                      
* Z5PROF=                                                                       
* TODAY=     SO CAN FIND ACTIVITY IN NON-CURRENT PERIOD                         
*---------------------------------------------                                  
*=============================================================                  
         TITLE 'SPSI2GEN - GENERATE SI2 T/A REQUEST CARDS'                      
SPSI2GEN CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         ENTRY COMFACS                                                          
*                                                                               
         NBASE 0,SPSI2GEN,=V(REGSAVE)                                           
*                                                                               
         LARL  RC,SPSI2WKD                                                      
         USING SPSI2WKD,RC                                                      
         ST    RD,XBASERD                                                       
*                                                                               
         LARL  RF,CLTREC                                                        
         ST    RF,ADCLT                                                         
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         LA    RE,SPSI2GEN          SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         MVC   DUB(8),=CL8'T00A15' LOAD CLUNPK                                  
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VCLUNPK,4(R1)                                                    
*                                                                               
         MVC   DUB(8),=CL8'T00A1D' LOAD GETBROAD                                
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VGETBRD,4(R1)                                                    
*                                                                               
         MVC   DUB(8),=CL8'T00A38' LOAD OFFICER                                 
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VOFFICER,4(R1)                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A7A' LOAD STAPACK                                 
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VSTAPACK,4(R1)                                                   
*                                                                               
         MVC   DUB(8),=CL8'T00A9E' LOAD CABLETAB                                
         GOTO1 =V(LOADER),DMCB,DUB,0                                            
         MVC   VT00A9E,4(R1)                                                    
*                                                                               
         LARL  R1,STAWORK          PASS A(T00A9E) TO STAPACK                    
         XC    0(L'STAWORK,R1),0(R1)                                            
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,QSTP_T00A9E                                              
         MVC   STAPACOM,VT00A9E                                                 
         GOTO1 VSTAPACK,(R1)                                                    
         DROP  R1                                                               
         EJECT                                                                  
*==================================================================*            
*  READ PARAMETER CARDS                                                         
*==================================================================*            
                                                                                
NXTCRD   BAS   RE,GETCARD                                                       
         JNE   ENDCARD                                                          
*                                                                               
NXTCRD1  CLC   =C'DDSIO=',CARD                                                  
         BNE   NXTCRD2                                                          
         L     RE,=V(DDSIO)                                                     
         MVC   0(8,RE),CARD+6                                                   
         B     NXTCRD                                                           
*                                                                               
         USING SSBOFFD,RE                                                       
NXTCRD2  CLC   =C'DSPACE=',CARD                                                 
         BNE   NXTCRD4                                                          
         L     RE,=V(SSB)                                                       
         MVC   SSODSPAC,CARD+7                                                  
                                                                                
* OPEN CTFILE IN CONTROL SYSTEM SO CAN READ IT                                  
* NOW THAT WE KNOW WHAT DATASPACE TO USE                                        
                                                                                
         LARL  RE,UTL                                                           
         MVI   4(RE),X'0A'                                                      
         LA    R0,CTFLIST                                                       
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'CONTROL',(R0)                     
         J     NXTCRD                                                           
*                                                                               
CTFLIST  DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         B     NXTCRD                                                           
         DROP  RE                                                               
*                                                                               
NXTCRD4  CLC   =C'TRACE=',CARD                                                  
         BNE   NXTCRD6                                                          
         CLI   CARD+6,C'Y'                                                      
         BNE   *+8                                                              
         OI    TRACEFLG,X'01'                                                   
         B     NXTCRD                                                           
*                                                                               
NXTCRD6  CLC   CARD(6),=C'TODAY='                                               
         JNE   NXTCRD8                                                          
         GOTO1 VDATCON,DMCB,CARD+6,QTODAY   GET DDS FORM OF YYMMDD              
         J     NXTCRD                                                           
*                                                                               
NXTCRD8  CLC   =C'PROFILE=DISK',CARD                                            
         BNE   NXTCRD10                                                         
*                                                                               
         LARL  RF,SSB                                                           
         USING SSBOFFD,RF                                                       
         LARL  RE,MASTC                                                         
         ST    RE,SSOMASTC                                                      
*                                                                               
         USING MASTC,RE                                                         
         OI    MCRFLAG1,MCRPROF                                                 
         J     NXTCRD                                                           
         DROP  RE,RF                                                            
*                                                                               
NXTCRD10 CLC   =C'I2NPROF=',CARD                                                
         JNE   NXTCRD12                                                         
         MVI   OVERI2N,C'Y'                                                     
         MVC   SVI2PROF,CARD+8                                                  
         J     NXTCRD                                                           
*                                                                               
NXTCRD12 CLC   =C'Z5PROF=',CARD                                                 
         JNE   GETSYS                                                           
         MVC   SVZ5PROF,CARD+8                                                  
         MVI   OVERZ5,C'Y'                                                      
         J     NXTCRD                                                           
*                                                                               
GETSYS   DS    0H                                                               
         CLC   =C'ORIG=',CARD                                                   
         JNE   *+2                                                              
*                                                                               
GETORIG  MVC   SVORIG,CARD+5       SAVE FIRST ORIG=                             
*                                                                               
         BAS   RE,GETIDREC                                                      
                                                                                
* FIND A SYSTEM ELEM TO GET AGYCODE                                             
                                                                                
         LARL  R6,RDATA                                                         
         LA    R6,28(R6)                                                        
*                                                                               
         MVI   ELCDLO,X'06'                                                     
         MVI   ELCDHI,X'06'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
*                                                                               
         USING CTAGYD,R6                                                        
         MVC   SVAGYA,CTAGYID                                                   
         DROP  R6                                                               
*                                                                               
         LARL  R6,RDATA                                                         
         LA    R6,28(R6)                                                        
         USING CTSYSD,R6                                                        
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
*                                                                               
GETOR4   BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         CLI   CTSYSNUM,2          TEST SPOT                                    
         JNE   GETOR4                                                           
*                                                                               
         MVC   SVAGYB,CTSYSAGB                                                  
         MVC   SVSYSSE,CTSYSSE     SAVE SPOT SYSTEM NUMBER                      
         J     NXTCRD                                                           
                                                                                
*============================================================                   
*============================================================                   
*============================================================                   
* DEST= AND ADDITIONAL ORIG= CARD NO LONGER SUPPORTED                           
*============================================================                   
*============================================================                   
*============================================================                   
                                                                                
         LARL  R4,IDTAB                                                         
         LHI   R5,20                                                            
*                                                                               
         MVC   0(10,R4),SVORIG     MOVE ORIGIN ID TO TABLE ENTRY                
**NOP**  MVC   10(10,R4),=CL10'DDS'     SET DEFAULT DEST                        
**NOP**  MVC   20(4,R4),SVSYSLMT        AND LIMIT ACCESS CODE                   
         J     GETOR20                                                          
         DROP  R6                                                               
*                                                                               
GETOR10  BAS   RE,GETCARD          GET NEXT POSSIBLE ORIG=                      
         JNE   ENDCARD             IF EOD, STOP NOW                             
*                                                                               
         CLC   =C'ORIG=',CARD      TEST ANOTHER ORIGIN=                         
         JNE   NXTCRD1                                                          
*                                                                               
GETOR12  BAS   RE,GETIDREC                                                      
*                                                                               
         USING CTSYSD,R6                                                        
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
         LARL  R6,RDATA                                                         
         LA    R6,28(R6)                                                        
*                                                                               
GETOR14  BRAS  RE,NEXTEL                                                        
         JNE   *+2                                                              
         CLI   CTSYSNUM,2          TEST SPOT                                    
         JNE   GETOR14                                                          
*                                                                               
         MVC   0(10,R4),CARD+5     MOVE ORIGIN TO TABLE                         
         MVC   20(4,R4),CTSYSLMT   AND LIMIT ACCESS CODE                        
*                                                                               
GETOR20  BAS   RE,GETCARD          GET DEST= CARD OF PAIR                       
         JNE   ENDCARD             IF EOD, DONE                                 
*                                                                               
         CLC   =C'ORIG=',CARD      TEST IF NEW ORIG= CARD                       
         JNE   GETOR22                                                          
         LA    R4,24(R4)           NEXT ENTRY                                   
         JCT   R5,GETOR12                                                       
         DC    H'0'                                                             
*                                                                               
GETOR22  CLC   =C'DEST=',CARD                                                   
         JNE   NXTCRD1                                                          
*                                                                               
         MVC   10(10,R4),CARD+5    MOVE DEST TO TABLE                           
         CLC   =C'DDS',CARD+5                                                   
         JE    GETOR24                                                          
*                                                                               
         BAS   RE,GETIDREC                                                      
*                                                                               
GETOR24  LA    R4,24(R4)                                                        
         JCT   R5,GETOR10                                                       
         DC    H'0'                                                             
*                                                                               
GETIDREC NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),CARD+5                                                
         MVC   KEYSAVE,KEY                                                      
         LARL  R6,RDATA                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                  
*                                                                               
         CLC   KEY(25),0(R6)                                                    
         JE    EXIT                                                             
*                                                                               
GETSERR  MVC   P(20),=CL20'INVALID INPUT CARD: '                                
         MVC   P+20(60),CARD                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
ENDPROG  L    RD,XBASERD                                                        
         XBASE                                                                  
*                                                                               
GETCARD  NTR1                                                                   
*                                                                               
GETCARD2 GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
         MVC   P(80),CARD                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLI   CARD,C'*'           TEST CARD NOP                                
         JE    GETCARD2                                                         
         CLC   =C'/*',CARD         SET CC ON EXIT                               
         JE    GETCARD4                                                         
         CR    RB,RB               SET CC EQ IF NOT EOF                         
         J     *+6                                                              
GETCARD4 LTR   RB,RB               SET CC NEQ IF EOF                            
         XIT1                                                                   
         EJECT                                                                  
*=====================================================================          
* OPEN SPOT FILES                                                               
*=====================================================================          
         EJECT                                                                  
ENDCARD  LARL  RF,UTL                                                           
         MVC   4(1,RF),SVSYSSE     SET SYSTEM NUMBER IN UTL                     
*                                                                               
         LARL  R0,RDATA                                                         
         ST    R0,AIO                                                           
         LARL  R0,SPTFLIST                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMOPEN',=C'SPOT',(R0),AIO                    
*                                                                               
RAGY     LA    R2,KEY                                                           
         USING AGYHDRD,R2          READ AGENCY HEADER                           
         XC    KEY,KEY                                                          
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,SVAGYA                                                   
         DROP  R2                                                               
*                                                                               
         BRAS  RE,HIGH                                                          
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCDLO,X'01'                                                     
         MVI   ELCDHI,X'01'                                                     
         LA    R6,24(R6)                                                        
         BRAS  RE,NEXTEL2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING AGYEL,R6                                                         
         MVC   CTRYSAVE,AGYPCNDA   CANADA                                       
         DROP  R6                                                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         LARL  RE,SORTCARD                                                      
         ST    RE,DMCB                                                          
         LARL  RE,RECCARD                                                       
         ST    RE,DMCB+4                                                        
         GOTO1 VSORTER,DMCB       INITIALIZE SORTER                             
*                                                                               
IN0      OPEN  (RECVIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         OPEN  (REQOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         LARL  R1,STAWORK          FINISH STAPACK INIT NOW                      
         USING STAPACKD,R1                                                      
*                                                                               
         MVI   STAPACT,C'U'        SET FOR UNPACK CALLS                         
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,SVAGYA                                                   
         MVC   STAPCTRY,CTRYSAVE                                                
         DROP  R1                                                               
                                                                                
* SO FEB21/19 GOES BACK A YEAR TO FEB/18                                        
                                                                                
         CLI   QTODAY,C' '         ANY DATE OVERRIDE                            
         JH    IN1                 YES                                          
         GOTO1 VDATCON,DMCB,(5,0),QTODAY       GET TODAY YYMMDD                 
*                                                                               
IN1      MVC   WORK(4),QTODAY                                                   
         MVC   WORK+4(2),=C'15'    INSURE DATE IN RIGHT MONTH                   
         SR    R0,R0                                                            
         BCTR  R0,0                GO BACK A YEAR                               
         GOTO1 VADDAY,DMCB,(C'Y',WORK),DUB,(R0)                                 
                                                                                
* NOW GET START OF THAT BROADCAST MONTH                                         
                                                                                
         MVC   DUB+4(2),=C'15'                                                  
         GOTO1 VGETBRD,DMCB,(X'01',DUB),WORK+6,VGETDAY,VADDAY                   
*                                                                               
         MVC   DUB,WORK+6                                                       
         GOTO1 VDATCON,DMCB,DUB,(3,QSTARTB)                                     
         GOTO1 (RF),(R1),DUB,(2,QSTARTP)                                        
                                                                                
* NOW GET BROADCAST MONTH DATES                                                 
* TABLE HAS 2-BYTE PACKED BRDMON DATES/4 BYTE EBCDIC YYMM                       
                                                                                
         MVC   WORK(6),DUB                                                      
         LA    R4,DATELIST                                                      
         XC    DATELIST,DATELIST                                                
*                                                                               
* WORK HAS YYMM15 OF FIRST BROADCAST MONTH                                      
*                                                                               
IN2      GOTO1 VGETBRD,DMCB,(X'01',WORK),WORK+6,VGETDAY,VADDAY                  
                                                                                
* WORK+6 HAS BRDMON START, WORK+12 HAS BRDMON END                               
                                                                                
         GOTO1 VDATCON,DMCB,WORK+6,(2,0(R4))                                    
         GOTO1 (RF),(R1),WORK+12,(2,2(R4))                                      
         MVC   4(4,R4),WORK+12          SAVE END BRDMON YYMM IN TABLE           
         LA    R4,8(R4)                                                         
*                                                                               
         MVC   DUB(4),WORK+12      YYMM OF END BRDMON                           
         MVC   DUB+4(2),=C'15'                                                  
         GOTO1 VADDAY,DMCB,(C'M',DUB),WORK,1                                    
         CLC   WORK(6),QTODAY                                                   
         JL    IN2                                                              
         MVC   0(4,R4),=F'-1'      SET EOT FLAG                                 
         EJECT                                                                  
*============================================================                   
*** PROCESS INPUT FILE ***                                                      
*============================================================                   
                                                                                
QSPTFIL  EQU   X'21'                                                            
QCOPY    EQU   1                                                                
QCHG     EQU   2                                                                
QADD     EQU   3                                                                
*                                                                               
IN10     LA    R0,DM$RECVHDR-4                                                  
         LARL  R1,RECVIN                                                        
         GET   (1),(0)                                                          
         AP    SORTCNT,=P'1'                                                    
*                                                                               
         CLI   DM$RRECTY,QCOPY     IS REC A COPY?                               
         JE    *+12                                                             
         CLI   DM$RRECTY,QADD      IS REC AN ADD?                               
         JNE   IN10                                                             
*                                                                               
         LA    RE,DM$RECVHDR-4     POINT TO RECORD LENGTH                       
         AH    RE,0(RE)            ADD LENGTH                                   
         XC    0(2,RE),0(RE)       CLEAR EOR                                    
*                                                                               
         CLI   DM$RFILTY,QSPTFIL   SPTFILE ?                                    
         JNE   IN10                                                             
*                                                                               
         LA    R2,DM$RECVHDR+24    R2 -> DATA RECORD.DO NOT USE R2 !!!          
         USING BUYRECD,R2                                                       
*                                                                               
         CLI   0(R2),X'10'         TEST BUYREC                                  
         JL    IN10                NO                                           
         TM    0(R2),X'08'         TEST FOR 'COPY' BIT                          
         JO    IN10                YES-SKIP THIS RECORD                         
* FILTER ON AGENCY                                                              
         LLC   R0,0(R2)                                                         
         N     R0,=X'000000F0'     DROP MEDIA                                   
         CLM   R0,1,SVAGYB         RIGHT AGENCY                                 
         JNE   IN10                                                             
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         JE    IN20                                                             
         MVC   P(10),=CL10'REC='                                                
         GOTO1 =V(HEXOUT),DMCB,0(R2),P+10,13,=C'N'                              
         GOTO1 =V(PRINTER)                                                      
         J     IN20                                                             
*                                                                               
ENDIN    BRAS  RE,GETREQ                                                        
         L     RD,XBASERD                                                       
         XBASE                                                                  
         EJECT                                                                  
*==============================================================                 
* BUY RECORD PROCESSING                                                         
*==============================================================                 
                                                                                
IN20     TM    BDCIND2,BDCCANAQ    CANADIAN BUY?                                
         JZ    IN22                                                             
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'          DROP AGY                                     
         CLI   BYTE,X'03'          TEST NETWORK                                 
         JNE   IN22                                                             
         OC    BUYKMKTN,BUYKMKTN   ONLY DO MARKET 0 BUYS FOR NETWORK            
         JNZ   IN10                                                             
*                                                                               
IN22     CLC   BDEND,QSTARTB       BUY END BEFORE REQUEST PERIOD                
         JL    IN10                                                             
*                                                                               
IN30     BRAS  RE,BLDTAB           EXTRACT DATA FROM ADD/COPY                   
         MVC   BAGYMCL,BUYKAM      SAVE A-M/CLT                                 
*                                                                               
         CLI   DM$RRECTY,QADD      RECORD ADDED?                                
         JE    IN40                                                             
*                                                                               
IN32     CLI   DM$RRECTY,QCOPY     MUST HAVE BEEN A COPY                        
         JNE   *+2                                                              
*                                                                               
         LA    R0,DM$RECVHDR-4     GET CHANGE                                   
         LARL  R1,RECVIN                                                        
         GET   (1),(0)                                                          
         CLI   DM$RRECTY,QCHG                                                   
         JNE   *+2                 UHOH                                         
*                                                                               
         BRAS  RE,BLDTAB           BUILD CHANGE TABLE                           
*                                                                               
IN40     BRAS  RE,COMPTAB          GO SEE WHAT CHANGED                          
*                                                                               
         J     IN10                                                             
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* BUILD TABLE OF SPOTS IN BUY RECORD                                            
* ON ENTRY R2 POINTS TO BUY RECORD                                              
*===============================================================                
                                                                                
         USING BUYRECD,R2                                                       
BLDTAB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LARL  R4,CHGTAB                                                        
         XC    0(BUYTABL,R4),0(R4)   CLEAR FIRST ENTRY                          
         XC    CHGID,CHGID                                                      
*                                                                               
         CLI   DM$RRECTY,QCHG      IS REC A CHANGE                              
         JE    BLDTAB2                                                          
*                                                                               
         LARL  R4,CPYTAB                                                        
         XC    0(BUYTABL,R4),0(R4)   CLEAR FIRST ENTRY                          
         XC    CPYID,CPYID                                                      
*                                                                               
BLDTAB2  LARL  R4,CPYTAB           THIS TABLE FOR ADDS/COPIES                   
         LA    R5,CPYID                                                         
*                                                                               
         CLI   DM$RRECTY,QCHG                                                   
         JNE   BLDTAB4                                                          
*                                                                               
         LARL  R4,CHGTAB           THIS TABLE FOR CHANGES                       
         LA    R5,CHGID                                                         
*                                                                               
BLDTAB4  ST    R4,ABUYTAB               SAVE TABLE ADDRESS                      
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         BRAS  RE,NEXTEL                                                        
         JNE   BLDTAB6                                                          
         MVC   0(12,R5),IDCONNO-IDELEM(R6)                                      
*                                                                               
BLDTAB6  SR    R7,R7               CLEAR TABLE ENTRY COUNTER                    
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,12                                                        
*                                                                               
BLDTAB12 BRAS  RE,NEXTEL                                                        
         JNE   BLDTAB30                                                         
*                                                                               
         USING REGELEM,R6                                                       
         CLC   RDATE,FIRSTBRD      SPOT PRIOR TO FIRST DATE IN TABLE            
         JL    BLDTAB12            SKIP IT                                      
         CLC   RDATE,LASTBRD+2     SPOT AFTER LAST DATE IN TABLE                
         JH    BLDTAB12                                                         
*                                                                               
         USING BUYTABD,R4                                                       
         MVC   BTDATE,RDATE                                                     
****     MVC   BTPAID,RPAY         PER PRUS, NO I2 FOR PAYING SPOT              
         MVC   BTTIME,BDTIMST      MOVE START/END TIMES                         
         MVC   BTSEC,BDSEC         MOVE SPOTLENLN                               
         MVC   BTSPREP,BDREP       MOVE SPECIAL REP                             
*                                                                               
         TM    RSTATUS,RSMINUSQ                                                 
         JZ    *+8                                                              
         MVI   BTMINUS,C'Y'                                                     
*                                                                               
         TM    RSTATUS,RSMINSDQ                                                 
         JZ    *+8                                                              
         MVI   BTMSSD,C'Y'                                                      
*                                                                               
         MVC   BTCOST,BDCOST                                                    
         TM    RSTATUS,RSRATOVQ                                                 
         JZ    *+10                                                             
         MVC   BTCOST,RPCOST                                                    
*                                                                               
         MVI   BTPRD,X'FF'                                                      
         CLI   RLEN,10             TEST ALLOCATED                               
         JNH   BLDTAB14                                                         
         MVC   BTPRD,10(R6)                                                     
         CLI   RLEN,14                                                          
         JNH   BLDTAB14                                                         
         MVC   BTPRD2,14(R6)                                                    
         DROP  R6                                                               
*                                                                               
BLDTAB14 LLC   RE,1(R6)                                                         
         AR    RE,R6                                                            
         CLI   0(RE),X'10'         TEST AFFID FOLLOWS                           
         JNE   BLDTAB20                                                         
         USING AFFELEM,RE                                                       
         MVC   BTAFFDT,ADATE                                                    
         MVC   BTAFFTM,ATIME                                                    
         DROP  RE                                                               
*                                                                               
BLDTAB20 LA    R4,BUYTABL(R4)                                                   
         XC    0(BUYTABL,R4),0(R4) CLEAR NEXT ENTRY                             
         BCTR  R7,0                BUMP COUNTER                                 
         J     BLDTAB12                                                         
                                                                                
* SORT THE ENTRIES BEFORE EXIT                                                  
                                                                                
BLDTAB30 MVC   0(2,R4),=F'-1'      SET EOT FLAG                                 
         LPR   R7,R7                                                            
         JZ    BLDTABX                                                          
         GOTO1 =V(XSORT),DMCB,ABUYTAB,(R7),BUYTABL,BUYTABL,0                    
*                                                                               
BLDTABX  J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* ON ENTRY R2 POINTS TO BUY RECORD                                              
*===============================================================                
                                                                                
         USING BUYRECD,R2                                                       
*                                                                               
COMPTAB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LARL  R4,CPYTAB                                                        
         OC    0(2,R4),0(R4)      TEST ANY ENTRIES                              
         JNZ   *+10                                                             
         MVC   0(2,R4),=F'-1'      SET HIGH ENTRY                               
*                                                                               
         LARL  R5,CHGTAB                                                        
         OC    0(2,R5),0(R5)      TEST ANY ENTRIES                              
         JNZ   *+10                                                             
         MVC   0(2,R5),=F'-1'      SET HIGH ENTRY                               
*                                                                               
         CLC   CPYID,CHGID         TEST CHANGE OF ID                            
         JE    COMP10              NO                                           
         CLI   DM$RRECTY,QADD      RECORD ADDED?                                
         JE    COMP10                                                           
* IF ID CHANGES, NO NEED TO REMATCH UNDER OLD ID SO CLEAR CPYTAB                
         LARL  R4,CPYTAB                                                        
         XC    0(BUYTABL,R4),0(R4)                                              
         MVC   0(2,R4),=F'-1'      SET COPY TAB TO NO ENT                       
                                                                                
*==================================================================             
* COMPARE TABLES AND GENERATE REQUESTS FOR ENTRIES THAT DON'T MATCH             
* ADDS/COPIES ARE ONE TABLE, CHANGES THE OTHER                                  
*==================================================================             
                                                                                
COMP10   CLC   0(BUYTABL,R4),0(R5) ENTRIES MATCH?                               
         JH    COMP12              COPY IS HIGH                                 
         JL    COMP14              COPY IS LOW                                  
         CLI   0(R4),X'FF'         TEST FOR EOT                                 
         JE    COMPX                                                            
         LA    R4,BUYTABL(R4)                                                   
         LA    R5,BUYTABL(R5)                                                   
         J     COMP10                                                           
                                                                                
* COPY IS HIGH - GEN SORT FOR CHANGE ENTRY                                      
                                                                                
COMP12   LR    R1,R5               POINT TO ENTRY                               
         BAS   RE,GETDATE          GET BRDMON DATE IN FULL                      
         MVI   THISREC,X'02'       SET THIS IS CHANGE                           
         BAS   RE,BLDSORT                                                       
         LA    R5,BUYTABL(R5)      NEXT CHANGE ENTRY                            
         J     COMP10                                                           
                                                                                
* COPY/ADD IS LOW - GEN SORT FOR COPY ENTRY                                     
                                                                                
COMP14   LR    R1,R4                                                            
         BAS   RE,GETDATE                                                       
         MVI   THISREC,X'01'                                                    
         BAS   RE,BLDSORT                                                       
         LA    R4,BUYTABL(R4)      NEXT COPY ENTRY                              
         J     COMP10                                                           
*                                                                               
COMPX    J     EXIT                                                             
                                                                                
*========================================================                       
* FIND ENTRY IN DATELIST FOR THIS SPOT                                          
*========================================================                       
                                                                                
GETDATE  NTR1                                                                   
         LA    R6,DATELIST                                                      
*                                                                               
GETDAT2  CLC   BTDATE-BUYTABD(2,R1),0(R6)   SPOT BEFORE BRDMON START            
         JL    GETDAT4                                                          
         CLC   BTDATE-BUYTABD(2,R1),2(R6)   SPOT AFTER BRDMON END               
         JNH   GETDATX                                                          
*                                                                               
GETDAT4  LA    R6,8(R6)                                                         
         CLI   0(R6),X'FF'                                                      
         JNE   GETDAT2                                                          
         J     EXIT                                                             
*                                                                               
GETDATX  MVC   FULL,4(R6)          MOVE YYMM                                    
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* AT ENTRY, R1 POINTS TO BUYTAB ENTRY                                           
* R6 POINTS TO DATETAB ENTRY                                                    
*=========================================================                      
                                                                                
         USING BUYTABD,R1                                                       
BLDSORT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QRECORD,SPACES                                                   
         MVC   QRECORD2,SPACES                                                  
*                                                                               
         MVC   QCODE(2),=C'U2'                                                  
         MVC   QAGY(2),SVAGYA                                                   
         LLC   RE,BUYKAM                                                        
         N     RE,=X'0000000F'     DROP AGY                                     
         LA    RE,MDTAB-1(RE)      MOVE MEDIA CODE                              
         MVC   QMED,0(RE)                                                       
*                                                                               
         BAS   RE,GETCLT                                                        
         MVC   QCLT,CLIENT         MOVE CORRECTED ALPHA/NUM CLIENT              
*                                                                               
         MVC   QUSERID,SVUSERID    AND SET USERID                               
*                                                                               
*&&DO*&& CLI   SKIPCLT,C'Y'                                                     
*&&DO*&& JE    EXIT                                                             
         CLI   SVI2PRF9,C'N'       TEST AUTO I2'S ACTIVE                        
         JNE   BLDSORT1            NO - EXIT                                    
         MVC   SKIPAMC,BAGYMCL                                                  
         J     BLDSORTX                                                         
*                                                                               
BLDSORT1 MVI   QBYID,C'Y'                                                       
         CLI   SVI2PRF8,C'Y'       TEST REQUEST BY ID                           
         JE    BLDSORT2                                                         
         MVI   QBYID,C'N'                                                       
*                                                                               
BLDSORT2 LA    RF,=C'POL'                                                       
         CLI   SVZ5PRF15,C'Y'      TEST REQUEST BY POL                          
         JE    BLDSORT4                                                         
         CLI   BTPRD,X'FF'                                                      
         JE    BLDSORT4                                                         
*                                                                               
         L     RF,ADCLT                                                         
         AHI   RF,CLIST-CLTHDRD                                                 
*                                                                               
BLDSORT3 CLC   BTPRD,3(RF)                                                      
         JE    BLDSORT4                                                         
         AHI   RF,4                                                             
         CLI   0(RF),C'A'                                                       
         JNL   BLDSORT3                                                         
         DC    H'0'                                                             
*                                                                               
BLDSORT4 MVC   QPRD,0(RF)                                                       
*                                                                               
         LAY   R1,STAWORK                                                       
         USING STAPACKD,R1                                                      
         MVC   STAPMED,QMED                                                     
         MVC   STAPMKT(5),BUYKMSTA                                              
*                                                                               
         GOTO1 VSTAPACK,(R1)                                                    
         CLI   STAPERR,0                                                        
         JNE   *+2                                                              
*                                                                               
         MVC   QMKT,STAPQMKT                                                    
         MVC   QSTA,STAPQSTA                                                    
         CLI   QSTA+4,C' '                                                      
         JH    *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         CLI   QSTA,C'0'           TEST CABLE STATION                           
         JL    *+10                NO  -                                        
         MVC   QSTA,SPACES         THEN REMOVE STATION                          
*                                                                               
         CLI   CTRYSAVE,C'C'                                                    
         BNE   BLDSORT6                                                         
         CLI   QSTA+4,C'/'                                                      
         BNE   BLDSORT6                                                         
         MVI   QSTA+4,C' '                                                      
         MVC   QCBLNET,STAPQNET                                                 
         DROP  R1                                                               
*                                                                               
BLDSORT6 MVC   QEST(3),=CL3'NO '                                                
         CLI   SVI2PRF5,C'Y'       TEST RUN BY ESTIMATE                         
         JE    BLDSORT8            NO                                           
*                                                                               
         LLC   R0,BUYKEST          ESTIMATE                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
*                                                                               
BLDSORT8 MVC   WORK(4),FULL                                                     
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 VDATCON,DMCB,WORK,(X'20',WORK+6)                                 
         MVC   QSTART(4),WORK+6     MOS  YYMM                                   
*                                                                               
         MVC   Q2USER(10),THISORIG                                              
         MVC   Q2USER+10(10),THISDEST                                           
*                                                                               
         MVI   QOPT4,C'S'          SET TO PRINT SUMMARIES ONLY                  
         MVC   QUESTOR(10),=C'AUTO U2 RQ'                                       
*                                                                               
         LA    R1,LASTCPY                                                       
         CLI   THISREC,X'01'                                                    
         JE    *+8                                                              
         LA    R1,LASTCHG                                                       
*                                                                               
         CLC   QRECORD(80),0(R1)   TEST SAME AS LAST SORTREC                    
         JE    BLDSORTX                                                         
         MVC   0(80,R1),QRECORD    SAVE LAST SORT RECORD                        
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',QRECORD                                     
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    BLDSORTX                                                         
         MVC   P(10),=CL10'SRT='                                                
         MVC   P+10(100),QRECORD                                                
         GOTO1 =V(PRINTER)                                                      
*                                                                               
BLDSORTX J     EXIT                                                             
         LTORG                                                                  
*                                                                               
MDTAB    DC    C'TRNX***C'                                                      
         EJECT                                                                  
GETREQ   NTR1  BASE=*,LABEL=*                                                   
         CP    SORTCNT,=P'0'       ANY DATA                                     
         JE    GETREQX             NO                                           
         MVC   SVUSERID,SPACES                                                  
*                                                                               
GETREQ2  GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R2,15,DMCB+4                                                     
         BZ    GETREQX             FINISH, IF END OF TABLE                      
*                                                                               
         CLI   TRACEFLG,X'00'                                                   
         BE    GETREQ4                                                          
         MVC   P(10),=CL10'SRT GET='                                            
         MVC   P+10(100),0(R2)                                                  
         GOTO1 =V(PRINTER)                                                      
*                                                                               
GETREQ4  MVC   QBAGYMCL-QAREA(3,R2),SPACES   BLANK OUT HEX DATA                 
*                                                                               
         CLC   SVUSERID,QUSERID-QAREA(R2)    TEST SAME USERID                   
         JNE   GETREQ6                                                          
*                                                                               
         MVC   QUSERID-QAREA(10,R2),SPACES   BLANK IT IN REQ CARD               
         CLC   0(80,R2),QAREA                TEST  SAME REQ                     
         JE    GETREQ2                                                          
         J     GETREQ10                                                         
*                                                                               
GETREQ6  MVC   SVUSERID,QUSERID-QAREA(R2)    SAVE NEW USERID                    
         MVC   QUSERID-QAREA(10,R2),SPACES   BLANK IT IN REQ CARD               
*                                                                               
         MVI   PARMOUT,C' '                                                     
         MVC   PARMOUT+1(79),PARMOUT                                            
         MVC   PARMOUT(11),=C'SET ORIGIN='                                      
         MVC   PARMOUT+11(10),SVUSERID                                          
         LA    R0,PARMOUT                                                       
         BAS   RE,PUTREQ                                                        
         B     GETREQ10                                                         
*==============================================================                 
*==============================================================                 
* CODE BELOW IS NOP - DESTID=ORIGINID ALWAYS                                    
*==============================================================                 
*==============================================================                 
         CLI   Q2USER+10,C' '      TEST NO DEST OVERRIDE                        
         JE    GETREQ10                                                         
         MVC   PARMOUT,SPACES                                                   
         MVC   PARMOUT(16),=C'SET DESTINATION='                                 
         MVC   PARMOUT+16(10),Q2USER+10-QAREA(R2)                               
         LA    R0,PARMOUT                                                       
**NOP**  BAS   RE,PUTREQ           FOR DDS OUTPUT, NO DEST OVERRIDE             
                                                                                
* PUT REQUEST TO OUTPUT FILE                                                    
                                                                                
GETREQ10 LR    R0,R2                                                            
         BAS   RE,PUTREQ                                                        
*                                                                               
         MVC   QAREA(80),0(R2)     SAVE THIS REQUEST                            
         AP    REQCOUNT,=P'1'                                                   
         J     GETREQ2                                                          
*                                                                               
GETREQX  DS    0H                                                               
         GOTO1 VSORTER,DMCB,=C'END'                                             
*                                                                               
         CLOSE REQOUT                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         CLOSE RECVIN                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         LARL  R0,RDATA                                                         
         ST    R0,AIO                                                           
         GOTO1 =V(DATAMGR),DMCB,=C'DMCLSE',=C'SPOT',,AIO                        
         GOTO1 (RF),(R1),=C'DMCLSE',=C'CONTROL'                                 
*                                                                               
         MVC   P(12),=C'REQUESTS OUT'                                           
         OI    REQCOUNT+3,X'0F'                                                 
         UNPK  P+14(6),REQCOUNT                                                 
         GOTO1 =V(PRINTER)                                                      
         J     EXIT                                                             
                                                                                
*=======================================================                        
* R0 HAS ADDRESS OF DATA TO BE PUT                                              
*=======================================================                        
                                                                                
PUTREQ   NTR1                                                                   
         LARL  R1,REQOUT                                                        
         PUT   (1),(0)                                                          
*                                                                               
         LR    RF,R0                                                            
         MVC   P(160),0(RF)                                                     
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
*=========================================================                      
* DATAMGR CALLS                                                                 
*=========================================================                      
                                                                                
HIGH     LARL  RF,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         J     DIR                                                              
*                                                                               
SEQ      LARL  RF,DMRSEQ                                                        
*                                                                               
DIR      NTR1                                                                   
         ST    RF,DMCB                                                          
         LARL  RF,SPTDIR                                                        
         ST    RF,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,,,KEYSAVE,KEY                                      
         J     EXIT                                                             
*                                                                               
GET      NTR1                                                                   
         LARL  RF,GETREC                                                        
         ST    RF,DMCB                                                          
         LARL  RF,SPTFILE                                                       
         ST    RF,DMCB+4                                                        
         GOTO1 VDATAMGR,DMCB,,,KEY+14,AIO,DMWORK                                
*                                                                               
EQXIT    CR    RB,RB               SET CC EQ                                    
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NEQ                                   
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
GETREC   DC    CL8'GETREC'                                                      
SPTDIR   DC    CL8'SPTDIR'                                                      
SPTFILE  DC    CL8'SPTFILE'                                                     
         LTORG                                                                  
*                                                                               
*=========================================================                      
* USE I2 PROFILE TO SEE IF CLIENT SHOULD BE OUTPUT                              
*=========================================================                      
         USING BUYTABD,R4                                                       
GETCLT   NTR1  BASE=*,LABEL=*                                                   
         CLC   QBAGYMCL,BAGYMCL    THIS FIELD CLEARED IF SKIPPED                
         JE    GETCLTX                                                          
         CLC   SKIPAMC,BAGYMCL     SO SAVED A/M/C IS HERE                       
         BE    GETCLTX                                                          
         MVC   QBAGYMCL,BAGYMCL                                                 
         MVI   SKIPCLT,C'N'                                                     
* READ NEW CLTHDR                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMCL                                                 
         BRAS  RE,HIGH                                                          
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,ADCLT                                                        
         BRAS  RE,GET                                                           
*                                                                               
         L     R6,ADCLT            GET CORRECT EBCDIC CLIENT                    
         USING CLTHDRD,R6                                                       
         GOTO1 VCLUNPK,DMCB,(CPROF+6,CKEYCLT),CLIENT                            
                                                                                
* GET EBCDIC USERID                                                             
                                                                                
         XC    KEY,KEY                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),DM$RUSER                                               
         LARL  R6,IDREC                                                         
         GOTO1 =V(DATAMGR),DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                  
*                                                                               
         CLC   KEY(25),0(R6)                                                    
         JNE   *+2                                                              
*                                                                               
         LA    RE,28(R6)           POINT TO FIRST ELEM                          
*                                                                               
GETIDEL  LLC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         JE    *+2                                                              
         CLI   0(RE),2                                                          
         JNE   GETIDEL                                                          
*                                                                               
         MVC   SVUSERID,2(RE)                                                   
                                                                                
*===========================================================                    
                                                                                
*&&DO                                                                           
         LARL  R4,IDTAB                                                         
*                                                                               
         CLC   =C'DDS',10(R4)                                                   
         JE    GETCLT10                                                         
*                                                                               
         LA    R5,OFCWORK                                                       
         USING OFFICED,R5                                                       
         XC    OFCWORK,OFCWORK                                                  
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCCLT,CLIENT                                                    
         MVC   OFCAGY,SVAGYA                                                    
         MVC   OFCOFC,COFFICE                                                   
*                                                                               
GETCLT2  MVC   OFCLMT,20(R4)                                                    
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'2',(R5)),ACOMFACS,SVOFCLST                      
                                                                                
         CLI   0(R1),0             TEST USE THIS ID                             
         BE    GETCLT10            YES                                          
*                                                                               
         LA    R4,24(R4)                                                        
         CLI   0(R4),0             TEST FOR ANOTHER ENTRY                       
         JNE   GETCLT2             AND SEE IF THAT'S THE LIMIT ACCS             
*                                                                               
         MVI   SKIPCLT,C'Y'                                                     
         MVC   SKIPAMC,BAGYMCL                                                  
         MVC   P(38),=C'CLIENT XXX HAS NO LIMIT ACCESS'                         
         MVC   P+7(3),CLIENT                                                    
         GOTO1 =V(PRINTER)                                                      
         J     NEQXIT                                                           
*                                                                               
GETCLT10 MVC   THISORIG,0(R4)                                                   
         MVC   THISDEST,10(R4)                                                  
*&&                                                                             
                                                                                
*===========================================================                    
* NOW GET PROFILES FOR THIS CLIENT                                              
*===========================================================                    
                                                                                
GETCLT12 CLI   OVERI2N,C'Y'                                                     
         JE    GETCLT14                                                         
*                                                                               
         MVC   WORK(12),=CL12'SI2N'                                             
         NI    WORK,X'FF'-X'40'      MAKE S LOWERCASE                           
         MVC   WORK+4(2),SVAGYA                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLIENT                                                 
         MVI   WORK+10,C'*'                                                     
         L     RE,ADCLT                                                         
         MVC   WORK+11(1),COFFICE-CLTHDRD(RE)                                   
         GOTO1 =V(GETPROF),DMCB,WORK,SVI2PROF,VDATAMGR                          
*                                                                               
GETCLT14 CLI   OVERZ5,C'Y'                                                      
         JE    GETCLTX                                                          
*                                                                               
         MVC   WORK(12),=CL12'S0Z5'                                             
         MVC   WORK+4(2),SVAGYA                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),CLIENT                                                 
         MVI   WORK+10,C'*'                                                     
         L     RE,ADCLT                                                         
         MVC   WORK+11(1),COFFICE-CLTHDRD(RE)                                   
         GOTO1 =V(GETPROF),DMCB,WORK,SVZ5PROF,VDATAMGR                          
*                                                                               
GETCLTX  J     EQXIT                                                            
         DROP  R6                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
REQOUT   DCB   DDNAME=REQOUT,DSORG=PS,RECFM=FB,LRECL=80,               X        
               BLKSIZE=3200,MACRF=PM                                            
*                                                                               
* SORT ON USERID, THEN A/M-CLT/PRD/EST/MKT/STA                                  
SORTCARD DC   CL80'SORT FIELDS=(44,10,A,1,43,A),FORMAT=BI'                      
RECCARD  DC   CL80'RECORD TYPE=F,LENGTH=80'                                     
*                                                                               
*                                                                               
         EJECT                                                                  
SPSI2WKD DS    0D                                                               
VDATAMGR DC    V(DATAMGR)                                                       
ACOMFACS DC    A(COMFACS)                                                       
VDATCON  DC    V(DATCON)                                                        
VGETDAY  DC    V(GETDAY)                                                        
VADDAY   DC    V(ADDAY)                                                         
VSORTER  DC    V(SORTER)                                                        
VSTAPACK DS    A                                                                
VCLUNPK  DS    A                                                                
VOFFICER DS    A                                                                
VGETBRD  DS    A                                                                
VT00A9E  DS    A                                                                
XBASERD  DS    A                                                                
SORTCNT  DC    PL4'0'                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'DATELIST'                                                    
DATELIST DS    XL104             2-BYTE BRDMON START/END + 4 BYTE YYMM          
FIRSTBRD EQU   DATELIST,2        START OF FIRST BRDMON IN TABLE                 
LASTBRD  EQU   DATELIST+88,2     START OF LAST  BRDMON IN TABLE                 
*                                                                               
*                                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYT2     DS    X                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
TRACEFLG DC    X'00'                                                            
CTRYSAVE DS    C                                                                
BAGYMCL  DS    XL3                                                              
SKIPCLT  DS    C                   USED WHEN PROFILE HAS OVRD                   
SKIPAMC  DS    XL3                                                              
CLIENT   DS    CL3                                                              
OVERI2N  DS    C                    C'Y' IF PROFILE OVRD                        
OVERZ5   DS    C                                                                
SVUSERID DS    CL10                                                             
*                                                                               
         DS    0D                                                               
SVI2PROF DC    XL16'00'                                                         
SVZ5PROF DC    XL16'00'                                                         
*                                                                               
SVI2PRF5  EQU   SVI2PROF+5          REQUEST BY ESTIMATE                         
SVI2PRF8  EQU   SVI2PROF+8          REQUEST BY BUYID                            
SVI2PRF9  EQU   SVI2PROF+9          AUTO I2'S ACTIVE                            
*                                                                               
SVZ5PRF15 EQU   SVZ5PROF+15         REQUEST BY POL                              
*                                                                               
         DS    0D                                                               
QRECORD  DS    0CL80                                                            
QAREA    DS    0CL80   COLUMN                                                   
QPROG    DS    0CL2    ------                                                   
QCODE    DS    CL2        1        PROGRAM CODE                                 
QAGY     DS    CL2        3        AGENCY CODE                                  
QMED     DS    CL1        5        MEDIA CODE (R/T)                             
QCLT     DS    CL3        6        CLIENT CODE                                  
QPGR     DS    CL1        9        PROCESS BY DIVISION                          
QMGR     DS    CL1       10        PROCESS BY DISTRICT                          
QCLOFFC  DS    CL1       11        CLIENT OFFICE FILTER                         
QBYID    EQU   QCLOFFC             C'Y' IF BUYS PROCESSED BY ID                 
QPRD     DS    CL3       12        PRODUCT MNEMONIC                             
QMKT     DS    CL4       15        MARKET NUMBER                                
QSTA     DS    CL5       19        STATION CALL LETTERS                         
QEST     DS    CL3       24        ESTIMATE NUMBER                              
QESTEND  DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
         DS    CL1       30                                                     
QCONTREQ DS    CL1       31        C'*' ==> DATA IN QAREA2                      
QSTAUTO  DS    CL3       32        AUTO REQUEST START DATE                      
QENDAUTO DS    CL3       35        AUTO REQUEST END DATE                        
QSTART   DS    CL6       38        REQUEST START DATE                           
QEND     DS    0CL6      44        REQUEST END DATE                             
QTODAY   DS    CL6       44                                                     
         DS    CL4       50                                                     
         ORG   QTODAY                                                           
QUSERID  DS    CL10      44        ORIGIN/DEST USERID                           
         DS    CL2       54                                                     
         DS    CL1       56                                                     
         DS    CL1       57                                                     
         DS    CL1       58                                                     
QBAGYMCL DS    CL3       59        BINARY A-M/CLT FOR CLTHDR READ               
*                                                                               
QOPT1    DS    CL1       62        OPTION 1                                     
QOPT2    DS    CL1       63        OPTION 2                                     
QOPT3    DS    CL1       64        OPTION 3                                     
QOPT4    DS    CL1       65        OPTION 4                                     
QOPT5    DS    CL1       66        OPTION 5                                     
         DS    CL2       67                                                     
QUESTOR  DS    CL12      69        REQUESTOR NAME                               
*                                                                               
QRECORD2 DS    0CL80               REQUEST CARD 2 DATA AREA                     
QCRRNCY  DS    CL1     COL 01      CURRENCY OVRD - U=US,C=CANADA                
QLANG    DS    CL1         02      LANGUAGE (C'F' FOR FRENCH)                   
QGST     DS    CL1         03      C'I OR Y' = INCLUDE INPUT GST                
*                                  C'O'      = INCLUDE OUTPUT GST               
         DS    CL1         04                                                   
QCLGID   DS    CL1         05      CLTGRP ID                                    
QCLGRP   DS    CL4         06      CLTGRP NUMBER                                
         DS    CL3         10                                                   
QCBLNET  DS    CL3         13      CABLE NETWORK                                
QCOST2   DS    CL1         16      REPORT COST2 DOLLARS                         
         DS    CL1         17                                                   
         DS    CL1         18                                                   
         DS    CL1         19                                                   
         DS    CL1         20                                                   
Q2USER   DS    CL50        21                                                   
         DS    CL4         71                                                   
         DS    CL1         75                                                   
         DS    CL1         76                                                   
         DS    CL1         77                                                   
         DS    CL1         78                                                   
         DS    CL1         79                                                   
*                                                                               
         DS    CL1         80                                                   
         EJECT                                                                  
         DS    0D                                                               
QSAVE    DS    CL160               SAVE AREA FOR PREVIOUS REQUESTS              
REQCOUNT DC    PL4'0'                                                           
*                                                                               
         DS    0D                                                               
STAWORK  DS    XL32                                                             
OFCWORK  DS    XL48                                                             
*                                                                               
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR '                                                    
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE '                                                    
         DC    CL2'X '                                                          
*                                                                               
DUB      DS    D                                                                
QSTARTB  DS    XL3                 YMD    12 MONTHS AGO                         
QSTARTP  DS    XL2                 2-BYTE PACKED 12 MONTHS AGO                  
QTODAYP  DS    XL2                                                              
         DS    XL1                 SPARE                                        
         DS    0D                                                               
KEY      DS    XL48                                                             
KEYSAVE  DS    XL48                                                             
CARD     DS    CL80                                                             
WORK     DS    XL80                                                             
SVORIG   DS    CL10                                                             
SVDEST   DS    CL10                                                             
SVAGYA   DS    CL2                                                              
SVAGYB   DS    XL1                                                              
SVSYSSE  DS    XL1                                                              
SVSYSLMT DS    XL4                                                              
SVOFCLST DS    XL256                                                            
PARMOUT  EQU   SVOFCLST                                                         
*                                                                               
* FULLWORD-ALIGNED                                                              
*                                                                               
AIO      DS    A                                                                
ABUYTAB  DS    A                                                                
ADCLT    DS    A                                                                
DMCB     DS    6F                                                               
DMWORK   DS    24F                                                              
*                                                                               
CPYID    DS    CL12                                                             
CHGID    DS    CL12                                                             
THISREC  DS    C                   X'01'=COPY/ADD, X'02'=CHANGE                 
LASTCPY  DS    CL160               LAST COPY TO SORT                            
LASTCHG  DS    CL160               LAST CHANGE TO SORT                          
THISORIG DS    CL10                                                             
THISDEST DS    CL10                                                             
*                                                                               
         DS    0D                                                               
IDTAB    DS    20CL24              ORIG ID(10)/DESTID(10)/LMTACCS(4)            
IDTABX   EQU   *-IDTAB                                                          
*                                                                               
         DS    0D                                                               
         DC    CL8'**UTL **'                                                    
UTL      DC    F'0',X'00'                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      DC    256X'00'                                                         
         ORG   SSB                                                              
         DC    XL2'00',X'FF',X'02' NO RECOVERY                                  
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RECVLEN  DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
*PREFIX=DM$                                                                     
       ++INCLUDE DMRCVRHDR                                                      
*PREFIX=                                                                        
RDATA    DS    8200X                                                            
*                                                                               
*                                                                               
COMFACS  DS    0D                                                               
CDMGR    DC    V(DATAMGR)                                                       
CCALLOFF DC    V(CALLOFF)                                                       
         DC    200A(0)                                                          
         DS    0D                                                               
         DC    CL8'*CLTREC*'                                                    
CLTREC   DS    4096C                                                            
         DS    0D                                                               
         DC    CL8'*CPYTAB*'                                                    
CPYTAB   DS    2000D                                                            
CPYTABX  EQU    *                                                               
         DS    0D                                                               
         DC    CL8'*CHGTAB*'                                                    
CHGTAB   DS    2000D                                                            
CHGTABX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
IDREC    DS    4000C                                                            
         PRINT OFF                                                              
MASTC    DS    0D                                                               
       ++INCLUDE DDMASTC                                                        
         PRINT ON                                                               
*                                                                               
BUYTABD  DSECT                                                                  
BTDATE   DS    XL2                                                              
BTTIME   DS    XL4                 BUY START END TIMES                          
BTSEC    DS    XL1                                                              
BTPAID   DS    XL2                                                              
BTMINUS  DS    CL1                                                              
BTMSSD   DS    CL1                                                              
BTMATCHD DS    CL1                                                              
BTCOST   DS    XL3                                                              
BTPRD    DS    XL1                                                              
BTPRD2   DS    XL1                                                              
BTSPREP  DS    XL2                                                              
BTAFFDT  DS    XL2   ??                                                         
BTAFFTM  DS    XL2   ??                                                         
BUYTABX  EQU   *                                                                
BUYTABL  EQU   BUYTABX-BUYTABD                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDSYSELD                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
SSBOFFD  DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPSI2GEN  05/28/19'                                      
         END                                                                    
