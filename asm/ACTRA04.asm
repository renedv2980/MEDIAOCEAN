*          DATA SET ACTRA04    AT LEVEL 023 AS OF 02/15/19                      
*PHASE T62204B                                                                  
         TITLE '(T62204)  BILLING TRANSFER - TRACE OVERLAY'                     
T62204   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62204**,RR=RE                                                 
*                                                                               
         USING TWAD,R5            R5=A(TWA)                                     
         USING SAVAREA,R6         R6=A(SAVE AREA)                               
         USING WORKD,R7           R7=A(GLOBAL WORKING STORAGE)                  
         L     RC,APALOCAL        RC=A(LOCAL WORKING STORAGE)                   
         USING LOCALD,RC                                                        
         ST    RB,APBASE1                                                       
         ST    RB,APNTRYA                                                       
         ST    RD,APWORKA                                                       
         ST    RE,APRELO             RELOCATION FACTOR                          
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   APMODE,APMDISK        DISPLAY KEY FROM SWAP                      
         BE    DISKEY                                                           
         CLI   APMODE,APMVALK        VALIDATE KEY                               
         BE    VALKEY                                                           
         CLI   APMODE,APMDISR        DISPLAY RECORD                             
         BE    DISREC                                                           
*                                                                               
* NOTE :  IF SWAPPING TO THIS APPLICATION                                       
*         PROWNUM AND RECTYPE SET ON ENTRY                                      
*         OTHERWISE SET IN VALKEY                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*====================================*                                          
* DISKEY - DISPLAY KEY SELECTED      *                                          
*====================================*                                          
*                                                                               
DISKEY   LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
         MVC   TRCSYS(L'MPRKSYS),MPRKSYS     DISPLAY SYSTEM                     
         MVC   TRCSYS+1(L'MPRKALPH),MPRKALPH AGY ALPHA (SPLIT FILES)            
         OI    TRCSYSH+6,X'80'                                                  
         MVC   TRCMED,MPRKMED     DISPLAY MEDIA                                 
         OI    TRCMEDH+6,X'80'                                                  
         GOTO1 ADISOFF,APPARM,MPRKOFC,TRCOFF                                    
         BNE   *+8                                                              
         OI    TRCOFFH+6,X'80'                                                  
         MVC   TRCCLT,MPRKCLI     DISPLAY CLIENT                                
         OI    TRCCLTH+6,X'80'                                                  
         MVC   TRCPRD,MPRKPRD     DISPLAY PRODUCT                               
         OI    TRCPRDH+6,X'80'                                                  
         EDIT  PROWNUM,(3,TRCPST),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TRCPSTH+6,X'80'    DISPLAY POSTING ROW NUMBER                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*=====================================*                                         
* VALKEY - VALIDATES KEY              *                                         
*=====================================*                                         
*                                                                               
VALKEY   LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
*                                                                               
         GOTO1 ADOSPEC            SET SPECIFIC RECORD INFORMATION               
         GOTO1 AVALSYS,TRCSYSH    VALIDATE SYSTEM                               
         BNE   VALKX                                                            
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR SPLIT FILES                     
         MVC   MPRKSYS,QSYS                                                     
*                                                                               
         LA    R3,TRCMEDH         VALIDATE MEDIA                                
         GOTO1 AVALMED,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKMED,QMED                                                     
         OC    QMED,QMED          IF NO MEDIA                                   
         BNZ   VALOFF                                                           
         GOTO1 AVALFLD,APPARM,TRCOFFH,3                                         
         CLI   APPARM,X'FF'       NO OFF/CLT/PRD                                
         BE    MISSERR                                                          
*                                 VALIDATE OFFICE                               
VALOFF   LA    R3,TRCOFFH         VALIDATE OFFICE                               
         MVI   OCFLAG,0           NO OFFICE INPUT                               
         GOTO1 AVALOFF,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKOFC,QOFF                                                     
         OC    QOFF,QOFF          IF NO OFFICE                                  
         BZ    VALKCLT            CHECK CLIENT INPUT                            
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
*                                                                               
VALKCLT  LA    R3,TRCCLTH         VALIDATE CLIENT                               
         GOTO1 AVALCLT,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKCLI,QCLT                                                     
         OC    QCLT,QCLT          IF NO CLIENT REQUESTED                        
         BNZ   VALKCLT5                                                         
         GOTO1 AVALFLD,APPARM,TRCPRDH,1                                         
         CLI   APPARM,X'FF'       CAN'T HAVE PRODUCT                            
         BE    MISSERR                                                          
         B     VALKPRD                                                          
*                                                                               
VALKCLT5 CLI   OCFLAG,C'O'        YES - CAN'T HAVE OFFICE TOO                   
         BE    *+12                                                             
         MVI   OCFLAG,C'C'        SET CLIENT REQUESTED ALONE                    
         B     VALKPRD                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKX                                                            
*                                                                               
VALKPRD  LA    R3,TRCPRDH         VALIDATE PRODUCT                              
         GOTO1 AVALPRD,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKPRD,QPRD                                                     
*                                                                               
VALKPOST LA    R1,TRCPSTH         VALIDATE POSTING                              
         MVI   FVMINL,1           FIELD REQUIRED                                
         MVI   FVMAXL,3           MAX LENGTH                                    
         GOTO1 AFVAL                                                            
         BNE   VALKX                                                            
         OC    SCFULL,SCFULL                                                    
         BZ    VALKPERR                                                         
         TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
VALKPERR MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALKX                                                            
*                                                                               
         CLC   SCFULL+3(1),PMAXNUM   VALID NUMBER                               
         BNH   VALK30                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKX                                                            
*                                                                               
VALK30   MVC   PROWNUM,SCFULL+3                                                 
         GOTO1 ASETFILE           SET ACC FILES & LIMIT ACCESS                  
         BE    VALK40                                                           
         LA    R2,TRCSYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALKX                                                            
*                                                                               
VALK40   MVC   FVMSGNO,=AL2(FVFOK)                                              
         OI    APINDS,APIOKDIS    OKAY TO DISPLAY                               
         BAS   RE,CLRSCR          CLEAR SCREEN - BEFORE DISPLAY REC             
*                                                                               
VALKX    B     EXIT                                                             
         SPACE                                                                  
MISSERR  MVC   FVMSGNO,=AL2(FVIMISS)                                            
         STCM  R3,15,APCURSOR                                                   
         B     VALKX                                                            
         DROP  R2                                                               
         EJECT                                                                  
*=====================================*                                         
* DISREC - DISPLAYS RECORD            *                                         
*=====================================*                                         
*                                                                               
DISREC   BAS   RE,CHGPOSTS        DISPLAY POSTING LABEL                         
         BAS   RE,CHKPFKS                                                       
         LA    R2,TRCL1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R2                                                          
*                                                                               
         CLI   ELETYPE,MBTTRCV    RECEIVABLES                                   
         BE    DISR5                                                            
         CLI   ELETYPE,MBTTCOS    COSTING                                       
         BE    DISR8                                                            
         CLI   ELETYPE,MBTTINTC   INTERNAL COSTING                              
         BNE   DISR8                                                            
DISR5    OC    QCLT,QCLT          CLT REQUESTED?                                
         BZ    DISR15             NO - CAN'T READ P/U/L                         
         GOTO1 ARDPROD,APPARM,ELETYPE,ACCOUNT                                   
         BNE   DISR15                                                           
         MVC   DACC,ACCOUNT                                                     
         B     DISR15                                                           
*                                                                               
DISR8    CLI   ELETYPE,MBTTINC                                                  
         BE    DISR10                                                           
         CLI   ELETYPE,MBTTNET                                                  
         BE    DISR10                                                           
         CLI   ELETYPE,MBTTCD                                                   
         BE    DISR10                                                           
         CLI   ELETYPE,MBTTBIL                                                  
         BE    DISR10                                                           
         CLI   ELETYPE,MBTTREV                                                  
         BNE   DISR12                                                           
DISR10   OC    QSYS,QSYS                                                        
         BZ    DISR12                                                           
         OC    QMED,QMED                                                        
         BZ    DISR12                                                           
         GOTO1 ARDMI,APPARM,ELETYPE,ACCOUNT                                     
         BNE   DISR15                                                           
         MVC   DACC,ACCOUNT                                                     
         B     DISR15                                                           
*                                                                               
DISR12   CLI   ELETYPE,MBTTGST    IF OUTPUT GST                                 
         BNE   *+12                                                             
         MVI   RULTYPE,C'O'                                                     
         B     *+16                                                             
         CLI   ELETYPE,MBTTGSTI   IF INPUT GST                                  
         BNE   DISR13                                                           
         MVI   RULTYPE,C'I'                                                     
*                                                                               
         MVI   PRVNUM,0           NO PROVIDENCE                                 
         MVC   OUTCODE,SVCGST                                                   
         CLI   SVPGST,0                                                         
         BE    *+10                                                             
         MVC   OUTCODE,SVPGST                                                   
         BAS   RE,GETVAT          GET GST ACCOUNT                               
         MVC   DACC,ACCOUNT                                                     
*                                                                               
DISR13   MVI   PRVNUM,1           READ BY PROVIDENCE FIRST                      
         MVI   RULTYPE,C'O'       SET LOOK FOR OUTPUT                           
         CLI   ELETYPE,MBTTPQO    IF OUTPUT PST FOR PQ                          
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNBO    OR IF OUTPUT PST FOR NB                       
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNSO    OR IF OUTPUT PST FOR NS                       
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNFO    OR IF OUTPUT PST FOR NF                       
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTBCO    OR IF OUTPUT PST FOR BC                       
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTONO    OR IF OUTPUT PST FOR ON                       
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTPEO    OR IF OUTPUT PST FOR PE                       
         BE    DISR14                                                           
*                                                                               
         MVI   RULTYPE,C'I'       SET LOOK FOR INPUT                            
         CLI   ELETYPE,MBTTPQI    IF INPUT PST FOR PQ                           
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNBI    OR INPUT PST FOR NB                           
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNSI    OR INPUT PST FOR NS                           
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTNFI    OR INPUT PST FOR NF                           
         BNE   DISR15                                                           
         CLI   ELETYPE,MBTTBCI    OR INPUT PST FOR BC                           
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTPEI    OR INPUT PST FOR PE                           
         BE    DISR14                                                           
         CLI   ELETYPE,MBTTONI    OR INPUT PST FOR ON                           
         BNE   DISR15                                                           
*                                                                               
DISR14   BAS   RE,SETPRV          SET PROVIDENCE INFO                           
         BAS   RE,GETVAT          GET PST ACCOUNT                               
         MVC   DACC,ACCOUNT                                                     
*                                                                               
DISR15   LA    R2,TRCL2DH         PT TO SYSTEM LINE                             
         OC    QSYS,QSYS          IF SYSTEM INPUT - READ RECORD BUT             
         BZ    DISR20             DON'T GENERATE POSTBL                         
         GOTO1 ARDSYS,APPARM,RECTYPE                                            
         BNE   DISR20                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR20   LA    R2,TRCL3DH         PT TO MEDIA LINE                              
         OC    QMED,QMED          IF MEDIA INPUT - READ RECORD BUT              
         BZ    DISR25             DON'T GENERATE POST TABLE                     
         GOTO1 ARDMED,APPARM,RECTYPE                                            
         BNE   DISR25                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR25   LA    R2,TRCL4DH         PT TO OFFICE GROUP LINE                       
         CLI   QSYS,C'P'                                                        
         BNE   DISR26                                                           
         CLI   SVPROF+1,C'Y'      USING OFFICE GROUPS?                          
         BNE   DISR30             NO - SKIP LINE                                
         B     DISR27                                                           
DISR26   CLI   SVPROF,C'Y'        USING SPOT/NET OFFICE GROUPS?                 
         BNE   DISR30             NO - SKIP LINE                                
DISR27   OC    SVOFFG,SVOFFG                                                    
         BZ    DISR30                                                           
         GOTO1 ARDOFF,APPARM,RECTYPE,SVOFFG                                     
         BNE   DISR30                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR30   LA    R2,TRCL5DH         PT TO OFFICE LINE                             
         MVC   OFFCODE,=C'  '                                                   
         OC    QCLT,QCLT          IF CLIENT INPUT -                             
         BZ    *+14                                                             
         MVC   OFFCODE(1),SVCOFF  OFFICE CODE IS SVCOFF                         
         B     DISR35                                                           
         OC    QOFF,QOFF          ANY OFFICE INPUT?                             
         BZ    DISR40                                                           
         CLI   QOFFIND,C'O'       YES - OFFICE CODE                             
         BNE   DISR40             NO - GO TO NEXT                               
         MVC   OFFCODE(1),QOFF                                                  
DISR35   GOTO1 ARDOFF,APPARM,RECTYPE,OFFCODE                                    
         BNE   DISR40                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR40   LA    R2,TRCL6DH         PT TO CLIENT LINE                             
         OC    QCLT,QCLT          IF CLIENT INPUT - READ RECORD                 
         BZ    DISR50             BUT DON'T GENERATE TABLE                      
         GOTO1 ARDCLT,APPARM,RECTYPE                                            
         BNE   DISR50                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR50   LA    R2,TRCL7DH         PT TO PRODUCT LINE                            
         OC    QPRD,QPRD          IF PRODUCT INPUT - READ RECORD BUT            
         BZ    DISR60             DON'T GENERATE TABLE                          
         GOTO1 ARDPRD,APPARM,RECTYPE                                            
         BNE   DISR60                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR60   CLI   APPFKEY,PFK03      RETURN TO MAINT SCREEN                        
         BNE   DISRX                                                            
         TM    TWAFLAG,TWAFMAI    DID WE COME FROM MAINT SCREEN?                
         BZ    DISRX              NO - DON'T DO ANYTHING                        
         NI    TWAFLAG,FF-TWAFMAI YES - TURN OFF FLAG & RETURN                  
         OI    TWAFLAG,TWAFTRC    INDICATE SWAPPED FROM TRACE SCREEN            
         L     R2,ATIA                                                          
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(2,0),(R2)                           
         LA    R0,TRAPTGH                                                       
         LA    RE,TRAPTGH-TWAD(R2)                                              
         LH    R1,=Y(TWASVLEN)                                                  
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R1,TRAMSGH         TRANSMIT THE SCREEN                           
         LH    RF,=Y(TWASVLEN)                                                  
         LA    RF,TRAPTGH(RF)                                                   
         SR    RE,RE                                                            
         OI    6(R1),X'80'                                                      
         ICM   RE,1,0(R1)                                                       
         BZ    *+10                                                             
         BXLE  R1,RE,*-12                                                       
         DC    H'0'                                                             
         MVI   1(R1),1                                                          
         MVI   2(R1),1                                                          
         MVI   APMODE,APMRET                                                    
         MVC   APPARM,INREC       KEEP SAME RECORD                              
         MVI   APPARM+1,ACTMAI                                                  
*                                                                               
DISRX    TM    TWAMODE,TWAMLSM    ARE WE LIST (AT SOME POINT)                   
         BNO   DISRXX             NO - GET OUT                                  
         MVI   TWALSACT,ACTRACE   YES -TELL GENERAL THIS IS LAST ACTION         
         OI    TWALSCTL,TWALSRTN  AND TRACE WANTS CONTROL                       
         OI    TWALSCTL,TWALSHLD  AND SCREEN HELD ON CRT                        
DISRXX   L     R1,AACTHDR                                                       
         STCM  R1,15,APCURSOR     PT CURSOR TO ACTION FIELD                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
* CHGPOSTS - CHANGES POST FIELD IN KEY  & POST DESCRIPTION FIELD                
*                                                                               
CHGPOSTS NTR1                                                                   
         SR    RE,RE                                                            
         ZIC   R1,PROWNUM                                                       
         BCTR  R1,0                                                             
         LA    RF,APTABLN         LENGTH OF APTABLE                             
         MR    RE,R1                                                            
         L     R2,APTABLE         CURRENT TABLE                                 
         AR    R2,RF              PT TO CORRECT ROW                             
         MVC   ELETYPE,0(R2)                                                    
         MVC   TRCDES(26),1(R2)   MOVE IN TRACE POSTING ROW LABEL               
         OI    TRCDESH+6,X'80'                                                  
         B     EXIT                                                             
         SPACE                                                                  
*              ROUTINE TO SET INFORMATION FOR PROVIDENCE                        
*                                                                               
SETPRV   NTR1                                                                   
         XR    R1,R1               GET DISP INTO CLI/PRD CODES                  
         LA    RF,PRVTAB                                                        
         USING PRVTABD,RF                                                       
SETPRV5  CLI   0(RF),X'FF'         TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   RULTYPE,C'O'        IF LOOKING FOR OUTPUT PST                    
         BNE   SETPRV10                                                         
         CLC   PRVTOUT,ELETYPE                                                  
         BNE   SETPRV20                                                         
         MVC   PRVCODE,PRVTCODE    SET PROVINCE                                 
         MVI   OUTCODE,C'S'        SET DEFAULT AS STANDARD                      
         LA    RE,SVCPST                                                        
         AR    RE,R1                                                            
         CLI   0(RE),0             IF CLIENT TAX CODE OVERRIDE                  
         BE    *+10                                                             
         MVC   OUTCODE,0(RE)       SET CLIENT TAX CODE OVERRIDE                 
         LA    RE,SVPPST                                                        
         AR    RE,R1                                                            
         CLI   0(RE),0             IF PRODUCT CODE OVERRIDE                     
         BE    *+10                                                             
         MVC   OUTCODE,0(RE)       SET PRODUCT TAX CODE OVERRIDE                
         B     SETPRVX                                                          
*                                                                               
SETPRV10 CLC   PRVTIN,ELETYPE      TEST MATCH ON INPUT PST                      
         BNE   SETPRV20                                                         
         MVC   PRVCODE,PRVTCODE    SET PROVINCE                                 
         MVI   OUTCODE,0           USE DEFAULT FROM ACC RECORD                  
         B     SETPRVX                                                          
*                                                                               
SETPRV20 LA    RF,L'PRVTAB(RF)                                                  
         LA    R1,1(R1)                                                         
         B     SETPRV5                                                          
*                                                                               
SETPRVX  B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
*==========================================*                                    
* PRTLIN - PULLS OUT REC VALUES & DISPLAYS *                                    
*           ON SCREEN LINE                 *                                    
*==========================================*                                    
*                                                                               
PRTLIN   NTR1                                                                   
         L     R3,AIOAREA1        PT TO POSTING TABLE                           
         AH    R3,DATADISP        PT TO FIRST ELEMENT                           
         USING MBTELD,R3                                                        
*                                                                               
PRTL5    CLI   0(R3),0            END OF RECORD?                                
         BE    PRTLX                                                            
         CLI   0(R3),MBTELQ       MEDIA TRANSFER ELEMENT?                       
         BNE   PRTL30             NO - TRY NEXT ELEMENT                         
         CLC   MBTTYP,ELETYPE     CORRECT POSTING                               
         BNE   PRTL30             NO TRY NEXT ELEMENT                           
*                                                                               
         MVC   DACC,MBTULA        MOVE ACCOUNT TO SCREEN                        
         MVC   DAMT,MBTAMTX       MOVE AMOUNT TO SCREEN                         
         MVC   DMEMO,MBTMEMOX     MOVE MEMO TO SCREEN                           
         LA    R4,DLSTACT         MOVE LAST ACTIVITY TO SCREEN                  
         MVC   0(L'MBTPERID,R4),MBTPERID                                        
         LA    R4,L'MBTPERID+1(R4)                                              
         GOTO1 VDATCON,APPARM,(3,MBTCHNG),(8,0(R4))                             
         OI    DLINH+6,X'80'                                                    
         B     PRTLX                                                            
*                                                                               
PRTL30   SR    R0,R0              GET NEXT ELEMENT IN RECORD                    
         ICM   R0,1,1(R3)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
         B     PRTL5                                                            
*                                                                               
PRTLX    B     EXIT                                                             
         DROP  R2                                                               
         SPACE 2                                                                
         EJECT                                                                  
*==========================================*                                    
* CHKPFKS- CHECKS IF PFKEY WAS HIT       S *                                    
*==========================================*                                    
*                                                                               
CHKPFKS  NTR1                                                                   
         CLI   APPFKEY,PFK08      PREVIOUS POSTING?                             
         BNE   CHKPFK10                                                         
         ZIC   R1,PROWNUM                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BNZ   CHKPFK5                                                          
         ZIC   R1,PMAXNUM                                                       
CHKPFK5  STC   R1,PROWNUM                                                       
         EDIT  PROWNUM,(3,TRCPST),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TRCPSTH+6,X'80'    RE-DISPLAY POSTING ROW NUMBER                 
         BAS   RE,CHGPOSTS        RE-DISPLAY POSTING LABEL                      
         BAS   RE,CLRSCR                                                        
         B     CHKPFKX                                                          
*                                                                               
CHKPFK10 CLI   APPFKEY,PFK11      NEXT POSTING?                                 
         BNE   CHKPFKX                                                          
         ZIC   RE,PROWNUM                                                       
         ZIC   R1,PMAXNUM         IF AT MAX                                     
         CR    RE,R1                                                            
         BNE   *+8                                                              
         LA    RE,0               START AT 1 AGAIN                              
         LA    RE,1(RE)                                                         
         STC   RE,PROWNUM                                                       
         EDIT  PROWNUM,(3,TRCPST),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TRCPSTH+6,X'80'    RE-DISPLAY POSTING ROW NUMBER                 
         BAS   RE,CHGPOSTS        RE-DISPLAY POSTING LABEL                      
         BAS   RE,CLRSCR                                                        
CHKPFKX  B     EXIT                                                             
         EJECT                                                                  
CLRSCR   NTR1                                                                   
         LA    R1,TRCL1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R1                                                          
*                                                                               
CLRSCR5  XC    DACC,DACC                                                        
         XC    DAMT,DAMT                                                        
         XC    DMEMO,DMEMO                                                      
         XC    DLSTACT,DLSTACT                                                  
         OI    DLINH+6,X'80'                                                    
*                                                                               
         LA    RF,TRCL2DH-TRCL1DH LENGTH OF ONE LINE                            
         AR    R1,RF              PT TO NEXT LINE                               
         LA    RE,TRCPFKH                                                       
         CR    R1,RE                                                            
         BL    CLRSCR5                                                          
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------*                     
* GETVAT - GET DEFAULT OUTPUT GST/PST ACCOUNT             *                     
*          READS FOR PROVINCE/OFFICE/EFFECTIVE DATE       *                     
*                    PROVINCE/EFFECTIVE DATE              *                     
*                    OFFICE/EFFECTIVE DATE                *                     
*                    EFFECTIVE DATE                       *                     
*---------------------------------------------------------*                     
*                                                                               
GETVAT   NTR1                                                                   
         XC    AOFF,AOFF          GET ACCOUNT OFFICE                            
         GOTO1 ARDPROD,APPARM,ELETYPE,ACCOUNT                                   
         XC    ACCOUNT,ACCOUNT                                                  
*                                                                               
         CLI   PRVNUM,0           IF PROVIDENCE NUMBER PROVIDED                 
         BE    GETVAT10                                                         
         MVI   RULES,C'P'         READ BY PROVINCE/OFFICE/EFFECT DT             
         B     GETVAT20                                                         
*                                                                               
GETVAT5  MVI   RULES,C'V'         READ BY PROVINCE/EFFECTIVE DATE               
         B     GETVAT20                                                         
*                                                                               
GETVAT10 MVI   RULES,C'O'         READ BY OFFICE/EFFECTIVE DATE                 
         B     GETVAT20                                                         
*                                                                               
GETVAT15 MVI   RULES,C'E'         READ BY EFFECTIVE DATE                        
*                                                                               
GETVAT20 L     R4,AIOAREA2                                                      
         USING TAXRECD,R4                                                       
*                                                                               
GETVAT30 BAS   RE,RDRULES                                                       
         CLC   IOKEY(TAXKDATE-TAXKEY),0(R4)                                     
         BNE   GETVAT90                                                         
*                                                                               
         LR    R2,R4                                                            
         LA    R1,ACCORFST                                                      
         AR    R2,R1                                                            
         USING TAXELD,R2                                                        
         SR    R0,R0                                                            
GETVAT50 CLI   0(R2),0            END OF RECORD?                                
         BE    GETVAT90                                                         
         CLI   RULTYPE,C'O'        IF LOOKING FOR OUTPUT ACCOUNT                
         BNE   GETVAT60                                                         
         CLI   0(R2),TAXOELQ      X'DF'                                         
         BNE   GETVAT80                                                         
         CLC   TAXCODE,OUTCODE    MATCH ON TAX CODE                             
         BNE   GETVAT80                                                         
         MVC   ACCOUNT,TAXACT     DEFAULT OUTPUT ACCOUNT                        
         B     GETVATX                                                          
*                                                                               
GETVAT60 CLI   0(R2),TAXIELQ       X'DE'                                        
         BNE   GETVAT80                                                         
         TM    TAXINDS,TAXIDFLT    MATCH ON DEFAULT ACCOUNT                     
         BZ    GETVAT80                                                         
         MVC   ACCOUNT,TAXACT      DEFAULT INPUT ACCOUNT                        
         B     GETVATX                                                          
*                                                                               
GETVAT80 ICM   R0,1,1(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         B     GETVAT50                                                         
*                                                                               
GETVAT90 CLI   RULES,C'P'         IF NO LUCK WITH PROV/OFFICE/DATE              
         BE    GETVAT5            TRY PROV/OFFICE NEXT                          
         CLI   RULES,C'V'         IF NO LUCK WITH PROV/OFFICE                   
         BE    GETVAT10           TRY OFFICE/DATE NEXT                          
         CLI   RULES,C'O'         IF NO LUCK WITH OFFICE/DATE                   
         BE    GETVAT15           TRY JUST DATE                                 
GETVATX  B     EXIT                                                             
         DROP  R4,R2                                                            
         EJECT                                                                  
*------------------------------------------------------------*                  
* RDRULES - READS GSTRULES RECORD TO GET GST ACCOUNT         *                  
*   NTRY- RULES  - RECORD TO READ                            *                  
*   XIT - CC CODE SET                                        *                  
*------------------------------------------------------------*                  
*                                                                               
RDRULES  NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R4,IOKEY                                                         
         USING TAXRECD,R4                                                       
         MVI   TAXKTYP,TAXKTYPQ   X'05'                                         
         MVC   TAXKCPY,COMPANY2   OTHER COMPANY CODE - IF APPLICABLE            
         MVC   TAXKOFF,FFS                                                      
*                                                                               
         CLI   RULES,C'P'         IF READING BY PROV/OFF/DATE                   
         BNE   RDRULES4                                                         
         MVC   TAXKPRV,PRVCODE    SET PROVIDENCE CODE                           
         MVC   TAXKOFF,AOFF       SET ACC OFFICE CODE                           
         B     RDRULES8           GO SET DATE                                   
*                                                                               
RDRULES4 CLI   RULES,C'V'          IF READING PROV/DATE                         
         BNE   *+14                                                             
         MVC   TAXKPRV,PRVCODE    SET PROVIDENCE CODE                           
         B     RDRULES8           GO SET DATE                                   
*                                                                               
         CLI   RULES,C'O'         IF READING BY OFF/DATE                        
         BNE   *+10                                                             
         MVC   TAXKOFF,AOFF       SET ACC OFFICE CODE                           
*                                                                               
RDRULES8 GOTO1 VDATCON,APPARM,(5,APFULL),(1,TAXKDATE)                           
         XC    TAXKDATE,FFS        GET COMPLEMENT                               
         GOTO1 AMIOACC,APPARM,IORD+IOACCFIL+IO2,=C'SE2'                         
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE                                                                  
FFS      DC    X'FFFFFF'                                                        
         EJECT                                                                  
*=============*                                                                 
* LITERAL POOL*                                                                 
*=============*                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
*                                                                               
* -- ACTRAPRV - TABLE OF CANADIAN PROVIDENCE CODES                              
*                                                                               
       ++INCLUDE ACTRAPRV                                                       
*                                                                               
* -- ACTRAWRK                                                                   
*                                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
OFFCODE  DS    CL2                                                              
ELETYPE  DS    XL1                                                              
RULES    DS    CL1                P,O,E                                         
RULTYPE  DS    CL1                O=OUTPUT, I=INPUT                             
PRVCODE  DS    CL2                PROVIDENCE CODE                               
PRVNUM   DS    XL1                FLAG INDICATES FIRST READ BY PROV             
OUTCODE  DS    CL1                GST/PST CODE                                  
ACCOUNT  DS    XL14                                                             
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER TRACE SCREEN                                            
*                                                                               
DISD     DSECT                                                                  
DDESH    DS    CL8                FOR HEADER                                    
DDES     DS    CL15               DISCRIPTION                                   
DLINH    DS    CL8                                                              
DACC     DS    CL14               ACCOUNT                                       
         DS    CL2                                                              
DAMT     DS    CL8                AMOUNT                                        
         DS    CL2                                                              
DMEMO    DS    CL8                MEMO                                          
         DS    CL2                                                              
DLSTACT  DS    CL13               LAST ACTIVITY                                 
         DS    CL1                                                              
DISDL    EQU   *-DDESH                                                          
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFCD                                                       
         SPACE 2                                                                
         ORG                                                                    
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023ACTRA04   02/15/19'                                      
         END                                                                    
