*          DATA SET ACTRA06    AT LEVEL 012 AS OF 02/15/19                      
*PHASE T62206B                                                                  
         TITLE '(T62206)  BILLING TRANSFER - PROF TRACE OVERLAY'                
T62206   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T62206**,RR=RE                                                 
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
* NOTE : IF SWAPPING TO THIS APPLICATION                                        
*        PROWNUM SET TO PROFILE ROW NUMBER                                      
*        AND RECTYPE SET TO BILLING TYPE                                        
*        IF STRAIGHT ENTRY - VALUES CALCULATED                                  
*        ON VALKEY                                                              
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISKEY - DISPLAY KEY SELECTED                                                 
***********************************************************************         
         USING MPRRECD,R2                                                       
DISKEY   LA    R2,APRECKEY                                                      
         MVC   TR2SYS(L'MPRKSYS),MPRKSYS     DISPLAY SYSTEM                     
         MVC   TR2SYS+1(L'MPRKALPH),MPRKALPH AGY ALPHA(MED SPLIT FILE)          
         OI    TR2SYSH+6,X'80'                                                  
         MVC   TR2MED,MPRKMED     DISPLAY MEDIA                                 
         OI    TR2MEDH+6,X'80'                                                  
         GOTO1 ADISOFF,APPARM,MPRKOFC,TR2OFF                                    
         BNE   *+8                                                              
         OI    TR2OFFH+6,X'80'                                                  
         MVC   TR2CLT,MPRKCLI     DISPLAY CLIENT                                
         OI    TR2CLTH+6,X'80'                                                  
         MVC   TR2PRD,MPRKPRD     DISPLAY PRODUCT                               
         OI    TR2PRDH+6,X'80'                                                  
         EDIT  PROWNUM,(3,TR2PRO),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TR2PROH+6,X'80'    DISPLAY PROFILE ROW NUMBER                    
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALKEY - VALIDATE KEY                                                         
***********************************************************************         
VALKEY   LA    R2,APRECKEY                                                      
         USING MPRRECD,R2                                                       
*                                                                               
         GOTO1 ADOSPEC            SET SPECIFIC RECORD INFO                      
         GOTO1 AVALSYS,TR2SYSH    VALIDATE SYSTEM                               
         BNE   VALKX                                                            
         MVC   MPRKALPH,QALPH     AGY ALPHA FOR MED SPLIT FILES(IF SET)         
         MVC   MPRKSYS,QSYS                                                     
*                                                                               
         LA    R3,TR2MEDH         VALIDATE MEDIA                                
         GOTO1 AVALMED,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKMED,QMED                                                     
         OC    QMED,QMED          IF NO MEDIA                                   
         BNZ   VALOFF                                                           
         GOTO1 AVALFLD,APPARM,TR2OFFH,3                                         
         CLI   APPARM,X'FF'       CAN'T BE ANY OFF/CLT/PRD                      
         BE    MISSERR                                                          
*                                                                               
VALOFF   LA    R3,TR2OFFH         VALIDATE OFFICE                               
         MVI   OCFLAG,0           MARK NO OFFICE INPUTTED                       
         GOTO1 AVALOFF,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKOFC,QOFF                                                     
         OC    QOFF,QOFF          IF NO OFFICE                                  
         BZ    VALKCLT            CHECK CLIENT INPUT                            
         MVI   OCFLAG,C'O'        MARK OFFICE INPUTTED                          
*                                                                               
VALKCLT  LA    R3,TR2CLTH         VALIDATE CLIENT                               
         GOTO1 AVALCLT,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKCLI,QCLT                                                     
         OC    QCLT,QCLT          IF NO CLIENT REQUESTED                        
         BNZ   VALKCLT5                                                         
         GOTO1 AVALFLD,APPARM,TR2PRDH,1                                         
         CLI   APPARM,X'FF'       CAN'T BE ANY PRD                              
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
VALKPRD  LA    R3,TR2PRDH         VALIDATE PRODUCT                              
         GOTO1 AVALPRD,(R3)                                                     
         BNE   VALKX                                                            
         MVC   MPRKPRD,QPRD                                                     
*                                                                               
VALKPOST LA    R1,TR2PROH         VALIDATE POSTING                              
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
         CLC   SCFULL+3(1),PMAXNUM      REQUESTABLE PROFILE #?                  
         BNH   VALK30                                                           
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKX                                                            
*                                                                               
VALK30   MVC   PROWNUM,SCFULL+3                                                 
         GOTO1 ASETFILE           SET ACC FILE & LIMIT ACCESS                   
         BE    VALK40                                                           
         LA    R2,TR2SYSH                                                       
         ST    R2,APCURSOR                                                      
         B     VALKX                                                            
*                                                                               
VALK40   OI    APINDS,APIOKDIS    OKAY TO DISPLAY                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
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
         BAS   RE,CHKPFKS         CHECK PFKEY HIT                               
*                                                                               
         LA    R2,TR2L1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R2                                                          
         OC    QSYS,QSYS          IF SYSTEM INPUT - READ RECORD                 
         BZ    DISR20                                                           
         GOTO1 ARDSYS,APPARM,RECTYPE                                            
         BNE   DISR20                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR20   LA    R2,TR2L2DH         PT TO MEDIA LINE                              
         OC    QMED,QMED          IF MEDIA INPUT - READ RECORD                  
         BZ    DISR22                                                           
         GOTO1 ARDMED,APPARM,RECTYPE                                            
         BNE   DISR22                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR22   LA    R2,TR2L3DH         PT TO OFFICE GROUP LINE                       
         CLI   QSYS,C'P'                                                        
         BNE   DISR23                                                           
         CLI   SVPROF+1,C'Y'      USE OFFICE GROUPS?                            
         BNE   DISR30                                                           
         B     DISR27                                                           
DISR23   CLI   SVPROF,C'Y'        USE OFFICE GROUP?                             
         BNE   DISR30             NO                                            
DISR27   OC    SVOFFG,SVOFFG                                                    
         BZ    DISR30                                                           
         GOTO1 ARDOFF,APPARM,RECTYPE,SVOFFG                                     
         BNE   DISR30                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR30   LA    R2,TR2L4DH         PT TO OFFICE LINE                             
         MVC   OFFCODE,=C'  '                                                   
         OC    QCLT,QCLT          IF CLIENT INPUT -                             
         BZ    *+14                                                             
         MVC   OFFCODE(1),SVCOFF  YES                                           
         B     DISR35                                                           
         OC    QOFF,QOFF          WAS ANY OFFICE INPUT?                         
         BZ    DISR40             NO                                            
         CLI   QOFFIND,C'O'       YES - WAS IT OFFICE CODE                      
         BNE   DISR40             NO - GO TO NEXT                               
         MVC   OFFCODE(1),QOFF                                                  
DISR35   GOTO1 ARDOFF,APPARM,RECTYPE,OFFCODE                                    
         BNE   DISR40                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR40   LA    R2,TR2L5DH         PT TO CLIENT LINE                             
         OC    QCLT,QCLT          IF CLIENT INPUT - READ RECORD                 
         BZ    DISR50                                                           
         GOTO1 ARDCLT,APPARM,RECTYPE                                            
         BNE   DISR50                                                           
         BAS   RE,PRTLIN          PRINT LINE USING REC VALUES                   
*                                                                               
DISR50   LA    R2,TR2L6DH         PT TO PRODUCT LINE                            
         OC    QPRD,QPRD          IF PRODUCT INPUT - READ RECORD                
         BZ    DISR60                                                           
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
         MVC   APPARM,INREC                                                     
         MVI   APPARM+1,ACTMAI                                                  
DISRX    TM    TWAMODE,TWAMLSM    ARE WE LIST (AT SOME POINT)                   
         BNO   DISRXX             NO - GET OUT                                  
         MVI   TWALSACT,ACTRACE   YES -TELL GENERAL THIS IS LAST ACTION         
         OI    TWALSCTL,TWALSRTN  AND TRACE WANTS CONTROL                       
         OI    TWALSCTL,TWALSHLD  AND SCREEN HELD ON CRT                        
DISRXX   L     R1,AACTHDR                                                       
         STCM  R1,15,APCURSOR     SET CURSOR TO ACTION FIELD                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
*=======================================*                                       
* CHGPOSTS - CHANGES POST FIELD IN KEY  *                                       
*          - AND POST DESCRIPTION FIELD *                                       
*=======================================*                                       
*                                                                               
CHGPOSTS NTR1                                                                   
         SR    RE,RE                                                            
         ZIC   R1,PROWNUM                                                       
         BCTR  R1,0                                                             
         LA    RF,APTABLN         LENGTH OF APTABLE                             
         MR    RE,R1                                                            
         L     R2,APTABLE                                                       
         AR    R2,RF                                                            
         MVC   ELETYPE,0(R2)                                                    
         MVC   TR2DES(26),1(R2)   MOVE IN TRACE POSTING ROW LABEL               
         OI    TR2DESH+6,X'80'                                                  
         B     EXIT                                                             
         EJECT                                                                  
*=======================================*                                       
* CHKPFKS - CHECH IF PFKEY WAS HIT      *                                       
*=======================================*                                       
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
         EDIT  PROWNUM,(3,TR2PRO),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TR2PROH+6,X'80'    RE-DISPLAY PROFILE ROW NUMBER                 
         BAS   RE,CHGPOSTS        RE-DISPLAY PROFILE LABEL                      
         BAS   RE,CLRSCR                                                        
         B     CHKPFKX                                                          
*                                                                               
CHKPFK10 CLI   APPFKEY,PFK11      NEXT POSTING?                                 
         BNE   CHKPFKX                                                          
         ZIC   RE,PROWNUM                                                       
         ZIC   R1,PMAXNUM         IF AT 10                                      
         CR    RE,R1                                                            
         BNE   *+8                                                              
         LA    RE,0               START AT 1 AGAIN                              
         LA    RE,1(RE)                                                         
         STC   RE,PROWNUM                                                       
         EDIT  PROWNUM,(3,TR2PRO),DUB=APDUB,WRK=APWORK,ALIGN=LEFT               
         OI    TR2PROH+6,X'80'    RE-DISPLAY PROFILE ROW NUMBER                 
         BAS   RE,CHGPOSTS        RE-DISPLAY PROFILE LABEL                      
         BAS   RE,CLRSCR                                                        
*                                                                               
CHKPFKX  B     EXIT                                                             
         EJECT                                                                  
*==========================================*                                    
* PRTLIN - PULLS OUT REC VALUES & DISPLAYS *                                    
*           ON SCREEN LINE                 *                                    
*==========================================*                                    
*                                                                               
PRTLIN   NTR1                                                                   
         L     R3,AIOAREA1        PT TO POSTING TABLE                           
         AH    R3,DATADISP        PT TO FIRST ELEMENT                           
         USING MTPELD,R3                                                        
*                                                                               
PRTL5    CLI   0(R3),0            END OF RECORD?                                
         BE    PRTLX                                                            
         CLI   0(R3),MTPELQ       MEDIA TRANSFER PROFILE?                       
         BNE   PRTL30             NO - TRY NEXT ELEMENT                         
         CLC   MTPFNUM,ELETYPE    CORRECT PROFILE                               
         BNE   PRTL30             NO TRY NEXT ELEMENT                           
*                                                                               
         CLI   MTPFNUM,MTPFIPCT                                                 
         BE    *+12                                                             
         CLI   MTPFNUM,MTPFIPT2                                                 
         BNE   PRTL20                                                           
         EDIT  (4,MTPFDATA),(5,DVAL),2,ALIGN=LEFT,ZERO=NOBLANK,        X        
               DUB=APDUB,WRK=APWORK                                             
         B     PRTL25                                                           
*                                                                               
PRTL20   ZIC   RE,MTPLN                                                         
         LA    RF,MTPFDATA-MTPEL                                                
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DVAL(0),MTPFDATA   MOVE IN VARIABLE LENGTH DATA                  
*                                                                               
PRTL25   LA    R4,DLSTACT         MOVE LAST ACTIVITY TO SCREEN                  
         MVC   0(L'MTPFCHID,R4),MTPFCHID                                        
         LA    R4,L'MTPFCHID+1(R4)                                              
         GOTO1 VDATCON,APPARM,(3,MTPFCHDT),(8,0(R4))                            
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
CLRSCR   NTR1                                                                   
         LA    R1,TR2L1DH         PT TO FIRST LINE ON SCREEN                    
         USING DISD,R1                                                          
*                                                                               
CLRSCR5  XC    DVAL,DVAL                                                        
         XC    DLSTACT,DLSTACT                                                  
         OI    DLINH+6,X'80'                                                    
*                                                                               
         LA    RF,TR2L2DH-TR2L1DH LENGTH OF ONE LINE                            
         AR    R1,RF              PT TO NEXT LINE                               
         LA    RE,TR2PFKH                                                       
         CR    R1,RE                                                            
         BL    CLRSCR5                                                          
         B     EXIT                                                             
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
* -- ACTRAWRK                                                                   
*                                                                               
       ++INCLUDE ACTRAWRK                                                       
         EJECT                                                                  
LOCALD   DSECT                                                                  
OCFLAG   DS    CL1                O= OFFICE INPUTTED                            
OFFCODE  DS    CL2                                                              
ELETYPE  DS    XL1                ELEMENT TYPE                                  
         EJECT                                                                  
*                                                                               
* DISD - DSECT TO COVER TRACE SCREEN                                            
*                                                                               
DISD     DSECT                                                                  
DDESH    DS    CL8                FOR HEADER                                    
DDES     DS    CL15               DISCRIPTION                                   
DLINH    DS    CL8                                                              
DVAL     DS    CL20               VALUE                                         
         DS    CL4                                                              
DLSTACT  DS    CL13               LAST ACTIVITY                                 
         DS    CL1                                                              
DISDL    EQU   *-DDESH                                                          
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   TRAPTGH                                                          
       ++INCLUDE ACTRAFAD                                                       
         SPACE 2                                                                
         ORG                                                                    
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACTRA06   02/15/19'                                      
         END                                                                    
